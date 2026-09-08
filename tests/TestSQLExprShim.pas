program TestSQLExprShim;

{$mode delphi}
{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, SQLDB, SQLite3Conn,
  SqlExpr,  // Our shim!
  DBClient, Provider;  // TClientDataSet / TDataSetProvider shims

var
  Conn: SqlExpr.TSQLConnection;  // Our shim TSQLConnection
  DS: SqlExpr.TSQLDataSet;       // Our shim TSQLDataSet
  DBPath: string;
  Owner: TComponent;
  Prov: TDataSetProvider;
  CDS: TClientDataSet;
  Failed: Boolean;
  Writer: SqlExpr.TSQLConnection;

// A second connection (stands in for an external "sqlite3 db INSERT ...")
// must be able to write while the first connection is connected but idle;
// SQLDB otherwise keeps the read transaction and its SHARED lock open until
// Disconnect (sqlite-bug-catalog #13).
procedure CheckExternalWrite(const Stage: string; ExpectRows: Integer);
begin
  try
    Writer.ExecuteDirect('INSERT INTO customers VALUES (' + IntToStr(ExpectRows) +
      ', ''Ext'', ''ext@example.com'')');
  except
    on E: Exception do
    begin
      WriteLn('FAIL: external write blocked ', Stage, ': ', E.Message);
      Halt(1);
    end;
  end;
  DS.SQL.Text := 'SELECT COUNT(*) AS n FROM customers';
  DS.Open;
  if DS.Fields[0].AsInteger <> ExpectRows then
  begin
    WriteLn('FAIL: ', Stage, ': expected ', ExpectRows, ' rows, got ', DS.Fields[0].AsInteger);
    Halt(1);
  end;
  DS.Close;
  WriteLn('  external write ok ', Stage, ' (', ExpectRows, ' rows)');
end;

begin
  WriteLn('=== SQLExpr Shim Test (Delphi-compatible API → SQLDB) ===');
  WriteLn;

  DBPath := '/tmp/dbdesigner_shim_test.db';

  // Remove old test DB
  if FileExists(DBPath) then
    DeleteFile(DBPath);

  Conn := SqlExpr.TSQLConnection.Create(nil);
  DS := SqlExpr.TSQLDataSet.Create(nil);
  Writer := SqlExpr.TSQLConnection.Create(nil);
  try
    Writer.DriverName := 'SQLite';
    Writer.Params.Values['Database'] := DBPath;
    // Configure using Delphi-style DriverName + Params
    Conn.DriverName := 'SQLite';
    Conn.Params.Values['Database'] := DBPath;

    // Link the dataset BEFORE the connection is opened, exactly as the
    // .lfm streaming of DBDM.lfm (Database = SQLConn) does. SQLDB copies the
    // connection's transaction into the dataset at this moment, so the
    // connection must already own one (sqlite-bug-catalog #2).
    DS.SQLConnection := Conn;
    if DS.Transaction = nil then
    begin
      WriteLn('FAIL: dataset linked before Open has no transaction');
      Halt(1);
    end;

    // Open connection (shim maps DriverName→ConnectorType)
    Conn.Open;
    WriteLn('Connected via shim DriverName="SQLite" → ConnectorType="', Conn.ConnectorType, '"');
    WriteLn('Database: ', Conn.DatabaseName);

    // Create a table via ExecuteDirect
    Conn.ExecuteDirect('CREATE TABLE customers (id INTEGER PRIMARY KEY, name TEXT, email TEXT)');
    WriteLn('Created customers table');

    // Insert data
    Conn.ExecuteDirect('INSERT INTO customers VALUES (1, ''Alice'', ''alice@example.com'')');
    Conn.ExecuteDirect('INSERT INTO customers VALUES (2, ''Bob'', ''bob@example.com'')');
    Conn.ExecuteDirect('INSERT INTO customers VALUES (3, ''Charlie'', ''charlie@example.com'')');
    WriteLn('Inserted 3 rows');

    // Query using our TSQLDataSet shim (linked above)
    DS.SQL.Text := 'SELECT * FROM customers ORDER BY id';
    DS.Open;

    WriteLn;
    WriteLn('Query results via TSQLDataSet shim:');
    while not DS.EOF do
    begin
      WriteLn('  id=', DS.FieldByName('id').AsInteger,
              ' name=', DS.FieldByName('name').AsString,
              ' email=', DS.FieldByName('email').AsString);
      DS.Next;
    end;
    DS.Close;

    WriteLn;
    WriteLn('Testing that an idle connection does not lock the file:');
    Writer.Open;
    CheckExternalWrite('after a closed SELECT', 4);

    // Test SetSchemaInfo (table listing)
    WriteLn;
    WriteLn('Testing SetSchemaInfo(stTables):');
    DS.SetSchemaInfo(1 {stTables}, '', '');  // stTables = 1
    DS.Open;
    while not DS.EOF do
    begin
      // Column 3 is TABLE_NAME in our schema layout
      WriteLn('  Table: ', DS.Fields[3].AsString);
      DS.Next;
    end;
    DS.Close;

    // TClientDataSet fed by a TDataSetProvider, wired by name exactly like
    // EditorQuery.lfm (OutputQry -> OutputDataSetProvider -> OutputClientDataSet,
    // sqlite-bug-catalog #5): the client dataset must copy the rows and an
    // invalid statement must raise the SQL error, not the TBufDataset one.
    WriteLn;
    WriteLn('Testing TClientDataSet via ProviderName:');
    Owner := TComponent.Create(nil);
    try
      Prov := TDataSetProvider.Create(Owner);
      Prov.Name := 'OutputDataSetProvider';
      Prov.DataSet := DS;
      CDS := TClientDataSet.Create(Owner);
      CDS.Name := 'OutputClientDataSet';
      CDS.ProviderName := 'OutputDataSetProvider';
      CDS.ReadOnly := True;

      DS.SQL.Text := 'SELECT * FROM customers ORDER BY id';
      CDS.Open;
      if (CDS.RecordCount <> 4) or (CDS.FieldByName('name').AsString <> 'Alice') then
      begin
        WriteLn('FAIL: client dataset has ', CDS.RecordCount, ' rows, first name "',
          CDS.FieldByName('name').AsString, '" (expected 4 / Alice)');
        Halt(1);
      end;
      WriteLn('  ', CDS.RecordCount, ' rows copied, first row: ', CDS.FieldByName('name').AsString);
      // The provider closed the source query after the fetch (Delphi
      // semantics), so the file must be writable while the client dataset
      // stays open - Query mode leaves it open after Execute.
      if DS.Active then
      begin
        WriteLn('FAIL: source query still open after the client dataset fetch');
        Halt(1);
      end;
      CheckExternalWrite('while the client dataset is open', 5);
      CDS.Close;
      DS.Close;

      DS.SQL.Text := 'SELECT * FROM nosuch';
      Failed := False;
      try
        CDS.Open;
      except
        on E: Exception do
        begin
          Failed := Pos('nosuch', E.Message) > 0;
          WriteLn('  invalid statement raised: ', E.Message);
        end;
      end;
      if not Failed then
      begin
        WriteLn('FAIL: invalid statement did not raise the SQL error');
        Halt(1);
      end;
    finally
      Owner.Free;
    end;

    Writer.Close;
    Conn.Close;
    WriteLn;
    WriteLn('SUCCESS: SQLExpr shim works correctly with SQLite!');
  finally
    DS.Free;
    Conn.Free;
    Writer.Free;
    // Cleanup
    if FileExists(DBPath) then
      DeleteFile(DBPath);
  end;
end.
