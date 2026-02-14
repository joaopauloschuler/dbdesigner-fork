program TestSQLExprShim;

{$mode delphi}
{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, SQLDB, SQLite3Conn,
  SqlExpr;  // Our shim!

var
  Conn: SqlExpr.TSQLConnection;  // Our shim TSQLConnection
  DS: SqlExpr.TSQLDataSet;       // Our shim TSQLDataSet
  DBPath: string;
begin
  WriteLn('=== SQLExpr Shim Test (Delphi-compatible API → SQLDB) ===');
  WriteLn;

  DBPath := '/tmp/dbdesigner_shim_test.db';

  // Remove old test DB
  if FileExists(DBPath) then
    DeleteFile(DBPath);

  Conn := SqlExpr.TSQLConnection.Create(nil);
  DS := SqlExpr.TSQLDataSet.Create(nil);
  try
    // Configure using Delphi-style DriverName + Params
    Conn.DriverName := 'SQLite';
    Conn.Params.Values['Database'] := DBPath;

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

    // Query using our TSQLDataSet shim
    DS.SQLConnection := Conn;  // Uses our shim's SQLConnection property
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

    // Test SetSchemaInfo (table listing)
    WriteLn;
    WriteLn('Testing SetSchemaInfo(stTables):');
    DS.SetSchemaInfo(4 {stTables}, '', '');  // stTables = 4
    DS.Open;
    while not DS.EOF do
    begin
      // Column 3 is TABLE_NAME in our schema layout
      WriteLn('  Table: ', DS.Fields[3].AsString);
      DS.Next;
    end;
    DS.Close;

    Conn.Close;
    WriteLn;
    WriteLn('SUCCESS: SQLExpr shim works correctly with SQLite!');
  finally
    DS.Free;
    Conn.Free;
    // Cleanup
    if FileExists(DBPath) then
      DeleteFile(DBPath);
  end;
end.
