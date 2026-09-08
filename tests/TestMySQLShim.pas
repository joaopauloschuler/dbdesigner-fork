program TestMySQLShim;

// Standalone check of the sqlexpr.pas shim against a MySQL 8 server
// (mysql-bug-catalog #1 and #2). Expects the server used by the catalog:
// 127.0.0.1:3306, user bpsa / password bpsa, scratch database dbdtest.
// Override with MYSQL_HOST / MYSQL_PORT / MYSQL_USER / MYSQL_PASSWORD /
// MYSQL_DATABASE. Prints SUCCESS, or SKIP when the server is unreachable.
//
//   fpc -Mdelphi -Fusrc/clx_shims -FU<scratch> -o<scratch>/TestMySQLShim tests/TestMySQLShim.pas

{$mode delphi}
{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, SQLDB,
  SqlExpr;  // Our shim (links MySQL80Conn + MySQLLib)

var
  Conn: SqlExpr.TSQLConnection;
  DS: SqlExpr.TSQLDataSet;
  Host, Port, User, Pass, DBName: string;
  n: Integer;

function Env(const Name, Default: string): string;
begin
  Result := GetEnvironmentVariable(Name);
  if Result = '' then
    Result := Default;
end;

procedure Fail(const Msg: string);
begin
  WriteLn('FAIL: ', Msg);
  Halt(1);
end;

// Opens DS with the given schema query and checks the number of fields the
// connector exposes. Bare "NULL AS x" columns were dropped by mysql80conn
// (MYSQL_TYPE_NULL is not mapped to a FieldDef), which misaligned every
// positional Fields[n] read in DBDM/DBEERDM.
procedure CheckSchema(const What: string; SchemaType: Integer; const ObjName: string;
  ExpectFields: Integer);
var
  i: Integer;
  Line: string;
begin
  DS.SetSchemaInfo(SchemaType, ObjName, '');
  WriteLn('  ', What, ' query: ', StringReplace(DS.SQL.Text, #13#10, ' ', [rfReplaceAll]));
  DS.Open;
  if DS.FieldCount <> ExpectFields then
    Fail(What + ': expected ' + IntToStr(ExpectFields) + ' fields, got ' +
      IntToStr(DS.FieldCount) + ' (bare NULL columns dropped?)');
  n := 0;
  while not DS.EOF do
  begin
    Line := '';
    for i := 0 to DS.FieldCount - 1 do
      Line := Line + DS.Fields[i].AsString + '|';
    WriteLn('    ', Line);
    Inc(n);
    DS.Next;
  end;
  WriteLn('  ', What, ': ', DS.FieldCount, ' fields, ', n, ' rows');
  DS.Close;
end;

begin
  WriteLn('=== SQLExpr Shim Test against MySQL 8 ===');
  Host := Env('MYSQL_HOST', '127.0.0.1');
  Port := Env('MYSQL_PORT', '3306');
  User := Env('MYSQL_USER', 'bpsa');
  Pass := Env('MYSQL_PASSWORD', 'bpsa');
  DBName := Env('MYSQL_DATABASE', 'dbdtest');

  Conn := SqlExpr.TSQLConnection.Create(nil);
  DS := SqlExpr.TSQLDataSet.Create(nil);
  try
    // Same values DBDM reads from DBConn.ini
    Conn.DriverName := 'MySQL';
    Conn.Params.Values['HostName'] := Host;
    Conn.Params.Values['Port'] := Port;
    Conn.Params.Values['Database'] := DBName;
    Conn.Params.Values['User_Name'] := User;
    Conn.Params.Values['Password'] := Pass;
    DS.SQLConnection := Conn;

    if Conn.ConnectorType <> 'MySQL 8.0' then
      Fail('DriverName="MySQL" mapped to ConnectorType="' + Conn.ConnectorType +
        '" (expected "MySQL 8.0"; is MySQL80Conn linked?)');

    try
      Conn.Open;
    except
      on E: Exception do
      begin
        WriteLn('SKIP: cannot connect to MySQL at ', Host, ':', Port, ' as ', User,
          ' (database ', DBName, '): ', E.Message);
        Halt(0);
      end;
    end;
    WriteLn('Connected via shim DriverName="MySQL" -> ConnectorType="', Conn.ConnectorType,
      '" to ', Host, ':', Port, '/', DBName);

    Conn.ExecuteDirect('DROP TABLE IF EXISTS shimtest_customers');
    Conn.ExecuteDirect('CREATE TABLE shimtest_customers (' +
      'id INTEGER NOT NULL AUTO_INCREMENT, name VARCHAR(40) NOT NULL, ' +
      'email VARCHAR(80), PRIMARY KEY (id), UNIQUE KEY shimtest_email (email), ' +
      'KEY shimtest_name (name(10))) ENGINE=InnoDB');
    WriteLn('Created shimtest_customers');
    Conn.ExecuteDirect('INSERT INTO shimtest_customers (name, email) VALUES (''Alice'', ''alice@example.com'')');
    Conn.ExecuteDirect('INSERT INTO shimtest_customers (name, email) VALUES (''Bob'', ''bob@example.com'')');
    Conn.ExecuteDirect('INSERT INTO shimtest_customers (name, email) VALUES (''Charlie'', ''charlie@example.com'')');
    WriteLn('Inserted 3 rows');

    DS.SQL.Text := 'SELECT * FROM shimtest_customers ORDER BY id';
    DS.Open;
    n := 0;
    while not DS.EOF do
    begin
      WriteLn('  id=', DS.FieldByName('id').AsInteger,
              ' name=', DS.FieldByName('name').AsString,
              ' email=', DS.FieldByName('email').AsString);
      Inc(n);
      DS.Next;
    end;
    DS.Close;
    if n <> 3 then
      Fail('SELECT returned ' + IntToStr(n) + ' rows, expected 3');

    // dbExpress layouts the shim reproduces (see TSQLDataSet.SetSchemaInfo):
    // stTables 5 fields, stColumns 14 fields, stIndexes 11 fields.
    WriteLn;
    WriteLn('Testing SetSchemaInfo:');
    CheckSchema('stTables', stTables, '', 5);
    DS.SetSchemaInfo(stTables, '', '');
    DS.Open;
    n := -1;
    while not DS.EOF do
    begin
      if DS.Fields[3].AsString = 'shimtest_customers' then
        n := DS.RecNo;
      DS.Next;
    end;
    DS.Close;
    if n < 0 then
      Fail('stTables: Fields[3] (TABLE_NAME) never equals shimtest_customers');

    CheckSchema('stColumns', stColumns, 'shimtest_customers', 14);
    DS.SetSchemaInfo(stColumns, 'shimtest_customers', '');
    DS.Open;
    if (DS.Fields[3].AsString <> 'shimtest_customers') or (DS.Fields[4].AsString <> 'id') or
       (DS.Fields[5].AsInteger <> 1) or (LowerCase(DS.Fields[8].AsString) <> 'int') or
       (DS.Fields[13].AsInteger <> 0) then
      Fail('stColumns: first row misaligned: ' + DS.Fields[3].AsString + '|' +
        DS.Fields[4].AsString + '|' + DS.Fields[5].AsString + '|' + DS.Fields[8].AsString +
        '|' + DS.Fields[13].AsString);
    DS.Next; DS.Next;
    if (DS.Fields[4].AsString <> 'email') or (DS.Fields[10].AsInteger <> 80) or
       (DS.Fields[13].AsInteger <> 1) then
      Fail('stColumns: email row misaligned (COLUMN_NAME/COLUMN_LENGTH/COLUMN_NULLABLE)');
    DS.Close;

    CheckSchema('stIndexes', stIndexes, 'shimtest_customers', 11);
    DS.SetSchemaInfo(stIndexes, 'shimtest_customers', '');
    DS.Open;
    // ORDER BY INDEX_NAME: PRIMARY, shimtest_email, shimtest_name
    if (DS.Fields[4].AsString <> 'PRIMARY') or (DS.Fields[5].AsString <> 'id') or
       (DS.Fields[6].AsInteger <> 1) or (DS.Fields[7].AsString <> 'PRIMARY') or
       (DS.Fields[8].AsString <> 'BTREE') then
      Fail('stIndexes: PRIMARY row misaligned: ' + DS.Fields[4].AsString + '|' +
        DS.Fields[5].AsString + '|' + DS.Fields[6].AsString + '|' + DS.Fields[7].AsString +
        '|' + DS.Fields[8].AsString);
    DS.Next;
    if (DS.Fields[4].AsString <> 'shimtest_email') or (DS.Fields[7].AsString <> 'shimtest_email') then
      Fail('stIndexes: UNIQUE row misaligned (PKEY_NAME must carry the unique index name)');
    DS.Next;
    if (DS.Fields[4].AsString <> 'shimtest_name') or (not DS.Fields[7].IsNull) then
      Fail('stIndexes: non-unique row misaligned (PKEY_NAME must be NULL)');
    DS.Close;

    // The reverse engineering / synchronisation code reads SHOW statements too;
    // MySQL 8 adds NULL-typed columns there (Packed), so make sure the rows
    // that are read by position still come back complete.
    WriteLn;
    DS.SQL.Text := 'SHOW KEYS FROM shimtest_customers';
    DS.Open;
    WriteLn('  SHOW KEYS: ', DS.FieldCount, ' fields (Key_name=', DS.FieldByName('Key_name').AsString,
      ', Column_name=', DS.FieldByName('Column_name').AsString, ')');
    if (DS.Fields[2].AsString <> 'PRIMARY') or (DS.Fields[4].AsString <> 'id') then
      Fail('SHOW KEYS: Fields[2]/Fields[4] are not Key_name/Column_name');
    DS.Close;

    Conn.ExecuteDirect('DROP TABLE shimtest_customers');
    Conn.Close;
    WriteLn;
    WriteLn('SUCCESS: SQLExpr shim works correctly with MySQL 8!');
  finally
    DS.Free;
    Conn.Free;
  end;
end.
