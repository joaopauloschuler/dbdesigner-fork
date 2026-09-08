program TestMySQLShim;

// Standalone check of the sqlexpr.pas shim against a MySQL 8 server
// (mysql-bug-catalog #1, #2, #4, #5, #7). Expects the server used by the catalog:
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
    // The reverse engineering reads SHOW KEYS by name (mysql-bug-catalog #5):
    // PRIMARY, shimtest_email (unique), shimtest_name (prefix 10)
    if DS.FieldByName('Non_unique').AsString <> '0' then
      Fail('SHOW KEYS: PRIMARY has Non_unique=' + DS.FieldByName('Non_unique').AsString);
    DS.Next;
    if (DS.FieldByName('Key_name').AsString <> 'shimtest_email') or
       (DS.FieldByName('Non_unique').AsString <> '0') or
       (not DS.FieldByName('Sub_part').IsNull) then
      Fail('SHOW KEYS: shimtest_email row: Key_name/Non_unique/Sub_part = ' +
        DS.FieldByName('Key_name').AsString + '/' + DS.FieldByName('Non_unique').AsString +
        '/' + DS.FieldByName('Sub_part').AsString);
    DS.Next;
    if (DS.FieldByName('Key_name').AsString <> 'shimtest_name') or
       (DS.FieldByName('Non_unique').AsString <> '1') or
       (DS.FieldByName('Sub_part').AsString <> '10') then
      Fail('SHOW KEYS: shimtest_name row: Key_name/Non_unique/Sub_part = ' +
        DS.FieldByName('Key_name').AsString + '/' + DS.FieldByName('Non_unique').AsString +
        '/' + DS.FieldByName('Sub_part').AsString);
    DS.Close;

    // SHOW FIELDS by name: Extra carries auto_increment (mysql-bug-catalog #4)
    DS.SQL.Text := 'SHOW FIELDS FROM shimtest_customers';
    DS.Open;
    WriteLn('  SHOW FIELDS: ', DS.FieldCount, ' fields (Field=', DS.FieldByName('Field').AsString,
      ', Extra=', DS.FieldByName('Extra').AsString, ')');
    if (DS.FieldByName('Field').AsString <> 'id') or (DS.FieldByName('Key').AsString <> 'PRI') or
       (DS.FieldByName('Null').AsString <> 'NO') or
       (Pos('auto_increment', LowerCase(DS.FieldByName('Extra').AsString)) = 0) then
      Fail('SHOW FIELDS: id row: Field/Key/Null/Extra = ' + DS.FieldByName('Field').AsString +
        '/' + DS.FieldByName('Key').AsString + '/' + DS.FieldByName('Null').AsString + '/' +
        DS.FieldByName('Extra').AsString);
    DS.Close;

    // FOREIGN KEY metadata the reverse engineering uses (mysql-bug-catalog #7)
    Conn.ExecuteDirect('DROP TABLE IF EXISTS shimtest_orders');
    Conn.ExecuteDirect('CREATE TABLE shimtest_orders (' +
      'id INTEGER NOT NULL AUTO_INCREMENT, customer_id INTEGER NOT NULL, ' +
      'parent_id INTEGER NULL, PRIMARY KEY (id), ' +
      'CONSTRAINT shimtest_fk_customer FOREIGN KEY (customer_id) ' +
      'REFERENCES shimtest_customers (id) ON DELETE CASCADE ON UPDATE RESTRICT, ' +
      'CONSTRAINT shimtest_fk_parent FOREIGN KEY (parent_id) ' +
      'REFERENCES shimtest_orders (id)) ENGINE=InnoDB');
    DS.SQL.Text := 'SELECT k.TABLE_NAME AS tblname, k.CONSTRAINT_NAME AS fkname, ' +
      'k.ORDINAL_POSITION AS fkseq, k.COLUMN_NAME AS fkcol, ' +
      'k.REFERENCED_TABLE_NAME AS reftable, k.REFERENCED_COLUMN_NAME AS refcol, ' +
      'r.UPDATE_RULE AS on_update, r.DELETE_RULE AS on_delete ' +
      'FROM information_schema.KEY_COLUMN_USAGE k ' +
      'JOIN information_schema.REFERENTIAL_CONSTRAINTS r ' +
      'ON r.CONSTRAINT_SCHEMA=k.CONSTRAINT_SCHEMA ' +
      'AND r.CONSTRAINT_NAME=k.CONSTRAINT_NAME AND r.TABLE_NAME=k.TABLE_NAME ' +
      'WHERE k.TABLE_SCHEMA=DATABASE() AND k.TABLE_NAME=''shimtest_orders'' ' +
      'AND k.REFERENCED_TABLE_NAME IS NOT NULL ' +
      'ORDER BY k.TABLE_NAME, k.CONSTRAINT_NAME, k.ORDINAL_POSITION';
    DS.Open;
    n := 0;
    while not DS.EOF do
    begin
      WriteLn('  FK: ', DS.FieldByName('tblname').AsString, '.', DS.FieldByName('fkname').AsString,
        ' ', DS.FieldByName('fkcol').AsString, ' -> ', DS.FieldByName('reftable').AsString, '.',
        DS.FieldByName('refcol').AsString, ' ON UPDATE ', DS.FieldByName('on_update').AsString,
        ' ON DELETE ', DS.FieldByName('on_delete').AsString);
      Inc(n);
      DS.Next;
    end;
    if n <> 2 then
      Fail('information_schema FK query returned ' + IntToStr(n) + ' rows, expected 2');
    DS.First;
    if (DS.FieldByName('fkname').AsString <> 'shimtest_fk_customer') or
       (DS.FieldByName('fkcol').AsString <> 'customer_id') or
       (DS.FieldByName('reftable').AsString <> 'shimtest_customers') or
       (DS.FieldByName('refcol').AsString <> 'id') or
       (DS.FieldByName('on_update').AsString <> 'RESTRICT') or
       (DS.FieldByName('on_delete').AsString <> 'CASCADE') then
      Fail('information_schema FK query: shimtest_fk_customer row is wrong');
    DS.Next;
    if (DS.FieldByName('fkname').AsString <> 'shimtest_fk_parent') or
       (DS.FieldByName('reftable').AsString <> 'shimtest_orders') or
       (DS.FieldByName('on_delete').AsString <> 'NO ACTION') then
      Fail('information_schema FK query: self-referencing shimtest_fk_parent row is wrong');
    DS.Close;
    Conn.ExecuteDirect('DROP TABLE shimtest_orders');

    Conn.ExecuteDirect('DROP TABLE shimtest_customers');
    Conn.Close;
    WriteLn;
    WriteLn('SUCCESS: SQLExpr shim works correctly with MySQL 8!');
  finally
    DS.Free;
    Conn.Free;
  end;
end.
