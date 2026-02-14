program TestSQLite;

{$mode delphi}
{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, SQLDB, SQLite3Conn;

var
  Conn: TSQLite3Connection;
  Trans: TSQLTransaction;
  Query: TSQLQuery;
  DBPath: string;
begin
  WriteLn('=== SQLite Connection Test ===');
  WriteLn;

  DBPath := '/tmp/dbdesigner_test.db';

  // Remove old test DB
  if FileExists(DBPath) then
    DeleteFile(DBPath);

  Conn := TSQLite3Connection.Create(nil);
  Trans := TSQLTransaction.Create(nil);
  Query := TSQLQuery.Create(nil);
  try
    Conn.DatabaseName := DBPath;
    Trans.Database := Conn;
    Conn.Transaction := Trans;
    Query.Database := Conn;
    Query.Transaction := Trans;

    // Open connection (creates the file)
    Conn.Open;
    WriteLn('Connected to SQLite: ', DBPath);

    // Create a table
    Conn.ExecuteDirect('CREATE TABLE test_table (id INTEGER PRIMARY KEY, name TEXT, value REAL)');
    Trans.Commit;
    WriteLn('Created test_table');

    // Insert data
    Conn.ExecuteDirect('INSERT INTO test_table VALUES (1, ''hello'', 3.14)');
    Conn.ExecuteDirect('INSERT INTO test_table VALUES (2, ''world'', 2.71)');
    Trans.Commit;
    WriteLn('Inserted 2 rows');

    // Query data
    Query.SQL.Text := 'SELECT * FROM test_table ORDER BY id';
    Query.Open;
    WriteLn;
    WriteLn('Query results:');
    while not Query.EOF do
    begin
      WriteLn('  id=', Query.FieldByName('id').AsInteger,
              ' name=', Query.FieldByName('name').AsString,
              ' value=', Query.FieldByName('value').AsFloat:0:2);
      Query.Next;
    end;
    Query.Close;

    // Test table listing (metadata)
    WriteLn;
    Conn.GetTableNames(Query.SQL);
    WriteLn('Tables in DB: ', Query.SQL.Text);

    Conn.Close;
    WriteLn;
    WriteLn('SUCCESS: SQLite connectivity works!');
  finally
    Query.Free;
    Trans.Free;
    Conn.Free;
    // Cleanup
    if FileExists(DBPath) then
      DeleteFile(DBPath);
  end;
end.
