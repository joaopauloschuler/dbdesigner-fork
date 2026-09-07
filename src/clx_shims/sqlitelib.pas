unit SQLiteLib;

// Picks the SQLite shared-library name for sqlite3dyn/SQLite3Conn.
//
// FPC's sqlite3dyn defaults to 'libsqlite3.so', a symlink that only the
// libsqlite3-dev package provides. Distribution runtimes ship just the
// versioned 'libsqlite3.so.0', so on a fresh machine the connector could not
// load without a manual symlink on LD_LIBRARY_PATH. Try the versioned name
// first and fall back to the plain one; the default stays if neither loads
// so the connector still reports the usual "could not load" error.

{$mode delphi}

interface

implementation

{$IFDEF LINUX}
uses dynlibs, sqlite3dyn;

procedure PickSQLiteLibrary;
const
  Candidates: array[0..1] of string = ('libsqlite3.so.0', 'libsqlite3.so');
var
  i: Integer;
  h: TLibHandle;
begin
  for i := Low(Candidates) to High(Candidates) do
  begin
    h := LoadLibrary(Candidates[i]);
    if h <> NilHandle then
    begin
      FreeLibrary(h);
      SQLiteDefaultLibrary := Candidates[i];
      Exit;
    end;
  end;
end;

initialization
  PickSQLiteLibrary;
{$ENDIF}

end.
