unit MySQLLib;

// Picks the MySQL client library for mysql80dyn/MySQL80Conn.
//
// FPC's mysql80dyn tries the unversioned 'libmysqlclient.so' first, a symlink
// that only libmysqlclient-dev provides. Distribution runtimes ship just the
// versioned 'libmysqlclient.so.21', so on a fresh machine the connector could
// not load without a manual symlink or LD_LIBRARY_PATH. Probe the versioned
// name first and, when it loads, initialise mysql80dyn with it at start-up;
// the later parameterless InitialiseMysql in TMySQL80Connection.DoInternalConnect
// then only bumps the reference count. If nothing loads the connector keeps
// its usual "could not load" error at Open time.

{$mode delphi}

interface

implementation

{$IFDEF LINUX}
uses SysUtils, dynlibs, mysql80dyn;

procedure PickMySQLLibrary;
const
  Candidates: array[0..1] of string = ('libmysqlclient.so.21', 'libmysqlclient.so');
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
      try
        InitialiseMysql(Candidates[i]);
      except
        // leave it to the connector to report at Open
      end;
      Exit;
    end;
  end;
end;

initialization
  PickMySQLLibrary;
finalization
  if MysqlLoadedLibrary <> '' then
    ReleaseMysql;
{$ENDIF}

end.
