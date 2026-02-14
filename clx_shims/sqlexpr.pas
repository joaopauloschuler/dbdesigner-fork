
unit SqlExpr;
{$mode delphi}
interface
uses Classes, DB, SQLDB, SysUtils, DBXpress;

const
  // Schema type constants (Delphi dbExpress)
  stNoSchema = 0;
  stTables = 1;
  stSysTables = 2;
  stColumns = 3;
  stIndexes = 4;
  stProcedures = 5;

type
  TSQLConnection = class(TSQLConnector)
  private
    FDriverName: string;
    FGetDriverFunc: string;
    FLibraryName: string;
    FVendorLib: string;
    FTableScope: TTableScopes;
    FActiveStatements: Integer;
    procedure SetDriverNameEx(const Value: string);
    procedure UpdateConnectorType;
    procedure ApplyParamsToConnection;
  public
    procedure Open;
    property ActiveStatements: Integer read FActiveStatements;
  published
    property DriverName: string read FDriverName write SetDriverNameEx;
    property GetDriverFunc: string read FGetDriverFunc write FGetDriverFunc;
    property LibraryName: string read FLibraryName write FLibraryName;
    property VendorLib: string read FVendorLib write FVendorLib;
    property TableScope: TTableScopes read FTableScope write FTableScope;
  end;

  // Re-export TSQLQuery
  TSQLQuery = SQLDB.TSQLQuery;

  TSQLDataSet = class(SQLDB.TSQLQuery)
  private
    function GetSQLConnection: TSQLConnection;
    procedure SetSQLConnection(Value: TSQLConnection);
  public
    procedure SetSchemaInfo(SchemaType: Integer; const SchemaObjectName, SchemaPattern: string); reintroduce;
    function ExecSQL(ExecDirect: Boolean): Integer; overload;
    function GetQuoteChar: string;
  published
    property SQLConnection: TSQLConnection read GetSQLConnection write SetSQLConnection;
  end;

  TSQLMonitor = class(TComponent)
  private
    FSQLConnection: TComponent;
    FOnLogTrace: TNotifyEvent;
    FActive: Boolean;
    FTraceList: TStringList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property TraceList: TStringList read FTraceList;
  published
    property SQLConnection: TComponent read FSQLConnection write FSQLConnection;
    property OnLogTrace: TNotifyEvent read FOnLogTrace write FOnLogTrace;
    property Active: Boolean read FActive write FActive;
  end;

implementation

{ TSQLConnection }

procedure TSQLConnection.SetDriverNameEx(const Value: string);
begin
  FDriverName := Value;
  UpdateConnectorType;
end;

procedure TSQLConnection.UpdateConnectorType;
var
  LowerDriver: string;
begin
  LowerDriver := LowerCase(FDriverName);
  if Pos('mysql', LowerDriver) > 0 then
    ConnectorType := 'MySQL 5.7'
  else if Pos('sqlite', LowerDriver) > 0 then
    ConnectorType := 'SQLite3'
  else if Pos('oracle', LowerDriver) > 0 then
    ConnectorType := 'Oracle'
  else if (Pos('mssql', LowerDriver) > 0) or (Pos('microsoft', LowerDriver) > 0) then
    ConnectorType := 'MSSQLServer'
  else if Pos('interbase', LowerDriver) > 0 then
    ConnectorType := 'Firebird'
  else if Pos('firebird', LowerDriver) > 0 then
    ConnectorType := 'Firebird'
  else if (Pos('odbc', LowerDriver) > 0) or (Pos('openodbc', LowerDriver) > 0) then
    ConnectorType := 'ODBC'
  else if Pos('postgre', LowerDriver) > 0 then
    ConnectorType := 'PostgreSQL';
end;

procedure TSQLConnection.ApplyParamsToConnection;
var
  i: Integer;
  ParamName, ParamValue: string;
begin
  // Map Delphi dbExpress-style Params to SQLDB connection properties
  for i := 0 to Params.Count - 1 do
  begin
    ParamName := Params.Names[i];
    ParamValue := Params.ValueFromIndex[i];

    if SameText(ParamName, 'HostName') then
      HostName := ParamValue
    else if SameText(ParamName, 'Database') or SameText(ParamName, 'DataBase') then
      DatabaseName := ParamValue
    else if SameText(ParamName, 'User_Name') or SameText(ParamName, 'UserName') then
      UserName := ParamValue
    else if SameText(ParamName, 'Password') then
      Password := ParamValue
    else if SameText(ParamName, 'Port') then
    begin
      // Some databases need port in params
      if ParamValue <> '' then
        Params.Values['Port'] := ParamValue;
    end;
    // Other params (BlobSize, LocaleCode, ErrorResourceFile) are ignored
    // as they don't apply to SQLDB
  end;
end;

procedure TSQLConnection.Open;
begin
  // Ensure ConnectorType is set before connecting
  if ConnectorType = '' then
    UpdateConnectorType;

  // Map Params to SQLDB connection properties
  ApplyParamsToConnection;

  // Ensure a transaction is available (SQLDB requires one)
  if Transaction = nil then
  begin
    Transaction := TSQLTransaction.Create(Self);
    Transaction.DataBase := Self;
  end;

  try
    inherited;
  except
    on E: Exception do
    begin
      // Re-raise with more context
      raise;
    end;
  end;
end;

{ TSQLDataSet }

function TSQLDataSet.GetSQLConnection: TSQLConnection;
begin
  if Database is TSQLConnection then
    Result := TSQLConnection(Database)
  else
    Result := nil;
end;

procedure TSQLDataSet.SetSQLConnection(Value: TSQLConnection);
begin
  Database := Value;
end;

procedure TSQLDataSet.SetSchemaInfo(SchemaType: Integer; const SchemaObjectName, SchemaPattern: string);
var
  Conn: TSQLConnection;
  LowerDriver: string;
begin
  if SchemaType = stTables then
  begin
    // Generate appropriate SQL for listing tables based on the driver
    Conn := GetSQLConnection;
    if Conn <> nil then
      LowerDriver := LowerCase(Conn.FDriverName)
    else
      LowerDriver := '';

    if Pos('mysql', LowerDriver) > 0 then
      SQL.Text := 'SELECT NULL AS RECNO, NULL AS CATALOG_NAME, NULL AS SCHEMA_NAME, ' +
                  'TABLE_NAME, TABLE_TYPE FROM INFORMATION_SCHEMA.TABLES ' +
                  'WHERE TABLE_SCHEMA = DATABASE() ORDER BY TABLE_NAME'
    else if Pos('postgre', LowerDriver) > 0 then
      SQL.Text := 'SELECT NULL AS RECNO, NULL AS CATALOG_NAME, schemaname AS SCHEMA_NAME, ' +
                  'tablename AS TABLE_NAME, ''TABLE'' AS TABLE_TYPE ' +
                  'FROM pg_tables WHERE schemaname NOT IN (''pg_catalog'', ''information_schema'') ' +
                  'ORDER BY tablename'
    else if Pos('sqlite', LowerDriver) > 0 then
      SQL.Text := 'SELECT NULL AS RECNO, NULL AS CATALOG_NAME, NULL AS SCHEMA_NAME, ' +
                  'name AS TABLE_NAME, type AS TABLE_TYPE FROM sqlite_master ' +
                  'WHERE type=''table'' ORDER BY name'
    else
      // Generic fallback - may work for some databases
      SQL.Text := 'SELECT NULL AS RECNO, NULL AS CATALOG_NAME, NULL AS SCHEMA_NAME, ' +
                  'TABLE_NAME, TABLE_TYPE FROM INFORMATION_SCHEMA.TABLES ORDER BY TABLE_NAME';
  end;
  // For stNoSchema, the caller sets SQL.Text directly - no action needed
end;

function TSQLDataSet.GetQuoteChar: string;
var
  LowerDriver: string;
  Conn: TSQLConnection;
begin
  Result := '"';
  Conn := GetSQLConnection;
  if Conn <> nil then
  begin
    LowerDriver := LowerCase(Conn.FDriverName);
    if Pos('mysql', LowerDriver) > 0 then
      Result := '`';
  end;
end;

function TSQLDataSet.ExecSQL(ExecDirect: Boolean): Integer;
begin
  inherited ExecSQL;
  Result := RowsAffected;
end;

{ TSQLMonitor }

constructor TSQLMonitor.Create(AOwner: TComponent);
begin
  inherited;
  FTraceList := TStringList.Create;
end;

destructor TSQLMonitor.Destroy;
begin
  FTraceList.Free;
  inherited;
end;

initialization
  RegisterClass(TSQLConnection);
  RegisterClass(TSQLDataSet);
  RegisterClass(TSQLMonitor);

end.
