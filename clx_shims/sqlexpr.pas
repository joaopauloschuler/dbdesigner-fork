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

  // Re-export TSQLQuery with dbExpress extensions
  TSQLQuery = SQLDB.TSQLQuery;

  TSQLDataSet = class(SQLDB.TSQLQuery)
  public
    procedure SetSchemaInfo(SchemaType: Integer; const SchemaObjectName, SchemaPattern: string); reintroduce;
    function ExecSQL(ExecDirect: Boolean): Integer; overload;
    function GetQuoteChar: string;
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
  else if Pos('mssql', LowerDriver) > 0 then
    ConnectorType := 'MSSQLServer'
  else if Pos('interbase', LowerDriver) > 0 then
    ConnectorType := 'Firebird'
  else if Pos('firebird', LowerDriver) > 0 then
    ConnectorType := 'Firebird'
  else if Pos('odbc', LowerDriver) > 0 then
    ConnectorType := 'ODBC';
end;

procedure TSQLConnection.Open;
begin
  // Ensure ConnectorType is set before connecting
  if ConnectorType = '' then
    UpdateConnectorType;
  // Ensure a transaction is available (SQLDB requires one)
  if Transaction = nil then
  begin
    Transaction := TSQLTransaction.Create(Self);
    Transaction.DataBase := Self;
  end;
  inherited;
end;

{ TSQLDataSet }

procedure TSQLDataSet.SetSchemaInfo(SchemaType: Integer; const SchemaObjectName, SchemaPattern: string);
begin
  // No-op stub - dbExpress schema info not directly applicable to SQLDB
  // The caller typically sets SQL.Text after this call anyway
end;

function TSQLDataSet.GetQuoteChar: string;
begin
  // Return the identifier quote character for the database
  // Most databases use '"', MySQL uses '`'
  if Assigned(Database) and (Database is TSQLConnection) then
    Result := '"'
  else
    Result := '"';
end;

function TSQLDataSet.ExecSQL(ExecDirect: Boolean): Integer;
begin
  inherited ExecSQL;
  Result := RowsAffected;
end;

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
