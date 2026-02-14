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
  public
    property ActiveStatements: Integer read FActiveStatements;
  published
    property DriverName: string read FDriverName write FDriverName;
    property GetDriverFunc: string read FGetDriverFunc write FGetDriverFunc;
    property LibraryName: string read FLibraryName write FLibraryName;
    property VendorLib: string read FVendorLib write FVendorLib;
    property TableScope: TTableScopes read FTableScope write FTableScope;
  end;

  // Re-export TSQLQuery with dbExpress extensions
  TSQLQuery = SQLDB.TSQLQuery;

  TSQLDataSet = class(SQLDB.TSQLQuery)
  public
    procedure SetSchemaInfo(SchemaType: Integer; const SchemaObjectName, SchemaPattern: string);
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
