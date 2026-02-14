unit SqlExpr;
{$mode delphi}
interface
uses Classes, DB, SQLDB;

type
  // Stub types - will be replaced with SQLDB equivalents in Phase 2
  TSQLConnection = class(TSQLConnector)
  end;
  TSQLDataSet = class(TSQLQuery)
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

end.
