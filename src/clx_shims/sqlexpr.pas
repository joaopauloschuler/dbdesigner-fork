
unit SqlExpr;
{$mode delphi}
interface
uses Classes, DB, SQLDB, SysUtils, DBXpress, SQLite3Conn, SQLiteLib, MySQL80Conn, MySQLLib;

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
    constructor Create(AOwner: TComponent); override;
    procedure Open;
    procedure Close; reintroduce;
    procedure ExecuteDirect(const ASQL: string); reintroduce;
    procedure ReleaseIdleTransaction;
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
  protected
    procedure InternalClose; override;
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

constructor TSQLConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // dbExpress connections carry an implicit transaction, SQLDB needs an
  // explicit TSQLTransaction. Own one from the start so that datasets linked
  // to this connection while the .lfm is streamed (Database = SQLConn) or
  // before Open inherit it in TCustomSQLQuery.SetDatabase - otherwise their
  // Transaction stays nil and the first Open raises "Transaction not set".
  Transaction := TSQLTransaction.Create(Self);
end;

procedure TSQLConnection.SetDriverNameEx(const Value: string);
begin
  FDriverName := Value;
  UpdateConnectorType;
end;

procedure TSQLConnection.UpdateConnectorType;
var
  LowerDriver, NewType: string;
begin
  LowerDriver := LowerCase(FDriverName);
  NewType := '';
  if Pos('mysql', LowerDriver) > 0 then
    NewType := 'MySQL 8.0'
  else if Pos('sqlite', LowerDriver) > 0 then
    NewType := 'SQLite3'
  else if Pos('oracle', LowerDriver) > 0 then
    NewType := 'Oracle'
  else if (Pos('mssql', LowerDriver) > 0) or (Pos('microsoft', LowerDriver) > 0) then
    NewType := 'MSSQLServer'
  else if Pos('interbase', LowerDriver) > 0 then
    NewType := 'Firebird'
  else if Pos('firebird', LowerDriver) > 0 then
    NewType := 'Firebird'
  else if (Pos('odbc', LowerDriver) > 0) or (Pos('openodbc', LowerDriver) > 0) then
    NewType := 'ODBC'
  else if Pos('postgre', LowerDriver) > 0 then
    NewType := 'PostgreSQL';
  if NewType <> '' then
  begin
    try
      ConnectorType := NewType;
    except
      // Connector not available (driver unit not linked) - ignore during
      // form loading. Will raise again at Open time if still missing.
    end;
  end;
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

  inherited Open;
end;

procedure TSQLConnection.Close;
begin
  // In SQLDB, closing ends the transaction too
  if (Transaction <> nil) and Transaction.Active then
    Transaction.Rollback;
  Connected := False;
end;

procedure TSQLConnection.ExecuteDirect(const ASQL: string);
begin
  // Ensure we have an active transaction
  if (Transaction <> nil) and (not Transaction.Active) then
    Transaction.StartTransaction;
  inherited ExecuteDirect(ASQL);
  // Auto-commit after DML (Delphi dbExpress auto-commits). CommitRetaining:
  // Commit would CloseDataSets on every dataset of the transaction.
  if (Transaction <> nil) and Transaction.Active then
    Transaction.CommitRetaining;
end;

procedure TSQLConnection.ReleaseIdleTransaction;
var
  i: Integer;
begin
  // dbExpress reads are auto-committed; SQLDB keeps the transaction (and with
  // SQLite its SHARED lock, which blocks every other writer) open until an
  // explicit commit. Once no dataset of this connection is open any more,
  // COMMIT and re-BEGIN (deferred, takes no lock) so that the file is free
  // while the application is connected but idle (sqlite-bug-catalog #13).
  if (Transaction = nil) or (not Transaction.Active) then
    Exit;
  for i := 0 to DataSetCount - 1 do
    if DataSets[i].Active then
      Exit;
  Transaction.CommitRetaining;
end;

{ TSQLDataSet }

procedure TSQLDataSet.InternalClose;
var
  Conn: TSQLConnection;
begin
  inherited InternalClose;
  Conn := GetSQLConnection;
  if (Conn <> nil) and Conn.Connected then
    Conn.ReleaseIdleTransaction;
end;

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
  LowerDriver, QuotedName: string;
begin
  Conn := GetSQLConnection;
  if Conn <> nil then
    LowerDriver := LowerCase(Conn.FDriverName)
  else
    LowerDriver := '';

  if SchemaType = stTables then
  begin
    if Pos('mysql', LowerDriver) > 0 then
      SQL.Text := 'SELECT CAST(NULL AS CHAR) AS RECNO, CAST(NULL AS CHAR) AS CATALOG_NAME, CAST(NULL AS CHAR) AS SCHEMA_NAME, ' +
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
      SQL.Text := 'SELECT NULL AS RECNO, NULL AS CATALOG_NAME, NULL AS SCHEMA_NAME, ' +
                  'TABLE_NAME, TABLE_TYPE FROM INFORMATION_SCHEMA.TABLES ORDER BY TABLE_NAME';
  end
  else if SchemaType = stColumns then
  begin
    // Delphi dbExpress stColumns returns:
    // 0:RECNO 1:CATALOG_NAME 2:SCHEMA_NAME 3:TABLE_NAME 4:COLUMN_NAME
    // 5:COLUMN_POSITION 6:COLUMN_TYPE 7:COLUMN_DATATYPE 8:COLUMN_TYPENAME
    // 9:COLUMN_SUBTYPE 10:COLUMN_LENGTH 11:COLUMN_PRECISION 12:COLUMN_SCALE 13:COLUMN_NULLABLE
    QuotedName := StringReplace(SchemaObjectName, '''', '''''', [rfReplaceAll]);
    if Pos('mysql', LowerDriver) > 0 then
      SQL.Text := 'SELECT CAST(NULL AS CHAR) AS RECNO, CAST(NULL AS CHAR) AS CATALOG_NAME, CAST(NULL AS CHAR) AS SCHEMA_NAME, ' +
                  'TABLE_NAME, COLUMN_NAME, ORDINAL_POSITION AS COLUMN_POSITION, ' +
                  '0 AS COLUMN_TYPE, 0 AS COLUMN_DATATYPE, DATA_TYPE AS COLUMN_TYPENAME, ' +
                  'CAST(NULL AS CHAR) AS COLUMN_SUBTYPE, CHARACTER_MAXIMUM_LENGTH AS COLUMN_LENGTH, ' +
                  'NUMERIC_PRECISION AS COLUMN_PRECISION, NUMERIC_SCALE AS COLUMN_SCALE, ' +
                  'CASE IS_NULLABLE WHEN ''YES'' THEN 1 ELSE 0 END AS COLUMN_NULLABLE ' +
                  'FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME=''' + QuotedName + ''' ' +
                  'AND TABLE_SCHEMA = DATABASE() ORDER BY ORDINAL_POSITION'
    else if Pos('postgre', LowerDriver) > 0 then
      SQL.Text := 'SELECT NULL AS RECNO, NULL AS CATALOG_NAME, NULL AS SCHEMA_NAME, ' +
                  'table_name AS TABLE_NAME, column_name AS COLUMN_NAME, ' +
                  'ordinal_position AS COLUMN_POSITION, 0 AS COLUMN_TYPE, 0 AS COLUMN_DATATYPE, ' +
                  'data_type AS COLUMN_TYPENAME, NULL AS COLUMN_SUBTYPE, ' +
                  'character_maximum_length AS COLUMN_LENGTH, ' +
                  'numeric_precision AS COLUMN_PRECISION, numeric_scale AS COLUMN_SCALE, ' +
                  'CASE is_nullable WHEN ''YES'' THEN 1 ELSE 0 END AS COLUMN_NULLABLE ' +
                  'FROM information_schema.columns WHERE table_name=''' + QuotedName + ''' ' +
                  'ORDER BY ordinal_position'
    else if Pos('sqlite', LowerDriver) > 0 then
      SQL.Text := 'SELECT NULL AS RECNO, NULL AS CATALOG_NAME, NULL AS SCHEMA_NAME, ' +
                  '''' + QuotedName + ''' AS TABLE_NAME, name AS COLUMN_NAME, ' +
                  'cid AS COLUMN_POSITION, 0 AS COLUMN_TYPE, 0 AS COLUMN_DATATYPE, ' +
                  'type AS COLUMN_TYPENAME, NULL AS COLUMN_SUBTYPE, ' +
                  'NULL AS COLUMN_LENGTH, NULL AS COLUMN_PRECISION, NULL AS COLUMN_SCALE, ' +
                  'CASE "notnull" WHEN 1 THEN 0 ELSE 1 END AS COLUMN_NULLABLE ' +
                  'FROM pragma_table_info(''' + QuotedName + ''')'
    else
      SQL.Text := 'SELECT NULL AS RECNO, NULL AS CATALOG_NAME, NULL AS SCHEMA_NAME, ' +
                  'TABLE_NAME, COLUMN_NAME, ORDINAL_POSITION AS COLUMN_POSITION, ' +
                  '0 AS COLUMN_TYPE, 0 AS COLUMN_DATATYPE, DATA_TYPE AS COLUMN_TYPENAME, ' +
                  'NULL AS COLUMN_SUBTYPE, CHARACTER_MAXIMUM_LENGTH AS COLUMN_LENGTH, ' +
                  'NUMERIC_PRECISION AS COLUMN_PRECISION, NUMERIC_SCALE AS COLUMN_SCALE, ' +
                  'CASE IS_NULLABLE WHEN ''YES'' THEN 1 ELSE 0 END AS COLUMN_NULLABLE ' +
                  'FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME=''' + QuotedName + ''' ' +
                  'ORDER BY ORDINAL_POSITION';
  end
  else if SchemaType = stIndexes then
  begin
    // Delphi dbExpress stIndexes returns:
    // 0:RECNO 1:CATALOG_NAME 2:SCHEMA_NAME 3:TABLE_NAME 4:INDEX_NAME
    // 5:COLUMN_NAME 6:COLUMN_POSITION 7:PKEY_NAME 8:INDEX_TYPE 9:SORT_ORDER 10:FILTER
    QuotedName := StringReplace(SchemaObjectName, '''', '''''', [rfReplaceAll]);
    if Pos('mysql', LowerDriver) > 0 then
      SQL.Text := 'SELECT CAST(NULL AS CHAR) AS RECNO, CAST(NULL AS CHAR) AS CATALOG_NAME, CAST(NULL AS CHAR) AS SCHEMA_NAME, ' +
                  'TABLE_NAME, INDEX_NAME, COLUMN_NAME, SEQ_IN_INDEX AS COLUMN_POSITION, ' +
                  'CASE NON_UNIQUE WHEN 0 THEN INDEX_NAME ELSE NULL END AS PKEY_NAME, ' +
                  'INDEX_TYPE, COLLATION AS SORT_ORDER, CAST(NULL AS CHAR) AS FILTER ' +
                  'FROM INFORMATION_SCHEMA.STATISTICS WHERE TABLE_NAME=''' + QuotedName + ''' ' +
                  'AND TABLE_SCHEMA = DATABASE() ORDER BY INDEX_NAME, SEQ_IN_INDEX'
    else if Pos('postgre', LowerDriver) > 0 then
      SQL.Text := 'SELECT NULL AS RECNO, NULL AS CATALOG_NAME, NULL AS SCHEMA_NAME, ' +
                  't.relname AS TABLE_NAME, i.relname AS INDEX_NAME, ' +
                  'a.attname AS COLUMN_NAME, a.attnum AS COLUMN_POSITION, ' +
                  'CASE WHEN ix.indisprimary THEN i.relname ELSE NULL END AS PKEY_NAME, ' +
                  'CASE WHEN ix.indisunique THEN ''UNIQUE'' ELSE ''INDEX'' END AS INDEX_TYPE, ' +
                  '''ASC'' AS SORT_ORDER, NULL AS FILTER ' +
                  'FROM pg_class t JOIN pg_index ix ON t.oid = ix.indrelid ' +
                  'JOIN pg_class i ON i.oid = ix.indexrelid ' +
                  'JOIN pg_attribute a ON a.attrelid = t.oid AND a.attnum = ANY(ix.indkey) ' +
                  'WHERE t.relname = ''' + QuotedName + ''' ' +
                  'ORDER BY i.relname, a.attnum'
    else if Pos('sqlite', LowerDriver) > 0 then
      SQL.Text := 'SELECT NULL AS RECNO, NULL AS CATALOG_NAME, NULL AS SCHEMA_NAME, ' +
                  '''' + QuotedName + ''' AS TABLE_NAME, il.name AS INDEX_NAME, ' +
                  'ii.name AS COLUMN_NAME, ii.seqno AS COLUMN_POSITION, ' +
                  'NULL AS PKEY_NAME, ' +
                  'CASE il."unique" WHEN 1 THEN ''UNIQUE'' ELSE ''INDEX'' END AS INDEX_TYPE, ' +
                  '''ASC'' AS SORT_ORDER, NULL AS FILTER ' +
                  'FROM pragma_index_list(''' + QuotedName + ''') il ' +
                  'JOIN pragma_index_info(il.name) ii ORDER BY il.name, ii.seqno'
    else
      SQL.Text := 'SELECT NULL AS RECNO, NULL AS CATALOG_NAME, NULL AS SCHEMA_NAME, ' +
                  'TABLE_NAME, INDEX_NAME, COLUMN_NAME, ORDINAL_POSITION AS COLUMN_POSITION, ' +
                  'NULL AS PKEY_NAME, NULL AS INDEX_TYPE, NULL AS SORT_ORDER, NULL AS FILTER ' +
                  'FROM INFORMATION_SCHEMA.STATISTICS WHERE TABLE_NAME=''' + QuotedName + ''' ' +
                  'ORDER BY INDEX_NAME, ORDINAL_POSITION';
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
var
  Conn: TSQLConnection;
begin
  inherited ExecSQL;
  Result := RowsAffected;
  // dbExpress auto-commits every statement; SQLDB leaves the DML inside the
  // connection's TSQLTransaction, which nobody commits and Close rolls back
  // (db-ui-bug-catalog #7: "1 Rows affected" but the row never reached the
  // server). CommitRetaining rather than Commit so open datasets (table data
  // editor, client dataset fetch) survive; sqlite then re-BEGINs deferred,
  // so no lock is held while idle (sqlite-bug-catalog #13).
  Conn := GetSQLConnection;
  if (Conn <> nil) and (Conn.Transaction <> nil) and Conn.Transaction.Active then
    Conn.Transaction.CommitRetaining;
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
