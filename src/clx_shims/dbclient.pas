
unit DBClient;
{$mode delphi}
interface
uses Classes, DB, BufDataset, SysUtils, Variants, SQLDB;

type
  TCustomClientDataSet = class(TBufDataset)
  private
    FProviderName: string;
    FSourceDataSet: TDataSet;
    FTableName: string;
    procedure SetProviderName(const Value: string);
    function FindProviderDataSet: TDataSet;
    procedure CopyProviderFlags;
  protected
    procedure InternalOpen; override;
    // Delphi's TDataSetProvider resolved the change log of the client dataset
    // into UPDATE/INSERT/DELETE statements on the provider's connection;
    // TBufDataset itself only raises "ApplyRecUpdate not supported"
    // (model-edit-bug-catalog #43). Done here with the table name and the
    // primary-key ProviderFlags that the source TSQLQuery worked out on Open.
    procedure ApplyRecUpdate(UpdateKind: DB.TUpdateKind); override;
  public
    procedure Open; reintroduce;
    property ProviderName: string read FProviderName write SetProviderName;
  end;

  TClientDataSet = class(TCustomClientDataSet)
  published
    property ProviderName;
    property ReadOnly;
  end;

implementation

uses Provider;

type
  // Access to the protected TableName of the source query.
  TSQLQueryAccess = class(TCustomSQLQuery);

{ TCustomClientDataSet }

procedure TCustomClientDataSet.CopyProviderFlags;
var
  i: Integer;
  SrcField: TField;
begin
  FTableName := '';
  if not (FSourceDataSet is TCustomSQLQuery) then
    Exit;
  FTableName := TSQLQueryAccess(FSourceDataSet).TableName;
  // pfInKey is set by TSQLQuery (UsePrimaryKeyAsKey) on the fields of the
  // table's primary key; CopyFromDataset only copies the FieldDefs.
  for i := 0 to Fields.Count - 1 do
  begin
    SrcField := FSourceDataSet.FindField(Fields[i].FieldName);
    if SrcField <> nil then
      Fields[i].ProviderFlags := SrcField.ProviderFlags;
  end;
end;

procedure TCustomClientDataSet.ApplyRecUpdate(UpdateKind: DB.TUpdateKind);
var
  Src: TCustomSQLQuery;
  Conn: TSQLConnection;
  Qry: TSQLQuery;
  i: Integer;
  F: TField;
  QuotedName, SetPart, WherePart, ColPart, ValPart, SQL: string;

  procedure AddWhere(AField: TField);
  begin
    if not (pfInKey in AField.ProviderFlags) then
      Exit;
    if WherePart <> '' then
      WherePart := WherePart + ' and ';
    QuotedName := Conn.FieldNameQuoteChars[0] + AField.FieldName + Conn.FieldNameQuoteChars[1];
    if VarIsNull(AField.OldValue) then
      WherePart := WherePart + '(' + QuotedName + ' is null)'
    else
      WherePart := WherePart + '(' + QuotedName + '=:"OLD_' + AField.FieldName + '")';
  end;

begin
  if not (FSourceDataSet is TCustomSQLQuery) then
    raise EDatabaseError.Create('Cannot apply changes: the result is not bound to a SQL query.');
  Src := TCustomSQLQuery(FSourceDataSet);
  if (FTableName = '') or not (Src.DataBase is TSQLConnection) then
    raise EDatabaseError.Create('Cannot apply changes: the statement is not a single-table SELECT.');
  Conn := TSQLConnection(Src.DataBase);

  SetPart := ''; WherePart := ''; ColPart := ''; ValPart := '';
  for i := 0 to Fields.Count - 1 do
  begin
    F := Fields[i];
    if F.FieldKind <> fkData then
      Continue;
    QuotedName := Conn.FieldNameQuoteChars[0] + F.FieldName + Conn.FieldNameQuoteChars[1];
    case UpdateKind of
      DB.ukModify:
        begin
          AddWhere(F);
          if (pfInUpdate in F.ProviderFlags) and not F.ReadOnly then
            SetPart := SetPart + QuotedName + '=:"' + F.FieldName + '",';
        end;
      DB.ukInsert:
        if (pfInUpdate in F.ProviderFlags) and not F.ReadOnly and not F.IsNull then
        begin
          ColPart := ColPart + QuotedName + ',';
          ValPart := ValPart + ':"' + F.FieldName + '",';
        end;
      DB.ukDelete:
        AddWhere(F);
    end;
  end;

  case UpdateKind of
    DB.ukModify:
      begin
        if SetPart = '' then
          raise EDatabaseError.Create('Cannot apply changes: no updatable fields.');
        if WherePart = '' then
          raise EDatabaseError.Create('Cannot apply changes: the table ' + FTableName + ' has no primary key in the result.');
        SQL := 'update ' + FTableName + ' set ' + Copy(SetPart, 1, Length(SetPart) - 1) + ' where ' + WherePart;
      end;
    DB.ukInsert:
      begin
        if ColPart = '' then
          raise EDatabaseError.Create('Cannot apply changes: the new record has no values.');
        SQL := 'insert into ' + FTableName + ' (' + Copy(ColPart, 1, Length(ColPart) - 1) + ') values (' +
          Copy(ValPart, 1, Length(ValPart) - 1) + ')';
      end;
    else
      begin
        if WherePart = '' then
          raise EDatabaseError.Create('Cannot apply changes: the table ' + FTableName + ' has no primary key in the result.');
        SQL := 'delete from ' + FTableName + ' where ' + WherePart;
      end;
  end;

  Qry := TSQLQuery.Create(nil);
  try
    Qry.DataBase := Conn;
    Qry.Transaction := Src.Transaction;
    Qry.ParseSQL := False;
    Qry.SQL.Text := SQL;
    for i := 0 to Qry.Params.Count - 1 do
    begin
      if Copy(Qry.Params[i].Name, 1, 4) = 'OLD_' then
      begin
        F := FieldByName(Copy(Qry.Params[i].Name, 5, MaxInt));
        Qry.Params[i].AssignFieldValue(F, F.OldValue);
      end
      else
      begin
        F := FieldByName(Qry.Params[i].Name);
        Qry.Params[i].AssignFieldValue(F, F.Value);
      end;
    end;
    if (Qry.Transaction <> nil) and not TSQLTransaction(Qry.Transaction).Active then
      TSQLTransaction(Qry.Transaction).StartTransaction;
    Qry.ExecSQL;
    // dbExpress auto-committed; SQLDB keeps the statement in the transaction
    // (see the sqlexpr shim's ExecSQL / ExecuteDirect).
    if (Qry.Transaction <> nil) and TSQLTransaction(Qry.Transaction).Active then
      TSQLTransaction(Qry.Transaction).CommitRetaining;
  finally
    Qry.Free;
  end;
end;

procedure TCustomClientDataSet.SetProviderName(const Value: string);
begin
  FProviderName := Value;
  FSourceDataSet := nil; // Reset cached reference
end;

function TCustomClientDataSet.FindProviderDataSet: TDataSet;
var
  i: Integer;
  Comp: TComponent;
begin
  Result := nil;
  if (FProviderName = '') or (Owner = nil) then
    Exit;

  // Find the TDataSetProvider by name in our owner
  for i := 0 to Owner.ComponentCount - 1 do
  begin
    Comp := Owner.Components[i];
    if (Comp is TDataSetProvider) and (SameText(Comp.Name, FProviderName)) then
    begin
      Result := TDataSetProvider(Comp).DataSet;
      Exit;
    end;
  end;
end;

procedure TCustomClientDataSet.InternalOpen;
begin
  // The buffer is prepared by Open (CreateDataset via CopyFromDataset);
  // TBufDataset.InternalOpen itself refuses to open without fields.
  inherited InternalOpen;
end;

procedure TCustomClientDataSet.Open;
var
  WasReadOnly, SourceWasActive: Boolean;
begin
  if Active then
    Exit;

  // If we have a provider, resolve the source dataset before opening
  if (FSourceDataSet = nil) and (FProviderName <> '') then
    FSourceDataSet := FindProviderDataSet;

  if FSourceDataSet = nil then
  begin
    inherited Open;
    Exit;
  end;

  // Open the provider's dataset; its exception (SQL error, no connection,
  // ...) must reach the caller - silently opening an empty buffer would only
  // give TBufDataset's "Missing (compatible) underlying dataset".
  SourceWasActive := FSourceDataSet.Active;
  if not SourceWasActive then
    FSourceDataSet.Open;

  // CopyFromDataset builds the FieldDefs from the source fields, calls
  // CreateDataset, opens the buffer and appends every source row (which needs
  // the buffer to be writable); afterwards the cursor sits on the last row.
  WasReadOnly := ReadOnly;
  ReadOnly := False;
  try
    CopyFromDataset(FSourceDataSet, True);
    // The Append/Post of every copied row went into the change log; without
    // the merge each fetched row is a pending insert and ApplyUpdates would
    // INSERT the whole result again ("Duplicate entry ... for key PRIMARY").
    MergeChangeLog;
    CopyProviderFlags;
  finally
    ReadOnly := WasReadOnly;
    // Like Delphi's TDataSetProvider: a source dataset the provider opened
    // for the fetch is closed again afterwards (the rows live in this
    // buffer), so no statement/lock stays behind (sqlite-bug-catalog #13).
    if not SourceWasActive then
      FSourceDataSet.Close;
  end;
  First;
end;

initialization
  RegisterClass(TClientDataSet);

end.
