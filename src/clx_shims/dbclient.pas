
unit DBClient;
{$mode delphi}
interface
uses Classes, DB, BufDataset, SysUtils;

type
  TCustomClientDataSet = class(TBufDataset)
  private
    FProviderName: string;
    FSourceDataSet: TDataSet;
    procedure SetProviderName(const Value: string);
    function FindProviderDataSet: TDataSet;
  protected
    procedure InternalOpen; override;
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

{ TCustomClientDataSet }

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
