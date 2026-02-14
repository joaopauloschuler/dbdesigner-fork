
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
  // If we have a provider, try to copy data from its source dataset
  if (FSourceDataSet = nil) and (FProviderName <> '') then
    FSourceDataSet := FindProviderDataSet;

  if (FSourceDataSet <> nil) and (not FSourceDataSet.Active) then
  begin
    try
      FSourceDataSet.Open;
    except
      // If source can't open, proceed without it
      FSourceDataSet := nil;
    end;
  end;

  if FSourceDataSet <> nil then
  begin
    // Copy field definitions from the source dataset
    FieldDefs.Clear;
    FieldDefs.Assign(FSourceDataSet.FieldDefs);
    // Create the in-memory dataset structure
    inherited InternalOpen;
    // Copy data from source to our buffer
    CopyFromDataset(FSourceDataSet);
  end
  else
    inherited InternalOpen;
end;

procedure TCustomClientDataSet.Open;
begin
  // If we have a provider, resolve the source dataset before opening
  if (FSourceDataSet = nil) and (FProviderName <> '') then
    FSourceDataSet := FindProviderDataSet;

  inherited Open;
end;

initialization
  RegisterClass(TClientDataSet);

end.
