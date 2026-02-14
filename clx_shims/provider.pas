unit Provider;
{$mode delphi}
interface
uses Classes, DB;

type
  TResolverResponse = (rrSkip, rrAbort, rrMerge, rrApply, rrIgnore);

  EUpdateError = class(EDatabaseError)
  end;

  TUpdateErrorEvent = procedure(Sender: TObject; DataSet: TDataSet;
    E: EUpdateError; UpdateKind: Integer; var Response: TResolverResponse) of object;

  TDataSetProvider = class(TComponent)
  private
    FDataSet: TDataSet;
    FOnUpdateError: TUpdateErrorEvent;
  published
    property DataSet: TDataSet read FDataSet write FDataSet;
    property OnUpdateError: TUpdateErrorEvent read FOnUpdateError write FOnUpdateError;
  end;

implementation

initialization
  RegisterClass(TDataSetProvider);

end.
