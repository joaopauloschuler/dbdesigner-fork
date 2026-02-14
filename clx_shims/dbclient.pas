unit DBClient;
{$mode delphi}
interface
uses Classes, DB, BufDataset;

type
  TCustomClientDataSet = class(TBufDataset)
  end;

  TClientDataSet = class(TCustomClientDataSet)
  end;

implementation

initialization
  RegisterClass(TClientDataSet);

end.
