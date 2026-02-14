unit Provider;
{$mode delphi}
interface
uses Classes, DB;

type
  TDataSetProvider = class(TComponent)
  private
    FDataSet: TDataSet;
  published
    property DataSet: TDataSet read FDataSet write FDataSet;
  end;

implementation
end.
