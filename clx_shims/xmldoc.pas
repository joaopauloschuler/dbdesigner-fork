unit XMLDoc;
{$mode delphi}
interface
uses Classes, SysUtils, XMLIntf, laz2_DOM, laz2_XMLRead, laz2_XMLWrite;

// Re-export XMLIntf types
type
  TXMLDocument = TXMLDocumentWrapper;

function NewXMLDocument: IXMLDocument;
function LoadXMLDocument(const FileName: string): IXMLDocument;

implementation

function NewXMLDocument: IXMLDocument;
begin
  Result := XMLIntf.NewXMLDocument;
end;

function LoadXMLDocument(const FileName: string): IXMLDocument;
begin
  Result := XMLIntf.LoadXMLDocument(FileName);
end;

end.
