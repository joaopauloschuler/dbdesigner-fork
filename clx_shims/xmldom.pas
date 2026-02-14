unit xmldom;
{$mode delphi}
interface
uses Classes, SysUtils, laz2_DOM;

// Minimal compatibility types for Delphi xmldom
type
  IDOMDocument = TDOMDocument;
  IDOMNode = TDOMNode;
  IDOMNodeList = TDOMNodeList;
  IDOMElement = TDOMElement;
  IDOMImplementation = TDOMImplementation;

implementation
end.
