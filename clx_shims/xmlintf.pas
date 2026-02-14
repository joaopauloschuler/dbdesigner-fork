unit XMLIntf;
{$mode delphi}
interface
uses Classes, SysUtils, Variants, laz2_DOM, laz2_XMLRead, laz2_XMLWrite;

type
  // Forward declarations  
  IXMLNode = interface;
  IXMLNodeList = interface;
  IXMLDocument = interface;

  IXMLNode = interface
    ['{E8E6E801-0000-0001-0000-000000000001}']
    function GetNodeName: DOMString;
    function GetNodeValue: Variant;
    procedure SetNodeValue(const Value: Variant);
    function GetText: DOMString;
    procedure SetText(const Value: DOMString);
    function GetChildNodes: IXMLNodeList;
    function GetAttributes(const Name: DOMString): DOMString;
    procedure SetAttributes(const Name: DOMString; const Value: DOMString);
    function AddChild(const TagName: DOMString): IXMLNode;
    function GetDOMNode: TDOMNode;
    function GetOwnerDocument: IXMLDocument;
    property NodeName: DOMString read GetNodeName;
    property NodeValue: Variant read GetNodeValue write SetNodeValue;
    property OwnerDocument: IXMLDocument read GetOwnerDocument;
    property Text: DOMString read GetText write SetText;
    property ChildNodes: IXMLNodeList read GetChildNodes;
    property Attributes[const Name: DOMString]: DOMString read GetAttributes write SetAttributes; default;
  end;

  IXMLNodeList = interface
    ['{E8E6E801-0000-0002-0000-000000000002}']
    function GetCount: Integer;
    function GetNode(Index: Integer): IXMLNode;
    property Count: Integer read GetCount;
    property Nodes[Index: Integer]: IXMLNode read GetNode; default;
  end;

  IXMLDocument = interface
    ['{E8E6E801-0000-0003-0000-000000000003}']
    function GetDocumentElement: IXMLNode;
    function GetDOMDocument: TDOMDocument;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    procedure SaveToXml(var XML: string);
    function CreateElement(const TagName: DOMString): IXMLNode;
    function GetDocBinding(const TagName: DOMString; AClass: TClass; const NS: DOMString): IXMLNode;
    property DocumentElement: IXMLNode read GetDocumentElement;
  end;

  // Delphi XML Data Binding compatibility
  IXMLNodeCollection = interface(IXMLNodeList)
    ['{E8E6E801-0000-0004-0000-000000000004}']
  end;

  // Base class for XML data binding generated classes
  TXMLNodeIndexed = class;

  TXMLNode = class(TInterfacedObject, IXMLNode)
  private
    FNode: TDOMNode;
    FOwnerDoc: TDOMDocument;
    FChildNodeDefs: TStringList;
    FChildNodesIndexed: TXMLNodeIndexed;
    FAttributeNodesIndexed: TXMLNodeIndexed;
  public
    constructor Create(ANode: TDOMNode; AOwnerDoc: TDOMDocument);
    destructor Destroy; override;
    procedure AfterConstruction; override;
    function GetNodeName: DOMString;
    function GetNodeValue: Variant;
    procedure SetNodeValue(const Value: Variant);
    function GetText: DOMString;
    procedure SetText(const Value: DOMString);
    function GetChildNodes: IXMLNodeList;
    function GetAttributes(const Name: DOMString): DOMString;
    procedure SetAttributes(const Name: DOMString; const Value: DOMString);
    function AddChild(const TagName: DOMString): IXMLNode;
    function GetDOMNode: TDOMNode;
    function GetOwnerDocument: IXMLDocument;
    // Delphi XML Data Binding support
    procedure RegisterChildNode(const TagName: DOMString; ChildNodeClass: TClass);
    procedure SetAttribute(const AttrName: DOMString; const Value: DOMString); overload;
    procedure SetAttribute(const AttrName: DOMString; Value: Integer); overload;
    property ChildNodes: TXMLNodeIndexed read FChildNodesIndexed;
    property AttributeNodes: TXMLNodeIndexed read FAttributeNodesIndexed;
  end;

  // Helper class for indexed access by name - ChildNodes['name'] / AttributeNodes['name']
  TXMLNodeIndexed = class
  private
    FOwner: TXMLNode;
    FIsAttribute: Boolean;
    function GetNodeByName(const Name: DOMString): IXMLNode;
  public
    constructor Create(AOwner: TXMLNode; IsAttribute: Boolean);
    property Items[const Name: DOMString]: IXMLNode read GetNodeByName; default;
  end;

  TXMLNodeCollection = class(TXMLNode, IXMLNodeCollection, IXMLNodeList)
  private
    FItemTag: DOMString;
    FItemInterface: TGUID;
    function GetList(Index: Integer): IXMLNode;
  public
    function GetCount: Integer;
    function GetNode(Index: Integer): IXMLNode;
    function AddItem(Index: Integer): IXMLNode;
    property ItemTag: DOMString read FItemTag write FItemTag;
    property ItemInterface: TGUID read FItemInterface write FItemInterface;
    property List[Index: Integer]: IXMLNode read GetList;
  end;

  // Concrete implementations
  TXMLNodeWrapper = class(TInterfacedObject, IXMLNode)
  public
    function GetOwnerDocument: IXMLDocument;
  private
    FNode: TDOMNode;
    FOwnerDoc: TDOMDocument;
  public
    constructor Create(ANode: TDOMNode; AOwnerDoc: TDOMDocument);
    function GetNodeName: DOMString;
    function GetNodeValue: Variant;
    procedure SetNodeValue(const Value: Variant);
    function GetText: DOMString;
    procedure SetText(const Value: DOMString);
    function GetChildNodes: IXMLNodeList;
    function GetAttributes(const Name: DOMString): DOMString;
    procedure SetAttributes(const Name: DOMString; const Value: DOMString);
    function AddChild(const TagName: DOMString): IXMLNode;
    function GetDOMNode: TDOMNode;
  end;

  TXMLNodeListWrapper = class(TInterfacedObject, IXMLNodeList)
  private
    FNodeList: TDOMNodeList;
    FOwnerDoc: TDOMDocument;
  public
    constructor Create(ANodeList: TDOMNodeList; AOwnerDoc: TDOMDocument);
    function GetCount: Integer;
    function GetNode(Index: Integer): IXMLNode;
  end;

  TXMLDocumentWrapper = class(TInterfacedObject, IXMLDocument)
  private
    FDoc: TDOMDocument;
    FOwnsDoc: Boolean;
  public
    constructor Create; overload;
    constructor CreateFromDoc(ADoc: TDOMDocument; AOwns: Boolean = True);
    destructor Destroy; override;
    function GetDocumentElement: IXMLNode;
    function GetDOMDocument: TDOMDocument;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    procedure SaveToXml(var XML: string);
    function CreateElement(const TagName: DOMString): IXMLNode;
    function GetDocBinding(const TagName: DOMString; AClass: TClass; const NS: DOMString): IXMLNode;
  end;

function NewXMLDocument: IXMLDocument;
function LoadXMLDocument(const FileName: string): IXMLDocument;
function LoadXMLData(const XMLData: string): IXMLDocument;

implementation

{ TXMLNodeWrapper }

constructor TXMLNodeWrapper.Create(ANode: TDOMNode; AOwnerDoc: TDOMDocument);
begin
  inherited Create;
  FNode := ANode;
  FOwnerDoc := AOwnerDoc;
end;

function TXMLNodeWrapper.GetNodeName: DOMString;
begin
  Result := FNode.NodeName;
end;

function TXMLNodeWrapper.GetNodeValue: Variant;
begin
  Result := String(FNode.NodeValue);
end;

procedure TXMLNodeWrapper.SetNodeValue(const Value: Variant);
begin
  FNode.NodeValue := DOMString(VarToStr(Value));
end;

function TXMLNodeWrapper.GetText: DOMString;
begin
  Result := FNode.TextContent;
end;

procedure TXMLNodeWrapper.SetText(const Value: DOMString);
begin
  FNode.TextContent := Value;
end;

function TXMLNodeWrapper.GetChildNodes: IXMLNodeList;
begin
  Result := TXMLNodeListWrapper.Create(FNode.ChildNodes, FOwnerDoc);
end;

function TXMLNodeWrapper.GetAttributes(const Name: DOMString): DOMString;
begin
  if FNode is TDOMElement then
    Result := TDOMElement(FNode).GetAttribute(Name)
  else
    Result := '';
end;

procedure TXMLNodeWrapper.SetAttributes(const Name: DOMString; const Value: DOMString);
begin
  if FNode is TDOMElement then
    TDOMElement(FNode).SetAttribute(Name, Value);
end;

function TXMLNodeWrapper.AddChild(const TagName: DOMString): IXMLNode;
var
  NewNode: TDOMElement;
begin
  NewNode := FOwnerDoc.CreateElement(TagName);
  FNode.AppendChild(NewNode);
  Result := TXMLNodeWrapper.Create(NewNode, FOwnerDoc);
end;

function TXMLNodeWrapper.GetDOMNode: TDOMNode;
begin
  Result := FNode;
end;

function TXMLNodeWrapper.GetOwnerDocument: IXMLDocument;
begin
  Result := TXMLDocumentWrapper.CreateFromDoc(FOwnerDoc, False);
end;

{ TXMLNodeListWrapper }

constructor TXMLNodeListWrapper.Create(ANodeList: TDOMNodeList; AOwnerDoc: TDOMDocument);
begin
  inherited Create;
  FNodeList := ANodeList;
  FOwnerDoc := AOwnerDoc;
end;

function TXMLNodeListWrapper.GetCount: Integer;
begin
  Result := FNodeList.Count;
end;

function TXMLNodeListWrapper.GetNode(Index: Integer): IXMLNode;
begin
  Result := TXMLNodeWrapper.Create(FNodeList[Index], FOwnerDoc);
end;

{ TXMLNodeIndexed }

constructor TXMLNodeIndexed.Create(AOwner: TXMLNode; IsAttribute: Boolean);
begin
  inherited Create;
  FOwner := AOwner;
  FIsAttribute := IsAttribute;
end;

function TXMLNodeIndexed.GetNodeByName(const Name: DOMString): IXMLNode;
var
  Child: TDOMNode;
begin
  if FIsAttribute then
  begin
    // Return a wrapper around the attribute
    if (FOwner.FNode is TDOMElement) then
    begin
      Child := TDOMElement(FOwner.FNode).GetAttributeNode(Name);
      if Child <> nil then
        Result := TXMLNodeWrapper.Create(Child, FOwner.FOwnerDoc)
      else
      begin
        // Create attribute if not exists
        TDOMElement(FOwner.FNode).SetAttribute(Name, '');
        Child := TDOMElement(FOwner.FNode).GetAttributeNode(Name);
        Result := TXMLNodeWrapper.Create(Child, FOwner.FOwnerDoc);
      end;
    end
    else
      Result := nil;
  end
  else
  begin
    // Find first child element with this name
    Child := FOwner.FNode.FindNode(Name);
    if Child <> nil then
      Result := TXMLNodeWrapper.Create(Child, FOwner.FOwnerDoc)
    else
    begin
      // Create child if not found
      Child := FOwner.FOwnerDoc.CreateElement(Name);
      FOwner.FNode.AppendChild(Child);
      Result := TXMLNodeWrapper.Create(Child, FOwner.FOwnerDoc);
    end;
  end;
end;

{ TXMLNode }

constructor TXMLNode.Create(ANode: TDOMNode; AOwnerDoc: TDOMDocument);
begin
  inherited Create;
  FNode := ANode;
  FOwnerDoc := AOwnerDoc;
  FChildNodeDefs := TStringList.Create;
  FChildNodesIndexed := TXMLNodeIndexed.Create(Self, False);
  FAttributeNodesIndexed := TXMLNodeIndexed.Create(Self, True);
end;

destructor TXMLNode.Destroy;
begin
  FAttributeNodesIndexed.Free;
  FChildNodesIndexed.Free;
  FChildNodeDefs.Free;
  inherited;
end;

procedure TXMLNode.AfterConstruction;
begin
  inherited;
end;

function TXMLNode.GetNodeName: DOMString;
begin
  Result := FNode.NodeName;
end;

function TXMLNode.GetNodeValue: Variant;
begin
  Result := String(FNode.NodeValue);
end;

procedure TXMLNode.SetNodeValue(const Value: Variant);
begin
  FNode.NodeValue := DOMString(VarToStr(Value));
end;

function TXMLNode.GetText: DOMString;
begin
  Result := FNode.TextContent;
end;

procedure TXMLNode.SetText(const Value: DOMString);
begin
  FNode.TextContent := Value;
end;

function TXMLNode.GetChildNodes: IXMLNodeList;
begin
  Result := TXMLNodeListWrapper.Create(FNode.ChildNodes, FOwnerDoc);
end;

function TXMLNode.GetAttributes(const Name: DOMString): DOMString;
begin
  if FNode is TDOMElement then
    Result := TDOMElement(FNode).GetAttribute(Name)
  else
    Result := '';
end;

procedure TXMLNode.SetAttributes(const Name: DOMString; const Value: DOMString);
begin
  if FNode is TDOMElement then
    TDOMElement(FNode).SetAttribute(Name, Value);
end;

function TXMLNode.AddChild(const TagName: DOMString): IXMLNode;
var
  NewElem: TDOMElement;
begin
  NewElem := FOwnerDoc.CreateElement(TagName);
  FNode.AppendChild(NewElem);
  Result := TXMLNodeWrapper.Create(NewElem, FOwnerDoc);
end;

function TXMLNode.GetDOMNode: TDOMNode;
begin
  Result := FNode;
end;

function TXMLNode.GetOwnerDocument: IXMLDocument;
begin
  Result := TXMLDocumentWrapper.CreateFromDoc(FOwnerDoc, False);
end;

procedure TXMLNode.RegisterChildNode(const TagName: DOMString; ChildNodeClass: TClass);
begin
  FChildNodeDefs.Values[TagName] := ChildNodeClass.ClassName;
end;

procedure TXMLNode.SetAttribute(const AttrName: DOMString; const Value: DOMString);
begin
  if FNode is TDOMElement then
    TDOMElement(FNode).SetAttribute(AttrName, Value);
end;

procedure TXMLNode.SetAttribute(const AttrName: DOMString; Value: Integer);
begin
  SetAttribute(AttrName, IntToStr(Value));
end;

{ TXMLNodeCollection }

function TXMLNodeCollection.GetCount: Integer;
var
  i, cnt: Integer;
  child: TDOMNode;
begin
  if FItemTag <> '' then
  begin
    cnt := 0;
    child := FNode.FirstChild;
    while child <> nil do
    begin
      if (child.NodeType = ELEMENT_NODE) and (child.NodeName = FItemTag) then
        Inc(cnt);
      child := child.NextSibling;
    end;
    Result := cnt;
  end
  else
    Result := FNode.ChildNodes.Count;
end;

function TXMLNodeCollection.GetNode(Index: Integer): IXMLNode;
begin
  Result := GetList(Index);
end;

function TXMLNodeCollection.GetList(Index: Integer): IXMLNode;
var
  i, cnt: Integer;
  child: TDOMNode;
begin
  Result := nil;
  if FItemTag <> '' then
  begin
    cnt := 0;
    child := FNode.FirstChild;
    while child <> nil do
    begin
      if (child.NodeType = ELEMENT_NODE) and (child.NodeName = FItemTag) then
      begin
        if cnt = Index then
        begin
          Result := TXMLNodeWrapper.Create(child, FOwnerDoc);
          Exit;
        end;
        Inc(cnt);
      end;
      child := child.NextSibling;
    end;
  end
  else if (Index >= 0) and (Index < FNode.ChildNodes.Count) then
    Result := TXMLNodeWrapper.Create(FNode.ChildNodes[Index], FOwnerDoc);
end;

function TXMLNodeCollection.AddItem(Index: Integer): IXMLNode;
var
  NewElem: TDOMElement;
  RefNode: TDOMNode;
  cnt, i: Integer;
  child: TDOMNode;
begin
  NewElem := FOwnerDoc.CreateElement(FItemTag);
  if Index < 0 then
  begin
    // Append at end
    FNode.AppendChild(NewElem);
  end
  else
  begin
    // Insert at specific position
    cnt := 0;
    child := FNode.FirstChild;
    RefNode := nil;
    while child <> nil do
    begin
      if (child.NodeType = ELEMENT_NODE) and (child.NodeName = FItemTag) then
      begin
        if cnt = Index then
        begin
          RefNode := child;
          Break;
        end;
        Inc(cnt);
      end;
      child := child.NextSibling;
    end;
    if RefNode <> nil then
      FNode.InsertBefore(NewElem, RefNode)
    else
      FNode.AppendChild(NewElem);
  end;
  Result := TXMLNodeWrapper.Create(NewElem, FOwnerDoc);
end;

{ TXMLDocumentWrapper }

constructor TXMLDocumentWrapper.Create;
begin
  inherited Create;
  FDoc := TDOMDocument.Create;
  FOwnsDoc := True;
end;

constructor TXMLDocumentWrapper.CreateFromDoc(ADoc: TDOMDocument; AOwns: Boolean);
begin
  inherited Create;
  FDoc := ADoc;
  FOwnsDoc := AOwns;
end;

destructor TXMLDocumentWrapper.Destroy;
begin
  if FOwnsDoc then
    FDoc.Free;
  inherited;
end;

function TXMLDocumentWrapper.GetDocumentElement: IXMLNode;
begin
  if FDoc.DocumentElement <> nil then
    Result := TXMLNodeWrapper.Create(FDoc.DocumentElement, FDoc)
  else
    Result := nil;
end;

function TXMLDocumentWrapper.GetDOMDocument: TDOMDocument;
begin
  Result := FDoc;
end;

procedure TXMLDocumentWrapper.LoadFromFile(const FileName: string);
var
  XMLDoc: TXMLDocument;
begin
  if FOwnsDoc and (FDoc <> nil) then
    FDoc.Free;
  XMLDoc := nil;
  ReadXMLFile(XMLDoc, FileName);
  FDoc := XMLDoc;
  FOwnsDoc := True;
end;

procedure TXMLDocumentWrapper.SaveToFile(const FileName: string);
begin
  WriteXMLFile(TXMLDocument(FDoc), FileName);
end;

procedure TXMLDocumentWrapper.SaveToXml(var XML: string);
var
  SS: TStringStream;
begin
  SS := TStringStream.Create('');
  try
    WriteXMLFile(TXMLDocument(FDoc), SS);
    XML := SS.DataString;
  finally
    SS.Free;
  end;
end;

function TXMLDocumentWrapper.CreateElement(const TagName: DOMString): IXMLNode;
var
  elem: TDOMElement;
begin
  elem := FDoc.CreateElement(TagName);
  Result := TXMLNodeWrapper.Create(elem, FDoc);
end;

function TXMLDocumentWrapper.GetDocBinding(const TagName: DOMString; AClass: TClass; const NS: DOMString): IXMLNode;
begin
  // Simplified: return document element
  Result := GetDocumentElement;
end;

function NewXMLDocument: IXMLDocument;
begin
  Result := TXMLDocumentWrapper.Create;
end;

function LoadXMLDocument(const FileName: string): IXMLDocument;
begin
  Result := TXMLDocumentWrapper.Create;
  Result.LoadFromFile(FileName);
end;

function LoadXMLData(const XMLData: string): IXMLDocument;
var
  SS: TStringStream;
  Doc: TXMLDocument;
begin
  SS := TStringStream.Create(XMLData);
  try
    ReadXMLFile(Doc, SS);
    Result := TXMLDocumentWrapper.CreateFromDoc(Doc, True);
  finally
    SS.Free;
  end;
end;

end.
