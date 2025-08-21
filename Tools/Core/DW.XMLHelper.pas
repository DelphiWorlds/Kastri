unit DW.XMLHelper;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2025 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  Xml.XmlIntf;

type
  TXMLHelper = record
  private
    class function HasSameAttributes(const ANodeA, ANodeB: IXMLNode): Boolean; static;
  public
    class function FindMatchingNode(const AParentNode: IXMLNode; const AAttribute: string): IXMLNode; overload; static;
    class function FindMatchingNode(const AParentNode, ANode: IXMLNode): IXMLNode; overload; static;
    class function FindNodeByAttribute(const AParentNode: IXMLNode;const AAttributeName, AAttributeValue: string): IXMLNode; static;
    class procedure DeleteAttribute(const ANode: IXMLNode; const AAttributeName: string); static;
    class function NodeExists(const AParent, ANode: IXMLNode): Boolean; static;
    class function TreeDepth(const ANode: IXMLNode): Integer; static;
  end;

implementation

uses
  System.SysUtils;

class procedure TXMLHelper.DeleteAttribute(const ANode: IXMLNode; const AAttributeName: string);
var
  LAttributeNode: IXMLNode;
begin
  LAttributeNode := ANode.AttributeNodes.FindNode(AAttributeName);
  if LAttributeNode <> nil then
    ANode.AttributeNodes.Delete(LAttributeNode.NodeName);
end;

class function TXMLHelper.FindMatchingNode(const AParentNode: IXMLNode; const AAttribute: string): IXMLNode;
var
  LParts: TArray<string>;
begin
  Result := nil;
  LParts := AAttribute.Split(['=']);
  if Length(LParts) > 1 then
    Result := FindNodeByAttribute(AParentNode, LParts[0], LParts[1].DeQuotedString('"'));
end;

class function TXMLHelper.FindMatchingNode(const AParentNode, ANode: IXMLNode): IXMLNode;
var
  I: Integer;
begin
  Result := nil;
  if AParentNode.NodeName.Equals(ANode.NodeName) and HasSameAttributes(AParentNode, ANode) then
    Result := AParentNode
  else
  begin
    for I := 0 to AParentNode.ChildNodes.Count - 1 do
    begin
      Result := FindMatchingNode(AParentNode.ChildNodes.Get(I), ANode);
      if Result <> nil then
        Break;
    end;
  end;
end;

class function TXMLHelper.FindNodeByAttribute(const AParentNode: IXMLNode; const AAttributeName, AAttributeValue: string): IXMLNode;
var
  I: Integer;
  LChildNode, LAttributeNode: IXMLNode;
begin
  Result := nil;
  for I := 0 to AParentNode.ChildNodes.Count - 1 do
  begin
    LChildNode := AParentNode.ChildNodes.Get(I);
    LAttributeNode := LChildNode.AttributeNodes.FindNode(AAttributeName);
    if (LAttributeNode <> nil) and (LAttributeNode.NodeValue = AAttributeValue) then
    begin
      Result := LChildNode;
      Break;
    end;
  end;
end;

class function TXMLHelper.HasSameAttributes(const ANodeA, ANodeB: IXMLNode): Boolean;
var
  I: Integer;
  LAttributeNodeA, LAttributeNodeB: IXMLNode;
begin
  Result := True;
  for I := 0 to ANodeB.AttributeNodes.Count - 1 do
  begin
    LAttributeNodeA := ANodeB.AttributeNodes.Get(I);
    if not LAttributeNodeA.NamespaceURI.ToLower.Equals('merge') then
    begin
      LAttributeNodeB := ANodeA.AttributeNodes.FindNode(LAttributeNodeA.NodeName);
      // Attribute missing, or has an attribute with the same name, but value is different
      if (LAttributeNodeB = nil) or (LAttributeNodeA.NodeValue <> LAttributeNodeB.NodeValue) then
      begin
        Result := False;
        Break;
      end;
    end;
  end;
end;

class function TXMLHelper.NodeExists(const AParent, ANode: IXMLNode): Boolean;
var
  I, J: Integer;
  LChild, LAttributeNode: IXMLNode;
  LNodeAttributeValue, LChildAttributeValue: string;
begin
  Result := False;
  for I := 0 to AParent.ChildNodes.Count - 1 do
  begin
    LChild := AParent.ChildNodes[I];
    if LChild.NodeName.Equals(ANode.NodeName) and (LChild.AttributeNodes.Count > 0) and (ANode.AttributeNodes.Count > 0) then
    begin
      Result := True;
      for J := 0 to LChild.AttributeNodes.Count - 1 do
      begin
        LAttributeNode := LChild.AttributeNodes.Nodes[J];
        if ANode.HasAttribute(LAttributeNode.NodeName) then
        begin
          LNodeAttributeValue := ANode.GetAttribute(LAttributeNode.NodeName);
          LChildAttributeValue := LChild.GetAttribute(LAttributeNode.NodeName);
          if ANode.GetAttribute(LAttributeNode.NodeName) <> LChild.GetAttribute(LAttributeNode.NodeName) then
          begin
            Result := False;
            Break;
          end;
        end;
      end;
      if Result then
        Break;
    end;
  end;
end;

class function TXMLHelper.TreeDepth(const ANode: IXMLNode): Integer;
var
  LParent: IXMLNode;
begin
  Result := 0;
  LParent := ANode.ParentNode;
  while LParent <> nil do
  begin
    Inc(Result);
    LParent := LParent.ParentNode;
  end;
end;

end.
