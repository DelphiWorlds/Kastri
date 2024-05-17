unit DW.ManifestMerger;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2024 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  Xml.XMLIntf;

type
  TManifestMerger = record
  private
    class function InternalMergeManifest(const AMergeFileName, AManifestFileName: string; const ANeedsXMLFormat: Boolean = True): Integer; static;
  public
    class function MergeManifest(const AMergePath, AManifestFileName: string): Integer; static;
    class function MergeManifestSubNodes(const AManifestParent, AMergeParent: IXmlNode): Integer; static; // ; const ASubNodeParents: array of string): Integer; static;
  end;

implementation

uses
  DW.OSLog,
  System.IOUtils, System.SysUtils, System.StrUtils, System.Variants,
  Xml.XMLDoc,
  DW.XMLHelper;

type
  OleVariantHelper = record helper for OleVariant
  public
    function AsString: string;
  end;

{ OleVariantHelper }

function OleVariantHelper.AsString: string;
begin
  if VarIsNull(Self) then
    Result := ''
  else
    Result := Self;
end;

// e.g.: <manifest xmlns:android="http://schemas.android.com/apk/res/android" package="com.embarcadero.AGDemo"

class function TManifestMerger.InternalMergeManifest(const AMergeFileName, AManifestFileName: string; const ANeedsXMLFormat: Boolean = True): Integer;
var
  LManifest, LMerge: IXMLDocument;
  LXML, LPackageName, LMergeText: string;
begin
  LXML := '';
  LManifest := LoadXMLDocument(AManifestFileName);
  LPackageName := LManifest.DocumentElement.Attributes['package'].AsString;
  LMergeText := TFile.ReadAllText(AMergeFileName);
  LMergeText := LMergeText.Replace('%package%', LPackageName, [rfReplaceAll]);
  LMerge := NewXMLDocument;
  LMerge.XML.Text := LMergeText;
  LMerge.Active := True;
  Result := MergeManifestSubNodes(LManifest.DocumentElement, LMerge.DocumentElement); //, ['manifest']);  // application
  if Result = 0 then
  begin
    // Using FormatXMLData so the result is more readable
    LXML := LManifest.XML.Text;
    if ANeedsXMLFormat then
      LXML := FormatXMLData(LXML);
  end;
  if not LXML.IsEmpty then
    TFile.WriteAllText(AManifestFileName, LXML);
end;

class function TManifestMerger.MergeManifest(const AMergePath, AManifestFileName: string): Integer;
var
  LResultFileName: string;
  LMergeFileNames: TArray<string>;
  I: Integer;
begin
  Result := 1; // Something is missing
  if TFile.Exists(AManifestFileName) and not AMergePath.IsEmpty then
  begin
    if TDirectory.Exists(AMergePath) then
    begin
      LMergeFileNames := TDirectory.GetFiles(AMergePath, '*Manifest.merge.xml', TSearchOption.soTopDirectoryOnly);
      if Length(LMergeFileNames) > 0 then
      begin
        LResultFileName := TPath.ChangeExtension(TPath.GetTempFileName, '.xml');
        TFile.Copy(AManifestFileName, LResultFileName);
        try
          for I := 0 to Length(LMergeFileNames) - 1 do
          begin
            Result := InternalMergeManifest(LMergeFileNames[I], LResultFileName, False);
            if Result <> 0 then
              Break;
          end;
          if Result = 0 then
            TFile.Copy(LResultFileName, AManifestFileName, True);
        finally
          TFile.Delete(LResultFileName);
        end;
      end;
    end
    else if TFile.Exists(AMergePath) then
      Result := InternalMergeManifest(AMergePath, AManifestFileName);
  end;
end;

class function TManifestMerger.MergeManifestSubNodes(const AManifestParent, AMergeParent: IXmlNode): Integer;
var
  I, LIndex, LNodeIndex: Integer;
  LMergeNode, LParentNode, LNode: IXmlNode;
  LMergeAttribute, LKeyAttribute, LKeyAttributeValue: string;
  LParts: TArray<string>;
  LIsDelete: Boolean;
begin
  Result := 0;
  LIndex := 0;
  for I := 0 to AMergeParent.ChildNodes.Count - 1 do
  begin
    LIsDelete := False;
    LMergeNode := AMergeParent.ChildNodes[I];
    LMergeAttribute := LMergeNode.Attributes['merge:replace'].AsString;
    if LMergeAttribute.IsEmpty then
    begin
      LMergeAttribute := LMergeNode.Attributes['merge:delete'].AsString;
      if not LMergeAttribute.IsEmpty then
        LIsDelete := True;
    end;
    if not LMergeAttribute.IsEmpty then
    begin
      // TOSLog.d('Found merge:replace node');
      LParts := LMergeAttribute.Split(['='], 2);
      if Length(LParts) = 2 then
      begin
        LKeyAttribute := LParts[0]; // eg: android:name
        LKeyAttributeValue := AnsiDequotedStr(LParts[1], '"'); // eg: com.embarcadero.firebase.messaging.ProxyFirebaseMessagingService
        LNode := TXMLHelper.FindNodeByAttribute(AManifestParent, LKeyAttribute, LKeyAttributeValue);
        if LNode <> nil then
        begin
          // TOSLog.d('Found node with attribute: %s=%s', [LKeyAttribute, LKeyAttributeValue]);
          if not LIsDelete then
          begin
            TXMLHelper.DeleteAttribute(LMergeNode, 'merge:replace');
            // Replace the existing node with the merge node
            LNodeIndex := AManifestParent.ChildNodes.IndexOf(LNode);
            AManifestParent.ChildNodes.Remove(LNode);
            LNode := LMergeNode.CloneNode(True);
            AManifestParent.ChildNodes.Insert(LNodeIndex, LNode);
          end
          else
            AManifestParent.ChildNodes.Remove(LNode);
          // TOSLog.d('Node is now: %s', [LNode.WriteToString]);
        end
        else
          TOSLog.d('Did not find node with attribute: %s="%s"', [LKeyAttribute, LKeyAttributeValue]);
      end;
    end
    else
    begin
      LMergeAttribute := LMergeNode.Attributes['merge:update'].AsString;
      if LMergeAttribute.IsEmpty or LMergeAttribute.Equals('existing') then
        LParentNode := TXMLHelper.FindMatchingNode(AManifestParent, LMergeNode)
      else if LMergeAttribute.Equals('add') then
        LParentNode := nil
      else
        LParentNode := TXMLHelper.FindMatchingNode(AManifestParent, LMergeAttribute);
      if (LParentNode <> nil) and (LMergeAttribute.Equals('existing') or (TXMLHelper.TreeDepth(LParentNode) = TXMLHelper.TreeDepth(LMergeNode))) then
      begin
        // LParentNode := AManifestParent.FindNode(LMergeNode.Name);
        // if LParentNode <> nil then
        begin
          // Don't replace the node in question, but add any new attributes, and update any existing

          // ..then iterate the children of the merge node
          MergeManifestSubNodes(LParentNode, LMergeNode);
        end;
      end
      else
      begin
        TXMLHelper.DeleteAttribute(LMergeNode, 'merge:update');
        // Otherwise assume the node is being added
        // LNode := AManifestParent.NodeNewAtIndex(LIndex, LMergeNode.Name);
        // LNode.Assign(LMergeNode);
        LNode := LMergeNode.CloneNode(True);
        AManifestParent.ChildNodes.Insert(LIndex, LNode);
        Inc(LIndex);
      end;
    end;
  end;
end;

end.
