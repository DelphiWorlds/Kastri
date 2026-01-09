unit DW.ResourcesMerger;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2026 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

type
  TMergeOptions = record
    IgnoreEmptyIdNodes: Boolean;
  end;

  TResourcesMerger = record
  public
    class var Options: TMergeOptions;
    class function MergeResources(const ASource, ATarget, AOutputFolder: string): Integer; static;
  end;

implementation

uses
  System.IOUtils, System.SysUtils, System.StrUtils,
  Xml.XMLIntf, Xml.XMLDoc,
  DW.XMLHelper;

const
  cErrorCodeNoSource = 1;
  cErrorCodeNoTarget = 2;
  cErrorCodeNoCreateOutputFolder = 3;
  cErrorCodeNoMixFileAndFolder = 4;

  cErrorDescriptions: array[1..4] of string = (
    'No source specified',
    'No target specified',
    'Cannot create output folder',
    'Source and target must be either files, or folders'
  );

function IsXmlResource(const AXmlDoc: IXMLDocument): Boolean;
var
  LRoot: IXMLNode;
begin
  LRoot := AXmlDoc.DocumentElement;
  if LRoot <> nil then
    Result := LRoot.NodeName.ToLower = 'resources'
  else
    Result := False;
end;

function XmlResource(const AXmlFile: string; out AXmlDoc: IXMLDocument): Boolean;
begin
  AXmlDoc := LoadXMLDocument(AXmlFile);
  Result := IsXmlResource(AXmlDoc);
  if not Result then
    AXmlDoc := nil;
end;

// AXmlDoc is the *source*, to be merged wih the target: AFileName
function MergeWithXML(const ATargetFileName, AOutputFileName: string; const ASourceXmlDoc: IXMLDocument): Boolean;
var
  LTargetXmlDoc: IXMLDocument;
  LTargetDocumentNode, LSourceDocumentNode, LSourceAttributeNode, LNewNode, LSourceNode: IXMLNode;
  I: Integer;
  LIsMismatch: Boolean;
begin
  Result := False;
  LTargetXmlDoc := LoadXMLDocument(ATargetFileName);
  if IsXmlResource(LTargetXmlDoc) then
  begin
    LIsMismatch := False;
    LTargetDocumentNode := LTargetXmlDoc.DocumentElement;
    LSourceDocumentNode := ASourceXmlDoc.DocumentElement;
    for I := 0 to LSourceDocumentNode.AttributeNodes.Count - 1 do
    begin
      LSourceAttributeNode := LSourceDocumentNode.AttributeNodes.Nodes[I];
      if not LTargetDocumentNode.HasAttribute(LSourceAttributeNode.NodeName) then
        LTargetDocumentNode.SetAttribute(LSourceAttributeNode.NodeName, LSourceAttributeNode.NodeValue)
      else if LTargetDocumentNode.GetAttribute(LSourceAttributeNode.NodeName) <> LSourceAttributeNode.NodeValue then
      begin
        // raise Exception.CreateFmt(sExceptionDocumentElementAttributesMergeFailed, [AFileName]);
        LIsMismatch := True;
        Break;
      end;
    end;
    if not LIsMismatch then
    begin
      for I := 0 to LSourceDocumentNode.ChildNodes.Count - 1 do
      begin
        LSourceNode := LSourceDocumentNode.ChildNodes[I];
        if not TXMLHelper.NodeExists(LTargetDocumentNode, LSourceNode) then
        begin
          // If it is not an id node, or empty id nodes are to be ignored and the node is not empty...
          if not LSourceNode.NodeName.Equals('id') or (TResourcesMerger.Options.IgnoreEmptyIdNodes and (LSourceNode.ChildNodes.Count > 0)) then
          begin
            // Clone the source node
            LNewNode := LSourceDocumentNode.ChildNodes.Nodes[I].CloneNode(True);
            // Add it to the target
            LTargetDocumentNode.ChildNodes.Add(LNewNode);
          end;
        end;
      end;
      if LTargetXmlDoc.Modified then
        LTargetXmlDoc.SaveToFile(AOutputFileName);
      Result := True;
    end;
  end;
end;

function MergeResourceFiles(const ASourceFileName, ATargetFileName, AOutputFileName: string): Boolean;
var
  LXmlDoc: IXMLDocument;
begin
  if not TFile.Exists(ATargetFileName) then
  begin
    // Target does not exist, just copy it to output
    Result := False;
    if ForceDirectories(TPath.GetDirectoryName(AOutputFileName)) then
    begin
      TFile.Copy(ASourceFileName, AOutputFileName);
      Result := True;
    end;
  end
  else if TPath.GetExtension(ASourceFileName).EndsWith('xml', True) then
    // Otherwise merge with target, and output to specified filename (which could be the same)
    Result := XmlResource(ASourceFileName, LXmlDoc) and MergeWithXML(ATargetFileName, AOutputFileName, LXmlDoc)
  else
    Result := True; // File exists, and is not xml - have overwrite option?
end;

class function TResourcesMerger.MergeResources(const ASource, ATarget, AOutputFolder: string): Integer;
var
  LResourceFileName, LMergeFileName, LOutputFileName: string;
  LStartIndex: Integer;
  LIsSourceFile, LIsTargetFile: Boolean;
begin
  Result := -MaxInt; // Unknown error
  LIsSourceFile := not ASource.IsEmpty and TFile.Exists(ASource);
  LIsTargetFile := not ASource.IsEmpty and TFile.Exists(ATarget);
  if not ASource.IsEmpty and (LIsSourceFile or TDirectory.Exists(ASource)) then
  begin
    if not ATarget.IsEmpty and (LIsTargetFile or TDirectory.Exists(ATarget)) then
    begin
      if AOutputFolder.IsEmpty or TDirectory.Exists(AOutputFolder) or ForceDirectories(AOutputFolder) then
      begin
        if LIsSourceFile and LIsTargetFile then
        begin
          if AOutputFolder.IsEmpty then
            LOutputFileName := ATarget
          else
            LOutputFileName := TPath.Combine(AOutputFolder, TPath.GetFileName(ATarget));
          if MergeResourceFiles(ASource, ATarget, LOutputFileName) then
            Result := 0;
        end
        else if not (LIsSourceFile or LIsTargetFile) then
        begin
          LStartIndex := Length(ASource);
          // Iterate all files in the source
          for LResourceFileName in TDirectory.GetFiles(ASource, '*.*', TSearchOption.soAllDirectories) do
          begin
            // File to merge becomes the target path plus the source file name
            LMergeFileName := TPath.Combine(ATarget, LResourceFileName.Substring(LStartIndex + 1)); // values\values.xml
            // Output in-place if output folder not provided
            if AOutputFolder.IsEmpty then
              LOutputFileName := LMergeFileName
            else
              LOutputFileName := TPath.Combine(AOutputFolder, LResourceFileName.Substring(LStartIndex + 1));
            // Becomes e.g.: Z:\Kastri\Demos\Biometric\Android\Debug\BiometricDemoD11\res\values\values.xml
            MergeResourceFiles(LResourceFileName, LMergeFileName, LOutputFileName);
          end;
          Result := 0;
        end
        else
          Result := 4; // Cannot mix file and folder
     end
      else
        Result := 3; // Could not create output folder
    end
    else
      Result := 2; // No target
  end
  else
    Result := 1; // No source
  if IsConsole and (Result in [1..4]) then
    Writeln(cErrorDescriptions[Result]);
end;

initialization
  // Ignore empty id nodes by default
  TResourcesMerger.Options.IgnoreEmptyIdNodes := True;

end.
