unit DW.StartUpCopy;

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

type
  TStartUpCopy = record
  public
    /// <summary>
    ///   Copies files from the app package to the path represented by TPath.GetDocumentsPath, or a subfolder thereof
    /// </summary>
    /// <remarks>
    ///   Guarantees that the app document is the latest that was deployed when the app package was built
    ///   Applies to iOS and Android ONLY
    ///   For iOS, the files originate from the StartUp/Documents folder inside the app package
    ///   For Android, the files originate from the assets/internal folder inside the app package
    /// </remarks>
    class procedure CopyDocuments(const AFileNames: TArray<string>; const AParentFolder: string = ''); static;
  end;

implementation

uses
  {$IF Defined(ANDROID)}
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.Helpers,
  {$ENDIF}
  {$IF Defined(IOS)}
  iOSapi.Foundation,
  {$ENDIF}
  System.IOUtils, System.SysUtils, System.StrUtils;

// TODO: Perhaps overwrite only if the file has changed?
// Unfortunately this means reading all of both files first, and doing a checksum, as the date of the file will be when the app package was built
{$IF Defined(ANDROID)}
class procedure TStartUpCopy.CopyDocuments(const AFileNames: TArray<string>; const AParentFolder: string = '');
var
  LAssetManager: JAssetManager;
  LFileNames: TJavaObjectArray<JString>;
  I, LBytesRead: Integer;
  LSourceFolder, LDestFolder: JString;
  LInputStream: JInputStream;
  LOutputStream: JOutputStream;
  LBuffer: TJavaArray<Byte>;
  LFileName: string;
begin
  if AParentFolder.IsEmpty then
    LSourceFolder := StringToJString('internal')
  else
    LSourceFolder := StringToJString('internal' + PathDelim + AParentFolder);
  LAssetManager := TAndroidHelper.Context.getAssets;
  LFileNames := LAssetManager.list(LSourceFolder);
  if LFileNames <> nil then
  try
    if AParentFolder.IsEmpty then
      LDestFolder := StringToJString(TPath.GetDocumentsPath)
    else
      LDestFolder := StringToJString(TPath.GetDocumentsPath + PathDelim + AParentFolder);
    LBuffer := TJavaArray<Byte>.Create(1024);
    try
      for I := 0 to LFileNames.Length - 1 do
      begin
        LFileName := JStringToString(LFileNames[I]);
        if MatchStr(LFileName, AFileNames) then
        begin
          LInputStream := LAssetManager.open(LSourceFolder.concat(StringToJString(PathDelim + LFileName)));
          try
            LOutputStream := TJFileOutputStream.JavaClass.init(TJFile.JavaClass.init(LDestFolder, LFileNames[I]));
            try
              repeat
                LBytesRead := LInputStream.read(LBuffer);
                if LBytesRead > 0 then
                  LOutputStream.write(LBuffer, 0, LBytesRead);
              until LBytesRead <= 0;
            finally
              LOutputStream.close;
            end;
          finally
            LInputStream.close;
          end;
        end;
      end;
    finally
      LBuffer.Free;
    end;
  finally
    LFileNames.Free;
  end;
end;

{$ELSEIF Defined(IOS)}
class procedure TStartUpCopy.CopyDocuments(const AFileNames: TArray<string>; const AParentFolder: string = '');
var
  LSourcePath, LSourceFileName, LDestPath: string;
  LIndex: Integer;
  LMainBundle: NSBundle;
begin
  LMainBundle := TNSBundle.Wrap(TNSBundle.OCClass.mainBundle);
  LSourcePath := UTF8ToString(LMainBundle.resourcePath.UTF8String) + PathDelim + 'StartUp' + PathDelim + 'Documents';
  if not AParentFolder.IsEmpty then
    LSourcePath := LSourcePath + PathDelim + AParentFolder;
  if TDirectory.Exists(LSourcePath) then
  begin
    if AParentFolder.IsEmpty then
      LDestPath := TPath.GetDocumentsPath
    else
      LDestPath := TPath.GetDocumentsPath + PathDelim + AParentFolder;
    for LSourceFileName in TDirectory.GetFiles(LSourcePath, '*.*', TSearchOption.soTopDirectoryOnly) do
    begin
      LIndex := IndexStr(TPath.GetFileName(LSourceFileName), AFileNames);
      if LIndex > -1 then
        TFile.Copy(LSourceFileName, TPath.Combine(LDestPath, AFileNames[LIndex]), True);
    end;
  end;
end;

{$ELSE}
class procedure TStartUpCopy.CopyDocuments(const AFileNames: TArray<string>; const AParentFolder: string = '');
begin

end;
{$ENDIF}

end.
