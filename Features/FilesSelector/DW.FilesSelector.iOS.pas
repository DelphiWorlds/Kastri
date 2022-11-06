unit DW.FilesSelector.iOS;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2021 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // macOS
  Macapi.ObjectiveC, 
  // iOS
  iOSapi.Foundation, iOSapi.UIKit, iOSapi.CocoaTypes,
  // DW
  DW.iOSapi.UIKit, DW.FilesSelector;

type
  TPlatformFilesSelector = class;

  TUIDocumentPickerDelegate = class(TOCLocal, UIDocumentPickerDelegate)
  private
    FController: UIDocumentPickerViewController;
    FSelector: TPlatformFilesSelector;
    procedure DestroyController;
  public
    { UIDocumentPickerDelegate }
    procedure documentPicker(controller: UIDocumentPickerViewController; didPickDocumentsAtURLs: NSArray); overload; cdecl;
    procedure documentPicker(controller: UIDocumentPickerViewController; didPickDocumentAtURL: NSURL); overload; cdecl;
    procedure documentPickerWasCancelled(controller: UIDocumentPickerViewController); cdecl;
  public
    constructor Create(const ASelector: TPlatformFilesSelector);
    destructor Destroy; override;
    procedure ShowPicker;
  end;

  TPlatformFilesSelector = class(TCustomPlatformFilesSelector)
  private
    FPicker: TUIDocumentPickerDelegate;
    FUTIs: TArray<string>;
    function GetUTI(const AFileKind: TFileKind): string;
  protected
    procedure DoSelect(const AMode: TSelectionMode); override;
    procedure FileKindsChanged; override;
    procedure FileTypesChanged; override;
    property UTIs: TArray<string> read FUTIs;
  public
    constructor Create(const ASelector: TFilesSelector); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.IOUtils,
  // macOS
  Macapi.Helpers, 
  // iOS
  iOSapi.Helpers,
  // DW
  DW.Macapi.Helpers;

// https://escapetech.eu/manuals/qdrop/uti.html
// "public.image", "public.audio", "public.movie", "public.text", "public.item", "public.content", "public.source-code"

{ TUIDocumentPickerDelegate }

constructor TUIDocumentPickerDelegate.Create(const ASelector: TPlatformFilesSelector);
begin
  inherited Create;
  FSelector := ASelector;
end;

destructor TUIDocumentPickerDelegate.Destroy;
begin
  DestroyController;
  inherited;
end;

procedure TUIDocumentPickerDelegate.DestroyController;
begin
  FController := nil;
end;

procedure TUIDocumentPickerDelegate.ShowPicker;
begin
  DestroyController;
  FController := TUIDocumentPickerViewController.Alloc;
  FController := TUIDocumentPickerViewController.Wrap(FController.initWithDocumentTypes(StringArrayToNSArray(FSelector.UTIs), UIDocumentPickerModeImport));
  FController.setAllowsMultipleSelection(True);
  FController.setDelegate(GetObjectID);
  FController.setTitle(StrToNSStr(FSelector.Title));
  TiOSHelper.SharedApplication.keyWindow.rootViewController.presentViewController(FController, True, nil);
end;

procedure TUIDocumentPickerDelegate.documentPicker(controller: UIDocumentPickerViewController; didPickDocumentAtURL: NSURL);
begin
  //
end;

procedure TUIDocumentPickerDelegate.documentPicker(controller: UIDocumentPickerViewController; didPickDocumentsAtURLs: NSArray);
var
  I: Integer;
  LEscapedFileName: NSString;
  LSelectedFile: TSelectedFile;
begin
  for I := 0 to didPickDocumentsAtURLs.count - 1 do
  begin
    LEscapedFileName := TNSURL.Wrap(didPickDocumentsAtURLs.objectAtIndex(I)).path.stringByReplacingPercentEscapesUsingEncoding(NSUTF8StringEncoding);
    LSelectedFile.DecodedPath := NSStrToStr(LEscapedFileName);
    LSelectedFile.RawPath := NSStrToStr(TNSURL.Wrap(didPickDocumentsAtURLs.objectAtIndex(I)).path);
    LSelectedFile.DisplayName := TPath.GetFileName(LSelectedFile.DecodedPath);
    FSelector.AddSelectedFile(LSelectedFile);
  end;
  FSelector.DoComplete(True);
end;

procedure TUIDocumentPickerDelegate.documentPickerWasCancelled(controller: UIDocumentPickerViewController);
begin
  FSelector.DoComplete(False);
end;

{ TPlatformFilesSelector }

constructor TPlatformFilesSelector.Create(const ASelector: TFilesSelector);
begin
  inherited;
  FPicker := TUIDocumentPickerDelegate.Create(Self);
  FileKindsChanged;
end;

destructor TPlatformFilesSelector.Destroy;
begin
  FPicker.Free;
  inherited;
end;

procedure TPlatformFilesSelector.DoSelect(const AMode: TSelectionMode);
begin
  FPicker.ShowPicker;
end;

procedure TPlatformFilesSelector.FileKindsChanged;
var
  LFileKind: TFileKind;
begin
  FUTIs := [];
  for LFileKind := Low(TFileKind) to High(TFileKind) do
  begin
    if (LFileKind in FileKinds) or (FileKinds = []) then
      FUTIs := FUTIs + [GetUTI(LFileKind)];
  end;
end;

procedure TPlatformFilesSelector.FileTypesChanged;
begin
  FUTIs := FFileTypes.ToStringArray;
end;

function TPlatformFilesSelector.GetUTI(const AFileKind: TFileKind): string;
begin
  case AFileKind of
    TFileKind.Image:
      Result := 'public.image';
    TFileKind.Audio:
      Result := 'public.audio';
    TFileKind.Movie:
      Result := 'public.movie';
    TFileKind.Text:
      Result := 'public.text';
    TFileKind.Item:
      Result := 'public.item';
    TFileKind.Content:
      Result := 'public.content';
    TFileKind.SourceCode:
      Result := 'public.source-code';
    TFileKind.PDF:
      Result := 'com.adobe.pdf';
  else
    Result := 'public.item';
  end;
end;

end.
