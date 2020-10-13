unit DW.FilesSelector.iOS;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{    Copyright 2020 Dave Nottage under MIT license      }
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
  DW.FilesSelector;

const
  UIDocumentPickerModeImport = 0;
  UIDocumentPickerModeOpen = 1;
  UIDocumentPickerModeExportToService = 2;
  UIDocumentPickerModeMoveToService = 3;

type
  UIDocumentPickerMode = NSInteger;

  UIDocumentPickerViewController = interface;

  UIDocumentPickerDelegate = interface(IObjectiveC)
    ['{A67376E3-219D-4A29-9894-66806636368D}']
    [MethodName('documentPicker:didPickDocumentsAtURLs:')]
    procedure documentPickerDidPickDocumentsAtURLs(controller: UIDocumentPickerViewController; urls: NSArray); cdecl;
    [MethodName('documentPicker:didPickDocumentAtURL:')]
    procedure documentPickerDidPickDocumentAtURL(controller: UIDocumentPickerViewController; url: NSURL); cdecl;
    procedure documentPickerWasCancelled(controller: UIDocumentPickerViewController); cdecl;
  end;

  UIDocumentPickerViewControllerClass = interface(UIViewControllerClass)
    ['{8DFC305A-759F-4C08-914A-AC449E089E7C}']
  end;

  UIDocumentPickerViewController = interface(UIViewController)
    ['{36D767A1-60AD-4071-839B-7D1E7345B2D9}']
    function allowsMultipleSelection: Boolean; cdecl;
    function delegate: Pointer; cdecl;
    function documentPickerMode: UIDocumentPickerMode; cdecl;
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    [MethodName('initWithDocumentTypes:inMode:')]
    function initWithDocumentTypes(allowedUTIs: NSArray; mode: UIDocumentPickerMode): Pointer; cdecl;
    [MethodName('initWithURL:inMode:')]
    function initWithURL(url: NSURL; mode: UIDocumentPickerMode): Pointer; cdecl;
    [MethodName('initWithURLs:inMode:')]
    function initWithURLs(urls: NSArray; mode: UIDocumentPickerMode): Pointer; cdecl;
    procedure setAllowsMultipleSelection(allowsMultipleSelection: Boolean); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TUIDocumentPickerViewController = class(TOCGenericImport<UIDocumentPickerViewControllerClass, UIDocumentPickerViewController>) end;

  TPlatformFilesSelector = class;

  TUIDocumentPickerDelegate = class(TOCLocal, UIDocumentPickerDelegate)
  private
    FController: UIDocumentPickerViewController;
    FSelector: TPlatformFilesSelector;
    procedure DestroyController;
  public
    { UIDocumentPickerDelegate }
    [MethodName('documentPicker:didPickDocumentsAtURLs:')]
    procedure documentPickerDidPickDocumentsAtURLs(controller: UIDocumentPickerViewController; urls: NSArray); cdecl;
    [MethodName('documentPicker:didPickDocumentAtURL:')]
    procedure documentPickerDidPickDocumentAtURL(controller: UIDocumentPickerViewController; url: NSURL); cdecl;
    procedure documentPickerWasCancelled(controller: UIDocumentPickerViewController); cdecl;
  public
    constructor Create(const ASelector: TPlatformFilesSelector);
    destructor Destroy; override;
    procedure ShowPicker;
  end;

  TPlatformFilesSelector = class(TCustomPlatformFilesSelector)
  private
    FPicker: TUIDocumentPickerDelegate;
  protected
    procedure DoSelect; override;
  public
    constructor Create(const ASelector: TFilesSelector); override;
    destructor Destroy; override;
  end;

implementation

uses
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
  FSelector.FileTypes.Clear;
  FSelector.FileTypes.Add('public.image');
  FSelector.FileTypes.Add('public.audio');
  FSelector.FileTypes.Add('public.movie');
  FSelector.FileTypes.Add('public.text');
  FSelector.FileTypes.Add('public.item');
  FSelector.FileTypes.Add('public.content');
  FSelector.FileTypes.Add('public.source-code');
  DestroyController;
  FController := TUIDocumentPickerViewController.Alloc;
  FController := TUIDocumentPickerViewController.Wrap(FController.initWithDocumentTypes(StringsToNSArray(FSelector.FileTypes), UIDocumentPickerModeOpen));
  FController.setDelegate(GetObjectID);
  FController.setTitle(StrToNSStr(FSelector.Title));
  TiOSHelper.SharedApplication.keyWindow.rootViewController.presentViewController(FController, True, nil);
end;

procedure TUIDocumentPickerDelegate.documentPickerDidPickDocumentAtURL(controller: UIDocumentPickerViewController; url: NSURL);
begin
  FSelector.Files.Add(NSUrlToStr(url));
  FSelector.DoComplete(True);
end;

procedure TUIDocumentPickerDelegate.documentPickerDidPickDocumentsAtURLs(controller: UIDocumentPickerViewController; urls: NSArray);
var
  I: Integer;
begin
  for I := 0 to urls.count - 1 do
    FSelector.Files.Add(NSUrlToStr(TNSURL.Wrap(urls.objectAtIndex(I))));
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
end;

destructor TPlatformFilesSelector.Destroy;
begin
  FPicker.Free;
  inherited;
end;

procedure TPlatformFilesSelector.DoSelect;
begin
  FPicker.ShowPicker;
end;

end.
