unit DW.QuickLook.iOS;

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
  // RTL
  System.TypInfo,
  // Mac
  Macapi.ObjectiveC,
  // iOS
  iOSapi.CoreGraphics, iOSapi.UIKit, iOSapi.CocoaTypes, iOSapi.Foundation,
  // DW
  DW.iOSapi.QuickLook;

type
  CustomQLPreviewController = interface(QLPreviewController)
    ['{1850AB17-2BBA-4374-9A02-AA7F36193AAC}']
    function shouldAutorotateToInterfaceOrientation(toInterfaceOrientation: UIInterfaceOrientation): Boolean; cdecl;
  end;

  TCustomQLPreviewController = class(TOCLocal)
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor Create;
    function shouldAutorotateToInterfaceOrientation(toInterfaceOrientation: UIInterfaceOrientation): Boolean; cdecl;
  end;

  TQLPreviewItem = class(TOCLocal, QLPreviewItem)
  private
    FTitle: NSString;
    FURL: NSURL;
  public
    { QLPreviewItem }
    function previewItemTitle: NSString; cdecl;
    function previewItemURL: NSURL; cdecl;
  public
    constructor Create(const AFileName: string);
  end;

  TQLPreviewControllerDataSource = class(TOCLocal, QLPreviewControllerDataSource)
  private
    FItem: QLPreviewItem;
  protected
    procedure SetFileName(const AFileName: string);
  public
    { QLPreviewControllerDataSource }
    function numberOfPreviewItemsInPreviewController(controller: QLPreviewController): NSInteger; cdecl;
    function previewController(controller: QLPreviewController; previewItemAtIndex: NSInteger): QLPreviewItem; cdecl;
  end;

  TQLPreviewControllerDelegate = class(TOCLocal, QLPreviewControllerDelegate)
  public
    procedure previewControllerDidDismiss(controller: QLPreviewController); cdecl;
    [MethodName('previewController:didSaveEditedCopyOfPreviewItem:atURL:')]
    procedure previewControllerDidSaveEditedCopyOfPreviewItem(controller: QLPreviewController; didSaveEditedCopyOfPreviewItem: Pointer; atURL: NSURL); cdecl;
    [MethodName('previewController:didUpdateContentsOfPreviewItem:')]
    procedure previewControllerDidUpdateContentsOfPreviewItem(controller: QLPreviewController; didUpdateContentsOfPreviewItem: Pointer); cdecl;
    [MethodName('previewController:editingModeForPreviewItem:')]
    function previewControllerEditingModeForPreviewItem(controller: QLPreviewController; editingModeForPreviewItem: Pointer): QLPreviewItemEditingMode; cdecl;
    [MethodName('previewController:frameForPreviewItem:inSourceView:')]
    function previewControllerFrameForPreviewItem(controller: QLPreviewController; frameForPreviewItem: Pointer; var inSourceView: Pointer): CGRect; cdecl;
    [MethodName('previewController:shouldOpenURL:forPreviewItem:')]
    function previewControllerShouldOpenURL(controller: QLPreviewController; shouldOpenURL: NSURL; forPreviewItem: Pointer): Boolean; cdecl;
    [MethodName('previewController:transitionImageForPreviewItem:contentRect:')]
    function previewControllerTransitionImageForPreviewItem(controller: QLPreviewController; transitionImageForPreviewItem: Pointer; contentRect: PCGRect): UIImage; cdecl;
    [MethodName('previewController:transitionViewForPreviewItem:')]
    function previewControllerTransitionViewForPreviewItem(controller: QLPreviewController; transitionViewForPreviewItem: Pointer): UIView; cdecl;
    procedure previewControllerWillDismiss(controller: QLPreviewController); cdecl;
  end;

  TQLPreview = class(TInterfacedObject)
  private
    FController: QLPreviewController;
    FControllerDataSource: TQLPreviewControllerDataSource;
    FControllerDelegate: TQLPreviewControllerDelegate;
  public
    constructor Create;
    destructor Destroy; override;
    function OpenDocument(const AFilename: string): Boolean;
  end;

implementation

uses
  // RTL
  System.IOUtils, System.SysUtils,
  // Mac
  Macapi.Helpers, Macapi.CoreFoundation,
  // iOS
  iOSapi.Helpers,
  // FMX
  FMX.Forms;

{ TCustomQLPreviewController }

constructor TCustomQLPreviewController.Create;
var
  LID: Pointer;
begin
  inherited;
  LID := QLPreviewController(Super).initWithNibName(nil, nil);
  if GetObjectID <> LID then
    UpdateObjectID(LID);
end;

function TCustomQLPreviewController.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(CustomQLPreviewController);
end;

function TCustomQLPreviewController.shouldAutorotateToInterfaceOrientation(toInterfaceOrientation: UIInterfaceOrientation): Boolean;
begin
  case toInterfaceOrientation of
    UIInterfaceOrientationLandscapeLeft:
      Result := TFormOrientation.Landscape in Application.FormFactor.Orientations;
    UIInterfaceOrientationLandscapeRight:
      Result := TFormOrientation.InvertedLandscape in Application.FormFactor.Orientations;
    UIInterfaceOrientationPortrait:
      Result := TFormOrientation.Portrait in Application.FormFactor.Orientations;
    UIInterfaceOrientationPortraitUpsideDown:
      Result := TFormOrientation.InvertedPortrait in Application.FormFactor.Orientations;
  else
    Result := False;
  end;
end;

{ TQLPreviewControllerDelegate }

procedure TQLPreviewControllerDelegate.previewControllerDidDismiss(controller: QLPreviewController);
begin
  //
end;

procedure TQLPreviewControllerDelegate.previewControllerDidSaveEditedCopyOfPreviewItem(controller: QLPreviewController;
  didSaveEditedCopyOfPreviewItem: Pointer; atURL: NSURL);
begin
  //
end;

procedure TQLPreviewControllerDelegate.previewControllerDidUpdateContentsOfPreviewItem(controller: QLPreviewController;
  didUpdateContentsOfPreviewItem: Pointer);
begin
  //
end;

function TQLPreviewControllerDelegate.previewControllerEditingModeForPreviewItem(controller: QLPreviewController;
  editingModeForPreviewItem: Pointer): QLPreviewItemEditingMode;
begin
  Result := QLPreviewItemEditingModeDisabled;
end;

function TQLPreviewControllerDelegate.previewControllerFrameForPreviewItem(controller: QLPreviewController; frameForPreviewItem: Pointer;
  var inSourceView: Pointer): CGRect;
begin
  // Apparently on iOS this method is not called anyway?
  Result := CGRectMake(0, 0, 0, 0);
end;

function TQLPreviewControllerDelegate.previewControllerShouldOpenURL(controller: QLPreviewController; shouldOpenURL: NSURL;
  forPreviewItem: Pointer): Boolean;
begin
  Result := True;
end;

function TQLPreviewControllerDelegate.previewControllerTransitionImageForPreviewItem(controller: QLPreviewController;
  transitionImageForPreviewItem: Pointer; contentRect: PCGRect): UIImage;
begin
  Result := nil;
end;

function TQLPreviewControllerDelegate.previewControllerTransitionViewForPreviewItem(controller: QLPreviewController;
  transitionViewForPreviewItem: Pointer): UIView;
begin
  // Apparently on iOS this method is not called anyway?
  Result := nil;
end;

procedure TQLPreviewControllerDelegate.previewControllerWillDismiss(controller: QLPreviewController);
begin
  //
end;

{ TQLPreviewItem }

constructor TQLPreviewItem.Create(const AFileName: string);
begin
  inherited Create;
  FURL := TNSURL.Wrap(TNSURL.OCClass.fileURLWithPath(StrToNSStr(AFileName)));
  FTitle := StrToNSStr('');
end;

function TQLPreviewItem.previewItemTitle: NSString;
begin
  Result := FTitle;
end;

function TQLPreviewItem.previewItemURL: NSURL;
begin
  Result := FURL;
end;

{ TQLPreviewControllerDataSource }

function TQLPreviewControllerDataSource.numberOfPreviewItemsInPreviewController(controller: QLPreviewController): NSInteger;
begin
  Result := 1;
end;

function TQLPreviewControllerDataSource.previewController(controller: QLPreviewController; previewItemAtIndex: NSInteger): QLPreviewItem;
begin
  Result := FItem;
end;

procedure TQLPreviewControllerDataSource.SetFileName(const AFileName: string);
begin
  FItem := nil;
  FItem := TQLPreviewItem.Create(AFileName);
end;

{ TQLPreview }

constructor TQLPreview.Create;
begin
  inherited;
  FController := QLPreviewController(TCustomQLPreviewController.Create.Super);
  FControllerDelegate := TQLPreviewControllerDelegate.Create;
  FController.setDelegate(FControllerDelegate.GetObjectID);
  FControllerDataSource := TQLPreviewControllerDataSource.Create;
  FController.setDataSource(FControllerDataSource.GetObjectID);
end;

destructor TQLPreview.Destroy;
begin
  FController := nil;
  FControllerDelegate.Free;
  FControllerDataSource.Free;
  inherited;
end;

function TQLPreview.OpenDocument(const AFilename: string): Boolean;
begin
  Result := False;
  if TFile.Exists(AFilename) then
  begin
    FControllerDataSource.SetFileName(AFilename);
    TiOSHelper.SharedApplication.keyWindow.rootViewController.presentModalViewController(FController, False);
    FController.reloadData;
    Result := True;
  end;
end;

end.
