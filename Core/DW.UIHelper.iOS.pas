unit DW.UIHelper.iOS;

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
  System.Types, System.Classes,
  // FMX
  FMX.Types,
  // DW
  DW.UIHelper;

type
  /// <summary>
  ///   Helper functions specific to UI
  /// </summary>
  TPlatformUIHelper = record
  public
    class procedure CopyImageToClipboard(const AImage: TStream); static;
    class function GetStatusBarOffset: Single; static;
    /// <summary>
    ///   Special function for handling of "notch" based devices
    /// </summary>
    class function GetOffsetRect: TRectF; overload; static;
    class function GetOffsetRect(const AHandle: TWindowHandle): TRectF; overload; static;
    class function GetScreenOrientation: TScreenOrientation; static;
    class function GetUserInterfaceStyle: TUserInterfaceStyle; static;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // macOS
  Macapi.ObjectiveC, Macapi.Helpers,
  // iOS
  iOSapi.UIKit, iOSapi.Foundation, iOSapi.CocoaTypes, iOSapi.Helpers, iOSapi.CoreGraphics,
  // FMX
  FMX.Forms, FMX.Platform.iOS;

const
  UIUserInterfaceStyleUnsepcified = 0;
  UIUserInterfaceStyleLight = 1;
  UIUserInterfaceStyleDark = 2;

type
  UITraitCollection = interface;
  UIUserInterfaceStyle = NSInteger;

  UIView = interface(iOSapi.UIKit.UIView)
    ['{9E246E80-7773-400C-8027-9FF0FA6FFA3E}']
    function safeAreaInsets: UIEdgeInsets; cdecl;
  end;
  TUIView = class(TOCGenericImport<UIViewClass, UIView>) end;

  UISceneClass = interface(UIResponderClass)
    ['{B29A0103-BD28-4C15-A714-7B19A035BD72}']
  end;

  UIScene = interface(UIResponder)
    ['{F55CD73F-350C-4F24-BAA4-43D2F4E420AC}']
  end;
  TUIScene = class(TOCGenericImport<UISceneClass, UIScene>) end;

  UIStatusBarManagerClass = interface(NSObjectClass)
    ['{D243D87E-37FA-43EF-8194-83DDCD4A606E}']
    {class} function new: Pointer; cdecl;
  end;

  UIStatusBarManager = interface(NSObject)
    ['{53F0B58E-F362-4EE4-B192-CA6E4CC35D20}']
    function isStatusBarHidden: Boolean; cdecl;
    function statusBarFrame: CGRect; cdecl;
    function statusBarStyle: UIStatusBarStyle; cdecl;
  end;
  TUIStatusBarManager = class(TOCGenericImport<UIStatusBarManagerClass, UIStatusBarManager>) end;

  UIWindowSceneClass = interface(UISceneClass)
    ['{34A039CA-CCBF-4D77-9298-24B749520EC2}']
  end;

  UIWindowScene = interface(UIScene)
    ['{388D2839-468C-4444-93E6-B3617A8ACB84}']
    function statusBarManager: UIStatusBarManager; cdecl;
  end;
  TUIWindowScene = class(TOCGenericImport<UIWindowSceneClass, UIWindowScene>) end;

  UIWindow = interface(iOSapi.UIKit.UIWindow)
    ['{20970E36-91D0-4EC4-AD9A-1852F1F31696}']
    function windowScene: UIWindowScene; cdecl;
  end;
  TUIWindow = class(TOCGenericImport<UIWindowClass, UIWindow>) end;


  UITraitCollectionClass = interface(NSObjectClass)
    ['{44D2D594-8CF0-4AA8-A38A-D54947ADD045}']
    {class} function current: UITraitCollection; cdecl;
  end;
  UITraitCollection = interface(NSObject)
    ['{F8037D43-27B4-4C0B-85C5-CAD58CBBD697}']
    function userInterfaceStyle: UIUserInterfaceStyle; cdecl;
  end;
  TUITraitCollection = class(TOCGenericImport<UITraitCollectionClass, UITraitCollection>)
  end;

  UITraitEnvironmentClass = interface(NSObjectClass)
    ['{52E664EE-1A01-486A-9A9B-9647BCDCA370}']
  end;
  UITraitEnvironment = interface(NSObject)
    ['{36F5C50F-B09D-4A20-B1A2-4410C5DB838C}']
    function traitCollection: UITraitCollection; cdecl;
  end;
  TUITraitEnvironment = class(TOCGenericImport<UITraitEnvironmentClass, UITraitEnvironment>)
  end;

function UIApplicationOpenSettingsURLString: NSString;
begin
  Result := CocoaNSStringConst(libUIKit, 'UIApplicationOpenSettingsURLString');
end;

{ TPlatformUIHelper }

class function TPlatformUIHelper.GetOffsetRect: TRectF;
begin
  Result := TRectF.Empty;
  if Application.MainForm <> nil then
    Result := GetOffsetRect(Application.MainForm.Handle);
end;

class procedure TPlatformUIHelper.CopyImageToClipboard(const AImage: TStream);
var
  LData: NSData;
  LPasteboard: UIPasteboard;
  LMemoryStream: TMemoryStream;
  LIsCopy: Boolean;
begin
  LIsCopy := False;
  if not (AImage is TMemoryStream) then
  begin
    LIsCopy := True;
    LMemoryStream := TMemoryStream.Create;
  end
  else
    LMemoryStream := TMemoryStream(AImage);
  try
    if LIsCopy then
      LMemoryStream.CopyFrom(AImage, AImage.Size);
    LData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(LMemoryStream.Memory, LMemoryStream.Size, False));
    LPasteboard := TUIPasteboard.Wrap(TUIPasteboard.OCClass.generalPasteboard);
    LPasteboard.setData(LData, StrToNSStr('public.jpeg'));
  finally
    if LIsCopy then
      LMemoryStream.Free;
  end;
end;

class function TPlatformUIHelper.GetOffsetRect(const AHandle: TWindowHandle): TRectF;
var
  LInsets: UIEdgeInsets;
begin
  Result := TRectF.Empty;
  if TOSVersion.Check(11) and (AHandle <> nil) then
  begin
    LInsets := TUIView.Wrap(NSObjectToID(WindowHandleToPlatform(AHandle).View)).safeAreaInsets;
    Result := RectF(LInsets.left, LInsets.top, LInsets.right, LInsets.bottom);
  end;
end;

class function TPlatformUIHelper.GetUserInterfaceStyle: TUserInterfaceStyle;
var
  LTraitCollection: UITraitCollection;
  LTraitEnvironment: UITraitEnvironment;
begin
  Result := TUserInterfaceStyle.Light;
  if TOSVersion.Check(13) then
  begin
    LTraitEnvironment := TUITraitEnvironment.Wrap(NSObjectToId(TiOSHelper.MainScreen));
    LTraitCollection := LTraitEnvironment.traitCollection;
    case LTraitCollection.userInterfaceStyle of
      UIUserInterfaceStyleDark:
        Result := TUserInterfaceStyle.Dark;
    end;
  end;
end;

class function TPlatformUIHelper.GetScreenOrientation: TScreenOrientation;
begin
  case TiOSHelper.SharedApplication.keyWindow.rootViewController.interfaceOrientation of
    UIInterfaceOrientationLandscapeLeft:
      Result := TScreenOrientation.Landscape;
    UIInterfaceOrientationLandscapeRight:
      Result := TScreenOrientation.InvertedLandscape;
    UIInterfaceOrientationPortrait:
      Result := TScreenOrientation.Portrait;
    UIInterfaceOrientationPortraitUpsideDown:
      Result := TScreenOrientation.InvertedPortrait;
  else
    Result := TScreenOrientation.Portrait
  end;
end;

class function TPlatformUIHelper.GetStatusBarOffset: Single;
begin
  if TOSVersion.Check(13) then
    Result := TUIWindow.Wrap(NSObjectToID(TiOSHelper.SharedApplication.keyWindow)).windowScene.statusBarManager.statusBarFrame.size.height
  else
    Result := TiOSHelper.SharedApplication.statusBarFrame.size.height;
  Result := Ord(GetScreenOrientation = TScreenOrientation.Portrait) * Result;
end;

end.
