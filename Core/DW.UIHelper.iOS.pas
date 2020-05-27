unit DW.UIHelper.iOS;

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
  // RTL
  System.Types,
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
    /// <summary>
    ///   Special function for handling of "notch" based devices
    /// </summary>
    class function GetOffsetRect: TRectF; overload; static;
    class function GetOffsetRect(const AHandle: TWindowHandle): TRectF; overload; static;
    class function GetUserInterfaceStyle: TUserInterfaceStyle; static;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // macOS
  Macapi.ObjectiveC, Macapi.Helpers,
  // iOS
  iOSapi.UIKit, iOSapi.Foundation, iOSapi.CocoaTypes, iOSapi.Helpers,
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
  TUIView = class(TOCGenericImport<UIViewClass, UIView>)  end;

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

class function TPlatformUIHelper.GetOffsetRect: TRectF;
begin
  Result := TRectF.Empty;
  if Application.MainForm <> nil then
    Result := GetOffsetRect(Application.MainForm.Handle);
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

end.
