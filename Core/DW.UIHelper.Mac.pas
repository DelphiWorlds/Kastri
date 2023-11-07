unit DW.UIHelper.Mac;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2023 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // RTL
  System.Types, System.Classes,
  Macapi.AppKit,
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
    class function ActiveScreen: NSScreen; static;
    class procedure CopyImageToClipboard(const AImage: TStream); static;
    class function GetBrightness: Single; static;
    class function GetStatusBarOffset(const APrimary: Boolean = False): Single; static;
    /// <summary>
    ///   Special function for handling of "notch" based devices
    /// </summary>
    class function GetOffsetRect: TRectF; overload; static;
    class function GetOffsetRect(const AHandle: TWindowHandle): TRectF; overload; static;
    class function GetScreenOrientation: TScreenOrientation; static;
    class function GetUserInterfaceStyle: TUserInterfaceStyle; static;
    class function PrimaryScreen: NSScreen; static;
    class procedure SetBrightness(const AValue: Single); static;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // macOS
  Macapi.ObjectiveC, Macapi.Helpers,
  Macapi.Foundation, Macapi.CocoaTypes, Macapi.CoreGraphics,
  // FMX
  FMX.Forms, FMX.Platform.Mac,
  DW.Macapi.Helpers;

const
  NSApplicationPresentationFullScreen = 1024;
  NSApplicationPresentationAutoHideToolbar = 2048;
  NSApplicationPresentationDisableCursorLocationAssistance = 4096;

{ TPlatformUIHelper }

class function TPlatformUIHelper.GetOffsetRect: TRectF;
begin
  Result := TRectF.Empty;
{
  if Application.MainForm <> nil then
    Result := GetOffsetRect(Application.MainForm.Handle);
}
end;

class function TPlatformUIHelper.ActiveScreen: NSScreen;
begin
  Result := TNSScreen.Wrap(TNSScreen.OCClass.mainScreen);
end;

class function TPlatformUIHelper.PrimaryScreen: NSScreen;
begin
  Result := TNSScreen.Wrap(TNSScreen.OCClass.screens.objectAtIndex(0));
end;

class procedure TPlatformUIHelper.CopyImageToClipboard(const AImage: TStream);
{
var
  LData: NSData;
  LPasteboard: UIPasteboard;
  LMemoryStream: TMemoryStream;
  LIsCopy: Boolean;
}
begin
{
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
}
end;

class function TPlatformUIHelper.GetBrightness: Single;
begin
  Result := 1;
  // Result := TiOSHelper.MainScreen.brightness;
end;

class function TPlatformUIHelper.GetOffsetRect(const AHandle: TWindowHandle): TRectF;
//var
//  LInsets: UIEdgeInsets;
begin
  Result := TRectF.Empty;
{
  if TOSVersion.Check(11) and (AHandle <> nil) then
  begin
    LInsets := TUIView.Wrap(NSObjectToID(WindowHandleToPlatform(AHandle).View)).safeAreaInsets;
    Result := RectF(LInsets.left, LInsets.top, LInsets.right, LInsets.bottom);
  end;
}
end;

class function TPlatformUIHelper.GetUserInterfaceStyle: TUserInterfaceStyle;
{
var
  LTraitCollection: UITraitCollection;
  LTraitEnvironment: UITraitEnvironment;
}
begin
  Result := TUserInterfaceStyle.Light;
{
  if TOSVersion.Check(13) then
  begin
    LTraitEnvironment := TUITraitEnvironment.Wrap(NSObjectToId(TiOSHelper.MainScreen));
    LTraitCollection := LTraitEnvironment.traitCollection;
    case LTraitCollection.userInterfaceStyle of
      UIUserInterfaceStyleDark:
        Result := TUserInterfaceStyle.Dark;
    end;
  end;
}
end;

class procedure TPlatformUIHelper.SetBrightness(const AValue: Single);
begin
//  if (AValue >= 0) and (AValue <= 1) then
//    TiOSHelper.MainScreen.setBrightness(AValue);
end;

class function TPlatformUIHelper.GetScreenOrientation: TScreenOrientation;
begin
  Result := TScreenOrientation.Portrait;
end;

class function TPlatformUIHelper.GetStatusBarOffset(const APrimary: Boolean = False): Single;
var
  LPresentationOptions: NSApplicationPresentationOptions;
  LScreen: NSScreen;
begin
  Result := 0;
  LPresentationOptions := TMacHelperEx.SharedApplication.currentSystemPresentationOptions;
  if (LPresentationOptions and NSApplicationPresentationFullScreen) = 0 then
  begin
    if APrimary then
      LScreen := PrimaryScreen
    else
      LScreen := ActiveScreen;
    Result := LScreen.frame.size.height - LScreen.visibleFrame.size.height;
  end;
end;

end.
