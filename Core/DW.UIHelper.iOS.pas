unit DW.UIHelper.iOS;

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
    class function GetBrightness: Single; static;
    class function GetStatusBarOffset: Single; static;
    /// <summary>
    ///   Special function for handling of "notch" based devices
    /// </summary>
    class function GetOffsetRect: TRectF; overload; static;
    class function GetOffsetRect(const AHandle: TWindowHandle): TRectF; overload; static;
    class function GetScreenOrientation: TScreenOrientation; static;
    class function GetUserInterfaceStyle: TUserInterfaceStyle; static;
    class procedure SetBrightness(const AValue: Single); static;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // macOS
  Macapi.ObjectiveC, Macapi.Helpers,
  // iOS
  iOSapi.Foundation, iOSapi.CocoaTypes, iOSapi.Helpers, iOSapi.CoreGraphics,
  {$IF CompilerVersion > 36} iOSapi.UIKit, {$ENDIF}
  // FMX
  FMX.Forms, FMX.Platform.iOS
  // DW
  {$IF CompilerVersion < 37}, DW.iOSapi.UIKit {$ENDIF};

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
    {$IF (CompilerVersion < 37)}
    LPasteboard := TUIPasteboard.Wrap(TUIPasteboard.OCClass.generalPasteboard);
    {$ELSE}
    LPasteboard := TUIPasteboard.OCClass.generalPasteboard;
    {$ENDIF}
    LPasteboard.setData(LData, StrToNSStr('public.jpeg'));
  finally
    if LIsCopy then
      LMemoryStream.Free;
  end;
end;

class function TPlatformUIHelper.GetBrightness: Single;
begin
  Result := TiOSHelper.MainScreen.brightness;
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

class procedure TPlatformUIHelper.SetBrightness(const AValue: Single);
begin
  if (AValue >= 0) and (AValue <= 1) then
    TiOSHelper.MainScreen.setBrightness(AValue);
end;

class function TPlatformUIHelper.GetScreenOrientation: TScreenOrientation;
begin
  case TiOSHelper.CurrentDevice.orientation of
    UIDeviceOrientationLandscapeRight:
      Result := TScreenOrientation.Landscape;
    UIDeviceOrientationLandscapeLeft:
      Result := TScreenOrientation.InvertedLandscape;
    UIDeviceOrientationPortrait:
      Result := TScreenOrientation.Portrait;
    UIDeviceOrientationPortraitUpsideDown:
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
