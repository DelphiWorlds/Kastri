unit DW.UIHelper.Android;

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

uses
  // RTL
  System.Types, System.UITypes,
  // FMX
  FMX.Types, FMX.Forms,
  // DW
  DW.UIHelper;

type
  /// <summary>
  ///   Helper functions specific to UI
  /// </summary>
  TPlatformUIHelper = record
  private
    class function GetResourceHeight(const AResourceName: string): Single; static;
  public
    class function GetBrightness: Single; static;
    class function GetNavigationBarOffset: Single; static;
    /// <summary>
    ///   Special function for handling of "notch" based devices
    /// </summary>
    class function GetOffsetRect: TRectF; overload; static;
    class function GetOffsetRect(const AHandle: TWindowHandle): TRectF; overload; static;
    class function GetScreenOrientation: TScreenOrientation; static;
    class function GetStatusBarOffset: Single; static;
    class function GetUserInterfaceStyle: TUserInterfaceStyle; static;
    class function HorzTextAlignToGravity(const AAlign: TTextAlign): Integer; static;
    class function IsFullScreen: Boolean; static;
    class procedure NeedsFullScreen; static;
    class procedure Render(const AForm: TForm); static;
    class procedure SetBrightness(const AValue: Single); static;
    class function TFontStylesToStyle(const AStyle: TFontStyles): Integer; static;
    class function VertTextAlignToGravity(const AAlign: TTextAlign): Integer; static;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.Helpers, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.App, Androidapi.JNI.Provider, Androidapi.JNI.JavaTypes,
  // FMX
  FMX.Platform, FMX.Platform.UI.Android, FMX.Platform.Android,
  // DW
  DW.Androidapi.JNI.DWUtility, DW.Android.Helpers;

{ TPlatformUIHelper }

class function TPlatformUIHelper.GetOffsetRect: TRectF;
begin
  // Yet to be implemented. Work is in progress
  Result := TRectF.Empty;
end;

class function TPlatformUIHelper.GetBrightness: Single;
begin
  // Result := TJSettings_System.JavaClass.getInt(TAndroidHelper.ContentResolver, TJSettings_System.JavaClass.SCREEN_BRIGHTNESS) / 255;
  Result := TJDWUtility.JavaClass.getScreenBrightness(TAndroidHelper.Activity);
end;

class function TPlatformUIHelper.GetNavigationBarOffset: Single;
begin
  if TAndroidHelperEx.CheckBuildAndTarget(35) or IsFullScreen then
    Result := GetResourceHeight('navigation_bar_height')
  else
    Result := 0;
end;

class function TPlatformUIHelper.GetOffsetRect(const AHandle: TWindowHandle): TRectF;
begin
  // Yet to be implemented. Work is in progress
  Result := TRectF.Empty;
end;

class function TPlatformUIHelper.GetResourceHeight(const AResourceName: string): Single;
var
  LID: Integer;
  LResources: JResources;
begin
  Result := 0;
  LResources := TAndroidHelper.Context.getResources;
  LID := LResources.getIdentifier(StringToJString(AResourceName), StringToJString('dimen'), StringToJString('android'));
  if LID > 0 then
    Result := LResources.getDimensionPixelSize(LID) / TUIHelper.GetScale;
end;

class function TPlatformUIHelper.GetScreenOrientation: TScreenOrientation;
var
  LRotation: Integer;
begin
  LRotation := TAndroidHelper.Activity.getWindowManager.getDefaultDisplay.getRotation;
  if LRotation = TJSurface.JavaClass.ROTATION_180 then
    Result := TScreenOrientation.InvertedPortrait
  else if LRotation = TJSurface.JavaClass.ROTATION_90 then
    Result := TScreenOrientation.Landscape
  else if LRotation = TJSurface.JavaClass.ROTATION_270 then
    Result := TScreenOrientation.InvertedLandscape
  else
    Result := TScreenOrientation.Portrait;
end;

class function TPlatformUIHelper.GetStatusBarOffset: Single;
begin
  Result := GetResourceHeight('status_bar_height');
end;

class function TPlatformUIHelper.GetUserInterfaceStyle: TUserInterfaceStyle;
var
  LConfiguration: JConfiguration;
  LNightMode: Integer;
begin
  LConfiguration := TAndroidHelper.Context.getResources.getConfiguration;
  LNightMode := LConfiguration.uiMode and TJConfiguration.JavaClass.UI_MODE_NIGHT_MASK;
  if LNightMode = TJConfiguration.JavaClass.UI_MODE_NIGHT_YES then
    Result := TUserInterfaceStyle.Dark
  else
    Result := TUserInterfaceStyle.Light;
end;

class procedure TPlatformUIHelper.Render(const AForm: TForm);
begin
  TAndroidWindowHandle(AForm.Handle).Render.Render;
end;

// https://stackoverflow.com/a/18312812/3164070
class procedure TPlatformUIHelper.SetBrightness(const AValue: Single);
//var
//  LParams: JWindowManager_LayoutParams;
begin
  if (AValue >= 0) and (AValue <= 1) then
  begin
    TJDWUtility.JavaClass.setScreenBrightness(TAndroidHelper.Activity, AValue);
//    LParams := TAndroidHelper.Activity.getWindow.getAttributes;
//    LParams.screenBrightness := AValue;
//    TAndroidHelper.Activity.getWindow.setAttributes(LParams);
  end;
end;

class function TPlatformUIHelper.VertTextAlignToGravity(const AAlign: TTextAlign): Integer;
begin
  case AAlign of
    TTextAlign.Center:
      Result := TJGravity.JavaClass.CENTER_VERTICAL;
    TTextAlign.Leading:
      Result := TJGravity.JavaClass.TOP;
    TTextAlign.Trailing:
      Result := TJGravity.JavaClass.BOTTOM;
  else
    Result := TJGravity.JavaClass.CENTER_VERTICAL;
  end;
end;

class function TPlatformUIHelper.HorzTextAlignToGravity(const AAlign: TTextAlign): Integer;
begin
  case AAlign of
    TTextAlign.Center:
      Result := TJGravity.JavaClass.CENTER;
    TTextAlign.Leading:
      Result := TJGravity.JavaClass.LEFT;
    TTextAlign.Trailing:
      Result := TJGravity.JavaClass.RIGHT;
  else
    Result := TJGravity.JavaClass.CENTER;
  end;
end;

class function TPlatformUIHelper.IsFullScreen: Boolean;
begin
  Result := (TAndroidHelper.Activity.getWindow.getAttributes.flags and TJWindowManager_LayoutParams.JavaClass.FLAG_LAYOUT_NO_LIMITS) > 0;
end;

class procedure TPlatformUIHelper.NeedsFullScreen;
begin
  TAndroidHelper.Activity.getWindow.setFlags(TJWindowManager_LayoutParams.JavaClass.FLAG_LAYOUT_NO_LIMITS,
    TJWindowManager_LayoutParams.JavaClass.FLAG_LAYOUT_NO_LIMITS);
end;

class function TPlatformUIHelper.TFontStylesToStyle(const AStyle: TFontStyles): Integer;
begin
  if (TFontStyle.fsBold in AStyle) and (TFontStyle.fsItalic in AStyle) then
    Result := TJTypeface.JavaClass.BOLD_ITALIC
  else if TFontStyle.fsBold in AStyle then
    Result := TJTypeface.JavaClass.BOLD
  else if TFontStyle.fsItalic in AStyle then
    Result := TJTypeface.JavaClass.ITALIC
  else
    Result := TJTypeface.JavaClass.NORMAL;
end;

end.
