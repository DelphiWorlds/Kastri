unit DW.UIHelper.Android;

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
  System.Types,
  // FMX
  FMX.Types, FMX.Forms,
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
    class function GetScreenOrientation: TScreenOrientation; static;
    class function GetStatusBarOffset: Single; static;
    class function GetUserInterfaceStyle: TUserInterfaceStyle; static;
    class procedure Render(const AForm: TForm); static;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.Helpers, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.App,
  // FMX
  FMX.Platform.UI.Android;

{ TPlatformUIHelper }

class function TPlatformUIHelper.GetOffsetRect: TRectF;
begin
  // Yet to be implemented. Work is in progress
  Result := TRectF.Empty;
end;

class function TPlatformUIHelper.GetOffsetRect(const AHandle: TWindowHandle): TRectF;
begin
  // Yet to be implemented. Work is in progress
  Result := TRectF.Empty;
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
  Result := 0;
end;

class function TPlatformUIHelper.GetUserInterfaceStyle: TUserInterfaceStyle;
begin
  // Yet to be implemented. Work is in progress
  Result := TUserInterfaceStyle.Light;
end;

class procedure TPlatformUIHelper.Render(const AForm: TForm);
begin
  TAndroidWindowHandle(AForm.Handle).Render.Render;
end;

end.
