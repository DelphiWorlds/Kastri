unit DW.VirtualKeyboard.Helpers;

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
  System.Types,
  // FMX
  FMX.Types;

type
  TVirtualKeyboard = record
  public
    class procedure EnableToolbar(const AEnable: Boolean); static;
    class function GetBounds: TRect; static;
    class procedure Hide; static;
    class function IsVisible: Boolean; static;
    class procedure Show(const AObject: TFmxObject); static;
  end;

implementation

uses
  {$IF Defined(ANDROID)}
  // DW
  DW.VirtualKeyboard.Android,
  {$ENDIF}
  // FMX
  FMX.Platform, FMX.VirtualKeyboard;

{ TVirtualKeyboard }

class function TVirtualKeyboard.GetBounds: TRect;
begin
  {$IF Defined(ANDROID)}
  Result := TPlatformVirtualKeyboard.GetBounds;
  {$ELSE}
  Result := TRect.Empty;
  {$ENDIF}
end;

class procedure TVirtualKeyboard.Hide;
var
  LService: IFMXVirtualKeyboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, LService) then
    LService.HideVirtualKeyboard;
end;

class function TVirtualKeyboard.IsVisible: Boolean;
var
  LService: IFMXVirtualKeyboardService;
begin
  Result := False;
  if TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, LService) then
    Result := TVirtualKeyboardState.Visible in LService.VirtualKeyboardState;
end;

class procedure TVirtualKeyboard.Show(const AObject: TFmxObject);
var
  LService: IFMXVirtualKeyboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, LService) then
    LService.ShowVirtualKeyboard(AObject);
end;

class procedure TVirtualKeyboard.EnableToolbar(const AEnable: Boolean);
var
  LService: IFMXVirtualKeyboardToolbarService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardToolbarService, LService) then
    LService.SetToolbarEnabled(AEnable);
end;

end.
