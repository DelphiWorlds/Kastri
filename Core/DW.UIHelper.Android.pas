unit DW.UIHelper.Android;

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
    class function GetUserInterfaceStyle: TUserInterfaceStyle; static;
    class procedure Render(const AForm: TForm); static;
  end;

implementation

uses
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
