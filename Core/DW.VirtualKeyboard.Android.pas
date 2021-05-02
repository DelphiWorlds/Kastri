unit DW.VirtualKeyboard.Android;

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
  System.Types;

type
  TPlatformVirtualKeyboard = record
  public
    class function GetBounds: TRect; static;
  end;

implementation

uses
  // Android
  Androidapi.JNI.GraphicsContentViewText,
  // FMX
  {$IF CompilerVersion > 32}
  FMX.Platform.UI.Android,
  {$ENDIF}
  FMX.Platform.Android;

class function TPlatformVirtualKeyboard.GetBounds: TRect;
var
  ContentRect, TotalRect: JRect;
begin
  ContentRect := TJRect.Create;
  TotalRect := TJRect.Create;
  MainActivity.getWindow.getDecorView.getWindowVisibleDisplayFrame(ContentRect);
  MainActivity.getWindow.getDecorView.getDrawingRect(TotalRect);
  Result := TRectF.Create(ConvertPixelToPoint(TPointF.Create(TotalRect.left, TotalRect.top + ContentRect.height)),
    ConvertPixelToPoint(TPointF.Create(TotalRect.right, TotalRect.bottom))).Truncate;
end;

end.
