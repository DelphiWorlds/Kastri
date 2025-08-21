unit DW.Android.Service;

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
  System.Android.Service;

type
  TLocalServiceConnection = class(System.Android.Service.TLocalServiceConnection)
  public
    class procedure StartForegroundService(const AServiceName: string); static;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.Helpers, AndroidApi.JNI.GraphicsContentViewText,
  // DW
  DW.Consts.Android, DW.Androidapi.JNI.Content;

{ TLocalServiceConnection }

class procedure TLocalServiceConnection.StartForegroundService(const AServiceName: string);
var
  LIntent: JIntent;
  LService: string;
begin
  if TOSVersion.Check(8) then
  begin
    LIntent := TJIntent.Create;
    LService := AServiceName;
    if not LService.StartsWith(cEMBTJavaServicePrefix) then
      LService := cEMBTJavaServicePrefix + LService;
    LIntent.setClassName(TAndroidHelper.Context.getPackageName(), TAndroidHelper.StringToJString(LService));
    TJContextWrapper.Wrap(System.JavaContext).startForegroundService(LIntent);
  end
  else
    StartService(AServiceName);
end;

end.
