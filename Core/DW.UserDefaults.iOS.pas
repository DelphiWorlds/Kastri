unit DW.UserDefaults.iOS;

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
  // iOS
  iOSapi.Foundation;

type
  TUserDefaults = record
  public
    class function GetValue(const AKey: string; const ADefault: string = ''): string; static;
    class procedure SetValue(const AKey, AValue: string); overload; static;
    class procedure SetValue(const AKey: NSString; const AValue: NSObject); overload; static;
  end;

implementation

uses
  // macOS
  Macapi.Helpers,
  // DW
  DW.iOSapi.Helpers;

{ TUserDefaults }

class function TUserDefaults.GetValue(const AKey: string; const ADefault: string = ''): string;
var
  LValue: NSString;
begin
  LValue := TiOSHelperEx.StandardUserDefaults.stringForKey(StrToNSStr(AKey));
  if LValue <> nil then
    Result := NSStrToStr(LValue)
  else
    Result := ADefault;
end;

class procedure TUserDefaults.SetValue(const AKey, AValue: string);
begin
  TiOSHelperEx.StandardUserDefaults.setObject(StringToID(AValue), StrToNSStr(AKey));
end;

class procedure TUserDefaults.SetValue(const AKey: NSString; const AValue: NSObject);
begin
  TiOSHelperEx.StandardUserDefaults.setObject(NSObjectToID(AValue), AKey);
end;

end.
