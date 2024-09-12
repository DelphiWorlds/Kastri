unit DW.OSMetadata.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2024 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // Android
  Androidapi.JNI.Os;

type
  TPlatformOSMetadata = record
  private
    class var FMetadata: JBundle;
    class function GetMetadata: JBundle; static;
  public
    class function ContainsKey(const AKey: string): Boolean; static;
    class function GetValue(const AKey: string; var AValue: string): Boolean; overload; static;
    class function GetValue(const AKey: string; var AValue: Integer): Boolean; overload; static;
    class function GetValue(const AKey: string; var AValue: Int64): Boolean; overload; static;
  end;

implementation

uses
  // Android
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.Helpers;

{ TPlatformOSMetadata }

class function TPlatformOSMetadata.GetMetadata: JBundle;
var
  LFlags: Integer;
begin
  if FMetadata = nil then
  begin
    LFlags := TJPackageManager.JavaClass.GET_META_DATA;
    FMetadata := TAndroidHelper.Context.getPackageManager.getApplicationInfo(TAndroidHelper.Context.getPackageName, LFlags).metaData;
  end;
  Result := FMetadata;
end;

class function TPlatformOSMetadata.GetValue(const AKey: string; var AValue: Integer): Boolean;
begin
  Result := ContainsKey(AKey);
  if Result then
    AValue := GetMetadata.getInt(StringToJString(AKey));
end;

class function TPlatformOSMetadata.ContainsKey(const AKey: string): Boolean;
begin
  Result := GetMetadata.containsKey(StringToJString(AKey));
end;

class function TPlatformOSMetadata.GetValue(const AKey: string; var AValue: string): Boolean;
begin
  Result := ContainsKey(AKey);
  if Result then
    AValue := JStringToString(GetMetadata.getString(StringToJString(AKey)));
end;

class function TPlatformOSMetadata.GetValue(const AKey: string; var AValue: Int64): Boolean;
begin
  Result := ContainsKey(AKey);
  if Result then
    AValue := GetMetadata.getLong(StringToJString(AKey));
end;

end.