unit DW.OSMetadata.Win;

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
  DW.FileVersionInfo.Win;

type
  TPlatformOSMetadata = class(TObject)
  private
    class var FMetadata: TFileVersionInfo;
    class destructor DestroyClass;
    class function GetMetadata: TFileVersionInfo; static;
  public
    class function ContainsKey(const AKey: string): Boolean; static;
    class function GetValue(const AKey: string; var AValue: string): Boolean; static;
  end;

implementation

uses
  System.SysUtils;

{ TPlatformOSMetadata }

class destructor TPlatformOSMetadata.DestroyClass;
begin
  FMetadata.Free;
end;

class function TPlatformOSMetadata.GetMetadata: TFileVersionInfo;
begin
  if FMetadata = nil then
    FMetadata := TFileVersionInfo.Create(GetModuleName(HInstance));
  Result := FMetadata;
end;

class function TPlatformOSMetadata.ContainsKey(const AKey: string): Boolean;
begin
  Result := not GetMetadata.Values[AKey].IsEmpty;
end;

class function TPlatformOSMetadata.GetValue(const AKey: string; var AValue: string): Boolean;
begin
  AValue := GetMetadata.Values[AKey];
  Result := not AValue.IsEmpty;
end;

end.