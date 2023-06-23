unit DW.MobileDeviceInfo;

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

// Designed to be used with the file Resources\MobileDeviceInfo.json in this repo
// Based on data from: https://www.theiphonewiki.com/wiki/Models

interface

type
  TMobileDevice = class(TObject)
  private
    FArchitecture: string;
    FCode: string;
    FDescription: string;
    FModelName: string;
  public
    property Architecture: string read FArchitecture write FArchitecture;
    property Code: string read FCode write FCode;
    property Description: string read FDescription write FDescription;
    property ModelName: string read FModelName write FModelName;
  end;

  TMobileDevices = TArray<TMobileDevice>;

  TMobileDeviceInfo = class(TObject)
  private
    FDevices: TMobileDevices;
  public
    function FindDevice(const ACode: string; out ADevice: TMobileDevice): Boolean;
    function FindDeviceByModel(const AModelName: string; out ADevice: TMobileDevice): Boolean;
    property Devices: TMobileDevices read FDevices write FDevices;
  end;

implementation

uses
  // RTL
  System.SysUtils;

{ TMobileDeviceInfo }

function TMobileDeviceInfo.FindDevice(const ACode: string; out ADevice: TMobileDevice): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Length(FDevices) - 1 do
  begin
    if FDevices[I].Code.Equals(ACode) then
    begin
      ADevice := FDevices[I];
      Result := True;
      Break;
    end;
  end;
end;

function TMobileDeviceInfo.FindDeviceByModel(const AModelName: string; out ADevice: TMobileDevice): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Length(FDevices) - 1 do
  begin
    if FDevices[I].ModelName.Equals(AModelName) then
    begin
      ADevice := FDevices[I];
      Result := True;
      Break;
    end;
  end;
end;

end.
