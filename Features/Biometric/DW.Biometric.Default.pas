unit DW.Biometric.Default;

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
  System.SysUtils,
  // DW
  DW.Biometric;

type
  TPlatformBiometric = class(TCustomPlatformBiometric)
  protected
    function  GetBiometricCapability: TBiometricCapabilityResult; override;
  public
    function CanVerify: Boolean; override;
    function HasUserInterface: Boolean; override;
    function IsBiometryLockedOut: Boolean; override;
  public
    class function GetBiometryKind: TBiometryKind; override;
    class function IsSupported: Boolean; override;
  end;

implementation

{ TPlatformBiometric }

function TPlatformBiometric.CanVerify: Boolean;
begin
  Result := False;
end;

function TPlatformBiometric.GetBiometricCapability: TBiometricCapabilityResult;
begin
  Result := TBiometricCapabilityResult.NoHardware;
end;

class function TPlatformBiometric.GetBiometryKind: TBiometryKind;
begin
  Result := TBiometryKind.None;
end;

function TPlatformBiometric.HasUserInterface: Boolean;
begin
  Result := False;
end;

function TPlatformBiometric.IsBiometryLockedOut: Boolean;
begin
  Result := False;
end;

class function TPlatformBiometric.IsSupported: Boolean;
begin
  Result := False;
end;

end.
