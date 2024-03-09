unit DW.Connectivity.Default;

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
  DW.Connectivity;

type
  TPlatformConnectivity = class(TObject)
  public
    class function IsConnectedToInternet: Boolean;
    class function IsWifiInternetConnection: Boolean;
  public
    constructor Create(const AConnectivity: TConnectivity);
  end;

implementation

{ TPlatformConnectivity }

constructor TPlatformConnectivity.Create(const AConnectivity: TConnectivity);
begin
  inherited Create;
  //
end;

class function TPlatformConnectivity.IsConnectedToInternet: Boolean;
begin
  Result := False;
end;

class function TPlatformConnectivity.IsWifiInternetConnection: Boolean;
begin
  Result := False;
end;

end.
