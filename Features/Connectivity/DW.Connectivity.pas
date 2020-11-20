unit DW.Connectivity;

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

type
  TConnectivityChangeEvent = procedure(Sender: TObject; const IsConnected: Boolean) of object;

  TConnectivity = class(TObject)
  private
    FPlatformConnectivity: TObject;
    FSkipValidation: Boolean;
    FOnConnectivityChange: TConnectivityChangeEvent;
  protected
    procedure DoConnectivityChange(const IsConnected: Boolean);
  public
    class function IsConnectedToInternet: Boolean;
    class function IsWifiInternetConnection: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    /// <summary>
    ///   On Android, skips the validation check for the network capability
    /// </summary>
    /// <remarks>
    ///   May need to set this property to True for some devices
    /// </remarks>
    property SkipValidation: Boolean read FSkipValidation write FSkipValidation;
    property OnConnectivityChange: TConnectivityChangeEvent read FOnConnectivityChange write FOnConnectivityChange;
  end;

implementation

{$IF Defined(ANDROID)}
uses
  DW.Connectivity.Android;
{$ELSEIF Defined(IOS)}
uses
  DW.Connectivity.iOS;
{$ELSEIF Defined(MSWINDOWS)}
uses
  DW.Connectivity.Win;
{$ELSE}
uses
  DW.Connectivity.Default;
{$ENDIF}

{ TConnectivity }

constructor TConnectivity.Create;
begin
  inherited;
  FPlatformConnectivity := TPlatformConnectivity.Create(Self);
end;

destructor TConnectivity.Destroy;
begin
  FPlatformConnectivity.Free;
  inherited;
end;

procedure TConnectivity.DoConnectivityChange(const IsConnected: Boolean);
begin
  if Assigned(FOnConnectivityChange) then
    FOnConnectivityChange(Self, IsConnected);
end;

class function TConnectivity.IsConnectedToInternet: Boolean;
begin
  Result := TPlatformConnectivity.IsConnectedToInternet;
end;

class function TConnectivity.IsWifiInternetConnection: Boolean;
begin
  Result := TPlatformConnectivity.IsWifiInternetConnection;
end;

end.
