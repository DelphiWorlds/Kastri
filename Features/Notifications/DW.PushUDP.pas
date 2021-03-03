unit DW.PushUDP;

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
  System.SysUtils, System.Classes, System.Messaging,
  // Indy
  IdBaseComponent, IdComponent, IdUDPBase, IdUDPClient,
  // FMX
  FMX.Types;

type
  TPushUDP = class(TDataModule)
    UDPClient: TIdUDPClient;
    UDPTimer: TTimer;
    procedure UDPTimerTimer(Sender: TObject);
  private
    FDeviceID: string;
    FToken: string;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
    procedure SendDeviceInfo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateDeviceInfo(const ADeviceID, AToken: string);
  end;

var
  PushUDP: TPushUDP;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses
  // RTL
  System.JSON,
  // FMX
  FMX.Platform,
  // DW
  DW.Consts.Android, DW.OSMetadata;

{ TPushUDP }

constructor TPushUDP.Create(AOwner: TComponent);
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
end;

destructor TPushUDP.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  inherited;
end;

procedure TPushUDP.ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
{
  case TApplicationEventMessage(AMsg).Value.Event of
    TApplicationEvent.BecameActive:
      UDPTimer.Enabled := True;
    TApplicationEvent.WillBecomeInactive:
      UDPTimer.Enabled := False;
  end;
}
end;

procedure TPushUDP.UDPTimerTimer(Sender: TObject);
begin
  if not FToken.IsEmpty and not FDeviceID.IsEmpty then
    SendDeviceInfo;
end;

procedure TPushUDP.SendDeviceInfo;
var
  LJSON: TJSONObject;
  LOS, LChannelId: string;
begin
  if TOSVersion.Platform = TOSVersion.TPlatform.pfiOS then
    LOS := 'IOS'
  else if TOSVersion.Platform = TOSVersion.TPlatform.pfAndroid then
    LOS := 'Android'
  else
    LOS := 'Unknown';
  TOSMetadata.GetValue(cMetadataFCMDefaultChannelId, LChannelId);
  LJSON := TJSONObject.Create;
  try
    LJSON.AddPair('deviceid', FDeviceID);
    LJSON.AddPair('token', FToken);
    LJSON.AddPair('channelid', LChannelId);
    LJSON.AddPair('os', LOS);
    UDPClient.Broadcast(LJSON.ToJSON, UDPClient.Port);
  finally
    LJSON.Free;
  end;
end;

procedure TPushUDP.UpdateDeviceInfo(const ADeviceID, AToken: string);
begin
  FDeviceID := ADeviceID;
  FToken := AToken;
end;

end.
