unit DW.ServiceCommander.Android;

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
  System.Messaging;

type
  TServiceCommander = class(TObject)
  private
    class var FCommander: TServiceCommander;
    class var FServiceName: string;
    class constructor CreateClass;
    class destructor DestroyClass;
  private
    procedure ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
  public
    class procedure SendCommand(const ACommand: Integer);
    class procedure StartService(AServiceName: string);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.Android.Service,
  // Android
  Androidapi.JNI.GraphicsContentViewText, Androidapi.Helpers, AndroidApi.JNI.JavaTypes,
  // FMX
  FMX.Platform,
  // DW
  DW.Androidapi.JNI.SupportV4, DW.Android.Helpers, DW.Consts.Android;

class constructor TServiceCommander.CreateClass;
begin
  FCommander := TServiceCommander.Create;
end;

class destructor TServiceCommander.DestroyClass;
begin
  FCommander.Free;
end;

constructor TServiceCommander.Create;
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
end;

destructor TServiceCommander.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  inherited;
end;

procedure TServiceCommander.ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
var
  LMessage: TApplicationEventMessage;
begin
  LMessage := TApplicationEventMessage(M);
  case LMessage.Value.Event of
    TApplicationEvent.BecameActive:
      SendCommand(cServiceCommandAppBecameActive);
    TApplicationEvent.EnteredBackground:
      SendCommand(cServiceCommandAppEnteredBackground);
  end;
end;

class procedure TServiceCommander.SendCommand(const ACommand: Integer);
var
  LIntent: JIntent;
begin
  LIntent := TJIntent.JavaClass.init(StringToJString(cServiceCommandAction));
  LIntent.putExtra(StringToJString(cServiceBroadcastParamCommand), ACommand);
  TJLocalBroadcastManager.JavaClass.getInstance(TAndroidHelper.Context).sendBroadcast(LIntent);
end;

class procedure TServiceCommander.StartService(AServiceName: string);
begin
  FServiceName := AServiceName;
  if not AServiceName.StartsWith(cEMBTJavaServicePrefix) then
    AServiceName := cEMBTJavaServicePrefix + AServiceName;
  if not TAndroidHelperEx.IsServiceRunning(AServiceName) then
    TLocalServiceConnection.StartService(AServiceName);
end;

end.
