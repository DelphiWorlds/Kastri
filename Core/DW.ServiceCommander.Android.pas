unit DW.ServiceCommander.Android;

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
  System.Messaging;

type
  TServiceCommander = class(TObject)
  private
    class var FCommander: TServiceCommander;
    class var FIsRequestingPermissions: Boolean;
    class var FServiceName: string;
    class constructor CreateClass;
    class destructor DestroyClass;
  private
    procedure ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
  public
    class procedure SendCommand(const ACommand: Integer);
    class procedure SendCommandJSON(const AJSON: string);
    class procedure SendMessage(const AText: string);
    /// <summary>
    ///   Attempts to start the service if it is not already running
    /// </summary>
    /// <remarks>
    ///   NOTE: Result is True if the method attempts to start the service - it does not mean it succeeded in starting the service
    /// </remarks>
    class function StartService(AServiceName: string): Boolean;
    class property IsRequestingPermissions: Boolean read FIsRequestingPermissions write FIsRequestingPermissions;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  DW.OSLog,
  // RTL
  System.SysUtils, System.Android.Service, System.IOUtils,
  // Android
  Androidapi.JNI.GraphicsContentViewText, Androidapi.Helpers, AndroidApi.JNI.JavaTypes,
  // FMX
  FMX.Platform,
  // DW
  {$IF CompilerVersion < 35} DW.Androidapi.JNI.SupportV4, {$ELSE} DW.Androidapi.JNI.Androidx.LocalBroadcastManager, {$ENDIF}
  DW.Android.Helpers, DW.Consts.Android;

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
    begin
      if FIsRequestingPermissions then
        SendCommand(cServiceCommandAppIsRequestingPermissions)
      else
        SendCommand(cServiceCommandAppEnteredBackground);
    end;
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

class procedure TServiceCommander.SendCommandJSON(const AJSON: string);
var
  LIntent: JIntent;
begin
  LIntent := TJIntent.JavaClass.init(StringToJString(cServiceCommandJSONAction));
  LIntent.putExtra(StringToJString(cServiceBroadcastParamCommand), StringToJString(AJSON));
  TJLocalBroadcastManager.JavaClass.getInstance(TAndroidHelper.Context).sendBroadcast(LIntent);
end;

class procedure TServiceCommander.SendMessage(const AText: string);
var
  LIntent: JIntent;
begin
  LIntent := TJIntent.JavaClass.init(StringToJString(cServiceMessageAction));
  LIntent.putExtra(StringToJString(cServiceBroadcastParamMessage), StringToJString(AText));
  TJLocalBroadcastManager.JavaClass.getInstance(TAndroidHelper.Context).sendBroadcast(LIntent);
end;

class function TServiceCommander.StartService(AServiceName: string): Boolean;
begin
  Result := False;
  FServiceName := AServiceName;
  if not AServiceName.StartsWith(cEMBTJavaServicePrefix) then
    AServiceName := cEMBTJavaServicePrefix + AServiceName;
  if not TAndroidHelperEx.IsServiceRunning(AServiceName) then
  begin
    TOSLog.d('Starting %s', [AServiceName]);
    TLocalServiceConnection.StartService(AServiceName);
    Result := True;
  end
  else
    TOSLog.d('Service %s is already running', [AServiceName]);
end;

end.
