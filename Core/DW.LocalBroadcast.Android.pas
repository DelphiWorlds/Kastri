unit DW.LocalBroadcast.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2026 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

type
  ILocalBroadcast = interface(IInterface)
    ['{B066EDC4-5B0A-45E0-929B-E57E7A57FCDE}']
    procedure SendCommand(const ACommand: string);
    procedure SendMessage(const AMessage: string);
    procedure SendState(const AState: Integer);
  end;

var
  LocalBroadcast: ILocalBroadcast;

implementation

uses
  // Android
  Androidapi.Helpers, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes,
  // DW
  DW.Androidapi.JNI.AndroidX.LocalBroadcastManager, DW.Consts.Android;

type
  TLocalBroadcast = class(TInterfacedObject, ILocalBroadcast)
  private
    FLocalBroadcastManager: JLocalBroadcastManager;
    procedure Send(const AAction, AParam, AValue: string);
  public
    { ILocalBroadcast }
    procedure SendCommand(const ACommand: string);
    procedure SendMessage(const AMessage: string);
    procedure SendState(const AState: Integer);
  public
    constructor Create;
  end;

{ TLocalBroadcast }

constructor TLocalBroadcast.Create;
begin
  inherited;
  FLocalBroadcastManager := TJLocalBroadcastManager.JavaClass.getInstance(TAndroidHelper.Context);
end;

procedure TLocalBroadcast.Send(const AAction, AParam, AValue: string);
var
  LIntent: JIntent;
begin
  LIntent := TJIntent.JavaClass.init(StringToJString(AAction));
  LIntent.putExtra(StringToJString(AParam), StringToJString(AValue));
  FLocalBroadcastManager.sendBroadcast(LIntent);
end;

procedure TLocalBroadcast.SendCommand(const ACommand: string);
begin
  Send(cServiceCommandAction, cServiceBroadcastParamCommand, ACommand);
end;

procedure TLocalBroadcast.SendMessage(const AMessage: string);
begin
  Send(cServiceMessageAction, cServiceBroadcastParamMessage, AMessage);
end;

procedure TLocalBroadcast.SendState(const AState: Integer);
var
  LIntent: JIntent;
begin
  LIntent := TJIntent.JavaClass.init(StringToJString(cServiceStateAction));
  LIntent.putExtra(StringToJString(cServiceBroadcastParamState), AState);
  FLocalBroadcastManager.sendBroadcast(LIntent);
end;

initialization
  LocalBroadcast := TLocalBroadcast.Create;

end.
