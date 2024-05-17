unit DW.StartUpHook.Android;

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

implementation

uses
  // RTL
  System.Messaging,
  // Android
  Androidapi.AppGlue, Androidapi.NativeActivity,
  // DW
  DW.Messaging;

type
  TStartUpHook = class(TObject)
  private
    class var FInstance: TStartUpHook;
    class constructor CreateClass;
    class destructor DestroyClass;
  private
    FSavedApplicationCommandEventHandler: TOnApplicationCommand;
    procedure ApplicationCommandEventHandler(const AAppGlue: TAndroidApplicationGlue; const ACommand: TAndroidApplicationCommand);
    procedure DoApplicationCommandEvent(const AAppGlue: TAndroidApplicationGlue; const ACommand: TAndroidApplicationCommand);
    procedure HookApplicationCommandEvent;
    procedure UnhookApplicationCommandEvent;
  public
    constructor Create;
    destructor Destroy; override;
  end;

class constructor TStartUpHook.CreateClass;
begin
  FInstance := TStartUpHook.Create;
end;

class destructor TStartUpHook.DestroyClass;
begin
  FInstance.Free;
end;

constructor TStartUpHook.Create;
begin
  inherited;
  HookApplicationCommandEvent;
end;

destructor TStartUpHook.Destroy;
begin
  UnhookApplicationCommandEvent;
  inherited;
end;

procedure TStartUpHook.HookApplicationCommandEvent;
var
  LAppGlue: TAndroidApplicationGlue;
begin
  if System.DelphiActivity <> nil then
  begin
    LAppGlue := PANativeActivity(System.DelphiActivity)^.instance;
    FSavedApplicationCommandEventHandler := LAppGlue.OnApplicationCommandEvent;
    LAppGlue.OnApplicationCommandEvent := ApplicationCommandEventHandler;
  end;
end;

procedure TStartUpHook.UnhookApplicationCommandEvent;
var
  LAppGlue: TAndroidApplicationGlue;
begin
  if System.DelphiActivity <> nil then
  begin
    LAppGlue := PANativeActivity(System.DelphiActivity)^.instance;
    LAppGlue.OnApplicationCommandEvent := FSavedApplicationCommandEventHandler;
    FSavedApplicationCommandEventHandler := nil;
  end;
end;

procedure TStartUpHook.DoApplicationCommandEvent(const AAppGlue: TAndroidApplicationGlue; const ACommand: TAndroidApplicationCommand);
begin
  if Assigned(FSavedApplicationCommandEventHandler) then
    FSavedApplicationCommandEventHandler(AAppGlue, ACommand);
end;

procedure TStartUpHook.ApplicationCommandEventHandler(const AAppGlue: TAndroidApplicationGlue; const ACommand: TAndroidApplicationCommand);
begin
  try
    if ACommand in [TAndroidApplicationCommand.GainedFocus, TAndroidApplicationCommand.LostFocus] then
      TMessageManager.DefaultManager.SendMessage(Self, TWindowFocusChangedMessage.Create(ACommand = TAndroidApplicationCommand.GainedFocus), True);
  finally
    DoApplicationCommandEvent(AAppGlue, ACommand);
  end;
end;

end.
