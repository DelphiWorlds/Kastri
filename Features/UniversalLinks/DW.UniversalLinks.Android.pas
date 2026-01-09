unit DW.UniversalLinks.Android;

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

implementation

uses
  // RTL
  System.SysUtils, System.Messaging,
  // Android
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.Helpers, Androidapi.JNI.App,
  // FMX
  FMX.Platform, FMX.Platform.Android,
  // DW
  DW.UniversalLinks;

type
  TPlatformUniversalLinks = class(TObject)
  private
    class var FInstance: TPlatformUniversalLinks;
    class constructor CreateClass;
    class destructor DestroyClass;
  private
    FIntent: JIntent;
    FIsLaunching: Boolean;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
    // function HasAppLinkCategories(const AIntent: JIntent): Boolean;
    function IsViewAction(const AAction: JString): Boolean;
    procedure MessageReceivedNotificationMessageHandler(const Sender: TObject; const AMsg: TMessage);
  public
    constructor Create;
    destructor Destroy; override;
  end;

{ TPlatformUniversalLinks }

class constructor TPlatformUniversalLinks.CreateClass;
begin
  FInstance := TPlatformUniversalLinks.Create;
end;

class destructor TPlatformUniversalLinks.DestroyClass;
begin
  FInstance.Free;
end;

constructor TPlatformUniversalLinks.Create;
begin
  inherited;
  // Intent from launching the app from NOT RUNNING state
  FIntent := TAndroidHelper.Activity.getIntent;
  // Register for intents sent in the RUNNING state with view action
  MainActivity.registerIntentAction(TJIntent.JavaClass.ACTION_VIEW);
  TMessageManager.DefaultManager.SubscribeToMessage(TMessageReceivedNotification, MessageReceivedNotificationMessageHandler);
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
end;

destructor TPlatformUniversalLinks.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TMessageReceivedNotification, MessageReceivedNotificationMessageHandler);
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  inherited;
end;

function TPlatformUniversalLinks.IsViewAction(const AAction: JString): Boolean;
begin
  Result := (AAction <> nil) and AAction.equals(TJIntent.JavaClass.ACTION_VIEW);
end;

procedure TPlatformUniversalLinks.MessageReceivedNotificationMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  // Intent from launching the app from RUNNING state
  FIntent := TMessageReceivedNotification(AMsg).Value;
end;

(*
function TPlatformUniversalLinks.HasAppLinkCategories(const AIntent: JIntent): Boolean;
begin
  Result := AIntent.hasCategory(TJIntent.JavaClass.CATEGORY_DEFAULT) and AIntent.hasCategory(TJIntent.JavaClass.CATEGORY_BROWSABLE);
end;
*)

procedure TPlatformUniversalLinks.ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
var
  LMessage: TApplicationEventMessage;
  LContext: TObject;
begin
  case TApplicationEventMessage(AMsg).Value.Event of
    TApplicationEvent.WillBecomeForeground:
      // App is being launched, or switched to from another app
      FIsLaunching := True;
    TApplicationEvent.BecameActive:
    begin
      if FIsLaunching then
      begin
        FIsLaunching := False;
        if (FIntent <> nil) and (FIntent.getData <> nil) and IsViewAction(FIntent.getAction) { and HasAppLinkCategories(LIntent.getCategories) } then
        begin
          // Intent has view action, and URI data associated with it
          LContext := TOpenApplicationContext.Create(JStringToString(FIntent.getData.toString));
          LMessage := TApplicationEventMessage.Create(TApplicationEventData.Create(TApplicationEvent.OpenURL, LContext));
          TMessageManager.DefaultManager.SendMessage(nil, LMessage);
        end;
      end;
      FIntent := nil;
    end;
  end;
end;

end.
