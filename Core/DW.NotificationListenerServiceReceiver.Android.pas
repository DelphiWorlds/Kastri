unit DW.NotificationListenerServiceReceiver.Android;

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

{$SCOPEDENUMS ON}

uses
  // Android
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.App, Androidapi.JNI.JavaTypes, Androidapi.JNIBridge,
  // DW
  DW.MultiReceiver.Android, DW.Androidapi.JNI.NotificationListenerService;

type
  JDWNotificationListenerService = interface;

  JDWNotificationListenerServiceClass = interface(JNotificationListenerServiceClass)
    ['{D7A1E4BA-DBA0-4A58-B7ED-B385C1EF2DBB}']
    {class} function _GetACTION_NOTIFICATION_POSTED: JString; cdecl;
    {class} function _GetACTION_NOTIFICATION_REMOVED: JString; cdecl;
    {class} function _GetEXTRA_NOTIFICATION: JString; cdecl;
    {class} property ACTION_NOTIFICATION_POSTED: JString read _GetACTION_NOTIFICATION_POSTED;
    {class} property ACTION_NOTIFICATION_REMOVED: JString read _GetACTION_NOTIFICATION_REMOVED;
    {class} property EXTRA_NOTIFICATION: JString read _GetEXTRA_NOTIFICATION;
  end;

  // Needs dw-kastri-base-2.0.0.jar
  [JavaSignature('com/delphiworlds/kastri/DWNotificationListenerService')]
  JDWNotificationListenerService = interface(JNotificationListenerService)
    ['{EB8A070E-7C93-4EAC-8627-046EC5B255A7}']
  end;
  TJDWNotificationListenerService = class(TJavaGenericImport<JDWNotificationListenerServiceClass, JDWNotificationListenerService>)
  end;

  TNotificationEventKind = (Unknown, Posted, Removed);

  TNotificationEvent = procedure(Sender: TObject; const Notification: JNotification; const EventKind: TNotificationEventKind) of object;

  TNotificationListenerServiceReceiver = class(TMultiReceiver)
  private
    FOnNotification: TNotificationEvent;
    procedure DoNotification(const ANotification: JNotification; const AEventKind: TNotificationEventKind);
  protected
    procedure ConfigureActions; override;
    procedure Receive(context: JContext; intent: JIntent); override;
  public
    constructor Create;
    property OnNotification: TNotificationEvent read FOnNotification write FOnNotification;
  end;

implementation

uses
  Androidapi.JNI.Os;

{ TNotificationListenerServiceReceiver }

constructor TNotificationListenerServiceReceiver.Create;
begin
  inherited Create(True);
end;

procedure TNotificationListenerServiceReceiver.ConfigureActions;
begin
  IntentFilter.addAction(TJDWNotificationListenerService.JavaClass.ACTION_NOTIFICATION_POSTED);
  IntentFilter.addAction(TJDWNotificationListenerService.JavaClass.ACTION_NOTIFICATION_REMOVED);
end;

procedure TNotificationListenerServiceReceiver.Receive(context: JContext; intent: JIntent);
var
  LNotificationExtras: JBundle;
  LNotification: JNotification;
  LEventKind: TNotificationEventKind;
begin
  LNotificationExtras := intent.getBundleExtra(TJDWNotificationListenerService.JavaClass.EXTRA_NOTIFICATION);
  if LNotificationExtras <> nil then
  begin
    LEventKind := TNotificationEventKind.Unknown;
    if intent.getAction <> nil then
    begin
      if intent.getAction.equals(TJDWNotificationListenerService.JavaClass.ACTION_NOTIFICATION_POSTED) then
        LEventKind := TNotificationEventKind.Posted
      else if intent.getAction.equals(TJDWNotificationListenerService.JavaClass.ACTION_NOTIFICATION_REMOVED) then
        LEventKind := TNotificationEventKind.Removed;
    end;
    LNotification := TJNotification.JavaClass.init;
    LNotification.extras := LNotificationExtras;
    DoNotification(LNotification, LEventKind);
  end;
end;

procedure TNotificationListenerServiceReceiver.DoNotification(const ANotification: JNotification; const AEventKind: TNotificationEventKind);
begin
  if Assigned(FOnNotification) then
    FOnNotification(Self, ANotification, AEventKind);
end;

end.
