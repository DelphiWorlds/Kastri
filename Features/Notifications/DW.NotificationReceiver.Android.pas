unit DW.NotificationReceiver;

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
  // Android
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes,
  // DW
  DW.MultiReceiver.Android;

type
  TNotificationReceiver = class(TMultiReceiver)
  protected
    procedure Receive(context: JContext; intent: JIntent); override;
    procedure ConfigureActions; override;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.Helpers, Androidapi.JNIBridge, Androidapi.JNI.App,
  // DW
  DW.Androidapi.JNI.DWMultiBroadcastReceiver;

function NotificationManager: JNotificationManager;
var
  LObject: JObject;
begin
  LObject := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.NOTIFICATION_SERVICE);
  Result := TJNotificationManager.Wrap((LObject as ILocalObject).GetObjectID);
end;

{ TNotificationReceiver }

procedure TNotificationReceiver.ConfigureActions;
begin
  IntentFilter.addAction(TJDWMultiBroadcastReceiver.JavaClass.ACTION_NOTIFICATION);
end;

procedure TNotificationReceiver.Receive(context: JContext; intent: JIntent);
var
  LNotification: JNotification;
begin
  LNotification := TJNotification.Wrap(intent.getParcelableExtra(TJDWMultiBroadcastReceiver.JavaClass.EXTRA_NOTIFICATION));
  NotificationManager.notify(intent.getIntExtra(TJDWMultiBroadcastReceiver.JavaClass.EXTRA_NOTIFICATION_ID, 0), LNotification);
end;

end.
