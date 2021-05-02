unit DW.Androidapi.JNI.SupportV4;

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
  Androidapi.JNIBridge, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.JNI.Net, Androidapi.JNI.Os,
  Androidapi.JNI.Support, Androidapi.JNI.App, Androidapi.JNI.Widget;

type
  JFileProvider = interface;
  JLocalBroadcastManager = interface;
  JNotificationCompat_Builder = interface;

  JFileProviderClass = interface(JContentProviderClass)
    ['{33A87969-5731-4791-90F6-3AD22F2BB822}']
    {class} function getUriForFile(context: JContext; authority: JString; _file: JFile): Jnet_Uri; cdecl;
    {class} function init: JFileProvider; cdecl;
  end;

  [JavaSignature('android/support/v4/content/FileProvider')]
  JFileProvider = interface(JContentProvider)
    ['{12F5DD38-A3CE-4D2E-9F68-24933C9D221B}']
    procedure attachInfo(context: JContext; info: JProviderInfo); cdecl;
    function delete(uri: Jnet_Uri; selection: JString; selectionArgs: TJavaObjectArray<JString>): Integer; cdecl;
    function getType(uri: Jnet_Uri): JString; cdecl;
    function insert(uri: Jnet_Uri; values: JContentValues): Jnet_Uri; cdecl;
    function onCreate: Boolean; cdecl;
    function openFile(uri: Jnet_Uri; mode: JString): JParcelFileDescriptor; cdecl;
    function query(uri: Jnet_Uri; projection: TJavaObjectArray<JString>; selection: JString; selectionArgs: TJavaObjectArray<JString>;
      sortOrder: JString): JCursor; cdecl;
    function update(uri: Jnet_Uri; values: JContentValues; selection: JString; selectionArgs: TJavaObjectArray<JString>): Integer; cdecl;
  end;
  TJFileProvider = class(TJavaGenericImport<JFileProviderClass, JFileProvider>) end;

  JLocalBroadcastManagerClass = interface(JObjectClass)
    ['{5CCF81B2-E170-47C4-873E-32E644085880}']
    {class} function getInstance(context: JContext): JLocalBroadcastManager; cdecl;
  end;

  [JavaSignature('android/support/v4/content/LocalBroadcastManager')]
  JLocalBroadcastManager = interface(JObject)
    ['{B5D9B2DA-E150-4CC5-BBDA-58FCD42C6C1E}']
    procedure registerReceiver(receiver: JBroadcastReceiver; filter: JIntentFilter); cdecl;
    function sendBroadcast(intent: JIntent): Boolean; cdecl;
    procedure sendBroadcastSync(intent: JIntent); cdecl;
    procedure unregisterReceiver(receiver: JBroadcastReceiver); cdecl;
  end;
  TJLocalBroadcastManager = class(TJavaGenericImport<JLocalBroadcastManagerClass, JLocalBroadcastManager>) end;

  JNotificationCompat_BuilderClass = interface(JObjectClass)
    ['{6EC74C2C-EBCC-4A55-98B6-6DD36DE3BA8C}']
    {class} function init(context: JContext): JNotificationCompat_Builder; cdecl; overload;
    {class} function init(context: JContext; channelId: JString): JNotificationCompat_Builder; cdecl; overload;
  end;

  [JavaSignature('android/support/v4/app/NotificationCompat$Builder')]
  JNotificationCompat_Builder = interface(Androidapi.JNI.Support.JNotificationCompat_Builder)
    ['{81FD10B1-0D7F-4F6E-BF92-6A74F52C424C}']
    function addAction(icon: Integer; title: JCharSequence; intent: JPendingIntent): JNotificationCompat_Builder; cdecl;
    function build: JNotification; cdecl;
    function getNotification: JNotification; cdecl;//Deprecated
    function setAutoCancel(autoCancel: Boolean): JNotificationCompat_Builder; cdecl;
    function setChannelId(channelId: JString): JNotificationCompat_Builder; cdecl;
    function setContent(views: JRemoteViews): JNotificationCompat_Builder; cdecl;
    function setContentInfo(info: JCharSequence): JNotificationCompat_Builder; cdecl;
    function setContentIntent(intent: JPendingIntent): JNotificationCompat_Builder; cdecl;
    function setContentText(text: JCharSequence): JNotificationCompat_Builder; cdecl;
    function setContentTitle(title: JCharSequence): JNotificationCompat_Builder; cdecl;
    function setDefaults(defaults: Integer): JNotificationCompat_Builder; cdecl;
    function setDeleteIntent(intent: JPendingIntent): JNotificationCompat_Builder; cdecl;
    function setFullScreenIntent(intent: JPendingIntent; highPriority: Boolean): JNotificationCompat_Builder; cdecl;
    function setLargeIcon(icon: JBitmap): JNotificationCompat_Builder; cdecl;
    function setLights(argb: Integer; onMs: Integer; offMs: Integer): JNotificationCompat_Builder; cdecl;
    function setNumber(number: Integer): JNotificationCompat_Builder; cdecl;
    function setOngoing(ongoing: Boolean): JNotificationCompat_Builder; cdecl;
    function setOnlyAlertOnce(onlyAlertOnce: Boolean): JNotificationCompat_Builder; cdecl;
    function setPriority(pri: Integer): JNotificationCompat_Builder; cdecl;
    function setProgress(max: Integer; progress: Integer; indeterminate: Boolean): JNotificationCompat_Builder; cdecl;
    function setSmallIcon(icon: Integer): JNotificationCompat_Builder; cdecl; overload;
    function setSmallIcon(icon: Integer; level: Integer): JNotificationCompat_Builder; cdecl; overload;
    function setSound(sound: Jnet_Uri): JNotificationCompat_Builder; cdecl; overload;
    function setSound(sound: Jnet_Uri; streamType: Integer): JNotificationCompat_Builder; cdecl; overload;
    function setStyle(style: JNotificationCompat_Style): JNotificationCompat_Builder; cdecl;
    function setSubText(text: JCharSequence): JNotificationCompat_Builder; cdecl;
    function setTicker(tickerText: JCharSequence): JNotificationCompat_Builder; cdecl; overload;
    function setTicker(tickerText: JCharSequence; views: JRemoteViews): JNotificationCompat_Builder; cdecl; overload;
    function setUsesChronometer(b: Boolean): JNotificationCompat_Builder; cdecl;
    function setVibrate(pattern: TJavaArray<Int64>): JNotificationCompat_Builder; cdecl;
    function setWhen(when: Int64): JNotificationCompat_Builder; cdecl;
  end;
  TJNotificationCompat_Builder = class(TJavaGenericImport<JNotificationCompat_BuilderClass, JNotificationCompat_Builder>) end;

implementation

end.

