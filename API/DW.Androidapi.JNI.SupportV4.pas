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
  {$IF CompilerVersion > 34}
  Japp_RemoteInput = interface;
  JNotificationCompat_Action = interface;
  JNotificationCompat_Builder = interface;
  JNotificationCompat_Extender = interface;
  JNotificationCompat_Style = interface;
  {$ENDIF}

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

  {$IF CompilerVersion > 34}
  JNotificationCompat_ExtenderClass = interface(IJavaClass)
    ['{FE44EF36-1DB6-47B7-AA66-5F91FE5134C5}']
  end;

  [JavaSignature('android/support/v4/app/NotificationCompat$Extender')]
  JNotificationCompat_Extender = interface(IJavaInstance)
    ['{C96F5828-B7E4-48EC-9A8C-8B8518A347BB}']
    function extend(builder: JNotificationCompat_Builder): JNotificationCompat_Builder; cdecl;
  end;
  TJNotificationCompat_Extender = class(TJavaGenericImport<JNotificationCompat_ExtenderClass, JNotificationCompat_Extender>) end;

  JNotificationCompat_StyleClass = interface(JObjectClass)
    ['{A76478B0-8BCB-4AFA-AFCD-CB0460219CDA}']
    {class} function init: JNotificationCompat_Style; cdecl;
  end;

  [JavaSignature('android/support/v4/app/NotificationCompat$Style')]
  JNotificationCompat_Style = interface(JObject)
    ['{5C782C73-8C4B-4ADA-994D-4293E0D2D282}']
    function build: JNotification; cdecl;
    procedure setBuilder(builder: JNotificationCompat_Builder); cdecl;
  end;
  TJNotificationCompat_Style = class(TJavaGenericImport<JNotificationCompat_StyleClass, JNotificationCompat_Style>) end;

  Japp_RemoteInputClass = interface(JObjectClass)
    ['{2D5C5EF8-294E-4B0F-A648-9D1EA5BED891}']
    {class} function _GetEXTRA_RESULTS_DATA: JString; cdecl;
    {class} function _GetRESULTS_CLIP_LABEL: JString; cdecl;
    {class} procedure addDataResultToIntent(remoteInput: Japp_RemoteInput; intent: JIntent; results: JMap); cdecl;
    {class} procedure addResultsToIntent(remoteInputs: TJavaObjectArray<Japp_RemoteInput>; intent: JIntent; results: JBundle); cdecl;
    {class} function getDataResultsFromIntent(intent: JIntent; remoteInputResultKey: JString): JMap; cdecl;
    {class} function getResultsFromIntent(intent: JIntent): JBundle; cdecl;
    {class} property EXTRA_RESULTS_DATA: JString read _GetEXTRA_RESULTS_DATA;
    {class} property RESULTS_CLIP_LABEL: JString read _GetRESULTS_CLIP_LABEL;
  end;

  [JavaSignature('android/support/v4/app/RemoteInput')]
  Japp_RemoteInput = interface(JObject)
    ['{728FBA2D-C0B9-45AC-A5DD-24B89188687E}']
    function getAllowFreeFormInput: Boolean; cdecl;
    function getAllowedDataTypes: JSet; cdecl;
    function getChoices: TJavaObjectArray<JCharSequence>; cdecl;
    function getExtras: JBundle; cdecl;
    function getLabel: JCharSequence; cdecl;
    function getResultKey: JString; cdecl;
    function isDataOnly: Boolean; cdecl;
  end;
  TJapp_RemoteInput = class(TJavaGenericImport<Japp_RemoteInputClass, Japp_RemoteInput>) end;

  JNotificationCompat_ActionClass = interface(JObjectClass)
    ['{9A2975B1-7BCA-4120-BAE9-A99F23F48159}']
    {class} function init(icon: Integer; title: JCharSequence; intent: JPendingIntent): JNotificationCompat_Action; cdecl;
  end;

  [JavaSignature('android/support/v4/app/NotificationCompat$Action')]
  JNotificationCompat_Action = interface(JObject)
    ['{2C05FC19-2838-47F8-97B0-44836566B08D}']
    function _GetactionIntent: JPendingIntent; cdecl;
    procedure _SetactionIntent(Value: JPendingIntent); cdecl;
    function _Geticon: Integer; cdecl;
    procedure _Seticon(Value: Integer); cdecl;
    function _Gettitle: JCharSequence; cdecl;
    procedure _Settitle(Value: JCharSequence); cdecl;
    function getActionIntent: JPendingIntent; cdecl;
    function getAllowGeneratedReplies: Boolean; cdecl;
    function getDataOnlyRemoteInputs: TJavaObjectArray<Japp_RemoteInput>; cdecl;
    function getExtras: JBundle; cdecl;
    function getIcon: Integer; cdecl;
    function getRemoteInputs: TJavaObjectArray<Japp_RemoteInput>; cdecl;
    function getTitle: JCharSequence; cdecl;
    property actionIntent: JPendingIntent read _GetactionIntent write _SetactionIntent;
    property icon: Integer read _Geticon write _Seticon;
    property title: JCharSequence read _Gettitle write _Settitle;
  end;
  TJNotificationCompat_Action = class(TJavaGenericImport<JNotificationCompat_ActionClass, JNotificationCompat_Action>) end;

  JNotificationCompat_BuilderClass = interface(JObjectClass)
    ['{6EC74C2C-EBCC-4A55-98B6-6DD36DE3BA8C}']
    {class} function init(context: JContext; channelId: JString): JNotificationCompat_Builder; cdecl; overload;
    {class} function init(context: JContext): JNotificationCompat_Builder; cdecl; overload;//Deprecated
  end;

  [JavaSignature('android/support/v4/app/NotificationCompat$Builder')]
  JNotificationCompat_Builder = interface(JObject)
    ['{7DE9C385-1C34-413C-9E85-D8FA90028065}']
    function _GetmPeople: JArrayList; cdecl;
    procedure _SetmPeople(Value: JArrayList); cdecl;
    function addAction(icon: Integer; title: JCharSequence; intent: JPendingIntent): JNotificationCompat_Builder; cdecl; overload;
    function addAction(action: JNotificationCompat_Action): JNotificationCompat_Builder; cdecl; overload;
    function addExtras(extras: JBundle): JNotificationCompat_Builder; cdecl;
    function addPerson(uri: JString): JNotificationCompat_Builder; cdecl;
    function build: JNotification; cdecl;
    function extend(extender: JNotificationCompat_Extender): JNotificationCompat_Builder; cdecl;
    function getExtras: JBundle; cdecl;
    function getNotification: JNotification; cdecl;//Deprecated
    function setAutoCancel(autoCancel: Boolean): JNotificationCompat_Builder; cdecl;
    function setBadgeIconType(icon: Integer): JNotificationCompat_Builder; cdecl;
    function setCategory(category: JString): JNotificationCompat_Builder; cdecl;
    function setChannel(channelId: JString): JNotificationCompat_Builder; cdecl;//Deprecated
    function setChannelId(channelId: JString): JNotificationCompat_Builder; cdecl;
    function setColor(argb: Integer): JNotificationCompat_Builder; cdecl;
    function setColorized(colorize: Boolean): JNotificationCompat_Builder; cdecl;
    function setContent(views: JRemoteViews): JNotificationCompat_Builder; cdecl;
    function setContentInfo(info: JCharSequence): JNotificationCompat_Builder; cdecl;
    function setContentIntent(intent: JPendingIntent): JNotificationCompat_Builder; cdecl;
    function setContentText(text: JCharSequence): JNotificationCompat_Builder; cdecl;
    function setContentTitle(title: JCharSequence): JNotificationCompat_Builder; cdecl;
    function setCustomBigContentView(contentView: JRemoteViews): JNotificationCompat_Builder; cdecl;
    function setCustomContentView(contentView: JRemoteViews): JNotificationCompat_Builder; cdecl;
    function setCustomHeadsUpContentView(contentView: JRemoteViews): JNotificationCompat_Builder; cdecl;
    function setDefaults(defaults: Integer): JNotificationCompat_Builder; cdecl;
    function setDeleteIntent(intent: JPendingIntent): JNotificationCompat_Builder; cdecl;
    function setExtras(extras: JBundle): JNotificationCompat_Builder; cdecl;
    function setFullScreenIntent(intent: JPendingIntent; highPriority: Boolean): JNotificationCompat_Builder; cdecl;
    function setGroup(groupKey: JString): JNotificationCompat_Builder; cdecl;
    function setGroupAlertBehavior(groupAlertBehavior: Integer): JNotificationCompat_Builder; cdecl;
    function setGroupSummary(isGroupSummary: Boolean): JNotificationCompat_Builder; cdecl;
    function setLargeIcon(icon: JBitmap): JNotificationCompat_Builder; cdecl;
    function setLights(argb: Integer; onMs: Integer; offMs: Integer): JNotificationCompat_Builder; cdecl;
    function setLocalOnly(b: Boolean): JNotificationCompat_Builder; cdecl;
    function setNumber(number: Integer): JNotificationCompat_Builder; cdecl;
    function setOngoing(ongoing: Boolean): JNotificationCompat_Builder; cdecl;
    function setOnlyAlertOnce(onlyAlertOnce: Boolean): JNotificationCompat_Builder; cdecl;
    function setPriority(pri: Integer): JNotificationCompat_Builder; cdecl;
    function setProgress(max: Integer; progress: Integer; indeterminate: Boolean): JNotificationCompat_Builder; cdecl;
    function setPublicVersion(n: JNotification): JNotificationCompat_Builder; cdecl;
    function setRemoteInputHistory(text: TJavaObjectArray<JCharSequence>): JNotificationCompat_Builder; cdecl;
    function setShortcutId(shortcutId: JString): JNotificationCompat_Builder; cdecl;
    function setShowWhen(show: Boolean): JNotificationCompat_Builder; cdecl;
    function setSmallIcon(icon: Integer): JNotificationCompat_Builder; cdecl; overload;
    function setSmallIcon(icon: Integer; level: Integer): JNotificationCompat_Builder; cdecl; overload;
    function setSortKey(sortKey: JString): JNotificationCompat_Builder; cdecl;
    function setSound(sound: Jnet_Uri): JNotificationCompat_Builder; cdecl; overload;
    function setSound(sound: Jnet_Uri; streamType: Integer): JNotificationCompat_Builder; cdecl; overload;
    function setStyle(style: JNotificationCompat_Style): JNotificationCompat_Builder; cdecl;
    function setSubText(text: JCharSequence): JNotificationCompat_Builder; cdecl;
    function setTicker(tickerText: JCharSequence): JNotificationCompat_Builder; cdecl; overload;
    function setTicker(tickerText: JCharSequence; views: JRemoteViews): JNotificationCompat_Builder; cdecl; overload;
    function setTimeout(durationMs: Int64): JNotificationCompat_Builder; cdecl;//Deprecated
    function setTimeoutAfter(durationMs: Int64): JNotificationCompat_Builder; cdecl;
    function setUsesChronometer(b: Boolean): JNotificationCompat_Builder; cdecl;
    function setVibrate(pattern: TJavaArray<Int64>): JNotificationCompat_Builder; cdecl;
    function setVisibility(visibility: Integer): JNotificationCompat_Builder; cdecl;
    function setWhen(when: Int64): JNotificationCompat_Builder; cdecl;
    property mPeople: JArrayList read _GetmPeople write _SetmPeople;
  end;
  TJNotificationCompat_Builder = class(TJavaGenericImport<JNotificationCompat_BuilderClass, JNotificationCompat_Builder>) end;
{$ENDIF}

implementation

end.

