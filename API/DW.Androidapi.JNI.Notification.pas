unit DW.Androidapi.JNI.Notification;

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
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.App, Androidapi.JNI.Os, Androidapi.JNI.Net, Androidapi.JNI.PlayServices,
  Androidapi.JNI.Widget, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Media;

type
  JNotification_Builder = interface;
  JNotification_DecoratedCustomViewStyle = interface;
  JNotification_Extender = interface;
  JNotification_MediaStyle = interface;
  JNotification_Style = interface;

  JNotification_BuilderClass = interface(JObjectClass)
    ['{78AE5E21-5CF0-4460-AC2C-EE1ADADCFE4B}']
    function init(context: JContext): JNotification_Builder; cdecl; overload;
    function init(context: JContext; channelId: JString): JNotification_Builder; cdecl; overload;
  end;

  [JavaSignature('android/app/Notification$Builder')]
  JNotification_Builder = interface(JObject)
    ['{AE3F89ED-03AE-49BD-9AC6-2880216AE4A4}']
    function addAction(action: JNotification_Action): JNotification_Builder; cdecl; overload;
    function addAction(icon: Integer; title: JCharSequence; intent: JPendingIntent): JNotification_Builder;  cdecl; overload;
    function addExtras(extras: JBundle): JNotification_Builder; cdecl;
    {$IF CompilerVersion < 35}
    function addPerson(person: JPerson): JNotification_Builder; cdecl; overload;
    {$ENDIF}
    function addPerson(uri: JString): JNotification_Builder;  cdecl; overload;
    function build: JNotification; cdecl;
    function createBigContentView: JRemoteViews; cdecl;
    function createContentView: JRemoteViews; cdecl;
    function createHeadsUpContentView: JRemoteViews; cdecl;
    function extend(extender: JNotification_Extender): JNotification_Builder; cdecl;
    function getExtras: JBundle; cdecl;
    function getNotification: JNotification;  cdecl;
    function getStyle: JNotification_Style; cdecl;
    function setAutoCancel(autoCancel: boolean): JNotification_Builder; cdecl;
    function setBadgeIconType(icon: Integer): JNotification_Builder; cdecl;
    function setCategory(category: JString): JNotification_Builder; cdecl;
    function setChannelId(channelId: JString): JNotification_Builder; cdecl;
    function setChronometerCountDown(countDown: boolean): JNotification_Builder; cdecl;
    function setColor(argb: Integer): JNotification_Builder; cdecl;
    function setColorized(colorize: boolean): JNotification_Builder; cdecl;
    function setContent(views: JRemoteViews): JNotification_Builder;  cdecl;
    function setContentInfo(info: JCharSequence): JNotification_Builder;  cdecl;
    function setContentIntent(intent: JPendingIntent): JNotification_Builder; cdecl;
    function setContentText(text: JCharSequence): JNotification_Builder; cdecl;
    function setContentTitle(title: JCharSequence): JNotification_Builder; cdecl;
    function setCustomBigContentView(contentView: JRemoteViews): JNotification_Builder; cdecl;
    function setCustomContentView(contentView: JRemoteViews): JNotification_Builder; cdecl;
    function setCustomHeadsUpContentView(contentView: JRemoteViews): JNotification_Builder; cdecl;
    function setDefaults(defaults: Integer): JNotification_Builder;  cdecl;
    function setDeleteIntent(intent: JPendingIntent): JNotification_Builder; cdecl;
    function setExtras(extras: JBundle): JNotification_Builder; cdecl;
    function setFullScreenIntent(intent: JPendingIntent; highPriority: boolean): JNotification_Builder; cdecl;
    function setGroup(groupKey: JString): JNotification_Builder; cdecl;
    function setGroupAlertBehavior(groupAlertBehavior: Integer): JNotification_Builder; cdecl;
    function setGroupSummary(isGroupSummary: boolean): JNotification_Builder; cdecl;
    function setLargeIcon(b: JBitmap): JNotification_Builder; cdecl; overload;
    function setLargeIcon(icon: JIcon): JNotification_Builder; cdecl; overload;
    function setLights(argb: Integer; onMs: Integer; offMs: Integer): JNotification_Builder;  cdecl;
    function setLocalOnly(localOnly: boolean): JNotification_Builder; cdecl;
    function setNumber(number: Integer): JNotification_Builder; cdecl;
    function setOngoing(ongoing: boolean): JNotification_Builder; cdecl;
    function setOnlyAlertOnce(onlyAlertOnce: boolean): JNotification_Builder; cdecl;
    function setPriority(pri: Integer): JNotification_Builder;  cdecl;
    function setProgress(max: Integer; progress: Integer; indeterminate: boolean): JNotification_Builder; cdecl;
    function setPublicVersion(n: JNotification): JNotification_Builder; cdecl;
    function setRemoteInputHistory(text: TJavaArray<JCharSequence>): JNotification_Builder; cdecl;
    function setSettingsText(text: JCharSequence): JNotification_Builder; cdecl;
    function setShortcutId(shortcutId: JString): JNotification_Builder; cdecl;
    function setShowWhen(show: boolean): JNotification_Builder; cdecl;
    function setSmallIcon(icon: Integer): JNotification_Builder; cdecl; overload;
    function setSmallIcon(icon: Integer; level: Integer): JNotification_Builder; cdecl; overload;
    function setSmallIcon(icon: JIcon): JNotification_Builder; cdecl; overload;
    function setSortKey(sortKey: JString): JNotification_Builder; cdecl;
    function setSound(sound: Jnet_Uri): JNotification_Builder;  cdecl; overload;
    function setSound(sound: Jnet_Uri; audioAttributes: JAudioAttributes): JNotification_Builder;  cdecl; overload;
    function setSound(sound: Jnet_Uri; streamType: Integer): JNotification_Builder;  cdecl; overload;
    function setStyle(style: JNotification_Style): JNotification_Builder; cdecl;
    function setSubText(text: JCharSequence): JNotification_Builder; cdecl;
    function setTicker(tickerText: JCharSequence): JNotification_Builder; cdecl; overload;
    function setTicker(tickerText: JCharSequence; views: JRemoteViews): JNotification_Builder;  cdecl; overload;
    function setTimeoutAfter(durationMs: Int64): JNotification_Builder; cdecl;
    function setUsesChronometer(b: boolean): JNotification_Builder; cdecl;
    function setVibrate(pattern: TJavaArray<Int64>): JNotification_Builder;  cdecl;
    function setVisibility(visibility: Integer): JNotification_Builder; cdecl;
    function setWhen(when: Int64): JNotification_Builder; cdecl;
  end;
  TJNotification_Builder = class(TJavaGenericImport<JNotification_BuilderClass, JNotification_Builder>)
  end;

  JNotification_StyleClass = interface(JObjectClass)
    ['{4D4881BB-8E17-4B71-9698-7244D2DDA12D}']
  end;

  [JavaSignature('android/app/Notification$Style')]
  JNotification_Style = interface(JObject)
    ['{EFCA32F1-62A3-4983-AB8C-B30DA820F905}']
    function build: JNotification; cdecl;
    procedure setBuilder(builder: JNotification_Builder); cdecl;
  end;
  TJNotification_Style = class(TJavaGenericImport<JNotification_StyleClass, JNotification_Style>)
  end;

  JNotification_ExtenderClass = interface(JObjectClass)
    ['{A81B521E-99E8-4DC8-8A4E-758EBB96ED22}']
  end;

  [JavaSignature('android/app/Notification$Extender')]
  JNotification_Extender = interface(JObject)
    ['{53C8BD5E-BE87-4C75-971B-7E144EE9D7F0}']
    function extend(JNotification_Builderparam0 : JNotification_Builder) : JNotification_Builder; cdecl;
  end;
  TJNotification_Extender = class(TJavaGenericImport<JNotification_ExtenderClass, JNotification_Extender>)
  end;

  JNotification_MediaStyleClass = interface(JNotification_StyleClass)
    ['{14424BCB-096D-423C-81B0-880AF96A4516}']
    function init: JNotification_MediaStyle; cdecl; overload;
    function init(builder: JNotification_Builder): JNotification_MediaStyle; cdecl; overload;
  end;

  [JavaSignature('android/app/Notification$MediaStyle')]
  JNotification_MediaStyle = interface(JNotification_Style)
    ['{7F781850-B629-4E54-9399-02409828B3FF}']
    function setMediaSession(token: JMediaSession_Token): JNotification_MediaStyle; cdecl;
    function setShowActionsInCompactView(actions: TJavaArray<Integer>): JNotification_MediaStyle; cdecl;
  end;
  TJNotification_MediaStyle = class(TJavaGenericImport<JNotification_MediaStyleClass, JNotification_MediaStyle>)
  end;

  // API 24+
  JNotification_DecoratedCustomViewStyleClass = interface(JNotification_StyleClass)
    ['{1ACB123B-6A4D-4D13-90C3-1F5CF8806FA6}']
    function init: JNotification_DecoratedCustomViewStyle; cdecl; overload;
  end;

  [JavaSignature('android/app/Notification$DecoratedCustomViewStyle')]
  JNotification_DecoratedCustomViewStyle = interface(JNotification_Style)
    ['{D19D754C-F8DF-4261-A9C8-173347D26DA2}']
  end;
  TJNotification_DecoratedCustomViewStyle = class(TJavaGenericImport<JNotification_DecoratedCustomViewStyleClass, JNotification_DecoratedCustomViewStyle>)
  end;

implementation

end.
