unit DW.Androidapi.JNI.AndroidX.App;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2023 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // Android
  Androidapi.JNIBridge, Androidapi.Jni.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.App, Androidapi.JNI.Os,
  Androidapi.JNI.Widget, Androidapi.JNI.Net,
  // DW
  DW.Androidapi.JNI.AndroidX.Content;

type
  JNotificationCompat_Action = interface;
  JNotificationCompat_BubbleMetadata = interface;
  JNotificationCompat_Builder = interface;
  JNotificationCompat_DecoratedCustomViewStyle = interface;
  JNotificationCompat_Extender = interface;
  JNotificationCompat_Style = interface;
  JRemoteInput = interface;

  JRemoteInputClass = interface(JObjectClass)
    ['{4AFD7682-C801-4A28-A256-4A1990F73FFE}']
    {class} function _GetEDIT_CHOICES_BEFORE_SENDING_AUTO: Integer; cdecl;
    {class} function _GetEDIT_CHOICES_BEFORE_SENDING_DISABLED: Integer; cdecl;
    {class} function _GetEDIT_CHOICES_BEFORE_SENDING_ENABLED: Integer; cdecl;
    {class} function _GetEXTRA_RESULTS_DATA: JString; cdecl;
    {class} function _GetRESULTS_CLIP_LABEL: JString; cdecl;
    {class} function _GetSOURCE_CHOICE: Integer; cdecl;
    {class} function _GetSOURCE_FREE_FORM_INPUT: Integer; cdecl;
    {class} procedure addDataResultToIntent(remoteInput: JRemoteInput; intent: JIntent; map: JMap); cdecl;
    {class} procedure addResultsToIntent(remoteInput: TJavaObjectArray<JRemoteInput>; intent: JIntent; bundle: JBundle); cdecl;
    {class} function getDataResultsFromIntent(intent: JIntent; string_: JString): JMap; cdecl;
    {class} function getResultsFromIntent(intent: JIntent): JBundle; cdecl;
    {class} function getResultsSource(intent: JIntent): Integer; cdecl;
    {class} procedure setResultsSource(intent: JIntent; i: Integer); cdecl;
    {class} property EDIT_CHOICES_BEFORE_SENDING_AUTO: Integer read _GetEDIT_CHOICES_BEFORE_SENDING_AUTO;
    {class} property EDIT_CHOICES_BEFORE_SENDING_DISABLED: Integer read _GetEDIT_CHOICES_BEFORE_SENDING_DISABLED;
    {class} property EDIT_CHOICES_BEFORE_SENDING_ENABLED: Integer read _GetEDIT_CHOICES_BEFORE_SENDING_ENABLED;
    {class} property EXTRA_RESULTS_DATA: JString read _GetEXTRA_RESULTS_DATA;
    {class} property RESULTS_CLIP_LABEL: JString read _GetRESULTS_CLIP_LABEL;
    {class} property SOURCE_CHOICE: Integer read _GetSOURCE_CHOICE;
    {class} property SOURCE_FREE_FORM_INPUT: Integer read _GetSOURCE_FREE_FORM_INPUT;
  end;

  [JavaSignature('androidx/core/app/RemoteInput')]
  JRemoteInput = interface(JObject)
    ['{734D869C-48D1-4828-81DF-2EDBBB88F105}']
    function getAllowFreeFormInput: Boolean; cdecl;
    function getAllowedDataTypes: JSet; cdecl;
    function getChoices: TJavaObjectArray<JCharSequence>; cdecl;
    function getEditChoicesBeforeSending: Integer; cdecl;
    function getExtras: JBundle; cdecl;
    function getLabel: JCharSequence; cdecl;
    function getResultKey: JString; cdecl;
    function isDataOnly: Boolean; cdecl;
  end;
  TJRemoteInput = class(TJavaGenericImport<JRemoteInputClass, JRemoteInput>) end;

  JNotificationCompat_BubbleMetadataClass = interface(JObjectClass)
    ['{33DACB07-1885-4B88-8DF1-C32E27638D14}']
    {class} // function fromPlatform(bubbleMetadata: JNotification_BubbleMetadata): JNotificationCompat_BubbleMetadata; cdecl;
    {class} // function toPlatform(bubbleMetadata: JNotificationCompat_BubbleMetadata): JNotification_BubbleMetadata; cdecl;
  end;

  [JavaSignature('androidx/core/app/NotificationCompat$BubbleMetadata')]
  JNotificationCompat_BubbleMetadata = interface(JObject)
    ['{D9B394E6-0C5F-4CEE-91DA-64ADF5B4C9AF}']
    function getAutoExpandBubble: Boolean; cdecl;
    function getDeleteIntent: JPendingIntent; cdecl;
    function getDesiredHeight: Integer; cdecl;
    function getDesiredHeightResId: Integer; cdecl;
    // function getIcon: Jdrawable_IconCompat; cdecl;
    function getIntent: JPendingIntent; cdecl;
    function getShortcutId: JString; cdecl;
    function isNotificationSuppressed: Boolean; cdecl;
    procedure setFlags(i: Integer); cdecl;
  end;
  TJNotificationCompat_BubbleMetadata = class(TJavaGenericImport<JNotificationCompat_BubbleMetadataClass, JNotificationCompat_BubbleMetadata>) end;

  JNotificationCompat_ActionClass = interface(JObjectClass)
    ['{069FC3BA-E978-4659-A8D7-7EEC6D4F9652}']
    {class} function _GetSEMANTIC_ACTION_ARCHIVE: Integer; cdecl;
    {class} function _GetSEMANTIC_ACTION_CALL: Integer; cdecl;
    {class} function _GetSEMANTIC_ACTION_DELETE: Integer; cdecl;
    {class} function _GetSEMANTIC_ACTION_MARK_AS_READ: Integer; cdecl;
    {class} function _GetSEMANTIC_ACTION_MARK_AS_UNREAD: Integer; cdecl;
    {class} function _GetSEMANTIC_ACTION_MUTE: Integer; cdecl;
    {class} function _GetSEMANTIC_ACTION_NONE: Integer; cdecl;
    {class} function _GetSEMANTIC_ACTION_REPLY: Integer; cdecl;
    {class} function _GetSEMANTIC_ACTION_THUMBS_DOWN: Integer; cdecl;
    {class} function _GetSEMANTIC_ACTION_THUMBS_UP: Integer; cdecl;
    {class} function _GetSEMANTIC_ACTION_UNMUTE: Integer; cdecl;
    {class} function init(i: Integer; charSequence: JCharSequence; pendingIntent: JPendingIntent): JNotificationCompat_Action; cdecl; overload;
    {class} // function init(iconCompat: Jdrawable_IconCompat; charSequence: JCharSequence; pendingIntent: JPendingIntent): JNotificationCompat_Action; cdecl; overload;
    {class} property SEMANTIC_ACTION_ARCHIVE: Integer read _GetSEMANTIC_ACTION_ARCHIVE;
    {class} property SEMANTIC_ACTION_CALL: Integer read _GetSEMANTIC_ACTION_CALL;
    {class} property SEMANTIC_ACTION_DELETE: Integer read _GetSEMANTIC_ACTION_DELETE;
    {class} property SEMANTIC_ACTION_MARK_AS_READ: Integer read _GetSEMANTIC_ACTION_MARK_AS_READ;
    {class} property SEMANTIC_ACTION_MARK_AS_UNREAD: Integer read _GetSEMANTIC_ACTION_MARK_AS_UNREAD;
    {class} property SEMANTIC_ACTION_MUTE: Integer read _GetSEMANTIC_ACTION_MUTE;
    {class} property SEMANTIC_ACTION_NONE: Integer read _GetSEMANTIC_ACTION_NONE;
    {class} property SEMANTIC_ACTION_REPLY: Integer read _GetSEMANTIC_ACTION_REPLY;
    {class} property SEMANTIC_ACTION_THUMBS_DOWN: Integer read _GetSEMANTIC_ACTION_THUMBS_DOWN;
    {class} property SEMANTIC_ACTION_THUMBS_UP: Integer read _GetSEMANTIC_ACTION_THUMBS_UP;
    {class} property SEMANTIC_ACTION_UNMUTE: Integer read _GetSEMANTIC_ACTION_UNMUTE;
  end;

  [JavaSignature('androidx/core/app/NotificationCompat$Action')]
  JNotificationCompat_Action = interface(JObject)
    ['{57074D9D-6157-4142-A2C1-D61ED3F54404}']
    function _GetactionIntent: JPendingIntent; cdecl;
    procedure _SetactionIntent(Value: JPendingIntent); cdecl;
    function _Geticon: Integer; cdecl;
    procedure _Seticon(Value: Integer); cdecl;
    function _Gettitle: JCharSequence; cdecl;
    procedure _Settitle(Value: JCharSequence); cdecl;
    function getActionIntent: JPendingIntent; cdecl;
    function getAllowGeneratedReplies: Boolean; cdecl;
    function getDataOnlyRemoteInputs: TJavaObjectArray<JRemoteInput>; cdecl;
    function getExtras: JBundle; cdecl;
    function getIcon: Integer; cdecl;
    // function getIconCompat: Jdrawable_IconCompat; cdecl;
    function getRemoteInputs: TJavaObjectArray<JRemoteInput>; cdecl;
    function getSemanticAction: Integer; cdecl;
    function getShowsUserInterface: Boolean; cdecl;
    function getTitle: JCharSequence; cdecl;
    function isContextual: Boolean; cdecl;
    property actionIntent: JPendingIntent read _GetactionIntent write _SetactionIntent;
    property icon: Integer read _Geticon write _Seticon;
    property title: JCharSequence read _Gettitle write _Settitle;
  end;
  TJNotificationCompat_Action = class(TJavaGenericImport<JNotificationCompat_ActionClass, JNotificationCompat_Action>) end;

  JNotificationCompat_BuilderClass = interface(JObjectClass)
    ['{BF06ED8F-16CF-4D2D-A029-4A43FD71907F}']
    {class} function init(context: JContext): JNotificationCompat_Builder; cdecl; overload;
    {class} function init(context: JContext; notification: JNotification): JNotificationCompat_Builder; cdecl; overload;
    {class} function init(context: JContext; string_: JString): JNotificationCompat_Builder; cdecl; overload;
  end;

  [JavaSignature('androidx/core/app/NotificationCompat$Builder')]
  JNotificationCompat_Builder = interface(JObject)
    ['{86BE52A4-03F6-4537-98A9-F94A2266274F}']
    function _GetmActions: JArrayList; cdecl;
    procedure _SetmActions(Value: JArrayList); cdecl;
    function _GetmContext: JContext; cdecl;
    procedure _SetmContext(Value: JContext); cdecl;
    function _GetmPeople: JArrayList; cdecl;
    procedure _SetmPeople(Value: JArrayList); cdecl;
    function _GetmPersonList: JArrayList; cdecl;
    procedure _SetmPersonList(Value: JArrayList); cdecl;
    function addAction(action: JNotificationCompat_Action): JNotificationCompat_Builder; cdecl; overload;
    function addAction(i: Integer; charSequence: JCharSequence; pendingIntent: JPendingIntent): JNotificationCompat_Builder; cdecl; overload;
    function addExtras(bundle: JBundle): JNotificationCompat_Builder; cdecl;
    function addInvisibleAction(action: JNotificationCompat_Action): JNotificationCompat_Builder; cdecl; overload;
    function addInvisibleAction(i: Integer; charSequence: JCharSequence; pendingIntent: JPendingIntent): JNotificationCompat_Builder; cdecl; overload;
    // function addPerson(person: JPerson): JNotificationCompat_Builder; cdecl; overload;
    function addPerson(string_: JString): JNotificationCompat_Builder; cdecl; overload;
    function build: JNotification; cdecl;
    function clearActions: JNotificationCompat_Builder; cdecl;
    function clearInvisibleActions: JNotificationCompat_Builder; cdecl;
    function clearPeople: JNotificationCompat_Builder; cdecl;
    function createBigContentView: JRemoteViews; cdecl;
    function createContentView: JRemoteViews; cdecl;
    function createHeadsUpContentView: JRemoteViews; cdecl;
    function extend(extender: JNotificationCompat_Extender): JNotificationCompat_Builder; cdecl;
    function getBigContentView: JRemoteViews; cdecl;
    function getBubbleMetadata: JNotificationCompat_BubbleMetadata; cdecl;
    function getColor: Integer; cdecl;
    function getContentView: JRemoteViews; cdecl;
    function getExtras: JBundle; cdecl;
    function getHeadsUpContentView: JRemoteViews; cdecl;
    function getNotification: JNotification; cdecl;
    function getPriority: Integer; cdecl;
    function getWhenIfShowing: Int64; cdecl;
    function setAllowSystemGeneratedContextualActions(b: Boolean): JNotificationCompat_Builder; cdecl;
    function setAutoCancel(b: Boolean): JNotificationCompat_Builder; cdecl;
    function setBadgeIconType(i: Integer): JNotificationCompat_Builder; cdecl;
    function setBubbleMetadata(bubbleMetadata: JNotificationCompat_BubbleMetadata): JNotificationCompat_Builder; cdecl;
    function setCategory(string_: JString): JNotificationCompat_Builder; cdecl;
    function setChannelId(string_: JString): JNotificationCompat_Builder; cdecl;
    function setChronometerCountDown(b: Boolean): JNotificationCompat_Builder; cdecl;
    function setColor(i: Integer): JNotificationCompat_Builder; cdecl;
    function setColorized(b: Boolean): JNotificationCompat_Builder; cdecl;
    function setContent(remoteViews: JRemoteViews): JNotificationCompat_Builder; cdecl;
    function setContentInfo(charSequence: JCharSequence): JNotificationCompat_Builder; cdecl;
    function setContentIntent(pendingIntent: JPendingIntent): JNotificationCompat_Builder; cdecl;
    function setContentText(charSequence: JCharSequence): JNotificationCompat_Builder; cdecl;
    function setContentTitle(charSequence: JCharSequence): JNotificationCompat_Builder; cdecl;
    function setCustomBigContentView(remoteViews: JRemoteViews): JNotificationCompat_Builder; cdecl;
    function setCustomContentView(remoteViews: JRemoteViews): JNotificationCompat_Builder; cdecl;
    function setCustomHeadsUpContentView(remoteViews: JRemoteViews): JNotificationCompat_Builder; cdecl;
    function setDefaults(i: Integer): JNotificationCompat_Builder; cdecl;
    function setDeleteIntent(pendingIntent: JPendingIntent): JNotificationCompat_Builder; cdecl;
    function setExtras(bundle: JBundle): JNotificationCompat_Builder; cdecl;
    function setFullScreenIntent(pendingIntent: JPendingIntent; b: Boolean): JNotificationCompat_Builder; cdecl;
    function setGroup(string_: JString): JNotificationCompat_Builder; cdecl;
    function setGroupAlertBehavior(i: Integer): JNotificationCompat_Builder; cdecl;
    function setGroupSummary(b: Boolean): JNotificationCompat_Builder; cdecl;
    function setLargeIcon(bitmap: JBitmap): JNotificationCompat_Builder; cdecl;
    function setLights(i: Integer; i1: Integer; i2: Integer): JNotificationCompat_Builder; cdecl;
    function setLocalOnly(b: Boolean): JNotificationCompat_Builder; cdecl;
    function setLocusId(locusIdCompat: JLocusIdCompat): JNotificationCompat_Builder; cdecl;
    function setNotificationSilent: JNotificationCompat_Builder; cdecl;
    function setNumber(i: Integer): JNotificationCompat_Builder; cdecl;
    function setOngoing(b: Boolean): JNotificationCompat_Builder; cdecl;
    function setOnlyAlertOnce(b: Boolean): JNotificationCompat_Builder; cdecl;
    function setPriority(i: Integer): JNotificationCompat_Builder; cdecl;
    function setProgress(i: Integer; i1: Integer; b: Boolean): JNotificationCompat_Builder; cdecl;
    function setPublicVersion(notification: JNotification): JNotificationCompat_Builder; cdecl;
    function setRemoteInputHistory(charSequence: TJavaObjectArray<JCharSequence>): JNotificationCompat_Builder; cdecl;
    function setSettingsText(charSequence: JCharSequence): JNotificationCompat_Builder; cdecl;
    function setShortcutId(string_: JString): JNotificationCompat_Builder; cdecl;
    function setShortcutInfo(shortcutInfoCompat: JShortcutInfoCompat): JNotificationCompat_Builder; cdecl;
    function setShowWhen(b: Boolean): JNotificationCompat_Builder; cdecl;
    function setSilent(b: Boolean): JNotificationCompat_Builder; cdecl;
    // function setSmallIcon(iconCompat: Jdrawable_IconCompat): JNotificationCompat_Builder; cdecl; overload;
    function setSmallIcon(i: Integer): JNotificationCompat_Builder; cdecl; overload;
    function setSmallIcon(i: Integer; i1: Integer): JNotificationCompat_Builder; cdecl; overload;
    function setSortKey(string_: JString): JNotificationCompat_Builder; cdecl;
    function setSound(uri: Jnet_Uri): JNotificationCompat_Builder; cdecl; overload;
    function setSound(uri: Jnet_Uri; i: Integer): JNotificationCompat_Builder; cdecl; overload;
    function setStyle(style: JNotificationCompat_Style): JNotificationCompat_Builder; cdecl;
    function setSubText(charSequence: JCharSequence): JNotificationCompat_Builder; cdecl;
    function setTicker(charSequence: JCharSequence): JNotificationCompat_Builder; cdecl; overload;
    function setTicker(charSequence: JCharSequence; remoteViews: JRemoteViews): JNotificationCompat_Builder; cdecl; overload;
    function setTimeoutAfter(l: Int64): JNotificationCompat_Builder; cdecl;
    function setUsesChronometer(b: Boolean): JNotificationCompat_Builder; cdecl;
    function setVibrate(l: TJavaArray<Int64>): JNotificationCompat_Builder; cdecl;
    function setVisibility(i: Integer): JNotificationCompat_Builder; cdecl;
    function setWhen(l: Int64): JNotificationCompat_Builder; cdecl;
    property mActions: JArrayList read _GetmActions write _SetmActions;
    property mContext: JContext read _GetmContext write _SetmContext;
    property mPeople: JArrayList read _GetmPeople write _SetmPeople;
    property mPersonList: JArrayList read _GetmPersonList write _SetmPersonList;
  end;
  TJNotificationCompat_Builder = class(TJavaGenericImport<JNotificationCompat_BuilderClass, JNotificationCompat_Builder>) end;

  JNotificationCompat_ExtenderClass = interface(IJavaClass)
    ['{E526C472-F346-4A76-A119-1213F3C14C3E}']
  end;

  [JavaSignature('androidx/core/app/NotificationCompat$Extender')]
  JNotificationCompat_Extender = interface(IJavaInstance)
    ['{49AAF7F2-478D-49AE-B03C-B03C07E46B1F}']
    function extend(builder: JNotificationCompat_Builder): JNotificationCompat_Builder; cdecl;
  end;
  TJNotificationCompat_Extender = class(TJavaGenericImport<JNotificationCompat_ExtenderClass, JNotificationCompat_Extender>) end;

  JNotificationCompat_StyleClass = interface(JObjectClass)
    ['{57D9C489-B5D5-466A-99BE-38DF51239448}']
    {class} function extractStyleFromNotification(notification: JNotification): JNotificationCompat_Style; cdecl;
    {class} function init: JNotificationCompat_Style; cdecl;
  end;

  [JavaSignature('androidx/core/app/NotificationCompat$Style')]
  JNotificationCompat_Style = interface(JObject)
    ['{89931428-DFD9-4BD3-8C5A-80B6E446ECD6}']
    procedure addCompatExtras(bundle: JBundle); cdecl;
    // procedure apply(notificationBuilderWithBuilderAccessor: JNotificationBuilderWithBuilderAccessor); cdecl;
    function applyStandardTemplate(b: Boolean; i: Integer; b1: Boolean): JRemoteViews; cdecl;
    function build: JNotification; cdecl;
    procedure buildIntoRemoteViews(remoteViews: JRemoteViews; remoteViews1: JRemoteViews); cdecl;
    function createColoredBitmap(i: Integer; i1: Integer): JBitmap; cdecl; overload;
    function displayCustomViewInline: Boolean; cdecl;
    // function makeBigContentView(notificationBuilderWithBuilderAccessor: JNotificationBuilderWithBuilderAccessor): JRemoteViews; cdecl;
    // function makeContentView(notificationBuilderWithBuilderAccessor: JNotificationBuilderWithBuilderAccessor): JRemoteViews; cdecl;
    // function makeHeadsUpContentView(notificationBuilderWithBuilderAccessor: JNotificationBuilderWithBuilderAccessor): JRemoteViews; cdecl;
    procedure setBuilder(builder: JNotificationCompat_Builder); cdecl;
  end;
  TJNotificationCompat_Style = class(TJavaGenericImport<JNotificationCompat_StyleClass, JNotificationCompat_Style>) end;

  // API 24+
  JNotificationCompat_DecoratedCustomViewStyleClass = interface(JNotificationCompat_StyleClass)
    ['{136C847E-D749-437D-8EE1-06D6DB9B856F}']
    function init: JNotificationCompat_DecoratedCustomViewStyle; cdecl; overload;
  end;

  [JavaSignature('androidx/core/app/NotificationCompat$DecoratedCustomViewStyle')]
  JNotificationCompat_DecoratedCustomViewStyle = interface(JNotificationCompat_Style)
    ['{50371CBC-753D-4C0C-9D87-9F178AA42F06}']
  end;
  TJNotificationCompat_DecoratedCustomViewStyle = class(TJavaGenericImport<JNotificationCompat_DecoratedCustomViewStyleClass,
    JNotificationCompat_DecoratedCustomViewStyle>)
  end;

implementation

end.
