unit DW.Androidapi.JNI.Firebase;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{    Copyright 2020 Dave Nottage under MIT license      }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.JNI.App, Androidapi.JNI.Os,
  Androidapi.JNI.Net;

type
  JFirebaseOptions = interface;
  JFirebaseOptions_Builder = interface;
  JFirebaseApp = interface;
  JFirebaseInstanceId = interface;
  JFirebaseMessaging = interface;
  JRemoteMessage = interface;
  JRemoteMessage_Builder = interface;
  JRemoteMessage_Notification = interface;

  JFirebaseOptionsClass = interface(JObjectClass)
    ['{0B0D6931-3744-44B5-9909-4158E93EB585}']
    {class} function fromResource(context: JContext): JFirebaseOptions; cdecl;
  end;

  [JavaSignature('com/google/firebase/FirebaseOptions')]
  JFirebaseOptions = interface(JObject)
    ['{7F682156-DC17-414F-A537-358359AA01A7}']
    function equals(o: JObject): Boolean; cdecl;
    function getApiKey: JString; cdecl;
    function getApplicationId: JString; cdecl;
    function getDatabaseUrl: JString; cdecl;
    function getGaTrackingId: JString; cdecl;
    function getGcmSenderId: JString; cdecl;
    function getProjectId: JString; cdecl;
    function getStorageBucket: JString; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
  end;
  TJFirebaseOptions = class(TJavaGenericImport<JFirebaseOptionsClass, JFirebaseOptions>) end;

  JFirebaseOptions_BuilderClass = interface(JObjectClass)
    ['{745E3C1A-7F86-49A7-8377-CB3D7F953870}']
    {class} function init: JFirebaseOptions_Builder; cdecl; overload;
    {class} function init(options: JFirebaseOptions): JFirebaseOptions_Builder; cdecl; overload;
  end;

  [JavaSignature('com/google/firebase/FirebaseOptions$Builder')]
  JFirebaseOptions_Builder = interface(JObject)
    ['{71400595-7BCC-4AA2-BCDC-BA3F5FF3CD8F}']
    function build: JFirebaseOptions; cdecl;
    function setApiKey(P1: JString): JFirebaseOptions_Builder; cdecl;
    function setApplicationId(P1: JString): JFirebaseOptions_Builder; cdecl;
    function setDatabaseUrl(P1: JString): JFirebaseOptions_Builder; cdecl;
    function setGaTrackingId(P1: JString): JFirebaseOptions_Builder; cdecl;
    function setGcmSenderId(P1: JString): JFirebaseOptions_Builder; cdecl;
    function setProjectId(P1: JString): JFirebaseOptions_Builder; cdecl;
    function setStorageBucket(P1: JString): JFirebaseOptions_Builder; cdecl;
  end;
  TJFirebaseOptions_Builder = class(TJavaGenericImport<JFirebaseOptions_BuilderClass, JFirebaseOptions_Builder>) end;

  JFirebaseAppClass = interface(JObjectClass)
    ['{DE33391C-BCF4-4718-A557-7703B772E23F}']
    {class} function _GetDEFAULT_APP_NAME: JString; cdecl;
    {class} procedure clearInstancesForTest; cdecl;
    {class} function getApps(context: JContext): JList; cdecl;
    {class} function getInstance: JFirebaseApp; cdecl; overload;
    {class} function getInstance(name: JString): JFirebaseApp; cdecl; overload;
    {class} function getPersistenceKey(P1: JString; P2: JFirebaseOptions): JString; cdecl; overload;
    {class} function initializeApp(context: JContext): JFirebaseApp; cdecl; overload;
    {class} function initializeApp(context: JContext; options: JFirebaseOptions): JFirebaseApp; cdecl; overload;
    {class} function initializeApp(context: JContext; options: JFirebaseOptions; name: JString): JFirebaseApp; cdecl; overload;
    {class} procedure onBackgroundStateChanged(P1: Boolean); cdecl;
    {class} property DEFAULT_APP_NAME: JString read _GetDEFAULT_APP_NAME;
  end;

  [JavaSignature('com/google/firebase/FirebaseApp')]
  JFirebaseApp = interface(JObject)
    ['{3612CB9C-FD82-4362-A93E-7D59FC068B9A}']
    procedure delete; cdecl;
    function equals(o: JObject): Boolean; cdecl;
    function &get(P1: Jlang_Class): JObject; cdecl;
    function getApplicationContext: JContext; cdecl;
    function getListeners: JList; cdecl;
    function getName: JString; cdecl;
    function getOptions: JFirebaseOptions; cdecl;
    function getPersistenceKey: JString; cdecl; overload;
    // function getToken(P1: Boolean): JTask; cdecl;
    function getUid: JString; cdecl;
    function hashCode: Integer; cdecl;
    function isAutomaticDataCollectionEnabled: Boolean; cdecl;
    function isDefaultApp: Boolean; cdecl;
    procedure setAutomaticDataCollectionEnabled(P1: Boolean); cdecl;
    procedure setAutomaticResourceManagementEnabled(enabled: Boolean); cdecl;
    function toString: JString; cdecl;
  end;
  TJFirebaseApp = class(TJavaGenericImport<JFirebaseAppClass, JFirebaseApp>) end;

  JFirebaseInstanceIdClass = interface(JObjectClass)
    ['{E5387FB1-8757-45AF-B699-6781707A7D57}']
    {class} function getInstance: JFirebaseInstanceId; cdecl; overload;
    {class} function getInstance(app: JFirebaseApp): JFirebaseInstanceId; cdecl; overload;
  end;

  [JavaSignature('com/google/firebase/iid/FirebaseInstanceId')]
  JFirebaseInstanceId = interface(JObject)
    ['{F759878D-C386-4118-9578-A6FC6A4D6FBD}']
    procedure deleteInstanceId; cdecl;
    procedure deleteToken(authorizedEntity: JString; scope: JString); cdecl;
    function getCreationTime: Int64; cdecl;
    function getId: JString; cdecl;
    function getInstanceId: JObject; cdecl; // JTask
    function getToken: JString; cdecl; overload;
    function getToken(authorizedEntity: JString; scope: JString): JString; cdecl; overload;
  end;
  TJFirebaseInstanceId = class(TJavaGenericImport<JFirebaseInstanceIdClass, JFirebaseInstanceId>) end;

  JFirebaseMessagingClass = interface(JObjectClass)
    ['{CAB4B525-5E1E-40E8-9A7C-CA3608FE26C6}']
    {class} function _GetINSTANCE_ID_SCOPE: JString; cdecl;
    {class} function getInstance: JFirebaseMessaging; cdecl; overload;
    {class} property INSTANCE_ID_SCOPE: JString read _GetINSTANCE_ID_SCOPE;
  end;

  [JavaSignature('com/google/firebase/messaging/FirebaseMessaging')]
  JFirebaseMessaging = interface(JObject)
    ['{F33877E5-DAA2-4097-9527-CE1CDBD66A67}']
    function isAutoInitEnabled: Boolean; cdecl;
    procedure send(msg: JRemoteMessage); cdecl;
    procedure setAutoInitEnabled(enable: Boolean); cdecl;
    procedure subscribeToTopic(topic: JString); cdecl;
    procedure unsubscribeFromTopic(topic: JString); cdecl;
  end;
  TJFirebaseMessaging = class(TJavaGenericImport<JFirebaseMessagingClass, JFirebaseMessaging>) end;

  JRemoteMessageClass = interface(JObjectClass)
    ['{11580A60-FC9D-4E20-85FF-214301CAAFA7}']
    {class} function init(bundle: JBundle): JRemoteMessage; cdecl;
  end;

  [JavaSignature('com/google/firebase/messaging/RemoteMessage')]
  JRemoteMessage = interface(JObject)
    ['{E876DFED-D24E-49AD-9249-CB5E54493D6B}']
    function getCollapseKey: JString; cdecl;
    // function getData - cannot import due to generics
    function getFrom: JString; cdecl;
    function getMessageId: JString; cdecl;
    function getMessageType: JString; cdecl;
    function getNotification: JRemoteMessage_Notification; cdecl;
    function getSentTime: Int64; cdecl;
    function getTo: JString; cdecl;
    function getTtl: Integer; cdecl;
    procedure writeToParcel(parcel: JParcel; i: Integer); cdecl;
  end;
  TJRemoteMessage = class(TJavaGenericImport<JRemoteMessageClass, JRemoteMessage>) end;

  JRemoteMessage_BuilderClass = interface(JObjectClass)
    ['{35B9F744-43A1-4297-9252-ED2BF9D1D85A}']
    {class} function init(P1: JString): JRemoteMessage_Builder; cdecl;
  end;

  [JavaSignature('com/google/firebase/messaging/RemoteMessage$Builder')]
  JRemoteMessage_Builder = interface(JObject)
    ['{6752DCDD-3F0B-42AE-B975-7169E534BCE6}']
    function addData(P1: JString; P2: JString): JRemoteMessage_Builder; cdecl;
    function build: JRemoteMessage; cdecl;
    function clearData: JRemoteMessage_Builder; cdecl;
    function setCollapseKey(P1: JString): JRemoteMessage_Builder; cdecl;
    function setData(P1: JMap): JRemoteMessage_Builder; cdecl;
    function setMessageId(P1: JString): JRemoteMessage_Builder; cdecl;
    function setMessageType(P1: JString): JRemoteMessage_Builder; cdecl;
    function setTtl(P1: Integer): JRemoteMessage_Builder; cdecl;
  end;
  TJRemoteMessage_Builder = class(TJavaGenericImport<JRemoteMessage_BuilderClass, JRemoteMessage_Builder>) end;

  JRemoteMessage_MessagePriorityClass = interface(JAnnotationClass)
    ['{93677AA8-BF5A-4DF1-A952-12D72D47F89B}']
  end;

  [JavaSignature('com/google/firebase/messaging/RemoteMessage$MessagePriority')]
  JRemoteMessage_MessagePriority = interface(JAnnotation)
    ['{1CCA28CD-67A7-4C7E-BD5C-F80DA0754803}']
  end;
  TJRemoteMessage_MessagePriority = class(TJavaGenericImport<JRemoteMessage_MessagePriorityClass, JRemoteMessage_MessagePriority>) end;

  JRemoteMessage_NotificationClass = interface(JObjectClass)
    ['{A640E4D3-825E-46CA-A401-A15CD3B40341}']
  end;

  [JavaSignature('com/google/firebase/messaging/RemoteMessage$Notification')]
  JRemoteMessage_Notification = interface(JObject)
    ['{FDFAB296-F7C9-4BB6-A482-7AF50ED8D5C1}']
    function getBody: JString; cdecl;
    function getBodyLocalizationArgs: TJavaObjectArray<JString>; cdecl;
    function getBodyLocalizationKey: JString; cdecl;
    function getClickAction: JString; cdecl;
    function getColor: JString; cdecl;
    function getIcon: JString; cdecl;
    function getLink: Jnet_Uri; cdecl;
    function getSound: JString; cdecl;
    function getTag: JString; cdecl;
    function getTitle: JString; cdecl;
    function getTitleLocalizationArgs: TJavaObjectArray<JString>; cdecl;
    function getTitleLocalizationKey: JString; cdecl;
  end;
  TJRemoteMessage_Notification = class(TJavaGenericImport<JRemoteMessage_NotificationClass, JRemoteMessage_Notification>) end;

implementation

end.
