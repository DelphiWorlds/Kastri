unit DW.iOSapi.FirebaseMessaging;

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
  // macOS
  Macapi.ObjectiveC,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UserNotifications;

const
  FIRMessagingErrorUnknown = 0;
  FIRMessagingErrorAuthentication = 1;
  FIRMessagingErrorNoAccess = 2;
  FIRMessagingErrorTimeout = 3;
  FIRMessagingErrorNetwork = 4;
  FIRMessagingErrorOperationInProgress = 5;
  FIRMessagingErrorInvalidRequest = 7;
  FIRMessagingErrorInvalidTopicName = 8;
  FIRMessagingMessageStatusUnknown = 0;
  FIRMessagingMessageStatusNew = 1;
  FIRMessagingAPNSTokenTypeUnknown = 0;
  FIRMessagingAPNSTokenTypeSandbox = 1;
  FIRMessagingAPNSTokenTypeProd = 2;

type
  FIRMessagingMessageInfo = interface;
  FIRMessagingDelegate = interface;
  FIRMessaging = interface;
  FIRMessagingExtensionHelper = interface;

  FIRMessagingFCMTokenFetchCompletion = procedure(FCMToken: NSString; error: NSError) of object;
  FIRMessagingDeleteFCMTokenCompletion = procedure(error: NSError) of object;

  FIRMessagingTopicOperationCompletion = procedure(error: NSError) of object;
  FIRMessagingError = NSInteger;
  FIRMessagingMessageStatus = NSInteger;
  FIRMessagingAPNSTokenType = NSInteger;
  TFIRMessagingBlockMethod1 = procedure(token: NSString; error: NSError) of object;
  TFIRMessagingBlockMethod2 = procedure(error: NSError) of object;
  TFIRMessagingBlockMethod3 = procedure(FCMToken: NSString; error: NSError) of object;
  TFIRMessagingExtensionHelperBlockMethod1 = procedure(param1: UNNotificationContent) of object;

  FIRMessagingMessageInfoClass = interface(NSObjectClass)
    ['{FDAC534F-3D79-4FF6-824E-50DC7423662A}']
  end;

  FIRMessagingMessageInfo = interface(NSObject)
    ['{4D70F5C5-3635-405F-895C-F41C8D1FD76B}']
    function status: FIRMessagingMessageStatus; cdecl;
  end;
  TFIRMessagingMessageInfo = class(TOCGenericImport<FIRMessagingMessageInfoClass, FIRMessagingMessageInfo>) end;

  {$IF Defined(FIREBASE_PRE_V10)}
  FIRMessagingRemoteMessageClass = interface(NSObjectClass)
    ['{EF45D074-C7A5-4DB2-BCD1-53B8650419F4}']
  end;

  FIRMessagingRemoteMessage = interface(NSObject)
    ['{6E2F8E14-FD8D-4B5D-8026-A607BE0B8F9C}']
    function appData: NSDictionary; cdecl;
  end;
  TFIRMessagingRemoteMessage = class(TOCGenericImport<FIRMessagingRemoteMessageClass, FIRMessagingRemoteMessage>) end;

  FIRMessagingDelegate = interface(IObjectiveC)
    ['{264C1F0E-3EA9-42AC-9802-EF1BC9A7E321}']
    procedure applicationReceivedRemoteMessage(remoteMessage: FIRMessagingRemoteMessage); cdecl; // Deprecated?
    [MethodName('messaging:didReceiveMessage:')]
    procedure didReceiveMessage(messaging: FIRMessaging; remoteMessage: FIRMessagingRemoteMessage); cdecl;
    [MethodName('messaging:didRefreshRegistrationToken:')]
    procedure didRefreshRegistrationToken(messaging: FIRMessaging; fcmToken: NSString); cdecl;
    [MethodName('messaging:didReceiveRegistrationToken:')]
    procedure didReceiveRegistrationToken(messaging: FIRMessaging; fcmToken: NSString); cdecl;
  end;
  {$ELSE}

  FIRMessagingDelegate = interface(IObjectiveC)
    ['{D2E4D0A8-B2CD-4C69-B6C8-01619DC5E09B}']
    procedure messaging(messaging: FIRMessaging; didReceiveRegistrationToken: NSString); cdecl;
  end;
  {$ENDIF}

  FIRMessagingClass = interface(NSObjectClass)
    ['{62AF9A4C-681E-4BCD-9063-6209CAE08296}']
    {class} function messaging: pointer; cdecl;
  end;

  FIRMessaging = interface(NSObject)
    ['{A721C3D4-82EB-4A7B-A5E5-42EF9E8F618E}']
    function APNSToken: NSData; cdecl;
    function appDidReceiveMessage(message: NSDictionary): FIRMessagingMessageInfo; cdecl;
    function delegate: Pointer; cdecl;
    procedure deleteDataWithCompletion(completion: TFIRMessagingBlockMethod2); cdecl;
    procedure deleteFCMTokenForSenderID(senderID: NSString; completion: TFIRMessagingBlockMethod2); cdecl;
    procedure deleteTokenWithCompletion(completion: TFIRMessagingBlockMethod2); cdecl;
    function FCMToken: NSString; cdecl;
    function isAutoInitEnabled: Boolean; cdecl;
    procedure retrieveFCMTokenForSenderID(senderID: NSString; completion: TFIRMessagingBlockMethod3); cdecl;
    procedure setAPNSToken(APNSToken: NSData); overload; cdecl;
    procedure setAPNSToken(apnsToken: NSData; &type: FIRMessagingAPNSTokenType); overload; cdecl;
    procedure setAutoInitEnabled(autoInitEnabled: Boolean); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure subscribeToTopic(topic: NSString; completion: TFIRMessagingBlockMethod2); overload; cdecl;
    procedure subscribeToTopic(topic: NSString); overload; cdecl;
    procedure tokenWithCompletion(completion: TFIRMessagingBlockMethod1); cdecl;
    procedure unsubscribeFromTopic(topic: NSString; completion: TFIRMessagingBlockMethod2); overload; cdecl;
    procedure unsubscribeFromTopic(topic: NSString); overload; cdecl;
  end;
  TFIRMessaging = class(TOCGenericImport<FIRMessagingClass, FIRMessaging>) end;

  FIRMessagingExtensionHelperClass = interface(NSObjectClass)
    ['{285EE430-1E8E-4BCE-B678-70BA356C973A}']
  end;

  FIRMessagingExtensionHelper = interface(NSObject)
    ['{0127DE9D-DC22-408E-895D-B4CD1D8089EB}']
    procedure exportDeliveryMetricsToBigQueryWithMessageInfo(info: NSDictionary); cdecl;
    procedure populateNotificationContent(content: UNMutableNotificationContent; withContentHandler: TFIRMessagingExtensionHelperBlockMethod1); cdecl;
  end;
  TFIRMessagingExtensionHelper = class(TOCGenericImport<FIRMessagingExtensionHelperClass, FIRMessagingExtensionHelper>) end;

function kFIRInstanceIDTokenRefreshNotification: NSString; cdecl;

implementation

uses
  // macOS
  Macapi.Helpers,
  // DW
  DW.iOSapi.FirebaseCore;

function kFIRInstanceIDTokenRefreshNotification: NSString;
begin
  // http://stackoverflow.com/questions/43592047/delphi-ios-how-to-import-const-defined-in-3rd-party-library
  Result := StrToNSStr('com.firebase.iid.notif.refresh-token');
end;

procedure FirebaseMessagingLoader; cdecl; external framework 'FirebaseMessaging';
procedure GoogleDataTransportLoader; cdecl; external framework 'GoogleDataTransport';

end.