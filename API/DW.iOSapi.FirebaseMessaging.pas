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
  iOSapi.CocoaTypes, iOSapi.Foundation;

const
  // Unknown error.
  FIRInstanceIDErrorUnknown = 0;
  // Auth Error -- GCM couldn't validate request from this client.
  FIRInstanceIDErrorAuthentication = 1;
  // NoAccess -- InstanceID service cannot be accessed.
  FIRInstanceIDErrorNoAccess = 2;
  // Timeout -- Request to InstanceID backend timed out.
  FIRInstanceIDErrorTimeout = 3;
  // Network -- No network available to reach the servers.
  FIRInstanceIDErrorNetwork = 4;
  // OperationInProgress -- Another similar operation in progress, bailing this one.
  FIRInstanceIDErrorOperationInProgress = 5;
  // InvalidRequest -- Some parameters of the request were invalid.
  FIRInstanceIDErrorInvalidRequest = 7;
  // Unknown token type.
  FIRInstanceIDAPNSTokenTypeUnknown = 0;
  // Sandbox token type.
  FIRInstanceIDAPNSTokenTypeSandbox = 1;
  // Production token type.
  FIRInstanceIDAPNSTokenTypeProd = 2;

  // Unknown error.
  FIRMessagingErrorUnknown = 0;
  // FIRMessaging couldn't validate request from this client.
  FIRMessagingErrorAuthentication = 1;
  // InstanceID service cannot be accessed.
  FIRMessagingErrorNoAccess = 2;
  // Request to InstanceID backend timed out.
  FIRMessagingErrorTimeout = 3;
  // No network available to reach the servers.
  FIRMessagingErrorNetwork = 4;
  // Another similar operation in progress, bailing this one.
  FIRMessagingErrorOperationInProgress = 5;
  // Some parameters of the request were invalid.
  FIRMessagingErrorInvalidRequest = 7;

  // Unknown status.
  FIRMessagingMessageStatusUnknown = 0;
  // New downstream message received by the app.
  FIRMessagingMessageStatusNew = 1;
  // Unknown token type.
  FIRMessasingAPNSTokenTypeUnknown = 0;

  // Sandbox token type.
  FIRMessasingAPNSTokenTypeSandbox = 1;
  // Production token type.
  FIRMessasingAPNSTokenTypeProd = 2;

type
  FIRInstanceIDAPNSTokenType = NSInteger;
  FIRMessagingAPNSTokenType = NSInteger;
  FIRInstanceIDError = NSUInteger;
  FIRMessagingError = NSUInteger;
  FIRMessagingMessageStatus = NSInteger;

  TFIRMessagingConnectCompletion = procedure(error: NSError) of object;

  FIRMessagingMessageInfoClass = interface(NSObjectClass)
    ['{FDAC534F-3D79-4FF6-824E-50DC7423662A}']
  end;

  FIRMessagingMessageInfo = interface(NSObject)
    ['{4D70F5C5-3635-405F-895C-F41C8D1FD76B}']
    function status: FIRMessagingMessageStatus; cdecl;
  end;
  TFIRMessagingMessageInfo = class(TOCGenericImport<FIRMessagingMessageInfoClass, FIRMessagingMessageInfo>) end;

  FIRMessagingRemoteMessageClass = interface(NSObjectClass)
    ['{EF45D074-C7A5-4DB2-BCD1-53B8650419F4}']
  end;

  FIRMessagingRemoteMessage = interface(NSObject)
    ['{6E2F8E14-FD8D-4B5D-8026-A607BE0B8F9C}']
    function appData: NSDictionary; cdecl;
  end;
  TFIRMessagingRemoteMessage = class(TOCGenericImport<FIRMessagingRemoteMessageClass, FIRMessagingRemoteMessage>) end;

  FIRMessaging = interface;

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

  FIRMessagingClass = interface(NSObjectClass)
    ['{62AF9A4C-681E-4BCD-9063-6209CAE08296}']
    {class} function messaging: pointer; cdecl;
  end;

  FIRMessaging = interface(NSObject)
    ['{A721C3D4-82EB-4A7B-A5E5-42EF9E8F618E}']
    function APNSToken: NSData; cdecl;
    procedure connectWithCompletion(handler: TFIRMessagingConnectCompletion); cdecl;
    function delegate: Pointer; cdecl;
    procedure disconnect; cdecl;
    procedure sendMessage(msg: NSDictionary; receiver: NSString; messageID: NSString; ttl: Int64); cdecl;
    procedure setAPNSToken(apnsToken: NSData; tokenType: FIRMessagingAPNSTokenType); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    function shouldEstablishDirectChannel: Boolean; cdecl;
    procedure setShouldEstablishDirectChannel(value: Boolean); cdecl;
    procedure subscribeToTopic(topic: NSString); cdecl;
    procedure unsubscribeFromTopic(topic: NSString); cdecl;
  end;
  TFIRMessaging = class(TOCGenericImport<FIRMessagingClass, FIRMessaging>) end;

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

end.
