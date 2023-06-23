unit iOSapi.FirebaseMessaging;

{*******************************************************}
{                                                       }
{            CodeGear Delphi Runtime Library            }
{                                                       }
{ Copyright(c) 2010-2022 Embarcadero Technologies, Inc. }
{                  All rights reserved                  }
{                                                       }
{*******************************************************}

interface

uses
  Macapi.ObjectiveC,
  iOSapi.CocoaTypes, iOSapi.Foundation;

const
  FIRMessagingErrorUnknown = 0;
  FIRMessagingErrorAuthentication = 1;
  FIRMessagingErrorNoAccess = 2;
  FIRMessagingErrorTimeout = 3;
  FIRMessagingErrorNetwork = 4;
  FIRMessagingErrorOperationInProgress = 5;
  FIRMessagingErrorInvalidRequest = 7;
  FIRMessagingMessageStatusUnknown = 0;
  FIRMessagingMessageStatusNew = 1;
  FIRMessasingAPNSTokenTypeUnknown = 0;
  FIRMessasingAPNSTokenTypeSandbox = 1;
  FIRMessasingAPNSTokenTypeProd = 2;

type
  FIRInstanceIDAPNSTokenType = NSInteger;
  FIRMessagingAPNSTokenType = NSInteger;
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
    procedure applicationReceivedRemoteMessage(remoteMessage: FIRMessagingRemoteMessage); cdecl;
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
  iOSapi.FirebaseCommon,
  Macapi.Helpers;

function kFIRInstanceIDTokenRefreshNotification: NSString;
begin
  Result := StrToNSStr('com.firebase.iid.notif.refresh-token');
end;

// DW - Commented out declarations not required for Firebase iOS SDK 8.15
// procedure FirebaseInstanceIDLoader; cdecl; external {$IFNDEF IOS32}framework{$ENDIF} 'FirebaseInstanceID';
procedure FirebaseMessagingLoader; cdecl; external {$IFNDEF IOS32}framework{$ENDIF} 'FirebaseMessaging';
// procedure ProtobufLoader; cdecl; external {$IFNDEF IOS32}framework{$ENDIF} 'Protobuf';

end.
