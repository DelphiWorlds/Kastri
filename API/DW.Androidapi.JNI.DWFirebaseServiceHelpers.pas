unit DW.Androidapi.JNI.DWFirebaseServiceHelpers;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2024 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText;

type
  JDWFirebaseMessagingService = interface;

  JDWFirebaseMessagingServiceClass = interface(JObjectClass)
    ['{3111661E-03CE-45AB-9F60-90A0813EF914}']
    {class} function _GetACTION_MESSAGE_RECEIVED: JString; cdecl;
    {class} function _GetACTION_NEW_TOKEN: JString; cdecl;
    {class} procedure queryToken(context: JContext); cdecl;
    {class} property ACTION_MESSAGE_RECEIVED: JString read _GetACTION_MESSAGE_RECEIVED;
    {class} property ACTION_NEW_TOKEN: JString read _GetACTION_NEW_TOKEN;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWFirebaseMessagingService')]
  JDWFirebaseMessagingService = interface(JObject)
    ['{D6C0E41A-3BD9-4477-A554-459A5916CE8F}']
  end;
  TJDWFirebaseMessagingService = class(TJavaGenericImport<JDWFirebaseMessagingServiceClass, JDWFirebaseMessagingService>) end;

  JDWNotificationPublisherClass = interface(JObjectClass)
    ['{05A7A5CF-1094-4D99-A5EF-9DBE9BF180A6}']
    {class} procedure sendNotification(context: JContext; intent: JIntent; pending: Boolean); cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWNotificationPublisher')]
  JDWNotificationPublisher = interface(JObject)
    ['{3CE7C4D0-4C63-4617-95F8-F357693EE84A}']
  end;
  TJDWNotificationPublisher = class(TJavaGenericImport<JDWNotificationPublisherClass, JDWNotificationPublisher>) end;

implementation

end.
