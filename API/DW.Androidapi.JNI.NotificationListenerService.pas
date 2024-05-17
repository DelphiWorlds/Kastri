unit DW.Androidapi.JNI.NotificationListenerService;

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
  Androidapi.JNI.JavaTypes, Androidapi.JNIBridge, Androidapi.JNI.App, Androidapi.JNI.Os;

type
  JNotificationListenerService = interface;
  JStatusBarNotification = interface;

  JStatusBarNotificationClass = interface(JObjectClass)
    ['{4B787750-AA6C-4217-99D9-9430904AFF10}']
    function _GetCREATOR: JParcelable_Creator; cdecl;
    function init(&in: JParcel): JStatusBarNotification; cdecl; overload;
    function init(pkg: JString; opPkg: JString; id: Integer; tag: JString; uid: Integer; initialPid: Integer; score: Integer;
      notification: JNotification; user: JUserHandle; postTime: Int64): JStatusBarNotification; cdecl; overload;
    property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/service/notification/StatusBarNotification')]
  JStatusBarNotification = interface(JObject)
    ['{162AA736-7712-4F52-B892-7AF93949A72F}']
    function clone: JStatusBarNotification; cdecl;
    function describeContents: Integer; cdecl;
    function getGroupKey: JString; cdecl;
    function getId: Integer; cdecl;
    function getKey: JString; cdecl;
    function getNotification: JNotification; cdecl;
    function getOverrideGroupKey: JString; cdecl;
    function getPackageName: JString; cdecl;
    function getPostTime: Int64; cdecl;
    function getTag: JString; cdecl;
    function getUser: JUserHandle; cdecl;
    function getUserId: Integer; deprecated; cdecl;
    function isClearable: boolean; cdecl;
    function isGroup: boolean; cdecl;
    function isOngoing: boolean; cdecl;
    function toString: JString; cdecl;
    procedure setOverrideGroupKey(overrideGroupKey: JString); cdecl;
    procedure writeToParcel(&out: JParcel; flags: Integer); cdecl;
  end;
  TJStatusBarNotification = class(TJavaGenericImport<JStatusBarNotificationClass, JStatusBarNotification>)
  end;

  // Placeholder declarations - full import may be added later
  JNotificationListenerServiceClass = interface(JServiceClass)
    ['{41298EA6-1A45-4980-8A47-D0E3996F2C61}']
  end;

  [JavaSignature('android/service/notification/NotificationListenerService')]
  JNotificationListenerService = interface(JService)
    ['{20970361-7F92-4F3C-BBC7-B2473B008468}']
  end;
  TJNotificationListenerService = class(TJavaGenericImport<JNotificationListenerServiceClass, JNotificationListenerService>)
  end;

implementation

end.

