unit DW.iOSapi.UserNotifications;

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
  // macOS
  Macapi.CoreFoundation, Macapi.ObjCRuntime, Macapi.ObjectiveC,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.CoreLocation;

const
  UNErrorCodeNotificationsNotAllowed = 1;
  UNErrorCodeAttachmentInvalidURL = 100;
  UNErrorCodeAttachmentUnrecognizedType = 101;
  UNErrorCodeAttachmentInvalidFileSize = 102;
  UNErrorCodeAttachmentNotInDataStore = 103;
  UNErrorCodeAttachmentMoveIntoDataStoreFailed = 104;
  UNErrorCodeAttachmentCorrupt = 105;
  UNErrorCodeNotificationInvalidNoDate = 1400;
  UNErrorCodeNotificationInvalidNoContent = 1401;
  UNNotificationActionOptionAuthenticationRequired = (1 shl 0);
  UNNotificationActionOptionDestructive = (1 shl 1);
  UNNotificationActionOptionForeground = (1 shl 2);
  UNNotificationCategoryOptionNone = (0);
  UNNotificationCategoryOptionCustomDismissAction = (1 shl 0);
  UNNotificationCategoryOptionAllowInCarPlay = (2 shl 0);
  UNAuthorizationStatusNotDetermined = 0;
  UNAuthorizationStatusDenied = 1;
  UNAuthorizationStatusAuthorized = 2;
  UNNotificationSettingNotSupported = 0;
  UNNotificationSettingDisabled = 1;
  UNNotificationSettingEnabled = 2;
  UNAlertStyleNone = 0;
  UNAlertStyleBanner = 1;
  UNAlertStyleAlert = 2;
  UNAuthorizationOptionBadge = (1 shl 0);
  UNAuthorizationOptionSound = (1 shl 1);
  UNAuthorizationOptionAlert = (1 shl 2);
  UNAuthorizationOptionCarPlay = (1 shl 3);
  UNNotificationPresentationOptionBadge = (1 shl 0);
  UNNotificationPresentationOptionSound = (1 shl 1);
  UNNotificationPresentationOptionAlert = (1 shl 2);
  UNNotificationPresentationOptionNone = 0;

type
  UNUserNotificationCenterSupport = interface;
  UNNotificationRequest = interface;
  UNNotification = interface;
  UNNotificationAction = interface;
  UNTextInputNotificationAction = interface;
  UNNotificationAttachment = interface;
  UNNotificationCategory = interface;
  UNNotificationSound = interface;
  UNNotificationContent = interface;
  UNMutableNotificationContent = interface;
  UNNotificationTrigger = interface;
  UNNotificationResponse = interface;
  UNTextInputNotificationResponse = interface;
  UNNotificationServiceExtension = interface;
  UNNotificationSettings = interface;
  UNPushNotificationTrigger = interface;
  UNTimeIntervalNotificationTrigger = interface;
  UNCalendarNotificationTrigger = interface;
  UNLocationNotificationTrigger = interface;
  UNUserNotificationCenterDelegate = interface;
  UNUserNotificationCenter = interface;

  UNErrorCode = NSInteger;
  UNNotificationActionOptions = NSUInteger;
  UNNotificationCategoryOptions = NSUInteger;
  TUserNotificationsWithContentHandler = procedure(param1: UNNotificationContent) of object;
  UNAuthorizationStatus = NSInteger;
  UNNotificationSetting = NSInteger;
  UNAlertStyle = NSInteger;
  UNAuthorizationOptions = NSUInteger;
  TUserNotificationsCompletionHandler = procedure(granted: Boolean; error: NSError) of object;
  TUserNotificationsCompletionHandler1 = procedure(param1: NSSet) of object;
  TUserNotificationsCompletionHandler2 = procedure(param1: UNNotificationSettings) of object;
  TUserNotificationsWithCompletionHandler = procedure(error: NSError) of object;
  TUserNotificationsCompletionHandler3 = procedure(param1: NSArray) of object;
  UNNotificationPresentationOptions = NSUInteger;
  TUserNotificationsWithCompletionHandler1 = procedure(param1: UNNotificationPresentationOptions) of object;
  TUserNotificationsWithCompletionHandler2 = procedure of object;

  UNUserNotificationCenterSupport = interface(IObjectiveC)
    ['{0D9872BF-27AB-46A9-928E-7AB0AB5C22CD}']
    function localizedUserNotificationStringForKey(key: NSString; arguments: NSArray): NSString; cdecl;
  end;

  UNNotificationRequestClass = interface(NSObjectClass)
    ['{766845C9-A53C-409A-A893-8FF5E2A3A295}']
    { class } function requestWithIdentifier(identifier: NSString; content: UNNotificationContent; trigger: UNNotificationTrigger): Pointer; cdecl;
  end;

  UNNotificationRequest = interface(NSObject)
    ['{E820727F-A92E-4FD1-932D-DEA46FDE531A}']
    function identifier: NSString; cdecl;
    function content: UNNotificationContent; cdecl;
    function trigger: UNNotificationTrigger; cdecl;
  end;
  TUNNotificationRequest = class(TOCGenericImport<UNNotificationRequestClass, UNNotificationRequest>)
  end;

  UNNotificationClass = interface(NSObjectClass)
    ['{4A126250-84C2-4B9A-B3F3-BE5EA13BE722}']
  end;

  UNNotification = interface(NSObject)
    ['{ABF55DA5-D3BF-4595-8624-939F911EAC73}']
    function date: NSDate; cdecl;
    function request: UNNotificationRequest; cdecl;
  end;
  TUNNotification = class(TOCGenericImport<UNNotificationClass, UNNotification>)
  end;

  UNNotificationActionClass = interface(NSObjectClass)
    ['{AAD80C86-7813-45E2-9E04-5A351819A2C4}']
    { class } function actionWithIdentifier(identifier: NSString; title: NSString;
      options: UNNotificationActionOptions): Pointer; cdecl;
  end;

  UNNotificationAction = interface(NSObject)
    ['{611AA3F4-23E3-4DA6-B754-705686F9F44A}']
    function identifier: NSString; cdecl;
    function title: NSString; cdecl;
    function options: UNNotificationActionOptions; cdecl;
  end;
  TUNNotificationAction = class(TOCGenericImport<UNNotificationActionClass, UNNotificationAction>)
  end;

  UNTextInputNotificationActionClass = interface(UNNotificationActionClass)
    ['{0A7AB4DD-B2D0-4639-9343-FB32BBB7B928}']
    { class } function actionWithIdentifier(identifier: NSString; title: NSString; options: UNNotificationActionOptions;
      textInputButtonTitle: NSString; textInputPlaceholder: NSString): Pointer; cdecl;
  end;

  UNTextInputNotificationAction = interface(UNNotificationAction)
    ['{8E4C487C-EEB2-48D9-8C06-14A00F8F8216}']
    function textInputButtonTitle: NSString; cdecl;
    function textInputPlaceholder: NSString; cdecl;
  end;
  TUNTextInputNotificationAction = class(TOCGenericImport<UNTextInputNotificationActionClass, UNTextInputNotificationAction>)
  end;

  UNNotificationAttachmentClass = interface(NSObjectClass)
    ['{140486D0-F352-4687-8BF4-272014BDD8AF}']
    { class } function attachmentWithIdentifier(identifier: NSString; URL: NSURL; options: NSDictionary; error: NSError): Pointer; cdecl;
  end;

  UNNotificationAttachment = interface(NSObject)
    ['{A28C94FE-2CFF-45AA-A684-569BF30BF56B}']
    function identifier: NSString; cdecl;
    function URL: NSURL; cdecl;
    function &type: NSString; cdecl;
  end;
  TUNNotificationAttachment = class(TOCGenericImport<UNNotificationAttachmentClass, UNNotificationAttachment>)
  end;

  UNNotificationCategoryClass = interface(NSObjectClass)
    ['{EB8B0B0A-65EC-403A-9372-DAD56B2CEA1A}']
    { class } function categoryWithIdentifier(identifier: NSString; actions: NSArray; intentIdentifiers: NSArray;
      options: UNNotificationCategoryOptions): Pointer; cdecl;
  end;

  UNNotificationCategory = interface(NSObject)
    ['{B4152D45-2CDD-4DB9-A8C1-8068E400E880}']
    function identifier: NSString; cdecl;
    function actions: NSArray; cdecl;
    function intentIdentifiers: NSArray; cdecl;
    function options: UNNotificationCategoryOptions; cdecl;
  end;
  TUNNotificationCategory = class(TOCGenericImport<UNNotificationCategoryClass, UNNotificationCategory>)
  end;

  UNNotificationSoundClass = interface(NSObjectClass)
    ['{FCE6B805-3175-4248-BB72-1FE34575B153}']
    { class } function defaultSound: Pointer; cdecl;
    { class } function soundNamed(name: NSString): Pointer; cdecl;
  end;

  UNNotificationSound = interface(NSObject)
    ['{34307A81-6AE8-459F-8403-21B36D2D1493}']
  end;
  TUNNotificationSound = class(TOCGenericImport<UNNotificationSoundClass, UNNotificationSound>)
  end;

  UNNotificationContentClass = interface(NSObjectClass)
    ['{5905664A-B654-4AAE-9CD1-B133744C5CC7}']
  end;

  UNNotificationContent = interface(NSObject)
    ['{79B9EAF0-E737-439E-9198-62A4D1FE6934}']
    function attachments: NSArray; cdecl;
    function badge: NSNumber; cdecl;
    function body: NSString; cdecl;
    function categoryIdentifier: NSString; cdecl;
    function launchImageName: NSString; cdecl;
    function sound: UNNotificationSound; cdecl;
    function subtitle: NSString; cdecl;
    function threadIdentifier: NSString; cdecl;
    function title: NSString; cdecl;
    function userInfo: NSDictionary; cdecl;
  end;
  TUNNotificationContent = class(TOCGenericImport<UNNotificationContentClass, UNNotificationContent>)
  end;

  UNMutableNotificationContentClass = interface(UNNotificationContentClass)
    ['{23144A48-3417-4DDC-9880-6EA2FA41FB94}']
  end;

  UNMutableNotificationContent = interface(UNNotificationContent)
    ['{23D4F7BE-4009-4C3A-A3FB-EF9A5B0271D4}']
    procedure setAttachments(attachments: NSArray); cdecl;
    function attachments: NSArray; cdecl;
    procedure setBadge(badge: NSNumber); cdecl;
    function badge: NSNumber; cdecl;
    procedure setBody(body: NSString); cdecl;
    function body: NSString; cdecl;
    procedure setCategoryIdentifier(categoryIdentifier: NSString); cdecl;
    function categoryIdentifier: NSString; cdecl;
    procedure setLaunchImageName(launchImageName: NSString); cdecl;
    function launchImageName: NSString; cdecl;
    procedure setSound(sound: UNNotificationSound); cdecl;
    function sound: UNNotificationSound; cdecl;
    procedure setSubtitle(subtitle: NSString); cdecl;
    function subtitle: NSString; cdecl;
    procedure setThreadIdentifier(threadIdentifier: NSString); cdecl;
    function threadIdentifier: NSString; cdecl;
    procedure setTitle(title: NSString); cdecl;
    function title: NSString; cdecl;
    procedure setUserInfo(userInfo: NSDictionary); cdecl;
    function userInfo: NSDictionary; cdecl;
  end;
  TUNMutableNotificationContent = class(TOCGenericImport<UNMutableNotificationContentClass, UNMutableNotificationContent>)
  end;

  UNNotificationTriggerClass = interface(NSObjectClass)
    ['{DF3BF20A-1545-47BB-971B-93CEA63AFF19}']
  end;

  UNNotificationTrigger = interface(NSObject)
    ['{FAFC8592-3F5B-4374-8FC3-8D5E9F5FAFA9}']
    function repeats: Boolean; cdecl;
  end;
  TUNNotificationTrigger = class(TOCGenericImport<UNNotificationTriggerClass, UNNotificationTrigger>)
  end;

  UNNotificationResponseClass = interface(NSObjectClass)
    ['{F42D92EC-5C1E-4EB9-879F-1ADD4C18D908}']
  end;

  UNNotificationResponse = interface(NSObject)
    ['{818F4DC8-18ED-4783-AB22-B35BC2422E2F}']
    function notification: UNNotification; cdecl;
    function actionIdentifier: NSString; cdecl;
  end;
  TUNNotificationResponse = class(TOCGenericImport<UNNotificationResponseClass, UNNotificationResponse>)
  end;

  UNTextInputNotificationResponseClass = interface(UNNotificationResponseClass)
    ['{9A3AE889-4DCE-445C-8B9B-CCE340AB904F}']
  end;

  UNTextInputNotificationResponse = interface(UNNotificationResponse)
    ['{5A12C54D-52BD-41DA-BFC5-ABF43900A11A}']
    function userText: NSString; cdecl;
  end;
  TUNTextInputNotificationResponse = class(TOCGenericImport<UNTextInputNotificationResponseClass, UNTextInputNotificationResponse>)
  end;

  UNNotificationServiceExtensionClass = interface(NSObjectClass)
    ['{0C1B0248-5108-4276-A55C-E05B2D937BF3}']
  end;

  UNNotificationServiceExtension = interface(NSObject)
    ['{2B180EA3-5AEB-42A6-8E7D-84B9D1F22D10}']
    procedure didReceiveNotificationRequest(request: UNNotificationRequest; withContentHandler: TUserNotificationsWithContentHandler); cdecl;
    procedure serviceExtensionTimeWillExpire; cdecl;
  end;
  TUNNotificationServiceExtension = class(TOCGenericImport<UNNotificationServiceExtensionClass, UNNotificationServiceExtension>)
  end;

  UNNotificationSettingsClass = interface(NSObjectClass)
    ['{83B7F625-FBAE-4EE3-8AC8-BD557B6124D3}']
  end;

  UNNotificationSettings = interface(NSObject)
    ['{587D433B-89B3-4607-8532-B8FD83DBDFBB}']
    function authorizationStatus: UNAuthorizationStatus; cdecl;
    function soundSetting: UNNotificationSetting; cdecl;
    function badgeSetting: UNNotificationSetting; cdecl;
    function alertSetting: UNNotificationSetting; cdecl;
    function notificationCenterSetting: UNNotificationSetting; cdecl;
    function lockScreenSetting: UNNotificationSetting; cdecl;
    function carPlaySetting: UNNotificationSetting; cdecl;
    function alertStyle: UNAlertStyle; cdecl;
  end;
  TUNNotificationSettings = class(TOCGenericImport<UNNotificationSettingsClass, UNNotificationSettings>)
  end;

  UNPushNotificationTriggerClass = interface(UNNotificationTriggerClass)
    ['{54519FC8-B579-41E0-94F0-C21602C55941}']
  end;

  UNPushNotificationTrigger = interface(UNNotificationTrigger)
    ['{19A824D2-8C7B-4DAE-954A-4E134FD5F16F}']
  end;
  TUNPushNotificationTrigger = class(TOCGenericImport<UNPushNotificationTriggerClass, UNPushNotificationTrigger>)
  end;

  UNTimeIntervalNotificationTriggerClass = interface(UNNotificationTriggerClass)
    ['{E297E94F-B29B-40E9-A1F1-8B8897B87714}']
    { class } function triggerWithTimeInterval(timeInterval: NSTimeInterval; repeats: Boolean): Pointer; cdecl;
  end;

  UNTimeIntervalNotificationTrigger = interface(UNNotificationTrigger)
    ['{F2FD0FB8-B3A0-4475-8CA6-8B4969069144}']
    function timeInterval: NSTimeInterval; cdecl;
    function nextTriggerDate: NSDate; cdecl;
  end;
  TUNTimeIntervalNotificationTrigger = class(TOCGenericImport<UNTimeIntervalNotificationTriggerClass, UNTimeIntervalNotificationTrigger>)
  end;

  UNCalendarNotificationTriggerClass = interface(UNNotificationTriggerClass)
    ['{FA2817E5-7BC5-444E-9940-CAC2ED682160}']
    { class } function triggerWithDateMatchingComponents(dateComponents: NSDateComponents; repeats: Boolean): Pointer; cdecl;
  end;

  UNCalendarNotificationTrigger = interface(UNNotificationTrigger)
    ['{CA10EE0F-D83E-40CA-AFE1-937B7B08C8E1}']
    function dateComponents: NSDateComponents; cdecl;
    function nextTriggerDate: NSDate; cdecl;
  end;
  TUNCalendarNotificationTrigger = class(TOCGenericImport<UNCalendarNotificationTriggerClass, UNCalendarNotificationTrigger>)
  end;

  UNLocationNotificationTriggerClass = interface(UNNotificationTriggerClass)
    ['{8C7EF509-C0A0-4DB8-9ED1-3EE2CF08FC71}']
    { class } function triggerWithRegion(region: CLRegion; repeats: Boolean): Pointer; cdecl;
  end;

  UNLocationNotificationTrigger = interface(UNNotificationTrigger)
    ['{58F20440-A87C-4CC4-8A74-DE6C4BCB7823}']
    function region: CLRegion; cdecl;
  end;
  TUNLocationNotificationTrigger = class(TOCGenericImport<UNLocationNotificationTriggerClass, UNLocationNotificationTrigger>)
  end;

  PUNLocationNotificationTrigger = Pointer;

  UNUserNotificationCenterClass = interface(NSObjectClass)
    ['{73086005-B709-44DF-A489-05E771ABAD66}']
    { class } function currentNotificationCenter: UNUserNotificationCenter; cdecl;
  end;

  UNUserNotificationCenter = interface(NSObject)
    ['{43FE4570-8CAB-44D9-8F7A-85060C962305}']
    procedure addNotificationRequest(request: UNNotificationRequest; withCompletionHandler: TUserNotificationsWithCompletionHandler); cdecl;
    function delegate: Pointer; cdecl;
    procedure getDeliveredNotificationsWithCompletionHandler(completionHandler: TUserNotificationsCompletionHandler3); cdecl;
    procedure getNotificationCategoriesWithCompletionHandler(completionHandler: TUserNotificationsCompletionHandler1); cdecl;
    procedure getNotificationSettingsWithCompletionHandler(completionHandler: TUserNotificationsCompletionHandler2); cdecl;
    procedure getPendingNotificationRequestsWithCompletionHandler(completionHandler: TUserNotificationsCompletionHandler3); cdecl;
    procedure removeAllDeliveredNotifications; cdecl;
    procedure removeAllPendingNotificationRequests; cdecl;
    procedure removeDeliveredNotificationsWithIdentifiers(identifiers: NSArray); cdecl;
    procedure removePendingNotificationRequestsWithIdentifiers(identifiers: NSArray); cdecl;
    procedure requestAuthorizationWithOptions(options: UNAuthorizationOptions; completionHandler: TUserNotificationsCompletionHandler); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setNotificationCategories(categories: NSSet); cdecl;
    function supportsContentExtensions: Boolean; cdecl;
  end;
  TUNUserNotificationCenter = class(TOCGenericImport<UNUserNotificationCenterClass, UNUserNotificationCenter>)
  end;

  PUNUserNotificationCenter = Pointer;

  UNUserNotificationCenterDelegate = interface(IObjectiveC)
    ['{53E6E4D3-F99B-4E8C-AA6B-6CCAF31F7252}']
    [MethodName('userNotificationCenter:openSettingsForNotification:')]
    procedure userNotificationCenter(center: UNUserNotificationCenter; notification: UNNotification); overload; cdecl;
    [MethodName('userNotificationCenter:didReceiveNotificationResponse:withCompletionHandler:')]
    procedure userNotificationCenter(center: UNUserNotificationCenter; response: UNNotificationResponse; completionHandler: Pointer); overload; cdecl;
    [MethodName('userNotificationCenter:willPresentNotification:withCompletionHandler:')]
    procedure userNotificationCenter(center: UNUserNotificationCenter; notification: UNNotification; completionHandler: Pointer); overload; cdecl;
  end;

function UNErrorDomain: NSString;
function UNNotificationAttachmentOptionsTypeHintKey: NSString;
function UNNotificationAttachmentOptionsThumbnailHiddenKey: NSString;
function UNNotificationAttachmentOptionsThumbnailClippingRectKey: NSString;
function UNNotificationAttachmentOptionsThumbnailTimeKey: NSString;
function UNNotificationDefaultActionIdentifier: NSString;
function UNNotificationDismissActionIdentifier: NSString;
function UserNotificationCenter: UNUserNotificationCenter;

const
  libUserNotifications = '/System/Library/Frameworks/UserNotifications.framework/UserNotifications';

implementation

{$IF Defined(IOS) and not Defined(CPUARM)}
uses
  Posix.Dlfcn;

var
  UserNotificationsModule: THandle;
{$ENDIF IOS}

function UserNotificationCenter: UNUserNotificationCenter;
begin
  Result := TUNUserNotificationCenter.OCClass.currentNotificationCenter;
end;

function UNErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libUserNotifications, 'UNErrorDomain');
end;

function UNNotificationAttachmentOptionsTypeHintKey: NSString;
begin
  Result := CocoaNSStringConst(libUserNotifications, 'UNNotificationAttachmentOptionsTypeHintKey');
end;

function UNNotificationAttachmentOptionsThumbnailHiddenKey: NSString;
begin
  Result := CocoaNSStringConst(libUserNotifications, 'UNNotificationAttachmentOptionsThumbnailHiddenKey');
end;

function UNNotificationAttachmentOptionsThumbnailClippingRectKey: NSString;
begin
  Result := CocoaNSStringConst(libUserNotifications, 'UNNotificationAttachmentOptionsThumbnailClippingRectKey');
end;

function UNNotificationAttachmentOptionsThumbnailTimeKey: NSString;
begin
  Result := CocoaNSStringConst(libUserNotifications, 'UNNotificationAttachmentOptionsThumbnailTimeKey');
end;

function UNNotificationDefaultActionIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libUserNotifications, 'UNNotificationDefaultActionIdentifier');
end;

function UNNotificationDismissActionIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libUserNotifications, 'UNNotificationDismissActionIdentifier');
end;

{$IF Defined(IOS) and not Defined(CPUARM)}
initialization
  UserNotificationsModule := dlopen(MarshaledAString(libUserNotifications), RTLD_LAZY);

finalization
  dlclose(UserNotificationsModule);
{$ENDIF IOS}

end.
