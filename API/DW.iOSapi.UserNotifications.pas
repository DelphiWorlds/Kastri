unit DW.iOSapi.UserNotifications;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2026 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation,
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
  UNErrorCodeContentProvidingObjectNotAllowed = 1500;
  UNErrorCodeContentProvidingInvalid = 1501;
  UNErrorCodeBadgeInputInvalid = 1600;
  UNNotificationActionOptionAuthenticationRequired = 1;
  UNNotificationActionOptionDestructive = 2;
  UNNotificationActionOptionForeground = 4;
  UNNotificationCategoryOptionCustomDismissAction = 1;
  UNNotificationCategoryOptionAllowInCarPlay = 2;
  UNNotificationCategoryOptionHiddenPreviewsShowTitle = 4;
  UNNotificationCategoryOptionHiddenPreviewsShowSubtitle = 8;
  UNNotificationCategoryOptionAllowAnnouncement = 16;
  UNNotificationInterruptionLevelPassive = 0;
  UNNotificationInterruptionLevelActive = 1;
  UNNotificationInterruptionLevelTimeSensitive = 2;
  UNNotificationInterruptionLevelCritical = 3;
  UNAuthorizationStatusNotDetermined = 0;
  UNAuthorizationStatusDenied = 1;
  UNAuthorizationStatusAuthorized = 2;
  UNAuthorizationStatusProvisional = 3;
  UNAuthorizationStatusEphemeral = 4;
  UNShowPreviewsSettingAlways = 0;
  UNShowPreviewsSettingWhenAuthenticated = 1;
  UNShowPreviewsSettingNever = 2;
  UNNotificationSettingNotSupported = 0;
  UNNotificationSettingDisabled = 1;
  UNNotificationSettingEnabled = 2;
  UNAlertStyleNone = 0;
  UNAlertStyleBanner = 1;
  UNAlertStyleAlert = 2;
  UNAuthorizationOptionBadge = 1;
  UNAuthorizationOptionSound = 2;
  UNAuthorizationOptionAlert = 4;
  UNAuthorizationOptionCarPlay = 8;
  UNAuthorizationOptionCriticalAlert = 16;
  UNAuthorizationOptionProvidesAppNotificationSettings = 32;
  UNAuthorizationOptionProvisional = 64;
  UNAuthorizationOptionAnnouncement = 128;
  UNAuthorizationOptionTimeSensitive = 256;
  UNNotificationPresentationOptionBadge = 1;
  UNNotificationPresentationOptionSound = 2;
  UNNotificationPresentationOptionAlert = 4;
  UNNotificationPresentationOptionList = 8;
  UNNotificationPresentationOptionBanner = 16;

type
  UNNotification = interface;
  UNNotificationAction = interface;
  UNTextInputNotificationAction = interface;
  UNNotificationActionIcon = interface;
  UNNotificationAttachment = interface;
  UNNotificationCategory = interface;
  UNNotificationContentProviding = interface;
  UNNotificationContent = interface;
  UNMutableNotificationContent = interface;
  UNNotificationRequest = interface;
  UNNotificationResponse = interface;
  UNTextInputNotificationResponse = interface;
  UNNotificationServiceExtension = interface;
  UNNotificationSettings = interface;
  UNNotificationSound = interface;
  UNNotificationTrigger = interface;
  UNPushNotificationTrigger = interface;
  UNTimeIntervalNotificationTrigger = interface;
  UNCalendarNotificationTrigger = interface;
  UNLocationNotificationTrigger = interface;
  UNUserNotificationCenter = interface;
  UNUserNotificationCenterDelegate = interface;

  UNErrorCode = NSInteger;
  UNNotificationActionOptions = NSInteger;
  UNNotificationCategoryOptions = NSInteger;
  UNNotificationInterruptionLevel = NSInteger;
  UNAuthorizationStatus = NSInteger;
  UNShowPreviewsSetting = NSInteger;
  UNNotificationSetting = NSInteger;
  UNAlertStyle = NSInteger;
  UNNotificationSoundName = NSString;
  UNAuthorizationOptions = NSInteger;
  UNNotificationPresentationOptions = NSInteger;
  TUNNotificationServiceExtensionBlockMethod1 = procedure(contentToDeliver: UNNotificationContent) of object;
  TUNUserNotificationCenterBlockMethod1 = procedure(granted: Boolean; error: NSError) of object;
  TUNUserNotificationCenterBlockMethod2 = procedure(categories: NSSet) of object;
  TUNUserNotificationCenterBlockMethod3 = procedure(settings: UNNotificationSettings) of object;
  TUNUserNotificationCenterBlockMethod4 = procedure(error: NSError) of object;
  TUNUserNotificationCenterBlockMethod5 = procedure(requests: NSArray) of object;
  TUNUserNotificationCenterBlockMethod6 = procedure(notifications: NSArray) of object;
  TUNUserNotificationCenterDelegateBlockMethod1 = procedure(options: UNNotificationPresentationOptions) of object;
  TUNUserNotificationCenterDelegateBlockMethod2 = procedure of object;

  UNNotificationClass = interface(NSObjectClass)
    ['{D8EAEC48-F31F-4729-AC97-369777DEE85A}']
  end;

  UNNotification = interface(NSObject)
    ['{9BD08FB1-EAD4-409F-99B9-07C6B2233AD9}']
    function date: NSDate; cdecl;
    function request: UNNotificationRequest; cdecl;
  end;
  TUNNotification = class(TOCGenericImport<UNNotificationClass, UNNotification>) end;

  UNNotificationActionClass = interface(NSObjectClass)
    ['{54642E05-431C-4BE0-AD71-8A6BF71ACF54}']
    {class} function actionWithIdentifier(identifier: NSString; title: NSString; options: UNNotificationActionOptions): Pointer; overload; cdecl;
    {class} function actionWithIdentifier(identifier: NSString; title: NSString; options: UNNotificationActionOptions;
      icon: UNNotificationActionIcon): Pointer; overload; cdecl;
  end;

  UNNotificationAction = interface(NSObject)
    ['{5F4A9A21-9A1C-423D-B941-6D46D057723B}']
    function icon: UNNotificationActionIcon; cdecl;
    function identifier: NSString; cdecl;
    function options: UNNotificationActionOptions; cdecl;
    function title: NSString; cdecl;
  end;
  TUNNotificationAction = class(TOCGenericImport<UNNotificationActionClass, UNNotificationAction>) end;

  UNTextInputNotificationActionClass = interface(UNNotificationActionClass)
    ['{C5C379F3-B15C-47CE-9350-FD5D667BA261}']
    {class} function actionWithIdentifier(identifier: NSString; title: NSString; options: UNNotificationActionOptions; textInputButtonTitle: NSString;
      textInputPlaceholder: NSString): Pointer; overload; cdecl;
    {class} function actionWithIdentifier(identifier: NSString; title: NSString; options: UNNotificationActionOptions; icon: UNNotificationActionIcon;
      textInputButtonTitle: NSString; textInputPlaceholder: NSString): Pointer; overload; cdecl;
  end;

  UNTextInputNotificationAction = interface(UNNotificationAction)
    ['{3C8417B0-79A1-41D0-AECC-DE95852D8C18}']
    function textInputButtonTitle: NSString; cdecl;
    function textInputPlaceholder: NSString; cdecl;
  end;
  TUNTextInputNotificationAction = class(TOCGenericImport<UNTextInputNotificationActionClass, UNTextInputNotificationAction>) end;

  UNNotificationActionIconClass = interface(NSObjectClass)
    ['{4477A59E-E677-4414-A81E-B5E13377ED63}']
    {class} function iconWithSystemImageName(systemImageName: NSString): Pointer; cdecl;
    {class} function iconWithTemplateImageName(templateImageName: NSString): Pointer; cdecl;
  end;

  UNNotificationActionIcon = interface(NSObject)
    ['{D9457529-8255-45AA-A655-890506E2430E}']
  end;
  TUNNotificationActionIcon = class(TOCGenericImport<UNNotificationActionIconClass, UNNotificationActionIcon>) end;

  UNNotificationAttachmentClass = interface(NSObjectClass)
    ['{0053282C-2113-46EF-8EE3-4F60FFD4F676}']
    {class} function attachmentWithIdentifier(identifier: NSString; URL: NSURL; options: NSDictionary; error: PPointer): Pointer; cdecl;
  end;

  UNNotificationAttachment = interface(NSObject)
    ['{5E92FD3B-F8F6-4E2A-9512-91800A616D54}']
    function &type: NSString; cdecl;
    function identifier: NSString; cdecl;
    function URL: NSURL; cdecl;
  end;
  TUNNotificationAttachment = class(TOCGenericImport<UNNotificationAttachmentClass, UNNotificationAttachment>) end;

  UNNotificationCategoryClass = interface(NSObjectClass)
    ['{149254AF-CF62-4925-BE81-EEF141B34A8B}']
    {class} function categoryWithIdentifier(identifier: NSString; actions: NSArray; intentIdentifiers: NSArray;
      hiddenPreviewsBodyPlaceholder: NSString; categorySummaryFormat: NSString; options: UNNotificationCategoryOptions): Pointer; overload; cdecl;
    {class} function categoryWithIdentifier(identifier: NSString; actions: NSArray; intentIdentifiers: NSArray;
      hiddenPreviewsBodyPlaceholder: NSString; options: UNNotificationCategoryOptions): Pointer; overload; cdecl;
    {class} function categoryWithIdentifier(identifier: NSString; actions: NSArray; intentIdentifiers: NSArray;
      options: UNNotificationCategoryOptions): Pointer; overload; cdecl;
  end;

  UNNotificationCategory = interface(NSObject)
    ['{896DC04D-E269-4635-9B00-7C9DCC3CCA0C}']
    function actions: NSArray; cdecl;
    function categorySummaryFormat: NSString; cdecl;
    function hiddenPreviewsBodyPlaceholder: NSString; cdecl;
    function identifier: NSString; cdecl;
    function intentIdentifiers: NSArray; cdecl;
    function options: UNNotificationCategoryOptions; cdecl;
  end;
  TUNNotificationCategory = class(TOCGenericImport<UNNotificationCategoryClass, UNNotificationCategory>) end;

  UNNotificationContentProviding = interface(IObjectiveC)
    ['{B3CA65D1-3C10-4A1E-BAE4-F716C10B965E}']
  end;

  UNNotificationContentClass = interface(NSObjectClass)
    ['{A511FCE0-8237-4DC4-8395-8C4D40B38AD3}']
  end;

  UNNotificationContent = interface(NSObject)
    ['{6677A931-BA25-43A1-A4E9-8A5357BFD01B}']
    function attachments: NSArray; cdecl;
    function badge: NSNumber; cdecl;
    function body: NSString; cdecl;
    function categoryIdentifier: NSString; cdecl;
    function contentByUpdatingWithProvider(provider: Pointer; error: PPointer): UNNotificationContent; cdecl;
    function filterCriteria: NSString; cdecl;
    function interruptionLevel: UNNotificationInterruptionLevel; cdecl;
    function launchImageName: NSString; cdecl;
    function relevanceScore: Double; cdecl;
    function sound: UNNotificationSound; cdecl;
    function subtitle: NSString; cdecl;
    function summaryArgument: NSString; cdecl; // API_DEPRECATED("summaryArgument is ignored", ios(12.0, 15.0), watchos(5.0, 8.0), tvos(12.0, 15.0))
    function summaryArgumentCount: NSUInteger; cdecl; // API_DEPRECATED("summaryArgumentCount is ignored", ios(12.0, 15.0), watchos(5.0, 8.0), tvos(12.0, 15.0))
    function targetContentIdentifier: NSString; cdecl;
    function threadIdentifier: NSString; cdecl;
    function title: NSString; cdecl;
    function userInfo: NSDictionary; cdecl;
  end;
  TUNNotificationContent = class(TOCGenericImport<UNNotificationContentClass, UNNotificationContent>) end;

  UNMutableNotificationContentClass = interface(UNNotificationContentClass)
    ['{E69F0275-A73A-4B3C-89CF-D8DE3EB7841C}']
  end;

  UNMutableNotificationContent = interface(UNNotificationContent)
    ['{A6A844EE-2088-4A54-B5FE-B136C5200425}']
    function attachments: NSArray; cdecl;
    function badge: NSNumber; cdecl;
    function body: NSString; cdecl;
    function categoryIdentifier: NSString; cdecl;
    function filterCriteria: NSString; cdecl;
    function interruptionLevel: UNNotificationInterruptionLevel; cdecl;
    function launchImageName: NSString; cdecl;
    function relevanceScore: Double; cdecl;
    procedure setAttachments(attachments: NSArray); cdecl;
    procedure setBadge(badge: NSNumber); cdecl;
    procedure setBody(body: NSString); cdecl;
    procedure setCategoryIdentifier(categoryIdentifier: NSString); cdecl;
    procedure setFilterCriteria(filterCriteria: NSString); cdecl;
    procedure setInterruptionLevel(interruptionLevel: UNNotificationInterruptionLevel); cdecl;
    procedure setLaunchImageName(launchImageName: NSString); cdecl;
    procedure setRelevanceScore(relevanceScore: Double); cdecl;
    procedure setSound(sound: UNNotificationSound); cdecl;
    procedure setSubtitle(subtitle: NSString); cdecl;
    procedure setSummaryArgument(summaryArgument: NSString); cdecl; // API_DEPRECATED("summaryArgument is ignored", ios(12.0, 15.0), watchos(5.0, 8.0), tvos(12.0, 15.0))
    procedure setSummaryArgumentCount(summaryArgumentCount: NSUInteger); cdecl; // API_DEPRECATED("summaryArgumentCount is ignored", ios(12.0, 15.0), watchos(5.0, 8.0), tvos(12.0, 15.0))
    procedure setTargetContentIdentifier(targetContentIdentifier: NSString); cdecl;
    procedure setThreadIdentifier(threadIdentifier: NSString); cdecl;
    procedure setTitle(title: NSString); cdecl;
    procedure setUserInfo(userInfo: NSDictionary); cdecl;
    function sound: UNNotificationSound; cdecl;
    function subtitle: NSString; cdecl;
    function summaryArgument: NSString; cdecl; // API_DEPRECATED("summaryArgument is ignored", ios(12.0, 15.0), watchos(5.0, 8.0), tvos(12.0, 15.0))
    function summaryArgumentCount: NSUInteger; cdecl; // API_DEPRECATED("summaryArgumentCount is ignored", ios(12.0, 15.0), watchos(5.0, 8.0), tvos(12.0, 15.0))
    function targetContentIdentifier: NSString; cdecl;
    function threadIdentifier: NSString; cdecl;
    function title: NSString; cdecl;
    function userInfo: NSDictionary; cdecl;
  end;
  TUNMutableNotificationContent = class(TOCGenericImport<UNMutableNotificationContentClass, UNMutableNotificationContent>) end;

  UNNotificationRequestClass = interface(NSObjectClass)
    ['{0304D133-51C7-4CBA-808A-54FB64735895}']
    {class} function requestWithIdentifier(identifier: NSString; content: UNNotificationContent; trigger: UNNotificationTrigger): Pointer; cdecl;
  end;

  UNNotificationRequest = interface(NSObject)
    ['{E5B2593F-7DEC-49ED-806A-2CD11BA60643}']
    function content: UNNotificationContent; cdecl;
    function identifier: NSString; cdecl;
    function trigger: UNNotificationTrigger; cdecl;
  end;
  TUNNotificationRequest = class(TOCGenericImport<UNNotificationRequestClass, UNNotificationRequest>) end;

  UNNotificationResponseClass = interface(NSObjectClass)
    ['{0E791895-8446-4C84-9587-A48C7E3CD3F7}']
  end;

  UNNotificationResponse = interface(NSObject)
    ['{6E76568C-AB77-4514-90CF-0438031DA243}']
    function actionIdentifier: NSString; cdecl;
    function notification: UNNotification; cdecl;
  end;
  TUNNotificationResponse = class(TOCGenericImport<UNNotificationResponseClass, UNNotificationResponse>) end;

  UNTextInputNotificationResponseClass = interface(UNNotificationResponseClass)
    ['{8DDF4C38-FBFF-4B63-9111-089D2973E806}']
  end;

  UNTextInputNotificationResponse = interface(UNNotificationResponse)
    ['{356EE7C3-ED11-46C1-8739-ADE3BE2CDEE3}']
    function userText: NSString; cdecl;
  end;
  TUNTextInputNotificationResponse = class(TOCGenericImport<UNTextInputNotificationResponseClass, UNTextInputNotificationResponse>) end;

  UNNotificationServiceExtensionClass = interface(NSObjectClass)
    ['{C36A3480-9577-4186-8234-64AD3C645AC2}']
  end;

  UNNotificationServiceExtension = interface(NSObject)
    ['{A24A646C-110C-4FCB-9DD1-8212F3E644FB}']
    procedure didReceiveNotificationRequest(request: UNNotificationRequest; withContentHandler: TUNNotificationServiceExtensionBlockMethod1); cdecl;
    procedure serviceExtensionTimeWillExpire; cdecl;
  end;
  TUNNotificationServiceExtension = class(TOCGenericImport<UNNotificationServiceExtensionClass, UNNotificationServiceExtension>) end;

  UNNotificationSettingsClass = interface(NSObjectClass)
    ['{ECA72DC2-1A0D-4ACB-A454-53BE0EC2F751}']
  end;

  UNNotificationSettings = interface(NSObject)
    ['{FE7B4A3A-5FF8-413C-98A5-7C0C5B0C96BD}']
    function alertSetting: UNNotificationSetting; cdecl;
    function alertStyle: UNAlertStyle; cdecl;
    function announcementSetting: UNNotificationSetting; cdecl;
    function authorizationStatus: UNAuthorizationStatus; cdecl;
    function badgeSetting: UNNotificationSetting; cdecl;
    function carPlaySetting: UNNotificationSetting; cdecl;
    function criticalAlertSetting: UNNotificationSetting; cdecl;
    function directMessagesSetting: UNNotificationSetting; cdecl;
    function lockScreenSetting: UNNotificationSetting; cdecl;
    function notificationCenterSetting: UNNotificationSetting; cdecl;
    function providesAppNotificationSettings: Boolean; cdecl;
    function scheduledDeliverySetting: UNNotificationSetting; cdecl;
    function showPreviewsSetting: UNShowPreviewsSetting; cdecl;
    function soundSetting: UNNotificationSetting; cdecl;
    function timeSensitiveSetting: UNNotificationSetting; cdecl;
  end;
  TUNNotificationSettings = class(TOCGenericImport<UNNotificationSettingsClass, UNNotificationSettings>) end;

  UNNotificationSoundClass = interface(NSObjectClass)
    ['{2EFAB85B-26F6-4002-9303-C22A46162616}']
    {class} function criticalSoundNamed(name: UNNotificationSoundName; withAudioVolume: Single): Pointer; overload; cdecl;
    {class} function criticalSoundNamed(name: UNNotificationSoundName): Pointer; overload; cdecl;
    {class} function defaultCriticalSound: UNNotificationSound; cdecl;
    {class} function defaultCriticalSoundWithAudioVolume(volume: Single): Pointer; cdecl;
    {class} function defaultRingtoneSound: UNNotificationSound; cdecl;
    {class} function defaultSound: UNNotificationSound; cdecl;
    {class} function ringtoneSoundNamed(name: UNNotificationSoundName): Pointer; cdecl;
    {class} function soundNamed(name: UNNotificationSoundName): Pointer; cdecl;
  end;

  UNNotificationSound = interface(NSObject)
    ['{7382ACA4-FD53-4252-B1E2-04D714EAADEC}']
  end;
  TUNNotificationSound = class(TOCGenericImport<UNNotificationSoundClass, UNNotificationSound>) end;

  UNNotificationTriggerClass = interface(NSObjectClass)
    ['{828A983B-39D1-486B-9EF9-4216A85C24C4}']
  end;

  UNNotificationTrigger = interface(NSObject)
    ['{8AA763D6-3B88-446D-AC0E-882F74BE9649}']
    function repeats: Boolean; cdecl;
  end;
  TUNNotificationTrigger = class(TOCGenericImport<UNNotificationTriggerClass, UNNotificationTrigger>) end;

  UNPushNotificationTriggerClass = interface(UNNotificationTriggerClass)
    ['{51137A8C-EE89-44C6-AA3E-0CC6122CC49D}']
  end;

  UNPushNotificationTrigger = interface(UNNotificationTrigger)
    ['{B9DF26A6-9A81-41E7-A841-B87FE858D296}']
  end;
  TUNPushNotificationTrigger = class(TOCGenericImport<UNPushNotificationTriggerClass, UNPushNotificationTrigger>) end;

  UNTimeIntervalNotificationTriggerClass = interface(UNNotificationTriggerClass)
    ['{CF30B6A0-EB14-45D9-865C-6783BF0AE5AD}']
    {class} function triggerWithTimeInterval(timeInterval: NSTimeInterval; repeats: Boolean): Pointer; cdecl;
  end;

  UNTimeIntervalNotificationTrigger = interface(UNNotificationTrigger)
    ['{327665CA-1BDD-4EA3-ABF6-5F3F357EC3AE}']
    function nextTriggerDate: NSDate; cdecl;
    function timeInterval: NSTimeInterval; cdecl;
  end;
  TUNTimeIntervalNotificationTrigger = class(TOCGenericImport<UNTimeIntervalNotificationTriggerClass, UNTimeIntervalNotificationTrigger>) end;

  UNCalendarNotificationTriggerClass = interface(UNNotificationTriggerClass)
    ['{D9D1FDC6-41E2-40DD-B9E4-AE572DC2CB3E}']
    {class} function triggerWithDateMatchingComponents(dateComponents: NSDateComponents; repeats: Boolean): Pointer; cdecl;
  end;

  UNCalendarNotificationTrigger = interface(UNNotificationTrigger)
    ['{C4A9940F-F15D-43A8-9C82-0CA0A94B213E}']
    function dateComponents: NSDateComponents; cdecl;
    function nextTriggerDate: NSDate; cdecl;
  end;
  TUNCalendarNotificationTrigger = class(TOCGenericImport<UNCalendarNotificationTriggerClass, UNCalendarNotificationTrigger>) end;

  UNLocationNotificationTriggerClass = interface(UNNotificationTriggerClass)
    ['{6229CA2B-FCBC-4071-95E3-574AE723A089}']
    {class} function triggerWithRegion(region: CLRegion; repeats: Boolean): Pointer; cdecl;
  end;

  UNLocationNotificationTrigger = interface(UNNotificationTrigger)
    ['{3104E48C-DD2D-490B-97B2-7C292F831A45}']
    function region: CLRegion; cdecl;
  end;
  TUNLocationNotificationTrigger = class(TOCGenericImport<UNLocationNotificationTriggerClass, UNLocationNotificationTrigger>) end;

  UNUserNotificationCenterClass = interface(NSObjectClass)
    ['{0803C60D-41E5-4988-876D-7B064DF0D1EC}']
    {class} function currentNotificationCenter: UNUserNotificationCenter; cdecl;
  end;

  UNUserNotificationCenter = interface(NSObject)
    ['{7FC1C7A7-87E9-4EA2-BC63-B6B8ABC2A72B}']
    procedure addNotificationRequest(request: UNNotificationRequest; withCompletionHandler: TUNUserNotificationCenterBlockMethod4); cdecl;
    function delegate: Pointer; cdecl;
    procedure getDeliveredNotificationsWithCompletionHandler(completionHandler: TUNUserNotificationCenterBlockMethod6); cdecl;
    procedure getNotificationCategoriesWithCompletionHandler(completionHandler: TUNUserNotificationCenterBlockMethod2); cdecl;
    procedure getNotificationSettingsWithCompletionHandler(completionHandler: TUNUserNotificationCenterBlockMethod3); cdecl;
    procedure getPendingNotificationRequestsWithCompletionHandler(completionHandler: TUNUserNotificationCenterBlockMethod5); cdecl;
    procedure removeAllDeliveredNotifications; cdecl;
    procedure removeAllPendingNotificationRequests; cdecl;
    procedure removeDeliveredNotificationsWithIdentifiers(identifiers: NSArray); cdecl;
    procedure removePendingNotificationRequestsWithIdentifiers(identifiers: NSArray); cdecl;
    procedure requestAuthorizationWithOptions(options: UNAuthorizationOptions; completionHandler: TUNUserNotificationCenterBlockMethod1); cdecl;
    procedure setBadgeCount(newBadgeCount: NSInteger; withCompletionHandler: TUNUserNotificationCenterBlockMethod4); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setNotificationCategories(categories: NSSet); cdecl;
    function supportsContentExtensions: Boolean; cdecl;
  end;
  TUNUserNotificationCenter = class(TOCGenericImport<UNUserNotificationCenterClass, UNUserNotificationCenter>) end;

  UNUserNotificationCenterDelegate = interface(IObjectiveC)
    ['{19F3D3EF-C1F3-416D-B619-568BF96D74EB}']
    procedure userNotificationCenter(center: UNUserNotificationCenter; openSettingsForNotification: UNNotification); overload; cdecl;
    procedure userNotificationCenter(center: UNUserNotificationCenter; didReceiveNotificationResponse: UNNotificationResponse;
      withCompletionHandler: Pointer); overload; cdecl;
    procedure userNotificationCenter(center: UNUserNotificationCenter; willPresentNotification: UNNotification;
      withCompletionHandler: Pointer); overload; cdecl;
  end;

function UNErrorDomain: NSString;
function UNNotificationAttachmentOptionsTypeHintKey: NSString;
function UNNotificationAttachmentOptionsThumbnailHiddenKey: NSString;
function UNNotificationAttachmentOptionsThumbnailClippingRectKey: NSString;
function UNNotificationAttachmentOptionsThumbnailTimeKey: NSString;
function UNNotificationDefaultActionIdentifier: NSString;
function UNNotificationDismissActionIdentifier: NSString;

const
  libUserNotifications = '/System/Library/Frameworks/UserNotifications.framework/UserNotifications';

implementation

uses
  Posix.Dlfcn;

var
  UserNotificationsModule: THandle;

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

initialization
  UserNotificationsModule := dlopen(MarshaledAString(libUserNotifications), RTLD_LAZY);

finalization
  dlclose(UserNotificationsModule);

end.