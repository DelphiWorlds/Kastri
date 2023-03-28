unit DW.Notifications.iOS;

// ***************** NOTE **************************
//      THIS UNIT IS CURRENTLY EXPERIMENTAL
//           USE AT YOUR OWN RISK!
//
// It may or may not be removed from the Kastri library

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
  iOSapi.Foundation,
  Macapi.ObjectiveC,
  DW.Notifications, DW.iOSapi.UserNotifications;

type
  TNotificationMode = (DidReceive, WillPresent);

  TPlatformNotifications = class;

  TUserNotificationCenterDelegate = class(TOCLocal, UNUserNotificationCenterDelegate)
  private
    FNotifications: TNotifications;
    procedure ProcessLocalNotification(request: UNNotificationRequest; const AMode: TNotificationMode);
    procedure ProcessNotificationRequest(request: UNNotificationRequest; const AMode: TNotificationMode);
    procedure ProcessRemoteNotification(request: UNNotificationRequest; const AMode: TNotificationMode);
  protected
    property Notifications: TNotifications read FNotifications write FNotifications;
  public
    { UNUserNotificationCenterDelegate }
    [MethodName('userNotificationCenter:openSettingsForNotification:')]
    procedure userNotificationCenter(center: UNUserNotificationCenter; notification: UNNotification); overload; cdecl;
    [MethodName('userNotificationCenter:didReceiveNotificationResponse:withCompletionHandler:')]
    procedure userNotificationCenter(center: UNUserNotificationCenter; response: UNNotificationResponse; completionHandler: Pointer); overload; cdecl;
    [MethodName('userNotificationCenter:willPresentNotification:withCompletionHandler:')]
    procedure userNotificationCenter(center: UNUserNotificationCenter; notification: UNNotification; completionHandler: Pointer); overload; cdecl;
  end;

  TAuthorizationCallback = reference to procedure(const AGranted: Boolean);

  TPlatformNotifications = class(TCustomPlatformNotifications)
  private
    class var FNotificationCenterDelegate: TUserNotificationCenterDelegate;
    class destructor DestroyClass;
  private
    procedure AddNotificationRequestCompletionHandler(error: NSError);
    function GetNotificationContent(ANotification: TNotification): UNMutableNotificationContent;
    function GetNotificationTrigger(ANotification: TNotification; const AImmediate: Boolean): UNCalendarNotificationTrigger;
    procedure IssueNotification(const ANotification: TNotification; const AImmediate: Boolean);
    procedure RequestAuthorization;
    procedure RequestAuthorizationCompletionHandler(granted: Boolean; error: NSError);
  protected
    procedure CancelAll; override;
    procedure CancelNotification(const AName: string); override;
    procedure PresentNotification(const ANotification: TNotification); override;
    procedure ScheduleNotification(const ANotification: TNotification); override;
  public
    class procedure UpdateDelegate;
  public
    constructor Create(const ANotifications: TNotifications); override;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils, System.Messaging, System.DateUtils,
  Macapi.Helpers, Macapi.ObjCRuntime,
  iOSapi.CocoaTypes,
  FMX.Platform,
  DW.OSLog,
  DW.Macapi.ObjCRuntime, DW.iOSapi.Helpers, DW.Macapi.Helpers;

type
  TOpenNotifications = class(TNotifications);

function NotificationCenter: UNUserNotificationCenter;
begin
  Result := TUNUserNotificationCenter.OCClass.currentNotificationCenter;
end;

// Non-buggy version (as opposed to the one in Macapi.Helpers)
function GetGMTDateTime(const ADateTime: TDateTime): TDateTime;
begin
  Result := IncSecond(ADateTime, -TNSTimeZone.Wrap(TNSTimeZone.OCClass.localTimeZone).secondsFromGMT);
end;

{ TUserNotificationCenterDelegate }

procedure TUserNotificationCenterDelegate.ProcessNotificationRequest(request: UNNotificationRequest; const AMode: TNotificationMode);
begin
  if (request.trigger <> nil) and request.trigger.isKindOfClass(objc_getClass('UNPushNotificationTrigger')) then
    ProcessRemoteNotification(request, AMode)
  else
    ProcessLocalNotification(request, AMode);
end;

procedure TUserNotificationCenterDelegate.ProcessRemoteNotification(request: UNNotificationRequest; const AMode: TNotificationMode);
var
  LJSON: string;
  LUserInfo: NSMutableDictionary;
begin
  LUserInfo := TNSMutableDictionary.Wrap(TNSMutableDictionary.OCClass.dictionaryWithDictionary(request.content.userInfo));
  if AMode = TNotificationMode.DidReceive then
    LUserInfo.setValueForKey(StringToID('1'), StrToNSStr('didReceive'));
  LJSON := TiOSHelperEx.NSDictionaryToJSON(LUserInfo);
  TMessageManager.DefaultManager.SendMessage(nil, TPushRemoteNotificationMessage.Create(TPushNotificationData.Create(LJSON)));
end;

procedure TUserNotificationCenterDelegate.ProcessLocalNotification(request: UNNotificationRequest; const AMode: TNotificationMode);
var
  LNotification: TNotification;
  LContent: UNNotificationContent;
  LUserInfo: TNSDictionaryHelper;
begin
  if FNotifications <> nil then
  begin
    LContent := request.content;
    LNotification.Name := NSStrToStr(request.identifier);
    LNotification.AlertBody := NSStrToStr(LContent.body);
    LNotification.Title := NSStrToStr(LContent.title);
    LNotification.Subtitle := NSStrToStr(LContent.subtitle);
    LNotification.EnableSound := LContent.sound <> nil;
    // Result.SoundName := ?
    LNotification.HasAction := LContent.categoryIdentifier <> nil;
    LUserInfo := TNSDictionaryHelper.Create(LContent.userInfo);
    LNotification.RepeatInterval := TRepeatInterval(LUserInfo.GetValue('RepeatInterval', 0));
    TOpenNotifications(FNotifications).DoNotificationReceived(LNotification);
  end;
end;

procedure TUserNotificationCenterDelegate.userNotificationCenter(center: UNUserNotificationCenter;
  response: UNNotificationResponse; completionHandler: Pointer);
var
  LBlockImp: procedure; cdecl;
begin
  ProcessNotificationRequest(response.notification.request, TNotificationMode.DidReceive);
  @LBlockImp := imp_implementationWithBlock(completionHandler);
  LBlockImp;
  imp_removeBlock(@LBlockImp);
end;

procedure TUserNotificationCenterDelegate.userNotificationCenter(center: UNUserNotificationCenter;
  notification: UNNotification; completionHandler: Pointer);
var
  LBlockImp: procedure(options: UNNotificationPresentationOptions); cdecl;
  LOptions: UNNotificationPresentationOptions;
begin
  ProcessNotificationRequest(notification.request, TNotificationMode.WillPresent);
  @LBlockImp := imp_implementationWithBlock(completionHandler);
  LOptions := UNNotificationPresentationOptionAlert;
  LBlockImp(LOptions);
  imp_removeBlock(@LBlockImp);
end;

procedure TUserNotificationCenterDelegate.userNotificationCenter(center: UNUserNotificationCenter; notification: UNNotification);
begin
  //
end;

{ TPlatformNotifications }

class procedure TPlatformNotifications.UpdateDelegate;
begin
  FNotificationCenterDelegate := TUserNotificationCenterDelegate.Create;
  NotificationCenter.setDelegate(FNotificationCenterDelegate.GetObjectID);
end;

class destructor TPlatformNotifications.DestroyClass;
begin
  FNotificationCenterDelegate.Free;
end;

constructor TPlatformNotifications.Create(const ANotifications: TNotifications);
begin
  inherited;
  TPlatformNotifications.UpdateDelegate;
  FNotificationCenterDelegate.Notifications := ANotifications;
  RequestAuthorization;
end;

destructor TPlatformNotifications.Destroy;
begin
  //
  inherited;
end;

procedure TPlatformNotifications.RequestAuthorizationCompletionHandler(granted: Boolean; error: NSError);
begin
  //
end;

procedure TPlatformNotifications.RequestAuthorization;
var
  LOptions: UNAuthorizationOptions;
begin
  LOptions := UNAuthorizationOptionSound or UNAuthorizationOptionAlert or UNAuthorizationOptionBadge or UNAuthorizationOptionCarPlay;
  NotificationCenter.requestAuthorizationWithOptions(LOptions, RequestAuthorizationCompletionHandler);
end;

procedure TPlatformNotifications.CancelAll;
begin
  NotificationCenter.removeAllPendingNotificationRequests;
end;

procedure TPlatformNotifications.CancelNotification(const AName: string);
begin
  NotificationCenter.removePendingNotificationRequestsWithIdentifiers(StringArrayToNSArray([AName]));
end;

function TPlatformNotifications.GetNotificationContent(ANotification: TNotification): UNMutableNotificationContent;
var
  LSound: Pointer;
  LUserInfo: TNSMutableDictionaryHelper;
begin
  Result := TUNMutableNotificationContent.Create;
  Result.setTitle(StrToNSStr(ANotification.Title));
  Result.setSubtitle(StrToNSStr(ANotification.SubTitle));
  Result.setBody(StrToNSStr(ANotification.AlertBody));
  Result.setBadge(TNSNumber.Wrap(TNSNumber.OCClass.numberWithInteger(ANotification.Number)));
  if ANotification.EnableSound then
  begin
    if ANotification.SoundName.IsEmpty then
      LSound := TUNNotificationSound.OCClass.defaultSound
    else
      LSound := TUNNotificationSound.OCClass.soundNamed(StrToNSStr(ANotification.SoundName));
    Result.setSound(TUNNotificationSound.Wrap(LSound));
  end
  else
    Result.setSound(nil);
  LUserInfo := TNSMutableDictionaryHelper.Create(TNSMutableDictionary.Create);
  LUserInfo.SetValue(Ord(ANotification.RepeatInterval), 'RepeatInterval');
  Result.setUserInfo(LUserInfo.Dictionary);
end;

function TPlatformNotifications.GetNotificationTrigger(ANotification: TNotification; const AImmediate: Boolean): UNCalendarNotificationTrigger;
const
  cDayDateUnits = NSHourCalendarUnit or NSMinuteCalendarUnit or NSSecondCalendarUnit;
  cAllDateUnits = NSYearCalendarUnit or NSMonthCalendarUnit or NSDayCalendarUnit or cDayDateUnits;
  cRepeatingDateUnits: array[TRepeatInterval] of NSUInteger = (
    cDayDateUnits, { None }
    NSSecondCalendarUnit, { Second }
    NSMinuteCalendarUnit or NSSecondCalendarUnit, { Minute }
    cDayDateUnits, { Hour }
    NSDayCalendarUnit or cDayDateUnits, { Day }
    NSWeekCalendarUnit or cDayDateUnits, { Week }
    NSWeekdayCalendarUnit or NSMonthCalendarUnit or NSDayCalendarUnit or cDayDateUnits, { Weekday }
    NSMonthCalendarUnit or NSDayCalendarUnit or cDayDateUnits, { Month }
    NSQuarterCalendarUnit or NSDayCalendarUnit or cDayDateUnits, { Quarter - according to Apple: "largely unimplemented" }
    NSYearCalendarUnit or NSDayCalendarUnit or NSMonthCalendarUnit or cDayDateUnits, { Year }
    cDayDateUnits { Era - why would you use a notification with this repeat interval??? }
  );
var
  LTrigger: Pointer;
  LCalendar: NSCalendar;
  LTriggerDate: NSDateComponents;
  LDateUnits: NSUInteger;
  LRepeating: Boolean;
  LDate: TDateTime;
begin
  LCalendar := TNSCalendar.Wrap(TNSCalendar.OCClass.currentCalendar);
  LDateUnits := cDayDateUnits;
  // Does not seem to consistently allow immediate within a second of the current time
  LDate := IncSecond(Now);
  if not AImmediate then
  begin
    LDateUnits := cRepeatingDateUnits[ANotification.RepeatInterval];
    LDate := ANotification.FireDate;
  end;
  LDate := GetGMTDateTime(LDate);
  LTriggerDate := LCalendar.components(LDateUnits, DateTimeToNSDate(LDate));
  LRepeating := (ANotification.RepeatInterval <> TRepeatInterval.None) and not AImmediate;
  LTrigger := TUNCalendarNotificationTrigger.OCClass.triggerWithDateMatchingComponents(LTriggerDate, LRepeating);
  Result := TUNCalendarNotificationTrigger.Wrap(LTrigger);
end;

procedure TPlatformNotifications.IssueNotification(const ANotification: TNotification; const AImmediate: Boolean);
var
  LContent: UNMutableNotificationContent;
  LTrigger: UNCalendarNotificationTrigger;
  LPointer: Pointer;
  LRequest: UNNotificationRequest;
  LName: string;
begin
  LName := ANotification.Name;
  if LName.IsEmpty then
    LName := 'ImmediateNotification';
  LContent := GetNotificationContent(ANotification);
  LTrigger := GetNotificationTrigger(ANotification, AImmediate);
  LPointer := TUNNotificationRequest.OCClass.requestWithIdentifier(StrToNSStr(LName), LContent, LTrigger);
  LRequest := TUNNotificationRequest.Wrap(LPointer);
  NotificationCenter.addNotificationRequest(LRequest, AddNotificationRequestCompletionHandler);
end;

procedure TPlatformNotifications.PresentNotification(const ANotification: TNotification);
begin
  IssueNotification(ANotification, True);
end;

procedure TPlatformNotifications.ScheduleNotification(const ANotification: TNotification);
begin
  IssueNotification(ANotification, False);
end;

procedure TPlatformNotifications.AddNotificationRequestCompletionHandler(error: NSError);
begin
  // If error = nil: all good :-) Note: May be in a separate thread
end;

end.
