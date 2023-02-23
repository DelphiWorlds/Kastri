unit DW.iOSapi.EventKit;

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
  Macapi.ObjectiveC, Macapi.CoreFoundation,
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.CoreGraphics, iOSapi.CoreLocation, iOSapi.MapKit, iOSapi.AddressBook;

const
  EKAuthorizationStatusNotDetermined = 0;
  EKAuthorizationStatusRestricted = 1;
  EKAuthorizationStatusDenied = 2;
  EKAuthorizationStatusAuthorized = 3;
  EKWeekdaySunday = 1;
  EKWeekdayMonday = 2;
  EKWeekdayTuesday = 3;
  EKWeekdayWednesday = 4;
  EKWeekdayThursday = 5;
  EKWeekdayFriday = 6;
  EKWeekdaySaturday = 7;
  EKSunday = EKWeekdaySunday;
  EKMonday = EKWeekdayMonday;
  EKTuesday = EKWeekdayTuesday;
  EKWednesday = EKWeekdayWednesday;
  EKThursday = EKWeekdayThursday;
  EKFriday = EKWeekdayFriday;
  EKSaturday = EKWeekdaySaturday;
  EKRecurrenceFrequencyDaily = 0;
  EKRecurrenceFrequencyWeekly = 1;
  EKRecurrenceFrequencyMonthly = 2;
  EKRecurrenceFrequencyYearly = 3;
  EKParticipantTypeUnknown = 0;
  EKParticipantTypePerson = 1;
  EKParticipantTypeRoom = 2;
  EKParticipantTypeResource = 3;
  EKParticipantTypeGroup = 4;
  EKParticipantRoleUnknown = 0;
  EKParticipantRoleRequired = 1;
  EKParticipantRoleOptional = 2;
  EKParticipantRoleChair = 3;
  EKParticipantRoleNonParticipant = 4;
  EKParticipantScheduleStatusNone = 0;
  EKParticipantScheduleStatusPending = 1;
  EKParticipantScheduleStatusSent = 2;
  EKParticipantScheduleStatusDelivered = 3;
  EKParticipantScheduleStatusRecipientNotRecognized = 4;
  EKParticipantScheduleStatusNoPrivileges = 5;
  EKParticipantScheduleStatusDeliveryFailed = 6;
  EKParticipantScheduleStatusCannotDeliver = 7;
  EKParticipantScheduleStatusRecipientNotAllowed = 8;
  EKParticipantStatusUnknown = 0;
  EKParticipantStatusPending = 1;
  EKParticipantStatusAccepted = 2;
  EKParticipantStatusDeclined = 3;
  EKParticipantStatusTentative = 4;
  EKParticipantStatusDelegated = 5;
  EKParticipantStatusCompleted = 6;
  EKParticipantStatusInProcess = 7;
  EKCalendarTypeLocal = 0;
  EKCalendarTypeCalDAV = 1;
  EKCalendarTypeExchange = 2;
  EKCalendarTypeSubscription = 3;
  EKCalendarTypeBirthday = 4;
  EKCalendarEventAvailabilityNone = 0;
  EKCalendarEventAvailabilityBusy = 1;
  EKCalendarEventAvailabilityFree = 2;
  EKCalendarEventAvailabilityTentative = 4;
  EKCalendarEventAvailabilityUnavailable = 8;
  EKSourceTypeLocal = 0;
  EKSourceTypeExchange = 1;
  EKSourceTypeCalDAV = 2;
  EKSourceTypeMobileMe = 3;
  EKSourceTypeSubscribed = 4;
  EKSourceTypeBirthdays = 5;
  EKEntityTypeEvent = 0;
  EKEntityTypeReminder = 1;
  EKEntityMaskEvent = 1;
  EKEntityMaskReminder = 2;
  EKAlarmProximityNone = 0;
  EKAlarmProximityEnter = 1;
  EKAlarmProximityLeave = 2;
  EKAlarmTypeDisplay = 0;
  EKAlarmTypeAudio = 1;
  EKAlarmTypeProcedure = 2;
  EKAlarmTypeEmail = 3;
  EKReminderPriorityNone = 0;
  EKReminderPriorityHigh = 1;
  EKReminderPriorityMedium = 5;
  EKReminderPriorityLow = 9;
  EKEventAvailabilityNotSupported = -1;
  EKEventAvailabilityBusy = 0;
  EKEventAvailabilityFree = 1;
  EKEventAvailabilityTentative = 2;
  EKEventAvailabilityUnavailable = 3;
  EKEventStatusNone = 0;
  EKEventStatusConfirmed = 1;
  EKEventStatusTentative = 2;
  EKEventStatusCanceled = 3;
  EKSpanThisEvent = 0;
  EKSpanFutureEvents = 1;

type
  EKObject = interface;
  EKAlarm = interface;
  EKCalendar = interface;
  EKCalendarItem = interface;
  EKEvent = interface;
  EKEventStore = interface;
  EKParticipant = interface;
  EKRecurrenceDayOfWeek = interface;
  EKRecurrenceEnd = interface;
  EKRecurrenceRule = interface;
  EKReminder = interface;
  EKSource = interface;
  EKStructuredLocation = interface;
  EKVirtualConferenceRoomTypeDescriptor = interface;
  EKVirtualConferenceURLDescriptor = interface;
  EKVirtualConferenceDescriptor = interface;
  EKVirtualConferenceProvider = interface;

  PBoolean = ^Boolean;
  EKAuthorizationStatus = NSInteger;
  EKWeekday = NSInteger;
  EKRecurrenceFrequency = NSInteger;
  EKParticipantType = NSInteger;
  EKParticipantRole = NSInteger;
  EKParticipantScheduleStatus = NSInteger;
  EKParticipantStatus = NSInteger;
  EKCalendarType = NSInteger;
  EKCalendarEventAvailabilityMask = NSInteger;
  EKSourceType = NSInteger;
  EKEntityType = NSInteger;
  EKEntityMask = NSInteger;
  EKAlarmProximity = NSInteger;
  EKAlarmType = NSInteger;
  EKReminderPriority = NSInteger;
  EKEventAvailability = NSInteger;
  EKEventStatus = NSInteger;
  EKSpan = NSInteger;

  EKEventSearchCallback = procedure(event: EKEvent; stop: PBoolean) of object;

  EKEventStoreRequestAccessCompletionHandler = procedure(granted: Boolean; error: NSError) of object;
  ABRecordRef = CFTypeRef;
  ABAddressBookRef = CFTypeRef;
  EKVirtualConferenceRoomTypeIdentifier = NSString;
  TEKEventStoreBlockMethod1 = procedure(reminders: NSArray) of object;
  TEKVirtualConferenceProviderBlockMethod1 = procedure(param1: NSArray; param2: NSError) of object;
  TEKVirtualConferenceProviderBlockMethod2 = procedure(param1: EKVirtualConferenceDescriptor; param2: NSError) of object;

  EKObjectClass = interface(NSObjectClass)
    ['{1CBE647E-EAC3-41D5-B303-E3E2523B895C}']
  end;

  EKObject = interface(NSObject)
    ['{A1FD3CE1-80DC-412F-9D37-71CC0422453E}']
    function hasChanges: Boolean; cdecl;
    function isNew: Boolean; cdecl;
    function refresh: Boolean; cdecl;
    procedure reset; cdecl;
    procedure rollback; cdecl;
  end;
  TEKObject = class(TOCGenericImport<EKObjectClass, EKObject>) end;

  EKAlarmClass = interface(EKObjectClass)
    ['{16758912-4CB0-4405-957A-5988B5AC6417}']
    {class} function alarmWithAbsoluteDate(date: NSDate): EKAlarm; cdecl;
    {class} function alarmWithRelativeOffset(offset: NSTimeInterval): EKAlarm; cdecl;
  end;

  EKAlarm = interface(EKObject)
    ['{F14BE895-0CAF-499D-BB3C-F049C817FEEF}']
    function absoluteDate: NSDate; cdecl;
    function proximity: EKAlarmProximity; cdecl;
    function relativeOffset: NSTimeInterval; cdecl;
    procedure setAbsoluteDate(absoluteDate: NSDate); cdecl;
    procedure setProximity(proximity: EKAlarmProximity); cdecl;
    procedure setRelativeOffset(relativeOffset: NSTimeInterval); cdecl;
    procedure setStructuredLocation(structuredLocation: EKStructuredLocation); cdecl;
    procedure setUrl(url: NSURL); cdecl;
    function structuredLocation: EKStructuredLocation; cdecl;
    function url: NSURL; cdecl;
  end;
  TEKAlarm = class(TOCGenericImport<EKAlarmClass, EKAlarm>) end;

  EKCalendarClass = interface(EKObjectClass)
    ['{23035E0B-3B75-4BE6-A38C-C0A449AF6DC6}']
    {class} function calendarForEntityType(entityType: EKEntityType; eventStore: EKEventStore): EKCalendar; cdecl;
    {class} function calendarWithEventStore(eventStore: EKEventStore): EKCalendar; cdecl;
  end;

  EKCalendar = interface(EKObject)
    ['{333C2BD3-2648-4561-BBB9-4315F3520557}']
    function allowedEntityTypes: EKEntityMask; cdecl;
    function allowsContentModifications: Boolean; cdecl;
    function calendarIdentifier: NSString; cdecl;
    function CGColor: CGColorRef; cdecl;
    function isImmutable: Boolean; cdecl;
    function isSubscribed: Boolean; cdecl;
    procedure setCGColor(CGColor: CGColorRef); cdecl;
    procedure setSource(source: EKSource); cdecl;
    procedure setTitle(title: NSString); cdecl;
    function source: EKSource; cdecl;
    function supportedEventAvailabilities: EKCalendarEventAvailabilityMask; cdecl;
    function title: NSString; cdecl;
    function &type: EKCalendarType; cdecl;
  end;
  TEKCalendar = class(TOCGenericImport<EKCalendarClass, EKCalendar>) end;

  EKCalendarItemClass = interface(EKObjectClass)
    ['{598910D5-51A1-4167-B708-5BB04077E4A0}']
  end;

  EKCalendarItem = interface(EKObject)
    ['{2D8B9552-F7F5-491A-A48D-1D789817E73A}']
    procedure addAlarm(alarm: EKAlarm); cdecl;
    procedure addRecurrenceRule(rule: EKRecurrenceRule); cdecl;
    function alarms: NSArray; cdecl;
    function attendees: NSArray; cdecl;
    function calendar: EKCalendar; cdecl;
    function calendarItemExternalIdentifier: NSString; cdecl;
    function calendarItemIdentifier: NSString; cdecl;
    function creationDate: NSDate; cdecl;
    function hasAlarms: Boolean; cdecl;
    function hasAttendees: Boolean; cdecl;
    function hasNotes: Boolean; cdecl;
    function hasRecurrenceRules: Boolean; cdecl;
    function lastModifiedDate: NSDate; cdecl;
    function location: NSString; cdecl;
    function notes: NSString; cdecl;
    function recurrenceRules: NSArray; cdecl;
    procedure removeAlarm(alarm: EKAlarm); cdecl;
    procedure removeRecurrenceRule(rule: EKRecurrenceRule); cdecl;
    procedure setAlarms(alarms: NSArray); cdecl;
    procedure setCalendar(calendar: EKCalendar); cdecl;
    procedure setLocation(location: NSString); cdecl;
    procedure setNotes(notes: NSString); cdecl;
    procedure setRecurrenceRules(recurrenceRules: NSArray); cdecl;
    procedure setTimeZone(timeZone: NSTimeZone); cdecl;
    procedure setTitle(title: NSString); cdecl;
    procedure setURL(URL: NSURL); cdecl;
    function timeZone: NSTimeZone; cdecl;
    function title: NSString; cdecl;
    function URL: NSURL; cdecl;
    function UUID: NSString; cdecl;
  end;
  TEKCalendarItem = class(TOCGenericImport<EKCalendarItemClass, EKCalendarItem>) end;

  EKEventClass = interface(EKCalendarItemClass)
    ['{BFB02560-E29C-4DA1-9554-9A38AF659D68}']
    {class} function eventWithEventStore(eventStore: EKEventStore): EKEvent; cdecl;
  end;

  EKEvent = interface(EKCalendarItem)
    ['{B31C6D9B-42C1-4569-8D31-7667ACA32F9A}']
    function availability: EKEventAvailability; cdecl;
    function birthdayContactIdentifier: NSString; cdecl;
    function birthdayPersonID: NSInteger; cdecl;
    function birthdayPersonUniqueID: NSString; cdecl;
    function compareStartDateWithEvent(other: EKEvent): NSComparisonResult; cdecl;
    function endDate: NSDate; cdecl;
    function eventIdentifier: NSString; cdecl;
    function isAllDay: Boolean; cdecl;
    function isDetached: Boolean; cdecl;
    function occurrenceDate: NSDate; cdecl;
    function organizer: EKParticipant; cdecl;
    function refresh: Boolean; cdecl;
    procedure setAllDay(allDay: Boolean); cdecl;
    procedure setAvailability(availability: EKEventAvailability); cdecl;
    procedure setEndDate(endDate: NSDate); cdecl;
    procedure setStartDate(startDate: NSDate); cdecl;
    procedure setStructuredLocation(structuredLocation: EKStructuredLocation); cdecl;
    function startDate: NSDate; cdecl;
    function status: EKEventStatus; cdecl;
    function structuredLocation: EKStructuredLocation; cdecl;
  end;
  TEKEvent = class(TOCGenericImport<EKEventClass, EKEvent>) end;

  EKEventStoreClass = interface(NSObjectClass)
    ['{C9D8D069-600A-40CC-BA85-EE8AFEE961A4}']
    {class} function authorizationStatusForEntityType(entityType: EKEntityType): EKAuthorizationStatus; cdecl;
  end;

  EKEventStore = interface(NSObject)
    ['{ECF2613C-BF45-4548-AC53-9ADF4422C817}']
    function calendarItemsWithExternalIdentifier(externalIdentifier: NSString): NSArray; cdecl;
    function calendarItemWithIdentifier(identifier: NSString): EKCalendarItem; cdecl;
    function calendars: NSArray; cdecl;
    function calendarsForEntityType(entityType: EKEntityType): NSArray; cdecl;
    function calendarWithIdentifier(identifier: NSString): EKCalendar; cdecl;
    procedure cancelFetchRequest(fetchIdentifier: Pointer); cdecl;
    function commit(error: PPointer): Boolean; cdecl;
    function defaultCalendarForNewEvents: EKCalendar; cdecl;
    function defaultCalendarForNewReminders: EKCalendar; cdecl;
    function delegateSources: NSArray; cdecl;
    procedure enumerateEventsMatchingPredicate(predicate: NSPredicate; usingBlock: EKEventSearchCallback); cdecl;
    function eventsMatchingPredicate(predicate: NSPredicate): NSArray; cdecl;
    function eventStoreIdentifier: NSString; cdecl;
    function eventWithIdentifier(identifier: NSString): EKEvent; cdecl;
    function fetchRemindersMatchingPredicate(predicate: NSPredicate; completion: TEKEventStoreBlockMethod1): Pointer; cdecl;
    function initWithAccessToEntityTypes(entityTypes: EKEntityMask): Pointer; cdecl;
    function initWithSources(sources: NSArray): Pointer; cdecl;
    function predicateForCompletedRemindersWithCompletionDateStarting(startDate: NSDate; ending: NSDate; calendars: NSArray): NSPredicate; cdecl;
    function predicateForEventsWithStartDate(startDate: NSDate; endDate: NSDate; calendars: NSArray): NSPredicate; cdecl;
    function predicateForIncompleteRemindersWithDueDateStarting(startDate: NSDate; ending: NSDate; calendars: NSArray): NSPredicate; cdecl;
    function predicateForRemindersInCalendars(calendars: NSArray): NSPredicate; cdecl;
    procedure refreshSourcesIfNecessary; cdecl;
    function removeCalendar(calendar: EKCalendar; commit: Boolean; error: PPointer): Boolean; cdecl;
    function removeEvent(event: EKEvent; span: EKSpan; error: PPointer): Boolean; overload; cdecl;
    function removeEvent(event: EKEvent; span: EKSpan; commit: Boolean; error: PPointer): Boolean; overload; cdecl;
    function removeReminder(reminder: EKReminder; commit: Boolean; error: PPointer): Boolean; cdecl;
    procedure requestAccessToEntityType(entityType: EKEntityType; completion: EKEventStoreRequestAccessCompletionHandler); cdecl;
    procedure reset; cdecl;
    function saveCalendar(calendar: EKCalendar; commit: Boolean; error: PPointer): Boolean; cdecl;
    function saveEvent(event: EKEvent; span: EKSpan; commit: Boolean; error: PPointer): Boolean; overload; cdecl;
    function saveEvent(event: EKEvent; span: EKSpan; error: PPointer): Boolean; overload; cdecl;
    function saveReminder(reminder: EKReminder; commit: Boolean; error: PPointer): Boolean; cdecl;
    function sources: NSArray; cdecl;
    function sourceWithIdentifier(identifier: NSString): EKSource; cdecl;
  end;
  TEKEventStore = class(TOCGenericImport<EKEventStoreClass, EKEventStore>) end;

  EKParticipantClass = interface(EKObjectClass)
    ['{877A20BD-A35A-492A-A142-B1671938846B}']
  end;

  EKParticipant = interface(EKObject)
    ['{8F471BF5-EEEE-4C70-B69A-2E30F443542B}']
    function ABRecordWithAddressBook(addressBook: ABAddressBookRef): ABRecordRef; cdecl;
    function contactPredicate: NSPredicate; cdecl;
    function isCurrentUser: Boolean; cdecl;
    function name: NSString; cdecl;
    function participantRole: EKParticipantRole; cdecl;
    function participantStatus: EKParticipantStatus; cdecl;
    function participantType: EKParticipantType; cdecl;
    function URL: NSURL; cdecl;
  end;
  TEKParticipant = class(TOCGenericImport<EKParticipantClass, EKParticipant>) end;

  EKRecurrenceDayOfWeekClass = interface(NSObjectClass)
    ['{9EE611D3-F42A-49E5-B893-3DD2D5443079}']
    {class} function dayOfWeek(dayOfTheWeek: EKWeekday): Pointer; overload; cdecl;
    {class} function dayOfWeek(dayOfTheWeek: EKWeekday; weekNumber: NSInteger): Pointer; overload; cdecl;
  end;

  EKRecurrenceDayOfWeek = interface(NSObject)
    ['{4C716179-3D74-4AB6-845C-BB3E7F5F5B75}']
    function dayOfTheWeek: EKWeekday; cdecl;
    function initWithDayOfTheWeek(dayOfTheWeek: EKWeekday; weekNumber: NSInteger): Pointer; cdecl;
    function weekNumber: NSInteger; cdecl;
  end;
  TEKRecurrenceDayOfWeek = class(TOCGenericImport<EKRecurrenceDayOfWeekClass, EKRecurrenceDayOfWeek>) end;

  EKRecurrenceEndClass = interface(NSObjectClass)
    ['{270AE147-E764-47A6-A1EB-DC67B27529FB}']
    {class} function recurrenceEndWithEndDate(endDate: NSDate): Pointer; cdecl;
    {class} function recurrenceEndWithOccurrenceCount(occurrenceCount: NSUInteger): Pointer; cdecl;
  end;

  EKRecurrenceEnd = interface(NSObject)
    ['{FE8F6198-17B3-49FA-B6DE-9420570D2752}']
    function endDate: NSDate; cdecl;
    function occurrenceCount: NSUInteger; cdecl;
  end;
  TEKRecurrenceEnd = class(TOCGenericImport<EKRecurrenceEndClass, EKRecurrenceEnd>) end;

  EKRecurrenceRuleClass = interface(EKObjectClass)
    ['{D404D131-5C39-4E95-8E59-F48DF0299392}']
  end;

  EKRecurrenceRule = interface(EKObject)
    ['{610106FD-6A64-46A4-9DA3-24A0BA3EE7A7}']
    function calendarIdentifier: NSString; cdecl;
    function daysOfTheMonth: NSArray; cdecl;
    function daysOfTheWeek: NSArray; cdecl;
    function daysOfTheYear: NSArray; cdecl;
    function firstDayOfTheWeek: NSInteger; cdecl;
    function frequency: EKRecurrenceFrequency; cdecl;
    function initRecurrenceWithFrequency(&type: EKRecurrenceFrequency; interval: NSInteger; &end: EKRecurrenceEnd): Pointer; overload; cdecl;
    function initRecurrenceWithFrequency(&type: EKRecurrenceFrequency; interval: NSInteger; daysOfTheWeek: NSArray; daysOfTheMonth: NSArray;
      monthsOfTheYear: NSArray; weeksOfTheYear: NSArray; daysOfTheYear: NSArray; setPositions: NSArray; &end: EKRecurrenceEnd): Pointer; overload; cdecl;
    function interval: NSInteger; cdecl;
    function monthsOfTheYear: NSArray; cdecl;
    function recurrenceEnd: EKRecurrenceEnd; cdecl;
    function setPositions: NSArray; cdecl;
    procedure setRecurrenceEnd(recurrenceEnd: EKRecurrenceEnd); cdecl;
    function weeksOfTheYear: NSArray; cdecl;
  end;
  TEKRecurrenceRule = class(TOCGenericImport<EKRecurrenceRuleClass, EKRecurrenceRule>) end;

  EKReminderClass = interface(EKCalendarItemClass)
    ['{C1994F54-E7FE-4F6A-BE15-3C6B5A1D2760}']
    {class} function reminderWithEventStore(eventStore: EKEventStore): EKReminder; cdecl;
  end;

  EKReminder = interface(EKCalendarItem)
    ['{E75E7608-FEE0-4E58-BBCC-3A8BB0C490A2}']
    function completionDate: NSDate; cdecl;
    function dueDateComponents: NSDateComponents; cdecl;
    function isCompleted: Boolean; cdecl;
    function priority: NSUInteger; cdecl;
    procedure setCompleted(completed: Boolean); cdecl;
    procedure setCompletionDate(completionDate: NSDate); cdecl;
    procedure setDueDateComponents(dueDateComponents: NSDateComponents); cdecl;
    procedure setPriority(priority: NSUInteger); cdecl;
    procedure setStartDateComponents(startDateComponents: NSDateComponents); cdecl;
    function startDateComponents: NSDateComponents; cdecl;
  end;
  TEKReminder = class(TOCGenericImport<EKReminderClass, EKReminder>) end;

  EKSourceClass = interface(EKObjectClass)
    ['{190B4D16-5491-441D-8214-B8F9073AE6D9}']
  end;

  EKSource = interface(EKObject)
    ['{2741FEBB-C580-4CA5-A324-82542C7EDC65}']
    function calendars: NSSet; cdecl;
    function calendarsForEntityType(entityType: EKEntityType): NSSet; cdecl;
    function isDelegate: Boolean; cdecl;
    function sourceIdentifier: NSString; cdecl;
    function sourceType: EKSourceType; cdecl;
    function title: NSString; cdecl;
  end;
  TEKSource = class(TOCGenericImport<EKSourceClass, EKSource>) end;

  EKStructuredLocationClass = interface(EKObjectClass)
    ['{4C6482BA-95A5-40C2-8C0B-1F3DA0BC9774}']
    {class} function locationWithMapItem(mapItem: MKMapItem): Pointer; cdecl;
    {class} function locationWithTitle(title: NSString): Pointer; cdecl;
  end;

  EKStructuredLocation = interface(EKObject)
    ['{DF23ED70-9019-429A-90E1-0C8F56D08A87}']
    function geoLocation: CLLocation; cdecl;
    function radius: Double; cdecl;
    procedure setGeoLocation(geoLocation: CLLocation); cdecl;
    procedure setRadius(radius: Double); cdecl;
    procedure setTitle(title: NSString); cdecl;
    function title: NSString; cdecl;
  end;
  TEKStructuredLocation = class(TOCGenericImport<EKStructuredLocationClass, EKStructuredLocation>) end;

  EKVirtualConferenceRoomTypeDescriptorClass = interface(NSObjectClass)
    ['{BBF0DC08-058E-46F7-BE1F-D8D1FBFCB5E4}']
    {class} function new: Pointer; cdecl;
  end;

  EKVirtualConferenceRoomTypeDescriptor = interface(NSObject)
    ['{B4741F59-918F-4261-9A8E-2B1AF817C138}']
    function identifier: EKVirtualConferenceRoomTypeIdentifier; cdecl;
    function initWithTitle(title: NSString; identifier: EKVirtualConferenceRoomTypeIdentifier): Pointer; cdecl;
    function title: NSString; cdecl;
  end;
  TEKVirtualConferenceRoomTypeDescriptor = class(TOCGenericImport<EKVirtualConferenceRoomTypeDescriptorClass, EKVirtualConferenceRoomTypeDescriptor>) end;

  EKVirtualConferenceURLDescriptorClass = interface(NSObjectClass)
    ['{527BAFD3-2479-4710-B477-D9DDDDD452E7}']
    {class} function new: Pointer; cdecl;
  end;

  EKVirtualConferenceURLDescriptor = interface(NSObject)
    ['{95506E95-F859-427D-8EC7-17BD7AC00D0B}']
    function initWithTitle(title: NSString; URL: NSURL): Pointer; cdecl;
    function title: NSString; cdecl;
    function URL: NSURL; cdecl;
  end;
  TEKVirtualConferenceURLDescriptor = class(TOCGenericImport<EKVirtualConferenceURLDescriptorClass, EKVirtualConferenceURLDescriptor>) end;

  EKVirtualConferenceDescriptorClass = interface(NSObjectClass)
    ['{6E8680F3-BD73-4353-83BC-9C6BDEEE8FBD}']
    {class} function new: Pointer; cdecl;
  end;

  EKVirtualConferenceDescriptor = interface(NSObject)
    ['{09052190-10E0-4991-9D6A-2413B710156A}']
    function conferenceDetails: NSString; cdecl;
    function initWithTitle(title: NSString; URLDescriptors: NSArray; conferenceDetails: NSString): Pointer; cdecl;
    function title: NSString; cdecl;
    function URLDescriptors: NSArray; cdecl;
  end;
  TEKVirtualConferenceDescriptor = class(TOCGenericImport<EKVirtualConferenceDescriptorClass, EKVirtualConferenceDescriptor>) end;

  EKVirtualConferenceProviderClass = interface(NSObjectClass)
    ['{CFD67F43-036B-4794-9DEE-A267152BCA33}']
  end;

  EKVirtualConferenceProvider = interface(NSObject)
    ['{E609F739-C3AE-4710-90B5-5E7697210A6C}']
    procedure fetchAvailableRoomTypesWithCompletionHandler(completionHandler: TEKVirtualConferenceProviderBlockMethod1); cdecl;
    procedure fetchVirtualConferenceForIdentifier(identifier: EKVirtualConferenceRoomTypeIdentifier; completionHandler: TEKVirtualConferenceProviderBlockMethod2); cdecl;
  end;
  TEKVirtualConferenceProvider = class(TOCGenericImport<EKVirtualConferenceProviderClass, EKVirtualConferenceProvider>) end;

function EKEventStoreChangedNotification: NSString;
function EKErrorDomain: NSString;

const
  libEventKit = '/System/Library/Frameworks/EventKit.framework/EventKit';

implementation

uses
  Posix.Dlfcn;

var
  EventKitModule: THandle;

function EKEventStoreChangedNotification: NSString;
begin
  Result := CocoaNSStringConst(libEventKit, 'EKEventStoreChangedNotification');
end;

function EKErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libEventKit, 'EKErrorDomain');
end;

initialization
  EventKitModule := dlopen(MarshaledAString(libEventKit), RTLD_LAZY);

finalization
  dlclose(EventKitModule);

end.