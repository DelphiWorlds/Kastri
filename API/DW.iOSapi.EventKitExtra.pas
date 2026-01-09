unit DW.iOSapi.EventKitExtra;

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
  Macapi.ObjectiveC,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation;

type
  EKObject = interface;
  EKRecurrenceEnd = interface;
  EKRecurrenceRule = interface;

  EKRecurrenceFrequency = NSInteger;

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

implementation

end.
