unit DW.Androidapi.JNI.AndroidX.WearOngoing;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2025 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.App, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.JNI.Os,
  Androidapi.JNI.Util,
  // DW
  DW.Androidapi.JNI.AndroidX.App, DW.Androidapi.JNI.AndroidX.Content;

type
  JOngoingActivity = interface;
  JOngoingActivity_Builder = interface;
  JSerializationHelper = interface;
  JStatus = interface;
  JStatus_Builder = interface;
  JStatus_Part = interface;
  JStatus_TimerOrStopwatchPart = interface;
  JStatus_StopwatchPart = interface;
  JStatus_TextPart = interface;
  JStatus_TimerPart = interface;
  JTimeDependentText = interface;

  JOngoingActivityClass = interface(JObjectClass)
    ['{C3D18138-CE5A-4EA7-BAAB-683EDFBF5A7B}']
    {class} function recoverOngoingActivity(context: JContext; predicate: Jfunction_Predicate): JOngoingActivity; cdecl; overload;
    {class} function recoverOngoingActivity(context: JContext): JOngoingActivity; cdecl; overload;
    {class} function recoverOngoingActivity(context: JContext; i: Integer): JOngoingActivity; cdecl; overload;
  end;

  [JavaSignature('androidx/wear/ongoing/OngoingActivity')]
  JOngoingActivity = interface(JObject)
    ['{FE098703-396E-43F9-B728-004D63F79862}']
    procedure apply(context: JContext); cdecl;
    function getAnimatedIcon: JIcon; cdecl;
    function getCategory: JString; cdecl;
    function getLocusId: JLocusIdCompat; cdecl;
    function getNotificationId: Integer; cdecl;
    function getOngoingActivityId: Integer; cdecl;
    function getStaticIcon: JIcon; cdecl;
    function getStatus: JStatus; cdecl;
    function getTag: JString; cdecl;
    function getTimestamp: Int64; cdecl;
    function getTitle: JString; cdecl;
    function getTouchIntent: JPendingIntent; cdecl;
    procedure update(context: JContext; status: JStatus); cdecl;
  end;
  TJOngoingActivity = class(TJavaGenericImport<JOngoingActivityClass, JOngoingActivity>) end;

  JOngoingActivity_BuilderClass = interface(JObjectClass)
    ['{24FF4752-FB28-48DA-A1F3-6BB77678B9F3}']
    {class} function init(context: JContext; i: Integer; builder: JNotificationCompat_Builder): JOngoingActivity_Builder; cdecl; overload;
    {class} function init(context: JContext; string_: JString; i: Integer; builder: JNotificationCompat_Builder): JOngoingActivity_Builder; cdecl; overload;
  end;

  [JavaSignature('androidx/wear/ongoing/OngoingActivity$Builder')]
  JOngoingActivity_Builder = interface(JObject)
    ['{A09790E5-FA6B-4BC9-944C-217B5C5D87A6}']
    function build: JOngoingActivity; cdecl;
    function setAnimatedIcon(icon: JIcon): JOngoingActivity_Builder; cdecl; overload;
    function setAnimatedIcon(i: Integer): JOngoingActivity_Builder; cdecl; overload;
    function setCategory(string_: JString): JOngoingActivity_Builder; cdecl;
    function setLocusId(locusIdCompat: JLocusIdCompat): JOngoingActivity_Builder; cdecl;
    function setOngoingActivityId(i: Integer): JOngoingActivity_Builder; cdecl;
    function setStaticIcon(icon: JIcon): JOngoingActivity_Builder; cdecl; overload;
    function setStaticIcon(i: Integer): JOngoingActivity_Builder; cdecl; overload;
    function setStatus(status: JStatus): JOngoingActivity_Builder; cdecl;
    function setTitle(string_: JString): JOngoingActivity_Builder; cdecl;
    function setTouchIntent(pendingIntent: JPendingIntent): JOngoingActivity_Builder; cdecl;
  end;
  TJOngoingActivity_Builder = class(TJavaGenericImport<JOngoingActivity_BuilderClass, JOngoingActivity_Builder>) end;

  JSerializationHelperClass = interface(JObjectClass)
    ['{B3E98843-3702-4969-956E-5FC468E048C9}']
    {class} procedure copy(bundle: JBundle; bundle1: JBundle); cdecl;
    {class} function create(notification: JNotification): JOngoingActivity; cdecl; overload;
    {class} function create(bundle: JBundle): JOngoingActivity; cdecl; overload;
    {class} function hasOngoingActivity(notification: JNotification): Boolean; cdecl;
  end;

  [JavaSignature('androidx/wear/ongoing/SerializationHelper')]
  JSerializationHelper = interface(JObject)
    ['{2660C93B-B030-4764-9C70-F1FEF16AC029}']
  end;
  TJSerializationHelper = class(TJavaGenericImport<JSerializationHelperClass, JSerializationHelper>) end;

  JStatusClass = interface(JObjectClass)
    ['{8AEFCC86-C657-447A-B374-89A6217245E6}']
    {class} function forPart(part: JStatus_Part): JStatus; cdecl;
  end;

  [JavaSignature('androidx/wear/ongoing/Status')]
  JStatus = interface(JObject)
    ['{AF405978-1132-4351-BE9A-DD0059484C79}']
    function getNextChangeTimeMillis(l: Int64): Int64; cdecl;
    function getPart(string_: JString): JStatus_Part; cdecl;
    function getPartNames: JSet; cdecl;
    function getTemplates: JList; cdecl;
    function getText(context: JContext; l: Int64): JCharSequence; cdecl;
  end;
  TJongoing_Status = class(TJavaGenericImport<JStatusClass, JStatus>) end;

  JStatus_BuilderClass = interface(JObjectClass)
    ['{9B2952FF-985C-4FE2-8C71-A44F3EA93DA9}']
    {class} function init: JStatus_Builder; cdecl;
  end;

  [JavaSignature('androidx/wear/ongoing/Status$Builder')]
  JStatus_Builder = interface(JObject)
    ['{B15F4165-4599-417F-9E6C-6CC08281EC84}']
    function addPart(string_: JString; part: JStatus_Part): JStatus_Builder; cdecl;
    function addTemplate(charSequence: JCharSequence): JStatus_Builder; cdecl;
    function build: JStatus; cdecl;
  end;
  TJStatus_Builder = class(TJavaGenericImport<JStatus_BuilderClass, JStatus_Builder>) end;

  JStatus_PartClass = interface(JObjectClass)
    ['{200E9ACE-40F7-40F7-B1F4-B4D2C664F010}']
  end;

  [JavaSignature('androidx/wear/ongoing/Status$Part')]
  JStatus_Part = interface(JObject)
    ['{6D7CF182-7C92-4CEB-8363-0FA794CB4BB2}']
  end;
  TJStatus_Part = class(TJavaGenericImport<JStatus_PartClass, JStatus_Part>) end;

  JStatus_TimerOrStopwatchPartClass = interface(JStatus_PartClass)
    ['{A3BBE6E4-6CC3-4A10-9FA9-A6A3B25FE55D}']
  end;

  [JavaSignature('androidx/wear/ongoing/Status$TimerOrStopwatchPart')]
  JStatus_TimerOrStopwatchPart = interface(JStatus_Part)
    ['{5EE4EB98-54D6-46A5-A984-23E3A1D24764}']
    function equals(object_: JObject): Boolean; cdecl;
    function getNextChangeTimeMillis(l: Int64): Int64; cdecl;
    function getPausedAtMillis: Int64; cdecl;
    function getText(context: JContext; l: Int64): JCharSequence; cdecl;
    function getTimeZeroMillis: Int64; cdecl;
    function getTotalDurationMillis: Int64; cdecl;
    function hasTotalDuration: Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function isCountDown: Boolean; cdecl;
    function isPaused: Boolean; cdecl;
  end;
  TJStatus_TimerOrStopwatchPart = class(TJavaGenericImport<JStatus_TimerOrStopwatchPartClass, JStatus_TimerOrStopwatchPart>) end;

  JStatus_StopwatchPartClass = interface(JStatus_TimerOrStopwatchPartClass)
    ['{785386C5-E103-48D6-97D8-8630B8BF2D7A}']
    {class} function init(l: Int64; l1: Int64; l2: Int64): JStatus_StopwatchPart; cdecl; overload;
    {class} function init(l: Int64; l1: Int64): JStatus_StopwatchPart; cdecl; overload;
    {class} function init(l: Int64): JStatus_StopwatchPart; cdecl; overload;
  end;

  [JavaSignature('androidx/wear/ongoing/Status$StopwatchPart')]
  JStatus_StopwatchPart = interface(JStatus_TimerOrStopwatchPart)
    ['{5533B779-AC8F-4C65-ADAD-C2920F8A3D20}']
  end;
  TJStatus_StopwatchPart = class(TJavaGenericImport<JStatus_StopwatchPartClass, JStatus_StopwatchPart>) end;

  JStatus_TextPartClass = interface(JStatus_PartClass)
    ['{8EA7DA59-F203-4609-AA03-10AD27077DD2}']
    {class} function init(string_: JString): JStatus_TextPart; cdecl;
  end;

  [JavaSignature('androidx/wear/ongoing/Status$TextPart')]
  JStatus_TextPart = interface(JStatus_Part)
    ['{98B593AB-582D-4824-862D-5B55F763F40D}']
    function equals(object_: JObject): Boolean; cdecl;
    function getNextChangeTimeMillis(l: Int64): Int64; cdecl;
    function getText(context: JContext; l: Int64): JCharSequence; cdecl;
    function hashCode: Integer; cdecl;
  end;
  TJStatus_TextPart = class(TJavaGenericImport<JStatus_TextPartClass, JStatus_TextPart>) end;

  JStatus_TimerPartClass = interface(JStatus_TimerOrStopwatchPartClass)
    ['{4E9A3729-8AB4-4884-8B15-E5437DF09206}']
    {class} function init(l: Int64; l1: Int64; l2: Int64): JStatus_TimerPart; cdecl; overload;
    {class} function init(l: Int64; l1: Int64): JStatus_TimerPart; cdecl; overload;
    {class} function init(l: Int64): JStatus_TimerPart; cdecl; overload;
  end;

  [JavaSignature('androidx/wear/ongoing/Status$TimerPart')]
  JStatus_TimerPart = interface(JStatus_TimerOrStopwatchPart)
    ['{CA54D479-0B8A-464E-8252-03A78DB4A057}']
  end;
  TJStatus_TimerPart = class(TJavaGenericImport<JStatus_TimerPartClass, JStatus_TimerPart>) end;

  JTimeDependentTextClass = interface(IJavaClass)
    ['{02343463-15E8-4D65-BB51-FFAEB9C63578}']
  end;

  [JavaSignature('androidx/wear/ongoing/TimeDependentText')]
  JTimeDependentText = interface(IJavaInstance)
    ['{D40F5FBE-3BAA-41B2-BF58-997E5ACCE9A0}']
    function getNextChangeTimeMillis(l: Int64): Int64; cdecl;
    function getText(context: JContext; l: Int64): JCharSequence; cdecl;
  end;
  TJTimeDependentText = class(TJavaGenericImport<JTimeDependentTextClass, JTimeDependentText>) end;

implementation

end.
