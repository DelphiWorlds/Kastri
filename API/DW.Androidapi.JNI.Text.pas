unit DW.Androidapi.JNI.Text;

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
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText,
  // DW
  DW.Androidapi.JNI.Util;

type
  JDateFormat = interface;
  JDateUtils = interface;
  Jformat_Formatter = interface;
  JHtml = interface;
  JHtml_ImageGetter = interface;
  JHtml_TagHandler = interface;

  Jformat_FormatterClass = interface(JObjectClass)
    ['{CE2E86ED-14E2-4E45-AA19-2EE1220871E4}']
    {class} function formatFileSize(context: JContext; sizeBytes: Int64): JString; cdecl;
    {class} function formatIpAddress(ipv4Address: Integer): JString; cdecl;
    {class} function formatShortFileSize(context: JContext; sizeBytes: Int64): JString; cdecl;
  end;

  [JavaSignature('android/text/format/Formatter')]
  Jformat_Formatter = interface(JObject)
    ['{BA8A83F2-33D2-4261-B26D-F65DB01CDE11}']
  end;
  TJformat_Formatter = class(TJavaGenericImport<Jformat_FormatterClass, Jformat_Formatter>)
  end;

  JHtmlClass = interface(JObjectClass)
    ['{E7013A47-AB8B-44A5-AE40-F44391E22EF8}']
    function _GetFROM_HTML_MODE_COMPACT: Integer; cdecl;
    function _GetFROM_HTML_MODE_LEGACY: Integer; cdecl;
    function _GetFROM_HTML_OPTION_USE_CSS_COLORS: Integer; cdecl;
    function _GetFROM_HTML_SEPARATOR_LINE_BREAK_BLOCKQUOTE: Integer; cdecl;
    function _GetFROM_HTML_SEPARATOR_LINE_BREAK_DIV: Integer; cdecl;
    function _GetFROM_HTML_SEPARATOR_LINE_BREAK_HEADING: Integer; cdecl;
    function _GetFROM_HTML_SEPARATOR_LINE_BREAK_LIST: Integer; cdecl;
    function _GetFROM_HTML_SEPARATOR_LINE_BREAK_LIST_ITEM: Integer; cdecl;
    function _GetFROM_HTML_SEPARATOR_LINE_BREAK_PARAGRAPH: Integer; cdecl;
    function _GetTO_HTML_PARAGRAPH_LINES_CONSECUTIVE: Integer; cdecl;
    function _GetTO_HTML_PARAGRAPH_LINES_INDIVIDUAL: Integer; cdecl;
    function escapeHtml(text: JCharSequence): JString; cdecl;
    function fromHtml(source: JString): JSpanned; cdecl; overload;
    function fromHtml(source: JString; flags: Integer): JSpanned; cdecl; overload;
    function fromHtml(source: JString; flags: Integer; imageGetter: JHtml_ImageGetter; tagHandler: JHtml_TagHandler): JSpanned; cdecl; overload;
    function fromHtml(source: JString; imageGetter: JHtml_ImageGetter; tagHandler: JHtml_TagHandler): JSpanned; cdecl; overload;
    function toHtml(text: JSpanned): JString; cdecl; overload;
    function toHtml(text: JSpanned; option: Integer): JString; cdecl; overload;
    property FROM_HTML_MODE_COMPACT: Integer read _GetFROM_HTML_MODE_COMPACT;
    property FROM_HTML_MODE_LEGACY: Integer read _GetFROM_HTML_MODE_LEGACY;
    property FROM_HTML_OPTION_USE_CSS_COLORS: Integer read _GetFROM_HTML_OPTION_USE_CSS_COLORS;
    property FROM_HTML_SEPARATOR_LINE_BREAK_BLOCKQUOTE: Integer read _GetFROM_HTML_SEPARATOR_LINE_BREAK_BLOCKQUOTE;
    property FROM_HTML_SEPARATOR_LINE_BREAK_DIV: Integer read _GetFROM_HTML_SEPARATOR_LINE_BREAK_DIV;
    property FROM_HTML_SEPARATOR_LINE_BREAK_HEADING: Integer read _GetFROM_HTML_SEPARATOR_LINE_BREAK_HEADING;
    property FROM_HTML_SEPARATOR_LINE_BREAK_LIST: Integer read _GetFROM_HTML_SEPARATOR_LINE_BREAK_LIST;
    property FROM_HTML_SEPARATOR_LINE_BREAK_LIST_ITEM: Integer read _GetFROM_HTML_SEPARATOR_LINE_BREAK_LIST_ITEM;
    property FROM_HTML_SEPARATOR_LINE_BREAK_PARAGRAPH: Integer read _GetFROM_HTML_SEPARATOR_LINE_BREAK_PARAGRAPH;
    property TO_HTML_PARAGRAPH_LINES_CONSECUTIVE: Integer read _GetTO_HTML_PARAGRAPH_LINES_CONSECUTIVE;
    property TO_HTML_PARAGRAPH_LINES_INDIVIDUAL: Integer read _GetTO_HTML_PARAGRAPH_LINES_INDIVIDUAL;
  end;

  [JavaSignature('android/text/Html')]
  JHtml = interface(JObject)
    ['{937D7A71-86A7-478B-A43E-299BDD1333E9}']
  end;
  TJHtml = class(TJavaGenericImport<JHtmlClass, JHtml>)
  end;

  JHtml_ImageGetterClass = interface(JObjectClass)
    ['{02BF8F98-1B7C-4BA6-80C0-6BC4A3EDAD9C}']
  end;

  [JavaSignature('android/text/Html$ImageGetter')]
  JHtml_ImageGetter = interface(JObject)
    ['{9C6C6F1F-808A-4642-9991-63017C382C14}']
    function getDrawable(source: JString): JDrawable; cdecl;
  end;
  TJHtml_ImageGetter = class(TJavaGenericImport<JHtml_ImageGetterClass, JHtml_ImageGetter>)
  end;

  // Placeholder declarations
  JHtml_TagHandlerClass = interface(JObjectClass)
    ['{79B76907-D8D1-44D7-A9AF-E66A8A49FEDE}']
  end;

  [JavaSignature('android/text/Html$TagHandler')]
  JHtml_TagHandler = interface(JObject)
    ['{C972415C-A1B7-419C-9C98-71A4495FA361}']
  end;
  TJHtml_TagHandler = class(TJavaGenericImport<JHtml_TagHandlerClass, JHtml_TagHandler>)
  end;

  JDateFormatClass = interface(JObjectClass)
    ['{E9A75876-EDA1-44CE-B159-46BACF1805F7}']
    {class} function format(inFormat: JCharSequence; inDate: JCalendar): JCharSequence; overload; cdecl;
    {class} function format(inFormat: JCharSequence; inDate: JDate): JCharSequence; overload; cdecl;
    {class} function format(inFormat: JCharSequence; inTimeInMillis: Int64): JCharSequence; overload; cdecl;
    {class} function getBestDateTimePattern(locale: JLocale; skeleton: JString): JString; cdecl;
    {class} function getDateFormat(context: JContext): JDateFormat; cdecl;
    {class} function getDateFormatOrder(context: JContext): TJavaArray<Char>; cdecl;
    {class} function getLongDateFormat(context: JContext): JDateFormat; cdecl;
    {class} function getMediumDateFormat(context: JContext): JDateFormat; cdecl;
    {class} function getTimeFormat(context: JContext): JDateFormat; cdecl;
    {class} function is24HourFormat(context: JContext): Boolean; cdecl;
  end;

  [JavaSignature('android/text/format/DateFormat')]
  JDateFormat = interface(JObject)
    ['{65E305D7-04D6-4C33-8AB0-9FE366F3F24D}']
  end;
  TJDateFormat = class(TJavaGenericImport<JDateFormatClass, JDateFormat>) end;

  JDateUtilsClass = interface(JObjectClass)
    ['{D62FD894-2AE2-44DD-907F-8F8C19890161}']
    {class} function _GetABBREV_MONTH_FORMAT: JString; cdecl;
    {class} function _GetABBREV_WEEKDAY_FORMAT: JString; cdecl;
    {class} function _GetDAY_IN_MILLIS: Int64; cdecl;
    {class} function _GetFORMAT_12HOUR: Integer; cdecl;
    {class} function _GetFORMAT_24HOUR: Integer; cdecl;
    {class} function _GetFORMAT_ABBREV_ALL: Integer; cdecl;
    {class} function _GetFORMAT_ABBREV_MONTH: Integer; cdecl;
    {class} function _GetFORMAT_ABBREV_RELATIVE: Integer; cdecl;
    {class} function _GetFORMAT_ABBREV_TIME: Integer; cdecl;
    {class} function _GetFORMAT_ABBREV_WEEKDAY: Integer; cdecl;
    {class} function _GetFORMAT_CAP_AMPM: Integer; cdecl;
    {class} function _GetFORMAT_CAP_MIDNIGHT: Integer; cdecl;
    {class} function _GetFORMAT_CAP_NOON: Integer; cdecl;
    {class} function _GetFORMAT_CAP_NOON_MIDNIGHT: Integer; cdecl;
    {class} function _GetFORMAT_NO_MIDNIGHT: Integer; cdecl;
    {class} function _GetFORMAT_NO_MONTH_DAY: Integer; cdecl;
    {class} function _GetFORMAT_NO_NOON: Integer; cdecl;
    {class} function _GetFORMAT_NO_NOON_MIDNIGHT: Integer; cdecl;
    {class} function _GetFORMAT_NO_YEAR: Integer; cdecl;
    {class} function _GetFORMAT_NUMERIC_DATE: Integer; cdecl;
    {class} function _GetFORMAT_SHOW_DATE: Integer; cdecl;
    {class} function _GetFORMAT_SHOW_TIME: Integer; cdecl;
    {class} function _GetFORMAT_SHOW_WEEKDAY: Integer; cdecl;
    {class} function _GetFORMAT_SHOW_YEAR: Integer; cdecl;
    {class} function _GetFORMAT_UTC: Integer; cdecl;
    {class} function _GetHOUR_IN_MILLIS: Int64; cdecl;
    {class} function _GetHOUR_MINUTE_24: JString; cdecl;
    {class} function _GetLENGTH_LONG: Integer; cdecl;
    {class} function _GetLENGTH_MEDIUM: Integer; cdecl;
    {class} function _GetLENGTH_SHORT: Integer; cdecl;
    {class} function _GetLENGTH_SHORTER: Integer; cdecl;
    {class} function _GetLENGTH_SHORTEST: Integer; cdecl;
    {class} function _GetMINUTE_IN_MILLIS: Int64; cdecl;
    {class} function _GetMONTH_DAY_FORMAT: JString; cdecl;
    {class} function _GetMONTH_FORMAT: JString; cdecl;
    {class} function _GetNUMERIC_MONTH_FORMAT: JString; cdecl;
    {class} function _GetSECOND_IN_MILLIS: Int64; cdecl;
    {class} function _GetWEEKDAY_FORMAT: JString; cdecl;
    {class} function _GetWEEK_IN_MILLIS: Int64; cdecl;
    {class} function _GetYEAR_FORMAT: JString; cdecl;
    {class} function _GetYEAR_FORMAT_TWO_DIGITS: JString; cdecl;
    {class} function _GetYEAR_IN_MILLIS: Int64; cdecl;
    {class} function _GetsameMonthTable: TJavaArray<Integer>; cdecl;
    {class} function _GetsameYearTable: TJavaArray<Integer>; cdecl;
    {class} function formatDateRange(context: JContext; long: Int64; long_1: Int64; int: Integer): JString; overload; cdecl;
    {class} function formatDateRange(context: JContext; formatter: JFormatter; long: Int64; long_1: Int64; int: Integer): JFormatter; overload; cdecl;
    {class} function formatDateRange(context: JContext; formatter: JFormatter; long: Int64; long_1: Int64; int: Integer;
      string_1: JString): JFormatter; overload; cdecl;
    {class} function formatDateTime(context: JContext; long: Int64; int: Integer): JString; cdecl;
    {class} function formatElapsedTime(long: Int64): JString; overload; cdecl;
    {class} function formatElapsedTime(stringBuilder: JStringBuilder; long: Int64): JString; overload; cdecl;
    {class} function formatSameDayTime(long: Int64; long_1: Int64; int: Integer; int_1: Integer): JCharSequence; cdecl;
    {class} function getAMPMString(int: Integer): JString; cdecl;
    {class} function getDayOfWeekString(int: Integer; int_1: Integer): JString; cdecl;
    {class} function getMonthString(int: Integer; int_1: Integer): JString; cdecl;
    {class} function getRelativeDateTimeString(context: JContext; long: Int64; long_1: Int64; long_2: Int64; int: Integer): JCharSequence; cdecl;
    {class} function getRelativeTimeSpanString(context: JContext; long: Int64; boolean: Boolean): JCharSequence; overload; cdecl;
    {class} function getRelativeTimeSpanString(long: Int64; long_1: Int64; long_2: Int64): JCharSequence; overload; cdecl;
    {class} function getRelativeTimeSpanString(context: JContext; long: Int64): JCharSequence; overload; cdecl;
    {class} function getRelativeTimeSpanString(long: Int64; long_1: Int64; long_2: Int64; int: Integer): JCharSequence; overload; cdecl;
    {class} function getRelativeTimeSpanString(long: Int64): JCharSequence; overload; cdecl;
    {class} function isToday(long: Int64): Boolean; cdecl;
    {class} property ABBREV_MONTH_FORMAT: JString read _GetABBREV_MONTH_FORMAT;
    {class} property ABBREV_WEEKDAY_FORMAT: JString read _GetABBREV_WEEKDAY_FORMAT;
    {class} property DAY_IN_MILLIS: Int64 read _GetDAY_IN_MILLIS;
    {class} property FORMAT_12HOUR: Integer read _GetFORMAT_12HOUR;
    {class} property FORMAT_24HOUR: Integer read _GetFORMAT_24HOUR;
    {class} property FORMAT_ABBREV_ALL: Integer read _GetFORMAT_ABBREV_ALL;
    {class} property FORMAT_ABBREV_MONTH: Integer read _GetFORMAT_ABBREV_MONTH;
    {class} property FORMAT_ABBREV_RELATIVE: Integer read _GetFORMAT_ABBREV_RELATIVE;
    {class} property FORMAT_ABBREV_TIME: Integer read _GetFORMAT_ABBREV_TIME;
    {class} property FORMAT_ABBREV_WEEKDAY: Integer read _GetFORMAT_ABBREV_WEEKDAY;
    {class} property FORMAT_CAP_AMPM: Integer read _GetFORMAT_CAP_AMPM;
    {class} property FORMAT_CAP_MIDNIGHT: Integer read _GetFORMAT_CAP_MIDNIGHT;
    {class} property FORMAT_CAP_NOON: Integer read _GetFORMAT_CAP_NOON;
    {class} property FORMAT_CAP_NOON_MIDNIGHT: Integer read _GetFORMAT_CAP_NOON_MIDNIGHT;
    {class} property FORMAT_NO_MIDNIGHT: Integer read _GetFORMAT_NO_MIDNIGHT;
    {class} property FORMAT_NO_MONTH_DAY: Integer read _GetFORMAT_NO_MONTH_DAY;
    {class} property FORMAT_NO_NOON: Integer read _GetFORMAT_NO_NOON;
    {class} property FORMAT_NO_NOON_MIDNIGHT: Integer read _GetFORMAT_NO_NOON_MIDNIGHT;
    {class} property FORMAT_NO_YEAR: Integer read _GetFORMAT_NO_YEAR;
    {class} property FORMAT_NUMERIC_DATE: Integer read _GetFORMAT_NUMERIC_DATE;
    {class} property FORMAT_SHOW_DATE: Integer read _GetFORMAT_SHOW_DATE;
    {class} property FORMAT_SHOW_TIME: Integer read _GetFORMAT_SHOW_TIME;
    {class} property FORMAT_SHOW_WEEKDAY: Integer read _GetFORMAT_SHOW_WEEKDAY;
    {class} property FORMAT_SHOW_YEAR: Integer read _GetFORMAT_SHOW_YEAR;
    {class} property FORMAT_UTC: Integer read _GetFORMAT_UTC;
    {class} property HOUR_IN_MILLIS: Int64 read _GetHOUR_IN_MILLIS;
    {class} property HOUR_MINUTE_24: JString read _GetHOUR_MINUTE_24;
    {class} property LENGTH_LONG: Integer read _GetLENGTH_LONG;
    {class} property LENGTH_MEDIUM: Integer read _GetLENGTH_MEDIUM;
    {class} property LENGTH_SHORT: Integer read _GetLENGTH_SHORT;
    {class} property LENGTH_SHORTER: Integer read _GetLENGTH_SHORTER;
    {class} property LENGTH_SHORTEST: Integer read _GetLENGTH_SHORTEST;
    {class} property MINUTE_IN_MILLIS: Int64 read _GetMINUTE_IN_MILLIS;
    {class} property MONTH_DAY_FORMAT: JString read _GetMONTH_DAY_FORMAT;
    {class} property MONTH_FORMAT: JString read _GetMONTH_FORMAT;
    {class} property NUMERIC_MONTH_FORMAT: JString read _GetNUMERIC_MONTH_FORMAT;
    {class} property SECOND_IN_MILLIS: Int64 read _GetSECOND_IN_MILLIS;
    {class} property WEEKDAY_FORMAT: JString read _GetWEEKDAY_FORMAT;
    {class} property WEEK_IN_MILLIS: Int64 read _GetWEEK_IN_MILLIS;
    {class} property YEAR_FORMAT: JString read _GetYEAR_FORMAT;
    {class} property YEAR_FORMAT_TWO_DIGITS: JString read _GetYEAR_FORMAT_TWO_DIGITS;
    {class} property YEAR_IN_MILLIS: Int64 read _GetYEAR_IN_MILLIS;
    {class} property sameMonthTable: TJavaArray<Integer> read _GetsameMonthTable;
    {class} property sameYearTable: TJavaArray<Integer> read _GetsameYearTable;
  end;

  [JavaSignature('android/text/format/DateUtils')]
  JDateUtils = interface(JObject)
    ['{65A26ED2-54C8-4065-BE43-95D4A4C6E707}']
  end;
  TJDateUtils = class(TJavaGenericImport<JDateUtilsClass, JDateUtils>) end;

implementation

end.
