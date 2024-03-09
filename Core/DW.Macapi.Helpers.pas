unit DW.Macapi.Helpers;

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
  // RTL
  System.Classes, System.SysUtils,
  // macOS
  {$IF Defined(MACOS)}
  Macapi.CoreFoundation,
  {$ENDIF}
  {$IF Defined(MACDEV)}
  Macapi.Foundation, Macapi.AppKit;
  {$ENDIF}
  // iOS
  {$IF Defined(IOS)}
  iOSapi.Foundation;
  {$ENDIF}

type
  TNSDictionaryHelper = record
  private
    FDictionary: NSDictionary;
    function GetValuePtr(const AKey: string): Pointer;
  public
    constructor Create(const ADictionary: NSDictionary);
    function GetValue(const AKey: string; const ADefault: Double = 0): Double; overload;
    function GetValue(const AKey: string; const ADefault: Integer = 0): Integer; overload;
    function GetValue(const AKey: string; const ADefault: string = ''): string; overload;
  end;

  TNSMutableDictionaryHelper = record
  private
    FDictionary: NSMutableDictionary;
  public
    constructor Create(const ADictionary: NSMutableDictionary);
    function Dictionary: NSDictionary;
    procedure SetValue(const AValue: Integer; const AKey: string); overload;
    procedure SetValue(const AValue, AKey: string); overload;
  end;

  TNSArrayHelper = record
  public
    class function FromNSObjects(const AArray: array of NSObject): NSArray; static;
  end;

  TNSDataHelper = record
  public
    class function ToBytes(const AData: NSData): TBytes; static;
  end;

  TMacHelperEx = record
  public
    // class function GetLocationManagerAuthorization: TAuthorizationType; static;
    // class function NSDictionaryToJSON(const ADictionary: NSDictionary): string; static;
    class function GetBundleValue(const AKey: string): string; static;
    class function GetBundleValueNS(const AKey: string): NSString; static;
    class function MainBundle: NSBundle; static;
    {$IF Defined(MACDEV)}
    class function SharedApplication: NSApplication; static;
    {$ENDIF}
    class function StandardUserDefaults: NSUserDefaults; static;
  end;

/// <summary>
///   Retrieves cocoa double constant
/// </summary>
function CocoaDoubleConst(const AFwk: string; const AConstStr: string): Double;
/// <summary>
///   Retrieves a number value from an NSDictionary, with optional default (otherwise zero)
/// </summary>
function GetDictionaryNumberValue(const ADictionary: NSDictionary; const AKey: NSString; const ADefault: Double = 0): Double;
/// <summary>
///   Retrieves a string value from an NSDictionary, with optional default (otherwise blank)
/// </summary>
function GetDictionaryStringValue(const ADictionary: NSDictionary; const AKey: NSString; const ADefault: string = ''): string;
/// <summary>
///   Converts GMT to local time
/// </summary>
function GetLocalDateTime(const ADateTime: TDateTime): TDateTime;
/// <summary>
///   Puts string values from an NSArray into an string array
/// </summary>
function NSArrayToStringArray(const AArray: NSArray): TArray<string>;
/// <summary>
///   Puts string values from an array into an NSArray
/// </summary>
function StringArrayToNSArray(const AArray: array of string; const ADequote: Boolean = False): NSArray;
/// <summary>
///   Puts string values from a TStrings into an NSArray
/// </summary>
function StringsToNSArray(const AStrings: TStrings; const ADequote: Boolean = False): NSArray;
/// <summary>
///   Converts a string directly into an NSString reference (ID)
/// </summary>
function StrToObjectID(const AStr: string): Pointer; deprecated 'Use StringToID from Macapi.Helpers';
/// <summary>
///   Converts a string into an CFStringRef
/// </summary>
function StrToCFStringRef(const AStr: string): CFStringRef;
function NSErrorToStr(const AError: NSError): string;

implementation

uses
  // RTL
  System.DateUtils,
  // macOS
  Macapi.ObjectiveC, Macapi.Helpers;

{ TNSDictionaryHelper }

constructor TNSDictionaryHelper.Create(const ADictionary: NSDictionary);
begin
  FDictionary := ADictionary;
end;

function TNSDictionaryHelper.GetValuePtr(const AKey: string): Pointer;
begin
  Result := FDictionary.valueForKey(StrToNSStr(AKey));
end;

function TNSDictionaryHelper.GetValue(const AKey: string; const ADefault: Double = 0): Double;
var
  LValuePtr: Pointer;
begin
  Result := ADefault;
  LValuePtr := GetValuePtr(AKey);
  if LValuePtr <> nil then
    Result := TNSNumber.Wrap(LValuePtr).doubleValue;
end;

function TNSDictionaryHelper.GetValue(const AKey: string; const ADefault: Integer = 0): Integer;
var
  LValuePtr: Pointer;
begin
  Result := ADefault;
  LValuePtr := GetValuePtr(AKey);
  if LValuePtr <> nil then
    Result := TNSNumber.Wrap(LValuePtr).integerValue;
end;

function TNSDictionaryHelper.GetValue(const AKey: string; const ADefault: string = ''): string;
var
  LValuePtr: Pointer;
begin
  Result := ADefault;
  LValuePtr := GetValuePtr(AKey);
  if LValuePtr <> nil then
    Result := NSStrToStr(TNSString.Wrap(LValuePtr));
end;

{ TNSMutableDictionaryHelper }

constructor TNSMutableDictionaryHelper.Create(const ADictionary: NSMutableDictionary);
begin
  FDictionary := ADictionary;
end;

function TNSMutableDictionaryHelper.Dictionary: NSDictionary;
begin
  Result := TNSDictionary.Wrap(NSObjectToID(FDictionary));
end;

procedure TNSMutableDictionaryHelper.SetValue(const AValue: Integer; const AKey: string);
begin
  FDictionary.setObject(TNSNumber.OCClass.numberWithInt(AValue), NSObjectToID(StrToNSStr(AKey)));
end;

procedure TNSMutableDictionaryHelper.SetValue(const AValue, AKey: string);
begin
  FDictionary.setObject(NSObjectToID(StrToNSStr(AValue)), NSObjectToID(StrToNSStr(AKey)));
end;

function GetDictionaryNumberValue(const ADictionary: NSDictionary; const AKey: NSString; const ADefault: Double = 0): Double;
var
  LValuePtr: Pointer;
begin
  Result := ADefault;
  LValuePtr := ADictionary.valueForKey(AKey);
  if LValuePtr <> nil then
    Result := TNSNumber.Wrap(LValuePtr).doubleValue;
end;

function GetDictionaryStringValue(const ADictionary: NSDictionary; const AKey: NSString; const ADefault: string = ''): string;
var
  LValuePtr: Pointer;
begin
  Result := ADefault;
  LValuePtr := ADictionary.valueForKey(AKey);
  if LValuePtr <> nil then
    Result := NSStrToStr(TNSString.Wrap(LValuePtr));
end;

function CocoaDoubleConst(const AFwk: string; const AConstStr: string): Double;
var
  LObj: Pointer;
begin
  LObj := CocoaPointerConst(AFwk, AConstStr);
  if LObj <> nil then
    Result := Double(LObj^)
  else
    Result := 0;
end;

function NSArrayToStringArray(const AArray: NSArray): TArray<string>;
var
  I: Integer;
begin
  for I := 0 to AArray.count - 1 do
    Result := Result + [NSStrToStr(TNSString.Wrap(AArray.objectAtIndex(I)))];
end;

function StringsToNSArray(const AStrings: TStrings; const ADequote: Boolean = False): NSArray;
var
  LArray: array of Pointer;
  I: Integer;
  LString: string;
begin
  SetLength(LArray, AStrings.Count);
  for I := 0 to AStrings.Count - 1 do
  begin
    LString := AStrings[I];
    if ADequote then
      LString := AnsiDequotedStr(LString, '"');
    LArray[I] := NSObjectToID(StrToNSStr(LString));
  end;
  Result := TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(@LArray[0], Length(LArray)));
end;

function StringArrayToNSArray(const AArray: array of string; const ADequote: Boolean = False): NSArray;
var
  LArray: array of Pointer;
  I: Integer;
  LString: string;
begin
  SetLength(LArray, Length(AArray));
  for I := 0 to Length(AArray) - 1 do
  begin
    LString := AArray[I];
    if ADequote then
      LString := AnsiDequotedStr(LString, '"');
    LArray[I] := NSObjectToID(StrToNSStr(LString));
  end;
  Result := TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(@LArray[0], Length(LArray)));
end;

function StrToObjectID(const AStr: string): Pointer;
begin
  Result := NSObjectToID(StrToNSStr(AStr));
end;

function StrToCFStringRef(const AStr: string): CFStringRef;
begin
  Result := CFStringCreateWithCharacters(kCFAllocatorDefault, PChar(AStr), Length(AStr));
end;

function NSErrorToStr(const AError: NSError): string;
begin
  Result := Format('%s (%d): %s (%s)', [NSStrToStr(AError.domain), AError.code, NSStrToStr(AError.localizedDescription),
    NSStrToStr(AError.localizedFailureReason)]);
end;

function GetLocalDateTime(const ADateTime: TDateTime): TDateTime;
begin
  Result := IncSecond(ADateTime, TNSTimeZone.Wrap(TNSTimeZone.OCClass.localTimeZone).secondsFromGMT);
end;

{ TNSArrayHelper }

class function TNSArrayHelper.FromNSObjects(const AArray: array of NSObject): NSArray;
var
  I: Integer;
  LArray: NSMutableArray;
begin
  LArray := TNSMutableArray.Create;
  for I := 0 to Length(AArray) - 1 do
    LArray.addObject(NSObjectToID(AArray[I]));
  Result := LArray;
end;

{ TMacHelperEx }

class function TMacHelperEx.GetBundleValue(const AKey: string): string;
var
  LValue: NSString;
begin
  Result := '';
  LValue := GetBundleValueNS(AKey);
  if LValue <> nil then
    Result := NSStrToStr(LValue);
end;

class function TMacHelperEx.GetBundleValueNS(const AKey: string): NSString;
var
  LValueObject: Pointer;
begin
  Result := nil;
  LValueObject := MainBundle.infoDictionary.objectForKey(StringToID(AKey));
  if LValueObject <> nil then
    Result := TNSString.Wrap(LValueObject);
end;

class function TMacHelperEx.MainBundle: NSBundle;
begin
  Result := TNSBundle.Wrap(TNSBundle.OCClass.mainBundle);
end;

{$IF Defined(MACDEV)}
class function TMacHelperEx.SharedApplication: NSApplication;
begin
  Result := TNSApplication.Wrap(TNSApplication.OCClass.sharedApplication);
end;
{$ENDIF}

class function TMacHelperEx.StandardUserDefaults: NSUserDefaults;
begin
  Result := TNSUserDefaults.Wrap(TNSUserDefaults.OCClass.standardUserDefaults);
end;

{ TNSDataHelper }

class function TNSDataHelper.ToBytes(const AData: NSData): TBytes;
begin
  SetLength(Result, AData.length);
  Move(AData.bytes^, Result[0], AData.length);
end;

end.

