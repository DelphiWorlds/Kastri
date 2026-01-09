unit DW.OSEvents.Mac;

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

{$SCOPEDENUMS ON}

uses
  // RTL
  System.Classes,
  // macOS
  Macapi.CocoaTypes, Macapi.CoreFoundation;

type
  TOSEventKind = (KeyDown, KeyUp);

  TOSEventKinds = set of TOSEventKind;

  TEventTapEvent = procedure(Sender: TObject; var Handled: Boolean) of object;

  TPlatformOSEvents = class(TObject)
  private
    FEventKinds: TOSEventKinds;
    FEventMasks: array[TOSEventKind] of CGEventMask;
    FEventTap: CFMachPortRef;
    FRunLoop: CFRunLoopRef;
    FRunLoopSource: CFRunLoopSourceRef;
    FOnEventTap: TEventTapEvent;
    procedure EventTapRunLoopStart;
    procedure EventTapRunLoopStopped;
    function GetEventKindsMask(const AEventKinds: TOSEventKinds): CGEventMask;
    function HandleEventTap: Boolean;
  protected
    class function EventTapCallback(proxy: CGEventTapProxy; &type: CGEventType; event: CGEventRef; userInfo: Pointer): CGEventRef; cdecl; static;
  protected
    function EventTapHandler(const proxy: CGEventTapProxy; const &type: CGEventType; const event: CGEventRef): CGEventRef; virtual;
  public
    class function IsProcessTrusted: Boolean;
  public
    constructor Create(const AEventKinds: TOSEventKinds);
    function Start: Boolean;
    procedure Stop;
    property OnEventTap: TEventTapEvent read FOnEventTap write FOnEventTap;
  end;

  TKeyCodes = TArray<Word>;

  TOSKeyEvent = procedure(Sender: TObject; const Key: Word; const KeyChar: Char; const Shift: TShiftState) of object;

  TPlatformKeyPressOSEvents = class(TPlatformOSEvents)
  private
    FDownKeys: TKeyCodes;
    FIgnoreKeyEvents: Boolean;
    FOnKeyDown: TOSKeyEvent;
    FOnKeyUp: TOSKeyEvent;
    procedure SendKey(const AValue: Char; const AKeyDown: Boolean);
  protected
    function EventTapHandler(const proxy: CGEventTapProxy; const &type: CGEventType; const event: CGEventRef): CGEventRef; override;
    procedure KeyDown(const AKey: Word; const AKeyChar: Char; const AShift: TShiftState); virtual;
    procedure KeyUp(const AKey: Word; const AKeyChar: Char; const AShift: TShiftState); virtual;
  public
    constructor Create;
    function HasMultipleKeysDown: Boolean;
    function DownKeysCount: Integer;
    procedure ClearDownKeys;
    procedure SendKeys(const AValue: string);
    property DownKeys: TKeyCodes read FDownKeys;
    property OnKeyDown: TOSKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyUp: TOSKeyEvent read FOnKeyUp write FOnKeyUp;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // macOS
  Macapi.ObjectiveC, Macapi.CoreGraphics, Macapi.Foundation, Macapi.Helpers, Macapi.CoreServices,
  // FMX
  FMX.Platform, FMX.KeyMapping, FMX.Types;

const
  libApplicationServices = '/System/Library/Frameworks/ApplicationServices.framework/ApplicationServices';

type
  TKeyCodesHelper = record helper for TKeyCodes
    procedure Add(const ACode: Word);
    procedure Clear;
    function Count: Integer;
    function IndexOf(const ACode: Word): Integer;
    function Remove(const ACode: Word): Boolean;
  end;

  // Declaring this type and function here as CGEventTapCallBack is declared incorrectly in Macapi.CocoaTypes
  CGEventTapCallBack = function(proxy: CGEventTapProxy; &type: CGEventType; event: CGEventRef; userInfo: Pointer): CGEventRef; cdecl;

function CGEventTapCreate(tap: CGEventTapLocation; place: CGEventTapPlacement; options: CGEventTapOptions; eventsOfInterest: CGEventMask;
  callback: CGEventTapCallBack; userInfo: Pointer): CFMachPortRef; cdecl;
  external libCoreGraphics name _PU + 'CGEventTapCreate';

function AXIsProcessTrustedWithOptions(options: CFDictionaryRef): Boolean; cdecl;
  external libApplicationServices name _PU + 'AXIsProcessTrustedWithOptions';

function kAXTrustedCheckOptionPrompt: NSString;
begin
  Result := CocoaNSStringConst(libApplicationServices, 'kAXTrustedCheckOptionPrompt');
end;

function CGEventMaskBit(const eventType: CGEventType): CGEventMask;
begin
  Result := 1 shl eventType;
end;

function KeyCodeToChar(const AKeyCode: CGKeyCode): Char;
var
  LKeyMappingService: IFMXKeyMappingService;
  LKind: TKeyKind;
begin
  Result := #0;
  if TPlatformServices.Current.SupportsPlatformService(IFMXKeyMappingService, LKeyMappingService) then
    Result := Char(LKeyMappingService.PlatformKeyToVirtualKey(AKeyCode, LKind));
end;

{ TKeyCodesHelper }

procedure TKeyCodesHelper.Add(const ACode: Word);
begin
  if IndexOf(ACode) = -1 then
    Self := Self + [ACode];
end;

procedure TKeyCodesHelper.Clear;
begin
  Self := [];
end;

function TKeyCodesHelper.Count: Integer;
begin
  Result := Length(Self);
end;

function TKeyCodesHelper.IndexOf(const ACode: Word): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
  begin
    if Self[I] = ACode then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TKeyCodesHelper.Remove(const ACode: Word): Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  LIndex := IndexOf(ACode);
  if LIndex > -1 then
  begin
    Delete(Self, LIndex, 1);
    Result := True;
  end;
end;

{ TPlatformEvents }

constructor TPlatformOSEvents.Create(const AEventKinds: TOSEventKinds);
begin
  inherited Create;
  FEventKinds := AEventKinds;
  FEventMasks[TOSEventKind.KeyDown] := kCGEventKeyDown;
  FEventMasks[TOSEventKind.KeyUp] := kCGEventKeyUp;
end;

class function TPlatformOSEvents.EventTapCallback(proxy: CGEventTapProxy; &type: CGEventType; event: CGEventRef; userInfo: Pointer): CGEventRef;
begin
  Result := TPlatformOSEvents(userInfo).EventTapHandler(proxy, &type, event);
end;

function TPlatformOSEvents.EventTapHandler(const proxy: CGEventTapProxy; const &type: CGEventType; const event: CGEventRef): CGEventRef;
begin
  Result := event;
end;

function TPlatformOSEvents.GetEventKindsMask(const AEventKinds: TOSEventKinds): CGEventMask;
var
  LEventKind: TOSEventKind;
begin
  Result := 0;
  for LEventKind := Low(TOSEventKind) to High(TOSEventKind) do
  begin
    if LEventKind in AEventKinds then
      Result := Result or CGEventMaskBit(FEventMasks[LEventKind]);
  end;
end;

function TPlatformOSEvents.HandleEventTap: Boolean;
begin
  Result := False;
  if Assigned(FOnEventTap) then
    FOnEventTap(Self, Result);
end;

class function TPlatformOSEvents.IsProcessTrusted: Boolean;
var
  LOptions: NSMutableDictionary;
begin
  LOptions := TNSMutableDictionary.Create;
  LOptions.setObject(TNSNumber.OCClass.numberWithBool(True), NSStringToID(kAXTrustedCheckOptionPrompt));
  Result := AXIsProcessTrustedWithOptions(NSObjectToID(LOptions));
end;

function TPlatformOSEvents.Start: Boolean;
begin
  FEventTap := CGEventTapCreate(kCGSessionEventTap, kCGHeadInsertEventTap, 0, GetEventKindsMask(FEventKinds), EventTapCallback, Self);
  Result := FEventTap <> nil;
  if Result then
    TThread.CreateAnonymousThread(EventTapRunLoopStart).Start;
end;

procedure TPlatformOSEvents.EventTapRunLoopStart;
begin
  FRunLoop := CFRunLoopGetCurrent;
  CGEventTapEnable(FEventTap, true);
  FRunLoopSource := CFMachPortCreateRunLoopSource(kCFAllocatorDefault, FEventTap, 0);
  CFRunLoopAddSource(FRunLoop, FRunLoopSource, kCFRunLoopCommonModes);
  // The following line of code returns only once CFRunLoopStop has been called on FRunLoop (i.e. the CFRunLoop that was current at the time)
  CFRunLoopRun;
  EventTapRunLoopStopped;
end;

procedure TPlatformOSEvents.EventTapRunLoopStopped;
begin
  CGEventTapEnable(FEventTap, False);
  CFRelease(FEventTap);
  FEventTap := nil;
  CFRunLoopRemoveSource(FRunLoop, FRunLoopSource, kCFRunLoopCommonModes);
  CFRelease(FRunLoopSource);
  FRunLoopSource := nil;
  FRunLoop := nil;
end;

procedure TPlatformOSEvents.Stop;
begin
  CFRunLoopStop(FRunLoop);
end;

{ TPlatformKeyPressOSEvents }

constructor TPlatformKeyPressOSEvents.Create;
begin
  inherited Create([TOSEventKind.KeyDown, TOSEventKind.KeyUp]);
end;

procedure TPlatformKeyPressOSEvents.ClearDownKeys;
begin
  FDownKeys.Clear;
end;

function TPlatformKeyPressOSEvents.EventTapHandler(const proxy: CGEventTapProxy; const &type: CGEventType; const event: CGEventRef): CGEventRef;
var
  LKeyCode: CGKeyCode;
  LHandled: Boolean;
  LFlags: CGEventFlags;
  LShift: TShiftState;
  LKeyChar: Char;
begin
  Result := event;
  if not FIgnoreKeyEvents then
  begin
    LHandled := False;
    LFlags := CGEventGetFlags(event);
    if (LFlags and kCGEventFlagMaskShift) <> 0 then
      Include(LShift, ssShift);
    if (LFlags and kCGEventFlagMaskControl) <> 0 then
      Include(LShift, ssCtrl);
    if (LFlags and kCGEventFlagMaskCommand) <> 0 then
      Include(LShift, ssCommand);
    if (LFlags and kCGEventFlagMaskAlternate) <> 0 then
      Include(LShift, ssAlt);
    if (&type = kCGEventKeyDown) or (&type = kCGEventKeyUp) then
    begin
      LKeyCode := CGEventGetIntegerValueField(event, kCGKeyboardEventKeycode);
      LKeyChar := KeyCodeToChar(LKeyCode);
      if &type = kCGEventKeyDown then
      begin
        FDownKeys.Add(LKeyCode);
        KeyDown(LKeyCode, LKeyChar, LShift);
      end
      else if &type = kCGEventKeyUp then
      begin
        LHandled := not FDownKeys.Remove(LKeyCode);
        KeyUp(LKeyCode, LKeyChar, LShift);
      end;
      LHandled := LHandled or HandleEventTap;
    end;
    if LHandled then
      Result := nil;
  end;
end;

function TPlatformKeyPressOSEvents.HasMultipleKeysDown: Boolean;
begin
  Result := DownKeysCount > 1;
end;

procedure TPlatformKeyPressOSEvents.KeyDown(const AKey: Word; const AKeyChar: Char; const AShift: TShiftState);
begin
  if Assigned(FOnKeyDown) then
    FOnKeyDown(Self, AKey, AKeyChar, AShift);
end;

procedure TPlatformKeyPressOSEvents.KeyUp(const AKey: Word; const AKeyChar: Char; const AShift: TShiftState);
begin
  if Assigned(FOnKeyUp) then
    FOnKeyUp(Self, AKey, AKeyChar, AShift);
end;

procedure TPlatformKeyPressOSEvents.SendKey(const AValue: Char; const AKeyDown: Boolean);
var
  LEvent: CGEventRef;
begin
  LEvent := CGEventCreateKeyboardEvent(nil, 0, AKeyDown);
  if LEvent <> nil then
  try
    CGEventKeyboardSetUnicodeString(LEvent, 1, PChar(AValue));
    CGEventPost(kCGHIDEventTap, LEvent);
  finally
    CFRelease(LEvent);
  end;
end;

procedure TPlatformKeyPressOSEvents.SendKeys(const AValue: string);
var
  LChar: Char;
begin
  FIgnoreKeyEvents := True;
  try
    for LChar in AValue do
    begin
      SendKey(LChar, True);
      SendKey(LChar, False);
    end;
  finally
    FIgnoreKeyEvents := False;
  end;
end;

function TPlatformKeyPressOSEvents.DownKeysCount: Integer;
begin
  Result := FDownKeys.Count;
end;

end.
