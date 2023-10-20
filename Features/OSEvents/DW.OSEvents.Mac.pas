unit DW.OSEvents.Mac;

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

  TPlatformKeyPressOSEvents = class(TPlatformOSEvents)
  private
    FPressedKeys: TKeyCodes;
  protected
    function EventTapHandler(const proxy: CGEventTapProxy; const &type: CGEventType; const event: CGEventRef): CGEventRef; override;
  public
    constructor Create;
    function HasMultipleDown: Boolean;
    function DownCount: Integer;
  end;

implementation

uses
  DW.OSLog,
  // RTL
  System.Classes,
  // macOS
  Macapi.ObjectiveC, Macapi.CoreGraphics, Macapi.Foundation, Macapi.Helpers;

const
  libApplicationServices = '/System/Library/Frameworks/ApplicationServices.framework/ApplicationServices';

type
  TKeyCodesHelper = record helper for TKeyCodes
    procedure Add(const ACode: Word);
    function Count: Integer;
    function IndexOf(const ACode: Word): Integer;
    procedure Remove(const ACode: Word);
  end;

  // Declaring this type and function here as CGEventTapCallBack is declared incorrectly in Macapi.CocoaTypes
  CGEventTapCallBack = function(proxy: CGEventTapProxy; &type: CGEventType; event: CGEventRef; userInfo: Pointer): CGEventRef; cdecl;

function CGEventTapCreate(tap: CGEventTapLocation; place: CGEventTapPlacement; options: CGEventTapOptions; eventsOfInterest: CGEventMask;
  callback: CGEventTapCallBack; userInfo: Pointer): CFMachPortRef; cdecl;
  external libCoreGraphics name _PU + 'CGEventTapCreate';

function AXIsProcessTrustedWithOptions(options: CFDictionaryRef): Boolean; cdecl;
  external libApplicationServices name _PU + 'AXIsProcessTrustedWithOptions';

function CGEventMaskBit(const eventType: CGEventType): CGEventMask;
begin
  Result := 1 shl eventType;
end;

function kAXTrustedCheckOptionPrompt: NSString;
begin
  Result := CocoaNSStringConst(libApplicationServices, 'kAXTrustedCheckOptionPrompt');
end;

{ TKeyCodesHelper }

procedure TKeyCodesHelper.Add(const ACode: Word);
begin
  if IndexOf(ACode) = -1 then
    Self := Self + [ACode];
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

procedure TKeyCodesHelper.Remove(const ACode: Word);
var
  LIndex: Integer;
begin
  LIndex := IndexOf(ACode);
  if LIndex > -1 then
    Delete(Self, LIndex, 1);
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
  Result := False;
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

function TPlatformKeyPressOSEvents.EventTapHandler(const proxy: CGEventTapProxy; const &type: CGEventType; const event: CGEventRef): CGEventRef;
var
  LKeyCode: CGKeyCode;
  LHandled: Boolean;
begin
  Result := event;
  LHandled := False;
  if (&type = kCGEventKeyDown) or (&type = kCGEventKeyUp) then
  begin
    LKeyCode := CGEventGetIntegerValueField(event, kCGKeyboardEventKeycode);
    if &type = kCGEventKeyDown then
      FPressedKeys.Add(LKeyCode)
    else if &type = kCGEventKeyUp then
      FPressedKeys.Remove(LKeyCode);
    LHandled := HandleEventTap;
  end;
  if LHandled then
    Result := nil;
end;

function TPlatformKeyPressOSEvents.HasMultipleDown: Boolean;
begin
  Result := DownCount > 1;
end;

function TPlatformKeyPressOSEvents.DownCount: Integer;
begin
  Result := FPressedKeys.Count;
end;

end.
