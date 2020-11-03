unit DW.MuteCheck.iOS;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{    Copyright 2020 Dave Nottage under MIT license      }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

// ***** NOTE *****
// You will need to ensure that the AudioToolbox framework has been imported into the iOS SDK using the SDK Manager in Delphi

// Based partly on: https://github.com/moshegottlieb/SoundSwitch

interface

uses
  // DW
  DW.MuteCheck;

type
  SystemSoundID = UInt32;
  PSystemSoundID = ^SystemSoundID;

  TPlatformMuteCheck = class(TCustomPlatformMuteCheck)
  private
    class procedure SystemSoundCompletion(ssID: SystemSoundID; clientData: Pointer); cdecl; static;
  private
    FPlayStart: TDateTime;
    FSoundID: SystemSoundID;
    procedure DisposeSound;
    procedure PlaySystemSoundComplete;
  protected
    procedure Check; override;
    procedure SetSoundFileName(const Value: string); override;
  public
    constructor Create(const AMuteCheck: TMuteCheck); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.DateUtils,
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.Helpers,
  // iOS
  iOSapi.Foundation;

const
  kAudioServicesNoError = 0;
  kAudioServicesPropertyIsUISound = 1769174377;

  libAudioToolbox = '/System/Library/Frameworks/AudioToolbox.framework/AudioToolbox';

type
  AudioServicesPropertyID = UInt32;

  AudioServicesSystemSoundCompletionProc = procedure(ssID: SystemSoundID; clientData: Pointer); cdecl;

function AudioServicesAddSystemSoundCompletion(inSystemSoundID: SystemSoundID; inRunLoop: CFRunLoopRef; inRunLoopMode: CFStringRef;
  inCompletionRoutine: AudioServicesSystemSoundCompletionProc; inClientData: Pointer): OSStatus; cdecl;
  external libAudioToolbox name _PU + 'AudioServicesAddSystemSoundCompletion';
procedure AudioServicesRemoveSystemSoundCompletion(inSystemSoundID: SystemSoundID); cdecl;
  external libAudioToolbox name _PU + 'AudioServicesRemoveSystemSoundCompletion';
function AudioServicesCreateSystemSoundID(inFileURL: CFURLRef; outSystemSoundID: PSystemSoundID): OSStatus; cdecl;
  external libAudioToolbox name _PU + 'AudioServicesCreateSystemSoundID';
function AudioServicesDisposeSystemSoundID(inSystemSoundID: SystemSoundID): OSStatus; cdecl;
  external libAudioToolbox name _PU + 'AudioServicesDisposeSystemSoundID';
procedure AudioServicesPlaySystemSound(inSystemSoundID: SystemSoundID); cdecl;
  external libAudioToolbox name _PU + 'AudioServicesPlaySystemSound';
function AudioServicesSetProperty(inPropertyID: AudioServicesPropertyID; inSpecifierSize: UInt32; inSpecifier: Pointer; inPropertyDataSize: UInt32;
  inPropertyData: Pointer): OSStatus; cdecl;
  external libAudioToolbox name _PU + 'AudioServicesSetProperty';

{ TPlatformMuteCheck }

constructor TPlatformMuteCheck.Create(const AMuteCheck: TMuteCheck);
begin
  inherited;
  //
end;

destructor TPlatformMuteCheck.Destroy;
begin
  DisposeSound;
  inherited;
end;

procedure TPlatformMuteCheck.DisposeSound;
begin
  if FSoundID <> 0 then
  begin
    AudioServicesRemoveSystemSoundCompletion(FSoundID);
    AudioServicesDisposeSystemSoundID(FSoundID);
  end;
  FSoundID := 0;
end;

procedure TPlatformMuteCheck.PlaySystemSoundComplete;
begin
  HandleResult(MillisecondsBetween(Now, FPlayStart) < 100);
end;

class procedure TPlatformMuteCheck.SystemSoundCompletion(ssID: SystemSoundID; clientData: Pointer);
begin
  TPlatformMuteCheck(clientData).PlaySystemSoundComplete;
end;

procedure TPlatformMuteCheck.SetSoundFileName(const Value: string);
var
  LYes: UInt32;
  LStatus: OSStatus;
begin
  DisposeSound;
  LStatus := AudioServicesCreateSystemSoundID(TNSURL.OCClass.fileURLWithPath(StrToNSStr(Value)), @FSoundID);
  if LStatus = kAudioServicesNoError then
  begin
    LStatus := AudioServicesAddSystemSoundCompletion(FSoundID, CFRunLoopGetMain, kCFRunLoopDefaultMode, TPlatformMuteCheck.SystemSoundCompletion, Self);
    if LStatus = kAudioServicesNoError then
    begin
      LYes := 1;
      AudioServicesSetProperty(kAudioServicesPropertyIsUISound, SizeOf(FSoundID), @FSoundID, SizeOf(LYes), @LYes);
    end;
  end;
end;

procedure TPlatformMuteCheck.Check;
begin
  if FSoundID <> 0 then
  begin
    FPlayStart := Now;
    AudioServicesPlaySystemSound(FSoundID);
  end;
end;

end.
