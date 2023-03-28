unit DW.Macapi.AVKit;

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
  Macapi.AppKit, Macapi.CocoaTypes, Macapi.Dispatch, Macapi.Foundation, Macapi.Mach, Macapi.ObjCRuntime, Macapi.ObjectiveC, Macapi.AVFoundation;

const
  libAVKit = '/System/Library/Frameworks/AVKit.framework/AVKit';

  AVCaptureViewControlsStyleInline = 0;
  AVCaptureViewControlsStyleFloating = 1;
  AVCaptureViewControlsStyleInlineDeviceSelection = 2;
  AVCaptureViewControlsStyleDefault = AVCaptureViewControlsStyleInline;
  AVPlayerViewControlsStyleNone = 0;
  AVPlayerViewControlsStyleInline = 1;
  AVPlayerViewControlsStyleFloating = 2;
  AVPlayerViewControlsStyleMinimal = 3;
  AVPlayerViewControlsStyleDefault = AVPlayerViewControlsStyleInline;
  AVPlayerViewTrimOKButton = 0;
  AVPlayerViewTrimCancelButton = 1;

type
  AVCaptureViewDelegate = interface;
  AVCaptureView = interface;
  AVPlayerView = interface;

  AVCaptureViewControlsStyle = Integer;
  AVPlayerViewControlsStyle = Integer;
  AVPlayerViewTrimResult = Integer;

  TAVKitHandler = procedure(param1: AVPlayerViewTrimResult) of object;

  AVCaptureViewClass = interface(NSViewClass)
    ['{E87B1B83-ECD4-46F9-B21C-FE52136FBF8C}']
  end;

  AVCaptureView = interface(NSView)
    ['{258F6AA4-A1CC-4D49-94EA-3A25248497EE}']
    function session: AVCaptureSession; cdecl;
    procedure setSession(session: AVCaptureSession; showVideoPreview: Pointer; showAudioPreview: Pointer); cdecl;
    function fileOutput: AVCaptureFileOutput; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    function delegate: Pointer; cdecl;
    procedure setControlsStyle(controlsStyle: AVCaptureViewControlsStyle); cdecl;
    function controlsStyle: AVCaptureViewControlsStyle; cdecl;
    procedure setVideoGravity(videoGravity: NSString); cdecl;
    function videoGravity: NSString; cdecl;
  end;
  TAVCaptureView = class(TOCGenericImport<AVCaptureViewClass, AVCaptureView>)
  end;

  AVPlayerViewClass = interface(NSViewClass)
    ['{CD776F30-AA3A-4583-86F5-11A06A4B69DB}']
  end;

  AVPlayerView = interface(NSView)
    ['{F271C096-94EE-4F8B-856A-48B9129EB2ED}']
    procedure setPlayer(player: AVPlayer); cdecl;
    function player: AVPlayer; cdecl;
    procedure setControlsStyle(controlsStyle: AVPlayerViewControlsStyle); cdecl;
    function controlsStyle: AVPlayerViewControlsStyle; cdecl;
    procedure setVideoGravity(videoGravity: NSString); cdecl;
    function videoGravity: NSString; cdecl;
    function isReadyForDisplay: Integer; cdecl;
    function videoBounds: NSRect; cdecl;
    function contentOverlayView: NSView; cdecl;
    procedure setShowsFrameSteppingButtons(showsFrameSteppingButtons: Integer); cdecl;
    function showsFrameSteppingButtons: Integer; cdecl;
    procedure setShowsSharingServiceButton(showsSharingServiceButton: Integer); cdecl;
    function showsSharingServiceButton: Integer; cdecl;
    procedure setActionPopUpButtonMenu(actionPopUpButtonMenu: NSMenu); cdecl;
    function actionPopUpButtonMenu: NSMenu; cdecl;
    procedure setShowsFullScreenToggleButton(showsFullScreenToggleButton: Integer); cdecl;
    function showsFullScreenToggleButton: Integer; cdecl;
    function canBeginTrimming: Integer; cdecl;
    procedure beginTrimmingWithCompletionHandler(handler: TAVKitHandler); cdecl;
    procedure flashChapterNumber(chapterNumber: Pointer; chapterTitle: NSString); cdecl;
  end;
  TAVPlayerView = class(TOCGenericImport<AVPlayerViewClass, AVPlayerView>)
  end;

  AVCaptureViewDelegate = interface(IObjectiveC)
    ['{94DBF0F9-7022-482E-B2F6-30210EB83300}']
    procedure captureView(captureView: AVCaptureView; startRecordingToFileOutput: AVCaptureFileOutput); cdecl;
  end;

procedure libAVKitLoader; cdecl; external libAVKit;

procedure ForceLoadAVKit;

implementation

procedure ForceLoadAVKit;
begin
  AVMediaTypeAudio; // Force load AVFoundation first
  CocoaPointerConst(libAVKit, 'FakePointer');
end;

end.
