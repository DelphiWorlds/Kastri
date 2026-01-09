unit DW.Macapi.AVKit;

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
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.CocoaTypes, Macapi.AppKit, Macapi.AVFoundation, Macapi.Foundation,
  // DW
  DW.Macapi.AVFoundation;

const
  AVKitPlatformViewClass = NSView;
  AVKitPlatformColorClass = NSColor;
  AVCaptureViewControlsStyleInline = 0;
  AVCaptureViewControlsStyleFloating = 1;
  AVCaptureViewControlsStyleInlineDeviceSelection = 2;
  AVCaptureViewControlsStyleDefault = AVCaptureViewControlsStyleInline;
  AVVideoFrameAnalysisTypeNone = 0;
  AVVideoFrameAnalysisTypeDefault = 1;
  AVVideoFrameAnalysisTypeText = 2;
  AVVideoFrameAnalysisTypeSubject = 4;
  AVVideoFrameAnalysisTypeVisualSearch = 8;
  AVVideoFrameAnalysisTypeMachineReadableCode = 16;
  AVPlayerViewControlsStyleNone = 0;
  AVPlayerViewControlsStyleInline = 1;
  AVPlayerViewControlsStyleFloating = 2;
  AVPlayerViewControlsStyleMinimal = 3;
  AVPlayerViewControlsStyleDefault = AVPlayerViewControlsStyleInline;
  AVPlayerViewTrimOKButton = 0;
  AVPlayerViewTrimCancelButton = 1;
  AVRoutePickerViewButtonStateNormal = 0;
  AVRoutePickerViewButtonStateNormalHighlighted = 1;
  AVRoutePickerViewButtonStateActive = 2;
  AVRoutePickerViewButtonStateActiveHighlighted = 3;
  AVRoutePickerViewButtonStyleSystem = 0;
  AVRoutePickerViewButtonStylePlain = 1;
  AVRoutePickerViewButtonStyleCustom = 2;

type
  AVCaptureView = interface;
  AVCaptureViewDelegate = interface;
  AVPlaybackSpeed = interface;
  AVPictureInPictureController = interface;
  AVPictureInPictureControllerContentSource = interface;
  AVPictureInPictureControllerDelegate = interface;
  AVPictureInPictureSampleBufferPlaybackDelegate = interface;
  AVPlayerView = interface;
  AVPlayerViewDelegate = interface;
  AVPlayerViewPictureInPictureDelegate = interface;
  AVRoutePickerView = interface;
  AVRoutePickerViewDelegate = interface;

  AVCaptureViewControlsStyle = NSInteger;
  AVVideoFrameAnalysisType = NSInteger;
  AVPlayerViewControlsStyle = NSInteger;
  AVPlayerViewTrimResult = NSInteger;
  AVRoutePickerViewButtonState = NSInteger;
  AVRoutePickerViewButtonStyle = NSInteger;
  TAVPictureInPictureControllerDelegateBlockMethod1 = procedure(restored: Boolean) of object;
  TAVPictureInPictureSampleBufferPlaybackDelegateBlockMethod1 = procedure of object;
  TAVPlayerViewBlockMethod1 = procedure(result: AVPlayerViewTrimResult) of object;
  TAVPlayerViewDelegateBlockMethod1 = procedure(restored: Boolean) of object;
  TAVPlayerViewPictureInPictureDelegateBlockMethod1 = procedure(restored: Boolean) of object;

  AVCaptureViewClass = interface(NSViewClass)
    ['{A2216484-BA9E-480D-A4A5-21FCF8577133}']
  end;

  AVCaptureView = interface(NSView)
    ['{94212903-9D1B-4C43-8F79-A6F70B66B1F6}']
    function controlsStyle: AVCaptureViewControlsStyle; cdecl;
    function delegate: Pointer; cdecl;
    function fileOutput: AVCaptureFileOutput; cdecl;
    function session: AVCaptureSession; cdecl;
    procedure setControlsStyle(controlsStyle: AVCaptureViewControlsStyle); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setSession(session: AVCaptureSession; showVideoPreview: Boolean; showAudioPreview: Boolean); cdecl;
    procedure setVideoGravity(videoGravity: AVLayerVideoGravity); cdecl;
    function videoGravity: AVLayerVideoGravity; cdecl;
  end;
  TAVCaptureView = class(TOCGenericImport<AVCaptureViewClass, AVCaptureView>) end;

  AVCaptureViewDelegate = interface(IObjectiveC)
    ['{63C1D3A9-A710-47AC-A696-6C57AF030571}']
    procedure captureView(captureView: AVCaptureView; startRecordingToFileOutput: AVCaptureFileOutput); cdecl;
  end;

  AVPlaybackSpeedClass = interface(NSObjectClass)
    ['{06A91CFA-7EF1-46D6-BC4A-2ED5A8EB5486}']
    {class} function new: Pointer; cdecl;
    {class} function systemDefaultSpeeds: NSArray; cdecl;
  end;

  AVPlaybackSpeed = interface(NSObject)
    ['{F4AD71DD-30AE-473B-A3DB-A6DB2C933DAE}']
    function initWithRate(rate: Single; localizedName: NSString): Pointer; cdecl;
    function localizedName: NSString; cdecl;
    function localizedNumericName: NSString; cdecl;
    function rate: Single; cdecl;
  end;
  TAVPlaybackSpeed = class(TOCGenericImport<AVPlaybackSpeedClass, AVPlaybackSpeed>) end;

  AVPictureInPictureControllerClass = interface(NSObjectClass)
    ['{0BAB187F-0EB0-4090-BA3B-6AA889E93C70}']
    {class} function isPictureInPictureSupported: Boolean; cdecl;
  end;

  AVPictureInPictureController = interface(NSObject)
    ['{8095E594-C7E2-4B7F-9343-2748DFE58511}']
    function canStartPictureInPictureAutomaticallyFromInline: Boolean; cdecl;
    function canStopPictureInPicture: Boolean; cdecl;
    function contentSource: AVPictureInPictureControllerContentSource; cdecl;
    function delegate: Pointer; cdecl;
    function initWithContentSource(contentSource: AVPictureInPictureControllerContentSource): Pointer; cdecl;
    function initWithPlayerLayer(playerLayer: AVPlayerLayer): Pointer; cdecl;
    procedure invalidatePlaybackState; cdecl;
    function isPictureInPictureActive: Boolean; cdecl;
    function isPictureInPicturePossible: Boolean; cdecl;
    function isPictureInPictureSuspended: Boolean; cdecl;
    function playerLayer: AVPlayerLayer; cdecl;
    function requiresLinearPlayback: Boolean; cdecl;
    procedure setCanStartPictureInPictureAutomaticallyFromInline(canStartPictureInPictureAutomaticallyFromInline: Boolean); cdecl;
    procedure setContentSource(contentSource: AVPictureInPictureControllerContentSource); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setRequiresLinearPlayback(requiresLinearPlayback: Boolean); cdecl;
    procedure startPictureInPicture; cdecl;
    procedure stopPictureInPicture; cdecl;
  end;
  TAVPictureInPictureController = class(TOCGenericImport<AVPictureInPictureControllerClass, AVPictureInPictureController>) end;

  AVPictureInPictureControllerContentSourceClass = interface(NSObjectClass)
    ['{B00BEF47-21D4-4028-8E4C-E77BCCC54EBE}']
    {class} function new: Pointer; cdecl;
  end;

  AVPictureInPictureControllerContentSource = interface(NSObject)
    ['{BAA8ABF6-624D-445F-97DF-2834CB476251}']
    function initWithPlayerLayer(playerLayer: AVPlayerLayer): Pointer; cdecl;
    function initWithSampleBufferDisplayLayer(sampleBufferDisplayLayer: AVSampleBufferDisplayLayer; playbackDelegate: Pointer): Pointer; cdecl;
    function playerLayer: AVPlayerLayer; cdecl;
    function sampleBufferDisplayLayer: AVSampleBufferDisplayLayer; cdecl;
    function sampleBufferPlaybackDelegate: Pointer; cdecl;
  end;
  TAVPictureInPictureControllerContentSource = class(TOCGenericImport<AVPictureInPictureControllerContentSourceClass,
    AVPictureInPictureControllerContentSource>) end;

  AVPictureInPictureControllerDelegate = interface(IObjectiveC)
    ['{30D3F76A-5EBC-4858-AA43-6CE2419AD1B2}']
    procedure pictureInPictureController(pictureInPictureController: AVPictureInPictureController;
      restoreUserInterfaceForPictureInPictureStopWithCompletionHandler: Pointer); overload; cdecl;
    procedure pictureInPictureController(pictureInPictureController: AVPictureInPictureController;
      failedToStartPictureInPictureWithError: NSError); overload; cdecl;
    procedure pictureInPictureControllerDidStartPictureInPicture(pictureInPictureController: AVPictureInPictureController); cdecl;
    procedure pictureInPictureControllerDidStopPictureInPicture(pictureInPictureController: AVPictureInPictureController); cdecl;
    procedure pictureInPictureControllerWillStartPictureInPicture(pictureInPictureController: AVPictureInPictureController); cdecl;
    procedure pictureInPictureControllerWillStopPictureInPicture(pictureInPictureController: AVPictureInPictureController); cdecl;
  end;

  AVPictureInPictureSampleBufferPlaybackDelegate = interface(IObjectiveC)
    ['{44D201DC-C54D-4E12-AAAD-C5A9A785D962}']
    procedure pictureInPictureController(pictureInPictureController: AVPictureInPictureController;
      didTransitionToRenderSize: CMVideoDimensions); overload; cdecl;
    procedure pictureInPictureController(pictureInPictureController: AVPictureInPictureController;
      skipByInterval: CMTime; completionHandler: Pointer); overload; cdecl;
    procedure pictureInPictureController(pictureInPictureController: AVPictureInPictureController; setPlaying: Boolean); overload; cdecl;
    function pictureInPictureControllerIsPlaybackPaused(pictureInPictureController: AVPictureInPictureController): Boolean; cdecl;
    function pictureInPictureControllerShouldProhibitBackgroundAudioPlayback(pictureInPictureController: AVPictureInPictureController): Boolean; cdecl;
    function pictureInPictureControllerTimeRangeForPlayback(pictureInPictureController: AVPictureInPictureController): CMTimeRange; cdecl;
  end;

  AVPlayerViewClass = interface(NSViewClass)
    ['{CC79D9B2-DD19-4C45-89C2-9324BCE9807C}']
  end;

  AVPlayerView = interface(NSView)
    ['{5EE7F524-928B-495A-B2A2-F8D0D8E0CF47}']
    function actionPopUpButtonMenu: NSMenu; cdecl;
    function allowsMagnification: Boolean; cdecl;
    function allowsPictureInPicturePlayback: Boolean; cdecl;
    function allowsVideoFrameAnalysis: Boolean; cdecl;
    procedure beginTrimmingWithCompletionHandler(handler: TAVPlayerViewBlockMethod1); cdecl;
    function canBeginTrimming: Boolean; cdecl;
    function contentOverlayView: NSView; cdecl;
    function controlsStyle: AVPlayerViewControlsStyle; cdecl;
    function delegate: Pointer; cdecl;
    procedure flashChapterNumber(chapterNumber: NSUInteger; chapterTitle: NSString); cdecl;
    function isReadyForDisplay: Boolean; cdecl;
    function magnification: CGFloat; cdecl;
    function pictureInPictureDelegate: Pointer; cdecl;
    function player: AVPlayer; cdecl;
    function selectedSpeed: AVPlaybackSpeed; cdecl;
    procedure selectSpeed(speed: AVPlaybackSpeed); cdecl;
    procedure setActionPopUpButtonMenu(actionPopUpButtonMenu: NSMenu); cdecl;
    procedure setAllowsMagnification(allowsMagnification: Boolean); cdecl;
    procedure setAllowsPictureInPicturePlayback(allowsPictureInPicturePlayback: Boolean); cdecl;
    procedure setAllowsVideoFrameAnalysis(allowsVideoFrameAnalysis: Boolean); cdecl;
    procedure setControlsStyle(controlsStyle: AVPlayerViewControlsStyle); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setMagnification(magnification: CGFloat; centeredAtPoint: CGPoint); overload; cdecl;
    procedure setMagnification(magnification: CGFloat); overload; cdecl;
    procedure setPictureInPictureDelegate(pictureInPictureDelegate: Pointer); cdecl;
    procedure setPlayer(player: AVPlayer); cdecl;
    procedure setShowsFrameSteppingButtons(showsFrameSteppingButtons: Boolean); cdecl;
    procedure setShowsFullScreenToggleButton(showsFullScreenToggleButton: Boolean); cdecl;
    procedure setShowsSharingServiceButton(showsSharingServiceButton: Boolean); cdecl;
    procedure setShowsTimecodes(showsTimecodes: Boolean); cdecl;
    procedure setSpeeds(speeds: NSArray); cdecl;
    procedure setUpdatesNowPlayingInfoCenter(updatesNowPlayingInfoCenter: Boolean); cdecl;
    procedure setVideoFrameAnalysisTypes(videoFrameAnalysisTypes: AVVideoFrameAnalysisType); cdecl;
    procedure setVideoGravity(videoGravity: AVLayerVideoGravity); cdecl;
    function showsFrameSteppingButtons: Boolean; cdecl;
    function showsFullScreenToggleButton: Boolean; cdecl;
    function showsSharingServiceButton: Boolean; cdecl;
    function showsTimecodes: Boolean; cdecl;
    function speeds: NSArray; cdecl;
    function updatesNowPlayingInfoCenter: Boolean; cdecl;
    function videoBounds: NSRect; cdecl;
    function videoFrameAnalysisTypes: AVVideoFrameAnalysisType; cdecl;
    function videoGravity: AVLayerVideoGravity; cdecl;
  end;
  TAVPlayerView = class(TOCGenericImport<AVPlayerViewClass, AVPlayerView>) end;

  AVPlayerViewDelegate = interface(IObjectiveC)
    ['{1B104428-022C-4972-9B5F-94AB5F2CE8FC}']
    procedure playerView(playerView: AVPlayerView; restoreUserInterfaceForFullScreenExitWithCompletionHandler: Pointer); cdecl;
    procedure playerViewDidEnterFullScreen(playerView: AVPlayerView); cdecl;
    procedure playerViewDidExitFullScreen(playerView: AVPlayerView); cdecl;
    procedure playerViewWillEnterFullScreen(playerView: AVPlayerView); cdecl;
    procedure playerViewWillExitFullScreen(playerView: AVPlayerView); cdecl;
  end;

  AVPlayerViewPictureInPictureDelegate = interface(IObjectiveC)
    ['{EC9EF83E-16BE-4BC7-A25F-589B0AC2211D}']
    procedure playerView(playerView: AVPlayerView; restoreUserInterfaceForPictureInPictureStopWithCompletionHandler: Pointer); overload; cdecl;
    procedure playerView(playerView: AVPlayerView; failedToStartPictureInPictureWithError: NSError); overload; cdecl;
    procedure playerViewDidStartPictureInPicture(playerView: AVPlayerView); cdecl;
    procedure playerViewDidStopPictureInPicture(playerView: AVPlayerView); cdecl;
    function playerViewShouldAutomaticallyDismissAtPictureInPictureStart(playerView: AVPlayerView): Boolean; cdecl;
    procedure playerViewWillStartPictureInPicture(playerView: AVPlayerView); cdecl;
    procedure playerViewWillStopPictureInPicture(playerView: AVPlayerView); cdecl;
  end;

  AVRoutePickerViewClass = interface(NSViewClass)
    ['{17FADD1C-D3EA-4C4D-99FE-3E4CC159FC5D}']
  end;

  AVRoutePickerView = interface(NSView)
    ['{EDD54D0F-F844-436B-8A46-F80DEAEF3A2A}']
    function activeTintColor: NSColor; cdecl;
    // AVRouting function customRoutingController: AVCustomRoutingController; cdecl;
    function delegate: Pointer; cdecl;
    function isRoutePickerButtonBordered: Boolean; cdecl;
    function player: AVPlayer; cdecl;
    function prioritizesVideoDevices: Boolean; cdecl;
    function routePickerButtonColorForState(state: AVRoutePickerViewButtonState): NSColor; cdecl;
    function routePickerButtonStyle: AVRoutePickerViewButtonStyle; cdecl;
    procedure setActiveTintColor(activeTintColor: NSColor); cdecl;
    // AVRouting procedure setCustomRoutingController(customRoutingController: AVCustomRoutingController); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setPlayer(player: AVPlayer); cdecl;
    procedure setPrioritizesVideoDevices(prioritizesVideoDevices: Boolean); cdecl;
    procedure setRoutePickerButtonBordered(routePickerButtonBordered: Boolean); cdecl;
    procedure setRoutePickerButtonColor(color: NSColor; forState: AVRoutePickerViewButtonState); cdecl;
    procedure setRoutePickerButtonStyle(routePickerButtonStyle: AVRoutePickerViewButtonStyle); cdecl;
  end;
  TAVRoutePickerView = class(TOCGenericImport<AVRoutePickerViewClass, AVRoutePickerView>) end;

  AVRoutePickerViewDelegate = interface(IObjectiveC)
    ['{109BAF1E-F9DE-4944-B65C-3661D6466CC7}']
    procedure routePickerViewDidEndPresentingRoutes(routePickerView: AVRoutePickerView); cdecl;
    procedure routePickerViewWillBeginPresentingRoutes(routePickerView: AVRoutePickerView); cdecl;
  end;

const
  libAVKit = '/System/Library/Frameworks/AVKit.framework/AVKit';

implementation

uses
  System.SysUtils;

var
  AVKitModule: THandle;

initialization
  AVKitModule := LoadLibrary(libAVKit);

finalization
  if AVKitModule <> 0 then
    FreeLibrary(AVKitModule);

end.