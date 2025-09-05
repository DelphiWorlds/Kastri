unit DW.iOSapi.AVKit;

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
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation,
  // iOSapi
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit, iOSapi.CoreMedia, iOSapi.AVFoundation, iOSapi.CoreGraphics,
  // DW
  {$IF CompilerVersion < 37} DW.iOSapi.UIKit, {$ENDIF}
  DW.iOSapi.AVFoundation;

const
  AVKitPlatformViewClass = UIView;
  AVKitPlatformColorClass = UIColor;
  AVCaptureEventPhaseBegan = 0;
  AVCaptureEventPhaseEnded = 1;
  AVCaptureEventPhaseCancelled = 2;
  AVKitErrorUnknown = -1000;
  AVKitErrorPictureInPictureStartFailed = -1001;
  AVKitErrorContentRatingUnknown = -1100;
  AVKitErrorContentDisallowedByPasscode = -1101;
  AVKitErrorContentDisallowedByProfile = -1102;
  AVKitErrorRecordingFailed = -1200;
  AVVideoFrameAnalysisTypeNone = 0;
  AVVideoFrameAnalysisTypeDefault = 1;
  AVVideoFrameAnalysisTypeText = 2;
  AVVideoFrameAnalysisTypeSubject = 4;
  AVVideoFrameAnalysisTypeVisualSearch = 8;
  AVVideoFrameAnalysisTypeMachineReadableCode = 16;
  AVAudioSessionRouteSelectionNone = 0;
  AVAudioSessionRouteSelectionLocal = 1;
  AVAudioSessionRouteSelectionExternal = 2;
  AVPlayerViewControllerSkippingBehaviorDefault = 0;
  AVPlayerViewControllerSkippingBehaviorSkipItem = 1;
  AVRoutePickerViewButtonStateNormal = 0;
  AVRoutePickerViewButtonStateNormalHighlighted = 1;
  AVRoutePickerViewButtonStateActive = 2;
  AVRoutePickerViewButtonStateActiveHighlighted = 3;
  AVRoutePickerViewButtonStyleSystem = 0;
  AVRoutePickerViewButtonStylePlain = 1;
  AVRoutePickerViewButtonStyleCustom = 2;

type
  AVCaptureEvent = interface;
  AVCaptureEventInteraction = interface;
  AVInterstitialTimeRange = interface;
  AVPictureInPictureController = interface;
  AVPictureInPictureControllerContentSource = interface;
  AVPictureInPictureControllerDelegate = interface;
  AVPictureInPictureSampleBufferPlaybackDelegate = interface;
  AVPictureInPictureVideoCallViewController = interface;
  AVPlaybackSpeed = interface;
  AVPlayerViewController = interface;
  AVPlayerViewControllerDelegate = interface;
  AVPlayerViewControllerAnimationCoordinator = interface;
  AVRoutePickerView = interface;
  AVRoutePickerViewDelegate = interface;

  AVCaptureEventPhase = NSInteger;
  AVKitError = NSInteger;
  AVVideoFrameAnalysisType = NSInteger;
  AVAudioSessionRouteSelection = NSInteger;
  AVPlayerViewControllerSkippingBehavior = NSInteger;
  AVRoutePickerViewButtonState = NSInteger;
  AVRoutePickerViewButtonStyle = NSInteger;

  TAVCaptureEventInteractionBlockMethod1 = procedure(event: AVCaptureEvent) of object;
  TAVPictureInPictureControllerDelegateBlockMethod1 = procedure(restored: Boolean) of object;
  TAVPictureInPictureSampleBufferPlaybackDelegateBlockMethod1 = procedure of object;
  TAVAudioSessionBlockMethod1 = procedure(shouldStartPlayback: Boolean; routeSelection: AVAudioSessionRouteSelection) of object;
  TAVPlayerViewControllerBlockMethod1 = procedure(success: Boolean) of object;
  TAVPlayerViewControllerDelegateBlockMethod1 = procedure(restored: Boolean) of object;
  TAVPlayerViewControllerDelegateBlockMethod2 = procedure(success: Boolean) of object;
  TAVPlayerViewControllerAnimationCoordinatorBlockMethod1 = procedure of object;
  TAVPlayerViewControllerAnimationCoordinatorBlockMethod2 = procedure(finished: Boolean) of object;

  AVCaptureEventClass = interface(NSObjectClass)
    ['{8A5EA105-ADF1-446F-B6E6-62A408D73CCB}']
    {class} function new: Pointer; cdecl;
  end;

  AVCaptureEvent = interface(NSObject)
    ['{4E93607F-A2D6-4793-821B-C4120D3D7928}']
    function phase: AVCaptureEventPhase; cdecl;
  end;
  TAVCaptureEvent = class(TOCGenericImport<AVCaptureEventClass, AVCaptureEvent>) end;

  AVCaptureEventInteractionClass = interface(NSObjectClass)
    ['{B13D9AC6-6817-4FA9-8C65-5C8C1849DF11}']
    {class} function new: Pointer; cdecl;
  end;

  AVCaptureEventInteraction = interface(NSObject)
    ['{BD1C611C-100F-4677-8466-8DCF6582FE7C}']
    function initWithEventHandler(handler: TAVCaptureEventInteractionBlockMethod1): Pointer; cdecl;
    function initWithPrimaryEventHandler(primaryHandler: TAVCaptureEventInteractionBlockMethod1;
      secondaryEventHandler: TAVCaptureEventInteractionBlockMethod1): Pointer; cdecl;
    function isEnabled: Boolean; cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
  end;
  TAVCaptureEventInteraction = class(TOCGenericImport<AVCaptureEventInteractionClass, AVCaptureEventInteraction>) end;

  AVInterstitialTimeRangeClass = interface(NSObjectClass)
    ['{0CAF5759-D4AD-41D4-BC2A-80AEF0FCEF50}']
  end;

  AVInterstitialTimeRange = interface(NSObject)
    ['{FFE38CFC-6A8F-477E-975A-EC0D80102E34}']
    function initWithTimeRange(timeRange: CMTimeRange): Pointer; cdecl;
    function timeRange: CMTimeRange; cdecl;
  end;
  TAVInterstitialTimeRange = class(TOCGenericImport<AVInterstitialTimeRangeClass, AVInterstitialTimeRange>) end;

  AVPictureInPictureControllerClass = interface(NSObjectClass)
    ['{128A9713-D08E-4CDF-A5D4-6C5AB3C8B48F}']
    {class} function isPictureInPictureSupported: Boolean; cdecl;
    {class} function pictureInPictureButtonStartImage: UIImage; cdecl;
    {class} function pictureInPictureButtonStartImageCompatibleWithTraitCollection(traitCollection: UITraitCollection): UIImage; cdecl;
    {class} function pictureInPictureButtonStopImage: UIImage; cdecl;
    {class} function pictureInPictureButtonStopImageCompatibleWithTraitCollection(traitCollection: UITraitCollection): UIImage; cdecl;
  end;

  AVPictureInPictureController = interface(NSObject)
    ['{D9EE96B3-E58A-45F3-9889-75111EAE9D30}']
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
    ['{AF812D7E-324F-4985-81D2-0132B45C9196}']
    {class} function new: Pointer; cdecl;
  end;

  AVPictureInPictureControllerContentSource = interface(NSObject)
    ['{54C33575-DFC4-49B8-8495-B184A951BD13}']
    function activeVideoCallContentViewController: AVPictureInPictureVideoCallViewController; cdecl;
    function activeVideoCallSourceView: UIView; cdecl;
    function initWithActiveVideoCallSourceView(sourceView: UIView; contentViewController: AVPictureInPictureVideoCallViewController): Pointer; cdecl;
    function initWithPlayerLayer(playerLayer: AVPlayerLayer): Pointer; cdecl;
    function initWithSampleBufferDisplayLayer(sampleBufferDisplayLayer: AVSampleBufferDisplayLayer; playbackDelegate: Pointer): Pointer; cdecl;
    function playerLayer: AVPlayerLayer; cdecl;
    function sampleBufferDisplayLayer: AVSampleBufferDisplayLayer; cdecl;
    function sampleBufferPlaybackDelegate: Pointer; cdecl;
  end;
  TAVPictureInPictureControllerContentSource = class(TOCGenericImport<AVPictureInPictureControllerContentSourceClass,
    AVPictureInPictureControllerContentSource>) end;

  AVPictureInPictureControllerDelegate = interface(IObjectiveC)
    ['{33BA56D5-7742-41BD-9271-B8F52E28C8CC}']
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
    ['{23698877-8F8B-4419-9DC7-491B7BC1C948}']
    procedure pictureInPictureController(pictureInPictureController: AVPictureInPictureController;
      didTransitionToRenderSize: CMVideoDimensions); overload; cdecl;
    procedure pictureInPictureController(pictureInPictureController: AVPictureInPictureController; skipByInterval: CMTime;
      completionHandler: Pointer); overload; cdecl;
    procedure pictureInPictureController(pictureInPictureController: AVPictureInPictureController; setPlaying: Boolean); overload; cdecl;
    function pictureInPictureControllerIsPlaybackPaused(pictureInPictureController: AVPictureInPictureController): Boolean; cdecl;
    function pictureInPictureControllerShouldProhibitBackgroundAudioPlayback(pictureInPictureController: AVPictureInPictureController): Boolean; cdecl;
    function pictureInPictureControllerTimeRangeForPlayback(pictureInPictureController: AVPictureInPictureController): CMTimeRange; cdecl;
  end;

  AVPictureInPictureVideoCallViewControllerClass = interface(UIViewControllerClass)
    ['{84CED83F-B9FD-4E6C-96A7-8B346D36AD9C}']
  end;

  AVPictureInPictureVideoCallViewController = interface(UIViewController)
    ['{21D018C3-096A-4F20-A434-BFE7D274957D}']
  end;
  TAVPictureInPictureVideoCallViewController = class(TOCGenericImport<AVPictureInPictureVideoCallViewControllerClass,
    AVPictureInPictureVideoCallViewController>) end;

  AVPlaybackSpeedClass = interface(NSObjectClass)
    ['{1563BF1A-28B0-4EFC-8986-BA3A1CC5D6C0}']
    {class} function new: Pointer; cdecl;
    {class} function systemDefaultSpeeds: NSArray; cdecl;
  end;

  AVPlaybackSpeed = interface(NSObject)
    ['{188D3E35-2AAB-4638-8256-0B4A3135FF37}']
    function initWithRate(rate: Single; localizedName: NSString): Pointer; cdecl;
    function localizedName: NSString; cdecl;
    function localizedNumericName: NSString; cdecl;
    function rate: Single; cdecl;
  end;
  TAVPlaybackSpeed = class(TOCGenericImport<AVPlaybackSpeedClass, AVPlaybackSpeed>) end;

  AVPlayerViewControllerClass = interface(UIViewControllerClass)
    ['{54B91DBE-344A-4996-AE38-2C1DCAEB2212}']
  end;

  AVPlayerViewController = interface(UIViewController)
    ['{CBB53B56-FF82-4C79-8247-44F0C03949B6}']
    function allowedSubtitleOptionLanguages: NSArray; cdecl;
    function allowsPictureInPicturePlayback: Boolean; cdecl;
    function allowsVideoFrameAnalysis: Boolean; cdecl;
    function appliesPreferredDisplayCriteriaAutomatically: Boolean; cdecl;
    procedure beginTrimmingWithCompletionHandler(handler: TAVPlayerViewControllerBlockMethod1); cdecl;
    function canBeginTrimming: Boolean; cdecl;
    function canStartPictureInPictureAutomaticallyFromInline: Boolean; cdecl;
    function contentOverlayView: UIView; cdecl;
    function contextualActions: NSArray; cdecl;
    function contextualActionsInfoView: UIView; cdecl;
    function contextualActionsPreviewImage: UIImage; cdecl;
    function customInfoViewController: UIViewController; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("Use customInfoViewControllers", tvos(11.0, 15.0))
    function customInfoViewControllers: NSArray; cdecl;
    function customOverlayViewController: UIViewController; cdecl;
    function delegate: Pointer; cdecl;
    function entersFullScreenWhenPlaybackBegins: Boolean; cdecl;
    function exitsFullScreenWhenPlaybackEnds: Boolean; cdecl;
    function infoViewActions: NSArray; cdecl;
    function isReadyForDisplay: Boolean; cdecl;
    function isSkipBackwardEnabled: Boolean; cdecl;
    function isSkipForwardEnabled: Boolean; cdecl;
    function pixelBufferAttributes: NSDictionary; cdecl;
    function playbackControlsIncludeInfoViews: Boolean; cdecl;
    function playbackControlsIncludeTransportBar: Boolean; cdecl;
    function player: AVPlayer; cdecl;
    function requiresFullSubtitles: Boolean; cdecl;
    function requiresLinearPlayback: Boolean; cdecl;
    function requiresMonoscopicViewingMode: Boolean; cdecl;
    function selectedSpeed: AVPlaybackSpeed; cdecl;
    procedure selectSpeed(speed: AVPlaybackSpeed); cdecl;
    procedure setAllowedSubtitleOptionLanguages(allowedSubtitleOptionLanguages: NSArray); cdecl;
    procedure setAllowsPictureInPicturePlayback(allowsPictureInPicturePlayback: Boolean); cdecl;
    procedure setAllowsVideoFrameAnalysis(allowsVideoFrameAnalysis: Boolean); cdecl;
    procedure setAppliesPreferredDisplayCriteriaAutomatically(appliesPreferredDisplayCriteriaAutomatically: Boolean); cdecl;
    procedure setCanStartPictureInPictureAutomaticallyFromInline(canStartPictureInPictureAutomaticallyFromInline: Boolean); cdecl;
    procedure setContextualActions(contextualActions: NSArray); cdecl;
    procedure setContextualActionsPreviewImage(contextualActionsPreviewImage: UIImage); cdecl;
    procedure setCustomInfoViewController(customInfoViewController: UIViewController); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("Use customInfoViewControllers", tvos(11.0, 15.0))
    procedure setCustomInfoViewControllers(customInfoViewControllers: NSArray); cdecl;
    procedure setCustomOverlayViewController(customOverlayViewController: UIViewController); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setEntersFullScreenWhenPlaybackBegins(entersFullScreenWhenPlaybackBegins: Boolean); cdecl;
    procedure setExitsFullScreenWhenPlaybackEnds(exitsFullScreenWhenPlaybackEnds: Boolean); cdecl;
    procedure setInfoViewActions(infoViewActions: NSArray); cdecl;
    procedure setPixelBufferAttributes(pixelBufferAttributes: NSDictionary); cdecl;
    procedure setPlaybackControlsIncludeInfoViews(playbackControlsIncludeInfoViews: Boolean); cdecl;
    procedure setPlaybackControlsIncludeTransportBar(playbackControlsIncludeTransportBar: Boolean); cdecl;
    procedure setPlayer(player: AVPlayer); cdecl;
    procedure setRequiresFullSubtitles(requiresFullSubtitles: Boolean); cdecl;
    procedure setRequiresLinearPlayback(requiresLinearPlayback: Boolean); cdecl;
    procedure setRequiresMonoscopicViewingMode(requiresMonoscopicViewingMode: Boolean); cdecl;
    procedure setShowsPlaybackControls(showsPlaybackControls: Boolean); cdecl;
    procedure setShowsTimecodes(showsTimecodes: Boolean); cdecl;
    procedure setSkipBackwardEnabled(skipBackwardEnabled: Boolean); cdecl;
    procedure setSkipForwardEnabled(skipForwardEnabled: Boolean); cdecl;
    procedure setSkippingBehavior(skippingBehavior: AVPlayerViewControllerSkippingBehavior); cdecl;
    procedure setSpeeds(speeds: NSArray); cdecl;
    procedure setTransportBarCustomMenuItems(transportBarCustomMenuItems: NSArray); cdecl;
    procedure setTransportBarIncludesTitleView(transportBarIncludesTitleView: Boolean); cdecl;
    procedure setUpdatesNowPlayingInfoCenter(updatesNowPlayingInfoCenter: Boolean); cdecl;
    procedure setVideoFrameAnalysisTypes(videoFrameAnalysisTypes: AVVideoFrameAnalysisType); cdecl;
    procedure setVideoGravity(videoGravity: AVLayerVideoGravity); cdecl;
    function showsPlaybackControls: Boolean; cdecl;
    function showsTimecodes: Boolean; cdecl;
    function skippingBehavior: AVPlayerViewControllerSkippingBehavior; cdecl;
    function speeds: NSArray; cdecl;
    function toggleLookupAction: UIAction; cdecl;
    function transportBarCustomMenuItems: NSArray; cdecl;
    function transportBarIncludesTitleView: Boolean; cdecl;
    function unobscuredContentGuide: UILayoutGuide; cdecl;
    function updatesNowPlayingInfoCenter: Boolean; cdecl;
    function videoBounds: CGRect; cdecl;
    function videoFrameAnalysisTypes: AVVideoFrameAnalysisType; cdecl;
    function videoGravity: AVLayerVideoGravity; cdecl;
  end;
  TAVPlayerViewController = class(TOCGenericImport<AVPlayerViewControllerClass, AVPlayerViewController>) end;

  AVPlayerViewControllerDelegate = interface(IObjectiveC)
    ['{24B3029E-E160-4F6D-8DBA-1020CFDEA6B4}']
    function nextChannelInterstitialViewControllerForPlayerViewController(playerViewController: AVPlayerViewController): UIViewController; cdecl;
    procedure playerViewController(playerViewController: AVPlayerViewController; didSelectExternalSubtitleOptionLanguage: NSString); overload; cdecl;
    procedure playerViewController(playerViewController: AVPlayerViewController; didSelectMediaSelectionOption: AVMediaSelectionOption;
      inMediaSelectionGroup: AVMediaSelectionGroup); overload; cdecl;
    procedure playerViewController(playerViewController: AVPlayerViewController; failedToStartPictureInPictureWithError: NSError); overload; cdecl;
    procedure playerViewController(playerViewController: AVPlayerViewController; willResumePlaybackAfterUserNavigatedFromTime: CMTime;
      toTime: CMTime); overload; cdecl;
    procedure playerViewController(playerViewController: AVPlayerViewController; willPresentInterstitialTimeRange: AVInterstitialTimeRange); overload; cdecl;
    procedure playerViewController(playerViewController: AVPlayerViewController; willTransitionToVisibilityOfTransportBar: Boolean;
      withAnimationCoordinator: Pointer); overload; cdecl;
    // AVFoundation function playerViewController(playerViewController: AVPlayerViewController; shouldPresentContentProposal: AVContentProposal): Boolean; overload; cdecl;
    procedure playerViewController(playerViewController: AVPlayerViewController;
      willBeginFullScreenPresentationWithAnimationCoordinator: Pointer); overload; cdecl;
    // AVFoundation [MethodName('playerViewController:didAcceptContentProposal:')]
    // AVFoundation procedure playerViewControllerDidAcceptContentProposal(playerViewController: AVPlayerViewController; didAcceptContentProposal: AVContentProposal); cdecl;
    procedure playerViewControllerDidEndDismissalTransition(playerViewController: AVPlayerViewController); cdecl;
    [MethodName('playerViewController:didPresentInterstitialTimeRange:')]
    procedure playerViewControllerDidPresentInterstitialTimeRange(playerViewController: AVPlayerViewController;
      didPresentInterstitialTimeRange: AVInterstitialTimeRange); cdecl;
    // AVFoundation [MethodName('playerViewController:didRejectContentProposal:')]
    // AVFoundation procedure playerViewControllerDidRejectContentProposal(playerViewController: AVPlayerViewController; didRejectContentProposal: AVContentProposal); cdecl;
    procedure playerViewControllerDidStartPictureInPicture(playerViewController: AVPlayerViewController); cdecl;
    procedure playerViewControllerDidStopPictureInPicture(playerViewController: AVPlayerViewController); cdecl;
    [MethodName('playerViewController:restoreUserInterfaceForFullScreenExitWithCompletionHandler:')]
    procedure playerViewControllerRestoreUserInterfaceForFullScreenExitWithCompletionHandler(playerViewController: AVPlayerViewController;
      restoreUserInterfaceForFullScreenExitWithCompletionHandler: Pointer); cdecl;
    [MethodName('playerViewController:restoreUserInterfaceForPictureInPictureStopWithCompletionHandler:')]
    procedure playerViewControllerRestoreUserInterfaceForPictureInPictureStopWithCompletionHandler(playerViewController: AVPlayerViewController;
      restoreUserInterfaceForPictureInPictureStopWithCompletionHandler: Pointer); cdecl;
    function playerViewControllerShouldAutomaticallyDismissAtPictureInPictureStart(playerViewController: AVPlayerViewController): Boolean; cdecl;
    function playerViewControllerShouldDismiss(playerViewController: AVPlayerViewController): Boolean; cdecl;
    [MethodName('playerViewController:skipToNextChannel:')]
    procedure playerViewControllerSkipToNextChannel(playerViewController: AVPlayerViewController; skipToNextChannel: Pointer); cdecl;
    [MethodName('playerViewController:skipToPreviousChannel:')]
    procedure playerViewControllerSkipToPreviousChannel(playerViewController: AVPlayerViewController; skipToPreviousChannel: Pointer); cdecl;
    [MethodName('playerViewController:timeToSeekAfterUserNavigatedFromTime:toTime:')]
    function playerViewControllerTimeToSeekAfterUserNavigatedFromTime(playerViewController: AVPlayerViewController;
      timeToSeekAfterUserNavigatedFromTime: CMTime; toTime: CMTime): CMTime; cdecl;
    procedure playerViewControllerWillBeginDismissalTransition(playerViewController: AVPlayerViewController); cdecl;
    [MethodName('playerViewController:willEndFullScreenPresentationWithAnimationCoordinator:')]
    procedure playerViewControllerWillEndFullScreenPresentationWithAnimationCoordinator(playerViewController: AVPlayerViewController;
      willEndFullScreenPresentationWithAnimationCoordinator: Pointer); cdecl;
    procedure playerViewControllerWillStartPictureInPicture(playerViewController: AVPlayerViewController); cdecl;
    procedure playerViewControllerWillStopPictureInPicture(playerViewController: AVPlayerViewController); cdecl;
    function previousChannelInterstitialViewControllerForPlayerViewController(playerViewController: AVPlayerViewController): UIViewController; cdecl;
    procedure skipToNextItemForPlayerViewController(playerViewController: AVPlayerViewController); cdecl;
    procedure skipToPreviousItemForPlayerViewController(playerViewController: AVPlayerViewController); cdecl;
  end;

  AVPlayerViewControllerAnimationCoordinator = interface(IObjectiveC)
    ['{2ADBC222-14B0-4824-BCA7-5EBEC913BE84}']
    procedure addCoordinatedAnimations(animations: Pointer; completion: Pointer); cdecl;
  end;

  AVRoutePickerViewClass = interface(UIViewClass)
    ['{E7758EEE-ACD9-4734-B53E-4BEE7A0C2E98}']
  end;

  AVRoutePickerView = interface(UIView)
    ['{D0BD5164-FB52-4D10-BF1C-003E6519744F}']
    function activeTintColor: UIColor; cdecl;
    // AVFoundation function customRoutingController: AVCustomRoutingController; cdecl;
    function delegate: Pointer; cdecl;
    function isRoutePickerButtonBordered: Boolean; cdecl;
    function player: AVPlayer; cdecl;
    function prioritizesVideoDevices: Boolean; cdecl;
    function routePickerButtonColorForState(state: AVRoutePickerViewButtonState): UIColor; cdecl;
    function routePickerButtonStyle: AVRoutePickerViewButtonStyle; cdecl;
    procedure setActiveTintColor(activeTintColor: UIColor); cdecl;
    // AVFoundation procedure setCustomRoutingController(customRoutingController: AVCustomRoutingController); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setPlayer(player: AVPlayer); cdecl;
    procedure setPrioritizesVideoDevices(prioritizesVideoDevices: Boolean); cdecl;
    procedure setRoutePickerButtonBordered(routePickerButtonBordered: Boolean); cdecl;
    procedure setRoutePickerButtonColor(color: UIColor; forState: AVRoutePickerViewButtonState); cdecl;
    procedure setRoutePickerButtonStyle(routePickerButtonStyle: AVRoutePickerViewButtonStyle); cdecl;
  end;
  TAVRoutePickerView = class(TOCGenericImport<AVRoutePickerViewClass, AVRoutePickerView>) end;

  AVRoutePickerViewDelegate = interface(IObjectiveC)
    ['{536B119B-7679-45A0-BBD9-861024A49ECB}']
    procedure routePickerViewDidEndPresentingRoutes(routePickerView: AVRoutePickerView); cdecl;
    procedure routePickerViewWillBeginPresentingRoutes(routePickerView: AVRoutePickerView); cdecl;
  end;

function AVKitErrorDomain: NSString;

const
  libAVKit = '/System/Library/Frameworks/AVKit.framework/AVKit';

implementation

uses
  Posix.Dlfcn;

var
  AVKitModule: THandle;

function AVKitErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libAVKit, 'AVKitErrorDomain');
end;

initialization
  AVKitModule := dlopen(MarshaledAString(libAVKit), RTLD_LAZY);

finalization
  dlclose(AVKitModule);

end.