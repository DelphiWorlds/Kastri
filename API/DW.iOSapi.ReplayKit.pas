unit DW.iOSapi.ReplayKit;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2021 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  Macapi.ObjectiveC, Macapi.CoreFoundation, iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit, iOSapi.CoreMedia;

const
  RPSampleBufferTypeVideo = 1;
  RPSampleBufferTypeAudioApp = 2;
  RPSampleBufferTypeAudioMic = 3;
  RPCameraPositionFront = 1;
  RPCameraPositionBack = 2;
  RPRecordingErrorUnknown = -5800;
  RPRecordingErrorUserDeclined = -5801;
  RPRecordingErrorDisabled = -5802;
  RPRecordingErrorFailedToStart = -5803;
  RPRecordingErrorFailed = -5804;
  RPRecordingErrorInsufficientStorage = -5805;
  RPRecordingErrorInterrupted = -5806;
  RPRecordingErrorContentResize = -5807;
  RPRecordingErrorBroadcastInvalidSession = -5808;
  RPRecordingErrorSystemDormancy = -5809;
  RPRecordingErrorEntitlements = -5810;
  RPRecordingErrorActivePhoneCall = -5811;
  RPRecordingErrorFailedToSave = -5812;
  RPRecordingErrorCarPlay = -5813;
  RPRecordingErrorFailedApplicationConnectionInvalid = -5814;
  RPRecordingErrorFailedApplicationConnectionInterrupted = -5815;
  RPRecordingErrorFailedNoMatchingApplicationContext = -5816;
  RPRecordingErrorFailedMediaServicesFailure = -5817;
  RPRecordingErrorVideoMixingFailure = -5818;
  RPRecordingErrorBroadcastSetupFailed = -5819;
  RPRecordingErrorFailedToObtainURL = -5820;
  RPRecordingErrorFailedIncorrectTimeStamps = -5821;
  RPRecordingErrorFailedToProcessFirstSample = -5822;
  RPRecordingErrorFailedAssetWriterFailedToSave = -5823;
  RPRecordingErrorFailedNoAssetWriter = -5824;
  RPRecordingErrorFailedAssetWriterInWrongState = -5825;
  RPRecordingErrorFailedAssetWriterExportFailed = -5826;
  RPRecordingErrorFailedToRemoveFile = -5827;
  RPRecordingErrorFailedAssetWriterExportCanceled = -5828;
  RPRecordingErrorAttemptToStopNonRecording = -5829;
  RPRecordingErrorAttemptToStartInRecordingState = -5830;
  RPRecordingErrorPhotoFailure = -5831;
  RPRecordingErrorRecordingInvalidSession = -5832;
  RPRecordingErrorFailedToStartCaptureStack = -5833;
  RPRecordingErrorInvalidParameter = -5834;
  RPRecordingErrorFilePermissions = -5835;
  RPRecordingErrorExportClipToURLInProgress = -5836;
  RPRecordingErrorCodeSuccessful = 0;

type
  RPPreviewViewController = interface;
  RPPreviewViewControllerDelegate = interface;
  RPBroadcastConfiguration = interface;
  RPBroadcastHandler = interface;
  RPBroadcastMP4ClipHandler = interface;
  RPBroadcastSampleHandler = interface;
  RPScreenRecorder = interface;
  RPScreenRecorderDelegate = interface;
  RPBroadcastActivityViewController = interface;
  RPBroadcastActivityViewControllerDelegate = interface;
  RPBroadcastController = interface;
  RPBroadcastControllerDelegate = interface;
  RPSystemBroadcastPickerView = interface;

  RPSampleBufferType = NSInteger;
  RPCameraPosition = NSInteger;
  RPRecordingErrorCode = NSInteger;
  TNSExtensionContextBlockMethod1 = procedure(bundleID: NSString; displayName: NSString; appIcon: UIImage) of object;
  TRPScreenRecorderBlockMethod1 = procedure(error: NSError) of object;
  TRPScreenRecorderBlockMethod2 = procedure(previewViewController: RPPreviewViewController; error: NSError) of object;
  TRPScreenRecorderBlockMethod3 = procedure of object;
  TRPScreenRecorderBlockMethod4 = procedure(sampleBuffer: CMSampleBufferRef; bufferType: RPSampleBufferType; error: NSError) of object;
  TRPBroadcastActivityViewControllerBlockMethod1 = procedure(broadcastActivityViewController: RPBroadcastActivityViewController;
    error: NSError) of object;
  TRPBroadcastControllerBlockMethod1 = procedure(error: NSError) of object;

  RPPreviewViewControllerClass = interface(UIViewControllerClass)
    ['{843A3138-1A27-4CE0-B5CA-835FC403292D}']
  end;

  RPPreviewViewController = interface(UIViewController)
    ['{0422BDAF-FB59-4C17-AF47-AB60E9AD29A8}']
    function previewControllerDelegate: Pointer; cdecl;
    procedure setPreviewControllerDelegate(previewControllerDelegate: Pointer); cdecl;
  end;
  TRPPreviewViewController = class(TOCGenericImport<RPPreviewViewControllerClass, RPPreviewViewController>) end;

  RPPreviewViewControllerDelegate = interface(IObjectiveC)
    ['{2ABFDF1C-F5C8-4E14-B98C-3AC79300D9F4}']
    procedure previewController(previewController: RPPreviewViewController; didFinishWithActivityTypes: NSSet); cdecl;
    procedure previewControllerDidFinish(previewController: RPPreviewViewController); cdecl;
  end;

  RPBroadcastConfigurationClass = interface(NSObjectClass)
    ['{1873CB2B-8501-4D6D-99B3-005EA6638C66}']
  end;

  RPBroadcastConfiguration = interface(NSObject)
    ['{8A3C86B4-DB04-43DF-8704-928E4996A229}']
    function clipDuration: NSTimeInterval; cdecl;
    procedure setClipDuration(clipDuration: NSTimeInterval); cdecl;
    procedure setVideoCompressionProperties(videoCompressionProperties: NSDictionary); cdecl;
    function videoCompressionProperties: NSDictionary; cdecl;
  end;
  TRPBroadcastConfiguration = class(TOCGenericImport<RPBroadcastConfigurationClass, RPBroadcastConfiguration>) end;

  RPBroadcastHandlerClass = interface(NSObjectClass)
    ['{77D77028-E1EB-4F57-92B8-FE241052E040}']
  end;

  RPBroadcastHandler = interface(NSObject)
    ['{ED98E2C7-B886-4EC2-A906-023FDBAD9A69}']
    procedure updateBroadcastURL(broadcastURL: NSURL); cdecl;
    procedure updateServiceInfo(serviceInfo: NSDictionary); cdecl;
  end;
  TRPBroadcastHandler = class(TOCGenericImport<RPBroadcastHandlerClass, RPBroadcastHandler>) end;

  RPBroadcastMP4ClipHandlerClass = interface(RPBroadcastHandlerClass)
    ['{CFCC9FF3-9BD5-4790-A1EB-2711B66DB7F9}']
  end;

  RPBroadcastMP4ClipHandler = interface(RPBroadcastHandler)
    ['{BBB552B4-5AEE-4678-ACB2-60AFCF45936D}']
    procedure finishedProcessingMP4ClipWithUpdatedBroadcastConfiguration(broadcastConfiguration: RPBroadcastConfiguration; error: NSError); cdecl;
    procedure processMP4ClipWithURL(mp4ClipURL: NSURL; setupInfo: NSDictionary; finished: Boolean); cdecl;
  end;
  TRPBroadcastMP4ClipHandler = class(TOCGenericImport<RPBroadcastMP4ClipHandlerClass, RPBroadcastMP4ClipHandler>) end;

  RPBroadcastSampleHandlerClass = interface(RPBroadcastHandlerClass)
    ['{CB206599-1B38-447F-AF44-D171164367A4}']
  end;

  RPBroadcastSampleHandler = interface(RPBroadcastHandler)
    ['{D72D91B2-7999-4CB9-ACD3-233018FAA163}']
    procedure broadcastAnnotatedWithApplicationInfo(applicationInfo: NSDictionary); cdecl;
    procedure broadcastFinished; cdecl;
    procedure broadcastPaused; cdecl;
    procedure broadcastResumed; cdecl;
    procedure broadcastStartedWithSetupInfo(setupInfo: NSDictionary); cdecl;
    procedure finishBroadcastWithError(error: NSError); cdecl;
    procedure processSampleBuffer(sampleBuffer: CMSampleBufferRef; withType: RPSampleBufferType); cdecl;
  end;
  TRPBroadcastSampleHandler = class(TOCGenericImport<RPBroadcastSampleHandlerClass, RPBroadcastSampleHandler>) end;

  RPScreenRecorderClass = interface(NSObjectClass)
    ['{DF91A60C-D72F-4472-AD4B-EF7DAF8B8738}']
    {class} function sharedRecorder: RPScreenRecorder; cdecl;
  end;

  RPScreenRecorder = interface(NSObject)
    ['{E858723F-A241-4C0B-85A8-D890341318E1}']
    function cameraPosition: RPCameraPosition; cdecl;
    function cameraPreviewView: UIView; cdecl;
    function delegate: Pointer; cdecl;
    procedure discardRecordingWithHandler(handler: TRPScreenRecorderBlockMethod3); cdecl;
    procedure exportClipToURL(url: NSURL; duration: NSTimeInterval; completionHandler: TRPScreenRecorderBlockMethod1); cdecl;
    function isAvailable: Boolean; cdecl;
    function isCameraEnabled: Boolean; cdecl;
    function isMicrophoneEnabled: Boolean; cdecl;
    function isRecording: Boolean; cdecl;
    procedure setCameraEnabled(cameraEnabled: Boolean); cdecl;
    procedure setCameraPosition(cameraPosition: RPCameraPosition); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setMicrophoneEnabled(microphoneEnabled: Boolean); cdecl;
    procedure startCaptureWithHandler(captureHandler: TRPScreenRecorderBlockMethod4; completionHandler: TRPScreenRecorderBlockMethod1); cdecl;
    procedure startClipBufferingWithCompletionHandler(completionHandler: TRPScreenRecorderBlockMethod1); cdecl;
    procedure startRecordingWithHandler(handler: TRPScreenRecorderBlockMethod1); cdecl;
    procedure startRecordingWithMicrophoneEnabled(microphoneEnabled: Boolean; handler: TRPScreenRecorderBlockMethod1); cdecl; // API_DEPRECATED("Use microphoneEnabled property", ios(9.0, 10.0))
    procedure stopCaptureWithHandler(handler: TRPScreenRecorderBlockMethod1); cdecl;
    procedure stopClipBufferingWithCompletionHandler(completionHandler: TRPScreenRecorderBlockMethod1); cdecl;
    procedure stopRecordingWithHandler(handler: TRPScreenRecorderBlockMethod2); cdecl;
    procedure stopRecordingWithOutputURL(url: NSURL; completionHandler: TRPScreenRecorderBlockMethod1); cdecl;
  end;
  TRPScreenRecorder = class(TOCGenericImport<RPScreenRecorderClass, RPScreenRecorder>) end;

  RPScreenRecorderDelegate = interface(IObjectiveC)
    ['{7483EAF9-4FCB-4DD5-BDC3-DFFF8F354028}']
    procedure screenRecorder(screenRecorder: RPScreenRecorder; didStopRecordingWithPreviewViewController: RPPreviewViewController;
      error: NSError); overload; cdecl;
    procedure screenRecorder(screenRecorder: RPScreenRecorder; didStopRecordingWithError: NSError;
      previewViewController: RPPreviewViewController); overload; cdecl; // API_DEPRECATED("No longer supported", ios(9.0, 10.0), tvos(10.0, 10.0))
    procedure screenRecorderDidChangeAvailability(screenRecorder: RPScreenRecorder); cdecl;
  end;

  RPBroadcastActivityViewControllerClass = interface(UIViewControllerClass)
    ['{7E322DC9-673B-4167-B4CB-EA37AADEFBEC}']
    {class} procedure loadBroadcastActivityViewControllerWithHandler(handler: TRPBroadcastActivityViewControllerBlockMethod1); cdecl;
    {class} procedure loadBroadcastActivityViewControllerWithPreferredExtension(preferredExtension: NSString;
      handler: TRPBroadcastActivityViewControllerBlockMethod1); cdecl;
  end;

  RPBroadcastActivityViewController = interface(UIViewController)
    ['{2AD5CAF9-EFBB-4CF7-8579-764FCF248EB3}']
    function delegate: Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TRPBroadcastActivityViewController = class(TOCGenericImport<RPBroadcastActivityViewControllerClass, RPBroadcastActivityViewController>) end;

  RPBroadcastActivityViewControllerDelegate = interface(IObjectiveC)
    ['{2AD1099E-258D-44A3-B8A3-9A1529D3080C}']
    procedure broadcastActivityViewController(broadcastActivityViewController: RPBroadcastActivityViewController;
      didFinishWithBroadcastController: RPBroadcastController; error: NSError); cdecl;
  end;

  RPBroadcastControllerClass = interface(NSObjectClass)
    ['{776C0EB3-52B1-40D2-858F-82AD4099FB5E}']
  end;

  RPBroadcastController = interface(NSObject)
    ['{BEA79627-CC1A-48D7-9512-0DE17D7E56E6}']
    function broadcastExtensionBundleID: NSString; cdecl; // API_DEPRECATED("No longer supported", ios(10.0, 11.0), tvos(10.0, 11.0))
    function broadcastURL: NSURL; cdecl;
    function delegate: Pointer; cdecl;
    procedure finishBroadcastWithHandler(handler: TRPBroadcastControllerBlockMethod1); cdecl;
    function isBroadcasting: Boolean; cdecl;
    function isPaused: Boolean; cdecl;
    procedure pauseBroadcast; cdecl;
    procedure resumeBroadcast; cdecl;
    function serviceInfo: NSDictionary; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure startBroadcastWithHandler(handler: TRPBroadcastControllerBlockMethod1); cdecl;
  end;
  TRPBroadcastController = class(TOCGenericImport<RPBroadcastControllerClass, RPBroadcastController>) end;

  RPBroadcastControllerDelegate = interface(IObjectiveC)
    ['{D447E9D9-64FB-4C20-91FF-4C448EBAD4A1}']
    procedure broadcastController(broadcastController: RPBroadcastController; didUpdateBroadcastURL: NSURL); overload; cdecl;
    procedure broadcastController(broadcastController: RPBroadcastController; didUpdateServiceInfo: NSDictionary); overload; cdecl;
    procedure broadcastController(broadcastController: RPBroadcastController; didFinishWithError: NSError); overload; cdecl;
  end;

  RPSystemBroadcastPickerViewClass = interface(UIViewClass)
    ['{2E82D684-A087-4A74-B84B-EBBF8BA071DF}']
  end;

  RPSystemBroadcastPickerView = interface(UIView)
    ['{224E0DC6-425E-4B98-817A-90AF152211F3}']
    function preferredExtension: NSString; cdecl;
    procedure setPreferredExtension(preferredExtension: NSString); cdecl;
    procedure setShowsMicrophoneButton(showsMicrophoneButton: Boolean); cdecl;
    function showsMicrophoneButton: Boolean; cdecl;
  end;
  TRPSystemBroadcastPickerView = class(TOCGenericImport<RPSystemBroadcastPickerViewClass, RPSystemBroadcastPickerView>) end;

function RPVideoSampleOrientationKey: NSString;
function RPApplicationInfoBundleIdentifierKey: NSString;
function RPRecordingErrorDomain: NSString;
function SCStreamErrorDomain: NSString;

const
  libReplayKit = '/System/Library/Frameworks/ReplayKit.framework/ReplayKit';

implementation

uses
  Posix.Dlfcn;

var
  ReplayKitModule: THandle;

function RPVideoSampleOrientationKey: NSString;
begin
  Result := CocoaNSStringConst(libReplayKit, 'RPVideoSampleOrientationKey');
end;

function RPApplicationInfoBundleIdentifierKey: NSString;
begin
  Result := CocoaNSStringConst(libReplayKit, 'RPApplicationInfoBundleIdentifierKey');
end;

function RPRecordingErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libReplayKit, 'RPRecordingErrorDomain');
end;

function SCStreamErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libReplayKit, 'SCStreamErrorDomain');
end;

initialization
  ReplayKitModule := dlopen(MarshaledAString(libReplayKit), RTLD_LAZY);

finalization
  dlclose(ReplayKitModule);

end.