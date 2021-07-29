unit DW.iOSapi.PhotosUI;

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
  // macOS
  Macapi.ObjectiveC,
  // iOS
  iOSapi.Foundation, iOSapi.CocoaTypes, iOSapi.UIKit,
  // DW
  DW.iOSapi.Foundation, DW.iOSapi.Photos;

const
  PHLivePhotoBadgeOptionsOverContent = 1;
  PHLivePhotoBadgeOptionsLiveOff = 2;
  PHLivePhotoViewPlaybackStyleUndefined = 0;
  PHLivePhotoViewPlaybackStyleFull = 1;
  PHLivePhotoViewPlaybackStyleHint = 2;
  PHPickerConfigurationAssetRepresentationModeAutomatic = 0;
  PHPickerConfigurationAssetRepresentationModeCurrent = 1;
  PHPickerConfigurationAssetRepresentationModeCompatible = 2;

type
  PHLivePhotoView = interface;
  PHLivePhotoViewDelegate = interface;
  PHContentEditingController = interface;
  PHEditingExtensionContext = interface;
  PHPickerFilter = interface;
  PHPickerConfiguration = interface;
  PHPickerResult = interface;
  PHPickerViewControllerDelegate = interface;
  PHPickerViewController = interface;

  PHLivePhotoBadgeOptions = NSInteger;
  PHLivePhotoViewPlaybackStyle = NSInteger;
  PHPickerConfigurationAssetRepresentationMode = NSInteger;
  TPHContentEditingControllerBlockMethod1 = procedure(param1: PHContentEditingOutput) of object;

  PHLivePhotoViewClass = interface(UIViewClass)
    ['{77912A60-B662-41B6-A17A-796EFB79CCDA}']
    {class} function livePhotoBadgeImageWithOptions(badgeOptions: PHLivePhotoBadgeOptions): UIImage; cdecl;
  end;

  PHLivePhotoView = interface(UIView)
    ['{AED013F6-5FFF-4DAC-A273-74CCF154F9E5}']
    function delegate: Pointer; cdecl;
    function isMuted: Boolean; cdecl;
    function livePhoto: PHLivePhoto; cdecl;
    function playbackGestureRecognizer: UIGestureRecognizer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setLivePhoto(livePhoto: PHLivePhoto); cdecl;
    procedure setMuted(muted: Boolean); cdecl;
    procedure startPlaybackWithStyle(playbackStyle: PHLivePhotoViewPlaybackStyle); cdecl;
    procedure stopPlayback; cdecl;
  end;
  TPHLivePhotoView = class(TOCGenericImport<PHLivePhotoViewClass, PHLivePhotoView>) end;

  PHLivePhotoViewDelegate = interface(IObjectiveC)
    ['{64E89BBA-4279-42E2-981F-95F36DE75543}']
    [MethodName('livePhotoView:didEndPlaybackWithStyle:')]
    procedure livePhotoViewDidEndPlaybackWithStyle(livePhotoView: PHLivePhotoView; didEndPlaybackWithStyle: PHLivePhotoViewPlaybackStyle); cdecl;
    [MethodName('livePhotoView:willBeginPlaybackWithStyle:')]
    procedure livePhotoViewWillBeginPlaybackWithStyle(livePhotoView: PHLivePhotoView; willBeginPlaybackWithStyle: PHLivePhotoViewPlaybackStyle); cdecl;
  end;

  PHContentEditingController = interface(IObjectiveC)
    ['{8B60F43E-7CBF-47E0-8F68-58D98BECF7FD}']
    procedure cancelContentEditing; cdecl;
    function canHandleAdjustmentData(adjustmentData: PHAdjustmentData): Boolean; cdecl;
    procedure finishContentEditingWithCompletionHandler(completionHandler: TPHContentEditingControllerBlockMethod1); cdecl;
    function shouldShowCancelConfirmation: Boolean; cdecl;
    procedure startContentEditingWithInput(contentEditingInput: PHContentEditingInput; placeholderImage: UIImage); cdecl;
  end;

  PHEditingExtensionContextClass = interface(NSExtensionContextClass)
    ['{D037F3DD-7092-4721-A47C-3DECCB2036FC}']
  end;

  PHEditingExtensionContext = interface(NSExtensionContext)
    ['{EABF4E41-D6F6-412F-8C5A-ABAD6274529D}']
  end;
  TPHEditingExtensionContext = class(TOCGenericImport<PHEditingExtensionContextClass, PHEditingExtensionContext>) end;

  PHPickerFilterClass = interface(NSObjectClass)
    ['{9DD3F2E5-1356-4532-ABED-7AA4840FB935}']
    {class} function anyFilterMatchingSubfilters(subfilters: NSArray): PHPickerFilter; cdecl;
    {class} function imagesFilter: PHPickerFilter; cdecl;
    {class} function livePhotosFilter: PHPickerFilter; cdecl;
    {class} function new: Pointer; cdecl;
    {class} function videosFilter: PHPickerFilter; cdecl;
  end;

  PHPickerFilter = interface(NSObject)
    ['{5297FE3E-73C1-42F0-B0C2-E37C0B3E7B54}']
  end;
  TPHPickerFilter = class(TOCGenericImport<PHPickerFilterClass, PHPickerFilter>) end;

  PHPickerConfigurationClass = interface(NSObjectClass)
    ['{371FDE44-96DA-489A-9D57-E9206864F071}']
  end;

  PHPickerConfiguration = interface(NSObject)
    ['{84D4EC11-7860-43BA-B1B6-FDD678CC42C6}']
    function filter: PHPickerFilter; cdecl;
    function initWithPhotoLibrary(photoLibrary: PHPhotoLibrary): Pointer; cdecl;
    function preferredAssetRepresentationMode: PHPickerConfigurationAssetRepresentationMode; cdecl;
    function selectionLimit: NSInteger; cdecl;
    procedure setFilter(filter: PHPickerFilter); cdecl;
    procedure setPreferredAssetRepresentationMode(preferredAssetRepresentationMode: PHPickerConfigurationAssetRepresentationMode); cdecl;
    procedure setSelectionLimit(selectionLimit: NSInteger); cdecl;
  end;
  TPHPickerConfiguration = class(TOCGenericImport<PHPickerConfigurationClass, PHPickerConfiguration>) end;

  PHPickerResultClass = interface(NSObjectClass)
    ['{4EF3036F-99C2-4404-BDCF-F42CB8EFEA38}']
    {class} function new: Pointer; cdecl;
  end;

  PHPickerResult = interface(NSObject)
    ['{D74A1856-CB03-494E-BD2A-D75ED1DAAC92}']
    function assetIdentifier: NSString; cdecl;
    function itemProvider: NSItemProvider; cdecl;
  end;
  TPHPickerResult = class(TOCGenericImport<PHPickerResultClass, PHPickerResult>) end;

  PHPickerViewControllerDelegate = interface(IObjectiveC)
    ['{CEBF64C5-6311-4202-B379-D9C1A3B4B27A}']
    procedure picker(picker: PHPickerViewController; didFinishPicking: NSArray); cdecl;
  end;

  PHPickerViewControllerClass = interface(UIViewControllerClass)
    ['{1A0EF6C2-C76D-49FA-98DE-BB3BC8FD4B6C}']
    {class} function new: Pointer; cdecl;
  end;

  PHPickerViewController = interface(UIViewController)
    ['{4813696E-97D3-4D0B-B72D-91F493F08FDE}']
    function configuration: PHPickerConfiguration; cdecl;
    function delegate: Pointer; cdecl;
    function initWithCoder(coder: NSCoder): Pointer; cdecl;
    function initWithConfiguration(configuration: PHPickerConfiguration): Pointer; cdecl;
    function initWithNibName(nibNameOrNil: NSString; bundle: NSBundle): Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TPHPickerViewController = class(TOCGenericImport<PHPickerViewControllerClass, PHPickerViewController>) end;

const
  libPhotosUI = '/System/Library/Frameworks/PhotosUI.framework/PhotosUI';

implementation

uses
  Posix.Dlfcn;

var
  PhotosUIModule: THandle;

initialization
  PhotosUIModule := dlopen(MarshaledAString(libPhotosUI), RTLD_LAZY);

finalization
  dlclose(PhotosUIModule);

end.