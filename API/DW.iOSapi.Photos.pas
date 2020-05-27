unit DW.iOSapi.Photos;

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

interface

uses
  // macOS
  Macapi.ObjectiveC,
  // iOS
  iOSapi.Foundation, iOSapi.CocoaTypes, iOSapi.CoreLocation, iOSapi.CoreGraphics, iOSapi.UIKit, iOSapi.AVFoundation, iOSapi.CoreImage, iOSapi.CoreMedia,
  // DW
  DW.Macapi.Dispatch;

const
  PHImageContentModeAspectFit = 0;
  PHImageContentModeAspectFill = 1;
  PHImageContentModeDefault = PHImageContentModeAspectFit;
  PHCollectionListTypeMomentList = 1;
  PHCollectionListTypeFolder = 2;
  PHCollectionListTypeSmartFolder = 3;
  PHCollectionListSubtypeMomentListCluster = 1;
  PHCollectionListSubtypeMomentListYear = 2;
  PHCollectionListSubtypeRegularFolder = 100;
  PHCollectionListSubtypeSmartFolderEvents = 200;
  PHCollectionListSubtypeSmartFolderFaces = 201;
  PHCollectionListSubtypeAny = 2147483647;
  PHCollectionEditOperationDeleteContent = 1;
  PHCollectionEditOperationRemoveContent = 2;
  PHCollectionEditOperationAddContent = 3;
  PHCollectionEditOperationCreateContent = 4;
  PHCollectionEditOperationRearrangeContent = 5;
  PHCollectionEditOperationDelete = 6;
  PHCollectionEditOperationRename = 7;
  PHAssetCollectionTypeAlbum = 1;
  PHAssetCollectionTypeSmartAlbum = 2;
  PHAssetCollectionTypeMoment = 3;
  PHAssetCollectionSubtypeAlbumRegular = 2;
  PHAssetCollectionSubtypeAlbumSyncedEvent = 3;
  PHAssetCollectionSubtypeAlbumSyncedFaces = 4;
  PHAssetCollectionSubtypeAlbumSyncedAlbum = 5;
  PHAssetCollectionSubtypeAlbumImported = 6;
  PHAssetCollectionSubtypeAlbumMyPhotoStream = 100;
  PHAssetCollectionSubtypeAlbumCloudShared = 101;
  PHAssetCollectionSubtypeSmartAlbumGeneric = 200;
  PHAssetCollectionSubtypeSmartAlbumPanoramas = 201;
  PHAssetCollectionSubtypeSmartAlbumVideos = 202;
  PHAssetCollectionSubtypeSmartAlbumFavorites = 203;
  PHAssetCollectionSubtypeSmartAlbumTimelapses = 204;
  PHAssetCollectionSubtypeSmartAlbumAllHidden = 205;
  PHAssetCollectionSubtypeSmartAlbumRecentlyAdded = 206;
  PHAssetCollectionSubtypeSmartAlbumBursts = 207;
  PHAssetCollectionSubtypeSmartAlbumSlomoVideos = 208;
  PHAssetCollectionSubtypeSmartAlbumUserLibrary = 209;
  PHAssetCollectionSubtypeSmartAlbumSelfPortraits = 210;
  PHAssetCollectionSubtypeSmartAlbumScreenshots = 211;
  PHAssetCollectionSubtypeSmartAlbumDepthEffect = 212;
  PHAssetCollectionSubtypeSmartAlbumLivePhotos = 213;
  PHAssetCollectionSubtypeSmartAlbumAnimated = 214;
  PHAssetCollectionSubtypeSmartAlbumLongExposures = 215;
  PHAssetCollectionSubtypeSmartAlbumUnableToUpload = 216;
  PHAssetCollectionSubtypeAny = 2147483647;
  PHAssetEditOperationDelete = 1;
  PHAssetEditOperationContent = 2;
  PHAssetEditOperationProperties = 3;
  PHAssetPlaybackStyleUnsupported = 0;
  PHAssetPlaybackStyleImage = 1;
  PHAssetPlaybackStyleImageAnimated = 2;
  PHAssetPlaybackStyleLivePhoto = 3;
  PHAssetPlaybackStyleVideo = 4;
  PHAssetPlaybackStyleVideoLooping = 5;
  PHAssetMediaTypeUnknown = 0;
  PHAssetMediaTypeImage = 1;
  PHAssetMediaTypeVideo = 2;
  PHAssetMediaTypeAudio = 3;
  PHAssetMediaSubtypeNone = 0;
  PHAssetMediaSubtypePhotoPanorama = 1;
  PHAssetMediaSubtypePhotoHDR = 2;
  PHAssetMediaSubtypePhotoScreenshot = 4;
  PHAssetMediaSubtypePhotoLive = 8;
  PHAssetMediaSubtypePhotoDepthEffect = 16;
  PHAssetMediaSubtypeVideoStreamed = 65536;
  PHAssetMediaSubtypeVideoHighFrameRate = 131072;
  PHAssetMediaSubtypeVideoTimelapse = 262144;
  PHAssetBurstSelectionTypeNone = 0;
  PHAssetBurstSelectionTypeAutoPick = 1;
  PHAssetBurstSelectionTypeUserPick = 2;
  PHAssetSourceTypeNone = 0;
  PHAssetSourceTypeUserLibrary = 1;
  PHAssetSourceTypeCloudShared = 2;
  PHAssetSourceTypeiTunesSynced = 4;
  PHAssetResourceTypePhoto = 1;
  PHAssetResourceTypeVideo = 2;
  PHAssetResourceTypeAudio = 3;
  PHAssetResourceTypeAlternatePhoto = 4;
  PHAssetResourceTypeFullSizePhoto = 5;
  PHAssetResourceTypeFullSizeVideo = 6;
  PHAssetResourceTypeAdjustmentData = 7;
  PHAssetResourceTypeAdjustmentBasePhoto = 8;
  PHAssetResourceTypePairedVideo = 9;
  PHAssetResourceTypeFullSizePairedVideo = 10;
  PHAssetResourceTypeAdjustmentBasePairedVideo = 11;
  PHAssetResourceTypeAdjustmentBaseVideo = 12;
  PHAuthorizationStatusNotDetermined = 0;
  PHAuthorizationStatusRestricted = 1;
  PHAuthorizationStatusDenied = 2;
  PHAuthorizationStatusAuthorized = 3;
  PHPhotosErrorInvalid = -1;
  PHPhotosErrorUserCancelled = 3072;
  PHPhotosErrorLibraryVolumeOffline = 3114;
  PHPhotosErrorRelinquishingLibraryBundleToWriter = 3142;
  PHPhotosErrorSwitchingSystemPhotoLibrary = 3143;
  PHImageRequestOptionsVersionCurrent = 0;
  PHImageRequestOptionsVersionUnadjusted = 1;
  PHImageRequestOptionsVersionOriginal = 2;
  PHImageRequestOptionsDeliveryModeOpportunistic = 0;
  PHImageRequestOptionsDeliveryModeHighQualityFormat = 1;
  PHImageRequestOptionsDeliveryModeFastFormat = 2;
  PHImageRequestOptionsResizeModeNone = 0;
  PHImageRequestOptionsResizeModeFast = 1;
  PHImageRequestOptionsResizeModeExact = 2;
  PHVideoRequestOptionsVersionCurrent = 0;
  PHVideoRequestOptionsVersionOriginal = 1;
  PHVideoRequestOptionsDeliveryModeAutomatic = 0;
  PHVideoRequestOptionsDeliveryModeHighQualityFormat = 1;
  PHVideoRequestOptionsDeliveryModeMediumQualityFormat = 2;
  PHVideoRequestOptionsDeliveryModeFastFormat = 3;
  PHLivePhotoFrameTypePhoto = 0;
  PHLivePhotoFrameTypeVideo = 1;
  PHLivePhotoEditingErrorCodeUnknown = 0;
  PHLivePhotoEditingErrorCodeAborted = 1;

type
  PHAdjustmentData = interface;
  PHObject = interface;
  PHObjectPlaceholder = interface;
  PHFetchResult = interface;
  PHPhotoLibraryChangeObserver = interface;
  PHPhotoLibraryAvailabilityObserver = interface;
  PHPhotoLibrary = interface;
  PHAsset = interface;
  PHChangeRequest = interface;
  PHContentEditingOutput = interface;
  PHAssetChangeRequest = interface;
  PHContentEditingInputRequestOptions = interface;
  PHAssetCollectionChangeRequest = interface;
  PHAssetResourceCreationOptions = interface;
  PHAssetCreationRequest = interface;
  PHAssetResource = interface;
  PHAssetResourceRequestOptions = interface;
  PHAssetResourceManager = interface;
  PHChange = interface;
  PHObjectChangeDetails = interface;
  PHFetchResultChangeDetails = interface;
  PHCloudIdentifier = interface;
  PHCollection = interface;
  PHAssetCollection = interface;
  PHCollectionList = interface;
  PHCollectionListChangeRequest = interface;
  PHContentEditingInput = interface;
  PHFetchOptions = interface;
  PHImageRequestOptions = interface;
  PHLivePhotoRequestOptions = interface;
  PHVideoRequestOptions = interface;
  PHImageManager = interface;
  PHCachingImageManager = interface;
  PHLivePhoto = interface;
  PHLivePhotoEditingContext = interface;
  PHLivePhotoFrame = interface;
  PHProject = interface;
  PHProjectChangeRequest = interface;

  CGImagePropertyOrientation = NSUInteger;
  NSEnumerationOptions = NSUInteger;
  NSErrorDomain = NSString;

  PHImageContentMode = NSInteger;
  PHCollectionListType = NSInteger;
  PHCollectionListSubtype = NSInteger;
  PHCollectionEditOperation = NSInteger;
  PHAssetCollectionType = NSInteger;
  PHAssetCollectionSubtype = NSInteger;
  PHAssetEditOperation = NSInteger;
  PHAssetPlaybackStyle = NSInteger;
  PHAssetMediaType = NSInteger;
  PHAssetMediaSubtype = NSInteger;
  PHAssetBurstSelectionType = NSInteger;
  PHAssetSourceType = NSInteger;
  PHAssetResourceType = NSInteger;
  PHAuthorizationStatus = NSInteger;
  PHContentEditingInputRequestID = NSUInteger;
  PHAssetResourceDataRequestID = Int32;

  PHAssetResourceProgressHandler = procedure(progress: Double) of object;
  UIImageOrientation = NSInteger;
  PHImageRequestOptionsVersion = NSInteger;
  PHImageRequestOptionsDeliveryMode = NSInteger;
  PHImageRequestOptionsResizeMode = NSInteger;

  PHAssetImageProgressHandler = procedure(progress: Double; error: NSError; stop: PBoolean; info: NSDictionary) of object;
  PHVideoRequestOptionsVersion = NSInteger;
  PHVideoRequestOptionsDeliveryMode = NSInteger;

  PHAssetVideoProgressHandler = procedure(progress: Double; error: NSError; stop: PBoolean; info: NSDictionary) of object;
  PHImageRequestID = Int32;
  PHLivePhotoRequestID = Int32;

  PHLivePhotoFrameProcessingBlock = function(frame: Pointer; error: PPointer): CIImage of object;
  PHLivePhotoEditingOption = NSString;
  PHLivePhotoFrameType = NSInteger;
  PHLivePhotoEditingErrorCode = NSInteger;
  TPHFetchResultBlockMethod1 = procedure(obj: Pointer; idx: NSUInteger; stop: PBoolean) of object;
  TPHPhotoLibraryBlockMethod1 = procedure(status: PHAuthorizationStatus) of object;
  TPHPhotoLibraryBlockMethod2 = procedure(success: Boolean; error: NSError) of object;
  TPHContentEditingInputRequestOptionsBlockMethod1 = function(param1: PHAdjustmentData): Boolean of object;
  TPHContentEditingInputRequestOptionsBlockMethod2 = procedure() of object;
  TPHContentEditingInputRequestOptionsBlockMethod3 = procedure(param1: Double; param2: PBoolean) of object;
  TPHAssetBlockMethod1 = procedure(contentEditingInput: PHContentEditingInput; info: NSDictionary) of object;
  TPHAssetResourceManagerBlockMethod1 = procedure(data: NSData) of object;
  TPHAssetResourceManagerBlockMethod2 = procedure(error: NSError) of object;
  TPHFetchResultChangeDetailsBlockMethod1 = procedure(fromIndex: NSUInteger; toIndex: NSUInteger) of object;
  TPHImageManagerBlockMethod1 = procedure(result: UIImage; info: NSDictionary) of object;
  TPHImageManagerBlockMethod2 = procedure(imageData: NSData; dataUTI: NSString; orientation: UIImageOrientation; info: NSDictionary) of object;
  TPHImageManagerBlockMethod3 = procedure(imageData: NSData; dataUTI: NSString; orientation: CGImagePropertyOrientation; info: NSDictionary) of object;
  TPHImageManagerBlockMethod4 = procedure(livePhoto: PHLivePhoto; info: NSDictionary) of object;
  TPHImageManagerBlockMethod5 = procedure(playerItem: AVPlayerItem; info: NSDictionary) of object;
  TPHImageManagerBlockMethod6 = procedure(exportSession: AVAssetExportSession; info: NSDictionary) of object;
  TPHImageManagerBlockMethod7 = procedure(asset: AVAsset; audioMix: AVAudioMix; info: NSDictionary) of object;
  TPHLivePhotoBlockMethod1 = procedure(livePhoto: PHLivePhoto; info: NSDictionary) of object;
  TPHLivePhotoEditingContextBlockMethod1 = procedure(livePhoto: PHLivePhoto; error: NSError) of object;
  TPHLivePhotoEditingContextBlockMethod2 = procedure(success: Boolean; error: NSError) of object;

  PHAdjustmentDataClass = interface(NSObjectClass)
    ['{C055D876-C245-4AE4-A5B0-AFCC379160B6}']
  end;

  PHAdjustmentData = interface(NSObject)
    ['{F5778BEA-C31C-4791-9B34-AA1E341E7432}']
    function data: NSData; cdecl;
    function formatIdentifier: NSString; cdecl;
    function formatVersion: NSString; cdecl;
    [MethodName('initWithFormatIdentifier:formatVersion:data:')]
    function initWithFormatIdentifier(formatIdentifier: NSString; formatVersion: NSString; data: NSData): Pointer; cdecl;
  end;
  TPHAdjustmentData = class(TOCGenericImport<PHAdjustmentDataClass, PHAdjustmentData>) end;

  PHObjectClass = interface(NSObjectClass)
    ['{6B06DA2A-3F1F-4C29-9B7F-B2BD09F98837}']
  end;

  PHObject = interface(NSObject)
    ['{AC178D74-B24C-47F1-BED8-78ED4EA513FB}']
    function localIdentifier: NSString; cdecl;
  end;
  TPHObject = class(TOCGenericImport<PHObjectClass, PHObject>) end;

  PHObjectPlaceholderClass = interface(PHObjectClass)
    ['{E6FF2315-C9FA-4995-BCDA-7E5C92ADC81A}']
  end;

  PHObjectPlaceholder = interface(PHObject)
    ['{7870828A-1A4F-4475-B25F-2F1EF289FF18}']
  end;
  TPHObjectPlaceholder = class(TOCGenericImport<PHObjectPlaceholderClass, PHObjectPlaceholder>) end;

  PHFetchResultClass = interface(NSObjectClass)
    ['{83BAAECA-B652-489D-AC7C-4A9BE43886E9}']
  end;

  PHFetchResult = interface(NSObject)
    ['{D97178FB-7A95-4A1F-95FC-46788BBEF15D}']
    function containsObject(anObject: Pointer): Boolean; cdecl;
    function count: NSUInteger; cdecl;
    function countOfAssetsWithMediaType(mediaType: PHAssetMediaType): NSUInteger; cdecl;
    [MethodName('enumerateObjectsAtIndexes:options:usingBlock:')]
    procedure enumerateObjectsAtIndexes(s: NSIndexSet; opts: NSEnumerationOptions; block: TPHFetchResultBlockMethod1); cdecl;
    procedure enumerateObjectsUsingBlock(block: TPHFetchResultBlockMethod1); cdecl;
    [MethodName('enumerateObjectsWithOptions:usingBlock:')]
    procedure enumerateObjectsWithOptions(opts: NSEnumerationOptions; block: TPHFetchResultBlockMethod1); cdecl;
    function firstObject: Pointer; cdecl;
    function indexOfObject(anObject: Pointer): NSUInteger; overload; cdecl;
    [MethodName('indexOfObject:inRange:')]
    function indexOfObject(anObject: Pointer; range: NSRange): NSUInteger; overload; cdecl;
    function lastObject: Pointer; cdecl;
    function objectAtIndex(index: NSUInteger): Pointer; cdecl;
    function objectAtIndexedSubscript(idx: NSUInteger): Pointer; cdecl;
    function objectsAtIndexes(indexes: NSIndexSet): NSArray; cdecl;
  end;
  TPHFetchResult = class(TOCGenericImport<PHFetchResultClass, PHFetchResult>) end;

  PHPhotoLibraryChangeObserver = interface(IObjectiveC)
    ['{8B2F3EB0-0BF2-4062-B909-DF2C597FDE29}']
    procedure photoLibraryDidChange(changeInstance: PHChange); cdecl;
  end;

  PHPhotoLibraryAvailabilityObserver = interface(IObjectiveC)
    ['{82652A82-F90F-44DA-BE32-70F9E7AC4E5D}']
    procedure photoLibraryDidBecomeUnavailable(photoLibrary: PHPhotoLibrary); cdecl;
  end;

  PHPhotoLibraryClass = interface(NSObjectClass)
    ['{BF906226-BEE8-445C-A90C-EC110DFE02C9}']
    {class} function authorizationStatus: PHAuthorizationStatus; cdecl;
    {class} procedure requestAuthorization(handler: TPHPhotoLibraryBlockMethod1); cdecl;
    {class} function sharedPhotoLibrary: PHPhotoLibrary; cdecl;
  end;

  PHPhotoLibrary = interface(NSObject)
    ['{F3520244-8ADF-445C-B9C1-FC97448C0F8D}']
    function cloudIdentifiersForLocalIdentifiers(localIdentifiers: NSArray): NSArray; cdecl;
    function localIdentifiersForCloudIdentifiers(cloudIdentifiers: NSArray): NSArray; cdecl;
    [MethodName('performChanges:completionHandler:')]
    procedure performChanges(changeBlock: dispatch_block_t; completionHandler: TPHPhotoLibraryBlockMethod2); cdecl;
    [MethodName('performChangesAndWait:error:')]
    function performChangesAndWait(changeBlock: dispatch_block_t; error: PPointer): Boolean; cdecl;
    procedure registerAvailabilityObserver(observer: Pointer); cdecl;
    procedure registerChangeObserver(observer: Pointer); cdecl;
    function unavailabilityReason: NSError; cdecl;
    procedure unregisterAvailabilityObserver(observer: Pointer); cdecl;
    procedure unregisterChangeObserver(observer: Pointer); cdecl;
  end;
  TPHPhotoLibrary = class(TOCGenericImport<PHPhotoLibraryClass, PHPhotoLibrary>) end;

  PHAssetClass = interface(PHObjectClass)
    ['{E32C4D34-E3FB-40C5-9E21-9441F4028111}']
    [MethodName('fetchAssetsInAssetCollection:options:')]
    {class} function fetchAssetsInAssetCollection(assetCollection: PHAssetCollection; options: PHFetchOptions): PHFetchResult; cdecl;
    [MethodName('fetchAssetsWithALAssetURLs:options:')]
    {class} function fetchAssetsWithALAssetURLs(assetURLs: NSArray; options: PHFetchOptions): PHFetchResult; cdecl; // API_DEPRECATED("Will be removed in a future release", ios(8, 11), tvos(8, 11))
    [MethodName('fetchAssetsWithBurstIdentifier:options:')]
    {class} function fetchAssetsWithBurstIdentifier(burstIdentifier: NSString; options: PHFetchOptions): PHFetchResult; cdecl;
    [MethodName('fetchAssetsWithLocalIdentifiers:options:')]
    {class} function fetchAssetsWithLocalIdentifiers(identifiers: NSArray; options: PHFetchOptions): PHFetchResult; cdecl;
    [MethodName('fetchAssetsWithMediaType:options:')]
    {class} function fetchAssetsWithMediaType(mediaType: PHAssetMediaType; options: PHFetchOptions): PHFetchResult; cdecl;
    {class} function fetchAssetsWithOptions(options: PHFetchOptions): PHFetchResult; cdecl;
    [MethodName('fetchKeyAssetsInAssetCollection:options:')]
    {class} function fetchKeyAssetsInAssetCollection(assetCollection: PHAssetCollection; options: PHFetchOptions): PHFetchResult; cdecl;
  end;

  PHAsset = interface(PHObject)
    ['{DAC9EBAD-F7AF-4F51-B650-F8608C6D13F5}']
    function burstIdentifier: NSString; cdecl;
    function burstSelectionTypes: PHAssetBurstSelectionType; cdecl;
    procedure cancelContentEditingInputRequest(requestID: PHContentEditingInputRequestID); cdecl;
    function canPerformEditOperation(editOperation: PHAssetEditOperation): Boolean; cdecl;
    function creationDate: NSDate; cdecl;
    function duration: NSTimeInterval; cdecl;
    function isFavorite: Boolean; cdecl;
    function isHidden: Boolean; cdecl;
    function isSyncFailureHidden: Boolean; cdecl; // API_DEPRECATED("No longer supported", macos(10.14, 10.15))
    function location: CLLocation; cdecl;
    function mediaSubtypes: PHAssetMediaSubtype; cdecl;
    function mediaType: PHAssetMediaType; cdecl;
    function modificationDate: NSDate; cdecl;
    function pixelHeight: NSUInteger; cdecl;
    function pixelWidth: NSUInteger; cdecl;
    function playbackStyle: PHAssetPlaybackStyle; cdecl;
    function representsBurst: Boolean; cdecl;
    [MethodName('requestContentEditingInputWithOptions:completionHandler:')]
    function requestContentEditingInputWithOptions(options: PHContentEditingInputRequestOptions;
      completionHandler: TPHAssetBlockMethod1): PHContentEditingInputRequestID; cdecl;
    function sourceType: PHAssetSourceType; cdecl;
    function valueForKey(key: NSString): Pointer; cdecl; // Remove from here once it is added to NSObject
  end;
  TPHAsset = class(TOCGenericImport<PHAssetClass, PHAsset>) end;

  PHChangeRequestClass = interface(NSObjectClass)
    ['{46FCC1E5-6478-4A62-B531-E85B073915F2}']
  end;

  PHChangeRequest = interface(NSObject)
    ['{C46CD8BC-0792-4F09-A0D9-47F843781912}']
  end;
  TPHChangeRequest = class(TOCGenericImport<PHChangeRequestClass, PHChangeRequest>) end;

  PHContentEditingOutputClass = interface(NSObjectClass)
    ['{1C91D555-1616-417F-A212-73E0E1BC7299}']
  end;

  PHContentEditingOutput = interface(NSObject)
    ['{AEDE711A-4786-4163-A3DD-3919EB27A6AB}']
    function adjustmentData: PHAdjustmentData; cdecl;
    function initWithContentEditingInput(contentEditingInput: PHContentEditingInput): Pointer; cdecl;
    function initWithPlaceholderForCreatedAsset(placeholderForCreatedAsset: PHObjectPlaceholder): Pointer; cdecl;
    function renderedContentURL: NSURL; cdecl;
    procedure setAdjustmentData(adjustmentData: PHAdjustmentData); cdecl;
  end;
  TPHContentEditingOutput = class(TOCGenericImport<PHContentEditingOutputClass, PHContentEditingOutput>) end;

  PHAssetChangeRequestClass = interface(PHChangeRequestClass)
    ['{B7E87C8F-3559-457A-98D7-4B3D7887C9C7}']
    {class} function changeRequestForAsset(asset: PHAsset): Pointer; cdecl;
    {class} function creationRequestForAssetFromImage(image: UIImage): Pointer; cdecl;
    {class} function creationRequestForAssetFromImageAtFileURL(fileURL: NSURL): Pointer; cdecl;
    {class} function creationRequestForAssetFromVideoAtFileURL(fileURL: NSURL): Pointer; cdecl;
    {class} procedure deleteAssets(assets: Pointer); cdecl;
  end;

  PHAssetChangeRequest = interface(PHChangeRequest)
    ['{869502AD-1054-4931-9B2B-A28B1392E06F}']
    function contentEditingOutput: PHContentEditingOutput; cdecl;
    function creationDate: NSDate; cdecl;
    function isFavorite: Boolean; cdecl;
    function isHidden: Boolean; cdecl;
    function location: CLLocation; cdecl;
    function placeholderForCreatedAsset: PHObjectPlaceholder; cdecl;
    procedure revertAssetContentToOriginal; cdecl;
    procedure setContentEditingOutput(contentEditingOutput: PHContentEditingOutput); cdecl;
    procedure setCreationDate(creationDate: NSDate); cdecl;
    procedure setFavorite(favorite: Boolean); cdecl;
    procedure setHidden(hidden: Boolean); cdecl;
    procedure setLocation(location: CLLocation); cdecl;
  end;
  TPHAssetChangeRequest = class(TOCGenericImport<PHAssetChangeRequestClass, PHAssetChangeRequest>) end;

  PHContentEditingInputRequestOptionsClass = interface(NSObjectClass)
    ['{7A43BD36-FFDC-41D3-A8D5-05E9581117D0}']
  end;

  PHContentEditingInputRequestOptions = interface(NSObject)
    ['{BED24374-CF46-431D-9872-E619CAF419B9}']
    function canHandleAdjustmentData: TPHContentEditingInputRequestOptionsBlockMethod1; cdecl;
    function isNetworkAccessAllowed: Boolean; cdecl;
    function progressHandler: TPHContentEditingInputRequestOptionsBlockMethod3; cdecl;
    procedure setCanHandleAdjustmentData(canHandleAdjustmentData: TPHContentEditingInputRequestOptionsBlockMethod2); cdecl;
    procedure setNetworkAccessAllowed(networkAccessAllowed: Boolean); cdecl;
    procedure setProgressHandler(progressHandler: TPHContentEditingInputRequestOptionsBlockMethod2); cdecl;
  end;
  TPHContentEditingInputRequestOptions = class(TOCGenericImport<PHContentEditingInputRequestOptionsClass, PHContentEditingInputRequestOptions>) end;

  PHAssetCollectionChangeRequestClass = interface(PHChangeRequestClass)
    ['{92DC3996-E1B5-4DF6-AF1F-314E1CB4254E}']
    {class} function changeRequestForAssetCollection(assetCollection: PHAssetCollection): Pointer; overload; cdecl;
    [MethodName('changeRequestForAssetCollection:assets:')]
    {class} function changeRequestForAssetCollection(assetCollection: PHAssetCollection; assets: PHFetchResult): Pointer; overload; cdecl;
    {class} function creationRequestForAssetCollectionWithTitle(title: NSString): Pointer; cdecl;
    {class} procedure deleteAssetCollections(assetCollections: Pointer); cdecl;
  end;

  PHAssetCollectionChangeRequest = interface(PHChangeRequest)
    ['{90E70B8E-A789-4013-984E-0B9B993A32AD}']
    procedure addAssets(assets: Pointer); cdecl;
    [MethodName('insertAssets:atIndexes:')]
    procedure insertAssets(assets: Pointer; indexes: NSIndexSet); cdecl;
    [MethodName('moveAssetsAtIndexes:toIndex:')]
    procedure moveAssetsAtIndexes(fromIndexes: NSIndexSet; toIndex: NSUInteger); cdecl;
    function placeholderForCreatedAssetCollection: PHObjectPlaceholder; cdecl;
    procedure removeAssets(assets: Pointer); cdecl;
    procedure removeAssetsAtIndexes(indexes: NSIndexSet); cdecl;
    [MethodName('replaceAssetsAtIndexes:withAssets:')]
    procedure replaceAssetsAtIndexes(indexes: NSIndexSet; assets: Pointer); cdecl;
    procedure setTitle(title: NSString); cdecl;
    function title: NSString; cdecl;
  end;
  TPHAssetCollectionChangeRequest = class(TOCGenericImport<PHAssetCollectionChangeRequestClass, PHAssetCollectionChangeRequest>) end;

  PHAssetResourceCreationOptionsClass = interface(NSObjectClass)
    ['{A44C19CA-8357-4D14-AE4E-17A08373996C}']
  end;

  PHAssetResourceCreationOptions = interface(NSObject)
    ['{70F33C2B-DB8A-4567-A313-88DDDC8E5CD0}']
    function originalFilename: NSString; cdecl;
    procedure setOriginalFilename(originalFilename: NSString); cdecl;
    procedure setShouldMoveFile(shouldMoveFile: Boolean); cdecl;
    procedure setUniformTypeIdentifier(uniformTypeIdentifier: NSString); cdecl;
    function shouldMoveFile: Boolean; cdecl;
    function uniformTypeIdentifier: NSString; cdecl;
  end;
  TPHAssetResourceCreationOptions = class(TOCGenericImport<PHAssetResourceCreationOptionsClass, PHAssetResourceCreationOptions>) end;

  PHAssetCreationRequestClass = interface(PHAssetChangeRequestClass)
    ['{00585EA0-3C9E-42CD-831C-D8AD4D5CA6B3}']
    {class} function creationRequestForAsset: Pointer; cdecl;
    {class} function supportsAssetResourceTypes(types: NSArray): Boolean; cdecl;
  end;

  PHAssetCreationRequest = interface(PHAssetChangeRequest)
    ['{9111A9F7-AF13-4548-890F-75A957E79762}']
    [MethodName('addResourceWithType:fileURL:options:')]
    procedure addResourceWithType(&type: PHAssetResourceType; fileURL: NSURL; options: PHAssetResourceCreationOptions); overload; cdecl;
    [MethodName('addResourceWithType:data:options:')]
    procedure addResourceWithType(&type: PHAssetResourceType; data: NSData; options: PHAssetResourceCreationOptions); overload; cdecl;
  end;
  TPHAssetCreationRequest = class(TOCGenericImport<PHAssetCreationRequestClass, PHAssetCreationRequest>) end;

  PHAssetResourceClass = interface(NSObjectClass)
    ['{C18D1AC3-B994-4129-9680-4851EAB57251}']
    {class} function assetResourcesForAsset(asset: PHAsset): NSArray; cdecl;
    {class} function assetResourcesForLivePhoto(livePhoto: PHLivePhoto): NSArray; cdecl;
  end;

  PHAssetResource = interface(NSObject)
    ['{F7F27D91-EDEA-4191-BA37-40DE4C7AE29A}']
    function &type: PHAssetResourceType; cdecl;
    function assetLocalIdentifier: NSString; cdecl;
    function originalFilename: NSString; cdecl;
    function uniformTypeIdentifier: NSString; cdecl;
  end;
  TPHAssetResource = class(TOCGenericImport<PHAssetResourceClass, PHAssetResource>) end;

  PHAssetResourceRequestOptionsClass = interface(NSObjectClass)
    ['{792930ED-567A-4B69-9D29-46F9A7BCB436}']
  end;

  PHAssetResourceRequestOptions = interface(NSObject)
    ['{877B7CB3-E817-44F8-9DAF-CB3A73E796E4}']
    function isNetworkAccessAllowed: Boolean; cdecl;
    function progressHandler: PHAssetResourceProgressHandler; cdecl;
    procedure setNetworkAccessAllowed(networkAccessAllowed: Boolean); cdecl;
    procedure setProgressHandler(progressHandler: PHAssetResourceProgressHandler); cdecl;
  end;
  TPHAssetResourceRequestOptions = class(TOCGenericImport<PHAssetResourceRequestOptionsClass, PHAssetResourceRequestOptions>) end;

  PHAssetResourceManagerClass = interface(NSObjectClass)
    ['{1B9AD55A-693E-4807-A53E-7ADB737D7CDF}']
    {class} function defaultManager: PHAssetResourceManager; cdecl;
  end;

  PHAssetResourceManager = interface(NSObject)
    ['{AD6E31EF-01D8-4DBB-B410-B892A08E6941}']
    procedure cancelDataRequest(requestID: PHAssetResourceDataRequestID); cdecl;
    [MethodName('requestDataForAssetResource:options:dataReceivedHandler:completionHandler:')]
    function requestDataForAssetResource(resource: PHAssetResource; options: PHAssetResourceRequestOptions;
      handler: TPHAssetResourceManagerBlockMethod1; completionHandler: TPHAssetResourceManagerBlockMethod2): PHAssetResourceDataRequestID; cdecl;
    [MethodName('writeDataForAssetResource:toFile:options:completionHandler:')]
    procedure writeDataForAssetResource(resource: PHAssetResource; fileURL: NSURL; options: PHAssetResourceRequestOptions;
      completionHandler: TPHAssetResourceManagerBlockMethod2); cdecl;
  end;
  TPHAssetResourceManager = class(TOCGenericImport<PHAssetResourceManagerClass, PHAssetResourceManager>) end;

  PHChangeClass = interface(NSObjectClass)
    ['{3AEE2B4C-B553-42C5-A5FD-4F266FCB2379}']
  end;

  PHChange = interface(NSObject)
    ['{60C9FC36-4922-4C24-9C1B-9ED6E5B5BCB0}']
    function changeDetailsForFetchResult(&object: PHFetchResult): PHFetchResultChangeDetails; cdecl;
    function changeDetailsForObject(&object: PHObject): PHObjectChangeDetails; cdecl;
  end;
  TPHChange = class(TOCGenericImport<PHChangeClass, PHChange>) end;

  PHObjectChangeDetailsClass = interface(NSObjectClass)
    ['{62C176A7-5B2F-4B59-8200-B7B75D791937}']
  end;

  PHObjectChangeDetails = interface(NSObject)
    ['{6A02FC52-FB14-42AC-A133-51C5251ECEA9}']
    function assetContentChanged: Boolean; cdecl;
    function objectAfterChanges: Pointer; cdecl;
    function objectBeforeChanges: Pointer; cdecl;
    function objectWasDeleted: Boolean; cdecl;
  end;
  TPHObjectChangeDetails = class(TOCGenericImport<PHObjectChangeDetailsClass, PHObjectChangeDetails>) end;

  PHFetchResultChangeDetailsClass = interface(NSObjectClass)
    ['{888114D3-01E1-4A0C-A2DA-5D1D9B38EE01}']
    [MethodName('changeDetailsFromFetchResult:toFetchResult:changedObjects:')]
    {class} function changeDetailsFromFetchResult(fromResult: PHFetchResult; toResult: PHFetchResult; changedObjects: NSArray): Pointer; cdecl;
  end;

  PHFetchResultChangeDetails = interface(NSObject)
    ['{3C146833-A84E-4FE7-9F4B-4A0056795E57}']
    function changedIndexes: NSIndexSet; cdecl;
    function changedObjects: NSArray; cdecl;
    procedure enumerateMovesWithBlock(handler: TPHFetchResultChangeDetailsBlockMethod1); cdecl;
    function fetchResultAfterChanges: PHFetchResult; cdecl;
    function fetchResultBeforeChanges: PHFetchResult; cdecl;
    function hasIncrementalChanges: Boolean; cdecl;
    function hasMoves: Boolean; cdecl;
    function insertedIndexes: NSIndexSet; cdecl;
    function insertedObjects: NSArray; cdecl;
    function removedIndexes: NSIndexSet; cdecl;
    function removedObjects: NSArray; cdecl;
  end;
  TPHFetchResultChangeDetails = class(TOCGenericImport<PHFetchResultChangeDetailsClass, PHFetchResultChangeDetails>) end;

  PHCloudIdentifierClass = interface(NSObjectClass)
    ['{A5D73700-0764-4393-8787-51F350631E81}']
    {class} function notFoundIdentifier: PHCloudIdentifier; cdecl;
  end;

  PHCloudIdentifier = interface(NSObject)
    ['{C12824E9-C55B-4F8E-A17B-88A67A039652}']
    function initWithStringValue(stringValue: NSString): Pointer; cdecl;
    function stringValue: NSString; cdecl;
  end;
  TPHCloudIdentifier = class(TOCGenericImport<PHCloudIdentifierClass, PHCloudIdentifier>) end;

  PHCollectionClass = interface(PHObjectClass)
    ['{801AE5A6-980F-4671-823E-20273618438F}']
    [MethodName('fetchCollectionsInCollectionList:options:')]
    {class} function fetchCollectionsInCollectionList(collectionList: PHCollectionList; options: PHFetchOptions): PHFetchResult; cdecl;
    {class} function fetchTopLevelUserCollectionsWithOptions(options: PHFetchOptions): PHFetchResult; cdecl;
  end;

  PHCollection = interface(PHObject)
    ['{377C46B4-37BB-461B-AA02-73A33EAE811F}']
    function canContainAssets: Boolean; cdecl;
    function canContainCollections: Boolean; cdecl;
    function canPerformEditOperation(anOperation: PHCollectionEditOperation): Boolean; cdecl;
    function localizedTitle: NSString; cdecl;
  end;
  TPHCollection = class(TOCGenericImport<PHCollectionClass, PHCollection>) end;

  PHAssetCollectionClass = interface(PHCollectionClass)
    ['{E4217D6A-5B8F-494B-9E8F-344BD3A51950}']
    [MethodName('fetchAssetCollectionsContainingAsset:withType:options:')]
    {class} function fetchAssetCollectionsContainingAsset(asset: PHAsset; &type: PHAssetCollectionType; options: PHFetchOptions): PHFetchResult; cdecl;
    [MethodName('fetchAssetCollectionsWithALAssetGroupURLs:options:')]
    {class} function fetchAssetCollectionsWithALAssetGroupURLs(assetGroupURLs: NSArray; options: PHFetchOptions): PHFetchResult; cdecl;
    [MethodName('fetchAssetCollectionsWithLocalIdentifiers:options:')]
    {class} function fetchAssetCollectionsWithLocalIdentifiers(identifiers: NSArray; options: PHFetchOptions): PHFetchResult; cdecl;
    [MethodName('fetchAssetCollectionsWithType:subtype:options:')]
    {class} function fetchAssetCollectionsWithType(&type: PHAssetCollectionType; subtype: PHAssetCollectionSubtype;
      options: PHFetchOptions): PHFetchResult; cdecl;
    [MethodName('fetchMomentsInMomentList:options:')]
    {class} function fetchMomentsInMomentList(momentList: PHCollectionList; options: PHFetchOptions): PHFetchResult; cdecl; // API_DEPRECATED("Will be removed in a future release", ios(8, 13), tvos(10, 13))
    {class} function fetchMomentsWithOptions(options: PHFetchOptions): PHFetchResult; cdecl; // API_DEPRECATED("Will be removed in a future release", ios(8, 13), tvos(10, 13))
    [MethodName('transientAssetCollectionWithAssetFetchResult:title:')]
    {class} function transientAssetCollectionWithAssetFetchResult(fetchResult: PHFetchResult; title: NSString): PHAssetCollection; cdecl;
    [MethodName('transientAssetCollectionWithAssets:title:')]
    {class} function transientAssetCollectionWithAssets(assets: NSArray; title: NSString): PHAssetCollection; cdecl;
  end;

  PHAssetCollection = interface(PHCollection)
    ['{38D350DE-2405-4977-80A1-D5400306F4AC}']
    function approximateLocation: CLLocation; cdecl;
    function assetCollectionSubtype: PHAssetCollectionSubtype; cdecl;
    function assetCollectionType: PHAssetCollectionType; cdecl;
    function endDate: NSDate; cdecl;
    function estimatedAssetCount: NSUInteger; cdecl;
    function localizedLocationNames: NSArray; cdecl;
    function startDate: NSDate; cdecl;
  end;
  TPHAssetCollection = class(TOCGenericImport<PHAssetCollectionClass, PHAssetCollection>) end;

  PHCollectionListClass = interface(PHCollectionClass)
    ['{0B27C4FB-8DCC-45CD-B958-E56DAAE412FE}']
    [MethodName('fetchCollectionListsContainingCollection:options:')]
    {class} function fetchCollectionListsContainingCollection(collection: PHCollection; options: PHFetchOptions): PHFetchResult; cdecl;
    [MethodName('fetchCollectionListsWithLocalIdentifiers:options:')]
    {class} function fetchCollectionListsWithLocalIdentifiers(identifiers: NSArray; options: PHFetchOptions): PHFetchResult; cdecl;
    [MethodName('fetchCollectionListsWithType:subtype:options:')]
    {class} function fetchCollectionListsWithType(collectionListType: PHCollectionListType; subtype: PHCollectionListSubtype;
      options: PHFetchOptions): PHFetchResult; cdecl;
    [MethodName('fetchMomentListsWithSubtype:options:')]
    {class} function fetchMomentListsWithSubtype(momentListSubtype: PHCollectionListSubtype; options: PHFetchOptions): PHFetchResult; overload; cdecl; // API_DEPRECATED("Will be removed in a future release", ios(8, 13), tvos(10, 13))
    [MethodName('fetchMomentListsWithSubtype:containingMoment:options:')]
    {class} function fetchMomentListsWithSubtype(momentListSubtype: PHCollectionListSubtype; moment: PHAssetCollection;
      options: PHFetchOptions): PHFetchResult; overload; cdecl; // API_DEPRECATED("Will be removed in a future release", ios(8, 13), tvos(10, 13))
    [MethodName('transientCollectionListWithCollections:title:')]
    {class} function transientCollectionListWithCollections(collections: NSArray; title: NSString): PHCollectionList; cdecl;
    [MethodName('transientCollectionListWithCollectionsFetchResult:title:')]
    {class} function transientCollectionListWithCollectionsFetchResult(fetchResult: PHFetchResult; title: NSString): PHCollectionList; cdecl;
  end;

  PHCollectionList = interface(PHCollection)
    ['{DD0940D7-F7FB-4534-B1A4-F44B4734648A}']
    function collectionListSubtype: PHCollectionListSubtype; cdecl;
    function collectionListType: PHCollectionListType; cdecl;
    function endDate: NSDate; cdecl;
    function localizedLocationNames: NSArray; cdecl;
    function startDate: NSDate; cdecl;
  end;
  TPHCollectionList = class(TOCGenericImport<PHCollectionListClass, PHCollectionList>) end;

  PHCollectionListChangeRequestClass = interface(PHChangeRequestClass)
    ['{317D5C99-459E-4219-B73C-602BA1330ADE}']
    {class} function changeRequestForCollectionList(collectionList: PHCollectionList): Pointer; overload; cdecl;
    [MethodName('changeRequestForCollectionList:childCollections:')]
    {class} function changeRequestForCollectionList(collectionList: PHCollectionList; childCollections: PHFetchResult): Pointer; overload; cdecl;
    {class} function creationRequestForCollectionListWithTitle(title: NSString): Pointer; cdecl;
    {class} procedure deleteCollectionLists(collectionLists: Pointer); cdecl;
  end;

  PHCollectionListChangeRequest = interface(PHChangeRequest)
    ['{E388482E-9E51-4BE8-B0B8-CFED228E7087}']
    procedure addChildCollections(collections: Pointer); cdecl;
    [MethodName('insertChildCollections:atIndexes:')]
    procedure insertChildCollections(collections: Pointer; indexes: NSIndexSet); cdecl;
    [MethodName('moveChildCollectionsAtIndexes:toIndex:')]
    procedure moveChildCollectionsAtIndexes(indexes: NSIndexSet; toIndex: NSUInteger); cdecl;
    function placeholderForCreatedCollectionList: PHObjectPlaceholder; cdecl;
    procedure removeChildCollections(collections: Pointer); cdecl;
    procedure removeChildCollectionsAtIndexes(indexes: NSIndexSet); cdecl;
    [MethodName('replaceChildCollectionsAtIndexes:withChildCollections:')]
    procedure replaceChildCollectionsAtIndexes(indexes: NSIndexSet; collections: Pointer); cdecl;
    procedure setTitle(title: NSString); cdecl;
    function title: NSString; cdecl;
  end;
  TPHCollectionListChangeRequest = class(TOCGenericImport<PHCollectionListChangeRequestClass, PHCollectionListChangeRequest>) end;

  PHContentEditingInputClass = interface(NSObjectClass)
    ['{9A01B800-1468-43C6-A846-AE4185DC7C09}']
  end;

  PHContentEditingInput = interface(NSObject)
    ['{38ACE8E3-EFFA-4544-93D9-BE28FB83A94D}']
    function adjustmentData: PHAdjustmentData; cdecl;
    function audiovisualAsset: AVAsset; cdecl;
    function avAsset: AVAsset; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-audiovisualAsset", ios(8,9))
    function creationDate: NSDate; cdecl;
    function displaySizeImage: UIImage; cdecl;
    function fullSizeImageOrientation: Integer; cdecl;
    function fullSizeImageURL: NSURL; cdecl;
    function livePhoto: PHLivePhoto; cdecl;
    function location: CLLocation; cdecl;
    function mediaSubtypes: PHAssetMediaSubtype; cdecl;
    function mediaType: PHAssetMediaType; cdecl;
    function playbackStyle: PHAssetPlaybackStyle; cdecl;
    function uniformTypeIdentifier: NSString; cdecl;
  end;
  TPHContentEditingInput = class(TOCGenericImport<PHContentEditingInputClass, PHContentEditingInput>) end;

  PHFetchOptionsClass = interface(NSObjectClass)
    ['{CCD41008-0AAA-455D-A9C0-0ACD32DE5485}']
  end;

  PHFetchOptions = interface(NSObject)
    ['{AA70F911-E18F-4414-B8B1-00AED3989F43}']
    function fetchLimit: NSUInteger; cdecl;
    function includeAllBurstAssets: Boolean; cdecl;
    function includeAssetSourceTypes: PHAssetSourceType; cdecl;
    function includeHiddenAssets: Boolean; cdecl;
    function predicate: NSPredicate; cdecl;
    procedure setFetchLimit(fetchLimit: NSUInteger); cdecl;
    procedure setIncludeAllBurstAssets(includeAllBurstAssets: Boolean); cdecl;
    procedure setIncludeAssetSourceTypes(includeAssetSourceTypes: PHAssetSourceType); cdecl;
    procedure setIncludeHiddenAssets(includeHiddenAssets: Boolean); cdecl;
    procedure setPredicate(predicate: NSPredicate); cdecl;
    procedure setSortDescriptors(sortDescriptors: NSArray); cdecl;
    procedure setWantsIncrementalChangeDetails(wantsIncrementalChangeDetails: Boolean); cdecl;
    function sortDescriptors: NSArray; cdecl;
    function wantsIncrementalChangeDetails: Boolean; cdecl;
  end;
  TPHFetchOptions = class(TOCGenericImport<PHFetchOptionsClass, PHFetchOptions>) end;

  PHImageRequestOptionsClass = interface(NSObjectClass)
    ['{6AD2A2E3-C785-4A4E-B0A1-18C65A6C2190}']
  end;

  PHImageRequestOptions = interface(NSObject)
    ['{83A4AB2C-07E8-44F4-B164-06DA8FEB915A}']
    function deliveryMode: PHImageRequestOptionsDeliveryMode; cdecl;
    function isNetworkAccessAllowed: Boolean; cdecl;
    function isSynchronous: Boolean; cdecl;
    function normalizedCropRect: CGRect; cdecl;
    function progressHandler: PHAssetImageProgressHandler; cdecl;
    function resizeMode: PHImageRequestOptionsResizeMode; cdecl;
    procedure setDeliveryMode(deliveryMode: PHImageRequestOptionsDeliveryMode); cdecl;
    procedure setNetworkAccessAllowed(networkAccessAllowed: Boolean); cdecl;
    procedure setNormalizedCropRect(normalizedCropRect: CGRect); cdecl;
    procedure setProgressHandler(progressHandler: PHAssetImageProgressHandler); cdecl;
    procedure setResizeMode(resizeMode: PHImageRequestOptionsResizeMode); cdecl;
    procedure setSynchronous(synchronous: Boolean); cdecl;
    procedure setVersion(version: PHImageRequestOptionsVersion); cdecl;
    function version: PHImageRequestOptionsVersion; cdecl;
  end;
  TPHImageRequestOptions = class(TOCGenericImport<PHImageRequestOptionsClass, PHImageRequestOptions>) end;

  PHLivePhotoRequestOptionsClass = interface(NSObjectClass)
    ['{94A3DA4A-54F7-457C-A265-79521F2567FD}']
  end;

  PHLivePhotoRequestOptions = interface(NSObject)
    ['{E17AD140-AC1B-4391-BAA2-EE1E5538CB00}']
    function deliveryMode: PHImageRequestOptionsDeliveryMode; cdecl;
    function isNetworkAccessAllowed: Boolean; cdecl;
    function progressHandler: PHAssetImageProgressHandler; cdecl;
    procedure setDeliveryMode(deliveryMode: PHImageRequestOptionsDeliveryMode); cdecl;
    procedure setNetworkAccessAllowed(networkAccessAllowed: Boolean); cdecl;
    procedure setProgressHandler(progressHandler: PHAssetImageProgressHandler); cdecl;
    procedure setVersion(version: PHImageRequestOptionsVersion); cdecl;
    function version: PHImageRequestOptionsVersion; cdecl;
  end;
  TPHLivePhotoRequestOptions = class(TOCGenericImport<PHLivePhotoRequestOptionsClass, PHLivePhotoRequestOptions>) end;

  PHVideoRequestOptionsClass = interface(NSObjectClass)
    ['{21EAE628-E56F-430D-84DD-D2CCBEF7ECB7}']
  end;

  PHVideoRequestOptions = interface(NSObject)
    ['{223B9661-74CD-472E-A8D7-B6D56DE82E85}']
    function deliveryMode: PHVideoRequestOptionsDeliveryMode; cdecl;
    function isNetworkAccessAllowed: Boolean; cdecl;
    function progressHandler: PHAssetVideoProgressHandler; cdecl;
    procedure setDeliveryMode(deliveryMode: PHVideoRequestOptionsDeliveryMode); cdecl;
    procedure setNetworkAccessAllowed(networkAccessAllowed: Boolean); cdecl;
    procedure setProgressHandler(progressHandler: PHAssetVideoProgressHandler); cdecl;
    procedure setVersion(version: PHVideoRequestOptionsVersion); cdecl;
    function version: PHVideoRequestOptionsVersion; cdecl;
  end;
  TPHVideoRequestOptions = class(TOCGenericImport<PHVideoRequestOptionsClass, PHVideoRequestOptions>) end;

  PHImageManagerClass = interface(NSObjectClass)
    ['{BD3B7866-7B16-474A-A06C-4D933CC753CD}']
    {class} function defaultManager: PHImageManager; cdecl;
  end;

  PHImageManager = interface(NSObject)
    ['{61233286-510A-457D-9684-C102F1A9A2AC}']
    procedure cancelImageRequest(requestID: PHImageRequestID); cdecl;
    [MethodName('requestAVAssetForVideo:options:resultHandler:')]
    function requestAVAssetForVideo(asset: PHAsset; options: PHVideoRequestOptions;
      resultHandler: TPHImageManagerBlockMethod7): PHImageRequestID; cdecl;
    [MethodName('requestExportSessionForVideo:options:exportPreset:resultHandler:')]
    function requestExportSessionForVideo(asset: PHAsset; options: PHVideoRequestOptions; exportPreset: NSString;
      resultHandler: TPHImageManagerBlockMethod6): PHImageRequestID; cdecl;
    [MethodName('requestImageDataAndOrientationForAsset:options:resultHandler:')]
    function requestImageDataAndOrientationForAsset(asset: PHAsset; options: PHImageRequestOptions;
      resultHandler: TPHImageManagerBlockMethod3): PHImageRequestID; cdecl;
    [MethodName('requestImageDataForAsset:options:resultHandler:')]
    function requestImageDataForAsset(asset: PHAsset; options: PHImageRequestOptions;
      resultHandler: TPHImageManagerBlockMethod2): PHImageRequestID; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-requestImageDataAndOrientationForAsset:options:resultHandler:", ios(8, 13), tvos(8, 13))
    [MethodName('requestImageForAsset:targetSize:contentMode:options:resultHandler:')]
    function requestImageForAsset(asset: PHAsset; targetSize: CGSize; contentMode: PHImageContentMode; options: PHImageRequestOptions;
      resultHandler: TPHImageManagerBlockMethod1): PHImageRequestID; cdecl;
    [MethodName('requestLivePhotoForAsset:targetSize:contentMode:options:resultHandler:')]
    function requestLivePhotoForAsset(asset: PHAsset; targetSize: CGSize; contentMode: PHImageContentMode; options: PHLivePhotoRequestOptions;
      resultHandler: TPHImageManagerBlockMethod4): PHImageRequestID; cdecl;
    [MethodName('requestPlayerItemForVideo:options:resultHandler:')]
    function requestPlayerItemForVideo(asset: PHAsset; options: PHVideoRequestOptions;
      resultHandler: TPHImageManagerBlockMethod5): PHImageRequestID; cdecl;
  end;
  TPHImageManager = class(TOCGenericImport<PHImageManagerClass, PHImageManager>) end;

  PHCachingImageManagerClass = interface(PHImageManagerClass)
    ['{280C6DE2-2C00-4AF3-B86D-50F58E2B97ED}']
  end;

  PHCachingImageManager = interface(PHImageManager)
    ['{0CF18451-6753-4289-A8FF-BDCFBF7CF0A8}']
    function allowsCachingHighQualityImages: Boolean; cdecl;
    procedure setAllowsCachingHighQualityImages(allowsCachingHighQualityImages: Boolean); cdecl;
    [MethodName('startCachingImagesForAssets:targetSize:contentMode:options:')]
    procedure startCachingImagesForAssets(assets: NSArray; targetSize: CGSize; contentMode: PHImageContentMode; options: PHImageRequestOptions); cdecl;
    procedure stopCachingImagesForAllAssets; cdecl;
    [MethodName('stopCachingImagesForAssets:targetSize:contentMode:options:')]
    procedure stopCachingImagesForAssets(assets: NSArray; targetSize: CGSize; contentMode: PHImageContentMode; options: PHImageRequestOptions); cdecl;
  end;
  TPHCachingImageManager = class(TOCGenericImport<PHCachingImageManagerClass, PHCachingImageManager>) end;

  PHLivePhotoClass = interface(NSObjectClass)
    ['{44B557EC-4117-4262-AFC8-94E9790A6C85}']
    {class} procedure cancelLivePhotoRequestWithRequestID(requestID: PHLivePhotoRequestID); cdecl;
    [MethodName('requestLivePhotoWithResourceFileURLs:placeholderImage:targetSize:contentMode:resultHandler:')]
    {class} function requestLivePhotoWithResourceFileURLs(fileURLs: NSArray; image: UIImage; targetSize: CGSize; contentMode: PHImageContentMode;
      resultHandler: TPHLivePhotoBlockMethod1): PHLivePhotoRequestID; cdecl;
  end;

  PHLivePhoto = interface(NSObject)
    ['{0824AF6E-03B2-4983-9827-2755CC9CAFFA}']
    function size: CGSize; cdecl;
  end;
  TPHLivePhoto = class(TOCGenericImport<PHLivePhotoClass, PHLivePhoto>) end;

  PHLivePhotoEditingContextClass = interface(NSObjectClass)
    ['{B00CD3B2-F772-442E-B335-14A7EBE276E0}']
  end;

  PHLivePhotoEditingContext = interface(NSObject)
    ['{8E0F83DD-C372-4E28-B780-260155ECB032}']
    function audioVolume: Single; cdecl;
    procedure cancel; cdecl;
    function duration: CMTime; cdecl;
    function frameProcessor: PHLivePhotoFrameProcessingBlock; cdecl;
    function fullSizeImage: CIImage; cdecl;
    function initWithLivePhotoEditingInput(livePhotoInput: PHContentEditingInput): Pointer; cdecl;
    function orientation: CGImagePropertyOrientation; cdecl;
    function photoTime: CMTime; cdecl;
    [MethodName('prepareLivePhotoForPlaybackWithTargetSize:options:completionHandler:')]
    procedure prepareLivePhotoForPlaybackWithTargetSize(targetSize: CGSize; options: NSDictionary;
      handler: TPHLivePhotoEditingContextBlockMethod1); cdecl;
    [MethodName('saveLivePhotoToOutput:options:completionHandler:')]
    procedure saveLivePhotoToOutput(output: PHContentEditingOutput; options: NSDictionary; handler: TPHLivePhotoEditingContextBlockMethod2); cdecl;
    procedure setAudioVolume(audioVolume: Single); cdecl;
    procedure setFrameProcessor(frameProcessor: PHLivePhotoFrameProcessingBlock); cdecl;
  end;
  TPHLivePhotoEditingContext = class(TOCGenericImport<PHLivePhotoEditingContextClass, PHLivePhotoEditingContext>) end;

  PHLivePhotoFrame = interface(IObjectiveC)
    ['{B3B5B9E5-69F3-43E6-8739-541D205DBC9D}']
    function &type: PHLivePhotoFrameType; cdecl;
    function image: CIImage; cdecl;
    function renderScale: CGFloat; cdecl;
    function time: CMTime; cdecl;
  end;

  PHProjectClass = interface(PHAssetCollectionClass)
    ['{C21C6618-A130-4DDE-B473-80E0984D40B3}']
  end;

  PHProject = interface(PHAssetCollection)
    ['{AB8E729C-6F0B-42F9-94CE-290B991B2EDC}']
    function hasProjectPreview: Boolean; cdecl;
    function projectExtensionData: NSData; cdecl;
  end;
  TPHProject = class(TOCGenericImport<PHProjectClass, PHProject>) end;

  PHProjectChangeRequestClass = interface(PHChangeRequestClass)
    ['{2F60A9B4-FCA0-4771-B957-2172542D8B06}']
  end;

  PHProjectChangeRequest = interface(PHChangeRequest)
    ['{C54A8078-6124-42A9-BDA3-BF164CCA354F}']
    function initWithProject(project: PHProject): Pointer; cdecl;
    function projectExtensionData: NSData; cdecl;
    procedure removeAssets(assets: Pointer); cdecl;
    procedure setKeyAsset(keyAsset: PHAsset); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-setProjectPreviewImage:", macos(10.13, 10.14))
    procedure setProjectExtensionData(projectExtensionData: NSData); cdecl;
    procedure setProjectPreviewImage(previewImage: UIImage); cdecl;
    procedure setTitle(title: NSString); cdecl;
    function title: NSString; cdecl;
  end;
  TPHProjectChangeRequest = class(TOCGenericImport<PHProjectChangeRequestClass, PHProjectChangeRequest>) end;

function PHContentEditingInputResultIsInCloudKey: NSString;
function PHContentEditingInputCancelledKey: NSString;
function PHContentEditingInputErrorKey: NSString;
function PHLocalIdentifierNotFound: NSString;
function PHPhotosErrorDomain: NSErrorDomain;
// Exported const PHImageManagerMaximumSize has an unsupported type: const CGSize
function PHImageResultIsInCloudKey: NSString;
function PHImageResultIsDegradedKey: NSString;
function PHImageResultRequestIDKey: NSString;
function PHImageCancelledKey: NSString;
function PHImageErrorKey: NSString;
function PHLivePhotoInfoErrorKey: NSString;
function PHLivePhotoInfoIsDegradedKey: NSString;
function PHLivePhotoInfoCancelledKey: NSString;
function PHLivePhotoShouldRenderAtPlaybackTime: PHLivePhotoEditingOption;
function PHLivePhotoEditingErrorDomain: NSString;

const
  libPhotos = '/System/Library/Frameworks/Photos.framework/Photos';

implementation

{$IF Defined(IOS) and not Defined(CPUARM)}
uses
  Posix.Dlfcn;

var
  PhotosModule: THandle;
{$ENDIF}

function PHContentEditingInputResultIsInCloudKey: NSString;
begin
  Result := CocoaNSStringConst(libPhotos, 'PHContentEditingInputResultIsInCloudKey');
end;

function PHContentEditingInputCancelledKey: NSString;
begin
  Result := CocoaNSStringConst(libPhotos, 'PHContentEditingInputCancelledKey');
end;

function PHContentEditingInputErrorKey: NSString;
begin
  Result := CocoaNSStringConst(libPhotos, 'PHContentEditingInputErrorKey');
end;

function PHLocalIdentifierNotFound: NSString;
begin
  Result := CocoaNSStringConst(libPhotos, 'PHLocalIdentifierNotFound');
end;

function PHPhotosErrorDomain: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libPhotos, 'PHPhotosErrorDomain');
end;

function PHImageResultIsInCloudKey: NSString;
begin
  Result := CocoaNSStringConst(libPhotos, 'PHImageResultIsInCloudKey');
end;

function PHImageResultIsDegradedKey: NSString;
begin
  Result := CocoaNSStringConst(libPhotos, 'PHImageResultIsDegradedKey');
end;

function PHImageResultRequestIDKey: NSString;
begin
  Result := CocoaNSStringConst(libPhotos, 'PHImageResultRequestIDKey');
end;

function PHImageCancelledKey: NSString;
begin
  Result := CocoaNSStringConst(libPhotos, 'PHImageCancelledKey');
end;

function PHImageErrorKey: NSString;
begin
  Result := CocoaNSStringConst(libPhotos, 'PHImageErrorKey');
end;

function PHLivePhotoInfoErrorKey: NSString;
begin
  Result := CocoaNSStringConst(libPhotos, 'PHLivePhotoInfoErrorKey');
end;

function PHLivePhotoInfoIsDegradedKey: NSString;
begin
  Result := CocoaNSStringConst(libPhotos, 'PHLivePhotoInfoIsDegradedKey');
end;

function PHLivePhotoInfoCancelledKey: NSString;
begin
  Result := CocoaNSStringConst(libPhotos, 'PHLivePhotoInfoCancelledKey');
end;

function PHLivePhotoShouldRenderAtPlaybackTime: PHLivePhotoEditingOption;
begin
  Result := CocoaNSStringConst(libPhotos, 'PHLivePhotoShouldRenderAtPlaybackTime');
end;

function PHLivePhotoEditingErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libPhotos, 'PHLivePhotoEditingErrorDomain');
end;

{$IF Defined(IOS) and not Defined(CPUARM)}
initialization
  PhotosModule := dlopen(MarshaledAString(libPhotos), RTLD_LAZY);

finalization
  dlclose(PhotosModule)
{$ENDIF}

end.