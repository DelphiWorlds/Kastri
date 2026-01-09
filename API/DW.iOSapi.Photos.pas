unit DW.iOSapi.Photos;

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
  Macapi.ObjectiveC, Macapi.CoreFoundation,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.CoreLocation, iOSapi.CoreGraphics, iOSapi.CoreImage, iOSapi.CoreMedia, iOSapi.AVFoundation,
  iOSapi.UIKit,
  // DW
  DW.Macapi.Dispatch, DW.iOSapi.Foundation, DW.iOSapi.UniformTypeIdentifiers;

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
  PHAssetCollectionSubtypeSmartAlbumRAW = 217;
  PHAssetCollectionSubtypeSmartAlbumCinematic = 218;
  PHAssetCollectionSubtypeSmartAlbumSpatial = 219;
  PHAssetCollectionSubtypeAny = NSIntegerMax;
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
  PHAssetMediaSubtypeSpatialMedia = 1024;
  PHAssetMediaSubtypeVideoStreamed = 65536;
  PHAssetMediaSubtypeVideoHighFrameRate = 131072;
  PHAssetMediaSubtypeVideoTimelapse = 262144;
  PHAssetMediaSubtypeVideoCinematic = 2097152;
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
  PHAssetResourceTypePhotoProxy = 19;
  PHObjectTypeAsset = 1;
  PHObjectTypeAssetCollection = 2;
  PHObjectTypeCollectionList = 3;
  PHAuthorizationStatusNotDetermined = 0;
  PHAuthorizationStatusRestricted = 1;
  PHAuthorizationStatusDenied = 2;
  PHAuthorizationStatusAuthorized = 3;
  PHAuthorizationStatusLimited = 4;
  PHAccessLevelAddOnly = 1;
  PHAccessLevelReadWrite = 2;
  PHPhotosErrorInternalError = -1;
  PHPhotosErrorUserCancelled = 3072;
  PHPhotosErrorLibraryVolumeOffline = 3114;
  PHPhotosErrorRelinquishingLibraryBundleToWriter = 3142;
  PHPhotosErrorSwitchingSystemPhotoLibrary = 3143;
  PHPhotosErrorNetworkAccessRequired = 3164;
  PHPhotosErrorNetworkError = 3169;
  PHPhotosErrorIdentifierNotFound = 3201;
  PHPhotosErrorMultipleIdentifiersFound = 3202;
  PHPhotosErrorChangeNotSupported = 3300;
  PHPhotosErrorOperationInterrupted = 3301;
  PHPhotosErrorInvalidResource = 3302;
  PHPhotosErrorMissingResource = 3303;
  PHPhotosErrorNotEnoughSpace = 3305;
  PHPhotosErrorRequestNotSupportedForAsset = 3306;
  PHPhotosErrorAccessRestricted = 3310;
  PHPhotosErrorAccessUserDenied = 3311;
  PHPhotosErrorLibraryInFileProviderSyncRoot = 5423;
  PHPhotosErrorPersistentChangeTokenExpired = 3105;
  PHPhotosErrorPersistentChangeDetailsUnavailable = 3210;
  PHPhotosErrorInvalid = -1;
  PHLivePhotoFrameTypePhoto = 0;
  PHLivePhotoFrameTypeVideo = 1;
  PHLivePhotoEditingErrorCodeUnknown = 0;
  PHLivePhotoEditingErrorCodeAborted = 1;
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

type
  PHPhotoLibraryChangeObserver = interface;
  PHPhotoLibraryAvailabilityObserver = interface;
  PHPhotoLibrary = interface;
  PHObject = interface;
  PHObjectPlaceholder = interface;
  PHFetchResult = interface;
  PHAsset = interface;
  PHLivePhoto = interface;
  PHCollection = interface;
  PHAssetCollection = interface;
  PHCollectionList = interface;
  PHFetchOptions = interface;
  PHChange = interface;
  PHObjectChangeDetails = interface;
  PHFetchResultChangeDetails = interface;
  PHPersistentChange = interface;
  PHPersistentChangeToken = interface;
  PHPersistentChangeFetchResult = interface;
  PHPersistentObjectChangeDetails = interface;
  PHChangeRequest = interface;
  PHContentEditingOutput = interface;
  PHAssetChangeRequest = interface;
  PHContentEditingInputRequestOptions = interface;
  PHAssetResourceCreationOptions = interface;
  PHAssetCreationRequest = interface;
  PHAssetCollectionChangeRequest = interface;
  PHCollectionListChangeRequest = interface;
  PHLivePhotoEditingContext = interface;
  PHLivePhotoFrame = interface;
  PHImageRequestOptions = interface;
  PHLivePhotoRequestOptions = interface;
  PHVideoRequestOptions = interface;
  PHImageManager = interface;
  PHCachingImageManager = interface;
  PHAssetResourceRequestOptions = interface;
  PHAssetResourceManager = interface;
  PHAssetResource = interface;
  PHAdjustmentData = interface;
  PHContentEditingInput = interface;
  PHProject = interface;
  PHProjectChangeRequest = interface;
  PHCloudIdentifier = interface;
  PHCloudIdentifierMapping = interface;
  PHLocalIdentifierMapping = interface;

  PBoolean = ^Boolean;
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
  PHObjectType = NSInteger;
  PHAuthorizationStatus = NSInteger;
  PHAccessLevel = NSInteger;
  PHPhotosError = NSInteger;
  PHLivePhotoRequestID = Int32;
  PHContentEditingInputRequestID = NSUInteger;

  CGImagePropertyOrientation = NSUInteger;
  NSEnumerationOptions = NSUInteger;

  PHLivePhotoFrameProcessingBlock = function(frame: Pointer; error: PPointer): CIImage of object;
  PHLivePhotoEditingOption = NSString;
  PHLivePhotoFrameType = NSInteger;
  PHLivePhotoEditingErrorCode = NSInteger;
  UIImageOrientation = NSInteger;
  PHImageRequestOptionsVersion = NSInteger;
  PHImageRequestOptionsDeliveryMode = NSInteger;
  PHImageRequestOptionsResizeMode = NSInteger;

  PHAssetImageProgressHandler = procedure(progress: Double; error: NSError; stop: PBoolean; info: NSDictionary) of object;
  PHVideoRequestOptionsVersion = NSInteger;
  PHVideoRequestOptionsDeliveryMode = NSInteger;

  PHAssetVideoProgressHandler = procedure(progress: Double; error: NSError; stop: PBoolean; info: NSDictionary) of object;
  PHImageRequestID = Int32;
  PHAssetResourceDataRequestID = Int32;

  PHAssetResourceProgressHandler = procedure(progress: Double) of object;
  TPHPhotoLibraryBlockMethod1 = procedure(status: PHAuthorizationStatus) of object;
  TPHPhotoLibraryBlockMethod2 = procedure(success: Boolean; error: NSError) of object;
  TPHFetchResultBlockMethod1 = procedure(obj: Pointer; idx: NSUInteger; stop: PBoolean) of object;
  TPHLivePhotoBlockMethod1 = procedure(livePhoto: PHLivePhoto; info: NSDictionary) of object;
  TPHFetchResultChangeDetailsBlockMethod1 = procedure(fromIndex: NSUInteger; toIndex: NSUInteger) of object;
  TPHPersistentChangeFetchResultBlockMethod1 = procedure(change: PHPersistentChange; stop: PBoolean) of object;
  TPHContentEditingInputRequestOptionsBlockMethod1 = function(param1: PHAdjustmentData): Boolean of object;
  TPHContentEditingInputRequestOptionsBlockMethod2 = procedure of object;
  TPHContentEditingInputRequestOptionsBlockMethod3 = procedure(param1: Double; param2: PBoolean) of object;
  TPHAssetBlockMethod1 = procedure(contentEditingInput: PHContentEditingInput; info: NSDictionary) of object;
  TPHLivePhotoEditingContextBlockMethod1 = procedure(livePhoto: PHLivePhoto; error: NSError) of object;
  TPHLivePhotoEditingContextBlockMethod2 = procedure(success: Boolean; error: NSError) of object;
  TPHImageManagerBlockMethod1 = procedure(result: UIImage; info: NSDictionary) of object;
  TPHImageManagerBlockMethod2 = procedure(imageData: NSData; dataUTI: NSString; orientation: UIImageOrientation; info: NSDictionary) of object;
  TPHImageManagerBlockMethod3 = procedure(imageData: NSData; dataUTI: NSString; orientation: CGImagePropertyOrientation; info: NSDictionary) of object;
  TPHImageManagerBlockMethod4 = procedure(livePhoto: PHLivePhoto; info: NSDictionary) of object;
  TPHImageManagerBlockMethod5 = procedure(playerItem: AVPlayerItem; info: NSDictionary) of object;
  TPHImageManagerBlockMethod6 = procedure(exportSession: AVAssetExportSession; info: NSDictionary) of object;
  TPHImageManagerBlockMethod7 = procedure(asset: AVAsset; audioMix: AVAudioMix; info: NSDictionary) of object;
  TPHAssetResourceManagerBlockMethod1 = procedure(data: NSData) of object;
  TPHAssetResourceManagerBlockMethod2 = procedure(error: NSError) of object;

  PHPhotoLibraryChangeObserver = interface(IObjectiveC)
    ['{EA0D35A9-3AAD-4BAF-871F-38351C7ABEA2}']
    procedure photoLibraryDidChange(changeInstance: PHChange); cdecl;
  end;

  PHPhotoLibraryAvailabilityObserver = interface(IObjectiveC)
    ['{E8904FE9-9E5B-4126-8683-8D8D73F97FD4}']
    procedure photoLibraryDidBecomeUnavailable(photoLibrary: PHPhotoLibrary); cdecl;
  end;

  PHPhotoLibraryClass = interface(NSObjectClass)
    ['{254A70E7-F85B-4264-9EE8-E065CEC79FD7}']
    {class} function authorizationStatus: PHAuthorizationStatus; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+authorizationStatusForAccessLevel:", ios(8, API_TO_BE_DEPRECATED), macos(10.13, API_TO_BE_DEPRECATED), tvos(10, API_TO_BE_DEPRECATED))
    {class} function authorizationStatusForAccessLevel(accessLevel: PHAccessLevel): PHAuthorizationStatus; cdecl;
    {class} procedure requestAuthorization(handler: TPHPhotoLibraryBlockMethod1); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+requestAuthorizationForAccessLevel:handler:", ios(8, API_TO_BE_DEPRECATED), macos(10.13, API_TO_BE_DEPRECATED), tvos(10, API_TO_BE_DEPRECATED))
    {class} procedure requestAuthorizationForAccessLevel(accessLevel: PHAccessLevel; handler: TPHPhotoLibraryBlockMethod1); cdecl;
    {class} function sharedPhotoLibrary: PHPhotoLibrary; cdecl;
  end;

  PHPhotoLibrary = interface(NSObject)
    ['{BD5DBD49-3FDE-4C97-B66D-3FD716EAC5FE}']
    function cloudIdentifierMappingsForLocalIdentifiers(localIdentifiers: NSArray): NSDictionary; cdecl;
    function cloudIdentifiersForLocalIdentifiers(localIdentifiers: NSArray): NSArray; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-cloudIdentifierMappingsForLocalIdentifiers:", macos(10.13, 12))
    function currentChangeToken: PHPersistentChangeToken; cdecl;
    function fetchPersistentChangesSinceToken(token: PHPersistentChangeToken; error: PPointer): PHPersistentChangeFetchResult; cdecl;
    function localIdentifierMappingsForCloudIdentifiers(cloudIdentifiers: NSArray): NSDictionary; cdecl;
    function localIdentifiersForCloudIdentifiers(cloudIdentifiers: NSArray): NSArray; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-localIdentifierMappingsForCloudIdentifiers:", macos(10.13, 12))
    procedure performChanges(changeBlock: dispatch_block_t; completionHandler: TPHPhotoLibraryBlockMethod2); cdecl;
    function performChangesAndWait(changeBlock: dispatch_block_t; error: PPointer): Boolean; cdecl;
    procedure registerAvailabilityObserver(observer: Pointer); cdecl;
    procedure registerChangeObserver(observer: Pointer); cdecl;
    function unavailabilityReason: NSError; cdecl;
    procedure unregisterAvailabilityObserver(observer: Pointer); cdecl;
    procedure unregisterChangeObserver(observer: Pointer); cdecl;
  end;
  TPHPhotoLibrary = class(TOCGenericImport<PHPhotoLibraryClass, PHPhotoLibrary>) end;

  PHObjectClass = interface(NSObjectClass)
    ['{3770535C-93D2-4C5A-9556-5B658FF53EA7}']
  end;

  PHObject = interface(NSObject)
    ['{22676268-DB1B-4B93-AECC-00F262B1BBB9}']
    function localIdentifier: NSString; cdecl;
  end;
  TPHObject = class(TOCGenericImport<PHObjectClass, PHObject>) end;

  PHObjectPlaceholderClass = interface(PHObjectClass)
    ['{00E02051-39B3-44B2-9FED-62DD26BA6052}']
  end;

  PHObjectPlaceholder = interface(PHObject)
    ['{D32A72DF-2CAB-4649-B66A-DC8CDE6BC779}']
  end;
  TPHObjectPlaceholder = class(TOCGenericImport<PHObjectPlaceholderClass, PHObjectPlaceholder>) end;

  PHFetchResultClass = interface(NSObjectClass)
    ['{19F3ACC0-5417-4022-941A-3C612AE6CFED}']
  end;

  PHFetchResult = interface(NSObject)
    ['{CF73C636-ED2E-47DF-A449-B4A0DE9D75C4}']
    function containsObject(anObject: Pointer): Boolean; cdecl;
    function count: NSUInteger; cdecl;
    function countOfAssetsWithMediaType(mediaType: PHAssetMediaType): NSUInteger; cdecl;
    procedure enumerateObjectsAtIndexes(s: NSIndexSet; options: NSEnumerationOptions; usingBlock: TPHFetchResultBlockMethod1); cdecl;
    procedure enumerateObjectsUsingBlock(block: TPHFetchResultBlockMethod1); cdecl;
    procedure enumerateObjectsWithOptions(opts: NSEnumerationOptions; usingBlock: TPHFetchResultBlockMethod1); cdecl;
    function firstObject: Pointer; cdecl;
    function indexOfObject(anObject: Pointer): NSUInteger; overload; cdecl;
    function indexOfObject(anObject: Pointer; inRange: NSRange): NSUInteger; overload; cdecl;
    function lastObject: Pointer; cdecl;
    function objectAtIndex(index: NSUInteger): Pointer; cdecl;
    function objectAtIndexedSubscript(idx: NSUInteger): Pointer; cdecl;
    function objectsAtIndexes(indexes: NSIndexSet): NSArray; cdecl;
  end;
  TPHFetchResult = class(TOCGenericImport<PHFetchResultClass, PHFetchResult>) end;

  PHAssetClass = interface(PHObjectClass)
    ['{B745CC6C-6C9B-40C8-8170-55DC51EE7CF2}']
    {class} function fetchAssetsInAssetCollection(assetCollection: PHAssetCollection; options: PHFetchOptions): PHFetchResult; cdecl;
    {class} function fetchAssetsWithALAssetURLs(assetURLs: NSArray; options: PHFetchOptions): PHFetchResult; cdecl; // API_DEPRECATED("Will be removed in a future release", ios(8, 11), tvos(8, 11))
    {class} function fetchAssetsWithBurstIdentifier(burstIdentifier: NSString; options: PHFetchOptions): PHFetchResult; cdecl;
    {class} function fetchAssetsWithLocalIdentifiers(identifiers: NSArray; options: PHFetchOptions): PHFetchResult; cdecl;
    {class} function fetchAssetsWithMediaType(mediaType: PHAssetMediaType; options: PHFetchOptions): PHFetchResult; cdecl;
    {class} function fetchAssetsWithOptions(options: PHFetchOptions): PHFetchResult; cdecl;
    {class} function fetchKeyAssetsInAssetCollection(assetCollection: PHAssetCollection; options: PHFetchOptions): PHFetchResult; cdecl;
  end;

  PHAsset = interface(PHObject)
    ['{257417FF-0FE8-4539-8729-BCE9CDF036DC}']
    function adjustmentFormatIdentifier: NSString; cdecl;
    function burstIdentifier: NSString; cdecl;
    function burstSelectionTypes: PHAssetBurstSelectionType; cdecl;
    procedure cancelContentEditingInputRequest(requestID: PHContentEditingInputRequestID); cdecl;
    function canPerformEditOperation(editOperation: PHAssetEditOperation): Boolean; cdecl;
    function creationDate: NSDate; cdecl;
    function duration: NSTimeInterval; cdecl;
    function hasAdjustments: Boolean; cdecl;
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
    function requestContentEditingInputWithOptions(options: PHContentEditingInputRequestOptions;
      completionHandler: TPHAssetBlockMethod1): PHContentEditingInputRequestID; cdecl;
    function sourceType: PHAssetSourceType; cdecl;
  end;
  TPHAsset = class(TOCGenericImport<PHAssetClass, PHAsset>) end;

  PHLivePhotoClass = interface(NSObjectClass)
    ['{9A902BF5-112E-47B8-8651-6EBAD3737272}']
    {class} procedure cancelLivePhotoRequestWithRequestID(requestID: PHLivePhotoRequestID); cdecl;
    {class} function requestLivePhotoWithResourceFileURLs(fileURLs: NSArray; placeholderImage: UIImage; targetSize: CGSize;
      contentMode: PHImageContentMode; resultHandler: TPHLivePhotoBlockMethod1): PHLivePhotoRequestID; cdecl;
  end;

  PHLivePhoto = interface(NSObject)
    ['{2CF69832-6024-4964-A35B-BE5CEC4DCF44}']
    function size: CGSize; cdecl;
  end;
  TPHLivePhoto = class(TOCGenericImport<PHLivePhotoClass, PHLivePhoto>) end;

  PHCollectionClass = interface(PHObjectClass)
    ['{0E92F137-0E6C-48B4-B5B1-B95EB518AE70}']
    {class} function fetchCollectionsInCollectionList(collectionList: PHCollectionList; options: PHFetchOptions): PHFetchResult; cdecl;
    {class} function fetchTopLevelUserCollectionsWithOptions(options: PHFetchOptions): PHFetchResult; cdecl;
  end;

  PHCollection = interface(PHObject)
    ['{E693D1FA-5DE9-46DB-B0A1-11CB26B984F9}']
    function canContainAssets: Boolean; cdecl;
    function canContainCollections: Boolean; cdecl;
    function canPerformEditOperation(anOperation: PHCollectionEditOperation): Boolean; cdecl;
    function localizedTitle: NSString; cdecl;
  end;
  TPHCollection = class(TOCGenericImport<PHCollectionClass, PHCollection>) end;

  PHAssetCollectionClass = interface(PHCollectionClass)
    ['{2475B4F1-2B90-4CBD-ADDC-2EEB01762503}']
    {class} function fetchAssetCollectionsContainingAsset(asset: PHAsset; withType: PHAssetCollectionType;
      options: PHFetchOptions): PHFetchResult; cdecl;
    {class} function fetchAssetCollectionsWithALAssetGroupURLs(assetGroupURLs: NSArray; options: PHFetchOptions): PHFetchResult; cdecl; // API_DEPRECATED("Will be removed in a future release", ios(8,16), tvos(10,16), macos(10.15,13))
    {class} function fetchAssetCollectionsWithLocalIdentifiers(identifiers: NSArray; options: PHFetchOptions): PHFetchResult; cdecl;
    {class} function fetchAssetCollectionsWithType(&type: PHAssetCollectionType; subtype: PHAssetCollectionSubtype;
      options: PHFetchOptions): PHFetchResult; cdecl;
    {class} function fetchMomentsInMomentList(momentList: PHCollectionList; options: PHFetchOptions): PHFetchResult; cdecl; // API_DEPRECATED("Will be removed in a future release", ios(8, 13), tvos(10, 13))
    {class} function fetchMomentsWithOptions(options: PHFetchOptions): PHFetchResult; cdecl; // API_DEPRECATED("Will be removed in a future release", ios(8, 13), tvos(10, 13))
    {class} function transientAssetCollectionWithAssetFetchResult(fetchResult: PHFetchResult; title: NSString): PHAssetCollection; cdecl;
    {class} function transientAssetCollectionWithAssets(assets: NSArray; title: NSString): PHAssetCollection; cdecl;
  end;

  PHAssetCollection = interface(PHCollection)
    ['{EC9695CD-E92D-420D-946D-6F19893EAA98}']
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
    ['{BAD96505-8D39-4BB9-AE80-EAD72B7C6335}']
    {class} function fetchCollectionListsContainingCollection(collection: PHCollection; options: PHFetchOptions): PHFetchResult; cdecl;
    {class} function fetchCollectionListsWithLocalIdentifiers(identifiers: NSArray; options: PHFetchOptions): PHFetchResult; cdecl;
    {class} function fetchCollectionListsWithType(collectionListType: PHCollectionListType; subtype: PHCollectionListSubtype;
      options: PHFetchOptions): PHFetchResult; cdecl;
    {class} function fetchMomentListsWithSubtype(momentListSubtype: PHCollectionListSubtype; options: PHFetchOptions): PHFetchResult; overload; cdecl; // API_DEPRECATED("Will be removed in a future release", ios(8, 13), tvos(10, 13))
    {class} function fetchMomentListsWithSubtype(momentListSubtype: PHCollectionListSubtype; containingMoment: PHAssetCollection;
      options: PHFetchOptions): PHFetchResult; overload; cdecl; // API_DEPRECATED("Will be removed in a future release", ios(8, 13), tvos(10, 13))
    {class} function transientCollectionListWithCollections(collections: NSArray; title: NSString): PHCollectionList; cdecl;
    {class} function transientCollectionListWithCollectionsFetchResult(fetchResult: PHFetchResult; title: NSString): PHCollectionList; cdecl;
  end;

  PHCollectionList = interface(PHCollection)
    ['{EE3DEBC4-73B1-45BE-8EEA-ECF811E1BC54}']
    function collectionListSubtype: PHCollectionListSubtype; cdecl;
    function collectionListType: PHCollectionListType; cdecl;
    function endDate: NSDate; cdecl;
    function localizedLocationNames: NSArray; cdecl;
    function startDate: NSDate; cdecl;
  end;
  TPHCollectionList = class(TOCGenericImport<PHCollectionListClass, PHCollectionList>) end;

  PHFetchOptionsClass = interface(NSObjectClass)
    ['{3259AAB0-B037-41FA-96F1-5FB64D91DA85}']
  end;

  PHFetchOptions = interface(NSObject)
    ['{E780F6BD-5574-4FAC-B4AE-1455B1C129A0}']
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

  PHChangeClass = interface(NSObjectClass)
    ['{5EC0867C-E3D0-4982-B642-1B378D59A63C}']
  end;

  PHChange = interface(NSObject)
    ['{BD88369C-CC60-4872-88B7-2C0DC2DBF25F}']
    function changeDetailsForFetchResult(&object: PHFetchResult): PHFetchResultChangeDetails; cdecl;
    function changeDetailsForObject(&object: PHObject): PHObjectChangeDetails; cdecl;
  end;
  TPHChange = class(TOCGenericImport<PHChangeClass, PHChange>) end;

  PHObjectChangeDetailsClass = interface(NSObjectClass)
    ['{6BCBB858-FFBE-4454-9E01-2FD79D52ABD1}']
  end;

  PHObjectChangeDetails = interface(NSObject)
    ['{0E45EE91-B3E1-4B9B-B856-76FB7F8FDC08}']
    function assetContentChanged: Boolean; cdecl;
    function objectAfterChanges: Pointer; cdecl;
    function objectBeforeChanges: Pointer; cdecl;
    function objectWasDeleted: Boolean; cdecl;
  end;
  TPHObjectChangeDetails = class(TOCGenericImport<PHObjectChangeDetailsClass, PHObjectChangeDetails>) end;

  PHFetchResultChangeDetailsClass = interface(NSObjectClass)
    ['{8432D780-ABA7-4E1C-B524-4CF4335FC6D4}']
    {class} function changeDetailsFromFetchResult(fromResult: PHFetchResult; toFetchResult: PHFetchResult; changedObjects: NSArray): Pointer; cdecl;
  end;

  PHFetchResultChangeDetails = interface(NSObject)
    ['{2A4AEB86-CE12-448F-99EC-E29172BB6EAF}']
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

  PHPersistentChangeClass = interface(NSObjectClass)
    ['{F14B9E82-F2F8-41B0-8144-0AA533FC6D1C}']
    {class} function new: Pointer; cdecl;
  end;

  PHPersistentChange = interface(NSObject)
    ['{D1691E84-3B7A-49E3-9834-1927F3BF32AC}']
    function changeDetailsForObjectType(objectType: PHObjectType; error: PPointer): PHPersistentObjectChangeDetails; cdecl;
    function changeToken: PHPersistentChangeToken; cdecl;
  end;
  TPHPersistentChange = class(TOCGenericImport<PHPersistentChangeClass, PHPersistentChange>) end;

  PHPersistentChangeTokenClass = interface(NSObjectClass)
    ['{EF1EFF02-8B03-4C86-A3A8-588DE21F9FF9}']
    {class} function new: Pointer; cdecl;
  end;

  PHPersistentChangeToken = interface(NSObject)
    ['{C5D8114C-85B9-48E5-9140-119DC9A05E88}']
  end;
  TPHPersistentChangeToken = class(TOCGenericImport<PHPersistentChangeTokenClass, PHPersistentChangeToken>) end;

  PHPersistentChangeFetchResultClass = interface(NSObjectClass)
    ['{766AA716-2E5C-462C-AD0B-ADAD85174688}']
    {class} function new: Pointer; cdecl;
  end;

  PHPersistentChangeFetchResult = interface(NSObject)
    ['{7F676D83-91A1-4EA1-A83E-CB13E32B246B}']
    procedure enumerateChangesWithBlock(block: TPHPersistentChangeFetchResultBlockMethod1); cdecl;
  end;
  TPHPersistentChangeFetchResult = class(TOCGenericImport<PHPersistentChangeFetchResultClass, PHPersistentChangeFetchResult>) end;

  PHPersistentObjectChangeDetailsClass = interface(NSObjectClass)
    ['{72CFC4BB-12CC-40A7-AC86-A21E36893080}']
    {class} function new: Pointer; cdecl;
  end;

  PHPersistentObjectChangeDetails = interface(NSObject)
    ['{E919BC3A-D975-4AB4-9E10-DF471048B451}']
    function deletedLocalIdentifiers: NSSet; cdecl;
    function insertedLocalIdentifiers: NSSet; cdecl;
    function objectType: PHObjectType; cdecl;
    function updatedLocalIdentifiers: NSSet; cdecl;
  end;
  TPHPersistentObjectChangeDetails = class(TOCGenericImport<PHPersistentObjectChangeDetailsClass, PHPersistentObjectChangeDetails>) end;

  PHChangeRequestClass = interface(NSObjectClass)
    ['{20ABD6FF-2F3B-44A1-A6B0-65813DBAAB55}']
  end;

  PHChangeRequest = interface(NSObject)
    ['{E624A56A-9D1D-47A4-AA63-3A7AEE931326}']
  end;
  TPHChangeRequest = class(TOCGenericImport<PHChangeRequestClass, PHChangeRequest>) end;

  PHContentEditingOutputClass = interface(NSObjectClass)
    ['{A7D45342-7D20-4248-8FA4-EFD92063B09F}']
  end;

  PHContentEditingOutput = interface(NSObject)
    ['{5CE71570-57E7-410A-89BF-BAC5C2AF446A}']
    function adjustmentData: PHAdjustmentData; cdecl;
    function defaultRenderedContentType: UTType; cdecl;
    function initWithContentEditingInput(contentEditingInput: PHContentEditingInput): Pointer; cdecl;
    function initWithPlaceholderForCreatedAsset(placeholderForCreatedAsset: PHObjectPlaceholder): Pointer; cdecl;
    function renderedContentURL: NSURL; cdecl;
    function renderedContentURLForType(&type: UTType; error: PPointer): NSURL; cdecl;
    procedure setAdjustmentData(adjustmentData: PHAdjustmentData); cdecl;
    function supportedRenderedContentTypes: NSArray; cdecl;
  end;
  TPHContentEditingOutput = class(TOCGenericImport<PHContentEditingOutputClass, PHContentEditingOutput>) end;

  PHAssetChangeRequestClass = interface(PHChangeRequestClass)
    ['{4C904446-4053-48B5-A1A9-0FF958A9D615}']
    {class} function changeRequestForAsset(asset: PHAsset): Pointer; cdecl;
    {class} function creationRequestForAssetFromImage(image: UIImage): Pointer; cdecl;
    {class} function creationRequestForAssetFromImageAtFileURL(fileURL: NSURL): Pointer; cdecl;
    {class} function creationRequestForAssetFromVideoAtFileURL(fileURL: NSURL): Pointer; cdecl;
    {class} procedure deleteAssets(assets: Pointer); cdecl;
  end;

  PHAssetChangeRequest = interface(PHChangeRequest)
    ['{EBB16B1F-E74C-4E54-AAD0-DA5FD8E0FB88}']
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
    ['{1C53FF9B-21D9-4A52-968E-BA9F95567859}']
  end;

  PHContentEditingInputRequestOptions = interface(NSObject)
    ['{1961BD32-070F-4E68-BF9E-BA088C36DCD6}']
    function canHandleAdjustmentData: TPHContentEditingInputRequestOptionsBlockMethod1; cdecl;
    function isNetworkAccessAllowed: Boolean; cdecl;
    function progressHandler: TPHContentEditingInputRequestOptionsBlockMethod3; cdecl;
    procedure setCanHandleAdjustmentData(canHandleAdjustmentData: TPHContentEditingInputRequestOptionsBlockMethod2); cdecl;
    procedure setNetworkAccessAllowed(networkAccessAllowed: Boolean); cdecl;
    procedure setProgressHandler(progressHandler: TPHContentEditingInputRequestOptionsBlockMethod2); cdecl;
  end;
  TPHContentEditingInputRequestOptions = class(TOCGenericImport<PHContentEditingInputRequestOptionsClass, PHContentEditingInputRequestOptions>) end;

  PHAssetResourceCreationOptionsClass = interface(NSObjectClass)
    ['{79411F74-882C-4987-86DB-B65478371F38}']
  end;

  PHAssetResourceCreationOptions = interface(NSObject)
    ['{9CFC277E-D936-4970-839F-6931287F5781}']
    function originalFilename: NSString; cdecl;
    procedure setOriginalFilename(originalFilename: NSString); cdecl;
    procedure setShouldMoveFile(shouldMoveFile: Boolean); cdecl;
    procedure setUniformTypeIdentifier(uniformTypeIdentifier: NSString); cdecl;
    function shouldMoveFile: Boolean; cdecl;
    function uniformTypeIdentifier: NSString; cdecl;
  end;
  TPHAssetResourceCreationOptions = class(TOCGenericImport<PHAssetResourceCreationOptionsClass, PHAssetResourceCreationOptions>) end;

  PHAssetCreationRequestClass = interface(PHAssetChangeRequestClass)
    ['{7AEEF759-0DCB-4DA6-9E21-EE8B67DFEBD0}']
    {class} function creationRequestForAsset: Pointer; cdecl;
    {class} function supportsAssetResourceTypes(types: NSArray): Boolean; cdecl;
  end;

  PHAssetCreationRequest = interface(PHAssetChangeRequest)
    ['{8A5AF5EC-1477-4781-B244-BA6F975E229F}']
    procedure addResourceWithType(&type: PHAssetResourceType; fileURL: NSURL; options: PHAssetResourceCreationOptions); overload; cdecl;
    procedure addResourceWithType(&type: PHAssetResourceType; data: NSData; options: PHAssetResourceCreationOptions); overload; cdecl;
  end;
  TPHAssetCreationRequest = class(TOCGenericImport<PHAssetCreationRequestClass, PHAssetCreationRequest>) end;

  PHAssetCollectionChangeRequestClass = interface(PHChangeRequestClass)
    ['{6A4D91B9-81B8-4A98-BE77-F127D33C4044}']
    {class} function changeRequestForAssetCollection(assetCollection: PHAssetCollection): Pointer; overload; cdecl;
    {class} function changeRequestForAssetCollection(assetCollection: PHAssetCollection; assets: PHFetchResult): Pointer; overload; cdecl;
    {class} function creationRequestForAssetCollectionWithTitle(title: NSString): Pointer; cdecl;
    {class} procedure deleteAssetCollections(assetCollections: Pointer); cdecl;
  end;

  PHAssetCollectionChangeRequest = interface(PHChangeRequest)
    ['{F919134B-633C-40AB-AC73-D9F8DB05DF20}']
    procedure addAssets(assets: Pointer); cdecl;
    procedure insertAssets(assets: Pointer; atIndexes: NSIndexSet); cdecl;
    procedure moveAssetsAtIndexes(fromIndexes: NSIndexSet; toIndex: NSUInteger); cdecl;
    function placeholderForCreatedAssetCollection: PHObjectPlaceholder; cdecl;
    procedure removeAssets(assets: Pointer); cdecl;
    procedure removeAssetsAtIndexes(indexes: NSIndexSet); cdecl;
    procedure replaceAssetsAtIndexes(indexes: NSIndexSet; withAssets: Pointer); cdecl;
    procedure setTitle(title: NSString); cdecl;
    function title: NSString; cdecl;
  end;
  TPHAssetCollectionChangeRequest = class(TOCGenericImport<PHAssetCollectionChangeRequestClass, PHAssetCollectionChangeRequest>) end;

  PHCollectionListChangeRequestClass = interface(PHChangeRequestClass)
    ['{4A555403-72FD-4C71-91CF-00085B675771}']
    {class} function changeRequestForCollectionList(collectionList: PHCollectionList; childCollections: PHFetchResult): Pointer; overload; cdecl;
    {class} function changeRequestForCollectionList(collectionList: PHCollectionList): Pointer; overload; cdecl;
    {class} function changeRequestForTopLevelCollectionListUserCollections(childCollections: PHFetchResult): Pointer; cdecl;
    {class} function creationRequestForCollectionListWithTitle(title: NSString): Pointer; cdecl;
    {class} procedure deleteCollectionLists(collectionLists: Pointer); cdecl;
  end;

  PHCollectionListChangeRequest = interface(PHChangeRequest)
    ['{4C33CC5F-FB96-4F70-89F6-970CEF43475B}']
    procedure addChildCollections(collections: Pointer); cdecl;
    procedure insertChildCollections(collections: Pointer; atIndexes: NSIndexSet); cdecl;
    procedure moveChildCollectionsAtIndexes(indexes: NSIndexSet; toIndex: NSUInteger); cdecl;
    function placeholderForCreatedCollectionList: PHObjectPlaceholder; cdecl;
    procedure removeChildCollections(collections: Pointer); cdecl;
    procedure removeChildCollectionsAtIndexes(indexes: NSIndexSet); cdecl;
    procedure replaceChildCollectionsAtIndexes(indexes: NSIndexSet; withChildCollections: Pointer); cdecl;
    procedure setTitle(title: NSString); cdecl;
    function title: NSString; cdecl;
  end;
  TPHCollectionListChangeRequest = class(TOCGenericImport<PHCollectionListChangeRequestClass, PHCollectionListChangeRequest>) end;

  PHLivePhotoEditingContextClass = interface(NSObjectClass)
    ['{50E42289-2C44-4DFE-9625-F94D626AE262}']
  end;

  PHLivePhotoEditingContext = interface(NSObject)
    ['{0B610A59-3C68-4563-9D93-984265BBE87A}']
    function audioVolume: Single; cdecl;
    procedure cancel; cdecl;
    function duration: CMTime; cdecl;
    function frameProcessor: PHLivePhotoFrameProcessingBlock; cdecl;
    function fullSizeImage: CIImage; cdecl;
    function initWithLivePhotoEditingInput(livePhotoInput: PHContentEditingInput): Pointer; cdecl;
    function orientation: CGImagePropertyOrientation; cdecl;
    function photoTime: CMTime; cdecl;
    procedure prepareLivePhotoForPlaybackWithTargetSize(targetSize: CGSize; options: NSDictionary;
      completionHandler: TPHLivePhotoEditingContextBlockMethod1); cdecl;
    procedure saveLivePhotoToOutput(output: PHContentEditingOutput; options: NSDictionary;
      completionHandler: TPHLivePhotoEditingContextBlockMethod2); cdecl;
    procedure setAudioVolume(audioVolume: Single); cdecl;
    procedure setFrameProcessor(frameProcessor: PHLivePhotoFrameProcessingBlock); cdecl;
  end;
  TPHLivePhotoEditingContext = class(TOCGenericImport<PHLivePhotoEditingContextClass, PHLivePhotoEditingContext>) end;

  PHLivePhotoFrame = interface(IObjectiveC)
    ['{0234DDC9-5C0B-49E3-9600-A2B81155A18B}']
    function &type: PHLivePhotoFrameType; cdecl;
    function image: CIImage; cdecl;
    function renderScale: CGFloat; cdecl;
    function time: CMTime; cdecl;
  end;

  PHImageRequestOptionsClass = interface(NSObjectClass)
    ['{6AD7EB2B-9E41-4FEA-AE4E-3EBEBD7199F7}']
  end;

  PHImageRequestOptions = interface(NSObject)
    ['{A042321F-9B68-4330-9CF6-6976B5616CE0}']
    function allowSecondaryDegradedImage: Boolean; cdecl;
    function deliveryMode: PHImageRequestOptionsDeliveryMode; cdecl;
    function isNetworkAccessAllowed: Boolean; cdecl;
    function isSynchronous: Boolean; cdecl;
    function normalizedCropRect: CGRect; cdecl;
    function progressHandler: PHAssetImageProgressHandler; cdecl;
    function resizeMode: PHImageRequestOptionsResizeMode; cdecl;
    procedure setAllowSecondaryDegradedImage(allowSecondaryDegradedImage: Boolean); cdecl;
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
    ['{B841210B-B6EB-4DF6-BBD3-51BDD2ADF5B9}']
  end;

  PHLivePhotoRequestOptions = interface(NSObject)
    ['{CBBFA7E3-8AB0-4AE1-B65C-F55C1006C05E}']
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
    ['{05AE495D-6283-4EA7-ADDC-FC5820D756C6}']
  end;

  PHVideoRequestOptions = interface(NSObject)
    ['{99B8CD46-3C4B-47AE-A0DC-3CC4D03A6193}']
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
    ['{9350861A-867A-4334-B85E-013DC9B1202F}']
    {class} function defaultManager: PHImageManager; cdecl;
  end;

  PHImageManager = interface(NSObject)
    ['{A3D42589-0846-4447-925E-FB9B5DC3FB14}']
    procedure cancelImageRequest(requestID: PHImageRequestID); cdecl;
    function requestAVAssetForVideo(asset: PHAsset; options: PHVideoRequestOptions;
      resultHandler: TPHImageManagerBlockMethod7): PHImageRequestID; cdecl;
    function requestExportSessionForVideo(asset: PHAsset; options: PHVideoRequestOptions; exportPreset: NSString;
      resultHandler: TPHImageManagerBlockMethod6): PHImageRequestID; cdecl;
    function requestImageDataAndOrientationForAsset(asset: PHAsset; options: PHImageRequestOptions;
      resultHandler: TPHImageManagerBlockMethod3): PHImageRequestID; cdecl;
    function requestImageDataForAsset(asset: PHAsset; options: PHImageRequestOptions;
      resultHandler: TPHImageManagerBlockMethod2): PHImageRequestID; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-requestImageDataAndOrientationForAsset:options:resultHandler:", ios(8, 13), tvos(8, 13))
    function requestImageForAsset(asset: PHAsset; targetSize: CGSize; contentMode: PHImageContentMode; options: PHImageRequestOptions;
      resultHandler: TPHImageManagerBlockMethod1): PHImageRequestID; cdecl;
    function requestLivePhotoForAsset(asset: PHAsset; targetSize: CGSize; contentMode: PHImageContentMode; options: PHLivePhotoRequestOptions;
      resultHandler: TPHImageManagerBlockMethod4): PHImageRequestID; cdecl;
    function requestPlayerItemForVideo(asset: PHAsset; options: PHVideoRequestOptions;
      resultHandler: TPHImageManagerBlockMethod5): PHImageRequestID; cdecl;
  end;
  TPHImageManager = class(TOCGenericImport<PHImageManagerClass, PHImageManager>) end;

  PHCachingImageManagerClass = interface(PHImageManagerClass)
    ['{49EFD194-8534-432E-AA3E-E61B618B2094}']
  end;

  PHCachingImageManager = interface(PHImageManager)
    ['{26D3D7BB-CA47-4A4F-B76B-727CD8158513}']
    function allowsCachingHighQualityImages: Boolean; cdecl;
    procedure setAllowsCachingHighQualityImages(allowsCachingHighQualityImages: Boolean); cdecl;
    procedure startCachingImagesForAssets(assets: NSArray; targetSize: CGSize; contentMode: PHImageContentMode; options: PHImageRequestOptions); cdecl;
    procedure stopCachingImagesForAllAssets; cdecl;
    procedure stopCachingImagesForAssets(assets: NSArray; targetSize: CGSize; contentMode: PHImageContentMode; options: PHImageRequestOptions); cdecl;
  end;
  TPHCachingImageManager = class(TOCGenericImport<PHCachingImageManagerClass, PHCachingImageManager>) end;

  PHAssetResourceRequestOptionsClass = interface(NSObjectClass)
    ['{872F8D0D-3CFB-4280-AAA1-ED760D808F77}']
  end;

  PHAssetResourceRequestOptions = interface(NSObject)
    ['{03390A16-4F81-48D1-B747-2FBAA73561EC}']
    function isNetworkAccessAllowed: Boolean; cdecl;
    function progressHandler: PHAssetResourceProgressHandler; cdecl;
    procedure setNetworkAccessAllowed(networkAccessAllowed: Boolean); cdecl;
    procedure setProgressHandler(progressHandler: PHAssetResourceProgressHandler); cdecl;
  end;
  TPHAssetResourceRequestOptions = class(TOCGenericImport<PHAssetResourceRequestOptionsClass, PHAssetResourceRequestOptions>) end;

  PHAssetResourceManagerClass = interface(NSObjectClass)
    ['{B6E7B0C6-D972-4031-96F8-E7E9385E1A2D}']
    {class} function defaultManager: PHAssetResourceManager; cdecl;
  end;

  PHAssetResourceManager = interface(NSObject)
    ['{4DC3F78A-EC83-45AC-BD92-4468E18A8368}']
    procedure cancelDataRequest(requestID: PHAssetResourceDataRequestID); cdecl;
    function requestDataForAssetResource(resource: PHAssetResource; options: PHAssetResourceRequestOptions;
      dataReceivedHandler: TPHAssetResourceManagerBlockMethod1;
      completionHandler: TPHAssetResourceManagerBlockMethod2): PHAssetResourceDataRequestID; cdecl;
    procedure writeDataForAssetResource(resource: PHAssetResource; toFile: NSURL; options: PHAssetResourceRequestOptions;
      completionHandler: TPHAssetResourceManagerBlockMethod2); cdecl;
  end;
  TPHAssetResourceManager = class(TOCGenericImport<PHAssetResourceManagerClass, PHAssetResourceManager>) end;

  PHAssetResourceClass = interface(NSObjectClass)
    ['{766E45FE-D1BC-421C-8EF9-6B6DE4446721}']
    {class} function assetResourcesForAsset(asset: PHAsset): NSArray; cdecl;
    {class} function assetResourcesForLivePhoto(livePhoto: PHLivePhoto): NSArray; cdecl;
  end;

  PHAssetResource = interface(NSObject)
    ['{AFD9134C-1F8E-4ADE-B4E8-83DBE57837F8}']
    function &type: PHAssetResourceType; cdecl;
    function assetLocalIdentifier: NSString; cdecl;
    function originalFilename: NSString; cdecl;
    function pixelHeight: NSInteger; cdecl;
    function pixelWidth: NSInteger; cdecl;
    function uniformTypeIdentifier: NSString; cdecl;
  end;
  TPHAssetResource = class(TOCGenericImport<PHAssetResourceClass, PHAssetResource>) end;

  PHAdjustmentDataClass = interface(NSObjectClass)
    ['{B2B0B11D-EE70-4515-8E9B-50D9B5B946ED}']
  end;

  PHAdjustmentData = interface(NSObject)
    ['{8785FC2F-B571-4E63-96D8-AEDD4A3FE966}']
    function data: NSData; cdecl;
    function formatIdentifier: NSString; cdecl;
    function formatVersion: NSString; cdecl;
    function initWithFormatIdentifier(formatIdentifier: NSString; formatVersion: NSString; data: NSData): Pointer; cdecl;
  end;
  TPHAdjustmentData = class(TOCGenericImport<PHAdjustmentDataClass, PHAdjustmentData>) end;

  PHContentEditingInputClass = interface(NSObjectClass)
    ['{681AE3D8-C634-4064-B010-5281C3284D1E}']
  end;

  PHContentEditingInput = interface(NSObject)
    ['{85CFD1C9-1108-4C11-8725-B006F8779887}']
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

  PHProjectClass = interface(PHAssetCollectionClass)
    ['{4BCB75BF-4E81-45C3-A746-2DA93EED4157}']
  end;

  PHProject = interface(PHAssetCollection)
    ['{D91590DC-BC26-4CFC-BD76-59C5560C6C93}']
    function hasProjectPreview: Boolean; cdecl;
    function projectExtensionData: NSData; cdecl;
  end;
  TPHProject = class(TOCGenericImport<PHProjectClass, PHProject>) end;

  PHProjectChangeRequestClass = interface(PHChangeRequestClass)
    ['{900EC4EA-6FED-426A-9228-4365E4950211}']
  end;

  PHProjectChangeRequest = interface(PHChangeRequest)
    ['{38446885-4982-46B4-9FB8-E3E6C82FAC17}']
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

  PHCloudIdentifierClass = interface(NSObjectClass)
    ['{5C902173-5DB7-4FC2-8CB8-86459231128E}']
    {class} function notFoundIdentifier: PHCloudIdentifier; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("Check for PHPhotosErrorIdentifierNotFound in PHCloudIdentifierMapping.error", macos(10.13, 12))
  end;

  PHCloudIdentifier = interface(NSObject)
    ['{81A4A59D-38C1-4B52-8806-00193F2CE5FE}']
    function initWithStringValue(stringValue: NSString): Pointer; cdecl;
    function stringValue: NSString; cdecl;
  end;
  TPHCloudIdentifier = class(TOCGenericImport<PHCloudIdentifierClass, PHCloudIdentifier>) end;

  PHCloudIdentifierMappingClass = interface(NSObjectClass)
    ['{34F65146-C617-4BBB-B726-B8F4069B5438}']
  end;

  PHCloudIdentifierMapping = interface(NSObject)
    ['{B1F8A380-76F2-445B-9BF5-F84CCFF3ECA6}']
    function cloudIdentifier: PHCloudIdentifier; cdecl;
    function error: NSError; cdecl;
  end;
  TPHCloudIdentifierMapping = class(TOCGenericImport<PHCloudIdentifierMappingClass, PHCloudIdentifierMapping>) end;

  PHLocalIdentifierMappingClass = interface(NSObjectClass)
    ['{503240B4-0719-4BFA-84EA-59A637B0F998}']
  end;

  PHLocalIdentifierMapping = interface(NSObject)
    ['{01482D41-B248-48D0-85F7-0B5B1DEE5105}']
    function error: NSError; cdecl;
    function localIdentifier: NSString; cdecl;
  end;
  TPHLocalIdentifierMapping = class(TOCGenericImport<PHLocalIdentifierMappingClass, PHLocalIdentifierMapping>) end;

function PHPhotosErrorDomain: NSErrorDomain;
function PHLocalIdentifiersErrorKey: NSErrorUserInfoKey;
function PHLivePhotoInfoErrorKey: NSString;
function PHLivePhotoInfoIsDegradedKey: NSString;
function PHLivePhotoInfoCancelledKey: NSString;
function PHContentEditingInputResultIsInCloudKey: NSString;
function PHContentEditingInputCancelledKey: NSString;
function PHContentEditingInputErrorKey: NSString;
function PHLivePhotoShouldRenderAtPlaybackTime: PHLivePhotoEditingOption;
function PHLivePhotoEditingErrorDomain: NSString;
function PHImageResultIsInCloudKey: NSString;
function PHImageResultIsDegradedKey: NSString;
function PHImageResultRequestIDKey: NSString;
function PHImageCancelledKey: NSString;
function PHImageErrorKey: NSString;
function PHLocalIdentifierNotFound: NSString;

const
  libPhotos = '/System/Library/Frameworks/Photos.framework/Photos';

implementation

uses
  Posix.Dlfcn;

var
  PhotosModule: THandle;

function PHPhotosErrorDomain: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libPhotos, 'PHPhotosErrorDomain');
end;

function PHLocalIdentifiersErrorKey: NSString;
begin
  Result := CocoaNSStringConst(libPhotos, 'PHLocalIdentifiersErrorKey');
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

function PHLivePhotoShouldRenderAtPlaybackTime: NSString;
begin
  Result := CocoaNSStringConst(libPhotos, 'PHLivePhotoShouldRenderAtPlaybackTime');
end;

function PHLivePhotoEditingErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libPhotos, 'PHLivePhotoEditingErrorDomain');
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

function PHLocalIdentifierNotFound: NSString;
begin
  Result := CocoaNSStringConst(libPhotos, 'PHLocalIdentifierNotFound');
end;

initialization
  PhotosModule := dlopen(MarshaledAString(libPhotos), RTLD_LAZY);

finalization
  dlclose(PhotosModule);

end.