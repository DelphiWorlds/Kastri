unit DW.Macapi.CoreVideo;

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
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.Foundation, Macapi.CocoaTypes, Macapi.CoreGraphics;

const
  kCVSMPTETimeType24 = 0;
  kCVSMPTETimeType25 = 1;
  kCVSMPTETimeType30Drop = 2;
  kCVSMPTETimeType30 = 3;
  kCVSMPTETimeType2997 = 4;
  kCVSMPTETimeType2997Drop = 5;
  kCVSMPTETimeType60 = 6;
  kCVSMPTETimeType5994 = 7;
  kCVSMPTETimeValid = 1;
  kCVSMPTETimeRunning = 2;
  kCVTimeIsIndefinite = 1;
  kCVTimeStampVideoTimeValid = 1;
  kCVTimeStampHostTimeValid = 2;
  kCVTimeStampSMPTETimeValid = 4;
  kCVTimeStampVideoRefreshPeriodValid = 8;
  kCVTimeStampRateScalarValid = 16;
  kCVTimeStampTopField = 65536;
  kCVTimeStampBottomField = 131072;
  kCVTimeStampVideoHostTimeValid = kCVTimeStampVideoTimeValid or kCVTimeStampHostTimeValid;
  kCVTimeStampIsInterlaced = kCVTimeStampTopField or kCVTimeStampBottomField;
  kCVReturnSuccess = 0;
  kCVReturnFirst = -6660;
  kCVReturnError = kCVReturnFirst;
  kCVReturnInvalidArgument = -6661;
  kCVReturnAllocationFailed = -6662;
  kCVReturnUnsupported = -6663;
  kCVReturnInvalidDisplay = -6670;
  kCVReturnDisplayLinkAlreadyRunning = -6671;
  kCVReturnDisplayLinkNotRunning = -6672;
  kCVReturnDisplayLinkCallbacksNotSet = -6673;
  kCVReturnInvalidPixelFormat = -6680;
  kCVReturnInvalidSize = -6681;
  kCVReturnInvalidPixelBufferAttributes = -6682;
  kCVReturnPixelBufferNotOpenGLCompatible = -6683;
  kCVReturnPixelBufferNotMetalCompatible = -6684;
  kCVReturnWouldExceedAllocationThreshold = -6689;
  kCVReturnPoolAllocationFailed = -6690;
  kCVReturnInvalidPoolAttributes = -6691;
  kCVReturnRetry = -6692;
  kCVReturnLast = -6699;
  kCVAttachmentMode_ShouldNotPropagate = 0;
  kCVAttachmentMode_ShouldPropagate = 1;
  kCVPixelFormatType_1Monochrome = 1;
  kCVPixelFormatType_2Indexed = 2;
  kCVPixelFormatType_4Indexed = 4;
  kCVPixelFormatType_8Indexed = 8;
  kCVPixelFormatType_1IndexedGray_WhiteIsZero = 33;
  kCVPixelFormatType_2IndexedGray_WhiteIsZero = 34;
  kCVPixelFormatType_4IndexedGray_WhiteIsZero = 36;
  kCVPixelFormatType_8IndexedGray_WhiteIsZero = 40;
  kCVPixelFormatType_16BE555 = 16;
  kCVPixelFormatType_16LE555 = 1278555445;
  kCVPixelFormatType_16LE5551 = 892679473;
  kCVPixelFormatType_16BE565 = 1110783541;
  kCVPixelFormatType_16LE565 = 1278555701;
  kCVPixelFormatType_24RGB = 24;
  kCVPixelFormatType_24BGR = 842285639;
  kCVPixelFormatType_32ARGB = 32;
  kCVPixelFormatType_32BGRA = 1111970369;
  kCVPixelFormatType_32ABGR = 1094862674;
  kCVPixelFormatType_32RGBA = 1380401729;
  kCVPixelFormatType_64ARGB = 1647719521;
  kCVPixelFormatType_64RGBALE = 1815491698;
  kCVPixelFormatType_48RGB = 1647589490;
  kCVPixelFormatType_32AlphaGray = 1647522401;
  kCVPixelFormatType_16Gray = 1647392359;
  kCVPixelFormatType_30RGB = 1378955371;
  kCVPixelFormatType_30RGB_r210 = 1915892016;
  kCVPixelFormatType_422YpCbCr8 = 846624121;
  kCVPixelFormatType_4444YpCbCrA8 = 1983131704;
  kCVPixelFormatType_4444YpCbCrA8R = 1916022840;
  kCVPixelFormatType_4444AYpCbCr8 = 2033463352;
  kCVPixelFormatType_4444AYpCbCr16 = 2033463606;
  kCVPixelFormatType_4444AYpCbCrFloat = 1916036716;
  kCVPixelFormatType_444YpCbCr8 = 1983066168;
  kCVPixelFormatType_422YpCbCr16 = 1983000886;
  kCVPixelFormatType_422YpCbCr10 = 1983000880;
  kCVPixelFormatType_444YpCbCr10 = 1983131952;
  kCVPixelFormatType_420YpCbCr8Planar = 2033463856;
  kCVPixelFormatType_420YpCbCr8PlanarFullRange = 1714696752;
  kCVPixelFormatType_422YpCbCr_4A_8BiPlanar = 1630697081;
  kCVPixelFormatType_420YpCbCr8BiPlanarVideoRange = 875704438;
  kCVPixelFormatType_420YpCbCr8BiPlanarFullRange = 875704422;
  kCVPixelFormatType_422YpCbCr8BiPlanarVideoRange = 875704950;
  kCVPixelFormatType_422YpCbCr8BiPlanarFullRange = 875704934;
  kCVPixelFormatType_444YpCbCr8BiPlanarVideoRange = 875836534;
  kCVPixelFormatType_444YpCbCr8BiPlanarFullRange = 875836518;
  kCVPixelFormatType_422YpCbCr8_yuvs = 2037741171;
  kCVPixelFormatType_422YpCbCr8FullRange = 2037741158;
  kCVPixelFormatType_OneComponent8 = 1278226488;
  kCVPixelFormatType_TwoComponent8 = 843264056;
  kCVPixelFormatType_30RGBLEPackedWideGamut = 1999843442;
  kCVPixelFormatType_ARGB2101010LEPacked = 1815162994;
  kCVPixelFormatType_40ARGBLEWideGamut = 1999908961;
  kCVPixelFormatType_40ARGBLEWideGamutPremultiplied = 1999908973;
  kCVPixelFormatType_OneComponent10 = 1278226736;
  kCVPixelFormatType_OneComponent12 = 1278226738;
  kCVPixelFormatType_OneComponent16 = 1278226742;
  kCVPixelFormatType_TwoComponent16 = 843264310;
  kCVPixelFormatType_OneComponent16Half = 1278226536;
  kCVPixelFormatType_OneComponent32Float = 1278226534;
  kCVPixelFormatType_TwoComponent16Half = 843264104;
  kCVPixelFormatType_TwoComponent32Float = 843264102;
  kCVPixelFormatType_64RGBAHalf = 1380411457;
  kCVPixelFormatType_128RGBAFloat = 1380410945;
  kCVPixelFormatType_14Bayer_GRBG = 1735549492;
  kCVPixelFormatType_14Bayer_RGGB = 1919379252;
  kCVPixelFormatType_14Bayer_BGGR = 1650943796;
  kCVPixelFormatType_14Bayer_GBRG = 1734505012;
  kCVPixelFormatType_DisparityFloat16 = 1751411059;
  kCVPixelFormatType_DisparityFloat32 = 1717856627;
  kCVPixelFormatType_DepthFloat16 = 1751410032;
  kCVPixelFormatType_DepthFloat32 = 1717855600;
  kCVPixelFormatType_420YpCbCr10BiPlanarVideoRange = 2016686640;
  kCVPixelFormatType_422YpCbCr10BiPlanarVideoRange = 2016686642;
  kCVPixelFormatType_444YpCbCr10BiPlanarVideoRange = 2016687156;
  kCVPixelFormatType_420YpCbCr10BiPlanarFullRange = 2019963440;
  kCVPixelFormatType_422YpCbCr10BiPlanarFullRange = 2019963442;
  kCVPixelFormatType_444YpCbCr10BiPlanarFullRange = 2019963956;
  kCVPixelFormatType_420YpCbCr8VideoRange_8A_TriPlanar = 1982882104;
  kCVPixelFormatType_16VersatileBayer = 1651519798;
  kCVPixelFormatType_64RGBA_DownscaledProResRAW = 1651521076;
  kCVPixelFormatType_422YpCbCr16BiPlanarVideoRange = 1937125938;
  kCVPixelFormatType_444YpCbCr16BiPlanarVideoRange = 1937126452;
  kCVPixelFormatType_444YpCbCr16VideoRange_16A_TriPlanar = 1932812659;
  kCVPixelFormatType_Lossless_32BGRA = 641877825;
  kCVPixelFormatType_Lossless_64RGBAHalf = 642934849;
  kCVPixelFormatType_Lossless_420YpCbCr8BiPlanarVideoRange = 641234480;
  kCVPixelFormatType_Lossless_420YpCbCr8BiPlanarFullRange = 641230384;
  kCVPixelFormatType_Lossless_420YpCbCr10PackedBiPlanarVideoRange = 645428784;
  kCVPixelFormatType_Lossless_422YpCbCr10PackedBiPlanarVideoRange = 645428786;
  kCVPixelFormatType_Lossless_420YpCbCr10PackedBiPlanarFullRange = 645424688;
  kCVPixelFormatType_Lossy_32BGRA = 759318337;
  kCVPixelFormatType_Lossy_420YpCbCr8BiPlanarVideoRange = 758674992;
  kCVPixelFormatType_Lossy_420YpCbCr8BiPlanarFullRange = 758670896;
  kCVPixelFormatType_Lossy_420YpCbCr10PackedBiPlanarVideoRange = 762869296;
  kCVPixelFormatType_Lossy_422YpCbCr10PackedBiPlanarVideoRange = 762869298;
  kCVPixelBufferLock_ReadOnly = 1;
  kCVVersatileBayer_BayerPattern_RGGB = 0;
  kCVVersatileBayer_BayerPattern_GRBG = 1;
  kCVVersatileBayer_BayerPattern_GBRG = 2;
  kCVVersatileBayer_BayerPattern_BGGR = 3;
  kCVPixelBufferPoolFlushExcessBuffers = 1;

type
  PNativeUInt = ^NativeUInt;
  P__CVDisplayLink = Pointer;
  PP__CVDisplayLink = ^P__CVDisplayLink;
  P__CVBuffer = Pointer;
  PP__CVBuffer = ^P__CVBuffer;
  P__CVPixelBufferPool = Pointer;
  PP__CVPixelBufferPool = ^P__CVPixelBufferPool;
  PCVSMPTETime = ^CVSMPTETime;
  PCVTime = ^CVTime;
  PCVTimeStamp = ^CVTimeStamp;
  PCVPlanarComponentInfo = ^CVPlanarComponentInfo;
  PCVPlanarPixelBufferInfo = ^CVPlanarPixelBufferInfo;
  PCVPlanarPixelBufferInfo_YCbCrPlanar = ^CVPlanarPixelBufferInfo_YCbCrPlanar;
  PCVPlanarPixelBufferInfo_YCbCrBiPlanar = ^CVPlanarPixelBufferInfo_YCbCrBiPlanar;
  // PCVFillExtendedPixelsCallBackData = ^CVFillExtendedPixelsCallBackData;

  CVOptionFlags = UInt64;
  PCVOptionFlags = ^CVOptionFlags;

  CVSMPTETime = record
    subframes: SInt16;
    subframeDivisor: SInt16;
    counter: UInt32;
    &type: UInt32;
    flags: UInt32;
    hours: SInt16;
    minutes: SInt16;
    seconds: SInt16;
    frames: SInt16;
  end;

  CVSMPTETimeType = NSInteger;
  CVSMPTETimeFlags = NSInteger;
  CVTimeFlags = NSInteger;

  CVTime = record
    timeValue: Int64;
    timeScale: Int32;
    flags: Int32;
  end;

  CVTimeStampFlags = NSInteger;
  CVReturn = Int32;
  CVDisplayLinkRef = Pointer;
  PCVDisplayLinkRef = ^CVDisplayLinkRef;

  CVDisplayLinkOutputCallback = function(displayLink: CVDisplayLinkRef; inNow: PCVTimeStamp; inOutputTime: PCVTimeStamp; flagsIn: CVOptionFlags;
    flagsOut: PCVOptionFlags; displayLinkContext: Pointer): CVReturn; cdecl;

  CVDisplayLinkOutputHandler = function(displayLink: CVDisplayLinkRef; inNow: PCVTimeStamp; inOutputTime: PCVTimeStamp; flagsIn: CVOptionFlags;
    flagsOut: PCVOptionFlags): CVReturn of object;
  CVAttachmentMode = NSInteger;
  CVBufferRef = Pointer;
  PCVBufferRef = ^CVBufferRef;
  CVImageBufferRef = Pointer;
  PCVImageBufferRef = ^CVImageBufferRef;
  CVPixelBufferLockFlags = NSInteger;

  CVPlanarComponentInfo = record
    offset: Int32;
    rowBytes: UInt32;
  end;

  CVPlanarPixelBufferInfo = record
    componentInfo: array [0..0] of CVPlanarComponentInfo;
  end;

  CVPlanarPixelBufferInfo_YCbCrPlanar = record
    componentInfoY: CVPlanarComponentInfo;
    componentInfoCb: CVPlanarComponentInfo;
    componentInfoCr: CVPlanarComponentInfo;
  end;

  CVPlanarPixelBufferInfo_YCbCrBiPlanar = record
    componentInfoY: CVPlanarComponentInfo;
    componentInfoCbCr: CVPlanarComponentInfo;
  end;

  CVPixelBufferRef = Pointer;
  PCVPixelBufferRef = ^CVPixelBufferRef;

  CVPixelBufferReleaseBytesCallback = procedure(releaseRefCon: Pointer; baseAddress: Pointer); cdecl;

  CVPixelBufferReleasePlanarBytesCallback = procedure(releaseRefCon: Pointer; dataPtr: Pointer; dataSize: NativeUInt; numberOfPlanes: NativeUInt;
    planeAddresses: PPointer); cdecl;
  CVPixelBufferPoolRef = Pointer;
  PCVPixelBufferPoolRef = ^CVPixelBufferPoolRef;
  CVPixelBufferPoolFlushFlags = NSInteger;

  CVFillExtendedPixelsCallBack = function(pixelBuffer: CVPixelBufferRef; refCon: Pointer): Boolean; cdecl;

function kCVBufferPropagatedAttachmentsKey: NSString;
function kCVBufferNonPropagatedAttachmentsKey: NSString;
function kCVBufferMovieTimeKey: NSString;
function kCVBufferTimeValueKey: NSString;
function kCVBufferTimeScaleKey: NSString;
function kCVImageBufferCGColorSpaceKey: NSString;
function kCVImageBufferCleanApertureKey: NSString;
function kCVImageBufferCleanApertureWidthKey: NSString;
function kCVImageBufferCleanApertureHeightKey: NSString;
function kCVImageBufferCleanApertureHorizontalOffsetKey: NSString;
function kCVImageBufferCleanApertureVerticalOffsetKey: NSString;
function kCVImageBufferPreferredCleanApertureKey: NSString;
function kCVImageBufferFieldCountKey: NSString;
function kCVImageBufferFieldDetailKey: NSString;
function kCVImageBufferFieldDetailTemporalTopFirst: NSString;
function kCVImageBufferFieldDetailTemporalBottomFirst: NSString;
function kCVImageBufferFieldDetailSpatialFirstLineEarly: NSString;
function kCVImageBufferFieldDetailSpatialFirstLineLate: NSString;
function kCVImageBufferPixelAspectRatioKey: NSString;
function kCVImageBufferPixelAspectRatioHorizontalSpacingKey: NSString;
function kCVImageBufferPixelAspectRatioVerticalSpacingKey: NSString;
function kCVImageBufferDisplayDimensionsKey: NSString;
function kCVImageBufferDisplayWidthKey: NSString;
function kCVImageBufferDisplayHeightKey: NSString;
function kCVImageBufferGammaLevelKey: NSString;
function kCVImageBufferICCProfileKey: NSString;
function kCVImageBufferYCbCrMatrixKey: NSString;
function kCVImageBufferYCbCrMatrix_ITU_R_709_2: NSString;
function kCVImageBufferYCbCrMatrix_ITU_R_601_4: NSString;
function kCVImageBufferYCbCrMatrix_SMPTE_240M_1995: NSString;
function kCVImageBufferYCbCrMatrix_DCI_P3: NSString;
function kCVImageBufferYCbCrMatrix_P3_D65: NSString;
function kCVImageBufferYCbCrMatrix_ITU_R_2020: NSString;
function kCVImageBufferColorPrimariesKey: NSString;
function kCVImageBufferColorPrimaries_ITU_R_709_2: NSString;
function kCVImageBufferColorPrimaries_EBU_3213: NSString;
function kCVImageBufferColorPrimaries_SMPTE_C: NSString;
function kCVImageBufferColorPrimaries_P22: NSString;
function kCVImageBufferColorPrimaries_DCI_P3: NSString;
function kCVImageBufferColorPrimaries_P3_D65: NSString;
function kCVImageBufferColorPrimaries_ITU_R_2020: NSString;
function kCVImageBufferTransferFunctionKey: NSString;
function kCVImageBufferTransferFunction_ITU_R_709_2: NSString;
function kCVImageBufferTransferFunction_SMPTE_240M_1995: NSString;
function kCVImageBufferTransferFunction_UseGamma: NSString;
function kCVImageBufferTransferFunction_EBU_3213: NSString;
function kCVImageBufferTransferFunction_SMPTE_C: NSString;
function kCVImageBufferTransferFunction_sRGB: NSString;
function kCVImageBufferTransferFunction_ITU_R_2020: NSString;
function kCVImageBufferTransferFunction_SMPTE_ST_428_1: NSString;
function kCVImageBufferTransferFunction_SMPTE_ST_2084_PQ: NSString;
function kCVImageBufferTransferFunction_ITU_R_2100_HLG: NSString;
function kCVImageBufferTransferFunction_Linear: NSString;
function kCVImageBufferChromaLocationTopFieldKey: NSString;
function kCVImageBufferChromaLocationBottomFieldKey: NSString;
function kCVImageBufferChromaLocation_Left: NSString;
function kCVImageBufferChromaLocation_Center: NSString;
function kCVImageBufferChromaLocation_TopLeft: NSString;
function kCVImageBufferChromaLocation_Top: NSString;
function kCVImageBufferChromaLocation_BottomLeft: NSString;
function kCVImageBufferChromaLocation_Bottom: NSString;
function kCVImageBufferChromaLocation_DV420: NSString;
function kCVImageBufferChromaSubsamplingKey: NSString;
function kCVImageBufferChromaSubsampling_420: NSString;
function kCVImageBufferChromaSubsampling_422: NSString;
function kCVImageBufferChromaSubsampling_411: NSString;
function kCVImageBufferAlphaChannelIsOpaque: NSString;
function kCVImageBufferAlphaChannelModeKey: NSString;
function kCVImageBufferAlphaChannelMode_StraightAlpha: NSString;
function kCVImageBufferAlphaChannelMode_PremultipliedAlpha: NSString;
function kCVImageBufferPostDecodeProcessingSequenceMetadataKey: NSString;
function kCVImageBufferPostDecodeProcessingFrameMetadataKey: NSString;
function kCVImageBufferMasteringDisplayColorVolumeKey: NSString;
function kCVImageBufferContentLightLevelInfoKey: NSString;
function kCVImageBufferAmbientViewingEnvironmentKey: NSString;
function kCVImageBufferSceneIlluminationKey: NSString;
function kCVImageBufferRegionOfInterestKey: NSString;
function kCVImageBufferLogTransferFunctionKey: NSString;
function kCVImageBufferLogTransferFunction_AppleLog: NSString;
function kCVPixelBufferPixelFormatTypeKey: NSString;
function kCVPixelBufferMemoryAllocatorKey: NSString;
function kCVPixelBufferWidthKey: NSString;
function kCVPixelBufferHeightKey: NSString;
function kCVPixelBufferExtendedPixelsLeftKey: NSString;
function kCVPixelBufferExtendedPixelsTopKey: NSString;
function kCVPixelBufferExtendedPixelsRightKey: NSString;
function kCVPixelBufferExtendedPixelsBottomKey: NSString;
function kCVPixelBufferBytesPerRowAlignmentKey: NSString;
function kCVPixelBufferCGBitmapContextCompatibilityKey: NSString;
function kCVPixelBufferCGImageCompatibilityKey: NSString;
function kCVPixelBufferOpenGLCompatibilityKey: NSString;
function kCVPixelBufferPlaneAlignmentKey: NSString;
function kCVPixelBufferIOSurfacePropertiesKey: NSString;
function kCVPixelBufferOpenGLESCompatibilityKey: NSString;
function kCVPixelBufferMetalCompatibilityKey: NSString;
function kCVPixelBufferOpenGLTextureCacheCompatibilityKey: NSString;
function kCVPixelBufferOpenGLESTextureCacheCompatibilityKey: NSString;
function kCVPixelBufferVersatileBayerKey_BayerPattern: NSString;
function kCVPixelBufferProResRAWKey_SenselSitingOffsets: NSString;
function kCVPixelBufferProResRAWKey_BlackLevel: NSString;
function kCVPixelBufferProResRAWKey_WhiteLevel: NSString;
function kCVPixelBufferProResRAWKey_WhiteBalanceCCT: NSString;
function kCVPixelBufferProResRAWKey_WhiteBalanceRedFactor: NSString;
function kCVPixelBufferProResRAWKey_WhiteBalanceBlueFactor: NSString;
function kCVPixelBufferProResRAWKey_ColorMatrix: NSString;
function kCVPixelBufferProResRAWKey_GainFactor: NSString;
function kCVPixelBufferProResRAWKey_RecommendedCrop: NSString;
function kCVPixelBufferProResRAWKey_MetadataExtension: NSString;
function kCVPixelBufferIOSurfaceOpenGLTextureCompatibilityKey: NSString;
function kCVPixelBufferIOSurfaceOpenGLFBOCompatibilityKey: NSString;
function kCVPixelBufferIOSurfaceCoreAnimationCompatibilityKey: NSString;
function kCVPixelBufferIOSurfaceOpenGLESTextureCompatibilityKey: NSString;
function kCVPixelBufferIOSurfaceOpenGLESFBOCompatibilityKey: NSString;
function kCVPixelBufferPoolMinimumBufferCountKey: NSString;
function kCVPixelBufferPoolMaximumBufferAgeKey: NSString;
function kCVPixelBufferPoolAllocationThresholdKey: NSString;
function kCVPixelBufferPoolFreeBufferNotification: NSString;
function kCVMetalTextureCacheMaximumTextureAgeKey: NSString;
function kCVPixelFormatName: NSString;
function kCVPixelFormatConstant: NSString;
function kCVPixelFormatCodecType: NSString;
function kCVPixelFormatFourCC: NSString;
function kCVPixelFormatContainsAlpha: NSString;
function kCVPixelFormatContainsYCbCr: NSString;
function kCVPixelFormatContainsRGB: NSString;
function kCVPixelFormatContainsGrayscale: NSString;
function kCVPixelFormatContainsSenselArray: NSString;
function kCVPixelFormatComponentRange: NSString;
function kCVPixelFormatComponentRange_VideoRange: NSString;
function kCVPixelFormatComponentRange_FullRange: NSString;
function kCVPixelFormatComponentRange_WideRange: NSString;
function kCVPixelFormatPlanes: NSString;
function kCVPixelFormatBlockWidth: NSString;
function kCVPixelFormatBlockHeight: NSString;
function kCVPixelFormatBitsPerBlock: NSString;
function kCVPixelFormatBitsPerComponent: NSString;
function kCVPixelFormatBlockHorizontalAlignment: NSString;
function kCVPixelFormatBlockVerticalAlignment: NSString;
function kCVPixelFormatBlackBlock: NSString;
function kCVPixelFormatHorizontalSubsampling: NSString;
function kCVPixelFormatVerticalSubsampling: NSString;
function kCVPixelFormatOpenGLFormat: NSString;
function kCVPixelFormatOpenGLType: NSString;
function kCVPixelFormatOpenGLInternalFormat: NSString;
function kCVPixelFormatCGBitmapInfo: NSString;
function kCVPixelFormatQDCompatibility: NSString;
function kCVPixelFormatCGBitmapContextCompatibility: NSString;
function kCVPixelFormatCGImageCompatibility: NSString;
function kCVPixelFormatOpenGLCompatibility: NSString;
function kCVPixelFormatOpenGLESCompatibility: NSString;
function kCVPixelFormatFillExtendedPixelsCallback: NSString;

const
  libCoreVideo = '/System/Library/Frameworks/CoreVideo.framework/CoreVideo';

function CVGetCurrentHostTime: UInt64; cdecl;
  external libCoreVideo name _PU + 'CVGetCurrentHostTime';

function CVGetHostClockFrequency: Double; cdecl;
  external libCoreVideo name _PU + 'CVGetHostClockFrequency';

function CVGetHostClockMinimumTimeDelta: UInt32; cdecl;
  external libCoreVideo name _PU + 'CVGetHostClockMinimumTimeDelta';

function CVDisplayLinkGetTypeID: CFTypeID; cdecl;
  external libCoreVideo name _PU + 'CVDisplayLinkGetTypeID';

function CVDisplayLinkCreateWithCGDisplays(displayArray: PCGDirectDisplayID; count: CFIndex; displayLinkOut: PCVDisplayLinkRef): CVReturn; cdecl;
  external libCoreVideo name _PU + 'CVDisplayLinkCreateWithCGDisplays';

function CVDisplayLinkCreateWithOpenGLDisplayMask(mask: CGOpenGLDisplayMask; displayLinkOut: PCVDisplayLinkRef): CVReturn; cdecl;
  external libCoreVideo name _PU + 'CVDisplayLinkCreateWithOpenGLDisplayMask';

function CVDisplayLinkCreateWithCGDisplay(displayID: CGDirectDisplayID; displayLinkOut: PCVDisplayLinkRef): CVReturn; cdecl;
  external libCoreVideo name _PU + 'CVDisplayLinkCreateWithCGDisplay';

function CVDisplayLinkCreateWithActiveCGDisplays(displayLinkOut: PCVDisplayLinkRef): CVReturn; cdecl;
  external libCoreVideo name _PU + 'CVDisplayLinkCreateWithActiveCGDisplays';

function CVDisplayLinkSetCurrentCGDisplay(displayLink: CVDisplayLinkRef; displayID: CGDirectDisplayID): CVReturn; cdecl;
  external libCoreVideo name _PU + 'CVDisplayLinkSetCurrentCGDisplay';

function CVDisplayLinkSetCurrentCGDisplayFromOpenGLContext(displayLink: CVDisplayLinkRef; cglContext: CGLContextObj;
  cglPixelFormat: CGLPixelFormatObj): CVReturn; cdecl;
  external libCoreVideo name _PU + 'CVDisplayLinkSetCurrentCGDisplayFromOpenGLContext';

function CVDisplayLinkGetCurrentCGDisplay(displayLink: CVDisplayLinkRef): CGDirectDisplayID; cdecl;
  external libCoreVideo name _PU + 'CVDisplayLinkGetCurrentCGDisplay';

function CVDisplayLinkSetOutputCallback(displayLink: CVDisplayLinkRef; callback: CVDisplayLinkOutputCallback; userInfo: Pointer): CVReturn; cdecl;
  external libCoreVideo name _PU + 'CVDisplayLinkSetOutputCallback';

function CVDisplayLinkSetOutputHandler(displayLink: CVDisplayLinkRef; handler: CVDisplayLinkOutputHandler): CVReturn; cdecl;
  external libCoreVideo name _PU + 'CVDisplayLinkSetOutputHandler';

function CVDisplayLinkStart(displayLink: CVDisplayLinkRef): CVReturn; cdecl;
  external libCoreVideo name _PU + 'CVDisplayLinkStart';

function CVDisplayLinkStop(displayLink: CVDisplayLinkRef): CVReturn; cdecl;
  external libCoreVideo name _PU + 'CVDisplayLinkStop';

function CVDisplayLinkGetNominalOutputVideoRefreshPeriod(displayLink: CVDisplayLinkRef): CVTime; cdecl;
  external libCoreVideo name _PU + 'CVDisplayLinkGetNominalOutputVideoRefreshPeriod';

function CVDisplayLinkGetOutputVideoLatency(displayLink: CVDisplayLinkRef): CVTime; cdecl;
  external libCoreVideo name _PU + 'CVDisplayLinkGetOutputVideoLatency';

function CVDisplayLinkGetActualOutputVideoRefreshPeriod(displayLink: CVDisplayLinkRef): Double; cdecl;
  external libCoreVideo name _PU + 'CVDisplayLinkGetActualOutputVideoRefreshPeriod';

function CVDisplayLinkIsRunning(displayLink: CVDisplayLinkRef): Boolean; cdecl;
  external libCoreVideo name _PU + 'CVDisplayLinkIsRunning';

function CVDisplayLinkGetCurrentTime(displayLink: CVDisplayLinkRef; outTime: PCVTimeStamp): CVReturn; cdecl;
  external libCoreVideo name _PU + 'CVDisplayLinkGetCurrentTime';

function CVDisplayLinkTranslateTime(displayLink: CVDisplayLinkRef; inTime: PCVTimeStamp; outTime: PCVTimeStamp): CVReturn; cdecl;
  external libCoreVideo name _PU + 'CVDisplayLinkTranslateTime';

function CVDisplayLinkRetain(displayLink: CVDisplayLinkRef): CVDisplayLinkRef; cdecl;
  external libCoreVideo name _PU + 'CVDisplayLinkRetain';

procedure CVDisplayLinkRelease(displayLink: CVDisplayLinkRef); cdecl;
  external libCoreVideo name _PU + 'CVDisplayLinkRelease';

function CVBufferRetain(buffer: CVBufferRef): CVBufferRef; cdecl;
  external libCoreVideo name _PU + 'CVBufferRetain';

procedure CVBufferRelease(buffer: CVBufferRef); cdecl;
  external libCoreVideo name _PU + 'CVBufferRelease';

procedure CVBufferSetAttachment(buffer: CVBufferRef; key: CFStringRef; value: CFTypeRef; attachmentMode: CVAttachmentMode); cdecl;
  external libCoreVideo name _PU + 'CVBufferSetAttachment';

function CVBufferGetAttachment(buffer: CVBufferRef; key: CFStringRef; attachmentMode: PCVAttachmentMode): CFTypeRef; cdecl;
  external libCoreVideo name _PU + 'CVBufferGetAttachment';

procedure CVBufferRemoveAttachment(buffer: CVBufferRef; key: CFStringRef); cdecl;
  external libCoreVideo name _PU + 'CVBufferRemoveAttachment';

procedure CVBufferRemoveAllAttachments(buffer: CVBufferRef); cdecl;
  external libCoreVideo name _PU + 'CVBufferRemoveAllAttachments';

function CVBufferGetAttachments(buffer: CVBufferRef; attachmentMode: CVAttachmentMode): CFDictionaryRef; cdecl;
  external libCoreVideo name _PU + 'CVBufferGetAttachments';

procedure CVBufferSetAttachments(buffer: CVBufferRef; theAttachments: CFDictionaryRef; attachmentMode: CVAttachmentMode); cdecl;
  external libCoreVideo name _PU + 'CVBufferSetAttachments';

procedure CVBufferPropagateAttachments(sourceBuffer: CVBufferRef; destinationBuffer: CVBufferRef); cdecl;
  external libCoreVideo name _PU + 'CVBufferPropagateAttachments';

function CVBufferCopyAttachments(buffer: CVBufferRef; attachmentMode: CVAttachmentMode): CFDictionaryRef; cdecl;
  external libCoreVideo name _PU + 'CVBufferCopyAttachments';

function CVBufferCopyAttachment(buffer: CVBufferRef; key: CFStringRef; attachmentMode: PCVAttachmentMode): CFTypeRef; cdecl;
  external libCoreVideo name _PU + 'CVBufferCopyAttachment';

function CVBufferHasAttachment(buffer: CVBufferRef; key: CFStringRef): Boolean; cdecl;
  external libCoreVideo name _PU + 'CVBufferHasAttachment';

function CVYCbCrMatrixGetIntegerCodePointForString(yCbCrMatrixString: CFStringRef): Integer; cdecl;
  external libCoreVideo name _PU + 'CVYCbCrMatrixGetIntegerCodePointForString';

function CVColorPrimariesGetIntegerCodePointForString(colorPrimariesString: CFStringRef): Integer; cdecl;
  external libCoreVideo name _PU + 'CVColorPrimariesGetIntegerCodePointForString';

function CVTransferFunctionGetIntegerCodePointForString(transferFunctionString: CFStringRef): Integer; cdecl;
  external libCoreVideo name _PU + 'CVTransferFunctionGetIntegerCodePointForString';

function CVYCbCrMatrixGetStringForIntegerCodePoint(yCbCrMatrixCodePoint: Integer): CFStringRef; cdecl;
  external libCoreVideo name _PU + 'CVYCbCrMatrixGetStringForIntegerCodePoint';

function CVColorPrimariesGetStringForIntegerCodePoint(colorPrimariesCodePoint: Integer): CFStringRef; cdecl;
  external libCoreVideo name _PU + 'CVColorPrimariesGetStringForIntegerCodePoint';

function CVTransferFunctionGetStringForIntegerCodePoint(transferFunctionCodePoint: Integer): CFStringRef; cdecl;
  external libCoreVideo name _PU + 'CVTransferFunctionGetStringForIntegerCodePoint';

function CVImageBufferGetEncodedSize(imageBuffer: CVImageBufferRef): CGSize; cdecl;
  external libCoreVideo name _PU + 'CVImageBufferGetEncodedSize';

function CVImageBufferGetDisplaySize(imageBuffer: CVImageBufferRef): CGSize; cdecl;
  external libCoreVideo name _PU + 'CVImageBufferGetDisplaySize';

function CVImageBufferGetCleanRect(imageBuffer: CVImageBufferRef): CGRect; cdecl;
  external libCoreVideo name _PU + 'CVImageBufferGetCleanRect';

function CVImageBufferIsFlipped(imageBuffer: CVImageBufferRef): Boolean; cdecl;
  external libCoreVideo name _PU + 'CVImageBufferIsFlipped';

function CVImageBufferGetColorSpace(imageBuffer: CVImageBufferRef): CGColorSpaceRef; cdecl;
  external libCoreVideo name _PU + 'CVImageBufferGetColorSpace';

function CVImageBufferCreateColorSpaceFromAttachments(attachments: CFDictionaryRef): CGColorSpaceRef; cdecl;
  external libCoreVideo name _PU + 'CVImageBufferCreateColorSpaceFromAttachments';

function CVPixelBufferGetTypeID: CFTypeID; cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferGetTypeID';

function CVPixelBufferRetain(texture: CVPixelBufferRef): CVPixelBufferRef; cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferRetain';

procedure CVPixelBufferRelease(texture: CVPixelBufferRef); cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferRelease';

function CVPixelBufferCreateResolvedAttributesDictionary(allocator: CFAllocatorRef; attributes: CFArrayRef;
  resolvedDictionaryOut: PCFDictionaryRef): CVReturn; cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferCreateResolvedAttributesDictionary';

function CVPixelBufferCreate(allocator: CFAllocatorRef; width: NativeUInt; height: NativeUInt; pixelFormatType: OSType;
  pixelBufferAttributes: CFDictionaryRef; pixelBufferOut: PCVPixelBufferRef): CVReturn; cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferCreate';

function CVPixelBufferCreateWithBytes(allocator: CFAllocatorRef; width: NativeUInt; height: NativeUInt; pixelFormatType: OSType;
  baseAddress: Pointer; bytesPerRow: NativeUInt; releaseCallback: CVPixelBufferReleaseBytesCallback; releaseRefCon: Pointer;
  pixelBufferAttributes: CFDictionaryRef; pixelBufferOut: PCVPixelBufferRef): CVReturn; cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferCreateWithBytes';

function CVPixelBufferCreateWithPlanarBytes(allocator: CFAllocatorRef; width: NativeUInt; height: NativeUInt; pixelFormatType: OSType;
  dataPtr: Pointer; dataSize: NativeUInt; numberOfPlanes: NativeUInt; planeBaseAddress: PPointer; planeWidth: PNativeUInt; planeHeight: PNativeUInt;
  planeBytesPerRow: PNativeUInt; releaseCallback: CVPixelBufferReleasePlanarBytesCallback; releaseRefCon: Pointer;
  pixelBufferAttributes: CFDictionaryRef; pixelBufferOut: PCVPixelBufferRef): CVReturn; cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferCreateWithPlanarBytes';

function CVPixelBufferLockBaseAddress(pixelBuffer: CVPixelBufferRef; lockFlags: CVPixelBufferLockFlags): CVReturn; cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferLockBaseAddress';

function CVPixelBufferUnlockBaseAddress(pixelBuffer: CVPixelBufferRef; unlockFlags: CVPixelBufferLockFlags): CVReturn; cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferUnlockBaseAddress';

function CVPixelBufferGetWidth(pixelBuffer: CVPixelBufferRef): NativeUInt; cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferGetWidth';

function CVPixelBufferGetHeight(pixelBuffer: CVPixelBufferRef): NativeUInt; cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferGetHeight';

function CVPixelBufferGetPixelFormatType(pixelBuffer: CVPixelBufferRef): OSType; cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferGetPixelFormatType';

function CVPixelBufferGetBaseAddress(pixelBuffer: CVPixelBufferRef): Pointer; cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferGetBaseAddress';

function CVPixelBufferGetBytesPerRow(pixelBuffer: CVPixelBufferRef): NativeUInt; cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferGetBytesPerRow';

function CVPixelBufferGetDataSize(pixelBuffer: CVPixelBufferRef): NativeUInt; cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferGetDataSize';

function CVPixelBufferIsPlanar(pixelBuffer: CVPixelBufferRef): Boolean; cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferIsPlanar';

function CVPixelBufferGetPlaneCount(pixelBuffer: CVPixelBufferRef): NativeUInt; cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferGetPlaneCount';

function CVPixelBufferGetWidthOfPlane(pixelBuffer: CVPixelBufferRef; planeIndex: NativeUInt): NativeUInt; cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferGetWidthOfPlane';

function CVPixelBufferGetHeightOfPlane(pixelBuffer: CVPixelBufferRef; planeIndex: NativeUInt): NativeUInt; cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferGetHeightOfPlane';

function CVPixelBufferGetBaseAddressOfPlane(pixelBuffer: CVPixelBufferRef; planeIndex: NativeUInt): Pointer; cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferGetBaseAddressOfPlane';

function CVPixelBufferGetBytesPerRowOfPlane(pixelBuffer: CVPixelBufferRef; planeIndex: NativeUInt): NativeUInt; cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferGetBytesPerRowOfPlane';

procedure CVPixelBufferGetExtendedPixels(pixelBuffer: CVPixelBufferRef; extraColumnsOnLeft: PNativeUInt; extraColumnsOnRight: PNativeUInt;
  extraRowsOnTop: PNativeUInt; extraRowsOnBottom: PNativeUInt); cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferGetExtendedPixels';

function CVPixelBufferFillExtendedPixels(pixelBuffer: CVPixelBufferRef): CVReturn; cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferFillExtendedPixels';

function CVPixelBufferCopyCreationAttributes(pixelBuffer: CVPixelBufferRef): CFDictionaryRef; cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferCopyCreationAttributes';

function CVPixelBufferGetIOSurface(pixelBuffer: CVPixelBufferRef): IOSurfaceRef; cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferGetIOSurface';

function CVPixelBufferCreateWithIOSurface(allocator: CFAllocatorRef; surface: IOSurfaceRef; pixelBufferAttributes: CFDictionaryRef;
  pixelBufferOut: PCVPixelBufferRef): CVReturn; cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferCreateWithIOSurface';

function CVPixelBufferPoolGetTypeID: CFTypeID; cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferPoolGetTypeID';

function CVPixelBufferPoolRetain(pixelBufferPool: CVPixelBufferPoolRef): CVPixelBufferPoolRef; cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferPoolRetain';

procedure CVPixelBufferPoolRelease(pixelBufferPool: CVPixelBufferPoolRef); cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferPoolRelease';

function CVPixelBufferPoolCreate(allocator: CFAllocatorRef; poolAttributes: CFDictionaryRef; pixelBufferAttributes: CFDictionaryRef;
  poolOut: PCVPixelBufferPoolRef): CVReturn; cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferPoolCreate';

function CVPixelBufferPoolGetAttributes(pool: CVPixelBufferPoolRef): CFDictionaryRef; cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferPoolGetAttributes';

function CVPixelBufferPoolGetPixelBufferAttributes(pool: CVPixelBufferPoolRef): CFDictionaryRef; cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferPoolGetPixelBufferAttributes';

function CVPixelBufferPoolCreatePixelBuffer(allocator: CFAllocatorRef; pixelBufferPool: CVPixelBufferPoolRef;
  pixelBufferOut: PCVPixelBufferRef): CVReturn; cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferPoolCreatePixelBuffer';

function CVPixelBufferPoolCreatePixelBufferWithAuxAttributes(allocator: CFAllocatorRef; pixelBufferPool: CVPixelBufferPoolRef;
  auxAttributes: CFDictionaryRef; pixelBufferOut: PCVPixelBufferRef): CVReturn; cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferPoolCreatePixelBufferWithAuxAttributes';

procedure CVPixelBufferPoolFlush(pool: CVPixelBufferPoolRef; options: CVPixelBufferPoolFlushFlags); cdecl;
  external libCoreVideo name _PU + 'CVPixelBufferPoolFlush';

function CVPixelFormatDescriptionCreateWithPixelFormatType(allocator: CFAllocatorRef; pixelFormat: OSType): CFDictionaryRef; cdecl;
  external libCoreVideo name _PU + 'CVPixelFormatDescriptionCreateWithPixelFormatType';

function CVPixelFormatDescriptionArrayCreateWithAllPixelFormatTypes(allocator: CFAllocatorRef): CFArrayRef; cdecl;
  external libCoreVideo name _PU + 'CVPixelFormatDescriptionArrayCreateWithAllPixelFormatTypes';

procedure CVPixelFormatDescriptionRegisterDescriptionWithPixelFormatType(description: CFDictionaryRef; pixelFormat: OSType); cdecl;
  external libCoreVideo name _PU + 'CVPixelFormatDescriptionRegisterDescriptionWithPixelFormatType';

function CVIsCompressedPixelFormatAvailable(pixelFormatType: OSType): Boolean; cdecl;
  external libCoreVideo name _PU + 'CVIsCompressedPixelFormatAvailable';

implementation

uses
  System.SysUtils;

var
  CoreVideoModule: THandle;

function kCVBufferPropagatedAttachmentsKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVBufferPropagatedAttachmentsKey');
end;

function kCVBufferNonPropagatedAttachmentsKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVBufferNonPropagatedAttachmentsKey');
end;

function kCVBufferMovieTimeKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVBufferMovieTimeKey');
end;

function kCVBufferTimeValueKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVBufferTimeValueKey');
end;

function kCVBufferTimeScaleKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVBufferTimeScaleKey');
end;

function kCVImageBufferCGColorSpaceKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferCGColorSpaceKey');
end;

function kCVImageBufferCleanApertureKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferCleanApertureKey');
end;

function kCVImageBufferCleanApertureWidthKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferCleanApertureWidthKey');
end;

function kCVImageBufferCleanApertureHeightKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferCleanApertureHeightKey');
end;

function kCVImageBufferCleanApertureHorizontalOffsetKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferCleanApertureHorizontalOffsetKey');
end;

function kCVImageBufferCleanApertureVerticalOffsetKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferCleanApertureVerticalOffsetKey');
end;

function kCVImageBufferPreferredCleanApertureKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferPreferredCleanApertureKey');
end;

function kCVImageBufferFieldCountKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferFieldCountKey');
end;

function kCVImageBufferFieldDetailKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferFieldDetailKey');
end;

function kCVImageBufferFieldDetailTemporalTopFirst: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferFieldDetailTemporalTopFirst');
end;

function kCVImageBufferFieldDetailTemporalBottomFirst: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferFieldDetailTemporalBottomFirst');
end;

function kCVImageBufferFieldDetailSpatialFirstLineEarly: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferFieldDetailSpatialFirstLineEarly');
end;

function kCVImageBufferFieldDetailSpatialFirstLineLate: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferFieldDetailSpatialFirstLineLate');
end;

function kCVImageBufferPixelAspectRatioKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferPixelAspectRatioKey');
end;

function kCVImageBufferPixelAspectRatioHorizontalSpacingKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferPixelAspectRatioHorizontalSpacingKey');
end;

function kCVImageBufferPixelAspectRatioVerticalSpacingKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferPixelAspectRatioVerticalSpacingKey');
end;

function kCVImageBufferDisplayDimensionsKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferDisplayDimensionsKey');
end;

function kCVImageBufferDisplayWidthKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferDisplayWidthKey');
end;

function kCVImageBufferDisplayHeightKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferDisplayHeightKey');
end;

function kCVImageBufferGammaLevelKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferGammaLevelKey');
end;

function kCVImageBufferICCProfileKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferICCProfileKey');
end;

function kCVImageBufferYCbCrMatrixKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferYCbCrMatrixKey');
end;

function kCVImageBufferYCbCrMatrix_ITU_R_709_2: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferYCbCrMatrix_ITU_R_709_2');
end;

function kCVImageBufferYCbCrMatrix_ITU_R_601_4: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferYCbCrMatrix_ITU_R_601_4');
end;

function kCVImageBufferYCbCrMatrix_SMPTE_240M_1995: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferYCbCrMatrix_SMPTE_240M_1995');
end;

function kCVImageBufferYCbCrMatrix_DCI_P3: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferYCbCrMatrix_DCI_P3');
end;

function kCVImageBufferYCbCrMatrix_P3_D65: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferYCbCrMatrix_P3_D65');
end;

function kCVImageBufferYCbCrMatrix_ITU_R_2020: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferYCbCrMatrix_ITU_R_2020');
end;

function kCVImageBufferColorPrimariesKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferColorPrimariesKey');
end;

function kCVImageBufferColorPrimaries_ITU_R_709_2: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferColorPrimaries_ITU_R_709_2');
end;

function kCVImageBufferColorPrimaries_EBU_3213: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferColorPrimaries_EBU_3213');
end;

function kCVImageBufferColorPrimaries_SMPTE_C: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferColorPrimaries_SMPTE_C');
end;

function kCVImageBufferColorPrimaries_P22: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferColorPrimaries_P22');
end;

function kCVImageBufferColorPrimaries_DCI_P3: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferColorPrimaries_DCI_P3');
end;

function kCVImageBufferColorPrimaries_P3_D65: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferColorPrimaries_P3_D65');
end;

function kCVImageBufferColorPrimaries_ITU_R_2020: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferColorPrimaries_ITU_R_2020');
end;

function kCVImageBufferTransferFunctionKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferTransferFunctionKey');
end;

function kCVImageBufferTransferFunction_ITU_R_709_2: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferTransferFunction_ITU_R_709_2');
end;

function kCVImageBufferTransferFunction_SMPTE_240M_1995: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferTransferFunction_SMPTE_240M_1995');
end;

function kCVImageBufferTransferFunction_UseGamma: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferTransferFunction_UseGamma');
end;

function kCVImageBufferTransferFunction_EBU_3213: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferTransferFunction_EBU_3213');
end;

function kCVImageBufferTransferFunction_SMPTE_C: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferTransferFunction_SMPTE_C');
end;

function kCVImageBufferTransferFunction_sRGB: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferTransferFunction_sRGB');
end;

function kCVImageBufferTransferFunction_ITU_R_2020: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferTransferFunction_ITU_R_2020');
end;

function kCVImageBufferTransferFunction_SMPTE_ST_428_1: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferTransferFunction_SMPTE_ST_428_1');
end;

function kCVImageBufferTransferFunction_SMPTE_ST_2084_PQ: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferTransferFunction_SMPTE_ST_2084_PQ');
end;

function kCVImageBufferTransferFunction_ITU_R_2100_HLG: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferTransferFunction_ITU_R_2100_HLG');
end;

function kCVImageBufferTransferFunction_Linear: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferTransferFunction_Linear');
end;

function kCVImageBufferChromaLocationTopFieldKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferChromaLocationTopFieldKey');
end;

function kCVImageBufferChromaLocationBottomFieldKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferChromaLocationBottomFieldKey');
end;

function kCVImageBufferChromaLocation_Left: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferChromaLocation_Left');
end;

function kCVImageBufferChromaLocation_Center: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferChromaLocation_Center');
end;

function kCVImageBufferChromaLocation_TopLeft: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferChromaLocation_TopLeft');
end;

function kCVImageBufferChromaLocation_Top: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferChromaLocation_Top');
end;

function kCVImageBufferChromaLocation_BottomLeft: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferChromaLocation_BottomLeft');
end;

function kCVImageBufferChromaLocation_Bottom: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferChromaLocation_Bottom');
end;

function kCVImageBufferChromaLocation_DV420: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferChromaLocation_DV420');
end;

function kCVImageBufferChromaSubsamplingKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferChromaSubsamplingKey');
end;

function kCVImageBufferChromaSubsampling_420: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferChromaSubsampling_420');
end;

function kCVImageBufferChromaSubsampling_422: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferChromaSubsampling_422');
end;

function kCVImageBufferChromaSubsampling_411: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferChromaSubsampling_411');
end;

function kCVImageBufferAlphaChannelIsOpaque: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferAlphaChannelIsOpaque');
end;

function kCVImageBufferAlphaChannelModeKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferAlphaChannelModeKey');
end;

function kCVImageBufferAlphaChannelMode_StraightAlpha: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferAlphaChannelMode_StraightAlpha');
end;

function kCVImageBufferAlphaChannelMode_PremultipliedAlpha: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferAlphaChannelMode_PremultipliedAlpha');
end;

function kCVImageBufferPostDecodeProcessingSequenceMetadataKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferPostDecodeProcessingSequenceMetadataKey');
end;

function kCVImageBufferPostDecodeProcessingFrameMetadataKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferPostDecodeProcessingFrameMetadataKey');
end;

function kCVImageBufferMasteringDisplayColorVolumeKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferMasteringDisplayColorVolumeKey');
end;

function kCVImageBufferContentLightLevelInfoKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferContentLightLevelInfoKey');
end;

function kCVImageBufferAmbientViewingEnvironmentKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferAmbientViewingEnvironmentKey');
end;

function kCVImageBufferSceneIlluminationKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferSceneIlluminationKey');
end;

function kCVImageBufferRegionOfInterestKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferRegionOfInterestKey');
end;

function kCVImageBufferLogTransferFunctionKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferLogTransferFunctionKey');
end;

function kCVImageBufferLogTransferFunction_AppleLog: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVImageBufferLogTransferFunction_AppleLog');
end;

function kCVPixelBufferPixelFormatTypeKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferPixelFormatTypeKey');
end;

function kCVPixelBufferMemoryAllocatorKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferMemoryAllocatorKey');
end;

function kCVPixelBufferWidthKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferWidthKey');
end;

function kCVPixelBufferHeightKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferHeightKey');
end;

function kCVPixelBufferExtendedPixelsLeftKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferExtendedPixelsLeftKey');
end;

function kCVPixelBufferExtendedPixelsTopKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferExtendedPixelsTopKey');
end;

function kCVPixelBufferExtendedPixelsRightKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferExtendedPixelsRightKey');
end;

function kCVPixelBufferExtendedPixelsBottomKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferExtendedPixelsBottomKey');
end;

function kCVPixelBufferBytesPerRowAlignmentKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferBytesPerRowAlignmentKey');
end;

function kCVPixelBufferCGBitmapContextCompatibilityKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferCGBitmapContextCompatibilityKey');
end;

function kCVPixelBufferCGImageCompatibilityKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferCGImageCompatibilityKey');
end;

function kCVPixelBufferOpenGLCompatibilityKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferOpenGLCompatibilityKey');
end;

function kCVPixelBufferPlaneAlignmentKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferPlaneAlignmentKey');
end;

function kCVPixelBufferIOSurfacePropertiesKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferIOSurfacePropertiesKey');
end;

function kCVPixelBufferOpenGLESCompatibilityKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferOpenGLESCompatibilityKey');
end;

function kCVPixelBufferMetalCompatibilityKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferMetalCompatibilityKey');
end;

function kCVPixelBufferOpenGLTextureCacheCompatibilityKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferOpenGLTextureCacheCompatibilityKey');
end;

function kCVPixelBufferOpenGLESTextureCacheCompatibilityKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferOpenGLESTextureCacheCompatibilityKey');
end;

function kCVPixelBufferVersatileBayerKey_BayerPattern: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferVersatileBayerKey_BayerPattern');
end;

function kCVPixelBufferProResRAWKey_SenselSitingOffsets: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferProResRAWKey_SenselSitingOffsets');
end;

function kCVPixelBufferProResRAWKey_BlackLevel: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferProResRAWKey_BlackLevel');
end;

function kCVPixelBufferProResRAWKey_WhiteLevel: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferProResRAWKey_WhiteLevel');
end;

function kCVPixelBufferProResRAWKey_WhiteBalanceCCT: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferProResRAWKey_WhiteBalanceCCT');
end;

function kCVPixelBufferProResRAWKey_WhiteBalanceRedFactor: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferProResRAWKey_WhiteBalanceRedFactor');
end;

function kCVPixelBufferProResRAWKey_WhiteBalanceBlueFactor: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferProResRAWKey_WhiteBalanceBlueFactor');
end;

function kCVPixelBufferProResRAWKey_ColorMatrix: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferProResRAWKey_ColorMatrix');
end;

function kCVPixelBufferProResRAWKey_GainFactor: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferProResRAWKey_GainFactor');
end;

function kCVPixelBufferProResRAWKey_RecommendedCrop: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferProResRAWKey_RecommendedCrop');
end;

function kCVPixelBufferProResRAWKey_MetadataExtension: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferProResRAWKey_MetadataExtension');
end;

function kCVPixelBufferIOSurfaceOpenGLTextureCompatibilityKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferIOSurfaceOpenGLTextureCompatibilityKey');
end;

function kCVPixelBufferIOSurfaceOpenGLFBOCompatibilityKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferIOSurfaceOpenGLFBOCompatibilityKey');
end;

function kCVPixelBufferIOSurfaceCoreAnimationCompatibilityKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferIOSurfaceCoreAnimationCompatibilityKey');
end;

function kCVPixelBufferIOSurfaceOpenGLESTextureCompatibilityKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferIOSurfaceOpenGLESTextureCompatibilityKey');
end;

function kCVPixelBufferIOSurfaceOpenGLESFBOCompatibilityKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferIOSurfaceOpenGLESFBOCompatibilityKey');
end;

function kCVPixelBufferPoolMinimumBufferCountKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferPoolMinimumBufferCountKey');
end;

function kCVPixelBufferPoolMaximumBufferAgeKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferPoolMaximumBufferAgeKey');
end;

function kCVPixelBufferPoolAllocationThresholdKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferPoolAllocationThresholdKey');
end;

function kCVPixelBufferPoolFreeBufferNotification: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelBufferPoolFreeBufferNotification');
end;

function kCVMetalTextureCacheMaximumTextureAgeKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVMetalTextureCacheMaximumTextureAgeKey');
end;

function kCVPixelFormatName: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatName');
end;

function kCVPixelFormatConstant: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatConstant');
end;

function kCVPixelFormatCodecType: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatCodecType');
end;

function kCVPixelFormatFourCC: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatFourCC');
end;

function kCVPixelFormatContainsAlpha: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatContainsAlpha');
end;

function kCVPixelFormatContainsYCbCr: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatContainsYCbCr');
end;

function kCVPixelFormatContainsRGB: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatContainsRGB');
end;

function kCVPixelFormatContainsGrayscale: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatContainsGrayscale');
end;

function kCVPixelFormatContainsSenselArray: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatContainsSenselArray');
end;

function kCVPixelFormatComponentRange: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatComponentRange');
end;

function kCVPixelFormatComponentRange_VideoRange: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatComponentRange_VideoRange');
end;

function kCVPixelFormatComponentRange_FullRange: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatComponentRange_FullRange');
end;

function kCVPixelFormatComponentRange_WideRange: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatComponentRange_WideRange');
end;

function kCVPixelFormatPlanes: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatPlanes');
end;

function kCVPixelFormatBlockWidth: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatBlockWidth');
end;

function kCVPixelFormatBlockHeight: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatBlockHeight');
end;

function kCVPixelFormatBitsPerBlock: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatBitsPerBlock');
end;

function kCVPixelFormatBitsPerComponent: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatBitsPerComponent');
end;

function kCVPixelFormatBlockHorizontalAlignment: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatBlockHorizontalAlignment');
end;

function kCVPixelFormatBlockVerticalAlignment: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatBlockVerticalAlignment');
end;

function kCVPixelFormatBlackBlock: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatBlackBlock');
end;

function kCVPixelFormatHorizontalSubsampling: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatHorizontalSubsampling');
end;

function kCVPixelFormatVerticalSubsampling: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatVerticalSubsampling');
end;

function kCVPixelFormatOpenGLFormat: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatOpenGLFormat');
end;

function kCVPixelFormatOpenGLType: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatOpenGLType');
end;

function kCVPixelFormatOpenGLInternalFormat: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatOpenGLInternalFormat');
end;

function kCVPixelFormatCGBitmapInfo: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatCGBitmapInfo');
end;

function kCVPixelFormatQDCompatibility: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatQDCompatibility');
end;

function kCVPixelFormatCGBitmapContextCompatibility: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatCGBitmapContextCompatibility');
end;

function kCVPixelFormatCGImageCompatibility: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatCGImageCompatibility');
end;

function kCVPixelFormatOpenGLCompatibility: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatOpenGLCompatibility');
end;

function kCVPixelFormatOpenGLESCompatibility: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatOpenGLESCompatibility');
end;

function kCVPixelFormatFillExtendedPixelsCallback: NSString;
begin
  Result := CocoaNSStringConst(libCoreVideo, 'kCVPixelFormatFillExtendedPixelsCallback');
end;

initialization
  CoreVideoModule := LoadLibrary(libCoreVideo);

finalization
  if CoreVideoModule <> 0 then
    FreeLibrary(CoreVideoModule);

end.