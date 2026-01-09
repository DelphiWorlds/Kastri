unit DW.iOSapi.Metal;

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
  Macapi.ObjectiveC, iOSapi.CocoaTypes, Macapi.CoreFoundation, Macapi.Dispatch,
  // iOS
  iOSapi.Foundation,
  // DW
  DW.Macapi.Dispatch, DW.iOSapi.Foundation, DW.iOSapi.IOSurface;

const
  MTLResourceCPUCacheModeShift = 0;
  MTLResourceCPUCacheModeMask = ($f shl MTLResourceCPUCacheModeShift);
  MTLResourceStorageModeShift = 4;
  MTLResourceStorageModeMask = ($f shl MTLResourceStorageModeShift);
  MTLResourceHazardTrackingModeShift = 8;
  MTLResourceHazardTrackingModeMask = ($3 shl MTLResourceHazardTrackingModeShift);
  MTLCounterDontSample = -1;
  MTLMaxResourceStatePassSampleBuffers = 4;
  MTLMaxRenderPassSampleBuffers = 4;
  MTLMaxBlitPassSampleBuffers = 4;
  MTLMaxComputePassSampleBuffers = 4;
  MTLResourceUsageRead = 1;
  MTLResourceUsageWrite = 2;
  MTLResourceUsageSample = 4;
  MTLBarrierScopeBuffers = 1;
  MTLBarrierScopeTextures = 2;
  MTLBarrierScopeRenderTargets = 4;
  MTLPurgeableStateKeepCurrent = 1;
  MTLPurgeableStateNonVolatile = 2;
  MTLPurgeableStateVolatile = 3;
  MTLPurgeableStateEmpty = 4;
  MTLCPUCacheModeDefaultCache = 0;
  MTLCPUCacheModeWriteCombined = 1;
  MTLStorageModeShared = 0;
  MTLStorageModeManaged = 1;
  MTLStorageModePrivate = 2;
  MTLStorageModeMemoryless = 3;
  MTLHazardTrackingModeDefault = 0;
  MTLHazardTrackingModeUntracked = 1;
  MTLHazardTrackingModeTracked = 2;
  MTLResourceCPUCacheModeDefaultCache = 0;
  MTLResourceCPUCacheModeWriteCombined = 1;
  MTLResourceStorageModeShared = 0;
  MTLResourceStorageModeManaged = 16;
  MTLResourceStorageModePrivate = 32;
  MTLResourceStorageModeMemoryless = 48;
  MTLResourceHazardTrackingModeDefault = 0;
  MTLResourceHazardTrackingModeUntracked = 256;
  MTLResourceHazardTrackingModeTracked = 512;
  MTLResourceOptionCPUCacheModeDefault = MTLResourceCPUCacheModeDefaultCache;
  MTLResourceOptionCPUCacheModeWriteCombined = MTLResourceCPUCacheModeWriteCombined;
  MTLPixelFormatInvalid = 0;
  MTLPixelFormatA8Unorm = 1;
  MTLPixelFormatR8Unorm = 10;
  MTLPixelFormatR8Unorm_sRGB = 11;
  MTLPixelFormatR8Snorm = 12;
  MTLPixelFormatR8Uint = 13;
  MTLPixelFormatR8Sint = 14;
  MTLPixelFormatR16Unorm = 20;
  MTLPixelFormatR16Snorm = 22;
  MTLPixelFormatR16Uint = 23;
  MTLPixelFormatR16Sint = 24;
  MTLPixelFormatR16Float = 25;
  MTLPixelFormatRG8Unorm = 30;
  MTLPixelFormatRG8Unorm_sRGB = 31;
  MTLPixelFormatRG8Snorm = 32;
  MTLPixelFormatRG8Uint = 33;
  MTLPixelFormatRG8Sint = 34;
  MTLPixelFormatB5G6R5Unorm = 40;
  MTLPixelFormatA1BGR5Unorm = 41;
  MTLPixelFormatABGR4Unorm = 42;
  MTLPixelFormatBGR5A1Unorm = 43;
  MTLPixelFormatR32Uint = 53;
  MTLPixelFormatR32Sint = 54;
  MTLPixelFormatR32Float = 55;
  MTLPixelFormatRG16Unorm = 60;
  MTLPixelFormatRG16Snorm = 62;
  MTLPixelFormatRG16Uint = 63;
  MTLPixelFormatRG16Sint = 64;
  MTLPixelFormatRG16Float = 65;
  MTLPixelFormatRGBA8Unorm = 70;
  MTLPixelFormatRGBA8Unorm_sRGB = 71;
  MTLPixelFormatRGBA8Snorm = 72;
  MTLPixelFormatRGBA8Uint = 73;
  MTLPixelFormatRGBA8Sint = 74;
  MTLPixelFormatBGRA8Unorm = 80;
  MTLPixelFormatBGRA8Unorm_sRGB = 81;
  MTLPixelFormatRGB10A2Unorm = 90;
  MTLPixelFormatRGB10A2Uint = 91;
  MTLPixelFormatRG11B10Float = 92;
  MTLPixelFormatRGB9E5Float = 93;
  MTLPixelFormatBGR10A2Unorm = 94;
  MTLPixelFormatBGR10_XR = 554;
  MTLPixelFormatBGR10_XR_sRGB = 555;
  MTLPixelFormatRG32Uint = 103;
  MTLPixelFormatRG32Sint = 104;
  MTLPixelFormatRG32Float = 105;
  MTLPixelFormatRGBA16Unorm = 110;
  MTLPixelFormatRGBA16Snorm = 112;
  MTLPixelFormatRGBA16Uint = 113;
  MTLPixelFormatRGBA16Sint = 114;
  MTLPixelFormatRGBA16Float = 115;
  MTLPixelFormatBGRA10_XR = 552;
  MTLPixelFormatBGRA10_XR_sRGB = 553;
  MTLPixelFormatRGBA32Uint = 123;
  MTLPixelFormatRGBA32Sint = 124;
  MTLPixelFormatRGBA32Float = 125;
  MTLPixelFormatBC1_RGBA = 130;
  MTLPixelFormatBC1_RGBA_sRGB = 131;
  MTLPixelFormatBC2_RGBA = 132;
  MTLPixelFormatBC2_RGBA_sRGB = 133;
  MTLPixelFormatBC3_RGBA = 134;
  MTLPixelFormatBC3_RGBA_sRGB = 135;
  MTLPixelFormatBC4_RUnorm = 140;
  MTLPixelFormatBC4_RSnorm = 141;
  MTLPixelFormatBC5_RGUnorm = 142;
  MTLPixelFormatBC5_RGSnorm = 143;
  MTLPixelFormatBC6H_RGBFloat = 150;
  MTLPixelFormatBC6H_RGBUfloat = 151;
  MTLPixelFormatBC7_RGBAUnorm = 152;
  MTLPixelFormatBC7_RGBAUnorm_sRGB = 153;
  MTLPixelFormatPVRTC_RGB_2BPP = 160;
  MTLPixelFormatPVRTC_RGB_2BPP_sRGB = 161;
  MTLPixelFormatPVRTC_RGB_4BPP = 162;
  MTLPixelFormatPVRTC_RGB_4BPP_sRGB = 163;
  MTLPixelFormatPVRTC_RGBA_2BPP = 164;
  MTLPixelFormatPVRTC_RGBA_2BPP_sRGB = 165;
  MTLPixelFormatPVRTC_RGBA_4BPP = 166;
  MTLPixelFormatPVRTC_RGBA_4BPP_sRGB = 167;
  MTLPixelFormatEAC_R11Unorm = 170;
  MTLPixelFormatEAC_R11Snorm = 172;
  MTLPixelFormatEAC_RG11Unorm = 174;
  MTLPixelFormatEAC_RG11Snorm = 176;
  MTLPixelFormatEAC_RGBA8 = 178;
  MTLPixelFormatEAC_RGBA8_sRGB = 179;
  MTLPixelFormatETC2_RGB8 = 180;
  MTLPixelFormatETC2_RGB8_sRGB = 181;
  MTLPixelFormatETC2_RGB8A1 = 182;
  MTLPixelFormatETC2_RGB8A1_sRGB = 183;
  MTLPixelFormatASTC_4x4_sRGB = 186;
  MTLPixelFormatASTC_5x4_sRGB = 187;
  MTLPixelFormatASTC_5x5_sRGB = 188;
  MTLPixelFormatASTC_6x5_sRGB = 189;
  MTLPixelFormatASTC_6x6_sRGB = 190;
  MTLPixelFormatASTC_8x5_sRGB = 192;
  MTLPixelFormatASTC_8x6_sRGB = 193;
  MTLPixelFormatASTC_8x8_sRGB = 194;
  MTLPixelFormatASTC_10x5_sRGB = 195;
  MTLPixelFormatASTC_10x6_sRGB = 196;
  MTLPixelFormatASTC_10x8_sRGB = 197;
  MTLPixelFormatASTC_10x10_sRGB = 198;
  MTLPixelFormatASTC_12x10_sRGB = 199;
  MTLPixelFormatASTC_12x12_sRGB = 200;
  MTLPixelFormatASTC_4x4_LDR = 204;
  MTLPixelFormatASTC_5x4_LDR = 205;
  MTLPixelFormatASTC_5x5_LDR = 206;
  MTLPixelFormatASTC_6x5_LDR = 207;
  MTLPixelFormatASTC_6x6_LDR = 208;
  MTLPixelFormatASTC_8x5_LDR = 210;
  MTLPixelFormatASTC_8x6_LDR = 211;
  MTLPixelFormatASTC_8x8_LDR = 212;
  MTLPixelFormatASTC_10x5_LDR = 213;
  MTLPixelFormatASTC_10x6_LDR = 214;
  MTLPixelFormatASTC_10x8_LDR = 215;
  MTLPixelFormatASTC_10x10_LDR = 216;
  MTLPixelFormatASTC_12x10_LDR = 217;
  MTLPixelFormatASTC_12x12_LDR = 218;
  MTLPixelFormatASTC_4x4_HDR = 222;
  MTLPixelFormatASTC_5x4_HDR = 223;
  MTLPixelFormatASTC_5x5_HDR = 224;
  MTLPixelFormatASTC_6x5_HDR = 225;
  MTLPixelFormatASTC_6x6_HDR = 226;
  MTLPixelFormatASTC_8x5_HDR = 228;
  MTLPixelFormatASTC_8x6_HDR = 229;
  MTLPixelFormatASTC_8x8_HDR = 230;
  MTLPixelFormatASTC_10x5_HDR = 231;
  MTLPixelFormatASTC_10x6_HDR = 232;
  MTLPixelFormatASTC_10x8_HDR = 233;
  MTLPixelFormatASTC_10x10_HDR = 234;
  MTLPixelFormatASTC_12x10_HDR = 235;
  MTLPixelFormatASTC_12x12_HDR = 236;
  MTLPixelFormatGBGR422 = 240;
  MTLPixelFormatBGRG422 = 241;
  MTLPixelFormatDepth16Unorm = 250;
  MTLPixelFormatDepth32Float = 252;
  MTLPixelFormatStencil8 = 253;
  MTLPixelFormatDepth24Unorm_Stencil8 = 255;
  MTLPixelFormatDepth32Float_Stencil8 = 260;
  MTLPixelFormatX32_Stencil8 = 261;
  MTLPixelFormatX24_Stencil8 = 262;
  MTLTextureType1D = 0;
  MTLTextureType1DArray = 1;
  MTLTextureType2D = 2;
  MTLTextureType2DArray = 3;
  MTLTextureType2DMultisample = 4;
  MTLTextureTypeCube = 5;
  MTLTextureTypeCubeArray = 6;
  MTLTextureType3D = 7;
  MTLTextureType2DMultisampleArray = 8;
  MTLTextureTypeTextureBuffer = 9;
  MTLTextureSwizzleZero = 0;
  MTLTextureSwizzleOne = 1;
  MTLTextureSwizzleRed = 2;
  MTLTextureSwizzleGreen = 3;
  MTLTextureSwizzleBlue = 4;
  MTLTextureSwizzleAlpha = 5;
  MTLTextureUsageUnknown = 0;
  MTLTextureUsageShaderRead = 1;
  MTLTextureUsageShaderWrite = 2;
  MTLTextureUsageRenderTarget = 4;
  MTLTextureUsagePixelFormatView = 16;
  MTLDataTypeNone = 0;
  MTLDataTypeStruct = 1;
  MTLDataTypeArray = 2;
  MTLDataTypeFloat = 3;
  MTLDataTypeFloat2 = 4;
  MTLDataTypeFloat3 = 5;
  MTLDataTypeFloat4 = 6;
  MTLDataTypeFloat2x2 = 7;
  MTLDataTypeFloat2x3 = 8;
  MTLDataTypeFloat2x4 = 9;
  MTLDataTypeFloat3x2 = 10;
  MTLDataTypeFloat3x3 = 11;
  MTLDataTypeFloat3x4 = 12;
  MTLDataTypeFloat4x2 = 13;
  MTLDataTypeFloat4x3 = 14;
  MTLDataTypeFloat4x4 = 15;
  MTLDataTypeHalf = 16;
  MTLDataTypeHalf2 = 17;
  MTLDataTypeHalf3 = 18;
  MTLDataTypeHalf4 = 19;
  MTLDataTypeHalf2x2 = 20;
  MTLDataTypeHalf2x3 = 21;
  MTLDataTypeHalf2x4 = 22;
  MTLDataTypeHalf3x2 = 23;
  MTLDataTypeHalf3x3 = 24;
  MTLDataTypeHalf3x4 = 25;
  MTLDataTypeHalf4x2 = 26;
  MTLDataTypeHalf4x3 = 27;
  MTLDataTypeHalf4x4 = 28;
  MTLDataTypeInt = 29;
  MTLDataTypeInt2 = 30;
  MTLDataTypeInt3 = 31;
  MTLDataTypeInt4 = 32;
  MTLDataTypeUInt = 33;
  MTLDataTypeUInt2 = 34;
  MTLDataTypeUInt3 = 35;
  MTLDataTypeUInt4 = 36;
  MTLDataTypeShort = 37;
  MTLDataTypeShort2 = 38;
  MTLDataTypeShort3 = 39;
  MTLDataTypeShort4 = 40;
  MTLDataTypeUShort = 41;
  MTLDataTypeUShort2 = 42;
  MTLDataTypeUShort3 = 43;
  MTLDataTypeUShort4 = 44;
  MTLDataTypeChar = 45;
  MTLDataTypeChar2 = 46;
  MTLDataTypeChar3 = 47;
  MTLDataTypeChar4 = 48;
  MTLDataTypeUChar = 49;
  MTLDataTypeUChar2 = 50;
  MTLDataTypeUChar3 = 51;
  MTLDataTypeUChar4 = 52;
  MTLDataTypeBool = 53;
  MTLDataTypeBool2 = 54;
  MTLDataTypeBool3 = 55;
  MTLDataTypeBool4 = 56;
  MTLDataTypeTexture = 58;
  MTLDataTypeSampler = 59;
  MTLDataTypePointer = 60;
  MTLDataTypeR8Unorm = 62;
  MTLDataTypeR8Snorm = 63;
  MTLDataTypeR16Unorm = 64;
  MTLDataTypeR16Snorm = 65;
  MTLDataTypeRG8Unorm = 66;
  MTLDataTypeRG8Snorm = 67;
  MTLDataTypeRG16Unorm = 68;
  MTLDataTypeRG16Snorm = 69;
  MTLDataTypeRGBA8Unorm = 70;
  MTLDataTypeRGBA8Unorm_sRGB = 71;
  MTLDataTypeRGBA8Snorm = 72;
  MTLDataTypeRGBA16Unorm = 73;
  MTLDataTypeRGBA16Snorm = 74;
  MTLDataTypeRGB10A2Unorm = 75;
  MTLDataTypeRG11B10Float = 76;
  MTLDataTypeRGB9E5Float = 77;
  MTLDataTypeRenderPipeline = 78;
  MTLDataTypeComputePipeline = 79;
  MTLDataTypeIndirectCommandBuffer = 80;
  MTLDataTypeVisibleFunctionTable = 115;
  MTLDataTypeIntersectionFunctionTable = 116;
  MTLDataTypePrimitiveAccelerationStructure = 117;
  MTLDataTypeInstanceAccelerationStructure = 118;
  MTLArgumentTypeBuffer = 0;
  MTLArgumentTypeThreadgroupMemory = 1;
  MTLArgumentTypeTexture = 2;
  MTLArgumentTypeSampler = 3;
  MTLArgumentTypeImageblockData = 16;
  MTLArgumentTypeImageblock = 17;
  MTLArgumentTypeVisibleFunctionTable = 24;
  MTLArgumentTypePrimitiveAccelerationStructure = 25;
  MTLArgumentTypeInstanceAccelerationStructure = 26;
  MTLArgumentTypeIntersectionFunctionTable = 27;
  MTLArgumentAccessReadOnly = 0;
  MTLArgumentAccessReadWrite = 1;
  MTLArgumentAccessWriteOnly = 2;
  MTLFunctionOptionNone = 0;
  MTLFunctionOptionCompileToBinary = 1;
  MTLPatchTypeNone = 0;
  MTLPatchTypeTriangle = 1;
  MTLPatchTypeQuad = 2;
  MTLFunctionTypeVertex = 1;
  MTLFunctionTypeFragment = 2;
  MTLFunctionTypeKernel = 3;
  MTLFunctionTypeVisible = 5;
  MTLFunctionTypeIntersection = 6;
  MTLLanguageVersion1_0 = 65536;
  MTLLanguageVersion1_1 = 65537;
  MTLLanguageVersion1_2 = 65538;
  MTLLanguageVersion2_0 = 131072;
  MTLLanguageVersion2_1 = 131073;
  MTLLanguageVersion2_2 = 131074;
  MTLLanguageVersion2_3 = 131075;
  MTLLibraryTypeExecutable = 0;
  MTLLibraryTypeDynamic = 1;
  MTLLibraryErrorUnsupported = 1;
  MTLLibraryErrorInternal = 2;
  MTLLibraryErrorCompileFailure = 3;
  MTLLibraryErrorCompileWarning = 4;
  MTLLibraryErrorFunctionNotFound = 5;
  MTLLibraryErrorFileNotFound = 6;
  MTLCounterSampleBufferErrorOutOfMemory = 0;
  MTLCounterSampleBufferErrorInvalid = 1;
  MTLCounterSampleBufferErrorInternal = 2;
  MTLFeatureSet_iOS_GPUFamily1_v1 = 0;
  MTLFeatureSet_iOS_GPUFamily2_v1 = 1;
  MTLFeatureSet_iOS_GPUFamily1_v2 = 2;
  MTLFeatureSet_iOS_GPUFamily2_v2 = 3;
  MTLFeatureSet_iOS_GPUFamily3_v1 = 4;
  MTLFeatureSet_iOS_GPUFamily1_v3 = 5;
  MTLFeatureSet_iOS_GPUFamily2_v3 = 6;
  MTLFeatureSet_iOS_GPUFamily3_v2 = 7;
  MTLFeatureSet_iOS_GPUFamily1_v4 = 8;
  MTLFeatureSet_iOS_GPUFamily2_v4 = 9;
  MTLFeatureSet_iOS_GPUFamily3_v3 = 10;
  MTLFeatureSet_iOS_GPUFamily4_v1 = 11;
  MTLFeatureSet_iOS_GPUFamily1_v5 = 12;
  MTLFeatureSet_iOS_GPUFamily2_v5 = 13;
  MTLFeatureSet_iOS_GPUFamily3_v4 = 14;
  MTLFeatureSet_iOS_GPUFamily4_v2 = 15;
  MTLFeatureSet_iOS_GPUFamily5_v1 = 16;
  MTLFeatureSet_macOS_GPUFamily1_v1 = 10000;
  MTLFeatureSet_OSX_GPUFamily1_v1 = MTLFeatureSet_macOS_GPUFamily1_v1;
  MTLFeatureSet_macOS_GPUFamily1_v2 = 10001;
  MTLFeatureSet_OSX_GPUFamily1_v2 = MTLFeatureSet_macOS_GPUFamily1_v2;
  MTLFeatureSet_macOS_ReadWriteTextureTier2 = 10002;
  MTLFeatureSet_OSX_ReadWriteTextureTier2 = MTLFeatureSet_macOS_ReadWriteTextureTier2;
  MTLFeatureSet_macOS_GPUFamily1_v3 = 10003;
  MTLFeatureSet_macOS_GPUFamily1_v4 = 10004;
  MTLFeatureSet_macOS_GPUFamily2_v1 = 10005;
  MTLFeatureSet_tvOS_GPUFamily1_v1 = 30000;
  MTLFeatureSet_tvOS_GPUFamily1_v2 = 30001;
  MTLFeatureSet_tvOS_GPUFamily1_v3 = 30002;
  MTLFeatureSet_tvOS_GPUFamily1_v4 = 30004;
  MTLGPUFamilyApple1 = 1001;
  MTLGPUFamilyApple2 = 1002;
  MTLGPUFamilyApple3 = 1003;
  MTLGPUFamilyApple4 = 1004;
  MTLGPUFamilyApple5 = 1005;
  MTLGPUFamilyApple6 = 1006;
  MTLGPUFamilyApple7 = 1007;
  MTLGPUFamilyMac1 = 2001;
  MTLGPUFamilyMac2 = 2002;
  MTLGPUFamilyCommon1 = 3001;
  MTLGPUFamilyCommon2 = 3002;
  MTLGPUFamilyCommon3 = 3003;
  MTLGPUFamilyMacCatalyst1 = 4001;
  MTLGPUFamilyMacCatalyst2 = 4002;
  MTLPipelineOptionNone = 0;
  MTLPipelineOptionArgumentInfo = 1;
  MTLPipelineOptionBufferTypeInfo = 2;
  MTLPipelineOptionFailOnBinaryArchiveMiss = 4;
  MTLReadWriteTextureTierNone = 0;
  MTLReadWriteTextureTier1 = 1;
  MTLReadWriteTextureTier2 = 2;
  MTLArgumentBuffersTier1 = 0;
  MTLArgumentBuffersTier2 = 1;
  MTLSparseTextureRegionAlignmentModeOutward = 0;
  MTLSparseTextureRegionAlignmentModeInward = 1;
  MTLCounterSamplingPointAtStageBoundary = 0;
  MTLCounterSamplingPointAtDrawBoundary = 1;
  MTLCounterSamplingPointAtDispatchBoundary = 2;
  MTLCounterSamplingPointAtTileDispatchBoundary = 3;
  MTLCounterSamplingPointAtBlitBoundary = 4;
  MTLSparseTextureMappingModeMap = 0;
  MTLSparseTextureMappingModeUnmap = 1;
  MTLLoadActionDontCare = 0;
  MTLLoadActionLoad = 1;
  MTLLoadActionClear = 2;
  MTLStoreActionDontCare = 0;
  MTLStoreActionStore = 1;
  MTLStoreActionMultisampleResolve = 2;
  MTLStoreActionStoreAndMultisampleResolve = 3;
  MTLStoreActionUnknown = 4;
  MTLStoreActionCustomSampleDepthStore = 5;
  MTLStoreActionOptionNone = 0;
  MTLStoreActionOptionCustomSamplePositions = 1;
  MTLMultisampleDepthResolveFilterSample0 = 0;
  MTLMultisampleDepthResolveFilterMin = 1;
  MTLMultisampleDepthResolveFilterMax = 2;
  MTLMultisampleStencilResolveFilterSample0 = 0;
  MTLMultisampleStencilResolveFilterDepthResolvedSample = 1;
  MTLBlitOptionNone = 0;
  MTLBlitOptionDepthFromDepthStencil = 1;
  MTLBlitOptionStencilFromDepthStencil = 2;
  MTLBlitOptionRowLinearPVRTC = 4;
  MTLCommandBufferStatusNotEnqueued = 0;
  MTLCommandBufferStatusEnqueued = 1;
  MTLCommandBufferStatusCommitted = 2;
  MTLCommandBufferStatusScheduled = 3;
  MTLCommandBufferStatusCompleted = 4;
  MTLCommandBufferStatusError = 5;
  MTLCommandBufferErrorNone = 0;
  MTLCommandBufferErrorInternal = 1;
  MTLCommandBufferErrorTimeout = 2;
  MTLCommandBufferErrorPageFault = 3;
  MTLCommandBufferErrorBlacklisted = 4;
  MTLCommandBufferErrorNotPermitted = 7;
  MTLCommandBufferErrorOutOfMemory = 8;
  MTLCommandBufferErrorInvalidResource = 9;
  MTLCommandBufferErrorMemoryless = 10;
  MTLCommandBufferErrorDeviceRemoved = 11;
  MTLCommandBufferErrorOptionNone = 0;
  MTLCommandBufferErrorOptionEncoderExecutionStatus = 1;
  MTLCommandEncoderErrorStateUnknown = 0;
  MTLCommandEncoderErrorStateCompleted = 1;
  MTLCommandEncoderErrorStateAffected = 2;
  MTLCommandEncoderErrorStatePending = 3;
  MTLCommandEncoderErrorStateFaulted = 4;
  MTLDispatchTypeSerial = 0;
  MTLDispatchTypeConcurrent = 1;
  MTLCompareFunctionNever = 0;
  MTLCompareFunctionLess = 1;
  MTLCompareFunctionEqual = 2;
  MTLCompareFunctionLessEqual = 3;
  MTLCompareFunctionGreater = 4;
  MTLCompareFunctionNotEqual = 5;
  MTLCompareFunctionGreaterEqual = 6;
  MTLCompareFunctionAlways = 7;
  MTLStencilOperationKeep = 0;
  MTLStencilOperationZero = 1;
  MTLStencilOperationReplace = 2;
  MTLStencilOperationIncrementClamp = 3;
  MTLStencilOperationDecrementClamp = 4;
  MTLStencilOperationInvert = 5;
  MTLStencilOperationIncrementWrap = 6;
  MTLStencilOperationDecrementWrap = 7;
  MTLAttributeFormatInvalid = 0;
  MTLAttributeFormatUChar2 = 1;
  MTLAttributeFormatUChar3 = 2;
  MTLAttributeFormatUChar4 = 3;
  MTLAttributeFormatChar2 = 4;
  MTLAttributeFormatChar3 = 5;
  MTLAttributeFormatChar4 = 6;
  MTLAttributeFormatUChar2Normalized = 7;
  MTLAttributeFormatUChar3Normalized = 8;
  MTLAttributeFormatUChar4Normalized = 9;
  MTLAttributeFormatChar2Normalized = 10;
  MTLAttributeFormatChar3Normalized = 11;
  MTLAttributeFormatChar4Normalized = 12;
  MTLAttributeFormatUShort2 = 13;
  MTLAttributeFormatUShort3 = 14;
  MTLAttributeFormatUShort4 = 15;
  MTLAttributeFormatShort2 = 16;
  MTLAttributeFormatShort3 = 17;
  MTLAttributeFormatShort4 = 18;
  MTLAttributeFormatUShort2Normalized = 19;
  MTLAttributeFormatUShort3Normalized = 20;
  MTLAttributeFormatUShort4Normalized = 21;
  MTLAttributeFormatShort2Normalized = 22;
  MTLAttributeFormatShort3Normalized = 23;
  MTLAttributeFormatShort4Normalized = 24;
  MTLAttributeFormatHalf2 = 25;
  MTLAttributeFormatHalf3 = 26;
  MTLAttributeFormatHalf4 = 27;
  MTLAttributeFormatFloat = 28;
  MTLAttributeFormatFloat2 = 29;
  MTLAttributeFormatFloat3 = 30;
  MTLAttributeFormatFloat4 = 31;
  MTLAttributeFormatInt = 32;
  MTLAttributeFormatInt2 = 33;
  MTLAttributeFormatInt3 = 34;
  MTLAttributeFormatInt4 = 35;
  MTLAttributeFormatUInt = 36;
  MTLAttributeFormatUInt2 = 37;
  MTLAttributeFormatUInt3 = 38;
  MTLAttributeFormatUInt4 = 39;
  MTLAttributeFormatInt1010102Normalized = 40;
  MTLAttributeFormatUInt1010102Normalized = 41;
  MTLAttributeFormatUChar4Normalized_BGRA = 42;
  MTLAttributeFormatUChar = 45;
  MTLAttributeFormatChar = 46;
  MTLAttributeFormatUCharNormalized = 47;
  MTLAttributeFormatCharNormalized = 48;
  MTLAttributeFormatUShort = 49;
  MTLAttributeFormatShort = 50;
  MTLAttributeFormatUShortNormalized = 51;
  MTLAttributeFormatShortNormalized = 52;
  MTLAttributeFormatHalf = 53;
  MTLIndexTypeUInt16 = 0;
  MTLIndexTypeUInt32 = 1;
  MTLStepFunctionConstant = 0;
  MTLStepFunctionPerVertex = 1;
  MTLStepFunctionPerInstance = 2;
  MTLStepFunctionPerPatch = 3;
  MTLStepFunctionPerPatchControlPoint = 4;
  MTLStepFunctionThreadPositionInGridX = 5;
  MTLStepFunctionThreadPositionInGridY = 6;
  MTLStepFunctionThreadPositionInGridXIndexed = 7;
  MTLStepFunctionThreadPositionInGridYIndexed = 8;
  MTLMutabilityDefault = 0;
  MTLMutabilityMutable = 1;
  MTLMutabilityImmutable = 2;
  MTLPrimitiveTypePoint = 0;
  MTLPrimitiveTypeLine = 1;
  MTLPrimitiveTypeLineStrip = 2;
  MTLPrimitiveTypeTriangle = 3;
  MTLPrimitiveTypeTriangleStrip = 4;
  MTLVisibilityResultModeDisabled = 0;
  MTLVisibilityResultModeBoolean = 1;
  MTLVisibilityResultModeCounting = 2;
  MTLCullModeNone = 0;
  MTLCullModeFront = 1;
  MTLCullModeBack = 2;
  MTLWindingClockwise = 0;
  MTLWindingCounterClockwise = 1;
  MTLDepthClipModeClip = 0;
  MTLDepthClipModeClamp = 1;
  MTLTriangleFillModeFill = 0;
  MTLTriangleFillModeLines = 1;
  MTLRenderStageVertex = 1;
  MTLRenderStageFragment = 2;
  MTLBlendFactorZero = 0;
  MTLBlendFactorOne = 1;
  MTLBlendFactorSourceColor = 2;
  MTLBlendFactorOneMinusSourceColor = 3;
  MTLBlendFactorSourceAlpha = 4;
  MTLBlendFactorOneMinusSourceAlpha = 5;
  MTLBlendFactorDestinationColor = 6;
  MTLBlendFactorOneMinusDestinationColor = 7;
  MTLBlendFactorDestinationAlpha = 8;
  MTLBlendFactorOneMinusDestinationAlpha = 9;
  MTLBlendFactorSourceAlphaSaturated = 10;
  MTLBlendFactorBlendColor = 11;
  MTLBlendFactorOneMinusBlendColor = 12;
  MTLBlendFactorBlendAlpha = 13;
  MTLBlendFactorOneMinusBlendAlpha = 14;
  MTLBlendFactorSource1Color = 15;
  MTLBlendFactorOneMinusSource1Color = 16;
  MTLBlendFactorSource1Alpha = 17;
  MTLBlendFactorOneMinusSource1Alpha = 18;
  MTLBlendOperationAdd = 0;
  MTLBlendOperationSubtract = 1;
  MTLBlendOperationReverseSubtract = 2;
  MTLBlendOperationMin = 3;
  MTLBlendOperationMax = 4;
  MTLColorWriteMaskNone = 0;
  MTLColorWriteMaskRed = 8;
  MTLColorWriteMaskGreen = 4;
  MTLColorWriteMaskBlue = 2;
  MTLColorWriteMaskAlpha = 1;
  MTLColorWriteMaskAll = 15;
  MTLPrimitiveTopologyClassUnspecified = 0;
  MTLPrimitiveTopologyClassPoint = 1;
  MTLPrimitiveTopologyClassLine = 2;
  MTLPrimitiveTopologyClassTriangle = 3;
  MTLTessellationPartitionModePow2 = 0;
  MTLTessellationPartitionModeInteger = 1;
  MTLTessellationPartitionModeFractionalOdd = 2;
  MTLTessellationPartitionModeFractionalEven = 3;
  MTLTessellationFactorStepFunctionConstant = 0;
  MTLTessellationFactorStepFunctionPerPatch = 1;
  MTLTessellationFactorStepFunctionPerInstance = 2;
  MTLTessellationFactorStepFunctionPerPatchAndPerInstance = 3;
  MTLTessellationFactorFormatHalf = 0;
  MTLTessellationControlPointIndexTypeNone = 0;
  MTLTessellationControlPointIndexTypeUInt16 = 1;
  MTLTessellationControlPointIndexTypeUInt32 = 2;
  MTLVertexFormatInvalid = 0;
  MTLVertexFormatUChar2 = 1;
  MTLVertexFormatUChar3 = 2;
  MTLVertexFormatUChar4 = 3;
  MTLVertexFormatChar2 = 4;
  MTLVertexFormatChar3 = 5;
  MTLVertexFormatChar4 = 6;
  MTLVertexFormatUChar2Normalized = 7;
  MTLVertexFormatUChar3Normalized = 8;
  MTLVertexFormatUChar4Normalized = 9;
  MTLVertexFormatChar2Normalized = 10;
  MTLVertexFormatChar3Normalized = 11;
  MTLVertexFormatChar4Normalized = 12;
  MTLVertexFormatUShort2 = 13;
  MTLVertexFormatUShort3 = 14;
  MTLVertexFormatUShort4 = 15;
  MTLVertexFormatShort2 = 16;
  MTLVertexFormatShort3 = 17;
  MTLVertexFormatShort4 = 18;
  MTLVertexFormatUShort2Normalized = 19;
  MTLVertexFormatUShort3Normalized = 20;
  MTLVertexFormatUShort4Normalized = 21;
  MTLVertexFormatShort2Normalized = 22;
  MTLVertexFormatShort3Normalized = 23;
  MTLVertexFormatShort4Normalized = 24;
  MTLVertexFormatHalf2 = 25;
  MTLVertexFormatHalf3 = 26;
  MTLVertexFormatHalf4 = 27;
  MTLVertexFormatFloat = 28;
  MTLVertexFormatFloat2 = 29;
  MTLVertexFormatFloat3 = 30;
  MTLVertexFormatFloat4 = 31;
  MTLVertexFormatInt = 32;
  MTLVertexFormatInt2 = 33;
  MTLVertexFormatInt3 = 34;
  MTLVertexFormatInt4 = 35;
  MTLVertexFormatUInt = 36;
  MTLVertexFormatUInt2 = 37;
  MTLVertexFormatUInt3 = 38;
  MTLVertexFormatUInt4 = 39;
  MTLVertexFormatInt1010102Normalized = 40;
  MTLVertexFormatUInt1010102Normalized = 41;
  MTLVertexFormatUChar4Normalized_BGRA = 42;
  MTLVertexFormatUChar = 45;
  MTLVertexFormatChar = 46;
  MTLVertexFormatUCharNormalized = 47;
  MTLVertexFormatCharNormalized = 48;
  MTLVertexFormatUShort = 49;
  MTLVertexFormatShort = 50;
  MTLVertexFormatUShortNormalized = 51;
  MTLVertexFormatShortNormalized = 52;
  MTLVertexFormatHalf = 53;
  MTLVertexStepFunctionConstant = 0;
  MTLVertexStepFunctionPerVertex = 1;
  MTLVertexStepFunctionPerInstance = 2;
  MTLVertexStepFunctionPerPatch = 3;
  MTLVertexStepFunctionPerPatchControlPoint = 4;
  MTLSamplerMinMagFilterNearest = 0;
  MTLSamplerMinMagFilterLinear = 1;
  MTLSamplerMipFilterNotMipmapped = 0;
  MTLSamplerMipFilterNearest = 1;
  MTLSamplerMipFilterLinear = 2;
  MTLSamplerAddressModeClampToEdge = 0;
  MTLSamplerAddressModeMirrorClampToEdge = 1;
  MTLSamplerAddressModeRepeat = 2;
  MTLSamplerAddressModeMirrorRepeat = 3;
  MTLSamplerAddressModeClampToZero = 4;
  MTLSamplerAddressModeClampToBorderColor = 5;
  MTLSamplerBorderColorTransparentBlack = 0;
  MTLSamplerBorderColorOpaqueBlack = 1;
  MTLSamplerBorderColorOpaqueWhite = 2;
  MTLHeapTypeAutomatic = 0;
  MTLHeapTypePlacement = 1;
  MTLHeapTypeSparse = 2;
  MTLCaptureErrorNotSupported = 1;
  MTLCaptureErrorAlreadyCapturing = 2;
  MTLCaptureErrorInvalidDescriptor = 3;
  MTLCaptureDestinationDeveloperTools = 1;
  MTLCaptureDestinationGPUTraceDocument = 2;
  MTLIndirectCommandTypeDraw = 1;
  MTLIndirectCommandTypeDrawIndexed = 2;
  MTLIndirectCommandTypeDrawPatches = 4;
  MTLIndirectCommandTypeDrawIndexedPatches = 8;
  MTLIndirectCommandTypeConcurrentDispatch = 32;
  MTLIndirectCommandTypeConcurrentDispatchThreads = 64;
  MTLFunctionLogTypeValidation = 0;
  MTLAccelerationStructureUsageNone = 0;
  MTLAccelerationStructureUsageRefit = 1;
  MTLAccelerationStructureUsagePreferFastBuild = 2;
  MTLAccelerationStructureInstanceOptionNone = 0;
  MTLAccelerationStructureInstanceOptionDisableTriangleCulling = 1;
  MTLAccelerationStructureInstanceOptionTriangleFrontFacingWindingCounterClockwise = 2;
  MTLAccelerationStructureInstanceOptionOpaque = 4;
  MTLAccelerationStructureInstanceOptionNonOpaque = 8;
  MTLDynamicLibraryErrorNone = 0;
  MTLDynamicLibraryErrorInvalidFile = 1;
  MTLDynamicLibraryErrorCompilationFailure = 2;
  MTLDynamicLibraryErrorUnresolvedInstallName = 3;
  MTLDynamicLibraryErrorDependencyLoadFailure = 4;
  MTLDynamicLibraryErrorUnsupported = 5;
  MTLBinaryArchiveErrorNone = 0;
  MTLBinaryArchiveErrorInvalidFile = 1;
  MTLBinaryArchiveErrorUnexpectedElement = 2;
  MTLBinaryArchiveErrorCompilationFailure = 3;
  default = -9223372036854775808;
  MTLIntersectionFunctionSignatureNone = 0;
  MTLIntersectionFunctionSignatureInstancing = 1;
  MTLIntersectionFunctionSignatureTriangleData = 2;
  MTLIntersectionFunctionSignatureWorldSpaceData = 4;

type
  MTLCommandEncoder = interface;
  MTLResource = interface;
  MTLBuffer = interface;
  MTLSharedTextureHandle = interface;
  MTLTextureDescriptor = interface;
  MTLTexture = interface;
  MTLType = interface;
  MTLStructMember = interface;
  MTLStructType = interface;
  MTLArrayType = interface;
  MTLPointerType = interface;
  MTLTextureReferenceType = interface;
  MTLArgument = interface;
  MTLFunctionConstantValues = interface;
  MTLFunctionDescriptor = interface;
  MTLIntersectionFunctionDescriptor = interface;
  MTLVertexAttribute = interface;
  MTLAttribute = interface;
  MTLFunctionConstant = interface;
  MTLFunction = interface;
  MTLCompileOptions = interface;
  MTLLibrary = interface;
  MTLCounter = interface;
  MTLCounterSet = interface;
  MTLCounterSampleBufferDescriptor = interface;
  MTLCounterSampleBuffer = interface;
  MTLArgumentDescriptor = interface;
  MTLDevice = interface;
  MTLFence = interface;
  MTLResourceStatePassSampleBufferAttachmentDescriptor = interface;
  MTLResourceStatePassSampleBufferAttachmentDescriptorArray = interface;
  MTLResourceStatePassDescriptor = interface;
  MTLResourceStateCommandEncoder = interface;
  MTLRenderPassAttachmentDescriptor = interface;
  MTLRenderPassColorAttachmentDescriptor = interface;
  MTLRenderPassDepthAttachmentDescriptor = interface;
  MTLRenderPassStencilAttachmentDescriptor = interface;
  MTLRenderPassColorAttachmentDescriptorArray = interface;
  MTLRenderPassSampleBufferAttachmentDescriptor = interface;
  MTLRenderPassSampleBufferAttachmentDescriptorArray = interface;
  MTLRenderPassDescriptor = interface;
  MTLBlitPassSampleBufferAttachmentDescriptor = interface;
  MTLBlitPassSampleBufferAttachmentDescriptorArray = interface;
  MTLBlitPassDescriptor = interface;
  MTLBlitCommandEncoder = interface;
  MTLCommandBufferDescriptor = interface;
  MTLCommandBufferEncoderInfo = interface;
  MTLCommandBuffer = interface;
  MTLComputePassSampleBufferAttachmentDescriptor = interface;
  MTLComputePassSampleBufferAttachmentDescriptorArray = interface;
  MTLComputePassDescriptor = interface;
  MTLComputeCommandEncoder = interface;
  MTLCommandQueue = interface;
  MTLStencilDescriptor = interface;
  MTLDepthStencilDescriptor = interface;
  MTLDepthStencilState = interface;
  MTLDrawable = interface;
  MTLBufferLayoutDescriptor = interface;
  MTLBufferLayoutDescriptorArray = interface;
  MTLAttributeDescriptor = interface;
  MTLAttributeDescriptorArray = interface;
  MTLStageInputOutputDescriptor = interface;
  MTLPipelineBufferDescriptor = interface;
  MTLPipelineBufferDescriptorArray = interface;
  MTLLinkedFunctions = interface;
  MTLComputePipelineReflection = interface;
  MTLComputePipelineDescriptor = interface;
  MTLComputePipelineState = interface;
  MTLRenderCommandEncoder = interface;
  MTLRenderPipelineColorAttachmentDescriptor = interface;
  MTLRenderPipelineReflection = interface;
  MTLRenderPipelineDescriptor = interface;
  MTLRenderPipelineState = interface;
  MTLRenderPipelineColorAttachmentDescriptorArray = interface;
  MTLTileRenderPipelineColorAttachmentDescriptor = interface;
  MTLTileRenderPipelineColorAttachmentDescriptorArray = interface;
  MTLTileRenderPipelineDescriptor = interface;
  MTLVertexBufferLayoutDescriptor = interface;
  MTLVertexBufferLayoutDescriptorArray = interface;
  MTLVertexAttributeDescriptor = interface;
  MTLVertexAttributeDescriptorArray = interface;
  MTLVertexDescriptor = interface;
  MTLParallelRenderCommandEncoder = interface;
  MTLSamplerDescriptor = interface;
  MTLSamplerState = interface;
  MTLHeapDescriptor = interface;
  MTLHeap = interface;
  MTLArgumentEncoder = interface;
  MTLCaptureDescriptor = interface;
  MTLCaptureManager = interface;
  MTLCaptureScope = interface;
  MTLIndirectRenderCommand = interface;
  MTLIndirectComputeCommand = interface;
  MTLIndirectCommandBufferDescriptor = interface;
  MTLIndirectCommandBuffer = interface;
  MTLEvent = interface;
  MTLSharedEventListener = interface;
  MTLSharedEvent = interface;
  MTLSharedEventHandle = interface;
  MTLLogContainer = interface;
  MTLFunctionLogDebugLocation = interface;
  MTLFunctionLog = interface;
  MTLAccelerationStructureDescriptor = interface;
  MTLAccelerationStructureGeometryDescriptor = interface;
  MTLPrimitiveAccelerationStructureDescriptor = interface;
  MTLAccelerationStructureTriangleGeometryDescriptor = interface;
  MTLAccelerationStructureBoundingBoxGeometryDescriptor = interface;
  MTLInstanceAccelerationStructureDescriptor = interface;
  MTLAccelerationStructure = interface;
  MTLAccelerationStructureCommandEncoder = interface;
  MTLRasterizationRateSampleArray = interface;
  MTLRasterizationRateLayerDescriptor = interface;
  MTLRasterizationRateLayerArray = interface;
  MTLRasterizationRateMapDescriptor = interface;
  MTLRasterizationRateMap = interface;
  MTLDynamicLibrary = interface;
  MTLFunctionHandle = interface;
  MTLVisibleFunctionTableDescriptor = interface;
  MTLVisibleFunctionTable = interface;
  MTLBinaryArchiveDescriptor = interface;
  MTLBinaryArchive = interface;
  MTLIntersectionFunctionTableDescriptor = interface;
  MTLIntersectionFunctionTable = interface;

  PMTLSharedTextureHandlePrivate = Pointer;
  PPMTLSharedTextureHandlePrivate = ^PMTLSharedTextureHandlePrivate;
  PMTLSharedEventHandlePrivate = Pointer;
  PPMTLSharedEventHandlePrivate = ^PMTLSharedEventHandlePrivate;
  PMTLOrigin = ^MTLOrigin;
  PMTLSize = ^MTLSize;
  PMTLRegion = ^MTLRegion;
  PMTLSamplePosition = ^MTLSamplePosition;
  PMTLTextureSwizzleChannels = ^MTLTextureSwizzleChannels;
  PMTLCounterResultTimestamp = ^MTLCounterResultTimestamp;
  PMTLCounterResultStageUtilization = ^MTLCounterResultStageUtilization;
  PMTLCounterResultStatistic = ^MTLCounterResultStatistic;
  PMTLAccelerationStructureSizes = ^MTLAccelerationStructureSizes;
  PMTLSizeAndAlign = ^MTLSizeAndAlign;
  PMTLMapIndirectArguments = ^MTLMapIndirectArguments;
  PMTLClearColor = ^MTLClearColor;
  PMTLDispatchThreadgroupsIndirectArguments = ^MTLDispatchThreadgroupsIndirectArguments;
  PMTLStageInRegionIndirectArguments = ^MTLStageInRegionIndirectArguments;
  PMTLScissorRect = ^MTLScissorRect;
  PMTLViewport = ^MTLViewport;
  PMTLDrawPrimitivesIndirectArguments = ^MTLDrawPrimitivesIndirectArguments;
  PMTLDrawIndexedPrimitivesIndirectArguments = ^MTLDrawIndexedPrimitivesIndirectArguments;
  PMTLVertexAmplificationViewMapping = ^MTLVertexAmplificationViewMapping;
  PMTLDrawPatchIndirectArguments = ^MTLDrawPatchIndirectArguments;
  PMTLQuadTessellationFactorsHalf = ^MTLQuadTessellationFactorsHalf;
  PMTLTriangleTessellationFactorsHalf = ^MTLTriangleTessellationFactorsHalf;
  PMTLIndirectCommandBufferExecutionRange = ^MTLIndirectCommandBufferExecutionRange;

  PMTLArgument = ^MTLArgument;
  PMTLComputePipelineReflection = ^MTLComputePipelineReflection;
  PMTLRenderPipelineReflection = ^MTLRenderPipelineReflection;

  MTLOrigin = record
    x: NSUInteger;
    y: NSUInteger;
    z: NSUInteger;
  end;

  MTLSize = record
    width: NSUInteger;
    height: NSUInteger;
    depth: NSUInteger;
  end;

  MTLRegion = record
    origin: MTLOrigin;
    size: MTLSize;
  end;

  MTLSamplePosition = record
    x: Single;
    y: Single;
  end;

  MTLCoordinate2D = MTLSamplePosition;
  MTLResourceUsage = NSInteger;
  MTLBarrierScope = NSInteger;
  MTLPurgeableState = NSInteger;
  MTLCPUCacheMode = NSInteger;
  MTLStorageMode = NSInteger;
  MTLHazardTrackingMode = NSInteger;
  MTLResourceOptions = NSInteger;
  MTLPixelFormat = NSInteger;
  MTLTextureType = NSInteger;
  MTLTextureSwizzle = NSInteger;

  MTLTextureSwizzleChannels = record
    red: MTLTextureSwizzle;
    green: MTLTextureSwizzle;
    blue: MTLTextureSwizzle;
    alpha: MTLTextureSwizzle;
  end;

  MTLTextureUsage = NSInteger;
  MTLDataType = NSInteger;
  MTLArgumentType = NSInteger;
  MTLArgumentAccess = NSInteger;
  MTLFunctionOptions = NSInteger;
  MTLAutoreleasedArgument = MTLArgument;
  MTLPatchType = NSInteger;
  MTLFunctionType = NSInteger;
  MTLLanguageVersion = NSInteger;
  MTLLibraryType = NSInteger;
  MTLLibraryError = NSInteger;
  MTLCommonCounter = NSString;
  MTLCommonCounterSet = NSString;

  MTLCounterResultTimestamp = record
    timestamp: UInt64;
  end;

  MTLCounterResultStageUtilization = record
    totalCycles: UInt64;
    vertexCycles: UInt64;
    tessellationCycles: UInt64;
    postTessellationVertexCycles: UInt64;
    fragmentCycles: UInt64;
    renderTargetCycles: UInt64;
  end;

  MTLCounterResultStatistic = record
    tessellationInputPatches: UInt64;
    vertexInvocations: UInt64;
    postTessellationVertexInvocations: UInt64;
    clipperInvocations: UInt64;
    clipperPrimitivesOut: UInt64;
    fragmentInvocations: UInt64;
    fragmentsPassed: UInt64;
    computeKernelInvocations: UInt64;
  end;

  MTLCounterSampleBufferError = NSInteger;
  MTLFeatureSet = NSInteger;
  MTLGPUFamily = NSInteger;
  MTLPipelineOption = NSInteger;
  MTLReadWriteTextureTier = NSInteger;
  MTLArgumentBuffersTier = NSInteger;
  MTLSparseTextureRegionAlignmentMode = NSInteger;

  MTLAccelerationStructureSizes = record
    accelerationStructureSize: NSUInteger;
    buildScratchBufferSize: NSUInteger;
    refitScratchBufferSize: NSUInteger;
  end;

  MTLCounterSamplingPoint = NSInteger;

  MTLSizeAndAlign = record
    size: NSUInteger;
    align: NSUInteger;
  end;

  MTLAutoreleasedRenderPipelineReflection = MTLRenderPipelineReflection;
  MTLAutoreleasedComputePipelineReflection = MTLComputePipelineReflection;

  MTLNewLibraryCompletionHandler = procedure(&library: Pointer; error: NSError) of object;

  MTLNewRenderPipelineStateCompletionHandler = procedure(renderPipelineState: Pointer; error: NSError) of object;

  MTLNewRenderPipelineStateWithReflectionCompletionHandler = procedure(renderPipelineState: Pointer; reflection: MTLRenderPipelineReflection;
    error: NSError) of object;

  MTLNewComputePipelineStateCompletionHandler = procedure(computePipelineState: Pointer; error: NSError) of object;

  MTLNewComputePipelineStateWithReflectionCompletionHandler = procedure(computePipelineState: Pointer; reflection: MTLComputePipelineReflection;
    error: NSError) of object;
  MTLTimestamp = UInt64;
  PMTLTimestamp = ^MTLTimestamp;
  MTLSparseTextureMappingMode = NSInteger;

  MTLMapIndirectArguments = record
    regionOriginX: UInt32;
    regionOriginY: UInt32;
    regionOriginZ: UInt32;
    regionSizeWidth: UInt32;
    regionSizeHeight: UInt32;
    regionSizeDepth: UInt32;
    mipMapLevel: UInt32;
    sliceId: UInt32;
  end;

  MTLLoadAction = NSInteger;
  MTLStoreAction = NSInteger;
  MTLStoreActionOptions = NSInteger;

  MTLClearColor = record
    red: Double;
    green: Double;
    blue: Double;
    alpha: Double;
  end;

  MTLMultisampleDepthResolveFilter = NSInteger;
  MTLMultisampleStencilResolveFilter = NSInteger;
  MTLBlitOption = NSInteger;
  MTLCommandBufferStatus = NSInteger;
  MTLCommandBufferError = NSInteger;
  MTLCommandBufferErrorOption = NSInteger;
  MTLCommandEncoderErrorState = NSInteger;

  MTLCommandBufferHandler = procedure(p1: Pointer) of object;
  MTLDispatchType = NSInteger;

  MTLDispatchThreadgroupsIndirectArguments = record
    threadgroupsPerGrid: array [0..2] of UInt32;
  end;

  MTLStageInRegionIndirectArguments = record
    stageInOrigin: array [0..2] of UInt32;
    stageInSize: array [0..2] of UInt32;
  end;

  MTLCompareFunction = NSInteger;
  MTLStencilOperation = NSInteger;

  MTLDrawablePresentedHandler = procedure(p1: Pointer) of object;
  MTLAttributeFormat = NSInteger;
  MTLIndexType = NSInteger;
  MTLStepFunction = NSInteger;
  MTLMutability = NSInteger;
  MTLPrimitiveType = NSInteger;
  MTLVisibilityResultMode = NSInteger;

  MTLScissorRect = record
    x: NSUInteger;
    y: NSUInteger;
    width: NSUInteger;
    height: NSUInteger;
  end;

  MTLViewport = record
    originX: Double;
    originY: Double;
    width: Double;
    height: Double;
    znear: Double;
    zfar: Double;
  end;

  MTLCullMode = NSInteger;
  MTLWinding = NSInteger;
  MTLDepthClipMode = NSInteger;
  MTLTriangleFillMode = NSInteger;

  MTLDrawPrimitivesIndirectArguments = record
    vertexCount: UInt32;
    instanceCount: UInt32;
    vertexStart: UInt32;
    baseInstance: UInt32;
  end;

  MTLDrawIndexedPrimitivesIndirectArguments = record
    indexCount: UInt32;
    instanceCount: UInt32;
    indexStart: UInt32;
    baseVertex: Int32;
    baseInstance: UInt32;
  end;

  MTLVertexAmplificationViewMapping = record
    viewportArrayIndexOffset: UInt32;
    renderTargetArrayIndexOffset: UInt32;
  end;

  MTLDrawPatchIndirectArguments = record
    patchCount: UInt32;
    instanceCount: UInt32;
    patchStart: UInt32;
    baseInstance: UInt32;
  end;

  MTLQuadTessellationFactorsHalf = record
    edgeTessellationFactor: array [0..3] of UInt16;
    insideTessellationFactor: array [0..1] of UInt16;
  end;

  MTLTriangleTessellationFactorsHalf = record
    edgeTessellationFactor: array [0..2] of UInt16;
    insideTessellationFactor: UInt16;
  end;

  MTLRenderStages = NSInteger;
  MTLBlendFactor = NSInteger;
  MTLBlendOperation = NSInteger;
  MTLColorWriteMask = NSInteger;
  MTLPrimitiveTopologyClass = NSInteger;
  MTLTessellationPartitionMode = NSInteger;
  MTLTessellationFactorStepFunction = NSInteger;
  MTLTessellationFactorFormat = NSInteger;
  MTLTessellationControlPointIndexType = NSInteger;
  MTLVertexFormat = NSInteger;
  MTLVertexStepFunction = NSInteger;
  MTLSamplerMinMagFilter = NSInteger;
  MTLSamplerMipFilter = NSInteger;
  MTLSamplerAddressMode = NSInteger;
  MTLSamplerBorderColor = NSInteger;
  MTLHeapType = NSInteger;
  MTLCaptureError = NSInteger;
  MTLCaptureDestination = NSInteger;
  MTLIndirectCommandType = NSInteger;

  MTLIndirectCommandBufferExecutionRange = record
    location: UInt32;
    length: UInt32;
  end;

  MTLSharedEventNotificationBlock = procedure(p1: Pointer; value: UInt64) of object;
  MTLFunctionLogType = NSInteger;

  _MTLPackedFloat3 = record
    case Integer of
      0: (xyz: record x: Single; y: Single; z: Single end);
      1: (elements: array [0..2] of Single);
  end;

  MTLPackedFloat3 = _MTLPackedFloat3;

  _MTLPackedFloat4x3 = record
    columns: array [0..3] of MTLPackedFloat3;
  end;

  MTLPackedFloat4x3 = _MTLPackedFloat4x3;

  _MTLAxisAlignedBoundingBox = record
    min: MTLPackedFloat3;
    max: MTLPackedFloat3;
  end;

  MTLAxisAlignedBoundingBox = _MTLAxisAlignedBoundingBox;

  MTLAccelerationStructureUsage = NSInteger;
  MTLAccelerationStructureInstanceOptions = NSInteger;

  MTLAccelerationStructureInstanceDescriptor = record
    transformationMatrix: MTLPackedFloat4x3;
    options: MTLAccelerationStructureInstanceOptions;
    mask: UInt32;
    intersectionFunctionTableOffset: UInt32;
    accelerationStructureIndex: UInt32;
  end;

  P_MTLPackedFloat3 = ^_MTLPackedFloat3;
  P_MTLPackedFloat4x3 = ^_MTLPackedFloat4x3;
  P_MTLAxisAlignedBoundingBox = ^_MTLAxisAlignedBoundingBox;
  PMTLAccelerationStructureInstanceDescriptor = ^MTLAccelerationStructureInstanceDescriptor;
  Pid = Pointer;

  MTLDynamicLibraryError = NSInteger;
  MTLBinaryArchiveError = NSInteger;
  MTLIntersectionFunctionSignature = NSInteger;
  TMTLLibraryBlockMethod1 = procedure(&function: Pointer; error: NSError) of object;
  TMTLDeviceBlockMethod1 = procedure(pointer: Pointer; length: NSUInteger) of object;

  MTLCommandEncoder = interface(IObjectiveC)
    ['{FDB9B505-95A5-4DF5-8E1E-BE0AE76A6B01}']
    [MethodName('label')]
    function &label: NSString; cdecl;
    function device: Pointer; cdecl;
    procedure endEncoding; cdecl;
    procedure insertDebugSignpost(&string: NSString); cdecl;
    procedure popDebugGroup; cdecl;
    procedure pushDebugGroup(&string: NSString); cdecl;
    procedure setLabel(&label: NSString); cdecl;
  end;

  MTLResource = interface(IObjectiveC)
    ['{8375DC1D-3972-4BFD-BDEF-21FBCD17E4A5}']
    [MethodName('label')]
    function &label: NSString; cdecl;
    function allocatedSize: NSUInteger; cdecl;
    function cpuCacheMode: MTLCPUCacheMode; cdecl;
    function device: Pointer; cdecl;
    function hazardTrackingMode: MTLHazardTrackingMode; cdecl;
    function heap: Pointer; cdecl;
    function heapOffset: NSUInteger; cdecl;
    function isAliasable: Boolean; cdecl;
    procedure makeAliasable; cdecl;
    function resourceOptions: MTLResourceOptions; cdecl;
    procedure setLabel(&label: NSString); cdecl;
    function setPurgeableState(state: MTLPurgeableState): MTLPurgeableState; cdecl;
    function storageMode: MTLStorageMode; cdecl;
  end;

  MTLBuffer = interface(IObjectiveC)
    ['{8CF5CFBE-EC69-424A-9CB0-C3C5068BA4FD}']
    procedure addDebugMarker(marker: NSString; range: NSRange); cdecl;
    function contents: Pointer; cdecl;
    procedure didModifyRange(range: NSRange); cdecl;
    function length: NSUInteger; cdecl;
    function newTextureWithDescriptor(descriptor: MTLTextureDescriptor; offset: NSUInteger; bytesPerRow: NSUInteger): Pointer; cdecl;
    procedure removeAllDebugMarkers; cdecl;
  end;

  MTLSharedTextureHandleClass = interface(NSObjectClass)
    ['{BDDFCC1D-A729-4D07-B78D-7131A4A5EED9}']
  end;

  MTLSharedTextureHandle = interface(NSObject)
    ['{B4796391-C747-4602-906E-314AA837A547}']
    [MethodName('label')]
    function &label: NSString; cdecl;
    function device: Pointer; cdecl;
  end;
  TMTLSharedTextureHandle = class(TOCGenericImport<MTLSharedTextureHandleClass, MTLSharedTextureHandle>) end;

  MTLTextureDescriptorClass = interface(NSObjectClass)
    ['{3DDEB54E-3F13-4E03-B56F-713EC572ED10}']
    {class} function texture2DDescriptorWithPixelFormat(pixelFormat: MTLPixelFormat; width: NSUInteger; height: NSUInteger; mipmapped: Boolean): MTLTextureDescriptor; cdecl;
    {class} function textureBufferDescriptorWithPixelFormat(pixelFormat: MTLPixelFormat; width: NSUInteger; resourceOptions: MTLResourceOptions; usage: MTLTextureUsage): MTLTextureDescriptor; cdecl;
    {class} function textureCubeDescriptorWithPixelFormat(pixelFormat: MTLPixelFormat; size: NSUInteger; mipmapped: Boolean): MTLTextureDescriptor; cdecl;
  end;

  MTLTextureDescriptor = interface(NSObject)
    ['{4FFF91BF-9411-4E69-B516-3DBBAB64B12D}']
    function allowGPUOptimizedContents: Boolean; cdecl;
    function arrayLength: NSUInteger; cdecl;
    function cpuCacheMode: MTLCPUCacheMode; cdecl;
    function depth: NSUInteger; cdecl;
    function hazardTrackingMode: MTLHazardTrackingMode; cdecl;
    function height: NSUInteger; cdecl;
    function mipmapLevelCount: NSUInteger; cdecl;
    function pixelFormat: MTLPixelFormat; cdecl;
    function resourceOptions: MTLResourceOptions; cdecl;
    function sampleCount: NSUInteger; cdecl;
    procedure setAllowGPUOptimizedContents(allowGPUOptimizedContents: Boolean); cdecl;
    procedure setArrayLength(arrayLength: NSUInteger); cdecl;
    procedure setCpuCacheMode(cpuCacheMode: MTLCPUCacheMode); cdecl;
    procedure setDepth(depth: NSUInteger); cdecl;
    procedure setHazardTrackingMode(hazardTrackingMode: MTLHazardTrackingMode); cdecl;
    procedure setHeight(height: NSUInteger); cdecl;
    procedure setMipmapLevelCount(mipmapLevelCount: NSUInteger); cdecl;
    procedure setPixelFormat(pixelFormat: MTLPixelFormat); cdecl;
    procedure setResourceOptions(resourceOptions: MTLResourceOptions); cdecl;
    procedure setSampleCount(sampleCount: NSUInteger); cdecl;
    procedure setStorageMode(storageMode: MTLStorageMode); cdecl;
    procedure setSwizzle(swizzle: MTLTextureSwizzleChannels); cdecl;
    procedure setTextureType(textureType: MTLTextureType); cdecl;
    procedure setUsage(usage: MTLTextureUsage); cdecl;
    procedure setWidth(width: NSUInteger); cdecl;
    function storageMode: MTLStorageMode; cdecl;
    function swizzle: MTLTextureSwizzleChannels; cdecl;
    function textureType: MTLTextureType; cdecl;
    function usage: MTLTextureUsage; cdecl;
    function width: NSUInteger; cdecl;
  end;
  TMTLTextureDescriptor = class(TOCGenericImport<MTLTextureDescriptorClass, MTLTextureDescriptor>) end;

  MTLTexture = interface(IObjectiveC)
    ['{C20C642D-8974-4990-A9F2-269A5AD1B010}']
    function allowGPUOptimizedContents: Boolean; cdecl;
    function arrayLength: NSUInteger; cdecl;
    function buffer: Pointer; cdecl;
    function bufferBytesPerRow: NSUInteger; cdecl;
    function bufferOffset: NSUInteger; cdecl;
    function depth: NSUInteger; cdecl;
    function firstMipmapInTail: NSUInteger; cdecl;
    procedure getBytes(pixelBytes: Pointer; bytesPerRow: NSUInteger; fromRegion: MTLRegion; mipmapLevel: NSUInteger); overload; cdecl;
    procedure getBytes(pixelBytes: Pointer; bytesPerRow: NSUInteger; bytesPerImage: NSUInteger; fromRegion: MTLRegion; mipmapLevel: NSUInteger;
      slice: NSUInteger); overload; cdecl;
    function height: NSUInteger; cdecl;
    function iosurface: IOSurfaceRef; cdecl;
    function iosurfacePlane: NSUInteger; cdecl;
    function isFramebufferOnly: Boolean; cdecl;
    function isShareable: Boolean; cdecl;
    function isSparse: Boolean; cdecl;
    function mipmapLevelCount: NSUInteger; cdecl;
    function newSharedTextureHandle: MTLSharedTextureHandle; cdecl;
    function newTextureViewWithPixelFormat(pixelFormat: MTLPixelFormat; textureType: MTLTextureType; levels: NSRange;
      slices: NSRange): Pointer; overload; cdecl;
    function newTextureViewWithPixelFormat(pixelFormat: MTLPixelFormat): Pointer; overload; cdecl;
    function newTextureViewWithPixelFormat(pixelFormat: MTLPixelFormat; textureType: MTLTextureType; levels: NSRange; slices: NSRange;
      swizzle: MTLTextureSwizzleChannels): Pointer; overload; cdecl;
    function parentRelativeLevel: NSUInteger; cdecl;
    function parentRelativeSlice: NSUInteger; cdecl;
    function parentTexture: Pointer; cdecl;
    function pixelFormat: MTLPixelFormat; cdecl;
    procedure replaceRegion(region: MTLRegion; mipmapLevel: NSUInteger; withBytes: Pointer; bytesPerRow: NSUInteger); overload; cdecl;
    procedure replaceRegion(region: MTLRegion; mipmapLevel: NSUInteger; slice: NSUInteger; withBytes: Pointer; bytesPerRow: NSUInteger;
      bytesPerImage: NSUInteger); overload; cdecl;
    function rootResource: Pointer; cdecl; // API_DEPRECATED("Use parentTexture or buffer instead", macos(10.11, 10.12), ios(8.0, 10.0))
    function sampleCount: NSUInteger; cdecl;
    function swizzle: MTLTextureSwizzleChannels; cdecl;
    function tailSizeInBytes: NSUInteger; cdecl;
    function textureType: MTLTextureType; cdecl;
    function usage: MTLTextureUsage; cdecl;
    function width: NSUInteger; cdecl;
  end;

  MTLTypeClass = interface(NSObjectClass)
    ['{57AFECD7-165D-4528-8AB3-E5590E1BE323}']
  end;

  MTLType = interface(NSObject)
    ['{E464A197-BCCF-492B-B8B4-F02BB8E26C5D}']
    function dataType: MTLDataType; cdecl;
  end;
  TMTLType = class(TOCGenericImport<MTLTypeClass, MTLType>) end;

  MTLStructMemberClass = interface(NSObjectClass)
    ['{0C0E3E7D-7516-4904-9E39-B479D59886E9}']
  end;

  MTLStructMember = interface(NSObject)
    ['{E13B75A8-E07D-4716-B70E-1B1E6F32FEE4}']
    function argumentIndex: NSUInteger; cdecl;
    function arrayType: MTLArrayType; cdecl;
    function dataType: MTLDataType; cdecl;
    function name: NSString; cdecl;
    function offset: NSUInteger; cdecl;
    function pointerType: MTLPointerType; cdecl;
    function structType: MTLStructType; cdecl;
    function textureReferenceType: MTLTextureReferenceType; cdecl;
  end;
  TMTLStructMember = class(TOCGenericImport<MTLStructMemberClass, MTLStructMember>) end;

  MTLStructTypeClass = interface(MTLTypeClass)
    ['{CC5FF2CE-BE1D-4CEB-A7A9-1D8864A6A56F}']
  end;

  MTLStructType = interface(MTLType)
    ['{F1EEDEEF-EA48-4CAD-AF87-2145C407794E}']
    function memberByName(name: NSString): MTLStructMember; cdecl;
    function members: NSArray; cdecl;
  end;
  TMTLStructType = class(TOCGenericImport<MTLStructTypeClass, MTLStructType>) end;

  MTLArrayTypeClass = interface(MTLTypeClass)
    ['{C0313A4A-35A6-4FA3-B321-68B4D7D3B024}']
  end;

  MTLArrayType = interface(MTLType)
    ['{E683CCEA-C7BF-4A40-AE6F-A0975405CD52}']
    function argumentIndexStride: NSUInteger; cdecl;
    function arrayLength: NSUInteger; cdecl;
    function elementArrayType: MTLArrayType; cdecl;
    function elementPointerType: MTLPointerType; cdecl;
    function elementStructType: MTLStructType; cdecl;
    function elementTextureReferenceType: MTLTextureReferenceType; cdecl;
    function elementType: MTLDataType; cdecl;
    function stride: NSUInteger; cdecl;
  end;
  TMTLArrayType = class(TOCGenericImport<MTLArrayTypeClass, MTLArrayType>) end;

  MTLPointerTypeClass = interface(MTLTypeClass)
    ['{78066278-F0EE-414D-9103-45AD77663981}']
  end;

  MTLPointerType = interface(MTLType)
    ['{B8E010C0-CC72-4BA9-8315-C21BE5960B21}']
    function access: MTLArgumentAccess; cdecl;
    function alignment: NSUInteger; cdecl;
    function dataSize: NSUInteger; cdecl;
    function elementArrayType: MTLArrayType; cdecl;
    function elementIsArgumentBuffer: Boolean; cdecl;
    function elementStructType: MTLStructType; cdecl;
    function elementType: MTLDataType; cdecl;
  end;
  TMTLPointerType = class(TOCGenericImport<MTLPointerTypeClass, MTLPointerType>) end;

  MTLTextureReferenceTypeClass = interface(MTLTypeClass)
    ['{C723AB1F-F32D-47E1-A96B-68D6898328E3}']
  end;

  MTLTextureReferenceType = interface(MTLType)
    ['{492F7016-A660-4721-BBBA-29DB28F9F712}']
    function access: MTLArgumentAccess; cdecl;
    function isDepthTexture: Boolean; cdecl;
    function textureDataType: MTLDataType; cdecl;
    function textureType: MTLTextureType; cdecl;
  end;
  TMTLTextureReferenceType = class(TOCGenericImport<MTLTextureReferenceTypeClass, MTLTextureReferenceType>) end;

  MTLArgumentClass = interface(NSObjectClass)
    ['{3767E40B-2F67-4133-BA7F-B8C71A8CC56A}']
  end;

  MTLArgument = interface(NSObject)
    ['{2313B646-BFB6-45A2-813B-8391CA241307}']
    [MethodName('type')]
    function &type: MTLArgumentType; cdecl;
    function access: MTLArgumentAccess; cdecl;
    function arrayLength: NSUInteger; cdecl;
    function bufferAlignment: NSUInteger; cdecl;
    function bufferDataSize: NSUInteger; cdecl;
    function bufferDataType: MTLDataType; cdecl;
    function bufferPointerType: MTLPointerType; cdecl;
    function bufferStructType: MTLStructType; cdecl;
    function index: NSUInteger; cdecl;
    function isActive: Boolean; cdecl;
    function isDepthTexture: Boolean; cdecl;
    function name: NSString; cdecl;
    function textureDataType: MTLDataType; cdecl;
    function textureType: MTLTextureType; cdecl;
    function threadgroupMemoryAlignment: NSUInteger; cdecl;
    function threadgroupMemoryDataSize: NSUInteger; cdecl;
  end;
  TMTLArgument = class(TOCGenericImport<MTLArgumentClass, MTLArgument>) end;

  MTLFunctionConstantValuesClass = interface(NSObjectClass)
    ['{E63D3544-EB3A-4262-A888-F1DA0BF12FDB}']
  end;

  MTLFunctionConstantValues = interface(NSObject)
    ['{50D8A024-97CB-4BA5-BEB6-5FBD82242DFF}']
    procedure reset; cdecl;
    procedure setConstantValue(value: Pointer; &type: MTLDataType; atIndex: NSUInteger); overload; cdecl;
    procedure setConstantValue(value: Pointer; &type: MTLDataType; withName: NSString); overload; cdecl;
    procedure setConstantValues(values: Pointer; &type: MTLDataType; withRange: NSRange); cdecl;
  end;
  TMTLFunctionConstantValues = class(TOCGenericImport<MTLFunctionConstantValuesClass, MTLFunctionConstantValues>) end;

  MTLFunctionDescriptorClass = interface(NSObjectClass)
    ['{1DD54ABD-16F7-45C6-BF8C-3196205EAAE6}']
    {class} function functionDescriptor: MTLFunctionDescriptor; cdecl;
  end;

  MTLFunctionDescriptor = interface(NSObject)
    ['{78307ADF-DF2A-4834-A80F-82686FB54AF8}']
    function constantValues: MTLFunctionConstantValues; cdecl;
    function name: NSString; cdecl;
    function options: MTLFunctionOptions; cdecl;
    procedure setConstantValues(constantValues: MTLFunctionConstantValues); cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setOptions(options: MTLFunctionOptions); cdecl;
    procedure setSpecializedName(specializedName: NSString); cdecl;
    function specializedName: NSString; cdecl;
  end;
  TMTLFunctionDescriptor = class(TOCGenericImport<MTLFunctionDescriptorClass, MTLFunctionDescriptor>) end;

  MTLIntersectionFunctionDescriptorClass = interface(MTLFunctionDescriptorClass)
    ['{239E8218-82CC-4AE5-825F-175F9B468C0C}']
  end;

  MTLIntersectionFunctionDescriptor = interface(MTLFunctionDescriptor)
    ['{4B7FA4AF-7666-4FD1-B3EB-14E32FE94AEF}']
  end;
  TMTLIntersectionFunctionDescriptor = class(TOCGenericImport<MTLIntersectionFunctionDescriptorClass, MTLIntersectionFunctionDescriptor>) end;

  MTLVertexAttributeClass = interface(NSObjectClass)
    ['{CB87E35C-02B2-47DF-946A-38C676B6EE59}']
  end;

  MTLVertexAttribute = interface(NSObject)
    ['{896CDA66-1FD5-4AC9-8571-811E985308CE}']
    function attributeIndex: NSUInteger; cdecl;
    function attributeType: MTLDataType; cdecl;
    function isActive: Boolean; cdecl;
    function isPatchControlPointData: Boolean; cdecl;
    function isPatchData: Boolean; cdecl;
    function name: NSString; cdecl;
  end;
  TMTLVertexAttribute = class(TOCGenericImport<MTLVertexAttributeClass, MTLVertexAttribute>) end;

  MTLAttributeClass = interface(NSObjectClass)
    ['{804DEC5F-BD5C-4ED1-8194-BAE3BB866801}']
  end;

  MTLAttribute = interface(NSObject)
    ['{DF5FCB9A-C64B-4E01-AB97-90341D6FB7CF}']
    function attributeIndex: NSUInteger; cdecl;
    function attributeType: MTLDataType; cdecl;
    function isActive: Boolean; cdecl;
    function isPatchControlPointData: Boolean; cdecl;
    function isPatchData: Boolean; cdecl;
    function name: NSString; cdecl;
  end;
  TMTLAttribute = class(TOCGenericImport<MTLAttributeClass, MTLAttribute>) end;

  MTLFunctionConstantClass = interface(NSObjectClass)
    ['{A2B4B2E6-8A89-434A-873E-0C7C198CDC79}']
  end;

  MTLFunctionConstant = interface(NSObject)
    ['{E2F9F3B3-00F8-472D-8746-88563C847AC5}']
    [MethodName('type')]
    function &type: MTLDataType; cdecl;
    function index: NSUInteger; cdecl;
    function name: NSString; cdecl;
    function required: Boolean; cdecl;
  end;
  TMTLFunctionConstant = class(TOCGenericImport<MTLFunctionConstantClass, MTLFunctionConstant>) end;

  MTLFunction = interface(IObjectiveC)
    ['{B55DEACA-2E2B-425F-B1AC-853551D3A9B5}']
    [MethodName('label')]
    function &label: NSString; cdecl;
    function device: Pointer; cdecl;
    function functionConstantsDictionary: NSDictionary; cdecl;
    function functionType: MTLFunctionType; cdecl;
    function name: NSString; cdecl;
    function newArgumentEncoderWithBufferIndex(bufferIndex: NSUInteger; reflection: PMTLArgument): Pointer; overload; cdecl;
    function newArgumentEncoderWithBufferIndex(bufferIndex: NSUInteger): Pointer; overload; cdecl;
    function options: MTLFunctionOptions; cdecl;
    function patchControlPointCount: NSInteger; cdecl;
    function patchType: MTLPatchType; cdecl;
    procedure setLabel(&label: NSString); cdecl;
    function stageInputAttributes: NSArray; cdecl;
    function vertexAttributes: NSArray; cdecl;
  end;

  MTLCompileOptionsClass = interface(NSObjectClass)
    ['{A26C439E-33AE-4F2F-A592-8D0C6891C64F}']
  end;

  MTLCompileOptions = interface(NSObject)
    ['{FB306731-8F79-4B96-A6F3-4B4157E154ED}']
    function fastMathEnabled: Boolean; cdecl;
    function installName: NSString; cdecl;
    function languageVersion: MTLLanguageVersion; cdecl;
    function libraries: NSArray; cdecl;
    function libraryType: MTLLibraryType; cdecl;
    function preprocessorMacros: NSDictionary; cdecl;
    function preserveInvariance: Boolean; cdecl;
    procedure setFastMathEnabled(fastMathEnabled: Boolean); cdecl;
    procedure setInstallName(installName: NSString); cdecl;
    procedure setLanguageVersion(languageVersion: MTLLanguageVersion); cdecl;
    procedure setLibraries(libraries: NSArray); cdecl;
    procedure setLibraryType(libraryType: MTLLibraryType); cdecl;
    procedure setPreprocessorMacros(preprocessorMacros: NSDictionary); cdecl;
    procedure setPreserveInvariance(preserveInvariance: Boolean); cdecl;
  end;
  TMTLCompileOptions = class(TOCGenericImport<MTLCompileOptionsClass, MTLCompileOptions>) end;

  MTLLibrary = interface(IObjectiveC)
    ['{FF2237FC-884E-4447-9395-B155E37A0BDF}']
    [MethodName('label')]
    function &label: NSString; cdecl;
    [MethodName('type')]
    function &type: MTLLibraryType; cdecl;
    function device: Pointer; cdecl;
    function functionNames: NSArray; cdecl;
    function installName: NSString; cdecl;
    procedure newFunctionWithDescriptor(descriptor: MTLFunctionDescriptor; completionHandler: TMTLLibraryBlockMethod1); overload; cdecl;
    function newFunctionWithDescriptor(descriptor: MTLFunctionDescriptor; error: PPointer): Pointer; overload; cdecl;
    function newFunctionWithName(functionName: NSString): Pointer; overload; cdecl;
    function newFunctionWithName(name: NSString; constantValues: MTLFunctionConstantValues; error: PPointer): Pointer; overload; cdecl;
    procedure newFunctionWithName(name: NSString; constantValues: MTLFunctionConstantValues;
      completionHandler: TMTLLibraryBlockMethod1); overload; cdecl;
    procedure newIntersectionFunctionWithDescriptor(descriptor: MTLIntersectionFunctionDescriptor;
      completionHandler: TMTLLibraryBlockMethod1); overload; cdecl;
    function newIntersectionFunctionWithDescriptor(descriptor: MTLIntersectionFunctionDescriptor; error: PPointer): Pointer; overload; cdecl;
    procedure setLabel(&label: NSString); cdecl;
  end;

  MTLCounter = interface(IObjectiveC)
    ['{265F00FB-138B-48F8-A3CA-97B09E5D8E3D}']
    function name: NSString; cdecl;
  end;

  MTLCounterSet = interface(IObjectiveC)
    ['{0BEC5FC5-CC79-402C-8894-986E10104C49}']
    function counters: NSArray; cdecl;
    function name: NSString; cdecl;
  end;

  MTLCounterSampleBufferDescriptorClass = interface(NSObjectClass)
    ['{B995E281-E1EA-4F1E-BF3B-086354CFDCCE}']
  end;

  MTLCounterSampleBufferDescriptor = interface(NSObject)
    ['{75F60526-48C3-4154-843B-C854C57A2547}']
    [MethodName('label')]
    function &label: NSString; cdecl;
    function counterSet: Pointer; cdecl;
    function sampleCount: NSUInteger; cdecl;
    procedure setCounterSet(counterSet: Pointer); cdecl;
    procedure setLabel(&label: NSString); cdecl;
    procedure setSampleCount(sampleCount: NSUInteger); cdecl;
    procedure setStorageMode(storageMode: MTLStorageMode); cdecl;
    function storageMode: MTLStorageMode; cdecl;
  end;
  TMTLCounterSampleBufferDescriptor = class(TOCGenericImport<MTLCounterSampleBufferDescriptorClass, MTLCounterSampleBufferDescriptor>) end;

  MTLCounterSampleBuffer = interface(IObjectiveC)
    ['{DCC7D973-87D8-4908-A2DB-4009875672D5}']
    [MethodName('label')]
    function &label: NSString; cdecl;
    function device: Pointer; cdecl;
    function resolveCounterRange(range: NSRange): NSData; cdecl;
    function sampleCount: NSUInteger; cdecl;
  end;

  MTLArgumentDescriptorClass = interface(NSObjectClass)
    ['{24179177-3AE0-4289-9F01-159CED016C33}']
    {class} function argumentDescriptor: MTLArgumentDescriptor; cdecl;
  end;

  MTLArgumentDescriptor = interface(NSObject)
    ['{170C126D-3732-43BD-97D0-A0DCC8F38819}']
    function access: MTLArgumentAccess; cdecl;
    function arrayLength: NSUInteger; cdecl;
    function constantBlockAlignment: NSUInteger; cdecl;
    function dataType: MTLDataType; cdecl;
    function index: NSUInteger; cdecl;
    procedure setAccess(access: MTLArgumentAccess); cdecl;
    procedure setArrayLength(arrayLength: NSUInteger); cdecl;
    procedure setConstantBlockAlignment(constantBlockAlignment: NSUInteger); cdecl;
    procedure setDataType(dataType: MTLDataType); cdecl;
    procedure setIndex(index: NSUInteger); cdecl;
    procedure setTextureType(textureType: MTLTextureType); cdecl;
    function textureType: MTLTextureType; cdecl;
  end;
  TMTLArgumentDescriptor = class(TOCGenericImport<MTLArgumentDescriptorClass, MTLArgumentDescriptor>) end;

  MTLDevice = interface(IObjectiveC)
    ['{C811974B-7472-47F6-B86F-022F07CD9B79}']
    function accelerationStructureSizesWithDescriptor(descriptor: MTLAccelerationStructureDescriptor): MTLAccelerationStructureSizes; cdecl;
    function areBarycentricCoordsSupported: Boolean; cdecl;
    function areProgrammableSamplePositionsSupported: Boolean; cdecl;
    function areRasterOrderGroupsSupported: Boolean; cdecl;
    function argumentBuffersSupport: MTLArgumentBuffersTier; cdecl;
    procedure convertSparsePixelRegions(pixelRegions: PMTLRegion; toTileRegions: PMTLRegion; withTileSize: MTLSize;
      alignmentMode: MTLSparseTextureRegionAlignmentMode; numRegions: NSUInteger); cdecl;
    procedure convertSparseTileRegions(tileRegions: PMTLRegion; toPixelRegions: PMTLRegion; withTileSize: MTLSize; numRegions: NSUInteger); cdecl;
    function counterSets: NSArray; cdecl;
    function currentAllocatedSize: NSUInteger; cdecl;
    procedure getDefaultSamplePositions(positions: PMTLSamplePosition; count: NSUInteger); cdecl;
    function hasUnifiedMemory: Boolean; cdecl;
    function heapBufferSizeAndAlignWithLength(length: NSUInteger; options: MTLResourceOptions): MTLSizeAndAlign; cdecl;
    function heapTextureSizeAndAlignWithDescriptor(desc: MTLTextureDescriptor): MTLSizeAndAlign; cdecl;
    function isDepth24Stencil8PixelFormatSupported: Boolean; cdecl;
    function isHeadless: Boolean; cdecl;
    function isLowPower: Boolean; cdecl;
    function isRemovable: Boolean; cdecl;
    function maxArgumentBufferSamplerCount: NSUInteger; cdecl;
    function maxBufferLength: NSUInteger; cdecl;
    function maxThreadgroupMemoryLength: NSUInteger; cdecl;
    function maxThreadsPerThreadgroup: MTLSize; cdecl;
    function minimumLinearTextureAlignmentForPixelFormat(format: MTLPixelFormat): NSUInteger; cdecl;
    function minimumTextureBufferAlignmentForPixelFormat(format: MTLPixelFormat): NSUInteger; cdecl;
    function name: NSString; cdecl;
    function newAccelerationStructureWithDescriptor(descriptor: MTLAccelerationStructureDescriptor): Pointer; cdecl;
    function newAccelerationStructureWithSize(size: NSUInteger): Pointer; cdecl;
    function newArgumentEncoderWithArguments(arguments: NSArray): Pointer; cdecl;
    function newBinaryArchiveWithDescriptor(descriptor: MTLBinaryArchiveDescriptor; error: PPointer): Pointer; cdecl;
    function newBufferWithBytes(pointer: Pointer; length: NSUInteger; options: MTLResourceOptions): Pointer; cdecl;
    function newBufferWithBytesNoCopy(pointer: Pointer; length: NSUInteger; options: MTLResourceOptions; deallocator: TMTLDeviceBlockMethod1): Pointer; cdecl;
    function newBufferWithLength(length: NSUInteger; options: MTLResourceOptions): Pointer; cdecl;
    function newCommandQueue: Pointer; cdecl;
    function newCommandQueueWithMaxCommandBufferCount(maxCommandBufferCount: NSUInteger): Pointer; cdecl;
    function newComputePipelineStateWithDescriptor(descriptor: MTLComputePipelineDescriptor; options: MTLPipelineOption;
      reflection: PMTLComputePipelineReflection; error: PPointer): Pointer; overload; cdecl;
    procedure newComputePipelineStateWithDescriptor(descriptor: MTLComputePipelineDescriptor; options: MTLPipelineOption;
      completionHandler: MTLNewComputePipelineStateWithReflectionCompletionHandler); overload; cdecl;
    function newComputePipelineStateWithFunction(computeFunction: Pointer; options: MTLPipelineOption; reflection: PMTLComputePipelineReflection;
      error: PPointer): Pointer; overload; cdecl;
    function newComputePipelineStateWithFunction(computeFunction: Pointer; error: PPointer): Pointer; overload; cdecl;
    procedure newComputePipelineStateWithFunction(computeFunction: Pointer;
      completionHandler: MTLNewComputePipelineStateCompletionHandler); overload; cdecl;
    procedure newComputePipelineStateWithFunction(computeFunction: Pointer; options: MTLPipelineOption;
      completionHandler: MTLNewComputePipelineStateWithReflectionCompletionHandler); overload; cdecl;
    function newCounterSampleBufferWithDescriptor(descriptor: MTLCounterSampleBufferDescriptor; error: PPointer): Pointer; cdecl;
    function newDefaultLibrary: Pointer; cdecl;
    function newDefaultLibraryWithBundle(bundle: NSBundle; error: PPointer): Pointer; cdecl;
    function newDepthStencilStateWithDescriptor(descriptor: MTLDepthStencilDescriptor): Pointer; cdecl;
    function newDynamicLibrary(&library: Pointer; error: PPointer): Pointer; cdecl;
    function newDynamicLibraryWithURL(url: NSURL; error: PPointer): Pointer; cdecl;
    function newEvent: Pointer; cdecl;
    function newFence: Pointer; cdecl;
    function newHeapWithDescriptor(descriptor: MTLHeapDescriptor): Pointer; cdecl;
    function newIndirectCommandBufferWithDescriptor(descriptor: MTLIndirectCommandBufferDescriptor; maxCommandCount: NSUInteger;
      options: MTLResourceOptions): Pointer; cdecl;
    function newLibraryWithData(data: dispatch_data_t; error: PPointer): Pointer; cdecl;
    function newLibraryWithFile(filepath: NSString; error: PPointer): Pointer; cdecl;
    function newLibraryWithSource(source: NSString; options: MTLCompileOptions; error: PPointer): Pointer; overload; cdecl;
    procedure newLibraryWithSource(source: NSString; options: MTLCompileOptions; completionHandler: MTLNewLibraryCompletionHandler); overload; cdecl;
    function newLibraryWithURL(url: NSURL; error: PPointer): Pointer; cdecl;
    function newRasterizationRateMapWithDescriptor(descriptor: MTLRasterizationRateMapDescriptor): Pointer; cdecl;
    procedure newRenderPipelineStateWithDescriptor(descriptor: MTLRenderPipelineDescriptor; options: MTLPipelineOption;
      completionHandler: MTLNewRenderPipelineStateWithReflectionCompletionHandler); overload; cdecl;
    procedure newRenderPipelineStateWithDescriptor(descriptor: MTLRenderPipelineDescriptor;
      completionHandler: MTLNewRenderPipelineStateCompletionHandler); overload; cdecl;
    function newRenderPipelineStateWithDescriptor(descriptor: MTLRenderPipelineDescriptor; error: PPointer): Pointer; overload; cdecl;
    function newRenderPipelineStateWithDescriptor(descriptor: MTLRenderPipelineDescriptor; options: MTLPipelineOption;
      reflection: PMTLRenderPipelineReflection; error: PPointer): Pointer; overload; cdecl;
    function newRenderPipelineStateWithTileDescriptor(descriptor: MTLTileRenderPipelineDescriptor; options: MTLPipelineOption;
      reflection: PMTLRenderPipelineReflection; error: PPointer): Pointer; overload; cdecl;
    procedure newRenderPipelineStateWithTileDescriptor(descriptor: MTLTileRenderPipelineDescriptor; options: MTLPipelineOption;
      completionHandler: MTLNewRenderPipelineStateWithReflectionCompletionHandler); overload; cdecl;
    function newSamplerStateWithDescriptor(descriptor: MTLSamplerDescriptor): Pointer; cdecl;
    function newSharedEvent: Pointer; cdecl;
    function newSharedEventWithHandle(sharedEventHandle: MTLSharedEventHandle): Pointer; cdecl;
    function newSharedTextureWithDescriptor(descriptor: MTLTextureDescriptor): Pointer; cdecl;
    function newSharedTextureWithHandle(sharedHandle: MTLSharedTextureHandle): Pointer; cdecl;
    function newTextureWithDescriptor(descriptor: MTLTextureDescriptor; iosurface: IOSurfaceRef; plane: NSUInteger): Pointer; overload; cdecl;
    function newTextureWithDescriptor(descriptor: MTLTextureDescriptor): Pointer; overload; cdecl;
    function readWriteTextureSupport: MTLReadWriteTextureTier; cdecl;
    function recommendedMaxWorkingSetSize: UInt64; cdecl;
    function registryID: UInt64; cdecl;
    procedure sampleTimestamps(cpuTimestamp: PMTLTimestamp; gpuTimestamp: PMTLTimestamp); cdecl;
    function sparseTileSizeInBytes: NSUInteger; cdecl;
    function sparseTileSizeWithTextureType(textureType: MTLTextureType; pixelFormat: MTLPixelFormat; sampleCount: NSUInteger): MTLSize; cdecl;
    function supports32BitFloatFiltering: Boolean; cdecl;
    function supports32BitMSAA: Boolean; cdecl;
    function supportsBCTextureCompression: Boolean; cdecl;
    function supportsCounterSampling(samplingPoint: MTLCounterSamplingPoint): Boolean; cdecl;
    function supportsDynamicLibraries: Boolean; cdecl;
    function supportsFamily(gpuFamily: MTLGPUFamily): Boolean; cdecl;
    function supportsFeatureSet(featureSet: MTLFeatureSet): Boolean; cdecl;
    function supportsFunctionPointers: Boolean; cdecl;
    function supportsPullModelInterpolation: Boolean; cdecl;
    function supportsRasterizationRateMapWithLayerCount(layerCount: NSUInteger): Boolean; cdecl;
    function supportsRaytracing: Boolean; cdecl;
    function supportsShaderBarycentricCoordinates: Boolean; cdecl;
    function supportsTextureSampleCount(sampleCount: NSUInteger): Boolean; cdecl;
    function supportsVertexAmplificationCount(count: NSUInteger): Boolean; cdecl;
  end;

  MTLFence = interface(IObjectiveC)
    ['{BC1BFAAF-DE5E-425E-A7D2-F79F1F922A06}']
    [MethodName('label')]
    function &label: NSString; cdecl;
    function device: Pointer; cdecl;
    procedure setLabel(&label: NSString); cdecl;
  end;

  MTLResourceStatePassSampleBufferAttachmentDescriptorClass = interface(NSObjectClass)
    ['{C9220C70-E1D6-4586-9655-D08FC4951453}']
  end;

  MTLResourceStatePassSampleBufferAttachmentDescriptor = interface(NSObject)
    ['{3007A722-BC42-4430-9BA8-6A2559627180}']
    function endOfEncoderSampleIndex: NSUInteger; cdecl;
    function sampleBuffer: Pointer; cdecl;
    procedure setEndOfEncoderSampleIndex(endOfEncoderSampleIndex: NSUInteger); cdecl;
    procedure setSampleBuffer(sampleBuffer: Pointer); cdecl;
    procedure setStartOfEncoderSampleIndex(startOfEncoderSampleIndex: NSUInteger); cdecl;
    function startOfEncoderSampleIndex: NSUInteger; cdecl;
  end;
  TMTLResourceStatePassSampleBufferAttachmentDescriptor = class(TOCGenericImport<MTLResourceStatePassSampleBufferAttachmentDescriptorClass,
    MTLResourceStatePassSampleBufferAttachmentDescriptor>) end;

  MTLResourceStatePassSampleBufferAttachmentDescriptorArrayClass = interface(NSObjectClass)
    ['{47A8C274-1283-4A56-8CF8-E78D076EB659}']
  end;

  MTLResourceStatePassSampleBufferAttachmentDescriptorArray = interface(NSObject)
    ['{26E2C4C6-F386-4C5B-BC87-AF2B69B20B4A}']
    function objectAtIndexedSubscript(attachmentIndex: NSUInteger): MTLResourceStatePassSampleBufferAttachmentDescriptor; cdecl;
    procedure setObject(attachment: MTLResourceStatePassSampleBufferAttachmentDescriptor; atIndexedSubscript: NSUInteger); cdecl;
  end;
  TMTLResourceStatePassSampleBufferAttachmentDescriptorArray = class(TOCGenericImport<MTLResourceStatePassSampleBufferAttachmentDescriptorArrayClass,
    MTLResourceStatePassSampleBufferAttachmentDescriptorArray>) end;

  MTLResourceStatePassDescriptorClass = interface(NSObjectClass)
    ['{59F187B4-58F5-4A18-B74B-98693045FB76}']
    {class} function resourceStatePassDescriptor: MTLResourceStatePassDescriptor; cdecl;
  end;

  MTLResourceStatePassDescriptor = interface(NSObject)
    ['{AD6448B5-151C-4594-BB42-8A6ADF35DF2A}']
    function sampleBufferAttachments: MTLResourceStatePassSampleBufferAttachmentDescriptorArray; cdecl;
  end;
  TMTLResourceStatePassDescriptor = class(TOCGenericImport<MTLResourceStatePassDescriptorClass, MTLResourceStatePassDescriptor>) end;

  MTLResourceStateCommandEncoder = interface(IObjectiveC)
    ['{01E2A049-91BC-4F27-995A-2512014CC340}']
    procedure updateFence(fence: Pointer); cdecl;
    procedure updateTextureMapping(texture: Pointer; mode: MTLSparseTextureMappingMode; indirectBuffer: Pointer;
      indirectBufferOffset: NSUInteger); overload; cdecl;
    procedure updateTextureMapping(texture: Pointer; mode: MTLSparseTextureMappingMode; region: MTLRegion; mipLevel: NSUInteger;
      slice: NSUInteger); overload; cdecl;
    procedure updateTextureMappings(texture: Pointer; mode: MTLSparseTextureMappingMode; regions: PMTLRegion; mipLevels: PNSUInteger;
      slices: PNSUInteger; numRegions: NSUInteger); cdecl;
    procedure waitForFence(fence: Pointer); cdecl;
  end;

  MTLRenderPassAttachmentDescriptorClass = interface(NSObjectClass)
    ['{20DB0D54-A648-40C7-8572-32DCD7BEF9F0}']
  end;

  MTLRenderPassAttachmentDescriptor = interface(NSObject)
    ['{BFA4343F-7472-428C-A2B8-CF9AA9668435}']
    function depthPlane: NSUInteger; cdecl;
    function level: NSUInteger; cdecl;
    function loadAction: MTLLoadAction; cdecl;
    function resolveDepthPlane: NSUInteger; cdecl;
    function resolveLevel: NSUInteger; cdecl;
    function resolveSlice: NSUInteger; cdecl;
    function resolveTexture: Pointer; cdecl;
    procedure setDepthPlane(depthPlane: NSUInteger); cdecl;
    procedure setLevel(level: NSUInteger); cdecl;
    procedure setLoadAction(loadAction: MTLLoadAction); cdecl;
    procedure setResolveDepthPlane(resolveDepthPlane: NSUInteger); cdecl;
    procedure setResolveLevel(resolveLevel: NSUInteger); cdecl;
    procedure setResolveSlice(resolveSlice: NSUInteger); cdecl;
    procedure setResolveTexture(resolveTexture: Pointer); cdecl;
    procedure setSlice(slice: NSUInteger); cdecl;
    procedure setStoreAction(storeAction: MTLStoreAction); cdecl;
    procedure setStoreActionOptions(storeActionOptions: MTLStoreActionOptions); cdecl;
    procedure setTexture(texture: Pointer); cdecl;
    function slice: NSUInteger; cdecl;
    function storeAction: MTLStoreAction; cdecl;
    function storeActionOptions: MTLStoreActionOptions; cdecl;
    function texture: Pointer; cdecl;
  end;
  TMTLRenderPassAttachmentDescriptor = class(TOCGenericImport<MTLRenderPassAttachmentDescriptorClass, MTLRenderPassAttachmentDescriptor>) end;

  MTLRenderPassColorAttachmentDescriptorClass = interface(MTLRenderPassAttachmentDescriptorClass)
    ['{0FEC13B6-1D1F-4E18-AAEB-23EF9A8AEB4F}']
  end;

  MTLRenderPassColorAttachmentDescriptor = interface(MTLRenderPassAttachmentDescriptor)
    ['{2D5544B3-3150-48F9-AA8B-E2DDC3BA4F42}']
    function clearColor: MTLClearColor; cdecl;
    procedure setClearColor(clearColor: MTLClearColor); cdecl;
  end;
  TMTLRenderPassColorAttachmentDescriptor = class(TOCGenericImport<MTLRenderPassColorAttachmentDescriptorClass,
    MTLRenderPassColorAttachmentDescriptor>) end;

  MTLRenderPassDepthAttachmentDescriptorClass = interface(MTLRenderPassAttachmentDescriptorClass)
    ['{118505EA-2A76-485D-9BFE-5425486421D7}']
  end;

  MTLRenderPassDepthAttachmentDescriptor = interface(MTLRenderPassAttachmentDescriptor)
    ['{504BF323-ACFC-4026-BFDD-806AA7087408}']
    function clearDepth: Double; cdecl;
    function depthResolveFilter: MTLMultisampleDepthResolveFilter; cdecl;
    procedure setClearDepth(clearDepth: Double); cdecl;
    procedure setDepthResolveFilter(depthResolveFilter: MTLMultisampleDepthResolveFilter); cdecl;
  end;
  TMTLRenderPassDepthAttachmentDescriptor = class(TOCGenericImport<MTLRenderPassDepthAttachmentDescriptorClass,
    MTLRenderPassDepthAttachmentDescriptor>) end;

  MTLRenderPassStencilAttachmentDescriptorClass = interface(MTLRenderPassAttachmentDescriptorClass)
    ['{0A0A536D-872B-4E74-B393-9FBC2FD6FC53}']
  end;

  MTLRenderPassStencilAttachmentDescriptor = interface(MTLRenderPassAttachmentDescriptor)
    ['{CCBCFBC8-9C4C-45EF-898F-B62555E3621C}']
    function clearStencil: UInt32; cdecl;
    procedure setClearStencil(clearStencil: UInt32); cdecl;
    procedure setStencilResolveFilter(stencilResolveFilter: MTLMultisampleStencilResolveFilter); cdecl;
    function stencilResolveFilter: MTLMultisampleStencilResolveFilter; cdecl;
  end;
  TMTLRenderPassStencilAttachmentDescriptor = class(TOCGenericImport<MTLRenderPassStencilAttachmentDescriptorClass,
    MTLRenderPassStencilAttachmentDescriptor>) end;

  MTLRenderPassColorAttachmentDescriptorArrayClass = interface(NSObjectClass)
    ['{867C7206-9CAC-458A-8D93-8D9F658DA87A}']
  end;

  MTLRenderPassColorAttachmentDescriptorArray = interface(NSObject)
    ['{AC26D6BC-372E-4813-A2C2-DBCDC3B609B9}']
    function objectAtIndexedSubscript(attachmentIndex: NSUInteger): MTLRenderPassColorAttachmentDescriptor; cdecl;
    procedure setObject(attachment: MTLRenderPassColorAttachmentDescriptor; atIndexedSubscript: NSUInteger); cdecl;
  end;
  TMTLRenderPassColorAttachmentDescriptorArray = class(TOCGenericImport<MTLRenderPassColorAttachmentDescriptorArrayClass,
    MTLRenderPassColorAttachmentDescriptorArray>) end;

  MTLRenderPassSampleBufferAttachmentDescriptorClass = interface(NSObjectClass)
    ['{DFF233AB-9D2A-4C16-8640-29627D34E39F}']
  end;

  MTLRenderPassSampleBufferAttachmentDescriptor = interface(NSObject)
    ['{8E56DF81-9FC1-4CFE-8D12-3898304B2EFF}']
    function endOfFragmentSampleIndex: NSUInteger; cdecl;
    function endOfVertexSampleIndex: NSUInteger; cdecl;
    function sampleBuffer: Pointer; cdecl;
    procedure setEndOfFragmentSampleIndex(endOfFragmentSampleIndex: NSUInteger); cdecl;
    procedure setEndOfVertexSampleIndex(endOfVertexSampleIndex: NSUInteger); cdecl;
    procedure setSampleBuffer(sampleBuffer: Pointer); cdecl;
    procedure setStartOfFragmentSampleIndex(startOfFragmentSampleIndex: NSUInteger); cdecl;
    procedure setStartOfVertexSampleIndex(startOfVertexSampleIndex: NSUInteger); cdecl;
    function startOfFragmentSampleIndex: NSUInteger; cdecl;
    function startOfVertexSampleIndex: NSUInteger; cdecl;
  end;
  TMTLRenderPassSampleBufferAttachmentDescriptor = class(TOCGenericImport<MTLRenderPassSampleBufferAttachmentDescriptorClass,
    MTLRenderPassSampleBufferAttachmentDescriptor>) end;

  MTLRenderPassSampleBufferAttachmentDescriptorArrayClass = interface(NSObjectClass)
    ['{0EF2472F-DDF2-467C-BC08-B067CE28F5F9}']
  end;

  MTLRenderPassSampleBufferAttachmentDescriptorArray = interface(NSObject)
    ['{D16D20D4-A13F-4D9D-B568-57E6BB499E87}']
    function objectAtIndexedSubscript(attachmentIndex: NSUInteger): MTLRenderPassSampleBufferAttachmentDescriptor; cdecl;
    procedure setObject(attachment: MTLRenderPassSampleBufferAttachmentDescriptor; atIndexedSubscript: NSUInteger); cdecl;
  end;
  TMTLRenderPassSampleBufferAttachmentDescriptorArray = class(TOCGenericImport<MTLRenderPassSampleBufferAttachmentDescriptorArrayClass,
    MTLRenderPassSampleBufferAttachmentDescriptorArray>) end;

  MTLRenderPassDescriptorClass = interface(NSObjectClass)
    ['{A5FD125E-54C4-45BB-AE68-6591316ACB54}']
    {class} function renderPassDescriptor: MTLRenderPassDescriptor; cdecl;
  end;

  MTLRenderPassDescriptor = interface(NSObject)
    ['{3A4BD6CE-914C-47F3-B7A8-48410445C944}']
    function colorAttachments: MTLRenderPassColorAttachmentDescriptorArray; cdecl;
    function defaultRasterSampleCount: NSUInteger; cdecl;
    function depthAttachment: MTLRenderPassDepthAttachmentDescriptor; cdecl;
    function getSamplePositions(positions: PMTLSamplePosition; count: NSUInteger): NSUInteger; cdecl;
    function imageblockSampleLength: NSUInteger; cdecl;
    function rasterizationRateMap: Pointer; cdecl;
    function renderTargetArrayLength: NSUInteger; cdecl;
    function renderTargetHeight: NSUInteger; cdecl;
    function renderTargetWidth: NSUInteger; cdecl;
    function sampleBufferAttachments: MTLRenderPassSampleBufferAttachmentDescriptorArray; cdecl;
    procedure setDefaultRasterSampleCount(defaultRasterSampleCount: NSUInteger); cdecl;
    procedure setDepthAttachment(depthAttachment: MTLRenderPassDepthAttachmentDescriptor); cdecl;
    procedure setImageblockSampleLength(imageblockSampleLength: NSUInteger); cdecl;
    procedure setRasterizationRateMap(rasterizationRateMap: Pointer); cdecl;
    procedure setRenderTargetArrayLength(renderTargetArrayLength: NSUInteger); cdecl;
    procedure setRenderTargetHeight(renderTargetHeight: NSUInteger); cdecl;
    procedure setRenderTargetWidth(renderTargetWidth: NSUInteger); cdecl;
    procedure setSamplePositions(positions: PMTLSamplePosition; count: NSUInteger); cdecl;
    procedure setStencilAttachment(stencilAttachment: MTLRenderPassStencilAttachmentDescriptor); cdecl;
    procedure setThreadgroupMemoryLength(threadgroupMemoryLength: NSUInteger); cdecl;
    procedure setTileHeight(tileHeight: NSUInteger); cdecl;
    procedure setTileWidth(tileWidth: NSUInteger); cdecl;
    procedure setVisibilityResultBuffer(visibilityResultBuffer: Pointer); cdecl;
    function stencilAttachment: MTLRenderPassStencilAttachmentDescriptor; cdecl;
    function threadgroupMemoryLength: NSUInteger; cdecl;
    function tileHeight: NSUInteger; cdecl;
    function tileWidth: NSUInteger; cdecl;
    function visibilityResultBuffer: Pointer; cdecl;
  end;
  TMTLRenderPassDescriptor = class(TOCGenericImport<MTLRenderPassDescriptorClass, MTLRenderPassDescriptor>) end;

  MTLBlitPassSampleBufferAttachmentDescriptorClass = interface(NSObjectClass)
    ['{72F4A3E4-2C77-4610-887C-B9BEEAC083EF}']
  end;

  MTLBlitPassSampleBufferAttachmentDescriptor = interface(NSObject)
    ['{86038783-B327-476C-9D34-E0672195D26C}']
    function endOfEncoderSampleIndex: NSUInteger; cdecl;
    function sampleBuffer: Pointer; cdecl;
    procedure setEndOfEncoderSampleIndex(endOfEncoderSampleIndex: NSUInteger); cdecl;
    procedure setSampleBuffer(sampleBuffer: Pointer); cdecl;
    procedure setStartOfEncoderSampleIndex(startOfEncoderSampleIndex: NSUInteger); cdecl;
    function startOfEncoderSampleIndex: NSUInteger; cdecl;
  end;
  TMTLBlitPassSampleBufferAttachmentDescriptor = class(TOCGenericImport<MTLBlitPassSampleBufferAttachmentDescriptorClass,
    MTLBlitPassSampleBufferAttachmentDescriptor>) end;

  MTLBlitPassSampleBufferAttachmentDescriptorArrayClass = interface(NSObjectClass)
    ['{EB1E263C-2C8F-403D-BF2F-A4F2876F5D05}']
  end;

  MTLBlitPassSampleBufferAttachmentDescriptorArray = interface(NSObject)
    ['{960A71BC-A103-4A23-AE01-549A27E3513D}']
    function objectAtIndexedSubscript(attachmentIndex: NSUInteger): MTLBlitPassSampleBufferAttachmentDescriptor; cdecl;
    procedure setObject(attachment: MTLBlitPassSampleBufferAttachmentDescriptor; atIndexedSubscript: NSUInteger); cdecl;
  end;
  TMTLBlitPassSampleBufferAttachmentDescriptorArray = class(TOCGenericImport<MTLBlitPassSampleBufferAttachmentDescriptorArrayClass,
    MTLBlitPassSampleBufferAttachmentDescriptorArray>) end;

  MTLBlitPassDescriptorClass = interface(NSObjectClass)
    ['{980E06E1-0553-4EEB-A1B7-F100FB89AA76}']
    {class} function blitPassDescriptor: MTLBlitPassDescriptor; cdecl;
  end;

  MTLBlitPassDescriptor = interface(NSObject)
    ['{BF195689-F530-475F-818E-D68F5B6B46E4}']
    function sampleBufferAttachments: MTLBlitPassSampleBufferAttachmentDescriptorArray; cdecl;
  end;
  TMTLBlitPassDescriptor = class(TOCGenericImport<MTLBlitPassDescriptorClass, MTLBlitPassDescriptor>) end;

  MTLBlitCommandEncoder = interface(IObjectiveC)
    ['{5C555D59-EE76-42A0-85AF-70B8E0A95870}']
    procedure copyFromBuffer(sourceBuffer: Pointer; sourceOffset: NSUInteger; sourceBytesPerRow: NSUInteger; sourceBytesPerImage: NSUInteger;
      sourceSize: MTLSize; toTexture: Pointer; destinationSlice: NSUInteger; destinationLevel: NSUInteger;
      destinationOrigin: MTLOrigin); overload; cdecl;
    procedure copyFromBuffer(sourceBuffer: Pointer; sourceOffset: NSUInteger; sourceBytesPerRow: NSUInteger; sourceBytesPerImage: NSUInteger;
      sourceSize: MTLSize; toTexture: Pointer; destinationSlice: NSUInteger; destinationLevel: NSUInteger; destinationOrigin: MTLOrigin;
      options: MTLBlitOption); overload; cdecl;
    procedure copyFromBuffer(sourceBuffer: Pointer; sourceOffset: NSUInteger; toBuffer: Pointer; destinationOffset: NSUInteger;
      size: NSUInteger); overload; cdecl;
    procedure copyFromTexture(sourceTexture: Pointer; sourceSlice: NSUInteger; sourceLevel: NSUInteger; sourceOrigin: MTLOrigin;
      sourceSize: MTLSize; toBuffer: Pointer; destinationOffset: NSUInteger; destinationBytesPerRow: NSUInteger;
        destinationBytesPerImage: NSUInteger; options: MTLBlitOption); overload; cdecl;
    procedure copyFromTexture(sourceTexture: Pointer; sourceSlice: NSUInteger; sourceLevel: NSUInteger; toTexture: Pointer;
      destinationSlice: NSUInteger; destinationLevel: NSUInteger; sliceCount: NSUInteger; levelCount: NSUInteger); overload; cdecl;
    procedure copyFromTexture(sourceTexture: Pointer; toTexture: Pointer); overload; cdecl;
    procedure copyFromTexture(sourceTexture: Pointer; sourceSlice: NSUInteger; sourceLevel: NSUInteger; sourceOrigin: MTLOrigin;
      sourceSize: MTLSize; toBuffer: Pointer; destinationOffset: NSUInteger; destinationBytesPerRow: NSUInteger;
      destinationBytesPerImage: NSUInteger); overload; cdecl;
    procedure copyFromTexture(sourceTexture: Pointer; sourceSlice: NSUInteger; sourceLevel: NSUInteger; sourceOrigin: MTLOrigin;
      sourceSize: MTLSize; toTexture: Pointer; destinationSlice: NSUInteger; destinationLevel: NSUInteger;
      destinationOrigin: MTLOrigin); overload; cdecl;
    procedure copyIndirectCommandBuffer(source: Pointer; sourceRange: NSRange; destination: Pointer; destinationIndex: NSUInteger); cdecl;
    procedure fillBuffer(buffer: Pointer; range: NSRange; value: UInt8); cdecl;
    procedure generateMipmapsForTexture(texture: Pointer); cdecl;
    procedure getTextureAccessCounters(texture: Pointer; region: MTLRegion; mipLevel: NSUInteger; slice: NSUInteger; resetCounters: Boolean;
      countersBuffer: Pointer; countersBufferOffset: NSUInteger); cdecl;
    procedure optimizeContentsForCPUAccess(texture: Pointer; slice: NSUInteger; level: NSUInteger); overload; cdecl;
    procedure optimizeContentsForCPUAccess(texture: Pointer); overload; cdecl;
    procedure optimizeContentsForGPUAccess(texture: Pointer; slice: NSUInteger; level: NSUInteger); overload; cdecl;
    procedure optimizeContentsForGPUAccess(texture: Pointer); overload; cdecl;
    procedure optimizeIndirectCommandBuffer(indirectCommandBuffer: Pointer; withRange: NSRange); cdecl;
    procedure resetCommandsInBuffer(buffer: Pointer; withRange: NSRange); cdecl;
    procedure resetTextureAccessCounters(texture: Pointer; region: MTLRegion; mipLevel: NSUInteger; slice: NSUInteger); cdecl;
    procedure resolveCounters(sampleBuffer: Pointer; inRange: NSRange; destinationBuffer: Pointer; destinationOffset: NSUInteger); cdecl;
    procedure sampleCountersInBuffer(sampleBuffer: Pointer; atSampleIndex: NSUInteger; withBarrier: Boolean); cdecl;
    procedure synchronizeResource(resource: Pointer); cdecl;
    procedure synchronizeTexture(texture: Pointer; slice: NSUInteger; level: NSUInteger); cdecl;
    procedure updateFence(fence: Pointer); cdecl;
    procedure waitForFence(fence: Pointer); cdecl;
  end;

  MTLCommandBufferDescriptorClass = interface(NSObjectClass)
    ['{2A499736-6CC0-4A49-82AC-9F59D99ABD38}']
  end;

  MTLCommandBufferDescriptor = interface(NSObject)
    ['{AEB40006-95D4-4975-816C-F3A5BF358943}']
    function errorOptions: MTLCommandBufferErrorOption; cdecl;
    function retainedReferences: Boolean; cdecl;
    procedure setErrorOptions(errorOptions: MTLCommandBufferErrorOption); cdecl;
    procedure setRetainedReferences(retainedReferences: Boolean); cdecl;
  end;
  TMTLCommandBufferDescriptor = class(TOCGenericImport<MTLCommandBufferDescriptorClass, MTLCommandBufferDescriptor>) end;

  MTLCommandBufferEncoderInfo = interface(IObjectiveC)
    ['{5E343646-6E99-49C7-8938-33E20C5A3105}']
    [MethodName('label')]
    function &label: NSString; cdecl;
    function debugSignposts: NSArray; cdecl;
    function errorState: MTLCommandEncoderErrorState; cdecl;
  end;

  MTLCommandBuffer = interface(IObjectiveC)
    ['{A1E45D5F-547A-4E52-BEA3-D388DC42C344}']
    [MethodName('label')]
    function &label: NSString; cdecl;
    function accelerationStructureCommandEncoder: Pointer; cdecl;
    procedure addCompletedHandler(block: MTLCommandBufferHandler); cdecl;
    procedure addScheduledHandler(block: MTLCommandBufferHandler); cdecl;
    function blitCommandEncoder: Pointer; cdecl;
    function blitCommandEncoderWithDescriptor(blitPassDescriptor: MTLBlitPassDescriptor): Pointer; cdecl;
    function commandQueue: Pointer; cdecl;
    procedure commit; cdecl;
    function computeCommandEncoder: Pointer; cdecl;
    function computeCommandEncoderWithDescriptor(computePassDescriptor: MTLComputePassDescriptor): Pointer; cdecl;
    function computeCommandEncoderWithDispatchType(dispatchType: MTLDispatchType): Pointer; cdecl;
    function device: Pointer; cdecl;
    procedure encodeSignalEvent(event: Pointer; value: UInt64); cdecl;
    procedure encodeWaitForEvent(event: Pointer; value: UInt64); cdecl;
    procedure enqueue; cdecl;
    function error: NSError; cdecl;
    function errorOptions: MTLCommandBufferErrorOption; cdecl;
    function GPUEndTime: CFTimeInterval; cdecl;
    function GPUStartTime: CFTimeInterval; cdecl;
    function kernelEndTime: CFTimeInterval; cdecl;
    function kernelStartTime: CFTimeInterval; cdecl;
    function logs: Pointer; cdecl;
    function parallelRenderCommandEncoderWithDescriptor(renderPassDescriptor: MTLRenderPassDescriptor): Pointer; cdecl;
    procedure popDebugGroup; cdecl;
    procedure presentDrawable(drawable: Pointer); cdecl;
    [MethodName('presentDrawable:afterMinimumDuration:')]
    procedure presentDrawableAfterMinimumDuration(drawable: Pointer; afterMinimumDuration: CFTimeInterval); cdecl;
    [MethodName('presentDrawable:atTime:')]
    procedure presentDrawableAtTime(drawable: Pointer; atTime: CFTimeInterval); cdecl;
    procedure pushDebugGroup(&string: NSString); cdecl;
    function renderCommandEncoderWithDescriptor(renderPassDescriptor: MTLRenderPassDescriptor): Pointer; cdecl;
    function resourceStateCommandEncoder: Pointer; cdecl;
    function resourceStateCommandEncoderWithDescriptor(resourceStatePassDescriptor: MTLResourceStatePassDescriptor): Pointer; cdecl;
    function retainedReferences: Boolean; cdecl;
    procedure setLabel(&label: NSString); cdecl;
    function status: MTLCommandBufferStatus; cdecl;
    procedure waitUntilCompleted; cdecl;
    procedure waitUntilScheduled; cdecl;
  end;

  MTLComputePassSampleBufferAttachmentDescriptorClass = interface(NSObjectClass)
    ['{98BBFFE0-292D-4D4B-8742-86856CF43294}']
  end;

  MTLComputePassSampleBufferAttachmentDescriptor = interface(NSObject)
    ['{B9E5BE6C-3DF4-44BE-A89B-6AE237DC514D}']
    function endOfEncoderSampleIndex: NSUInteger; cdecl;
    function sampleBuffer: Pointer; cdecl;
    procedure setEndOfEncoderSampleIndex(endOfEncoderSampleIndex: NSUInteger); cdecl;
    procedure setSampleBuffer(sampleBuffer: Pointer); cdecl;
    procedure setStartOfEncoderSampleIndex(startOfEncoderSampleIndex: NSUInteger); cdecl;
    function startOfEncoderSampleIndex: NSUInteger; cdecl;
  end;
  TMTLComputePassSampleBufferAttachmentDescriptor = class(TOCGenericImport<MTLComputePassSampleBufferAttachmentDescriptorClass,
    MTLComputePassSampleBufferAttachmentDescriptor>) end;

  MTLComputePassSampleBufferAttachmentDescriptorArrayClass = interface(NSObjectClass)
    ['{48CFEC4C-030A-4E39-AAB3-887D42DBECF2}']
  end;

  MTLComputePassSampleBufferAttachmentDescriptorArray = interface(NSObject)
    ['{53EF06BE-761B-411B-974B-D7D41929324C}']
    function objectAtIndexedSubscript(attachmentIndex: NSUInteger): MTLComputePassSampleBufferAttachmentDescriptor; cdecl;
    procedure setObject(attachment: MTLComputePassSampleBufferAttachmentDescriptor; atIndexedSubscript: NSUInteger); cdecl;
  end;
  TMTLComputePassSampleBufferAttachmentDescriptorArray = class(TOCGenericImport<MTLComputePassSampleBufferAttachmentDescriptorArrayClass,
    MTLComputePassSampleBufferAttachmentDescriptorArray>) end;

  MTLComputePassDescriptorClass = interface(NSObjectClass)
    ['{50F209BA-866E-4B15-8EFE-143FA9C719B4}']
    {class} function computePassDescriptor: MTLComputePassDescriptor; cdecl;
  end;

  MTLComputePassDescriptor = interface(NSObject)
    ['{3E48708F-4473-4F03-9AE3-30E95A02EEEB}']
    function dispatchType: MTLDispatchType; cdecl;
    function sampleBufferAttachments: MTLComputePassSampleBufferAttachmentDescriptorArray; cdecl;
    procedure setDispatchType(dispatchType: MTLDispatchType); cdecl;
  end;
  TMTLComputePassDescriptor = class(TOCGenericImport<MTLComputePassDescriptorClass, MTLComputePassDescriptor>) end;

  MTLComputeCommandEncoder = interface(IObjectiveC)
    ['{B31C9C36-EA83-4626-8049-5084278D7D88}']
    procedure dispatchThreadgroups(threadgroupsPerGrid: MTLSize; threadsPerThreadgroup: MTLSize); cdecl;
    procedure dispatchThreadgroupsWithIndirectBuffer(indirectBuffer: Pointer; indirectBufferOffset: NSUInteger; threadsPerThreadgroup: MTLSize); cdecl;
    procedure dispatchThreads(threadsPerGrid: MTLSize; threadsPerThreadgroup: MTLSize); cdecl;
    function dispatchType: MTLDispatchType; cdecl;
    procedure executeCommandsInBuffer(indirectCommandbuffer: Pointer; indirectBuffer: Pointer; indirectBufferOffset: NSUInteger); overload; cdecl;
    procedure executeCommandsInBuffer(indirectCommandBuffer: Pointer; withRange: NSRange); overload; cdecl;
    procedure memoryBarrierWithResources(resources: Pid; count: NSUInteger); cdecl;
    procedure memoryBarrierWithScope(scope: MTLBarrierScope); cdecl;
    procedure sampleCountersInBuffer(sampleBuffer: Pointer; atSampleIndex: NSUInteger; withBarrier: Boolean); cdecl;
    procedure setAccelerationStructure(accelerationStructure: Pointer; atBufferIndex: NSUInteger); cdecl;
    procedure setBuffer(buffer: Pointer; offset: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setBufferOffset(offset: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setBuffers(buffers: Pid; offsets: PNSUInteger; withRange: NSRange); cdecl;
    procedure setBytes(bytes: Pointer; length: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setComputePipelineState(state: Pointer); cdecl;
    procedure setImageblockWidth(width: NSUInteger; height: NSUInteger); cdecl;
    procedure setIntersectionFunctionTable(intersectionFunctionTable: Pointer; atBufferIndex: NSUInteger); cdecl;
    procedure setIntersectionFunctionTables(intersectionFunctionTables: Pid; withBufferRange: NSRange); cdecl;
    procedure setSamplerState(sampler: Pointer; atIndex: NSUInteger); overload; cdecl;
    procedure setSamplerState(sampler: Pointer; lodMinClamp: Single; lodMaxClamp: Single; atIndex: NSUInteger); overload; cdecl;
    procedure setSamplerStates(samplers: Pid; lodMinClamps: PSingle; lodMaxClamps: PSingle; withRange: NSRange); overload; cdecl;
    procedure setSamplerStates(samplers: Pid; withRange: NSRange); overload; cdecl;
    procedure setStageInRegion(region: MTLRegion); cdecl;
    procedure setStageInRegionWithIndirectBuffer(indirectBuffer: Pointer; indirectBufferOffset: NSUInteger); cdecl;
    procedure setTexture(texture: Pointer; atIndex: NSUInteger); cdecl;
    procedure setTextures(textures: Pid; withRange: NSRange); cdecl;
    procedure setThreadgroupMemoryLength(length: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setVisibleFunctionTable(visibleFunctionTable: Pointer; atBufferIndex: NSUInteger); cdecl;
    procedure setVisibleFunctionTables(visibleFunctionTables: Pid; withBufferRange: NSRange); cdecl;
    procedure updateFence(fence: Pointer); cdecl;
    procedure useHeap(heap: Pointer); cdecl;
    procedure useHeaps(heaps: Pid; count: NSUInteger); cdecl;
    procedure useResource(resource: Pointer; usage: MTLResourceUsage); cdecl;
    procedure useResources(resources: Pid; count: NSUInteger; usage: MTLResourceUsage); cdecl;
    procedure waitForFence(fence: Pointer); cdecl;
  end;

  MTLCommandQueue = interface(IObjectiveC)
    ['{F441C534-2993-4787-B0EE-6A25EC83F757}']
    [MethodName('label')]
    function &label: NSString; cdecl;
    function commandBuffer: Pointer; cdecl;
    function commandBufferWithDescriptor(descriptor: MTLCommandBufferDescriptor): Pointer; cdecl;
    function commandBufferWithUnretainedReferences: Pointer; cdecl;
    function device: Pointer; cdecl;
    procedure insertDebugCaptureBoundary; cdecl; // API_DEPRECATED("Use MTLCaptureScope instead", macos(10.11, 10.13), ios(8.0, 11.0))
    procedure setLabel(&label: NSString); cdecl;
  end;

  MTLStencilDescriptorClass = interface(NSObjectClass)
    ['{D5C16B6D-4CEA-4714-9262-C23F8E562AF3}']
  end;

  MTLStencilDescriptor = interface(NSObject)
    ['{35A23BA2-5787-4EC0-9416-75EBF7F868DE}']
    function depthFailureOperation: MTLStencilOperation; cdecl;
    function depthStencilPassOperation: MTLStencilOperation; cdecl;
    function readMask: UInt32; cdecl;
    procedure setDepthFailureOperation(depthFailureOperation: MTLStencilOperation); cdecl;
    procedure setDepthStencilPassOperation(depthStencilPassOperation: MTLStencilOperation); cdecl;
    procedure setReadMask(readMask: UInt32); cdecl;
    procedure setStencilCompareFunction(stencilCompareFunction: MTLCompareFunction); cdecl;
    procedure setStencilFailureOperation(stencilFailureOperation: MTLStencilOperation); cdecl;
    procedure setWriteMask(writeMask: UInt32); cdecl;
    function stencilCompareFunction: MTLCompareFunction; cdecl;
    function stencilFailureOperation: MTLStencilOperation; cdecl;
    function writeMask: UInt32; cdecl;
  end;
  TMTLStencilDescriptor = class(TOCGenericImport<MTLStencilDescriptorClass, MTLStencilDescriptor>) end;

  MTLDepthStencilDescriptorClass = interface(NSObjectClass)
    ['{EBB89342-F6F0-4BBD-BC5C-6C94754DF86C}']
  end;

  MTLDepthStencilDescriptor = interface(NSObject)
    ['{DA5EA0EE-8FC1-4E63-AD86-BAFE33F590C4}']
    [MethodName('label')]
    function &label: NSString; cdecl;
    function backFaceStencil: MTLStencilDescriptor; cdecl;
    function depthCompareFunction: MTLCompareFunction; cdecl;
    function frontFaceStencil: MTLStencilDescriptor; cdecl;
    function isDepthWriteEnabled: Boolean; cdecl;
    procedure setBackFaceStencil(backFaceStencil: MTLStencilDescriptor); cdecl;
    procedure setDepthCompareFunction(depthCompareFunction: MTLCompareFunction); cdecl;
    procedure setDepthWriteEnabled(depthWriteEnabled: Boolean); cdecl;
    procedure setFrontFaceStencil(frontFaceStencil: MTLStencilDescriptor); cdecl;
    procedure setLabel(&label: NSString); cdecl;
  end;
  TMTLDepthStencilDescriptor = class(TOCGenericImport<MTLDepthStencilDescriptorClass, MTLDepthStencilDescriptor>) end;

  MTLDepthStencilState = interface(IObjectiveC)
    ['{68E92B59-78EC-4856-BCBA-62180773BF72}']
    [MethodName('label')]
    function &label: NSString; cdecl;
    function device: Pointer; cdecl;
  end;

  MTLDrawable = interface(IObjectiveC)
    ['{7A7ACBCC-EAA4-4CA2-8106-CD7C899ED074}']
    procedure addPresentedHandler(block: MTLDrawablePresentedHandler); cdecl;
    function drawableID: NSUInteger; cdecl;
    procedure present; cdecl;
    procedure presentAfterMinimumDuration(duration: CFTimeInterval); cdecl;
    procedure presentAtTime(presentationTime: CFTimeInterval); cdecl;
    function presentedTime: CFTimeInterval; cdecl;
  end;

  MTLBufferLayoutDescriptorClass = interface(NSObjectClass)
    ['{7C495D95-3E3C-47AA-8A27-AF7483C71761}']
  end;

  MTLBufferLayoutDescriptor = interface(NSObject)
    ['{B431C9A2-3EC9-4A92-AB70-8A64243A9FC2}']
    procedure setStepFunction(stepFunction: MTLStepFunction); cdecl;
    procedure setStepRate(stepRate: NSUInteger); cdecl;
    procedure setStride(stride: NSUInteger); cdecl;
    function stepFunction: MTLStepFunction; cdecl;
    function stepRate: NSUInteger; cdecl;
    function stride: NSUInteger; cdecl;
  end;
  TMTLBufferLayoutDescriptor = class(TOCGenericImport<MTLBufferLayoutDescriptorClass, MTLBufferLayoutDescriptor>) end;

  MTLBufferLayoutDescriptorArrayClass = interface(NSObjectClass)
    ['{45C8F8DA-A484-4015-9735-2B2FCB88E6E3}']
  end;

  MTLBufferLayoutDescriptorArray = interface(NSObject)
    ['{A598927F-631B-49FE-8677-410D50181DF6}']
    function objectAtIndexedSubscript(index: NSUInteger): MTLBufferLayoutDescriptor; cdecl;
    procedure setObject(bufferDesc: MTLBufferLayoutDescriptor; atIndexedSubscript: NSUInteger); cdecl;
  end;
  TMTLBufferLayoutDescriptorArray = class(TOCGenericImport<MTLBufferLayoutDescriptorArrayClass, MTLBufferLayoutDescriptorArray>) end;

  MTLAttributeDescriptorClass = interface(NSObjectClass)
    ['{3EF0CD52-EB1F-403C-8B0A-660BBEAB6A31}']
  end;

  MTLAttributeDescriptor = interface(NSObject)
    ['{111949D6-868C-4123-9AF0-04D9CF647333}']
    function bufferIndex: NSUInteger; cdecl;
    function format: MTLAttributeFormat; cdecl;
    function offset: NSUInteger; cdecl;
    procedure setBufferIndex(bufferIndex: NSUInteger); cdecl;
    procedure setFormat(format: MTLAttributeFormat); cdecl;
    procedure setOffset(offset: NSUInteger); cdecl;
  end;
  TMTLAttributeDescriptor = class(TOCGenericImport<MTLAttributeDescriptorClass, MTLAttributeDescriptor>) end;

  MTLAttributeDescriptorArrayClass = interface(NSObjectClass)
    ['{9672795E-BE99-4DDF-9F29-E99A7F892820}']
  end;

  MTLAttributeDescriptorArray = interface(NSObject)
    ['{A8FF9EE3-0B97-43D2-A5A1-A8C2C2E32A74}']
    function objectAtIndexedSubscript(index: NSUInteger): MTLAttributeDescriptor; cdecl;
    procedure setObject(attributeDesc: MTLAttributeDescriptor; atIndexedSubscript: NSUInteger); cdecl;
  end;
  TMTLAttributeDescriptorArray = class(TOCGenericImport<MTLAttributeDescriptorArrayClass, MTLAttributeDescriptorArray>) end;

  MTLStageInputOutputDescriptorClass = interface(NSObjectClass)
    ['{FB1C54BE-B68A-4E22-9746-8C89F6C53516}']
    {class} function stageInputOutputDescriptor: MTLStageInputOutputDescriptor; cdecl;
  end;

  MTLStageInputOutputDescriptor = interface(NSObject)
    ['{897FD8A3-FCC8-4799-848F-25A0338407EB}']
    function attributes: MTLAttributeDescriptorArray; cdecl;
    function indexBufferIndex: NSUInteger; cdecl;
    function indexType: MTLIndexType; cdecl;
    function layouts: MTLBufferLayoutDescriptorArray; cdecl;
    procedure reset; cdecl;
    procedure setIndexBufferIndex(indexBufferIndex: NSUInteger); cdecl;
    procedure setIndexType(indexType: MTLIndexType); cdecl;
  end;
  TMTLStageInputOutputDescriptor = class(TOCGenericImport<MTLStageInputOutputDescriptorClass, MTLStageInputOutputDescriptor>) end;

  MTLPipelineBufferDescriptorClass = interface(NSObjectClass)
    ['{D6317093-8623-4A06-9AD5-1C9D613B9B40}']
  end;

  MTLPipelineBufferDescriptor = interface(NSObject)
    ['{487D5E30-1274-4E94-8407-2FE095F4F8BB}']
    function mutability: MTLMutability; cdecl;
    procedure setMutability(mutability: MTLMutability); cdecl;
  end;
  TMTLPipelineBufferDescriptor = class(TOCGenericImport<MTLPipelineBufferDescriptorClass, MTLPipelineBufferDescriptor>) end;

  MTLPipelineBufferDescriptorArrayClass = interface(NSObjectClass)
    ['{30031BA3-A2CF-4482-9FA5-83F670D43F62}']
  end;

  MTLPipelineBufferDescriptorArray = interface(NSObject)
    ['{2B81B96A-9665-4F72-8F4E-83F1B2CF38A0}']
    function objectAtIndexedSubscript(bufferIndex: NSUInteger): MTLPipelineBufferDescriptor; cdecl;
    procedure setObject(buffer: MTLPipelineBufferDescriptor; atIndexedSubscript: NSUInteger); cdecl;
  end;
  TMTLPipelineBufferDescriptorArray = class(TOCGenericImport<MTLPipelineBufferDescriptorArrayClass, MTLPipelineBufferDescriptorArray>) end;

  MTLLinkedFunctionsClass = interface(NSObjectClass)
    ['{7CD663A1-D1B3-4B3D-87E6-D2FC4C2961DC}']
    {class} function linkedFunctions: MTLLinkedFunctions; cdecl;
  end;

  MTLLinkedFunctions = interface(NSObject)
    ['{A7B8D24A-2BC2-49A7-BF8E-3CC1D74A1ABB}']
    function binaryFunctions: NSArray; cdecl;
    function functions: NSArray; cdecl;
    function groups: NSDictionary; cdecl;
    procedure setBinaryFunctions(binaryFunctions: NSArray); cdecl;
    procedure setFunctions(functions: NSArray); cdecl;
    procedure setGroups(groups: NSDictionary); cdecl;
  end;
  TMTLLinkedFunctions = class(TOCGenericImport<MTLLinkedFunctionsClass, MTLLinkedFunctions>) end;

  MTLComputePipelineReflectionClass = interface(NSObjectClass)
    ['{9A81B759-FA22-4033-9FBB-C3D86A34A968}']
  end;

  MTLComputePipelineReflection = interface(NSObject)
    ['{1D66A9B0-4A29-4A8B-9BC0-A17BFE24219A}']
    function arguments: NSArray; cdecl;
  end;
  TMTLComputePipelineReflection = class(TOCGenericImport<MTLComputePipelineReflectionClass, MTLComputePipelineReflection>) end;

  MTLComputePipelineDescriptorClass = interface(NSObjectClass)
    ['{04F405F5-54B8-4A4A-99D9-DBEE728B5044}']
  end;

  MTLComputePipelineDescriptor = interface(NSObject)
    ['{C59AD1B8-F71E-4B5D-8BF2-BC40448A0E77}']
    [MethodName('label')]
    function &label: NSString; cdecl;
    function binaryArchives: NSArray; cdecl;
    function buffers: MTLPipelineBufferDescriptorArray; cdecl;
    function computeFunction: Pointer; cdecl;
    function insertLibraries: NSArray; cdecl;
    function linkedFunctions: MTLLinkedFunctions; cdecl;
    function maxCallStackDepth: NSUInteger; cdecl;
    function maxTotalThreadsPerThreadgroup: NSUInteger; cdecl;
    procedure reset; cdecl;
    procedure setBinaryArchives(binaryArchives: NSArray); cdecl;
    procedure setComputeFunction(computeFunction: Pointer); cdecl;
    procedure setInsertLibraries(insertLibraries: NSArray); cdecl;
    procedure setLabel(&label: NSString); cdecl;
    procedure setLinkedFunctions(linkedFunctions: MTLLinkedFunctions); cdecl;
    procedure setMaxCallStackDepth(maxCallStackDepth: NSUInteger); cdecl;
    procedure setMaxTotalThreadsPerThreadgroup(maxTotalThreadsPerThreadgroup: NSUInteger); cdecl;
    procedure setStageInputDescriptor(stageInputDescriptor: MTLStageInputOutputDescriptor); cdecl;
    procedure setSupportAddingBinaryFunctions(supportAddingBinaryFunctions: Boolean); cdecl;
    procedure setSupportIndirectCommandBuffers(supportIndirectCommandBuffers: Boolean); cdecl;
    procedure setThreadGroupSizeIsMultipleOfThreadExecutionWidth(threadGroupSizeIsMultipleOfThreadExecutionWidth: Boolean); cdecl;
    function stageInputDescriptor: MTLStageInputOutputDescriptor; cdecl;
    function supportAddingBinaryFunctions: Boolean; cdecl;
    function supportIndirectCommandBuffers: Boolean; cdecl;
    function threadGroupSizeIsMultipleOfThreadExecutionWidth: Boolean; cdecl;
  end;
  TMTLComputePipelineDescriptor = class(TOCGenericImport<MTLComputePipelineDescriptorClass, MTLComputePipelineDescriptor>) end;

  MTLComputePipelineState = interface(IObjectiveC)
    ['{C884498D-74E1-4BC6-9986-092513AFDF26}']
    [MethodName('label')]
    function &label: NSString; cdecl;
    function device: Pointer; cdecl;
    function functionHandleWithFunction(&function: Pointer): Pointer; cdecl;
    function imageblockMemoryLengthForDimensions(imageblockDimensions: MTLSize): NSUInteger; cdecl;
    function maxTotalThreadsPerThreadgroup: NSUInteger; cdecl;
    function newComputePipelineStateWithAdditionalBinaryFunctions(functions: NSArray; error: PPointer): Pointer; cdecl;
    function newIntersectionFunctionTableWithDescriptor(descriptor: MTLIntersectionFunctionTableDescriptor): Pointer; cdecl;
    function newVisibleFunctionTableWithDescriptor(descriptor: MTLVisibleFunctionTableDescriptor): Pointer; cdecl;
    function staticThreadgroupMemoryLength: NSUInteger; cdecl;
    function supportIndirectCommandBuffers: Boolean; cdecl;
    function threadExecutionWidth: NSUInteger; cdecl;
  end;

  MTLRenderCommandEncoder = interface(IObjectiveC)
    ['{CC9DFCA5-0F7B-43C9-9E98-ABCD03D7D5BD}']
    procedure dispatchThreadsPerTile(threadsPerTile: MTLSize); cdecl;
    procedure drawIndexedPatches(numberOfPatchControlPoints: NSUInteger; patchStart: NSUInteger; patchCount: NSUInteger; patchIndexBuffer: Pointer;
      patchIndexBufferOffset: NSUInteger; controlPointIndexBuffer: Pointer; controlPointIndexBufferOffset: NSUInteger; instanceCount: NSUInteger;
      baseInstance: NSUInteger); overload; cdecl;
    procedure drawIndexedPatches(numberOfPatchControlPoints: NSUInteger; patchIndexBuffer: Pointer; patchIndexBufferOffset: NSUInteger;
      controlPointIndexBuffer: Pointer; controlPointIndexBufferOffset: NSUInteger; indirectBuffer: Pointer;
      indirectBufferOffset: NSUInteger); overload; cdecl;
    procedure drawIndexedPrimitives(primitiveType: MTLPrimitiveType; indexCount: NSUInteger; indexType: MTLIndexType; indexBuffer: Pointer;
      indexBufferOffset: NSUInteger; instanceCount: NSUInteger; baseVertex: NSInteger; baseInstance: NSUInteger); overload; cdecl;
    procedure drawIndexedPrimitives(primitiveType: MTLPrimitiveType; indexCount: NSUInteger; indexType: MTLIndexType; indexBuffer: Pointer;
      indexBufferOffset: NSUInteger); overload; cdecl;
    procedure drawIndexedPrimitives(primitiveType: MTLPrimitiveType; indexCount: NSUInteger; indexType: MTLIndexType; indexBuffer: Pointer;
      indexBufferOffset: NSUInteger; instanceCount: NSUInteger); overload; cdecl;
    procedure drawIndexedPrimitives(primitiveType: MTLPrimitiveType; indexType: MTLIndexType; indexBuffer: Pointer; indexBufferOffset: NSUInteger;
      indirectBuffer: Pointer; indirectBufferOffset: NSUInteger); overload; cdecl;
    procedure drawPatches(numberOfPatchControlPoints: NSUInteger; patchIndexBuffer: Pointer; patchIndexBufferOffset: NSUInteger;
      indirectBuffer: Pointer; indirectBufferOffset: NSUInteger); overload; cdecl;
    procedure drawPatches(numberOfPatchControlPoints: NSUInteger; patchStart: NSUInteger; patchCount: NSUInteger; patchIndexBuffer: Pointer;
      patchIndexBufferOffset: NSUInteger; instanceCount: NSUInteger; baseInstance: NSUInteger); overload; cdecl;
    procedure drawPrimitives(primitiveType: MTLPrimitiveType; vertexStart: NSUInteger; vertexCount: NSUInteger); overload; cdecl;
    procedure drawPrimitives(primitiveType: MTLPrimitiveType; vertexStart: NSUInteger; vertexCount: NSUInteger;
      instanceCount: NSUInteger); overload; cdecl;
    procedure drawPrimitives(primitiveType: MTLPrimitiveType; vertexStart: NSUInteger; vertexCount: NSUInteger; instanceCount: NSUInteger;
      baseInstance: NSUInteger); overload; cdecl;
    procedure drawPrimitives(primitiveType: MTLPrimitiveType; indirectBuffer: Pointer; indirectBufferOffset: NSUInteger); overload; cdecl;
    procedure executeCommandsInBuffer(indirectCommandbuffer: Pointer; indirectBuffer: Pointer; indirectBufferOffset: NSUInteger); overload; cdecl;
    procedure executeCommandsInBuffer(indirectCommandBuffer: Pointer; withRange: NSRange); overload; cdecl;
    procedure memoryBarrierWithResources(resources: Pid; count: NSUInteger; afterStages: MTLRenderStages; beforeStages: MTLRenderStages); cdecl;
    procedure memoryBarrierWithScope(scope: MTLBarrierScope; afterStages: MTLRenderStages; beforeStages: MTLRenderStages); cdecl;
    procedure sampleCountersInBuffer(sampleBuffer: Pointer; atSampleIndex: NSUInteger; withBarrier: Boolean); cdecl;
    procedure setBlendColorRed(red: Single; green: Single; blue: Single; alpha: Single); cdecl;
    procedure setColorStoreAction(storeAction: MTLStoreAction; atIndex: NSUInteger); cdecl;
    procedure setColorStoreActionOptions(storeActionOptions: MTLStoreActionOptions; atIndex: NSUInteger); cdecl;
    procedure setCullMode(cullMode: MTLCullMode); cdecl;
    procedure setDepthBias(depthBias: Single; slopeScale: Single; clamp: Single); cdecl;
    procedure setDepthClipMode(depthClipMode: MTLDepthClipMode); cdecl;
    procedure setDepthStencilState(depthStencilState: Pointer); cdecl;
    procedure setDepthStoreAction(storeAction: MTLStoreAction); cdecl;
    procedure setDepthStoreActionOptions(storeActionOptions: MTLStoreActionOptions); cdecl;
    procedure setFragmentBuffer(buffer: Pointer; offset: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setFragmentBufferOffset(offset: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setFragmentBuffers(buffers: Pid; offsets: PNSUInteger; withRange: NSRange); cdecl;
    procedure setFragmentBytes(bytes: Pointer; length: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setFragmentSamplerState(sampler: Pointer; lodMinClamp: Single; lodMaxClamp: Single; atIndex: NSUInteger); overload; cdecl;
    procedure setFragmentSamplerState(sampler: Pointer; atIndex: NSUInteger); overload; cdecl;
    procedure setFragmentSamplerStates(samplers: Pid; lodMinClamps: PSingle; lodMaxClamps: PSingle; withRange: NSRange); overload; cdecl;
    procedure setFragmentSamplerStates(samplers: Pid; withRange: NSRange); overload; cdecl;
    procedure setFragmentTexture(texture: Pointer; atIndex: NSUInteger); cdecl;
    procedure setFragmentTextures(textures: Pid; withRange: NSRange); cdecl;
    procedure setFrontFacingWinding(frontFacingWinding: MTLWinding); cdecl;
    procedure setRenderPipelineState(pipelineState: Pointer); cdecl;
    procedure setScissorRect(rect: MTLScissorRect); cdecl;
    procedure setScissorRects(scissorRects: PMTLScissorRect; count: NSUInteger); cdecl;
    procedure setStencilFrontReferenceValue(frontReferenceValue: UInt32; backReferenceValue: UInt32); cdecl;
    procedure setStencilReferenceValue(referenceValue: UInt32); cdecl;
    procedure setStencilStoreAction(storeAction: MTLStoreAction); cdecl;
    procedure setStencilStoreActionOptions(storeActionOptions: MTLStoreActionOptions); cdecl;
    procedure setTessellationFactorBuffer(buffer: Pointer; offset: NSUInteger; instanceStride: NSUInteger); cdecl;
    procedure setTessellationFactorScale(scale: Single); cdecl;
    procedure setThreadgroupMemoryLength(length: NSUInteger; offset: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setTileBuffer(buffer: Pointer; offset: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setTileBufferOffset(offset: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setTileBuffers(buffers: Pid; offsets: PNSUInteger; withRange: NSRange); cdecl;
    procedure setTileBytes(bytes: Pointer; length: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setTileSamplerState(sampler: Pointer; lodMinClamp: Single; lodMaxClamp: Single; atIndex: NSUInteger); overload; cdecl;
    procedure setTileSamplerState(sampler: Pointer; atIndex: NSUInteger); overload; cdecl;
    procedure setTileSamplerStates(samplers: Pid; withRange: NSRange); overload; cdecl;
    procedure setTileSamplerStates(samplers: Pid; lodMinClamps: PSingle; lodMaxClamps: PSingle; withRange: NSRange); overload; cdecl;
    procedure setTileTexture(texture: Pointer; atIndex: NSUInteger); cdecl;
    procedure setTileTextures(textures: Pid; withRange: NSRange); cdecl;
    procedure setTriangleFillMode(fillMode: MTLTriangleFillMode); cdecl;
    procedure setVertexAmplificationCount(count: NSUInteger; viewMappings: PMTLVertexAmplificationViewMapping); cdecl;
    procedure setVertexBuffer(buffer: Pointer; offset: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setVertexBufferOffset(offset: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setVertexBuffers(buffers: Pid; offsets: PNSUInteger; withRange: NSRange); cdecl;
    procedure setVertexBytes(bytes: Pointer; length: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setVertexSamplerState(sampler: Pointer; atIndex: NSUInteger); overload; cdecl;
    procedure setVertexSamplerState(sampler: Pointer; lodMinClamp: Single; lodMaxClamp: Single; atIndex: NSUInteger); overload; cdecl;
    procedure setVertexSamplerStates(samplers: Pid; lodMinClamps: PSingle; lodMaxClamps: PSingle; withRange: NSRange); overload; cdecl;
    procedure setVertexSamplerStates(samplers: Pid; withRange: NSRange); overload; cdecl;
    procedure setVertexTexture(texture: Pointer; atIndex: NSUInteger); cdecl;
    procedure setVertexTextures(textures: Pid; withRange: NSRange); cdecl;
    procedure setViewport(viewport: MTLViewport); cdecl;
    procedure setViewports(viewports: PMTLViewport; count: NSUInteger); cdecl;
    procedure setVisibilityResultMode(mode: MTLVisibilityResultMode; offset: NSUInteger); cdecl;
    procedure textureBarrier; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("memoryBarrierWithScope:MTLBarrierScopeRenderTargets", macos(10.11, 10.14))
    function tileHeight: NSUInteger; cdecl;
    function tileWidth: NSUInteger; cdecl;
    procedure updateFence(fence: Pointer; afterStages: MTLRenderStages); cdecl;
    procedure useHeap(heap: Pointer; stages: MTLRenderStages); overload; cdecl;
    procedure useHeap(heap: Pointer); overload; cdecl;
    procedure useHeaps(heaps: Pid; count: NSUInteger); overload; cdecl;
    procedure useHeaps(heaps: Pid; count: NSUInteger; stages: MTLRenderStages); overload; cdecl;
    procedure useResource(resource: Pointer; usage: MTLResourceUsage); overload; cdecl;
    procedure useResource(resource: Pointer; usage: MTLResourceUsage; stages: MTLRenderStages); overload; cdecl;
    procedure useResources(resources: Pid; count: NSUInteger; usage: MTLResourceUsage; stages: MTLRenderStages); overload; cdecl;
    procedure useResources(resources: Pid; count: NSUInteger; usage: MTLResourceUsage); overload; cdecl;
    procedure waitForFence(fence: Pointer; beforeStages: MTLRenderStages); cdecl;
  end;

  MTLRenderPipelineColorAttachmentDescriptorClass = interface(NSObjectClass)
    ['{6EAED948-EDCB-4394-B67E-374630AD5BBC}']
  end;

  MTLRenderPipelineColorAttachmentDescriptor = interface(NSObject)
    ['{CF6F10D4-DAB4-4910-91B5-47DCB1D8FFFB}']
    function alphaBlendOperation: MTLBlendOperation; cdecl;
    function destinationAlphaBlendFactor: MTLBlendFactor; cdecl;
    function destinationRGBBlendFactor: MTLBlendFactor; cdecl;
    function isBlendingEnabled: Boolean; cdecl;
    function pixelFormat: MTLPixelFormat; cdecl;
    function rgbBlendOperation: MTLBlendOperation; cdecl;
    procedure setAlphaBlendOperation(alphaBlendOperation: MTLBlendOperation); cdecl;
    procedure setBlendingEnabled(blendingEnabled: Boolean); cdecl;
    procedure setDestinationAlphaBlendFactor(destinationAlphaBlendFactor: MTLBlendFactor); cdecl;
    procedure setDestinationRGBBlendFactor(destinationRGBBlendFactor: MTLBlendFactor); cdecl;
    procedure setPixelFormat(pixelFormat: MTLPixelFormat); cdecl;
    procedure setRgbBlendOperation(rgbBlendOperation: MTLBlendOperation); cdecl;
    procedure setSourceAlphaBlendFactor(sourceAlphaBlendFactor: MTLBlendFactor); cdecl;
    procedure setSourceRGBBlendFactor(sourceRGBBlendFactor: MTLBlendFactor); cdecl;
    procedure setWriteMask(writeMask: MTLColorWriteMask); cdecl;
    function sourceAlphaBlendFactor: MTLBlendFactor; cdecl;
    function sourceRGBBlendFactor: MTLBlendFactor; cdecl;
    function writeMask: MTLColorWriteMask; cdecl;
  end;
  TMTLRenderPipelineColorAttachmentDescriptor = class(TOCGenericImport<MTLRenderPipelineColorAttachmentDescriptorClass,
    MTLRenderPipelineColorAttachmentDescriptor>) end;

  MTLRenderPipelineReflectionClass = interface(NSObjectClass)
    ['{9FEBD3A0-D6DC-479A-A4E8-07BE4D251625}']
  end;

  MTLRenderPipelineReflection = interface(NSObject)
    ['{6572F74E-2474-48C7-A3DC-96F702E32C40}']
    function fragmentArguments: NSArray; cdecl;
    function tileArguments: NSArray; cdecl;
    function vertexArguments: NSArray; cdecl;
  end;
  TMTLRenderPipelineReflection = class(TOCGenericImport<MTLRenderPipelineReflectionClass, MTLRenderPipelineReflection>) end;

  MTLRenderPipelineDescriptorClass = interface(NSObjectClass)
    ['{8AA7BE66-4B4E-4779-BD23-4E1BEF1A9A45}']
  end;

  MTLRenderPipelineDescriptor = interface(NSObject)
    ['{E3200A47-13D7-4553-AD86-C4820BA1BEFA}']
    [MethodName('label')]
    function &label: NSString; cdecl;
    function binaryArchives: NSArray; cdecl;
    function colorAttachments: MTLRenderPipelineColorAttachmentDescriptorArray; cdecl;
    function depthAttachmentPixelFormat: MTLPixelFormat; cdecl;
    function fragmentBuffers: MTLPipelineBufferDescriptorArray; cdecl;
    function fragmentFunction: Pointer; cdecl;
    function inputPrimitiveTopology: MTLPrimitiveTopologyClass; cdecl;
    function isAlphaToCoverageEnabled: Boolean; cdecl;
    function isAlphaToOneEnabled: Boolean; cdecl;
    function isRasterizationEnabled: Boolean; cdecl;
    function isTessellationFactorScaleEnabled: Boolean; cdecl;
    function maxTessellationFactor: NSUInteger; cdecl;
    function maxVertexAmplificationCount: NSUInteger; cdecl;
    function rasterSampleCount: NSUInteger; cdecl;
    procedure reset; cdecl;
    function sampleCount: NSUInteger; cdecl;
    procedure setAlphaToCoverageEnabled(alphaToCoverageEnabled: Boolean); cdecl;
    procedure setAlphaToOneEnabled(alphaToOneEnabled: Boolean); cdecl;
    procedure setBinaryArchives(binaryArchives: NSArray); cdecl;
    procedure setDepthAttachmentPixelFormat(depthAttachmentPixelFormat: MTLPixelFormat); cdecl;
    procedure setFragmentFunction(fragmentFunction: Pointer); cdecl;
    procedure setInputPrimitiveTopology(inputPrimitiveTopology: MTLPrimitiveTopologyClass); cdecl;
    procedure setLabel(&label: NSString); cdecl;
    procedure setMaxTessellationFactor(maxTessellationFactor: NSUInteger); cdecl;
    procedure setMaxVertexAmplificationCount(maxVertexAmplificationCount: NSUInteger); cdecl;
    procedure setRasterizationEnabled(rasterizationEnabled: Boolean); cdecl;
    procedure setRasterSampleCount(rasterSampleCount: NSUInteger); cdecl;
    procedure setSampleCount(sampleCount: NSUInteger); cdecl;
    procedure setStencilAttachmentPixelFormat(stencilAttachmentPixelFormat: MTLPixelFormat); cdecl;
    procedure setSupportIndirectCommandBuffers(supportIndirectCommandBuffers: Boolean); cdecl;
    procedure setTessellationControlPointIndexType(tessellationControlPointIndexType: MTLTessellationControlPointIndexType); cdecl;
    procedure setTessellationFactorFormat(tessellationFactorFormat: MTLTessellationFactorFormat); cdecl;
    procedure setTessellationFactorScaleEnabled(tessellationFactorScaleEnabled: Boolean); cdecl;
    procedure setTessellationFactorStepFunction(tessellationFactorStepFunction: MTLTessellationFactorStepFunction); cdecl;
    procedure setTessellationOutputWindingOrder(tessellationOutputWindingOrder: MTLWinding); cdecl;
    procedure setTessellationPartitionMode(tessellationPartitionMode: MTLTessellationPartitionMode); cdecl;
    procedure setVertexDescriptor(vertexDescriptor: MTLVertexDescriptor); cdecl;
    procedure setVertexFunction(vertexFunction: Pointer); cdecl;
    function stencilAttachmentPixelFormat: MTLPixelFormat; cdecl;
    function supportIndirectCommandBuffers: Boolean; cdecl;
    function tessellationControlPointIndexType: MTLTessellationControlPointIndexType; cdecl;
    function tessellationFactorFormat: MTLTessellationFactorFormat; cdecl;
    function tessellationFactorStepFunction: MTLTessellationFactorStepFunction; cdecl;
    function tessellationOutputWindingOrder: MTLWinding; cdecl;
    function tessellationPartitionMode: MTLTessellationPartitionMode; cdecl;
    function vertexBuffers: MTLPipelineBufferDescriptorArray; cdecl;
    function vertexDescriptor: MTLVertexDescriptor; cdecl;
    function vertexFunction: Pointer; cdecl;
  end;
  TMTLRenderPipelineDescriptor = class(TOCGenericImport<MTLRenderPipelineDescriptorClass, MTLRenderPipelineDescriptor>) end;

  MTLRenderPipelineState = interface(IObjectiveC)
    ['{C7A34BB0-D320-4A65-AF67-BBA1E6DB8154}']
    [MethodName('label')]
    function &label: NSString; cdecl;
    function device: Pointer; cdecl;
    function imageblockMemoryLengthForDimensions(imageblockDimensions: MTLSize): NSUInteger; cdecl;
    function imageblockSampleLength: NSUInteger; cdecl;
    function maxTotalThreadsPerThreadgroup: NSUInteger; cdecl;
    function supportIndirectCommandBuffers: Boolean; cdecl;
    function threadgroupSizeMatchesTileSize: Boolean; cdecl;
  end;

  MTLRenderPipelineColorAttachmentDescriptorArrayClass = interface(NSObjectClass)
    ['{4F249328-87D0-41B9-A499-EA5E62670ACE}']
  end;

  MTLRenderPipelineColorAttachmentDescriptorArray = interface(NSObject)
    ['{039E6917-1187-4519-BAB6-808522BCC5A7}']
    function objectAtIndexedSubscript(attachmentIndex: NSUInteger): MTLRenderPipelineColorAttachmentDescriptor; cdecl;
    procedure setObject(attachment: MTLRenderPipelineColorAttachmentDescriptor; atIndexedSubscript: NSUInteger); cdecl;
  end;
  TMTLRenderPipelineColorAttachmentDescriptorArray = class(TOCGenericImport<MTLRenderPipelineColorAttachmentDescriptorArrayClass,
    MTLRenderPipelineColorAttachmentDescriptorArray>) end;

  MTLTileRenderPipelineColorAttachmentDescriptorClass = interface(NSObjectClass)
    ['{18767C1E-289C-4AAB-B33B-E5ADBAB623F8}']
  end;

  MTLTileRenderPipelineColorAttachmentDescriptor = interface(NSObject)
    ['{780A98AB-CFF8-41D3-80C2-C8E74B0D842C}']
    function pixelFormat: MTLPixelFormat; cdecl;
    procedure setPixelFormat(pixelFormat: MTLPixelFormat); cdecl;
  end;
  TMTLTileRenderPipelineColorAttachmentDescriptor = class(TOCGenericImport<MTLTileRenderPipelineColorAttachmentDescriptorClass,
    MTLTileRenderPipelineColorAttachmentDescriptor>) end;

  MTLTileRenderPipelineColorAttachmentDescriptorArrayClass = interface(NSObjectClass)
    ['{EF0DB603-5499-4206-BB3C-A808489B231A}']
  end;

  MTLTileRenderPipelineColorAttachmentDescriptorArray = interface(NSObject)
    ['{83C7D22D-734D-4FBC-B37F-BFEA644954E6}']
    function objectAtIndexedSubscript(attachmentIndex: NSUInteger): MTLTileRenderPipelineColorAttachmentDescriptor; cdecl;
    procedure setObject(attachment: MTLTileRenderPipelineColorAttachmentDescriptor; atIndexedSubscript: NSUInteger); cdecl;
  end;
  TMTLTileRenderPipelineColorAttachmentDescriptorArray = class(TOCGenericImport<MTLTileRenderPipelineColorAttachmentDescriptorArrayClass,
    MTLTileRenderPipelineColorAttachmentDescriptorArray>) end;

  MTLTileRenderPipelineDescriptorClass = interface(NSObjectClass)
    ['{DCA670F7-72D3-4D9E-B85B-382A3C01BF83}']
  end;

  MTLTileRenderPipelineDescriptor = interface(NSObject)
    ['{27795546-3B18-4E7A-AF33-C7EBD516582E}']
    [MethodName('label')]
    function &label: NSString; cdecl;
    function binaryArchives: NSArray; cdecl;
    function colorAttachments: MTLTileRenderPipelineColorAttachmentDescriptorArray; cdecl;
    function maxTotalThreadsPerThreadgroup: NSUInteger; cdecl;
    function rasterSampleCount: NSUInteger; cdecl;
    procedure reset; cdecl;
    procedure setBinaryArchives(binaryArchives: NSArray); cdecl;
    procedure setLabel(&label: NSString); cdecl;
    procedure setMaxTotalThreadsPerThreadgroup(maxTotalThreadsPerThreadgroup: NSUInteger); cdecl;
    procedure setRasterSampleCount(rasterSampleCount: NSUInteger); cdecl;
    procedure setThreadgroupSizeMatchesTileSize(threadgroupSizeMatchesTileSize: Boolean); cdecl;
    procedure setTileFunction(tileFunction: Pointer); cdecl;
    function threadgroupSizeMatchesTileSize: Boolean; cdecl;
    function tileBuffers: MTLPipelineBufferDescriptorArray; cdecl;
    function tileFunction: Pointer; cdecl;
  end;
  TMTLTileRenderPipelineDescriptor = class(TOCGenericImport<MTLTileRenderPipelineDescriptorClass, MTLTileRenderPipelineDescriptor>) end;

  MTLVertexBufferLayoutDescriptorClass = interface(NSObjectClass)
    ['{F60C6890-96DF-47AF-948B-C14F702D8B39}']
  end;

  MTLVertexBufferLayoutDescriptor = interface(NSObject)
    ['{10A02B5B-02FC-416A-9C70-21A4EE593739}']
    procedure setStepFunction(stepFunction: MTLVertexStepFunction); cdecl;
    procedure setStepRate(stepRate: NSUInteger); cdecl;
    procedure setStride(stride: NSUInteger); cdecl;
    function stepFunction: MTLVertexStepFunction; cdecl;
    function stepRate: NSUInteger; cdecl;
    function stride: NSUInteger; cdecl;
  end;
  TMTLVertexBufferLayoutDescriptor = class(TOCGenericImport<MTLVertexBufferLayoutDescriptorClass, MTLVertexBufferLayoutDescriptor>) end;

  MTLVertexBufferLayoutDescriptorArrayClass = interface(NSObjectClass)
    ['{1968F254-BC77-489F-B1F3-F267161E5708}']
  end;

  MTLVertexBufferLayoutDescriptorArray = interface(NSObject)
    ['{76D3705F-53E5-4892-B737-36A1F8B24926}']
    function objectAtIndexedSubscript(index: NSUInteger): MTLVertexBufferLayoutDescriptor; cdecl;
    procedure setObject(bufferDesc: MTLVertexBufferLayoutDescriptor; atIndexedSubscript: NSUInteger); cdecl;
  end;
  TMTLVertexBufferLayoutDescriptorArray = class(TOCGenericImport<MTLVertexBufferLayoutDescriptorArrayClass, MTLVertexBufferLayoutDescriptorArray>) end;

  MTLVertexAttributeDescriptorClass = interface(NSObjectClass)
    ['{868683EC-9254-4DC7-90B8-075AB41E3A18}']
  end;

  MTLVertexAttributeDescriptor = interface(NSObject)
    ['{104EFFFC-25E2-4E82-BCE1-1238DF58AC97}']
    function bufferIndex: NSUInteger; cdecl;
    function format: MTLVertexFormat; cdecl;
    function offset: NSUInteger; cdecl;
    procedure setBufferIndex(bufferIndex: NSUInteger); cdecl;
    procedure setFormat(format: MTLVertexFormat); cdecl;
    procedure setOffset(offset: NSUInteger); cdecl;
  end;
  TMTLVertexAttributeDescriptor = class(TOCGenericImport<MTLVertexAttributeDescriptorClass, MTLVertexAttributeDescriptor>) end;

  MTLVertexAttributeDescriptorArrayClass = interface(NSObjectClass)
    ['{3D7AB8BF-83F4-47A1-9F8D-45F4E121C1D2}']
  end;

  MTLVertexAttributeDescriptorArray = interface(NSObject)
    ['{62A88D0C-7F28-4792-9458-2209542CB20F}']
    function objectAtIndexedSubscript(index: NSUInteger): MTLVertexAttributeDescriptor; cdecl;
    procedure setObject(attributeDesc: MTLVertexAttributeDescriptor; atIndexedSubscript: NSUInteger); cdecl;
  end;
  TMTLVertexAttributeDescriptorArray = class(TOCGenericImport<MTLVertexAttributeDescriptorArrayClass, MTLVertexAttributeDescriptorArray>) end;

  MTLVertexDescriptorClass = interface(NSObjectClass)
    ['{D941707F-CAF7-42EC-B2AE-3D99CDFA1839}']
    {class} function vertexDescriptor: MTLVertexDescriptor; cdecl;
  end;

  MTLVertexDescriptor = interface(NSObject)
    ['{A082A33E-C98F-4870-A186-B22F6B41E5F8}']
    function attributes: MTLVertexAttributeDescriptorArray; cdecl;
    function layouts: MTLVertexBufferLayoutDescriptorArray; cdecl;
    procedure reset; cdecl;
  end;
  TMTLVertexDescriptor = class(TOCGenericImport<MTLVertexDescriptorClass, MTLVertexDescriptor>) end;

  MTLParallelRenderCommandEncoder = interface(IObjectiveC)
    ['{97CF2932-FD17-463B-A2D8-334BB22135F1}']
    function renderCommandEncoder: Pointer; cdecl;
    procedure setColorStoreAction(storeAction: MTLStoreAction; atIndex: NSUInteger); cdecl;
    procedure setColorStoreActionOptions(storeActionOptions: MTLStoreActionOptions; atIndex: NSUInteger); cdecl;
    procedure setDepthStoreAction(storeAction: MTLStoreAction); cdecl;
    procedure setDepthStoreActionOptions(storeActionOptions: MTLStoreActionOptions); cdecl;
    procedure setStencilStoreAction(storeAction: MTLStoreAction); cdecl;
    procedure setStencilStoreActionOptions(storeActionOptions: MTLStoreActionOptions); cdecl;
  end;

  MTLSamplerDescriptorClass = interface(NSObjectClass)
    ['{69F3A072-2FA4-4110-9C40-C20CF6DAA586}']
  end;

  MTLSamplerDescriptor = interface(NSObject)
    ['{B755DEFB-9088-4FAE-9BD6-0206D023D7F9}']
    [MethodName('label')]
    function &label: NSString; cdecl;
    function borderColor: MTLSamplerBorderColor; cdecl;
    function compareFunction: MTLCompareFunction; cdecl;
    function lodAverage: Boolean; cdecl;
    function lodMaxClamp: Single; cdecl;
    function lodMinClamp: Single; cdecl;
    function magFilter: MTLSamplerMinMagFilter; cdecl;
    function maxAnisotropy: NSUInteger; cdecl;
    function minFilter: MTLSamplerMinMagFilter; cdecl;
    function mipFilter: MTLSamplerMipFilter; cdecl;
    function normalizedCoordinates: Boolean; cdecl;
    function rAddressMode: MTLSamplerAddressMode; cdecl;
    function sAddressMode: MTLSamplerAddressMode; cdecl;
    procedure setBorderColor(borderColor: MTLSamplerBorderColor); cdecl;
    procedure setCompareFunction(compareFunction: MTLCompareFunction); cdecl;
    procedure setLabel(&label: NSString); cdecl;
    procedure setLodAverage(lodAverage: Boolean); cdecl;
    procedure setLodMaxClamp(lodMaxClamp: Single); cdecl;
    procedure setLodMinClamp(lodMinClamp: Single); cdecl;
    procedure setMagFilter(magFilter: MTLSamplerMinMagFilter); cdecl;
    procedure setMaxAnisotropy(maxAnisotropy: NSUInteger); cdecl;
    procedure setMinFilter(minFilter: MTLSamplerMinMagFilter); cdecl;
    procedure setMipFilter(mipFilter: MTLSamplerMipFilter); cdecl;
    procedure setNormalizedCoordinates(normalizedCoordinates: Boolean); cdecl;
    procedure setRAddressMode(rAddressMode: MTLSamplerAddressMode); cdecl;
    procedure setSAddressMode(sAddressMode: MTLSamplerAddressMode); cdecl;
    procedure setSupportArgumentBuffers(supportArgumentBuffers: Boolean); cdecl;
    procedure setTAddressMode(tAddressMode: MTLSamplerAddressMode); cdecl;
    function supportArgumentBuffers: Boolean; cdecl;
    function tAddressMode: MTLSamplerAddressMode; cdecl;
  end;
  TMTLSamplerDescriptor = class(TOCGenericImport<MTLSamplerDescriptorClass, MTLSamplerDescriptor>) end;

  MTLSamplerState = interface(IObjectiveC)
    ['{A132BE62-BFBD-4D16-BBE5-55F934824382}']
    [MethodName('label')]
    function &label: NSString; cdecl;
    function device: Pointer; cdecl;
  end;

  MTLHeapDescriptorClass = interface(NSObjectClass)
    ['{5A26F825-E0AC-4C9D-80E0-DC64E4460794}']
  end;

  MTLHeapDescriptor = interface(NSObject)
    ['{C1B2F76C-5FA2-4C9C-8320-2806D9E6856B}']
    [MethodName('type')]
    function &type: MTLHeapType; cdecl;
    function cpuCacheMode: MTLCPUCacheMode; cdecl;
    function hazardTrackingMode: MTLHazardTrackingMode; cdecl;
    function resourceOptions: MTLResourceOptions; cdecl;
    procedure setCpuCacheMode(cpuCacheMode: MTLCPUCacheMode); cdecl;
    procedure setHazardTrackingMode(hazardTrackingMode: MTLHazardTrackingMode); cdecl;
    procedure setResourceOptions(resourceOptions: MTLResourceOptions); cdecl;
    procedure setSize(size: NSUInteger); cdecl;
    procedure setStorageMode(storageMode: MTLStorageMode); cdecl;
    procedure setType(&type: MTLHeapType); cdecl;
    function size: NSUInteger; cdecl;
    function storageMode: MTLStorageMode; cdecl;
  end;
  TMTLHeapDescriptor = class(TOCGenericImport<MTLHeapDescriptorClass, MTLHeapDescriptor>) end;

  MTLHeap = interface(IObjectiveC)
    ['{E1D2FC5F-E431-4589-8AD6-B4762132D4A7}']
    [MethodName('label')]
    function &label: NSString; cdecl;
    [MethodName('type')]
    function &type: MTLHeapType; cdecl;
    function cpuCacheMode: MTLCPUCacheMode; cdecl;
    function currentAllocatedSize: NSUInteger; cdecl;
    function device: Pointer; cdecl;
    function hazardTrackingMode: MTLHazardTrackingMode; cdecl;
    function maxAvailableSizeWithAlignment(alignment: NSUInteger): NSUInteger; cdecl;
    function newBufferWithLength(length: NSUInteger; options: MTLResourceOptions): Pointer; overload; cdecl;
    function newBufferWithLength(length: NSUInteger; options: MTLResourceOptions; offset: NSUInteger): Pointer; overload; cdecl;
    function newTextureWithDescriptor(desc: MTLTextureDescriptor): Pointer; overload; cdecl;
    function newTextureWithDescriptor(descriptor: MTLTextureDescriptor; offset: NSUInteger): Pointer; overload; cdecl;
    function resourceOptions: MTLResourceOptions; cdecl;
    procedure setLabel(&label: NSString); cdecl;
    function setPurgeableState(state: MTLPurgeableState): MTLPurgeableState; cdecl;
    function size: NSUInteger; cdecl;
    function storageMode: MTLStorageMode; cdecl;
    function usedSize: NSUInteger; cdecl;
  end;

  MTLArgumentEncoder = interface(IObjectiveC)
    ['{2E74FEB3-1A30-4434-A90B-83676DCF8031}']
    [MethodName('label')]
    function &label: NSString; cdecl;
    function alignment: NSUInteger; cdecl;
    function constantDataAtIndex(index: NSUInteger): Pointer; cdecl;
    function device: Pointer; cdecl;
    function encodedLength: NSUInteger; cdecl;
    function newArgumentEncoderForBufferAtIndex(index: NSUInteger): Pointer; cdecl;
    procedure setAccelerationStructure(accelerationStructure: Pointer; atIndex: NSUInteger); cdecl;
    procedure setArgumentBuffer(argumentBuffer: Pointer; offset: NSUInteger); overload; cdecl;
    procedure setArgumentBuffer(argumentBuffer: Pointer; startOffset: NSUInteger; arrayElement: NSUInteger); overload; cdecl;
    procedure setBuffer(buffer: Pointer; offset: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setBuffers(buffers: Pid; offsets: PNSUInteger; withRange: NSRange); cdecl;
    procedure setComputePipelineState(pipeline: Pointer; atIndex: NSUInteger); cdecl;
    procedure setComputePipelineStates(pipelines: Pid; withRange: NSRange); cdecl;
    procedure setIndirectCommandBuffer(indirectCommandBuffer: Pointer; atIndex: NSUInteger); cdecl;
    procedure setIndirectCommandBuffers(buffers: Pid; withRange: NSRange); cdecl;
    procedure setIntersectionFunctionTable(intersectionFunctionTable: Pointer; atIndex: NSUInteger); cdecl;
    procedure setIntersectionFunctionTables(intersectionFunctionTables: Pid; withRange: NSRange); cdecl;
    procedure setLabel(&label: NSString); cdecl;
    procedure setRenderPipelineState(pipeline: Pointer; atIndex: NSUInteger); cdecl;
    procedure setRenderPipelineStates(pipelines: Pid; withRange: NSRange); cdecl;
    procedure setSamplerState(sampler: Pointer; atIndex: NSUInteger); cdecl;
    procedure setSamplerStates(samplers: Pid; withRange: NSRange); cdecl;
    procedure setTexture(texture: Pointer; atIndex: NSUInteger); cdecl;
    procedure setTextures(textures: Pid; withRange: NSRange); cdecl;
    procedure setVisibleFunctionTable(visibleFunctionTable: Pointer; atIndex: NSUInteger); cdecl;
    procedure setVisibleFunctionTables(visibleFunctionTables: Pid; withRange: NSRange); cdecl;
  end;

  MTLCaptureDescriptorClass = interface(NSObjectClass)
    ['{555837AC-FACF-4B22-9F22-85F010155DB7}']
  end;

  MTLCaptureDescriptor = interface(NSObject)
    ['{500084D7-B7D1-4E8B-A9ED-BB7AFC422A1A}']
    function captureObject: Pointer; cdecl;
    function destination: MTLCaptureDestination; cdecl;
    function outputURL: NSURL; cdecl;
    procedure setCaptureObject(captureObject: Pointer); cdecl;
    procedure setDestination(destination: MTLCaptureDestination); cdecl;
    procedure setOutputURL(outputURL: NSURL); cdecl;
  end;
  TMTLCaptureDescriptor = class(TOCGenericImport<MTLCaptureDescriptorClass, MTLCaptureDescriptor>) end;

  MTLCaptureManagerClass = interface(NSObjectClass)
    ['{CDAA0736-A36D-49F2-BD86-AA995EADE9AF}']
    {class} function sharedCaptureManager: MTLCaptureManager; cdecl;
  end;

  MTLCaptureManager = interface(NSObject)
    ['{C9D8868F-B0AF-4DAF-B6DB-C81C037002CA}']
    function defaultCaptureScope: Pointer; cdecl;
    function isCapturing: Boolean; cdecl;
    function newCaptureScopeWithCommandQueue(commandQueue: Pointer): Pointer; cdecl;
    function newCaptureScopeWithDevice(device: Pointer): Pointer; cdecl;
    procedure setDefaultCaptureScope(defaultCaptureScope: Pointer); cdecl;
    procedure startCaptureWithCommandQueue(commandQueue: Pointer); cdecl; // API_DEPRECATED("Use startCaptureWithDescriptor:error: instead", macos(10.13, 10.15), ios(11.0, 13.0))
    function startCaptureWithDescriptor(descriptor: MTLCaptureDescriptor; error: PPointer): Boolean; cdecl;
    procedure startCaptureWithDevice(device: Pointer); cdecl; // API_DEPRECATED("Use startCaptureWithDescriptor:error: instead", macos(10.13, 10.15), ios(11.0, 13.0))
    procedure startCaptureWithScope(captureScope: Pointer); cdecl; // API_DEPRECATED("Use startCaptureWithDescriptor:error: instead", macos(10.13, 10.15), ios(11.0, 13.0))
    procedure stopCapture; cdecl;
    function supportsDestination(destination: MTLCaptureDestination): Boolean; cdecl;
  end;
  TMTLCaptureManager = class(TOCGenericImport<MTLCaptureManagerClass, MTLCaptureManager>) end;

  MTLCaptureScope = interface(IObjectiveC)
    ['{614C0FCA-A159-4688-ABF3-83C74A42DDA5}']
    [MethodName('label')]
    function &label: NSString; cdecl;
    procedure beginScope; cdecl;
    function commandQueue: Pointer; cdecl;
    function device: Pointer; cdecl;
    procedure endScope; cdecl;
    procedure setLabel(&label: NSString); cdecl;
  end;

  MTLIndirectRenderCommand = interface(IObjectiveC)
    ['{F0FDBEFF-05C6-43DC-A05F-B082C454CEFA}']
    procedure drawIndexedPatches(numberOfPatchControlPoints: NSUInteger; patchStart: NSUInteger; patchCount: NSUInteger; patchIndexBuffer: Pointer;
      patchIndexBufferOffset: NSUInteger; controlPointIndexBuffer: Pointer; controlPointIndexBufferOffset: NSUInteger; instanceCount: NSUInteger;
      baseInstance: NSUInteger; tessellationFactorBuffer: Pointer; tessellationFactorBufferOffset: NSUInteger;
      tessellationFactorBufferInstanceStride: NSUInteger); cdecl;
    procedure drawIndexedPrimitives(primitiveType: MTLPrimitiveType; indexCount: NSUInteger; indexType: MTLIndexType; indexBuffer: Pointer;
      indexBufferOffset: NSUInteger; instanceCount: NSUInteger; baseVertex: NSInteger; baseInstance: NSUInteger); cdecl;
    procedure drawPatches(numberOfPatchControlPoints: NSUInteger; patchStart: NSUInteger; patchCount: NSUInteger; patchIndexBuffer: Pointer;
      patchIndexBufferOffset: NSUInteger; instanceCount: NSUInteger; baseInstance: NSUInteger; tessellationFactorBuffer: Pointer;
      tessellationFactorBufferOffset: NSUInteger; tessellationFactorBufferInstanceStride: NSUInteger); cdecl;
    procedure drawPrimitives(primitiveType: MTLPrimitiveType; vertexStart: NSUInteger; vertexCount: NSUInteger; instanceCount: NSUInteger;
      baseInstance: NSUInteger); cdecl;
    procedure reset; cdecl;
    procedure setFragmentBuffer(buffer: Pointer; offset: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setRenderPipelineState(pipelineState: Pointer); cdecl;
    procedure setVertexBuffer(buffer: Pointer; offset: NSUInteger; atIndex: NSUInteger); cdecl;
  end;

  MTLIndirectComputeCommand = interface(IObjectiveC)
    ['{4ADD5FCD-4A90-47A9-A949-43A374D223E2}']
    procedure clearBarrier; cdecl;
    procedure concurrentDispatchThreadgroups(threadgroupsPerGrid: MTLSize; threadsPerThreadgroup: MTLSize); cdecl;
    procedure concurrentDispatchThreads(threadsPerGrid: MTLSize; threadsPerThreadgroup: MTLSize); cdecl;
    procedure reset; cdecl;
    procedure setBarrier; cdecl;
    procedure setComputePipelineState(pipelineState: Pointer); cdecl;
    procedure setImageblockWidth(width: NSUInteger; height: NSUInteger); cdecl;
    procedure setKernelBuffer(buffer: Pointer; offset: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setStageInRegion(region: MTLRegion); cdecl;
    procedure setThreadgroupMemoryLength(length: NSUInteger; atIndex: NSUInteger); cdecl;
  end;

  MTLIndirectCommandBufferDescriptorClass = interface(NSObjectClass)
    ['{9BF7A5B0-C5F9-4E19-9804-934911F47BA9}']
  end;

  MTLIndirectCommandBufferDescriptor = interface(NSObject)
    ['{E1D35898-E0F3-4711-9FD0-4BD67D8D8DF2}']
    function commandTypes: MTLIndirectCommandType; cdecl;
    function inheritBuffers: Boolean; cdecl;
    function inheritPipelineState: Boolean; cdecl;
    function maxFragmentBufferBindCount: NSUInteger; cdecl;
    function maxKernelBufferBindCount: NSUInteger; cdecl;
    function maxVertexBufferBindCount: NSUInteger; cdecl;
    procedure setCommandTypes(commandTypes: MTLIndirectCommandType); cdecl;
    procedure setInheritBuffers(inheritBuffers: Boolean); cdecl;
    procedure setInheritPipelineState(inheritPipelineState: Boolean); cdecl;
    procedure setMaxFragmentBufferBindCount(maxFragmentBufferBindCount: NSUInteger); cdecl;
    procedure setMaxKernelBufferBindCount(maxKernelBufferBindCount: NSUInteger); cdecl;
    procedure setMaxVertexBufferBindCount(maxVertexBufferBindCount: NSUInteger); cdecl;
  end;
  TMTLIndirectCommandBufferDescriptor = class(TOCGenericImport<MTLIndirectCommandBufferDescriptorClass, MTLIndirectCommandBufferDescriptor>) end;

  MTLIndirectCommandBuffer = interface(IObjectiveC)
    ['{D03C8D3E-261D-43F1-BB92-EEE1850DE1BE}']
    function indirectComputeCommandAtIndex(commandIndex: NSUInteger): Pointer; cdecl;
    function indirectRenderCommandAtIndex(commandIndex: NSUInteger): Pointer; cdecl;
    procedure resetWithRange(range: NSRange); cdecl;
    function size: NSUInteger; cdecl;
  end;

  MTLEvent = interface(IObjectiveC)
    ['{10CA871A-CEB6-40C0-A03D-125BE92AEF50}']
    [MethodName('label')]
    function &label: NSString; cdecl;
    function device: Pointer; cdecl;
    procedure setLabel(&label: NSString); cdecl;
  end;

  MTLSharedEventListenerClass = interface(NSObjectClass)
    ['{8D168806-DCF3-4DA4-85C5-64E88DF2F83B}']
  end;

  MTLSharedEventListener = interface(NSObject)
    ['{80DE902A-395F-43E7-9A4C-85E69F521440}']
    function dispatchQueue: dispatch_queue_t; cdecl;
    function initWithDispatchQueue(dispatchQueue: dispatch_queue_t): Pointer; cdecl;
  end;
  TMTLSharedEventListener = class(TOCGenericImport<MTLSharedEventListenerClass, MTLSharedEventListener>) end;

  MTLSharedEvent = interface(IObjectiveC)
    ['{40625E16-9692-4EE1-A542-AFFB49B245EF}']
    function newSharedEventHandle: MTLSharedEventHandle; cdecl;
    procedure notifyListener(listener: MTLSharedEventListener; atValue: UInt64; block: MTLSharedEventNotificationBlock); cdecl;
    procedure setSignaledValue(signaledValue: UInt64); cdecl;
    function signaledValue: UInt64; cdecl;
  end;

  MTLSharedEventHandleClass = interface(NSObjectClass)
    ['{A7AB58C9-8C21-47E4-BB5C-53FA64D937AE}']
  end;

  MTLSharedEventHandle = interface(NSObject)
    ['{71808C31-AF59-4F22-9259-1E199AFE9A83}']
    [MethodName('label')]
    function &label: NSString; cdecl;
  end;
  TMTLSharedEventHandle = class(TOCGenericImport<MTLSharedEventHandleClass, MTLSharedEventHandle>) end;

  MTLLogContainer = interface(IObjectiveC)
    ['{FB9C2691-FEB6-4CBA-824E-35483C8642E3}']
  end;

  MTLFunctionLogDebugLocation = interface(IObjectiveC)
    ['{8CC8DA99-6D4B-464B-AE9E-A3D0AA163610}']
    function column: NSUInteger; cdecl;
    function functionName: NSString; cdecl;
    function line: NSUInteger; cdecl;
    function URL: NSURL; cdecl;
  end;

  MTLFunctionLog = interface(IObjectiveC)
    ['{33B3C609-A658-4194-81BB-644A4C8E56D6}']
    [MethodName('function')]
    function &function: Pointer; cdecl;
    [MethodName('type')]
    function &type: MTLFunctionLogType; cdecl;
    function debugLocation: Pointer; cdecl;
    function encoderLabel: NSString; cdecl;
  end;

  MTLAccelerationStructureDescriptorClass = interface(NSObjectClass)
    ['{76B749D3-2535-455B-B9B0-3AADC4DFED89}']
  end;

  MTLAccelerationStructureDescriptor = interface(NSObject)
    ['{94AA0B1B-258F-48B0-A096-14506ED7951A}']
    procedure setUsage(usage: MTLAccelerationStructureUsage); cdecl;
    function usage: MTLAccelerationStructureUsage; cdecl;
  end;
  TMTLAccelerationStructureDescriptor = class(TOCGenericImport<MTLAccelerationStructureDescriptorClass, MTLAccelerationStructureDescriptor>) end;

  MTLAccelerationStructureGeometryDescriptorClass = interface(NSObjectClass)
    ['{06E46D77-44F5-4B1D-A1B7-7BC05E65438E}']
  end;

  MTLAccelerationStructureGeometryDescriptor = interface(NSObject)
    ['{505380D4-42F3-4A84-B845-A1D1D7E63CAB}']
    function allowDuplicateIntersectionFunctionInvocation: Boolean; cdecl;
    function intersectionFunctionTableOffset: NSUInteger; cdecl;
    function opaque: Boolean; cdecl;
    procedure setAllowDuplicateIntersectionFunctionInvocation(allowDuplicateIntersectionFunctionInvocation: Boolean); cdecl;
    procedure setIntersectionFunctionTableOffset(intersectionFunctionTableOffset: NSUInteger); cdecl;
    procedure setOpaque(opaque: Boolean); cdecl;
  end;
  TMTLAccelerationStructureGeometryDescriptor = class(TOCGenericImport<MTLAccelerationStructureGeometryDescriptorClass,
    MTLAccelerationStructureGeometryDescriptor>) end;

  MTLPrimitiveAccelerationStructureDescriptorClass = interface(MTLAccelerationStructureDescriptorClass)
    ['{F92CB14B-2F52-4B1D-BF5D-3901206D3DB7}']
    {class} function descriptor: Pointer; cdecl;
  end;

  MTLPrimitiveAccelerationStructureDescriptor = interface(MTLAccelerationStructureDescriptor)
    ['{0F7977B7-E8D6-4C63-B5E0-20D93EF7FDCC}']
    function geometryDescriptors: NSArray; cdecl;
    procedure setGeometryDescriptors(geometryDescriptors: NSArray); cdecl;
  end;
  TMTLPrimitiveAccelerationStructureDescriptor = class(TOCGenericImport<MTLPrimitiveAccelerationStructureDescriptorClass,
    MTLPrimitiveAccelerationStructureDescriptor>) end;

  MTLAccelerationStructureTriangleGeometryDescriptorClass = interface(MTLAccelerationStructureGeometryDescriptorClass)
    ['{87B8A76E-3BED-41F3-8248-38B0CCF5C2E0}']
    {class} function descriptor: Pointer; cdecl;
  end;

  MTLAccelerationStructureTriangleGeometryDescriptor = interface(MTLAccelerationStructureGeometryDescriptor)
    ['{6EB83184-8022-44BE-98E2-1F79DE06B5FA}']
    function indexBuffer: Pointer; cdecl;
    function indexBufferOffset: NSUInteger; cdecl;
    function indexType: MTLIndexType; cdecl;
    procedure setIndexBuffer(indexBuffer: Pointer); cdecl;
    procedure setIndexBufferOffset(indexBufferOffset: NSUInteger); cdecl;
    procedure setIndexType(indexType: MTLIndexType); cdecl;
    procedure setTriangleCount(triangleCount: NSUInteger); cdecl;
    procedure setVertexBuffer(vertexBuffer: Pointer); cdecl;
    procedure setVertexBufferOffset(vertexBufferOffset: NSUInteger); cdecl;
    procedure setVertexStride(vertexStride: NSUInteger); cdecl;
    function triangleCount: NSUInteger; cdecl;
    function vertexBuffer: Pointer; cdecl;
    function vertexBufferOffset: NSUInteger; cdecl;
    function vertexStride: NSUInteger; cdecl;
  end;
  TMTLAccelerationStructureTriangleGeometryDescriptor = class(TOCGenericImport<MTLAccelerationStructureTriangleGeometryDescriptorClass,
    MTLAccelerationStructureTriangleGeometryDescriptor>) end;

  MTLAccelerationStructureBoundingBoxGeometryDescriptorClass = interface(MTLAccelerationStructureGeometryDescriptorClass)
    ['{B36ED013-72A7-4E45-B0EB-1DBC2900C06E}']
    {class} function descriptor: Pointer; cdecl;
  end;

  MTLAccelerationStructureBoundingBoxGeometryDescriptor = interface(MTLAccelerationStructureGeometryDescriptor)
    ['{66F36BF9-2392-4F99-A54B-FC51F90747CA}']
    function boundingBoxBuffer: Pointer; cdecl;
    function boundingBoxBufferOffset: NSUInteger; cdecl;
    function boundingBoxCount: NSUInteger; cdecl;
    function boundingBoxStride: NSUInteger; cdecl;
    procedure setBoundingBoxBuffer(boundingBoxBuffer: Pointer); cdecl;
    procedure setBoundingBoxBufferOffset(boundingBoxBufferOffset: NSUInteger); cdecl;
    procedure setBoundingBoxCount(boundingBoxCount: NSUInteger); cdecl;
    procedure setBoundingBoxStride(boundingBoxStride: NSUInteger); cdecl;
  end;
  TMTLAccelerationStructureBoundingBoxGeometryDescriptor = class(TOCGenericImport<MTLAccelerationStructureBoundingBoxGeometryDescriptorClass,
    MTLAccelerationStructureBoundingBoxGeometryDescriptor>) end;

  MTLInstanceAccelerationStructureDescriptorClass = interface(MTLAccelerationStructureDescriptorClass)
    ['{207EB5D5-98A7-4FC5-AE81-95A30E354121}']
    {class} function descriptor: Pointer; cdecl;
  end;

  MTLInstanceAccelerationStructureDescriptor = interface(MTLAccelerationStructureDescriptor)
    ['{3B5A6C11-74E2-4FFD-B015-5F18269A753A}']
    function instanceCount: NSUInteger; cdecl;
    function instancedAccelerationStructures: NSArray; cdecl;
    function instanceDescriptorBuffer: Pointer; cdecl;
    function instanceDescriptorBufferOffset: NSUInteger; cdecl;
    function instanceDescriptorStride: NSUInteger; cdecl;
    procedure setInstanceCount(instanceCount: NSUInteger); cdecl;
    procedure setInstancedAccelerationStructures(instancedAccelerationStructures: NSArray); cdecl;
    procedure setInstanceDescriptorBuffer(instanceDescriptorBuffer: Pointer); cdecl;
    procedure setInstanceDescriptorBufferOffset(instanceDescriptorBufferOffset: NSUInteger); cdecl;
    procedure setInstanceDescriptorStride(instanceDescriptorStride: NSUInteger); cdecl;
  end;
  TMTLInstanceAccelerationStructureDescriptor = class(TOCGenericImport<MTLInstanceAccelerationStructureDescriptorClass,
    MTLInstanceAccelerationStructureDescriptor>) end;

  MTLAccelerationStructure = interface(IObjectiveC)
    ['{148E9B28-DAA4-48A4-9870-B99CB6D4F9F1}']
    function size: NSUInteger; cdecl;
  end;

  MTLAccelerationStructureCommandEncoder = interface(IObjectiveC)
    ['{27BADD9E-F49B-474D-A259-2E2D759CD4DE}']
    procedure buildAccelerationStructure(accelerationStructure: Pointer; descriptor: MTLAccelerationStructureDescriptor; scratchBuffer: Pointer;
      scratchBufferOffset: NSUInteger); cdecl;
    procedure copyAccelerationStructure(sourceAccelerationStructure: Pointer; toAccelerationStructure: Pointer); cdecl;
    procedure copyAndCompactAccelerationStructure(sourceAccelerationStructure: Pointer; toAccelerationStructure: Pointer); cdecl;
    procedure refitAccelerationStructure(sourceAccelerationStructure: Pointer; descriptor: MTLAccelerationStructureDescriptor; destination: Pointer;
      scratchBuffer: Pointer; scratchBufferOffset: NSUInteger); cdecl;
    procedure sampleCountersInBuffer(sampleBuffer: Pointer; atSampleIndex: NSUInteger; withBarrier: Boolean); cdecl;
    procedure updateFence(fence: Pointer); cdecl;
    procedure useHeap(heap: Pointer); cdecl;
    procedure useHeaps(heaps: Pid; count: NSUInteger); cdecl;
    procedure useResource(resource: Pointer; usage: MTLResourceUsage); cdecl;
    procedure useResources(resources: Pid; count: NSUInteger; usage: MTLResourceUsage); cdecl;
    procedure waitForFence(fence: Pointer); cdecl;
    procedure writeCompactedAccelerationStructureSize(accelerationStructure: Pointer; toBuffer: Pointer; offset: NSUInteger); cdecl;
  end;

  MTLRasterizationRateSampleArrayClass = interface(NSObjectClass)
    ['{CC0D9DBF-0B60-44A2-95CA-B0C7577C5D3A}']
  end;

  MTLRasterizationRateSampleArray = interface(NSObject)
    ['{8C7D643F-2EE6-41F7-AA18-ED8D557F82C8}']
    function objectAtIndexedSubscript(index: NSUInteger): NSNumber; cdecl;
    procedure setObject(value: NSNumber; atIndexedSubscript: NSUInteger); cdecl;
  end;
  TMTLRasterizationRateSampleArray = class(TOCGenericImport<MTLRasterizationRateSampleArrayClass, MTLRasterizationRateSampleArray>) end;

  MTLRasterizationRateLayerDescriptorClass = interface(NSObjectClass)
    ['{455471FA-DF08-452B-887A-79F88ABE477E}']
  end;

  MTLRasterizationRateLayerDescriptor = interface(NSObject)
    ['{D6EAAD95-43DA-4EFA-A15A-24F5F7EF129B}']
    function horizontal: MTLRasterizationRateSampleArray; cdecl;
    function horizontalSampleStorage: PSingle; cdecl;
    function initWithSampleCount(sampleCount: MTLSize): Pointer; overload; cdecl;
    function initWithSampleCount(sampleCount: MTLSize; horizontal: PSingle; vertical: PSingle): Pointer; overload; cdecl;
    function sampleCount: MTLSize; cdecl;
    function vertical: MTLRasterizationRateSampleArray; cdecl;
    function verticalSampleStorage: PSingle; cdecl;
  end;
  TMTLRasterizationRateLayerDescriptor = class(TOCGenericImport<MTLRasterizationRateLayerDescriptorClass, MTLRasterizationRateLayerDescriptor>) end;
  PMTLRasterizationRateLayerDescriptor = ^MTLRasterizationRateLayerDescriptor;

  MTLRasterizationRateLayerArrayClass = interface(NSObjectClass)
    ['{A750C5BD-4062-4D91-8525-024FA3DBC48B}']
  end;

  MTLRasterizationRateLayerArray = interface(NSObject)
    ['{5F129A81-19CE-4EDC-A03C-C9D34C26D954}']
    function objectAtIndexedSubscript(layerIndex: NSUInteger): MTLRasterizationRateLayerDescriptor; cdecl;
    procedure setObject(layer: MTLRasterizationRateLayerDescriptor; atIndexedSubscript: NSUInteger); cdecl;
  end;
  TMTLRasterizationRateLayerArray = class(TOCGenericImport<MTLRasterizationRateLayerArrayClass, MTLRasterizationRateLayerArray>) end;

  MTLRasterizationRateMapDescriptorClass = interface(NSObjectClass)
    ['{02A1CB5E-CB6B-4520-863A-F408562222D1}']
    {class} function rasterizationRateMapDescriptorWithScreenSize(screenSize: MTLSize; layerCount: NSUInteger; layers:
      PMTLRasterizationRateLayerDescriptor): MTLRasterizationRateMapDescriptor; overload; cdecl;
    {class} function rasterizationRateMapDescriptorWithScreenSize(screenSize: MTLSize;
      layer: MTLRasterizationRateLayerDescriptor): MTLRasterizationRateMapDescriptor; overload; cdecl;
    {class} function rasterizationRateMapDescriptorWithScreenSize(screenSize: MTLSize): MTLRasterizationRateMapDescriptor; overload; cdecl;
  end;

  MTLRasterizationRateMapDescriptor = interface(NSObject)
    ['{47CFC615-825E-455A-BFBF-20E69975319B}']
    [MethodName('label')]
    function &label: NSString; cdecl;
    function layerAtIndex(layerIndex: NSUInteger): MTLRasterizationRateLayerDescriptor; cdecl;
    function layerCount: NSUInteger; cdecl;
    function layers: MTLRasterizationRateLayerArray; cdecl;
    function screenSize: MTLSize; cdecl;
    procedure setLabel(&label: NSString); cdecl;
    procedure setLayer(layer: MTLRasterizationRateLayerDescriptor; atIndex: NSUInteger); cdecl;
    procedure setScreenSize(screenSize: MTLSize); cdecl;
  end;
  TMTLRasterizationRateMapDescriptor = class(TOCGenericImport<MTLRasterizationRateMapDescriptorClass, MTLRasterizationRateMapDescriptor>) end;

  MTLRasterizationRateMap = interface(IObjectiveC)
    ['{0470A42E-8EB0-4398-839B-8182C9376C62}']
    [MethodName('label')]
    function &label: NSString; cdecl;
    procedure copyParameterDataToBuffer(buffer: Pointer; offset: NSUInteger); cdecl;
    function device: Pointer; cdecl;
    function layerCount: NSUInteger; cdecl;
    function mapPhysicalToScreenCoordinates(physicalCoordinates: MTLCoordinate2D; forLayer: NSUInteger): MTLCoordinate2D; cdecl;
    function mapScreenToPhysicalCoordinates(screenCoordinates: MTLCoordinate2D; forLayer: NSUInteger): MTLCoordinate2D; cdecl;
    function parameterBufferSizeAndAlign: MTLSizeAndAlign; cdecl;
    function physicalGranularity: MTLSize; cdecl;
    function physicalSizeForLayer(layerIndex: NSUInteger): MTLSize; cdecl;
    function screenSize: MTLSize; cdecl;
  end;

  MTLDynamicLibrary = interface(IObjectiveC)
    ['{0A58D4E2-B75B-4909-B0F0-19068407B16A}']
    [MethodName('label')]
    function &label: NSString; cdecl;
    function device: Pointer; cdecl;
    function installName: NSString; cdecl;
    function serializeToURL(url: NSURL; error: PPointer): Boolean; cdecl;
    procedure setLabel(&label: NSString); cdecl;
  end;

  MTLFunctionHandle = interface(IObjectiveC)
    ['{FE4D18CE-D423-4145-A9EC-32FA3855980C}']
    function device: Pointer; cdecl;
    function functionType: MTLFunctionType; cdecl;
    function name: NSString; cdecl;
  end;

  MTLVisibleFunctionTableDescriptorClass = interface(NSObjectClass)
    ['{3186E0A8-5C81-4CE7-B6B3-0339491D57B9}']
    {class} function visibleFunctionTableDescriptor: MTLVisibleFunctionTableDescriptor; cdecl;
  end;

  MTLVisibleFunctionTableDescriptor = interface(NSObject)
    ['{34724945-3038-48CF-B01C-63D665A88203}']
    function functionCount: NSUInteger; cdecl;
    procedure setFunctionCount(functionCount: NSUInteger); cdecl;
  end;
  TMTLVisibleFunctionTableDescriptor = class(TOCGenericImport<MTLVisibleFunctionTableDescriptorClass, MTLVisibleFunctionTableDescriptor>) end;

  MTLVisibleFunctionTable = interface(IObjectiveC)
    ['{9DF02309-3242-4D15-8FFE-DEA7549820F7}']
    procedure setFunction(&function: Pointer; atIndex: NSUInteger); cdecl;
    procedure setFunctions(functions: Pid; withRange: NSRange); cdecl;
  end;

  MTLBinaryArchiveDescriptorClass = interface(NSObjectClass)
    ['{97715C5E-D61B-4024-BA73-A58ACFB56118}']
  end;

  MTLBinaryArchiveDescriptor = interface(NSObject)
    ['{116987FF-3029-49FC-970F-AAE5FB29C7A4}']
    procedure setUrl(url: NSURL); cdecl;
    function url: NSURL; cdecl;
  end;
  TMTLBinaryArchiveDescriptor = class(TOCGenericImport<MTLBinaryArchiveDescriptorClass, MTLBinaryArchiveDescriptor>) end;

  MTLBinaryArchive = interface(IObjectiveC)
    ['{6BE46D81-CEF4-40D4-AB7A-0661DF7E7DE3}']
    [MethodName('label')]
    function &label: NSString; cdecl;
    function addComputePipelineFunctionsWithDescriptor(descriptor: MTLComputePipelineDescriptor; error: PPointer): Boolean; cdecl;
    function addRenderPipelineFunctionsWithDescriptor(descriptor: MTLRenderPipelineDescriptor; error: PPointer): Boolean; cdecl;
    function addTileRenderPipelineFunctionsWithDescriptor(descriptor: MTLTileRenderPipelineDescriptor; error: PPointer): Boolean; cdecl;
    function device: Pointer; cdecl;
    function serializeToURL(url: NSURL; error: PPointer): Boolean; cdecl;
    procedure setLabel(&label: NSString); cdecl;
  end;

  MTLIntersectionFunctionTableDescriptorClass = interface(NSObjectClass)
    ['{0D1DFD03-EB2D-4557-BB0A-B59D8E80362D}']
    {class} function intersectionFunctionTableDescriptor: MTLIntersectionFunctionTableDescriptor; cdecl;
  end;

  MTLIntersectionFunctionTableDescriptor = interface(NSObject)
    ['{99CE0048-C1F0-4118-9B41-52BB277DE4CA}']
    function functionCount: NSUInteger; cdecl;
    procedure setFunctionCount(functionCount: NSUInteger); cdecl;
  end;
  TMTLIntersectionFunctionTableDescriptor = class(TOCGenericImport<MTLIntersectionFunctionTableDescriptorClass,
    MTLIntersectionFunctionTableDescriptor>) end;

  MTLIntersectionFunctionTable = interface(IObjectiveC)
    ['{06C308F2-C082-4C28-97FC-F36E393AE34F}']
    procedure setBuffer(buffer: Pointer; offset: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setBuffers(buffers: Pid; offsets: PNSUInteger; withRange: NSRange); cdecl;
    procedure setFunction(&function: Pointer; atIndex: NSUInteger); cdecl;
    procedure setFunctions(functions: Pid; withRange: NSRange); cdecl;
    procedure setOpaqueTriangleIntersectionFunctionWithSignature(signature: MTLIntersectionFunctionSignature; withRange: NSRange); overload; cdecl;
    procedure setOpaqueTriangleIntersectionFunctionWithSignature(signature: MTLIntersectionFunctionSignature; atIndex: NSUInteger); overload; cdecl;
    procedure setVisibleFunctionTable(functionTable: Pointer; atBufferIndex: NSUInteger); cdecl;
    procedure setVisibleFunctionTables(functionTables: Pid; withBufferRange: NSRange); cdecl;
  end;

function MTLLibraryErrorDomain: NSErrorDomain;
function MTLCommonCounterTimestamp: MTLCommonCounter;
function MTLCommonCounterTessellationInputPatches: MTLCommonCounter;
function MTLCommonCounterVertexInvocations: MTLCommonCounter;
function MTLCommonCounterPostTessellationVertexInvocations: MTLCommonCounter;
function MTLCommonCounterClipperInvocations: MTLCommonCounter;
function MTLCommonCounterClipperPrimitivesOut: MTLCommonCounter;
function MTLCommonCounterFragmentInvocations: MTLCommonCounter;
function MTLCommonCounterFragmentsPassed: MTLCommonCounter;
function MTLCommonCounterComputeKernelInvocations: MTLCommonCounter;
function MTLCommonCounterTotalCycles: MTLCommonCounter;
function MTLCommonCounterVertexCycles: MTLCommonCounter;
function MTLCommonCounterTessellationCycles: MTLCommonCounter;
function MTLCommonCounterPostTessellationVertexCycles: MTLCommonCounter;
function MTLCommonCounterFragmentCycles: MTLCommonCounter;
function MTLCommonCounterRenderTargetWriteCycles: MTLCommonCounter;
function MTLCommonCounterSetTimestamp: MTLCommonCounterSet;
function MTLCommonCounterSetStageUtilization: MTLCommonCounterSet;
function MTLCommonCounterSetStatistic: MTLCommonCounterSet;
function MTLCounterErrorDomain: NSErrorDomain;
function MTLCommandBufferErrorDomain: NSErrorDomain;
function MTLCommandBufferEncoderInfoErrorKey: NSErrorUserInfoKey;
function MTLCaptureErrorDomain: NSErrorDomain;
function MTLDynamicLibraryDomain: NSErrorDomain;
function MTLBinaryArchiveDomain: NSErrorDomain;

const
  libMetal = '/System/Library/Frameworks/Metal.framework/Metal';

function MTLCreateSystemDefaultDevice: Pointer; cdecl;
  external libMetal name _PU + 'MTLCreateSystemDefaultDevice';

function MTLCopyAllDevices: NSArray; cdecl;
  external libMetal name _PU + 'MTLCopyAllDevices';

implementation

{$IF Defined(IOS) and not Defined(CPUARM)}
uses
  Posix.Dlfcn;

var
  MetalModule: THandle;
{$ENDIF}

function MTLLibraryErrorDomain: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libMetal, 'MTLLibraryErrorDomain');
end;

function MTLCommonCounterTimestamp: MTLCommonCounter;
begin
  Result := CocoaNSStringConst(libMetal, 'MTLCommonCounterTimestamp');
end;

function MTLCommonCounterTessellationInputPatches: MTLCommonCounter;
begin
  Result := CocoaNSStringConst(libMetal, 'MTLCommonCounterTessellationInputPatches');
end;

function MTLCommonCounterVertexInvocations: MTLCommonCounter;
begin
  Result := CocoaNSStringConst(libMetal, 'MTLCommonCounterVertexInvocations');
end;

function MTLCommonCounterPostTessellationVertexInvocations: MTLCommonCounter;
begin
  Result := CocoaNSStringConst(libMetal, 'MTLCommonCounterPostTessellationVertexInvocations');
end;

function MTLCommonCounterClipperInvocations: MTLCommonCounter;
begin
  Result := CocoaNSStringConst(libMetal, 'MTLCommonCounterClipperInvocations');
end;

function MTLCommonCounterClipperPrimitivesOut: MTLCommonCounter;
begin
  Result := CocoaNSStringConst(libMetal, 'MTLCommonCounterClipperPrimitivesOut');
end;

function MTLCommonCounterFragmentInvocations: MTLCommonCounter;
begin
  Result := CocoaNSStringConst(libMetal, 'MTLCommonCounterFragmentInvocations');
end;

function MTLCommonCounterFragmentsPassed: MTLCommonCounter;
begin
  Result := CocoaNSStringConst(libMetal, 'MTLCommonCounterFragmentsPassed');
end;

function MTLCommonCounterComputeKernelInvocations: MTLCommonCounter;
begin
  Result := CocoaNSStringConst(libMetal, 'MTLCommonCounterComputeKernelInvocations');
end;

function MTLCommonCounterTotalCycles: MTLCommonCounter;
begin
  Result := CocoaNSStringConst(libMetal, 'MTLCommonCounterTotalCycles');
end;

function MTLCommonCounterVertexCycles: MTLCommonCounter;
begin
  Result := CocoaNSStringConst(libMetal, 'MTLCommonCounterVertexCycles');
end;

function MTLCommonCounterTessellationCycles: MTLCommonCounter;
begin
  Result := CocoaNSStringConst(libMetal, 'MTLCommonCounterTessellationCycles');
end;

function MTLCommonCounterPostTessellationVertexCycles: MTLCommonCounter;
begin
  Result := CocoaNSStringConst(libMetal, 'MTLCommonCounterPostTessellationVertexCycles');
end;

function MTLCommonCounterFragmentCycles: MTLCommonCounter;
begin
  Result := CocoaNSStringConst(libMetal, 'MTLCommonCounterFragmentCycles');
end;

function MTLCommonCounterRenderTargetWriteCycles: MTLCommonCounter;
begin
  Result := CocoaNSStringConst(libMetal, 'MTLCommonCounterRenderTargetWriteCycles');
end;

function MTLCommonCounterSetTimestamp: MTLCommonCounterSet;
begin
  Result := CocoaNSStringConst(libMetal, 'MTLCommonCounterSetTimestamp');
end;

function MTLCommonCounterSetStageUtilization: MTLCommonCounterSet;
begin
  Result := CocoaNSStringConst(libMetal, 'MTLCommonCounterSetStageUtilization');
end;

function MTLCommonCounterSetStatistic: MTLCommonCounterSet;
begin
  Result := CocoaNSStringConst(libMetal, 'MTLCommonCounterSetStatistic');
end;

function MTLCounterErrorDomain: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libMetal, 'MTLCounterErrorDomain');
end;

function MTLCommandBufferErrorDomain: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libMetal, 'MTLCommandBufferErrorDomain');
end;

function MTLCommandBufferEncoderInfoErrorKey: NSErrorUserInfoKey;
begin
  Result := CocoaNSStringConst(libMetal, 'MTLCommandBufferEncoderInfoErrorKey');
end;

function MTLCaptureErrorDomain: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libMetal, 'MTLCaptureErrorDomain');
end;

function MTLDynamicLibraryDomain: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libMetal, 'MTLDynamicLibraryDomain');
end;

function MTLBinaryArchiveDomain: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libMetal, 'MTLBinaryArchiveDomain');
end;

{$IF Defined(IOS) and not Defined(CPUARM)}
initialization
  MetalModule := dlopen(MarshaledAString(libMetal), RTLD_LAZY);

finalization
  dlclose(MetalModule)
{$ENDIF}

end.