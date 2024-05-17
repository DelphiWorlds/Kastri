unit DW.Macapi.CoreAudioTypes;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2024 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.Foundation, Macapi.CocoaTypes;

const
  kAudio_NoError = 0;
  kAudio_UnimplementedError = -4;
  kAudio_FileNotFoundError = -43;
  kAudio_FilePermissionError = -54;
  kAudio_TooManyFilesOpenError = -42;
  kAudio_BadFilePathError = 561017960;
  kAudio_ParamError = -50;
  kAudio_MemFullError = -108;
  kAudioFormatLinearPCM = 1819304813;
  kAudioFormatAC3 = 1633889587;
  kAudioFormat60958AC3 = 1667326771;
  kAudioFormatAppleIMA4 = 1768775988;
  kAudioFormatMPEG4AAC = 1633772320;
  kAudioFormatMPEG4CELP = 1667591280;
  kAudioFormatMPEG4HVXC = 1752594531;
  kAudioFormatMPEG4TwinVQ = 1953986161;
  kAudioFormatMACE3 = 1296122675;
  kAudioFormatMACE6 = 1296122678;
  kAudioFormatULaw = 1970037111;
  kAudioFormatALaw = 1634492791;
  kAudioFormatQDesign = 1363430723;
  kAudioFormatQDesign2 = 1363430706;
  kAudioFormatQUALCOMM = 1365470320;
  kAudioFormatMPEGLayer1 = 778924081;
  kAudioFormatMPEGLayer2 = 778924082;
  kAudioFormatMPEGLayer3 = 778924083;
  kAudioFormatTimeCode = 1953066341;
  kAudioFormatMIDIStream = 1835623529;
  kAudioFormatParameterValueStream = 1634760307;
  kAudioFormatAppleLossless = 1634492771;
  kAudioFormatMPEG4AAC_HE = 1633772392;
  kAudioFormatMPEG4AAC_LD = 1633772396;
  kAudioFormatMPEG4AAC_ELD = 1633772389;
  kAudioFormatMPEG4AAC_ELD_SBR = 1633772390;
  kAudioFormatMPEG4AAC_ELD_V2 = 1633772391;
  kAudioFormatMPEG4AAC_HE_V2 = 1633772400;
  kAudioFormatMPEG4AAC_Spatial = 1633772403;
  kAudioFormatMPEGD_USAC = 1970495843;
  kAudioFormatAMR = 1935764850;
  kAudioFormatAMR_WB = 1935767394;
  kAudioFormatAudible = 1096107074;
  kAudioFormatiLBC = 1768710755;
  kAudioFormatDVIIntelIMA = 1836253201;
  kAudioFormatMicrosoftGSM = 1836253233;
  kAudioFormatAES3 = 1634038579;
  kAudioFormatEnhancedAC3 = 1700998451;
  kAudioFormatFLAC = 1718378851;
  kAudioFormatOpus = 1869641075;
  kAudioFormatFlagIsFloat = 1 shl 0;
  kAudioFormatFlagIsBigEndian = 1 shl 1;
  kAudioFormatFlagIsSignedInteger = 1 shl 2;
  kAudioFormatFlagIsPacked = 1 shl 3;
  kAudioFormatFlagIsAlignedHigh = 1 shl 4;
  kAudioFormatFlagIsNonInterleaved = 1 shl 5;
  kAudioFormatFlagIsNonMixable = 1 shl 6;
  kAudioFormatFlagsAreAllClear = -2147483648;
  kLinearPCMFormatFlagIsFloat = kAudioFormatFlagIsFloat;
  kLinearPCMFormatFlagIsBigEndian = kAudioFormatFlagIsBigEndian;
  kLinearPCMFormatFlagIsSignedInteger = kAudioFormatFlagIsSignedInteger;
  kLinearPCMFormatFlagIsPacked = kAudioFormatFlagIsPacked;
  kLinearPCMFormatFlagIsAlignedHigh = kAudioFormatFlagIsAlignedHigh;
  kLinearPCMFormatFlagIsNonInterleaved = kAudioFormatFlagIsNonInterleaved;
  kLinearPCMFormatFlagIsNonMixable = kAudioFormatFlagIsNonMixable;
  kLinearPCMFormatFlagsSampleFractionShift = 7;
  kLinearPCMFormatFlagsSampleFractionMask = 8064;
  kLinearPCMFormatFlagsAreAllClear = kAudioFormatFlagsAreAllClear;
  kAppleLosslessFormatFlag_16BitSourceData = 1;
  kAppleLosslessFormatFlag_20BitSourceData = 2;
  kAppleLosslessFormatFlag_24BitSourceData = 3;
  kAppleLosslessFormatFlag_32BitSourceData = 4;
  kAudioFormatFlagsNativeEndian = 0;
  kAudioFormatFlagsCanonical = 9;
  kAudioFormatFlagsAudioUnitCanonical = 41;
  kAudioFormatFlagsNativeFloatPacked = 9;
  kSMPTETimeType24 = 0;
  kSMPTETimeType25 = 1;
  kSMPTETimeType30Drop = 2;
  kSMPTETimeType30 = 3;
  kSMPTETimeType2997 = 4;
  kSMPTETimeType2997Drop = 5;
  kSMPTETimeType60 = 6;
  kSMPTETimeType5994 = 7;
  kSMPTETimeType60Drop = 8;
  kSMPTETimeType5994Drop = 9;
  kSMPTETimeType50 = 10;
  kSMPTETimeType2398 = 11;
  kSMPTETimeUnknown = 0;
  kSMPTETimeValid = 1 shl 0;
  kSMPTETimeRunning = 1 shl 1;
  kAudioTimeStampNothingValid = 0;
  kAudioTimeStampSampleTimeValid = 1 shl 0;
  kAudioTimeStampHostTimeValid = 1 shl 1;
  kAudioTimeStampRateScalarValid = 1 shl 2;
  kAudioTimeStampWordClockTimeValid = 1 shl 3;
  kAudioTimeStampSMPTETimeValid = 1 shl 4;
  kAudioTimeStampSampleHostTimeValid = kAudioTimeStampSampleTimeValid or kAudioTimeStampHostTimeValid;
  kAudioChannelLabel_Unknown = -1;
  kAudioChannelLabel_Unused = 0;
  kAudioChannelLabel_UseCoordinates = 100;
  kAudioChannelLabel_Left = 1;
  kAudioChannelLabel_Right = 2;
  kAudioChannelLabel_Center = 3;
  kAudioChannelLabel_LFEScreen = 4;
  kAudioChannelLabel_LeftSurround = 5;
  kAudioChannelLabel_RightSurround = 6;
  kAudioChannelLabel_LeftCenter = 7;
  kAudioChannelLabel_RightCenter = 8;
  kAudioChannelLabel_CenterSurround = 9;
  kAudioChannelLabel_LeftSurroundDirect = 10;
  kAudioChannelLabel_RightSurroundDirect = 11;
  kAudioChannelLabel_TopCenterSurround = 12;
  kAudioChannelLabel_VerticalHeightLeft = 13;
  kAudioChannelLabel_VerticalHeightCenter = 14;
  kAudioChannelLabel_VerticalHeightRight = 15;
  kAudioChannelLabel_TopBackLeft = 16;
  kAudioChannelLabel_TopBackCenter = 17;
  kAudioChannelLabel_TopBackRight = 18;
  kAudioChannelLabel_RearSurroundLeft = 33;
  kAudioChannelLabel_RearSurroundRight = 34;
  kAudioChannelLabel_LeftWide = 35;
  kAudioChannelLabel_RightWide = 36;
  kAudioChannelLabel_LFE2 = 37;
  kAudioChannelLabel_LeftTotal = 38;
  kAudioChannelLabel_RightTotal = 39;
  kAudioChannelLabel_HearingImpaired = 40;
  kAudioChannelLabel_Narration = 41;
  kAudioChannelLabel_Mono = 42;
  kAudioChannelLabel_DialogCentricMix = 43;
  kAudioChannelLabel_CenterSurroundDirect = 44;
  kAudioChannelLabel_Haptic = 45;
  kAudioChannelLabel_LeftTopFront = kAudioChannelLabel_VerticalHeightLeft;
  kAudioChannelLabel_CenterTopFront = kAudioChannelLabel_VerticalHeightCenter;
  kAudioChannelLabel_RightTopFront = kAudioChannelLabel_VerticalHeightRight;
  kAudioChannelLabel_LeftTopMiddle = 49;
  kAudioChannelLabel_CenterTopMiddle = kAudioChannelLabel_TopCenterSurround;
  kAudioChannelLabel_RightTopMiddle = 51;
  kAudioChannelLabel_LeftTopRear = 52;
  kAudioChannelLabel_CenterTopRear = 53;
  kAudioChannelLabel_RightTopRear = 54;
  kAudioChannelLabel_LeftSideSurround = 55;
  kAudioChannelLabel_RightSideSurround = 56;
  kAudioChannelLabel_LeftBottom = 57;
  kAudioChannelLabel_RightBottom = 58;
  kAudioChannelLabel_CenterBottom = 59;
  kAudioChannelLabel_LeftTopSurround = 60;
  kAudioChannelLabel_RightTopSurround = 61;
  kAudioChannelLabel_LFE3 = 62;
  kAudioChannelLabel_LeftBackSurround = 63;
  kAudioChannelLabel_RightBackSurround = 64;
  kAudioChannelLabel_LeftEdgeOfScreen = 65;
  kAudioChannelLabel_RightEdgeOfScreen = 66;
  kAudioChannelLabel_Ambisonic_W = 200;
  kAudioChannelLabel_Ambisonic_X = 201;
  kAudioChannelLabel_Ambisonic_Y = 202;
  kAudioChannelLabel_Ambisonic_Z = 203;
  kAudioChannelLabel_MS_Mid = 204;
  kAudioChannelLabel_MS_Side = 205;
  kAudioChannelLabel_XY_X = 206;
  kAudioChannelLabel_XY_Y = 207;
  kAudioChannelLabel_BinauralLeft = 208;
  kAudioChannelLabel_BinauralRight = 209;
  kAudioChannelLabel_HeadphonesLeft = 301;
  kAudioChannelLabel_HeadphonesRight = 302;
  kAudioChannelLabel_ClickTrack = 304;
  kAudioChannelLabel_ForeignLanguage = 305;
  kAudioChannelLabel_Discrete = 400;
  kAudioChannelLabel_Discrete_0 = 65536;
  kAudioChannelLabel_Discrete_1 = 65537;
  kAudioChannelLabel_Discrete_2 = 65538;
  kAudioChannelLabel_Discrete_3 = 65539;
  kAudioChannelLabel_Discrete_4 = 65540;
  kAudioChannelLabel_Discrete_5 = 65541;
  kAudioChannelLabel_Discrete_6 = 65542;
  kAudioChannelLabel_Discrete_7 = 65543;
  kAudioChannelLabel_Discrete_8 = 65544;
  kAudioChannelLabel_Discrete_9 = 65545;
  kAudioChannelLabel_Discrete_10 = 65546;
  kAudioChannelLabel_Discrete_11 = 65547;
  kAudioChannelLabel_Discrete_12 = 65548;
  kAudioChannelLabel_Discrete_13 = 65549;
  kAudioChannelLabel_Discrete_14 = 65550;
  kAudioChannelLabel_Discrete_15 = 65551;
  kAudioChannelLabel_Discrete_65535 = 131071;
  kAudioChannelLabel_HOA_ACN = 500;
  kAudioChannelLabel_HOA_ACN_0 = 131072;
  kAudioChannelLabel_HOA_ACN_1 = 131073;
  kAudioChannelLabel_HOA_ACN_2 = 131074;
  kAudioChannelLabel_HOA_ACN_3 = 131075;
  kAudioChannelLabel_HOA_ACN_4 = 131076;
  kAudioChannelLabel_HOA_ACN_5 = 131077;
  kAudioChannelLabel_HOA_ACN_6 = 131078;
  kAudioChannelLabel_HOA_ACN_7 = 131079;
  kAudioChannelLabel_HOA_ACN_8 = 131080;
  kAudioChannelLabel_HOA_ACN_9 = 131081;
  kAudioChannelLabel_HOA_ACN_10 = 131082;
  kAudioChannelLabel_HOA_ACN_11 = 131083;
  kAudioChannelLabel_HOA_ACN_12 = 131084;
  kAudioChannelLabel_HOA_ACN_13 = 131085;
  kAudioChannelLabel_HOA_ACN_14 = 131086;
  kAudioChannelLabel_HOA_ACN_15 = 131087;
  kAudioChannelLabel_HOA_ACN_65024 = 196096;
  kAudioChannelLabel_HOA_SN3D = kAudioChannelLabel_HOA_ACN_0;
  kAudioChannelLabel_HOA_N3D = 3 shl 16;
  kAudioChannelLabel_Object = 4 shl 16;
  kAudioChannelLabel_BeginReserved = -268435456;
  kAudioChannelLabel_EndReserved = -2;
  kAudioChannelBit_Left = 1 shl 0;
  kAudioChannelBit_Right = 1 shl 1;
  kAudioChannelBit_Center = 1 shl 2;
  kAudioChannelBit_LFEScreen = 1 shl 3;
  kAudioChannelBit_LeftSurround = 1 shl 4;
  kAudioChannelBit_RightSurround = 1 shl 5;
  kAudioChannelBit_LeftCenter = 1 shl 6;
  kAudioChannelBit_RightCenter = 1 shl 7;
  kAudioChannelBit_CenterSurround = 1 shl 8;
  kAudioChannelBit_LeftSurroundDirect = 1 shl 9;
  kAudioChannelBit_RightSurroundDirect = 1 shl 10;
  kAudioChannelBit_TopCenterSurround = 1 shl 11;
  kAudioChannelBit_VerticalHeightLeft = 1 shl 12;
  kAudioChannelBit_VerticalHeightCenter = 1 shl 13;
  kAudioChannelBit_VerticalHeightRight = 1 shl 14;
  kAudioChannelBit_TopBackLeft = 1 shl 15;
  kAudioChannelBit_TopBackCenter = 1 shl 16;
  kAudioChannelBit_TopBackRight = 1 shl 17;
  kAudioChannelBit_LeftTopFront = kAudioChannelBit_VerticalHeightLeft;
  kAudioChannelBit_CenterTopFront = kAudioChannelBit_VerticalHeightCenter;
  kAudioChannelBit_RightTopFront = kAudioChannelBit_VerticalHeightRight;
  kAudioChannelBit_LeftTopMiddle = 1 shl 21;
  kAudioChannelBit_CenterTopMiddle = kAudioChannelBit_TopCenterSurround;
  kAudioChannelBit_RightTopMiddle = 1 shl 23;
  kAudioChannelBit_LeftTopRear = 1 shl 24;
  kAudioChannelBit_CenterTopRear = 1 shl 25;
  kAudioChannelBit_RightTopRear = 1 shl 26;
  kAudioChannelFlags_AllOff = 0;
  kAudioChannelFlags_RectangularCoordinates = 1 shl 0;
  kAudioChannelFlags_SphericalCoordinates = 1 shl 1;
  kAudioChannelFlags_Meters = 1 shl 2;
  kAudioChannelCoordinates_LeftRight = 0;
  kAudioChannelCoordinates_BackFront = 1;
  kAudioChannelCoordinates_DownUp = 2;
  kAudioChannelCoordinates_Azimuth = 0;
  kAudioChannelCoordinates_Elevation = 1;
  kAudioChannelCoordinates_Distance = 2;
  kAudioChannelLayoutTag_UseChannelDescriptions = 0;
  kAudioChannelLayoutTag_UseChannelBitmap = 65536;
  kAudioChannelLayoutTag_Mono = 6553601;
  kAudioChannelLayoutTag_Stereo = 6619138;
  kAudioChannelLayoutTag_StereoHeadphones = 6684674;
  kAudioChannelLayoutTag_MatrixStereo = 6750210;
  kAudioChannelLayoutTag_MidSide = 6815746;
  kAudioChannelLayoutTag_XY = 6881282;
  kAudioChannelLayoutTag_Binaural = 6946818;
  kAudioChannelLayoutTag_Ambisonic_B_Format = 7012356;
  kAudioChannelLayoutTag_Quadraphonic = 7077892;
  kAudioChannelLayoutTag_Pentagonal = 7143429;
  kAudioChannelLayoutTag_Hexagonal = 7208966;
  kAudioChannelLayoutTag_Octagonal = 7274504;
  kAudioChannelLayoutTag_Cube = 7340040;
  kAudioChannelLayoutTag_MPEG_1_0 = kAudioChannelLayoutTag_Mono;
  kAudioChannelLayoutTag_MPEG_2_0 = kAudioChannelLayoutTag_Stereo;
  kAudioChannelLayoutTag_MPEG_3_0_A = 7405571;
  kAudioChannelLayoutTag_MPEG_3_0_B = 7471107;
  kAudioChannelLayoutTag_MPEG_4_0_A = 7536644;
  kAudioChannelLayoutTag_MPEG_4_0_B = 7602180;
  kAudioChannelLayoutTag_MPEG_5_0_A = 7667717;
  kAudioChannelLayoutTag_MPEG_5_0_B = 7733253;
  kAudioChannelLayoutTag_MPEG_5_0_C = 7798789;
  kAudioChannelLayoutTag_MPEG_5_0_D = 7864325;
  kAudioChannelLayoutTag_MPEG_5_1_A = 7929862;
  kAudioChannelLayoutTag_MPEG_5_1_B = 7995398;
  kAudioChannelLayoutTag_MPEG_5_1_C = 8060934;
  kAudioChannelLayoutTag_MPEG_5_1_D = 8126470;
  kAudioChannelLayoutTag_MPEG_6_1_A = 8192007;
  kAudioChannelLayoutTag_MPEG_7_1_A = 8257544;
  kAudioChannelLayoutTag_MPEG_7_1_B = 8323080;
  kAudioChannelLayoutTag_MPEG_7_1_C = 8388616;
  kAudioChannelLayoutTag_Emagic_Default_7_1 = 8454152;
  kAudioChannelLayoutTag_SMPTE_DTV = 8519688;
  kAudioChannelLayoutTag_ITU_1_0 = kAudioChannelLayoutTag_Mono;
  kAudioChannelLayoutTag_ITU_2_0 = kAudioChannelLayoutTag_Stereo;
  kAudioChannelLayoutTag_ITU_2_1 = 8585219;
  kAudioChannelLayoutTag_ITU_2_2 = 8650756;
  kAudioChannelLayoutTag_ITU_3_0 = kAudioChannelLayoutTag_MPEG_3_0_A;
  kAudioChannelLayoutTag_ITU_3_1 = kAudioChannelLayoutTag_MPEG_4_0_A;
  kAudioChannelLayoutTag_ITU_3_2 = kAudioChannelLayoutTag_MPEG_5_0_A;
  kAudioChannelLayoutTag_ITU_3_2_1 = kAudioChannelLayoutTag_MPEG_5_1_A;
  kAudioChannelLayoutTag_ITU_3_4_1 = kAudioChannelLayoutTag_MPEG_7_1_C;
  kAudioChannelLayoutTag_DVD_0 = kAudioChannelLayoutTag_Mono;
  kAudioChannelLayoutTag_DVD_1 = kAudioChannelLayoutTag_Stereo;
  kAudioChannelLayoutTag_DVD_2 = kAudioChannelLayoutTag_ITU_2_1;
  kAudioChannelLayoutTag_DVD_3 = kAudioChannelLayoutTag_ITU_2_2;
  kAudioChannelLayoutTag_DVD_4 = 8716291;
  kAudioChannelLayoutTag_DVD_5 = 8781828;
  kAudioChannelLayoutTag_DVD_6 = 8847365;
  kAudioChannelLayoutTag_DVD_7 = kAudioChannelLayoutTag_MPEG_3_0_A;
  kAudioChannelLayoutTag_DVD_8 = kAudioChannelLayoutTag_MPEG_4_0_A;
  kAudioChannelLayoutTag_DVD_9 = kAudioChannelLayoutTag_MPEG_5_0_A;
  kAudioChannelLayoutTag_DVD_10 = 8912900;
  kAudioChannelLayoutTag_DVD_11 = 8978437;
  kAudioChannelLayoutTag_DVD_12 = kAudioChannelLayoutTag_MPEG_5_1_A;
  kAudioChannelLayoutTag_DVD_13 = kAudioChannelLayoutTag_DVD_8;
  kAudioChannelLayoutTag_DVD_14 = kAudioChannelLayoutTag_DVD_9;
  kAudioChannelLayoutTag_DVD_15 = kAudioChannelLayoutTag_DVD_10;
  kAudioChannelLayoutTag_DVD_16 = kAudioChannelLayoutTag_DVD_11;
  kAudioChannelLayoutTag_DVD_17 = kAudioChannelLayoutTag_DVD_12;
  kAudioChannelLayoutTag_DVD_18 = 9043973;
  kAudioChannelLayoutTag_DVD_19 = kAudioChannelLayoutTag_MPEG_5_0_B;
  kAudioChannelLayoutTag_DVD_20 = kAudioChannelLayoutTag_MPEG_5_1_B;
  kAudioChannelLayoutTag_AudioUnit_4 = kAudioChannelLayoutTag_Quadraphonic;
  kAudioChannelLayoutTag_AudioUnit_5 = kAudioChannelLayoutTag_Pentagonal;
  kAudioChannelLayoutTag_AudioUnit_6 = kAudioChannelLayoutTag_Hexagonal;
  kAudioChannelLayoutTag_AudioUnit_8 = kAudioChannelLayoutTag_Octagonal;
  kAudioChannelLayoutTag_AudioUnit_5_0 = kAudioChannelLayoutTag_MPEG_5_0_B;
  kAudioChannelLayoutTag_AudioUnit_6_0 = 9109510;
  kAudioChannelLayoutTag_AudioUnit_7_0 = 9175047;
  kAudioChannelLayoutTag_AudioUnit_7_0_Front = 9699335;
  kAudioChannelLayoutTag_AudioUnit_5_1 = kAudioChannelLayoutTag_MPEG_5_1_A;
  kAudioChannelLayoutTag_AudioUnit_6_1 = kAudioChannelLayoutTag_MPEG_6_1_A;
  kAudioChannelLayoutTag_AudioUnit_7_1 = kAudioChannelLayoutTag_MPEG_7_1_C;
  kAudioChannelLayoutTag_AudioUnit_7_1_Front = kAudioChannelLayoutTag_MPEG_7_1_A;
  kAudioChannelLayoutTag_AAC_3_0 = kAudioChannelLayoutTag_MPEG_3_0_B;
  kAudioChannelLayoutTag_AAC_Quadraphonic = kAudioChannelLayoutTag_Quadraphonic;
  kAudioChannelLayoutTag_AAC_4_0 = kAudioChannelLayoutTag_MPEG_4_0_B;
  kAudioChannelLayoutTag_AAC_5_0 = kAudioChannelLayoutTag_MPEG_5_0_D;
  kAudioChannelLayoutTag_AAC_5_1 = kAudioChannelLayoutTag_MPEG_5_1_D;
  kAudioChannelLayoutTag_AAC_6_0 = 9240582;
  kAudioChannelLayoutTag_AAC_6_1 = 9306119;
  kAudioChannelLayoutTag_AAC_7_0 = 9371655;
  kAudioChannelLayoutTag_AAC_7_1 = kAudioChannelLayoutTag_MPEG_7_1_B;
  kAudioChannelLayoutTag_AAC_7_1_B = 11993096;
  kAudioChannelLayoutTag_AAC_7_1_C = 12058632;
  kAudioChannelLayoutTag_AAC_Octagonal = 9437192;
  kAudioChannelLayoutTag_TMH_10_2_std = 9502736;
  kAudioChannelLayoutTag_TMH_10_2_full = 9568277;
  kAudioChannelLayoutTag_AC3_1_0_1 = 9764866;
  kAudioChannelLayoutTag_AC3_3_0 = 9830403;
  kAudioChannelLayoutTag_AC3_3_1 = 9895940;
  kAudioChannelLayoutTag_AC3_3_0_1 = 9961476;
  kAudioChannelLayoutTag_AC3_2_1_1 = 10027012;
  kAudioChannelLayoutTag_AC3_3_1_1 = 10092549;
  kAudioChannelLayoutTag_EAC_6_0_A = 10158086;
  kAudioChannelLayoutTag_EAC_7_0_A = 10223623;
  kAudioChannelLayoutTag_EAC3_6_1_A = 10289159;
  kAudioChannelLayoutTag_EAC3_6_1_B = 10354695;
  kAudioChannelLayoutTag_EAC3_6_1_C = 10420231;
  kAudioChannelLayoutTag_EAC3_7_1_A = 10485768;
  kAudioChannelLayoutTag_EAC3_7_1_B = 10551304;
  kAudioChannelLayoutTag_EAC3_7_1_C = 10616840;
  kAudioChannelLayoutTag_EAC3_7_1_D = 10682376;
  kAudioChannelLayoutTag_EAC3_7_1_E = 10747912;
  kAudioChannelLayoutTag_EAC3_7_1_F = 10813448;
  kAudioChannelLayoutTag_EAC3_7_1_G = 10878984;
  kAudioChannelLayoutTag_EAC3_7_1_H = 10944520;
  kAudioChannelLayoutTag_DTS_3_1 = 11010052;
  kAudioChannelLayoutTag_DTS_4_1 = 11075589;
  kAudioChannelLayoutTag_DTS_6_0_A = 11141126;
  kAudioChannelLayoutTag_DTS_6_0_B = 11206662;
  kAudioChannelLayoutTag_DTS_6_0_C = 11272198;
  kAudioChannelLayoutTag_DTS_6_1_A = 11337735;
  kAudioChannelLayoutTag_DTS_6_1_B = 11403271;
  kAudioChannelLayoutTag_DTS_6_1_C = 11468807;
  kAudioChannelLayoutTag_DTS_7_0 = 11534343;
  kAudioChannelLayoutTag_DTS_7_1 = 11599880;
  kAudioChannelLayoutTag_DTS_8_0_A = 11665416;
  kAudioChannelLayoutTag_DTS_8_0_B = 11730952;
  kAudioChannelLayoutTag_DTS_8_1_A = 11796489;
  kAudioChannelLayoutTag_DTS_8_1_B = 11862025;
  kAudioChannelLayoutTag_DTS_6_1_D = 11927559;
  kAudioChannelLayoutTag_WAVE_2_1 = kAudioChannelLayoutTag_DVD_4;
  kAudioChannelLayoutTag_WAVE_3_0 = kAudioChannelLayoutTag_MPEG_3_0_A;
  kAudioChannelLayoutTag_WAVE_4_0_A = kAudioChannelLayoutTag_ITU_2_2;
  kAudioChannelLayoutTag_WAVE_4_0_B = 12124164;
  kAudioChannelLayoutTag_WAVE_5_0_A = kAudioChannelLayoutTag_MPEG_5_0_A;
  kAudioChannelLayoutTag_WAVE_5_0_B = 12189701;
  kAudioChannelLayoutTag_WAVE_5_1_A = kAudioChannelLayoutTag_MPEG_5_1_A;
  kAudioChannelLayoutTag_WAVE_5_1_B = 12255238;
  kAudioChannelLayoutTag_WAVE_6_1 = 12320775;
  kAudioChannelLayoutTag_WAVE_7_1 = 12386312;
  kAudioChannelLayoutTag_HOA_ACN_SN3D = 12451840;
  kAudioChannelLayoutTag_HOA_ACN_N3D = 12517376;
  kAudioChannelLayoutTag_Atmos_5_1_2 = 12713992;
  kAudioChannelLayoutTag_Atmos_5_1_4 = 12779530;
  kAudioChannelLayoutTag_Atmos_7_1_2 = 12845066;
  kAudioChannelLayoutTag_Atmos_7_1_4 = 12582924;
  kAudioChannelLayoutTag_Atmos_9_1_6 = 12648464;
  kAudioChannelLayoutTag_Logic_Mono = kAudioChannelLayoutTag_Mono;
  kAudioChannelLayoutTag_Logic_Stereo = kAudioChannelLayoutTag_Stereo;
  kAudioChannelLayoutTag_Logic_Quadraphonic = kAudioChannelLayoutTag_Quadraphonic;
  kAudioChannelLayoutTag_Logic_4_0_A = kAudioChannelLayoutTag_MPEG_4_0_A;
  kAudioChannelLayoutTag_Logic_4_0_B = kAudioChannelLayoutTag_MPEG_4_0_B;
  kAudioChannelLayoutTag_Logic_4_0_C = 12910596;
  kAudioChannelLayoutTag_Logic_5_0_A = kAudioChannelLayoutTag_MPEG_5_0_A;
  kAudioChannelLayoutTag_Logic_5_0_B = kAudioChannelLayoutTag_MPEG_5_0_B;
  kAudioChannelLayoutTag_Logic_5_0_C = kAudioChannelLayoutTag_MPEG_5_0_C;
  kAudioChannelLayoutTag_Logic_5_0_D = kAudioChannelLayoutTag_MPEG_5_0_D;
  kAudioChannelLayoutTag_Logic_5_1_A = kAudioChannelLayoutTag_MPEG_5_1_A;
  kAudioChannelLayoutTag_Logic_5_1_B = kAudioChannelLayoutTag_MPEG_5_1_B;
  kAudioChannelLayoutTag_Logic_5_1_C = kAudioChannelLayoutTag_MPEG_5_1_C;
  kAudioChannelLayoutTag_Logic_5_1_D = kAudioChannelLayoutTag_MPEG_5_1_D;
  kAudioChannelLayoutTag_Logic_6_0_A = kAudioChannelLayoutTag_AAC_6_0;
  kAudioChannelLayoutTag_Logic_6_0_B = 12976134;
  kAudioChannelLayoutTag_Logic_6_0_C = kAudioChannelLayoutTag_AudioUnit_6_0;
  kAudioChannelLayoutTag_Logic_6_1_A = kAudioChannelLayoutTag_AAC_6_1;
  kAudioChannelLayoutTag_Logic_6_1_B = 13041671;
  kAudioChannelLayoutTag_Logic_6_1_C = kAudioChannelLayoutTag_MPEG_6_1_A;
  kAudioChannelLayoutTag_Logic_6_1_D = 13107207;
  kAudioChannelLayoutTag_Logic_7_1_A = kAudioChannelLayoutTag_AudioUnit_7_1;
  kAudioChannelLayoutTag_Logic_7_1_B = 13172744;
  kAudioChannelLayoutTag_Logic_7_1_C = kAudioChannelLayoutTag_MPEG_7_1_C;
  kAudioChannelLayoutTag_Logic_7_1_SDDS_A = kAudioChannelLayoutTag_MPEG_7_1_A;
  kAudioChannelLayoutTag_Logic_7_1_SDDS_B = kAudioChannelLayoutTag_MPEG_7_1_B;
  kAudioChannelLayoutTag_Logic_7_1_SDDS_C = kAudioChannelLayoutTag_Emagic_Default_7_1;
  kAudioChannelLayoutTag_Logic_Atmos_5_1_2 = kAudioChannelLayoutTag_Atmos_5_1_2;
  kAudioChannelLayoutTag_Logic_Atmos_5_1_4 = kAudioChannelLayoutTag_Atmos_5_1_4;
  kAudioChannelLayoutTag_Logic_Atmos_7_1_2 = kAudioChannelLayoutTag_Atmos_7_1_2;
  kAudioChannelLayoutTag_Logic_Atmos_7_1_4_A = kAudioChannelLayoutTag_Atmos_7_1_4;
  kAudioChannelLayoutTag_Logic_Atmos_7_1_4_B = 13238284;
  kAudioChannelLayoutTag_Logic_Atmos_7_1_6 = 13303822;
  kAudioChannelLayoutTag_DiscreteInOrder = 9633792;
  kAudioChannelLayoutTag_CICP_1 = kAudioChannelLayoutTag_MPEG_1_0;
  kAudioChannelLayoutTag_CICP_2 = kAudioChannelLayoutTag_MPEG_2_0;
  kAudioChannelLayoutTag_CICP_3 = kAudioChannelLayoutTag_MPEG_3_0_A;
  kAudioChannelLayoutTag_CICP_4 = kAudioChannelLayoutTag_MPEG_4_0_A;
  kAudioChannelLayoutTag_CICP_5 = kAudioChannelLayoutTag_MPEG_5_0_A;
  kAudioChannelLayoutTag_CICP_6 = kAudioChannelLayoutTag_MPEG_5_1_A;
  kAudioChannelLayoutTag_CICP_7 = kAudioChannelLayoutTag_MPEG_7_1_B;
  kAudioChannelLayoutTag_CICP_9 = kAudioChannelLayoutTag_ITU_2_1;
  kAudioChannelLayoutTag_CICP_10 = kAudioChannelLayoutTag_ITU_2_2;
  kAudioChannelLayoutTag_CICP_11 = kAudioChannelLayoutTag_MPEG_6_1_A;
  kAudioChannelLayoutTag_CICP_12 = kAudioChannelLayoutTag_MPEG_7_1_C;
  kAudioChannelLayoutTag_CICP_13 = 13369368;
  kAudioChannelLayoutTag_CICP_14 = 13434888;
  kAudioChannelLayoutTag_CICP_15 = 13500428;
  kAudioChannelLayoutTag_CICP_16 = 13565962;
  kAudioChannelLayoutTag_CICP_17 = 13631500;
  kAudioChannelLayoutTag_CICP_18 = 13697038;
  kAudioChannelLayoutTag_CICP_19 = 13762572;
  kAudioChannelLayoutTag_CICP_20 = 13828110;
  kAudioChannelLayoutTag_Ogg_3_0 = kAudioChannelLayoutTag_AC3_3_0;
  kAudioChannelLayoutTag_Ogg_4_0 = kAudioChannelLayoutTag_WAVE_4_0_B;
  kAudioChannelLayoutTag_Ogg_5_0 = 13893637;
  kAudioChannelLayoutTag_Ogg_5_1 = 13959174;
  kAudioChannelLayoutTag_Ogg_6_1 = 14024711;
  kAudioChannelLayoutTag_Ogg_7_1 = 14090248;
  kAudioChannelLayoutTag_BeginReserved = -268435456;
  kAudioChannelLayoutTag_EndReserved = -65537;
  kAudioChannelLayoutTag_Unknown = -65536;
  kMPEG4Object_AAC_Main = 1;
  kMPEG4Object_AAC_LC = 2;
  kMPEG4Object_AAC_SSR = 3;
  kMPEG4Object_AAC_LTP = 4;
  kMPEG4Object_AAC_SBR = 5;
  kMPEG4Object_AAC_Scalable = 6;
  kMPEG4Object_TwinVQ = 7;
  kMPEG4Object_CELP = 8;
  kMPEG4Object_HVXC = 9;
  AVAudioSessionErrorCodeNone = 0;
  AVAudioSessionErrorCodeMediaServicesFailed = 1836282486;
  AVAudioSessionErrorCodeIsBusy = 560030580;
  AVAudioSessionErrorCodeIncompatibleCategory = 560161140;
  AVAudioSessionErrorCodeCannotInterruptOthers = 560557684;
  AVAudioSessionErrorCodeMissingEntitlement = 1701737535;
  AVAudioSessionErrorCodeSiriIsRecording = 1936290409;
  AVAudioSessionErrorCodeCannotStartPlaying = 561015905;
  AVAudioSessionErrorCodeCannotStartRecording = 561145187;
  AVAudioSessionErrorCodeBadParam = -50;
  AVAudioSessionErrorCodeInsufficientPriority = 561017449;
  AVAudioSessionErrorCodeResourceNotAvailable = 561145203;
  AVAudioSessionErrorCodeUnspecified = 2003329396;
  AVAudioSessionErrorCodeExpiredSession = 561210739;
  AVAudioSessionErrorCodeSessionNotActive = 1768841571;
  AVAudioSessionErrorInsufficientPriority = 561017449;

type
  PAudioValueRange = ^AudioValueRange;
  PAudioValueTranslation = ^AudioValueTranslation;
  PAudioBuffer = ^AudioBuffer;
  PAudioBufferList = ^AudioBufferList;
  PAudioStreamBasicDescription = ^AudioStreamBasicDescription;
  PAudioStreamPacketDescription = ^AudioStreamPacketDescription;
  PSMPTETime = ^SMPTETime;
  PAudioTimeStamp = ^AudioTimeStamp;
  PAudioClassDescription = ^AudioClassDescription;
  PAudioChannelDescription = ^AudioChannelDescription;
  PAudioChannelLayout = ^AudioChannelLayout;
  PAudioFormatListItem = ^AudioFormatListItem;

  AudioValueRange = record
    mMinimum: Float64;
    mMaximum: Float64;
  end;

  AudioValueTranslation = record
    mInputData: Pointer;
    mInputDataSize: UInt32;
    mOutputData: Pointer;
    mOutputDataSize: UInt32;
  end;

  AudioBuffer = record
    mNumberChannels: UInt32;
    mDataByteSize: UInt32;
    mData: Pointer;
  end;

  AudioBufferList = record
    mNumberBuffers: UInt32;
    mBuffers: array [0..0] of AudioBuffer;
  end;

  AudioSampleType = Float32;
  AudioUnitSampleType = Float32;
  AudioFormatID = UInt32;
  AudioFormatFlags = UInt32;

  AudioStreamBasicDescription = record
    mSampleRate: Float64;
    mFormatID: AudioFormatID;
    mFormatFlags: AudioFormatFlags;
    mBytesPerPacket: UInt32;
    mFramesPerPacket: UInt32;
    mBytesPerFrame: UInt32;
    mChannelsPerFrame: UInt32;
    mBitsPerChannel: UInt32;
    mReserved: UInt32;
  end;

  AudioStreamPacketDescription = record
    mStartOffset: SInt64;
    mVariableFramesInPacket: UInt32;
    mDataByteSize: UInt32;
  end;

  SMPTETimeType = NSInteger;
  SMPTETimeFlags = NSInteger;

  SMPTETime = record
    mSubframes: SInt16;
    mSubframeDivisor: SInt16;
    mCounter: UInt32;
    mType: SMPTETimeType;
    mFlags: SMPTETimeFlags;
    mHours: SInt16;
    mMinutes: SInt16;
    mSeconds: SInt16;
    mFrames: SInt16;
  end;

  AudioTimeStampFlags = NSInteger;

  AudioTimeStamp = record
    mSampleTime: Float64;
    mHostTime: UInt64;
    mRateScalar: Float64;
    mWordClockTime: UInt64;
    mSMPTETime: SMPTETime;
    mFlags: AudioTimeStampFlags;
    mReserved: UInt32;
  end;

  AudioClassDescription = record
    mType: OSType;
    mSubType: OSType;
    mManufacturer: OSType;
  end;

  AudioChannelLabel = UInt32;
  AudioChannelLayoutTag = UInt32;
  AudioChannelBitmap = NSInteger;
  AudioChannelFlags = NSInteger;
  AudioChannelCoordinateIndex = NSInteger;

  AudioChannelDescription = record
    mChannelLabel: AudioChannelLabel;
    mChannelFlags: AudioChannelFlags;
    mCoordinates: array [0..2] of Float32;
  end;

  AudioChannelLayout = record
    mChannelLayoutTag: AudioChannelLayoutTag;
    mChannelBitmap: AudioChannelBitmap;
    mNumberChannelDescriptions: UInt32;
    mChannelDescriptions: array [0..0] of AudioChannelDescription;
  end;

  AudioFormatListItem = record
    mASBD: AudioStreamBasicDescription;
    mChannelLayoutTag: AudioChannelLayoutTag;
  end;

  MPEG4ObjectID = NSInteger;
  AVAudioInteger = LongInt;
  AVAudioUInteger = LongWord;
  AudioSessionID = UInt32;
  AVAudioSessionErrorCode = NSInteger;

const
  libCoreAudioTypes = '/System/Library/Frameworks/CoreAudioTypes.framework/CoreAudioTypes';

implementation

uses
  System.SysUtils;

var
  CoreAudioTypesModule: THandle;

initialization
  CoreAudioTypesModule := LoadLibrary(libCoreAudioTypes);

finalization
  if CoreAudioTypesModule <> 0 then
    FreeLibrary(CoreAudioTypesModule);

end.