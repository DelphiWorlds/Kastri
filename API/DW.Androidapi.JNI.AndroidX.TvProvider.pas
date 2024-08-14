unit DW.Androidapi.JNI.AndroidX.TvProvider;

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

// Import of tvprovider-1.0.0.jar

interface

uses
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.Net, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Media;

type
  JBasePreviewProgram = interface;
  JBasePreviewProgram_Builder = interface;
  JBaseProgram = interface;
  JBaseProgram_Builder = interface;
  JChannel = interface;
  JChannel_Builder = interface;
  JChannelLogoUtils = interface;
  JPreviewProgram = interface;
  JPreviewProgram_Builder = interface;
  JTvContractCompat = interface;
  JTvContractCompat_Channels = interface;
  JTvContractCompat_PreviewPrograms = interface;
  JTvContractCompat_Programs = interface;
  JTvContractCompat_WatchNextPrograms = interface;

  JChannelClass = interface(JObjectClass)
    ['{25828E27-00FF-43CE-83F7-78CC12DED969}']
    {class} function _GetPROJECTION: TJavaObjectArray<JString>; cdecl;
    {class} function fromCursor(cursor: JCursor): JChannel; cdecl;
    {class} property PROJECTION: TJavaObjectArray<JString> read _GetPROJECTION;
  end;

  [JavaSignature('androidx/tvprovider/media/tv/Channel')]
  JChannel = interface(JObject)
    ['{AB3C2DF4-8861-4F29-B3BD-D76C60D96B21}']
    function equals(object_1: JObject): Boolean; cdecl;
    function getAppLinkColor: Integer; cdecl;
    function getAppLinkIconUri: Jnet_Uri; cdecl;
    function getAppLinkIntent: JIntent; cdecl;
    function getAppLinkIntentUri: Jnet_Uri; cdecl;
    function getAppLinkPosterArtUri: Jnet_Uri; cdecl;
    function getAppLinkText: JString; cdecl;
    function getConfigurationDisplayOrder: Integer; cdecl;
    function getDescription: JString; cdecl;
    function getDisplayName: JString; cdecl;
    function getDisplayNumber: JString; cdecl;
    function getId: Int64; cdecl;
    function getInputId: JString; cdecl;
    function getInternalProviderDataByteArray: TJavaArray<Byte>; cdecl;
    function getInternalProviderFlag1: JLong; cdecl;
    function getInternalProviderFlag2: JLong; cdecl;
    function getInternalProviderFlag3: JLong; cdecl;
    function getInternalProviderFlag4: JLong; cdecl;
    function getInternalProviderId: JString; cdecl;
    function getNetworkAffiliation: JString; cdecl;
    function getOriginalNetworkId: Integer; cdecl;
    function getPackageName: JString; cdecl;
    function getServiceId: Integer; cdecl;
    function getServiceType: JString; cdecl;
    function getSystemChannelKey: JString; cdecl;
    function getTransportStreamId: Integer; cdecl;
    function getType: JString; cdecl;
    function getVideoFormat: JString; cdecl;
    function hashCode: Integer; cdecl;
    function isBrowsable: Boolean; cdecl;
    function isLocked: Boolean; cdecl;
    function isSearchable: Boolean; cdecl;
    function isSystemApproved: Boolean; cdecl;
    function isTransient: Boolean; cdecl;
    function toContentValues(boolean: Boolean): JContentValues; overload; cdecl;
    function toContentValues: JContentValues; overload; cdecl;
    function toString: JString; cdecl;
  end;
  TJChannel = class(TJavaGenericImport<JChannelClass, JChannel>) end;

  JChannel_BuilderClass = interface(JObjectClass)
    ['{480EFF65-4870-43B4-B885-4CE8D3B8895B}']
    {class} function init(channel: JChannel): JChannel_Builder; overload; cdecl;
    {class} function init: JChannel_Builder; overload; cdecl;
  end;

  [JavaSignature('androidx/tvprovider/media/tv/Channel$Builder')]
  JChannel_Builder = interface(JObject)
    ['{DED4CE74-005A-4E01-8231-5D1DE87B8421}']
    function build: JChannel; cdecl;
    function setAppLinkColor(int: Integer): JChannel_Builder; cdecl;
    function setAppLinkIconUri(uri: Jnet_Uri): JChannel_Builder; cdecl;
    function setAppLinkIntent(intent: JIntent): JChannel_Builder; cdecl;
    function setAppLinkIntentUri(uri: Jnet_Uri): JChannel_Builder; cdecl;
    function setAppLinkPosterArtUri(uri: Jnet_Uri): JChannel_Builder; cdecl;
    function setAppLinkText(string_1: JString): JChannel_Builder; cdecl;
    function setBrowsable(boolean: Boolean): JChannel_Builder; cdecl;
    function setConfigurationDisplayOrder(int: Integer): JChannel_Builder; cdecl;
    function setDescription(string_1: JString): JChannel_Builder; cdecl;
    function setDisplayName(string_1: JString): JChannel_Builder; cdecl;
    function setDisplayNumber(string_1: JString): JChannel_Builder; cdecl;
    function setInputId(string_1: JString): JChannel_Builder; cdecl;
    function setInternalProviderData(bytes: TJavaArray<Byte>): JChannel_Builder; overload; cdecl;
    function setInternalProviderData(string_1: JString): JChannel_Builder; overload; cdecl;
    function setInternalProviderFlag1(long: Int64): JChannel_Builder; cdecl;
    function setInternalProviderFlag2(long: Int64): JChannel_Builder; cdecl;
    function setInternalProviderFlag3(long: Int64): JChannel_Builder; cdecl;
    function setInternalProviderFlag4(long: Int64): JChannel_Builder; cdecl;
    function setInternalProviderId(string_1: JString): JChannel_Builder; cdecl;
    function setLocked(boolean: Boolean): JChannel_Builder; cdecl;
    function setNetworkAffiliation(string_1: JString): JChannel_Builder; cdecl;
    function setOriginalNetworkId(int: Integer): JChannel_Builder; cdecl;
    function setSearchable(boolean: Boolean): JChannel_Builder; cdecl;
    function setServiceId(int: Integer): JChannel_Builder; cdecl;
    function setServiceType(string_1: JString): JChannel_Builder; cdecl;
    function setSystemApproved(boolean: Boolean): JChannel_Builder; cdecl;
    function setSystemChannelKey(string_1: JString): JChannel_Builder; cdecl;
    function setTransient(boolean: Boolean): JChannel_Builder; cdecl;
    function setTransportStreamId(int: Integer): JChannel_Builder; cdecl;
    function setType(string_1: JString): JChannel_Builder; cdecl;
    function setVideoFormat(string_1: JString): JChannel_Builder; cdecl;
  end;
  TJChannel_Builder = class(TJavaGenericImport<JChannel_BuilderClass, JChannel_Builder>) end;

  JBaseProgramClass = interface(JObjectClass)
    ['{40965D25-C1B0-4BA7-AFCC-662A561C6164}']
    {class} function _GetPROJECTION: TJavaObjectArray<JString>; cdecl;
    {class} property PROJECTION: TJavaObjectArray<JString> read _GetPROJECTION;
  end;

  [JavaSignature('androidx/tvprovider/media/tv/BaseProgram')]
  JBaseProgram = interface(JObject)
    ['{61F92EDE-BA2E-4E50-8D20-BCE73AB4E9AC}']
    function equals(object_1: JObject): Boolean; cdecl;
    function getAudioLanguages: TJavaObjectArray<JString>; cdecl;
    function getCanonicalGenres: TJavaObjectArray<JString>; cdecl;
    function getContentRatings: TJavaObjectArray<JTvContentRating>; cdecl;
    function getDescription: JString; cdecl;
    function getEpisodeNumber: JString; cdecl;
    function getEpisodeTitle: JString; cdecl;
    function getId: Int64; cdecl;
    function getInternalProviderDataByteArray: TJavaArray<Byte>; cdecl;
    function getInternalProviderFlag1: JLong; cdecl;
    function getInternalProviderFlag2: JLong; cdecl;
    function getInternalProviderFlag3: JLong; cdecl;
    function getInternalProviderFlag4: JLong; cdecl;
    function getLongDescription: JString; cdecl;
    function getPackageName: JString; cdecl;
    function getPosterArtUri: Jnet_Uri; cdecl;
    function getReviewRating: JString; cdecl;
    function getReviewRatingStyle: Integer; cdecl;
    function getSeasonNumber: JString; cdecl;
    function getSeasonTitle: JString; cdecl;
    function getThumbnailUri: Jnet_Uri; cdecl;
    function getTitle: JString; cdecl;
    function getVideoHeight: Integer; cdecl;
    function getVideoWidth: Integer; cdecl;
    function hashCode: Integer; cdecl;
    function isSearchable: Boolean; cdecl;
    function toContentValues: JContentValues; cdecl;
    function toString: JString; cdecl;
  end;
  TJBaseProgram = class(TJavaGenericImport<JBaseProgramClass, JBaseProgram>) end;

  JBaseProgram_BuilderClass = interface(JObjectClass)
    ['{6FFEBC2A-8022-4EDA-A26D-5352535FC64E}']
    {class} function init(baseprogram: JBaseProgram): JBaseProgram_Builder; overload; cdecl;
    {class} function init: JBaseProgram_Builder; overload; cdecl;
  end;

  [JavaSignature('androidx/tvprovider/media/tv/BaseProgram$Builder')]
  JBaseProgram_Builder = interface(JObject)
    ['{5F807D1E-F8F3-4B0E-9436-87422192BA93}']
    function setAudioLanguages(strings: TJavaObjectArray<JString>): JBaseProgram_Builder; cdecl;
    function setCanonicalGenres(strings: TJavaObjectArray<JString>): JBaseProgram_Builder; cdecl;
    function setContentRatings(tvcontentratings: TJavaObjectArray<JTvContentRating>): JBaseProgram_Builder; cdecl;
    function setDescription(string_1: JString): JBaseProgram_Builder; cdecl;
    function setEpisodeNumber(int: Integer): JBaseProgram_Builder; overload; cdecl;
    function setEpisodeNumber(string_1: JString; int: Integer): JBaseProgram_Builder; overload; cdecl;
    function setEpisodeTitle(string_1: JString): JBaseProgram_Builder; cdecl;
    function setId(long: Int64): JBaseProgram_Builder; cdecl;
    function setInternalProviderData(bytes: TJavaArray<Byte>): JBaseProgram_Builder; cdecl;
    function setInternalProviderFlag1(long: Int64): JBaseProgram_Builder; cdecl;
    function setInternalProviderFlag2(long: Int64): JBaseProgram_Builder; cdecl;
    function setInternalProviderFlag3(long: Int64): JBaseProgram_Builder; cdecl;
    function setInternalProviderFlag4(long: Int64): JBaseProgram_Builder; cdecl;
    function setLongDescription(string_1: JString): JBaseProgram_Builder; cdecl;
    function setPackageName(string_1: JString): JBaseProgram_Builder; cdecl;
    function setPosterArtUri(uri: Jnet_Uri): JBaseProgram_Builder; cdecl;
    function setReviewRating(string_1: JString): JBaseProgram_Builder; cdecl;
    function setReviewRatingStyle(int: Integer): JBaseProgram_Builder; cdecl;
    function setSearchable(boolean: Boolean): JBaseProgram_Builder; cdecl;
    function setSeasonNumber(int: Integer): JBaseProgram_Builder; overload; cdecl;
    function setSeasonNumber(string_1: JString; int: Integer): JBaseProgram_Builder; overload; cdecl;
    function setSeasonTitle(string_1: JString): JBaseProgram_Builder; cdecl;
    function setThumbnailUri(uri: Jnet_Uri): JBaseProgram_Builder; cdecl;
    function setTitle(string_1: JString): JBaseProgram_Builder; cdecl;
    function setVideoHeight(int: Integer): JBaseProgram_Builder; cdecl;
    function setVideoWidth(int: Integer): JBaseProgram_Builder; cdecl;
  end;
  TJBaseProgram_Builder = class(TJavaGenericImport<JBaseProgram_BuilderClass, JBaseProgram_Builder>) end;

  JBasePreviewProgramClass = interface(JBaseProgramClass)
    ['{0CBA0F08-70FE-4ED9-ACAA-2F7D7E563576}']
    {class} function _GetPROJECTION: TJavaObjectArray<JString>; cdecl;
    {class} property PROJECTION: TJavaObjectArray<JString> read _GetPROJECTION;
  end;

  [JavaSignature('androidx/tvprovider/media/tv/BasePreviewProgram')]
  JBasePreviewProgram = interface(JBaseProgram)
    ['{9F1ADFEC-A034-4F1F-9BBE-2C4EA30C7129}']
    function equals(object_1: JObject): Boolean; cdecl;
    function getAuthor: JString; cdecl;
    function getAvailability: Integer; cdecl;
    function getContentId: JString; cdecl;
    function getDurationMillis: Integer; cdecl;
    function getEndTimeUtcMillis: Int64; cdecl;
    function getGenre: JString; cdecl;
    function getIntent: JIntent; cdecl;
    function getIntentUri: Jnet_Uri; cdecl;
    function getInteractionCount: Int64; cdecl;
    function getInteractionType: Integer; cdecl;
    function getInternalProviderId: JString; cdecl;
    function getItemCount: Integer; cdecl;
    function getLastPlaybackPositionMillis: Integer; cdecl;
    function getLogoContentDescription: JString; cdecl;
    function getLogoUri: Jnet_Uri; cdecl;
    function getOfferPrice: JString; cdecl;
    function getPosterArtAspectRatio: Integer; cdecl;
    function getPreviewAudioUri: Jnet_Uri; cdecl;
    function getPreviewVideoUri: Jnet_Uri; cdecl;
    function getReleaseDate: JString; cdecl;
    function getStartTimeUtcMillis: Int64; cdecl;
    function getStartingPrice: JString; cdecl;
    function getThumbnailAspectRatio: Integer; cdecl;
    function getTvSeriesItemType: Integer; cdecl;
    function getType: Integer; cdecl;
    function isBrowsable: Boolean; cdecl;
    function isLive: Boolean; cdecl;
    function isTransient: Boolean; cdecl;
    function toContentValues(boolean: Boolean): JContentValues; overload; cdecl;
    function toContentValues: JContentValues; overload; cdecl;
  end;
  TJBasePreviewProgram = class(TJavaGenericImport<JBasePreviewProgramClass, JBasePreviewProgram>) end;

  JBasePreviewProgram_BuilderClass = interface(JBaseProgram_BuilderClass)
    ['{D44DF9E2-58A4-4BBC-B0DF-09018C34BFCA}']
    {class} function init: JBasePreviewProgram_Builder; overload; cdecl;
    {class} function init(basepreviewprogram: JBasePreviewProgram): JBasePreviewProgram_Builder; overload; cdecl;
  end;

  [JavaSignature('androidx/tvprovider/media/tv/BasePreviewProgram$Builder')]
  JBasePreviewProgram_Builder = interface(JBaseProgram_Builder)
    ['{2B8C04C5-90DE-470F-844A-06126463B4AC}']
    function setAuthor(string_1: JString): JBasePreviewProgram_Builder; cdecl;
    function setAvailability(int: Integer): JBasePreviewProgram_Builder; cdecl;
    function setBrowsable(boolean: Boolean): JBasePreviewProgram_Builder; cdecl;
    function setContentId(string_1: JString): JBasePreviewProgram_Builder; cdecl;
    function setDurationMillis(int: Integer): JBasePreviewProgram_Builder; cdecl;
    function setEndTimeUtcMillis(long: Int64): JBasePreviewProgram_Builder; cdecl;
    function setGenre(string_1: JString): JBasePreviewProgram_Builder; cdecl;
    function setIntent(intent: JIntent): JBasePreviewProgram_Builder; cdecl;
    function setIntentUri(uri: Jnet_Uri): JBasePreviewProgram_Builder; cdecl;
    function setInteractionCount(long: Int64): JBasePreviewProgram_Builder; cdecl;
    function setInteractionType(int: Integer): JBasePreviewProgram_Builder; cdecl;
    function setInternalProviderId(string_1: JString): JBasePreviewProgram_Builder; cdecl;
    function setItemCount(int: Integer): JBasePreviewProgram_Builder; cdecl;
    function setLastPlaybackPositionMillis(int: Integer): JBasePreviewProgram_Builder; cdecl;
    function setLive(boolean: Boolean): JBasePreviewProgram_Builder; cdecl;
    function setLogoContentDescription(string_1: JString): JBasePreviewProgram_Builder; cdecl;
    function setLogoUri(uri: Jnet_Uri): JBasePreviewProgram_Builder; cdecl;
    function setOfferPrice(string_1: JString): JBasePreviewProgram_Builder; cdecl;
    function setPosterArtAspectRatio(int: Integer): JBasePreviewProgram_Builder; cdecl;
    function setPreviewAudioUri(uri: Jnet_Uri): JBasePreviewProgram_Builder; cdecl;
    function setPreviewVideoUri(uri: Jnet_Uri): JBasePreviewProgram_Builder; cdecl;
    function setReleaseDate(date: JDate): JBasePreviewProgram_Builder; overload; cdecl;
    function setReleaseDate(string_1: JString): JBasePreviewProgram_Builder; overload; cdecl;
    function setStartTimeUtcMillis(long: Int64): JBasePreviewProgram_Builder; cdecl;
    function setStartingPrice(string_1: JString): JBasePreviewProgram_Builder; cdecl;
    function setThumbnailAspectRatio(int: Integer): JBasePreviewProgram_Builder; cdecl;
    function setTransient(boolean: Boolean): JBasePreviewProgram_Builder; cdecl;
    function setTvSeriesItemType(int: Integer): JBasePreviewProgram_Builder; cdecl;
    function setType(int: Integer): JBasePreviewProgram_Builder; cdecl;
  end;
  TJBasePreviewProgram_Builder = class(TJavaGenericImport<JBasePreviewProgram_BuilderClass, JBasePreviewProgram_Builder>) end;

  JPreviewProgramClass = interface(JBasePreviewProgramClass)
    ['{40AD3CBB-E06D-4535-9200-137141099767}']
    {class} function _GetPROJECTION: TJavaObjectArray<JString>; cdecl;
    {class} function fromCursor(cursor: JCursor): JPreviewProgram; cdecl;
    {class} property PROJECTION: TJavaObjectArray<JString> read _GetPROJECTION;
  end;

  [JavaSignature('androidx/tvprovider/media/tv/PreviewProgram')]
  JPreviewProgram = interface(JBasePreviewProgram)
    ['{96AE1764-B989-4E50-93D9-F357F3B1078D}']
    function equals(object_1: JObject): Boolean; cdecl;
    function getChannelId: Int64; cdecl;
    function getWeight: Integer; cdecl;
    function hasAnyUpdatedValues(previewprogram: JPreviewProgram): Boolean; cdecl;
    function toContentValues(boolean: Boolean): JContentValues; overload; cdecl;
    function toContentValues: JContentValues; overload; cdecl;
    function toString: JString; cdecl;
  end;
  TJPreviewProgram = class(TJavaGenericImport<JPreviewProgramClass, JPreviewProgram>) end;

  JPreviewProgram_BuilderClass = interface(JBasePreviewProgram_BuilderClass)
    ['{27B00DC1-256D-4133-A650-DA4E4D3DB0B3}']
    {class} function init(previewprogram: JPreviewProgram): JPreviewProgram_Builder; overload; cdecl;
    {class} function init: JPreviewProgram_Builder; overload; cdecl;
  end;

  [JavaSignature('androidx/tvprovider/media/tv/PreviewProgram$Builder')]
  JPreviewProgram_Builder = interface(JBasePreviewProgram_Builder)
    ['{5622E2D1-91AD-410E-A806-1FEA831C09C3}']
    function build: JPreviewProgram; cdecl;
    function setChannelId(long: Int64): JPreviewProgram_Builder; cdecl;
    function setWeight(int: Integer): JPreviewProgram_Builder; cdecl;
  end;
  TJPreviewProgram_Builder = class(TJavaGenericImport<JPreviewProgram_BuilderClass, JPreviewProgram_Builder>) end;

  JChannelLogoUtilsClass = interface(JObjectClass)
    ['{6EA0AB43-E9F3-469F-84E7-E49BA99B361A}']
    {class} function loadChannelLogo(context: JContext; channelId: Int64): JBitmap; cdecl;
    {class} function storeChannelLogo(context: JContext; channelId: Int64; bitmap: JBitmap): Boolean; overload; cdecl;
    {class} function storeChannelLogo(context: JContext; channelId: Int64; uri: Jnet_Uri): Boolean; overload; cdecl;
  end;

  [JavaSignature('androidx/tvprovider/media/tv/ChannelLogoUtils')]
  JChannelLogoUtils = interface(JObject)
    ['{D280D454-EE4B-49A3-8EB9-3CC4D6AB084F}']
  end;
  TJChannelLogoUtils = class(TJavaGenericImport<JChannelLogoUtilsClass, JChannelLogoUtils>) end;

  JTvContractCompatClass = interface(JObjectClass)
    ['{8120BEBA-B944-4E98-97CD-9B8B40E80475}']
    {class} function _GetACTION_CHANNEL_BROWSABLE_REQUESTED: JString; cdecl;
    {class} function _GetACTION_INITIALIZE_PROGRAMS: JString; cdecl;
    {class} function _GetACTION_PREVIEW_PROGRAM_ADDED_TO_WATCH_NEXT: JString; cdecl;
    {class} function _GetACTION_PREVIEW_PROGRAM_BROWSABLE_DISABLED: JString; cdecl;
    {class} function _GetACTION_REQUEST_CHANNEL_BROWSABLE: JString; cdecl;
    {class} function _GetACTION_WATCH_NEXT_PROGRAM_BROWSABLE_DISABLED: JString; cdecl;
    {class} function _GetAUTHORITY: JString; cdecl;
    {class} function _GetEXTRA_CHANNEL_ID: JString; cdecl;
    {class} function _GetEXTRA_COLUMN_NAME: JString; cdecl;
    {class} function _GetEXTRA_DATA_TYPE: JString; cdecl;
    {class} function _GetEXTRA_DEFAULT_VALUE: JString; cdecl;
    {class} function _GetEXTRA_EXISTING_COLUMN_NAMES: JString; cdecl;
    {class} function _GetEXTRA_PACKAGE_NAME: JString; cdecl;
    {class} function _GetEXTRA_PREVIEW_PROGRAM_ID: JString; cdecl;
    {class} function _GetEXTRA_WATCH_NEXT_PROGRAM_ID: JString; cdecl;
    {class} function _GetMETHOD_ADD_COLUMN: JString; cdecl;
    {class} function _GetMETHOD_GET_COLUMNS: JString; cdecl;
    {class} function _GetPARAM_BROWSABLE_ONLY: JString; cdecl;
    {class} function _GetPARAM_CANONICAL_GENRE: JString; cdecl;
    {class} function _GetPARAM_CHANNEL: JString; cdecl;
    {class} function _GetPARAM_END_TIME: JString; cdecl;
    {class} function _GetPARAM_INPUT: JString; cdecl;
    {class} function _GetPARAM_START_TIME: JString; cdecl;
    {class} function _GetPERMISSION_READ_TV_LISTINGS: JString; cdecl;
    {class} function buildChannelLogoUri(long: Int64): Jnet_Uri; overload; cdecl;
    {class} function buildChannelLogoUri(uri: Jnet_Uri): Jnet_Uri; overload; cdecl;
    {class} function buildChannelUri(long: Int64): Jnet_Uri; cdecl;
    {class} function buildChannelUriForPassthroughInput(string_1: JString): Jnet_Uri; cdecl;
    {class} function buildChannelsUriForInput(string_1: JString): Jnet_Uri; cdecl;
    {class} function buildInputId(componentname: JComponentName): JString; cdecl;
    {class} function buildPreviewProgramUri(long: Int64): Jnet_Uri; cdecl;
    {class} function buildPreviewProgramsUriForChannel(long: Int64): Jnet_Uri; overload; cdecl;
    {class} function buildPreviewProgramsUriForChannel(uri: Jnet_Uri): Jnet_Uri; overload; cdecl;
    {class} function buildProgramUri(long: Int64): Jnet_Uri; cdecl;
    {class} function buildProgramsUriForChannel(uri: Jnet_Uri; long: Int64; long_1: Int64): Jnet_Uri; overload; cdecl;
    {class} function buildProgramsUriForChannel(long: Int64): Jnet_Uri; overload; cdecl;
    {class} function buildProgramsUriForChannel(uri: Jnet_Uri): Jnet_Uri; overload; cdecl;
    {class} function buildProgramsUriForChannel(long: Int64; long_1: Int64; long_2: Int64): Jnet_Uri; overload; cdecl;
    {class} function buildRecordedProgramUri(long: Int64): Jnet_Uri; cdecl;
    {class} function buildWatchNextProgramUri(long: Int64): Jnet_Uri; cdecl;
    {class} function isChannelUri(uri: Jnet_Uri): Boolean; cdecl;
    {class} function isChannelUriForPassthroughInput(uri: Jnet_Uri): Boolean; cdecl;
    {class} function isChannelUriForTunerInput(uri: Jnet_Uri): Boolean; cdecl;
    {class} function isProgramUri(uri: Jnet_Uri): Boolean; cdecl;
    {class} function isRecordedProgramUri(uri: Jnet_Uri): Boolean; cdecl;
    {class} procedure requestChannelBrowsable(context: JContext; long: Int64); cdecl;
    {class} property ACTION_CHANNEL_BROWSABLE_REQUESTED: JString read _GetACTION_CHANNEL_BROWSABLE_REQUESTED;
    {class} property ACTION_INITIALIZE_PROGRAMS: JString read _GetACTION_INITIALIZE_PROGRAMS;
    {class} property ACTION_PREVIEW_PROGRAM_ADDED_TO_WATCH_NEXT: JString read _GetACTION_PREVIEW_PROGRAM_ADDED_TO_WATCH_NEXT;
    {class} property ACTION_PREVIEW_PROGRAM_BROWSABLE_DISABLED: JString read _GetACTION_PREVIEW_PROGRAM_BROWSABLE_DISABLED;
    {class} property ACTION_REQUEST_CHANNEL_BROWSABLE: JString read _GetACTION_REQUEST_CHANNEL_BROWSABLE;
    {class} property ACTION_WATCH_NEXT_PROGRAM_BROWSABLE_DISABLED: JString read _GetACTION_WATCH_NEXT_PROGRAM_BROWSABLE_DISABLED;
    {class} property AUTHORITY: JString read _GetAUTHORITY;
    {class} property EXTRA_CHANNEL_ID: JString read _GetEXTRA_CHANNEL_ID;
    {class} property EXTRA_COLUMN_NAME: JString read _GetEXTRA_COLUMN_NAME;
    {class} property EXTRA_DATA_TYPE: JString read _GetEXTRA_DATA_TYPE;
    {class} property EXTRA_DEFAULT_VALUE: JString read _GetEXTRA_DEFAULT_VALUE;
    {class} property EXTRA_EXISTING_COLUMN_NAMES: JString read _GetEXTRA_EXISTING_COLUMN_NAMES;
    {class} property EXTRA_PACKAGE_NAME: JString read _GetEXTRA_PACKAGE_NAME;
    {class} property EXTRA_PREVIEW_PROGRAM_ID: JString read _GetEXTRA_PREVIEW_PROGRAM_ID;
    {class} property EXTRA_WATCH_NEXT_PROGRAM_ID: JString read _GetEXTRA_WATCH_NEXT_PROGRAM_ID;
    {class} property METHOD_ADD_COLUMN: JString read _GetMETHOD_ADD_COLUMN;
    {class} property METHOD_GET_COLUMNS: JString read _GetMETHOD_GET_COLUMNS;
    {class} property PARAM_BROWSABLE_ONLY: JString read _GetPARAM_BROWSABLE_ONLY;
    {class} property PARAM_CANONICAL_GENRE: JString read _GetPARAM_CANONICAL_GENRE;
    {class} property PARAM_CHANNEL: JString read _GetPARAM_CHANNEL;
    {class} property PARAM_END_TIME: JString read _GetPARAM_END_TIME;
    {class} property PARAM_INPUT: JString read _GetPARAM_INPUT;
    {class} property PARAM_START_TIME: JString read _GetPARAM_START_TIME;
    {class} property PERMISSION_READ_TV_LISTINGS: JString read _GetPERMISSION_READ_TV_LISTINGS;
  end;

  [JavaSignature('androidx/tvprovider/media/tv/TvContractCompat')]
  JTvContractCompat = interface(JObject)
    ['{7B2A6118-5851-4F82-B0DE-B87597D7097B}']
  end;
  TJTvContractCompat = class(TJavaGenericImport<JTvContractCompatClass, JTvContractCompat>) end;

  JTvContractCompat_ChannelsClass = interface(JObjectClass)
    ['{0B4DC7D4-757C-4E6C-9CED-1452320B0AC1}']
    {class} function _GetCOLUMN_APP_LINK_COLOR: JString; cdecl;
    {class} function _GetCOLUMN_APP_LINK_ICON_URI: JString; cdecl;
    {class} function _GetCOLUMN_APP_LINK_INTENT_URI: JString; cdecl;
    {class} function _GetCOLUMN_APP_LINK_POSTER_ART_URI: JString; cdecl;
    {class} function _GetCOLUMN_APP_LINK_TEXT: JString; cdecl;
    {class} function _GetCOLUMN_BROWSABLE: JString; cdecl;
    {class} function _GetCOLUMN_CONFIGURATION_DISPLAY_ORDER: JString; cdecl;
    {class} function _GetCOLUMN_DESCRIPTION: JString; cdecl;
    {class} function _GetCOLUMN_DISPLAY_NAME: JString; cdecl;
    {class} function _GetCOLUMN_DISPLAY_NUMBER: JString; cdecl;
    {class} function _GetCOLUMN_INPUT_ID: JString; cdecl;
    {class} function _GetCOLUMN_INTERNAL_PROVIDER_DATA: JString; cdecl;
    {class} function _GetCOLUMN_INTERNAL_PROVIDER_FLAG1: JString; cdecl;
    {class} function _GetCOLUMN_INTERNAL_PROVIDER_FLAG2: JString; cdecl;
    {class} function _GetCOLUMN_INTERNAL_PROVIDER_FLAG3: JString; cdecl;
    {class} function _GetCOLUMN_INTERNAL_PROVIDER_FLAG4: JString; cdecl;
    {class} function _GetCOLUMN_INTERNAL_PROVIDER_ID: JString; cdecl;
    {class} function _GetCOLUMN_LOCKED: JString; cdecl;
    {class} function _GetCOLUMN_NETWORK_AFFILIATION: JString; cdecl;
    {class} function _GetCOLUMN_ORIGINAL_NETWORK_ID: JString; cdecl;
    {class} function _GetCOLUMN_SEARCHABLE: JString; cdecl;
    {class} function _GetCOLUMN_SERVICE_ID: JString; cdecl;
    {class} function _GetCOLUMN_SERVICE_TYPE: JString; cdecl;
    {class} function _GetCOLUMN_SYSTEM_APPROVED: JString; cdecl;
    {class} function _GetCOLUMN_SYSTEM_CHANNEL_KEY: JString; cdecl;
    {class} function _GetCOLUMN_TRANSIENT: JString; cdecl;
    {class} function _GetCOLUMN_TRANSPORT_STREAM_ID: JString; cdecl;
    {class} function _GetCOLUMN_TYPE: JString; cdecl;
    {class} function _GetCOLUMN_VERSION_NUMBER: JString; cdecl;
    {class} function _GetCOLUMN_VIDEO_FORMAT: JString; cdecl;
    {class} function _GetCONTENT_ITEM_TYPE: JString; cdecl;
    {class} function _GetCONTENT_TYPE: JString; cdecl;
    {class} function _GetCONTENT_URI: Jnet_Uri; cdecl;
    {class} function _GetSERVICE_TYPE_AUDIO: JString; cdecl;
    {class} function _GetSERVICE_TYPE_AUDIO_VIDEO: JString; cdecl;
    {class} function _GetSERVICE_TYPE_OTHER: JString; cdecl;
    {class} function _GetTYPE_1SEG: JString; cdecl;
    {class} function _GetTYPE_ATSC_C: JString; cdecl;
    {class} function _GetTYPE_ATSC_M_H: JString; cdecl;
    {class} function _GetTYPE_ATSC_T: JString; cdecl;
    {class} function _GetTYPE_CMMB: JString; cdecl;
    {class} function _GetTYPE_DTMB: JString; cdecl;
    {class} function _GetTYPE_DVB_C: JString; cdecl;
    {class} function _GetTYPE_DVB_C2: JString; cdecl;
    {class} function _GetTYPE_DVB_H: JString; cdecl;
    {class} function _GetTYPE_DVB_S: JString; cdecl;
    {class} function _GetTYPE_DVB_S2: JString; cdecl;
    {class} function _GetTYPE_DVB_SH: JString; cdecl;
    {class} function _GetTYPE_DVB_T: JString; cdecl;
    {class} function _GetTYPE_DVB_T2: JString; cdecl;
    {class} function _GetTYPE_ISDB_C: JString; cdecl;
    {class} function _GetTYPE_ISDB_S: JString; cdecl;
    {class} function _GetTYPE_ISDB_T: JString; cdecl;
    {class} function _GetTYPE_ISDB_TB: JString; cdecl;
    {class} function _GetTYPE_NTSC: JString; cdecl;
    {class} function _GetTYPE_OTHER: JString; cdecl;
    {class} function _GetTYPE_PAL: JString; cdecl;
    {class} function _GetTYPE_PREVIEW: JString; cdecl;
    {class} function _GetTYPE_SECAM: JString; cdecl;
    {class} function _GetTYPE_S_DMB: JString; cdecl;
    {class} function _GetTYPE_T_DMB: JString; cdecl;
    {class} function _GetVIDEO_FORMAT_1080I: JString; cdecl;
    {class} function _GetVIDEO_FORMAT_1080P: JString; cdecl;
    {class} function _GetVIDEO_FORMAT_2160P: JString; cdecl;
    {class} function _GetVIDEO_FORMAT_240P: JString; cdecl;
    {class} function _GetVIDEO_FORMAT_360P: JString; cdecl;
    {class} function _GetVIDEO_FORMAT_4320P: JString; cdecl;
    {class} function _GetVIDEO_FORMAT_480I: JString; cdecl;
    {class} function _GetVIDEO_FORMAT_480P: JString; cdecl;
    {class} function _GetVIDEO_FORMAT_576I: JString; cdecl;
    {class} function _GetVIDEO_FORMAT_576P: JString; cdecl;
    {class} function _GetVIDEO_FORMAT_720P: JString; cdecl;
    {class} function _GetVIDEO_RESOLUTION_ED: JString; cdecl;
    {class} function _GetVIDEO_RESOLUTION_FHD: JString; cdecl;
    {class} function _GetVIDEO_RESOLUTION_HD: JString; cdecl;
    {class} function _GetVIDEO_RESOLUTION_SD: JString; cdecl;
    {class} function _GetVIDEO_RESOLUTION_UHD: JString; cdecl;
    {class} function getVideoResolution(string_1: JString): JString; cdecl;
    {class} property COLUMN_APP_LINK_COLOR: JString read _GetCOLUMN_APP_LINK_COLOR;
    {class} property COLUMN_APP_LINK_ICON_URI: JString read _GetCOLUMN_APP_LINK_ICON_URI;
    {class} property COLUMN_APP_LINK_INTENT_URI: JString read _GetCOLUMN_APP_LINK_INTENT_URI;
    {class} property COLUMN_APP_LINK_POSTER_ART_URI: JString read _GetCOLUMN_APP_LINK_POSTER_ART_URI;
    {class} property COLUMN_APP_LINK_TEXT: JString read _GetCOLUMN_APP_LINK_TEXT;
    {class} property COLUMN_BROWSABLE: JString read _GetCOLUMN_BROWSABLE;
    {class} property COLUMN_CONFIGURATION_DISPLAY_ORDER: JString read _GetCOLUMN_CONFIGURATION_DISPLAY_ORDER;
    {class} property COLUMN_DESCRIPTION: JString read _GetCOLUMN_DESCRIPTION;
    {class} property COLUMN_DISPLAY_NAME: JString read _GetCOLUMN_DISPLAY_NAME;
    {class} property COLUMN_DISPLAY_NUMBER: JString read _GetCOLUMN_DISPLAY_NUMBER;
    {class} property COLUMN_INPUT_ID: JString read _GetCOLUMN_INPUT_ID;
    {class} property COLUMN_INTERNAL_PROVIDER_DATA: JString read _GetCOLUMN_INTERNAL_PROVIDER_DATA;
    {class} property COLUMN_INTERNAL_PROVIDER_FLAG1: JString read _GetCOLUMN_INTERNAL_PROVIDER_FLAG1;
    {class} property COLUMN_INTERNAL_PROVIDER_FLAG2: JString read _GetCOLUMN_INTERNAL_PROVIDER_FLAG2;
    {class} property COLUMN_INTERNAL_PROVIDER_FLAG3: JString read _GetCOLUMN_INTERNAL_PROVIDER_FLAG3;
    {class} property COLUMN_INTERNAL_PROVIDER_FLAG4: JString read _GetCOLUMN_INTERNAL_PROVIDER_FLAG4;
    {class} property COLUMN_INTERNAL_PROVIDER_ID: JString read _GetCOLUMN_INTERNAL_PROVIDER_ID;
    {class} property COLUMN_LOCKED: JString read _GetCOLUMN_LOCKED;
    {class} property COLUMN_NETWORK_AFFILIATION: JString read _GetCOLUMN_NETWORK_AFFILIATION;
    {class} property COLUMN_ORIGINAL_NETWORK_ID: JString read _GetCOLUMN_ORIGINAL_NETWORK_ID;
    {class} property COLUMN_SEARCHABLE: JString read _GetCOLUMN_SEARCHABLE;
    {class} property COLUMN_SERVICE_ID: JString read _GetCOLUMN_SERVICE_ID;
    {class} property COLUMN_SERVICE_TYPE: JString read _GetCOLUMN_SERVICE_TYPE;
    {class} property COLUMN_SYSTEM_APPROVED: JString read _GetCOLUMN_SYSTEM_APPROVED;
    {class} property COLUMN_SYSTEM_CHANNEL_KEY: JString read _GetCOLUMN_SYSTEM_CHANNEL_KEY;
    {class} property COLUMN_TRANSIENT: JString read _GetCOLUMN_TRANSIENT;
    {class} property COLUMN_TRANSPORT_STREAM_ID: JString read _GetCOLUMN_TRANSPORT_STREAM_ID;
    {class} property COLUMN_TYPE: JString read _GetCOLUMN_TYPE;
    {class} property COLUMN_VERSION_NUMBER: JString read _GetCOLUMN_VERSION_NUMBER;
    {class} property COLUMN_VIDEO_FORMAT: JString read _GetCOLUMN_VIDEO_FORMAT;
    {class} property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
    {class} property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
    {class} property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
    {class} property SERVICE_TYPE_AUDIO: JString read _GetSERVICE_TYPE_AUDIO;
    {class} property SERVICE_TYPE_AUDIO_VIDEO: JString read _GetSERVICE_TYPE_AUDIO_VIDEO;
    {class} property SERVICE_TYPE_OTHER: JString read _GetSERVICE_TYPE_OTHER;
    {class} property TYPE_1SEG: JString read _GetTYPE_1SEG;
    {class} property TYPE_ATSC_C: JString read _GetTYPE_ATSC_C;
    {class} property TYPE_ATSC_M_H: JString read _GetTYPE_ATSC_M_H;
    {class} property TYPE_ATSC_T: JString read _GetTYPE_ATSC_T;
    {class} property TYPE_CMMB: JString read _GetTYPE_CMMB;
    {class} property TYPE_DTMB: JString read _GetTYPE_DTMB;
    {class} property TYPE_DVB_C: JString read _GetTYPE_DVB_C;
    {class} property TYPE_DVB_C2: JString read _GetTYPE_DVB_C2;
    {class} property TYPE_DVB_H: JString read _GetTYPE_DVB_H;
    {class} property TYPE_DVB_S: JString read _GetTYPE_DVB_S;
    {class} property TYPE_DVB_S2: JString read _GetTYPE_DVB_S2;
    {class} property TYPE_DVB_SH: JString read _GetTYPE_DVB_SH;
    {class} property TYPE_DVB_T: JString read _GetTYPE_DVB_T;
    {class} property TYPE_DVB_T2: JString read _GetTYPE_DVB_T2;
    {class} property TYPE_ISDB_C: JString read _GetTYPE_ISDB_C;
    {class} property TYPE_ISDB_S: JString read _GetTYPE_ISDB_S;
    {class} property TYPE_ISDB_T: JString read _GetTYPE_ISDB_T;
    {class} property TYPE_ISDB_TB: JString read _GetTYPE_ISDB_TB;
    {class} property TYPE_NTSC: JString read _GetTYPE_NTSC;
    {class} property TYPE_OTHER: JString read _GetTYPE_OTHER;
    {class} property TYPE_PAL: JString read _GetTYPE_PAL;
    {class} property TYPE_PREVIEW: JString read _GetTYPE_PREVIEW;
    {class} property TYPE_SECAM: JString read _GetTYPE_SECAM;
    {class} property TYPE_S_DMB: JString read _GetTYPE_S_DMB;
    {class} property TYPE_T_DMB: JString read _GetTYPE_T_DMB;
    {class} property VIDEO_FORMAT_1080I: JString read _GetVIDEO_FORMAT_1080I;
    {class} property VIDEO_FORMAT_1080P: JString read _GetVIDEO_FORMAT_1080P;
    {class} property VIDEO_FORMAT_2160P: JString read _GetVIDEO_FORMAT_2160P;
    {class} property VIDEO_FORMAT_240P: JString read _GetVIDEO_FORMAT_240P;
    {class} property VIDEO_FORMAT_360P: JString read _GetVIDEO_FORMAT_360P;
    {class} property VIDEO_FORMAT_4320P: JString read _GetVIDEO_FORMAT_4320P;
    {class} property VIDEO_FORMAT_480I: JString read _GetVIDEO_FORMAT_480I;
    {class} property VIDEO_FORMAT_480P: JString read _GetVIDEO_FORMAT_480P;
    {class} property VIDEO_FORMAT_576I: JString read _GetVIDEO_FORMAT_576I;
    {class} property VIDEO_FORMAT_576P: JString read _GetVIDEO_FORMAT_576P;
    {class} property VIDEO_FORMAT_720P: JString read _GetVIDEO_FORMAT_720P;
    {class} property VIDEO_RESOLUTION_ED: JString read _GetVIDEO_RESOLUTION_ED;
    {class} property VIDEO_RESOLUTION_FHD: JString read _GetVIDEO_RESOLUTION_FHD;
    {class} property VIDEO_RESOLUTION_HD: JString read _GetVIDEO_RESOLUTION_HD;
    {class} property VIDEO_RESOLUTION_SD: JString read _GetVIDEO_RESOLUTION_SD;
    {class} property VIDEO_RESOLUTION_UHD: JString read _GetVIDEO_RESOLUTION_UHD;
  end;

  [JavaSignature('androidx/tvprovider/media/tv/TvContractCompat$Channels')]
  JTvContractCompat_Channels = interface(JObject)
    ['{F6DF08B6-C2F1-4374-8BBD-6B6913192A39}']
  end;
  TJTvContractCompat_Channels = class(TJavaGenericImport<JTvContractCompat_ChannelsClass, JTvContractCompat_Channels>) end;

  JTvContractCompat_PreviewProgramsClass = interface(JObjectClass)
    ['{3736F46D-3464-4518-8C9E-77553B810C84}']
    {class} function _GetCOLUMN_CHANNEL_ID: JString; cdecl;
    {class} function _GetCOLUMN_WEIGHT: JString; cdecl;
    {class} function _GetCONTENT_ITEM_TYPE: JString; cdecl;
    {class} function _GetCONTENT_TYPE: JString; cdecl;
    {class} function _GetCONTENT_URI: Jnet_Uri; cdecl;
    {class} function _GetTYPE_ALBUM: Integer; cdecl;
    {class} function _GetTYPE_ARTIST: Integer; cdecl;
    {class} function _GetTYPE_CHANNEL: Integer; cdecl;
    {class} function _GetTYPE_CLIP: Integer; cdecl;
    {class} function _GetTYPE_EVENT: Integer; cdecl;
    {class} function _GetTYPE_MOVIE: Integer; cdecl;
    {class} function _GetTYPE_PLAYLIST: Integer; cdecl;
    {class} function _GetTYPE_STATION: Integer; cdecl;
    {class} function _GetTYPE_TRACK: Integer; cdecl;
    {class} function _GetTYPE_TV_EPISODE: Integer; cdecl;
    {class} function _GetTYPE_TV_SEASON: Integer; cdecl;
    {class} function _GetTYPE_TV_SERIES: Integer; cdecl;
    {class} property COLUMN_CHANNEL_ID: JString read _GetCOLUMN_CHANNEL_ID;
    {class} property COLUMN_WEIGHT: JString read _GetCOLUMN_WEIGHT;
    {class} property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
    {class} property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
    {class} property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
    {class} property TYPE_ALBUM: Integer read _GetTYPE_ALBUM;
    {class} property TYPE_ARTIST: Integer read _GetTYPE_ARTIST;
    {class} property TYPE_CHANNEL: Integer read _GetTYPE_CHANNEL;
    {class} property TYPE_CLIP: Integer read _GetTYPE_CLIP;
    {class} property TYPE_EVENT: Integer read _GetTYPE_EVENT;
    {class} property TYPE_MOVIE: Integer read _GetTYPE_MOVIE;
    {class} property TYPE_PLAYLIST: Integer read _GetTYPE_PLAYLIST;
    {class} property TYPE_STATION: Integer read _GetTYPE_STATION;
    {class} property TYPE_TRACK: Integer read _GetTYPE_TRACK;
    {class} property TYPE_TV_EPISODE: Integer read _GetTYPE_TV_EPISODE;
    {class} property TYPE_TV_SEASON: Integer read _GetTYPE_TV_SEASON;
    {class} property TYPE_TV_SERIES: Integer read _GetTYPE_TV_SERIES;
  end;

  [JavaSignature('androidx/tvprovider/media/tv/TvContractCompat$PreviewPrograms')]
  JTvContractCompat_PreviewPrograms = interface(JObject)
    ['{319AEBE5-7A72-4DD5-8B59-33D0A7CD7899}']
  end;
  TJTvContractCompat_PreviewPrograms = class(TJavaGenericImport<JTvContractCompat_PreviewProgramsClass, JTvContractCompat_PreviewPrograms>) end;

  JTvContractCompat_ProgramsClass = interface(JObjectClass)
    ['{BA553894-5E50-477C-B2E1-FEA45C4F0BB4}']
    {class} function _GetCOLUMN_BROADCAST_GENRE: JString; cdecl;
    {class} function _GetCOLUMN_CHANNEL_ID: JString; cdecl;
    {class} function _GetCOLUMN_END_TIME_UTC_MILLIS: JString; cdecl;
    {class} function _GetCOLUMN_EPISODE_NUMBER: JString; cdecl;
    {class} function _GetCOLUMN_RECORDING_PROHIBITED: JString; cdecl;
    {class} function _GetCOLUMN_SEASON_NUMBER: JString; cdecl;
    {class} function _GetCOLUMN_START_TIME_UTC_MILLIS: JString; cdecl;
    {class} function _GetCONTENT_ITEM_TYPE: JString; cdecl;
    {class} function _GetCONTENT_TYPE: JString; cdecl;
    {class} function _GetCONTENT_URI: Jnet_Uri; cdecl;
    {class} property COLUMN_BROADCAST_GENRE: JString read _GetCOLUMN_BROADCAST_GENRE;
    {class} property COLUMN_CHANNEL_ID: JString read _GetCOLUMN_CHANNEL_ID;
    {class} property COLUMN_END_TIME_UTC_MILLIS: JString read _GetCOLUMN_END_TIME_UTC_MILLIS;
    {class} property COLUMN_EPISODE_NUMBER: JString read _GetCOLUMN_EPISODE_NUMBER;
    {class} property COLUMN_RECORDING_PROHIBITED: JString read _GetCOLUMN_RECORDING_PROHIBITED;
    {class} property COLUMN_SEASON_NUMBER: JString read _GetCOLUMN_SEASON_NUMBER;
    {class} property COLUMN_START_TIME_UTC_MILLIS: JString read _GetCOLUMN_START_TIME_UTC_MILLIS;
    {class} property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
    {class} property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
    {class} property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
  end;

  [JavaSignature('androidx/tvprovider/media/tv/TvContractCompat$Programs')]
  JTvContractCompat_Programs = interface(JObject)
    ['{7BF4187C-0AFF-4E05-8E25-F87B5A1F2152}']
  end;
  TJTvContractCompat_Programs = class(TJavaGenericImport<JTvContractCompat_ProgramsClass, JTvContractCompat_Programs>) end;

  JTvContractCompat_WatchNextProgramsClass = interface(JObjectClass)
    ['{DB3BEEE7-0443-4FC5-8EB0-95A0564A5739}']
    {class} function _GetCOLUMN_LAST_ENGAGEMENT_TIME_UTC_MILLIS: JString; cdecl;
    {class} function _GetCOLUMN_WATCH_NEXT_TYPE: JString; cdecl;
    {class} function _GetCONTENT_ITEM_TYPE: JString; cdecl;
    {class} function _GetCONTENT_TYPE: JString; cdecl;
    {class} function _GetCONTENT_URI: Jnet_Uri; cdecl;
    {class} function _GetWATCH_NEXT_TYPE_CONTINUE: Integer; cdecl;
    {class} function _GetWATCH_NEXT_TYPE_NEW: Integer; cdecl;
    {class} function _GetWATCH_NEXT_TYPE_NEXT: Integer; cdecl;
    {class} function _GetWATCH_NEXT_TYPE_WATCHLIST: Integer; cdecl;
    {class} property COLUMN_LAST_ENGAGEMENT_TIME_UTC_MILLIS: JString read _GetCOLUMN_LAST_ENGAGEMENT_TIME_UTC_MILLIS;
    {class} property COLUMN_WATCH_NEXT_TYPE: JString read _GetCOLUMN_WATCH_NEXT_TYPE;
    {class} property CONTENT_ITEM_TYPE: JString read _GetCONTENT_ITEM_TYPE;
    {class} property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
    {class} property CONTENT_URI: Jnet_Uri read _GetCONTENT_URI;
    {class} property WATCH_NEXT_TYPE_CONTINUE: Integer read _GetWATCH_NEXT_TYPE_CONTINUE;
    {class} property WATCH_NEXT_TYPE_NEW: Integer read _GetWATCH_NEXT_TYPE_NEW;
    {class} property WATCH_NEXT_TYPE_NEXT: Integer read _GetWATCH_NEXT_TYPE_NEXT;
    {class} property WATCH_NEXT_TYPE_WATCHLIST: Integer read _GetWATCH_NEXT_TYPE_WATCHLIST;
  end;

  [JavaSignature('androidx/tvprovider/media/tv/TvContractCompat$WatchNextPrograms')]
  JTvContractCompat_WatchNextPrograms = interface(JObject)
    ['{4397E5C8-B805-40AC-A0C3-C2A3DCBDABF5}']
  end;
  TJTvContractCompat_WatchNextPrograms = class(TJavaGenericImport<JTvContractCompat_WatchNextProgramsClass, JTvContractCompat_WatchNextPrograms>) end;

implementation

end.
