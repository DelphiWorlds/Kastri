unit DW.Macapi.CoreSpotlight;

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
  // macOSs
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.CocoaTypes, Macapi.Foundation,
  // DW
  DW.Macapi.Foundation, DW.Macapi.UniformTypeIdentifiers;

const
  CoreSpotlightAPIVersion = 40;
  CSIndexErrorCodeUnknownError = -1;
  CSIndexErrorCodeIndexUnavailableError = -1000;
  CSIndexErrorCodeInvalidItemError = -1001;
  CSIndexErrorCodeInvalidClientStateError = -1002;
  CSIndexErrorCodeRemoteConnectionError = -1003;
  CSIndexErrorCodeQuotaExceeded = -1004;
  CSIndexErrorCodeIndexingUnsupported = -1005;
  CSSearchQueryErrorCodeUnknown = -2000;
  CSSearchQueryErrorCodeIndexUnreachable = -2001;
  CSSearchQueryErrorCodeInvalidQuery = -2002;
  CSSearchQueryErrorCodeCancelled = -2003;
  CSSearchQuerySourceOptionDefault = 0;
  CSSearchQuerySourceOptionAllowMail = 1;
  CSSuggestionKindNone = 0;
  CSSuggestionKindCustom = 1;
  CSSuggestionKindDefault = 2;

type
  CSPerson = interface;
  CSSearchableItemAttributeSet = interface;
  CSLocalizedString = interface;
  CSCustomAttributeKey = interface;
  CSImportExtension = interface;
  CSSearchableItem = interface;
  CSSearchableIndex = interface;
  CSSearchableIndexDelegate = interface;
  CSIndexExtensionRequestHandler = interface;
  CSSearchQueryContext = interface;
  CSSearchQuery = interface;
  CSSuggestion = interface;
  CSUserQueryContext = interface;
  CSUserQuery = interface;

  CSIndexErrorCode = NSInteger;
  CSSearchQueryErrorCode = NSInteger;
  CSSearchQuerySourceOptions = NSInteger;
  CSSuggestionKind = NSInteger;
  TCSSearchableIndexBlockMethod1 = procedure(error: NSError) of object;
  TCSSearchableIndexBlockMethod2 = procedure(clientState: NSData; error: NSError) of object;
  TCSSearchableIndexBlockMethod3 = procedure(param1: NSData; param2: NSError) of object;
  TCSSearchableIndexDelegateBlockMethod1 = procedure of object;
  TCSSearchQueryBlockMethod1 = procedure(param1: NSArray) of object;
  TCSSearchQueryBlockMethod2 = procedure of object;
  TCSSearchQueryBlockMethod3 = procedure(param1: NSError) of object;
  TCSUserQueryBlockMethod1 = procedure(param1: NSArray) of object;
  TCSUserQueryBlockMethod2 = procedure of object;

  CSPersonClass = interface(NSObjectClass)
    ['{84E90CB3-8C43-4E51-A7A0-C7AE4327FF09}']
  end;

  CSPerson = interface(NSObject)
    ['{41D7EE1B-FE56-42B0-89DF-7816AEDD28D3}']
    function contactIdentifier: NSString; cdecl;
    function displayName: NSString; cdecl;
    function handleIdentifier: NSString; cdecl;
    function handles: NSArray; cdecl;
    function initWithDisplayName(displayName: NSString; handles: NSArray; handleIdentifier: NSString): Pointer; cdecl;
    procedure setContactIdentifier(contactIdentifier: NSString); cdecl;
  end;
  TCSPerson = class(TOCGenericImport<CSPersonClass, CSPerson>) end;

  CSSearchableItemAttributeSetClass = interface(NSObjectClass)
    ['{55192BC4-2E6A-4B5F-A62B-385E3B1DB119}']
  end;

  CSSearchableItemAttributeSet = interface(NSObject)
    ['{D31640B8-DD0F-4C96-90E7-312C3135C0C6}']
    function accountHandles: NSArray; cdecl;
    function accountIdentifier: NSString; cdecl;
    function acquisitionMake: NSString; cdecl;
    function acquisitionModel: NSString; cdecl;
    function actionIdentifiers: NSArray; cdecl;
    function addedDate: NSDate; cdecl;
    function additionalRecipients: NSArray; cdecl;
    function album: NSString; cdecl;
    function allDay: NSNumber; cdecl;
    function alternateNames: NSArray; cdecl;
    function altitude: NSNumber; cdecl;
    function aperture: NSNumber; cdecl;
    function artist: NSString; cdecl;
    function audiences: NSArray; cdecl;
    function audioBitRate: NSNumber; cdecl;
    function audioChannelCount: NSNumber; cdecl;
    function audioEncodingApplication: NSString; cdecl;
    function audioSampleRate: NSNumber; cdecl;
    function audioTrackNumber: NSNumber; cdecl;
    function authorAddresses: NSArray; cdecl;
    function authorEmailAddresses: NSArray; cdecl;
    function authorNames: NSArray; cdecl;
    function authors: NSArray; cdecl;
    function bitsPerSample: NSNumber; cdecl;
    function cameraOwner: NSString; cdecl;
    function city: NSString; cdecl;
    function codecs: NSArray; cdecl;
    function colorSpace: NSString; cdecl;
    function comment: NSString; cdecl;
    function completionDate: NSDate; cdecl;
    function composer: NSString; cdecl;
    function contactKeywords: NSArray; cdecl;
    function containerDisplayName: NSString; cdecl;
    function containerIdentifier: NSString; cdecl;
    function containerOrder: NSNumber; cdecl;
    function containerTitle: NSString; cdecl;
    function contentCreationDate: NSDate; cdecl;
    function contentDescription: NSString; cdecl;
    function contentModificationDate: NSDate; cdecl;
    function contentRating: NSNumber; cdecl;
    function contentSources: NSArray; cdecl;
    function contentType: NSString; cdecl;
    function contentTypeTree: NSArray; cdecl;
    function contentURL: NSURL; cdecl;
    function contributors: NSArray; cdecl;
    function copyright: NSString; cdecl;
    function country: NSString; cdecl;
    function coverage: NSArray; cdecl;
    function creator: NSString; cdecl;
    function darkThumbnailURL: NSURL; cdecl;
    function deliveryType: NSNumber; cdecl;
    function director: NSString; cdecl;
    function displayName: NSString; cdecl;
    function domainIdentifier: NSString; cdecl;
    function downloadedDate: NSDate; cdecl;
    function dueDate: NSDate; cdecl;
    function duration: NSNumber; cdecl;
    function editors: NSArray; cdecl;
    function emailAddresses: NSArray; cdecl;
    function emailHeaders: NSDictionary; cdecl;
    function encodingApplications: NSArray; cdecl;
    function endDate: NSDate; cdecl;
    function EXIFGPSVersion: NSString; cdecl;
    function EXIFVersion: NSString; cdecl;
    function exposureMode: NSNumber; cdecl;
    function exposureProgram: NSString; cdecl;
    function exposureTime: NSNumber; cdecl;
    function exposureTimeString: NSString; cdecl;
    function fileSize: NSNumber; cdecl;
    function fNumber: NSNumber; cdecl;
    function focalLength: NSNumber; cdecl;
    function fontNames: NSArray; cdecl;
    function fullyFormattedAddress: NSString; cdecl;
    function genre: NSString; cdecl;
    function GPSAreaInformation: NSString; cdecl;
    function GPSDateStamp: NSDate; cdecl;
    function GPSDestBearing: NSNumber; cdecl;
    function GPSDestDistance: NSNumber; cdecl;
    function GPSDestLatitude: NSNumber; cdecl;
    function GPSDestLongitude: NSNumber; cdecl;
    function GPSDifferental: NSNumber; cdecl;
    function GPSDOP: NSNumber; cdecl;
    function GPSMapDatum: NSString; cdecl;
    function GPSMeasureMode: NSString; cdecl;
    function GPSProcessingMethod: NSString; cdecl;
    function GPSStatus: NSString; cdecl;
    function GPSTrack: NSNumber; cdecl;
    function hasAlphaChannel: NSNumber; cdecl;
    function headline: NSString; cdecl;
    function hiddenAdditionalRecipients: NSArray; cdecl;
    function HTMLContentData: NSData; cdecl;
    function identifier: NSString; cdecl;
    function imageDirection: NSNumber; cdecl;
    function importantDates: NSArray; cdecl;
    function information: NSString; cdecl;
    function initWithContentType(contentType: UTType): Pointer; cdecl;
    function initWithItemContentType(itemContentType: NSString): Pointer; cdecl; // API_DEPRECATED("Use initWithContentType instead", macos(10.13, API_TO_BE_DEPRECATED), ios(9.0, API_TO_BE_DEPRECATED))
    function instantMessageAddresses: NSArray; cdecl;
    function instructions: NSString; cdecl;
    function isFlashOn: NSNumber; cdecl;
    function isFocalLength35mm: NSNumber; cdecl;
    function isGeneralMIDISequence: NSNumber; cdecl;
    function isLikelyJunk: NSNumber; cdecl;
    function isLocal: NSNumber; cdecl;
    function ISOSpeed: NSNumber; cdecl;
    function isRedEyeOn: NSNumber; cdecl;
    function isStreamable: NSNumber; cdecl;
    function isUserCreated: NSNumber; cdecl;
    function isUserCurated: NSNumber; cdecl;
    function isUserOwned: NSNumber; cdecl;
    function keySignature: NSString; cdecl;
    function keywords: NSArray; cdecl;
    function kind: NSString; cdecl;
    function languages: NSArray; cdecl;
    function lastUsedDate: NSDate; cdecl;
    function latitude: NSNumber; cdecl;
    function layerNames: NSArray; cdecl;
    function lensModel: NSString; cdecl;
    function longitude: NSNumber; cdecl;
    function lyricist: NSString; cdecl;
    function mailboxIdentifiers: NSArray; cdecl;
    function maxAperture: NSNumber; cdecl;
    function mediaTypes: NSArray; cdecl;
    function metadataModificationDate: NSDate; cdecl;
    function meteringMode: NSString; cdecl;
    function musicalGenre: NSString; cdecl;
    function musicalInstrumentCategory: NSString; cdecl;
    function musicalInstrumentName: NSString; cdecl;
    function namedLocation: NSString; cdecl;
    function organizations: NSArray; cdecl;
    function orientation: NSNumber; cdecl;
    function originalFormat: NSString; cdecl;
    function originalSource: NSString; cdecl;
    function pageCount: NSNumber; cdecl;
    function pageHeight: NSNumber; cdecl;
    function pageWidth: NSNumber; cdecl;
    function participants: NSArray; cdecl;
    function path: NSString; cdecl;
    function performers: NSArray; cdecl;
    function phoneNumbers: NSArray; cdecl;
    function pixelCount: NSNumber; cdecl;
    function pixelHeight: NSNumber; cdecl;
    function pixelWidth: NSNumber; cdecl;
    function playCount: NSNumber; cdecl;
    function postalCode: NSString; cdecl;
    function primaryRecipients: NSArray; cdecl;
    function producer: NSString; cdecl;
    function profileName: NSString; cdecl;
    function projects: NSArray; cdecl;
    function providerDataTypeIdentifiers: NSArray; cdecl;
    function providerFileTypeIdentifiers: NSArray; cdecl;
    function providerInPlaceFileTypeIdentifiers: NSArray; cdecl;
    function publishers: NSArray; cdecl;
    function rankingHint: NSNumber; cdecl;
    function rating: NSNumber; cdecl;
    function ratingDescription: NSString; cdecl;
    function recipientAddresses: NSArray; cdecl;
    function recipientEmailAddresses: NSArray; cdecl;
    function recipientNames: NSArray; cdecl;
    function recordingDate: NSDate; cdecl;
    function relatedUniqueIdentifier: NSString; cdecl;
    function resolutionHeightDPI: NSNumber; cdecl;
    function resolutionWidthDPI: NSNumber; cdecl;
    function rights: NSString; cdecl;
    function role: NSString; cdecl;
    function securityMethod: NSString; cdecl;
    procedure setAccountHandles(accountHandles: NSArray); cdecl;
    procedure setAccountIdentifier(accountIdentifier: NSString); cdecl;
    procedure setAcquisitionMake(acquisitionMake: NSString); cdecl;
    procedure setAcquisitionModel(acquisitionModel: NSString); cdecl;
    procedure setActionIdentifiers(actionIdentifiers: NSArray); cdecl;
    procedure setAddedDate(addedDate: NSDate); cdecl;
    procedure setAdditionalRecipients(additionalRecipients: NSArray); cdecl;
    procedure setAlbum(album: NSString); cdecl;
    procedure setAllDay(allDay: NSNumber); cdecl;
    procedure setAlternateNames(alternateNames: NSArray); cdecl;
    procedure setAltitude(altitude: NSNumber); cdecl;
    procedure setAperture(aperture: NSNumber); cdecl;
    procedure setArtist(artist: NSString); cdecl;
    procedure setAudiences(audiences: NSArray); cdecl;
    procedure setAudioBitRate(audioBitRate: NSNumber); cdecl;
    procedure setAudioChannelCount(audioChannelCount: NSNumber); cdecl;
    procedure setAudioEncodingApplication(audioEncodingApplication: NSString); cdecl;
    procedure setAudioSampleRate(audioSampleRate: NSNumber); cdecl;
    procedure setAudioTrackNumber(audioTrackNumber: NSNumber); cdecl;
    procedure setAuthorAddresses(authorAddresses: NSArray); cdecl;
    procedure setAuthorEmailAddresses(authorEmailAddresses: NSArray); cdecl;
    procedure setAuthorNames(authorNames: NSArray); cdecl;
    procedure setAuthors(authors: NSArray); cdecl;
    procedure setBitsPerSample(bitsPerSample: NSNumber); cdecl;
    procedure setCameraOwner(cameraOwner: NSString); cdecl;
    procedure setCity(city: NSString); cdecl;
    procedure setCodecs(codecs: NSArray); cdecl;
    procedure setColorSpace(colorSpace: NSString); cdecl;
    procedure setComment(comment: NSString); cdecl;
    procedure setCompletionDate(completionDate: NSDate); cdecl;
    procedure setComposer(composer: NSString); cdecl;
    procedure setContactKeywords(contactKeywords: NSArray); cdecl;
    procedure setContainerDisplayName(containerDisplayName: NSString); cdecl;
    procedure setContainerIdentifier(containerIdentifier: NSString); cdecl;
    procedure setContainerOrder(containerOrder: NSNumber); cdecl;
    procedure setContainerTitle(containerTitle: NSString); cdecl;
    procedure setContentCreationDate(contentCreationDate: NSDate); cdecl;
    procedure setContentDescription(contentDescription: NSString); cdecl;
    procedure setContentModificationDate(contentModificationDate: NSDate); cdecl;
    procedure setContentRating(contentRating: NSNumber); cdecl;
    procedure setContentSources(contentSources: NSArray); cdecl;
    procedure setContentType(contentType: NSString); cdecl;
    procedure setContentTypeTree(contentTypeTree: NSArray); cdecl;
    procedure setContentURL(contentURL: NSURL); cdecl;
    procedure setContributors(contributors: NSArray); cdecl;
    procedure setCopyright(copyright: NSString); cdecl;
    procedure setCountry(country: NSString); cdecl;
    procedure setCoverage(coverage: NSArray); cdecl;
    procedure setCreator(creator: NSString); cdecl;
    procedure setDarkThumbnailURL(darkThumbnailURL: NSURL); cdecl;
    procedure setDeliveryType(deliveryType: NSNumber); cdecl;
    procedure setDirector(director: NSString); cdecl;
    procedure setDisplayName(displayName: NSString); cdecl;
    procedure setDomainIdentifier(domainIdentifier: NSString); cdecl;
    procedure setDownloadedDate(downloadedDate: NSDate); cdecl;
    procedure setDueDate(dueDate: NSDate); cdecl;
    procedure setDuration(duration: NSNumber); cdecl;
    procedure setEditors(editors: NSArray); cdecl;
    procedure setEmailAddresses(emailAddresses: NSArray); cdecl;
    procedure setEmailHeaders(emailHeaders: NSDictionary); cdecl;
    procedure setEncodingApplications(encodingApplications: NSArray); cdecl;
    procedure setEndDate(endDate: NSDate); cdecl;
    procedure setEXIFGPSVersion(EXIFGPSVersion: NSString); cdecl;
    procedure setEXIFVersion(EXIFVersion: NSString); cdecl;
    procedure setExposureMode(exposureMode: NSNumber); cdecl;
    procedure setExposureProgram(exposureProgram: NSString); cdecl;
    procedure setExposureTime(exposureTime: NSNumber); cdecl;
    procedure setExposureTimeString(exposureTimeString: NSString); cdecl;
    procedure setFileSize(fileSize: NSNumber); cdecl;
    procedure setFlashOn(flashOn: NSNumber); cdecl;
    procedure setFNumber(fNumber: NSNumber); cdecl;
    procedure setFocalLength(focalLength: NSNumber); cdecl;
    procedure setFocalLength35mm(focalLength35mm: NSNumber); cdecl;
    procedure setFontNames(fontNames: NSArray); cdecl;
    procedure setFullyFormattedAddress(fullyFormattedAddress: NSString); cdecl;
    procedure setGeneralMIDISequence(generalMIDISequence: NSNumber); cdecl;
    procedure setGenre(genre: NSString); cdecl;
    procedure setGPSAreaInformation(GPSAreaInformation: NSString); cdecl;
    procedure setGPSDateStamp(GPSDateStamp: NSDate); cdecl;
    procedure setGPSDestBearing(GPSDestBearing: NSNumber); cdecl;
    procedure setGPSDestDistance(GPSDestDistance: NSNumber); cdecl;
    procedure setGPSDestLatitude(GPSDestLatitude: NSNumber); cdecl;
    procedure setGPSDestLongitude(GPSDestLongitude: NSNumber); cdecl;
    procedure setGPSDifferental(GPSDifferental: NSNumber); cdecl;
    procedure setGPSDOP(GPSDOP: NSNumber); cdecl;
    procedure setGPSMapDatum(GPSMapDatum: NSString); cdecl;
    procedure setGPSMeasureMode(GPSMeasureMode: NSString); cdecl;
    procedure setGPSProcessingMethod(GPSProcessingMethod: NSString); cdecl;
    procedure setGPSStatus(GPSStatus: NSString); cdecl;
    procedure setGPSTrack(GPSTrack: NSNumber); cdecl;
    procedure setHasAlphaChannel(hasAlphaChannel: NSNumber); cdecl;
    procedure setHeadline(headline: NSString); cdecl;
    procedure setHiddenAdditionalRecipients(hiddenAdditionalRecipients: NSArray); cdecl;
    procedure setHTMLContentData(HTMLContentData: NSData); cdecl;
    procedure setIdentifier(identifier: NSString); cdecl;
    procedure setImageDirection(imageDirection: NSNumber); cdecl;
    procedure setImportantDates(importantDates: NSArray); cdecl;
    procedure setInformation(information: NSString); cdecl;
    procedure setInstantMessageAddresses(instantMessageAddresses: NSArray); cdecl;
    procedure setInstructions(instructions: NSString); cdecl;
    procedure setISOSpeed(ISOSpeed: NSNumber); cdecl;
    procedure setKeySignature(keySignature: NSString); cdecl;
    procedure setKeywords(keywords: NSArray); cdecl;
    procedure setKind(kind: NSString); cdecl;
    procedure setLanguages(languages: NSArray); cdecl;
    procedure setLastUsedDate(lastUsedDate: NSDate); cdecl;
    procedure setLatitude(latitude: NSNumber); cdecl;
    procedure setLayerNames(layerNames: NSArray); cdecl;
    procedure setLensModel(lensModel: NSString); cdecl;
    procedure setLikelyJunk(likelyJunk: NSNumber); cdecl;
    procedure setLocal(local: NSNumber); cdecl;
    procedure setLongitude(longitude: NSNumber); cdecl;
    procedure setLyricist(lyricist: NSString); cdecl;
    procedure setMailboxIdentifiers(mailboxIdentifiers: NSArray); cdecl;
    procedure setMaxAperture(maxAperture: NSNumber); cdecl;
    procedure setMediaTypes(mediaTypes: NSArray); cdecl;
    procedure setMetadataModificationDate(metadataModificationDate: NSDate); cdecl;
    procedure setMeteringMode(meteringMode: NSString); cdecl;
    procedure setMusicalGenre(musicalGenre: NSString); cdecl;
    procedure setMusicalInstrumentCategory(musicalInstrumentCategory: NSString); cdecl;
    procedure setMusicalInstrumentName(musicalInstrumentName: NSString); cdecl;
    procedure setNamedLocation(namedLocation: NSString); cdecl;
    procedure setOrganizations(organizations: NSArray); cdecl;
    procedure setOrientation(orientation: NSNumber); cdecl;
    procedure setOriginalFormat(originalFormat: NSString); cdecl;
    procedure setOriginalSource(originalSource: NSString); cdecl;
    procedure setPageCount(pageCount: NSNumber); cdecl;
    procedure setPageHeight(pageHeight: NSNumber); cdecl;
    procedure setPageWidth(pageWidth: NSNumber); cdecl;
    procedure setParticipants(participants: NSArray); cdecl;
    procedure setPath(path: NSString); cdecl;
    procedure setPerformers(performers: NSArray); cdecl;
    procedure setPhoneNumbers(phoneNumbers: NSArray); cdecl;
    procedure setPixelCount(pixelCount: NSNumber); cdecl;
    procedure setPixelHeight(pixelHeight: NSNumber); cdecl;
    procedure setPixelWidth(pixelWidth: NSNumber); cdecl;
    procedure setPlayCount(playCount: NSNumber); cdecl;
    procedure setPostalCode(postalCode: NSString); cdecl;
    procedure setPrimaryRecipients(primaryRecipients: NSArray); cdecl;
    procedure setProducer(producer: NSString); cdecl;
    procedure setProfileName(profileName: NSString); cdecl;
    procedure setProjects(projects: NSArray); cdecl;
    procedure setProviderDataTypeIdentifiers(providerDataTypeIdentifiers: NSArray); cdecl;
    procedure setProviderFileTypeIdentifiers(providerFileTypeIdentifiers: NSArray); cdecl;
    procedure setProviderInPlaceFileTypeIdentifiers(providerInPlaceFileTypeIdentifiers: NSArray); cdecl;
    procedure setPublishers(publishers: NSArray); cdecl;
    procedure setRankingHint(rankingHint: NSNumber); cdecl;
    procedure setRating(rating: NSNumber); cdecl;
    procedure setRatingDescription(ratingDescription: NSString); cdecl;
    procedure setRecipientAddresses(recipientAddresses: NSArray); cdecl;
    procedure setRecipientEmailAddresses(recipientEmailAddresses: NSArray); cdecl;
    procedure setRecipientNames(recipientNames: NSArray); cdecl;
    procedure setRecordingDate(recordingDate: NSDate); cdecl;
    procedure setRedEyeOn(redEyeOn: NSNumber); cdecl;
    procedure setRelatedUniqueIdentifier(relatedUniqueIdentifier: NSString); cdecl;
    procedure setResolutionHeightDPI(resolutionHeightDPI: NSNumber); cdecl;
    procedure setResolutionWidthDPI(resolutionWidthDPI: NSNumber); cdecl;
    procedure setRights(rights: NSString); cdecl;
    procedure setRole(role: NSString); cdecl;
    procedure setSecurityMethod(securityMethod: NSString); cdecl;
    procedure setSharedItemContentType(sharedItemContentType: UTType); cdecl;
    procedure setSpeed(speed: NSNumber); cdecl;
    procedure setStartDate(startDate: NSDate); cdecl;
    procedure setStateOrProvince(stateOrProvince: NSString); cdecl;
    procedure setStreamable(streamable: NSNumber); cdecl;
    procedure setSubject(subject: NSString); cdecl;
    procedure setSubThoroughfare(subThoroughfare: NSString); cdecl;
    procedure setSupportsNavigation(supportsNavigation: NSNumber); cdecl;
    procedure setSupportsPhoneCall(supportsPhoneCall: NSNumber); cdecl;
    procedure setTempo(tempo: NSNumber); cdecl;
    procedure setTextContent(textContent: NSString); cdecl;
    procedure setTheme(theme: NSString); cdecl;
    procedure setThoroughfare(thoroughfare: NSString); cdecl;
    procedure setThumbnailData(thumbnailData: NSData); cdecl;
    procedure setThumbnailURL(thumbnailURL: NSURL); cdecl;
    procedure setTimeSignature(timeSignature: NSString); cdecl;
    procedure setTimestamp(timestamp: NSDate); cdecl;
    procedure setTitle(title: NSString); cdecl;
    procedure setTotalBitRate(totalBitRate: NSNumber); cdecl;
    procedure setURL(URL: NSURL); cdecl;
    procedure setUserCreated(userCreated: NSNumber); cdecl;
    procedure setUserCurated(userCurated: NSNumber); cdecl;
    procedure setUserOwned(userOwned: NSNumber); cdecl;
    procedure setValue(value: Pointer; forCustomKey: CSCustomAttributeKey); cdecl;
    procedure setVersion(version: NSString); cdecl;
    procedure setVideoBitRate(videoBitRate: NSNumber); cdecl;
    procedure setWeakRelatedUniqueIdentifier(weakRelatedUniqueIdentifier: NSString); cdecl;
    procedure setWhiteBalance(whiteBalance: NSNumber); cdecl;
    function sharedItemContentType: UTType; cdecl;
    function speed: NSNumber; cdecl;
    function startDate: NSDate; cdecl;
    function stateOrProvince: NSString; cdecl;
    function subject: NSString; cdecl;
    function subThoroughfare: NSString; cdecl;
    function supportsNavigation: NSNumber; cdecl;
    function supportsPhoneCall: NSNumber; cdecl;
    function tempo: NSNumber; cdecl;
    function textContent: NSString; cdecl;
    function theme: NSString; cdecl;
    function thoroughfare: NSString; cdecl;
    function thumbnailData: NSData; cdecl;
    function thumbnailURL: NSURL; cdecl;
    function timeSignature: NSString; cdecl;
    function timestamp: NSDate; cdecl;
    function title: NSString; cdecl;
    function totalBitRate: NSNumber; cdecl;
    function URL: NSURL; cdecl;
    function valueForCustomKey(key: CSCustomAttributeKey): Pointer; cdecl;
    function version: NSString; cdecl;
    function videoBitRate: NSNumber; cdecl;
    function weakRelatedUniqueIdentifier: NSString; cdecl;
    function whiteBalance: NSNumber; cdecl;
  end;
  TCSSearchableItemAttributeSet = class(TOCGenericImport<CSSearchableItemAttributeSetClass, CSSearchableItemAttributeSet>) end;

  CSLocalizedStringClass = interface(NSStringClass)
    ['{BEB61BA9-4FAD-42A4-A8BB-C3079C5E13E1}']
  end;

  CSLocalizedString = interface(NSString)
    ['{1B6A9802-B672-4DC2-AFE6-F998B499412A}']
    function initWithLocalizedStrings(localizedStrings: NSDictionary): Pointer; cdecl;
    function localizedString: NSString; cdecl;
  end;
  TCSLocalizedString = class(TOCGenericImport<CSLocalizedStringClass, CSLocalizedString>) end;

  CSCustomAttributeKeyClass = interface(NSObjectClass)
    ['{11464F3E-3566-48E3-A6F6-2AFC42A4603F}']
  end;

  CSCustomAttributeKey = interface(NSObject)
    ['{EDC76B58-BA2D-44BF-8990-88100D39B8AE}']
    function initWithKeyName(keyName: NSString; searchable: Boolean; searchableByDefault: Boolean; unique: Boolean;
      multiValued: Boolean): Pointer; overload; cdecl;
    function initWithKeyName(keyName: NSString): Pointer; overload; cdecl;
    function isMultiValued: Boolean; cdecl;
    function isSearchable: Boolean; cdecl;
    function isSearchableByDefault: Boolean; cdecl;
    function isUnique: Boolean; cdecl;
    function keyName: NSString; cdecl;
  end;
  TCSCustomAttributeKey = class(TOCGenericImport<CSCustomAttributeKeyClass, CSCustomAttributeKey>) end;

  CSImportExtensionClass = interface(NSObjectClass)
    ['{25966AF9-1163-41B8-92E4-E3772EDE4452}']
  end;

  CSImportExtension = interface(NSObject)
    ['{E2ED65F3-9F91-41C3-9E34-15AE799EB5A5}']
    function updateAttributes(attributes: CSSearchableItemAttributeSet; forFileAtURL: NSURL; error: PPointer): Boolean; cdecl;
  end;
  TCSImportExtension = class(TOCGenericImport<CSImportExtensionClass, CSImportExtension>) end;

  CSSearchableItemClass = interface(NSObjectClass)
    ['{7DCE681A-F2DD-41CC-BB84-8930CA0D1275}']
  end;

  CSSearchableItem = interface(NSObject)
    ['{F681C2BC-D0AA-423E-B1B3-88D594A5AFAE}']
    function attributeSet: CSSearchableItemAttributeSet; cdecl;
    function compareByRank(other: CSSearchableItem): NSComparisonResult; cdecl;
    function domainIdentifier: NSString; cdecl;
    function expirationDate: NSDate; cdecl;
    function initWithUniqueIdentifier(uniqueIdentifier: NSString; domainIdentifier: NSString;
      attributeSet: CSSearchableItemAttributeSet): Pointer; cdecl;
    procedure setAttributeSet(attributeSet: CSSearchableItemAttributeSet); cdecl;
    procedure setDomainIdentifier(domainIdentifier: NSString); cdecl;
    procedure setExpirationDate(expirationDate: NSDate); cdecl;
    procedure setUniqueIdentifier(uniqueIdentifier: NSString); cdecl;
    function uniqueIdentifier: NSString; cdecl;
  end;
  TCSSearchableItem = class(TOCGenericImport<CSSearchableItemClass, CSSearchableItem>) end;

  CSSearchableIndexClass = interface(NSObjectClass)
    ['{153B0A8E-D61D-4197-8CDA-BFAAF1B95A99}']
    {class} function defaultSearchableIndex: Pointer; cdecl;
    {class} function isIndexingAvailable: Boolean; cdecl;
  end;

  CSSearchableIndex = interface(NSObject)
    ['{5C7520E0-2A9D-4B3A-8C2C-D4AF3786F009}']
    procedure beginIndexBatch; cdecl;
    procedure deleteAllSearchableItemsWithCompletionHandler(completionHandler: TCSSearchableIndexBlockMethod1); cdecl;
    procedure deleteSearchableItemsWithDomainIdentifiers(domainIdentifiers: NSArray; completionHandler: TCSSearchableIndexBlockMethod1); cdecl;
    procedure deleteSearchableItemsWithIdentifiers(identifiers: NSArray; completionHandler: TCSSearchableIndexBlockMethod1); cdecl;
    procedure endIndexBatchWithClientState(clientState: NSData; completionHandler: TCSSearchableIndexBlockMethod1); cdecl;
    procedure fetchDataForBundleIdentifier(bundleIdentifier: NSString; itemIdentifier: NSString; contentType: UTType;
      completionHandler: TCSSearchableIndexBlockMethod3); cdecl;
    procedure fetchLastClientStateWithCompletionHandler(completionHandler: TCSSearchableIndexBlockMethod2); cdecl;
    function indexDelegate: Pointer; cdecl;
    procedure indexSearchableItems(items: NSArray; completionHandler: TCSSearchableIndexBlockMethod1); cdecl;
    function initWithName(name: NSString): Pointer; overload; cdecl;
    function initWithName(name: NSString; protectionClass: NSFileProtectionType): Pointer; overload; cdecl;
    procedure setIndexDelegate(indexDelegate: Pointer); cdecl;
  end;
  TCSSearchableIndex = class(TOCGenericImport<CSSearchableIndexClass, CSSearchableIndex>) end;

  CSSearchableIndexDelegate = interface(IObjectiveC)
    ['{B2168251-9E53-47B7-9FB1-500AB64AF033}']
    function dataForSearchableIndex(searchableIndex: CSSearchableIndex; itemIdentifier: NSString; typeIdentifier: NSString;
      error: PPointer): NSData; cdecl;
    function fileURLForSearchableIndex(searchableIndex: CSSearchableIndex; itemIdentifier: NSString; typeIdentifier: NSString; inPlace: Boolean;
      error: PPointer): NSURL; cdecl;
    procedure searchableIndex(searchableIndex: CSSearchableIndex; reindexSearchableItemsWithIdentifiers: NSArray;
      acknowledgementHandler: Pointer); overload; cdecl;
    procedure searchableIndex(searchableIndex: CSSearchableIndex; reindexAllSearchableItemsWithAcknowledgementHandler: Pointer); overload; cdecl;
    procedure searchableIndexDidFinishThrottle(searchableIndex: CSSearchableIndex); cdecl;
    procedure searchableIndexDidThrottle(searchableIndex: CSSearchableIndex); cdecl;
  end;

  CSIndexExtensionRequestHandlerClass = interface(NSObjectClass)
    ['{79626134-6281-4C7D-A742-DCA0DE16ED66}']
  end;

  CSIndexExtensionRequestHandler = interface(NSObject)
    ['{AB95D9F1-EC33-42B9-A680-77B5588CFF49}']
  end;
  TCSIndexExtensionRequestHandler = class(TOCGenericImport<CSIndexExtensionRequestHandlerClass, CSIndexExtensionRequestHandler>) end;

  CSSearchQueryContextClass = interface(NSObjectClass)
    ['{ABB5D880-B41A-4005-A4E2-DD4B60BD6262}']
  end;

  CSSearchQueryContext = interface(NSObject)
    ['{0BBF5691-1CC2-47EE-878D-F68C36016850}']
    function fetchAttributes: NSArray; cdecl;
    function filterQueries: NSArray; cdecl;
    function keyboardLanguage: NSString; cdecl;
    procedure setFetchAttributes(fetchAttributes: NSArray); cdecl;
    procedure setFilterQueries(filterQueries: NSArray); cdecl;
    procedure setKeyboardLanguage(keyboardLanguage: NSString); cdecl;
    procedure setSourceOptions(sourceOptions: CSSearchQuerySourceOptions); cdecl;
    function sourceOptions: CSSearchQuerySourceOptions; cdecl;
  end;
  TCSSearchQueryContext = class(TOCGenericImport<CSSearchQueryContextClass, CSSearchQueryContext>) end;

  CSSearchQueryClass = interface(NSObjectClass)
    ['{27E7C237-A8C0-4AB7-9714-C05CF73BF959}']
  end;

  CSSearchQuery = interface(NSObject)
    ['{CF1F95F7-751A-4ABD-BDA8-1BB9CA9795A6}']
    procedure cancel; cdecl;
    function completionHandler: TCSSearchQueryBlockMethod3; cdecl;
    function foundItemCount: NSUInteger; cdecl;
    function foundItemsHandler: TCSSearchQueryBlockMethod1; cdecl;
    function initWithQueryString(queryString: NSString; queryContext: CSSearchQueryContext): Pointer; overload; cdecl;
    function initWithQueryString(queryString: NSString; attributes: NSArray): Pointer; overload; cdecl;
    function isCancelled: Boolean; cdecl;
    function protectionClasses: NSArray; cdecl;
    procedure setCompletionHandler(completionHandler: TCSSearchQueryBlockMethod2); cdecl;
    procedure setFoundItemsHandler(foundItemsHandler: TCSSearchQueryBlockMethod2); cdecl;
    procedure setProtectionClasses(protectionClasses: NSArray); cdecl;
    procedure start; cdecl;
  end;
  TCSSearchQuery = class(TOCGenericImport<CSSearchQueryClass, CSSearchQuery>) end;

  CSSuggestionClass = interface(NSObjectClass)
    ['{85692899-F79E-462C-8AFB-F15890B2D62B}']
  end;

  CSSuggestion = interface(NSObject)
    ['{48D8578F-7228-4035-BCC0-4A2BBF0B56B8}']
    function compare(other: CSSuggestion): NSComparisonResult; cdecl;
    function compareByRank(other: CSSuggestion): NSComparisonResult; cdecl;
    function localizedAttributedSuggestion: NSAttributedString; cdecl;
    function score: NSNumber; cdecl;
    function suggestionDataSources: NSArray; cdecl;
    function suggestionKind: CSSuggestionKind; cdecl;
  end;
  TCSSuggestion = class(TOCGenericImport<CSSuggestionClass, CSSuggestion>) end;

  CSUserQueryContextClass = interface(CSSearchQueryContextClass)
    ['{D8CEE9D3-1428-40BF-987C-6E2F615F6AEA}']
    {class} function userQueryContext: CSUserQueryContext; cdecl;
    {class} function userQueryContextWithCurrentSuggestion(currentSuggestion: CSSuggestion): CSUserQueryContext; cdecl;
  end;

  CSUserQueryContext = interface(CSSearchQueryContext)
    ['{460DA68B-87C0-47F2-8FC0-F4C5168481B0}']
    function enableRankedResults: Boolean; cdecl;
    function maxResultCount: NSInteger; cdecl;
    function maxSuggestionCount: NSInteger; cdecl;
    procedure setEnableRankedResults(enableRankedResults: Boolean); cdecl;
    procedure setMaxResultCount(maxResultCount: NSInteger); cdecl;
    procedure setMaxSuggestionCount(maxSuggestionCount: NSInteger); cdecl;
  end;
  TCSUserQueryContext = class(TOCGenericImport<CSUserQueryContextClass, CSUserQueryContext>) end;

  CSUserQueryClass = interface(CSSearchQueryClass)
    ['{74BB7B2A-7D66-4BA2-97E6-B4680A14B970}']
  end;

  CSUserQuery = interface(CSSearchQuery)
    ['{90C8BEA0-2867-4A57-8445-6EE5451147C3}']
    procedure cancel; cdecl;
    function foundSuggestionCount: NSInteger; cdecl;
    function foundSuggestionsHandler: TCSUserQueryBlockMethod1; cdecl;
    function initWithUserQueryString(userQueryString: NSString; userQueryContext: CSUserQueryContext): Pointer; cdecl;
    procedure setFoundSuggestionsHandler(foundSuggestionsHandler: TCSUserQueryBlockMethod2); cdecl;
    procedure start; cdecl;
  end;
  TCSUserQuery = class(TOCGenericImport<CSUserQueryClass, CSUserQuery>) end;

function CoreSpotlightVersionNumber: Double;
function CSMailboxInbox: NSString;
function CSMailboxDrafts: NSString;
function CSMailboxSent: NSString;
function CSMailboxJunk: NSString;
function CSMailboxTrash: NSString;
function CSMailboxArchive: NSString;
function CSSearchableItemActionType: NSString;
function CSSearchableItemActivityIdentifier: NSString;
function CSActionIdentifier: NSString;
function CSQueryContinuationActionType: NSString;
function CSSearchQueryString: NSString;
function CSIndexErrorDomain: NSString;
function CSSearchQueryErrorDomain: NSErrorDomain;
function CSSuggestionHighlightAttributeName: NSAttributedStringKey;

const
  libCoreSpotlight = '/System/Library/Frameworks/CoreSpotlight.framework/CoreSpotlight';

implementation

uses
  System.SysUtils;

var
  CoreSpotlightModule: THandle;

function CoreSpotlightVersionNumber: Double;
begin
  Result := CocoaDoubleConst(libCoreSpotlight, 'CoreSpotlightVersionNumber');
end;

function CSMailboxInbox: NSString;
begin
  Result := CocoaNSStringConst(libCoreSpotlight, 'CSMailboxInbox');
end;

function CSMailboxDrafts: NSString;
begin
  Result := CocoaNSStringConst(libCoreSpotlight, 'CSMailboxDrafts');
end;

function CSMailboxSent: NSString;
begin
  Result := CocoaNSStringConst(libCoreSpotlight, 'CSMailboxSent');
end;

function CSMailboxJunk: NSString;
begin
  Result := CocoaNSStringConst(libCoreSpotlight, 'CSMailboxJunk');
end;

function CSMailboxTrash: NSString;
begin
  Result := CocoaNSStringConst(libCoreSpotlight, 'CSMailboxTrash');
end;

function CSMailboxArchive: NSString;
begin
  Result := CocoaNSStringConst(libCoreSpotlight, 'CSMailboxArchive');
end;

function CSSearchableItemActionType: NSString;
begin
  Result := CocoaNSStringConst(libCoreSpotlight, 'CSSearchableItemActionType');
end;

function CSSearchableItemActivityIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libCoreSpotlight, 'CSSearchableItemActivityIdentifier');
end;

function CSActionIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libCoreSpotlight, 'CSActionIdentifier');
end;

function CSQueryContinuationActionType: NSString;
begin
  Result := CocoaNSStringConst(libCoreSpotlight, 'CSQueryContinuationActionType');
end;

function CSSearchQueryString: NSString;
begin
  Result := CocoaNSStringConst(libCoreSpotlight, 'CSSearchQueryString');
end;

function CSIndexErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libCoreSpotlight, 'CSIndexErrorDomain');
end;

function CSSearchQueryErrorDomain: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libCoreSpotlight, 'CSSearchQueryErrorDomain');
end;

function CSSuggestionHighlightAttributeName: NSString;
begin
  Result := CocoaNSStringConst(libCoreSpotlight, 'CSSuggestionHighlightAttributeName');
end;

initialization
  CoreSpotlightModule := LoadLibrary(libCoreSpotlight);

finalization
  if CoreSpotlightModule <> 0 then
    FreeLibrary(CoreSpotlightModule);

end.