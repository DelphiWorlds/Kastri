unit DW.iOSapi.PassKit;

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
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit, iOSapi.CoreGraphics,
  // DW
  DW.iOSapi.Foundation, DW.iOSapi.Contacts;

const
  PKPaymentAuthorizationStatusSuccess = 0;
  PKPaymentAuthorizationStatusFailure = 1;
  PKPaymentAuthorizationStatusInvalidBillingPostalAddress = 2;
  PKPaymentAuthorizationStatusInvalidShippingPostalAddress = 3;
  PKPaymentAuthorizationStatusInvalidShippingContact = 4;
  PKPaymentAuthorizationStatusPINRequired = 5;
  PKPaymentAuthorizationStatusPINIncorrect = 6;
  PKPaymentAuthorizationStatusPINLockout = 7;
  PKPaymentButtonStyleWhite = 0;
  PKPaymentButtonStyleWhiteOutline = 1;
  PKPaymentButtonStyleBlack = 2;
  PKPaymentButtonStyleAutomatic = 3;
  PKPaymentButtonTypePlain = 0;
  PKPaymentButtonTypeBuy = 1;
  PKPaymentButtonTypeSetUp = 2;
  PKPaymentButtonTypeInStore = 3;
  PKPaymentButtonTypeDonate = 4;
  PKPaymentButtonTypeCheckout = 5;
  PKPaymentButtonTypeBook = 6;
  PKPaymentButtonTypeSubscribe = 7;
  PKPaymentButtonTypeReload = 8;
  PKPaymentButtonTypeAddMoney = 9;
  PKPaymentButtonTypeTopUp = 10;
  PKPaymentButtonTypeOrder = 11;
  PKPaymentButtonTypeRent = 12;
  PKPaymentButtonTypeSupport = 13;
  PKPaymentButtonTypeContribute = 14;
  PKPaymentButtonTypeTip = 15;
  PKPaymentButtonTypeContinue = 16;
  PKRadioTechnologyNone = 0;
  PKRadioTechnologyNFC = 1;
  PKRadioTechnologyBluetooth = 2;
  PKPassTypeBarcode = 0;
  PKPassTypeSecureElement = 1;
  PKPassTypePayment = PKPassTypeSecureElement;
  PKPassTypeAny = -1;
  PKSecureElementPassActivationStateActivated = 0;
  PKSecureElementPassActivationStateRequiresActivation = 1;
  PKSecureElementPassActivationStateActivating = 2;
  PKSecureElementPassActivationStateSuspended = 3;
  PKSecureElementPassActivationStateDeactivated = 4;
  PKPaymentPassActivationStateActivated = 0;
  PKPaymentPassActivationStateRequiresActivation = 1;
  PKPaymentPassActivationStateActivating = 2;
  PKPaymentPassActivationStateSuspended = 3;
  PKPaymentPassActivationStateDeactivated = 4;
  PKUnknownError = -1;
  PKInvalidDataError = 1;
  PKUnsupportedVersionError = 2;
  PKInvalidSignature = 3;
  PKNotEntitledError = 4;
  PKPaymentUnknownError = -1;
  PKPaymentShippingContactInvalidError = 1;
  PKPaymentBillingContactInvalidError = 2;
  PKPaymentShippingAddressUnserviceableError = 3;
  PKPaymentCouponCodeInvalidError = 4;
  PKPaymentCouponCodeExpiredError = 5;
  PKDisbursementUnknownError = -1;
  PKDisbursementUnsupportedCardError = 1;
  PKDisbursementRecipientContactInvalidError = 2;
  PKAddPaymentPassErrorUnsupported = 0;
  PKAddPaymentPassErrorUserCancelled = 1;
  PKAddPaymentPassErrorSystemCancelled = 2;
  PKAddSecureElementPassGenericError = 0;
  PKAddSecureElementPassUnknownError = PKAddSecureElementPassGenericError;
  PKAddSecureElementPassUserCanceledError = 1;
  PKAddSecureElementPassUnavailableError = 2;
  PKAddSecureElementPassInvalidConfigurationError = 3;
  PKAddSecureElementPassDeviceNotSupportedError = 4;
  PKAddSecureElementPassDeviceNotReadyError = 5;
  PKAddSecureElementPassOSVersionNotSupportedError = 6;
  PKShareSecureElementPassUnknownError = 0;
  PKShareSecureElementPassSetupError = 1;
  PKPassLibraryDidAddPasses = 0;
  PKPassLibraryShouldReviewPasses = 1;
  PKPassLibraryDidCancelAddPasses = 2;
  PKAutomaticPassPresentationSuppressionResultNotSupported = 0;
  PKAutomaticPassPresentationSuppressionResultAlreadyPresenting = 1;
  PKAutomaticPassPresentationSuppressionResultDenied = 2;
  PKAutomaticPassPresentationSuppressionResultCancelled = 3;
  PKAutomaticPassPresentationSuppressionResultSuccess = 4;
  PKPaymentSummaryItemTypeFinal = 0;
  PKPaymentSummaryItemTypePending = 1;
  PKMerchantCapability3DS = 1;
  PKMerchantCapabilityEMV = 2;
  PKMerchantCapabilityCredit = 4;
  PKMerchantCapabilityDebit = 8;
  PKMerchantCapabilityInstantFundsOut = 128;
  PKAddressFieldNone = 0;
  PKAddressFieldPostalAddress = 1;
  PKAddressFieldPhone = 2;
  PKAddressFieldEmail = 4;
  PKAddressFieldName = 8;
  PKAddressFieldAll = PKAddressFieldPostalAddress or PKAddressFieldPhone or PKAddressFieldEmail or PKAddressFieldName;
  PKShippingTypeShipping = 0;
  PKShippingTypeDelivery = 1;
  PKShippingTypeStorePickup = 2;
  PKShippingTypeServicePickup = 3;
  PKShippingContactEditingModeAvailable = 1;
  PKShippingContactEditingModeStorePickup = 2;
  PKShippingContactEditingModeEnabled = PKShippingContactEditingModeAvailable;
  PKApplePayLaterAvailable = 0;
  PKApplePayLaterUnavailableItemIneligible = 1;
  PKApplePayLaterUnavailableRecurringTransaction = 2;
  PKPaymentMethodTypeUnknown = 0;
  PKPaymentMethodTypeDebit = 1;
  PKPaymentMethodTypeCredit = 2;
  PKPaymentMethodTypePrepaid = 3;
  PKPaymentMethodTypeStore = 4;
  PKPaymentMethodTypeEMoney = 5;
  PKAddPaymentPassStylePayment = 0;
  PKAddPaymentPassStyleAccess = 1;
  PKBarcodeEventConfigurationDataTypeUnknown = 0;
  PKBarcodeEventConfigurationDataTypeSigningKeyMaterial = 1;
  PKBarcodeEventConfigurationDataTypeSigningCertificate = 2;
  PKAddShareablePassConfigurationPrimaryActionAdd = 0;
  PKAddShareablePassConfigurationPrimaryActionShare = 1;
  PKIssuerProvisioningExtensionAuthorizationResultCanceled = 0;
  PKIssuerProvisioningExtensionAuthorizationResultAuthorized = 1;
  PKVehicleConnectionErrorCodeUnknown = 0;
  PKVehicleConnectionErrorCodeSessionUnableToStart = 1;
  PKVehicleConnectionErrorCodeSessionNotActive = 2;
  PKVehicleConnectionSessionConnectionStateDisconnected = 0;
  PKVehicleConnectionSessionConnectionStateConnected = 1;
  PKVehicleConnectionSessionConnectionStateConnecting = 2;
  PKVehicleConnectionSessionConnectionStateFailedToConnect = 3;
  PKIdentityErrorUnknown = 0;
  PKIdentityErrorNotSupported = 1;
  PKIdentityErrorCancelled = 2;
  PKIdentityErrorNetworkUnavailable = 3;
  PKIdentityErrorNoElementsRequested = 4;
  PKIdentityErrorRequestAlreadyInProgress = 5;
  PKIdentityErrorInvalidNonce = 6;
  PKIdentityErrorInvalidElement = 7;
  PKIdentityErrorRegionNotSupported = 8;

type
  PKObject = interface;
  PKPass = interface;
  PKSecureElementPass = interface;
  PKPaymentPass = interface;
  PKPassLibrary = interface;
  PKLabeledValue = interface;
  PKContact = interface;
  PKStoredValuePassProperties = interface;
  PKStoredValuePassBalance = interface;
  PKTransitPassProperties = interface;
  PKSuicaPassProperties = interface;
  PKPaymentSummaryItem = interface;
  PKDeferredPaymentSummaryItem = interface;
  PKRecurringPaymentSummaryItem = interface;
  PKDateComponentsRange = interface;
  PKShippingMethod = interface;
  PKAutomaticReloadPaymentRequest = interface;
  PKRecurringPaymentRequest = interface;
  PKDeferredPaymentRequest = interface;
  PKPaymentRequest = interface;
  PKAutomaticReloadPaymentSummaryItem = interface;
  PKPaymentTokenContext = interface;
  PKPaymentAuthorizationResult = interface;
  PKPaymentRequestUpdate = interface;
  PKPaymentRequestShippingContactUpdate = interface;
  PKPaymentRequestShippingMethodUpdate = interface;
  PKPaymentRequestPaymentMethodUpdate = interface;
  PKPaymentRequestMerchantSessionUpdate = interface;
  PKPaymentRequestCouponCodeUpdate = interface;
  PKPaymentOrderDetails = interface;
  PKPaymentToken = interface;
  PKPayment = interface;
  PKPaymentMethod = interface;
  PKPaymentAuthorizationViewControllerDelegate = interface;
  PKPaymentAuthorizationViewController = interface;
  PKPaymentAuthorizationControllerDelegate = interface;
  PKPaymentAuthorizationController = interface;
  PKPaymentButton = interface;
  PKAddPassesViewControllerDelegate = interface;
  PKAddPassesViewController = interface;
  PKAddPaymentPassRequestConfiguration = interface;
  PKAddPaymentPassRequest = interface;
  PKAddPaymentPassViewControllerDelegate = interface;
  PKAddPaymentPassViewController = interface;
  PKDisbursementRequest = interface;
  PKInstantFundsOutFeeSummaryItem = interface;
  PKDisbursementSummaryItem = interface;
  PKBarcodeEventMetadataRequest = interface;
  PKBarcodeEventMetadataResponse = interface;
  PKBarcodeEventSignatureRequest = interface;
  PKBarcodeEventSignatureResponse = interface;
  PKBarcodeEventConfigurationRequest = interface;
  PKPaymentInformationEventExtension = interface;
  PKPaymentInformationRequestHandling = interface;
  PKAddPassMetadataPreview = interface;
  PKAddSecureElementPassConfiguration = interface;
  PKAddCarKeyPassConfiguration = interface;
  PKPaymentMerchantSession = interface;
  PKShareablePassMetadataPreview = interface;
  PKShareablePassMetadata = interface;
  PKAddShareablePassConfiguration = interface;
  PKIdentityDocumentMetadata = interface;
  PKJapanIndividualNumberCardMetadata = interface;
  PKAddIdentityDocumentConfiguration = interface;
  PKIssuerProvisioningExtensionAuthorizationProviding = interface;
  PKIssuerProvisioningExtensionHandler = interface;
  PKIssuerProvisioningExtensionStatus = interface;
  PKIssuerProvisioningExtensionPassEntry = interface;
  PKIssuerProvisioningExtensionPaymentPassEntry = interface;
  PKVehicleConnectionDelegate = interface;
  PKVehicleConnectionSession = interface;
  PKIdentityAuthorizationController = interface;
  PKIdentityDocument = interface;
  PKIdentityDocumentDescriptor = interface;
  PKIdentityDriversLicenseDescriptor = interface;
  PKIdentityNationalIDCardDescriptor = interface;
  PKIdentityElement = interface;
  PKIdentityIntentToStore = interface;
  PKIdentityRequest = interface;

  PKEncryptionScheme = NSString;
  PKPaymentNetwork = NSString;
  PKContactField = NSString;
  PKPaymentAuthorizationStatus = NSInteger;
  PKPaymentButtonStyle = NSInteger;
  PKPaymentButtonType = NSInteger;
  PKRadioTechnology = NSInteger;
  PKPassType = NSInteger;
  PKSecureElementPassActivationState = NSInteger;
  PKPaymentPassActivationState = NSInteger;
  PKPassKitErrorCode = NSInteger;
  PKPaymentErrorCode = NSInteger;
  PKPaymentErrorKey = NSString;
  PKDisbursementErrorCode = NSInteger;
  PKDisbursementErrorKey = NSString;
  PKAddPaymentPassError = NSInteger;
  PKAddSecureElementPassErrorCode = NSInteger;
  PKShareSecureElementPassErrorCode = NSInteger;
  PKPassLibraryAddPassesStatus = NSInteger;
  PKAutomaticPassPresentationSuppressionResult = NSInteger;
  PKSuppressionRequestToken = NSUInteger;
  PKPassLibraryNotificationName = NSString;
  PKPassLibraryNotificationKey = NSString;
  PKStoredValuePassBalanceType = NSString;
  PKPaymentSummaryItemType = NSInteger;
  PKMerchantCapability = NSInteger;
  PKMerchantCategoryCode = SInt16;
  PKAddressField = NSInteger;
  PKShippingType = NSInteger;
  PKShippingContactEditingMode = NSInteger;
  PKApplePayLaterAvailability = NSInteger;
  PKPaymentMethodType = NSInteger;
  PKAddPaymentPassStyle = NSInteger;
  PKBarcodeEventConfigurationDataType = NSInteger;

  PKInformationRequestCompletionBlock = procedure(p1: PKBarcodeEventMetadataResponse) of object;

  PKSignatureRequestCompletionBlock = procedure(p1: PKBarcodeEventSignatureResponse) of object;
  PKAddShareablePassConfigurationPrimaryAction = NSInteger;
  PKIssuerProvisioningExtensionAuthorizationResult = NSInteger;
  PKVehicleConnectionErrorCode = NSInteger;
  PKVehicleConnectionSessionConnectionState = NSInteger;
  PKIdentityError = NSInteger;
  TPKPassLibraryBlockMethod1 = procedure(result: PKAutomaticPassPresentationSuppressionResult) of object;
  TPKPassLibraryBlockMethod2 = procedure(status: PKPassLibraryAddPassesStatus) of object;
  TPKPassLibraryBlockMethod3 = procedure(success: Boolean; error: NSError) of object;
  TPKPassLibraryBlockMethod4 = procedure(signedData: NSData; signature: NSData; error: NSError) of object;
  TPKPassLibraryBlockMethod5 = procedure(encryptedServiceProviderData: NSDictionary; error: NSError) of object;
  TPKPassLibraryBlockMethod6 = procedure(serviceProviderData: NSData; error: NSError) of object;
  TPKPaymentAuthorizationViewControllerDelegateBlockMethod1 = procedure(result: PKPaymentAuthorizationResult) of object;
  TPKPaymentAuthorizationViewControllerDelegateBlockMethod2 = procedure(update: PKPaymentRequestMerchantSessionUpdate) of object;
  TPKPaymentAuthorizationViewControllerDelegateBlockMethod3 = procedure(update: PKPaymentRequestCouponCodeUpdate) of object;
  TPKPaymentAuthorizationViewControllerDelegateBlockMethod4 = procedure(update: PKPaymentRequestShippingMethodUpdate) of object;
  TPKPaymentAuthorizationViewControllerDelegateBlockMethod5 = procedure(update: PKPaymentRequestShippingContactUpdate) of object;
  TPKPaymentAuthorizationViewControllerDelegateBlockMethod6 = procedure(update: PKPaymentRequestPaymentMethodUpdate) of object;
  TPKPaymentAuthorizationViewControllerDelegateBlockMethod7 = procedure(status: PKPaymentAuthorizationStatus) of object;
  TPKPaymentAuthorizationViewControllerDelegateBlockMethod8 = procedure(status: PKPaymentAuthorizationStatus; summaryItems: NSArray) of object;
  TPKPaymentAuthorizationViewControllerDelegateBlockMethod9 = procedure(status: PKPaymentAuthorizationStatus; shippingMethods: NSArray;
    summaryItems: NSArray) of object;
  TPKPaymentAuthorizationViewControllerDelegateBlockMethod10 = procedure(summaryItems: NSArray) of object;
  TPKPaymentAuthorizationControllerDelegateBlockMethod1 = procedure(result: PKPaymentAuthorizationResult) of object;
  TPKPaymentAuthorizationControllerDelegateBlockMethod2 = procedure(status: PKPaymentAuthorizationStatus) of object;
  TPKPaymentAuthorizationControllerDelegateBlockMethod3 = procedure(update: PKPaymentRequestMerchantSessionUpdate) of object;
  TPKPaymentAuthorizationControllerDelegateBlockMethod4 = procedure(update: PKPaymentRequestCouponCodeUpdate) of object;
  TPKPaymentAuthorizationControllerDelegateBlockMethod5 = procedure(requestUpdate: PKPaymentRequestShippingMethodUpdate) of object;
  TPKPaymentAuthorizationControllerDelegateBlockMethod6 = procedure(requestUpdate: PKPaymentRequestShippingContactUpdate) of object;
  TPKPaymentAuthorizationControllerDelegateBlockMethod7 = procedure(requestUpdate: PKPaymentRequestPaymentMethodUpdate) of object;
  TPKPaymentAuthorizationControllerDelegateBlockMethod8 = procedure(status: PKPaymentAuthorizationStatus; summaryItems: NSArray) of object;
  TPKPaymentAuthorizationControllerDelegateBlockMethod9 = procedure(status: PKPaymentAuthorizationStatus; shippingMethods: NSArray;
    summaryItems: NSArray) of object;
  TPKPaymentAuthorizationControllerDelegateBlockMethod10 = procedure(summaryItems: NSArray) of object;
  TPKPaymentAuthorizationControllerBlockMethod1 = procedure(success: Boolean) of object;
  TPKPaymentAuthorizationControllerBlockMethod2 = procedure of object;
  TPKAddPaymentPassViewControllerDelegateBlockMethod1 = procedure(request: PKAddPaymentPassRequest) of object;
  TPKPaymentInformationRequestHandlingBlockMethod1 = procedure of object;
  TPKAddShareablePassConfigurationBlockMethod1 = procedure(shareableCredentialConfiguration: PKAddShareablePassConfiguration; error: NSError) of object;
  TPKAddIdentityDocumentConfigurationBlockMethod1 = procedure(credentialConfiguration: PKAddIdentityDocumentConfiguration; error: NSError) of object;
  TPKIssuerProvisioningExtensionAuthorizationProvidingBlockMethod1 = procedure(param1: PKIssuerProvisioningExtensionAuthorizationResult) of object;
  TPKIssuerProvisioningExtensionAuthorizationProvidingBlockMethod2 = procedure of object;
  TPKIssuerProvisioningExtensionHandlerBlockMethod1 = procedure(status: PKIssuerProvisioningExtensionStatus) of object;
  TPKIssuerProvisioningExtensionHandlerBlockMethod2 = procedure(entries: NSArray) of object;
  TPKIssuerProvisioningExtensionHandlerBlockMethod3 = procedure(request: PKAddPaymentPassRequest) of object;
  TPKVehicleConnectionSessionBlockMethod1 = procedure(session: PKVehicleConnectionSession; error: NSError) of object;
  TPKIdentityAuthorizationControllerBlockMethod1 = procedure(canRequest: Boolean) of object;
  TPKIdentityAuthorizationControllerBlockMethod2 = procedure(document: PKIdentityDocument; error: NSError) of object;

  PKObjectClass = interface(NSObjectClass)
    ['{81E36C07-0DF7-4575-A54E-2B736FF8A258}']
  end;

  PKObject = interface(NSObject)
    ['{6519CF2B-8743-4601-9ED8-5DD1B53111B6}']
  end;
  TPKObject = class(TOCGenericImport<PKObjectClass, PKObject>) end;

  PKPassClass = interface(PKObjectClass)
    ['{5DEDC1AC-3CFF-42B2-9A04-C5E9F883BD62}']
  end;

  PKPass = interface(PKObject)
    ['{AF854BB0-922D-4787-8ED1-65331C729453}']
    function authenticationToken: NSString; cdecl;
    function deviceName: NSString; cdecl;
    function icon: UIImage; cdecl;
    function initWithData(data: NSData; error: PPointer): Pointer; cdecl;
    function isRemotePass: Boolean; cdecl;
    function localizedDescription: NSString; cdecl;
    function localizedName: NSString; cdecl;
    function localizedValueForFieldKey(key: NSString): Pointer; cdecl;
    function organizationName: NSString; cdecl;
    function passType: PKPassType; cdecl;
    function passTypeIdentifier: NSString; cdecl;
    function passURL: NSURL; cdecl;
    function paymentPass: PKPaymentPass; cdecl; // API_DEPRECATED("Use -[PKPass secureElementPass] instead", macos(11.0, API_TO_BE_DEPRECATED), ios(8.0, API_TO_BE_DEPRECATED), watchos(3.0, API_TO_BE_DEPRECATED))
    function relevantDate: NSDate; cdecl; // API_DEPRECATED("Use relevantDates", macos(11.0, 15.0), ios(6.0, 18.0), watchos(2.0, 11.0))
    function relevantDates: NSArray; cdecl;
    function secureElementPass: PKSecureElementPass; cdecl;
    function serialNumber: NSString; cdecl;
    function userInfo: NSDictionary; cdecl;
    function webServiceURL: NSURL; cdecl;
  end;
  TPKPass = class(TOCGenericImport<PKPassClass, PKPass>) end;

  PKSecureElementPassClass = interface(PKPassClass)
    ['{ABE5E811-79AB-456A-B9D5-7A70DA84B567}']
  end;

  PKSecureElementPass = interface(PKPass)
    ['{AA628EEB-54E7-4696-B7C9-AEDC8E0BAFF4}']
    function deviceAccountIdentifier: NSString; cdecl;
    function deviceAccountNumberSuffix: NSString; cdecl;
    function devicePassIdentifier: NSString; cdecl;
    function pairedTerminalIdentifier: NSString; cdecl;
    function passActivationState: PKSecureElementPassActivationState; cdecl;
    function primaryAccountIdentifier: NSString; cdecl;
    function primaryAccountNumberSuffix: NSString; cdecl;
  end;
  TPKSecureElementPass = class(TOCGenericImport<PKSecureElementPassClass, PKSecureElementPass>) end;

  PKPaymentPassClass = interface(PKSecureElementPassClass)
    ['{7E76333B-8BFD-4D56-AB3A-B7ED10EF6BD5}']
  end;

  PKPaymentPass = interface(PKSecureElementPass)
    ['{A992CECE-8C66-49C7-9106-46F19DCFBF60}']
    function activationState: PKPaymentPassActivationState; cdecl; // API_DEPRECATED("Use [PKSecureElementPass passActivationState] instead", macos(11.0, API_TO_BE_DEPRECATED), ios(8.0, API_TO_BE_DEPRECATED), watchos(3.0, API_TO_BE_DEPRECATED))
  end;
  TPKPaymentPass = class(TOCGenericImport<PKPaymentPassClass, PKPaymentPass>) end;

  PKPassLibraryClass = interface(NSObjectClass)
    ['{3157C512-3D75-4030-BD33-6600AFB2F8CB}']
    {class} procedure endAutomaticPassPresentationSuppressionWithRequestToken(requestToken: PKSuppressionRequestToken); cdecl;
    {class} function isPassLibraryAvailable: Boolean; cdecl;
    {class} function isPaymentPassActivationAvailable: Boolean; cdecl; // API_DEPRECATED("Use -[PKPassLibrary isPaymentPassActivationAvailable] instead", ios(8.0, 9.0))
    {class} function isSuppressingAutomaticPassPresentation: Boolean; cdecl;
    {class} function requestAutomaticPassPresentationSuppressionWithResponseHandler(responseHandler: TPKPassLibraryBlockMethod1): PKSuppressionRequestToken; cdecl;
  end;

  PKPassLibrary = interface(NSObject)
    ['{B0DA9061-D785-4556-BABD-B656DEFF7036}']
    procedure activatePaymentPass(paymentPass: PKPaymentPass; withActivationCode: NSString; completion: TPKPassLibraryBlockMethod3); overload; cdecl; // API_DEPRECATED("Use activatePaymentPass:withActivationData:completion: instead", ios(8.0, 9.0))
    procedure activatePaymentPass(paymentPass: PKPaymentPass; withActivationData: NSData; completion: TPKPassLibraryBlockMethod3); overload; cdecl; // API_DEPRECATED("Use activateSecureElementPass:withActivationData:completion: instead", ios(8.0, API_TO_BE_DEPRECATED))
    procedure activateSecureElementPass(secureElementPass: PKSecureElementPass; withActivationData: NSData;
      completion: TPKPassLibraryBlockMethod3); cdecl;
    procedure addPasses(passes: NSArray; withCompletionHandler: TPKPassLibraryBlockMethod2); cdecl;
    function canAddFelicaPass: Boolean; cdecl;
    function canAddPaymentPassWithPrimaryAccountIdentifier(primaryAccountIdentifier: NSString): Boolean; cdecl; // API_DEPRECATED("Use -[PKPassLibrary canAddSecureElementPassWithPrimaryAccountIdentifier] instead", ios(9.0, API_TO_BE_DEPRECATED))
    function canAddSecureElementPassWithPrimaryAccountIdentifier(primaryAccountIdentifier: NSString): Boolean; cdecl;
    function containsPass(pass: PKPass): Boolean; cdecl;
    procedure encryptedServiceProviderDataForSecureElementPass(secureElementPass: PKSecureElementPass; completion: TPKPassLibraryBlockMethod5); cdecl;
    function isPaymentPassActivationAvailable: Boolean; cdecl; // API_DEPRECATED("Use -[PKPassLibrary isSecureElementPassActivationAvailable] instead", ios(9.0, API_TO_BE_DEPRECATED))
    function isSecureElementPassActivationAvailable: Boolean; cdecl;
    procedure openPaymentSetup; cdecl;
    function passes: NSArray; cdecl;
    function passesOfType(passType: PKPassType): NSArray; cdecl;
    function passesWithReaderIdentifier(readerIdentifier: NSString): NSSet; cdecl;
    function passWithPassTypeIdentifier(identifier: NSString; serialNumber: NSString): PKPass; cdecl;
    procedure presentPaymentPass(pass: PKPaymentPass); cdecl; // API_DEPRECATED("Use -[PKPassLibrary presentSecureElementPass:] instead", ios(10.0, API_TO_BE_DEPRECATED))
    procedure presentSecureElementPass(pass: PKSecureElementPass); cdecl;
    function remotePaymentPasses: NSArray; cdecl; // API_DEPRECATED("Use -[PKPassLibrary remoteSecureElementPasses] instead", ios(9.0, API_TO_BE_DEPRECATED))
    function remoteSecureElementPasses: NSArray; cdecl;
    procedure removePass(pass: PKPass); cdecl;
    function replacePassWithPass(pass: PKPass): Boolean; cdecl;
    procedure serviceProviderDataForSecureElementPass(secureElementPass: PKSecureElementPass; completion: TPKPassLibraryBlockMethod6); cdecl;
    procedure signData(signData: NSData; withSecureElementPass: PKSecureElementPass; completion: TPKPassLibraryBlockMethod4); cdecl;
  end;
  TPKPassLibrary = class(TOCGenericImport<PKPassLibraryClass, PKPassLibrary>) end;

  PKLabeledValueClass = interface(NSObjectClass)
    ['{AAF8A292-ACE7-40F9-A372-FF93E48C3F2E}']
  end;

  PKLabeledValue = interface(NSObject)
    ['{402D1675-EAA7-4534-B2B0-E56CEDBCD006}']
    function &label: NSString; cdecl;
    function initWithLabel(&label: NSString; value: NSString): Pointer; cdecl;
    function value: NSString; cdecl;
  end;
  TPKLabeledValue = class(TOCGenericImport<PKLabeledValueClass, PKLabeledValue>) end;

  PKContactClass = interface(NSObjectClass)
    ['{5AA1AE45-7F4C-471F-9D51-02F4591C8ABE}']
  end;

  PKContact = interface(NSObject)
    ['{A150846A-57A2-42BD-B450-8E3B02C7C174}']
    function emailAddress: NSString; cdecl;
    function name: NSPersonNameComponents; cdecl;
    function phoneNumber: CNPhoneNumber; cdecl;
    function postalAddress: CNPostalAddress; cdecl;
    procedure setEmailAddress(emailAddress: NSString); cdecl;
    procedure setName(name: NSPersonNameComponents); cdecl;
    procedure setPhoneNumber(phoneNumber: CNPhoneNumber); cdecl;
    procedure setPostalAddress(postalAddress: CNPostalAddress); cdecl;
    procedure setSupplementarySubLocality(supplementarySubLocality: NSString); cdecl; // API_DEPRECATED("Use subLocality and subAdministrativeArea on -postalAddress instead", ios(9.2, 10.3), watchos(3.0, 3.2))
    function supplementarySubLocality: NSString; cdecl; // API_DEPRECATED("Use subLocality and subAdministrativeArea on -postalAddress instead", ios(9.2, 10.3), watchos(3.0, 3.2))
  end;
  TPKContact = class(TOCGenericImport<PKContactClass, PKContact>) end;

  PKStoredValuePassPropertiesClass = interface(NSObjectClass)
    ['{7474B539-AB68-44F9-9F0A-EE0E60047C61}']
    {class} function passPropertiesForPass(pass: PKPass): Pointer; cdecl;
  end;

  PKStoredValuePassProperties = interface(NSObject)
    ['{E6C0073D-7E3D-4A29-979F-00D544D11446}']
    function balances: NSArray; cdecl;
    function expirationDate: NSDate; cdecl;
    function isBlacklisted: Boolean; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("isBlocked", ios(15.0, 15.0), watchos(8.0, 8.0), macos(12.0, 12.0))
    function isBlocked: Boolean; cdecl;
  end;
  TPKStoredValuePassProperties = class(TOCGenericImport<PKStoredValuePassPropertiesClass, PKStoredValuePassProperties>) end;

  PKStoredValuePassBalanceClass = interface(NSObjectClass)
    ['{0F039078-8026-40D1-9990-B6ED4BEE28A6}']
    {class} function new: Pointer; cdecl;
  end;

  PKStoredValuePassBalance = interface(NSObject)
    ['{FD39918A-6344-4D9F-B5A0-206A535575ED}']
    function amount: NSDecimalNumber; cdecl;
    function balanceType: PKStoredValuePassBalanceType; cdecl;
    function currencyCode: NSString; cdecl;
    function expiryDate: NSDate; cdecl;
    function isEqualToBalance(balance: PKStoredValuePassBalance): Boolean; cdecl;
  end;
  TPKStoredValuePassBalance = class(TOCGenericImport<PKStoredValuePassBalanceClass, PKStoredValuePassBalance>) end;

  PKTransitPassPropertiesClass = interface(PKStoredValuePassPropertiesClass)
    ['{45EDB999-D507-48AF-958E-899304B2EFCA}']
  end;

  PKTransitPassProperties = interface(PKStoredValuePassProperties)
    ['{D12C5ED8-D56E-4FC8-8A26-1F5A3854D9CE}']
    function expirationDate: NSDate; cdecl;
    function isBlacklisted: Boolean; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("isBlocked", ios(11.3, 15.0), watchos(4.3, 8.0), macos(11.0, 12.0))
    function isBlocked: Boolean; cdecl;
    function isInStation: Boolean; cdecl;
    function transitBalance: NSDecimalNumber; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("balances", ios(11.3, 15.0), watchos(4.3, 8.0), macos(11.0, 12.0))
    function transitBalanceCurrencyCode: NSString; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("balances", ios(11.3, 15.0), watchos(4.3, 8.0), macos(11.0, 12.0))
  end;
  TPKTransitPassProperties = class(TOCGenericImport<PKTransitPassPropertiesClass, PKTransitPassProperties>) end;

  PKSuicaPassPropertiesClass = interface(PKTransitPassPropertiesClass)
    ['{53F41B78-E6A3-44FA-B697-0F4BE31D8BD5}']
    {class} function passPropertiesForPass(pass: PKPass): Pointer; cdecl;
  end;

  PKSuicaPassProperties = interface(PKTransitPassProperties)
    ['{8415CC9F-0A4E-4C29-99D0-FAA40623B572}']
    function isBalanceAllowedForCommute: Boolean; cdecl;
    function isBlacklisted: Boolean; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("isBlocked", ios(10.1, 15.0), watchos(3.1, 8.0))
    function isGreenCarTicketUsed: Boolean; cdecl;
    function isInShinkansenStation: Boolean; cdecl;
    function isInStation: Boolean; cdecl;
    function isLowBalanceGateNotificationEnabled: Boolean; cdecl;
    function transitBalance: NSDecimalNumber; cdecl;
    function transitBalanceCurrencyCode: NSString; cdecl;
  end;
  TPKSuicaPassProperties = class(TOCGenericImport<PKSuicaPassPropertiesClass, PKSuicaPassProperties>) end;

  PKPaymentSummaryItemClass = interface(NSObjectClass)
    ['{3E88E902-2D48-4722-830B-0478759D2E2C}']
    {class} function summaryItemWithLabel(&label: NSString; amount: NSDecimalNumber): Pointer; overload; cdecl;
    {class} function summaryItemWithLabel(&label: NSString; amount: NSDecimalNumber; &type: PKPaymentSummaryItemType): Pointer; overload; cdecl;
  end;

  PKPaymentSummaryItem = interface(NSObject)
    ['{366F2EE4-E613-423F-9B4E-ACCA6B9027AB}']
    function &label: NSString; cdecl;
    function &type: PKPaymentSummaryItemType; cdecl;
    function amount: NSDecimalNumber; cdecl;
    procedure setAmount(amount: NSDecimalNumber); cdecl;
    procedure setLabel(&label: NSString); cdecl;
    procedure setType(&type: PKPaymentSummaryItemType); cdecl;
  end;
  TPKPaymentSummaryItem = class(TOCGenericImport<PKPaymentSummaryItemClass, PKPaymentSummaryItem>) end;

  PKDeferredPaymentSummaryItemClass = interface(PKPaymentSummaryItemClass)
    ['{5814C38B-0B82-4699-B755-B535A290B220}']
  end;

  PKDeferredPaymentSummaryItem = interface(PKPaymentSummaryItem)
    ['{C04C5A01-3937-4EA1-A073-3377169DE509}']
    function deferredDate: NSDate; cdecl;
    procedure setDeferredDate(deferredDate: NSDate); cdecl;
  end;
  TPKDeferredPaymentSummaryItem = class(TOCGenericImport<PKDeferredPaymentSummaryItemClass, PKDeferredPaymentSummaryItem>) end;

  PKRecurringPaymentSummaryItemClass = interface(PKPaymentSummaryItemClass)
    ['{BAC24146-7353-4D8F-883E-24A9AF9B6835}']
  end;

  PKRecurringPaymentSummaryItem = interface(PKPaymentSummaryItem)
    ['{4A33F204-6116-452F-B721-1AF1D97EDD8C}']
    function endDate: NSDate; cdecl;
    function intervalCount: NSInteger; cdecl;
    function intervalUnit: NSCalendarUnit; cdecl;
    procedure setEndDate(endDate: NSDate); cdecl;
    procedure setIntervalCount(intervalCount: NSInteger); cdecl;
    procedure setIntervalUnit(intervalUnit: NSCalendarUnit); cdecl;
    procedure setStartDate(startDate: NSDate); cdecl;
    function startDate: NSDate; cdecl;
  end;
  TPKRecurringPaymentSummaryItem = class(TOCGenericImport<PKRecurringPaymentSummaryItemClass, PKRecurringPaymentSummaryItem>) end;

  PKDateComponentsRangeClass = interface(NSObjectClass)
    ['{717D6C1E-7979-4359-950D-C6510AC7C140}']
  end;

  PKDateComponentsRange = interface(NSObject)
    ['{41263C18-2CAD-49A4-A3A3-9CBFA3A5522C}']
    function endDateComponents: NSDateComponents; cdecl;
    function initWithStartDateComponents(startDateComponents: NSDateComponents; endDateComponents: NSDateComponents): Pointer; cdecl;
    function startDateComponents: NSDateComponents; cdecl;
  end;
  TPKDateComponentsRange = class(TOCGenericImport<PKDateComponentsRangeClass, PKDateComponentsRange>) end;

  PKShippingMethodClass = interface(PKPaymentSummaryItemClass)
    ['{4FB140AB-B1F1-4C73-B723-60FF2266AD9C}']
  end;

  PKShippingMethod = interface(PKPaymentSummaryItem)
    ['{8C132965-9B25-449A-9C69-F45CF857EEA6}']
    function dateComponentsRange: PKDateComponentsRange; cdecl;
    function detail: NSString; cdecl;
    function identifier: NSString; cdecl;
    procedure setDateComponentsRange(dateComponentsRange: PKDateComponentsRange); cdecl;
    procedure setDetail(detail: NSString); cdecl;
    procedure setIdentifier(identifier: NSString); cdecl;
  end;
  TPKShippingMethod = class(TOCGenericImport<PKShippingMethodClass, PKShippingMethod>) end;

  PKAutomaticReloadPaymentRequestClass = interface(NSObjectClass)
    ['{A4BEF889-F15D-46A2-A2CD-DFAF8721CD2C}']
  end;

  PKAutomaticReloadPaymentRequest = interface(NSObject)
    ['{4886C392-0305-440A-B661-E9D24868DC1F}']
    function automaticReloadBilling: PKAutomaticReloadPaymentSummaryItem; cdecl;
    function billingAgreement: NSString; cdecl;
    function initWithPaymentDescription(paymentDescription: NSString; automaticReloadBilling: PKAutomaticReloadPaymentSummaryItem;
      managementURL: NSURL): Pointer; cdecl;
    function managementURL: NSURL; cdecl;
    function paymentDescription: NSString; cdecl;
    procedure setAutomaticReloadBilling(automaticReloadBilling: PKAutomaticReloadPaymentSummaryItem); cdecl;
    procedure setBillingAgreement(billingAgreement: NSString); cdecl;
    procedure setManagementURL(managementURL: NSURL); cdecl;
    procedure setPaymentDescription(paymentDescription: NSString); cdecl;
    procedure setTokenNotificationURL(tokenNotificationURL: NSURL); cdecl;
    function tokenNotificationURL: NSURL; cdecl;
  end;
  TPKAutomaticReloadPaymentRequest = class(TOCGenericImport<PKAutomaticReloadPaymentRequestClass, PKAutomaticReloadPaymentRequest>) end;

  PKRecurringPaymentRequestClass = interface(NSObjectClass)
    ['{9D05978B-D15A-4899-8727-46F3085A9E93}']
  end;

  PKRecurringPaymentRequest = interface(NSObject)
    ['{A9A098CE-54BA-4F3B-BDDE-C8ED6B3B8ECE}']
    function billingAgreement: NSString; cdecl;
    function initWithPaymentDescription(paymentDescription: NSString; regularBilling: PKRecurringPaymentSummaryItem;
      managementURL: NSURL): Pointer; cdecl;
    function managementURL: NSURL; cdecl;
    function paymentDescription: NSString; cdecl;
    function regularBilling: PKRecurringPaymentSummaryItem; cdecl;
    procedure setBillingAgreement(billingAgreement: NSString); cdecl;
    procedure setManagementURL(managementURL: NSURL); cdecl;
    procedure setPaymentDescription(paymentDescription: NSString); cdecl;
    procedure setRegularBilling(regularBilling: PKRecurringPaymentSummaryItem); cdecl;
    procedure setTokenNotificationURL(tokenNotificationURL: NSURL); cdecl;
    procedure setTrialBilling(trialBilling: PKRecurringPaymentSummaryItem); cdecl;
    function tokenNotificationURL: NSURL; cdecl;
    function trialBilling: PKRecurringPaymentSummaryItem; cdecl;
  end;
  TPKRecurringPaymentRequest = class(TOCGenericImport<PKRecurringPaymentRequestClass, PKRecurringPaymentRequest>) end;

  PKDeferredPaymentRequestClass = interface(NSObjectClass)
    ['{D4A8DD6D-C40A-48BB-AF69-AD44518372EB}']
  end;

  PKDeferredPaymentRequest = interface(NSObject)
    ['{9574D293-DBF1-403C-8912-C4C8A37BF65D}']
    function billingAgreement: NSString; cdecl;
    function deferredBilling: PKDeferredPaymentSummaryItem; cdecl;
    function freeCancellationDate: NSDate; cdecl;
    function freeCancellationDateTimeZone: NSTimeZone; cdecl;
    function initWithPaymentDescription(paymentDescription: NSString; deferredBilling: PKDeferredPaymentSummaryItem;
      managementURL: NSURL): Pointer; cdecl;
    function managementURL: NSURL; cdecl;
    function paymentDescription: NSString; cdecl;
    procedure setBillingAgreement(billingAgreement: NSString); cdecl;
    procedure setDeferredBilling(deferredBilling: PKDeferredPaymentSummaryItem); cdecl;
    procedure setFreeCancellationDate(freeCancellationDate: NSDate); cdecl;
    procedure setFreeCancellationDateTimeZone(freeCancellationDateTimeZone: NSTimeZone); cdecl;
    procedure setManagementURL(managementURL: NSURL); cdecl;
    procedure setPaymentDescription(paymentDescription: NSString); cdecl;
    procedure setTokenNotificationURL(tokenNotificationURL: NSURL); cdecl;
    function tokenNotificationURL: NSURL; cdecl;
  end;
  TPKDeferredPaymentRequest = class(TOCGenericImport<PKDeferredPaymentRequestClass, PKDeferredPaymentRequest>) end;

  PKPaymentRequestClass = interface(NSObjectClass)
    ['{143F8C54-3DFE-4903-A5F3-9B3368887745}']
    {class} function availableNetworks: NSArray; cdecl;
    {class} function paymentBillingAddressInvalidErrorWithKey(postalAddressKey: NSString; localizedDescription: NSString): NSError; cdecl;
    {class} function paymentContactInvalidErrorWithContactField(field: PKContactField; localizedDescription: NSString): NSError; cdecl;
    {class} function paymentCouponCodeExpiredErrorWithLocalizedDescription(localizedDescription: NSString): NSError; cdecl;
    {class} function paymentCouponCodeInvalidErrorWithLocalizedDescription(localizedDescription: NSString): NSError; cdecl;
    {class} function paymentShippingAddressInvalidErrorWithKey(postalAddressKey: NSString; localizedDescription: NSString): NSError; cdecl;
    {class} function paymentShippingAddressUnserviceableErrorWithLocalizedDescription(localizedDescription: NSString): NSError; cdecl;
  end;

  PKPaymentRequest = interface(NSObject)
    ['{D5B9C2E3-F3DE-4F2C-B5BF-142379866775}']
    function applePayLaterAvailability: PKApplePayLaterAvailability; cdecl;
    function applicationData: NSData; cdecl;
    function automaticReloadPaymentRequest: PKAutomaticReloadPaymentRequest; cdecl;
    function billingContact: PKContact; cdecl;
    function countryCode: NSString; cdecl;
    function couponCode: NSString; cdecl;
    function currencyCode: NSString; cdecl;
    function deferredPaymentRequest: PKDeferredPaymentRequest; cdecl;
    function merchantCapabilities: PKMerchantCapability; cdecl;
    function merchantCategoryCode: PKMerchantCategoryCode; cdecl;
    function merchantIdentifier: NSString; cdecl;
    function multiTokenContexts: NSArray; cdecl;
    function paymentSummaryItems: NSArray; cdecl;
    function recurringPaymentRequest: PKRecurringPaymentRequest; cdecl;
    function requiredBillingAddressFields: PKAddressField; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("requiredBillingContactFields", ios(8.0, 11.0), watchos(2.0, 4.0))
    function requiredBillingContactFields: NSSet; cdecl;
    function requiredShippingAddressFields: PKAddressField; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("requiredShippingContactFields", ios(8.0, 11.0), watchos(2.0, 4.0))
    function requiredShippingContactFields: NSSet; cdecl;
    procedure setApplePayLaterAvailability(applePayLaterAvailability: PKApplePayLaterAvailability); cdecl;
    procedure setApplicationData(applicationData: NSData); cdecl;
    procedure setAutomaticReloadPaymentRequest(automaticReloadPaymentRequest: PKAutomaticReloadPaymentRequest); cdecl;
    procedure setBillingContact(billingContact: PKContact); cdecl;
    procedure setCountryCode(countryCode: NSString); cdecl;
    procedure setCouponCode(couponCode: NSString); cdecl;
    procedure setCurrencyCode(currencyCode: NSString); cdecl;
    procedure setDeferredPaymentRequest(deferredPaymentRequest: PKDeferredPaymentRequest); cdecl;
    procedure setMerchantCapabilities(merchantCapabilities: PKMerchantCapability); cdecl;
    procedure setMerchantCategoryCode(merchantCategoryCode: PKMerchantCategoryCode); cdecl;
    procedure setMerchantIdentifier(merchantIdentifier: NSString); cdecl;
    procedure setMultiTokenContexts(multiTokenContexts: NSArray); cdecl;
    procedure setPaymentSummaryItems(paymentSummaryItems: NSArray); cdecl;
    procedure setRecurringPaymentRequest(recurringPaymentRequest: PKRecurringPaymentRequest); cdecl;
    procedure setRequiredBillingAddressFields(requiredBillingAddressFields: PKAddressField); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("requiredBillingContactFields", ios(8.0, 11.0), watchos(2.0, 4.0))
    procedure setRequiredBillingContactFields(requiredBillingContactFields: NSSet); cdecl;
    procedure setRequiredShippingAddressFields(requiredShippingAddressFields: PKAddressField); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("requiredShippingContactFields", ios(8.0, 11.0), watchos(2.0, 4.0))
    procedure setRequiredShippingContactFields(requiredShippingContactFields: NSSet); cdecl;
    procedure setShippingContact(shippingContact: PKContact); cdecl;
    procedure setShippingContactEditingMode(shippingContactEditingMode: PKShippingContactEditingMode); cdecl;
    procedure setShippingMethods(shippingMethods: NSArray); cdecl;
    procedure setShippingType(shippingType: PKShippingType); cdecl;
    procedure setSupportedCountries(supportedCountries: NSSet); cdecl;
    procedure setSupportedNetworks(supportedNetworks: NSArray); cdecl;
    procedure setSupportsCouponCode(supportsCouponCode: Boolean); cdecl;
    function shippingContact: PKContact; cdecl;
    function shippingContactEditingMode: PKShippingContactEditingMode; cdecl;
    function shippingMethods: NSArray; cdecl;
    function shippingType: PKShippingType; cdecl;
    function supportedCountries: NSSet; cdecl;
    function supportedNetworks: NSArray; cdecl;
    function supportsCouponCode: Boolean; cdecl;
  end;
  TPKPaymentRequest = class(TOCGenericImport<PKPaymentRequestClass, PKPaymentRequest>) end;

  PKAutomaticReloadPaymentSummaryItemClass = interface(PKPaymentSummaryItemClass)
    ['{C8FEC307-7499-4168-96A5-6F6FA6DB2C80}']
  end;

  PKAutomaticReloadPaymentSummaryItem = interface(PKPaymentSummaryItem)
    ['{29AC277B-CBB7-4B1B-8E57-DD99ECB5CC61}']
    procedure setThresholdAmount(thresholdAmount: NSDecimalNumber); cdecl;
    function thresholdAmount: NSDecimalNumber; cdecl;
  end;
  TPKAutomaticReloadPaymentSummaryItem = class(TOCGenericImport<PKAutomaticReloadPaymentSummaryItemClass, PKAutomaticReloadPaymentSummaryItem>) end;

  PKPaymentTokenContextClass = interface(NSObjectClass)
    ['{8C6B4D71-0C46-4BB3-B105-43FB0D48EAB1}']
  end;

  PKPaymentTokenContext = interface(NSObject)
    ['{C0F05EB8-1CFD-4FFB-971C-39803CCF0ACF}']
    function amount: NSDecimalNumber; cdecl;
    function externalIdentifier: NSString; cdecl;
    function initWithMerchantIdentifier(merchantIdentifier: NSString; externalIdentifier: NSString; merchantName: NSString; merchantDomain: NSString;
      amount: NSDecimalNumber): Pointer; cdecl;
    function merchantDomain: NSString; cdecl;
    function merchantIdentifier: NSString; cdecl;
    function merchantName: NSString; cdecl;
    procedure setAmount(amount: NSDecimalNumber); cdecl;
    procedure setExternalIdentifier(externalIdentifier: NSString); cdecl;
    procedure setMerchantDomain(merchantDomain: NSString); cdecl;
    procedure setMerchantIdentifier(merchantIdentifier: NSString); cdecl;
    procedure setMerchantName(merchantName: NSString); cdecl;
  end;
  TPKPaymentTokenContext = class(TOCGenericImport<PKPaymentTokenContextClass, PKPaymentTokenContext>) end;

  PKPaymentAuthorizationResultClass = interface(NSObjectClass)
    ['{4C1A5A81-247E-4E8B-9F18-5AB838BA2CEE}']
  end;

  PKPaymentAuthorizationResult = interface(NSObject)
    ['{36CB5237-687B-480C-8298-F86837B8CC36}']
    function errors: NSArray; cdecl;
    function initWithStatus(status: PKPaymentAuthorizationStatus; errors: NSArray): Pointer; cdecl;
    function orderDetails: PKPaymentOrderDetails; cdecl;
    procedure setErrors(errors: NSArray); cdecl;
    procedure setOrderDetails(orderDetails: PKPaymentOrderDetails); cdecl;
    procedure setStatus(status: PKPaymentAuthorizationStatus); cdecl;
    function status: PKPaymentAuthorizationStatus; cdecl;
  end;
  TPKPaymentAuthorizationResult = class(TOCGenericImport<PKPaymentAuthorizationResultClass, PKPaymentAuthorizationResult>) end;

  PKPaymentRequestUpdateClass = interface(NSObjectClass)
    ['{FDBD1277-F177-4B30-A7C8-5B4AF0BDBA1B}']
  end;

  PKPaymentRequestUpdate = interface(NSObject)
    ['{A6F69B99-79E5-400B-A740-BF62A407B364}']
    function automaticReloadPaymentRequest: PKAutomaticReloadPaymentRequest; cdecl;
    function deferredPaymentRequest: PKDeferredPaymentRequest; cdecl;
    function initWithPaymentSummaryItems(paymentSummaryItems: NSArray): Pointer; cdecl;
    function multiTokenContexts: NSArray; cdecl;
    function paymentSummaryItems: NSArray; cdecl;
    function recurringPaymentRequest: PKRecurringPaymentRequest; cdecl;
    procedure setAutomaticReloadPaymentRequest(automaticReloadPaymentRequest: PKAutomaticReloadPaymentRequest); cdecl;
    procedure setDeferredPaymentRequest(deferredPaymentRequest: PKDeferredPaymentRequest); cdecl;
    procedure setMultiTokenContexts(multiTokenContexts: NSArray); cdecl;
    procedure setPaymentSummaryItems(paymentSummaryItems: NSArray); cdecl;
    procedure setRecurringPaymentRequest(recurringPaymentRequest: PKRecurringPaymentRequest); cdecl;
    procedure setShippingMethods(shippingMethods: NSArray); cdecl;
    procedure setStatus(status: PKPaymentAuthorizationStatus); cdecl;
    function shippingMethods: NSArray; cdecl;
    function status: PKPaymentAuthorizationStatus; cdecl;
  end;
  TPKPaymentRequestUpdate = class(TOCGenericImport<PKPaymentRequestUpdateClass, PKPaymentRequestUpdate>) end;

  PKPaymentRequestShippingContactUpdateClass = interface(PKPaymentRequestUpdateClass)
    ['{A89E09C4-DF5E-459E-87B7-C366244CB6A3}']
  end;

  PKPaymentRequestShippingContactUpdate = interface(PKPaymentRequestUpdate)
    ['{44CB9A47-8FAE-4BF0-BBAD-670AB6AB3AB2}']
    function errors: NSArray; cdecl;
    function initWithErrors(errors: NSArray; paymentSummaryItems: NSArray; shippingMethods: NSArray): Pointer; cdecl;
    procedure setErrors(errors: NSArray); cdecl;
    procedure setShippingMethods(shippingMethods: NSArray); cdecl;
    function shippingMethods: NSArray; cdecl;
  end;
  TPKPaymentRequestShippingContactUpdate = class(TOCGenericImport<PKPaymentRequestShippingContactUpdateClass, PKPaymentRequestShippingContactUpdate>) end;

  PKPaymentRequestShippingMethodUpdateClass = interface(PKPaymentRequestUpdateClass)
    ['{C88AB503-8430-4EE3-9122-E42DAA72B7C3}']
  end;

  PKPaymentRequestShippingMethodUpdate = interface(PKPaymentRequestUpdate)
    ['{41A809CD-99AC-437B-B0BE-34ED2E51469B}']
  end;
  TPKPaymentRequestShippingMethodUpdate = class(TOCGenericImport<PKPaymentRequestShippingMethodUpdateClass, PKPaymentRequestShippingMethodUpdate>) end;

  PKPaymentRequestPaymentMethodUpdateClass = interface(PKPaymentRequestUpdateClass)
    ['{C2243A91-776F-4168-8946-9A6224F49D25}']
  end;

  PKPaymentRequestPaymentMethodUpdate = interface(PKPaymentRequestUpdate)
    ['{3B66A72E-E6A8-4EB4-8313-54F5E96134A1}']
    function errors: NSArray; cdecl;
    function initWithErrors(errors: NSArray; paymentSummaryItems: NSArray): Pointer; cdecl;
    procedure setErrors(errors: NSArray); cdecl;
  end;
  TPKPaymentRequestPaymentMethodUpdate = class(TOCGenericImport<PKPaymentRequestPaymentMethodUpdateClass, PKPaymentRequestPaymentMethodUpdate>) end;

  PKPaymentRequestMerchantSessionUpdateClass = interface(NSObjectClass)
    ['{76B74097-64BA-4619-957C-DDFE01F128A4}']
  end;

  PKPaymentRequestMerchantSessionUpdate = interface(NSObject)
    ['{1D7B0FCB-6A59-4D40-B8E8-DB370EE57BB0}']
    function initWithStatus(status: PKPaymentAuthorizationStatus; merchantSession: PKPaymentMerchantSession): Pointer; cdecl;
    function session: PKPaymentMerchantSession; cdecl;
    procedure setSession(session: PKPaymentMerchantSession); cdecl;
    procedure setStatus(status: PKPaymentAuthorizationStatus); cdecl;
    function status: PKPaymentAuthorizationStatus; cdecl;
  end;
  TPKPaymentRequestMerchantSessionUpdate = class(TOCGenericImport<PKPaymentRequestMerchantSessionUpdateClass, PKPaymentRequestMerchantSessionUpdate>) end;

  PKPaymentRequestCouponCodeUpdateClass = interface(PKPaymentRequestUpdateClass)
    ['{D293492A-9BA6-4CA7-ADA9-EA17FE88ACF1}']
  end;

  PKPaymentRequestCouponCodeUpdate = interface(PKPaymentRequestUpdate)
    ['{0411AC09-8FB2-42B2-BAB2-4DA344017A61}']
    function errors: NSArray; cdecl;
    function initWithErrors(errors: NSArray; paymentSummaryItems: NSArray; shippingMethods: NSArray): Pointer; cdecl;
    procedure setErrors(errors: NSArray); cdecl;
  end;
  TPKPaymentRequestCouponCodeUpdate = class(TOCGenericImport<PKPaymentRequestCouponCodeUpdateClass, PKPaymentRequestCouponCodeUpdate>) end;

  PKPaymentOrderDetailsClass = interface(NSObjectClass)
    ['{B349E0EF-E8DB-4EA6-8899-E99217FA7542}']
    {class} function new: Pointer; cdecl;
  end;

  PKPaymentOrderDetails = interface(NSObject)
    ['{EC5465AC-246D-4C97-ACA1-918E6CB98BB3}']
    function authenticationToken: NSString; cdecl;
    function initWithOrderTypeIdentifier(orderTypeIdentifier: NSString; orderIdentifier: NSString; webServiceURL: NSURL;
      authenticationToken: NSString): Pointer; cdecl;
    function orderIdentifier: NSString; cdecl;
    function orderTypeIdentifier: NSString; cdecl;
    procedure setAuthenticationToken(authenticationToken: NSString); cdecl;
    procedure setOrderIdentifier(orderIdentifier: NSString); cdecl;
    procedure setOrderTypeIdentifier(orderTypeIdentifier: NSString); cdecl;
    procedure setWebServiceURL(webServiceURL: NSURL); cdecl;
    function webServiceURL: NSURL; cdecl;
  end;
  TPKPaymentOrderDetails = class(TOCGenericImport<PKPaymentOrderDetailsClass, PKPaymentOrderDetails>) end;

  PKPaymentTokenClass = interface(NSObjectClass)
    ['{BA620328-5096-4C21-8BAB-CE6241A386B6}']
  end;

  PKPaymentToken = interface(NSObject)
    ['{AFB1FECE-56DF-4100-9234-96D062396685}']
    function paymentData: NSData; cdecl;
    function paymentInstrumentName: NSString; cdecl; // API_DEPRECATED("Use paymentMethod instead", ios(8.0, 9.0))
    function paymentMethod: PKPaymentMethod; cdecl;
    function paymentNetwork: NSString; cdecl; // API_DEPRECATED("Use paymentMethod instead", ios(8.0, 9.0))
    function transactionIdentifier: NSString; cdecl;
  end;
  TPKPaymentToken = class(TOCGenericImport<PKPaymentTokenClass, PKPaymentToken>) end;

  PKPaymentClass = interface(NSObjectClass)
    ['{4DC07F58-DD1B-4207-99B3-FB338C025B0D}']
  end;

  PKPayment = interface(NSObject)
    ['{53847225-5A58-4790-9963-2F612FE39B99}']
    function billingContact: PKContact; cdecl;
    function shippingContact: PKContact; cdecl;
    function shippingMethod: PKShippingMethod; cdecl;
    function token: PKPaymentToken; cdecl;
  end;
  TPKPayment = class(TOCGenericImport<PKPaymentClass, PKPayment>) end;

  PKPaymentMethodClass = interface(NSObjectClass)
    ['{E4947A40-4FA1-443A-B298-8383B92B4ECE}']
  end;

  PKPaymentMethod = interface(NSObject)
    ['{5813E36A-D6C5-4FB1-BC74-FD3146713153}']
    function &type: PKPaymentMethodType; cdecl;
    function billingAddress: CNContact; cdecl;
    function displayName: NSString; cdecl;
    function network: PKPaymentNetwork; cdecl;
    function paymentPass: PKPaymentPass; cdecl; // API_DEPRECATED("Use -[PKPass secureElementPass] instead", macos(11.0, API_TO_BE_DEPRECATED), ios(8.0, API_TO_BE_DEPRECATED), watchos(3.0, API_TO_BE_DEPRECATED))
    function secureElementPass: PKSecureElementPass; cdecl;
  end;
  TPKPaymentMethod = class(TOCGenericImport<PKPaymentMethodClass, PKPaymentMethod>) end;

  PKPaymentAuthorizationViewControllerDelegate = interface(IObjectiveC)
    ['{F299E013-8586-4D17-870A-FAAE68176B93}']
    procedure paymentAuthorizationViewController(controller: PKPaymentAuthorizationViewController; didChangeCouponCode: NSString;
      handler: Pointer); overload; cdecl;
    procedure paymentAuthorizationViewController(controller: PKPaymentAuthorizationViewController; didSelectShippingMethod: PKShippingMethod;
      handler: Pointer); overload; cdecl;
    procedure paymentAuthorizationViewController(controller: PKPaymentAuthorizationViewController; didSelectShippingContact: PKContact;
      handler: Pointer); overload; cdecl;
    procedure paymentAuthorizationViewController(controller: PKPaymentAuthorizationViewController; didSelectPaymentMethod: PKPaymentMethod;
      handler: Pointer); overload; cdecl;
    procedure paymentAuthorizationViewController(controller: PKPaymentAuthorizationViewController;
      didRequestMerchantSessionUpdate: Pointer); overload; cdecl;
    procedure paymentAuthorizationViewController(controller: PKPaymentAuthorizationViewController; didAuthorizePayment: PKPayment;
      handler: Pointer); overload; cdecl;
    [MethodName('paymentAuthorizationViewController:didAuthorizePayment:completion:')]
    procedure paymentAuthorizationViewControllerDidAuthorizePayment(controller: PKPaymentAuthorizationViewController; didAuthorizePayment: PKPayment;
      completion: Pointer); cdecl; // API_DEPRECATED("Use paymentAuthorizationViewController:didAuthorizePayment:handler: instead to provide more granular errors", ios(8.0, 11.0))
    procedure paymentAuthorizationViewControllerDidFinish(controller: PKPaymentAuthorizationViewController); cdecl;
    [MethodName('paymentAuthorizationViewController:didSelectPaymentMethod:completion:')]
    procedure paymentAuthorizationViewControllerDidSelectPaymentMethod(controller: PKPaymentAuthorizationViewController;
      didSelectPaymentMethod: PKPaymentMethod; completion: Pointer); cdecl; // API_DEPRECATED("Use paymentAuthorizationViewController:didSelectPaymentMethod:handler: instead to provide more granular errors", ios(9.0, 11.0))
    [MethodName('paymentAuthorizationViewController:didSelectShippingContact:completion:')]
    procedure paymentAuthorizationViewControllerDidSelectShippingContact(controller: PKPaymentAuthorizationViewController;
      didSelectShippingContact: PKContact; completion: Pointer); cdecl; // API_DEPRECATED("Use paymentAuthorizationViewController:didSelectShippingContact:handler: instead to provide more granular errors", ios(9.0, 11.0))
    [MethodName('paymentAuthorizationViewController:didSelectShippingMethod:completion:')]
    procedure paymentAuthorizationViewControllerDidSelectShippingMethod(controller: PKPaymentAuthorizationViewController;
      didSelectShippingMethod: PKShippingMethod; completion: Pointer); cdecl; // API_DEPRECATED("Use paymentAuthorizationViewController:didSelectShippingMethod:handler: instead to provide more granular errors", ios(8.0, 11.0))
    procedure paymentAuthorizationViewControllerWillAuthorizePayment(controller: PKPaymentAuthorizationViewController); cdecl;
  end;

  PKPaymentAuthorizationViewControllerClass = interface(UIViewControllerClass)
    ['{8A52FE5E-A866-4078-A3BD-43E772C905E4}']
    {class} function canMakePayments: Boolean; cdecl;
    {class} function canMakePaymentsUsingNetworks(supportedNetworks: NSArray; capabilities: PKMerchantCapability): Boolean; overload; cdecl;
    {class} function canMakePaymentsUsingNetworks(supportedNetworks: NSArray): Boolean; overload; cdecl;
    {class} function supportsDisbursements: Boolean; cdecl;
    {class} function supportsDisbursementsUsingNetworks(supportedNetworks: NSArray): Boolean; overload; cdecl;
    {class} function supportsDisbursementsUsingNetworks(supportedNetworks: NSArray; capabilities: PKMerchantCapability): Boolean; overload; cdecl;
  end;

  PKPaymentAuthorizationViewController = interface(UIViewController)
    ['{C57F61EF-C80B-4D57-9737-394D7A58554B}']
    function delegate: Pointer; cdecl;
    function initWithDisbursementRequest(request: PKDisbursementRequest): Pointer; cdecl;
    function initWithPaymentRequest(request: PKPaymentRequest): Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TPKPaymentAuthorizationViewController = class(TOCGenericImport<PKPaymentAuthorizationViewControllerClass, PKPaymentAuthorizationViewController>) end;

  PKPaymentAuthorizationControllerDelegate = interface(IObjectiveC)
    ['{F4B56F3C-B7FF-47AE-8162-7FD0B5C00CFB}']
    procedure paymentAuthorizationController(controller: PKPaymentAuthorizationController; didChangeCouponCode: NSString;
      handler: Pointer); overload; cdecl;
    procedure paymentAuthorizationController(controller: PKPaymentAuthorizationController; didSelectShippingMethod: PKShippingMethod;
      handler: Pointer); overload; cdecl;
    procedure paymentAuthorizationController(controller: PKPaymentAuthorizationController; didSelectShippingContact: PKContact;
      handler: Pointer); overload; cdecl;
    procedure paymentAuthorizationController(controller: PKPaymentAuthorizationController; didSelectPaymentMethod: PKPaymentMethod;
      handler: Pointer); overload; cdecl;
    procedure paymentAuthorizationController(controller: PKPaymentAuthorizationController; didRequestMerchantSessionUpdate: Pointer); overload; cdecl;
    procedure paymentAuthorizationController(controller: PKPaymentAuthorizationController; didAuthorizePayment: PKPayment;
      handler: Pointer); overload; cdecl;
    [MethodName('paymentAuthorizationController:didAuthorizePayment:completion:')]
    procedure paymentAuthorizationControllerDidAuthorizePayment(controller: PKPaymentAuthorizationController; didAuthorizePayment: PKPayment;
      completion: Pointer); cdecl; // API_DEPRECATED("Use paymentAuthorizationController:didAuthorizePayment:handler: instead to provide more granular errors", ios(10.0, 11.0), watchos(3.0, 4.0))
    procedure paymentAuthorizationControllerDidFinish(controller: PKPaymentAuthorizationController); cdecl;
    [MethodName('paymentAuthorizationController:didSelectPaymentMethod:completion:')]
    procedure paymentAuthorizationControllerDidSelectPaymentMethod(controller: PKPaymentAuthorizationController;
      didSelectPaymentMethod: PKPaymentMethod; completion: Pointer); cdecl; // API_DEPRECATED("Use paymentAuthorizationController:didSelectPaymentMethod:handler: instead to provide more granular errors", ios(10.0, 11.0), watchos(3.0, 4.0))
    [MethodName('paymentAuthorizationController:didSelectShippingContact:completion:')]
    procedure paymentAuthorizationControllerDidSelectShippingContact(controller: PKPaymentAuthorizationController;
      didSelectShippingContact: PKContact; completion: Pointer); cdecl; // API_DEPRECATED("Use paymentAuthorizationController:didSelectShippingContact:handler: instead to provide more granular errors", ios(10.0, 11.0), watchos(3.0, 4.0))
    [MethodName('paymentAuthorizationController:didSelectShippingMethod:completion:')]
    procedure paymentAuthorizationControllerDidSelectShippingMethod(controller: PKPaymentAuthorizationController;
      didSelectShippingMethod: PKShippingMethod; completion: Pointer); cdecl; // API_DEPRECATED("Use paymentAuthorizationController:didSelectShippingMethod:handler: instead to provide more granular errors", ios(10.0, 11.0), watchos(3.0, 4.0))
    procedure paymentAuthorizationControllerWillAuthorizePayment(controller: PKPaymentAuthorizationController); cdecl;
    function presentationWindowForPaymentAuthorizationController(controller: PKPaymentAuthorizationController): UIWindow; cdecl;
  end;

  PKPaymentAuthorizationControllerClass = interface(NSObjectClass)
    ['{AAD2C9BC-1EF3-47EE-B8F4-690075D47617}']
    {class} function canMakePayments: Boolean; cdecl;
    {class} function canMakePaymentsUsingNetworks(supportedNetworks: NSArray; capabilities: PKMerchantCapability): Boolean; overload; cdecl;
    {class} function canMakePaymentsUsingNetworks(supportedNetworks: NSArray): Boolean; overload; cdecl;
    {class} function supportsDisbursements: Boolean; cdecl;
    {class} function supportsDisbursementsUsingNetworks(supportedNetworks: NSArray): Boolean; overload; cdecl;
    {class} function supportsDisbursementsUsingNetworks(supportedNetworks: NSArray; capabilities: PKMerchantCapability): Boolean; overload; cdecl;
  end;

  PKPaymentAuthorizationController = interface(NSObject)
    ['{09BB71D6-864A-4647-8F36-21DF5FAA5F00}']
    function delegate: Pointer; cdecl;
    procedure dismissWithCompletion(completion: TPKPaymentAuthorizationControllerBlockMethod2); cdecl;
    function initWithDisbursementRequest(request: PKDisbursementRequest): Pointer; cdecl;
    function initWithPaymentRequest(request: PKPaymentRequest): Pointer; cdecl;
    procedure presentWithCompletion(completion: TPKPaymentAuthorizationControllerBlockMethod1); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TPKPaymentAuthorizationController = class(TOCGenericImport<PKPaymentAuthorizationControllerClass, PKPaymentAuthorizationController>) end;

  PKPaymentButtonClass = interface(UIButtonClass)
    ['{C4094093-8067-4554-B587-89EDA21FF010}']
    {class} function buttonWithType(buttonType: PKPaymentButtonType; style: PKPaymentButtonStyle): Pointer; cdecl;
  end;

  PKPaymentButton = interface(UIButton)
    ['{2BF6B5AE-FF0D-486F-96EB-BB61B00B7B36}']
    function cornerRadius: CGFloat; cdecl;
    function initWithPaymentButtonType(&type: PKPaymentButtonType; paymentButtonStyle: PKPaymentButtonStyle): Pointer; cdecl;
    procedure setCornerRadius(cornerRadius: CGFloat); cdecl;
  end;
  TPKPaymentButton = class(TOCGenericImport<PKPaymentButtonClass, PKPaymentButton>) end;

  PKAddPassesViewControllerDelegate = interface(IObjectiveC)
    ['{31B92189-42DE-4E0F-AED9-7F42F2AB8FAF}']
    procedure addPassesViewControllerDidFinish(controller: PKAddPassesViewController); cdecl;
  end;

  PKAddPassesViewControllerClass = interface(UIViewControllerClass)
    ['{97F43BE3-0F0E-4686-8E66-7727F265DFFE}']
    {class} function canAddPasses: Boolean; cdecl;
  end;

  PKAddPassesViewController = interface(UIViewController)
    ['{4BD1BB19-96D8-464B-9369-6DEA46B48D15}']
    function delegate: Pointer; cdecl;
    function initWithIssuerData(issuerData: NSData; signature: NSData; error: PPointer): Pointer; cdecl;
    function initWithPass(pass: PKPass): Pointer; cdecl;
    function initWithPasses(passes: NSArray): Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TPKAddPassesViewController = class(TOCGenericImport<PKAddPassesViewControllerClass, PKAddPassesViewController>) end;

  PKAddPaymentPassRequestConfigurationClass = interface(NSObjectClass)
    ['{3306DB22-DB63-4526-9E9B-12EA864A48F7}']
  end;

  PKAddPaymentPassRequestConfiguration = interface(NSObject)
    ['{658FD4A7-61B6-4063-8824-1DFA542AABC7}']
    function cardDetails: NSArray; cdecl;
    function cardholderName: NSString; cdecl;
    function encryptionScheme: PKEncryptionScheme; cdecl;
    function initWithEncryptionScheme(encryptionScheme: PKEncryptionScheme): Pointer; cdecl;
    function localizedDescription: NSString; cdecl;
    function paymentNetwork: PKPaymentNetwork; cdecl;
    function primaryAccountIdentifier: NSString; cdecl;
    function primaryAccountSuffix: NSString; cdecl;
    function productIdentifiers: NSSet; cdecl;
    function requiresFelicaSecureElement: Boolean; cdecl;
    procedure setCardDetails(cardDetails: NSArray); cdecl;
    procedure setCardholderName(cardholderName: NSString); cdecl;
    procedure setLocalizedDescription(localizedDescription: NSString); cdecl;
    procedure setPaymentNetwork(paymentNetwork: PKPaymentNetwork); cdecl;
    procedure setPrimaryAccountIdentifier(primaryAccountIdentifier: NSString); cdecl;
    procedure setPrimaryAccountSuffix(primaryAccountSuffix: NSString); cdecl;
    procedure setProductIdentifiers(productIdentifiers: NSSet); cdecl;
    procedure setRequiresFelicaSecureElement(requiresFelicaSecureElement: Boolean); cdecl;
    procedure setStyle(style: PKAddPaymentPassStyle); cdecl;
    function style: PKAddPaymentPassStyle; cdecl;
  end;
  TPKAddPaymentPassRequestConfiguration = class(TOCGenericImport<PKAddPaymentPassRequestConfigurationClass, PKAddPaymentPassRequestConfiguration>) end;

  PKAddPaymentPassRequestClass = interface(NSObjectClass)
    ['{3E7C08D4-9A10-45CD-A8FD-DD32D729F5AD}']
  end;

  PKAddPaymentPassRequest = interface(NSObject)
    ['{6F662E30-39F6-4A25-BCF4-A62F5D37D809}']
    function activationData: NSData; cdecl;
    function encryptedPassData: NSData; cdecl;
    function ephemeralPublicKey: NSData; cdecl;
    procedure setActivationData(activationData: NSData); cdecl;
    procedure setEncryptedPassData(encryptedPassData: NSData); cdecl;
    procedure setEphemeralPublicKey(ephemeralPublicKey: NSData); cdecl;
    procedure setWrappedKey(wrappedKey: NSData); cdecl;
    function wrappedKey: NSData; cdecl;
  end;
  TPKAddPaymentPassRequest = class(TOCGenericImport<PKAddPaymentPassRequestClass, PKAddPaymentPassRequest>) end;

  PKAddPaymentPassViewControllerDelegate = interface(IObjectiveC)
    ['{54D1661F-80A6-40C5-A359-B78131189DBC}']
    procedure addPaymentPassViewController(controller: PKAddPaymentPassViewController; generateRequestWithCertificateChain: NSArray; nonce: NSData;
      nonceSignature: NSData; completionHandler: Pointer); overload; cdecl;
    procedure addPaymentPassViewController(controller: PKAddPaymentPassViewController; didFinishAddingPaymentPass: PKPaymentPass;
      error: NSError); overload; cdecl;
  end;

  PKAddPaymentPassViewControllerClass = interface(UIViewControllerClass)
    ['{C3460272-EB97-4EFE-A137-19A619445B5B}']
    {class} function canAddPaymentPass: Boolean; cdecl;
  end;

  PKAddPaymentPassViewController = interface(UIViewController)
    ['{6C42037A-2BE8-4ADB-BAE1-E2304CE345B1}']
    function delegate: Pointer; cdecl;
    function initWithRequestConfiguration(configuration: PKAddPaymentPassRequestConfiguration; delegate: Pointer): Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TPKAddPaymentPassViewController = class(TOCGenericImport<PKAddPaymentPassViewControllerClass, PKAddPaymentPassViewController>) end;

  PKDisbursementRequestClass = interface(NSObjectClass)
    ['{C651BE9C-DB90-4559-83E9-2F35B3C19CF1}']
    {class} function disbursementCardUnsupportedError: NSError; cdecl;
    {class} function disbursementContactInvalidErrorWithContactField(field: PKContactField; localizedDescription: NSString): NSError; cdecl;
  end;

  PKDisbursementRequest = interface(NSObject)
    ['{02319EBB-09A5-4991-8F6A-2F0EF495C1A0}']
    function applicationData: NSData; cdecl;
    function currencyCode: NSString; cdecl;
    function initWithMerchantIdentifier(merchantIdentifier: NSString; currencyCode: NSString; regionCode: NSString; supportedNetworks: NSArray;
      merchantCapabilities: PKMerchantCapability; summaryItems: NSArray): Pointer; cdecl;
    function merchantCapabilities: PKMerchantCapability; cdecl;
    function merchantIdentifier: NSString; cdecl;
    function recipientContact: PKContact; cdecl;
    function regionCode: NSString; cdecl;
    function requiredRecipientContactFields: NSArray; cdecl;
    procedure setApplicationData(applicationData: NSData); cdecl;
    procedure setCurrencyCode(currencyCode: NSString); cdecl;
    procedure setMerchantCapabilities(merchantCapabilities: PKMerchantCapability); cdecl;
    procedure setMerchantIdentifier(merchantIdentifier: NSString); cdecl;
    procedure setRecipientContact(recipientContact: PKContact); cdecl;
    procedure setRegionCode(regionCode: NSString); cdecl;
    procedure setRequiredRecipientContactFields(requiredRecipientContactFields: NSArray); cdecl;
    procedure setSummaryItems(summaryItems: NSArray); cdecl;
    procedure setSupportedNetworks(supportedNetworks: NSArray); cdecl;
    procedure setSupportedRegions(supportedRegions: NSArray); cdecl;
    function summaryItems: NSArray; cdecl;
    function supportedNetworks: NSArray; cdecl;
    function supportedRegions: NSArray; cdecl;
  end;
  TPKDisbursementRequest = class(TOCGenericImport<PKDisbursementRequestClass, PKDisbursementRequest>) end;

  PKInstantFundsOutFeeSummaryItemClass = interface(PKPaymentSummaryItemClass)
    ['{2BF8F34E-B2A0-4DF6-8BF8-4088DAB10DD2}']
  end;

  PKInstantFundsOutFeeSummaryItem = interface(PKPaymentSummaryItem)
    ['{5D4A8DF9-FEB4-4621-AA4E-FEA2350E0CC8}']
  end;
  TPKInstantFundsOutFeeSummaryItem = class(TOCGenericImport<PKInstantFundsOutFeeSummaryItemClass, PKInstantFundsOutFeeSummaryItem>) end;

  PKDisbursementSummaryItemClass = interface(PKPaymentSummaryItemClass)
    ['{68F1FFD2-801A-4BE4-A916-960FB8F606CB}']
  end;

  PKDisbursementSummaryItem = interface(PKPaymentSummaryItem)
    ['{A7648623-07BB-4A35-B92B-8C51AA2B691B}']
  end;
  TPKDisbursementSummaryItem = class(TOCGenericImport<PKDisbursementSummaryItemClass, PKDisbursementSummaryItem>) end;

  PKBarcodeEventMetadataRequestClass = interface(NSObjectClass)
    ['{EAF03B08-A88B-4107-BE04-0BFAFA71CFEB}']
  end;

  PKBarcodeEventMetadataRequest = interface(NSObject)
    ['{38A43FED-7A06-4CC9-8ACC-A00AD56E52DA}']
    function deviceAccountIdentifier: NSString; cdecl;
    function lastUsedBarcodeIdentifier: NSString; cdecl;
  end;
  TPKBarcodeEventMetadataRequest = class(TOCGenericImport<PKBarcodeEventMetadataRequestClass, PKBarcodeEventMetadataRequest>) end;

  PKBarcodeEventMetadataResponseClass = interface(NSObjectClass)
    ['{9561AC65-476A-4716-9612-B3D254BA6A7C}']
  end;

  PKBarcodeEventMetadataResponse = interface(NSObject)
    ['{ADE2DAF3-5601-4798-A5CC-9CA75E7ECE99}']
    function initWithPaymentInformation(paymentInformation: NSData): Pointer; cdecl;
    function paymentInformation: NSData; cdecl;
    procedure setPaymentInformation(paymentInformation: NSData); cdecl;
  end;
  TPKBarcodeEventMetadataResponse = class(TOCGenericImport<PKBarcodeEventMetadataResponseClass, PKBarcodeEventMetadataResponse>) end;

  PKBarcodeEventSignatureRequestClass = interface(NSObjectClass)
    ['{5D8A4358-992E-453F-A2F0-A79913766DC2}']
  end;

  PKBarcodeEventSignatureRequest = interface(NSObject)
    ['{A41DB9B2-A506-4953-B395-9E20F97A4F0F}']
    function amount: NSNumber; cdecl;
    function barcodeIdentifier: NSString; cdecl;
    function currencyCode: NSString; cdecl;
    function deviceAccountIdentifier: NSString; cdecl;
    function merchantName: NSString; cdecl;
    function partialSignature: NSData; cdecl;
    function rawMerchantName: NSString; cdecl;
    function transactionDate: NSDate; cdecl;
    function transactionIdentifier: NSString; cdecl;
    function transactionStatus: NSString; cdecl;
  end;
  TPKBarcodeEventSignatureRequest = class(TOCGenericImport<PKBarcodeEventSignatureRequestClass, PKBarcodeEventSignatureRequest>) end;

  PKBarcodeEventSignatureResponseClass = interface(NSObjectClass)
    ['{EAD6ACD1-00C0-4259-A613-DB534271EBC0}']
  end;

  PKBarcodeEventSignatureResponse = interface(NSObject)
    ['{4B3002AA-97C3-4F15-8564-4B62BDA4FD00}']
    function initWithSignedData(signedData: NSData): Pointer; cdecl;
    procedure setSignedData(signedData: NSData); cdecl;
    function signedData: NSData; cdecl;
  end;
  TPKBarcodeEventSignatureResponse = class(TOCGenericImport<PKBarcodeEventSignatureResponseClass, PKBarcodeEventSignatureResponse>) end;

  PKBarcodeEventConfigurationRequestClass = interface(NSObjectClass)
    ['{39B22896-3A86-4B55-8740-5EBF2DE6B43D}']
  end;

  PKBarcodeEventConfigurationRequest = interface(NSObject)
    ['{318BF5B5-64A7-4A49-ADC9-EDC4634BF124}']
    function configurationData: NSData; cdecl;
    function configurationDataType: PKBarcodeEventConfigurationDataType; cdecl;
    function deviceAccountIdentifier: NSString; cdecl;
  end;
  TPKBarcodeEventConfigurationRequest = class(TOCGenericImport<PKBarcodeEventConfigurationRequestClass, PKBarcodeEventConfigurationRequest>) end;

  PKPaymentInformationEventExtensionClass = interface(NSObjectClass)
    ['{9A14476F-B6A0-42E2-9598-5D33259DC41C}']
  end;

  PKPaymentInformationEventExtension = interface(NSObject)
    ['{9542BD5A-C011-43E7-B1C9-5DC2F7C34473}']
  end;
  TPKPaymentInformationEventExtension = class(TOCGenericImport<PKPaymentInformationEventExtensionClass, PKPaymentInformationEventExtension>) end;

  PKPaymentInformationRequestHandling = interface(IObjectiveC)
    ['{DF20CAD4-6D70-41A5-999D-992880FCAEE7}']
    procedure handleConfigurationRequest(configurationRequest: PKBarcodeEventConfigurationRequest; completion: Pointer); cdecl;
    procedure handleInformationRequest(infoRequest: PKBarcodeEventMetadataRequest; completion: PKInformationRequestCompletionBlock); cdecl;
    procedure handleSignatureRequest(signatureRequest: PKBarcodeEventSignatureRequest; completion: PKSignatureRequestCompletionBlock); cdecl;
  end;

  PKAddPassMetadataPreviewClass = interface(NSObjectClass)
    ['{FD69D730-93BF-4F37-8736-2C12BEB88D8A}']
    {class} function new: Pointer; cdecl;
    {class} function previewWithPassThumbnail(passThumbnail: CGImageRef; localizedDescription: NSString): Pointer; cdecl;
  end;

  PKAddPassMetadataPreview = interface(NSObject)
    ['{78E1AAF5-C50E-4B6A-B9B1-5E29E651E18F}']
    function initWithPassThumbnail(passThumbnail: CGImageRef; localizedDescription: NSString): Pointer; cdecl;
    function localizedDescription: NSString; cdecl;
    function passThumbnailImage: CGImageRef; cdecl;
  end;
  TPKAddPassMetadataPreview = class(TOCGenericImport<PKAddPassMetadataPreviewClass, PKAddPassMetadataPreview>) end;

  PKAddSecureElementPassConfigurationClass = interface(NSObjectClass)
    ['{4F46ADFD-2A2D-43EC-9780-607DF922BD4F}']
    {class} function new: Pointer; cdecl;
  end;

  PKAddSecureElementPassConfiguration = interface(NSObject)
    ['{53FF1CFD-0B1B-4ED8-BA85-C44222C0D839}']
    function issuerIdentifier: NSString; cdecl;
    function localizedDescription: NSString; cdecl;
    procedure setIssuerIdentifier(issuerIdentifier: NSString); cdecl;
    procedure setLocalizedDescription(localizedDescription: NSString); cdecl;
  end;
  TPKAddSecureElementPassConfiguration = class(TOCGenericImport<PKAddSecureElementPassConfigurationClass, PKAddSecureElementPassConfiguration>) end;

  PKAddCarKeyPassConfigurationClass = interface(PKAddSecureElementPassConfigurationClass)
    ['{26C24F00-27B6-43D4-BC43-8CA76DF8E743}']
  end;

  PKAddCarKeyPassConfiguration = interface(PKAddSecureElementPassConfiguration)
    ['{20083DF5-F88C-4710-8C49-1906256ADE0E}']
    function manufacturerIdentifier: NSString; cdecl;
    function password: NSString; cdecl;
    function provisioningTemplateIdentifier: NSString; cdecl;
    procedure setManufacturerIdentifier(manufacturerIdentifier: NSString); cdecl;
    procedure setPassword(password: NSString); cdecl;
    procedure setProvisioningTemplateIdentifier(provisioningTemplateIdentifier: NSString); cdecl;
    procedure setSupportedRadioTechnologies(supportedRadioTechnologies: PKRadioTechnology); cdecl;
    function supportedRadioTechnologies: PKRadioTechnology; cdecl;
  end;
  TPKAddCarKeyPassConfiguration = class(TOCGenericImport<PKAddCarKeyPassConfigurationClass, PKAddCarKeyPassConfiguration>) end;

  PKPaymentMerchantSessionClass = interface(NSObjectClass)
    ['{381DCA42-EDF3-4C46-9B6C-F65E2B3B32F3}']
  end;

  PKPaymentMerchantSession = interface(NSObject)
    ['{3D0ECB44-F2C5-42DE-8FC2-95279ED9679E}']
    function initWithDictionary(dictionary: NSDictionary): Pointer; cdecl;
  end;
  TPKPaymentMerchantSession = class(TOCGenericImport<PKPaymentMerchantSessionClass, PKPaymentMerchantSession>) end;

  PKShareablePassMetadataPreviewClass = interface(PKAddPassMetadataPreviewClass)
    ['{20D8E8D7-6403-4910-BB61-E39D2148E996}']
    {class} function previewWithTemplateIdentifier(templateIdentifier: NSString): Pointer; cdecl;
  end;

  PKShareablePassMetadataPreview = interface(PKAddPassMetadataPreview)
    ['{6FCC8B36-AD27-465B-9DDC-5C2072E539DA}']
    function initWithTemplateIdentifier(templateIdentifier: NSString): Pointer; cdecl;
    function ownerDisplayName: NSString; cdecl;
    function provisioningTemplateIdentifier: NSString; cdecl;
    procedure setOwnerDisplayName(ownerDisplayName: NSString); cdecl;
  end;
  TPKShareablePassMetadataPreview = class(TOCGenericImport<PKShareablePassMetadataPreviewClass, PKShareablePassMetadataPreview>) end;

  PKShareablePassMetadataClass = interface(NSObjectClass)
    ['{FB43BEDE-64CE-41A3-BBC3-4EA93A16A8A4}']
  end;

  PKShareablePassMetadata = interface(NSObject)
    ['{911AD0C7-4A32-4408-B217-8E8FAB20048B}']
    function accountHash: NSString; cdecl;
    function cardConfigurationIdentifier: NSString; cdecl;
    function cardTemplateIdentifier: NSString; cdecl;
    function credentialIdentifier: NSString; cdecl;
    function initWithProvisioningCredentialIdentifier(credentialIdentifier: NSString; sharingInstanceIdentifier: NSString;
      cardTemplateIdentifier: NSString; preview: PKShareablePassMetadataPreview): Pointer; overload; cdecl;
    function initWithProvisioningCredentialIdentifier(credentialIdentifier: NSString; sharingInstanceIdentifier: NSString;
      passThumbnailImage: CGImageRef; ownerDisplayName: NSString; localizedDescription: NSString; accountHash: NSString; templateIdentifier: NSString;
      relyingPartyIdentifier: NSString; requiresUnifiedAccessCapableDevice: Boolean): Pointer; overload; cdecl; // API_DEPRECATED("Use initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardTemplateIdentifier:passPreviewMetadata:", ios(15.0, 16.0))
    function initWithProvisioningCredentialIdentifier(credentialIdentifier: NSString; cardConfigurationIdentifier: NSString;
      sharingInstanceIdentifier: NSString; passThumbnailImage: CGImageRef; ownerDisplayName: NSString;
      localizedDescription: NSString): Pointer; overload; cdecl; // API_DEPRECATED("Use initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardConfigurationIdentifier:passPreviewMetadata:", ios(14.0, 16.0))
    [MethodName('initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardConfigurationIdentifier:preview:')]
    function initWithProvisioningCredentialIdentifierSharingInstanceIdentifier(credentialIdentifier: NSString; sharingInstanceIdentifier: NSString;
      cardConfigurationIdentifier: NSString; preview: PKShareablePassMetadataPreview): Pointer; cdecl;
    function localizedDescription: NSString; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("preview.localizedDescription", ios(14.0, 16.0), watchos(8.0, 9.0))
    function ownerDisplayName: NSString; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("preview.ownerDisplayName", ios(14.0, 16.0), watchos(8.0, 9.0))
    function passThumbnailImage: CGImageRef; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("preview.passThumbnailImage", ios(14.0, 16.0), watchos(8.0, 9.0))
    function preview: PKShareablePassMetadataPreview; cdecl;
    function relyingPartyIdentifier: NSString; cdecl;
    function requiresUnifiedAccessCapableDevice: Boolean; cdecl;
    function serverEnvironmentIdentifier: NSString; cdecl;
    procedure setAccountHash(accountHash: NSString); cdecl;
    procedure setRelyingPartyIdentifier(relyingPartyIdentifier: NSString); cdecl;
    procedure setRequiresUnifiedAccessCapableDevice(requiresUnifiedAccessCapableDevice: Boolean); cdecl;
    procedure setServerEnvironmentIdentifier(serverEnvironmentIdentifier: NSString); cdecl;
    function sharingInstanceIdentifier: NSString; cdecl;
    function templateIdentifier: NSString; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("cardTemplateIdentifier", ios(15.0, 16.0))
  end;
  TPKShareablePassMetadata = class(TOCGenericImport<PKShareablePassMetadataClass, PKShareablePassMetadata>) end;

  PKAddShareablePassConfigurationClass = interface(PKAddSecureElementPassConfigurationClass)
    ['{D8D788DC-FED4-4CFF-9F9E-F6769A819B5C}']
    {class} procedure configurationForPassMetadata(passMetadata: NSArray; provisioningPolicyIdentifier: NSString;
      primaryAction: PKAddShareablePassConfigurationPrimaryAction; completion: TPKAddShareablePassConfigurationBlockMethod1); overload; cdecl; // API_DEPRECATED("Use configurationForPassMetadata:primaryAction:completion:", ios(14.0, 16.0), watchos(7.0, 9.0))
    {class} procedure configurationForPassMetadata(passMetadata: NSArray; primaryAction: PKAddShareablePassConfigurationPrimaryAction;
      completion: TPKAddShareablePassConfigurationBlockMethod1); overload; cdecl;
  end;

  PKAddShareablePassConfiguration = interface(PKAddSecureElementPassConfiguration)
    ['{478CFB71-58AF-4382-8BF9-07F34E5C0D0C}']
    function credentialsMetadata: NSArray; cdecl;
    function primaryAction: PKAddShareablePassConfigurationPrimaryAction; cdecl;
    function provisioningPolicyIdentifier: NSString; cdecl; // API_DEPRECATED("provisioningPolicyIdentifier has been deprecated. You can stop setting this property in the init with no repercussions.", ios(14.0, 16.0), watchos(7.0, 9.0))
  end;
  TPKAddShareablePassConfiguration = class(TOCGenericImport<PKAddShareablePassConfigurationClass, PKAddShareablePassConfiguration>) end;

  PKIdentityDocumentMetadataClass = interface(NSObjectClass)
    ['{838F2ACF-0C05-4811-8FD4-BFB4EBCC38D9}']
    {class} function new: Pointer; cdecl;
  end;

  PKIdentityDocumentMetadata = interface(NSObject)
    ['{7F74F9F8-EC64-415C-8A96-48B8E68844B7}']
    function cardConfigurationIdentifier: NSString; cdecl;
    function cardTemplateIdentifier: NSString; cdecl;
    function credentialIdentifier: NSString; cdecl;
    function serverEnvironmentIdentifier: NSString; cdecl;
    procedure setServerEnvironmentIdentifier(serverEnvironmentIdentifier: NSString); cdecl;
    function sharingInstanceIdentifier: NSString; cdecl;
  end;
  TPKIdentityDocumentMetadata = class(TOCGenericImport<PKIdentityDocumentMetadataClass, PKIdentityDocumentMetadata>) end;

  PKJapanIndividualNumberCardMetadataClass = interface(PKIdentityDocumentMetadataClass)
    ['{4DCCE8F8-448F-4D9D-92B3-EAE660CA39C0}']
  end;

  PKJapanIndividualNumberCardMetadata = interface(PKIdentityDocumentMetadata)
    ['{871C6CAF-FF2C-454F-81AB-772087058227}']
    function authenticationPassword: NSString; cdecl;
    function initWithProvisioningCredentialIdentifier(credentialIdentifier: NSString; sharingInstanceIdentifier: NSString;
      cardTemplateIdentifier: NSString; preview: PKAddPassMetadataPreview): Pointer; cdecl;
    [MethodName('initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardConfigurationIdentifier:preview:')]
    function initWithProvisioningCredentialIdentifierSharingInstanceIdentifier(credentialIdentifier: NSString; sharingInstanceIdentifier: NSString;
      cardConfigurationIdentifier: NSString; preview: PKAddPassMetadataPreview): Pointer; cdecl;
    function preview: PKAddPassMetadataPreview; cdecl;
    procedure setAuthenticationPassword(authenticationPassword: NSString); cdecl;
    procedure setPreview(preview: PKAddPassMetadataPreview); cdecl;
    procedure setSigningPassword(signingPassword: NSString); cdecl;
    function signingPassword: NSString; cdecl;
  end;
  TPKJapanIndividualNumberCardMetadata = class(TOCGenericImport<PKJapanIndividualNumberCardMetadataClass, PKJapanIndividualNumberCardMetadata>) end;

  PKAddIdentityDocumentConfigurationClass = interface(PKAddSecureElementPassConfigurationClass)
    ['{1E524AE7-93FA-43ED-9F66-FA3DD4F288A8}']
    {class} procedure configurationForMetadata(metadata: PKIdentityDocumentMetadata; completion: TPKAddIdentityDocumentConfigurationBlockMethod1); cdecl;
  end;

  PKAddIdentityDocumentConfiguration = interface(PKAddSecureElementPassConfiguration)
    ['{C8EEF8E9-0E53-4CC3-84E4-D11DA3D4A1A5}']
    function metadata: PKIdentityDocumentMetadata; cdecl;
  end;
  TPKAddIdentityDocumentConfiguration = class(TOCGenericImport<PKAddIdentityDocumentConfigurationClass, PKAddIdentityDocumentConfiguration>) end;

  PKIssuerProvisioningExtensionAuthorizationProviding = interface(IObjectiveC)
    ['{438708BA-0506-4D07-BF63-E050ECED22D9}']
    function completionHandler: TPKIssuerProvisioningExtensionAuthorizationProvidingBlockMethod1; cdecl;
    procedure setCompletionHandler(completionHandler: Pointer); cdecl;
  end;

  PKIssuerProvisioningExtensionHandlerClass = interface(NSObjectClass)
    ['{D485E0A2-5D64-46F6-8D1D-9716CB527A3B}']
  end;

  PKIssuerProvisioningExtensionHandler = interface(NSObject)
    ['{E7B16372-5899-47BA-8F8F-9F4ACBCDB954}']
    procedure generateAddPaymentPassRequestForPassEntryWithIdentifier(identifier: NSString; configuration: PKAddPaymentPassRequestConfiguration;
      certificateChain: NSArray; nonce: NSData; nonceSignature: NSData; completionHandler: TPKIssuerProvisioningExtensionHandlerBlockMethod3); cdecl;
    procedure passEntriesWithCompletion(completion: TPKIssuerProvisioningExtensionHandlerBlockMethod2); cdecl;
    procedure remotePassEntriesWithCompletion(completion: TPKIssuerProvisioningExtensionHandlerBlockMethod2); cdecl;
    procedure statusWithCompletion(completion: TPKIssuerProvisioningExtensionHandlerBlockMethod1); cdecl;
  end;
  TPKIssuerProvisioningExtensionHandler = class(TOCGenericImport<PKIssuerProvisioningExtensionHandlerClass, PKIssuerProvisioningExtensionHandler>) end;

  PKIssuerProvisioningExtensionStatusClass = interface(NSObjectClass)
    ['{85884994-1D9F-4CBE-861E-977367647B8A}']
  end;

  PKIssuerProvisioningExtensionStatus = interface(NSObject)
    ['{209AA927-2BC5-4855-800B-40CC9C291F40}']
    function passEntriesAvailable: Boolean; cdecl;
    function remotePassEntriesAvailable: Boolean; cdecl;
    function requiresAuthentication: Boolean; cdecl;
    procedure setPassEntriesAvailable(passEntriesAvailable: Boolean); cdecl;
    procedure setRemotePassEntriesAvailable(remotePassEntriesAvailable: Boolean); cdecl;
    procedure setRequiresAuthentication(requiresAuthentication: Boolean); cdecl;
  end;
  TPKIssuerProvisioningExtensionStatus = class(TOCGenericImport<PKIssuerProvisioningExtensionStatusClass, PKIssuerProvisioningExtensionStatus>) end;

  PKIssuerProvisioningExtensionPassEntryClass = interface(NSObjectClass)
    ['{5B54D5BB-89C4-404E-899A-DB590BD90B11}']
  end;

  PKIssuerProvisioningExtensionPassEntry = interface(NSObject)
    ['{E6D39909-693B-4B09-8328-819E448D4085}']
    function art: CGImageRef; cdecl;
    function identifier: NSString; cdecl;
    function title: NSString; cdecl;
  end;
  TPKIssuerProvisioningExtensionPassEntry = class(TOCGenericImport<PKIssuerProvisioningExtensionPassEntryClass, PKIssuerProvisioningExtensionPassEntry>) end;

  PKIssuerProvisioningExtensionPaymentPassEntryClass = interface(PKIssuerProvisioningExtensionPassEntryClass)
    ['{84D63AF3-5CE5-49E6-A22C-DFF827726C72}']
  end;

  PKIssuerProvisioningExtensionPaymentPassEntry = interface(PKIssuerProvisioningExtensionPassEntry)
    ['{A97F7C1C-941E-4505-8850-B36326B1BC8A}']
    function addRequestConfiguration: PKAddPaymentPassRequestConfiguration; cdecl;
    function initWithIdentifier(identifier: NSString; title: NSString; art: CGImageRef;
      addRequestConfiguration: PKAddPaymentPassRequestConfiguration): Pointer; cdecl;
  end;
  TPKIssuerProvisioningExtensionPaymentPassEntry = class(TOCGenericImport<PKIssuerProvisioningExtensionPaymentPassEntryClass, PKIssuerProvisioningExtensionPaymentPassEntry>) end;

  PKVehicleConnectionDelegate = interface(IObjectiveC)
    ['{4E9F6D2E-FB1C-488C-8018-397A614684DA}']
    procedure sessionDidChangeConnectionState(newState: PKVehicleConnectionSessionConnectionState); cdecl;
    procedure sessionDidReceiveData(data: NSData); cdecl;
  end;

  PKVehicleConnectionSessionClass = interface(NSObjectClass)
    ['{DB25F722-92B3-485A-8435-29FE6E935357}']
    {class} function new: Pointer; cdecl;
    {class} procedure sessionForPass(pass: PKSecureElementPass; delegate: Pointer; completion: TPKVehicleConnectionSessionBlockMethod1); cdecl;
  end;

  PKVehicleConnectionSession = interface(NSObject)
    ['{30050616-E964-402F-8853-6CE8793D35B7}']
    function connectionStatus: PKVehicleConnectionSessionConnectionState; cdecl;
    function delegate: Pointer; cdecl;
    procedure invalidate; cdecl;
    function sendData(message: NSData; error: PPointer): Boolean; cdecl;
  end;
  TPKVehicleConnectionSession = class(TOCGenericImport<PKVehicleConnectionSessionClass, PKVehicleConnectionSession>) end;

  PKIdentityAuthorizationControllerClass = interface(NSObjectClass)
    ['{013FABA0-8767-4593-BDF3-FDA40BF90C1C}']
  end;

  PKIdentityAuthorizationController = interface(NSObject)
    ['{6D2248AD-5FAF-482F-A21C-642409631E7F}']
    procedure cancelRequest; cdecl;
    procedure checkCanRequestDocument(descriptor: Pointer; completion: TPKIdentityAuthorizationControllerBlockMethod1); cdecl;
    procedure requestDocument(request: PKIdentityRequest; completion: TPKIdentityAuthorizationControllerBlockMethod2); cdecl;
  end;
  TPKIdentityAuthorizationController = class(TOCGenericImport<PKIdentityAuthorizationControllerClass, PKIdentityAuthorizationController>) end;

  PKIdentityDocumentClass = interface(NSObjectClass)
    ['{63B877C3-BA30-4A6B-A94F-2DDE590B412D}']
    {class} function new: Pointer; cdecl;
  end;

  PKIdentityDocument = interface(NSObject)
    ['{CC19B440-F54D-4982-A0B9-60CA82200CEB}']
    function encryptedData: NSData; cdecl;
  end;
  TPKIdentityDocument = class(TOCGenericImport<PKIdentityDocumentClass, PKIdentityDocument>) end;

  PKIdentityDocumentDescriptor = interface(IObjectiveC)
    ['{B17EFE69-6B98-4168-8AE8-1A65598E14E7}']
    procedure addElements(elements: NSArray; withIntentToStore: PKIdentityIntentToStore); cdecl;
    function elements: NSArray; cdecl;
    function intentToStoreForElement(element: PKIdentityElement): PKIdentityIntentToStore; cdecl;
  end;

  PKIdentityDriversLicenseDescriptorClass = interface(NSObjectClass)
    ['{EB0980D5-BC2A-4F89-B879-BD8F4EF9634B}']
  end;

  PKIdentityDriversLicenseDescriptor = interface(NSObject)
    ['{B8BE1CA2-4206-413F-80F1-6529E788D48E}']
  end;
  TPKIdentityDriversLicenseDescriptor = class(TOCGenericImport<PKIdentityDriversLicenseDescriptorClass, PKIdentityDriversLicenseDescriptor>) end;

  PKIdentityNationalIDCardDescriptorClass = interface(NSObjectClass)
    ['{7C1B1736-86C6-4559-8189-27788D485F4A}']
  end;

  PKIdentityNationalIDCardDescriptor = interface(NSObject)
    ['{F8D937AB-C9FC-45CB-BEEF-6DC86AF80D25}']
    function regionCode: NSString; cdecl;
    procedure setRegionCode(regionCode: NSString); cdecl;
  end;
  TPKIdentityNationalIDCardDescriptor = class(TOCGenericImport<PKIdentityNationalIDCardDescriptorClass, PKIdentityNationalIDCardDescriptor>) end;

  PKIdentityElementClass = interface(NSObjectClass)
    ['{41675884-7870-4B2C-96D8-CC002646EE03}']
    {class} function addressElement: PKIdentityElement; cdecl;
    {class} function ageElement: PKIdentityElement; cdecl;
    {class} function ageThresholdElementWithAge(age: NSInteger): Pointer; cdecl;
    {class} function dateOfBirthElement: PKIdentityElement; cdecl;
    {class} function documentDHSComplianceStatusElement: PKIdentityElement; cdecl;
    {class} function documentExpirationDateElement: PKIdentityElement; cdecl;
    {class} function documentIssueDateElement: PKIdentityElement; cdecl;
    {class} function documentNumberElement: PKIdentityElement; cdecl;
    {class} function drivingPrivilegesElement: PKIdentityElement; cdecl;
    {class} function familyNameElement: PKIdentityElement; cdecl;
    {class} function givenNameElement: PKIdentityElement; cdecl;
    {class} function issuingAuthorityElement: PKIdentityElement; cdecl;
    {class} function new: Pointer; cdecl;
    {class} function portraitElement: PKIdentityElement; cdecl;
    {class} function sexElement: PKIdentityElement; cdecl;
  end;

  PKIdentityElement = interface(NSObject)
    ['{03B1E51F-AE31-4F14-98D7-FD0DCE204877}']
  end;
  TPKIdentityElement = class(TOCGenericImport<PKIdentityElementClass, PKIdentityElement>) end;

  PKIdentityIntentToStoreClass = interface(NSObjectClass)
    ['{AE750CD6-9DA2-4935-8BB1-D9604FCACCE3}']
    {class} function mayStoreIntent: PKIdentityIntentToStore; cdecl;
    {class} function mayStoreIntentForDays(days: NSInteger): Pointer; cdecl;
    {class} function new: Pointer; cdecl;
    {class} function willNotStoreIntent: PKIdentityIntentToStore; cdecl;
  end;

  PKIdentityIntentToStore = interface(NSObject)
    ['{2AE0E1E6-7FC2-419A-A2A7-9B7140A7E8BC}']
  end;
  TPKIdentityIntentToStore = class(TOCGenericImport<PKIdentityIntentToStoreClass, PKIdentityIntentToStore>) end;

  PKIdentityRequestClass = interface(NSObjectClass)
    ['{13E0B166-0D99-46F5-8D7B-F113A0C98053}']
  end;

  PKIdentityRequest = interface(NSObject)
    ['{A11AE79E-067C-4ED8-8A94-425168D1FF9B}']
    function descriptor: Pointer; cdecl;
    function merchantIdentifier: NSString; cdecl;
    function nonce: NSData; cdecl;
    procedure setDescriptor(descriptor: Pointer); cdecl;
    procedure setMerchantIdentifier(merchantIdentifier: NSString); cdecl;
    procedure setNonce(nonce: NSData); cdecl;
  end;
  TPKIdentityRequest = class(TOCGenericImport<PKIdentityRequestClass, PKIdentityRequest>) end;

function PKEncryptionSchemeECC_V2: PKEncryptionScheme;
function PKEncryptionSchemeRSA_V2: PKEncryptionScheme;
function PKPaymentNetworkAmex: PKPaymentNetwork;
function PKPaymentNetworkBancomat: PKPaymentNetwork;
function PKPaymentNetworkPagoBancomat: PKPaymentNetwork;
function PKPaymentNetworkBancontact: PKPaymentNetwork;
function PKPaymentNetworkCarteBancaire: PKPaymentNetwork;
function PKPaymentNetworkCarteBancaires: PKPaymentNetwork;
function PKPaymentNetworkCartesBancaires: PKPaymentNetwork;
function PKPaymentNetworkChinaUnionPay: PKPaymentNetwork;
function PKPaymentNetworkDankort: PKPaymentNetwork;
function PKPaymentNetworkDiscover: PKPaymentNetwork;
function PKPaymentNetworkEftpos: PKPaymentNetwork;
function PKPaymentNetworkElectron: PKPaymentNetwork;
function PKPaymentNetworkElo: PKPaymentNetwork;
function PKPaymentNetworkIDCredit: PKPaymentNetwork;
function PKPaymentNetworkInterac: PKPaymentNetwork;
function PKPaymentNetworkJCB: PKPaymentNetwork;
function PKPaymentNetworkMada: PKPaymentNetwork;
function PKPaymentNetworkMaestro: PKPaymentNetwork;
function PKPaymentNetworkMasterCard: PKPaymentNetwork;
function PKPaymentNetworkMir: PKPaymentNetwork;
function PKPaymentNetworkPrivateLabel: PKPaymentNetwork;
function PKPaymentNetworkQuicPay: PKPaymentNetwork;
function PKPaymentNetworkSuica: PKPaymentNetwork;
function PKPaymentNetworkVisa: PKPaymentNetwork;
function PKPaymentNetworkVPay: PKPaymentNetwork;
function PKPaymentNetworkBarcode: PKPaymentNetwork;
function PKPaymentNetworkGirocard: PKPaymentNetwork;
function PKPaymentNetworkWaon: PKPaymentNetwork;
function PKPaymentNetworkNanaco: PKPaymentNetwork;
function PKPaymentNetworkPostFinance: PKPaymentNetwork;
function PKPaymentNetworkTmoney: PKPaymentNetwork;
function PKPaymentNetworkMeeza: PKPaymentNetwork;
function PKPaymentNetworkNAPAS: PKPaymentNetwork;
function PKPaymentNetworkBankAxept: PKPaymentNetwork;
function PKContactFieldPostalAddress: PKContactField;
function PKContactFieldEmailAddress: PKContactField;
function PKContactFieldPhoneNumber: PKContactField;
function PKContactFieldName: PKContactField;
function PKContactFieldPhoneticName: PKContactField;
function PKPassKitErrorDomain: NSString;
function PKPaymentErrorDomain: NSString;
function PKPaymentErrorContactFieldUserInfoKey: PKPaymentErrorKey;
function PKPaymentErrorPostalAddressUserInfoKey: PKPaymentErrorKey;
function PKDisbursementErrorDomain: NSString;
function PKDisbursementErrorContactFieldUserInfoKey: PKDisbursementErrorKey;
function PKAddSecureElementPassErrorDomain: NSString;
function PKShareSecureElementPassErrorDomain: NSString;
function PKPassLibraryDidChangeNotification: PKPassLibraryNotificationName;
function PKPassLibraryRemotePaymentPassesDidChangeNotification: PKPassLibraryNotificationName;
function PKPassLibraryAddedPassesUserInfoKey: PKPassLibraryNotificationKey;
function PKPassLibraryReplacementPassesUserInfoKey: PKPassLibraryNotificationKey;
function PKPassLibraryRemovedPassInfosUserInfoKey: PKPassLibraryNotificationKey;
function PKPassLibraryPassTypeIdentifierUserInfoKey: PKPassLibraryNotificationKey;
function PKPassLibrarySerialNumberUserInfoKey: PKPassLibraryNotificationKey;
function PKPassLibraryRecoveredPassesUserInfoKey: PKPassLibraryNotificationKey;
function PKStoredValuePassBalanceTypeCash: PKStoredValuePassBalanceType;
function PKStoredValuePassBalanceTypeLoyaltyPoints: PKStoredValuePassBalanceType;
// function PKMerchantCategoryCodeNone: PKMerchantCategoryCode;
function PKIdentityErrorDomain: NSErrorDomain;

const
  libPassKit = '/System/Library/Frameworks/PassKit.framework/PassKit';

implementation

uses
  Posix.Dlfcn;

var
  PassKitModule: THandle;

function PKEncryptionSchemeECC_V2: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKEncryptionSchemeECC_V2');
end;

function PKEncryptionSchemeRSA_V2: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKEncryptionSchemeRSA_V2');
end;

function PKPaymentNetworkAmex: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkAmex');
end;

function PKPaymentNetworkBancomat: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkBancomat');
end;

function PKPaymentNetworkPagoBancomat: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkPagoBancomat');
end;

function PKPaymentNetworkBancontact: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkBancontact');
end;

function PKPaymentNetworkCarteBancaire: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkCarteBancaire');
end;

function PKPaymentNetworkCarteBancaires: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkCarteBancaires');
end;

function PKPaymentNetworkCartesBancaires: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkCartesBancaires');
end;

function PKPaymentNetworkChinaUnionPay: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkChinaUnionPay');
end;

function PKPaymentNetworkDankort: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkDankort');
end;

function PKPaymentNetworkDiscover: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkDiscover');
end;

function PKPaymentNetworkEftpos: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkEftpos');
end;

function PKPaymentNetworkElectron: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkElectron');
end;

function PKPaymentNetworkElo: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkElo');
end;

function PKPaymentNetworkIDCredit: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkIDCredit');
end;

function PKPaymentNetworkInterac: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkInterac');
end;

function PKPaymentNetworkJCB: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkJCB');
end;

function PKPaymentNetworkMada: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkMada');
end;

function PKPaymentNetworkMaestro: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkMaestro');
end;

function PKPaymentNetworkMasterCard: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkMasterCard');
end;

function PKPaymentNetworkMir: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkMir');
end;

function PKPaymentNetworkPrivateLabel: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkPrivateLabel');
end;

function PKPaymentNetworkQuicPay: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkQuicPay');
end;

function PKPaymentNetworkSuica: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkSuica');
end;

function PKPaymentNetworkVisa: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkVisa');
end;

function PKPaymentNetworkVPay: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkVPay');
end;

function PKPaymentNetworkBarcode: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkBarcode');
end;

function PKPaymentNetworkGirocard: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkGirocard');
end;

function PKPaymentNetworkWaon: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkWaon');
end;

function PKPaymentNetworkNanaco: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkNanaco');
end;

function PKPaymentNetworkPostFinance: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkPostFinance');
end;

function PKPaymentNetworkTmoney: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkTmoney');
end;

function PKPaymentNetworkMeeza: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkMeeza');
end;

function PKPaymentNetworkNAPAS: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkNAPAS');
end;

function PKPaymentNetworkBankAxept: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentNetworkBankAxept');
end;

function PKContactFieldPostalAddress: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKContactFieldPostalAddress');
end;

function PKContactFieldEmailAddress: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKContactFieldEmailAddress');
end;

function PKContactFieldPhoneNumber: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKContactFieldPhoneNumber');
end;

function PKContactFieldName: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKContactFieldName');
end;

function PKContactFieldPhoneticName: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKContactFieldPhoneticName');
end;

function PKPassKitErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPassKitErrorDomain');
end;

function PKPaymentErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentErrorDomain');
end;

function PKPaymentErrorContactFieldUserInfoKey: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentErrorContactFieldUserInfoKey');
end;

function PKPaymentErrorPostalAddressUserInfoKey: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPaymentErrorPostalAddressUserInfoKey');
end;

function PKDisbursementErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKDisbursementErrorDomain');
end;

function PKDisbursementErrorContactFieldUserInfoKey: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKDisbursementErrorContactFieldUserInfoKey');
end;

function PKAddSecureElementPassErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKAddSecureElementPassErrorDomain');
end;

function PKShareSecureElementPassErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKShareSecureElementPassErrorDomain');
end;

function PKPassLibraryDidChangeNotification: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPassLibraryDidChangeNotification');
end;

function PKPassLibraryRemotePaymentPassesDidChangeNotification: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPassLibraryRemotePaymentPassesDidChangeNotification');
end;

function PKPassLibraryAddedPassesUserInfoKey: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPassLibraryAddedPassesUserInfoKey');
end;

function PKPassLibraryReplacementPassesUserInfoKey: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPassLibraryReplacementPassesUserInfoKey');
end;

function PKPassLibraryRemovedPassInfosUserInfoKey: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPassLibraryRemovedPassInfosUserInfoKey');
end;

function PKPassLibraryPassTypeIdentifierUserInfoKey: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPassLibraryPassTypeIdentifierUserInfoKey');
end;

function PKPassLibrarySerialNumberUserInfoKey: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPassLibrarySerialNumberUserInfoKey');
end;

function PKPassLibraryRecoveredPassesUserInfoKey: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPassLibraryRecoveredPassesUserInfoKey');
end;

function PKStoredValuePassBalanceTypeCash: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKStoredValuePassBalanceTypeCash');
end;

function PKStoredValuePassBalanceTypeLoyaltyPoints: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKStoredValuePassBalanceTypeLoyaltyPoints');
end;

//function PKMerchantCategoryCodeNone: PKMerchantCategoryCode;
//begin
//  Result := PSInt16(CocoaPointerConst(libPassKit, 'PKMerchantCategoryCodeNone'))^;
//end;

function PKIdentityErrorDomain: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKIdentityErrorDomain');
end;

initialization
  PassKitModule := dlopen(MarshaledAString(libPassKit), RTLD_LAZY);

finalization
  dlclose(PassKitModule);

end.