unit DW.iOSapi.PassKit;

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
  Macapi.CoreFoundation, Macapi.CoreServices, Macapi.Dispatch, Macapi.Mach, Macapi.ObjCRuntime, Macapi.ObjectiveC,
  // iOS
  iOSapi.AddressBook, iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit,
  // DW
  DW.iOSapi.Contacts, DW.iOSapi.Foundation;

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
  PKPaymentButtonTypePlain = 0;
  PKPaymentButtonTypeBuy = 1;
  PKPaymentButtonTypeSetUp = 2;
  PKPaymentButtonTypeInStore = 3;
  PKPaymentButtonTypeDonate = 4;
  PKUnknownError = -1;
  PKInvalidDataError = 1;
  PKUnsupportedVersionError = 2;
  PKInvalidSignature = 3;
  PKNotEntitledError = 4;
  PKPassTypeBarcode = 0;
  PKPassTypePayment = 1;
  PKPassTypeAny = not NSUInteger(0);
  PKPaymentPassActivationStateActivated = 0;
  PKPaymentPassActivationStateRequiresActivation = 1;
  PKPaymentPassActivationStateActivating = 2;
  PKPaymentPassActivationStateSuspended = 3;
  PKPaymentPassActivationStateDeactivated = 4;
  PKPassLibraryDidAddPasses = 0;
  PKPassLibraryShouldReviewPasses = 1;
  PKPassLibraryDidCancelAddPasses = 2;
  PKAutomaticPassPresentationSuppressionResultNotSupported = 0;
  PKAutomaticPassPresentationSuppressionResultAlreadyPresenting = 1;
  PKAutomaticPassPresentationSuppressionResultDenied = 2;
  PKAutomaticPassPresentationSuppressionResultCancelled = 3;
  PKAutomaticPassPresentationSuppressionResultSuccess = 4;
  PKMerchantCapability3DS = 1 shl 0;
  PKMerchantCapabilityEMV = 1 shl 1;
  PKMerchantCapabilityCredit = 1 shl 2;
  PKMerchantCapabilityDebit = 1 shl 3;
  PKAddressFieldNone = 0;
  PKAddressFieldPostalAddress = 1 shl 0;
  PKAddressFieldPhone = 1 shl 1;
  PKAddressFieldEmail = 1 shl 2;
  PKAddressFieldName = 1 shl 3;
  PKAddressFieldAll = (PKAddressFieldPostalAddress or PKAddressFieldPhone or PKAddressFieldEmail or PKAddressFieldName);
  PKShippingTypeShipping = 0;
  PKShippingTypeDelivery = 1;
  PKShippingTypeStorePickup = 2;
  PKShippingTypeServicePickup = 3;
  PKPaymentSummaryItemTypeFinal = 0;
  PKPaymentSummaryItemTypePending = 1;
  PKPaymentMethodTypeUnknown = 0;
  PKPaymentMethodTypeDebit = 1;
  PKPaymentMethodTypeCredit = 2;
  PKPaymentMethodTypePrepaid = 3;
  PKPaymentMethodTypeStore = 4;
  PKAddPassButtonStyleBlack = 0;
  PKAddPassButtonStyleBlackOutline = 1;
  PKAddPaymentPassErrorUnsupported = 0;
  PKAddPaymentPassErrorUserCancelled = 1;
  PKAddPaymentPassErrorSystemCancelled = 2;

type
  PKObject = interface;
  PKPaymentPass = interface;
  PKPass = interface;
  PKPassLibrary = interface;
  PKLabeledValue = interface;
  PKContact = interface;
  PKSuicaPassProperties = interface;
  PKPaymentSummaryItem = interface;
  PKShippingMethod = interface;
  PKPaymentRequest = interface;
  PKPaymentMethod = interface;
  PKPaymentToken = interface;
  PKPayment = interface;
  PKPaymentAuthorizationViewController = interface;
  PKPaymentAuthorizationViewControllerDelegate = interface;
  PKPaymentAuthorizationController = interface;
  PKPaymentAuthorizationControllerDelegate = interface;
  PKAddPassButton = interface;
  PKPaymentButton = interface;
  PKAddPassesViewController = interface;
  PKAddPassesViewControllerDelegate = interface;
  PKAddPaymentPassViewController = interface;
  PKAddPaymentPassRequestConfiguration = interface;
  PKAddPaymentPassRequest = interface;
  PKAddPaymentPassViewControllerDelegate = interface;

  PKEncryptionScheme = NSString;
  PPKEncryptionScheme = ^PKEncryptionScheme;
  PKPaymentNetwork = NSString;
  PPKPaymentNetwork = ^PKPaymentNetwork;
  PKPaymentAuthorizationStatus = NSInteger;
  PKPaymentButtonStyle = NSInteger;
  PKPaymentButtonType = NSInteger;
  PKPassKitErrorCode = NSInteger;
  PKPassType = NSUInteger;

  PKPaymentPassActivationState = NSUInteger;
  PKPassLibraryAddPassesStatus = NSInteger;
  PKAutomaticPassPresentationSuppressionResult = NSUInteger;
  PKSuppressionRequestToken = NSUInteger;
  PPKSuppressionRequestToken = ^PKSuppressionRequestToken;
  TPassKitResponseHandler = procedure(param1: PKAutomaticPassPresentationSuppressionResult) of object;
  TPassKitWithCompletionHandler = procedure(param1: PKPassLibraryAddPassesStatus) of object;
  TPassKitCompletion = procedure(param1: Boolean; param2: NSError) of object;
  PKPassLibraryNotificationName = NSString;
  PPKPassLibraryNotificationName = ^PKPassLibraryNotificationName;
  PKPassLibraryNotificationKey = NSString;
  PPKPassLibraryNotificationKey = ^PKPassLibraryNotificationKey;
  PKMerchantCapability = NSUInteger;
  PKAddressField = NSUInteger;
  PKShippingType = NSUInteger;
  PKPaymentSummaryItemType = NSUInteger;
  ABRecordRef = CFTypeRef;
  PABRecordRef = ^ABRecordRef;
  PKPaymentMethodType = NSUInteger;

  TPassKitCompletion1 = procedure(param1: PKPaymentAuthorizationStatus) of object;
  TPassKitCompletion2 = procedure(param1: PKPaymentAuthorizationStatus; param2: NSArray) of object;
  TPassKitCompletion3 = procedure(param1: PKPaymentAuthorizationStatus; param2: NSArray; param3: NSArray) of object;
  TPassKitCompletion4 = procedure(param1: NSArray) of object;
  TPassKitCompletion5 = procedure(param1: Boolean) of object;
  TPassKitCompletion6 = procedure() of object;
  PKAddPassButtonStyle = NSInteger;
  PKAddPaymentPassError = NSInteger;
  TPassKitCompletionHandler = procedure(param1: PKAddPaymentPassRequest) of object;

  PKObjectClass = interface(NSObjectClass)
    ['{286DE893-476A-453C-824D-84996D9C88F2}']
  end;

  PKObject = interface(NSObject)
    ['{49C04773-5FB4-4238-AA6F-E389BD7D60A8}']
  end;
  TPKObject = class(TOCGenericImport<PKObjectClass, PKObject>)
  end;

  PKPassClass = interface(PKObjectClass)
    ['{1F705288-E236-4E28-8D2A-008A7381D6E1}']
  end;

  PKPass = interface(PKObject)
    ['{93949183-00A1-43FA-8912-28F6DA3FDB2B}']
    function initWithData(data: NSData; error: NSError): Pointer; cdecl;
    function passType: PKPassType; cdecl;
    function paymentPass: PKPaymentPass; cdecl;
    function serialNumber: NSString; cdecl;
    function passTypeIdentifier: NSString; cdecl;
    function webServiceURL: NSURL; cdecl;
    function authenticationToken: NSString; cdecl;
    function icon: UIImage; cdecl;
    function localizedName: NSString; cdecl;
    function localizedDescription: NSString; cdecl;
    function organizationName: NSString; cdecl;
    function relevantDate: NSDate; cdecl;
    function userInfo: NSDictionary; cdecl;
    function passURL: NSURL; cdecl;
    function isRemotePass: Boolean; cdecl;
    function deviceName: NSString; cdecl;
    function localizedValueForFieldKey(key: NSString): Pointer; cdecl;
  end;
  TPKPass = class(TOCGenericImport<PKPassClass, PKPass>)
  end;

  PKPaymentPassClass = interface(PKPassClass)
    ['{17BF5AD4-F5EF-4CA1-AFE0-18F5DFF98DD9}']
  end;

  PKPaymentPass = interface(PKPass)
    ['{DE55CA90-BB6D-4A7A-93EC-A0E39701AC8C}']
    function primaryAccountIdentifier: NSString; cdecl;
    function primaryAccountNumberSuffix: NSString; cdecl;
    function deviceAccountIdentifier: NSString; cdecl;
    function deviceAccountNumberSuffix: NSString; cdecl;
    function activationState: PKPaymentPassActivationState; cdecl;
  end;
  TPKPaymentPass = class(TOCGenericImport<PKPaymentPassClass, PKPaymentPass>)
  end;

  PKPassLibraryClass = interface(NSObjectClass)
    ['{F4A6448E-D61A-4A4A-AEF0-996910F3F664}']
    { class } function isPassLibraryAvailable: Boolean; cdecl;
    { class } function requestAutomaticPassPresentationSuppressionWithResponseHandler(responseHandler: TPassKitResponseHandler)
      : PKSuppressionRequestToken; cdecl;
    { class } procedure endAutomaticPassPresentationSuppressionWithRequestToken(requestToken: PKSuppressionRequestToken); cdecl;
    { class } function isSuppressingAutomaticPassPresentation: Boolean; cdecl;
    { class } function isPaymentPassActivationAvailable: Boolean; cdecl;
  end;

  PKPassLibrary = interface(NSObject)
    ['{B7231203-AA00-4CEE-B37B-A23426EA7F36}']
    function isPaymentPassActivationAvailable: Boolean; cdecl;
    function passes: NSArray; cdecl;
    function passWithPassTypeIdentifier(identifier: NSString; serialNumber: NSString): PKPass; cdecl;
    function passesOfType(passType: PKPassType): NSArray; cdecl;
    function remotePaymentPasses: NSArray; cdecl;
    procedure removePass(pass: PKPass); cdecl;
    function containsPass(pass: PKPass): Boolean; cdecl;
    function replacePassWithPass(pass: PKPass): Boolean; cdecl;
    procedure addPasses(passes: NSArray; withCompletionHandler: TPassKitWithCompletionHandler); cdecl;
    procedure openPaymentSetup; cdecl;
    procedure presentPaymentPass(pass: PKPaymentPass); cdecl;
    function canAddPaymentPassWithPrimaryAccountIdentifier(primaryAccountIdentifier: NSString): Boolean; cdecl;
    function canAddFelicaPass: Boolean; cdecl;
    [MethodName('activatePaymentPass:withActivationData:completion:')]
    procedure activatePaymentPassWithActivationDataCompletion(paymentPass: PKPaymentPass; withActivationData: NSData;
      completion: TPassKitCompletion); cdecl;
    [MethodName('activatePaymentPass:withActivationCode:completion:')]
    procedure activatePaymentPassWithActivationCodeCompletion(paymentPass: PKPaymentPass; withActivationCode: NSString;
      completion: TPassKitCompletion); cdecl;
  end;
  TPKPassLibrary = class(TOCGenericImport<PKPassLibraryClass, PKPassLibrary>)
  end;

  PKLabeledValueClass = interface(NSObjectClass)
    ['{AA366AD0-546C-4E29-9317-DA9621CCE0CE}']
  end;

  PKLabeledValue = interface(NSObject)
    ['{F830C3BE-B0E9-4E4A-8F74-48174BFB94A4}']
    function initWithLabel(&label: NSString; value: NSString): Pointer; cdecl;
    function &label: NSString; cdecl;
    function value: NSString; cdecl;
  end;
  TPKLabeledValue = class(TOCGenericImport<PKLabeledValueClass, PKLabeledValue>)
  end;

  PKContactClass = interface(NSObjectClass)
    ['{15DCBFA4-CE4A-4830-8DCF-23CBADAF2D8E}']
  end;

  PKContact = interface(NSObject)
    ['{4944E756-1181-4F22-B1B6-205B7A25A121}']
    procedure setName(name: NSPersonNameComponents); cdecl;
    function name: NSPersonNameComponents; cdecl;
    procedure setPostalAddress(postalAddress: CNPostalAddress); cdecl;
    function postalAddress: CNPostalAddress; cdecl;
    procedure setEmailAddress(emailAddress: NSString); cdecl;
    function emailAddress: NSString; cdecl;
    procedure setPhoneNumber(phoneNumber: CNPhoneNumber); cdecl;
    function phoneNumber: CNPhoneNumber; cdecl;
    procedure setSupplementarySubLocality(supplementarySubLocality: NSString); cdecl;
    function supplementarySubLocality: NSString; cdecl;
  end;
  TPKContact = class(TOCGenericImport<PKContactClass, PKContact>)
  end;

  PKSuicaPassPropertiesClass = interface(NSObjectClass)
    ['{D3892F6E-ED4F-4C5F-A552-F3A17039DFCA}']
    { class } function passPropertiesForPass(pass: PKPass): Pointer; cdecl;
  end;

  PKSuicaPassProperties = interface(NSObject)
    ['{07F5DF1C-92A5-48A6-B1EF-D229FDFE9110}']
    function transitBalance: NSDecimalNumber; cdecl;
    function transitBalanceCurrencyCode: NSString; cdecl;
    function isInStation: Boolean; cdecl;
    function isInShinkansenStation: Boolean; cdecl;
    function isGreenCarTicketUsed: Boolean; cdecl;
    function isBlacklisted: Boolean; cdecl;
  end;
  TPKSuicaPassProperties = class(TOCGenericImport<PKSuicaPassPropertiesClass, PKSuicaPassProperties>)
  end;

  PKPaymentSummaryItemClass = interface(NSObjectClass)
    ['{302F54EF-FCCC-4CCE-8A99-B4E6E30DFADA}']
    [MethodName('summaryItemWithLabel:amount:')]
    { class } function summaryItemWithLabelAmount(&label: NSString; amount: NSDecimalNumber): Pointer; cdecl;
    [MethodName('summaryItemWithLabel:amount:type:')]
    { class } function summaryItemWithLabelAmountType(&label: NSString; amount: NSDecimalNumber; &type: PKPaymentSummaryItemType)
      : Pointer; cdecl;
  end;

  PKPaymentSummaryItem = interface(NSObject)
    ['{F788F79F-8772-4F79-B206-0CCAEFABF9E1}']
    procedure setLabel(&label: NSString); cdecl;
    function &label: NSString; cdecl;
    procedure setAmount(amount: NSDecimalNumber); cdecl;
    function amount: NSDecimalNumber; cdecl;
    procedure setType(&type: PKPaymentSummaryItemType); cdecl;
    function &type: PKPaymentSummaryItemType; cdecl;
  end;
  TPKPaymentSummaryItem = class(TOCGenericImport<PKPaymentSummaryItemClass, PKPaymentSummaryItem>)
  end;

  PKShippingMethodClass = interface(PKPaymentSummaryItemClass)
    ['{45EADB4C-6022-4204-AB34-100766853E1C}']
  end;

  PKShippingMethod = interface(PKPaymentSummaryItem)
    ['{CA55226A-160C-4272-BCF1-CD502CB447FA}']
    procedure setIdentifier(identifier: NSString); cdecl;
    function identifier: NSString; cdecl;
    procedure setDetail(detail: NSString); cdecl;
    function detail: NSString; cdecl;
  end;
  TPKShippingMethod = class(TOCGenericImport<PKShippingMethodClass, PKShippingMethod>)
  end;

  PKPaymentRequestClass = interface(NSObjectClass)
    ['{039261FA-1505-449D-B5B4-DE09D8766390}']
    { class } function availableNetworks: NSArray; cdecl;
  end;

  PKPaymentRequest = interface(NSObject)
    ['{65543C2C-3262-4C22-90FE-ED58CADA793B}']
    procedure setMerchantIdentifier(merchantIdentifier: NSString); cdecl;
    function merchantIdentifier: NSString; cdecl;
    procedure setCountryCode(countryCode: NSString); cdecl;
    function countryCode: NSString; cdecl;
    procedure setSupportedNetworks(supportedNetworks: NSArray); cdecl;
    function supportedNetworks: NSArray; cdecl;
    procedure setMerchantCapabilities(merchantCapabilities: PKMerchantCapability); cdecl;
    function merchantCapabilities: PKMerchantCapability; cdecl;
    procedure setPaymentSummaryItems(paymentSummaryItems: NSArray); cdecl;
    function paymentSummaryItems: NSArray; cdecl;
    procedure setCurrencyCode(currencyCode: NSString); cdecl;
    function currencyCode: NSString; cdecl;
    procedure setRequiredBillingAddressFields(requiredBillingAddressFields: PKAddressField); cdecl;
    function requiredBillingAddressFields: PKAddressField; cdecl;
    procedure setBillingContact(billingContact: PKContact); cdecl;
    function billingContact: PKContact; cdecl;
    procedure setRequiredShippingAddressFields(requiredShippingAddressFields: PKAddressField); cdecl;
    function requiredShippingAddressFields: PKAddressField; cdecl;
    procedure setShippingContact(shippingContact: PKContact); cdecl;
    function shippingContact: PKContact; cdecl;
    procedure setShippingMethods(shippingMethods: NSArray); cdecl;
    function shippingMethods: NSArray; cdecl;
    procedure setShippingType(shippingType: PKShippingType); cdecl;
    function shippingType: PKShippingType; cdecl;
    procedure setApplicationData(applicationData: NSData); cdecl;
    function applicationData: NSData; cdecl;
    procedure setShippingAddress(shippingAddress: ABRecordRef); cdecl;
    function shippingAddress: ABRecordRef; cdecl;
    procedure setBillingAddress(billingAddress: ABRecordRef); cdecl;
    function billingAddress: ABRecordRef; cdecl;
  end;
  TPKPaymentRequest = class(TOCGenericImport<PKPaymentRequestClass, PKPaymentRequest>)
  end;

  PKPaymentMethodClass = interface(NSObjectClass)
    ['{7DDF3E1A-25B7-428E-B3B6-9DD38928D17A}']
  end;

  PKPaymentMethod = interface(NSObject)
    ['{84F34924-769D-484B-BDF1-44721C6DE079}']
    function displayName: NSString; cdecl;
    function network: PKPaymentNetwork; cdecl;
    function &type: PKPaymentMethodType; cdecl;
    function paymentPass: PKPaymentPass; cdecl;
  end;
  TPKPaymentMethod = class(TOCGenericImport<PKPaymentMethodClass, PKPaymentMethod>)
  end;

  PKPaymentTokenClass = interface(NSObjectClass)
    ['{4488B8AC-B28C-4191-BC80-641319969E51}']
  end;

  PKPaymentToken = interface(NSObject)
    ['{FC9FE201-3915-4F12-8B6B-04EFE72EEB26}']
    function paymentMethod: PKPaymentMethod; cdecl;
    function paymentInstrumentName: NSString; cdecl;
    function paymentNetwork: NSString; cdecl;
    function transactionIdentifier: NSString; cdecl;
    function paymentData: NSData; cdecl;
  end;
  TPKPaymentToken = class(TOCGenericImport<PKPaymentTokenClass, PKPaymentToken>)
  end;

  PKPaymentClass = interface(NSObjectClass)
    ['{FFA3BF8F-5602-4B61-B6CD-2A88435B0B10}']
  end;

  PKPayment = interface(NSObject)
    ['{B97F9417-6956-4AFA-BC6D-129215F161BE}']
    function token: PKPaymentToken; cdecl;
    function billingContact: PKContact; cdecl;
    function billingAddress: ABRecordRef; cdecl;
    function shippingContact: PKContact; cdecl;
    function shippingAddress: ABRecordRef; cdecl;
    function shippingMethod: PKShippingMethod; cdecl;
  end;
  TPKPayment = class(TOCGenericImport<PKPaymentClass, PKPayment>)
  end;

  PKPaymentAuthorizationViewControllerClass = interface(UIViewControllerClass)
    ['{84A4250E-9F02-4BF5-BBCD-0CA23D86B17B}']
    { class } function canMakePayments: Boolean; cdecl;
    [MethodName('canMakePaymentsUsingNetworks:')]
    { class } function canMakePaymentsUsingNetworks(supportedNetworks: NSArray): Boolean; cdecl;
    [MethodName('canMakePaymentsUsingNetworks:capabilities:')]
    { class } function canMakePaymentsUsingNetworksCapabilities(supportedNetworks: NSArray; capabilities: PKMerchantCapability): Boolean; cdecl;
  end;

  PKPaymentAuthorizationViewController = interface(UIViewController)
    ['{C40BA7AA-0997-4D66-8692-45B3CC5506DA}']
    procedure setDelegate(delegate: Pointer); cdecl;
    function delegate: Pointer; cdecl;
    function initWithPaymentRequest(request: PKPaymentRequest): Pointer; cdecl;
  end;
  TPKPaymentAuthorizationViewController = class(TOCGenericImport<PKPaymentAuthorizationViewControllerClass, PKPaymentAuthorizationViewController>)
  end;

  PKPaymentAuthorizationControllerClass = interface(NSObjectClass)
    ['{4E2C3337-1C33-438D-A28A-6DA543DC8B37}']
    { class } function canMakePayments: Boolean; cdecl;
    [MethodName('canMakePaymentsUsingNetworks:')]
    { class } function canMakePaymentsUsingNetworks(supportedNetworks: NSArray): Boolean; cdecl;
    [MethodName('canMakePaymentsUsingNetworks:capabilities:')]
    { class } function canMakePaymentsUsingNetworksCapabilities(supportedNetworks: NSArray; capabilities: PKMerchantCapability): Boolean; cdecl;
  end;

  PKPaymentAuthorizationController = interface(NSObject)
    ['{4C3647CC-C5F1-4D67-B378-D4E231E195A9}']
    procedure setDelegate(delegate: Pointer); cdecl;
    function delegate: Pointer; cdecl;
    function initWithPaymentRequest(request: PKPaymentRequest): Pointer; cdecl;
    procedure presentWithCompletion(completion: TPassKitCompletion5); cdecl;
    procedure dismissWithCompletion(completion: TPassKitCompletion6); cdecl;
  end;
  TPKPaymentAuthorizationController = class(TOCGenericImport<PKPaymentAuthorizationControllerClass, PKPaymentAuthorizationController>)
  end;

  PKAddPassButtonClass = interface(UIButtonClass)
    ['{0710D88E-05CE-4D0D-A5AE-F64A2DDA7AA4}']
    { class } function addPassButtonWithStyle(addPassButtonStyle: PKAddPassButtonStyle): Pointer; cdecl;
  end;

  PKAddPassButton = interface(UIButton)
    ['{6A378E5C-DFF4-4433-83D3-7D3C3D52C8D8}']
    function initWithAddPassButtonStyle(style: PKAddPassButtonStyle): Pointer; cdecl;
    procedure setAddPassButtonStyle(addPassButtonStyle: PKAddPassButtonStyle); cdecl;
    function addPassButtonStyle: PKAddPassButtonStyle; cdecl;
  end;
  TPKAddPassButton = class(TOCGenericImport<PKAddPassButtonClass, PKAddPassButton>)
  end;

  PKPaymentButtonClass = interface(UIButtonClass)
    ['{2351115F-B102-4236-A262-9EF44C751641}']
    { class } function buttonWithType(buttonType: PKPaymentButtonType;style: PKPaymentButtonStyle): Pointer; cdecl;
  end;

  PKPaymentButton = interface(UIButton)
    ['{DAD4A90F-FE0F-4C76-BDD8-E1F677C449AB}']
    function initWithPaymentButtonType(&type: PKPaymentButtonType; paymentButtonStyle: PKPaymentButtonStyle): Pointer; cdecl;
  end;
  TPKPaymentButton = class(TOCGenericImport<PKPaymentButtonClass, PKPaymentButton>)
  end;

  PKAddPassesViewControllerClass = interface(UIViewControllerClass)
    ['{1F12884D-7822-4E95-9CA9-D683673B2EE3}']
    { class } function canAddPasses: Boolean; cdecl;
  end;

  PKAddPassesViewController = interface(UIViewController)
    ['{7DF0C867-3B72-449A-A48A-7661DF89692C}']
    function initWithPass(pass: PKPass): Pointer; cdecl;
    function initWithPasses(passes: NSArray): Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    function delegate: Pointer; cdecl;
  end;
  TPKAddPassesViewController = class(TOCGenericImport<PKAddPassesViewControllerClass, PKAddPassesViewController>)
  end;

  PKAddPaymentPassViewControllerClass = interface(UIViewControllerClass)
    ['{0EBA8E12-838C-461E-A529-1B8E803FC092}']
    { class } function canAddPaymentPass: Boolean; cdecl;
  end;

  PKAddPaymentPassViewController = interface(UIViewController)
    ['{E9427793-2BDA-4155-8E7A-85D89508E1CE}']
    function initWithRequestConfiguration(configuration: PKAddPaymentPassRequestConfiguration; delegate: Pointer): Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    function delegate: Pointer; cdecl;
  end;
  TPKAddPaymentPassViewController = class(TOCGenericImport<PKAddPaymentPassViewControllerClass, PKAddPaymentPassViewController>)
  end;

  PKAddPaymentPassRequestConfigurationClass = interface(NSObjectClass)
    ['{68A2BD7E-BDDB-4FC9-9B7E-526B511D7727}']
  end;

  PKAddPaymentPassRequestConfiguration = interface(NSObject)
    ['{275F2AFC-C3FD-4015-8BCF-CD4F2EF56341}']
    function initWithEncryptionScheme(encryptionScheme: PKEncryptionScheme): Pointer; cdecl;
    function encryptionScheme: PKEncryptionScheme; cdecl;
    procedure setCardholderName(cardholderName: NSString); cdecl;
    function cardholderName: NSString; cdecl;
    procedure setPrimaryAccountSuffix(primaryAccountSuffix: NSString); cdecl;
    function primaryAccountSuffix: NSString; cdecl;
    procedure setCardDetails(cardDetails: NSArray); cdecl;
    function cardDetails: NSArray; cdecl;
    procedure setLocalizedDescription(localizedDescription: NSString); cdecl;
    function localizedDescription: NSString; cdecl;
    procedure setPrimaryAccountIdentifier(primaryAccountIdentifier: NSString); cdecl;
    function primaryAccountIdentifier: NSString; cdecl;
    procedure setPaymentNetwork(paymentNetwork: PKPaymentNetwork); cdecl;
    function paymentNetwork: PKPaymentNetwork; cdecl;
    procedure setRequiresFelicaSecureElement(requiresFelicaSecureElement: Boolean); cdecl;
    function requiresFelicaSecureElement: Boolean; cdecl;
  end;
  TPKAddPaymentPassRequestConfiguration = class(TOCGenericImport<PKAddPaymentPassRequestConfigurationClass, PKAddPaymentPassRequestConfiguration>)
  end;

  PKAddPaymentPassRequestClass = interface(NSObjectClass)
    ['{A58A10EA-66C2-408C-9FC2-AB5E13A9C782}']
  end;

  PKAddPaymentPassRequest = interface(NSObject)
    ['{A03F25BA-AB6B-4050-AA93-4D65F053E9BA}']
    function init: Pointer; cdecl;
    procedure setEncryptedPassData(encryptedPassData: NSData); cdecl;
    function encryptedPassData: NSData; cdecl;
    procedure setActivationData(activationData: NSData); cdecl;
    function activationData: NSData; cdecl;
    procedure setEphemeralPublicKey(ephemeralPublicKey: NSData); cdecl;
    function ephemeralPublicKey: NSData; cdecl;
    procedure setWrappedKey(wrappedKey: NSData); cdecl;
    function wrappedKey: NSData; cdecl;
  end;
  TPKAddPaymentPassRequest = class(TOCGenericImport<PKAddPaymentPassRequestClass, PKAddPaymentPassRequest>)
  end;

  PKPaymentAuthorizationViewControllerDelegate = interface(IObjectiveC)
    ['{4CD9FEDC-FC71-415D-A11C-AD1F39BE2F1D}']
    [MethodName('paymentAuthorizationViewController:didAuthorizePayment:completion:')]
    procedure paymentAuthorizationViewControllerDidAuthorizePaymentCompletion(controller: PKPaymentAuthorizationViewController;
      didAuthorizePayment: PKPayment; completion: TPassKitCompletion1); cdecl;
    procedure paymentAuthorizationViewControllerDidFinish(controller: PKPaymentAuthorizationViewController); cdecl;
    procedure paymentAuthorizationViewControllerWillAuthorizePayment(controller: PKPaymentAuthorizationViewController); cdecl;
    [MethodName('paymentAuthorizationViewController:didSelectShippingMethod:completion:')]
    procedure paymentAuthorizationViewControllerDidSelectShippingMethodCompletion(controller: PKPaymentAuthorizationViewController;
      didSelectShippingMethod: PKShippingMethod; completion: TPassKitCompletion2); cdecl;
    [MethodName('paymentAuthorizationViewController:didSelectShippingAddress:completion:')]
    procedure paymentAuthorizationViewControllerDidSelectShippingAddressCompletion(controller: PKPaymentAuthorizationViewController;
      didSelectShippingAddress: ABRecordRef; completion: TPassKitCompletion3); cdecl;
    [MethodName('paymentAuthorizationViewController:didSelectShippingContact:completion:')]
    procedure paymentAuthorizationViewControllerDidSelectShippingContactCompletion(controller: PKPaymentAuthorizationViewController;
      didSelectShippingContact: PKContact; completion: TPassKitCompletion3); cdecl;
    [MethodName('paymentAuthorizationViewController:didSelectPaymentMethod:completion:')]
    procedure paymentAuthorizationViewControllerDidSelectPaymentMethodCompletion(controller: PKPaymentAuthorizationViewController;
      didSelectPaymentMethod: PKPaymentMethod; completion: TPassKitCompletion4); cdecl;
  end;

  PKPaymentAuthorizationControllerDelegate = interface(IObjectiveC)
    ['{A1781F2B-47B7-46DE-A6D2-904824CB45B2}']
    [MethodName('paymentAuthorizationController:didAuthorizePayment:completion:')]
    procedure paymentAuthorizationControllerDidAuthorizePaymentCompletion(controller: PKPaymentAuthorizationController;
      didAuthorizePayment: PKPayment; completion: TPassKitCompletion1); cdecl;
    procedure paymentAuthorizationControllerDidFinish(controller: PKPaymentAuthorizationController); cdecl;
    procedure paymentAuthorizationControllerWillAuthorizePayment(controller: PKPaymentAuthorizationController); cdecl;
    [MethodName('paymentAuthorizationController:didSelectShippingMethod:completion:')]
    procedure paymentAuthorizationControllerDidSelectShippingMethodCompletion(controller: PKPaymentAuthorizationController;
      didSelectShippingMethod: PKShippingMethod; completion: TPassKitCompletion2); cdecl;
    [MethodName('paymentAuthorizationController:didSelectShippingContact:completion:')]
    procedure paymentAuthorizationControllerDidSelectShippingContactCompletion(controller: PKPaymentAuthorizationController;
      didSelectShippingContact: PKContact; completion: TPassKitCompletion3); cdecl;
    [MethodName('paymentAuthorizationController:didSelectPaymentMethod:completion:')]
    procedure paymentAuthorizationControllerDidSelectPaymentMethodCompletion(controller: PKPaymentAuthorizationController;
      didSelectPaymentMethod: PKPaymentMethod; completion: TPassKitCompletion4); cdecl;
  end;

  PKAddPassesViewControllerDelegate = interface(IObjectiveC)
    ['{46ECAC5A-31D0-43DA-8F49-EDBC96946B52}']
    procedure addPassesViewControllerDidFinish(controller: PKAddPassesViewController); cdecl;
  end;

  PKAddPaymentPassViewControllerDelegate = interface(IObjectiveC)
    ['{09C5287C-BB63-4C1E-8FF7-64521C688CF7}']
    [MethodName('addPaymentPassViewController:generateRequestWithCertificateChain:nonce:nonceSignature:completionHandler:')]
    procedure addPaymentPassViewControllerGenerateRequestWithCertificateChainNonceNonceSignatureCompletionHandler(
      controller: PKAddPaymentPassViewController; generateRequestWithCertificateChain: NSArray; nonce: NSData; nonceSignature: NSData;
      completionHandler: TPassKitCompletionHandler); cdecl;
    [MethodName('addPaymentPassViewController:didFinishAddingPaymentPass:error:')]
    procedure addPaymentPassViewControllerDidFinishAddingPaymentPassError(controller: PKAddPaymentPassViewController;
      didFinishAddingPaymentPass: PKPaymentPass; error: NSError); cdecl;
  end;

function PKEncryptionSchemeECC_V2: Pointer;
function PKEncryptionSchemeRSA_V2: Pointer;
function PKPaymentNetworkAmex: Pointer;
function PKPaymentNetworkCarteBancaire: Pointer;
function PKPaymentNetworkChinaUnionPay: Pointer;
function PKPaymentNetworkDiscover: Pointer;
function PKPaymentNetworkInterac: Pointer;
function PKPaymentNetworkMasterCard: Pointer;
function PKPaymentNetworkPrivateLabel: Pointer;
function PKPaymentNetworkVisa: Pointer;
function PKPaymentNetworkJCB: Pointer;
function PKPaymentNetworkSuica: Pointer;
function PKPaymentNetworkQuicPay: Pointer;
function PKPaymentNetworkIDCredit: Pointer;
function PKPassKitErrorDomain: NSString;
function PKPassLibraryDidChangeNotification: Pointer;
function PKPassLibraryRemotePaymentPassesDidChangeNotification: Pointer;
function PKPassLibraryAddedPassesUserInfoKey: Pointer;
function PKPassLibraryReplacementPassesUserInfoKey: Pointer;
function PKPassLibraryRemovedPassInfosUserInfoKey: Pointer;
function PKPassLibraryPassTypeIdentifierUserInfoKey: Pointer;
function PKPassLibrarySerialNumberUserInfoKey: Pointer;

const
  libPassKit = '/System/Library/Frameworks/PassKit.framework/PassKit';

implementation

{$IF defined(IOS) and NOT defined(CPUARM)}
uses
  Posix.Dlfcn;

var
  PassKitModule: THandle;
{$ENDIF IOS}

function PKPassKitErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libPassKit, 'PKPassKitErrorDomain');
end;

function PKEncryptionSchemeECC_V2: Pointer;
begin
  Result := CocoaPointerConst(libPassKit, 'PKEncryptionSchemeECC_V2');
end;

function PKEncryptionSchemeRSA_V2: Pointer;
begin
  Result := CocoaPointerConst(libPassKit, 'PKEncryptionSchemeRSA_V2');
end;

function PKPaymentNetworkAmex: Pointer;
begin
  Result := CocoaPointerConst(libPassKit, 'PKPaymentNetworkAmex');
end;

function PKPaymentNetworkCarteBancaire: Pointer;
begin
  Result := CocoaPointerConst(libPassKit, 'PKPaymentNetworkCarteBancaire');
end;

function PKPaymentNetworkChinaUnionPay: Pointer;
begin
  Result := CocoaPointerConst(libPassKit, 'PKPaymentNetworkChinaUnionPay');
end;

function PKPaymentNetworkDiscover: Pointer;
begin
  Result := CocoaPointerConst(libPassKit, 'PKPaymentNetworkDiscover');
end;

function PKPaymentNetworkInterac: Pointer;
begin
  Result := CocoaPointerConst(libPassKit, 'PKPaymentNetworkInterac');
end;

function PKPaymentNetworkMasterCard: Pointer;
begin
  Result := CocoaPointerConst(libPassKit, 'PKPaymentNetworkMasterCard');
end;

function PKPaymentNetworkPrivateLabel: Pointer;
begin
  Result := CocoaPointerConst(libPassKit, 'PKPaymentNetworkPrivateLabel');
end;

function PKPaymentNetworkVisa: Pointer;
begin
  Result := CocoaPointerConst(libPassKit, 'PKPaymentNetworkVisa');
end;

function PKPaymentNetworkJCB: Pointer;
begin
  Result := CocoaPointerConst(libPassKit, 'PKPaymentNetworkJCB');
end;

function PKPaymentNetworkSuica: Pointer;
begin
  Result := CocoaPointerConst(libPassKit, 'PKPaymentNetworkSuica');
end;

function PKPaymentNetworkQuicPay: Pointer;
begin
  Result := CocoaPointerConst(libPassKit, 'PKPaymentNetworkQuicPay');
end;

function PKPaymentNetworkIDCredit: Pointer;
begin
  Result := CocoaPointerConst(libPassKit, 'PKPaymentNetworkIDCredit');
end;

function PKPassLibraryDidChangeNotification: Pointer;
begin
  Result := CocoaPointerConst(libPassKit, 'PKPassLibraryDidChangeNotification');
end;

function PKPassLibraryRemotePaymentPassesDidChangeNotification: Pointer;
begin
  Result := CocoaPointerConst(libPassKit, 'PKPassLibraryRemotePaymentPassesDidChangeNotification');
end;

function PKPassLibraryAddedPassesUserInfoKey: Pointer;
begin
  Result := CocoaPointerConst(libPassKit, 'PKPassLibraryAddedPassesUserInfoKey');
end;

function PKPassLibraryReplacementPassesUserInfoKey: Pointer;
begin
  Result := CocoaPointerConst(libPassKit, 'PKPassLibraryReplacementPassesUserInfoKey');
end;

function PKPassLibraryRemovedPassInfosUserInfoKey: Pointer;
begin
  Result := CocoaPointerConst(libPassKit, 'PKPassLibraryRemovedPassInfosUserInfoKey');
end;

function PKPassLibraryPassTypeIdentifierUserInfoKey: Pointer;
begin
  Result := CocoaPointerConst(libPassKit, 'PKPassLibraryPassTypeIdentifierUserInfoKey');
end;

function PKPassLibrarySerialNumberUserInfoKey: Pointer;
begin
  Result := CocoaPointerConst(libPassKit, 'PKPassLibrarySerialNumberUserInfoKey');
end;

{$IF defined(IOS) and NOT defined(CPUARM)}
initialization
  PassKitModule := dlopen(MarshaledAString(libPassKit), RTLD_LAZY);

finalization
  dlclose(PassKitModule);
{$ENDIF IOS}

end.
