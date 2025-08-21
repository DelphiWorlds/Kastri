unit DW.iOSapi.FirebaseAuth;

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
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit,
  // DW
  DW.iOSapi.FirebaseCore;

const
  FIRAuthErrorCodeInvalidCustomToken = 17000;
  FIRAuthErrorCodeCustomTokenMismatch = 17002;
  FIRAuthErrorCodeInvalidCredential = 17004;
  FIRAuthErrorCodeUserDisabled = 17005;
  FIRAuthErrorCodeOperationNotAllowed = 17006;
  FIRAuthErrorCodeEmailAlreadyInUse = 17007;
  FIRAuthErrorCodeInvalidEmail = 17008;
  FIRAuthErrorCodeWrongPassword = 17009;
  FIRAuthErrorCodeTooManyRequests = 17010;
  FIRAuthErrorCodeUserNotFound = 17011;
  FIRAuthErrorCodeAccountExistsWithDifferentCredential = 17012;
  FIRAuthErrorCodeRequiresRecentLogin = 17014;
  FIRAuthErrorCodeProviderAlreadyLinked = 17015;
  FIRAuthErrorCodeNoSuchProvider = 17016;
  FIRAuthErrorCodeInvalidUserToken = 17017;
  FIRAuthErrorCodeNetworkError = 17020;
  FIRAuthErrorCodeUserTokenExpired = 17021;
  FIRAuthErrorCodeInvalidAPIKey = 17023;
  FIRAuthErrorCodeUserMismatch = 17024;
  FIRAuthErrorCodeCredentialAlreadyInUse = 17025;
  FIRAuthErrorCodeWeakPassword = 17026;
  FIRAuthErrorCodeAppNotAuthorized = 17028;
  FIRAuthErrorCodeExpiredActionCode = 17029;
  FIRAuthErrorCodeInvalidActionCode = 17030;
  FIRAuthErrorCodeInvalidMessagePayload = 17031;
  FIRAuthErrorCodeInvalidSender = 17032;
  FIRAuthErrorCodeInvalidRecipientEmail = 17033;
  FIRAuthErrorCodeMissingEmail = 17034;
  FIRAuthErrorCodeMissingIosBundleID = 17036;
  FIRAuthErrorCodeMissingAndroidPackageName = 17037;
  FIRAuthErrorCodeUnauthorizedDomain = 17038;
  FIRAuthErrorCodeInvalidContinueURI = 17039;
  FIRAuthErrorCodeMissingContinueURI = 17040;
  FIRAuthErrorCodeMissingPhoneNumber = 17041;
  FIRAuthErrorCodeInvalidPhoneNumber = 17042;
  FIRAuthErrorCodeMissingVerificationCode = 17043;
  FIRAuthErrorCodeInvalidVerificationCode = 17044;
  FIRAuthErrorCodeMissingVerificationID = 17045;
  FIRAuthErrorCodeInvalidVerificationID = 17046;
  FIRAuthErrorCodeMissingAppCredential = 17047;
  FIRAuthErrorCodeInvalidAppCredential = 17048;
  FIRAuthErrorCodeSessionExpired = 17051;
  FIRAuthErrorCodeQuotaExceeded = 17052;
  FIRAuthErrorCodeMissingAppToken = 17053;
  FIRAuthErrorCodeNotificationNotForwarded = 17054;
  FIRAuthErrorCodeAppNotVerified = 17055;
  FIRAuthErrorCodeCaptchaCheckFailed = 17056;
  FIRAuthErrorCodeWebContextAlreadyPresented = 17057;
  FIRAuthErrorCodeWebContextCancelled = 17058;
  FIRAuthErrorCodeAppVerificationUserInteractionFailure = 17059;
  FIRAuthErrorCodeInvalidClientID = 17060;
  FIRAuthErrorCodeWebNetworkRequestFailed = 17061;
  FIRAuthErrorCodeWebInternalError = 17062;
  FIRAuthErrorCodeWebSignInUserInteractionFailure = 17063;
  FIRAuthErrorCodeLocalPlayerNotAuthenticated = 17066;
  FIRAuthErrorCodeNullUser = 17067;
  FIRAuthErrorCodeDynamicLinkNotActivated = 17068;
  FIRAuthErrorCodeInvalidProviderID = 17071;
  FIRAuthErrorCodeTenantIDMismatch = 17072;
  FIRAuthErrorCodeUnsupportedTenantOperation = 17073;
  FIRAuthErrorCodeInvalidDynamicLinkDomain = 17074;
  FIRAuthErrorCodeRejectedCredential = 17075;
  FIRAuthErrorCodeGameKitNotLinked = 17076;
  FIRAuthErrorCodeSecondFactorRequired = 17078;
  FIRAuthErrorCodeMissingMultiFactorSession = 17081;
  FIRAuthErrorCodeMissingMultiFactorInfo = 17082;
  FIRAuthErrorCodeInvalidMultiFactorSession = 17083;
  FIRAuthErrorCodeMultiFactorInfoNotFound = 17084;
  FIRAuthErrorCodeAdminRestrictedOperation = 17085;
  FIRAuthErrorCodeUnverifiedEmail = 17086;
  FIRAuthErrorCodeSecondFactorAlreadyEnrolled = 17087;
  FIRAuthErrorCodeMaximumSecondFactorCountExceeded = 17088;
  FIRAuthErrorCodeUnsupportedFirstFactor = 17089;
  FIRAuthErrorCodeEmailChangeNeedsVerification = 17090;
  FIRAuthErrorCodeMissingOrInvalidNonce = 17094;
  FIRAuthErrorCodeMissingClientIdentifier = 17993;
  FIRAuthErrorCodeKeychainError = 17995;
  FIRAuthErrorCodeInternalError = 17999;
  FIRAuthErrorCodeMalformedJWT = 18000;
  FIRAuthAPNSTokenTypeUnknown = 0;
  FIRAuthAPNSTokenTypeSandbox = 1;
  FIRAuthAPNSTokenTypeProd = 2;
  FIRActionCodeOperationUnknown = 0;
  FIRActionCodeOperationPasswordReset = 1;
  FIRActionCodeOperationVerifyEmail = 2;
  FIRActionCodeOperationRecoverEmail = 3;
  FIRActionCodeOperationEmailLink = 4;
  FIRActionCodeOperationVerifyAndChangeEmail = 5;
  FIRActionCodeOperationRevertSecondFactorAddition = 6;

type
  FIRActionCodeInfo = interface;
  FIRActionCodeURL = interface;
  FIRAuth = interface;
  FIRAuthDataResult = interface;
  FIRMultiFactorAssertion = interface;
  FIRMultiFactorInfo = interface;
  FIRMultiFactorSession = interface;
  FIRMultiFactor = interface;
  FIRUserInfo = interface;
  FIRUser = interface;
  FIRUserProfileChangeRequest = interface;
  FIRGitHubAuthProvider = interface;
  FIRAuthCredential = interface;
  FIROAuthCredential = interface;
  FIRGameCenterAuthProvider = interface;
  FIRPhoneAuthCredential = interface;
  FIRAuthUIDelegate = interface;
  FIRFederatedAuthProvider = interface;
  FIREmailAuthProvider = interface;
  FIRFacebookAuthProvider = interface;
  FIRActionCodeSettings = interface;
  FIRAdditionalUserInfo = interface;
  FIRAuthTokenResult = interface;
  FIRGoogleAuthProvider = interface;
  FIRMultiFactorResolver = interface;
  FIROAuthProvider = interface;
  FIRTwitterAuthProvider = interface;
  FIRUserMetadata = interface;
  FIRAuthSettings = interface;
  FIRPhoneAuthProvider = interface;
  FIRPhoneMultiFactorAssertion = interface;
  FIRPhoneMultiFactorGenerator = interface;
  FIRPhoneMultiFactorInfo = interface;

  FIRAuthErrorCode = NSInteger;
  FIRAuthAPNSTokenType = NSInteger;

  FIRUserUpdateCallback = procedure(error: NSError) of object;
  FIRAuthStateDidChangeListenerHandle = Pointer;

  FIRAuthStateDidChangeListenerBlock = procedure(auth: FIRAuth; user: FIRUser) of object;
  FIRIDTokenDidChangeListenerHandle = Pointer;

  FIRIDTokenDidChangeListenerBlock = procedure(auth: FIRAuth; user: FIRUser) of object;

  FIRAuthDataResultCallback = procedure(authResult: FIRAuthDataResult; error: NSError) of object;

  FIRAuthResultCallback = procedure(user: FIRUser; error: NSError) of object;

  FIRProviderQueryCallback = procedure(providers: NSArray; error: NSError) of object;

  FIRSignInMethodQueryCallback = procedure(p1: NSArray; p2: NSError) of object;

  FIRSendPasswordResetCallback = procedure(error: NSError) of object;

  FIRSendSignInLinkToEmailCallback = procedure(error: NSError) of object;

  FIRConfirmPasswordResetCallback = procedure(error: NSError) of object;

  FIRVerifyPasswordResetCodeCallback = procedure(email: NSString; error: NSError) of object;

  FIRApplyActionCodeCallback = procedure(error: NSError) of object;

  FIRAuthVoidErrorCallback = procedure(p1: NSError) of object;
  FIRActionCodeOperation = NSInteger;

  FIRCheckActionCodeCallBack = procedure(info: FIRActionCodeInfo; error: NSError) of object;

  FIRMultiFactorSessionCallback = procedure(session: FIRMultiFactorSession; error: NSError) of object;

  FIRAuthTokenCallback = procedure(token: NSString; error: NSError) of object;

  FIRAuthTokenResultCallback = procedure(tokenResult: FIRAuthTokenResult; error: NSError) of object;

  FIRUserProfileChangeCallback = procedure(error: NSError) of object;

  FIRSendEmailVerificationCallback = procedure(error: NSError) of object;

  FIRGameCenterCredentialCallback = procedure(credential: FIRAuthCredential; error: NSError) of object;

  FIRAuthCredentialCallback = procedure(credential: FIRAuthCredential; error: NSError) of object;

  FIRVerificationResultCallback = procedure(verificationID: NSString; error: NSError) of object;
  TFIRAuthBlockMethod1 = procedure(error: NSError) of object;
  TFIRAuthBlockMethod2 = procedure(param1: NSArray; param2: NSError) of object;
  TFIRAuthBlockMethod3 = procedure(authResult: FIRAuthDataResult; error: NSError) of object;
  TFIRAuthBlockMethod4 = procedure(email: NSString; error: NSError) of object;
  TFIRAuthBlockMethod5 = procedure(auth: FIRAuth; user: FIRUser) of object;
  TFIRMultiFactorBlockMethod1 = procedure(credential: FIRMultiFactorSession; error: NSError) of object;
  TFIRMultiFactorBlockMethod2 = procedure(error: NSError) of object;
  TFIRUserBlockMethod1 = procedure(error: NSError) of object;
  TFIRUserBlockMethod2 = procedure(authResult: FIRAuthDataResult; error: NSError) of object;
  TFIRUserBlockMethod3 = procedure(tokenResult: FIRAuthTokenResult; error: NSError) of object;
  TFIRUserBlockMethod4 = procedure(token: NSString; error: NSError) of object;
  TFIRUserBlockMethod5 = procedure(user: FIRUser; error: NSError) of object;
  TFIRUserProfileChangeRequestBlockMethod1 = procedure(error: NSError) of object;
  TFIRGameCenterAuthProviderBlockMethod1 = procedure(credential: FIRAuthCredential; error: NSError) of object;
  TFIRAuthUIDelegateBlockMethod1 = procedure of object;
  TFIRFederatedAuthProviderBlockMethod1 = procedure(credential: FIRAuthCredential; error: NSError) of object;
  TFIRPhoneAuthProviderBlockMethod1 = procedure(verificationID: NSString; error: NSError) of object;

  FIRActionCodeInfoClass = interface(NSObjectClass)
    ['{B8A40272-7CE1-48EA-8723-E46D2E07DE25}']
  end;

  FIRActionCodeInfo = interface(NSObject)
    ['{2D207CDE-F902-4971-9828-89ADE713F6AF}']
    function email: NSString; cdecl;
    function operation: FIRActionCodeOperation; cdecl;
    function previousEmail: NSString; cdecl;
  end;
  TFIRActionCodeInfo = class(TOCGenericImport<FIRActionCodeInfoClass, FIRActionCodeInfo>) end;

  FIRActionCodeURLClass = interface(NSObjectClass)
    ['{35DB2189-D604-4F18-9422-AEEDDEE723D4}']
    {class} function actionCodeURLWithLink(link: NSString): Pointer; cdecl;
  end;

  FIRActionCodeURL = interface(NSObject)
    ['{A162CA41-2825-4511-B77D-8DE9F15AFC26}']
    function APIKey: NSString; cdecl;
    function code: NSString; cdecl;
    function continueURL: NSURL; cdecl;
    function languageCode: NSString; cdecl;
    function operation: FIRActionCodeOperation; cdecl;
  end;
  TFIRActionCodeURL = class(TOCGenericImport<FIRActionCodeURLClass, FIRActionCodeURL>) end;

  FIRAuthClass = interface(NSObjectClass)
    ['{5A3AE063-E3D7-4077-BB24-D7592FFE8B05}']
    {class} function auth: FIRAuth; cdecl;
    {class} function authWithApp(app: FIRApp): FIRAuth; cdecl;
  end;

  FIRAuth = interface(NSObject)
    ['{9BA62F17-AF87-4814-A5D8-02386F115AAA}']
    function addAuthStateDidChangeListener(listener: TFIRAuthBlockMethod5): FIRAuthStateDidChangeListenerHandle; cdecl;
    function addIDTokenDidChangeListener(listener: TFIRAuthBlockMethod5): FIRIDTokenDidChangeListenerHandle; cdecl;
    function APNSToken: NSData; cdecl;
    function app: FIRApp; cdecl;
    procedure applyActionCode(code: NSString; completion: TFIRAuthBlockMethod1); cdecl;
    function canHandleNotification(userInfo: NSDictionary): Boolean; cdecl;
    function canHandleURL(URL: NSURL): Boolean; cdecl;
    procedure checkActionCode(code: NSString; completion: FIRCheckActionCodeCallBack); cdecl;
    procedure confirmPasswordResetWithCode(code: NSString; newPassword: NSString; completion: TFIRAuthBlockMethod1); cdecl;
    procedure createUserWithEmail(email: NSString; password: NSString; completion: TFIRAuthBlockMethod3); cdecl;
    function currentUser: FIRUser; cdecl;
    procedure fetchSignInMethodsForEmail(email: NSString; completion: TFIRAuthBlockMethod2); cdecl;
    function getStoredUserForAccessGroup(accessGroup: NSString; error: PPointer): FIRUser; cdecl;
    function isSignInWithEmailLink(link: NSString): Boolean; cdecl;
    function languageCode: NSString; cdecl;
    procedure removeAuthStateDidChangeListener(listenerHandle: FIRAuthStateDidChangeListenerHandle); cdecl;
    procedure removeIDTokenDidChangeListener(listenerHandle: FIRIDTokenDidChangeListenerHandle); cdecl;
    procedure sendPasswordResetWithEmail(email: NSString; completion: TFIRAuthBlockMethod1); overload; cdecl;
    procedure sendPasswordResetWithEmail(email: NSString; actionCodeSettings: FIRActionCodeSettings; completion: TFIRAuthBlockMethod1); overload; cdecl;
    procedure sendSignInLinkToEmail(email: NSString; actionCodeSettings: FIRActionCodeSettings; completion: TFIRAuthBlockMethod1); cdecl;
    procedure setAPNSToken(token: NSData; &type: FIRAuthAPNSTokenType); overload; cdecl;
    procedure setAPNSToken(APNSToken: NSData); overload; cdecl;
    procedure setLanguageCode(languageCode: NSString); cdecl;
    procedure setSettings(settings: FIRAuthSettings); cdecl;
    procedure setTenantID(tenantID: NSString); cdecl;
    function settings: FIRAuthSettings; cdecl;
    procedure signInAnonymouslyWithCompletion(completion: TFIRAuthBlockMethod3); cdecl;
    procedure signInWithCredential(credential: FIRAuthCredential; completion: TFIRAuthBlockMethod3); cdecl;
    procedure signInWithCustomToken(token: NSString; completion: TFIRAuthBlockMethod3); cdecl;
    [MethodName('signInWithEmail:link:completion:')]
    procedure signInWithEmailLink(email: NSString; link: NSString; completion: TFIRAuthBlockMethod3); cdecl;
    [MethodName('signInWithEmail:password:completion:')]
    procedure signInWithEmailPassword(email: NSString; password: NSString; completion: TFIRAuthBlockMethod3); cdecl;
    procedure signInWithProvider(provider: Pointer; UIDelegate: Pointer; completion: TFIRAuthBlockMethod3); cdecl;
    function signOut(error: PPointer): Boolean; cdecl;
    function tenantID: NSString; cdecl;
    procedure updateCurrentUser(user: FIRUser; completion: TFIRAuthBlockMethod1); cdecl;
    procedure useAppLanguage; cdecl;
    procedure useEmulatorWithHost(host: NSString; port: NSInteger); cdecl;
    function userAccessGroup: NSString; cdecl;
    function useUserAccessGroup(accessGroup: NSString; error: PPointer): Boolean; cdecl;
    procedure verifyPasswordResetCode(code: NSString; completion: TFIRAuthBlockMethod4); cdecl;
  end;
  TFIRAuth = class(TOCGenericImport<FIRAuthClass, FIRAuth>) end;

  FIRAuthDataResultClass = interface(NSObjectClass)
    ['{71E58638-A452-48E0-B675-D1B45B0E2FCA}']
  end;

  FIRAuthDataResult = interface(NSObject)
    ['{ED6E9D40-8F04-46CB-8D11-990F65000B4C}']
    function additionalUserInfo: FIRAdditionalUserInfo; cdecl;
    function credential: FIRAuthCredential; cdecl;
    function user: FIRUser; cdecl;
  end;
  TFIRAuthDataResult = class(TOCGenericImport<FIRAuthDataResultClass, FIRAuthDataResult>) end;

  FIRMultiFactorAssertionClass = interface(NSObjectClass)
    ['{04959579-A694-4195-AD8A-CA54C2726C1A}']
  end;

  FIRMultiFactorAssertion = interface(NSObject)
    ['{F9B15794-F115-48F2-A660-05FC47C17B9D}']
    function factorID: NSString; cdecl;
  end;
  TFIRMultiFactorAssertion = class(TOCGenericImport<FIRMultiFactorAssertionClass, FIRMultiFactorAssertion>) end;

  FIRMultiFactorInfoClass = interface(NSObjectClass)
    ['{DF322DC9-729D-42DE-9763-9BA3218679BA}']
  end;

  FIRMultiFactorInfo = interface(NSObject)
    ['{EDD43A86-14D4-4A7C-B6AD-9870535CBFB9}']
    function displayName: NSString; cdecl;
    function enrollmentDate: NSDate; cdecl;
    function factorID: NSString; cdecl;
    function UID: NSString; cdecl;
  end;
  TFIRMultiFactorInfo = class(TOCGenericImport<FIRMultiFactorInfoClass, FIRMultiFactorInfo>) end;

  FIRMultiFactorSessionClass = interface(NSObjectClass)
    ['{E5E10CF8-1EB7-4B28-9A39-8ED47E0BD72B}']
  end;

  FIRMultiFactorSession = interface(NSObject)
    ['{1B9E1CBD-8611-40AD-98D4-95BE00CC4CFF}']
  end;
  TFIRMultiFactorSession = class(TOCGenericImport<FIRMultiFactorSessionClass, FIRMultiFactorSession>) end;

  FIRMultiFactorClass = interface(NSObjectClass)
    ['{54DFFCE3-E217-46CF-93B1-639E03B8A976}']
  end;

  FIRMultiFactor = interface(NSObject)
    ['{0F4B68F2-5F25-4511-BEEF-FFA2C745CBCE}']
    function enrolledFactors: NSArray; cdecl;
    procedure enrollWithAssertion(assertion: FIRMultiFactorAssertion; displayName: NSString; completion: TFIRMultiFactorBlockMethod2); cdecl;
    procedure getSessionWithCompletion(completion: TFIRMultiFactorBlockMethod1); cdecl;
    procedure unenrollWithFactorUID(factorUID: NSString; completion: TFIRMultiFactorBlockMethod2); cdecl;
    procedure unenrollWithInfo(factorInfo: FIRMultiFactorInfo; completion: TFIRMultiFactorBlockMethod2); cdecl;
  end;
  TFIRMultiFactor = class(TOCGenericImport<FIRMultiFactorClass, FIRMultiFactor>) end;

  FIRUserInfo = interface(IObjectiveC)
    ['{FE92460D-F9A8-4579-86A8-5519C5C0E535}']
    function displayName: NSString; cdecl;
    function email: NSString; cdecl;
    function phoneNumber: NSString; cdecl;
    function photoURL: NSURL; cdecl;
    function providerID: NSString; cdecl;
    function uid: NSString; cdecl;
  end;

  FIRUserClass = interface(NSObjectClass)
    ['{1F789680-E74E-4E94-A02B-E44DD926F6C1}']
  end;

  FIRUser = interface(NSObject)
    ['{965C7032-3D62-44C6-9E6B-1F3042B9B0A2}']
    procedure deleteWithCompletion(completion: TFIRUserBlockMethod1); cdecl;
    procedure getIDTokenForcingRefresh(forceRefresh: Boolean; completion: TFIRUserBlockMethod4); cdecl;
    procedure getIDTokenResultForcingRefresh(forceRefresh: Boolean; completion: TFIRUserBlockMethod3); cdecl;
    procedure getIDTokenResultWithCompletion(completion: TFIRUserBlockMethod3); cdecl;
    procedure getIDTokenWithCompletion(completion: TFIRUserBlockMethod4); cdecl;
    function isAnonymous: Boolean; cdecl;
    function isEmailVerified: Boolean; cdecl;
    procedure linkWithCredential(credential: FIRAuthCredential; completion: TFIRUserBlockMethod2); cdecl;
    procedure linkWithProvider(provider: Pointer; UIDelegate: Pointer; completion: TFIRUserBlockMethod2); cdecl;
    function metadata: FIRUserMetadata; cdecl;
    function multiFactor: FIRMultiFactor; cdecl;
    function profileChangeRequest: FIRUserProfileChangeRequest; cdecl;
    function providerData: NSArray; cdecl;
    procedure reauthenticateWithCredential(credential: FIRAuthCredential; completion: TFIRUserBlockMethod2); cdecl;
    procedure reauthenticateWithProvider(provider: Pointer; UIDelegate: Pointer; completion: TFIRUserBlockMethod2); cdecl;
    function refreshToken: NSString; cdecl;
    procedure reloadWithCompletion(completion: TFIRUserBlockMethod1); cdecl;
    procedure sendEmailVerificationBeforeUpdatingEmail(email: NSString; actionCodeSettings: FIRActionCodeSettings; completion: TFIRUserBlockMethod1); overload; cdecl;
    procedure sendEmailVerificationBeforeUpdatingEmail(email: NSString; completion: TFIRUserBlockMethod1); overload; cdecl;
    procedure sendEmailVerificationWithActionCodeSettings(actionCodeSettings: FIRActionCodeSettings; completion: TFIRUserBlockMethod1); cdecl;
    procedure sendEmailVerificationWithCompletion(completion: TFIRUserBlockMethod1); cdecl;
    function tenantID: NSString; cdecl;
    procedure unlinkFromProvider(provider: NSString; completion: TFIRUserBlockMethod5); cdecl;
    procedure updateEmail(email: NSString; completion: TFIRUserBlockMethod1); cdecl;
    procedure updatePassword(password: NSString; completion: TFIRUserBlockMethod1); cdecl;
    procedure updatePhoneNumberCredential(phoneNumberCredential: FIRPhoneAuthCredential; completion: TFIRUserBlockMethod1); cdecl;
  end;
  TFIRUser = class(TOCGenericImport<FIRUserClass, FIRUser>) end;

  FIRUserProfileChangeRequestClass = interface(NSObjectClass)
    ['{53B4D44A-D79C-4B9F-A82A-1BF00F8FA9B9}']
  end;

  FIRUserProfileChangeRequest = interface(NSObject)
    ['{DE131D5F-4DCA-46E1-B61D-36CA20B5FEC3}']
    procedure commitChangesWithCompletion(completion: TFIRUserProfileChangeRequestBlockMethod1); cdecl;
    function displayName: NSString; cdecl;
    function photoURL: NSURL; cdecl;
    procedure setDisplayName(displayName: NSString); cdecl;
    procedure setPhotoURL(photoURL: NSURL); cdecl;
  end;
  TFIRUserProfileChangeRequest = class(TOCGenericImport<FIRUserProfileChangeRequestClass, FIRUserProfileChangeRequest>) end;

  FIRGitHubAuthProviderClass = interface(NSObjectClass)
    ['{B5406126-CD96-408A-A1D2-333B2BF91DC6}']
    {class} function credentialWithToken(token: NSString): FIRAuthCredential; cdecl;
  end;

  FIRGitHubAuthProvider = interface(NSObject)
    ['{5E28A7C8-70BD-46F7-ABB2-A27349CC790E}']
  end;
  TFIRGitHubAuthProvider = class(TOCGenericImport<FIRGitHubAuthProviderClass, FIRGitHubAuthProvider>) end;

  FIRAuthCredentialClass = interface(NSObjectClass)
    ['{D0ADD525-F15D-43DD-BD39-E7E1521F93A9}']
  end;

  FIRAuthCredential = interface(NSObject)
    ['{256900D2-6909-4D67-BAA4-F1849BD65CAC}']
    function provider: NSString; cdecl;
  end;
  TFIRAuthCredential = class(TOCGenericImport<FIRAuthCredentialClass, FIRAuthCredential>) end;

  FIROAuthCredentialClass = interface(FIRAuthCredentialClass)
    ['{0E9A5B1F-C24F-4F09-B0E4-7E5D2E850CF7}']
  end;

  FIROAuthCredential = interface(FIRAuthCredential)
    ['{EDE9D124-146C-4E77-8AFC-5E763D3EDB8F}']
    function accessToken: NSString; cdecl;
    function IDToken: NSString; cdecl;
    function secret: NSString; cdecl;
  end;
  TFIROAuthCredential = class(TOCGenericImport<FIROAuthCredentialClass, FIROAuthCredential>) end;

  FIRGameCenterAuthProviderClass = interface(NSObjectClass)
    ['{AF23A932-5E15-490D-846F-5B9AD0DA3C00}']
    {class} procedure getCredentialWithCompletion(completion: TFIRGameCenterAuthProviderBlockMethod1); cdecl;
  end;

  FIRGameCenterAuthProvider = interface(NSObject)
    ['{E1ADB952-7C83-4DAE-8B2A-C3A1A58DA089}']
  end;
  TFIRGameCenterAuthProvider = class(TOCGenericImport<FIRGameCenterAuthProviderClass, FIRGameCenterAuthProvider>) end;

  FIRPhoneAuthCredentialClass = interface(FIRAuthCredentialClass)
    ['{2EE6BFF4-8ABD-4A23-BD78-395F047C7823}']
  end;

  FIRPhoneAuthCredential = interface(FIRAuthCredential)
    ['{7D16928E-4C14-489F-8858-DA9FD9C78141}']
  end;
  TFIRPhoneAuthCredential = class(TOCGenericImport<FIRPhoneAuthCredentialClass, FIRPhoneAuthCredential>) end;

  FIRAuthUIDelegate = interface(IObjectiveC)
    ['{46B9AACA-949C-415A-BE50-B54BE8A0A522}']
    procedure dismissViewControllerAnimated(flag: Boolean; completion: TFIRAuthUIDelegateBlockMethod1); cdecl;
    procedure presentViewController(viewControllerToPresent: UIViewController; animated: Boolean; completion: TFIRAuthUIDelegateBlockMethod1); cdecl;
  end;

  FIRFederatedAuthProvider = interface(IObjectiveC)
    ['{174C713E-C303-47FB-841A-25EFD05B9798}']
    procedure getCredentialWithUIDelegate(UIDelegate: Pointer; completion: TFIRFederatedAuthProviderBlockMethod1); cdecl;
  end;

  FIREmailAuthProviderClass = interface(NSObjectClass)
    ['{AD184902-9BF5-4E1A-87AD-A4A9088C53DF}']
    [MethodName('credentialWithEmail:link:')]
    {class} function credentialWithEmailLink(email: NSString; link: NSString): FIRAuthCredential; cdecl;
    [MethodName('credentialWithEmail:password:')]
    {class} function credentialWithEmailPassword(email: NSString; password: NSString): FIRAuthCredential; cdecl;
  end;

  FIREmailAuthProvider = interface(NSObject)
    ['{9A665243-D607-4763-A220-B9CA56A799EE}']
  end;
  TFIREmailAuthProvider = class(TOCGenericImport<FIREmailAuthProviderClass, FIREmailAuthProvider>) end;

  FIRFacebookAuthProviderClass = interface(NSObjectClass)
    ['{93F630CE-53F2-4816-9613-07F411B7A5D2}']
    {class} function credentialWithAccessToken(accessToken: NSString): FIRAuthCredential; cdecl;
  end;

  FIRFacebookAuthProvider = interface(NSObject)
    ['{16F90112-4DEC-4DB6-A71C-BCBBD5D2A730}']
  end;
  TFIRFacebookAuthProvider = class(TOCGenericImport<FIRFacebookAuthProviderClass, FIRFacebookAuthProvider>) end;

  FIRActionCodeSettingsClass = interface(NSObjectClass)
    ['{A91C75C2-C4E4-4F7B-A6B1-BD87EB8BF1C9}']
  end;

  FIRActionCodeSettings = interface(NSObject)
    ['{9D17ECE4-A09E-4455-96D9-27C5E128C80E}']
    function androidInstallIfNotAvailable: Boolean; cdecl;
    function androidMinimumVersion: NSString; cdecl;
    function androidPackageName: NSString; cdecl;
    function dynamicLinkDomain: NSString; cdecl;
    function handleCodeInApp: Boolean; cdecl;
    function iOSBundleID: NSString; cdecl;
    procedure setAndroidPackageName(androidPackageName: NSString; installIfNotAvailable: Boolean; minimumVersion: NSString); cdecl;
    procedure setDynamicLinkDomain(dynamicLinkDomain: NSString); cdecl;
    procedure setHandleCodeInApp(handleCodeInApp: Boolean); cdecl;
    procedure setIOSBundleID(iOSBundleID: NSString); cdecl;
    procedure setURL(URL: NSURL); cdecl;
    function URL: NSURL; cdecl;
  end;
  TFIRActionCodeSettings = class(TOCGenericImport<FIRActionCodeSettingsClass, FIRActionCodeSettings>) end;

  FIRAdditionalUserInfoClass = interface(NSObjectClass)
    ['{34321F1A-746C-47B2-9031-5140ECE89CD8}']
  end;

  FIRAdditionalUserInfo = interface(NSObject)
    ['{AD4BA124-86EF-481C-B9FC-C08BF774C325}']
    function isNewUser: Boolean; cdecl;
    function profile: NSDictionary; cdecl;
    function providerID: NSString; cdecl;
    function username: NSString; cdecl;
  end;
  TFIRAdditionalUserInfo = class(TOCGenericImport<FIRAdditionalUserInfoClass, FIRAdditionalUserInfo>) end;

  FIRAuthTokenResultClass = interface(NSObjectClass)
    ['{AA08F24A-3D4A-4384-9C45-24736E65A450}']
  end;

  FIRAuthTokenResult = interface(NSObject)
    ['{69E2EED8-A118-416B-91F8-BDA7EFCC2412}']
    function authDate: NSDate; cdecl;
    function claims: NSDictionary; cdecl;
    function expirationDate: NSDate; cdecl;
    function issuedAtDate: NSDate; cdecl;
    function signInProvider: NSString; cdecl;
    function signInSecondFactor: NSString; cdecl;
    function token: NSString; cdecl;
  end;
  TFIRAuthTokenResult = class(TOCGenericImport<FIRAuthTokenResultClass, FIRAuthTokenResult>) end;

  FIRGoogleAuthProviderClass = interface(NSObjectClass)
    ['{EF2C5E9D-C986-4A9C-A890-1CDCE81A7FD3}']
    {class} function credentialWithIDToken(IDToken: NSString; accessToken: NSString): FIRAuthCredential; cdecl;
  end;

  FIRGoogleAuthProvider = interface(NSObject)
    ['{F35830DE-8ACE-419E-9EC4-88E769A09814}']
  end;
  TFIRGoogleAuthProvider = class(TOCGenericImport<FIRGoogleAuthProviderClass, FIRGoogleAuthProvider>) end;

  FIRMultiFactorResolverClass = interface(NSObjectClass)
    ['{DBF9CC92-A1CF-485D-AEA4-12633248A250}']
  end;

  FIRMultiFactorResolver = interface(NSObject)
    ['{AA81250F-8DD3-4B05-9F65-F71AABA5961D}']
    function auth: FIRAuth; cdecl;
    function hints: NSArray; cdecl;
    procedure resolveSignInWithAssertion(assertion: FIRMultiFactorAssertion; completion: FIRAuthDataResultCallback); cdecl;
    function session: FIRMultiFactorSession; cdecl;
  end;
  TFIRMultiFactorResolver = class(TOCGenericImport<FIRMultiFactorResolverClass, FIRMultiFactorResolver>) end;

  FIROAuthProviderClass = interface(NSObjectClass)
    ['{34342933-6FB0-4944-8ADB-EBA6D8B9513A}']
    [MethodName('credentialWithProviderID:accessToken:')]
    {class} function credentialWithProviderIDAccessToken(providerID: NSString; accessToken: NSString): FIROAuthCredential; cdecl;
    [MethodName('credentialWithProviderID:IDToken:rawNonce:accessToken:')]
    {class} function credentialWithProviderIDIDToken(providerID: NSString; IDToken: NSString; rawNonce: NSString; accessToken: NSString): FIROAuthCredential; overload; cdecl;
    [MethodName('credentialWithProviderID:IDToken:rawNonce:')]
    {class} function credentialWithProviderIDIDToken(providerID: NSString; IDToken: NSString; rawNonce: NSString): FIROAuthCredential; overload; cdecl;
    [MethodName('credentialWithProviderID:IDToken:accessToken:')]
    {class} function credentialWithProviderIDIDTokenAccessToken(providerID: NSString; IDToken: NSString; accessToken: NSString): FIROAuthCredential; cdecl;
    {class} function providerWithProviderID(providerID: NSString): FIROAuthProvider; overload; cdecl;
    {class} function providerWithProviderID(providerID: NSString; auth: FIRAuth): FIROAuthProvider; overload; cdecl;
  end;

  FIROAuthProvider = interface(NSObject)
    ['{224B2CB8-B64F-41FA-BB88-BEE2A0396901}']
    function customParameters: NSDictionary; cdecl;
    function providerID: NSString; cdecl;
    function scopes: NSArray; cdecl;
    procedure setCustomParameters(customParameters: NSDictionary); cdecl;
    procedure setScopes(scopes: NSArray); cdecl;
  end;
  TFIROAuthProvider = class(TOCGenericImport<FIROAuthProviderClass, FIROAuthProvider>) end;

  FIRTwitterAuthProviderClass = interface(NSObjectClass)
    ['{D752843E-20AB-46FE-972A-B57E479777D9}']
    {class} function credentialWithToken(token: NSString; secret: NSString): FIRAuthCredential; cdecl;
  end;

  FIRTwitterAuthProvider = interface(NSObject)
    ['{D78BD78A-E368-4137-9854-DE5159B80E90}']
  end;
  TFIRTwitterAuthProvider = class(TOCGenericImport<FIRTwitterAuthProviderClass, FIRTwitterAuthProvider>) end;

  FIRUserMetadataClass = interface(NSObjectClass)
    ['{7F4C945B-4526-4643-B3C9-33B75ACC0DD8}']
  end;

  FIRUserMetadata = interface(NSObject)
    ['{EBBD0F1F-4350-4ABA-BE05-74CC87A983C8}']
    function creationDate: NSDate; cdecl;
    function lastSignInDate: NSDate; cdecl;
  end;
  TFIRUserMetadata = class(TOCGenericImport<FIRUserMetadataClass, FIRUserMetadata>) end;

  FIRAuthSettingsClass = interface(NSObjectClass)
    ['{009BA2D1-9B93-4FC3-A737-1AA8905C58D1}']
  end;

  FIRAuthSettings = interface(NSObject)
    ['{4A865C20-2B7B-4F64-AA0C-CB8E30F9FD51}']
    function isAppVerificationDisabledForTesting: Boolean; cdecl;
    procedure setAppVerificationDisabledForTesting(appVerificationDisabledForTesting: Boolean); cdecl;
  end;
  TFIRAuthSettings = class(TOCGenericImport<FIRAuthSettingsClass, FIRAuthSettings>) end;

  FIRPhoneAuthProviderClass = interface(NSObjectClass)
    ['{405AC3FA-E717-4F98-A875-0C867DBBED42}']
    {class} function provider: Pointer; cdecl;
    {class} function providerWithAuth(auth: FIRAuth): Pointer; cdecl;
  end;

  FIRPhoneAuthProvider = interface(NSObject)
    ['{C3DFD883-E129-4EC6-A008-6B27D8947A7B}']
    function credentialWithVerificationID(verificationID: NSString; verificationCode: NSString): FIRPhoneAuthCredential; cdecl;
    procedure verifyPhoneNumber(phoneNumber: NSString; UIDelegate: Pointer; multiFactorSession: FIRMultiFactorSession; completion: FIRVerificationResultCallback); overload; cdecl;
    procedure verifyPhoneNumber(phoneNumber: NSString; UIDelegate: Pointer; completion: FIRVerificationResultCallback); overload; cdecl;
    procedure verifyPhoneNumberWithMultiFactorInfo(phoneMultiFactorInfo: FIRPhoneMultiFactorInfo; UIDelegate: Pointer; multiFactorSession: FIRMultiFactorSession; completion: TFIRPhoneAuthProviderBlockMethod1); cdecl;
  end;
  TFIRPhoneAuthProvider = class(TOCGenericImport<FIRPhoneAuthProviderClass, FIRPhoneAuthProvider>) end;

  FIRPhoneMultiFactorAssertionClass = interface(FIRMultiFactorAssertionClass)
    ['{0452AF44-EA30-43D1-8EE2-E8B1AA1D71E8}']
  end;

  FIRPhoneMultiFactorAssertion = interface(FIRMultiFactorAssertion)
    ['{7EA0933A-4367-4697-A5C1-5B097BB42B2B}']
  end;
  TFIRPhoneMultiFactorAssertion = class(TOCGenericImport<FIRPhoneMultiFactorAssertionClass, FIRPhoneMultiFactorAssertion>) end;

  FIRPhoneMultiFactorGeneratorClass = interface(NSObjectClass)
    ['{EB78E715-F5CD-4A66-AEAA-AFF601B3DBC7}']
    {class} function assertionWithCredential(phoneAuthCredential: FIRPhoneAuthCredential): FIRPhoneMultiFactorAssertion; cdecl;
  end;

  FIRPhoneMultiFactorGenerator = interface(NSObject)
    ['{94EDAAD6-8C14-4B1D-A6EC-4343A167DE78}']
  end;
  TFIRPhoneMultiFactorGenerator = class(TOCGenericImport<FIRPhoneMultiFactorGeneratorClass, FIRPhoneMultiFactorGenerator>) end;

  FIRPhoneMultiFactorInfoClass = interface(FIRMultiFactorInfoClass)
    ['{ED6CE58C-C635-4FC2-BDF1-7DC9597043D4}']
  end;

  FIRPhoneMultiFactorInfo = interface(FIRMultiFactorInfo)
    ['{FE2B05A3-AFC2-414B-89D3-CBE3B9928355}']
    function phoneNumber: NSString; cdecl;
  end;
  TFIRPhoneMultiFactorInfo = class(TOCGenericImport<FIRPhoneMultiFactorInfoClass, FIRPhoneMultiFactorInfo>) end;

implementation

procedure FirebaseAuthLoader; cdecl; external framework 'FirebaseAuth';

end.