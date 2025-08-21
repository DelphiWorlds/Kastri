unit DW.iOSapi.GoogleSignIn;

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
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit;

const
  kGIDAppCheckUnexpectedError = 1;
  kGIDSignInErrorCodeUnknown = -1;
  kGIDSignInErrorCodeKeychain = -2;
  kGIDSignInErrorCodeHasNoAuthInKeychain = -4;
  kGIDSignInErrorCodeCanceled = -5;
  kGIDSignInErrorCodeEMM = -6;
  kGIDSignInErrorCodeScopesAlreadyGranted = -8;
  kGIDSignInErrorCodeMismatchWithCurrentUser = -9;
  kGIDSignInButtonStyleStandard = 0;
  kGIDSignInButtonStyleWide = 1;
  kGIDSignInButtonStyleIconOnly = 2;
  kGIDSignInButtonColorSchemeDark = 0;
  kGIDSignInButtonColorSchemeLight = 1;

type
  GIDConfiguration = interface;
  GIDGoogleUser = interface;
  GIDProfileData = interface;
  GIDSignIn = interface;
  GIDToken = interface;
  GIDSignInResult = interface;
  GIDSignInButton = interface;

  GIDAppCheckErrorCode = NSInteger;
  GIDSignInErrorCode = NSInteger;
  GIDSignInButtonStyle = NSInteger;
  GIDSignInButtonColorScheme = NSInteger;
  TGIDGoogleUserBlockMethod1 = procedure(user: GIDGoogleUser; error: NSError) of object;
  TGIDGoogleUserBlockMethod2 = procedure(signInResult: GIDSignInResult; error: NSError) of object;
  TGIDSignInBlockMethod1 = procedure(error: NSError) of object;
  TGIDSignInBlockMethod2 = procedure(user: GIDGoogleUser; error: NSError) of object;
  TGIDSignInBlockMethod3 = procedure(signInResult: GIDSignInResult; error: NSError) of object;

  GIDConfigurationClass = interface(NSObjectClass)
    ['{AA35DB0D-62B7-4CE7-B828-57D094A15BD6}']
    {class} function new: Pointer; cdecl;
  end;

  GIDConfiguration = interface(NSObject)
    ['{33122608-BA7E-4C27-979A-4CF737334601}']
    function clientID: NSString; cdecl;
    function hostedDomain: NSString; cdecl;
    function initWithClientID(clientID: NSString): Pointer; overload; cdecl;
    function initWithClientID(clientID: NSString; serverClientID: NSString; hostedDomain: NSString; openIDRealm: NSString): Pointer; overload; cdecl;
    function initWithClientID(clientID: NSString; serverClientID: NSString): Pointer; overload; cdecl;
    function openIDRealm: NSString; cdecl;
    function serverClientID: NSString; cdecl;
  end;
  TGIDConfiguration = class(TOCGenericImport<GIDConfigurationClass, GIDConfiguration>) end;

  GIDGoogleUserClass = interface(NSObjectClass)
    ['{CB03AD41-F23E-420B-9FA9-76D75AF8DB1A}']
  end;

  GIDGoogleUser = interface(NSObject)
    ['{D7CAF07C-B538-46F0-B36F-32C354F19D85}']
    function accessToken: GIDToken; cdecl;
    procedure addScopes(scopes: NSArray; presentingViewController: UIViewController; completion: TGIDGoogleUserBlockMethod2); cdecl;
    function configuration: GIDConfiguration; cdecl;
    function fetcherAuthorizer: Pointer; cdecl;
    function grantedScopes: NSArray; cdecl;
    function idToken: GIDToken; cdecl;
    function profile: GIDProfileData; cdecl;
    function refreshToken: GIDToken; cdecl;
    procedure refreshTokensIfNeededWithCompletion(completion: TGIDGoogleUserBlockMethod1); cdecl;
    function userID: NSString; cdecl;
  end;
  TGIDGoogleUser = class(TOCGenericImport<GIDGoogleUserClass, GIDGoogleUser>) end;

  GIDProfileDataClass = interface(NSObjectClass)
    ['{4A5E849B-F04A-4A51-9446-202A29BE5402}']
  end;

  GIDProfileData = interface(NSObject)
    ['{2FFE0826-4207-4BA1-82DB-18249C0DB723}']
    function email: NSString; cdecl;
    function familyName: NSString; cdecl;
    function givenName: NSString; cdecl;
    function hasImage: Boolean; cdecl;
    function imageURLWithDimension(dimension: NSUInteger): NSURL; cdecl;
    function name: NSString; cdecl;
  end;
  TGIDProfileData = class(TOCGenericImport<GIDProfileDataClass, GIDProfileData>) end;

  GIDSignInClass = interface(NSObjectClass)
    ['{DB88611E-21FB-4DDB-9718-93F49D989287}']
    {class} function new: Pointer; cdecl;
    {class} function sharedInstance: GIDSignIn; cdecl;
  end;

  GIDSignIn = interface(NSObject)
    ['{BFC59FFC-5527-4B01-BB4E-38C7679FC452}']
    function configuration: GIDConfiguration; cdecl;
    procedure configureDebugProviderWithAPIKey(APIKey: NSString; completion: TGIDSignInBlockMethod1); cdecl;
    procedure configureWithCompletion(completion: TGIDSignInBlockMethod1); cdecl;
    function currentUser: GIDGoogleUser; cdecl;
    procedure disconnectWithCompletion(completion: TGIDSignInBlockMethod1); cdecl;
    function handleURL(url: NSURL): Boolean; cdecl;
    function hasPreviousSignIn: Boolean; cdecl;
    procedure restorePreviousSignInWithCompletion(completion: TGIDSignInBlockMethod2); cdecl;
    procedure setConfiguration(configuration: GIDConfiguration); cdecl;
    procedure signInWithPresentingViewController(presentingViewController: UIViewController; hint: NSString; additionalScopes: NSArray;
      completion: TGIDSignInBlockMethod3); overload; cdecl;
    procedure signInWithPresentingViewController(presentingViewController: UIViewController; completion: TGIDSignInBlockMethod3); overload; cdecl;
    procedure signInWithPresentingViewController(presentingViewController: UIViewController; hint: NSString;
      completion: TGIDSignInBlockMethod3); overload; cdecl;
    procedure signOut; cdecl;
  end;
  TGIDSignIn = class(TOCGenericImport<GIDSignInClass, GIDSignIn>) end;

  GIDTokenClass = interface(NSObjectClass)
    ['{75497CE5-087F-47FC-B36E-17CC9CB6BDC5}']
    {class} function new: Pointer; cdecl;
  end;

  GIDToken = interface(NSObject)
    ['{C3ECA282-4BCE-4087-9B41-105D3A632F51}']
    function expirationDate: NSDate; cdecl;
    function isEqualToToken(otherToken: GIDToken): Boolean; cdecl;
    function tokenString: NSString; cdecl;
  end;
  TGIDToken = class(TOCGenericImport<GIDTokenClass, GIDToken>) end;

  GIDSignInResultClass = interface(NSObjectClass)
    ['{0D9AB1C1-8005-4AC5-9FE2-D3DE7C4EEC24}']
    {class} function new: Pointer; cdecl;
  end;

  GIDSignInResult = interface(NSObject)
    ['{F9A9527B-C754-4FA1-8D78-52109EA737DF}']
    function serverAuthCode: NSString; cdecl;
    function user: GIDGoogleUser; cdecl;
  end;
  TGIDSignInResult = class(TOCGenericImport<GIDSignInResultClass, GIDSignInResult>) end;

  GIDSignInButtonClass = interface(UIControlClass)
    ['{E2715A69-6187-4D76-84F8-7F977C917B07}']
  end;

  GIDSignInButton = interface(UIControl)
    ['{9688E24C-01A1-43CF-845D-175D3D58E288}']
    function colorScheme: GIDSignInButtonColorScheme; cdecl;
    procedure setColorScheme(colorScheme: GIDSignInButtonColorScheme); cdecl;
    procedure setStyle(style: GIDSignInButtonStyle); cdecl;
    function style: GIDSignInButtonStyle; cdecl;
  end;
  TGIDSignInButton = class(TOCGenericImport<GIDSignInButtonClass, GIDSignInButton>) end;

implementation

uses
  DW.iOSapi.SwiftCompat;

procedure CLangRTLoader; cdecl;
  {$IF not Defined(IOSSIMULATOR)}
  external '/usr/lib/clang/lib/darwin/libclang_rt.ios.a'; // Fixes linker error: ___isOSVersionAtLeast missing (iOS SDK 12.x)
  {$ELSE}
  external '/usr/lib/clang/lib/darwin/libclang_rt.iossim.a'; // Fixes linker error: ___isOSVersionAtLeast missing (iOS SDK 12.x)
  {$ENDIF}
procedure AppAuthLoader; cdecl; external framework 'AppAuth';
{$IF not Defined(FB108)}
procedure AppCheckCoreLoader; cdecl; external framework 'AppCheckCore';
{$ENDIF}
procedure FBLPromisesLoader; cdecl; external framework 'FBLPromises';
procedure GoogleSignInLoader; cdecl; external framework 'GoogleSignIn';
procedure GoogleUtilitiesLoader; cdecl; external framework 'GoogleUtilities';
procedure GTMAppAuthLoader; cdecl; external framework 'GTMAppAuth';
procedure GTMSessionFetcherLoader; cdecl; external framework 'GTMSessionFetcher';

end.
