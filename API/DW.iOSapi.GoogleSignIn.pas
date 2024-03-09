unit DW.iOSapi.GoogleSignIn;

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
  Macapi.ObjectiveC, Macapi.CoreFoundation,
  // iOS
  iOSapi.Foundation, iOSapi.CocoaTypes, iOSapi.UIKit;

const
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

  GIDSignInErrorCode = NSInteger;
  GIDSignInButtonStyle = NSInteger;
  GIDSignInButtonColorScheme = NSInteger;
  
  TGIDGoogleUserBlockMethod1 = procedure(user: GIDGoogleUser; error: NSError) of object;
  TGIDGoogleUserBlockMethod2 = procedure(signInResult: GIDSignInResult; error: NSError) of object;
  TGIDSignInBlockMethod1 = procedure(user: GIDGoogleUser; error: NSError) of object;
  TGIDSignInBlockMethod2 = procedure(error: NSError) of object;
  TGIDSignInBlockMethod3 = procedure(signInResult: GIDSignInResult; error: NSError) of object;

  GIDConfigurationClass = interface(NSObjectClass)
    ['{802056A9-B1CF-4735-AFB1-4A7F5B2CEEBE}']
    {class} function new: Pointer; cdecl;
  end;

  GIDConfiguration = interface(NSObject)
    ['{C74457FF-E0D7-4A41-99A0-703F1A709722}']
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
    ['{DA8B3F2E-B389-435C-861F-CCAC4074AA64}']
  end;

  GIDGoogleUser = interface(NSObject)
    ['{CD4680A3-D134-458E-8A5B-5EC2B4B8009C}']
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
    ['{D804CECA-2C34-44A6-9010-DE42399588BE}']
  end;

  GIDProfileData = interface(NSObject)
    ['{5F077EB1-4CA3-4EFC-A0F0-4E456E26332C}']
    function email: NSString; cdecl;
    function familyName: NSString; cdecl;
    function givenName: NSString; cdecl;
    function hasImage: Boolean; cdecl;
    function imageURLWithDimension(dimension: NSUInteger): NSURL; cdecl;
    function name: NSString; cdecl;
  end;
  TGIDProfileData = class(TOCGenericImport<GIDProfileDataClass, GIDProfileData>) end;

  GIDSignInClass = interface(NSObjectClass)
    ['{69636B12-0846-42A3-A23B-368BB0EE522F}']
    {class} function new: Pointer; cdecl;
    {class} function sharedInstance: GIDSignIn; cdecl;
  end;

  GIDSignIn = interface(NSObject)
    ['{3D734D8D-4FD8-4D45-ABF6-5BAABF41971F}']
    function configuration: GIDConfiguration; cdecl;
    function currentUser: GIDGoogleUser; cdecl;
    procedure disconnectWithCompletion(completion: TGIDSignInBlockMethod2); cdecl;
    function handleURL(url: NSURL): Boolean; cdecl;
    function hasPreviousSignIn: Boolean; cdecl;
    procedure restorePreviousSignInWithCompletion(completion: TGIDSignInBlockMethod1); cdecl;
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
    ['{597DE854-9A51-493A-B563-3E04251C4206}']
    {class} function new: Pointer; cdecl;
  end;

  GIDToken = interface(NSObject)
    ['{8A3C755E-DAB9-466C-B966-1D5EE4DAF38B}']
    function expirationDate: NSDate; cdecl;
    function isEqualToToken(otherToken: GIDToken): Boolean; cdecl;
    function tokenString: NSString; cdecl;
  end;
  TGIDToken = class(TOCGenericImport<GIDTokenClass, GIDToken>) end;

  GIDSignInResultClass = interface(NSObjectClass)
    ['{39FC37B6-8ED7-4F1B-829C-134139620DAC}']
    {class} function new: Pointer; cdecl;
  end;

  GIDSignInResult = interface(NSObject)
    ['{44680F68-2D4F-48B8-9113-C7BCE69745D7}']
    function serverAuthCode: NSString; cdecl;
    function user: GIDGoogleUser; cdecl;
  end;
  TGIDSignInResult = class(TOCGenericImport<GIDSignInResultClass, GIDSignInResult>) end;

  GIDSignInButtonClass = interface(UIControlClass)
    ['{BBE7F445-FA2D-4737-8D0E-87190901D4AC}']
  end;

  GIDSignInButton = interface(UIControl)
    ['{2883765F-7275-4EAA-84A9-04E26C5103FB}']
    function colorScheme: GIDSignInButtonColorScheme; cdecl;
    procedure setColorScheme(colorScheme: GIDSignInButtonColorScheme); cdecl;
    procedure setStyle(style: GIDSignInButtonStyle); cdecl;
    function style: GIDSignInButtonStyle; cdecl;
  end;
  TGIDSignInButton = class(TOCGenericImport<GIDSignInButtonClass, GIDSignInButton>) end;

// function kGIDSignInErrorDomain: NSString;

implementation

procedure CLangRTLoader; cdecl;
  {$IF not Defined(IOSSIMULATOR)}
  external '/usr/lib/clang/lib/darwin/libclang_rt.ios.a'; // Fixes linker error: ___isPlatformVersionAtLeast missing (iOS SDK 12.x)
  {$ELSE}
  external '/usr/lib/clang/lib/darwin/libclang_rt.iossim.a'; // Fixes linker error: ___isPlatformVersionAtLeast missing (iOS SDK 12.x)
  {$ENDIF}
procedure AppAuthLoader; cdecl; external framework 'AppAuth';
procedure GoogleSignInLoader; cdecl; external framework 'GoogleSignIn';
// procedure GoogleUtilitiesLoader; cdecl; external framework 'GoogleUtilities';
procedure GTMAppAuthLoader; cdecl; external framework 'GTMAppAuth';
procedure GTMSessionFetcherLoader; cdecl; external framework 'GTMSessionFetcher';

end.