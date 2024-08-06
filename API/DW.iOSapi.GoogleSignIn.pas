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
  kGIDSignInErrorCodeNoCurrentUser = -7;
  kGIDSignInErrorCodeScopesAlreadyGranted = -8;
  kGIDSignInButtonStyleStandard = 0;
  kGIDSignInButtonStyleWide = 1;
  kGIDSignInButtonStyleIconOnly = 2;
  kGIDSignInButtonColorSchemeDark = 0;
  kGIDSignInButtonColorSchemeLight = 1;

type
  GIDAuthentication = interface;
  GIDConfiguration = interface;
  GIDGoogleUser = interface;
  GIDProfileData = interface;
  GIDSignIn = interface;
  GIDSignInButton = interface;

  GIDAuthenticationAction = procedure(authentication: GIDAuthentication; error: NSError) of object;
  GIDSignInErrorCode = NSInteger;

  GIDSignInCallback = procedure(user: GIDGoogleUser; error: NSError) of object;

  GIDDisconnectCallback = procedure(error: NSError) of object;
  GIDSignInButtonStyle = NSInteger;
  GIDSignInButtonColorScheme = NSInteger;

  GIDAuthenticationClass = interface(NSObjectClass)
    ['{5040A7E4-64B1-4072-AA5B-7B69436735BC}']
  end;

  GIDAuthentication = interface(NSObject)
    ['{5F16850E-2AC9-4EEC-8B76-1B866B406B9E}']
    function accessToken: NSString; cdecl;
    function accessTokenExpirationDate: NSDate; cdecl;
    function clientID: NSString; cdecl;
    procedure doWithFreshTokens(action: GIDAuthenticationAction); cdecl;
    function fetcherAuthorizer: Pointer; cdecl;
    function idToken: NSString; cdecl;
    function idTokenExpirationDate: NSDate; cdecl;
    function refreshToken: NSString; cdecl;
  end;
  TGIDAuthentication = class(TOCGenericImport<GIDAuthenticationClass, GIDAuthentication>) end;

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
    function authentication: GIDAuthentication; cdecl;
    function grantedScopes: NSArray; cdecl;
    function hostedDomain: NSString; cdecl;
    function openIDRealm: NSString; cdecl;
    function profile: GIDProfileData; cdecl;
    function serverAuthCode: NSString; cdecl;
    function serverClientID: NSString; cdecl;
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
    procedure addScopes(scopes: NSArray; presentingViewController: UIViewController; callback: GIDSignInCallback); cdecl;
    function currentUser: GIDGoogleUser; cdecl;
    procedure disconnectWithCallback(callback: GIDDisconnectCallback); cdecl;
    function handleURL(url: NSURL): Boolean; cdecl;
    function hasPreviousSignIn: Boolean; cdecl;
    procedure restorePreviousSignInWithCallback(callback: GIDSignInCallback); cdecl;
    procedure signInWithConfiguration(configuration: GIDConfiguration; presentingViewController: UIViewController;
      callback: GIDSignInCallback); overload; cdecl;
    procedure signInWithConfiguration(configuration: GIDConfiguration; presentingViewController: UIViewController; hint: NSString;
      callback: GIDSignInCallback); overload; cdecl;
    procedure signOut; cdecl;
  end;
  TGIDSignIn = class(TOCGenericImport<GIDSignInClass, GIDSignIn>) end;

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

procedure AppAuthLoader; cdecl; external framework 'AppAuth';
procedure GoogleSignInLoader; cdecl; external framework 'GoogleSignIn';
procedure GoogleUtilitiesLoader; cdecl; external framework 'GoogleUtilities';
procedure GTMAppAuthLoader; cdecl; external framework 'GTMAppAuth';
procedure GTMSessionFetcherLoader; cdecl; external framework 'GTMSessionFetcher';

end.