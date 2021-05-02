unit DW.iOSapi.GoogleSignIn;

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
  Macapi.ObjectiveC, Macapi.CoreFoundation, iOSapi.Foundation, iOSapi.CocoaTypes, iOSapi.UIKit;

const
  kGIDSignInErrorCodeUnknown = -1;
  kGIDSignInErrorCodeKeychain = -2;
  kGIDSignInErrorCodeHasNoAuthInKeychain = -4;
  kGIDSignInErrorCodeCanceled = -5;
  kGIDSignInErrorCodeEMM = -6;
  kGIDSignInButtonStyleStandard = 0;
  kGIDSignInButtonStyleWide = 1;
  kGIDSignInButtonStyleIconOnly = 2;
  kGIDSignInButtonColorSchemeDark = 0;
  kGIDSignInButtonColorSchemeLight = 1;

type
  GIDAuthentication = interface;
  GIDGoogleUser = interface;
  GIDProfileData = interface;
  GIDSignInDelegate = interface;
  GIDSignIn = interface;
  GIDSignInButton = interface;

  GIDAuthenticationHandler = procedure(authentication: GIDAuthentication; error: NSError) of object;

  GIDAccessTokenHandler = procedure(accessToken: NSString; error: NSError) of object;
  GIDSignInErrorCode = NSInteger;
  GIDSignInButtonStyle = NSInteger;
  GIDSignInButtonColorScheme = NSInteger;

  GIDAuthenticationClass = interface(NSObjectClass)
    ['{46189DFE-BFB2-4D22-99E6-26CCEB807E60}']
  end;

  GIDAuthentication = interface(NSObject)
    ['{9453B252-5F2F-4102-A7CE-81AF9733F321}']
    function accessToken: NSString; cdecl;
    function accessTokenExpirationDate: NSDate; cdecl;
    function clientID: NSString; cdecl;
    function fetcherAuthorizer: Pointer; cdecl;
    procedure getTokensWithHandler(handler: GIDAuthenticationHandler); cdecl;
    function idToken: NSString; cdecl;
    function idTokenExpirationDate: NSDate; cdecl;
    function refreshToken: NSString; cdecl;
    procedure refreshTokensWithHandler(handler: GIDAuthenticationHandler); cdecl;
  end;
  TGIDAuthentication = class(TOCGenericImport<GIDAuthenticationClass, GIDAuthentication>) end;

  GIDGoogleUserClass = interface(NSObjectClass)
    ['{DE0EB8E7-D633-4426-B3EB-2DDC45E2CBAE}']
  end;

  GIDGoogleUser = interface(NSObject)
    ['{9830D8C7-0561-4F74-AE1C-D98DCCE9A83F}']
    function authentication: GIDAuthentication; cdecl;
    function grantedScopes: NSArray; cdecl;
    function hostedDomain: NSString; cdecl;
    function profile: GIDProfileData; cdecl;
    function serverAuthCode: NSString; cdecl;
    function userID: NSString; cdecl;
  end;
  TGIDGoogleUser = class(TOCGenericImport<GIDGoogleUserClass, GIDGoogleUser>) end;

  GIDProfileDataClass = interface(NSObjectClass)
    ['{CEAAE2D3-32CC-4CAF-9859-FB2B1389B57C}']
  end;

  GIDProfileData = interface(NSObject)
    ['{D1FF28EB-7449-407C-BC67-CB009E6BC978}']
    function email: NSString; cdecl;
    function familyName: NSString; cdecl;
    function givenName: NSString; cdecl;
    function hasImage: Boolean; cdecl;
    function imageURLWithDimension(dimension: NSUInteger): NSURL; cdecl;
    function name: NSString; cdecl;
  end;
  TGIDProfileData = class(TOCGenericImport<GIDProfileDataClass, GIDProfileData>) end;

  GIDSignInDelegate = interface(IObjectiveC)
    ['{DB593C87-D849-4915-82AB-323EA443352F}']
    [MethodName('signIn:didDisconnectWithUser:withError:')]
    procedure signInDidDisconnectWithUser(signIn: GIDSignIn; didDisconnectWithUser: GIDGoogleUser; withError: NSError); cdecl;
    [MethodName('signIn:didSignInForUser:withError:')]
    procedure signInDidSignInForUser(signIn: GIDSignIn; didSignInForUser: GIDGoogleUser; withError: NSError); cdecl;
  end;

  GIDSignInClass = interface(NSObjectClass)
    ['{DF783D5E-D71A-40AB-BA9B-AF83AAD1FDC6}']
    {class} function new: Pointer; cdecl;
    {class} function sharedInstance: GIDSignIn; cdecl;
  end;

  GIDSignIn = interface(NSObject)
    ['{AA346F11-A96E-4D11-A217-29D911992AB3}']
    function clientID: NSString; cdecl;
    function currentUser: GIDGoogleUser; cdecl;
    function delegate: Pointer; cdecl;
    procedure disconnect; cdecl;
    function handleURL(url: NSURL): Boolean; cdecl;
    function hasPreviousSignIn: Boolean; cdecl;
    function hostedDomain: NSString; cdecl;
    function language: NSString; cdecl;
    function loginHint: NSString; cdecl;
    function openIDRealm: NSString; cdecl;
    function presentingViewController: UIViewController; cdecl;
    procedure restorePreviousSignIn; cdecl;
    function scopes: NSArray; cdecl;
    function serverClientID: NSString; cdecl;
    procedure setClientID(clientID: NSString); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setHostedDomain(hostedDomain: NSString); cdecl;
    procedure setLanguage(language: NSString); cdecl;
    procedure setLoginHint(loginHint: NSString); cdecl;
    procedure setOpenIDRealm(openIDRealm: NSString); cdecl;
    procedure setPresentingViewController(presentingViewController: UIViewController); cdecl;
    procedure setScopes(scopes: NSArray); cdecl;
    procedure setServerClientID(serverClientID: NSString); cdecl;
    procedure setShouldFetchBasicProfile(shouldFetchBasicProfile: Boolean); cdecl;
    function shouldFetchBasicProfile: Boolean; cdecl;
    procedure signIn; cdecl;
    procedure signOut; cdecl;
  end;
  TGIDSignIn = class(TOCGenericImport<GIDSignInClass, GIDSignIn>) end;

  GIDSignInButtonClass = interface(UIControlClass)
    ['{3519BE06-5058-42B6-A1E6-9417493B3B72}']
  end;

  GIDSignInButton = interface(UIControl)
    ['{60EDDEA5-BA3F-45D8-A1CE-DE9D2E8E8804}']
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