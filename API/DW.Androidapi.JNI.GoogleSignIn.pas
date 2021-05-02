unit DW.Androidapi.JNI.GoogleSignIn;

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
  // Android
  AndroidAPI.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.Os, Androidapi.JNI.PlayServices, Androidapi.JNI.Net, Androidapi.JNI.Accounts;

type
  JAbstractSafeParcelable = interface;
  JGoogleSignInAccount = interface;

  JAbstractSafeParcelableClass = interface(JSafeParcelableClass)
    ['{1EFD3F06-3AEB-4F1B-AF28-DBF94468AAD7}']
    {class} function describeContents: Integer; cdecl;
    {class} function init: JAbstractSafeParcelable; cdecl;
  end;

  [JavaSignature('com/google/android/gms/common/internal/safeparcel/AbstractSafeParcelable')]
  JAbstractSafeParcelable = interface(JSafeParcelable)
    ['{9D2BD299-2153-40FF-8355-5CDF7283BB22}']
  end;
  TJAbstractSafeParcelable = class(TJavaGenericImport<JAbstractSafeParcelableClass, JAbstractSafeParcelable>) end;

  JGoogleSignInAccountClass = interface(JAbstractSafeParcelableClass)
    ['{EC0E6DDA-2A4C-4B5C-9DBE-C2AA4EC1BBC9}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function _GetsClock: JClock; cdecl;
    // {class} function create(P1: JString; P2: JString; P3: JString; P4: JString; P5: Jnet_Uri; P6: JLong; P7: JString; P8: JSet): JGoogleSignInAccount; cdecl; overload;
    // {class} function create(P1: JString; P2: JString; P3: JString; P4: JString; P5: JString; P6: JString; P7: Jnet_Uri; P8: JLong; P9: JString; P10: JSet): JGoogleSignInAccount; cdecl; overload;
    {class} function createDefault: JGoogleSignInAccount; cdecl;
    {class} function fromJsonString(json: JString): JGoogleSignInAccount; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    {class} property sClock: JClock read _GetsClock;
  end;

  [JavaSignature('com/google/android/gms/auth/api/signin/GoogleSignInAccount')]
  JGoogleSignInAccount = interface(JAbstractSafeParcelable)
    ['{EC9C54EB-4987-44E4-8F1B-99C8EF459327}']
    function equals(obj: JObject): Boolean; cdecl;
    function getAccount: JAccount; cdecl;
    function getDisplayName: JString; cdecl;
    function getEmail: JString; cdecl;
    function getExpirationTimeSecs: Int64; cdecl;
    function getFamilyName: JString; cdecl;
    function getGivenName: JString; cdecl;
    function getGrantedScopes: JSet; cdecl;
    function getId: JString; cdecl;
    function getIdToken: JString; cdecl;
    function getObfuscatedIdentifier: JString; cdecl;
    function getPhotoUrl: Jnet_Uri; cdecl;
    function getRequestedScopes: JSet; cdecl;
    function getServerAuthCode: JString; cdecl;
    function hashCode: Integer; cdecl;
    function isExpired: Boolean; cdecl;
    function setServerAuthCode(authCode: JString): JGoogleSignInAccount; cdecl;
    function toJson: JString; cdecl;
    function toJsonForStorage: JString; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  end;
  TJGoogleSignInAccount = class(TJavaGenericImport<JGoogleSignInAccountClass, JGoogleSignInAccount>) end;

implementation

end.
