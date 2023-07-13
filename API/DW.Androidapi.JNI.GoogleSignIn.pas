unit DW.Androidapi.JNI.GoogleSignIn;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2023 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // DW
  {$IF RTLVersion >= 35.00}
  DW.Androidapi.JNI.PlayServices,
  {$ENDIF}
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.Os, Androidapi.JNI.PlayServices, Androidapi.JNI.Net, Androidapi.JNI.Accounts,
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.App, Androidapi.JNI.PlayServices.Tasks;

type
  JAbstractSafeParcelable = interface;
  JGoogleApi = interface;
  JGoogleSignIn = interface;
  JGoogleSignInAccount = interface;
  JGoogleSignInClient = interface;
  JGoogleSignInOptions = interface;
  JGoogleSignInOptionsExtension = interface;
  JGoogleSignInStatusCodes = interface;

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
    {class} function create(P1: JString; P2: JString; P3: JString; P4: JString; P5: Jnet_Uri; P6: JLong; P7: JString;
      P8: JSet): JGoogleSignInAccount; cdecl; overload;
    {class} function create(P1: JString; P2: JString; P3: JString; P4: JString; P5: JString; P6: JString; P7: Jnet_Uri; P8: JLong;
      P9: JString; P10: JSet): JGoogleSignInAccount; cdecl; overload;
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

  JGoogleSignInOptionsClass = interface(JAbstractSafeParcelableClass)
    ['{559C99A1-FB95-4D00-ADE6-7A1CF97B9F0D}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function _GetDEFAULT_GAMES_SIGN_IN: JGoogleSignInOptions; cdecl;
    {class} function _GetDEFAULT_SIGN_IN: JGoogleSignInOptions; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    {class} property DEFAULT_GAMES_SIGN_IN: JGoogleSignInOptions read _GetDEFAULT_GAMES_SIGN_IN;
    {class} property DEFAULT_SIGN_IN: JGoogleSignInOptions read _GetDEFAULT_SIGN_IN;
  end;

  [JavaSignature('com/google/android/gms/auth/api/signin/GoogleSignInOptions')]
  JGoogleSignInOptions = interface(JAbstractSafeParcelable)
    ['{36B205C0-37B9-4EB8-81F4-EB19A8611F86}']
    function equals(object_: JObject): Boolean; cdecl;
    function getAccount: JAccount; cdecl;
    function getExtensions: JArrayList; cdecl;
    function getScopeArray: TJavaObjectArray<JScope>; cdecl;
    function getScopes: JArrayList; cdecl;
    function getServerClientId: JString; cdecl;
    function hashCode: Integer; cdecl;
    function isForceCodeForRefreshToken: Boolean; cdecl;
    function isIdTokenRequested: Boolean; cdecl;
    function isServerAuthCodeRequested: Boolean; cdecl;
    procedure writeToParcel(parcel: JParcel; i: Integer); cdecl;
  end;
  TJGoogleSignInOptions = class(TJavaGenericImport<JGoogleSignInOptionsClass, JGoogleSignInOptions>) end;

  JGoogleSignInOptionsExtensionClass = interface(IJavaClass)
    ['{9E6B28A8-667E-4429-992C-5E5EB975AB92}']
    {class} function _GetFITNESS: Integer; cdecl;
    {class} function _GetGAMES: Integer; cdecl;
    {class} function getExtensionType: Integer; cdecl;
    {class} property FITNESS: Integer read _GetFITNESS;
    {class} property GAMES: Integer read _GetGAMES;
  end;

  [JavaSignature('com/google/android/gms/auth/api/signin/GoogleSignInOptionsExtension')]
  JGoogleSignInOptionsExtension = interface(IJavaInstance)
    ['{41075308-076B-4B00-A34F-EEC0E70BAA3C}']
    function getImpliedScopes: JList; cdecl;
    function toBundle: JBundle; cdecl;
  end;
  TJGoogleSignInOptionsExtension = class(TJavaGenericImport<JGoogleSignInOptionsExtensionClass, JGoogleSignInOptionsExtension>) end;

  // GoogleApi import is here only as a placeholder. Presently, it cannot be used in Delphi
  JGoogleApiClass = interface(JObjectClass)
    ['{28324BE6-4AEF-463E-8646-30A441837368}']
  end;

  [JavaSignature('com/google/android/gms/common/api/GoogleApi')]
  JGoogleApi = interface(JObject)
    ['{A2B4ED5C-541B-4147-A11A-C310C160EEB1}']
  end;
  TJGoogleApi = class(TJavaGenericImport<JGoogleApiClass, JGoogleApi>) end;

  JGoogleSignInStatusCodesClass = interface(JCommonStatusCodesClass)
    ['{70EFADF8-065B-4BA6-BC01-CC7A5D2B0D64}']
    {class} function _GetSIGN_IN_CANCELLED: Integer; cdecl;
    {class} function _GetSIGN_IN_CURRENTLY_IN_PROGRESS: Integer; cdecl;
    {class} function _GetSIGN_IN_FAILED: Integer; cdecl;
    {class} function getStatusCodeString(i: Integer): JString; cdecl;
    {class} property SIGN_IN_CANCELLED: Integer read _GetSIGN_IN_CANCELLED;
    {class} property SIGN_IN_CURRENTLY_IN_PROGRESS: Integer read _GetSIGN_IN_CURRENTLY_IN_PROGRESS;
    {class} property SIGN_IN_FAILED: Integer read _GetSIGN_IN_FAILED;
  end;

  [JavaSignature('com/google/android/gms/auth/api/signin/GoogleSignInStatusCodes')]
  JGoogleSignInStatusCodes = interface(JCommonStatusCodes)
    ['{4CA72075-D99B-45C7-9F68-A5AE6D711D5B}']
  end;
  TJGoogleSignInStatusCodes = class(TJavaGenericImport<JGoogleSignInStatusCodesClass, JGoogleSignInStatusCodes>) end;

  JGoogleSignInClientClass = interface(JGoogleApiClass)
    ['{E63702C5-FCD2-4E38-9FEC-7FEFE07ED84E}']
  end;

  [JavaSignature('com/google/android/gms/auth/api/signin/GoogleSignInClient')]
  JGoogleSignInClient = interface(JGoogleApi)
    ['{6984FFDB-62E5-4D1B-8C47-49A1B8C68DEC}']
    function getSignInIntent: JIntent; cdecl;
    function revokeAccess: JTask; cdecl;
    function signOut: JTask; cdecl;
    function silentSignIn: JTask; cdecl;
  end;
  TJGoogleSignInClient = class(TJavaGenericImport<JGoogleSignInClientClass, JGoogleSignInClient>) end;

  JGoogleSignInClass = interface(JObjectClass)
    ['{5AEB3756-77F3-453E-A7F7-DA4709D160F6}']
    {class} function getAccountForExtension(context: JContext;
      googleSignInOptionsExtension: JGoogleSignInOptionsExtension): JGoogleSignInAccount; cdecl;
    {class} function getClient(context: JContext; googleSignInOptions: JGoogleSignInOptions): JGoogleSignInClient; cdecl; overload;
    {class} function getClient(activity: JActivity; googleSignInOptions: JGoogleSignInOptions): JGoogleSignInClient; cdecl; overload;
    {class} function getLastSignedInAccount(context: JContext): JGoogleSignInAccount; cdecl;
    {class} function getSignedInAccountFromIntent(intent: JIntent): JTask; cdecl;
    {class} function hasPermissions(googleSignInAccount: JGoogleSignInAccount;
      googleSignInOptionsExtension: JGoogleSignInOptionsExtension): Boolean; cdecl; overload;
    {class} procedure requestPermissions(activity: JActivity; i: Integer; googleSignInAccount: JGoogleSignInAccount;
      googleSignInOptionsExtension: JGoogleSignInOptionsExtension); cdecl; overload;
  end;

  [JavaSignature('com/google/android/gms/auth/api/signin/GoogleSignIn')]
  JGoogleSignIn = interface(JObject)
    ['{3FFBC235-52BB-44E8-88A3-A0555B694D7F}']
  end;
  TJGoogleSignIn = class(TJavaGenericImport<JGoogleSignInClass, JGoogleSignIn>) end;

implementation

end.
