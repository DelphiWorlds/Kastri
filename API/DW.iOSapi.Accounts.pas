unit DW.iOSapi.Accounts;

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
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation;

const
  ACAccountCredentialRenewResultRenewed = 0;
  ACAccountCredentialRenewResultRejected = 1;
  ACAccountCredentialRenewResultFailed = 2;
  ACErrorUnknown = 1;
  ACErrorAccountMissingRequiredProperty = 2;
  ACErrorAccountAuthenticationFailed = 3;
  ACErrorAccountTypeInvalid = 4;
  ACErrorAccountAlreadyExists = 5;
  ACErrorAccountNotFound = 6;
  ACErrorPermissionDenied = 7;
  ACErrorAccessInfoInvalid = 8;
  ACErrorClientPermissionDenied = 9;
  ACErrorAccessDeniedByProtectionPolicy = 10;
  ACErrorCredentialNotFound = 11;
  ACErrorFetchCredentialFailed = 12;
  ACErrorStoreCredentialFailed = 13;
  ACErrorRemoveCredentialFailed = 14;
  ACErrorUpdatingNonexistentAccount = 15;
  ACErrorInvalidClientBundleID = 16;
  ACErrorDeniedByPlugin = 17;
  ACErrorCoreDataSaveFailed = 18;
  ACErrorFailedSerializingAccountInfo = 19;
  ACErrorInvalidCommand = 20;
  ACErrorMissingTransportMessageID = 21;
  ACErrorCredentialItemNotFound = 22;
  ACErrorCredentialItemNotExpired = 23;

type
  ACAccount = interface;
  ACAccountType = interface;
  ACAccountCredential = interface;
  ACAccountStore = interface;

  ACAccountCredentialRenewResult = NSInteger;

  ACAccountStoreSaveCompletionHandler = procedure(success: Boolean; error: NSError) of object;

  ACAccountStoreRemoveCompletionHandler = procedure(success: Boolean; error: NSError) of object;

  ACAccountStoreRequestAccessCompletionHandler = procedure(granted: Boolean; error: NSError) of object;

  ACAccountStoreCredentialRenewalHandler = procedure(renewResult: ACAccountCredentialRenewResult; error: NSError) of object;
  ACErrorCode = Cardinal;

  ACAccountClass = interface(NSObjectClass)
    ['{267A42F1-7F46-4144-920F-F43314E041EF}']
  end;

  ACAccount = interface(NSObject)
    ['{215D0112-53C2-48D2-BA39-DEB5C76FDAEE}']
    function accountDescription: NSString; cdecl;
    function accountType: ACAccountType; cdecl;
    function credential: ACAccountCredential; cdecl;
    function identifier: NSString; cdecl;
    function initWithAccountType(&type: ACAccountType): Pointer; cdecl;
    procedure setAccountDescription(accountDescription: NSString); cdecl;
    procedure setAccountType(accountType: ACAccountType); cdecl;
    procedure setCredential(credential: ACAccountCredential); cdecl;
    procedure setUsername(username: NSString); cdecl;
    function userFullName: NSString; cdecl;
    function username: NSString; cdecl;
  end;
  TACAccount = class(TOCGenericImport<ACAccountClass, ACAccount>) end;

  ACAccountTypeClass = interface(NSObjectClass)
    ['{5B3829F7-1218-4E22-99D4-D73143DE5D54}']
  end;

  ACAccountType = interface(NSObject)
    ['{3E4E2F4A-02C4-40A1-8605-366366035B8E}']
    function accessGranted: Boolean; cdecl;
    function accountTypeDescription: NSString; cdecl;
    function identifier: NSString; cdecl;
  end;
  TACAccountType = class(TOCGenericImport<ACAccountTypeClass, ACAccountType>) end;

  ACAccountCredentialClass = interface(NSObjectClass)
    ['{ADC4E3CE-611F-4C54-86C6-411498602F41}']
  end;

  ACAccountCredential = interface(NSObject)
    ['{77239F6B-CEF6-4CFC-8036-03120826BA51}']
    function initWithOAuth2Token(token: NSString; refreshToken: NSString; expiryDate: NSDate): Pointer; cdecl;
    function initWithOAuthToken(token: NSString; tokenSecret: NSString): Pointer; cdecl;
    function oauthToken: NSString; cdecl;
    procedure setOauthToken(oauthToken: NSString); cdecl;
  end;
  TACAccountCredential = class(TOCGenericImport<ACAccountCredentialClass, ACAccountCredential>) end;

  ACAccountStoreClass = interface(NSObjectClass)
    ['{AA118CB5-F909-476A-B285-BD3313CA8AB0}']
  end;

  ACAccountStore = interface(NSObject)
    ['{99BD3AD0-1902-4056-B522-8C0E7BE84579}']
    function accounts: NSArray; cdecl;
    function accountsWithAccountType(accountType: ACAccountType): NSArray; cdecl;
    function accountTypeWithAccountTypeIdentifier(typeIdentifier: NSString): ACAccountType; cdecl;
    function accountWithIdentifier(identifier: NSString): ACAccount; cdecl;
    procedure removeAccount(account: ACAccount; withCompletionHandler: ACAccountStoreRemoveCompletionHandler); cdecl;
    procedure renewCredentialsForAccount(account: ACAccount; completion: ACAccountStoreCredentialRenewalHandler); cdecl;
    procedure requestAccessToAccountsWithType(accountType: ACAccountType;
      withCompletionHandler: ACAccountStoreRequestAccessCompletionHandler); overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-requestAccessToAccountsWithType:options:completion:", ios(5.0, 6.0))
    procedure requestAccessToAccountsWithType(accountType: ACAccountType; options: NSDictionary;
      completion: ACAccountStoreRequestAccessCompletionHandler); overload; cdecl;
    procedure saveAccount(account: ACAccount; withCompletionHandler: ACAccountStoreSaveCompletionHandler); cdecl;
  end;
  TACAccountStore = class(TOCGenericImport<ACAccountStoreClass, ACAccountStore>) end;

function ACAccountTypeIdentifierTwitter: NSString;
function ACAccountTypeIdentifierFacebook: NSString;
function ACAccountTypeIdentifierSinaWeibo: NSString;
function ACAccountTypeIdentifierTencentWeibo: NSString;
function ACAccountTypeIdentifierLinkedIn: NSString;
function ACFacebookAppIdKey: NSString;
function ACFacebookPermissionsKey: NSString;
function ACFacebookAudienceKey: NSString;
function ACFacebookAudienceEveryone: NSString;
function ACFacebookAudienceFriends: NSString;
function ACFacebookAudienceOnlyMe: NSString;
function ACLinkedInAppIdKey: NSString;
function ACLinkedInPermissionsKey: NSString;
function ACTencentWeiboAppIdKey: NSString;
function ACAccountStoreDidChangeNotification: NSString;
function ACErrorDomain: NSString;

const
  libAccounts = '/System/Library/Frameworks/Accounts.framework/Accounts';

implementation

uses
  Posix.Dlfcn;

var
  AccountsModule: THandle;

function ACAccountTypeIdentifierTwitter: NSString;
begin
  Result := CocoaNSStringConst(libAccounts, 'ACAccountTypeIdentifierTwitter');
end;

function ACAccountTypeIdentifierFacebook: NSString;
begin
  Result := CocoaNSStringConst(libAccounts, 'ACAccountTypeIdentifierFacebook');
end;

function ACAccountTypeIdentifierSinaWeibo: NSString;
begin
  Result := CocoaNSStringConst(libAccounts, 'ACAccountTypeIdentifierSinaWeibo');
end;

function ACAccountTypeIdentifierTencentWeibo: NSString;
begin
  Result := CocoaNSStringConst(libAccounts, 'ACAccountTypeIdentifierTencentWeibo');
end;

function ACAccountTypeIdentifierLinkedIn: NSString;
begin
  Result := CocoaNSStringConst(libAccounts, 'ACAccountTypeIdentifierLinkedIn');
end;

function ACFacebookAppIdKey: NSString;
begin
  Result := CocoaNSStringConst(libAccounts, 'ACFacebookAppIdKey');
end;

function ACFacebookPermissionsKey: NSString;
begin
  Result := CocoaNSStringConst(libAccounts, 'ACFacebookPermissionsKey');
end;

function ACFacebookAudienceKey: NSString;
begin
  Result := CocoaNSStringConst(libAccounts, 'ACFacebookAudienceKey');
end;

function ACFacebookAudienceEveryone: NSString;
begin
  Result := CocoaNSStringConst(libAccounts, 'ACFacebookAudienceEveryone');
end;

function ACFacebookAudienceFriends: NSString;
begin
  Result := CocoaNSStringConst(libAccounts, 'ACFacebookAudienceFriends');
end;

function ACFacebookAudienceOnlyMe: NSString;
begin
  Result := CocoaNSStringConst(libAccounts, 'ACFacebookAudienceOnlyMe');
end;

function ACLinkedInAppIdKey: NSString;
begin
  Result := CocoaNSStringConst(libAccounts, 'ACLinkedInAppIdKey');
end;

function ACLinkedInPermissionsKey: NSString;
begin
  Result := CocoaNSStringConst(libAccounts, 'ACLinkedInPermissionsKey');
end;

function ACTencentWeiboAppIdKey: NSString;
begin
  Result := CocoaNSStringConst(libAccounts, 'ACTencentWeiboAppIdKey');
end;

function ACAccountStoreDidChangeNotification: NSString;
begin
  Result := CocoaNSStringConst(libAccounts, 'ACAccountStoreDidChangeNotification');
end;

function ACErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libAccounts, 'ACErrorDomain');
end;

initialization
  AccountsModule := dlopen(MarshaledAString(libAccounts), RTLD_LAZY);

finalization
  dlclose(AccountsModule);

end.