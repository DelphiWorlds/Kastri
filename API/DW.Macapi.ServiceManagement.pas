unit DW.Macapi.ServiceManagement;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2026 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.CocoaTypes, Macapi.Foundation, Macapi.Security;

const
  kSMErrorInternalFailure = 2;
  kSMErrorInvalidSignature = 3;
  kSMErrorAuthorizationFailure = 4;
  kSMErrorToolNotValid = 5;
  kSMErrorJobNotFound = 6;
  kSMErrorServiceUnavailable = 7;
  kSMErrorJobPlistNotFound = 8;
  kSMErrorJobMustBeEnabled = 9;
  kSMErrorInvalidPlist = 10;
  kSMErrorLaunchDeniedByUser = 11;
  kSMErrorAlreadyRegistered = 12;
  SMAppServiceStatusNotRegistered = 0;
  SMAppServiceStatusEnabled = 1;
  SMAppServiceStatusRequiresApproval = 2;
  SMAppServiceStatusNotFound = 3;

type
  SMAppService = interface;

  SMAppServiceStatus = NSInteger;
  TSMAppServiceBlockMethod1 = procedure(error: NSError) of object;

  SMAppServiceClass = interface(NSObjectClass)
    ['{26B5AB6E-8C23-4733-978F-8CFE134D3D8F}']
    {class} function agentServiceWithPlistName(plistName: NSString): Pointer; cdecl;
    {class} function daemonServiceWithPlistName(plistName: NSString): Pointer; cdecl;
    {class} function loginItemServiceWithIdentifier(identifier: NSString): Pointer; cdecl;
    {class} function mainAppService: SMAppService; cdecl;
    {class} procedure openSystemSettingsLoginItems; cdecl;
    {class} function statusForLegacyURL(url: NSURL): SMAppServiceStatus; cdecl;
  end;

  SMAppService = interface(NSObject)
    ['{9AD41539-A9CF-436D-8EBB-DB470DBD260B}']
    function registerAndReturnError(error: PPointer): Boolean; cdecl;
    function status: SMAppServiceStatus; cdecl;
    function unregisterAndReturnError(error: PPointer): Boolean; cdecl;
    procedure unregisterWithCompletionHandler(handler: TSMAppServiceBlockMethod1); cdecl;
  end;
  TSMAppService = class(TOCGenericImport<SMAppServiceClass, SMAppService>) end;

function kSMErrorDomainIPC: NSString;
function kSMErrorDomainFramework: NSString;
function kSMErrorDomainLaunchd: NSString;
function SMAppServiceErrorDomain: NSString;
function kSMDomainSystemLaunchd: NSString;
function kSMDomainUserLaunchd: NSString;

const
  libServiceManagement = '/System/Library/Frameworks/ServiceManagement.framework/ServiceManagement';

function SMLoginItemSetEnabled(identifier: CFStringRef; enabled: Boolean): Boolean; cdecl;
  external libServiceManagement name _PU + 'SMLoginItemSetEnabled';

function SMJobCopyDictionary(domain: CFStringRef; jobLabel: CFStringRef): CFDictionaryRef; cdecl;
  external libServiceManagement name _PU + 'SMJobCopyDictionary';

function SMCopyAllJobDictionaries(domain: CFStringRef): CFArrayRef; cdecl;
  external libServiceManagement name _PU + 'SMCopyAllJobDictionaries';

function SMJobSubmit(domain: CFStringRef; job: CFDictionaryRef; auth: AuthorizationRef; outError: PCFErrorRef): Boolean; cdecl;
  external libServiceManagement name _PU + 'SMJobSubmit';

function SMJobRemove(domain: CFStringRef; jobLabel: CFStringRef; auth: AuthorizationRef; wait: Boolean; outError: PCFErrorRef): Boolean; cdecl;
  external libServiceManagement name _PU + 'SMJobRemove';

function SMJobBless(domain: CFStringRef; executableLabel: CFStringRef; auth: AuthorizationRef; outError: PCFErrorRef): Boolean; cdecl;
  external libServiceManagement name _PU + 'SMJobBless';

implementation

uses
  System.SysUtils;

var
  ServiceManagementModule: THandle;

function kSMErrorDomainIPC: NSString;
begin
  Result := CocoaNSStringConst(libServiceManagement, 'kSMErrorDomainIPC');
end;

function kSMErrorDomainFramework: NSString;
begin
  Result := CocoaNSStringConst(libServiceManagement, 'kSMErrorDomainFramework');
end;

function kSMErrorDomainLaunchd: NSString;
begin
  Result := CocoaNSStringConst(libServiceManagement, 'kSMErrorDomainLaunchd');
end;

function SMAppServiceErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libServiceManagement, 'SMAppServiceErrorDomain');
end;

function kSMDomainSystemLaunchd: NSString;
begin
  Result := CocoaNSStringConst(libServiceManagement, 'kSMDomainSystemLaunchd');
end;

function kSMDomainUserLaunchd: NSString;
begin
  Result := CocoaNSStringConst(libServiceManagement, 'kSMDomainUserLaunchd');
end;

initialization
  ServiceManagementModule := LoadLibrary(libServiceManagement);

finalization
  if ServiceManagementModule <> 0 then
    FreeLibrary(ServiceManagementModule);

end.