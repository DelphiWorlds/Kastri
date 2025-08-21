unit DW.iOSapi.Social;

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
  DW.iOSapi.Accounts;

const
  SLRequestMethodGET = 0;
  SLRequestMethodPOST = 1;
  SLRequestMethodDELETE = 2;
  SLRequestMethodPUT = 3;
  SLComposeViewControllerResultCancelled = 0;
  SLComposeViewControllerResultDone = 1;

type
  SLRequest = interface;
  SLComposeServiceViewController = interface;
  SLComposeViewController = interface;
  SLComposeSheetConfigurationItem = interface;

  SLRequestMethod = NSInteger;

  SLRequestHandler = procedure(responseData: NSData; urlResponse: NSHTTPURLResponse; error: NSError) of object;
  SLComposeViewControllerResult = NSInteger;

  SLComposeViewControllerCompletionHandler = procedure(result: SLComposeViewControllerResult) of object;

  SLComposeSheetConfigurationItemTapHandler = procedure of object;

  SLRequestClass = interface(NSObjectClass)
    ['{CA2C6CE8-C976-4BE7-B7ED-A1A18C1831CC}']
    {class} function requestForServiceType(serviceType: NSString; requestMethod: SLRequestMethod; URL: NSURL;
      parameters: NSDictionary): SLRequest; cdecl;
  end;

  SLRequest = interface(NSObject)
    ['{44389C65-C2A6-4FFB-A69B-17C8E60EBCA3}']
    function account: ACAccount; cdecl;
    procedure addMultipartData(data: NSData; withName: NSString; &type: NSString; filename: NSString); cdecl;
    function parameters: NSDictionary; cdecl;
    procedure performRequestWithHandler(handler: SLRequestHandler); cdecl;
    function preparedURLRequest: NSURLRequest; cdecl;
    function requestMethod: SLRequestMethod; cdecl;
    procedure setAccount(account: ACAccount); cdecl;
    function URL: NSURL; cdecl;
  end;
  TSLRequest = class(TOCGenericImport<SLRequestClass, SLRequest>) end;

  SLComposeServiceViewControllerClass = interface(UIViewControllerClass)
    ['{C45D0EF4-08F3-4F25-B979-B5FED498C43B}']
  end;

  SLComposeServiceViewController = interface(UIViewController)
    ['{6D3A29C5-E538-4CA6-AF6C-AEB380C34269}']
    function autoCompletionViewController: UIViewController; cdecl;
    procedure cancel; cdecl;
    function charactersRemaining: NSNumber; cdecl;
    function configurationItems: NSArray; cdecl;
    function contentText: NSString; cdecl;
    procedure didSelectCancel; cdecl;
    procedure didSelectPost; cdecl;
    function isContentValid: Boolean; cdecl;
    function loadPreviewView: UIView; cdecl;
    function placeholder: NSString; cdecl;
    procedure popConfigurationViewController; cdecl;
    procedure presentationAnimationDidFinish; cdecl;
    procedure pushConfigurationViewController(viewController: UIViewController); cdecl;
    procedure reloadConfigurationItems; cdecl;
    procedure setAutoCompletionViewController(autoCompletionViewController: UIViewController); cdecl;
    procedure setCharactersRemaining(charactersRemaining: NSNumber); cdecl;
    procedure setPlaceholder(placeholder: NSString); cdecl;
    function textView: UITextView; cdecl;
    procedure validateContent; cdecl;
  end;
  TSLComposeServiceViewController = class(TOCGenericImport<SLComposeServiceViewControllerClass, SLComposeServiceViewController>) end;

  SLComposeViewControllerClass = interface(UIViewControllerClass)
    ['{74668937-8529-466E-94F7-61EA43AF005C}']
    {class} function composeViewControllerForServiceType(serviceType: NSString): SLComposeViewController; cdecl;
    {class} function isAvailableForServiceType(serviceType: NSString): Boolean; cdecl;
  end;

  SLComposeViewController = interface(UIViewController)
    ['{46269FBD-25AE-4C2A-B1F0-0ACBFEFC1F33}']
    function addImage(image: UIImage): Boolean; cdecl;
    function addURL(url: NSURL): Boolean; cdecl;
    function completionHandler: SLComposeViewControllerCompletionHandler; cdecl;
    function removeAllImages: Boolean; cdecl;
    function removeAllURLs: Boolean; cdecl;
    function serviceType: NSString; cdecl;
    procedure setCompletionHandler(completionHandler: SLComposeViewControllerCompletionHandler); cdecl;
    function setInitialText(text: NSString): Boolean; cdecl;
  end;
  TSLComposeViewController = class(TOCGenericImport<SLComposeViewControllerClass, SLComposeViewController>) end;

  SLComposeSheetConfigurationItemClass = interface(NSObjectClass)
    ['{21982D6F-2AD9-4847-A733-CB45E4F79A62}']
  end;

  SLComposeSheetConfigurationItem = interface(NSObject)
    ['{0CE91A6D-97A0-4A61-A424-C260D0936CCC}']
    procedure setTapHandler(tapHandler: SLComposeSheetConfigurationItemTapHandler); cdecl;
    procedure setTitle(title: NSString); cdecl;
    procedure setValue(value: NSString); cdecl;
    procedure setValuePending(valuePending: Boolean); cdecl;
    function tapHandler: SLComposeSheetConfigurationItemTapHandler; cdecl;
    function title: NSString; cdecl;
    function value: NSString; cdecl;
    function valuePending: Boolean; cdecl;
  end;
  TSLComposeSheetConfigurationItem = class(TOCGenericImport<SLComposeSheetConfigurationItemClass, SLComposeSheetConfigurationItem>) end;

function SLServiceTypeTwitter: NSString;
function SLServiceTypeFacebook: NSString;
function SLServiceTypeSinaWeibo: NSString;
function SLServiceTypeTencentWeibo: NSString;
function SLServiceTypeLinkedIn: NSString;

const
  libSocial = '/System/Library/Frameworks/Social.framework/Social';

implementation

uses
  Posix.Dlfcn;

var
  SocialModule: THandle;

function SLServiceTypeTwitter: NSString;
begin
  Result := CocoaNSStringConst(libSocial, 'SLServiceTypeTwitter');
end;

function SLServiceTypeFacebook: NSString;
begin
  Result := CocoaNSStringConst(libSocial, 'SLServiceTypeFacebook');
end;

function SLServiceTypeSinaWeibo: NSString;
begin
  Result := CocoaNSStringConst(libSocial, 'SLServiceTypeSinaWeibo');
end;

function SLServiceTypeTencentWeibo: NSString;
begin
  Result := CocoaNSStringConst(libSocial, 'SLServiceTypeTencentWeibo');
end;

function SLServiceTypeLinkedIn: NSString;
begin
  Result := CocoaNSStringConst(libSocial, 'SLServiceTypeLinkedIn');
end;

initialization
  SocialModule := dlopen(MarshaledAString(libSocial), RTLD_LAZY);

finalization
  dlclose(SocialModule);

end.