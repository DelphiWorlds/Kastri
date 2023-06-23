unit DW.iOSapi.Twitter;

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
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit,
  // DW
  DW.iOSapi.Social, DW.iOSapi.Accounts;

const
  TWRequestMethodGET = SLRequestMethodGET;
  TWRequestMethodPOST = SLRequestMethodPOST;
  TWRequestMethodDELETE = SLRequestMethodDELETE;
  TWTweetComposeViewControllerResultCancelled = SLComposeViewControllerResultCancelled;
  TWTweetComposeViewControllerResultDone = SLComposeViewControllerResultDone;

type
  TWRequest = interface;
  TWTweetComposeViewController = interface;

  TWRequestMethod = SLRequestMethod;
  TWRequestHandler = SLRequestHandler;
  TWTweetComposeViewControllerResult = SLComposeViewControllerResult;
  TWTweetComposeViewControllerCompletionHandler = SLComposeViewControllerCompletionHandler;

  TWRequestClass = interface(NSObjectClass)
    ['{FB03EB4F-BF95-4EAA-9F7F-942DBD492A0E}']
  end;

  TWRequest = interface(NSObject)
    ['{1F084355-A349-42EF-80D8-0293F604D2E0}']
    function account: ACAccount; cdecl;
    procedure addMultiPartData(data: NSData; withName: NSString; &type: NSString); cdecl;
    function initWithURL(url: NSURL; parameters: NSDictionary; requestMethod: TWRequestMethod): Pointer; cdecl;
    function parameters: NSDictionary; cdecl;
    procedure performRequestWithHandler(handler: TWRequestHandler); cdecl;
    function requestMethod: TWRequestMethod; cdecl;
    procedure setAccount(account: ACAccount); cdecl;
    function signedURLRequest: NSURLRequest; cdecl;
    function URL: NSURL; cdecl;
  end;
  TTWRequest = class(TOCGenericImport<TWRequestClass, TWRequest>) end;

  TWTweetComposeViewControllerClass = interface(UIViewControllerClass)
    ['{7F183FD4-F2B5-42D2-9F5F-8FDE84F11962}']
    {class} function canSendTweet: Boolean; cdecl;
  end;

  TWTweetComposeViewController = interface(UIViewController)
    ['{0125D5FF-F56C-458C-B042-883464EDC434}']
    function addImage(image: UIImage): Boolean; cdecl;
    function addURL(url: NSURL): Boolean; cdecl;
    function completionHandler: TWTweetComposeViewControllerCompletionHandler; cdecl;
    function removeAllImages: Boolean; cdecl;
    function removeAllURLs: Boolean; cdecl;
    procedure setCompletionHandler(completionHandler: TWTweetComposeViewControllerCompletionHandler); cdecl;
    function setInitialText(text: NSString): Boolean; cdecl;
  end;
  TTWTweetComposeViewController = class(TOCGenericImport<TWTweetComposeViewControllerClass, TWTweetComposeViewController>) end;

const
  libTwitter = '/System/Library/Frameworks/Twitter.framework/Twitter';

implementation

uses
  Posix.Dlfcn;

var
  TwitterModule: THandle;

initialization
  TwitterModule := dlopen(MarshaledAString(libTwitter), RTLD_LAZY);

finalization
  dlclose(TwitterModule);

end.