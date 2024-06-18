unit DW.iOSapi.LinkPresentation;

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
  // iOSapi
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit,
  // DW
  DW.iOSapi.Foundation;

const
  LPErrorUnknown = 1;
  LPErrorMetadataFetchFailed = 2;
  LPErrorMetadataFetchCancelled = 3;
  LPErrorMetadataFetchTimedOut = 4;
  LPErrorMetadataFetchNotAllowed = 5;

type
  LPLinkMetadata = interface;
  LPLinkView = interface;
  LPMetadataProvider = interface;

  LPErrorCode = NSInteger;
  TLPMetadataProviderBlockMethod1 = procedure(metadata: LPLinkMetadata; error: NSError) of object;

  LPLinkMetadataClass = interface(NSObjectClass)
    ['{F23C5FE9-D421-4E8D-9F6A-C29ACD9A410E}']
  end;

  LPLinkMetadata = interface(NSObject)
    ['{774F8C9B-8E4A-4614-993D-0FE3418202D5}']
    function iconProvider: NSItemProvider; cdecl;
    function imageProvider: NSItemProvider; cdecl;
    function originalURL: NSURL; cdecl;
    function remoteVideoURL: NSURL; cdecl;
    procedure setIconProvider(iconProvider: NSItemProvider); cdecl;
    procedure setImageProvider(imageProvider: NSItemProvider); cdecl;
    procedure setOriginalURL(originalURL: NSURL); cdecl;
    procedure setRemoteVideoURL(remoteVideoURL: NSURL); cdecl;
    procedure setTitle(title: NSString); cdecl;
    procedure setURL(URL: NSURL); cdecl;
    procedure setVideoProvider(videoProvider: NSItemProvider); cdecl;
    function title: NSString; cdecl;
    function URL: NSURL; cdecl;
    function videoProvider: NSItemProvider; cdecl;
  end;
  TLPLinkMetadata = class(TOCGenericImport<LPLinkMetadataClass, LPLinkMetadata>) end;

  LPLinkViewClass = interface(UIViewClass)
    ['{1F9FE5B9-7221-4F72-B678-2C5188BC2317}']
  end;

  LPLinkView = interface(UIView)
    ['{71AA0D15-07D0-4F54-A667-D62507DCB5F3}']
    procedure encodeWithCoder(coder: NSCoder); cdecl;
    function initWithCoder(coder: NSCoder): Pointer; cdecl;
    function initWithMetadata(metadata: LPLinkMetadata): Pointer; cdecl;
    function initWithURL(URL: NSURL): Pointer; cdecl;
    function metadata: LPLinkMetadata; cdecl;
    procedure setMetadata(metadata: LPLinkMetadata); cdecl;
  end;
  TLPLinkView = class(TOCGenericImport<LPLinkViewClass, LPLinkView>) end;

  LPMetadataProviderClass = interface(NSObjectClass)
    ['{4B71C695-F3E4-4D8F-9549-7A083C1A8B62}']
  end;

  LPMetadataProvider = interface(NSObject)
    ['{51F0511F-D6D0-466C-B465-DEDEF5F4EF77}']
    procedure cancel; cdecl;
    procedure setShouldFetchSubresources(shouldFetchSubresources: Boolean); cdecl;
    procedure setTimeout(timeout: NSTimeInterval); cdecl;
    function shouldFetchSubresources: Boolean; cdecl;
    procedure startFetchingMetadataForRequest(request: NSURLRequest; completionHandler: TLPMetadataProviderBlockMethod1); cdecl;
    procedure startFetchingMetadataForURL(URL: NSURL; completionHandler: TLPMetadataProviderBlockMethod1); cdecl;
    function timeout: NSTimeInterval; cdecl;
  end;
  TLPMetadataProvider = class(TOCGenericImport<LPMetadataProviderClass, LPMetadataProvider>) end;

function LPErrorDomain: NSErrorDomain;

const
  libLinkPresentation = '/System/Library/Frameworks/LinkPresentation.framework/LinkPresentation';

implementation

uses
  Posix.Dlfcn;

var
  LinkPresentationModule: THandle;

function LPErrorDomain: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libLinkPresentation, 'LPErrorDomain');
end;

initialization
  LinkPresentationModule := dlopen(MarshaledAString(libLinkPresentation), RTLD_LAZY);

finalization
  dlclose(LinkPresentationModule);

end.