unit DW.iOSapi.Messages;

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
  Macapi.ObjectiveC, Macapi.CoreFoundation,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.CoreGraphics, iOSapi.UIKit;

const
  MSMessagesAppPresentationStyleCompact = 0;
  MSMessagesAppPresentationStyleExpanded = 1;
  MSMessagesAppPresentationStyleTranscript = 2;
  MSMessagesAppPresentationContextMessages = 0;
  MSMessagesAppPresentationContextMedia = 1;
  MSStickerSizeSmall = 0;
  MSStickerSizeRegular = 1;
  MSStickerSizeLarge = 2;
  MSMessageErrorCodeUnknown = -1;
  MSMessageErrorCodeFileNotFound = 1;
  MSMessageErrorCodeFileUnreadable = 2;
  MSMessageErrorCodeImproperFileType = 3;
  MSMessageErrorCodeImproperFileURL = 4;
  MSMessageErrorCodeStickerFileImproperFileAttributes = 5;
  MSMessageErrorCodeStickerFileImproperFileSize = 6;
  MSMessageErrorCodeStickerFileImproperFileFormat = 7;
  MSMessageErrorCodeURLExceedsMaxSize = 8;
  MSMessageErrorCodeSendWithoutRecentInteraction = 9;
  MSMessageErrorCodeSendWhileNotVisible = 10;
  MSMessageErrorCodeAPIUnavailableInPresentationContext = 11;

type
  MSMessagesAppTranscriptPresentation = interface;
  MSMessagesAppViewController = interface;
  MSConversation = interface;
  MSMessage = interface;
  MSMessageLayout = interface;
  MSMessageTemplateLayout = interface;
  MSSession = interface;
  MSSticker = interface;
  MSStickerBrowserViewDataSource = interface;
  MSStickerBrowserView = interface;
  MSStickerBrowserViewController = interface;
  MSStickerView = interface;
  MSMessageLiveLayout = interface;

  MSMessagesAppPresentationStyle = NSInteger;
  MSMessagesAppPresentationContext = NSInteger;
  MSStickerSize = NSInteger;
  MSMessageErrorCode = NSInteger;
  TMSConversationBlockMethod1 = procedure(error: NSError) of object;

  MSMessagesAppTranscriptPresentation = interface(IObjectiveC)
    ['{F153D64C-8959-4D28-B5A2-665EBAE03CF8}']
    function contentSizeThatFits(size: CGSize): CGSize; cdecl;
  end;

  MSMessagesAppViewControllerClass = interface(UIViewControllerClass)
    ['{E80D5E13-2CE5-4BFF-B37A-4DF03115DBC5}']
  end;

  MSMessagesAppViewController = interface(UIViewController)
    ['{788A7CC2-F145-467F-92EA-179367021E9B}']
    function activeConversation: MSConversation; cdecl;
    procedure didBecomeActiveWithConversation(conversation: MSConversation); cdecl;
    procedure didCancelSendingMessage(message: MSMessage; conversation: MSConversation); cdecl;
    procedure didReceiveMessage(message: MSMessage; conversation: MSConversation); cdecl;
    procedure didResignActiveWithConversation(conversation: MSConversation); cdecl;
    procedure didSelectMessage(message: MSMessage; conversation: MSConversation); cdecl;
    procedure didStartSendingMessage(message: MSMessage; conversation: MSConversation); cdecl;
    procedure didTransitionToPresentationStyle(presentationStyle: MSMessagesAppPresentationStyle); cdecl;
    procedure dismiss; cdecl;
    function presentationContext: MSMessagesAppPresentationContext; cdecl;
    function presentationStyle: MSMessagesAppPresentationStyle; cdecl;
    procedure requestPresentationStyle(presentationStyle: MSMessagesAppPresentationStyle); cdecl;
    procedure willBecomeActiveWithConversation(conversation: MSConversation); cdecl;
    procedure willResignActiveWithConversation(conversation: MSConversation); cdecl;
    procedure willSelectMessage(message: MSMessage; conversation: MSConversation); cdecl;
    procedure willTransitionToPresentationStyle(presentationStyle: MSMessagesAppPresentationStyle); cdecl;
  end;
  TMSMessagesAppViewController = class(TOCGenericImport<MSMessagesAppViewControllerClass, MSMessagesAppViewController>) end;

  MSConversationClass = interface(NSObjectClass)
    ['{47906467-82A7-4445-A5A6-A171576657A3}']
  end;

  MSConversation = interface(NSObject)
    ['{A455CD4A-C742-413A-BE86-D50EA060C685}']
    procedure insertAttachment(URL: NSURL; withAlternateFilename: NSString; completionHandler: TMSConversationBlockMethod1); cdecl;
    procedure insertMessage(message: MSMessage; completionHandler: TMSConversationBlockMethod1); cdecl;
    procedure insertSticker(sticker: MSSticker; completionHandler: TMSConversationBlockMethod1); cdecl;
    procedure insertText(text: NSString; completionHandler: TMSConversationBlockMethod1); cdecl;
    function localParticipantIdentifier: NSUUID; cdecl;
    function remoteParticipantIdentifiers: NSArray; cdecl;
    function selectedMessage: MSMessage; cdecl;
    procedure sendAttachment(URL: NSURL; withAlternateFilename: NSString; completionHandler: TMSConversationBlockMethod1); cdecl;
    procedure sendMessage(message: MSMessage; completionHandler: TMSConversationBlockMethod1); cdecl;
    procedure sendSticker(sticker: MSSticker; completionHandler: TMSConversationBlockMethod1); cdecl;
    procedure sendText(text: NSString; completionHandler: TMSConversationBlockMethod1); cdecl;
  end;
  TMSConversation = class(TOCGenericImport<MSConversationClass, MSConversation>) end;

  MSMessageClass = interface(NSObjectClass)
    ['{FA3E1409-12DE-416A-AAC2-92595FDD9627}']
  end;

  MSMessage = interface(NSObject)
    ['{319D592D-63C6-40A0-8347-55FC6B926F05}']
    function accessibilityLabel: NSString; cdecl;
    function error: NSError; cdecl;
    function initWithSession(session: MSSession): Pointer; cdecl;
    function isPending: Boolean; cdecl;
    function layout: MSMessageLayout; cdecl;
    function senderParticipantIdentifier: NSUUID; cdecl;
    function session: MSSession; cdecl;
    procedure setAccessibilityLabel(accessibilityLabel: NSString); cdecl;
    procedure setError(error: NSError); cdecl;
    procedure setLayout(layout: MSMessageLayout); cdecl;
    procedure setShouldExpire(shouldExpire: Boolean); cdecl;
    procedure setSummaryText(summaryText: NSString); cdecl;
    procedure setURL(URL: NSURL); cdecl;
    function shouldExpire: Boolean; cdecl;
    function summaryText: NSString; cdecl;
    function URL: NSURL; cdecl;
  end;
  TMSMessage = class(TOCGenericImport<MSMessageClass, MSMessage>) end;

  MSMessageLayoutClass = interface(NSObjectClass)
    ['{430225B5-39A6-475B-85C2-9651ECE19240}']
  end;

  MSMessageLayout = interface(NSObject)
    ['{D14C3DBD-7D14-4C25-ABC0-EB1C07A0697D}']
  end;
  TMSMessageLayout = class(TOCGenericImport<MSMessageLayoutClass, MSMessageLayout>) end;

  MSMessageTemplateLayoutClass = interface(MSMessageLayoutClass)
    ['{1721A9BC-F97B-4433-9AE5-EBF781803BD1}']
  end;

  MSMessageTemplateLayout = interface(MSMessageLayout)
    ['{9AE61F0C-4F46-4FDA-812B-004E761B1CAD}']
    function caption: NSString; cdecl;
    function image: UIImage; cdecl;
    function imageSubtitle: NSString; cdecl;
    function imageTitle: NSString; cdecl;
    function mediaFileURL: NSURL; cdecl;
    procedure setCaption(caption: NSString); cdecl;
    procedure setImage(image: UIImage); cdecl;
    procedure setImageSubtitle(imageSubtitle: NSString); cdecl;
    procedure setImageTitle(imageTitle: NSString); cdecl;
    procedure setMediaFileURL(mediaFileURL: NSURL); cdecl;
    procedure setSubcaption(subcaption: NSString); cdecl;
    procedure setTrailingCaption(trailingCaption: NSString); cdecl;
    procedure setTrailingSubcaption(trailingSubcaption: NSString); cdecl;
    function subcaption: NSString; cdecl;
    function trailingCaption: NSString; cdecl;
    function trailingSubcaption: NSString; cdecl;
  end;
  TMSMessageTemplateLayout = class(TOCGenericImport<MSMessageTemplateLayoutClass, MSMessageTemplateLayout>) end;

  MSSessionClass = interface(NSObjectClass)
    ['{6FA4830F-5AB6-4D8E-84F5-F4004BF67E2C}']
  end;

  MSSession = interface(NSObject)
    ['{7A711D89-48D5-4192-B23C-FEEF33D5C80D}']
  end;
  TMSSession = class(TOCGenericImport<MSSessionClass, MSSession>) end;

  MSStickerClass = interface(NSObjectClass)
    ['{B3DBF412-A44A-45A9-B1A0-C1263FD088B4}']
  end;

  MSSticker = interface(NSObject)
    ['{0000207F-FF67-4BFC-8991-38DDCB1798B9}']
    function imageFileURL: NSURL; cdecl;
    function initWithContentsOfFileURL(fileURL: NSURL; localizedDescription: NSString; error: PPointer): Pointer; cdecl;
    function localizedDescription: NSString; cdecl;
  end;
  TMSSticker = class(TOCGenericImport<MSStickerClass, MSSticker>) end;

  MSStickerBrowserViewDataSource = interface(IObjectiveC)
    ['{C56CE87A-74AD-4FBA-8C5E-7A518434FE9D}']
    function numberOfStickersInStickerBrowserView(stickerBrowserView: MSStickerBrowserView): NSInteger; cdecl;
    function stickerBrowserView(stickerBrowserView: MSStickerBrowserView; stickerAtIndex: NSInteger): MSSticker; cdecl;
  end;

  MSStickerBrowserViewClass = interface(UIViewClass)
    ['{F0C3345E-70A8-40F7-9164-0EFF513C8EFC}']
  end;

  MSStickerBrowserView = interface(UIView)
    ['{9A4C36A4-FD05-49EB-900A-3337C2C76AB5}']
    function contentInset: UIEdgeInsets; cdecl;
    function contentOffset: CGPoint; cdecl;
    function dataSource: Pointer; cdecl;
    function initWithFrame(frame: CGRect; stickerSize: MSStickerSize): Pointer; overload; cdecl;
    function initWithFrame(frame: CGRect): Pointer; overload; cdecl;
    procedure reloadData; cdecl;
    procedure setContentInset(contentInset: UIEdgeInsets); cdecl;
    procedure setContentOffset(contentOffset: CGPoint); overload; cdecl;
    procedure setContentOffset(contentOffset: CGPoint; animated: Boolean); overload; cdecl;
    procedure setDataSource(dataSource: Pointer); cdecl;
    function stickerSize: MSStickerSize; cdecl;
  end;
  TMSStickerBrowserView = class(TOCGenericImport<MSStickerBrowserViewClass, MSStickerBrowserView>) end;

  MSStickerBrowserViewControllerClass = interface(UIViewControllerClass)
    ['{800E7B9C-6B6A-44F8-BB45-BFA96554B322}']
  end;

  MSStickerBrowserViewController = interface(UIViewController)
    ['{6632764D-2E01-455C-B708-5E760EF34095}']
    function initWithStickerSize(stickerSize: MSStickerSize): Pointer; cdecl;
    function stickerBrowserView: MSStickerBrowserView; cdecl;
    function stickerSize: MSStickerSize; cdecl;
  end;
  TMSStickerBrowserViewController = class(TOCGenericImport<MSStickerBrowserViewControllerClass, MSStickerBrowserViewController>) end;

  MSStickerViewClass = interface(UIViewClass)
    ['{5B55C64C-2850-4F7E-AC83-933912FC4562}']
  end;

  MSStickerView = interface(UIView)
    ['{9475AB47-6FC2-405D-9B61-FC9BDD0697B2}']
    function animationDuration: NSTimeInterval; cdecl;
    function initWithFrame(frame: CGRect; sticker: MSSticker): Pointer; cdecl;
    function isAnimating: Boolean; cdecl;
    procedure setSticker(sticker: MSSticker); cdecl;
    procedure startAnimating; cdecl;
    function sticker: MSSticker; cdecl;
    procedure stopAnimating; cdecl;
  end;
  TMSStickerView = class(TOCGenericImport<MSStickerViewClass, MSStickerView>) end;

  MSMessageLiveLayoutClass = interface(MSMessageLayoutClass)
    ['{8C8521E0-9037-4BD4-88FC-2C5959CD1AA3}']
  end;

  MSMessageLiveLayout = interface(MSMessageLayout)
    ['{71453FE8-D9DD-4FB0-813F-8737C7169572}']
    function alternateLayout: MSMessageTemplateLayout; cdecl;
    function initWithAlternateLayout(alternateLayout: MSMessageTemplateLayout): Pointer; cdecl;
  end;
  TMSMessageLiveLayout = class(TOCGenericImport<MSMessageLiveLayoutClass, MSMessageLiveLayout>) end;

function MSStickersErrorDomain: NSString;
function MSMessagesErrorDomain: NSString;

const
  libMessages = '/System/Library/Frameworks/Messages.framework/Messages';

implementation

{$IF Defined(IOS) and not Defined(CPUARM)}
uses
  // Posix
  Posix.Dlfcn;

var
  MessagesModule: THandle;
{$ENDIF}

function MSStickersErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libMessages, 'MSStickersErrorDomain');
end;

function MSMessagesErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libMessages, 'MSMessagesErrorDomain');
end;

{$IF Defined(IOS) and not Defined(CPUARM)}
initialization
  MessagesModule := dlopen(MarshaledAString(libMessages), RTLD_LAZY);

finalization
  dlclose(MessagesModule)
{$ENDIF}

end.