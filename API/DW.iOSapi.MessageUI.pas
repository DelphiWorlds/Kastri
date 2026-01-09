unit DW.iOSapi.MessageUI;

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
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit,
  // DW
  DW.iOSapi.Messages;

const
  MessageComposeResultCancelled = 0;
  MessageComposeResultSent = 1;
  MessageComposeResultFailed = 2;
  MFMailComposeResultCancelled = 0;
  MFMailComposeResultSaved = 1;
  MFMailComposeResultSent = 2;
  MFMailComposeResultFailed = 3;
  MFMailComposeErrorCodeSaveFailed = 0;
  MFMailComposeErrorCodeSendFailed = 1;

type
  MFMessageComposeViewController = interface;
  MFMessageComposeViewControllerDelegate = interface;
  MFMailComposeViewController = interface;
  MFMailComposeViewControllerDelegate = interface;
  MessageComposeResult = NSInteger;
  MFMailComposeResult = NSInteger;
  MFMailComposeErrorCode = NSInteger;
  NSErrorDomain = NSString;

  MFMessageComposeViewControllerClass = interface(UINavigationControllerClass)
    ['{F51EBD39-97CE-4403-A1D5-9D9E127AB886}']
    {class} function canSendAttachments: Boolean; cdecl;
    {class} function canSendSubject: Boolean; cdecl;
    {class} function canSendText: Boolean; cdecl;
    {class} function isSupportedAttachmentUTI(uti: NSString): Boolean; cdecl;
  end;

  MFMessageComposeViewController = interface(UINavigationController)
    ['{D94A2600-9573-4ABF-B53D-4104C4BEFBCC}']
    function addAttachmentData(attachmentData: NSData; typeIdentifier: NSString; filename: NSString): Boolean; cdecl;
    function addAttachmentURL(attachmentURL: NSURL; withAlternateFilename: NSString): Boolean; cdecl;
    function attachments: NSArray; cdecl;
    function body: NSString; cdecl;
    procedure disableUserAttachments; cdecl;
    function message: MSMessage; cdecl;
    function messageComposeDelegate: Pointer; cdecl;
    function recipients: NSArray; cdecl;
    procedure setBody(body: NSString); cdecl;
    procedure setMessage(message: MSMessage); cdecl;
    procedure setMessageComposeDelegate(messageComposeDelegate: Pointer); cdecl;
    procedure setRecipients(recipients: NSArray); cdecl;
    procedure setSubject(subject: NSString); cdecl;
    function subject: NSString; cdecl;
  end;

  TMFMessageComposeViewController = class(TOCGenericImport<MFMessageComposeViewControllerClass, MFMessageComposeViewController>)
  end;

  MFMessageComposeViewControllerDelegate = interface(IObjectiveC)
    ['{DAE97AD5-F149-4FC8-8516-A8DAC142D3F5}']
    procedure messageComposeViewController(controller: MFMessageComposeViewController; didFinishWithResult: MessageComposeResult); cdecl;
  end;

  MFMailComposeViewControllerClass = interface(UINavigationControllerClass)
    ['{C0942D95-7663-4D4D-88E4-A206BCE35F7A}']
    {class} function canSendMail: Boolean; cdecl;
  end;

  MFMailComposeViewController = interface(UINavigationController)
    ['{B389FBA8-C505-4503-BF9C-44322A8F57B6}']
    procedure addAttachmentData(attachment: NSData; mimeType: NSString; fileName: NSString); cdecl;
    function mailComposeDelegate: Pointer; cdecl;
    procedure setBccRecipients(bccRecipients: NSArray); cdecl;
    procedure setCcRecipients(ccRecipients: NSArray); cdecl;
    procedure setMailComposeDelegate(mailComposeDelegate: Pointer); cdecl;
    procedure setMessageBody(body: NSString; isHTML: Boolean); cdecl;
    procedure setPreferredSendingEmailAddress(emailAddress: NSString); cdecl;
    procedure setSubject(subject: NSString); cdecl;
    procedure setToRecipients(toRecipients: NSArray); cdecl;
  end;

  TMFMailComposeViewController = class(TOCGenericImport<MFMailComposeViewControllerClass, MFMailComposeViewController>)
  end;

  MFMailComposeViewControllerDelegate = interface(IObjectiveC)
    ['{D4A4F0C3-0FE8-4FD4-AA98-1135F536E042}']
    procedure mailComposeController(controller: MFMailComposeViewController; didFinishWithResult: MFMailComposeResult; error: NSError); cdecl;
  end;

function MFMessageComposeViewControllerAttachmentURL: NSString;
function MFMessageComposeViewControllerAttachmentAlternateFilename: NSString;
function MFMessageComposeViewControllerTextMessageAvailabilityDidChangeNotification: NSString;
function MFMessageComposeViewControllerTextMessageAvailabilityKey: NSString;
function MFMailComposeErrorDomain: NSErrorDomain;

const
  libMessageUI = '/System/Library/Frameworks/MessageUI.framework/MessageUI';

implementation

uses
  // Posix
  Posix.Dlfcn;

var
  MessageUIModule: THandle;

function MFMessageComposeViewControllerAttachmentURL: NSString;
begin
  Result := CocoaNSStringConst(libMessageUI, 'MFMessageComposeViewControllerAttachmentURL');
end;

function MFMessageComposeViewControllerAttachmentAlternateFilename: NSString;
begin
  Result := CocoaNSStringConst(libMessageUI, 'MFMessageComposeViewControllerAttachmentAlternateFilename');
end;

function MFMessageComposeViewControllerTextMessageAvailabilityDidChangeNotification: NSString;
begin
  Result := CocoaNSStringConst(libMessageUI, 'MFMessageComposeViewControllerTextMessageAvailabilityDidChangeNotification');
end;

function MFMessageComposeViewControllerTextMessageAvailabilityKey: NSString;
begin
  Result := CocoaNSStringConst(libMessageUI, 'MFMessageComposeViewControllerTextMessageAvailabilityKey');
end;

function MFMailComposeErrorDomain: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libMessageUI, 'MFMailComposeErrorDomain');
end;

initialization
  MessageUIModule := dlopen(MarshaledAString(libMessageUI), RTLD_LAZY);

finalization
  dlclose(MessageUIModule)

end.

