unit DW.SMS.iOS;

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
  Macapi.ObjectiveC,
  // DW
  DW.SMS, DW.iOSapi.MessageUI, DW.Types;

type
  TPlatformSMS = class;

  TMFMessageComposeViewControllerDelegate = class(TOCLocal, MFMessageComposeViewControllerDelegate)
  private
    FPlatformSMS: TPlatformSMS;
  public
    { MFMessageComposeViewControllerDelegate }
    procedure messageComposeViewController(controller: MFMessageComposeViewController; didFinishWithResult: MessageComposeResult); cdecl;
  public
    constructor Create(const APlatformSMS: TPlatformSMS);
  end;

  TPlatformSMS = class(TCustomPlatformSMS)
  private
    FController: MFMessageComposeViewController;
    FDelegate: TMFMessageComposeViewControllerDelegate;
    FDestinations: TArray<string>;
  protected
    function GetAuthorizationStatus: TAuthorizationStatus; override;
    procedure HandleMessageComposeResult(const AMessageComposeResult: MessageComposeResult);
    procedure RequestPermission; override;
    procedure SendTextMessage(const AText: string; const ADestinations: TArray<string>); override;
  public
    constructor Create(const ASMS: TSMS); override;
    destructor Destroy; override;
  end;

implementation

uses
  // macOS
  Macapi.Helpers,
  // iOS
  iOSapi.Helpers,
  // DW
  DW.Macapi.Helpers;

{ TMFMessageComposeViewControllerDelegate }

constructor TMFMessageComposeViewControllerDelegate.Create(const APlatformSMS: TPlatformSMS);
begin
  inherited Create;
  FPlatformSMS := APlatformSMS;
end;

procedure TMFMessageComposeViewControllerDelegate.messageComposeViewController(controller: MFMessageComposeViewController;
  didFinishWithResult: MessageComposeResult);
begin
  controller.dismissModalViewControllerAnimated(True);
  FPlatformSMS.HandleMessageComposeResult(didFinishWithResult);
end;

{ TPlatformSMS }

constructor TPlatformSMS.Create(const ASMS: TSMS);
begin
  inherited;
  FDelegate := TMFMessageComposeViewControllerDelegate.Create(Self);
end;

destructor TPlatformSMS.Destroy;
begin
  FDelegate.Free;
  inherited;
end;

function TPlatformSMS.GetAuthorizationStatus: TAuthorizationStatus;
begin
  if TMFMessageComposeViewController.OCClass.canSendText then
    Result := TAuthorizationStatus.Authorized
  else
    Result := TAuthorizationStatus.Denied;
end;

procedure TPlatformSMS.HandleMessageComposeResult(const AMessageComposeResult: MessageComposeResult);
var
  LMessageResult: TMessageResult;
begin
  case AMessageComposeResult of
    MessageComposeResultCancelled:
      LMessageResult := TMessageResult.Cancelled;
    MessageComposeResultSent:
      LMessageResult := TMessageResult.Sent;
    MessageComposeResultFailed:
      LMessageResult := TMessageResult.Failed;
  else
    LMessageResult := TMessageResult.Failed;
  end;
  DoMessageResult(FDestinations, LMessageResult);
end;

procedure TPlatformSMS.RequestPermission;
begin
  DoPermissionRequestResult(True);
end;

procedure TPlatformSMS.SendTextMessage(const AText: string; const ADestinations: TArray<string>);
begin
  // Should check: TMFMessageComposeViewController.OCClass.canSendText;
  FController := nil;
  FController := TMFMessageComposeViewController.Create;
  FController.setMessageComposeDelegate(FDelegate.GetObjectID);
  FDestinations := ADestinations;
  FController.setBody(StrToNSStr(AText));
  FController.setRecipients(StringArrayToNSArray(ADestinations));
  TiOSHelper.SharedApplication.keyWindow.rootViewController.presentModalViewController(FController, True);
end;

end.
