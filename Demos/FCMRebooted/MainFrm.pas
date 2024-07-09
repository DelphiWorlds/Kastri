unit MainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Json, System.Messaging,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Layouts, FMX.Objects, FMX.Memo.Types,
  DW.FCMManager;

type
  TfrmMain = class(TForm)
    ContentLayout: TLayout;
    ClearMessagesButton: TButton;
    FirebaseCMLabel: TLabel;
    MessagesLabel: TLabel;
    MessagesMemo: TMemo;
    TokenLabel: TLabel;
    TokenMemo: TMemo;
    RemoveNotificationsButton: TButton;
    procedure ClearMessagesButtonClick(Sender: TObject);
    procedure RemoveNotificationsButtonClick(Sender: TObject);
  private
    FShown: Boolean;
    procedure AddCategories;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
    procedure Category1Handler;
    procedure Category2ConfirmHandler;
    procedure Category2DenyHandler;
    procedure FCMMessageReceivedHandler(Sender: TObject; const AJSON: TJSONObject);
    procedure FCMNotificationCategoryHandler(Sender: TObject; const ACategory, AAction: string);
    procedure FCMTokenReceivedHandler(Sender: TObject; const AToken: string);
    procedure FCMStartedHandler(Sender: TObject);
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  System.Permissions,
  FMX.Platform,
  DW.OSLog, DW.UIHelper, DW.Consts.Android;

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
  // Please ensure that you add categories *before* calling Start
  AddCategories;
  FCM.ShowBannerIfForeground := True;
  FCM.OnMessageReceived := FCMMessageReceivedHandler;
  FCM.OnStarted := FCMStartedHandler;
  FCM.OnTokenReceived := FCMTokenReceivedHandler;
  FCM.OnNotificationCategory := FCMNotificationCategoryHandler;
  FCM.Start;
end;

destructor TfrmMain.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  inherited;
end;

procedure TfrmMain.Category2ConfirmHandler;
begin
  MessagesMemo.Lines.Add('User responded to confirm action of category2');
end;

procedure TfrmMain.Category2DenyHandler;
begin
  MessagesMemo.Lines.Add('User responded to deny action of category2');
end;

procedure TfrmMain.FCMNotificationCategoryHandler(Sender: TObject; const ACategory, AAction: string);
begin
  if AAction.IsEmpty then
    MessagesMemo.Lines.Add(Format('User responded to category: %s', [ACategory]))
  else
    MessagesMemo.Lines.Add(Format('User responded to action: %s of category: %s', [AAction, ACategory]));
end;

procedure TfrmMain.AddCategories;
var
  LCategory: INotificationCategory;
begin
  // Using **specific** handlers:
  LCategory := FCM.AddCategory('category1', Category1Handler);
  LCategory := FCM.AddCategory('category2');
  LCategory.AddAction('action1', 'Confirm', Category2ConfirmHandler);
  LCategory.AddAction('action2', 'Deny', Category2DenyHandler);
  // Using generic handler:
  LCategory := FCM.AddCategory('category3');
  LCategory := FCM.AddCategory('category4');
  LCategory.AddAction('action3', 'Accept');
  LCategory.AddAction('action4', 'Reject');
end;

procedure TfrmMain.ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  case TApplicationEventMessage(AMsg).Value.Event of
    TApplicationEvent.BecameActive:
    begin
      if not FShown then
      begin
        {$IF Defined(RELAYDEMO)}
        PermissionsService.RequestPermissions([cPermissionSendSMS],
          procedure(const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray)
          begin
            // No action needed with permissions result. If granted, the relay service will be able to send SMS messages
          end
        );
        {$ENDIF}
      end;
      FShown := True;
    end;
  end;
end;

procedure TfrmMain.Category1Handler;
begin
  MessagesMemo.Lines.Add('User responded to a notification of category1');
end;

procedure TfrmMain.ClearMessagesButtonClick(Sender: TObject);
begin
  MessagesMemo.Lines.Clear;
end;

procedure TfrmMain.RemoveNotificationsButtonClick(Sender: TObject);
begin
  FCM.RemoveNotifications;
end;

procedure TfrmMain.Resize;
begin
  inherited;
  // Spacing for iPhoneX etc display
  ContentLayout.Margins.Rect := TUIHelper.GetOffsetRect;
end;

procedure TfrmMain.FCMMessageReceivedHandler(Sender: TObject; const AJSON: TJSONObject);
begin
  TOSLog.d(AJSON.ToString);
  MessagesMemo.Lines.Add(AJSON.ToString);
end;

procedure TfrmMain.FCMStartedHandler(Sender: TObject);
begin
  FCM.SubscribeToTopic('FCMRebooted');
end;

procedure TfrmMain.FCMTokenReceivedHandler(Sender: TObject; const AToken: string);
begin
  TokenMemo.Text := AToken;
end;

end.
