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
    procedure ClearMessagesButtonClick(Sender: TObject);
  private
    FShown: Boolean;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
    procedure FCMMessageReceivedHandler(Sender: TObject; const AJSON: TJSONObject);
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
  // Turn ShowBannerIfForeground OFF when using data ONLY messages
  FCM.ShowBannerIfForeground := False;
  FCM.OnMessageReceived := FCMMessageReceivedHandler;
  FCM.OnStarted := FCMStartedHandler;
  FCM.OnTokenReceived := FCMTokenReceivedHandler;
  FCM.Start;
end;

destructor TfrmMain.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  inherited;
end;

procedure TfrmMain.ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  case TApplicationEventMessage(AMsg).Value.Event of
    TApplicationEvent.BecameActive:
    begin
      if not FShown then
      begin
        PermissionsService.RequestPermissions([cPermissionSendSMS],
          procedure(const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray)
          begin
            // No action needed with permissions result. If granted, the relay service will be able to send SMS messages
          end
        );
      end;
      FShown := True;
    end;
  end;
end;

procedure TfrmMain.ClearMessagesButtonClick(Sender: TObject);
begin
  MessagesMemo.Lines.Clear;
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
