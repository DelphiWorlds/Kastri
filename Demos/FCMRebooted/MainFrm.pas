unit MainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Json,
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
    procedure FCMMessageReceivedHandler(Sender: TObject; const AJSON: TJSONObject);
    procedure FCMStatusChangeHandler(Sender: TObject);
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  DW.OSLog, DW.UIHelper;

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  // Turn ShowBannerIfForeground OFF when using data ONLY messages
  FCM.ShowBannerIfForeground := False;
  FCM.OnMessageReceived := FCMMessageReceivedHandler;
  FCM.OnStatusChange := FCMStatusChangeHandler;
  FCM.Start;
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
  MessagesMemo.Lines.Add(AJSON.ToString);
end;

procedure TfrmMain.FCMStatusChangeHandler(Sender: TObject);
var
  LToken: string;
begin
  if FCM.IsStarted then
  begin
    LToken := FCM.GetToken;
    TokenMemo.Lines.Text := LToken;
    if not LToken.IsEmpty then
      TOSLog.d('Token: %s', [LToken]);
    FCM.SubscribeToTopic('FCMRebooted');
    TOSLog.d('Subscribed to FCMRebooted');
  end;
end;

end.
