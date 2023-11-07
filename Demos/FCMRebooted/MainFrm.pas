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
    procedure FCMTokenReceivedHandler(Sender: TObject; const AToken: string);
    procedure FCMStartedHandler(Sender: TObject);
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
  FCM.OnStarted := FCMStartedHandler;
  FCM.OnTokenReceived := FCMTokenReceivedHandler;
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
  TOSLog.d(AJSON.ToString);
  MessagesMemo.Lines.Add(AJSON.ToString);
end;

procedure TfrmMain.FCMStartedHandler(Sender: TObject);
begin
  FCM.SubscribeToTopic('FCMRebooted2');
end;

procedure TfrmMain.FCMTokenReceivedHandler(Sender: TObject; const AToken: string);
begin
  TokenMemo.Text := AToken;
end;

end.
