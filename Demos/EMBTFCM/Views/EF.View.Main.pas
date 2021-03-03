unit EF.View.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Json,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Layouts,
  FMX.StdCtrls, FMX.Memo.Types,
  DW.PushNotification;

type
  TMainView = class(TForm)
    TabControl: TTabControl;
    LogTab: TTabItem;
    LogMemo: TMemo;
    LogBottomLayout: TLayout;
    LogClearButton: TButton;
    procedure LogClearButtonClick(Sender: TObject);
  private
    FPushNotifications: TPushNotifications;
    procedure PushNotificationsMessageReceivedHandler(Sender: TObject; const AJSON: TJSONObject);
    procedure PushNotificationsTokenReceivedHandler(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainView: TMainView;

implementation

{$R *.fmx}

uses
  System.IOUtils;

{ TMainView }

constructor TMainView.Create(AOwner: TComponent);
begin
  inherited;
  FPushNotifications := TPushNotifications.Create('EMBTFCM Push Notifications');
  FPushNotifications.OnMessageReceived := PushNotificationsMessageReceivedHandler;
  FPushNotifications.OnTokenReceived := PushNotificationsTokenReceivedHandler;
  FPushNotifications.Start;
end;

destructor TMainView.Destroy;
begin
  FPushNotifications.Free;
  inherited;
end;

procedure TMainView.LogClearButtonClick(Sender: TObject);
begin
  LogMemo.Lines.Clear;
end;

procedure TMainView.PushNotificationsMessageReceivedHandler(Sender: TObject; const AJSON: TJSONObject);
begin
  LogMemo.Lines.Add(Format('Message Received - JSON: %s', [AJSON.ToString]));
  Log.d('Message Received - JSON: %s', [AJSON.ToString]);
end;

procedure TMainView.PushNotificationsTokenReceivedHandler(Sender: TObject);
begin
  LogMemo.Lines.Add(Format('Token Received: %s', [FPushNotifications.DeviceToken]));
  LogMemo.Lines.Add(Format('DeviceID: %s', [FPushNotifications.DeviceID]));
  Log.d('Token Received: %s', [FPushNotifications.DeviceToken]);
  Log.d('DeviceID: %s', [FPushNotifications.DeviceID]);
end;

end.
