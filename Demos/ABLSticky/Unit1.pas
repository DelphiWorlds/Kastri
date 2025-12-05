unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Messaging,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Layouts,
  DW.ServiceClient.Android,
  Androidapi.JNI.GraphicsContentViewText, DW.MultiReceiver.Android;

type
  TServiceMessageProc = reference to procedure(const AMessage: string);

  TServiceMessageReceiver = class(TMultiReceiver)
  private
    FHandler: TServiceMessageProc;
  protected
    procedure ConfigureActions; override;
    procedure Receive(context: JContext; intent: JIntent); override;
  public
    constructor Create(const AHandler: TServiceMessageProc);
  end;

  TForm1 = class(TForm)
    ButtonLayout: TLayout;
    StartButton: TButton;
    ClearButton: TButton;
    MessagesMemo: TMemo;
    procedure StartButtonClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
  private
    FServiceClient: IServiceClient;
    FServiceMessageReceiver: TServiceMessageReceiver;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
    procedure ServiceMessageReceiverMessageHandler(const AMessage: string);
    procedure StartService;
   public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  DW.LocationPermissions,
  Androidapi.Helpers,
  FMX.Platform,
  DW.Consts.Android;

{ TServiceMessageReceiver }

constructor TServiceMessageReceiver.Create(const AHandler: TServiceMessageProc);
begin
  inherited Create(True);
  FHandler := AHandler;
end;

procedure TServiceMessageReceiver.ConfigureActions;
begin
  IntentFilter.addAction(StringToJString(cServiceMessageAction));
end;

procedure TServiceMessageReceiver.Receive(context: JContext; intent: JIntent);
begin
  FHandler(JStringToString(intent.getStringExtra(StringToJString(cServiceBroadcastParamMessage))));
end;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
  FServiceClient := TServiceClient.Create('ABLStickyService');
  FServiceMessageReceiver := TServiceMessageReceiver.Create(ServiceMessageReceiverMessageHandler);
end;

destructor TForm1.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  FServiceMessageReceiver.Free;
  inherited;
end;

procedure TForm1.ServiceMessageReceiverMessageHandler(const AMessage: string);
begin
  MessagesMemo.Lines.Add(FormatDateTime('hh:nn:ss.zzz', Now) + ': ' + AMessage);
end;

procedure TForm1.ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  case TApplicationEventMessage(AMsg).Value.Event of
    TApplicationEvent.WillTerminate:
    begin
      // Start the service when the app terminates in case the app is "force" quit
      if LocationPermissions.HasRequiredPermissions then
        StartService;
    end;
  end;
end;

procedure TForm1.ClearButtonClick(Sender: TObject);
begin
  MessagesMemo.Lines.Clear;
end;

procedure TForm1.StartButtonClick(Sender: TObject);
begin
  LocationPermissions.RequestBackground(
    procedure(const AState: TPermissionsState)
    begin
      if AState <> TPermissionsState.Denied then
        StartService;
      // else perhaps show a message
    end
  );
end;

procedure TForm1.StartService;
begin
  FServiceClient.Start;
end;

end.
