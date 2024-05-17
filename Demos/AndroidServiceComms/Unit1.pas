unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Android.Service,
  AndroidApi.JNI.GraphicsContentViewText,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls,
  ASC.ServiceModule, ASC.Common, FMX.Objects;

type
  TForm1 = class(TForm)
    ConnectButton: TButton;
    Memo1: TMemo;
    PingButton: TButton;
    UseBindingCheckBox: TCheckBox;
    Image1: TImage;
    ImageButton: TButton;
    procedure ConnectButtonClick(Sender: TObject);
    procedure PingButtonClick(Sender: TObject);
    procedure ImageButtonClick(Sender: TObject);
  private
    FConnection: TLocalServiceConnection;
    // FReceiver and ReceiverReceiveHandler are for when *not* using service "binding"
    FReceiver: TLocalBroadcastReceiver;
    FService: TServiceModule;
    procedure DecodeImage(const ABase64: string);
    procedure ReceiverReceiveHandler(const AIntent: JIntent);
    procedure SendData(const AData: string);
    procedure ServiceConnectedHandler(const ALocalService: TAndroidBaseService);
    procedure ServiceDisconnectedHandler;
    procedure ServiceSendDataHandler(Sender: TObject; const AData: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  AndroidApi.JNI.JavaTypes, Androidapi.Helpers,
  DW.Androidapi.JNI.AndroidX.LocalBroadcastManager, DW.Android.Helpers, DW.Base64.Helpers;

const
  cServiceName = 'com.embarcadero.services.AndroidServiceCommsDemoService';

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  // FReceiver is needed for when not using service "binding"
  FReceiver := TLocalBroadcastReceiver.Create([cActionAppMessage], ReceiverReceiveHandler);

  // FConnection is needed for when using service "binding"
  FConnection := TLocalServiceConnection.Create;
  FConnection.OnConnected := ServiceConnectedHandler;
  FConnection.OnDisconnected := ServiceDisconnectedHandler;
end;

destructor TForm1.Destroy;
begin
  FReceiver.Free;
  FConnection.Free;
  inherited;
end;

procedure TForm1.ImageButtonClick(Sender: TObject);
begin
  SendData(TDataMessage.CreateCommandMessage('image').ToJSON);
end;

procedure TForm1.PingButtonClick(Sender: TObject);
var
  LMessage: TDataMessage;
begin
  LMessage := TDataMessage.CreateCommandMessage('ping');
  if not UseBindingCheckBox.IsChecked then
    SendData(LMessage.ToJSON)
  else if FService <> nil then
    FService.ReceiveData(LMessage.ToJSON)
  else
    Memo1.Lines.Add('Not bound to service');
end;

procedure TForm1.ReceiverReceiveHandler(const AIntent: JIntent);
var
  LExtraName: JString;
begin
  LExtraName := StringToJString(cExtraMessageData);
  if AIntent.hasExtra(LExtraName) then
    ServiceSendDataHandler(Self, JStringToString(AIntent.getStringExtra(LExtraName)));
end;

procedure TForm1.SendData(const AData: string);
var
  LIntent: JIntent;
begin
  LIntent := TJIntent.JavaClass.init(StringToJString(cActionServiceMessage));
  LIntent.putExtra(StringToJString(cExtraMessageData), StringToJString(AData));
  TJLocalBroadcastManager.JavaClass.getInstance(TAndroidHelper.Context).sendBroadcast(LIntent);
end;

procedure TForm1.ServiceConnectedHandler(const ALocalService: TAndroidBaseService);
begin
  FService := TServiceModule(ALocalService);
  FService.OnSendData := ServiceSendDataHandler;
  Memo1.Lines.Add('Binding to service completed');
end;

procedure TForm1.ServiceDisconnectedHandler;
begin
  FService := nil;
end;

procedure TForm1.ServiceSendDataHandler(Sender: TObject; const AData: string);
var
  LMessage: TDataMessage;
begin
  LMessage.FromJSON(AData);
  case LMessage.DataType of
    1:
      Memo1.Lines.Add('Received: ' + LMessage.Data);
    3:
    begin
      Memo1.Lines.Add('Received image message');
      if not LMessage.Data.IsEmpty then
        DecodeImage(LMessage.Data)
      else
        Memo1.Lines.Add('..but no data');
    end;
  end;
end;

procedure TForm1.DecodeImage(const ABase64: string);
var
  LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    TBase64Helper.Decode(ABase64, LStream);
    Image1.Bitmap.LoadFromStream(LStream);
  finally
    LStream.Free;
  end;
end;

procedure TForm1.ConnectButtonClick(Sender: TObject);
begin
  if UseBindingCheckBox.IsChecked then
    FConnection.BindService(cServiceName)
  else
    FConnection.StartService(cServiceName);
end;

end.
