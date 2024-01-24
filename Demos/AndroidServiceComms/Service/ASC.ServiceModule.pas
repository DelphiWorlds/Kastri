unit ASC.ServiceModule;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Android.Service,
  AndroidApi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Os,
  ASC.Common;

type
  TSendDataEvent = procedure(Sender: TObject; const Data: string) of object;

  TServiceModule = class(TAndroidService)
    procedure AndroidServiceCreate(Sender: TObject);
    function AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags, StartId: Integer): Integer;
  private
    // FReceiver, ReceiverReceiveHandler and SendData are for when *not* using service "binding"
    FReceiver: TLocalBroadcastReceiver;
    FOnSendData: TSendDataEvent;
    function GetImageBase64: string;
    procedure ReceiverReceiveHandler(const AIntent: JIntent);
    procedure SendData(const AData: string);
  public
    // This method is made public only so that the application can call it direct when using service "binding"
    procedure ReceiveData(const AData: string);
    // OnSendData is for when using service "binding"
    property OnSendData: TSendDataEvent read FOnSendData write FOnSendData;
  end;

var
  ServiceModule: TServiceModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses
  Androidapi.Helpers, Androidapi.JNI.JavaTypes, Androidapi.JNI.App,
  System.Net.HttpClient,
  DW.Base64.Helpers,
  DW.Androidapi.JNI.AndroidX.LocalBroadcastManager;

{ TServiceModule }

procedure TServiceModule.AndroidServiceCreate(Sender: TObject);
begin
  FReceiver := TLocalBroadcastReceiver.Create([cActionServiceMessage], ReceiverReceiveHandler);
end;

procedure TServiceModule.ReceiverReceiveHandler(const AIntent: JIntent);
var
  LExtraName: JString;
begin
  LExtraName := StringToJString(cExtraMessageData);
  if AIntent.hasExtra(LExtraName) then
    ReceiveData(JStringToString(AIntent.getStringExtra(LExtraName)));
end;

function TServiceModule.AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags, StartId: Integer): Integer;
begin
  SendData(TDataMessage.CreateStringMessage('Service started').ToJSON);
  Result := TJService.JavaClass.START_STICKY;
end;

function TServiceModule.GetImageBase64: string;
var
  LHTTP: THTTPClient;
  LResponse: IHTTPResponse;
begin
  Result := '';
  LHTTP := THTTPClient.Create;
  try
    LResponse := LHTTP.Get('https://upload.wikimedia.org/wikipedia/commons/thumb/1/15/Cat_August_2010-4.jpg/2880px-Cat_August_2010-4.jpg');
    if LResponse.StatusCode = 200 then
      Result := TBase64Helper.Encode(LResponse.ContentStream);
  finally
    LHTTP.Free;
  end;
end;

procedure TServiceModule.ReceiveData(const AData: string);
var
  LMessage: TDataMessage;
begin
  LMessage.FromJSON(AData);
  case LMessage.DataType of
    2:
    begin
      if LMessage.Data.Equals('ping') then
        SendData(TDataMessage.CreateStringMessage('pong').ToJSON)
      else if LMessage.Data.Equals('image') then
        SendData(TDataMessage.CreateImageMessage(GetImageBase64).ToJSON);
    end;
  end;
end;

procedure TServiceModule.SendData(const AData: string);
var
  LIntent: JIntent;
begin
  // If FOnSendData is not assigned, then service "binding" is not being used
  if not Assigned(FOnSendData) then
  begin
    LIntent := TJIntent.JavaClass.init(StringToJString(cActionAppMessage));
    LIntent.putExtra(StringToJString(cExtraMessageData), StringToJString(AData));
    TJLocalBroadcastManager.JavaClass.getInstance(TAndroidHelper.Context).sendBroadcast(LIntent);
  end
  else
    FOnSendData(Self, AData);
end;

end.
