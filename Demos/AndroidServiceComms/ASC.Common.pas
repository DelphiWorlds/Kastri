unit ASC.Common;

interface

uses
  AndroidApi.JNI.GraphicsContentViewText,
  DW.MultiReceiver.Android;

const
  cActionAppMessage = 'ACTION_APP_MESSAGE';
  cActionServiceMessage = 'ACTION_SERVICE_MESSAGE';
  cExtraMessageData = 'EXTRA_MESSAGE_DATA';

type
  TReceiveProc = reference to procedure(const Intent: JIntent);

  TLocalBroadcastReceiver = class(TMultiReceiver)
  private
    FActions: TArray<string>;
    FReceiveProc: TReceiveProc;
  protected
    procedure ConfigureActions; override;
    procedure Receive(context: JContext; intent: JIntent); override;
  public
    constructor Create(const AActions: TArray<string>; const AReceiveProc: TReceiveProc);
  end;

  TDataMessage = record
    DataType: Integer;
    Data: string;
    class function CreateCommandMessage(const ACommand: string): TDataMessage; static;
    class function CreateImageMessage(const ABase64: string): TDataMessage; static;
    class function CreateStringMessage(const AData: string): TDataMessage; static;
    procedure FromJSON(const AValue: string);
    function ToJSON: string;
  end;

implementation

uses
  System.JSON,
  Androidapi.Helpers, Androidapi.JNI.JavaTypes;

{ TLocalBroadcastReceiver }

constructor TLocalBroadcastReceiver.Create(const AActions: TArray<string>; const AReceiveProc: TReceiveProc);
begin
  FActions := AActions;
  inherited Create(True);
  FReceiveProc := AReceiveProc;
end;

procedure TLocalBroadcastReceiver.ConfigureActions;
var
  LAction: string;
begin
  for LAction in FActions do
    IntentFilter.addAction(StringToJString(LAction));
end;

procedure TLocalBroadcastReceiver.Receive(context: JContext; intent: JIntent);
begin
  if Assigned(FReceiveProc) then
    FReceiveProc(intent);
end;

{ TDataMessage }

class function TDataMessage.CreateCommandMessage(const ACommand: string): TDataMessage;
begin
  Result.DataType := 2;
  Result.Data := ACommand;
end;

class function TDataMessage.CreateImageMessage(const ABase64: string): TDataMessage;
begin
  Result.DataType := 3;
  Result.Data := ABase64;
end;

class function TDataMessage.CreateStringMessage(const AData: string): TDataMessage;
begin
  Result.DataType := 1;
  Result.Data := AData;
end;

procedure TDataMessage.FromJSON(const AValue: string);
var
  LJSON: TJSONValue;
begin
  LJSON := TJSONObject.ParseJSONValue(AValue);
  if LJSON <> nil then
  try
    LJSON.TryGetValue('DataType', DataType);
    LJSON.TryGetValue('Data', Data);
  finally
    LJSON.Free;
  end;
end;

function TDataMessage.ToJSON: string;
var
  LJSON: TJSONObject;
begin
  LJSON := TJSONObject.Create;
  try
    LJSON.AddPair('DataType', DataType);
    LJSON.AddPair('Data', Data);
    Result := LJSON.ToJSON;
  finally
    LJSON.Free;
  end;
end;

end.
