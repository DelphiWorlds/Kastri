unit DW.SMS.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2025 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // RTL
  System.Messaging,
  // Android
  Androidapi.JNI.Telephony, Androidapi.JNI.GraphicsContentViewText,
  // DW
  DW.SMS, DW.Types, DW.MultiReceiver.Android;

type
  TPlatformSMS = class;

  TSMSIntentReceiver = class(TMultiReceiver)
  private
    FPlatformSMS: TPlatformSMS;
  protected
    procedure Receive(context: JContext; intent: JIntent); override;
    procedure ConfigureActions; override;
  public
    constructor Create(const APlatformSMS: TPlatformSMS);
  end;

  TPlatformSMS = class(TCustomPlatformSMS)
  private
    FDestinations: TArray<string>;
    FIntentID: Integer;
    FMessage: string;
    FSMSIntentReceiver: TSMSIntentReceiver;
    FSMSManager: JSmsManager;
    procedure DoCannotSend;
    function GetMessageIntent(const ADestination: string): JIntent;
    function GetNextIntentID: Integer;
    procedure MessageResultNotificationHandler(const Sender: TObject; const AMsg: TMessage);
    procedure SendTextMessageViaAPI(const AText: string; const ADestinations: TArray<string>);
    procedure SendTextMessageViaIntent;
  protected
    function GetAuthorizationStatus: TAuthorizationStatus; override;
    procedure IntentReceived(const AIntent: JIntent; const AResultCode: Integer);
    procedure RequestPermission; override;
    procedure SendTextMessage(const AText: string; const ADestinations: TArray<string>); override;
  public
    constructor Create(const ASMS: TSMS); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.Permissions, System.SysUtils,
  // Android
  Androidapi.Helpers, Androidapi.JNI.App, Androidapi.JNI.JavaTypes, Androidapi.JNI.Support, Androidapi.JNI.Net, Androidapi.JNI.Provider,
  // DW
  DW.OSLog, DW.Consts.Android, DW.Permissions.Helpers;

const
  cACTION_MESSAGE_SENT = 'ACTION_MESSAGE_SENT';
  cEXTRA_DESTINATION = 'EXTRA_DESTINATION';
  cSMSRequestCode = 98765;

{$IF CompilerVersion < 35}
type
  TJcontent_ContextCompat = class(TJContextCompat);
{$ENDIF}

{ TSMSIntentReceiver }

constructor TSMSIntentReceiver.Create(const APlatformSMS: TPlatformSMS);
begin
  inherited Create;
  FPlatformSMS := APlatformSMS;
end;

procedure TSMSIntentReceiver.ConfigureActions;
begin
  IntentFilter.addAction(StringToJString(cACTION_MESSAGE_SENT));
end;

procedure TSMSIntentReceiver.Receive(context: JContext; intent: JIntent);
begin
  FPlatformSMS.IntentReceived(intent, GetResultCode);
end;

{ TPlatformSMS }

constructor TPlatformSMS.Create(const ASMS: TSMS);
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TMessageResultNotification, MessageResultNotificationHandler);
  FSMSManager := TJSmsManager.JavaClass.getDefault;
  FSMSIntentReceiver := TSMSIntentReceiver.Create(Self);
end;

destructor TPlatformSMS.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TMessageResultNotification, MessageResultNotificationHandler);
  FSMSIntentReceiver.Free;
  inherited;
end;

function TPlatformSMS.GetAuthorizationStatus: TAuthorizationStatus;
var
  LPermission: Integer;
begin
  LPermission := TJcontent_ContextCompat.JavaClass.checkSelfPermission(TAndroidHelper.Context,
    StringToJString(cPermissionSendSMS));
  if LPermission = TJPackageManager.JavaClass.PERMISSION_GRANTED then
    Result := TAuthorizationStatus.Authorized
  else if LPermission = TJPackageManager.JavaClass.PERMISSION_DENIED then
    Result := TAuthorizationStatus.Denied
  else
    Result := TAuthorizationStatus.NotDetermined;
end;

function TPlatformSMS.GetMessageIntent(const ADestination: string): JIntent;
begin
  Result := TJIntent.JavaClass.init(StringToJString(cACTION_MESSAGE_SENT));
  Result.putExtra(StringToJString(cEXTRA_DESTINATION), StringToJString(ADestination));
end;

function TPlatformSMS.GetNextIntentID: Integer;
begin
  Result := FIntentID;
  Inc(FIntentID);
end;

procedure TPlatformSMS.IntentReceived(const AIntent: JIntent; const AResultCode: Integer);
var
  LDestination: string;
begin
  LDestination := JStringToString(AIntent.getStringExtra(StringToJString(cEXTRA_DESTINATION)));
  if AResultCode = TJActivity.JavaClass.RESULT_OK then
    DoMessageResult([LDestination], TMessageResult.Sent)
  else
  begin
    TOSLog.d('Send to %s failed, result code: %d', [LDestination, AResultCode]);
    DoMessageResult([LDestination], TMessageResult.Failed);
  end;
//  else if AResultCode = TJSmsManager.JavaClass.RESULT_ERROR_GENERIC_FAILURE then
//  else if AResultCode = TJSmsManager.JavaClass.RESULT_ERROR_NO_SERVICE then
//  else if AResultCode = TJSmsManager.JavaClass.RESULT_ERROR_NULL_PDU then
//  else if AResultCode = TJSmsManager.JavaClass.RESULT_ERROR_RADIO_OFF then ;
end;

procedure TPlatformSMS.RequestPermission;
begin
  PermissionsService.RequestPermissions([cPermissionSendSMS],
    procedure(const APermissions: TPermissionArray; const AGrantResults: TPermissionStatusArray)
    begin
      DoPermissionRequestResult(AGrantResults[0] = TPermissionStatus.Granted);
    end
  );
end;

procedure TPlatformSMS.SendTextMessage(const AText: string; const ADestinations: TArray<string>);
begin
  if UseIntents then
  begin
    FDestinations := ADestinations;
    FMessage := AText;
    SendTextMessageViaIntent;
  end
  else
    SendTextMessageViaAPI(AText, ADestinations);
end;

procedure TPlatformSMS.SendTextMessageViaAPI(const AText: string; const ADestinations: TArray<string>);
var
  LSentIntent: JPendingIntent;
  LDestination: string;
  LParts, LSentIntents, LDeliveryIntents: JArrayList;
  LFlags: Integer;
begin
  LFlags := TJPendingIntent.JavaClass.FLAG_CANCEL_CURRENT or TJPendingIntent.JavaClass.FLAG_IMMUTABLE;
  for LDestination in ADestinations do
  begin
    LSentIntent := TJPendingIntent.JavaClass.getBroadcast(TAndroidHelper.Context, GetNextIntentID, GetMessageIntent(LDestination),
      LFlags);
    LParts := FSMSManager.divideMessage(StringToJString(AText));
    LSentIntents := TJArrayList.JavaClass.init(1);
    LSentIntents.add(LSentIntent);
    LDeliveryIntents := nil; // TODO?
    FSMSManager.sendMultipartTextMessage(StringToJString(LDestination), nil, LParts, LSentIntents, LDeliveryIntents);
  end;
end;

procedure TPlatformSMS.SendTextMessageViaIntent;
var
  LIntent: JIntent;
  LUri: Jnet_Uri;
  LSMSPackage: JString;
begin
  if Length(FDestinations) > 0 then
  begin
    LUri := TJnet_Uri.JavaClass.parse(StringToJString('smsto:' + string.Join(';', FDestinations[0])));
    LIntent := TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_SENDTO, LUri);
    LIntent.putExtra(StringToJString('sms_body'), StringToJString(FMessage));
    LIntent.putExtra(StringToJString('exit_on_sent'), True); // May not work on most devices
    LSMSPackage := TJTelephony_Sms.JavaClass.getDefaultSmsPackage(TAndroidHelper.Context);
    if LSMSPackage <> nil then
      LIntent.setPackage(LSMSPackage);
    if LIntent.resolveActivity(TAndroidHelper.Context.getPackageManager) <> nil then
      TAndroidHelper.Activity.startActivityForResult(LIntent, cSMSRequestCode)
    else
      DoCannotSend;
  end;
end;

procedure TPlatformSMS.MessageResultNotificationHandler(const Sender: TObject; const AMsg: TMessage);
var
  LMsg: TMessageResultNotification;
  LDestination: string;
begin
  LMsg := TMessageResultNotification(AMsg);
  if LMsg.RequestCode = cSMSRequestCode then
  begin
    if Length(FDestinations) > 0 then
    begin
      LDestination := FDestinations[0];
      Delete(FDestinations, 0, 1);
      if LMsg.ResultCode = TJActivity.JavaClass.RESULT_OK then
        DoMessageResult([LDestination], TMessageResult.Sent)
      else if LMsg.ResultCode = TJActivity.JavaClass.RESULT_CANCELED then
        DoMessageResult([LDestination], TMessageResult.Cancelled)
      else
        DoMessageResult([LDestination], TMessageResult.Failed);
      SendTextMessageViaIntent;
    end;
  end;
end;

procedure TPlatformSMS.DoCannotSend;
begin
  DoMessageResult(FDestinations, TMessageResult.CannotSend);
  FDestinations := [];
  FMessage := '';
end;

end.
