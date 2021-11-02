unit DW.SMS.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2021 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
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
    FIntentID: Integer;
    FSMSIntentReceiver: TSMSIntentReceiver;
    FSMSManager: JSmsManager;
    function GetMessageIntent(const ADestination: string): JIntent;
    function GetNextIntentID: Integer;
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
  System.Permissions,
  // Android
  Androidapi.Helpers, Androidapi.JNI.App, Androidapi.JNI.JavaTypes, Androidapi.JNI.Support,
  // DW
  DW.Consts.Android, DW.Permissions.Helpers;

const
  cACTION_MESSAGE_SENT = 'ACTION_MESSAGE_SENT';
  cEXTRA_DESTINATION = 'EXTRA_DESTINATION';

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
  FSMSManager := TJSmsManager.JavaClass.getDefault;
  FSMSIntentReceiver := TSMSIntentReceiver.Create(Self);
end;

destructor TPlatformSMS.Destroy;
begin
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
    DoMessageResult([LDestination], TMessageResult.Failed);
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
var
  LSentIntent, LDeliveryIntent: JPendingIntent;
  LDestination: string;
begin
  for LDestination in ADestinations do
  begin
    LSentIntent := TJPendingIntent.JavaClass.getBroadcast(TAndroidHelper.Context, GetNextIntentID, GetMessageIntent(LDestination),
      TJPendingIntent.JavaClass.FLAG_CANCEL_CURRENT);
    LDeliveryIntent := nil; // TODO
    FSMSManager.sendTextMessage(StringToJString(LDestination), nil, StringToJString(AText), LSentIntent, LDeliveryIntent);
  end;
end;

end.
