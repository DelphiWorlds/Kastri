unit DW.ServiceManager.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2026 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // Android
  Androidapi.JNI.App, Androidapi.JNI.JavaTypes;

type
  IServiceManager = interface
    ['{49C96D2A-A5CB-44A1-A1F6-D7B701F03C0F}']
    procedure StartForeground(const ANotificationCaption, ANotificationText, AChannelId, AChannelName: string);
    procedure StopForeground;
    procedure UpdateNotification(const ANotificationCaption, ANotificationText, AChannelId: string);
  end;

  TServiceManager = class(TInterfacedObject, IServiceManager)
  private
    FForegroundId: Integer;
    FNotificationManager: JNotificationManager;
    FService: JService;
    FServiceType: Integer;
    function CreateNotification(const ANotificationCaption, ANotificationText: string; const AChannelId: JString): JNotification;
    procedure CreateNotificationChannel(const AChannelId: JString; const AChannelName: JCharSequence);
    function GetDefaultIconId: Integer;
  public
    { IServiceManager }
    procedure StartForeground(const ANotificationCaption, ANotificationText, AChannelId, AChannelName: string);
    procedure StopForeground;
    procedure UpdateNotification(const ANotificationCaption, ANotificationText, AChannelId: string);
  public
    constructor Create(const AService: JService; const AServiceType: Integer = 0; const AForegroundId: Integer = 0);
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.JNI.GraphicsContentViewText, Androidapi.Helpers,
  // DW
  DW.Consts.Android;

const
  cServiceForegroundId = 3988; // Just a random number

{ TServiceManager }

constructor TServiceManager.Create(const AService: JService; const AServiceType: Integer = 0; const AForegroundId: Integer = 0);
begin
  inherited Create;
  FNotificationManager := TJNotificationManager.Wrap(TAndroidHelper.Context.getSystemService(TJContext.JavaClass.NOTIFICATION_SERVICE));
  FService := AService;
  if AForegroundId > 0 then
    FForegroundId := AForegroundId
  else
    FForegroundId := cServiceForegroundId;
  if AServiceType > 0 then
    FServiceType := AServiceType
  else
    FServiceType := TJServiceInfo.JavaClass.FOREGROUND_SERVICE_TYPE_LOCATION;
end;

procedure TServiceManager.CreateNotificationChannel(const AChannelId: JString; const AChannelName: JCharSequence);
var
  LChannel: JNotificationChannel;
begin
  if FNotificationManager.getNotificationChannel(AChannelId) = nil then
  begin
    // If a notification channel with the relevant id does not exist, one is created
    LChannel := TJNotificationChannel.JavaClass.init(AChannelId, AChannelName, TJNotificationManager.JavaClass.IMPORTANCE_HIGH);
    LChannel.setLockscreenVisibility(TJNotification.JavaClass.VISIBILITY_PRIVATE);
    FNotificationManager.createNotificationChannel(LChannel);
  end;
end;

function TServiceManager.GetDefaultIconId: Integer;
begin
  Result := TAndroidHelper.GetResourceID('drawable/ic_notification');
  if Result = 0 then
    Result := TAndroidHelper.Context.getApplicationInfo.icon;
end;

function TServiceManager.CreateNotification(const ANotificationCaption, ANotificationText: string; const AChannelId: JString): JNotification;
var
  LBuilder: JNotification_Builder;
  LFlags: Integer;
  LIntent: JIntent;
begin
  LIntent := TJIntent.Create;
  LIntent.setClassName(TAndroidHelper.Context.getPackageName, StringToJString(cEMBTFMXNativeActivity));
  LFlags := TJIntent.JavaClass.FLAG_ACTIVITY_NEW_TASK or TJPendingIntent.JavaClass.FLAG_IMMUTABLE;
  LBuilder := TJNotification_Builder.JavaClass.init(TAndroidHelper.Context, AChannelId);
  LBuilder.setContentTitle(StrToJCharSequence(ANotificationCaption));
  LBuilder.setContentText(StrToJCharSequence(ANotificationText));
  LBuilder.setSmallIcon(GetDefaultIconID);
  LBuilder.setPriority(TJNotification.JavaClass.PRIORITY_HIGH);
  LBuilder.setContentIntent(TJPendingIntent.JavaClass.getActivity(TAndroidHelper.Context, 0, LIntent, LFlags));
  LBuilder.setOngoing(True);
  if TOSVersion.Check(12) then
    LBuilder.setForegroundServiceBehavior(TJNotification.JavaClass.FOREGROUND_SERVICE_IMMEDIATE);
  Result := LBuilder.build;
end;

procedure TServiceManager.StartForeground(const ANotificationCaption, ANotificationText, AChannelId, AChannelName: string);
var
  LNotification: JNotification;
  LChannelId: JString;
begin
  if TOSVersion.Check(8) then
  begin
    LChannelId := StringToJString(AChannelId);
    CreateNotificationChannel(LChannelId, StrToJCharSequence(AChannelName));
    LNotification := CreateNotification(ANotificationCaption, ANotificationText, LChannelId);
    if TOSVersion.Check(10) then
      FService.startForeground(FForegroundId, LNotification, FServiceType)
    else
      FService.startForeground(FForegroundId, LNotification);
  end;
end;

procedure TServiceManager.StopForeground;
begin
  if TOSVersion.Check(8) then
    FService.stopForeground(True);
end;

procedure TServiceManager.UpdateNotification(const ANotificationCaption, ANotificationText, AChannelId: string);
var
  LNotification: JNotification;
begin
  LNotification := CreateNotification(ANotificationCaption, ANotificationText, StringToJString(AChannelId));
  FNotificationManager.notify(FForegroundId, LNotification);
end;

end.
