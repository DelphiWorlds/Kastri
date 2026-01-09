unit DW.ServiceClient.Android;

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
  Androidapi.JNI.GraphicsContentViewText;

type
  IServiceClient = interface(IInterface)
    ['{6CB84FA4-A95B-4F6A-99A6-21A18EAEB591}']
    function IsServiceRunning: Boolean;
    procedure Start(const AIntent: JIntent = nil);
    procedure Stop;
  end;

type
  TServiceClient = class(TInterfacedObject, IServiceClient)
  private
    FServiceName: string;
  public
    { IServiceClient }
    function IsServiceRunning: Boolean;
    procedure Start(const AIntent: JIntent = nil);
    procedure Stop;
  public
    constructor Create(const AServiceName: string);
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.JNI.JavaTypes, Androidapi.Helpers, Androidapi.JNIBridge, Androidapi.JNI.App,
  // DW
  DW.Androidapi.JNI.Os, DW.Consts.Android;

{ TServiceClient }

constructor TServiceClient.Create(const AServiceName: string);
begin
  inherited Create;
  FServiceName := AServiceName;
  if not FServiceName.Contains('.') and not FServiceName.StartsWith(cEMBTJavaServicePrefix) then
    FServiceName := cEMBTJavaServicePrefix + FServiceName;
end;

procedure TServiceClient.Start(const AIntent: JIntent = nil);
var
  LIntent: JIntent;
begin
  LIntent := AIntent;
  if LIntent = nil then
    LIntent := TJIntent.JavaClass.init;
  LIntent.setClassName(TAndroidHelper.Context.getPackageName, StringToJString(FServiceName));
  TAndroidHelper.Activity.startService(LIntent);
end;

procedure TServiceClient.Stop;
var
  LIntent: JIntent;
begin
  LIntent := TJIntent.JavaClass.init;
  LIntent.setClassName(TAndroidHelper.Context.getPackageName, StringToJString(FServiceName));
  TAndroidHelper.Activity.stopService(LIntent);
end;

function TServiceClient.IsServiceRunning: Boolean;
var
  LActivityManager: JActivityManager;
  LRunningServices: JList;
  LServiceInfo: JActivityManager_RunningServiceInfo;
  LServiceName: JString;
  I: Integer;
begin
  Result := False;
  LServiceName := StringToJString(FServiceName);
  LActivityManager := TJActivityManager.Wrap(TAndroidHelper.Context.getSystemService(TJContext.JavaClass.ACTIVITY_SERVICE));
  LRunningServices := LActivityManager.getRunningServices(MaxInt);
  for I := 0 to LRunningServices.size - 1 do
  begin
    LServiceInfo := TJActivityManager_RunningServiceInfo.Wrap(TAndroidHelper.JObjectToID(LRunningServices.get(I)));
    if LServiceName.equals(LServiceInfo.service.getClassName) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

end.
