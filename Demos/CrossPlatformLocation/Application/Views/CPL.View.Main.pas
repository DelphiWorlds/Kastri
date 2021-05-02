unit CPL.View.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Sensors,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Layouts,
  FMX.Memo.Types,
  {$IF Defined(ANDROID)}
  Androidapi.JNI.GraphicsContentViewText,
  DW.MultiReceiver.Android,
  {$ENDIF}
  DW.Location;

type
  TMessageReceivedEvent = procedure(Sender: TObject; const Msg: string) of object;

  {$IF Defined(ANDROID)}
  TLocalReceiver = class(TMultiReceiver)
  private
    FOnMessageReceived: TMessageReceivedEvent;
    procedure DoMessageReceived(const AMsg: string);
  protected
    procedure Receive(context: JContext; intent: JIntent); override;
    procedure ConfigureActions; override;
  public
    property OnMessageReceived: TMessageReceivedEvent read FOnMessageReceived write FOnMessageReceived;
  end;
  {$ENDIF}

  TMainView = class(TForm)
    Memo: TMemo;
    ClearButton: TButton;
    ContentLayout: TLayout;
    procedure ClearButtonClick(Sender: TObject);
  private
    FLocation: TLocation;
    {$IF Defined(ANDROID)}
    FReceiver: TLocalReceiver;
    {$ENDIF}
    function GetBasePermissions: TArray<string>;
    procedure LocationChangedHandler(Sender: TObject; const AData: TLocationData);
    procedure ReceiverMessageReceivedHandler(Sender: TObject; const AMsg: string);
    procedure RequestLocationPermissions(const APermissions: TArray<string>);
    procedure RequestLocationPermissionsPrelude;
    procedure StartLocation;
    procedure ShowRationale(const APostRationaleProc: TProc);
  protected
    procedure DoShow; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainView: TMainView;

implementation

{$R *.fmx}

uses
  System.Permissions,
  FMX.DialogService.Async,
  {$IF Defined(CLOUDLOGGING)}
  Grijjy.CloudLogging,
  {$ENDIF}
  DW.OSLog, DW.OSDevice,
  {$IF Defined(ANDROID)}
  Androidapi.Helpers, Androidapi.JNI.JavaTypes,
  DW.ServiceCommander.Android,
  {$ENDIF}
  DW.Sensors, DW.Consts.Android, DW.UIHelper,
  CPL.Consts;

const
  cBackgroundPermissionsMessage = 'This application requires access to location updates in the background'#13#10#13#10 +
    'When prompted, please tap the "Allow in settings" option and select "Allow all the time"';

{$IF Defined(ANDROID)}
{ TLocalReceiver }

procedure TLocalReceiver.ConfigureActions;
begin
  IntentFilter.addAction(StringToJString(cServiceMessageAction));
end;

procedure TLocalReceiver.DoMessageReceived(const AMsg: string);
begin
  if Assigned(FOnMessageReceived) then
    FOnMessageReceived(Self, AMsg);
end;

procedure TLocalReceiver.Receive(context: JContext; intent: JIntent);
begin
  if intent.getAction.equals(StringToJString(cServiceMessageAction)) then
    DoMessageReceived(JStringToString(intent.getStringExtra(StringToJString(cServiceBroadcastParamMessage))));
end;
{$ENDIF}

{ TMainView }

constructor TMainView.Create(AOwner: TComponent);
begin
  inherited;
  {$IF Defined(CLOUDLOGGING)}
  GrijjyLog.SetLogLevel(TgoLogLevel.Info);
  GrijjyLog.Connect(cCloudLoggingHost, cCloudLoggingName);
  {$ENDIF}
  {$IF Defined(ANDROID)}
  FReceiver := TLocalReceiver.Create(True);
  FReceiver.OnMessageReceived := ReceiverMessageReceivedHandler;
  {$ENDIF}
  FLocation := TLocation.Create;
  FLocation.Usage := TLocationUsage.Always;
  FLocation.Activity := TLocationActivity.Navigation;
  // FLocation.UsesService := True;
  FLocation.OnLocationChanged := LocationChangedHandler;
end;

destructor TMainView.Destroy;
begin
  {$IF Defined(ANDROID)}
  FReceiver.Free;
  {$ENDIF}
  FLocation.Free;
  inherited;
end;

procedure TMainView.DoShow;
begin
  inherited;
  {$IF Defined(ANDROID)}
  RequestLocationPermissionsPrelude;
  {$ELSE}
  StartLocation;
  {$ENDIF}
end;

procedure TMainView.Resize;
begin
  inherited;
  ContentLayout.Padding.Rect := TUIHelper.GetOffsetRect;
end;

procedure TMainView.ReceiverMessageReceivedHandler(Sender: TObject; const AMsg: string);
begin
  Memo.Lines.Add('Svc: ' + AMsg);
end;

function TMainView.GetBasePermissions: TArray<string>;
begin
  Result := [cPermissionAccessCoarseLocation, cPermissionAccessFineLocation];
end;

procedure TMainView.ShowRationale(const APostRationaleProc: TProc);
begin
  if TOSVersion.Check(10) and not PermissionsService.IsPermissionGranted(cPermissionAccessBackgroundLocation) then
  begin
    TDialogServiceAsync.MessageDialog(cBackgroundPermissionsMessage, TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0,
      procedure(const AResult: TModalResult)
      begin
        APostRationaleProc;
      end
    );
  end
  else
    APostRationaleProc;
end;

procedure TMainView.RequestLocationPermissions(const APermissions: TArray<string>);
begin
  {$IF Defined(ANDROID)}
  TServiceCommander.IsRequestingPermissions := True;
  {$ENDIF}
  PermissionsService.RequestPermissions(APermissions,
    procedure(const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>)
    begin
      if PermissionsService.IsEveryPermissionGranted(GetBasePermissions) then
        StartLocation;
      {$IF Defined(ANDROID)}
      TServiceCommander.IsRequestingPermissions := False;
      {$ENDIF}
    end,
    procedure(const APermissions: TArray<string>; const APostRationaleProc: TProc)
    begin
      ShowRationale(APostRationaleProc);
    end
  );
end;

procedure TMainView.RequestLocationPermissionsPrelude;
var
  LPermissions: TArray<string>;
begin
  LPermissions := GetBasePermissions;
  if not PermissionsService.IsEveryPermissionGranted(LPermissions) then
  begin
    // Calling ShowRationale here because the user needs to be aware that the background updates require the "Always" permission
    ShowRationale(
      procedure
      begin
        if TOSVersion.Check(10) then
          LPermissions := LPermissions + [cPermissionAccessBackgroundLocation];
        RequestLocationPermissions(LPermissions);
      end
    );
  end
  else if TOSVersion.Check(10) and not PermissionsService.IsPermissionGranted(cPermissionAccessBackgroundLocation) then
    RequestLocationPermissions([cPermissionAccessBackgroundLocation])
  else
    StartLocation;
end;

procedure TMainView.StartLocation;
begin
  {$IF Defined(ANDROID)}
  TServiceCommander.StartService(cServiceName);
  {$ENDIF}
  FLocation.IsActive := True;
end;

procedure TMainView.LocationChangedHandler(Sender: TObject; const AData: TLocationData);
var
  LTimestamp: string;
begin
  LTimestamp := FormatDateTime('hh:nn:ss.zzz', Now);
  Memo.Lines.Add(Format('%s - Location: %2.6f, %2.6f', [LTimestamp, AData.Location.Latitude, AData.Location.Longitude]));
  Memo.Lines.Add(Format('%s - Speed: %.1f, Altitude: %.1f, Bearing: %.1f', [LTimestamp, AData.Speed, AData.Altitude, AData.Bearing]));
end;

procedure TMainView.ClearButtonClick(Sender: TObject);
begin
  Memo.Lines.Clear;
end;

end.
