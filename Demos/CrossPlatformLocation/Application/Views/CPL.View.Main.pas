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
  TStateEvent = procedure(Sender: TObject; const State: Integer) of object;

  {$IF Defined(ANDROID)}
  TLocalReceiver = class(TMultiReceiver)
  private
    FOnMessageReceived: TMessageReceivedEvent;
    FOnState: TStateEvent;
    procedure DoMessageReceived(const AMsg: string);
    procedure DoState(const AState: Integer);
  protected
    procedure Receive(context: JContext; intent: JIntent); override;
    procedure ConfigureActions; override;
  public
    property OnMessageReceived: TMessageReceivedEvent read FOnMessageReceived write FOnMessageReceived;
    property OnState: TStateEvent read FOnState write FOnState;
  end;
  {$ENDIF}

  TLocationState = (Unavailable, Resumed, Paused);

  TMainView = class(TForm)
    Memo: TMemo;
    ClearButton: TButton;
    ContentLayout: TLayout;
    ButtonsLayout: TLayout;
    ChangeStateButton: TButton;
    procedure ClearButtonClick(Sender: TObject);
    procedure ChangeStateButtonClick(Sender: TObject);
  private
    FLocationState: TLocationState;
    FLocation: TLocation;
    {$IF Defined(ANDROID)}
    FReceiver: TLocalReceiver;
    {$ENDIF}
    function GetBasePermissions: TArray<string>;
    procedure LocationChangedHandler(Sender: TObject; const AData: TLocationData);
    procedure ReceiverMessageReceivedHandler(Sender: TObject; const AMsg: string);
    procedure ReceiverStateHandler(Sender: TObject; const AState: Integer);
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
  DW.ServiceCommander.Android, DW.Android.Helpers,
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
  IntentFilter.addAction(StringToJString(cServiceStateAction));
end;

procedure TLocalReceiver.DoMessageReceived(const AMsg: string);
begin
  if Assigned(FOnMessageReceived) then
    FOnMessageReceived(Self, AMsg);
end;

procedure TLocalReceiver.DoState(const AState: Integer);
begin
  if Assigned(FOnState) then
    FOnState(Self, AState);
end;

procedure TLocalReceiver.Receive(context: JContext; intent: JIntent);
begin
  if intent.getAction.equals(StringToJString(cServiceMessageAction)) then
    DoMessageReceived(JStringToString(intent.getStringExtra(StringToJString(cServiceBroadcastParamMessage))))
  else if intent.getAction.equals(StringToJString(cServiceStateAction)) then
    DoState(intent.getIntExtra(StringToJString(cServiceBroadcastParamState), cServiceStateLocationUpdatesUnavailable));
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
  FReceiver.OnState := ReceiverStateHandler;
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

procedure TMainView.ReceiverStateHandler(Sender: TObject; const AState: Integer);
begin
  TOSLog.d('TMainView.ReceiverStateHandler > %d', [AState]);
  case AState of
    cServiceStateLocationUpdatesResumed:
    begin
      FLocationState := TLocationState.Resumed;
      ChangeStateButton.Text := 'Pause';
      ChangeStateButton.Enabled := True;
    end;
    cServiceStateLocationUpdatesPaused:
    begin
      FLocationState := TLocationState.Paused;
      ChangeStateButton.Text := 'Resume';
      ChangeStateButton.Enabled := True;
    end;
    cServiceStateLocationUpdatesUnavailable:
    begin
      FLocationState := TLocationState.Unavailable;
      ChangeStateButton.Enabled := False;
    end;
  end;
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
  // Just request background permission
  else if TOSVersion.Check(10) and not PermissionsService.IsPermissionGranted(cPermissionAccessBackgroundLocation) then
    RequestLocationPermissions([cPermissionAccessBackgroundLocation])
  else
    StartLocation;
end;

procedure TMainView.StartLocation;
begin
  {$IF Defined(ANDROID)}
  if not TServiceCommander.StartService(cServiceName) then
    TServiceCommander.SendCommand(cServiceCommandCheckState);
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

procedure TMainView.ChangeStateButtonClick(Sender: TObject);
begin
  case FLocationState of
    TLocationState.Resumed:
    begin
      {$IF Defined(ANDROID)}
      TServiceCommander.SendCommand(cServiceCommandStopLocationUpdates);
      {$ENDIF}
    end;
    TLocationState.Paused:
    begin
      {$IF Defined(ANDROID)}
      TServiceCommander.SendCommand(cServiceCommandStartLocationUpdates);
      {$ENDIF}
    end;
  end;
end;

procedure TMainView.ClearButtonClick(Sender: TObject);
begin
  Memo.Lines.Clear;
end;

end.
