unit CD.View.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Sensors, System.Sensors.Components,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Layouts,
  CD.View.Camera;

type
  TMainView = class(TForm)
    TabControl: TTabControl;
    StartTab: TTabItem;
    StartButton: TButton;
    CameraTab: TTabItem;
    LocationSensor: TLocationSensor;
    StartImage: TImage;
    IncludeLocationSwitch: TSwitch;
    SwitchLayout: TLayout;
    IncludeLocationLabel: TLabel;
    StartLayout: TLayout;
    procedure StartButtonClick(Sender: TObject);
    procedure IncludeLocationSwitchSwitch(Sender: TObject);
    procedure LocationSensorLocationChanged(Sender: TObject; const OldLocation, NewLocation: TLocationCoord2D);
  private
    FCameraView: TCameraView;
    procedure HideCamera;
    procedure CameraViewAcceptImageClickHandler(Sender: TObject);
    procedure CameraViewCancelImageClickHandler(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  MainView: TMainView;

implementation

{$R *.fmx}

uses
  System.Permissions,
  DW.Consts.Android, DW.Permissions.Helpers;

{ TMainView }

constructor TMainView.Create(AOwner: TComponent);
begin
  inherited;
  TabControl.ActiveTab := StartTab;
  FCameraView := TCameraView.Create(CameraTab);
  FCameraView.AcceptImage.OnClick := CameraViewAcceptImageClickHandler;
  FCameraView.CancelImage.OnClick := CameraViewCancelImageClickHandler;
end;

procedure TMainView.HideCamera;
begin
  FCameraView.ShowPreview(False);
  TabControl.ActiveTab := StartTab;
end;

procedure TMainView.IncludeLocationSwitchSwitch(Sender: TObject);
begin
  if IncludeLocationSwitch.IsChecked then
  begin
    if not LocationSensor.Active then
    begin
      FCameraView.CaptureImage.Enabled := False;
      FCameraView.Camera.Location := TLocationCoord2D.Create(91, 181);
      PermissionsService.RequestPermissions([cPermissionAccessCoarseLocation, cPermissionAccessFineLocation],
        procedure(const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>)
        begin
          if AGrantResults.AreAllGranted then
            LocationSensor.Active := True
          else
            FCameraView.CaptureImage.Enabled := True;
        end
      );
    end
  end
  else
  begin
    FCameraView.Camera.IncludeLocation := False;
    LocationSensor.Active := False;
  end;
end;

procedure TMainView.LocationSensorLocationChanged(Sender: TObject; const OldLocation, NewLocation: TLocationCoord2D);
begin
  if (NewLocation.Latitude <> 0) or (FCameraView.Camera.Location.Latitude <> 91) then
  begin
    FCameraView.Camera.IncludeLocation := True;
    FCameraView.Camera.Location := NewLocation;
    FCameraView.CaptureImage.Enabled := True;
  end;
end;

procedure TMainView.StartButtonClick(Sender: TObject);
begin
  StartImage.Bitmap.Assign(nil);
  FCameraView.ShowPreview(True);
  TabControl.ActiveTab := CameraTab;
end;

procedure TMainView.CameraViewAcceptImageClickHandler(Sender: TObject);
begin
  HideCamera;
  StartImage.Bitmap.LoadFromStream(FCameraView.ImageStream);
end;

procedure TMainView.CameraViewCancelImageClickHandler(Sender: TObject);
begin
  HideCamera;
end;

end.
