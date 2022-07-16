program CameraDemoD11;



uses
  System.StartUpCopy,
  FMX.Forms,
  CD.View.Main in 'Views\CD.View.Main.pas' {MainView},
  CD.View.Camera in 'Views\CD.View.Camera.pas' {CameraView},
  CD.Types in 'Core\CD.Types.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainView, MainView);
  Application.Run;
end.
