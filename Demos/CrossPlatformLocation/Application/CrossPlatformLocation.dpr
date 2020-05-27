program CrossPlatformLocation;

uses
  System.StartUpCopy,
  FMX.Forms,
  CPL.View.Main in 'Views\CPL.View.Main.pas' {MainView};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainView, MainView);
  Application.Run;
end.
