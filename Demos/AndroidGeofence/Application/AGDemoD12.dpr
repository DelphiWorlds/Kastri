program AGDemoD12;

uses
  System.StartUpCopy,
  FMX.Forms,
  AG.View.Main in 'Views\AG.View.Main.pas' {MainView};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainView, MainView);
  Application.Run;
end.
