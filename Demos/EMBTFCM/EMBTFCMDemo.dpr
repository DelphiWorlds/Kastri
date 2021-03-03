program EMBTFCMDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  EF.View.Main in 'Views\EF.View.Main.pas' {MainView};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainView, MainView);
  Application.Run;
end.
