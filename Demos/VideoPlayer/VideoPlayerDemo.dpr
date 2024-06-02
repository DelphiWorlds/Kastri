program VideoPlayerDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  VPD.View.Main in 'Views\VPD.View.Main.pas' {MainView},
  VPD.View.Player in 'Views\VPD.View.Player.pas' {PlayerView},
  VPD.View.Files in 'Views\VPD.View.Files.pas' {FilesView},
  VPD.View.Streams in 'Views\VPD.View.Streams.pas' {StreamsView};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainView, MainView);
  Application.Run;
end.
