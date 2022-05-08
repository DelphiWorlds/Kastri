program ForegroundServiceDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  FSD.MainFrm in 'FSD.MainFrm.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
