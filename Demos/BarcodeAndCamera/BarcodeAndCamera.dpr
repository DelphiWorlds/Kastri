program BarcodeAndCamera;

uses
  System.StartUpCopy,
  FMX.Forms,
  BC.Forms.Main in 'BC.Forms.Main.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
