program FCMBaseDemoD12;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainFrm in '..\MainFrm.pas' {frmMain},
  FMX.PushNotification.FCM.iOS in '..\FMX.PushNotification.FCM.iOS.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
