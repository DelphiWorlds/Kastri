program FCMRelayService;

uses
  System.Android.ServiceApplication,
  FRD.ServiceModule in 'FRD.ServiceModule.pas' {ServiceModule: TAndroidIntentService};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TServiceModule, ServiceModule);
  Application.Run;
end.
