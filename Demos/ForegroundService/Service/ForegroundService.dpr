program ForegroundService;

uses
  System.Android.ServiceApplication,
  FSD.ServiceModule in 'FSD.ServiceModule.pas' {ServiceModule: TAndroidService};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TServiceModule, ServiceModule);
  Application.Run;
end.
