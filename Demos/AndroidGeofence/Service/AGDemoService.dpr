program AGDemoService;

uses
  System.Android.ServiceApplication,
  AG.ServiceModule in 'AG.ServiceModule.pas' {ServiceModule: TAndroidIntentService};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TServiceModule, ServiceModule);
  Application.Run;
end.
