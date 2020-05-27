program CrossPlatformLocationService;

uses
  System.Android.ServiceApplication,
  CPL.ServiceModule in 'CPL.ServiceModule.pas' {ServiceModule: TAndroidService};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TServiceModule, ServiceModule);
  Application.Run;
end.
