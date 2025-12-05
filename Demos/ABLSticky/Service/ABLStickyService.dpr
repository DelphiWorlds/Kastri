program ABLStickyService;

uses
  System.Android.ServiceApplication,
  ABLS.ServiceModule in 'ABLS.ServiceModule.pas' {ServiceModule: TAndroidService};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TServiceModule, ServiceModule);
  Application.Run;
end.
