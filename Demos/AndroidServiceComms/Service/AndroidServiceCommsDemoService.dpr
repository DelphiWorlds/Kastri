program AndroidServiceCommsDemoService;

uses
  System.Android.ServiceApplication,
  ASC.ServiceModule in 'ASC.ServiceModule.pas' {ServiceModule: TAndroidService};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TServiceModule, ServiceModule);
  Application.Run;
end.
