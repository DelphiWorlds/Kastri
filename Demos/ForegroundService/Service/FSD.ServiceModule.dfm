object ServiceModule: TServiceModule
  OnDestroy = AndroidServiceDestroy
  OnStartCommand = AndroidServiceStartCommand
  Height = 319
  Width = 432
end
