object LocationsDataModule: TLocationsDataModule
  OldCreateOrder = False
  Height = 174
  Width = 362
  object FDConnection: TFDConnection
    Params.Strings = (
      
        'Database=Z:\Source\DelphiWorlds\KastriFree\Demos\CrossPlatformLo' +
        'cation\CPL.sqlite'
      'DriverID=SQLite')
    ConnectedStoredUsage = []
    LoginPrompt = False
    Left = 80
    Top = 40
  end
  object LocationsTable: TFDTable
    IndexFieldNames = 'ID'
    Connection = FDConnection
    UpdateOptions.UpdateTableName = 'Locations'
    TableName = 'Locations'
    Left = 204
    Top = 40
    object LocationsTableID: TFDAutoIncField
      FieldName = 'ID'
      Origin = 'ID'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = True
    end
    object LocationsTableLatitude: TFloatField
      FieldName = 'Latitude'
      Origin = 'Latitude'
      Required = True
    end
    object LocationsTableLongitude: TFloatField
      FieldName = 'Longitude'
      Origin = 'Longitude'
      Required = True
    end
    object LocationsTableDeviceState: TIntegerField
      FieldName = 'DeviceState'
      Origin = 'DeviceState'
      Required = True
    end
  end
end
