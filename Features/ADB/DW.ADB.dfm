object ADBModule: TADBModule
  OldCreateOrder = False
  Height = 452
  Width = 687
  object LogFDMemTable: TFDMemTable
    FieldDefs = <
      item
        Name = 'ID'
        DataType = ftAutoInc
      end
      item
        Name = 'DeviceID'
        DataType = ftString
        Size = 64
      end
      item
        Name = 'LogDateTime'
        DataType = ftDateTime
      end
      item
        Name = 'Level'
        DataType = ftString
        Size = 1
      end
      item
        Name = 'ProcessID'
        DataType = ftInteger
      end
      item
        Name = 'ThreadID'
        DataType = ftInteger
      end
      item
        Name = 'Application'
        DataType = ftString
        Size = 512
      end
      item
        Name = 'Tag'
        DataType = ftString
        Size = 64
      end
      item
        Name = 'Text'
        DataType = ftString
        Size = 512
      end>
    IndexDefs = <>
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    StoreDefs = True
    Left = 108
    Top = 88
    object LogFDMemTableID: TFDAutoIncField
      FieldName = 'ID'
      ProviderFlags = [pfInUpdate, pfInWhere]
      AutoIncrementSeed = 0
      AutoIncrementStep = 1
      IdentityInsert = True
    end
    object LogFDMemTableDeviceID: TStringField
      FieldName = 'DeviceID'
      Size = 64
    end
    object LogFDMemTableLogDateTime: TDateTimeField
      FieldName = 'LogDateTime'
    end
    object LogFDMemTableLevel: TStringField
      FieldName = 'Level'
      Size = 1
    end
    object LogFDMemTableProcessID: TIntegerField
      FieldName = 'ProcessID'
    end
    object LogFDMemTableThreadID: TIntegerField
      FieldName = 'ThreadID'
    end
    object LogFDMemTableApplication: TStringField
      FieldName = 'Application'
      Size = 512
    end
    object LogFDMemTableTag: TStringField
      FieldName = 'Tag'
      Size = 64
    end
    object LogFDMemTableText: TStringField
      FieldName = 'Text'
      Size = 512
    end
  end
  object PeriodicTimer: TTimer
    Interval = 2000
    OnTimer = PeriodicTimerTimer
    Left = 216
    Top = 88
  end
  object DeviceCheckTimer: TTimer
    Enabled = False
    Interval = 500
    OnTimer = DeviceCheckTimerTimer
    Left = 306
    Top = 89
  end
end
