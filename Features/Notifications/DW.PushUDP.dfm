object PushUDP: TPushUDP
  OldCreateOrder = False
  Height = 391
  Width = 589
  object UDPClient: TIdUDPClient
    Active = True
    Port = 64220
    Left = 172
    Top = 84
  end
  object UDPTimer: TTimer
    OnTimer = UDPTimerTimer
    Left = 172
    Top = 148
  end
end
