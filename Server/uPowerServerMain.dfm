object JDPowerServer: TJDPowerServer
  OldCreateOrder = False
  AllowPause = False
  DisplayName = 'JD Power Server Service'
  AfterInstall = ServiceAfterInstall
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 227
  Width = 355
end
