object JDPowerGlobal: TJDPowerGlobal
  OldCreateOrder = False
  DisplayName = 'JD Power Global Service'
  AfterInstall = ServiceAfterInstall
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 150
  Width = 215
end
