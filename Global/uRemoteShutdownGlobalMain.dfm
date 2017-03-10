object JDRemoteShutdownGlo: TJDRemoteShutdownGlo
  OldCreateOrder = False
  DisplayName = 'JD Remote Shutdown Global Service'
  AfterInstall = ServiceAfterInstall
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 150
  Width = 215
end
