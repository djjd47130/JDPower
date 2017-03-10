object JDRemoteShutdownSvr: TJDRemoteShutdownSvr
  OldCreateOrder = False
  AllowPause = False
  DisplayName = 'JD Remote Shutdown Server Service'
  AfterInstall = ServiceAfterInstall
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 227
  Width = 355
end
