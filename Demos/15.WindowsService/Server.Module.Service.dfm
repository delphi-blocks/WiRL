object WiRLService: TWiRLService
  OldCreateOrder = False
  DisplayName = 'WiRL Service (Demo)'
  AfterInstall = ServiceAfterInstall
  AfterUninstall = ServiceAfterUninstall
  OnExecute = ServiceExecute
  Height = 188
  Width = 274
end
