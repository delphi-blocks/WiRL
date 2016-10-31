library WiRLServerWizard;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  DesignIDE,
  System.SysUtils,
  System.Classes,
  WiRL.Wizards.Modules.MainForm in 'WiRL.Wizards.Modules.MainForm.pas',
  WiRL.Wizards in 'WiRL.Wizards.pas',
  WiRL.Wizards.ProjectCreator in 'WiRL.Wizards.ProjectCreator.pas',
  WiRL.Wizards.Registration in 'WiRL.Wizards.Registration.pas',
  WiRL.Wizards.Utils in 'WiRL.Wizards.Utils.pas';

{$R *.res}

begin
end.
