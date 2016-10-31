unit WiRL.Wizards.Modules.Resources;

interface

uses
  WiRL.Wizards.Utils,
  ToolsAPI;

resourcestring
  SWiRLServerResources = 'WiRLServerResources';
  SServerResourcesFileName = 'ServerResources';

type
  TWiRLServerResourcesCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  public
    // IOTACreator
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;

    // IOTAModuleCreator
    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  end;

implementation

uses
  System.SysUtils;

{$REGION 'IOTACreator'}

function TWiRLServerResourcesCreator.GetCreatorType: string;
begin
  Result := sUnit;
end;

function TWiRLServerResourcesCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TWiRLServerResourcesCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TWiRLServerResourcesCreator.GetOwner: IOTAModule;
begin
  Result := ActiveProject;
end;

function TWiRLServerResourcesCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

{$ENDREGION}
{$REGION 'IOTAModuleCreator'}

function TWiRLServerResourcesCreator.GetAncestorName: string;
begin
  Result := '';
end;

function TWiRLServerResourcesCreator.GetImplFileName: string;
begin
  Result := GetCurrentDir + '\' + SServerResourcesFileName + '.pas';
end;

function TWiRLServerResourcesCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TWiRLServerResourcesCreator.GetFormName: string;
begin
  Result := '';
end;

function TWiRLServerResourcesCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TWiRLServerResourcesCreator.GetShowForm: Boolean;
begin
  Result := False;
end;

function TWiRLServerResourcesCreator.GetShowSource: Boolean;
begin
  Result := False;
end;

function TWiRLServerResourcesCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := NIL;
end;

function TWiRLServerResourcesCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := TWiRLSourceFile.Create(SWiRLServerResources);
end;

function TWiRLServerResourcesCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := NIL;
end;

procedure TWiRLServerResourcesCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
end;

{$ENDREGION}

end.
