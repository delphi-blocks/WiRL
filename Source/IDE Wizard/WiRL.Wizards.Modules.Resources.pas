unit WiRL.Wizards.Modules.Resources;

interface

uses
  WiRL.Wizards.Utils,
  ToolsAPI, WiRL.Wizards.Modules.Classes;

resourcestring
  SWiRLServerResources = 'WiRLServerResources';
  //SServerResourcesFileName = 'ServerResources';
  SServerResourcesFileName = 'ResourceUnit';

type
  TWiRLServerResourcesCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  private
    FServerConfig: TServerConfig;
    FFileName: string;
    FUnitName: string;
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

    constructor Create(AServerConfig: TServerConfig);
  end;

  TWiRLResourceFile = class(TWiRLSourceFile)
  public
    function GetSource: string; override;
    constructor Create(const AUnitName: string);
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
  Result := FFileName;
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
  Result := True;
end;

function TWiRLServerResourcesCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := NIL;
end;

function TWiRLServerResourcesCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := TWiRLResourceFile.Create(FUnitName);
end;

function TWiRLServerResourcesCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

constructor TWiRLServerResourcesCreator.Create(AServerConfig: TServerConfig);
var
  LSuffix: string;
begin
  inherited Create;
  FServerConfig := AServerConfig;
  FFileName := GetNewModuleFileName(SServerResourcesFileName, '', '', False, LSuffix);
  FUnitName := ExtractFileName(ChangeFileExt(FFileName, ''));
end;

procedure TWiRLServerResourcesCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
end;

{$ENDREGION}

{ TWiRLResourceFile }

constructor TWiRLResourceFile.Create(const AUnitName: string);
begin
  inherited Create(SWiRLServerResources, AUnitName);
end;

function TWiRLResourceFile.GetSource: string;
var
  LResourcePath: string;
  LClassName: string;
begin
  LClassName := 'T' + ModuleName;
  LResourcePath := StringReplace(LowerCase(ModuleName), 'unit', '', []);
  Result := inherited GetSource;
  Result := StringReplace(Result, '%RESOURCE_PATH%', LResourcePath, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%CLASS_NAME%', LClassName, [rfReplaceAll, rfIgnoreCase]);
end;

end.
