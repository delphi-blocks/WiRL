unit WiRL.Wizards.ProjectCreator;

interface

uses
  ToolsAPI, WiRL.Wizards.Modules.Classes;

resourcestring
  SWiRLServerProject = 'WiRLServerProject';

type
  TWiRLServerProjectCreator = class(TInterfacedObject, IOTACreator, IOTAProjectCreator50, IOTAProjectCreator80,IOTAProjectCreator160, IOTAProjectCreator{$IF CompilerVersion >= 32.0}, IOTAProjectCreator190 {$ENDIF})
  private
    FServerConfig: TServerConfig;
  public
    // IOTACreator
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;

    // IOTAProjectCreator
    function GetFileName: string;
    function GetOptionFileName: string; //deprecated;
    function GetShowSource: Boolean;
    procedure NewDefaultModule; //deprecated;
    function NewOptionSource(const ProjectName: string): IOTAFile; //deprecated;
    procedure NewProjectResource(const Project: IOTAProject);
    function NewProjectSource(const ProjectName: string): IOTAFile;

    // IOTAProjectCreator50
    procedure NewDefaultProjectModule(const Project: IOTAProject);

    // IOTAProjectCreator80
    function GetProjectPersonality: string;

    // IOTAProjectCreator160
    function GetFrameworkType: string;
    function GetPlatforms: TArray<string>;
    function GetPreferredPlatform: string;
    procedure SetInitialOptions(const NewProject: IOTAProject);

    // IOTAProjectCreator190
    function GetSupportedPlatforms: TArray<string>;

    constructor Create(AServerConfig: TServerConfig);
  end;

implementation

uses
  System.SysUtils,
  System.Types,
  System.Classes,
  PlatformAPI,
  WiRL.Wizards.Utils,
  WiRL.Wizards.Modules.MainForm,
  WiRL.Wizards.Modules.Resources;

{$REGION 'IOTACreator'}

constructor TWiRLServerProjectCreator.Create(AServerConfig: TServerConfig);
begin
  inherited Create;
  FServerConfig := AServerConfig;
end;

function TWiRLServerProjectCreator.GetCreatorType: string;
begin
  Result := sApplication;
end;

function TWiRLServerProjectCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TWiRLServerProjectCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TWiRLServerProjectCreator.GetOwner: IOTAModule;
begin
  Result := ActiveProjectGroup;
end;

function TWiRLServerProjectCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

{$ENDREGION}
{$REGION 'IOTAProjectCreator'}

function TWiRLServerProjectCreator.GetFileName: string;
var
  LSuffix: string;
begin
  Result := GetNewModuleFileName('Project', '',
      '', False, LSuffix, '.bdsproj;.dproj;.dpr;.dpk;.cbproj')
end;

function TWiRLServerProjectCreator.GetOptionFileName: string; deprecated;
begin
  Result := '';
end;

function TWiRLServerProjectCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TWiRLServerProjectCreator.GetSupportedPlatforms: TArray<string>;
begin
  Result := [GetPreferredPlatform];
end;

function TWiRLServerProjectCreator.NewProjectSource(const ProjectName: string): IOTAFile;
begin
  Result := TWiRLSourceFile.Create(SWiRLServerProject);
end;

function TWiRLServerProjectCreator.NewOptionSource(const ProjectName: string): IOTAFile; deprecated;
begin
  Result := nil;
end;

procedure TWiRLServerProjectCreator.NewDefaultModule; deprecated;
begin
end;

procedure TWiRLServerProjectCreator.NewProjectResource(const Project: IOTAProject);
begin
end;

{$ENDREGION}
{$REGION 'IOTAProjectCreator50'}

procedure TWiRLServerProjectCreator.NewDefaultProjectModule(const Project: IOTAProject);
var
  LModuleServices: IOTAModuleServices;
begin
  LModuleServices := BorlandIDEServices as IOTAModuleServices;
  LModuleServices.CreateModule(TWiRLServerMainFormCreator.Create(FServerConfig));
  LModuleServices.CreateModule(TWiRLServerResourcesCreator.Create(FServerConfig));
end;

{$ENDREGION}
{$REGION 'IOTAProjectCreator80'}

function TWiRLServerProjectCreator.GetProjectPersonality: string;
begin
  Result := sDelphiPersonality;
end;

{$ENDREGION}
{$REGION 'IOTAProjectCreator160'}

function TWiRLServerProjectCreator.GetFrameworkType: string;
begin
  Result := sFrameworkTypeVCL;
end;

function TWiRLServerProjectCreator.GetPlatforms: TArray<string>;
begin
  SetLength(Result, 2);
  Result[0] := cWin32Platform;
  Result[1] := cWin64Platform;
end;

function TWiRLServerProjectCreator.GetPreferredPlatform: string;
begin
  Result := cWin32Platform;
end;

procedure TWiRLServerProjectCreator.SetInitialOptions(const NewProject: IOTAProject);
begin
end;

{$ENDREGION}

end.
