unit WiRL.Wizards;

interface

uses
  System.SysUtils,
  ToolsAPI,
  PlatformAPI,
  WinApi.Windows,
  WiRL.Wizards.Dialogs.ServerProject,
  WiRL.Wizards.Modules.Classes;

resourcestring
  SProjectName = 'WiRL Server Application Wizard';
  SProjectComment = 'Creates a new WiRL Server Application';
  SResourceName = 'WiRL Resource Wizard';
  SResourceComment = 'Creates a new WiRL Resource';
  SAuthor = 'WiRL Development Team';
  SGalleryCategory = 'WiRL Library';
  SIDString = 'WiRL.Wizards';

type
  TWiRLServeProjectWizard = class(TNotifierObject, IOTAWizard, IOTARepositoryWizard, IOTARepositoryWizard60,
    IOTARepositoryWizard80, IOTAProjectWizard, IOTAProjectWizard100)
  public
    constructor Create;

    // IOTAWizard
    procedure Execute;
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;

    // IOTARepositoryWizard
    function GetAuthor: string;
    function GetComment: string;
    function GetGlyph: {$IFDEF WIN32}Cardinal{$ELSE}UInt64{$ENDIF};
    function GetPage: string;

    // IOTARepositoryWizard60
    function GetDesigner: string;

    // IOTARepositoryWizard80
    function GetGalleryCategory: IOTAGalleryCategory;
    function GetPersonality: string;

    // IOTAProjectWizard100
    function IsVisible(Project: IOTAProject): Boolean;
  end;

  TWiRLResourceModuleWizard = class(TNotifierObject, IOTAWizard, IOTARepositoryWizard, IOTARepositoryWizard60,
    IOTARepositoryWizard80, IOTAProjectWizard, IOTAProjectWizard100)
  public
    constructor Create;

    // IOTAWizard
    procedure Execute;
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;

    // IOTARepositoryWizard
    function GetAuthor: string;
    function GetComment: string;
    function GetGlyph: {$IFDEF WIN32}Cardinal{$ELSE}UInt64{$ENDIF};
    function GetPage: string;

    // IOTARepositoryWizard60
    function GetDesigner: string;

    // IOTARepositoryWizard80
    function GetGalleryCategory: IOTAGalleryCategory;
    function GetPersonality: string;

    // IOTAProjectWizard100
    function IsVisible(Project: IOTAProject): Boolean;
  end;

implementation

uses
  WiRL.Wizards.Modules.Resources,
  WiRL.Wizards.ProjectCreator, WiRL.Wizards.Dialogs.NewResource;

{ TWiRLServeProjectWizard }

constructor TWiRLServeProjectWizard.Create;
var
  LCategoryServices: IOTAGalleryCategoryManager;
begin
  inherited Create;
  LCategoryServices := BorlandIDEServices as IOTAGalleryCategoryManager;
  LCategoryServices.AddCategory(LCategoryServices.FindCategory(sCategoryRoot), SIDString, SGalleryCategory);
end;

{$REGION 'IOTAWizard'}

procedure TWiRLServeProjectWizard.Execute;
var
  LServerConfig: TServerConfig;
  LModuleServices: IOTAModuleServices;
begin
  if TformServerProjectDialog.FindConfig(LServerConfig) then
  begin
    LModuleServices := BorlandIDEServices as IOTAModuleServices;
    LModuleServices.CreateModule(TWiRLServerProjectCreator.Create(LServerConfig));
//    LModuleServices.CreateModule(TWiRLServerMainFormCreator.Create(LServerConfig));
//    LModuleServices.CreateModule(TWiRLServerResourcesCreator.Create(LServerConfig));
  end;
end;

function TWiRLServeProjectWizard.GetIDString: string;
begin
  Result := SIDString + '.Server';
end;

function TWiRLServeProjectWizard.GetName: string;
begin
  Result := SProjectName;
end;

function TWiRLServeProjectWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure TWiRLServeProjectWizard.AfterSave;
begin
end;

procedure TWiRLServeProjectWizard.BeforeSave;
begin
end;

procedure TWiRLServeProjectWizard.Destroyed;
begin
end;

procedure TWiRLServeProjectWizard.Modified;
begin
end;

{$ENDREGION}
{$REGION 'IOTARepositoryWizard'}

function TWiRLServeProjectWizard.GetAuthor: string;
begin
  Result := SAuthor;
end;

function TWiRLServeProjectWizard.GetComment: string;
begin
  Result := SProjectComment;
end;

function TWiRLServeProjectWizard.GetGlyph: {$IFDEF WIN32}Cardinal{$ELSE}UInt64{$ENDIF};
begin
{ TODO : function TWiRLServeProjectWizard.GetGlyph: Cardinal; }
  Result := LoadIcon(HInstance, 'WiRLServerWizardIcon');
end;

function TWiRLServeProjectWizard.GetPage: string;
begin
  Result := SGalleryCategory;
end;

{$ENDREGION}
{$REGION 'IOTARepositoryWizard60'}

function TWiRLServeProjectWizard.GetDesigner: string;
begin
  Result := dAny;
end;

{$ENDREGION}
{$REGION 'IOTARepositoryWizard80'}

function TWiRLServeProjectWizard.GetGalleryCategory: IOTAGalleryCategory;
begin
  Result := (BorlandIDEServices as IOTAGalleryCategoryManager).FindCategory(SIDString);
end;

function TWiRLServeProjectWizard.GetPersonality: string;
begin
  Result := sDelphiPersonality;
end;

{$ENDREGION}
{$REGION 'IOTAProjectWizard100'}

function TWiRLServeProjectWizard.IsVisible(Project: IOTAProject): Boolean;
begin
  Result := True;
end;

{$ENDREGION}


{ TWiRLResourceModuleWizard }

procedure TWiRLResourceModuleWizard.AfterSave;
begin

end;

procedure TWiRLResourceModuleWizard.BeforeSave;
begin

end;

constructor TWiRLResourceModuleWizard.Create;
var
  LCategoryServices: IOTAGalleryCategoryManager;
begin
  inherited Create;
  LCategoryServices := BorlandIDEServices as IOTAGalleryCategoryManager;
  LCategoryServices.AddCategory(LCategoryServices.FindCategory(sCategoryRoot), SIDString, SGalleryCategory);
end;

procedure TWiRLResourceModuleWizard.Destroyed;
begin

end;

procedure TWiRLResourceModuleWizard.Execute;
var
  LResourceConfig: TResourceConfig;
  LModuleServices: IOTAModuleServices;
  LModule: IOTAModule;
begin
  if TformNewResourceDialog.FindConfig(LResourceConfig) then
  begin
    LModuleServices := BorlandIDEServices as IOTAModuleServices;
    LModule := LModuleServices.CreateModule(TWiRLServerResourcesCreator.Create(LResourceConfig));
    LModuleServices.OpenModule(LModule.FileName);
  end;
end;

function TWiRLResourceModuleWizard.GetAuthor: string;
begin
  Result := SAuthor;
end;

function TWiRLResourceModuleWizard.GetComment: string;
begin
  Result := SResourceComment;
end;

function TWiRLResourceModuleWizard.GetDesigner: string;
begin
  Result := dAny;
end;

function TWiRLResourceModuleWizard.GetGalleryCategory: IOTAGalleryCategory;
begin
  Result := (BorlandIDEServices as IOTAGalleryCategoryManager).FindCategory(SIDString);
end;

function TWiRLResourceModuleWizard.GetGlyph: Cardinal;
begin
{ TODO : function TWiRLServeProjectWizard.GetGlyph: Cardinal; }
  Result := LoadIcon(HInstance, 'WiRLServerWizardIcon');
end;

function TWiRLResourceModuleWizard.GetIDString: string;
begin
  Result := SIDString + '.Resource';
end;

function TWiRLResourceModuleWizard.GetName: string;
begin
  Result := SResourceName;
end;

function TWiRLResourceModuleWizard.GetPage: string;
begin
  Result := SGalleryCategory;
end;

function TWiRLResourceModuleWizard.GetPersonality: string;
begin
  Result := sDelphiPersonality;
end;

function TWiRLResourceModuleWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

function TWiRLResourceModuleWizard.IsVisible(Project: IOTAProject): Boolean;
begin
  Result := Assigned(Project);
end;

procedure TWiRLResourceModuleWizard.Modified;
begin

end;

end.
