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
    FResourceConfig: TResourceConfig;
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

    constructor Create(AResourceConfig: TResourceConfig);
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
  Result := nil;
end;

function TWiRLServerResourcesCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
var
  LResourcePath: string;
  LClassName: string;
  LMethodsInterface: string;
  LMethodsImplementation: string;
begin
  LClassName := 'T' + FUnitName;
  LResourcePath := FResourceConfig.Name;

  LMethodsInterface := '';
  LMethodsImplementation := '';

  if FResourceConfig.MethodGET then
  begin
    LMethodsInterface := LMethodsInterface +
      '    [GET]' + sLineBreak +
      '    [Produces(TMediaType.TEXT_PLAIN)]' + sLineBreak +
      '    function GetValue([QueryParam(''name'')] const AName: string): string;' + sLineBreak + sLineBreak;
    LMethodsImplementation := LMethodsImplementation +
      'function ' + LClassName + '.GetValue(const AName: string): string;' + sLineBreak +
      'begin' + sLineBreak +
      '  Result := ''Hello '' + AName + ''!'';' + sLineBreak +
      'end;' + sLineBreak + sLineBreak;
  end;

  if FResourceConfig.MethodPOST then
  begin
    LMethodsInterface := LMethodsInterface +
      '    [POST]' + sLineBreak +
      '    [Consumes(TMediaType.TEXT_PLAIN)]' + sLineBreak +
      '    [Produces(TMediaType.TEXT_PLAIN)]' + sLineBreak +
      '    function PostValue([BodyParam] const AValue: string): string;' + sLineBreak + sLineBreak;
    LMethodsImplementation := LMethodsImplementation +
      'function ' + LClassName + '.PostValue(const AValue: string): string;' + sLineBreak +
      'begin' + sLineBreak +
      '  Result := AValue;' + sLineBreak +
      'end;' + sLineBreak + sLineBreak;
  end;

  if FResourceConfig.MethodPUT then
  begin
    LMethodsInterface := LMethodsInterface +
      '    [PUT]' + sLineBreak +
      '    [Consumes(TMediaType.TEXT_PLAIN)]' + sLineBreak +
      '    [Produces(TMediaType.TEXT_PLAIN)]' + sLineBreak +
      '    function PutValue([BodyParam] const AValue: string): string;' + sLineBreak + sLineBreak;
    LMethodsImplementation := LMethodsImplementation +
      'function ' + LClassName + '.PutValue(const AValue: string): string;' + sLineBreak +
      'begin' + sLineBreak +
      '  Result := AValue;' + sLineBreak +
      'end;' + sLineBreak + sLineBreak;
  end;

  if FResourceConfig.MethodDELETE then
  begin
    LMethodsInterface := LMethodsInterface +
      '    [DELETE]' + sLineBreak +
      '    [Path(''{id}'')]' + sLineBreak +
      '    [Produces(TMediaType.TEXT_PLAIN)]' + sLineBreak +
      '    function DeleteValue([PathParam(''id'')] AId: Integer): string;' + sLineBreak + sLineBreak;
    LMethodsImplementation := LMethodsImplementation +
      'function ' + LClassName + '.DeleteValue(AId: Integer): string;' + sLineBreak +
      'begin' + sLineBreak +
      '  Result := ''Delete value with id equals to '' + IntToStr(AId) + ''!'';' + sLineBreak +
      'end;' + sLineBreak + sLineBreak;
  end;

  Result := TWiRLSourceFile.Create(
    TSourceBuilder.FromResource(SWiRLServerResources)
      .Add('UNIT_NAME', FUnitName)
      .Add('RESOURCE_PATH', LResourcePath)
      .Add('CLASS_NAME', LClassName)
      .Add('METHODS_INTERFACE', LMethodsInterface)
      .Add('METHODS_IMPLEMENTATION', LMethodsImplementation)
      .Build
  );
end;

function TWiRLServerResourcesCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

constructor TWiRLServerResourcesCreator.Create(AResourceConfig: TResourceConfig);
var
  LSuffix: string;
begin
  inherited Create;
  FResourceConfig := AResourceConfig;
  FFileName := GetNewModuleFileName(SServerResourcesFileName, '', '', False, LSuffix);
  FUnitName := ExtractFileName(ChangeFileExt(FFileName, ''));
end;

procedure TWiRLServerResourcesCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
end;

{$ENDREGION}


end.
