unit WiRL.Wizards.Modules.MainForm;

interface

uses
  WiRL.Wizards.Utils,
  ToolsAPI, WiRL.Wizards.Modules.Classes;

resourcestring
  SWiRLServerMainFormSRC = 'WiRLServerMainFormSRC';
  SWiRLServerMainFormDFM = 'WiRLServerMainFormDFM';
  SMainFormFileName = 'FormUnit';

type
  TWiRLServerMainFormCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
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

implementation

uses
  System.SysUtils;

{$REGION 'IOTACreator'}

function TWiRLServerMainFormCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TWiRLServerMainFormCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TWiRLServerMainFormCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TWiRLServerMainFormCreator.GetOwner: IOTAModule;
begin
  Result := ActiveProject;
end;

function TWiRLServerMainFormCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

{$ENDREGION}
{$REGION 'IOTAModuleCreator'}

function TWiRLServerMainFormCreator.GetAncestorName: string;
begin
  Result := 'TForm';
end;

function TWiRLServerMainFormCreator.GetImplFileName: string;
begin
  Result := FFileName;
end;

function TWiRLServerMainFormCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TWiRLServerMainFormCreator.GetFormName: string;
begin
  Result := 'MainForm';
end;

function TWiRLServerMainFormCreator.GetMainForm: Boolean;
begin
  Result := True;
end;

function TWiRLServerMainFormCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TWiRLServerMainFormCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TWiRLServerMainFormCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := TWiRLSourceFile.Create(
    TSourceBuilder.FromResource(SWiRLServerMainFormDFM)
      .Add('SERVICE_PORT', IntToStr(FServerConfig.ServerPort))
      .Build
  );
end;

function TWiRLServerMainFormCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
var
  LMessageBodyUnit: string;
begin
  if FServerConfig.UseDefaultMessageBody then
    LMessageBodyUnit := 'WiRL.Core.MessageBody.Default,' + sLineBreak
  else
    LMessageBodyUnit := '';

  Result := TWiRLSourceFile.Create(
    TSourceBuilder.FromResource(SWiRLServerMainFormSRC)
      .Add('UNIT_NAME', FUnitName)
      .Add('ENGINE_PATH', FServerConfig.EnginePath)
      .Add('APP_PATH', FServerConfig.AppPath)
      .Add('SERVICE_PORT', IntToStr(FServerConfig.ServerPort))
      .Add('MESSAGE_BODY_UNIT', LMessageBodyUnit)
      .Build
  );

end;

function TWiRLServerMainFormCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

constructor TWiRLServerMainFormCreator.Create(AServerConfig: TServerConfig);
var
  LSuffix: string;
begin
  inherited Create;
  FServerConfig := AServerConfig;
  FFileName := GetNewModuleFileName(SMainFormFileName, '', '', False, LSuffix);
  FUnitName := ExtractFileName(ChangeFileExt(FFileName, ''));
end;

procedure TWiRLServerMainFormCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
end;

{$ENDREGION}

end.
