unit WiRL.Wizards.Modules.MainForm;

interface

uses
  WiRL.Wizards.Utils,
  ToolsAPI, WiRL.Wizards.Modules.Classes;

resourcestring
  SWiRLServerMainFormSRC = 'WiRLServerMainFormSRC';
  SWiRLServerMainFormDFM = 'WiRLServerMainFormDFM';
  SMainFormFileName = 'ServerMainForm';

type
  TWiRLServerMainFormCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  private
    FServerConfig: TServerConfig;
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

  TWiRLMainFormSource = class(TWiRLSourceFile)
  private
    FServerConfig: TServerConfig;
  public
    function GetSource: string; override;

    constructor Create(const AResourceName: string; AServerConfig: TServerConfig);
  end;

  TWiRLMainFormDfm = class(TWiRLSourceFile)
  private
    FServerConfig: TServerConfig;
  public
    function GetSource: string; override;

    constructor Create(const AResourceName: string; AServerConfig: TServerConfig);
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
  Result := GetCurrentDir + '\' + SMainFormFileName + '.pas';
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
  Result := TWiRLMainFormDfm.Create(SWiRLServerMainFormDFM, FServerConfig);
end;

function TWiRLServerMainFormCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := TWiRLMainFormSource.Create(SWiRLServerMainFormSRC, FServerConfig);
end;

function TWiRLServerMainFormCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := NIL;
end;

constructor TWiRLServerMainFormCreator.Create(AServerConfig: TServerConfig);
begin
  inherited Create;
  FServerConfig := AServerConfig;
end;

procedure TWiRLServerMainFormCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
end;

{$ENDREGION}

{ TWiRLMainFormSource }

constructor TWiRLMainFormSource.Create(const AResourceName: string;
  AServerConfig: TServerConfig);
begin
  inherited Create(AResourceName);
  FServerConfig := AServerConfig;
end;

function TWiRLMainFormSource.GetSource: string;
var
  MessageBodyUnit: string;
begin
  Result := inherited GetSource;

  if FServerConfig.UseDefaultMessageBody then
    MessageBodyUnit := '  WiRL.Core.MessageBody.Default,' + sLineBreak
  else
    MessageBodyUnit := '';

  Result := StringReplace(Result, '%ENGINE_PATH%', FServerConfig.EnginePath, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%APP_PATH%', FServerConfig.AppPath, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%SERVICE_PORT%', IntToStr(FServerConfig.ServerPort), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%MESSAGE_BODY_UNIT%', MessageBodyUnit, [rfReplaceAll, rfIgnoreCase]);
end;

{ TWiRLMainFormDfm }

constructor TWiRLMainFormDfm.Create(const AResourceName: string;
  AServerConfig: TServerConfig);
begin
  inherited Create(AResourceName);
  FServerConfig := AServerConfig;
end;

function TWiRLMainFormDfm.GetSource: string;
begin
  Result := inherited GetSource;

  Result := StringReplace(Result, '%SERVICE_PORT%', IntToStr(FServerConfig.ServerPort), [rfReplaceAll, rfIgnoreCase]);
end;

end.
