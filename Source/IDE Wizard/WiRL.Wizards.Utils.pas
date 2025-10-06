unit WiRL.Wizards.Utils;

interface

uses
  ToolsAPI;

/// <summary>
///  These funcions are all by David Hoyle's, and you can find them all in his book:
///  The Delphi Open Tools API book at:
/// </summary>

/// <summary>
///  Returns the current Project Group opened in the IDE
///  If there is no Project Group opened, returns NIL
/// </summary>
function ActiveProjectGroup: IOTAProjectGroup;

/// <summary>
///  Returns the current Project active (ie. in Bold) in the Project Manager
///  If there is no Project active, returns NILL
/// </summary>
function ActiveProject: IOTAProject;

/// <summary>
///  Retunrs Source Modules (DPR, DPK, etc) for the given Project
/// </summary>
function ProjectModule(const Project: IOTAProject): IOTAModule;

/// <summary>
///  Returns the active IDE Source Editor Interface. If there is no active Editor then this method returns NIL.
/// </summary>
function ActiveSourceEditor: IOTASourceEditor;

/// <summary>
///  Returns the IDE Source Editor Interface for a given Module. If there is no Editor then this method returns NIL.
/// </summary>
function SourceEditor(const Module: IOTAModule): IOTASourceEditor;

/// <summary>
///  Returns the SourceEditor source code as string
/// </summary>
function EditorAsString(const SourceEditor: IOTASourceEditor): string;

type
/// <summary>
///   Reads source code from a RT_RCDATA Resource
/// </summary>
  TWiRLSourceFile = class(TInterfacedObject, IOTAFile)
  strict private
    FResourceName: string;
    FModuleName: string;
  strict protected
    property ResourceName: string read FResourceName;
    property ModuleName: string read FModuleName;
  public
    constructor Create(const AResourceName: string); overload;
    constructor Create(const AResourceName, AModuleName: string); overload;
    function GetAge: TDateTime; virtual;
    function GetSource: string; virtual;
  end;

function GetDefaultDirectory: string;

function GetUniqueUnitName(const BaseUnitName: string): string;

function GetNewModuleFileName(const APrefix, AOptionalDirectory,
  AOptionalFileName: string; AUseDefaultFileExt: Boolean;
  out LSuffix: string; const AExtensions: string = ''): string;

implementation

uses
  System.Types,
  System.SysUtils,
	System.Classes;

function ExistsModule(const AModuleName: string): Boolean;
var
  ModuleServices: IOTAModuleServices;
  I: Integer;
begin
  Result := False;
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  for I := 0 to ModuleServices.ModuleCount - 1 do
  begin
    if SameText(ExtractFileName(ModuleServices.Modules[I].FileName), AModuleName) then
      Exit(True);
  end;
end;

function GetNewModuleFileName(const APrefix, AOptionalDirectory,
  AOptionalFileName: string; AUseDefaultFileExt: Boolean;
  out LSuffix: string; const AExtensions: string): string;
var
  LServices: IOTAModuleServices;

  function CheckFileExists(const AFileName: string): Boolean;
  begin
    Result := FileExists(AFileName) or
      ((BorlandIDEServices as IOTAModuleServices).FindModule(AFileName) <> nil);
  end;

  function CheckExtensions(const AFileName: string; const AExtensions: TStrings): Boolean;
  var
    Index: Integer;
  begin
    Result := False;
    for Index := 0 to AExtensions.Count - 1 do
    begin
      Result := CheckFileExists(ChangeFileExt(AFileName, AExtensions[Index]));
      if Result then
        Break;
    end;
  end;

  function ModuleOrFileExists(const AFileName: string; const AExtensions: TStrings): Boolean;
  begin
    Result := CheckFileExists(AFileName);
    if (not Result) and (AExtensions <> nil) then
      Result := CheckExtensions(AFileName, AExtensions)
  end;

  function CanFormatFileName(const AFileName: string): Boolean;
  begin
    Result := (Pos('%d', Lowercase(AFileName)) >= 1) or (Pos('%0:d', Lowercase(AFileName)) >= 1);
  end;

  function MakeFileName(const ADirectory, AFileName, AFileExt: string): string; overload;
  begin
    Result := Format('%0:s%1:s%2:d%3:s', [ADirectory, AFileName, AFileExt]);
  end;

  function MakeFileName(const ADirectory, AFileName, AIndex, AFileExt: string): string; overload;
  begin
    Assert(AFileName <> Format(AFileName, [AIndex]));
    Result := MakeFileName(ADirectory, Format(AFileName, [AIndex]), AFileExt);
  end;

  function FindNextAvailableFileName(const AFileName: string; out LSuffix: string; const AExtensions: TStrings): string;
  var
    I: Integer;
    LFileNameFormat: string;
  begin
    LSuffix := '';
    LFileNameFormat := AFileName;
    if not CanFormatFileName(LFileNameFormat) then
      LFileNameFormat := ExtractFilePath(LFileNameFormat) +
        ChangeFileExt(ExtractFileName(LFileNameFormat), '') + '%d' +
        ExtractFileExt(LFileNameFormat);
    I := 1;
    Result := Format(LFileNameFormat, [I]);
    while ModuleOrFileExists(Result, AExtensions) do
    begin
      Inc(I);
      Result := Format(LFileNameFormat, [I]);
    end;
    LSuffix := IntToStr(I);
  end;

  function GetDefaultFileExt: string;
  var
    LNewTextFileIdent, LNewClassName, LNewFileName: string;
  begin
    LServices.GetNewModuleAndClassName(APrefix,    // Do not localize
      LNewTextFileIdent, LNewClassName, LNewFileName);
    Result := ExtractFileExt(LNewFileName);
  end;

  function GetDefaultDirectory: string;
  var
    LNewTextFileIdent, LNewClassName, LNewFileName: string;
  begin
    LServices.GetNewModuleAndClassName(APrefix,    // Do not localize
      LNewTextFileIdent, LNewClassName, LNewFileName);
    Result := ExtractFilePath(LNewFileName);
  end;
var
  LFileName:string;
  LDirectory: string;
  LExtensions: TStrings;
begin
  LExtensions := nil;
  try
    if AExtensions <> '' then
    begin
      LExtensions := TStringList.Create;
      LExtensions.Delimiter := ';';
      LExtensions.DelimitedText := AExtensions;
      Assert(LExtensions.Count >= 1);
    end;

    LSuffix := '';
    LServices := (BorlandIDEServices as IOTAModuleServices);
    if AOptionalFileName = '' then
      LFileName := ChangeFileExt(APrefix + '%d', GetDefaultFileExt) // do not localize
    else
    begin
      LFileName := AOptionalFileName;
      if AUseDefaultFileExt then
        LFileName := ChangeFileExt(LFileName, GetDefaultFileExt);
    end;
    if AOptionalDirectory <> '' then
      LDirectory := ExtractFilePath(AOptionalDirectory)
    else
      LDirectory := GetDefaultDirectory;
    if not CanFormatFileName(LFileName) then
    begin
      Result := LDirectory + LFileName;
      if ModuleOrFileExists(Result, LExtensions) then
        Result := FindNextAvailableFileName(Result, LSuffix, LExtensions);
    end
    else
      Result := FindNextAvailableFileName(LDirectory + LFileName, LSuffix, LExtensions);
  finally
    LExtensions.Free;
  end;
end;

function GetDefaultDirectory: string;
var
  LServices: IOTAModuleServices;
  LNewTextFileIdent, LNewClassName, LNewFileName: string;
begin
  LServices := (BorlandIDEServices as IOTAModuleServices);
  LServices.GetNewModuleAndClassName('Unnamed',    // Do not localize
    LNewTextFileIdent, LNewClassName, LNewFileName);
  Result := ExtractFilePath(LNewFileName);
end;

function GetUniqueUnitName(const BaseUnitName: string): string;
var
  ModuleServices: IOTAModuleServices;
  Suffix: Integer;
begin
  ModuleServices := BorlandIDEServices as IOTAModuleServices;

  Suffix := 1;
  Result := BaseUnitName + IntToStr(Suffix);

  while ExistsModule(Result + '.pas') do
  begin
    Result := BaseUnitName + IntToStr(Suffix);
    Inc(Suffix);
  end;
end;

function ActiveProjectGroup: IOTAProjectGroup;
var
  I: Integer;
  AModuleServices: IOTAModuleServices;
  AModule: IOTAModule;
  AProjectGroup: IOTAProjectGroup;
begin
  Result := NIL;
  AModuleServices := BorlandIDEServices as IOTAModuleServices;
  for I := 0 to AModuleServices.ModuleCount - 1 do
  begin
    AModule := AModuleServices.Modules[I];
    if AModule.QueryInterface(IOTAProjectGroup, AProjectGroup) = S_OK then
      Break;
  end;
  Result := AProjectGroup;
end;

function ActiveProject: IOTAProject;
var
  PG: IOTAProjectGroup;
begin
  PG := ActiveProjectGroup;
  if PG <> NIL then
    Result := PG.ActiveProject;
end;

function ProjectModule(const Project: IOTAProject): IOTAModule;
var
  I: Integer;
  AModuleServices: IOTAModuleServices;
  AModule: IOTAModule;
  AProject: IOTAProject;
begin
  Result := NIL;
  AModuleServices := BorlandIDEServices as IOTAModuleServices;
  for I := 0 to AModuleServices.ModuleCount - 1 do
  begin
    AModule := AModuleServices.Modules[I];
    if (AModule.QueryInterface(IOTAProject, AProject) = S_OK) and (Project = AProject) then
      Break;
  end;
  Result := AProject;
end;

function SourceEditor(const Module: IOTAModule): IOTASourceEditor;
var
  I, LFileCount: Integer;
begin
  Result := NIL;
  if Module = NIL then
    Exit;

  LFileCount := Module.GetModuleFileCount;
  for I := 0 to LFileCount - 1 do
  begin
    if Module.GetModuleFileEditor(I).QueryInterface(IOTASourceEditor, Result) = S_OK then
      Break;
  end;
end;

function ActiveSourceEditor: IOTASourceEditor;
var
  CM: IOTAModule;
begin
  Result := NIL;
  if BorlandIDEServices = NIL then
    Exit;

  CM := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
  Result := SourceEditor(CM);
end;

function EditorAsString(const SourceEditor: IOTASourceEditor): string;
Const
  iBufferSize: Integer = 1024;
var
  Reader: IOTAEditReader;
  iPosition, iRead: Integer;
  strBuffer: AnsiString;
begin
  Result := '';
  Reader := SourceEditor.CreateReader;
  try
    iPosition := 0;
    repeat
      SetLength(strBuffer, iBufferSize);
      iRead := Reader.GetText(iPosition, PAnsiChar(strBuffer), iBufferSize);
      SetLength(strBuffer, iRead);
      Result := Result + string(strBuffer);
      Inc(iPosition, iRead);
    until iRead < iBufferSize;
  finally
    Reader := NIL;
  end;
end;

{$REGION 'TWiRLSourceFile'}

constructor TWiRLSourceFile.Create(const AResourceName: string);
begin
  inherited Create;
  FResourceName := AResourceName;
end;

constructor TWiRLSourceFile.Create(const AResourceName, AModuleName: string);
begin
  inherited Create;
  FResourceName := AResourceName;
  FModuleName := AModuleName;
end;

function TWiRLSourceFile.GetAge: TDateTime;
begin
  Result := -1;
end;

function TWiRLSourceFile.GetSource: string;
var
  Res: TResourceStream;
  S: TStrings;
begin
  if FModuleName = '' then
  begin
    FModuleName := GetUniqueUnitName('Unit');
  end;

  Res := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
  try
    if Res.Size = 0 then
      raise Exception.CreateFmt('Resource %s is empty', [ResourceName]);

    S := TStringList.Create;
    try
      Res.Position := 0;
      S.LoadFromStream(Res);
      Result := s.Text;
    finally
      S.Free;
    end;
  finally
    Res.Free;
  end;
  Result := StringReplace(Result, '%UNIT_NAME%', FModuleName, [rfReplaceAll, rfIgnoreCase]);
end;

{$ENDREGION}

end.
