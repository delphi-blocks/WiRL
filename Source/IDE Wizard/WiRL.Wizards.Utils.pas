unit WiRL.Wizards.Utils;

interface

uses
  System.Classes, System.SysUtils,
  ToolsAPI;

/// <summary>
///  Returns the current Project Group opened in the IDE
/// </summary>
function ActiveProjectGroup: IOTAProjectGroup;

/// <summary>
///  Returns the current Project active in the Project Manager
/// </summary>
function ActiveProject: IOTAProject;

type
  /// <summary>
  /// Simple implementation of IOTAFile interface.
  /// The constructor parameter is returned by GetSource method.
  /// </summary>
  TWiRLSourceFile = class(TInterfacedObject, IOTAFile)
  private
    FSource: string;
  public
    constructor Create(const ASource: string);
    function GetAge: TDateTime; virtual;
    function GetSource: string; virtual;
  end;

  /// <summary>
  /// Interface for building source code by replacing text portions.
  /// Use the class through this interface to avoid manual memory management.
  /// </summary>
  ISourceBuilder = interface
    ['{CFEF5D9B-047C-4475-859A-E67565D02D3C}']
    function Add(const AName, AValue: string): ISourceBuilder;
    function Build: string;
  end;

  /// <summary>
  /// Builder class for replacing text portions in source code templates.
  /// Always use through the ISourceBuilder interface to avoid manual memory management.
  /// Initialize with FromResource to load text from a resource, or FromString for direct text input.
  /// </summary>
  TSourceBuilder = class(TInterfacedObject, ISourceBuilder)
  private
    FValues: TStrings;
    FSource: string;
  public
    class function FromResource(const AResourceName: string): ISourceBuilder;
    class function FromString(const ASource: string): ISourceBuilder;
  public
    function Add(const AName, AValue: string): ISourceBuilder;
    function Build: string;

    constructor Create(const ASource: string);
    destructor Destroy; override;
  end;

function GetNewModuleFileName(const APrefix, AOptionalDirectory,
  AOptionalFileName: string; AUseDefaultFileExt: Boolean;
  out LSuffix: string; const AExtensions: string = ''): string;

function LoadStringResource(const ResourceName: string): string;

implementation

uses
  System.Types;

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

function ActiveProjectGroup: IOTAProjectGroup;
var
  AModuleServices: IOTAModuleServices;
begin
  AModuleServices := BorlandIDEServices as IOTAModuleServices;
  Result := AModuleServices.MainProjectGroup;
end;

function ActiveProject: IOTAProject;
var
  LModuleServices: IOTAModuleServices;
begin
  LModuleServices := BorlandIDEServices as IOTAModuleServices;
  Result := LModuleServices.GetActiveProject;
end;

function LoadStringResource(const ResourceName: string): string;
var
  LResourceStream: TResourceStream;
  LValue: TStrings;
begin
  LResourceStream := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
  try
    if LResourceStream.Size = 0 then
      raise Exception.CreateFmt('Resource %s is empty', [ResourceName]);

    LValue := TStringList.Create;
    try
      LResourceStream.Position := 0;
      LValue.LoadFromStream(LResourceStream);
      Result := LValue.Text;
    finally
      LValue.Free;
    end;
  finally
    LResourceStream.Free;
  end;
end;

{ TWiRLSourceFile }

constructor TWiRLSourceFile.Create(const ASource: string);
begin
  inherited Create;
  FSource := ASource;
end;

function TWiRLSourceFile.GetAge: TDateTime;
begin
  Result := -1;
end;

function TWiRLSourceFile.GetSource: string;
begin
  Result := FSource;
end;

{ TSourceBuilder }

function TSourceBuilder.Add(const AName, AValue: string): ISourceBuilder;
begin
  FValues.Add(AName + FValues.NameValueSeparator + AValue);
  Result := Self;
end;

function TSourceBuilder.Build: string;
var
  I: Integer;
begin
  Result := FSource;
  for I := 0 to FValues.Count - 1 do
  begin
    Result := StringReplace(Result, '%' + FValues.Names[I] + '%', FValues.ValueFromIndex[I], [rfReplaceAll, rfIgnoreCase]);
  end;
end;

constructor TSourceBuilder.Create(const ASource: string);
begin
  inherited Create;
  FValues := TStringList.Create;
  FSource := ASource;
end;

destructor TSourceBuilder.Destroy;
begin
  FValues.Free;
  inherited;
end;

class function TSourceBuilder.FromResource(const AResourceName: string): ISourceBuilder;
begin
  Result := TSourceBuilder.Create(LoadStringResource(AResourceName));
end;

class function TSourceBuilder.FromString(const ASource: string): ISourceBuilder;
begin
  Result := TSourceBuilder.Create(ASource);
end;

end.
