{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2025 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.OpenAPI.Resource;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Rtti,

  WiRL.Configuration.OpenAPI,
  WiRL.Core.JSON,
  WiRL.Core.OpenAPI,
  WiRL.Core.Application,
  WiRL.Core.Registry,
  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  WiRL.Core.Utils,
  WiRL.Rtti.Utils,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.Core.Metadata;

type
  TWiRLProcessEvent = function(const AFileName: string): TStream of object;

  TSwaggerUIProvider = class
  private
    FBasePath: string;
    FRootFolder: string;
    FExpandedRootFolder: string;
    FContentTypesForExt: TDictionary<string, string>;
    FIndexFileNames: TStringList;
    FPathEngine: TWiRLTemplatePaths;
    FOnProcess: TWiRLProcessEvent;
    function GetContentType(const AFileName: string): string;
    function ServeFileContent(const AFileName: string; AResponse: TWiRLResponse): TStream;
    procedure CheckRelativePath(const ARelativeURL: string);
    function DirectoryHasIndexFile(const ADirectory: string; out AIndexFullPath: string): Boolean;
    procedure SetRootFolderProp(const Value: string);
  protected
    procedure InitExtDictionary; virtual;
    procedure InitIndexFileNames; virtual;
  public
    constructor Create(const ABasePath, ARootFolder: string);
    destructor Destroy; override;

    function HandleRequest(ARequest: TWiRLRequest; AResponse: TWiRLResponse): TStream;

    property IndexFileNames: TStringList read FIndexFileNames;
    property ContentTypesForExt: TDictionary<string, string> read FContentTypesForExt;
    property OnProcess: TWiRLProcessEvent read FOnProcess write FOnProcess;
    property RootFolder: string read FRootFolder write SetRootFolderProp;
  end;

  TOpenAPIResourceCustom = class
  private
    [Context] App: TWiRLApplication;
    [Context] Conf: TWiRLConfigurationOpenAPI;
    [Context] Request: TWiRLRequest;
    [Context] Response: TWiRLResponse;
    [Context] Resource: TWiRLProxyResource;
    function FilterContent(const AFileName: string): TStream;
  public
    [GET, Produces(TMediaType.APPLICATION_JSON)]
    function GetSwaggerJSON: TJSONObject;

    [GET]
    [Produces(TMediaType.TEXT_HTML)]
    function GetSwaggerHTML(): TStream;

    [GET]
    [Path('/{file}')]
    [Produces(TMediaType.TEXT_HTML)]
    function GetSwaggerAssets: TStream;
  end;

  [Path('openapi')]
  TOpenAPIResourceDefault = class(TOpenAPIResourceCustom);

implementation

uses
  System.StrUtils, System.TypInfo, System.IOUtils,
  WiRL.Core.Exceptions;

{ TOpenAPIResourceCustom }

function TOpenAPIResourceCustom.FilterContent(const AFileName: string): TStream;
var
  LURL, LLine: string;
  LReader: TStreamReader;
  LWriter: TStreamWriter;
begin
  LURL := '';
  // Filters only html files
  if not SameText('.html', ExtractFileExt(AFileName)) then
    Exit(TFileStream.Create(AFileName, fmOpenRead));

  if Conf.Document.Servers.Count > 0 then
    LURL :=  IncludeTrailingSlash(Conf.Document.Servers[0].Url)
  else
    LURL := 'http://localhost/';

  LURL := IncludeTrailingSlash(CombineURL(LURL, Resource.GetSanitizedPath));

  Result := TMemoryStream.Create;
  LReader := TStreamReader.Create(AFileName);
  LWriter := TStreamWriter.Create(Result);
  try
    while not LReader.EndOfStream do
    begin
      LLine := LReader.ReadLine.Replace('{%url%}', LURL);
      LLine := LLine.Replace('{%logo%}', Conf.APILogo);
      LWriter.WriteLine(LLine);
    end;
    Result.Position := 0;
  finally
    LReader.Free;
    LWriter.Free;
  end;
end;

function TOpenAPIResourceCustom.GetSwaggerJSON: TJSONObject;
var
  LConfig: TOpenAPIv3EngineConfig;
begin
  LConfig.Application := App;
  LConfig.SwaggerResource := Resource.Path;
  Result := TOpenAPIv3Engine.Generate(LConfig);
end;

function TOpenAPIResourceCustom.GetSwaggerAssets: TStream;
var
  LProvider: TSwaggerUIProvider;
  LPathInfo: string;
begin
  LPathInfo := EnsureSuffix(CombineURL(App.Path, Resource.GetSanitizedPath), '/');

  LProvider := TSwaggerUIProvider.Create(LPathInfo, Conf.FolderGUIDoc);
  try
    Result := LProvider.HandleRequest(Request, Response);
  finally
    LProvider.Free;
  end;
end;

function TOpenAPIResourceCustom.GetSwaggerHTML: TStream;
var
  LProvider: TSwaggerUIProvider;
  LPathInfo: string;
begin
  LPathInfo := EnsureSuffix(CombineURL(App.Path, Resource.GetSanitizedPath), '/');
  LProvider := TSwaggerUIProvider.Create(LPathInfo, Conf.FolderGUIDoc);
  try
    LProvider.OnProcess := FilterContent;
    Result := LProvider.HandleRequest(Request, Response);
  finally
    LProvider.Free;
  end;
end;

{ TSwaggerUIProvider }

procedure TSwaggerUIProvider.CheckRelativePath(const ARelativeURL: string);
begin
  if ARelativeURL.Contains('..') then
    raise EWiRLWebApplicationException.Create('Unprocessable Entity', 422);
end;

constructor TSwaggerUIProvider.Create(const ABasePath, ARootFolder: string);
begin
  FContentTypesForExt := TDictionary<string, string>.Create;
  FPathEngine := TWiRLTemplatePaths.Create();
  FIndexFileNames := TStringList.Create;

  InitExtDictionary;
  InitIndexFileNames;

  FBasePath := ABasePath;
  FRootFolder := ARootFolder;
  FExpandedRootFolder := FPathEngine.Render(FRootFolder);
end;

destructor TSwaggerUIProvider.Destroy;
begin
  FContentTypesForExt.Free;
  FIndexFileNames.Free;
  FPathEngine.Free;
  inherited;
end;

function TSwaggerUIProvider.GetContentType(const AFileName: string): string;
begin
  if not FContentTypesForExt.TryGetValue(ExtractFileExt(AFileName), Result) then
    Result := TMediaType.APPLICATION_OCTET_STREAM;
end;

function TSwaggerUIProvider.DirectoryHasIndexFile(const ADirectory: string;
  out AIndexFullPath: string): Boolean;
var
  LIndex: Integer;
  LIndexFileName: string;
  LIndexFullFileName: string;
begin
  Result := False;
  for LIndex := 0 to IndexFileNames.Count - 1 do
  begin
    LIndexFileName := IndexFileNames[LIndex];
    LIndexFullFileName := TPath.Combine(ADirectory, LIndexFileName);
    if FileExists(LIndexFullFileName) then
    begin
      Result := True;
      AIndexFullPath := LIndexFullFileName;
      Break;
    end;
  end;
end;

function TSwaggerUIProvider.HandleRequest(ARequest: TWiRLRequest; AResponse: TWiRLResponse): TStream;
var
  LRelativeURL: string;
  LFullPath: string;
  LIndexFileFullPath: string;
begin
  LRelativeURL := ARequest.PathInfo.Substring(FBasePath.Length);
  LRelativeURL := LRelativeURL.Replace('/', PathDelim, [rfReplaceAll]);

  CheckRelativePath(LRelativeURL);
  if LRelativeURL.StartsWith(PathDelim) then
    LFullPath := FExpandedRootFolder + LRelativeURL
  else
    LFullPath := FExpandedRootFolder + PathDelim + LRelativeURL;

  if DirectoryExists(LFullPath) then
  begin
    if DirectoryHasIndexFile(LFullPath, LIndexFileFullPath) then
      Result := ServeFileContent(LIndexFileFullPath, AResponse)
    else
      raise EWiRLNotFoundException.CreateFmt('File [%s] not found', [LIndexFileFullPath]);
  end
  else if FileExists(LFullPath) then
    Result := ServeFileContent(LFullPath, AResponse)
  else
    raise EWiRLNotFoundException.CreateFmt('File [%s] not found', [LFullPath]);

  if not Assigned(Result) then
    raise EWiRLNotFoundException.CreateFmt('File not found', [LFullPath]);

end;

procedure TSwaggerUIProvider.InitExtDictionary;
begin
  ContentTypesForExt.Add('.svg', 'image/svg+xml');
  ContentTypesForExt.Add('.jpg', 'image/jpeg');
  ContentTypesForExt.Add('.jpeg', 'image/jpeg');
  ContentTypesForExt.Add('.png', 'image/png');
  ContentTypesForExt.Add('.pdf', 'application/pdf');
  ContentTypesForExt.Add('.htm', 'text/html');
  ContentTypesForExt.Add('.html', 'text/html');
  ContentTypesForExt.Add('.js', 'application/javascript');
  ContentTypesForExt.Add('.css', 'text/css');
  ContentTypesForExt.Add('.txt', 'text/plain');
end;

procedure TSwaggerUIProvider.InitIndexFileNames;
begin
  IndexFileNames.Add('index.html');
  IndexFileNames.Add('index.htm');
  IndexFileNames.Add('default.html');
  IndexFileNames.Add('default.htm');
end;

function TSwaggerUIProvider.ServeFileContent(const AFileName: string; AResponse: TWiRLResponse): TStream;
begin
  if Assigned(FOnProcess) then
    Result := FOnProcess(AFileName)
  else
    Result := TFileStream.Create(AFileName, fmOpenRead);

  AResponse.ContentType := GetContentType(AFileName);
end;

procedure TSwaggerUIProvider.SetRootFolderProp(const Value: string);
begin
  if FRootFolder <> Value then
  begin
    if (Value.Length > 1) and (Value.EndsWith(PathDelim)) then
      FRootFolder := Value.Substring(0, Value.Length - 1)
    else
      FRootFolder := Value;
  end;
end;

end.
