{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.FileSystemEngine;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.IOUtils,

  WiRL.http.Accept.MediaType,
  WiRL.Core.Context,
  WiRL.Core.Exceptions,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.http.Server;

type
  TWiRLFileSystemErrorEvent = procedure (ASender: TObject; AStatusCode: Integer; AContext: TWiRLContext) of object;

  TWiRLFileSystemEngine = class(TWiRLCustomEngine)
  private
  const
    DefaultRootFolder = '{AppPath}' + PathDelim + 'www';
    DefaultEngineName = 'WiRL FileSystemEngine';
  private
    FRootFolder: string;
    FExpandedRootFolder: string;
    FContentTypesForExt: TDictionary<string, string>;
    FIndexFileNames: TStringList;
    FOnError: TWiRLFileSystemErrorEvent;
    function GetContentType(const AFileName: string): string;
    procedure InitExtDictionary;
    procedure InitIndexFileNames;
    procedure ServeFileContent(const AFileNamme: string; AResponse: TWiRLResponse);
    procedure CheckRelativePath(const ARelativeURL: string);
    function DirectoryHasIndexFile(const ADirectory: string;
      out AIndexFullPath: string): Boolean;
    function ExpandMacros(const ATemplate: string): string;
    procedure HandleError(AStatusCode: Integer; AContext: TWiRLContext);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function SetEngineName(const AEngineName: string): TWiRLFileSystemEngine;
    function SetRootFolder(const ARootFolder: string): TWiRLFileSystemEngine;
    procedure HandleRequest(AContext: TWiRLContext); override;
    procedure Startup; override;
    property IndexFileNames: TStringList read FIndexFileNames;
    property ContentTypesForExt: TDictionary<string, string> read FContentTypesForExt;
  published
    property RootFolder: string read FRootFolder write FRootFolder;
    property OnError: TWiRLFileSystemErrorEvent read FOnError write FOnError;
  end;

implementation

const
  HtmlErrorTemplate =
    '<!DOCTYPE html>' + sLineBreak +
    '<html lang=en>' + sLineBreak +
    '  <meta charset=utf-8>' + sLineBreak +
    '  <meta name=viewport content="initial-scale=1, minimum-scale=1, width=device-width">' + sLineBreak +
    '  <title>{ErrorMessage}</title>' + sLineBreak +
    '  <h1>{AppName}</h1>' + sLineBreak +
    '  <p>{ErrorMessage}' + sLineBreak +
    '  <p>{Detail}' + sLineBreak +
    '</html>';

{ TWiRLFileSystemEngine }

procedure TWiRLFileSystemEngine.CheckRelativePath(const ARelativeURL: string);
begin
  if ARelativeURL.Contains('..') then
    raise EWiRLWebApplicationException.Create('Unprocessable Entity', 422);
end;

constructor TWiRLFileSystemEngine.Create(AOwner: TComponent);
begin
  inherited;
  FContentTypesForExt := TDictionary<string, string>.Create;
  FIndexFileNames := TStringList.Create;
  FRootFolder := DefaultRootFolder;
  FEngineName := DefaultEngineName;

  InitExtDictionary;
  InitIndexFileNames;
end;

destructor TWiRLFileSystemEngine.Destroy;
begin
  FContentTypesForExt.Free;
  FIndexFileNames.Free;
  inherited;
end;

function TWiRLFileSystemEngine.GetContentType(const AFileName: string): string;
begin
  if not FContentTypesForExt.TryGetValue(ExtractFileExt(AFileName), Result) then
    Result := TMediaType.APPLICATION_OCTET_STREAM;
end;

function TWiRLFileSystemEngine.DirectoryHasIndexFile(const ADirectory: string;
  out AIndexFullPath: string): Boolean;
var
  LIndex: Integer;
  LIndexFileName: string;
  LIndexFullFileName: string;
begin
  Result := False;
  for LIndex := 0 to IndexFileNames.Count-1 do
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

function TWiRLFileSystemEngine.ExpandMacros(const ATemplate: string): string;
begin
  Result := ATemplate.Replace('{AppPath}', ExtractFilePath(ParamStr(0)), [rfIgnoreCase]);
  Result := Result.Replace(PathDelim + PathDelim, PathDelim, [rfReplaceAll]);
end;

procedure TWiRLFileSystemEngine.HandleError(AStatusCode: Integer;
  AContext: TWiRLContext);
var
  LContent: string;
  LDetail: string;
  LErrorMessage: string;
begin
  LDetail := '';
  LErrorMessage := '<i>Error code: <b>' + IntToStr(AStatusCode) + '</b></i>';
  if AStatusCode = 404 then
  begin
    LDetail := 'The requested URL <code>' + AContext.Request.PathInfo + '</code> was not found on this server.';
    LErrorMessage := '<b>' + IntToStr(AStatusCode) + '</b> page not found';
  end;

  LContent := HtmlErrorTemplate;
  LContent := LContent.Replace('{ErrorMessage}', LErrorMessage, [rfReplaceAll]);
  LContent := LContent.Replace('{AppName}', FEngineName, [rfReplaceAll]);
  LContent := LContent.Replace('{Detail}', LDetail, [rfReplaceAll]);

  AContext.Response.StatusCode := AStatusCode;
  AContext.Response.ContentType := TMediaType.TEXT_HTML;
  AContext.Response.Content := LContent;

  if Assigned(FOnError) then
    FOnError(Self, AStatusCode, AContext);
end;

procedure TWiRLFileSystemEngine.HandleRequest(AContext: TWiRLContext);
var
  LRelativeURL: string;
  LFullPath: string;
  LIndexFileFullPath: string;
begin
  inherited;
  LRelativeURL := StringReplace(AContext.Request.PathInfo, '/', PathDelim, [rfReplaceAll]);
  CheckRelativePath(LRelativeURL);
  LFullPath := FExpandedRootFolder + LRelativeURL;

  if DirectoryExists(LFullPath) then
  begin
    if DirectoryHasIndexFile(LFullPath, LIndexFileFullPath) then
      ServeFileContent(LIndexFileFullPath, AContext.Response)
    else
      HandleError(404, AContext);
  end
  else if FileExists(LFullPath) then
  begin
    ServeFileContent(LFullPath, AContext.Response);
  end
  else
  begin
    HandleError(404, AContext);
  end;
end;

procedure TWiRLFileSystemEngine.InitExtDictionary;
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

procedure TWiRLFileSystemEngine.InitIndexFileNames;
begin
  IndexFileNames.Add('index.html');
  IndexFileNames.Add('index.htm');
  IndexFileNames.Add('default.html');
  IndexFileNames.Add('default.htm');
end;

procedure TWiRLFileSystemEngine.ServeFileContent(const AFileNamme: string;
  AResponse: TWiRLResponse);
begin
  AResponse.StatusCode := 200;
  AResponse.ContentStream := TFileStream.Create(AFileNamme, fmOpenRead);
  AResponse.ContentType := GetContentType(AFileNamme);
end;

function TWiRLFileSystemEngine.SetEngineName(
  const AEngineName: string): TWiRLFileSystemEngine;
begin
  FEngineName := AEngineName;
  Result := Self;
end;

function TWiRLFileSystemEngine.SetRootFolder(
  const ARootFolder: string): TWiRLFileSystemEngine;
begin
  RootFolder := ARootFolder;
  Result := Self;
end;

procedure TWiRLFileSystemEngine.Startup;
begin
  inherited;
  FExpandedRootFolder := ExpandMacros(FRootFolder);
end;

end.
