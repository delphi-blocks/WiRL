unit WiRL.http.ProxyEngine;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.IOUtils, System.SyncObjs,

  WiRL.http.Accept.MediaType,
  WiRL.Core.Context,
  WiRL.Core.Exceptions,
  WiRL.http.Core,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.http.Server,
  WiRL.http.Client;

type
  EWiRLProxyEngineException = class(EWiRLException);

  TWiRLProxyEngine = class(TWiRLCustomEngine)
  private
    FRemoteUrl: string;
    FDebug: Boolean;
    procedure SetRemoteUrlProp(const Value: string);
    function BuildUrl(ARequest: TWiRLRequest): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function SetEngineName(const AEngineName: string): TWiRLProxyEngine;
    function SetRemoteUrl(const Value: string): TWiRLProxyEngine;
    function SetDebug(AValue: Boolean): TWiRLProxyEngine;
    procedure HandleRequest(AContext: TWiRLContext); override;
    procedure Startup; override;
  published
    property RemoteUrl: string read FRemoteUrl write SetRemoteUrlProp;
//    property OnError: TWiRLFileSystemErrorEvent read FOnError write FOnError;
  end;

implementation

uses
  System.Net.HttpClient;

{ TWiRLProxyEngine }

function TWiRLProxyEngine.BuildUrl(ARequest: TWiRLRequest): string;
begin
  Result := FRemoteUrl + Copy(ARequest.PathInfo, Length(BasePath) + 1, Length(ARequest.PathInfo));
  if ARequest.Query <> '' then
    Result := Result + '?' + ARequest.Query;
end;

constructor TWiRLProxyEngine.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TWiRLProxyEngine.Destroy;
begin
  inherited;
end;

procedure TWiRLProxyEngine.HandleRequest(AContext: TWiRLContext);
var
  LUrl: string;
  HttpClient: TWiRLClient;
begin
  inherited;
  HttpClient := TWiRLClient.Create(nil);
  try
    HttpClient.NoProtocolErrorException := True;
    LUrl := BuildUrl(AContext.Request);
    AContext.Response.ContentStream := TMemoryStream.Create;

    if FDebug then
      AContext.Response.HeaderFields['x-remote-url'] := LUrl;
    HttpClient.Request.HeaderFields.Assign(AContext.Request.HeaderFields);

    if AContext.Request.Method = TWiRLHttpMethod.GET.ToString then
      HttpClient.Get(LUrl, AContext.Response.ContentStream)
    else if AContext.Request.Method = TWiRLHttpMethod.POST.ToString then
    begin
      AContext.Request.ContentStream.Position := 0;
      HttpClient.Post(LUrl, AContext.Request.ContentStream, AContext.Response.ContentStream);
    end
    else if AContext.Request.Method = TWiRLHttpMethod.PUT.ToString then
    begin
      AContext.Request.ContentStream.Position := 0;
      HttpClient.Put(LUrl, AContext.Request.ContentStream, AContext.Response.ContentStream);
    end
    else if AContext.Request.Method = TWiRLHttpMethod.DELETE.ToString then
    begin
      HttpClient.Delete(LUrl, AContext.Response.ContentStream);
    end
    else
      raise EWiRLProxyEngineException.CreateFmt('Method [%s] not implemented', [AContext.Request.Method]);

    if HttpClient.Response.StatusCode <> 200 then
      raise EWiRLWebApplicationException.Create(HttpClient.Response.ReasonString, HttpClient.Response.StatusCode);

    // Copy all Headers
    AContext.Response.HeaderFields.Assign(HttpClient.Response.HeaderFields);
    if FDebug then
      AContext.Response.HeaderFields['x-remote-url'] := LUrl;
  finally
    HttpClient.Free;
  end;
end;

function TWiRLProxyEngine.SetDebug(AValue: Boolean): TWiRLProxyEngine;
begin
  FDebug := True;
  Result := Self;
end;

function TWiRLProxyEngine.SetEngineName(
  const AEngineName: string): TWiRLProxyEngine;
begin
  FEngineName := AEngineName;
  Result := Self;
end;

function TWiRLProxyEngine.SetRemoteUrl(const Value: string): TWiRLProxyEngine;
begin
  FRemoteUrl := Value;
  Result := Self;
end;

procedure TWiRLProxyEngine.SetRemoteUrlProp(const Value: string);
begin
  FRemoteUrl := Value;
end;

procedure TWiRLProxyEngine.Startup;
begin
  inherited;

end;

end.
