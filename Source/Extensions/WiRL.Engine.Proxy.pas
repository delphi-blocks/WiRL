unit WiRL.Engine.Proxy;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.IOUtils, System.SyncObjs,

  WiRL.http.Accept.MediaType,
  WiRL.http.Client.Interfaces,
  WiRL.Core.Context.Server,
  WiRL.Core.Context,
  WiRL.Core.Exceptions,
  WiRL.http.Core,
  WiRL.http.URL,
  WiRL.http.Headers,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.http.Server,
  WiRL.http.Client,
  WiRL.Engine.Core;

type
  EWiRLProxyEngineException = class(EWiRLServerException);

  TWiRLProxyEngine = class(TWiRLCustomEngine)
  private
    FRemoteUrl: string;
    FDebug: Boolean;
    FHost: string;
    procedure SetRemoteUrlProp(const Value: string);
    function BuildUrl(ARequest: TWiRLRequest): string;
    procedure CopyRequestHeader(LSource, LTarget: IWiRLHeaders);
    procedure CopyResponseHeader(LSource, LTarget: IWiRLHeaders);
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

procedure TWiRLProxyEngine.CopyRequestHeader(LSource, LTarget: IWiRLHeaders);
var
  LHeader: TWiRLHeader;
begin
  for LHeader in LSource do
  begin
    if LHeader.Name = 'Host' then
      LTarget.AddHeader(TWiRLHeader.Create('Host', FHost))
    else
      LTarget.AddHeader(TWiRLHeader.Create(LHeader.Name, LHeader.Value));
  end;
end;

procedure TWiRLProxyEngine.CopyResponseHeader(LSource, LTarget: IWiRLHeaders);
var
  LHeader: TWiRLHeader;
begin
  LTarget.Clear;
  for LHeader in LSource do
  begin
    if LHeader.Name = 'Transfer-Encoding' then
      Continue;
    LTarget.AddHeader(TWiRLHeader.Create(LHeader.Name, LHeader.Value));
  end;
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
  LResponse: IWiRLResponse;
  LHeaders: IWiRLHeaders;
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

    LHeaders := TWiRLHeaders.Create;
    CopyRequestHeader(AContext.Request.Headers, LHeaders);

    if AContext.Request.Method = TWiRLHttpMethod.GET.ToString then
      LResponse := HttpClient.Get(LUrl, AContext.Response.ContentStream)
    else if AContext.Request.Method = TWiRLHttpMethod.POST.ToString then
    begin
      AContext.Request.ContentStream.Position := 0;
      LResponse := HttpClient.Post(LUrl, AContext.Request.ContentStream, AContext.Response.ContentStream);
    end
    else if AContext.Request.Method = TWiRLHttpMethod.PUT.ToString then
    begin
      AContext.Request.ContentStream.Position := 0;
      LResponse := HttpClient.Put(LUrl, AContext.Request.ContentStream, AContext.Response.ContentStream);
    end
    else if AContext.Request.Method = TWiRLHttpMethod.DELETE.ToString then
    begin
      LResponse := HttpClient.Delete(LUrl, AContext.Response.ContentStream);
    end
    else
      raise EWiRLProxyEngineException.CreateFmt('Method [%s] not implemented', [AContext.Request.Method]);

    if LResponse.StatusCode <> 200 then
      raise EWiRLWebApplicationException.Create(LResponse.StatusText, LResponse.StatusCode);

    // Copy all Headers
    CopyResponseHeader(LResponse.Headers, AContext.Response.Headers);
    if FDebug then
      AContext.Response.Headers['x-remote-url'] := LUrl;
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
var
  LURL: TWiRLURL;
begin
  inherited;
  LURL := TWiRLURL.Create(FRemoteUrl);
  try
    FHost := LURL.HostName + ':' + LURL.PortNumber.ToString;
  finally
    LURL.Free;
  end;
end;

end.
