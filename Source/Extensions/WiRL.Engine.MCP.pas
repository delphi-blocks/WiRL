{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2026 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Engine.MCP;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.SyncObjs, System.Diagnostics, System.Rtti,

  Neon.Core.Types,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,

  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Classes,
  MCPConnect.JRPC.Invoker,
  MCPConnect.JRPC.Server,
  MCPConnect.Transport.Base,

  MCPConnect.Configuration.Auth,

  WiRL.Engine.Core,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.http.Headers,
  WiRL.Core.Context.Server;

type
  TMCPEngine = class(TWiRLCustomEngine)
  private
    FJRPCServer: TJRPCServer;
    FAuthTokenConfig: TAuthTokenConfig;
    //function CheckAuthorization(Request: TWiRLRequest; Response: TWiRLResponse): Boolean;
    procedure SendHeaders(AMCPResponse: TMCPTransportResponse; AContext: TWiRLContext);
  public
    procedure HandleRequest(AContext: TWiRLContext); override;

    function SetServer(AServer: TJRPCServer): TMCPEngine;

    property JRPCServer: TJRPCServer read FJRPCServer write FJRPCServer;
  end;

  TMCPTransportWriterWiRL = class(TInterfacedObject, IMCPTransportWriter)
  private
    FContext: TWiRLContext;
  protected
    { IMCPTransportWriter }
    function Connected: Boolean;
    procedure Write(const AValue: string);
    procedure WriteComment(const AValue: string); overload;
    function SupportsStreaming: Boolean;
  public
    constructor Create(AContext: TWiRLContext);
  end;


implementation

uses
  Logify;

{ TMCPEngine }

procedure TMCPEngine.HandleRequest(AContext: TWiRLContext);
var
  LMcpHandler: IMCPTransportHandler;
  LWiRLRequest: TWiRLRequest;
  LWiRLResponse: TWiRLResponse;
begin
  if not Assigned(FJRPCServer) then
    raise EJRPCException.Create('JRPC JRPCServer not found');

  LWiRLRequest := AContext.Request;
  LWiRLResponse := AContext.Response;

  LMcpHandler := TMCPTransportHandler.Create(FJRPCServer, TMCPTransportWriterWiRL.Create(AContext));

  LMcpHandler.SendResponseHeadersProc :=
    procedure (AResponse: TMCPTransportResponse)
    begin
      SendHeaders(AResponse, AContext);
    end;

  LMcpHandler.ProcessRequest(

    procedure (ARequest: TMCPTransportRequest)
    var
      LHeader: TWiRLHeader;
    begin
      for LHeader in LWiRLRequest.Headers do
      begin
        var n := LHeader.Name;
        var v := LHeader.Value;
        ARequest.AddOrSetHeader(n, v);
      end;

      ARequest.Url := LWiRLRequest.PathInfo;
      ARequest.Command := LWiRLRequest.Method;
      ARequest.Content := LWiRLRequest.Content;

      Logger.LogInfo('SessionID ' + ARequest.Command + ' - ' + ARequest.GetHeader('Mcp-Session-Id'));
    end,

    procedure (AResponse: TMCPTransportResponse)
    begin
      LWiRLResponse.StatusCode := AResponse.Code;
      LWiRLResponse.Content := AResponse.Content;
      // SendHeaders after ContentText so indy can handle Content-Length
      SendHeaders(AResponse, AContext);

      //LogHttpResponse(AResponseInfo);
    end
  );
end;

procedure TMCPEngine.SendHeaders(AMCPResponse: TMCPTransportResponse;
  AContext: TWiRLContext);
var
  LHeader: TPair<string, string>;
  LWiRLResponse: TWiRLResponse;
begin
  LWiRLResponse := AContext.Response;
  LWiRLResponse.StatusCode := AMCPResponse.Code;
  for LHeader in AMCPResponse.Headers do
  begin
    LWiRLResponse.Headers.Values[LHeader.Key] := LHeader.Value;
  end;
end;

function TMCPEngine.SetServer(AServer: TJRPCServer): TMCPEngine;
begin
  FJRPCServer := AServer;
  Result := Self;

  FAuthTokenConfig := FJRPCServer.GetConfiguration<TAuthTokenConfig>;
end;

{ TMCPTransportWriterWiRL }

function TMCPTransportWriterWiRL.Connected: Boolean;
begin
  Result := False;
end;

constructor TMCPTransportWriterWiRL.Create(AContext: TWiRLContext);
begin
  inherited Create;
  FContext := AContext;
end;

function TMCPTransportWriterWiRL.SupportsStreaming: Boolean;
begin
  Result := False;
end;

procedure TMCPTransportWriterWiRL.Write(const AValue: string);
begin

end;

procedure TMCPTransportWriterWiRL.WriteComment(const AValue: string);
begin

end;

end.
