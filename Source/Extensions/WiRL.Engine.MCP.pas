{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2025 WiRL Team                                      }
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
  MCPConnect.JRPC.Invoker,
  MCPConnect.JRPC.Server,

  MCPConnect.Configuration.Auth,

  WiRL.Engine.Core,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.Core.Context.Server;

type
  TMCPEngine = class(TWiRLCustomEngine)
  private
    FServer: TJRPCServer;
    FAuthTokenConfig: TAuthTokenConfig;
    function CheckAuthorization(Request: TWiRLRequest; Response: TWiRLResponse): Boolean;
  public
    procedure HandleRequest(AWiRLContext: TWiRLContext); override;

    function SetServer(AServer: TJRPCServer): TMCPEngine;

    property Server: TJRPCServer read FServer write FServer;
  end;

implementation

uses
  MCPConnect.Core.Utils;

{ TMCPEngine }

function TMCPEngine.CheckAuthorization(Request: TWiRLRequest; Response: TWiRLResponse): Boolean;
begin
  Result := True;
  if Assigned(FAuthTokenConfig) and (FAuthTokenConfig.Token <> '') then
  begin
    case FAuthTokenConfig.Location of
      TAuthTokenLocation.Bearer:
      begin
        if Request.Authorization <> 'Bearer ' + FAuthTokenConfig.Token then
          Exit(False);
      end;

      TAuthTokenLocation.Cookie:
      begin
        if Request.CookieFields.Values[FAuthTokenConfig.CustomHeader] <> FAuthTokenConfig.Token then
          Exit(False);
      end;

      TAuthTokenLocation.Header:
      begin
        if Request.Headers.Values[FAuthTokenConfig.CustomHeader] <> FAuthTokenConfig.Token then
          Exit(False);
      end;

      else
        raise EJSONRPCException.Create('Invalid token location');
    end;
  end;
end;

procedure TMCPEngine.HandleRequest(AWiRLContext: TWiRLContext);
var
  LGarbageCollector: IGarbageCollector;
  LRequest: TJRPCRequest;
  LResponse: TJRPCResponse;
  LConstructorProxy: TJRPCConstructorProxy;
  LInstance: TObject;
  LInvokable: IJRPCInvokable;
  LContext: TJRPCContext;
begin
  if not Assigned(FServer) then
    raise EJSONRPCException.Create('Server not found');

  if not CheckAuthorization(AWiRLContext.Request, AWiRLContext.Response) then
  begin
    AWiRLContext.Response.StatusCode := 403;
    AWiRLContext.Response.Content := '';
    Exit;
  end;

  LGarbageCollector := TGarbageCollector.CreateInstance;

  LResponse := TJRPCResponse.Create;
  LGarbageCollector.Add(LResponse);

  LRequest := TNeon.JSONToObject<TJRPCRequest>(AWiRLContext.Request.Content, JRPCNeonConfig);
  LGarbageCollector.Add(LRequest);

  if not TJRPCRegistry.Instance.GetConstructorProxy(LRequest.Method, LConstructorProxy) then
  begin
    AWiRLContext.Response.StatusCode := 404;
    AWiRLContext.Response.ReasonString := 'Not found';
    Exit;
  end;
  LInstance := LConstructorProxy.ConstructorFunc();
  LGarbageCollector.Add(LInstance);

  LContext := TJRPCContext.Create;
  LGarbageCollector.Add(LContext);

  LContext.AddContent(LRequest);
  LContext.AddContent(LResponse);
  LContext.AddContent(FServer);

  // Injects the context inside the instance
  LContext.Inject(LInstance);

  LInvokable := TJRPCObjectInvoker.Create(LInstance);
  LInvokable.NeonConfig := LConstructorProxy.NeonConfig;
  if not LInvokable.Invoke(LContext, LRequest, LResponse) then
  begin
    AWiRLContext.Response.StatusCode := 404;
    AWiRLContext.Response.ReasonString := 'Not found';
    Exit;
  end;

  AWiRLContext.Response.ContentType := 'application/json';
  if LResponse.IsNotification then
  begin
    AWiRLContext.Response.StatusCode := 204;
    AWiRLContext.Response.Content := '';
  end
  else
  begin
    AWiRLContext.Response.Content := TNeon.ObjectToJSONString(LResponse, JRPCNeonConfig);
  end;
end;

function TMCPEngine.SetServer(AServer: TJRPCServer): TMCPEngine;
begin
  FServer := AServer;
  Result := Self;

  FAuthTokenConfig := FServer.GetConfiguration<TAuthTokenConfig>;
end;

end.
