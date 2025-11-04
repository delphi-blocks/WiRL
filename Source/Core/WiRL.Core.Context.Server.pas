{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Context.Server;

interface

uses
  System.Classes, System.SysUtils, System.Rtti,
  System.Generics.Collections, System.TypInfo,

  WiRL.http.URL,
  WiRL.Core.Context,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.Core.Auth.Context;

type
  TWiRLContextServer = class(TWiRLContextHttp)
  private
//    FEngine: TObject;
//    FApplication: TObject;
//    FResource: TObject;
//    FRequest: TWiRLRequest;
//    FAuthContext: TWiRLAuthContext;
    FResourceMethod: TObject;
    function GetResourceURL: TWiRLURL;
    function GetServer: TObject;
    procedure SetServer(const Value: TObject);
    function GetApplication: TObject;
    function GetAuthContext: TWiRLAuthContext;
    function GetEngine: TObject;
    function GetResource: TObject;
    procedure SetApplication(const Value: TObject);
    procedure SetAuthContext(const Value: TWiRLAuthContext);
    procedure SetEngine(const Value: TObject);
    procedure SetResource(const Value: TObject);
  public
    destructor Destroy; override;

    function GetCurrentEngineFromServer: TObject;
    function GetCurrentAppFromServer: TObject;

    property Server: TObject read GetServer write SetServer;
    property Engine: TObject read GetEngine write SetEngine;
    property Application: TObject read GetApplication write SetApplication;
    property Resource: TObject read GetResource write SetResource;
    // Alias for RequestURL (for compatibility reasons)
    property ResourceURL: TWiRLURL read GetResourceURL;
    property ResourceMethod: TObject read FResourceMethod write FResourceMethod;
    property AuthContext: TWiRLAuthContext read GetAuthContext write SetAuthContext;
  end;

  TWiRLContext = TWiRLContextServer;

implementation

uses
  WiRL.Core.Application,
  WiRL.Core.Metadata,
  WiRL.Engine.REST,
  WiRL.Configuration.Core,
  WiRL.http.Server;

destructor TWiRLContextServer.Destroy;
begin
  inherited;
end;

function TWiRLContextServer.GetCurrentAppFromServer: TObject;
var
  LServer: TWiRLServer;
  LEngine: TWiRLRESTEngine;
begin
  LServer := Server as TWiRLServer;
  LEngine := LServer.GetEngine(Request.PathInfo) as TWiRLRESTEngine;
  Result := LEngine.GetApplication(RequestURL);
end;

function TWiRLContextServer.GetCurrentEngineFromServer: TObject;
var
  LServer: TWiRLServer;
begin
  LServer := Server as TWiRLServer;
  Result := LServer.GetEngine(Request.PathInfo) as TWiRLRESTEngine;
end;

function TWiRLContextServer.GetApplication: TObject;
begin
  Result := FindContextDataAs<TWiRLApplication>;
end;

function TWiRLContextServer.GetAuthContext: TWiRLAuthContext;
begin
  Result := FindContextDataAs<TWiRLAuthContext>;
end;

function TWiRLContextServer.GetEngine: TObject;
begin
  Result := FindContextDataAs<TWiRLRESTEngine>;
end;

function TWiRLContextServer.GetResource: TObject;
begin
  Result := FindContextDataAs<TWiRLProxyResource>;
end;

function TWiRLContextServer.GetResourceURL: TWiRLURL;
begin
  Result := RequestURL;
end;

function TWiRLContextServer.GetServer: TObject;
begin
  Result := FindContextDataAs<TWiRLServer>;
end;

procedure TWiRLContextServer.SetApplication(const Value: TObject);
var
  LApp: TWiRLApplication;
  LPair: TPair<TWiRLConfigurationClass, TWiRLConfiguration>;
begin
  AddContainerOnce(Value);
  LApp := TWiRLApplication(Value);

  for LPair in LApp.Configs do
    AddContainerOnce(LPair.Value);
end;

procedure TWiRLContextServer.SetAuthContext(const Value: TWiRLAuthContext);
begin
  AddContainerOnce(Value);
  AddContainerOnce(Value.Subject);
end;

procedure TWiRLContextServer.SetEngine(const Value: TObject);
begin
  AddContainerOnce(Value);
end;

procedure TWiRLContextServer.SetResource(const Value: TObject);
begin
  AddContainerOnce(Value);
end;

procedure TWiRLContextServer.SetServer(const Value: TObject);
begin
  AddContainerOnce(Value);
end;

end.
