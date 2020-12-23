{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
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
    FResourceURL: TWiRLURL;
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

    property Server: TObject read GetServer write SetServer;
    property Engine: TObject read GetEngine write SetEngine;
    property Application: TObject read GetApplication write SetApplication;
    property Resource: TObject read GetResource write SetResource;
    property ResourceURL: TWiRLURL read GetResourceURL write FResourceURL;
    property AuthContext: TWiRLAuthContext read GetAuthContext write SetAuthContext;
  end;

  TWiRLContext = TWiRLContextServer;

implementation

uses
  WiRL.Core.Application,
  WiRL.Core.Resource,
  WiRL.Core.Engine,
  WiRL.Configuration.Core,
  WiRL.http.Server;

destructor TWiRLContextServer.Destroy;
begin
  FResourceURL.Free;
  inherited;
end;

function TWiRLContextServer.GetApplication: TObject;
begin
  Result := FindContainerAs<TWiRLApplication>;
end;

function TWiRLContextServer.GetAuthContext: TWiRLAuthContext;
begin
  Result := FindContainerAs<TWiRLAuthContext>;
end;

function TWiRLContextServer.GetEngine: TObject;
begin
  Result := FindContainerAs<TWiRLEngine>;
end;

function TWiRLContextServer.GetResource: TObject;
begin
  Result := FindContainerAs<TWiRLResource>;
end;

function TWiRLContextServer.GetResourceURL: TWiRLURL;
begin
  if not Assigned(FResourceURL) then
    FResourceURL := TWiRLURL.Create(Request);
  Result := FResourceURL;
end;

function TWiRLContextServer.GetServer: TObject;
begin
  Result := FindContainerAs<TWiRLServer>;
end;

procedure TWiRLContextServer.SetApplication(const Value: TObject);
var
  LApp: TWiRLApplication;
  LPair: TPair<TWiRLConfigurationClass, TWiRLConfiguration>;
begin
  AddContainerOnce(Value, False);
  LApp := TWiRLApplication(Value);

  for LPair in LApp.Configs do
    AddContainerOnce(LPair.Value, False);
end;

procedure TWiRLContextServer.SetAuthContext(const Value: TWiRLAuthContext);
begin
  AddContainerOnce(Value, False);
end;

procedure TWiRLContextServer.SetEngine(const Value: TObject);
begin
  AddContainerOnce(Value, False);
end;

procedure TWiRLContextServer.SetResource(const Value: TObject);
begin
  AddContainerOnce(Value, False);
end;

procedure TWiRLContextServer.SetServer(const Value: TObject);
begin
  AddContainerOnce(Value, False);
end;

end.
