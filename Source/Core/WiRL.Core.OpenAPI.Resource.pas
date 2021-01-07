{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.OpenAPI.Resource;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections,
  System.Rtti,

  Neon.Core.Persistence.Swagger,

  WiRL.Core.JSON,
  WiRL.Core.OpenAPI,
  WiRL.Core.Application,
  WiRL.Core.Registry,
  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  WiRL.Rtti.Utils,
  WiRL.http.Response,
  WiRL.Core.Resource;

type
  TOpenAPIResourceCustom = class
  private
    [Context] App: TWiRLApplication;
    [Context] Response: TWiRLResponse;
    [Context] Resource: TWiRLResource;
  public
    [GET, Produces(TMediaType.APPLICATION_JSON)]
    function GetSwaggerJSON: TJSONObject;

    [GET, Produces(TMediaType.TEXT_HTML)]
    function GetSwaggerUI: string;
  end;

  [Path('/swagger')]
  TOpenAPIResourceDefault = class(TOpenAPIResourceCustom);

implementation

uses
  System.StrUtils, System.TypInfo,

  WiRL.Core.Utils,
  WiRL.http.Server;

{ TOpenAPIResourceCustom }

function TOpenAPIResourceCustom.GetSwaggerJSON: TJSONObject;
begin
  Response.HeaderFields.AddPair('Access-Control-Allow-Origin', '*');
  Result := TOpenAPIv2Engine.Generate(App, Resource.Path);
end;

function TOpenAPIResourceCustom.GetSwaggerUI: string;
begin
  Result := '<html><body>Please, be patient!</body></html>';
end;

end.
