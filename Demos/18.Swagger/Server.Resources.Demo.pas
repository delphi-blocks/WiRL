{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Resources.Demo;

interface

uses
  System.Classes, System.SysUtils, System.JSON,

  WiRL.Core.OpenAPI.Resource,
  WiRL.Core.Engine,
  WiRL.Core.Application,
  WiRL.Core.Registry,
  WiRL.Core.Attributes,
  WiRL.Core.Application.Worker,
  WiRL.Core.MessageBody.Default,
  WiRL.Core.Auth.Context,
  WiRL.Core.Auth.Resource,
  WiRL.http.Accept.MediaType,
  WiRL.http.URL,
  WiRL.http.Request,
  WiRL.http.Response,

  Server.Entities;

type
  /// <summary>
  ///   This resource serves to test the OpenAPI 2.0 documentation generation
  /// </summary>
  [Path('/params')]
  TParametersResource = class(TObject)
    [Context] Response: TWiRLResponse;
  public
    /// <summary>
    ///   Method with a sample documentation
    /// </summary>
    /// <param name="AParam">
    ///   The first parameter
    /// </param>
    /// <returns>
    ///   Result is a string representing the input parameter
    /// </returns>
    /// <remarks>
    ///   Here is a sample remarks placeholder.
    /// </remarks>
    /// <response code="200">Responde description</response>
    [GET, Path('/test/{AParam}'), Produces(TMediaType.TEXT_PLAIN)]
    function ParamTest([PathParam] AParam: string): string;

    [GET, Path('/str/{AParam}'), Produces(TMediaType.TEXT_PLAIN)]
    function ParamStr([PathParam] AParam: string): string;

    [GET, Path('/int/{AParam}'), Produces(TMediaType.TEXT_PLAIN)]
    function ParamInt([PathParam] AParam: Integer): Integer;

    [GET, Path('/float/{AParam}'), Produces(TMediaType.TEXT_PLAIN)]
    function ParamFloat([PathParam] AParam: Double): Double;

    [GET, Path('/bool/{AParam}'), Produces(TMediaType.TEXT_PLAIN)]
    function ParamBool([PathParam] AParam: Boolean): Boolean;

    [GET, Path('/enum/{AParam}'), Produces(TMediaType.TEXT_PLAIN)]
    function ParamEnum([PathParam] AParam: TMyEnum): TMyEnum;

    [GET, Path('/date/{AParam}'), Produces(TMediaType.TEXT_PLAIN)]
    function ParamDate([PathParam] AParam: TDate): TDate;

    [GET, Path('/time/{AParam}'), Produces(TMediaType.TEXT_PLAIN)]
    function ParamTime([PathParam] AParam: TTime): TTime;

    [GET, Path('/datetime/{AParam}'), Produces(TMediaType.TEXT_PLAIN)]
    function ParamDateTime([PathParam] AParam: TDateTime): TDateTime;

    [POST, Path('/record')]
    [Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JSON)]
    function ParamRecord([BodyParam] AParam: TRecordParam): TRecordParam;
  end;

  [Path('/entities')]
  TEntitiesResource = class
    function ResponseStr([PathParam] AParam: string): string;

    [GET, Produces(TMediaType.APPLICATION_JSON)]
    function ResponseObject: TPerson;
  end;

  [Path('basic_auth')]
  TBasicAuthResource = class(TWiRLAuthBasicResource)
  private
  protected
    function Authenticate(const AUserName, APassword: string): TWiRLAuthResult; override;
  end;


implementation

uses
  System.DateUtils, System.StrUtils, System.IOUtils,

  WiRL.Core.JSON,
  WiRL.http.Accept.Language;

{ TParametersResource }

function TParametersResource.ParamFloat(AParam: Double): Double;
begin
  Result := AParam;
end;

function TParametersResource.ParamStr(AParam: string): string;
begin
  Result := AParam;
end;

function TParametersResource.ParamDate(AParam: TDate): TDate;
begin
  Result := AParam;
end;

function TParametersResource.ParamBool(AParam: Boolean): Boolean;
begin
  Result := AParam;
end;

function TParametersResource.ParamDateTime(AParam: TDateTime): TDateTime;
begin
  Result := AParam;
end;

function TParametersResource.ParamEnum(AParam: TMyEnum): TMyEnum;
begin
  Result := AParam;
end;

function TParametersResource.ParamTime(AParam: TTime): TTime;
begin
  Result := AParam;
end;

function TParametersResource.ParamInt(AParam: Integer): Integer;
begin
  Result := AParam;
end;

function TParametersResource.ParamRecord(AParam: TRecordParam): TRecordParam;
begin
  Result := AParam;
end;

function TParametersResource.ParamTest(AParam: string): string;
begin
  Result := AParam;
end;

{ TEntitiesResource }

function TEntitiesResource.ResponseObject: TPerson;
begin
  Result := TPerson.Create;
  Result.Name := 'Paolo';
  Result.Surname := 'Rossi';
  Result.DateProp := Now;
  Result.AddAddress('Piacenza', 'Italy');
  Result.AddAddress('Parma', 'Italy');
end;

function TEntitiesResource.ResponseStr(AParam: string): string;
begin
  Result := AParam;
end;

{ TBasicAuthResource }

function TBasicAuthResource.Authenticate(const AUserName, APassword: string): TWiRLAuthResult;
begin
  Result.Success := SameText(APassword, 'mypassword');
  Result.Roles := 'admin,manager,user'.Split([','])
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TParametersResource>;
  TWiRLResourceRegistry.Instance.RegisterResource<TEntitiesResource>;
  TWiRLResourceRegistry.Instance.RegisterResource<TBasicAuthResource>;

end.
