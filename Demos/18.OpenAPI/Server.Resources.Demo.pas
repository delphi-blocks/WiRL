{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2025 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Resources.Demo;

interface

uses
  System.Classes, System.SysUtils, System.JSON,

  WiRL.Core.OpenAPI.Resource,
  WiRL.Engine.REST,
  WiRL.Core.Application,
  WiRL.Core.Registry,
  WiRL.Core.Exceptions,
  WiRL.Core.Attributes,
  WiRL.Core.Application.Worker,
  WiRL.Core.MessageBody.Default,
  WiRL.Core.Auth.Context,
  WiRL.Core.Auth.Resource,
  WiRL.http.Accept.MediaType,
  WiRL.http.URL,
  WiRL.http.Request,
  WiRL.http.Response,
  Neon.Core.Persistence.JSON.Schema,
  Server.Entities;

type
{$SCOPEDENUMS ON}
  TEntity = (First, Second);

  /// <summary>
  ///   This **resource** serves to test the *OpenAPI 3* documentation generation
  /// </summary>
  [Path('/params')]
  TParametersResource = class(TObject)
    [Context] Response: TWiRLResponse;
  public
    [GET, Path('/test/list'), Produces(TMediaType.TEXT_PLAIN)]
    function ParamTestList: string;

    /// <summary>
    ///   Test doc method
    /// </summary>
    /// <param name="APathParam" required="true">
    ///   The first parameter
    /// </param>
    /// <returns>
    ///   Result is a string representing the input parameter
    /// </returns>
    [GET, Path('/test/{p}'), Produces(TMediaType.TEXT_PLAIN)]
    function ParamTest([PathParam('p')] APathParam: string): string;

    /// <summary>
    ///   Thist method shows several response codes
    /// </summary>
    /// <returns>
    ///   Resource's return value
    /// </returns>
    /// <remarks>
    ///   Here is a sample remarks placeholder.
    /// </remarks>
    /// <header name="X-Header" type="string">
    ///   Description of the header
    /// </header>
    /// <response code="405" name="BadRequest">
    ///   Bad request
    /// </response>
    /// <response code="400" name="BadRequest">
    ///   Bad request
    /// </response>
    /// <response code="404" name="NotFound">
    ///   [resource] not found in the database
    /// </response>
    [POST, Path('/record'), ResponseStatus(201, 'Entity created')]
    [Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JSON)]
    function ParamRecord([BodyParam] ABodyParam: TRecordParam): TRecordParam;

    procedure Test;
  end;

  [Path('/entities')]
  TEntitiesResource = class
    function ResponseStr([PathParam] AParam: string): string;

    [GET, Produces(TMediaType.APPLICATION_JSON)]
    function ResponseObject: TPerson;
  end;

  [Path('auth')]
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

procedure TParametersResource.Test;
begin
  //
end;

function TParametersResource.ParamRecord(ABodyParam: TRecordParam): TRecordParam;
begin
  // It's a record so there's a copy.
  Result := ABodyParam;
end;

function TParametersResource.ParamTest(APathParam: string): string;
begin
  Result := APathParam;
end;

function TParametersResource.ParamTestList: string;
begin
  Result := 'Test List';
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
  //TWiRLResourceRegistry.Instance.RegisterResource<TEntitiesResource>;
  TWiRLResourceRegistry.Instance.RegisterResource<TBasicAuthResource>;

end.

