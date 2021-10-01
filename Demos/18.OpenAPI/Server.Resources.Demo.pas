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
    ///   Method with a *sample* documentation
    /// </summary>
    /// <param name="APathParam" required="true">
    ///   The first parameter
    /// </param>
    /// <returns>
    ///   Result is a string representing the input parameter
    /// </returns>
    /// <remarks>
    ///   Here is a sample remarks placeholder.
    /// </remarks>
    /// <response code="200">Succesful response description</response>
    ///  <header name="X-Header" type="string">Description of the header</header>
    /// <response code="400">Bad request</response>
    /// <response code="404">[resource] not found in the database</response>
    [GET, Path('/test/{p}'), Produces(TMediaType.TEXT_PLAIN)]
    function ParamTest([PathParam('p')] APathParam: string): string;

    [POST, Path('/record')]
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
