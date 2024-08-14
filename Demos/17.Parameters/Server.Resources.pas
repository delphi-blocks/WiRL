{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2022 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Resources;

interface

uses
  System.Classes, System.SysUtils, System.JSON,

  WiRL.Engine.REST,
  WiRL.Core.Application,
  WiRL.Core.Registry,
  WiRL.Core.Attributes,
  WiRL.Core.Application.Worker,
  WiRL.Core.Metadata,
  WiRL.Core.MessageBody.Default,
  WiRL.Core.Auth.Context,
  WiRL.http.Accept.MediaType,
  WiRL.http.URL,
  WiRL.http.Request,
  WiRL.http.Response,

  Server.Entities;

type
  [Path('/params')]
  TParametersResource = class
  public
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

    [GET, Path('/customenum/{AParam}'), Produces(TMediaType.TEXT_PLAIN)]
    function ParamCustomEnum([PathParam] AParam: TCustomEnum): TCustomEnum;

    [GET, Path('/date/{AParam}'), Produces(TMediaType.TEXT_PLAIN)]
    function ParamDate([PathParam] AParam: TDate): TDate;

    [GET, Path('/time/{AParam}'), Produces(TMediaType.TEXT_PLAIN)]
    function ParamTime([PathParam] AParam: TTime): TTime;

    [GET, Path('/datetime/{AParam}'), Produces(TMediaType.TEXT_PLAIN)]
    function ParamDateTime([PathParam] AParam: TDateTime): TDateTime;

    [POST, Path('/record')]
    [Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JSON)]
    function ParamRecord([BodyParam] AParam: TRecordParam): TRecordParam;

    [GET, Path('/array/{AParam}')]
    [Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JSON)]
    function ParamArray([PathParam] AParam: TArrayInt): TArrayInt;

    [POST, Path('/arraybody')]
    [Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JSON)]
    function ParamArrayBody([BodyParam] AParam: TArrayParam): TArrayParam;
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

function TParametersResource.ParamArray(AParam: TArrayInt): TArrayInt;
begin
  Result := AParam;
end;

function TParametersResource.ParamArrayBody(AParam: TArrayParam): TArrayParam;
begin
  Result := AParam;
end;

function TParametersResource.ParamBool(AParam: Boolean): Boolean;
begin
  Result := AParam;
end;

function TParametersResource.ParamCustomEnum(AParam: TCustomEnum): TCustomEnum;
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

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TParametersResource>;

end.
