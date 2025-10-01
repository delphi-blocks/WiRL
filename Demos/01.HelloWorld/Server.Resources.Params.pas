{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2022 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Resources.Params;

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

  Demo.Entities;

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

    [GET, Path('/date/{AParam}'), Produces(TMediaType.TEXT_PLAIN)]
    function ParamDate([PathParam] AParam: TDate): TDate;

    [GET, Path('/time/{AParam}'), Produces(TMediaType.TEXT_PLAIN)]
    function ParamTime([PathParam] AParam: TTime): TTime;

    [GET, Path('/datetime/{AParam}'), Produces(TMediaType.TEXT_PLAIN)]
    function ParamDateTime([PathParam] AParam: TDateTime): TDateTime;

    [GET, Path('/object/{AParam}'), Produces(TMediaType.TEXT_PLAIN)]
    function ParamObject([PathParam] AParam: TSimpleParam): string;

    [POST, Path('/record')]
    [Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JSON)]
    function ParamRecord([BodyParam] AParam: TRecordParam): TRecordParam;

    [GET, Path('/array/{AParam}')]
    [Produces(TMediaType.APPLICATION_JSON)]
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
  Result := AParam / 10;
end;

function TParametersResource.ParamStr(AParam: string): string;
begin
  Result := 'Value: ' + AParam;
end;

function TParametersResource.ParamDate(AParam: TDate): TDate;
begin
  Result := IncYear(AParam, 1);
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
  Result := not AParam;
end;

function TParametersResource.ParamDateTime(AParam: TDateTime): TDateTime;
begin
  Result := IncHour(AParam, 12);
end;

function TParametersResource.ParamEnum(AParam: TMyEnum): TMyEnum;
begin
  Result := TMyEnum((Ord(AParam) + 1 ) mod Ord(High(TMyEnum)));
end;

function TParametersResource.ParamTime(AParam: TTime): TTime;
begin
  Result := AParam;
end;

function TParametersResource.ParamInt(AParam: Integer): Integer;
begin
  Result := AParam + 1;
end;

function TParametersResource.ParamObject(AParam: TSimpleParam): string;
begin
  Result := AParam.ToString;
end;

function TParametersResource.ParamRecord(AParam: TRecordParam): TRecordParam;
begin
  Result := AParam;
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TParametersResource>;

end.
