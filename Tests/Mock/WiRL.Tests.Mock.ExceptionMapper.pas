{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Tests.Mock.ExceptionMapper;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Defaults,

  WiRL.Core.JSON,
  WiRL.Core.Application,
  WiRL.Core.Exceptions;

type
  TWiRLMyNotFoundExceptionMapper = class(TWiRLExceptionMapper)
  public
    procedure HandleException(AExceptionContext: TWiRLExceptionContext); override;
  end;

implementation

uses
  WiRL.Tests.Mock.Classes;

procedure TWiRLMyNotFoundExceptionMapper.HandleException(
  AExceptionContext: TWiRLExceptionContext);
const
  StatusCode = 400;
var
  LMyException: EMyNotFoundException;
  LJSON: TJSONObject;
begin
  inherited;
  LMyException := AExceptionContext.Error as EMyNotFoundException;

  LJSON := TJSONObject.Create;
  try
    LJSON.AddPair(TJSONPair.Create('stausCode', TJSONNumber.Create(StatusCode)));
    LJSON.AddPair(TJSONPair.Create('message', TJSONString.Create(LMyException.Message)));
    LJSON.AddPair(TJSONPair.Create('exception', TJSONString.Create(LMyException.ClassName)));
    LJSON.AddPair(TJSONPair.Create('errorCode', TJSONNumber.Create(LMyException.ErrorCode)));

    AExceptionContext.Response.StatusCode := StatusCode;
    AExceptionContext.Response.ContentType := 'application/json';
    AExceptionContext.Response.Content := TJSONHelper.ToJSON(LJSON);

  finally
    LJSON.Free;
  end;

end;

initialization

  TWiRLExceptionMapperRegistry.Instance.RegisterExceptionMapper<TWiRLMyNotFoundExceptionMapper, EMyNotFoundException>();

end.
