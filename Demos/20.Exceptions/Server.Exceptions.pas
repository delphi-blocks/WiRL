{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2025 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Exceptions;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Defaults,

  WiRL.Core.JSON,
  WiRL.Core.Application,
  WiRL.Core.Exceptions;

type
  EMyNotFoundException = class(Exception)
  private
    FErrorCode: Integer;
  public
    property ErrorCode: Integer read FErrorCode;
    constructor Create(AErrorCode: Integer; const Msg: string);
  end;

  TWiRLMyNotFoundExceptionMapper = class(TWiRLExceptionMapper)
  public
    procedure HandleException(AExceptionContext: TWiRLExceptionContext); override;
  end;

implementation


{ TWiRLMyNotFoundExceptionMapper }

procedure TWiRLMyNotFoundExceptionMapper.HandleException(AExceptionContext: TWiRLExceptionContext);
const
  StatusCode = 400;
var
  MyException: EWiRLWebApplicationException;
  LJSON: TJSONObject;
begin
  inherited;
  MyException := AExceptionContext.Error as EWiRLWebApplicationException;

  LJSON := TJSONObject.Create;
  try
    LJSON.AddPair('status', MyException.Status);
    LJSON.AddPair('class', MyException.ClassName);
    LJSON.AddPair('test', MyException.Message);

    AExceptionContext.Response.StatusCode := StatusCode;
    AExceptionContext.Response.ContentType := 'application/json';
    AExceptionContext.Response.Content := TJSONHelper.ToJSON(LJSON);
  finally
    LJSON.Free;
  end;

end;

{ EMyNotFoundException }

constructor EMyNotFoundException.Create(AErrorCode: Integer; const Msg: string);
begin
  inherited Create(Msg);
  FErrorCode := AErrorCode;
end;

initialization
  //TWiRLExceptionMapperRegistry.Instance.RegisterExceptionMapper<TWiRLMyNotFoundExceptionMapper, EWiRLWebApplicationException>();

end.
