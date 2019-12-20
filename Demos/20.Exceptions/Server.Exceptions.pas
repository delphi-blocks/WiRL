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

procedure TWiRLMyNotFoundExceptionMapper.HandleException(
  AExceptionContext: TWiRLExceptionContext);
const
  StatusCode = 400;
var
  MyException: EMyNotFoundException;
  LJSON: TJSONObject;
begin
  inherited;
  MyException := AExceptionContext.Error as EMyNotFoundException;

  LJSON := TJSONObject.Create;
  try
    EWiRLWebApplicationException.ExceptionToJSON(MyException, StatusCode, LJSON);
    LJSON.AddPair(TJSONPair.Create('ErrorCode', TJSONNumber.Create(MyException.ErrorCode)));

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

  TWiRLExceptionMapperRegistry.Instance.RegisterExceptionMapper<TWiRLMyNotFoundExceptionMapper, EMyNotFoundException>();

end.
