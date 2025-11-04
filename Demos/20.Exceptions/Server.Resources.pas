{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2025 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Resources;

interface

uses
  System.SysUtils, System.Classes,

  WiRL.Core.Registry,
  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  WiRL.Core.MessageBody.Default,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.Core.Exceptions,

  Server.Exceptions;

type
  [Path('demo')]
  TDemoResource = class
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SampleText: string;

    [Path('error')]
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function GetCustomException: string;

    [Path('details')]
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function GetErrorDetails: string;

    [Path('exception')]
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function GetException: string;

    [Path('divbyzero')]
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function GetDivByZero: string;
  end;

implementation

{ TDemoResource }

function TDemoResource.GetCustomException: string;
begin
  //raise EMyNotFoundException.Create(102, 'Exception Message');
  try
    raise Exception.Create('Simple Exception');
  except
    on E: Exception do
    begin
      Exception.RaiseOuterException(EWiRLNotFoundException.Create('Not Found'));
    end;
  end;
end;

function TDemoResource.GetDivByZero: string;
begin
  Result := IntToStr(5 div StrToInt('0'));
end;

function TDemoResource.GetErrorDetails: string;
begin
  raise EWiRLWebApplicationException.Create('Invalid input', 400,
    TValuesUtil.MakeValueArray(
      //Pair.S('unit', 'Test.pas'), // Valore string
      Pair.N('line', 150), // Valore numerico
      Pair.B('failure', True), // Valore booleano
      Pair.D('timestamp', Now) // Valore numerico
    )
  );
end;

function TDemoResource.GetException: string;
begin
  raise Exception.Create('Test');

  raise Exception.Create('Error Message');
end;

function TDemoResource.SampleText: string;
begin
  Result := 'Hello, World!'
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TDemoResource>;

end.
