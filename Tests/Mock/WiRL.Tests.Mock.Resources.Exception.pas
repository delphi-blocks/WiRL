{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Tests.Mock.Resources.Exception;

interface

uses
  System.Classes, System.SysUtils, System.StrUtils, System.JSON,

  WiRL.http.Accept.MediaType,
  WiRL.Core.Attributes,
  WiRL.Core.MessageBody.Default,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.Core.Registry,
  WiRL.Core.Exceptions,
  WiRL.Core.Validators,
  WiRL.Tests.Mock.Filters, WiRL.Tests.Mock.Validators, WiRL.Tests.Mock.Classes;

type
  [Path('/exception')]
  TExceptionMapperResource = class
  public
    [GET]
    [Produces(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8)]
    function HelloWorld(): string;

    [GET]
    [Path('basic')]
    [Produces(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8)]
    function BasicException(): string;

    [GET]
    [Path('customnotfound')]
    [Produces(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8)]
    function CustomNotFoundException(): string;
  end;


implementation

{ TExceptionMapperResource }

function TExceptionMapperResource.BasicException: string;
begin
  raise Exception.Create('Error Message');
end;

function TExceptionMapperResource.CustomNotFoundException: string;
begin
  raise EMyNotFoundException.Create(123, 'Test');
end;

function TExceptionMapperResource.HelloWorld: string;
begin
  Result := 'Hello, exception!';
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TExceptionMapperResource>;

end.
