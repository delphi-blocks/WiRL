{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Tests.Mock.Resources.ContextInjection;

interface

uses
  System.Classes, System.SysUtils, System.StrUtils, System.JSON,

  WiRL.http.Accept.MediaType,
  WiRL.Core.Attributes,
  WiRL.Core.Application,
  WiRL.Core.MessageBody.Default,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.http.URL,
  WiRL.Core.Registry,

  WiRL.Tests.Mock.Classes;

type
  [Path('/contextinjection')]
  TContextInjectionResource = class
  private
    [Context]
    FApplication: TWiRLApplication;
    [Context]
    FRequest: TWiRLRequest;
    [Context]
    FURL: TWiRLURL;
  public
    [GET]
    [Produces(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8)]
    function HelloWorld(): string;

    [GET]
    [Path('request')]
    [Produces(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8)]
    function CheckResponseClassInjection(): string;

    [GET]
    [Path('application')]
    [Produces(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8)]
    function CheckApplicationClassInjection(): string;

    [GET]
    [Path('requestmethod')]
    [Produces(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8)]
    function CheckResponseMethodInjection([Context] ARequest: TWiRLRequest): string;

    [GET]
    [Path('person')]
    [Produces(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8)]
    function CheckPersonInjection([Context] APerson: TTestPersonObject): string;

    [GET]
    [Path('addcounter')]
    [Produces(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8)]
    function AddCounter([Context][Singleton] ACounter: TCounter): string;

    [GET]
    [Path('url')]
    [Produces(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8)]
    function CheckUrl(): string;
  end;

implementation

{ TContextInjectionResource }

function TContextInjectionResource.AddCounter(ACounter: TCounter): string;
begin
  ACounter.Inc;
  Result := ACounter.Value.ToString;
end;

function TContextInjectionResource.CheckApplicationClassInjection: string;
begin
  Result := FApplication.BasePath;
end;

function TContextInjectionResource.CheckPersonInjection(
  APerson: TTestPersonObject): string;
begin
  Result := Format('%s:%d', [APerson.Name, APerson.Age]);
end;

function TContextInjectionResource.CheckResponseClassInjection: string;
begin
  Result := FRequest.PathInfo;
end;

function TContextInjectionResource.CheckResponseMethodInjection(
  ARequest: TWiRLRequest): string;
begin
  Result := ARequest.QueryFields['value'];
end;

function TContextInjectionResource.CheckUrl: string;
begin
  Result := FURL.Path;
end;

function TContextInjectionResource.HelloWorld: string;
begin
  Result := 'Hello, context injection!';
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TContextInjectionResource>;

end.
