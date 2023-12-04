{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
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
  WiRL.Core.Exceptions, Server.Config;

type
  TTestObj = class
  private
    FStringValue: string;
    FIntValue: Integer;
  public
    property StringValue: string read FStringValue write FStringValue;
    property IntValue: Integer read FIntValue write FIntValue;
  end;

  [Path('demo')]
  TDemoResource = class
  private
    [Context] TestConfig: TTestConfig;
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SampleText: string;
    [Path('config')]
    [GET, Produces(TMediaType.APPLICATION_JSON)]
    function Config: TTestObj;
    [Path('date')]
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function GetCurrentDate: TDateTime;
  end;

implementation

{ TDemoResource }

function TDemoResource.Config: TTestObj;
begin
  Result := TTestObj.Create;
  Result.StringValue := ExtractFileName(ParamStr(0));
  Result.IntValue := Random(1000);
end;

function TDemoResource.GetCurrentDate: TDateTime;
begin
  Result := Now;
end;

function TDemoResource.SampleText: string;
begin
  Result :=
    'Hello config injection!' + sLineBreak + sLineBreak +
    'TestConfig.StringValue: ' + TestConfig.StringValue + sLineBreak +
    'TestConfig.IntValue: ' + TestConfig.IntValue.ToString;
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TDemoResource>;

end.
