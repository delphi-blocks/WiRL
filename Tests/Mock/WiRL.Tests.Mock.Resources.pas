{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Tests.Mock.Resources;

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
  [Path('/helloworld')]
  THelloWorldResource = class
  public
    [GET, Produces(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8)]
    function HelloWorld(): string;

    [ResponseBindingTest, RequestBindingTest]
    [GET, Path('/bindingfilter'), Produces(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8)]
    function BindingFilter(): string;

    [GET, Path('/echostring/{AString}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function EchoString([PathParam] AString: string): string;

    [GET, Path('/reversestring/{AString}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function ReverseString([PathParam] AString: string): string;

    [GET, Path('/params/{AOne}/{ATwo}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function Params([PathParam] AOne: string; [PathParam] ATwo: string): string;

    [GET, Path('/sum/{AOne}/{ATwo}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function Sum(
      [PathParam] AOne: Integer;
      [PathParam] ATwo: Integer): Integer;

    [GET, Path('/sumwithqueryparam?AOne={AOne}&ATwo={ATwo}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function SumWithQueryParam(
      [QueryParam] AOne: Integer;
      [QueryParam] ATwo: Integer): Integer;


    [GET, Path('/exception'), Produces(TMediaType.APPLICATION_JSON)]
    function TestException: string;

    [GET, Path('/exception401'), Produces(TMediaType.APPLICATION_JSON)]
    [Change401To400]
    function TestException401: string;

    [POST, Path('/postecho'), Produces(TMediaType.TEXT_PLAIN)]
    function PostEcho([BodyParam] AContent: string): string;

    [POST, Path('/postjson'), Produces(TMediaType.TEXT_PLAIN), Consumes(TMediaType.APPLICATION_JSON)]
    function PostJSONExample([BodyParam] AContent: TJSONObject): string;

    [POST, Path('/postbinary'), Produces(TMediaType.APPLICATION_OCTET_STREAM), Consumes(TMediaType.APPLICATION_OCTET_STREAM)]
    function PostBinary([BodyParam] AContent: TStream): TStream;
  end;

  [Path('/validator')]
  TValidatorResource = class
  public
    [GET, Produces(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8)]
    function HelloWorld(): string;

    [GET, Path('/echostring/')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function EchoString([NotNull][QueryParam('value')] const AValue: string): string;

    [GET, Path('/double/{AValue}'), Produces(TMediaType.TEXT_PLAIN)]
    function Double([PathParam][Max(50), Min(1, 'Too small')] AValue: Integer): Integer;

    [GET, Path('/buildemail?s1={s1}&s2={s2}'), Produces(TMediaType.TEXT_PLAIN)]
    function Concat([QueryParam('email'), Pattern('.+@.+\..+', 'E-Mail is not valid')] EMail: string; [QueryParam('name'), NotNull('Name required')] Name: string): string;

    [POST, Path('/json'), Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.TEXT_PLAIN)]
    function TestJson([BodyParam][NotNull, HasName] Json: TJSONObject): string;
  end;

  [Path('/messagebody')]
  TMessageBodyResource = class
  public
    [GET, Produces(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8)]
    function HelloWorld(): string;


    [POST]
    [Path('/json')]
    [Consumes(TMediaType.APPLICATION_JSON)]
    [Produces(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8)]
    function ParseJson([BodyParam] Json: TJSONObject): string;

    [GET]
    [Path('/jsonobject')]
    [Produces(TMediaType.APPLICATION_JSON)]
    function GetJsonObject(
      [QueryParam('name')] const AName: string;
      [QueryParam('age')] AAge: Integer): TTestPersonObject;

    [GET]
    [Path('/jsonrecord')]
    [Produces(TMediaType.APPLICATION_JSON)]
    function GetJsonRecord(
      [QueryParam('name')] const AName: string;
      [QueryParam('age')] AAge: Integer): TTestPersonRecord;

    [POST]
    [Path('/jsonobject')]
    [Consumes(TMediaType.APPLICATION_JSON)]
    [Produces(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8)]
    function ParseJsonObject([BodyParam] TestObject: TTestPersonObject): string;

    [POST]
    [Path('/jsonrecord')]
    [Consumes(TMediaType.APPLICATION_JSON)]
    [Produces(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8)]
    function ParseJsonRecord([BodyParam] TestRecord: TTestPersonRecord): string;

    [GET]
    [Path('/testobject')]
    [Produces(TestPersonMediaType)]
    function GetTestObject(
      [QueryParam('name')] const AName: string;
      [QueryParam('age')] AAge: Integer): TTestPersonObject;

    [POST]
    [Path('/testobject')]
    [Consumes(TestPersonMediaType)]
    [Produces(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8)]
    function ParseTestObject([BodyParam] TestObject: TTestPersonObject): string;

    [GET]
    [Path('/testrecord')]
    [Produces(TestPersonMediaType)]
    function GetTestRecord(
      [QueryParam('name')] const AName: string;
      [QueryParam('age')] AAge: Integer): TTestPersonRecord;

    [POST]
    [Path('/testrecord')]
    [Consumes(TestPersonMediaType)]
    [Produces(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8)]
    function ParseTestRecord([BodyParam] TestRecord: TTestPersonRecord): string;

  end;

implementation

{ THelloWorldResource }

function THelloWorldResource.BindingFilter: string;
begin
  Result := 'Binding filter';
end;

function THelloWorldResource.EchoString(AString: string): string;
begin
  Result := AString;
end;

function THelloWorldResource.HelloWorld: string;
begin
  Result := 'Hello, world!';
end;

function THelloWorldResource.Params(AOne, ATwo: string): string;
begin
  Result := AOne + ATwo;
end;

function THelloWorldResource.PostBinary(AContent: TStream): TStream;
begin
  Result := TMemoryStream.Create;
  Result.CopyFrom(AContent, AContent.Size);
  Result.Position := 0;
end;

function THelloWorldResource.PostEcho(AContent: string): string;
begin
  Result := AContent;
end;

function THelloWorldResource.PostJSONExample(AContent: TJSONObject): string;
begin
  Result := AContent.GetValue<string>('name');
end;

function THelloWorldResource.ReverseString(AString: string): string;
begin
  Result := System.StrUtils.ReverseString(AString);
end;

function THelloWorldResource.Sum(AOne, ATwo: Integer): Integer;
begin
  Result := AOne + ATwo;
end;

function THelloWorldResource.SumWithQueryParam(AOne, ATwo: Integer): Integer;
begin
  Result := AOne + ATwo;
end;

function THelloWorldResource.TestException: string;
begin
  raise Exception.Create('User Error Message');
end;

function THelloWorldResource.TestException401: string;
begin
  raise EWiRLNotAuthorizedException.Create('NotAuthorizedException');
end;

{ TValidatorResource }

function TValidatorResource.Concat(EMail, Name: string): string;
begin
  Result := Name + ' <' + EMail + '>';
end;

function TValidatorResource.Double(AValue: Integer): Integer;
begin
  Result := AValue * 2;
end;

function TValidatorResource.EchoString(const AValue: string): string;
begin
  Result := AValue;
end;

function TValidatorResource.HelloWorld: string;
begin
  Result := 'Hello, world!';
end;

function TValidatorResource.TestJson(Json: TJSONObject): string;
begin
  Result := Json.GetValue<string>('name');
end;

{ TMessageBodyResource }

function TMessageBodyResource.GetJsonObject(const AName: string;
  AAge: Integer): TTestPersonObject;
begin
  Result := TTestPersonObject.Create;
  try
    Result.Name := AName;
    Result.Age := AAge;
  except
    Result.Free;
    raise;
  end;
end;

function TMessageBodyResource.GetJsonRecord(const AName: string;
  AAge: Integer): TTestPersonRecord;
begin
  Result.Name := AName;
  Result.Age := AAge;
end;

function TMessageBodyResource.GetTestObject(const AName: string;
  AAge: Integer): TTestPersonObject;
begin
  Result := TTestPersonObject.Create;
  try
    Result.Name := AName;
    Result.Age := AAge;
  except
    Result.Free;
    raise;
  end;
end;

function TMessageBodyResource.GetTestRecord(const AName: string;
  AAge: Integer): TTestPersonRecord;
begin
  Result.Name := AName;
  Result.Age := AAge;
end;

function TMessageBodyResource.HelloWorld: string;
begin
  Result := 'Hello, message body!';
end;

function TMessageBodyResource.ParseJson(Json: TJSONObject): string;
begin
  Result := Json.GetValue<string>('name');
end;

function TMessageBodyResource.ParseJsonObject(TestObject: TTestPersonObject): string;
begin
  Result := Format('%s/%s/%d', [TestObject.ClassName, TestObject.Name, TestObject.Age]);
end;

function TMessageBodyResource.ParseJsonRecord(TestRecord: TTestPersonRecord): string;
begin
  Result := Format('%s/%s/%d', ['TTestPersonRecord', TestRecord.Name, TestRecord.Age]);
end;

function TMessageBodyResource.ParseTestObject(TestObject: TTestPersonObject): string;
begin
  Result := Format('%s/%s/%d', [TestObject.ClassName, TestObject.Name, TestObject.Age]);
end;

function TMessageBodyResource.ParseTestRecord(TestRecord: TTestPersonRecord): string;
begin
  Result := Format('%s/%s/%d', ['TTestPersonRecord', TestRecord.Name, TestRecord.Age]);
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<THelloWorldResource>;
  TWiRLResourceRegistry.Instance.RegisterResource<TValidatorResource>;
  TWiRLResourceRegistry.Instance.RegisterResource<TMessageBodyResource>;

end.
