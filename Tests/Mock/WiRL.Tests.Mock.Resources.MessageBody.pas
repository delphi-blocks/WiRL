{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Tests.Mock.Resources.MessageBody;

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

    [GET]
    [Path('/testobjectinurl')]
    [Produces(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8)]
    function ParseTestObjectInUrl([Consumes(TMediaType.APPLICATION_JSON)][QueryParam('person')] TestObject: TTestPersonObject): string;

    [POST]
    [Path('/readstream')]
    [Consumes(TMediaType.APPLICATION_OCTET_STREAM)]
    [Consumes(TMediaType.IMAGE_PNG)]
    [Produces(TMediaType.TEXT_PLAIN + TMediaType.WITH_CHARSET_UTF8)]
    function ReadStream([BodyParam] AStream: TStream): string;

  end;

implementation

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

function TMessageBodyResource.ParseTestObjectInUrl(
  TestObject: TTestPersonObject): string;
begin
  Result := Format('%s/%s/%d', [TestObject.ClassName, TestObject.Name, TestObject.Age]);
end;

function TMessageBodyResource.ParseTestRecord(TestRecord: TTestPersonRecord): string;
begin
  Result := Format('%s/%s/%d', ['TTestPersonRecord', TestRecord.Name, TestRecord.Age]);
end;

function TMessageBodyResource.ReadStream(AStream: TStream): string;
begin
  Result := AStream.Size.ToString;
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TMessageBodyResource>;

end.
