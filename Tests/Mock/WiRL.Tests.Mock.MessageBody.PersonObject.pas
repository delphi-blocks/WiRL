{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Tests.Mock.MessageBody.PersonObject;

interface

uses
  System.Classes, System.SysUtils, System.JSON, System.NetEncoding,

  System.Rtti,
  WiRL.http.Core,
  WiRL.http.Headers,
  WiRL.Core.Declarations,
  WiRL.Core.MessageBody.Classes,
  WiRL.Core.MessageBodyReader,
  WiRL.Core.MessageBodyWriter,

  WiRL.Engine.REST,
  WiRL.Core.Application,
  WiRL.Core.Registry,
  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType, WiRL.Tests.Mock.Classes;

type
  [Consumes(TestPersonMediaType)]
  [Produces(TestPersonMediaType)]
  TWiRLTestPersonObjectProvider = class(TMessageBodyProvider)
  public
    function ReadFrom(AType: TRttiType; AMediaType: TMediaType;
      AHeaders: IWiRLHeaders; AContentStream: TStream): TValue; override;

    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaders: IWiRLHeaders; AContentStream: TStream); override;
  end;

implementation

{ TWiRLTestPersonObjectProvider }

function TWiRLTestPersonObjectProvider.ReadFrom(AType: TRttiType; AMediaType: TMediaType;
  AHeaders: IWiRLHeaders; AContentStream: TStream): TValue;
var
  LContent: string;
  LTestPerson: TTestPersonObject;
  LStringList: TStringList;
begin
  LTestPerson := TTestPersonObject.Create;
  try
    LContent := ContentStreamToString(AMediaType.Charset, AContentStream);
    LStringList := TStringList.Create;
    try
      LStringList.Text := LContent;
      LTestPerson.Name := LStringList.Values['Name'];
      LTestPerson.Age := StrToIntDef(LStringList.Values['Age'], 0);
    finally
      LStringList.Free;
    end;
  except
    LTestPerson.Free;
    raise;
  end;
  Result := LTestPerson;
end;

procedure TWiRLTestPersonObjectProvider.WriteTo(const AValue: TValue;
  const AAttributes: TAttributeArray; AMediaType: TMediaType;
  AHeaders: IWiRLHeaders; AContentStream: TStream);
var
  LContent: TBytes;
  LStringList: TStringList;
  LTestPerson: TTestPersonObject;
begin
  inherited;
  if (not AValue.IsObject) or (not (AValue.AsObject is TTestPersonObject)) then
    raise Exception.Create('Object not supported');

  LTestPerson := TTestPersonObject(AValue.AsObject);

  LStringList := TStringList.Create;
  try
    LStringList.Values['Name'] := LTestPerson.Name;
    LStringList.Values['Age'] := IntToStr(LTestPerson.Age);
    LContent := TEncoding.UTF8.GetBytes(LStringList.Text);
  finally
    LStringList.Free;
  end;

  AContentStream.Write(LContent[0], Length(LContent));
end;

procedure RegisterMessageBodyProvider;
begin
  TMessageBodyReaderRegistry.Instance.RegisterReader<TTestPersonObject>(TWiRLTestPersonObjectProvider);

  TMessageBodyWriterRegistry.Instance.RegisterWriter(
    TWiRLTestPersonObjectProvider,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Boolean
    begin
      Result := Assigned(AType) and AType.IsInstance;
    end,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Integer
    begin
      Result := TMessageBodyWriterRegistry.AFFINITY_LOW;
    end
  );

end;

initialization
  RegisterMessageBodyProvider;

end.
