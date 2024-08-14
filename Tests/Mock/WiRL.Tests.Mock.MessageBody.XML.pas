{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Tests.Mock.MessageBody.XML;

interface

uses
  System.Classes, System.SysUtils, System.JSON, System.NetEncoding,

  System.Rtti,
  WiRL.http.Core,
  WiRL.http.Headers,
  WiRL.http.Accept.MediaType,
  WiRL.Core.Declarations,
  WiRL.Core.MessageBody.Classes,
  WiRL.Core.MessageBodyReader,
  WiRL.Core.MessageBodyWriter,
  WiRL.Core.Application,
  WiRL.Core.Registry,
  WiRL.Core.Attributes,
  WiRL.Engine.REST,
  WiRL.Tests.Mock.Classes;

type
  [Consumes(TMediaType.APPLICATION_XML)]
  [Produces(TMediaType.APPLICATION_XML)]
  TWiRLXmlObjectProvider = class(TMessageBodyProvider)
  public
    function ReadFrom(AType: TRttiType; AMediaType: TMediaType;
      AHeaders: IWiRLHeaders; AContentStream: TStream): TValue; override;

    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaders: IWiRLHeaders; AContentStream: TStream); override;
  end;

implementation

{ TWiRLXmlObjectProvider }

function TWiRLXmlObjectProvider.ReadFrom(AType: TRttiType; AMediaType: TMediaType;
  AHeaders: IWiRLHeaders; AContentStream: TStream): TValue;
begin
  raise Exception.Create('Not yet supported');
end;

procedure TWiRLXmlObjectProvider.WriteTo(const AValue: TValue;
  const AAttributes: TAttributeArray; AMediaType: TMediaType;
  AHeaders: IWiRLHeaders; AContentStream: TStream);
const
  TestXml: UTF8String = '<xml>TEST</xml>';
begin
  inherited;
  AContentStream.Write(TestXml[1], Length(TestXml));
end;

procedure RegisterMessageBodyProvider;
begin
  // TWiRLXmlObjectProvider

  TMessageBodyReaderRegistry.Instance.RegisterReader<TObject>(TWiRLXmlObjectProvider);
  TMessageBodyWriterRegistry.Instance.RegisterWriter(
    TWiRLXmlObjectProvider,
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
