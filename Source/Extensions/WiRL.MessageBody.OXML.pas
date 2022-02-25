{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.MessageBody.OXML;

interface

uses
  System.Classes, System.SysUtils, System.Rtti, System.Generics.Collections,

  // OXml
  OXmlRTTISerialize,
  OXmlUtils,

  // WiRL
  WiRL.Core.Attributes,
  WiRL.Core.Declarations,
  WiRL.http.Accept.MediaType,
  WiRL.Core.MessageBodyReader,
  WiRL.Core.MessageBodyWriter,
  WiRL.Core.MessageBody.Classes,
  WiRL.Core.Exceptions,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.http.Headers,
  WiRL.Configuration.Neon;

type
  [Consumes(TMediaType.APPLICATION_XML)]
  [Produces(TMediaType.APPLICATION_XML)]
  TWiRLXMLProvider = class(TMessageBodyProvider)
  private
    [Context] FConfigurationNeon: TWiRLConfigurationNeon;
  private
    procedure Deserialize(const ADocument: string; AObject: TObject);
  public
    function ReadFrom(AType: TRttiType; AMediaType: TMediaType;
      AHeaders: IWiRLHeaders; AContentStream: TStream): TValue; override;

    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaders: IWiRLHeaders; AContentStream: TStream); override;
  end;

implementation

uses
  Data.DB,
  System.TypInfo,
  WiRL.Core.Utils,
  WiRL.Rtti.Utils,
  WiRL.http.Core;

{ TWiRLXMLProvider }

procedure TWiRLXMLProvider.Deserialize(const ADocument: string; AObject: TObject);
var
  LDeserializer : TXMLRTTIDeserializer;
  LClassName: string;
begin
  LDeserializer := TXMLRTTIDeserializer.Create;
  try
    LDeserializer.UseRoot := False;
    LDeserializer.InitXML(ADocument);
    LDeserializer.ReadObjectInfo(LClassName);
    LDeserializer.ReadObject(AObject);
  finally
    LDeserializer.Free;
  end;
end;

function TWiRLXMLProvider.ReadFrom(AType: TRttiType; AMediaType: TMediaType; AHeaders: IWiRLHeaders;
  AContentStream: TStream): TValue;
var
  LObj: TObject;
begin
  LObj := TRttiHelper.CreateInstance(AType);
  Deserialize(ContentStreamToString(AMediaType.Charset, AContentStream), LObj);
end;

procedure TWiRLXMLProvider.WriteTo(const AValue: TValue; const AAttributes: TAttributeArray; AMediaType: TMediaType;
  AHeaders: IWiRLHeaders; AContentStream: TStream);
var
  LStreamWriter: TStreamWriter;
  LSer: TXMLRTTISerializer;
begin
  LStreamWriter := TStreamWriter.Create(AContentStream);
  try
    LSer := TXMLRTTISerializer.Create;
    try
      LSer.UseRoot := False;
      LSer.CollectionStyle := csOXML;
      LSer.ObjectVisibility := FConfigurationNeon.Visibility;
      LSer.WriterSettings.IndentType := itIndent;
      LSer.InitStream(AContentStream);

      LSer.WriteObject(AValue.AsObject);
      LSer.ReleaseDocument;

    finally
      LSer.Free;
    end;
  finally
    LStreamWriter.Free;
  end;
end;

initialization
  TMessageBodyReaderRegistry.Instance.RegisterReader(TWiRLXMLProvider,
    function(AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Boolean
    begin
      Result := Assigned(AType) and
        AType.IsInstance and
        not TRttiHelper.IsObjectOfType<TDataSet>(AType, True);
    end,
    function(AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Integer
    begin
      Result := TMessageBodyReaderRegistry.AFFINITY_HIGH;
    end
  );

  TMessageBodyWriterRegistry.Instance.RegisterWriter(TWiRLXMLProvider,
    function(AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Boolean
    begin
      Result := Assigned(AType) and
        AType.IsInstance and
        not TRttiHelper.IsObjectOfType<TDataSet>(AType, True);
    end,
    function(AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Integer
    begin
      Result := TMessageBodyWriterRegistry.AFFINITY_HIGH;
    end
  );

end.
