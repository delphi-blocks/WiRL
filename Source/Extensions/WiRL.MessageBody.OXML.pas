{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
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
  WiRL.Core.Exceptions,
  WiRL.Core.Request,
  WiRL.Core.Response;

type
  [Consumes(TMediaType.APPLICATION_XML)]
  TObjectReaderOXML = class(TInterfacedObject, IMessageBodyReader)
  private
    procedure Deserialize(const ADocument: string; AObject: TObject);
  public
    function ReadFrom(AParam: TRttiParameter;
      AMediaType: TMediaType; ARequest: TWiRLRequest): TValue;
  end;

  [Produces(TMediaType.APPLICATION_XML)]
  TObjectWriterOXML = class(TInterfacedObject, IMessageBodyWriter)
  public
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponse: TWiRLResponse);
  end;

implementation

uses
  Data.DB,
  System.TypInfo,
  WiRL.Core.Utils,
  WiRL.Rtti.Utils;

{ TObjectReaderOXML }

procedure TObjectReaderOXML.Deserialize(const ADocument: string; AObject: TObject);
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

function TObjectReaderOXML.ReadFrom(AParam: TRttiParameter;
  AMediaType: TMediaType; ARequest: TWiRLRequest): TValue;
var
  LObj: TObject;
begin
  LObj := TRttiHelper.CreateInstance(AParam.ParamType);
  Deserialize(ARequest.Content, LObj);
end;

{ TObjectWriterOXML }

procedure TObjectWriterOXML.WriteTo(const AValue: TValue;
  const AAttributes: TAttributeArray; AMediaType: TMediaType;
  AResponse: TWiRLResponse);
var
  LStreamWriter: TStreamWriter;
  LSer: TXMLRTTISerializer;
begin
  LStreamWriter := TStreamWriter.Create(AResponse.ContentStream);
  try
    LSer := TXMLRTTISerializer.Create;
    try
      LSer.UseRoot := False;
      LSer.CollectionStyle := csOXML;
      LSer.ObjectVisibility := [mvPublished];
      LSer.WriterSettings.IndentType := itIndent;
      LSer.InitStream(AResponse.ContentStream);

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
  TMessageBodyReaderRegistry.Instance.RegisterReader(TObjectReaderOXML,
    function (AType: TRttiType; AAttributes: TAttributeArray; AMediaType: TMediaType): Boolean
    begin
      Result := Assigned(AType) and
        AType.IsInstance and
        not TRttiHelper.IsObjectOfType<TDataSet>(AType, True);
    end,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
    begin
      Result := TMessageBodyReaderRegistry.AFFINITY_HIGH;
    end
  );

  TMessageBodyWriterRegistry.Instance.RegisterWriter(TObjectWriterOXML,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
    begin
      Result := Assigned(AType) and
        AType.IsInstance and
        not TRttiHelper.IsObjectOfType<TDataSet>(AType, True);
    end,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
    begin
      Result := TMessageBodyWriterRegistry.AFFINITY_HIGH;
    end
  );


end.
