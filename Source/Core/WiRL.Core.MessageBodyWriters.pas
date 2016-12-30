{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.MessageBodyWriters;

interface

uses
  System.Classes, System.SysUtils, System.Rtti,

  WiRL.Core.Classes,
  WiRL.Core.Attributes,
  WiRL.Core.Declarations,
  WiRL.Core.Request,
  WiRL.Core.Response,
  WiRL.http.Accept.MediaType,
  WiRL.Core.MessageBodyWriter,
  WiRL.Core.Exceptions;

type
  [Produces(TMediaType.APPLICATION_JSON)]
  TDateTimeWriter = class(TInterfacedObject, IMessageBodyWriter)
  public
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponse: TWiRLResponse);
  end;

  [Produces(TMediaType.APPLICATION_JSON)]
  TObjectWriter = class(TInterfacedObject, IMessageBodyWriter)
  private
    //[Context] Request: TWiRLRequest;
  public
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponse: TWiRLResponse);
  end;

  [Produces(TMediaType.APPLICATION_JSON)]
  TJSONValueWriter = class(TInterfacedObject, IMessageBodyWriter)
  public
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponse: TWiRLResponse);
    function AsObject: TObject;
  end;

  [Produces(TMediaType.WILDCARD)]
  TWildCardMediaTypeWriter = class(TInterfacedObject, IMessageBodyWriter)
  public
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponse: TWiRLResponse);
  end;

  [Produces(TMediaType.APPLICATION_OCTET_STREAM)]
  [Produces(TMediaType.WILDCARD)]
  TStreamValueWriter = class(TInterfacedObject, IMessageBodyWriter)
  public
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponse: TWiRLResponse);
  end;

implementation

uses
  System.TypInfo,
  WiRL.Core.JSON,
  WiRL.Core.Utils,
  WiRL.Rtti.Utils,
  WiRL.Core.Serialization;


{ TDateTimeWriter }

procedure TDateTimeWriter.WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
  AMediaType: TMediaType; AResponse: TWiRLResponse);
var
  LStreamWriter: TStreamWriter;
  LObj: TJSONObject;
begin
  LStreamWriter := TStreamWriter.Create(AResponse.ContentStream);
  try
    LObj := TJSONObject.Create(TJSONPair.Create('result', TJSONHelper.DateToJSON(AValue.AsExtended, False)));
    try
      LStreamWriter.Write(TJSONHelper.ToJSON(LObj));
    finally
      LObj.Free;
    end;
  finally
    LStreamWriter.Free;
  end;
end;

{ TObjectWriter }

procedure TObjectWriter.WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
  AMediaType: TMediaType; AResponse: TWiRLResponse);
var
  LStreamWriter: TStreamWriter;
  LObj: TJSONObject;
begin
  LStreamWriter := TStreamWriter.Create(AResponse.ContentStream);
  try
    LObj := TJSONSerializer.ObjectToJSON(AValue.AsObject);
    try
      LStreamWriter.Write(TJSONHelper.ToJSON(LObj));
    finally
      LObj.Free;
    end;
  finally
    LStreamWriter.Free;
  end;
end;

{ TJSONValueWriter }

function TJSONValueWriter.AsObject: TObject;
begin
  Result := Self;
end;

procedure TJSONValueWriter.WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
  AMediaType: TMediaType; AResponse: TWiRLResponse);
var
  LStreamWriter: TStreamWriter;
  LJSONValue: TJSONValue;
begin
  LStreamWriter := TStreamWriter.Create(AResponse.ContentStream);
  try
    LJSONValue := AValue.AsObject as TJSONValue;
    if Assigned(LJSONValue) then
      LStreamWriter.Write(TJSONHelper.ToJSON(LJSONValue));
  finally
    LStreamWriter.Free;
  end;
end;

{ TWildCardMediaTypeWriter }

procedure TWildCardMediaTypeWriter.WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
  AMediaType: TMediaType; AResponse: TWiRLResponse);
var
  LWriter: IMessageBodyWriter;
  LObj: TObject;
  LWriterClass: TClass;
  LStreamWriter: TStreamWriter;
  LEncoding: TEncoding;
begin
  if AValue.IsObject then
  begin
    if AValue.AsObject is TJSONValue then
      LWriterClass := TJSONValueWriter
    else if AValue.AsObject is TStream then
      LWriterClass := TStreamValueWriter
    else
      LWriterClass := TObjectWriter;

    LObj := LWriterClass.Create;
    if Supports(LObj, IMessageBodyWriter, LWriter) then
    begin
      try
        LWriter.WriteTo(AValue, AAttributes, AMediaType, AResponse);
      finally
        LWriter := nil;
      end;
    end
    else
      LObj.Free;
  end
  else
  begin
    if AMediaType.Charset = TMediaType.CHARSET_UTF8 then
      LEncoding := TUTF8EncodingNoBOM.Create
    else if AMediaType.Charset = TMediaType.CHARSET_UTF16BE then
      LEncoding := TUnicodeBEEncodingNoBOM.Create
    else if AMediaType.Charset = TMediaType.CHARSET_UTF16LE then
      LEncoding := TUnicodeLEEncodingNoBOM.Create
    else if AMediaType.Charset = TMediaType.CHARSET_UTF16 then
      LEncoding := TUnicodeLEEncodingNoBOM.Create
    else
      LEncoding := TMBCSEncoding.Create;

    LStreamWriter := TStreamWriter.Create(AResponse.ContentStream, LEncoding);
    try
      case AValue.Kind of
        tkUnknown: LStreamWriter.Write(AValue.AsType<string>);

        tkChar,
        tkWChar,
        tkString,
        tkUString,
        tkLString,
        tkWString: LStreamWriter.Write(AValue.AsType<string>);

        tkVariant: LStreamWriter.Write(AValue.ToString);

        tkInteger: LStreamWriter.Write(AValue.AsType<Integer>);

        tkInt64: LStreamWriter.Write(AValue.AsType<Int64>);

        tkEnumeration: AValue.ToString;

        tkFloat:
        begin
          if (AValue.TypeInfo = System.TypeInfo(TDateTime)) or
             (AValue.TypeInfo = System.TypeInfo(TDate)) or
             (AValue.TypeInfo = System.TypeInfo(TTime)) then
            LStreamWriter.Write(TJSONHelper.DateToJSON(AValue.AsType<TDateTime>))
          else
            LStreamWriter.Write(AValue.AsType<Currency>);
        end;

        tkSet: LStreamWriter.Write(AValue.ToString);

        tkArray,
        tkRecord,
        tkInterface,
        tkDynArray:
          raise EWiRLNotImplementedException.Create(
            'Resource''s returned type not supported',
            Self.ClassName, 'WriteTo'
          );
		  
      end;

    finally
      LStreamWriter.Free;
      LEncoding.Free;
    end;
  end;
end;

{ TStreamValueWriter }

procedure TStreamValueWriter.WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
  AMediaType: TMediaType; AResponse: TWiRLResponse);
var
  LStream: TStream;
begin
  if (not AValue.IsEmpty) and AValue.IsInstanceOf(TStream) then
  begin
    LStream := AValue.AsObject as TStream;
    if Assigned(LStream) then
      AResponse.ContentStream.CopyFrom(LStream, LStream.Size);
  end;
end;

procedure RegisterWriters;
begin
  TWiRLMessageBodyRegistry.Instance.RegisterWriter(
    TDatetimeWriter,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
    begin
      Result := Assigned(AType) and (AType.TypeKind = tkFloat);
    end,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
    begin
      Result := TWiRLMessageBodyRegistry.AFFINITY_HIGH;
    end
  );

  TWiRLMessageBodyRegistry.Instance.RegisterWriter<TJSONValue>(TJSONValueWriter);

  TWiRLMessageBodyRegistry.Instance.RegisterWriter(
    TStreamValueWriter,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
    begin
      Result := Assigned(AType) and TRttiHelper.IsObjectOfType<TStream>(AType, True);
    end,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
    begin
      Result := TWiRLMessageBodyRegistry.AFFINITY_HIGH;
    end
  );

  TWiRLMessageBodyRegistry.Instance.RegisterWriter(
    TObjectWriter,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
    begin
      Result := Assigned(AType) and AType.IsInstance;
    end,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
    begin
      Result := TWiRLMessageBodyRegistry.AFFINITY_VERY_LOW;
    end
  );

  TWiRLMessageBodyRegistry.Instance.RegisterWriter(
    TWildCardMediaTypeWriter,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
    begin
      Result := True;
    end,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
    begin
      Result := TWiRLMessageBodyRegistry.AFFINITY_VERY_LOW;
    end
  );
end;

initialization
  RegisterWriters;

end.
