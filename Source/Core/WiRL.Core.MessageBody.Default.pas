{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.MessageBody.Default;

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
  WiRL.Core.MessageBodyReader,
  WiRL.Core.MessageBody.Classes,
  WiRL.Core.Exceptions;

type
  /// <summary>
  ///   This is the <b>default</b> MessageBodyWriter for all Delphi string types.
  /// </summary>
  [Produces(TMediaType.WILDCARD)]
  TStringTypesMBWriter = class(TMessageBodyWriter)
  public
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponse: TWiRLResponse); override;
  end;

  /// <summary>
  ///   This is the <b>default</b> MessageBodyWriter for all Delphi simple types: integer,
  ///   double, etc...
  /// </summary>
  [Produces(TMediaType.TEXT_PLAIN)]
  TSimpleTypesMBWriter = class(TMessageBodyWriter)
  public
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponse: TWiRLResponse); override;
  end;

  /// <summary>
  ///   This is the <b>default</b> MessageBodyWriter for Delphi array and record types
  /// </summary>
  [Produces(TMediaType.APPLICATION_JSON)]
  TValueTypesMBWriter = class(TMessageBodyWriter)
  public
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponse: TWiRLResponse); override;
  end;

  /// <summary>
  ///   This is the standard TObject MessageBodyWriter and is using the WiRL Persistence library
  ///   (Neon Library), it's matched by the <see cref="WiRL.Core.MessageBody.Default|TObjectMBReader" />
  /// </summary>
  [Produces(TMediaType.APPLICATION_JSON)]
  TObjectMBWriter = class(TMessageBodyWriter)
  public
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponse: TWiRLResponse); override;
  end;

  /// <summary>
  ///   This is the standard TObject MessageBodyReader and is using the WiRL Persistence library
  ///   (Neon Library), it's matched by the <see cref="WiRL.Core.MessageBody.Default|TObjectMBWriter" />
  /// </summary>
  [Consumes(TMediaType.APPLICATION_JSON)]
  TObjectMBReader = class(TMessageBodyReader)
    function ReadFrom(AParam: TRttiParameter; AMediaType: TMediaType;
      ARequest: TWiRLRequest): TValue; override;
  end;

  /// <summary>
  ///   This is the standard JSONValue MessageBodyWriter using the Delphi JSON library.
  ///   It's matched by the <see cref="WiRL.Core.MessageBody.Default|TJSONValueMBReader" />
  /// </summary>
  [Produces(TMediaType.APPLICATION_JSON)]
  TJSONValueMBWriter = class(TMessageBodyWriter)
  public
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponse: TWiRLResponse); override;
    function AsObject: TObject;
  end;

  /// <summary>
  ///   This is the standard JSONValue MessageBodyReader using the Delphi JSON library. It's
  ///   matched by the <see cref="WiRL.Core.MessageBody.Default|TJSONValueMBWriter" />
  /// </summary>
  [Consumes(TMediaType.APPLICATION_JSON)]
  TJSONValueMBReader = class(TMessageBodyReader)
    function ReadFrom(AParam: TRttiParameter; AMediaType: TMediaType;
      ARequest: TWiRLRequest): TValue; override;
  end;

  /// <summary>
  ///   This is the standard TStream MessageBodyWriter using the Delphi TStream methods. It's
  ///   matched by the <see cref="WiRL.Core.MessageBody.Default|TStreamMBReader" />
  /// </summary>
  [Produces(TMediaType.APPLICATION_OCTET_STREAM)]
  [Produces(TMediaType.WILDCARD)]
  TStreamMBWriter = class(TMessageBodyWriter)
  public
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponse: TWiRLResponse); override;
  end;

  /// <summary>
  ///   This is the standard TStream MessageBodyWriter using the Delphi TStream methods.
  ///   It's matched by the <see cref="WiRL.Core.MessageBody.Default|TStreamMBWriter" />
  /// </summary>
  [Consumes(TMediaType.APPLICATION_OCTET_STREAM)]
  TStreamMBReader = class(TMessageBodyReader)
    function ReadFrom(AParam: TRttiParameter; AMediaType: TMediaType;
      ARequest: TWiRLRequest): TValue; override;
  end;

implementation

uses
  System.TypInfo,
  WiRL.Core.JSON,
  WiRL.Core.Utils,
  WiRL.Rtti.Utils,
  WiRL.Persistence.Core,
  WiRL.Persistence.JSON;

{ TStringTypesMBWriter }

procedure TStringTypesMBWriter.WriteTo(const AValue: TValue;
  const AAttributes: TAttributeArray; AMediaType: TMediaType;
  AResponse: TWiRLResponse);
var
  LStreamWriter: TStreamWriter;
  LEncoding: TEncoding;
begin
  LEncoding := AMediaType.GetDelphiEncoding;

  LStreamWriter := TStreamWriter.Create(AResponse.ContentStream, LEncoding);
  try
    case AValue.Kind of
      tkChar,
      tkWChar,
      tkString,
      tkUString,
      tkLString,
      tkWString: LStreamWriter.Write(AValue.AsType<string>);
    end;
  finally
    LStreamWriter.Free;
    LEncoding.Free;
  end;
end;

{ TObjectMBWriter }

procedure TObjectMBWriter.WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
  AMediaType: TMediaType; AResponse: TWiRLResponse);
var
  LStreamWriter: TStreamWriter;
  LJSON: TJSONValue;
begin
  LStreamWriter := TStreamWriter.Create(AResponse.ContentStream);
  try
    LJSON := TNeonMapperJSON.ObjectToJSON(AValue.AsObject, TNeonConfiguration.Default);
    try
      LStreamWriter.Write(TJSONHelper.ToJSON(LJSON));
    finally
      LJSON.Free;
    end;
  finally
    LStreamWriter.Free;
  end;
end;

{ TJSONValueMBWriter }

function TJSONValueMBWriter.AsObject: TObject;
begin
  Result := Self;
end;

procedure TJSONValueMBWriter.WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
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

{ TSimpleTypesMBWriter }

procedure TSimpleTypesMBWriter.WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
  AMediaType: TMediaType; AResponse: TWiRLResponse);
var
  LStreamWriter: TStreamWriter;
  LEncoding: TEncoding;
begin
  LEncoding := AMediaType.GetDelphiEncoding;

  LStreamWriter := TStreamWriter.Create(AResponse.ContentStream, LEncoding);
  try
    case AValue.Kind of
      tkUnknown: LStreamWriter.Write(AValue.AsType<string>);

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
    end;

  finally
    LStreamWriter.Free;
    LEncoding.Free;
  end;
end;

{ TValueTypesMBWriter }

procedure TValueTypesMBWriter.WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
  AMediaType: TMediaType; AResponse: TWiRLResponse);
var
  LStreamWriter: TStreamWriter;
  LEncoding: TEncoding;

  LJSON: TJSONValue;
begin
  LEncoding := AMediaType.GetDelphiEncoding;
  LStreamWriter := TStreamWriter.Create(AResponse.ContentStream, LEncoding);
  try
    case AValue.Kind of
      tkArray,
      tkDynArray,
      tkRecord:
      begin
        LJSON := TNeonMapperJSON.ValueToJSON(AValue, TNeonConfiguration.Default);
        try
          LStreamWriter.Write(TJSONHelper.ToJSON(LJSON));
        finally
          LJSON.Free;
        end;
      end;
    end;
  finally
    LStreamWriter.Free;
    LEncoding.Free;
  end;
end;

{ TStreamMBWriter }

procedure TStreamMBWriter.WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
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

{ TObjectMBReader }

function TObjectMBReader.ReadFrom(AParam: TRttiParameter;
  AMediaType: TMediaType; ARequest: TWiRLRequest): TValue;
begin
  Result := TNeonMapperJSON.JSONToObject(AParam.ParamType, ARequest.Content);
end;

{ TJSONValueMBReader }

function TJSONValueMBReader.ReadFrom(AParam: TRttiParameter;
  AMediaType: TMediaType; ARequest: TWiRLRequest): TValue;
begin
  Result := TJSONObject.ParseJSONValue(ARequest.Content);
end;

{ TStreamMBReader }

function TStreamMBReader.ReadFrom(AParam: TRttiParameter;
  AMediaType: TMediaType; ARequest: TWiRLRequest): TValue;
begin
  Result := ARequest.ContentStream;
end;

{ RegisterMessageBodyClasses }

procedure RegisterMessageBodyClasses;
begin
  // TStringTypesMBWriter
  TMessageBodyWriterRegistry.Instance.RegisterWriter(
    TStringTypesMBWriter,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
    begin
      Result := False;
      case AType.TypeKind of
        tkChar, tkString, tkWChar, tkLString,
        tkWString, tkUString: Result := True;
      end;
    end,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
    begin
      Result := TMessageBodyWriterRegistry.AFFINITY_VERY_LOW;
    end
  );

  // TSimpleTypesMBWriter
  TMessageBodyWriterRegistry.Instance.RegisterWriter(
    TSimpleTypesMBWriter,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
    begin
      Result := False;
      case AType.TypeKind of
        tkUnknown, tkInteger, tkChar, tkEnumeration,
        tkFloat, tkSet, tkVariant, tkInt64: Result := True;
      end;
    end,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
    begin
      Result := TMessageBodyWriterRegistry.AFFINITY_VERY_LOW;
    end
  );

  // TValueTypesMBWriter
  TMessageBodyWriterRegistry.Instance.RegisterWriter(
    TValueTypesMBWriter,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
    begin
      Result := False;
      case AType.TypeKind of
        tkArray, tkDynArray, tkRecord: Result := True;
      end;
    end,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
    begin
      Result := TMessageBodyWriterRegistry.AFFINITY_VERY_LOW;
    end
  );

  // TObjectMBWriter
  TMessageBodyWriterRegistry.Instance.RegisterWriter(
    TObjectMBWriter,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
    begin
      Result := Assigned(AType) and AType.IsInstance;
    end,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
    begin
      Result := TMessageBodyWriterRegistry.AFFINITY_VERY_LOW;
    end
  );

  // TJSONValueMBWriter
  TMessageBodyWriterRegistry.Instance.RegisterWriter<TJSONValue>(TJSONValueMBWriter);

  // TStreamMBWriter
  TMessageBodyWriterRegistry.Instance.RegisterWriter<TStream>(TStreamMBWriter, TMessageBodyWriterRegistry.AFFINITY_HIGH);

  // TObjectMBReader
  TMessageBodyReaderRegistry.Instance.RegisterReader<TObject>(TObjectMBReader);

  // TJSONValueMBReader
  TMessageBodyReaderRegistry.Instance.RegisterReader<TJSONValue>(TJSONValueMBReader);

  // TStreamMBReader
  TMessageBodyReaderRegistry.Instance.RegisterReader<TStream>(TStreamMBReader);

end;

initialization
  RegisterMessageBodyClasses;

end.
