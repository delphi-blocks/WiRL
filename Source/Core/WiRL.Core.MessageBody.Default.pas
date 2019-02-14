{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
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
  WiRL.http.Request,
  WiRL.http.Response,
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
  ///   This is the standard TObject MessageBodyReader/Writer and is using the WiRL Persistence library
  ///   (Neon Library).
  /// </summary>
  [Consumes(TMediaType.APPLICATION_JSON)]
  [Produces(TMediaType.APPLICATION_JSON)]
  TWiRLObjectProvider = class(TMessageBodyProvider)
  public
    function ReadFrom(AParam: TRttiParameter; AMediaType: TMediaType;
      ARequest: TWiRLRequest): TValue; override;

    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponse: TWiRLResponse); override;
  end;

  /// <summary>
  ///   This is the standard JSONValue MessageBodyReader/Writer using the Delphi JSON library.
  /// </summary>
  [Consumes(TMediaType.APPLICATION_JSON)]
  [Produces(TMediaType.APPLICATION_JSON)]
  TWiRLJSONValueProvider = class(TMessageBodyProvider)
  public
    function ReadFrom(AParam: TRttiParameter; AMediaType: TMediaType;
      ARequest: TWiRLRequest): TValue; override;

    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponse: TWiRLResponse); override;

    function AsObject: TObject;
  end;

  /// <summary>
  ///   This is the standard TStream MessageBodyReader/Writer using the Delphi TStream methods
  /// </summary>
  [Consumes(TMediaType.APPLICATION_OCTET_STREAM)]
  [Produces(TMediaType.APPLICATION_OCTET_STREAM), Produces(TMediaType.WILDCARD)]
  TWiRLStreamProvider = class(TMessageBodyProvider)
  public
    function ReadFrom(AParam: TRttiParameter; AMediaType: TMediaType;
      ARequest: TWiRLRequest): TValue; override;

    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponse: TWiRLResponse); override;
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

{ TWiRLJSONValueProvider }

function TWiRLJSONValueProvider.AsObject: TObject;
begin
  Result := Self;
end;

function TWiRLJSONValueProvider.ReadFrom(AParam: TRttiParameter; AMediaType:
    TMediaType; ARequest: TWiRLRequest): TValue;
begin
  Result := TJSONObject.ParseJSONValue(ARequest.Content);
end;

procedure TWiRLJSONValueProvider.WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
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

      tkEnumeration: LStreamWriter.Write(AValue.ToString);

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
        LJSON := TNeonMapperJSON.ValueToJSON(AValue, WiRLApplication.SerializerConfig);
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

{ TWiRLObjectProvider }

function TWiRLObjectProvider.ReadFrom(AParam: TRttiParameter; AMediaType:
    TMediaType; ARequest: TWiRLRequest): TValue;
begin
  Result := TNeonMapperJSON.JSONToObject(AParam.ParamType, ARequest.Content, WiRLApplication.SerializerConfig);
end;

procedure TWiRLObjectProvider.WriteTo(const AValue: TValue; const AAttributes:
    TAttributeArray; AMediaType: TMediaType; AResponse: TWiRLResponse);
var
  LStreamWriter: TStreamWriter;
  LJSON: TJSONValue;
begin
  LStreamWriter := TStreamWriter.Create(AResponse.ContentStream);
  try
    LJSON := TNeonMapperJSON.ObjectToJSON(AValue.AsObject, WiRLApplication.SerializerConfig);
    try
      LStreamWriter.Write(TJSONHelper.ToJSON(LJSON));
    finally
      LJSON.Free;
    end;
  finally
    LStreamWriter.Free;
  end;
end;

{ TWiRLStreamProvider }

function TWiRLStreamProvider.ReadFrom(AParam: TRttiParameter; AMediaType:
    TMediaType; ARequest: TWiRLRequest): TValue;
begin
  Result := ARequest.ContentStream;
end;

procedure TWiRLStreamProvider.WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
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

{ RegisterMessageBodyClasses }

procedure RegisterMessageBodyClasses;
begin
  // TStringTypesMBWriter
  TMessageBodyWriterRegistry.Instance.RegisterWriter(
    TStringTypesMBWriter,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Boolean
    begin
      Result := False;
      case AType.TypeKind of
        tkChar, tkString, tkWChar, tkLString,
        tkWString, tkUString: Result := True;
      end;
    end,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Integer
    begin
      Result := TMessageBodyWriterRegistry.AFFINITY_VERY_LOW;
    end
  );

  // TSimpleTypesMBWriter
  TMessageBodyWriterRegistry.Instance.RegisterWriter(
    TSimpleTypesMBWriter,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Boolean
    begin
      Result := False;
      case AType.TypeKind of
        tkUnknown, tkInteger, tkChar, tkEnumeration,
        tkFloat, tkSet, tkVariant, tkInt64: Result := True;
      end;
    end,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Integer
    begin
      Result := TMessageBodyWriterRegistry.AFFINITY_VERY_LOW;
    end
  );

  // TValueTypesMBWriter
  TMessageBodyWriterRegistry.Instance.RegisterWriter(
    TValueTypesMBWriter,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Boolean
    begin
      Result := False;
      case AType.TypeKind of
        tkArray, tkDynArray, tkRecord: Result := True;
      end;
    end,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Integer
    begin
      Result := TMessageBodyWriterRegistry.AFFINITY_VERY_LOW;
    end
  );

  // TWiRLObjectProvider
  TMessageBodyReaderRegistry.Instance.RegisterReader<TObject>(TWiRLObjectProvider);
  TMessageBodyWriterRegistry.Instance.RegisterWriter(
    TWiRLObjectProvider,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Boolean
    begin
      Result := Assigned(AType) and AType.IsInstance;
    end,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Integer
    begin
      Result := TMessageBodyWriterRegistry.AFFINITY_VERY_LOW;
    end
  );

  // TWiRLJSONValueProvider
  TMessageBodyReaderRegistry.Instance.RegisterReader<TJSONValue>(TWiRLJSONValueProvider);
  TMessageBodyWriterRegistry.Instance.RegisterWriter<TJSONValue>(TWiRLJSONValueProvider);

  // TWiRLStreamProvider
  TMessageBodyReaderRegistry.Instance.RegisterReader<TStream>(TWiRLStreamProvider);
  TMessageBodyWriterRegistry.Instance.RegisterWriter<TStream>(TWiRLStreamProvider, TMessageBodyWriterRegistry.AFFINITY_HIGH);

end;

initialization
  RegisterMessageBodyClasses;

end.
