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
  WiRL.http.Core,
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
  TWiRLStringWriter = class(TMessageBodyWriter)
  public
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaderFields: TWiRLHeaderList; AContentStream: TStream); override;
  end;

  /// <summary>
  ///   This is the <b>default</b> MessageBodyWriter for all Delphi simple types: integer,
  ///   double, etc...
  /// </summary>
  [Produces(TMediaType.TEXT_PLAIN)]
  TWiRLSimpleTypesWriter = class(TMessageBodyWriter)
  public
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaderFields: TWiRLHeaderList; AContentStream: TStream); override;
  end;

  /// <summary>
  ///   This is the <b>default</b> MessageBodyProvider for Delphi array and record types
  /// </summary>
  [Consumes(TMediaType.APPLICATION_JSON)]
  [Produces(TMediaType.APPLICATION_JSON)]
  [Produces(TMediaType.APPLICATION_JAVASCRIPT)]
  TWiRLValueTypesProvider = class(TMessageBodyProvider)
  public
    function ReadFrom(AParam: TRttiParameter; AMediaType: TMediaType;
      AHeaderFields: TWiRLHeaderList; AContentStream: TStream): TValue; override;

    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaderFields: TWiRLHeaderList; AContentStream: TStream); override;
  end;

  /// <summary>
  ///   This is the standard TObject MessageBodyReader/Writer and is using the WiRL Persistence library
  ///   (Neon Library).
  /// </summary>
  [Consumes(TMediaType.APPLICATION_JSON)]
  [Produces(TMediaType.APPLICATION_JSON)]
  [Produces(TMediaType.APPLICATION_JAVASCRIPT)]
  TWiRLObjectProvider = class(TMessageBodyProvider)
  public
    function ReadFrom(AParam: TRttiParameter; AMediaType: TMediaType;
      AHeaderFields: TWiRLHeaderList; AContentStream: TStream): TValue; override;

    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaderFields: TWiRLHeaderList; AContentStream: TStream); override;
  end;

  /// <summary>
  ///   This is the standard JSONValue MessageBodyReader/Writer using the Delphi JSON library.
  /// </summary>
  [Consumes(TMediaType.APPLICATION_JSON)]
  [Produces(TMediaType.APPLICATION_JSON)]
  [Produces(TMediaType.APPLICATION_JAVASCRIPT)]
  TWiRLJSONValueProvider = class(TMessageBodyProvider)
  public
    function ReadFrom(AParam: TRttiParameter; AMediaType: TMediaType;
      AHeaderFields: TWiRLHeaderList; AContentStream: TStream): TValue; override;

    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaderFields: TWiRLHeaderList; AContentStream: TStream); override;

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
      AHeaderFields: TWiRLHeaderList; AContentStream: TStream): TValue; override;

    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaderFields: TWiRLHeaderList; AContentStream: TStream); override;
  end;

implementation

uses
  System.TypInfo,
  WiRL.Core.JSON,
  WiRL.Core.Utils,
  WiRL.Rtti.Utils,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON;

{ TWiRLStringWriter }

procedure TWiRLStringWriter.WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaderFields: TWiRLHeaderList; AContentStream: TStream);
var
  LStreamWriter: TStreamWriter;
  LEncoding: TEncoding;
begin
  LEncoding := AMediaType.GetDelphiEncoding;

  LStreamWriter := TStreamWriter.Create(AContentStream, LEncoding);
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

function TWiRLJSONValueProvider.ReadFrom(AParam: TRttiParameter; AMediaType: TMediaType;
      AHeaderFields: TWiRLHeaderList; AContentStream: TStream): TValue;
begin
  Result := TJSONObject.ParseJSONValue(ContentStreamToString(AMediaType.Charset, AContentStream));
end;

procedure TWiRLJSONValueProvider.WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaderFields: TWiRLHeaderList; AContentStream: TStream);
var
  LJSONValue: TJSONValue;
begin
  LJSONValue := AValue.AsObject as TJSONValue;
  if Assigned(LJSONValue) then
  begin
    if AMediaType.IsType(TMediaType.APPLICATION_JAVASCRIPT) then
      WriteJSONPToStream(LJSONValue, AContentStream)
    else
      WriteJSONToStream(LJSONValue, AContentStream);
  end;
end;

{ TWiRLSimpleTypesWriter }

procedure TWiRLSimpleTypesWriter.WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaderFields: TWiRLHeaderList; AContentStream: TStream);
var
  LStreamWriter: TStreamWriter;
  LEncoding: TEncoding;
begin
  LEncoding := AMediaType.GetDelphiEncoding;

  LStreamWriter := TStreamWriter.Create(AContentStream, LEncoding);
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

{ TWiRLValueTypesProvider }

function TWiRLValueTypesProvider.ReadFrom(AParam: TRttiParameter; AMediaType: TMediaType;
      AHeaderFields: TWiRLHeaderList; AContentStream: TStream): TValue;
var
  LDes: TNeonDeserializerJSON;
  LJSON: TJSONValue;
  LValue: TValue;
begin
  LDes := TNeonDeserializerJSON.Create(WiRLApplication.SerializerConfig);
  try
    LJSON := TJSONObject.ParseJSONValue(ContentStreamToString(AMediaType.Charset, AContentStream));
    try
      TValue.Make(nil, AParam.ParamType.Handle, LValue);
      Result := LDes.JSONToTValue(LJSON, AParam.ParamType, LValue);
    finally
      LJSON.Free;
    end;
  finally
    LDes.Free;
  end;
end;

procedure TWiRLValueTypesProvider.WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaderFields: TWiRLHeaderList; AContentStream: TStream);
var
  LJSON: TJSONValue;
begin
  case AValue.Kind of
    tkArray,
    tkDynArray,
    tkRecord:
    begin
      LJSON := TNeon.ValueToJSON(AValue, WiRLApplication.SerializerConfig);
      try
        if AMediaType.IsType(TMediaType.APPLICATION_JAVASCRIPT) then
          WriteJSONPToStream(LJSON, AContentStream)
        else
          WriteJSONToStream(LJSON, AContentStream);
      finally
        LJSON.Free;
      end;
    end;
  end;
end;

{ TWiRLObjectProvider }

function TWiRLObjectProvider.ReadFrom(AParam: TRttiParameter; AMediaType: TMediaType;
      AHeaderFields: TWiRLHeaderList; AContentStream: TStream): TValue;
begin
  Result := TNeon.JSONToObject(AParam.ParamType, ContentStreamToString(AMediaType.Charset, AContentStream), WiRLApplication.SerializerConfig);
end;

procedure TWiRLObjectProvider.WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaderFields: TWiRLHeaderList; AContentStream: TStream);
var
  LJSON: TJSONValue;
begin
  LJSON := TNeon.ObjectToJSON(AValue.AsObject, WiRLApplication.SerializerConfig);
  try
    if AMediaType.IsType(TMediaType.APPLICATION_JAVASCRIPT) then
      WriteJSONPToStream(LJSON, AContentStream)
    else
      WriteJSONToStream(LJSON, AContentStream);
  finally
    LJSON.Free;
  end;
end;

{ TWiRLStreamProvider }

function TWiRLStreamProvider.ReadFrom(AParam: TRttiParameter; AMediaType: TMediaType;
      AHeaderFields: TWiRLHeaderList; AContentStream: TStream): TValue;
begin
  Result := TWiRLStreamWrapper.Create(AContentStream);
end;

procedure TWiRLStreamProvider.WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaderFields: TWiRLHeaderList; AContentStream: TStream);
var
  LStream: TStream;
begin
  if (not AValue.IsEmpty) and AValue.IsInstanceOf(TStream) then
  begin
    LStream := AValue.AsObject as TStream;
    if Assigned(LStream) then
      AContentStream.CopyFrom(LStream, LStream.Size);
  end;
end;

{ RegisterMessageBodyClasses }

procedure RegisterMessageBodyClasses;
begin
  // TWiRLStringWriter
  TMessageBodyWriterRegistry.Instance.RegisterWriter(
    TWiRLStringWriter,
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

  // TWiRLSimpleTypesWriter
  TMessageBodyWriterRegistry.Instance.RegisterWriter(
    TWiRLSimpleTypesWriter,
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

  // TWiRLValueTypesProvider
  TMessageBodyReaderRegistry.Instance.RegisterReader(
    TWiRLValueTypesProvider,
    function(AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Boolean
    begin
      Result := False;
      case AType.TypeKind of
        //tkArray, tkDynArray:
        tkRecord: Result := True;
      end;
    end,
    function(AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Integer
    begin
      Result := TMessageBodyWriterRegistry.AFFINITY_LOW;
    end
  );

  TMessageBodyWriterRegistry.Instance.RegisterWriter(
    TWiRLValueTypesProvider,
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
