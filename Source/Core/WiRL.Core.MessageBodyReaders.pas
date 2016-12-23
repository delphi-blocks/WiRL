unit WiRL.Core.MessageBodyReaders;

interface

uses
  System.Classes, System.SysUtils, System.Rtti, System.JSON,

  WiRL.Core.Attributes,
  WiRL.Core.Declarations,
  WiRL.Core.Request,
  WiRL.Core.MediaType,
  WiRL.Core.MessageBodyReader,
  WiRL.Core.Exceptions;

type
  [Consumes(TMediaType.APPLICATION_JSON)]
  TJSONObjectReader = class(TInterfacedObject, IMessageBodyReader)
    function ReadFrom(AParam: TRttiParameter;
      AMediaType: TMediaType; ARequest: TWiRLRequest): TValue;
  end;

  [Consumes(TMediaType.APPLICATION_JSON)]
  TJSONArrayReader = class(TInterfacedObject, IMessageBodyReader)
    function ReadFrom(AParam: TRttiParameter;
      AMediaType: TMediaType; ARequest: TWiRLRequest): TValue;
  end;

  [Consumes(TMediaType.APPLICATION_OCTET_STREAM)]
  TStreamReader = class(TInterfacedObject, IMessageBodyReader)
    function ReadFrom(AParam: TRttiParameter;
      AMediaType: TMediaType; ARequest: TWiRLRequest): TValue;
  end;


implementation

{ TJSONObjectReader }

function TJSONObjectReader.ReadFrom(AParam: TRttiParameter;
  AMediaType: TMediaType; ARequest: TWiRLRequest): TValue;
begin
  Result := TJSONObject.ParseJSONValue(ARequest.Content) as TJSONObject;
end;

{ TJSONArrayReader }

function TJSONArrayReader.ReadFrom(AParam: TRttiParameter;
  AMediaType: TMediaType; ARequest: TWiRLRequest): TValue;
begin
  Result := TJSONObject.ParseJSONValue(ARequest.Content) as TJSONArray;
end;

{ TStreamReader }

function TStreamReader.ReadFrom(AParam: TRttiParameter;
  AMediaType: TMediaType; ARequest: TWiRLRequest): TValue;
begin
  Result := ARequest.ContentStream;
end;

procedure RegisterReaders;
begin
  TMessageBodyReaderRegistry.Instance.RegisterReader<TJSONObject>(TJSONObjectReader);
  TMessageBodyReaderRegistry.Instance.RegisterReader<TJSONArray>(TJSONArrayReader);
  TMessageBodyReaderRegistry.Instance.RegisterReader<TStream>(TStreamReader);

//  TMessageBodyReaderRegistry.Instance.RegisterReader(
//    TJSONObjectReader,
//    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
//    begin
//      Result := Assigned(AType) and (AType.TypeKind = tkFloat);
//    end,
//    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
//    begin
//      Result := TWiRLMessageBodyRegistry.AFFINITY_HIGH;
//    end
//  );
end;

initialization
  RegisterReaders;

end.
