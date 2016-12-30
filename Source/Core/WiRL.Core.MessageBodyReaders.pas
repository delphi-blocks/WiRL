{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.MessageBodyReaders;

interface

uses
  System.Classes, System.SysUtils, System.Rtti, System.JSON,

  WiRL.Core.Attributes,
  WiRL.Core.Declarations,
  WiRL.Core.Request,
  WiRL.http.Accept.MediaType,
  WiRL.Core.MessageBodyReader,
  WiRL.Core.Exceptions;


type

  [Consumes(TMediaType.APPLICATION_JSON)]
  TJSONValueReader = class(TInterfacedObject, IMessageBodyReader)
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

function TJSONValueReader.ReadFrom(AParam: TRttiParameter;
  AMediaType: TMediaType; ARequest: TWiRLRequest): TValue;
begin
  Result := TJSONObject.ParseJSONValue(ARequest.Content);
end;

{ TStreamReader }

function TStreamReader.ReadFrom(AParam: TRttiParameter;
  AMediaType: TMediaType; ARequest: TWiRLRequest): TValue;
begin
  Result := ARequest.ContentStream;
end;

initialization
  TMessageBodyReaderRegistry.Instance.RegisterReader<TJSONValue>(TJSONValueReader);

  TMessageBodyReaderRegistry.Instance.RegisterReader<TStream>(TStreamReader);


end.
