{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.MessageBody.DJSON;

interface

uses
  System.Classes, System.SysUtils, System.Rtti, System.Generics.Collections,

  // DJSON
  DJSON,
  DJSON.Params,
  DJSON.Factory,

  // WiRL
  WiRL.Core.Attributes, 
  WiRL.Core.Declarations,
  WiRL.http.Accept.MediaType,
  WiRL.Core.MessageBodyReader,
  WiRL.Core.MessageBodyWriter,
  WiRL.Core.Exceptions,
  WiRL.http.Request,
  WiRL.http.Response;

type
  TDJSONParams = reference to procedure (AParams: IdjParams);

  TMessageBodyDJSON = class(TInterfacedObject)
  private
    class var FParams: TDJSONParams;
  protected
    function GetDJSONParams: IdjParams;
  public
    class property Params: TDJSONParams read FParams write FParams;
  end;

  [Consumes(TMediaType.APPLICATION_JSON)]
  TObjectReaderDJSON = class(TMessageBodyDJSON, IMessageBodyReader)
  public
    function ReadFrom(AType: TRttiType; AMediaType: TMediaType; ARequest: TWiRLRequest): TValue;
  end;

  [Produces(TMediaType.APPLICATION_JSON)]
  TObjectWriterDJSON = class(TMessageBodyDJSON, IMessageBodyWriter)
  public
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponse: TWiRLResponse);
  end;

implementation

uses
  System.TypInfo,
  Data.DB,

  WiRL.Core.JSON,
  WiRL.Core.Utils,
  WiRL.Rtti.Utils;

{ TMessageBodyDJSON }  

function TMessageBodyDJSON.GetDJSONParams: IdjParams;
begin
  Result := TdjFactory.NewParams;

  if Assigned(TMessageBodyDJSON.FParams) then
    TMessageBodyDJSON.FParams(Result)
  else
  begin
    Result.Engine := eDelphiStream;
    Result.SerializationMode := smJavaScript;
    Result.SerializationType := stProperties;
    Result.TypeAnnotations := False;
    Result.TimeUTC := False;
  end;
end;

{ TObjectReaderDJSON }

function TObjectReaderDJSON.ReadFrom(AType: TRttiType; AMediaType: TMediaType; ARequest: TWiRLRequest): TValue;
begin
  Result := dj.FromJSON(ARequest.Content).Params(GetDJSONParams).&ToValue(AType.Handle);
end;

{ TObjectWriterDJSON }

procedure TObjectWriterDJSON.WriteTo(const AValue: TValue;
  const AAttributes: TAttributeArray; AMediaType: TMediaType;
  AResponse: TWiRLResponse);
var
  LStreamWriter: TStreamWriter;
  LRes: string;
begin
  LStreamWriter := TStreamWriter.Create(AResponse.ContentStream);
  try
    LRes := dj.From(AValue.AsObject, GetDJSONParams).ToJson;
    LStreamWriter.Write(LRes);
  finally
    LStreamWriter.Free;
  end;
end;

initialization
  TMessageBodyReaderRegistry.Instance.RegisterReader(TObjectReaderDJSON,
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

  TMessageBodyWriterRegistry.Instance.RegisterWriter(TObjectWriterDJSON,
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
