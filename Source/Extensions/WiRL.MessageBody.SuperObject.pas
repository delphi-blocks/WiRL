{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.MessageBody.SuperObject;

interface

uses
  System.Classes, System.SysUtils, System.Rtti, System.TypInfo,

  WiRL.Core.Classes,
  WiRL.Core.Attributes,
  WiRL.Core.Declarations,
  WiRL.http.Core,
  WiRL.Rtti.Utils,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.http.Headers,
  WiRL.http.Accept.MediaType,
  WiRL.Core.MessageBodyWriter,
  WiRL.Core.MessageBodyReader,
  WiRL.Core.MessageBody.Classes,
  WiRL.Core.Exceptions,

  SuperObject;

type
  [Consumes(TMediaType.APPLICATION_JSON)]
  [Produces(TMediaType.APPLICATION_JSON)]
  TWiRLSuperObjectProvider = class(TMessageBodyProvider)
  public
    function ReadFrom(AType: TRttiType; AMediaType: TMediaType;
      AHeaders: IWiRLHeaders; AContentStream: TStream): TValue; override;

    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaders: IWiRLHeaders; AContentStream: TStream); override;
  end;

implementation

{ TWiRLSuperObjectProvider }

function TWiRLSuperObjectProvider.ReadFrom(AType: TRttiType; AMediaType: TMediaType;
      AHeaders: IWiRLHeaders; AContentStream: TStream): TValue;
begin
  Result := TValue.From<ISuperObject>(TSuperObject.ParseStream(AContentStream, True));
end;

procedure TWiRLSuperObjectProvider.WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaders: IWiRLHeaders; AContentStream: TStream);
var
  LStreamWriter: TStreamWriter;
  LSuperObject: ISuperObject;
begin
  LStreamWriter := TStreamWriter.Create(AContentStream);
  try
    if not Supports(AValue.AsInterface, ISuperObject, LSuperObject) then
      raise EWiRLServerException.Create('Invalid type cast');
    if Assigned(LSuperObject) then
      LStreamWriter.Write(LSuperObject.AsJSon());
  finally
    LStreamWriter.Free;
  end;
end;

procedure RegisterMessageBodyClasses;
begin
  TMessageBodyReaderRegistry.Instance.RegisterReader(TWiRLSuperObjectProvider, ISuperObject,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Integer
    begin
      Result := TMessageBodyWriterRegistry.AFFINITY_HIGH;
    end
  );

  TMessageBodyWriterRegistry.Instance.RegisterWriter(TWiRLSuperObjectProvider, ISuperObject,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Integer
    begin
      Result := TMessageBodyWriterRegistry.AFFINITY_HIGH;
    end
  );
end;


initialization
  RegisterMessageBodyClasses;

end.
