{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2024 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.MessageBody.SingleRecord;

interface

uses
  System.Classes, System.SysUtils, System.JSON,
  System.Types, System.RTTI, System.TypInfo, Data.DB,
  WiRL.Core.MessageBody.Classes,
  WiRL.Core.Declarations,
  WiRL.Core.Attributes,
  WiRL.Core.MessageBodyWriter,
  WiRL.Rtti.Utils,
  WiRL.http.Headers,
  WiRL.http.Accept.MediaType;

type
  [Produces(TMediaType.APPLICATION_JSON)]
  TWiRLRecordWriter = class(TMessageBodyProvider)
  public
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaders: IWiRLHeaders; AContentStream: TStream); override;
  end;

implementation

uses
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  Neon.Core.Utils;

function HasSingleRecordAttribute(const AAttributes: TAttributeArray): Boolean;
var
  LAttr: TCustomAttribute;
begin
  Result := False;
  for LAttr in AAttributes do
  begin
    if LAttr is SingleRecordAttribute then
      Exit(True);
  end;
end;

{ TWiRLRecordWriter }

procedure TWiRLRecordWriter.WriteTo(const AValue: TValue;
  const AAttributes: TAttributeArray; AMediaType: TMediaType;
  AHeaders: IWiRLHeaders; AContentStream: TStream);
var
  LDataSet: TDataSet;
  LJSONObject: TJSONObject;
  LBytes: TBytes;
begin
  inherited;
  LDataSet := AValue.AsType<TDataSet>;
  LDataSet.Open;

  LJSONObject := TDataSetUtils.RecordToJSONObject(LDataSet, True);
  try
    LBytes := TEncoding.UTF8.GetBytes(LJSONObject.ToJSON);
    AContentStream.Write(LBytes[0], Length(LBytes));
  finally
    LJSONObject.Free;
  end;
end;

procedure RegisterMessageBodyClasses;
begin
  TMessageBodyWriterRegistry.Instance.RegisterWriter(
    TWiRLRecordWriter,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Boolean
    begin
      Result := Assigned(AType) and TRttiUtils.IsObjectOfType<TDataSet>(AType) and HasSingleRecordAttribute(AAttributes);
    end,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Integer
    begin
      Result := TMessageBodyWriterRegistry.AFFINITY_HIGH;
    end
  );

end;

initialization

  RegisterMessageBodyClasses;

end.
