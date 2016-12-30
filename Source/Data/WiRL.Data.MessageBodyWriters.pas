{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Data.MessageBodyWriters;

interface

uses
  System.Classes, System.SysUtils, System.Rtti,
  WiRL.Core.Attributes,
  WiRL.Core.Response,
  WiRL.Core.Declarations,
  WiRL.http.Accept.MediaType,
  WiRL.Core.MessageBodyWriter;

type
  [Produces(TMediaType.APPLICATION_JSON)]
  TDataSetWriterJSON = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponse: TWiRLResponse);
  end;

  [Produces(TMediaType.APPLICATION_JSON)]
  TArrayDataSetWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponse: TWiRLResponse);
  end;

  [Produces(TMediaType.APPLICATION_XML)]
  TDataSetWriterXML = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponse: TWiRLResponse);
  end;

  [Produces(TMediaType.TEXT_CSV)]
  TDataSetWriterCSV = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponse: TWiRLResponse);
  end;

implementation

uses
  Data.DB, Datasnap.DBClient,
  WiRL.Core.JSON,
  WiRL.Data.Utils,
  WiRL.Rtti.Utils;

{ TDataSetWriterJSON }

procedure TDataSetWriterJSON.WriteTo(const AValue: TValue; const AAttributes:
    TAttributeArray; AMediaType: TMediaType; AResponse: TWiRLResponse);
var
  LStreamWriter: TStreamWriter;
  LResult: TJSONArray;
begin
  LStreamWriter := TStreamWriter.Create(AResponse.ContentStream);
  try
    LResult := TDataUtils.DataSetToJSONArray(AValue.AsObject as TDataSet);
    try
      LStreamWriter.Write(TJSONHelper.ToJSON(LResult));
    finally
      LResult.Free;
    end;
  finally
    LStreamWriter.Free;
  end;
end;

{ TDataSetWriterXML }

procedure TDataSetWriterXML.WriteTo(const AValue: TValue; const AAttributes:
    TAttributeArray; AMediaType: TMediaType; AResponse: TWiRLResponse);
var
  LStreamWriter: TStreamWriter;
begin
  LStreamWriter := TStreamWriter.Create(AResponse.ContentStream);
  try
    if AValue.AsObject is TClientDataSet then // CDS
      LStreamWriter.Write(TClientDataSet(AValue.AsObject).XMLData)
    else // default
      LStreamWriter.Write(TDataUtils.DataSetToXML(Avalue.AsObject as TDataSet));
  finally
    LStreamWriter.Free;
  end;
end;

{ TArrayDataSetWriter }

procedure TArrayDataSetWriter.WriteTo(const AValue: TValue; const AAttributes:
    TAttributeArray; AMediaType: TMediaType; AResponse: TWiRLResponse);
var
  LStreamWriter: TStreamWriter;
  LResult: TJSONObject;
  LData: TArray<TDataSet>;
  LCurrent: TDataSet;
begin
  LStreamWriter := TStreamWriter.Create(AResponse.ContentStream);
  try
    LData := AValue.AsType<TArray<TDataSet>>;
    LResult := TJSONObject.Create;
    try
      for LCurrent in LData do
        LResult.AddPair(LCurrent.Name, TDataUtils.DataSetToJSONArray(LCurrent));

      LStreamWriter.Write(TJSONHelper.ToJSON(LResult));
    finally
      LResult.Free;
    end;
  finally
    LStreamWriter.Free;
  end;
end;

{ TDataSetWriterCSV }

procedure TDataSetWriterCSV.WriteTo(const AValue: TValue; const AAttributes:
    TAttributeArray; AMediaType: TMediaType; AResponse: TWiRLResponse);
var
  LStreamWriter: TStreamWriter;
begin
  LStreamWriter := TStreamWriter.Create(AResponse.ContentStream);
  try
    LStreamWriter.Write(TDataUtils.DataSetToCSV(Avalue.AsObject as TDataSet));
  finally
    LStreamWriter.Free;
  end;
end;

procedure RegisterWriters;
begin
  TWiRLMessageBodyRegistry.Instance.RegisterWriter(
    TDataSetWriterJSON
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := Assigned(AType) and  TRttiHelper.IsObjectOfType<TDataSet>(AType); // and AMediaType = application/json
      end
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
      begin
        Result := TWiRLMessageBodyRegistry.AFFINITY_LOW;
      end
  );

  TWiRLMessageBodyRegistry.Instance.RegisterWriter(
    TArrayDataSetWriter
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := Assigned(AType) and TRttiHelper.IsDynamicArrayOf<TDataSet>(AType); // and AMediaType = application/json
      end
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
      begin
        Result := TWiRLMessageBodyRegistry.AFFINITY_LOW
      end
  );

  TWiRLMessageBodyRegistry.Instance.RegisterWriter(
    TDataSetWriterXML
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := Assigned(AType) and TRttiHelper.IsObjectOfType<TDataSet>(AType); // and AMediaType = application/xml
      end
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
      begin
        Result := TWiRLMessageBodyRegistry.AFFINITY_LOW;
      end
  );

  TWiRLMessageBodyRegistry.Instance.RegisterWriter(
    TDataSetWriterCSV
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := Assigned(AType) and TRttiHelper.IsObjectOfType<TDataSet>(AType); // and AMediaType = application/xml
      end
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
      begin
        Result := TWiRLMessageBodyRegistry.AFFINITY_LOW;
      end
  );

end;

initialization
  RegisterWriters;

end.
