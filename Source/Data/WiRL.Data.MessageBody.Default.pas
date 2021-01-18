{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Data.MessageBody.Default;

interface

uses
  System.Classes, System.SysUtils, System.Rtti,
  WiRL.Core.Attributes,
  WiRL.http.Core,
  WiRL.http.Headers,
  WiRL.http.Response,
  WiRL.Core.Declarations,
  WiRL.http.Accept.MediaType,
  WiRL.Core.MessageBody.Classes,
  WiRL.Core.MessageBodyReader,
  WiRL.Core.MessageBodyWriter,
  WiRL.Configuration.Neon;

type
  [Produces(TMediaType.APPLICATION_JSON)]
  TArrayDataSetWriter = class(TMessageBodyWriter)
  private
    [Context] WiRLConfigurationNeon: TWiRLConfigurationNeon;
  public
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaderFields: IWiRLHeaders; AContentStream: TStream); override;
  end;

  [Produces(TMediaType.APPLICATION_XML)]
  TDataSetWriterXML = class(TMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaderFields: IWiRLHeaders; AContentStream: TStream); override;
  end;

  [Produces(TMediaType.TEXT_CSV)]
  TDataSetWriterCSV = class(TMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaderFields: IWiRLHeaders; AContentStream: TStream); override;
  end;

implementation

uses
  Data.DB, Datasnap.DBClient,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  WiRL.Core.JSON,
  WiRL.Data.Utils,
  WiRL.Rtti.Utils;

{ TDataSetWriterXML }

procedure TDataSetWriterXML.WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaderFields: IWiRLHeaders; AContentStream: TStream);
var
  LStreamWriter: TStreamWriter;
begin
  LStreamWriter := TStreamWriter.Create(AContentStream);
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

procedure TArrayDataSetWriter.WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaderFields: IWiRLHeaders; AContentStream: TStream);
var
  LStreamWriter: TStreamWriter;
  LResult: TJSONObject;
  LData: TArray<TDataSet>;
  LCurrent: TDataSet;
begin
  LStreamWriter := TStreamWriter.Create(AContentStream);
  try
    LData := AValue.AsType<TArray<TDataSet>>;
    LResult := TJSONObject.Create;
    try
      { TODO -opaolo -c : LCurrent.Name can be empty and producing an JSON error 30/05/2017 16:26:09 }
      for LCurrent in LData do
        LResult.AddPair(LCurrent.Name, TNeon.ObjectToJSON(LCurrent, WiRLConfigurationNeon.GetNeonConfig));

      LStreamWriter.Write(TJSONHelper.ToJSON(LResult));
    finally
      LResult.Free;
    end;
  finally
    LStreamWriter.Free;
  end;
end;

{ TDataSetWriterCSV }

procedure TDataSetWriterCSV.WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaderFields: IWiRLHeaders; AContentStream: TStream);
var
  LStreamWriter: TStreamWriter;
begin
  LStreamWriter := TStreamWriter.Create(AContentStream);
  try
    LStreamWriter.Write(TDataUtils.DataSetToCSV(Avalue.AsObject as TDataSet));
  finally
    LStreamWriter.Free;
  end;
end;

{ RegisterMessageBodyClasses }

procedure RegisterMessageBodyClasses;
begin

  // TArrayDataSetWriter
  TMessageBodyWriterRegistry.Instance.RegisterWriter(
    TArrayDataSetWriter,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Boolean
    begin
      Result := Assigned(AType) and TRttiHelper.IsDynamicArrayOf<TDataSet>(AType); // and AMediaType = application/json
    end,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Integer
    begin
      Result := TMessageBodyWriterRegistry.AFFINITY_LOW
    end
  );

  // TDataSetWriterXML
  TMessageBodyWriterRegistry.Instance.RegisterWriter(
    TDataSetWriterXML,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Boolean
    begin
      Result := Assigned(AType) and TRttiHelper.IsObjectOfType<TDataSet>(AType); // and AMediaType = application/xml
    end,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Integer
    begin
      Result := TMessageBodyWriterRegistry.AFFINITY_LOW;
    end
  );

  // TDataSetWriterCSV
  TMessageBodyWriterRegistry.Instance.RegisterWriter(
    TDataSetWriterCSV,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Boolean
    begin
      Result := Assigned(AType) and TRttiHelper.IsObjectOfType<TDataSet>(AType); // and AMediaType = application/xml
    end,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Integer
    begin
      Result := TMessageBodyWriterRegistry.AFFINITY_LOW;
    end
  );

end;

initialization
  RegisterMessageBodyClasses;

end.
