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
  WiRL.Core.Classes,
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
  [Consumes(TMediaType.APPLICATION_JSON)]
  TArrayDataSetProvider = class(TMessageBodyProvider)
  private
    [Context] WiRLConfigurationNeon: TWiRLConfigurationNeon;
  public
    function ReadFrom(AType: TRttiType; AMediaType: TMediaType;
      AHeaders: IWiRLHeaders; AContentStream: TStream): TValue; override;

    procedure ReadFrom(AObject: TObject; AType: TRttitype; AMediaType: TMediaType;
	    AHeaders: IWiRLHeaders; AContentStream: TStream); override;

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
  System.JSON,
  Data.DB,
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
    LStreamWriter.Write(TDataUtils.DataSetToXML(Avalue.AsObject as TDataSet));
  finally
    LStreamWriter.Free;
  end;
end;

{ TArrayDataSetProvider }

function TArrayDataSetProvider.ReadFrom(AType: TRttiType; AMediaType: TMediaType;
  AHeaders: IWiRLHeaders; AContentStream: TStream): TValue;
begin
  raise Exception.Create('Not implemented');
end;

procedure TArrayDataSetProvider.ReadFrom(AObject: TObject; AType: TRttitype;
  AMediaType: TMediaType; AHeaders: IWiRLHeaders; AContentStream: TStream);
var
  LJson: TJSONValue;
  LBuffer: TBytes;
  LList: TDataSetList;
  LDataSet: TDataSet;
begin
  if not (AObject is TDataSetList) then
    raise EWiRLException.Create('Invalid entity');

  LList := TDataSetList(AObject);
    
  AContentStream.Position := 0;
  SetLength(LBuffer, AContentStream.Size);
  AContentStream.Read(LBuffer[0], AContentStream.Size);
  LJson := TJSONObject.ParseJSONValue(LBuffer, 0);
  try
    if not Assigned(LJson) then
      raise EWiRLException.Create('Invalid JSON');

    for LDataSet in LList do
    begin
      if not LDataSet.Active then
        LDataSet.Open;
      TNeon.JSONToObject(LDataSet, LJson.GetValue<TJSONValue>(LDataSet.Name), WiRLConfigurationNeon.GetNeonConfig);
    end;
    
  finally
    LJson.Free;
  end;
end;

procedure TArrayDataSetProvider.WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
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
  // TArrayDataSetProvider (reader and writer for TDataSet array)
  TMessageBodyReaderRegistry.Instance.RegisterReader(
    TArrayDataSetProvider,
    function(AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Boolean
    begin
      Result := Assigned(AType) and TRttiHelper.IsObjectOfType<TDataSetList>(AType);
    end,
    function(AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Integer
    begin
      Result := TMessageBodyWriterRegistry.AFFINITY_HIGH;
    end
  );

  // TArrayDataSetWriter
  TMessageBodyWriterRegistry.Instance.RegisterWriter(
    TArrayDataSetProvider,
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
