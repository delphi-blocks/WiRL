{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2023 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Data.FireDAC.MessageBody.Default;

interface

uses
  System.Classes, System.SysUtils, System.Rtti, Data.DB, FireDAC.Stan.Intf,

  WiRL.Core.Attributes,
  WiRL.Core.Declarations,
  WiRL.http.Accept.MediaType,
  WiRL.http.Core,
  WiRL.http.Headers,
//  WiRL.http.Request,
//  WiRL.http.Response,
  WiRL.Core.Classes,
  WiRL.Core.MessageBody.Classes,
  WiRL.Core.MessageBodyReader,
  WiRL.Core.MessageBodyWriter,
  WiRL.Core.Utils,
  WiRL.Data.Utils,
  WiRL.Data.FireDAC,
  WiRL.Data.FireDAC.Persistence,
  WiRL.Data.FireDAC.Utils;

type
  /// <summary>
  ///   This is the standard provider (MessageBodyReader/Writer) for the
  ///   TFDAdaptedDataSet class (the base class of the FireDAC datasets).
  /// </summary>
  /// <remarks>
  ///   This Provider supports JSON, XML, BIN data format
  /// </remarks>
  [Consumes(TMediaType.APPLICATION_XML), Consumes(TMediaType.APPLICATION_JSON), Consumes(TMediaType.APPLICATION_OCTET_STREAM)]
  [Produces(TMediaType.APPLICATION_XML), Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_OCTET_STREAM)]
  TWiRLFireDACAdaptedDataSetProvider = class(TMessageBodyProvider)
  private
    function GetStorageFormat(AMediaType: TMediaType): TFDStorageFormat;
  public
    function ReadFrom(AType: TRttitype; AMediaType: TMediaType;
      AHeaders: IWiRLHeaders; AContentStream: TStream): TValue; overload; override;

    procedure ReadFrom(AObject: TObject; AType: TRttitype; AMediaType: TMediaType;
	    AHeaders: IWiRLHeaders; AContentStream: TStream); overload; override;

    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaders: IWiRLHeaders; AContentStream: TStream); override;
  end;

  /// <summary>
  ///   This is the provider (MessageBodyReader/Writer) for an array of FireDAC datasets
  ///   (TFDAdaptedDataSet)
  /// </summary>
  /// <remarks>
  ///   The MessageBodyWriter can write any TFDAdaptedDataSet but the reader reads into
  ///   an array of TFDMemTable
  /// </remarks>
  [Consumes(TMediaType.APPLICATION_JSON)]
  [Produces(TMediaType.APPLICATION_JSON)]
  TWiRLFireDACDataSetArrayProvider = class(TMessageBodyProvider)
  public
    function ReadFrom(AType: TRttitype; AMediaType: TMediaType;
      AHeaders: IWiRLHeaders; AContentStream: TStream): TValue; override;

    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaders: IWiRLHeaders; AContentStream: TStream); override;
  end;

  /// <summary>
  ///   This is the standard provider (MessageBodyReader/Writer) for the
  ///   TFDAdaptedDataSets class
  /// </summary>
  /// <remarks>
  ///   The TFDAdaptedDataSets it's a list of TFDDataSets serialized in JSON
  /// </remarks>
  [Consumes(TMediaType.APPLICATION_JSON)]
  [Produces(TMediaType.APPLICATION_JSON)]
  TWiRLFireDACDataSetsProvider = class(TMessageBodyProvider)
  public
    function ReadFrom(AType: TRttitype; AMediaType: TMediaType;
      AHeaders: IWiRLHeaders; AContentStream: TStream): TValue; override;

    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaders: IWiRLHeaders; AContentStream: TStream); override;
  end;

implementation

uses
  FireDAC.Comp.Client,
  FireDAC.Stan.StorageBIN, FireDAC.Stan.StorageJSON, FireDAC.Stan.StorageXML,

  WiRL.Core.Exceptions,
  WiRL.Core.JSON,
  WiRL.Rtti.Utils;

{ TWiRLFireDACDataSetArrayProvider }

function TWiRLFireDACDataSetArrayProvider.ReadFrom(AType: TRttitype; AMediaType: TMediaType;
      AHeaders: IWiRLHeaders; AContentStream: TStream): TValue;
var
  LJSON: TJSONObject;
  LStreamReader: TStreamReader;
  LDataSets: TFireDACDataSets;
  LDataPair: TFireDACDataSetPair;
  LDSArray: TArray<TFDMemTable>;
  LMemTable: TFDMemTable;
begin
  AContentStream.Position := soFromBeginning;
  LStreamReader := TStreamReader.Create(AContentStream);
  try
    LJSON := TJSONObject.ParseJSONValue(LStreamReader.ReadToEnd) as TJSONObject;
    try
      LDataSets := TFireDACDataSets.Create;
      TFireDACJSONPersistor.JSONToDataSets(LJSON, LDataSets);

      SetLength(LDSArray, 0);
      for LDataPair in LDataSets do
      begin
        LMemTable := TFDMemTable.Create(nil);
        SetLength(LDSArray, Length(LDSArray) + 1);
        LDSArray[Length(LDSArray) - 1] := LMemTable;
      end;

      Result := TValue.From<TArray<TFDMemTable>>(LDSArray);
    finally
      LJSON.Free;
    end;
  finally
    LStreamReader.Free;
  end;
end;

procedure TWiRLFireDACDataSetArrayProvider.WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaders: IWiRLHeaders; AContentStream: TStream);
var
  LIndex: Integer;
  LStreamWriter: TStreamWriter;
  LDataSetList: TFireDACDataSets;
  LCurrent: TFDAdaptedDataSet;
  LResult: TJSONObject;
begin
  Assert(AValue.IsArray);

  LStreamWriter := TStreamWriter.Create(AContentStream);
  try
    LResult := TJSONObject.Create;
    try
      LDataSetList := TFireDACDataSets.Create;
      LDataSetList.Compression := TFDStreamCompression.Over10K;
      try
        for LIndex := 0 to AValue.GetArrayLength - 1 do
        begin
          LCurrent := AValue.GetArrayElement(LIndex).AsObject as TFDADaptedDataSet;
          LDataSetList.Add(LCurrent.Name, LCurrent);
        end;
        TFireDACJSONPersistor.DataSetsToJSON(LDataSetList, LResult);

        LStreamWriter.Write(TJSONHelper.ToJSON(LResult));
      finally
        LDataSetList.Free;
      end;
    finally
      LResult.Free;
    end;
  finally
    LStreamWriter.Free;
  end;
end;

{ TWiRLFireDACDataSetsProvider }

function TWiRLFireDACDataSetsProvider.ReadFrom(AType: TRttitype; AMediaType: TMediaType;
      AHeaders: IWiRLHeaders; AContentStream: TStream): TValue;
var
  LJSON: TJSONObject;
  LStreamReader: TStreamReader;
  LDataSets: TFireDACDataSets;
begin
  AContentStream.Position := soFromBeginning;
  LStreamReader := TStreamReader.Create(AContentStream);
  try
    LJSON := TJSONObject.ParseJSONValue(LStreamReader.ReadToEnd) as TJSONObject;
    try
      LDataSets := TFireDACDataSets.Create;
      TFireDACJSONPersistor.JSONToDataSets(LJSON, LDataSets);
      Result := TValue.From<TFireDACDataSets>(LDataSets);
    finally
      LJSON.Free;
    end;
  finally
    LStreamReader.Free;
  end;
end;

procedure TWiRLFireDACDataSetsProvider.WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaders: IWiRLHeaders; AContentStream: TStream);
var
  LJSON: TJSONObject;
  LStreamWriter: TStreamWriter;
  LDataSets: TFireDACDataSets;
begin
  LDataSets := AValue.AsType<TFireDACDataSets>;

  LJSON := TJSONObject.Create;
  try
    TFireDACJSONPersistor.DataSetsToJSON(LDataSets, LJSON);
    LStreamWriter := TStreamWriter.Create(AContentStream);
    try
      LStreamWriter.Write(TJSONHelper.ToJSON(LJSON));
    finally
      LStreamWriter.Free;
    end;
  finally
    LJSON.Free;
  end;
end;

{ TWiRLFireDACAdaptedDataSetProvider }

function TWiRLFireDACAdaptedDataSetProvider.GetStorageFormat(AMediaType: TMediaType): TFDStorageFormat;
begin
  if AMediaType.Matches(TMediaType.APPLICATION_XML) then
    Result := sfXML
  else if AMediaType.Matches(TMediaType.APPLICATION_JSON) then
    Result := sfJSON
  else if AMediaType.Matches(TMediaType.APPLICATION_OCTET_STREAM) then
    Result := sfBinary
  else
    raise EWiRLUnsupportedMediaTypeException.Create(
      Format('Unsupported media type [%s]', [AMediaType.ToString]),
      Self.ClassName, 'WriteTo'
    );
end;

function TWiRLFireDACAdaptedDataSetProvider.ReadFrom(AType: TRttitype; AMediaType: TMediaType;
      AHeaders: IWiRLHeaders; AContentStream: TStream): TValue;
var
  LDataSet: TFDMemTable;
  LStorageFormat: TFDStorageFormat;
begin
  LDataSet := TFDMemTable.Create(nil);
  try
    LStorageFormat := GetStorageFormat(AMediaType);
    LDataSet.LoadFromStream(AContentStream, LStorageFormat);
    Result := TValue.From<TFDMemTable>(LDataSet);
  except
    LDataSet.Free;
  end;
end;

procedure TWiRLFireDACAdaptedDataSetProvider.ReadFrom(AObject: TObject; AType: TRttitype;
      AMediaType: TMediaType; AHeaders: IWiRLHeaders; AContentStream: TStream);
var
  LDataSet: TFDMemTable;
  LStorageFormat: TFDStorageFormat;
begin
  if not (AObject is TFDMemTable) then
    raise EWiRLWebApplicationException.Create('Deserialization only to TFDMemTable', 501);

  LDataSet := AObject as TFDMemTable;
  LStorageFormat := GetStorageFormat(AMediaType);
  AContentStream.Position := 0;
  LDataSet.LoadFromStream(AContentStream, LStorageFormat);
end;

procedure TWiRLFireDACAdaptedDataSetProvider.WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaders: IWiRLHeaders; AContentStream: TStream);
var
  LDataSet: TFDAdaptedDataSet;
  LStorageFormat: TFDStorageFormat;
begin
  LDataSet := AValue.AsType<TFDAdaptedDataSet>;
  LDataSet.Active := True;
  LStorageFormat := GetStorageFormat(AMediaType);
  LDataSet.SaveToStream(AContentStream, LStorageFormat);
end;

{ RegisterMessageBodyClasses }

procedure RegisterMessageBodyClasses;
begin
  // TWiRLFireDACAdaptedDataSetProvider
  TMessageBodyReaderRegistry.Instance.RegisterReader<TFDAdaptedDataSet>(TWiRLFireDACAdaptedDataSetProvider);
  TMessageBodyWriterRegistry.Instance.RegisterWriter<TFDAdaptedDataSet>(TWiRLFireDACAdaptedDataSetProvider, TMessageBodyWriterRegistry.AFFINITY_HIGH);

  // TWiRLFireDACDataSetsProvider
  TMessageBodyReaderRegistry.Instance.RegisterReader<TFireDACDataSets>(TWiRLFireDACDataSetsProvider);
  TMessageBodyWriterRegistry.Instance.RegisterWriter<TFireDACDataSets>(TWiRLFireDACDataSetsProvider);

  // TWiRLFireDACDataSetArrayProvider
  TMessageBodyReaderRegistry.Instance.RegisterReader(
  TWiRLFireDACDataSetArrayProvider,
    function(AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Boolean
    begin
      Result := Assigned(AType) and TRttiHelper.IsDynamicArrayOf<TFDMemTable>(AType);
      // and AMediaType = application/json;dialect=FireDAC
    end,
    function(AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Integer
    begin
      Result := TMessageBodyWriterRegistry.AFFINITY_HIGH
    end
  );
  TMessageBodyWriterRegistry.Instance.RegisterWriter(
    TWiRLFireDACDataSetArrayProvider,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Boolean
    begin
      Result := Assigned(AType) and TRttiHelper.IsDynamicArrayOf<TFDAdaptedDataSet>(AType);
      // and AMediaType = application/json;dialect=FireDAC
    end,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Integer
    begin
      Result := TMessageBodyWriterRegistry.AFFINITY_HIGH
    end
  );
end;

initialization
  RegisterMessageBodyClasses;

end.

