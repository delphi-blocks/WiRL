{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Data.FireDAC.Persistence;

{$I ..\..\Core\WiRL.inc}

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  FireDAC.Comp.Client, FireDAC.Stan.StorageBin, FireDAC.Stan.Error,
  FireDAC.Dats, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  WiRL.Core.JSON;

{$SCOPEDENUMS ON}
{$M+}

type
  TFDStreamCompression = (None, Over1000K, Over100K, Over10K, Always);

  TFDStreamFormat = (Raw, Zip);
  TFDStreamFormatHelper = record helper for TFDStreamFormat
  public
    function ToString: string;
    procedure FromString(AValue: string);
  end;

  TFireDACDataSetPair = TPair<string, TFDAdaptedDataSet>;
  TFireDACDataSetList = TList<TFireDACDataSetPair>;

  TFireDACDataSets = class
  private
    FDataSetList: TFireDACDataSetList;
    FCompression: TFDStreamCompression;

    function GetItems(AIndex: Integer): TFireDACDataSetPair;
  public
    constructor Create;
    destructor Destroy; override;

    function GetEnumerator: TEnumerator<TFireDACDataSetPair>;
    function GetDataSet(const AName: string): TFDAdaptedDataSet;
    function Add(const AName: string; ADataSet: TFDAdaptedDataSet): Integer;
    procedure Clear;
    procedure FreeChilds;

    property Items[AIndex: Integer]: TFireDACDataSetPair read GetItems;
    property DataSet[const AName: string]: TFDAdaptedDataSet read GetDataSet;
    property Compression: TFDStreamCompression read FCompression write FCompression;
  end;

  TFireDACPersistDataSet = class
  private
    FName: string;
    FStream: TMemoryStream;
    FFormat: TFDStreamFormat;
    constructor Create(const AName: string);
  public
    class function New(const AName: string; ACompression: TFDStreamCompression; ADataSet: TFDAdaptedDataSet): TFireDACPersistDataSet;
    destructor Destroy; override;
  published
    property Name: string read FName;
    property Format: TFDStreamFormat read FFormat;
    property Stream: TMemoryStream read FStream write FStream;
  end;

  TFireDACPersistor = class
  protected
    class function DataSetToString(ACompression: TFDStreamCompression; ADataSet: TFDAdaptedDataSet): string;
    class procedure StringToDataSet(ACompression: TFDStreamCompression; const ASource: string; ADestination: TFDAdaptedDataSet);
  public
    class function DataSetToStream(ACompression: TFDStreamCompression; ASource:
        TFDAdaptedDataSet; ADestination: TStream): TFDStreamFormat;
    class procedure StreamToDataSet(AFormat: TFDStreamFormat; ASource: TStream; ADestination: TFDAdaptedDataSet);
  end;

  TFireDACJSONPersistor = class
  private
    const JFIELD_DATA = 'data';
    const JFIELD_NAME = 'name';
    const JFIELD_ENCODE = 'encode';
    const JFIELD_FORMAT = 'format';
  private
    class procedure Base64Encode(ASource, ADestination: TStream);
    class procedure Base64Decode(ASource, ADestination: TStream);
  public
    class function DataSetToJSON(const AName: string; ACompression: TFDStreamCompression; ASource: TFDAdaptedDataSet): TJSONObject; static;
    class procedure JSONToDataSet(ASource: TJSONObject; ADestination: TFDCustomMemTable); static;

    class procedure DataSetsToJSON(ASource: TFireDACDataSets; ADestination: TJSONObject); static;
    class procedure JSONToDataSets(ASource: TJSONObject; ADestination: TFireDACDataSets); static;
  end;

implementation

uses
  {$IFDEF HAS_NET_ENCODING}
  System.NetEncoding,
  {$ELSE}
  Soap.EncdDecd,
  {$ENDIF}
  System.ZLib,
  System.JSON,
  WiRL.Core.Exceptions;

{ TFireDACDataSets }

function TFireDACDataSets.Add(const AName: string; ADataSet: TFDAdaptedDataSet): Integer;
begin
  Result := FDataSetList.Add(TFireDACDataSetPair.Create(AName, ADataSet));
end;

procedure TFireDACDataSets.Clear;
begin
  FDataSetList.Clear;
end;

constructor TFireDACDataSets.Create;
begin
  FDataSetList := TFireDACDataSetList.Create;
  FCompression := TFDStreamCompression.Over10K;
end;

destructor TFireDACDataSets.Destroy;
begin
  FDataSetList.Free;
  inherited;
end;

procedure TFireDACDataSets.FreeChilds;
var
  LPair: TFireDACDataSetPair;
begin
  for LPair in Self do
    LPair.Value.Free;
  Clear;
end;

function TFireDACDataSets.GetDataSet(const AName: string): TFDAdaptedDataSet;
var
  LIndex: Integer;
begin
  Result := nil;
  if FDataSetList <> nil then
    for LIndex := 0 to FDataSetList.Count - 1 do
      if SameText(AName, FDataSetList[LIndex].Key) then
      begin
        Result := FDataSetList[LIndex].Value;
        Break;
      end;
  if Result = nil then
    raise EWiRLServerException.Create('No DataSet found');
end;

function TFireDACDataSets.GetEnumerator: TEnumerator<TFireDACDataSetPair>;
begin
  Result := FDataSetList.GetEnumerator;
end;

function TFireDACDataSets.GetItems(AIndex: Integer): TFireDACDataSetPair;
begin
  Result := FDataSetList[AIndex];
end;

{ TFireDACJSONPersistor }

class procedure TFireDACJSONPersistor.Base64Decode(ASource, ADestination: TStream);
begin
  ASource.Seek(0, TSeekOrigin.soBeginning);
  {$IFDEF HAS_NET_ENCODING}
  TNetEncoding.Base64.Decode(ASource, ADestination);
  {$ELSE}
  DecodeStream(ASource, ADestination);
  {$ENDIF}
end;

class procedure TFireDACJSONPersistor.Base64Encode(ASource, ADestination: TStream);
begin
  ASource.Seek(0, TSeekOrigin.soBeginning);
  {$IFDEF HAS_NET_ENCODING}
  TNetEncoding.Base64.Encode(ASource, ADestination);
  {$ELSE}
  EncodeStream(ASource, ADestination);
  {$ENDIF}
end;

class procedure TFireDACJSONPersistor.DataSetsToJSON(ASource: TFireDACDataSets; ADestination: TJSONObject);
var
  LPair: TFireDACDataSetPair;
  LActive: Boolean;
  LJSONDataSet: TJSONValue;
  LDataSet: TFDAdaptedDataSet;
begin
  for LPair in  ASource do
  begin
    LDataSet := LPair.Value;
    LActive := LDataSet.Active;
    if not LActive then
      LDataSet.Active := True;
    try
      LJSONDataSet := TFireDACJSONPersistor.DataSetToJSON(LPair.Key, ASource.Compression, LDataSet);
      ADestination.AddPair(TJSONPair.Create(LPair.Key, LJSONDataSet))
    finally
      if not LActive then
        LDataSet.Active := False;
    end;
  end;
end;

class function TFireDACJSONPersistor.DataSetToJSON(const AName: string;
    ACompression: TFDStreamCompression; ASource: TFDAdaptedDataSet): TJSONObject;
var
  LFormat: TFDStreamFormat;
  LBinStream: TMemoryStream;
  LStrStream: TStringStream;
begin
  LBinStream := TMemoryStream.Create;
  LStrStream := TStringStream.Create;
  try
    LFormat := TFireDACPersistor.DataSetToStream(ACompression, ASource, LBinStream);
    TFireDACJSONPersistor.Base64Encode(LBinStream, LStrStream);

    Result := TJSONObject.Create;

    Result.AddPair(TFireDACJSONPersistor.JFIELD_NAME, AName);
    Result.AddPair(TFireDACJSONPersistor.JFIELD_FORMAT, LFormat.ToString);
    Result.AddPair(TFireDACJSONPersistor.JFIELD_ENCODE, 'base64');
    Result.AddPair(TFireDACJSONPersistor.JFIELD_DATA, LStrStream.DataString);
  finally
    LStrStream.Free;
    LBinStream.Free;
  end;
end;

class procedure TFireDACJSONPersistor.JSONToDataSet(ASource: TJSONObject; ADestination: TFDCustomMemTable);
var
  LStrStream: TStringStream;
  LBinStream: TMemoryStream;
  LFormat: TFDStreamFormat;
begin
  LFormat.FromString(ASource.GetValue<string>(TFireDACJSONPersistor.JFIELD_FORMAT));
  ADestination.Name := ASource.GetValue<string>(TFireDACJSONPersistor.JFIELD_NAME);
  LStrStream := TStringStream.Create(ASource.GetValue<string>(TFireDACJSONPersistor.JFIELD_DATA));
  LBinStream := TMemoryStream.Create;
  try
    TFireDACJSONPersistor.Base64Decode(LStrStream, LBinStream);
    TFireDACPersistor.StreamToDataSet(LFormat, LBinStream, ADestination);
  finally
    LBinStream.Free;
    LStrStream.Free;
  end;
end;

class procedure TFireDACJSONPersistor.JSONToDataSets(ASource: TJSONObject; ADestination: TFireDACDataSets);
var
  LJSONPair: TJSONPair;
  LJSONData: TJSONObject;
  LMemTable: TFDMemTable;
begin
  for LJSONPair in ASource do
  begin
    if LJSONPair.JsonValue is TJSONObject then
    begin
      LJSONData := LJSONPair.JsonValue as TJSONObject;

      LMemTable := TFDMemTable.Create(nil);
      try
        TFireDACJSONPersistor.JSONToDataSet(LJSONData, LMemTable);
        ADestination.Add(LMemTable.Name, LMemTable);
      except
        LMemTable.Free;
        raise;
      end;
    end;
  end;
end;

class function TFireDACPersistor.DataSetToStream(ACompression: TFDStreamCompression;
  ASource: TFDAdaptedDataSet; ADestination: TStream): TFDStreamFormat;
var
  LMemStream: TMemoryStream;

  function ToCompress(AStream: TStream): Boolean;
  begin
    Result := False;

    case ACompression of
      TFDStreamCompression.None: Result := False;

      TFDStreamCompression.Over10K:
      begin
        if AStream.Size > (1024 * 10) then
          Result := True;
      end;

      TFDStreamCompression.Over100K:
      begin
        if AStream.Size > (1024 * 100) then
          Result := True;
      end;

      TFDStreamCompression.Over1000K:
      begin
        if AStream.Size > (1024 * 1000) then
          Result := True;
      end;

      TFDStreamCompression.Always: Result := True;
    end;
  end;

  procedure DoCompress(ASource, ADestination: TStream);
  var
    LCompressor: TZCompressionStream;
  begin
    ASource.Seek(0, TSeekOrigin.soBeginning);

    LCompressor := TZCompressionStream.Create(clDefault, ADestination);
    try
      LCompressor.CopyFrom(ASource, ASource.Size);
    finally
      LCompressor.Free;
    end;
  end;

  procedure DoCopy(ASource, ADestination: TStream);
  begin
    ASource.Seek(0, TSeekOrigin.soBeginning);
    ADestination.CopyFrom(ASource, ASource.Size);
  end;

begin
  LMemStream := TMemoryStream.Create;
  try
    ASource.SaveToStream(LMemStream, TFDStorageFormat.sfBinary);
    LMemStream.Seek(0, TSeekOrigin.soBeginning);

    if ToCompress(LMemStream) then
    begin
      Result := TFDStreamFormat.Zip;
      DoCompress(LMemStream, ADestination);
    end
    else
    begin
      Result := TFDStreamFormat.Raw;
      DoCopy(LMemStream, ADestination);
    end;

  finally
    LMemStream.Free;
  end;
end;

class function TFireDACPersistor.DataSetToString(ACompression: TFDStreamCompression;
    ADataSet: TFDAdaptedDataSet): string;
var
  LStringStream: TStringStream;
begin
  LStringStream := TStringStream.Create;
  try
    DataSetToStream(ACompression, ADataSet, LStringStream);
    Result := LStringStream.DataString;
  finally
    LStringStream.Free;
  end;
end;

class procedure TFireDACPersistor.StreamToDataSet(AFormat: TFDStreamFormat;
    ASource: TStream; ADestination: TFDAdaptedDataSet);
var
  LMemStream: TMemoryStream;

  procedure DoDecompress(ASource, ADestination: TStream);
  var
    Unzipper: TZDecompressionStream;
  begin
    ASource.Seek(0, TSeekOrigin.soBeginning);
    Unzipper := TZDecompressionStream.Create(ASource);
    try
      ADestination.CopyFrom(Unzipper, Unzipper.Size);
    finally
      Unzipper.Free;
    end;
  end;
begin
  if AFormat = TFDStreamFormat.Zip then
  begin
    LMemStream := TMemoryStream.Create;
    try
      DoDecompress(ASource, LMemStream);
      LMemStream.Seek(Longint(0), soFromBeginning);
      ADestination.LoadFromStream(LMemStream, TFDStorageFormat.sfBinary);
    finally
      LMemStream.Free;
    end;
  end
  else
  begin
    ASource.Seek(0, TSeekOrigin.soBeginning);
    ADestination.LoadFromStream(ASource, TFDStorageFormat.sfBinary);
  end;
end;

class procedure TFireDACPersistor.StringToDataSet(ACompression: TFDStreamCompression;
  const ASource: string; ADestination: TFDAdaptedDataSet);
begin

end;

constructor TFireDACPersistDataSet.Create(const AName: string);
begin
  FName := AName;
  FStream := TMemoryStream.Create;
end;

destructor TFireDACPersistDataSet.Destroy;
begin
  FStream.Free;
  inherited;
end;

class function TFireDACPersistDataSet.New(const AName: string;
  ACompression: TFDStreamCompression;
  ADataSet: TFDAdaptedDataSet): TFireDACPersistDataSet;
begin
  Result := TFireDACPersistDataSet.Create(AName);
  try
    Result.FFormat := TFireDACPersistor.DataSetToStream(ACompression, ADataSet, Result.FStream);
  except
    FreeAndNil(Result);
  end;
end;

{ TFDStreamFormatHelper }

procedure TFDStreamFormatHelper.FromString(AValue: string);
begin
  if SameText(AValue, 'raw') then
    Self := TFDStreamFormat.Raw
  else if SameText(AValue, 'zip') then
    Self := TFDStreamFormat.Zip
end;

function TFDStreamFormatHelper.ToString: string;
begin
  case Self of
    TFDStreamFormat.Raw: Result := 'raw';
    TFDStreamFormat.Zip: Result := 'zip';
  end;
end;

end.
