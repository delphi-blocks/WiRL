unit WiRL.Data.FireDAC.Updates;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, Data.DB,
  FireDAC.Comp.Client, FireDAC.Stan.StorageBin, FireDAC.Stan.Error, FireDAC.Dats,
  FireDAC.Stan.Option, FireDAC.Comp.DataSet, FireDAC.Stan.Intf, FireDAC.DApt.Intf,

  WiRL.Core.JSON,
  WiRL.Data.FireDAC.Persistence;

type
  // Only for reference
  //TFDUpdateErrorEvent = procedure (ASender: TDataSet; AException: EFDException;
    //ARow: TFDDatSRow; ARequest: TFDUpdateRequest; var AAction: TFDErrorAction) of object;

  /// <summary>
  ///   Mantains a list of errors, this class can be used in response to OnUpdateError FDDataSet events
  /// </summary>
  TFireDACUpdateErrors = class
  private
    FErrors: TStringList;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetErrorHandler(ASender: TFDMemTable); virtual;
    procedure ClearErrorHandler(ASender: TFDMemTable); virtual;
    procedure Clear;

    /// <remarks>
    ///   Same signature of TFDUpdateErrorEvent type
    /// </remarks>
    procedure UpdateError(ASender: TDataSet; AException: EFDException;
      ARow: TFDDatSRow; ARequest: TFDUpdateRequest; var AAction: TFDErrorAction); virtual;

    property Errors: TStringList read FErrors;
    property Count: Integer read GetCount;
  end;

  /// <summary>Delta list payload to apply updates.</summary>
  TFireDACApplyUpdates = class
  private
    FDataSets: TFireDACDataSets;
    FErrors: TFireDACUpdateErrors;

    function GetValue(AIndex: Integer): TFDMemTable;
    function GetValueByName(const AName: string): TFDMemTable;

    procedure CopyDataSet(ASource, ADest: TFDAdaptedDataSet);
    function InternalApplyUpdates(ADelta: TFDMemTable; AAdapter: TFDTableAdapter;
      AMaxErrors: Integer): Integer;
  public
    function ApplyUpdates(ADelta: TFDMemTable; ASelectCommand: TFDCustomCommand): Integer; overload;

    function ApplyUpdates(const AKey: string; ASelectCommand: TFDCustomCommand): Integer; overload;
    function ApplyUpdates(AIndex: Integer; ASelectCommand: TFDCustomCommand): Integer; overload;
    function ApplyUpdates(const AKey: string; AAdapter: TFDTableAdapter): Integer; overload;
    function ApplyUpdates(AIndex: Integer; AAdapter: TFDTableAdapter): Integer; overload;
  public
    constructor Create(const ADataSets: TFireDACDataSets);
    destructor Destroy; override;

    property DataSets: TFireDACDataSets read FDataSets;
    property Errors: TFireDACUpdateErrors read FErrors;
  end;


implementation

{ TFireDACUpdateErrors }

procedure TFireDACUpdateErrors.Clear;
begin
  FErrors.Clear;
end;

constructor TFireDACUpdateErrors.Create;
begin
  FErrors := TStringList.Create;
end;

destructor TFireDACUpdateErrors.Destroy;
begin
  FErrors.Free;
  inherited;
end;

procedure TFireDACUpdateErrors.SetErrorHandler(ASender: TFDMemTable);
begin
  if not Assigned(ASender.OnUpdateError) then
    ASender.OnUpdateError := UpdateError;
end;

procedure TFireDACUpdateErrors.ClearErrorHandler(ASender: TFDMemTable);
var
  LCurrentEvent: TFDUpdateErrorEvent;
begin
  LCurrentEvent := UpdateError;

  if Addr(ASender.OnUpdateError) = Addr(LCurrentEvent) then
    ASender.OnUpdateError := nil;
end;

function TFireDACUpdateErrors.GetCount: Integer;
begin
  Result := FErrors.Count;
end;

procedure TFireDACUpdateErrors.UpdateError(ASender: TDataSet; AException: EFDException;
    ARow: TFDDatSRow; ARequest: TFDUpdateRequest; var AAction: TFDErrorAction);
begin
  FErrors.Add(Format('%s %s', [AException.ClassName, AException.Message]));
end;

{ TFireDACApplyUpdates }

function TFireDACApplyUpdates.ApplyUpdates(AIndex: Integer;
  ASelectCommand: TFDCustomCommand): Integer;
var
  LDelta: TFDMemTable;
  LAdapter: TFDTableAdapter;
begin
  Assert(ASelectCommand <> nil);

  LDelta := GetValue(AIndex);

  LAdapter := TFDTableAdapter.Create(nil);
  try
    LAdapter.SelectCommand := ASelectCommand;
    Result := InternalApplyUpdates(LDelta, LAdapter, -1);
  finally
    LAdapter.Free;
  end;
end;

function TFireDACApplyUpdates.ApplyUpdates(const AKey: string;
  ASelectCommand: TFDCustomCommand): Integer;
var
  LDelta: TFDMemTable;
  LAdapter: TFDTableAdapter;
begin
  Assert(ASelectCommand <> nil);

  LDelta := GetValueByName(AKey);

  LAdapter := TFDTableAdapter.Create(nil);
  try
    LAdapter.SelectCommand := ASelectCommand;
    Result := InternalApplyUpdates(LDelta, LAdapter, -1);
  finally
    LAdapter.Free;
  end;
end;

function TFireDACApplyUpdates.ApplyUpdates(const AKey: string;
  AAdapter: TFDTableAdapter): Integer;
var
  LDelta: TFDMemTable;
begin
  LDelta := GetValueByName(AKey);
  Result := InternalApplyUpdates(LDelta, AAdapter, -1);
end;

function TFireDACApplyUpdates.ApplyUpdates(AIndex: Integer;
  AAdapter: TFDTableAdapter): Integer;
var
  LDelta: TFDMemTable;
begin
  LDelta := GetValue(AIndex);
  Result := InternalApplyUpdates(LDelta, AAdapter, -1);
end;

function TFireDACApplyUpdates.ApplyUpdates(ADelta: TFDMemTable;
  ASelectCommand: TFDCustomCommand): Integer;
var
  LAdapter: TFDTableAdapter;
begin
  Assert(ASelectCommand <> nil);

  LAdapter := TFDTableAdapter.Create(nil);
  try
    LAdapter.SelectCommand := ASelectCommand;
    Result := InternalApplyUpdates(ADelta, LAdapter, -1);
  finally
    LAdapter.Free;
  end;
end;

procedure TFireDACApplyUpdates.CopyDataSet(ASource, ADest: TFDAdaptedDataSet);
var
  LStream: TStream;
begin
  LStream := TMemoryStream.Create;
  try
    ASource.SaveToStream(LStream, TFDStorageFormat.sfBinary);
    LStream.Seek(0, TSeekOrigin.soBeginning);
    ADest.LoadFromStream(LStream, TFDStorageFormat.sfBinary);
  finally
    LStream.Free;
  end;
end;

constructor TFireDACApplyUpdates.Create(const ADataSets: TFireDACDataSets);
begin
  FErrors := TFireDACUpdateErrors.Create;
  FDataSets := ADataSets;
end;

destructor TFireDACApplyUpdates.Destroy;
begin
  FErrors.Free;
  inherited;
end;

function TFireDACApplyUpdates.GetValue(AIndex: Integer): TFDMemTable;
begin
  Result := FDataSets.Items[AIndex].Value as TFDMemTable;
end;

function TFireDACApplyUpdates.GetValueByName(const AName: string): TFDMemTable;
begin
  Result := FDataSets.DataSet[AName] as TFDMemTable;
end;

function TFireDACApplyUpdates.InternalApplyUpdates(ADelta: TFDMemTable;
  AAdapter: TFDTableAdapter; AMaxErrors: Integer): Integer;
var
  LFDMemTable: TFDMemTable;
  LFDAdapter: TFDTableAdapter;
  LStoreItems: TFDStoreItems;
begin
  Assert(AAdapter <> nil);
  LFDMemTable := TFDMemTable.Create(nil);
  LStoreItems := LFDMemTable.ResourceOptions.StoreItems;
  LFDAdapter := TFDTableAdapter.Create(nil);
  try
    if Assigned(AAdapter.SelectCommand) then
      LFDAdapter.SelectCommand := AAdapter.SelectCommand;
    if Assigned(AAdapter.InsertCommand) then
      LFDAdapter.InsertCommand := AAdapter.InsertCommand;
    if Assigned(AAdapter.UpdateCommand) then
      LFDAdapter.UpdateCommand := AAdapter.UpdateCommand;
    if Assigned(AAdapter.DeleteCommand) then
      LFDAdapter.DeleteCommand := AAdapter.DeleteCommand;

    LFDAdapter.UpdateTableName := AAdapter.UpdateTableName;
    if LFDAdapter.UpdateTableName = '' then
      LFDAdapter.UpdateTableName := LFDAdapter.SelectCommand.UpdateOptions.UpdateTableName;

    LStoreItems := LFDMemTable.ResourceOptions.StoreItems;
    LFDMemTable.ResourceOptions.StoreItems := [siMeta, siDelta];
    LFDMemTable.CachedUpdates := True;
    LFDMemTable.Adapter := LFDAdapter;
    CopyDataSet(ADelta, LFDMemTable);

    FErrors.SetErrorHandler(LFDMemTable);
    try
      Result := LFDMemTable.ApplyUpdates(AMaxErrors);
    finally
      FErrors.ClearErrorHandler(LFDMemTable);
    end;

  finally
    LFDMemTable.ResourceOptions.StoreItems := LStoreItems;
    LFDMemTable.Free;
    LFDAdapter.Free;
  end;
end;

end.
