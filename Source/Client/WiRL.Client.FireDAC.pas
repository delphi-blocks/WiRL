{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Client.FireDAC;

{$I ..\Core\WiRL.inc}

interface

uses
  System.Classes, System.SysUtils,
  WiRL.Core.JSON,
  FireDAC.Comp.Client,
  WiRL.Client.Resource,
  WiRL.http.Client.Interfaces,
  WiRL.http.Client, System.JSON;

type
  TWiRLFDResourceDatasetsItem = class(TCollectionItem)
  private
    FDataSet: TFDMemTable;
    FDataSetName: string;
    FSendDelta: Boolean;
    FSynchronize: Boolean;
    procedure SetDataSet(const Value: TFDMemTable);
  public
    constructor Create(Collection: TCollection); override;
  published
    property DataSetName: string read FDataSetName write FDataSetName;
    property DataSet: TFDMemTable read FDataSet write SetDataSet;
    property SendDelta: Boolean read FSendDelta write FSendDelta;
    property Synchronize: Boolean read FSynchronize write FSynchronize;
  end;

  TWiRLFDResourceDatasets = class(TCollection)
  private
    function GetItem(Index: Integer): TWiRLFDResourceDatasetsItem;
  public
    function Add: TWiRLFDResourceDatasetsItem;
    function FindItemByDataSetName(const AName: string): TWiRLFDResourceDatasetsItem;
    procedure ForEach(const ADoSomething: TProc<TWiRLFDResourceDatasetsItem>);
    property Item[Index: Integer]: TWiRLFDResourceDatasetsItem read GetItem;
  end;

  {$IFDEF HAS_NEW_PIDS}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator32 or pidiOSDevice32 or pidAndroid32Arm)]
  {$ELSE}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  {$ENDIF}
  TWiRLFDResource = class(TWiRLClientResource)
  private
    FResourceDataSets: TWiRLFDResourceDatasets;
    FJSONResponse: TJSONValue;

    procedure ReadDeltas(AResponse: IWiRLResponse);
    procedure WriteDeltas(AContent: TMemoryStream);
  protected
    procedure AfterGET(AResponse: IWiRLResponse); override;
    procedure BeforePOST(AContent: TMemoryStream); override;
    procedure AfterPOST(AResponse: IWiRLResponse); override;
    procedure BeforePUT(AContent: TMemoryStream); override;
    procedure AfterPUT(AResponse: IWiRLResponse); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property JSONResponse: TJSONValue read FJSONResponse write FJSONResponse;
    property ResourceDataSets: TWiRLFDResourceDatasets read FResourceDataSets write FResourceDataSets;
  end;

implementation

uses
  FireDAC.Comp.DataSet,
  WiRL.Core.Utils,
  WiRL.Client.Utils,

  WiRL.Data.FireDAC.Persistence,
  WiRL.Data.FireDAC.Updates,
  WiRL.Data.FireDAC.Utils;

{ TWiRLFDResource }

procedure TWiRLFDResource.AfterGET(AResponse: IWiRLResponse);
var
  LJSONObj: TJSONObject;
  LDataSets: TFireDACDataSets;
  LDSPair: TFireDACDataSetPair;
  LItem: TWiRLFDResourceDatasetsItem;
begin
  inherited;

  LJSONObj := TJSONObject.ParseJSONValue(AResponse.Content) as TJSONObject;

  LDataSets := TFireDACDataSets.Create;
  try
    TFireDACJSONPersistor.JSONToDataSets(LJSONObj, LDataSets);

    for LDSPair in LDataSets do
    begin
      LItem := FResourceDataSets.FindItemByDataSetName(LDSPair.Key);
      if Assigned(LItem) then
      begin
        if Assigned(LItem.DataSet) then
        begin
          if LItem.Synchronize then
            TThread.Synchronize(nil,
              procedure
              begin
                LItem.DataSet.DisableControls;
                try
                  LItem.DataSet.Close;
                  LItem.DataSet.Data := LDSPair.Value;
                  LItem.DataSet.ApplyUpdates;
                finally
                  LItem.DataSet.EnableControls;
                end;
              end
            )
          else
          begin
            LItem.DataSet.DisableControls;
            try
              LItem.DataSet.Close;
              LItem.DataSet.Data := LDSPair.Value;
              LItem.DataSet.ApplyUpdates;
            finally
              LItem.DataSet.EnableControls;
            end;
          end;
        end;
      end
      else
      begin
        LItem := FResourceDataSets.Add;
        LItem.DataSetName := LDSPair.Key;
      end;
    end;
  finally
    LDataSets.Free;
  end;
end;

procedure TWiRLFDResource.AfterPOST(AResponse: IWiRLResponse);
begin
  inherited;
  ReadDeltas(AResponse);
end;

procedure TWiRLFDResource.AfterPUT(AResponse: IWiRLResponse);
begin
  inherited;
  ReadDeltas(AResponse);
end;

procedure TWiRLFDResource.BeforePOST(AContent: TMemoryStream);
begin
  inherited;
  WriteDeltas(AContent);
end;

procedure TWiRLFDResource.BeforePUT(AContent: TMemoryStream);
begin
  inherited;
  WriteDeltas(AContent);
end;

constructor TWiRLFDResource.Create(AOwner: TComponent);
begin
  inherited;
  FResourceDataSets := TWiRLFDResourceDatasets.Create(TWiRLFDResourceDatasetsItem);
end;

destructor TWiRLFDResource.Destroy;
begin
  FJSONResponse.Free;
  FResourceDataSets.Free;
  inherited;
end;

procedure TWiRLFDResource.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = TOperation.opRemove then
  begin
    FResourceDataSets.ForEach(
      procedure (AItem: TWiRLFDResourceDatasetsItem)
      begin
        if AItem.DataSet = AComponent then
          AItem.DataSet := nil;
      end
    );
  end;
end;

procedure TWiRLFDResource.ReadDeltas(AResponse: IWiRLResponse);
begin
  if AResponse.StatusCode < 400 then
    FResourceDataSets.ForEach(
      procedure (AItem: TWiRLFDResourceDatasetsItem)
      begin
        if AItem.SendDelta and Assigned(AItem.DataSet) and (AItem.DataSet.Active) then
          AItem.DataSet.ApplyUpdates;
      end
    );

  if Assigned(FJSONResponse) then
    FJSONResponse.Free;
  FJSONResponse := StreamToJSONValue(AResponse.ContentStream);
end;

procedure TWiRLFDResource.WriteDeltas(AContent: TMemoryStream);
var
  LDelta: TFDMemTable;
  LDeltas: TFireDACDataSets;
  LJSONObj: TJSONObject;
  LWriter: TStreamWriter;
begin
  LDeltas := TFireDACDataSets.Create;
  try
    FResourceDataSets.ForEach(
      procedure(AItem: TWiRLFDResourceDatasetsItem)
      begin
        if AItem.SendDelta and Assigned(AItem.DataSet) and (AItem.DataSet.Active) then
        begin
          LDelta := TFDMemTable.Create(nil);
          try
            LDelta.Name := AItem.DataSetName;
            LDelta.Data := AItem.DataSet.Delta;
            LDeltas.Add(LDelta.Name, LDelta);
          except
            FreeAndNil(LDelta);
            raise;
          end;
        end;
      end
    );

    // Serialize deltas to JSON (TJSONObject)
    LJSONObj := TJSONObject.Create;
    try
      TFireDACJSONPersistor.DataSetsToJSON(LDeltas, LJSONObj);

      LWriter := TStreamWriter.Create(AContent);
      try
        LWriter.Write(TJSONHelper.ToJSON(LJSONObj));
      finally
        LWriter.Free;
      end;
    finally
      LJSONObj.Free;
    end;
  finally
    LDeltas.FreeChilds;
    LDeltas.Free;
  end;
end;

{ TWiRLFDResourceDatasets }

function TWiRLFDResourceDatasets.Add: TWiRLFDResourceDatasetsItem;
begin
  Result := inherited Add as TWiRLFDResourceDatasetsItem;
end;

function TWiRLFDResourceDatasets.FindItemByDataSetName(const AName: string):
    TWiRLFDResourceDatasetsItem;
var
  LIndex: Integer;
  LItem: TWiRLFDResourceDatasetsItem;
begin
  Result := nil;
  for LIndex := 0 to Count-1 do
  begin
    LItem := GetItem(LIndex);
    if SameText(LItem.DataSetName, AName) then
    begin
      Result := LItem;
      Break;
    end;
  end;
end;

procedure TWiRLFDResourceDatasets.ForEach(
  const ADoSomething: TProc<TWiRLFDResourceDatasetsItem>);
var
  LIndex: Integer;
begin
  if Assigned(ADoSomething) then
  begin
    for LIndex := 0 to Count-1 do
      ADoSomething(GetItem(LIndex));
  end;
end;

function TWiRLFDResourceDatasets.GetItem(
  Index: Integer): TWiRLFDResourceDatasetsItem;
begin
  Result := inherited GetItem(Index) as TWiRLFDResourceDatasetsItem;
end;

{ TWiRLFDResourceDatasetsItem }

constructor TWiRLFDResourceDatasetsItem.Create(Collection: TCollection);
begin
  inherited;
  FSendDelta := True;
  FSynchronize := True;
end;

procedure TWiRLFDResourceDatasetsItem.SetDataSet(const Value: TFDMemTable);
begin
  if FDataSet <> Value then
  begin
    FDataSet := Value;
    if Assigned(FDataSet) then
    begin
      if SendDelta then
        FDataSet.CachedUpdates := True;
      FDataSet.ActiveStoredUsage := [];
    end;
  end;
end;

end.
