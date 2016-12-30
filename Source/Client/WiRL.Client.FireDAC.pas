{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Client.FireDAC;

interface

uses
  System.Classes, System.SysUtils,
  WiRL.Core.JSON,
  FireDAC.Comp.Client,
  WiRL.Client.Resource,
  WiRL.Client.Client;

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
    function FindItemByDataSetName(AName: string): TWiRLFDResourceDatasetsItem;
    procedure ForEach(const ADoSomething: TProc<TWiRLFDResourceDatasetsItem>);
    property Item[Index: Integer]: TWiRLFDResourceDatasetsItem read GetItem;
  end;


  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  TWiRLFDResource = class(TWiRLClientResource)
  private
    FResourceDataSets: TWiRLFDResourceDatasets;
    FPOSTResponse: TJSONValue;
  protected
    procedure AfterGET(); override;
    procedure BeforePOST(AContent: TMemoryStream); override;
    procedure AfterPOST(); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property POSTResponse: TJSONValue read FPOSTResponse write FPOSTResponse;
    property ResourceDataSets: TWiRLFDResourceDatasets read FResourceDataSets write FResourceDataSets;
  end;

procedure Register;

implementation

uses
  Data.FireDACJSONReflect,
  FireDAC.Comp.DataSet,
  WiRL.Core.Utils,
  WiRL.Client.Utils,
  WiRL.Data.FireDAC.Utils;

procedure Register;
begin
  RegisterComponents('WiRL Client', [TWiRLFDResource]);
end;

{ TWiRLFDResource }

procedure TWiRLFDResource.AfterGET();
var
  LJSONObj: TJSONObject;
  LDataSets: TFDJSONDataSets;
  LName: string;
  LData: TFDAdaptedDataSet;
  LCount: Integer;
  LItem: TWiRLFDResourceDatasetsItem;
  LIndex: Integer;
begin
  inherited;

  LJSONObj := TJSONObject.ParseJSONValue(StreamToString(Client.Response.ContentStream)) as TJSONObject;

  LDataSets := TFDJSONDataSets.Create;
  try
    if not TFDJSONInterceptor.JSONObjectToDataSets(LJSONObj, LDataSets) then
      raise Exception.Create('Error deserializing data');

    LCount := TFDJSONDataSetsReader.GetListCount(LDataSets);
    for LIndex := 0 to LCount-1 do
    begin
      LName := TFDJSONDataSetsReader.GetListKey(LDataSets, LIndex);
      LData := TFDJSONDataSetsReader.GetListValue(LDataSets, LIndex);

      LItem := FResourceDataSets.FindItemByDataSetName(LName);
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
//                  LItem.DataSet.CopyDataSet(LData, [coStructure, coRestart, coAppend]);
                  LItem.DataSet.Data := LData;
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
//              LItem.DataSet.CopyDataSet(LData, [coStructure, coRestart, coAppend]);
              LItem.DataSet.Data := LData;
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
        LItem.DataSetName := LName;
      end;
    end;
  finally
    LDataSets.Free;
  end;
end;

procedure TWiRLFDResource.AfterPOST();
begin
  inherited;
  if Client.LastCmdSuccess then
    FResourceDataSets.ForEach(
      procedure (AItem: TWiRLFDResourceDatasetsItem)
      begin
        if AItem.SendDelta and Assigned(AItem.DataSet) and (AItem.DataSet.Active) then
          AItem.DataSet.ApplyUpdates;
      end
    );

  if Assigned(FPOSTResponse) then
    FPOSTResponse.Free;
  FPOSTResponse := StreamToJSONValue(Client.Response.ContentStream);
end;

procedure TWiRLFDResource.BeforePOST(AContent: TMemoryStream);
var
  LDeltas: TFDJSONDeltas;
  LJSONObj: TJSONObject;
  LWriter: TStreamWriter;
begin
  inherited;

  LDeltas := TFDJSONDeltas.Create;
  try
    FResourceDataSets.ForEach(
      procedure(AItem: TWiRLFDResourceDatasetsItem)
      begin
        if AItem.SendDelta and Assigned(AItem.DataSet) and (AItem.DataSet.Active) then
          TFDJSONDeltasWriter.ListAdd(LDeltas, AItem.DataSetName, AItem.DataSet);
      end
    );

    // serialize deltas to JSON (TJSONObject)
    LJSONObj := TJSONObject.Create;
    try
      TFDJSONInterceptor.DataSetsToJSONObject(LDeltas, LJSONObj);

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
    LDeltas.Free;
  end;
end;

constructor TWiRLFDResource.Create(AOwner: TComponent);
begin
  inherited;
  FResourceDataSets := TWiRLFDResourceDatasets.Create(TWiRLFDResourceDatasetsItem);
end;

destructor TWiRLFDResource.Destroy;
begin
  FPOSTResponse.Free;
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

{ TWiRLFDResourceDatasets }

function TWiRLFDResourceDatasets.Add: TWiRLFDResourceDatasetsItem;
begin
  Result := inherited Add as TWiRLFDResourceDatasetsItem;
end;

function TWiRLFDResourceDatasets.FindItemByDataSetName(
  AName: string): TWiRLFDResourceDatasetsItem;
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
