{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Data.FireDAC.DataModule;

interface

uses
  System.SysUtils, System.Classes,
  FireDAC.Comp.Client,

  // WiRL Core units
  WiRL.http.Request,
  WiRL.Core.JSON,
  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  WiRL.http.URL,
  // WiRL Data units
  WiRL.Data.MessageBody.Default,
  // WiRL FireDAC units
  WiRL.Data.FireDAC,
  WiRL.Data.FireDAC.Persistence,
  WiRL.Data.FireDAC.Updates,
  WiRL.Data.FireDAC.MessageBody.Default;

type
  RESTIncludeDefault = class(TCustomAttribute)
  private
    FDefaultValue: Boolean;
  public
    constructor Create(ADefaultValue: Boolean);
    property DefaultValue: Boolean read FDefaultValue write FDefaultValue;
  end;

  RESTExposeAttribute = class(TCustomAttribute);
  RESTInclude = class(RESTExposeAttribute);
  RESTExclude = class(RESTExposeAttribute);

  TWiRLFDDataModuleResource = class(TDataModule)
  private
    function GetResourceName: string;
  protected
    [Context] Request: TWiRLRequest;
    [Context] URL: TWiRLURL;

    procedure BeforeApplyUpdates(ADeltas: TFireDACDataSets; ADelta: TFDMemTable;
      ADataSet: TFDCustomQuery); virtual;

    procedure ApplyUpdates(ADeltas: TFireDACDataSets;
      AOnApplyUpdates: TProc<string, Integer, TFireDACApplyUpdates> = nil); virtual;
  public
    [GET][Produces(TMediaType.APPLICATION_JSON)]
    [Result: Singleton]
    function Retrieve: TArray<TFDCustomQuery>; virtual;

    [PUT, Produces(TMediaType.APPLICATION_JSON), Consumes(TMediaType.APPLICATION_JSON)]
    function Update([BodyParam] ADeltas: TFireDACDataSets): TJSONArray; virtual;

  published
    property ResourceName: string read GetResourceName;
  end;

implementation

{$R *.dfm}

uses
  System.JSON, System.Rtti, System.TypInfo, System.Generics.Collections,
  WiRL.Rtti.Utils;

{ TWiRLFDDataModuleResource }

procedure TWiRLFDDataModuleResource.ApplyUpdates(ADeltas: TFireDACDataSets;
    AOnApplyUpdates: TProc<string, Integer, TFireDACApplyUpdates> = nil);
var
  LApplyOperation: TFireDACApplyUpdates;
  LPair: TPair<string, TFDAdaptedDataSet>;
  LDelta: TFDMemTable;
  LDataSet: TFDCustomQuery;
  LApplyResult: Integer;
begin
  LApplyOperation := TFireDACApplyUpdates.Create(ADeltas);
  try
    for LPair in ADeltas do
    begin
      LDelta := LPair.Value as TFDMemTable;
      LDataSet := Self.FindComponent(LPair.Key) as TFDCustomQuery;
      BeforeApplyUpdates(ADeltas, LDelta, LDataSet);
      LApplyResult := LApplyOperation.ApplyUpdates(LDelta, LDataSet.Command);
      if Assigned(AOnApplyUpdates) then
        AOnApplyUpdates(LDataSet.Name, LApplyResult, LApplyOperation);
    end;
  finally
    LApplyOperation.Free;
  end;
end;

procedure TWiRLFDDataModuleResource.BeforeApplyUpdates(ADeltas: TFireDACDataSets; 
  ADelta: TFDMemTable; ADataSet: TFDCustomQuery);
begin

end;

function TWiRLFDDataModuleResource.GetResourceName: string;
var
  LResult: string;
begin
  LResult := '';
  TRttiHelper.IfHasAttribute<PathAttribute>(Self,
    procedure (AAttrib: PathAttribute)
    begin
      LResult := AAttrib.Value;
    end
  );
  Result := LResult;
end;

function TWiRLFDDataModuleResource.Retrieve: TArray<TFDCustomQuery>;
var
  LIncludeDefault: Boolean;
  LDataSets: TArray<TFDCustomQuery>;
begin
  // determine default behavior
  LIncludeDefault := True;
  TRttiHelper.IfHasAttribute<RESTIncludeDefault>(Self,
    procedure(AAttrib: RESTIncludeDefault)
    begin
      LIncludeDefault := AAttrib.DefaultValue;
    end
  );

  SetLength(LDataSets, 0);
  TRttiHelper.ForEachField(Self,
    function(AField: TRttiField): Boolean
    begin
      if (AField.Visibility >= TMemberVisibility.mvPublic)
        and (TRttiHelper.IsObjectOfType(AField.FieldType, TFDCustomQuery)) then
      begin
        if (LIncludeDefault or TRttiHelper.HasAttribute<RESTInclude>(AField))
           and (not TRttiHelper.HasAttribute<RESTExclude>(AField))
        then
        begin
          SetLength(LDataSets, Length(LDataSets) + 1);
          LDataSets[Length(LDataSets) - 1] := AField.GetValue(Self).AsObject as TFDCustomQuery;
        end;
      end;

      Result := True;
    end
  );

  Result := LDataSets;
end;

function TWiRLFDDataModuleResource.Update(ADeltas: TFireDACDataSets): TJSONArray;
var
  LResult: TJSONArray;
begin
  LResult := TJSONArray.Create;
  try
    ApplyUpdates(ADeltas,
      procedure(ADatasetName: string; AApplyResult: Integer; AApplyUpdates: TFireDACApplyUpdates)
      var
        LResultObj: TJSONObject;
      begin
        LResultObj := TJSONObject.Create;
        try
          LResultObj.AddPair('dataset', ADatasetName);
          LResultObj.AddPair('result', TJSONNumber.Create(AApplyResult));
          LResultObj.AddPair('errors', TJSONNumber.Create(AApplyUpdates.Errors.Count));
          LResultObj.AddPair('errorText', AApplyUpdates.Errors.Errors.Text);
          LResult.AddElement(LResultObj);
        except
          LResultObj.Free;
          raise;
        end;
      end
    );

    Result := LResult;
  except
    LResult.Free;
    raise;
  end;
end;

{ RESTIncludeDefault }

constructor RESTIncludeDefault.Create(ADefaultValue: Boolean);
begin
  inherited Create;
  FDefaultValue := ADefaultValue;
end;

end.
