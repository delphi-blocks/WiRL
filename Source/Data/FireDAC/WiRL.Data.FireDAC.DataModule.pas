{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Data.FireDAC.DataModule;

interface

uses
  System.SysUtils, System.Classes,
  Data.FireDACJSONReflect, FireDAC.Comp.Client,

  // WiRL Core units
  WiRL.Core.Request,
  WiRL.Core.JSON,
  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  WiRL.Core.URL,
  // WiRL Data units
  WiRL.Data.MessageBodyWriters,
  // WiRL FireDAC units
  WiRL.Data.FireDAC,
  WiRL.Data.FireDAC.MessageBodyWriters;

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

    procedure BeforeApplyUpdates(ADeltas: TFDJSONDeltas; ADelta: TFDMemTable;
      ADataSet: TFDCustomQuery); virtual;

    procedure ApplyUpdates(ADeltas: TFDJSONDeltas;
      AOnApplyUpdates: TProc<string, Integer, IFDJSONDeltasApplyUpdates> = nil); virtual;
  public
    [GET][Produces(TMediaType.APPLICATION_JSON)]
    function Retrieve: TArray<TFDCustomQuery>; virtual;

    [POST, Produces(TMediaType.APPLICATION_JSON), Consumes(TMediaType.APPLICATION_JSON)]
    function Update: TJSONArray; virtual;

  published
    property ResourceName: string read GetResourceName;
  end;

implementation

{$R *.dfm}

uses
  System.Rtti, System.TypInfo, System.Generics.Collections,
  WiRL.Rtti.Utils;

{ TWiRLFDDataModuleResource }

procedure TWiRLFDDataModuleResource.ApplyUpdates(ADeltas: TFDJSONDeltas;
  AOnApplyUpdates: TProc<string, Integer, IFDJSONDeltasApplyUpdates>);
var
  LApplyUpdates: IFDJSONDeltasApplyUpdates;
  LIndex: Integer;
  LDelta: TPair<string, TFDMemTable>;
  LDataSet: TFDCustomQuery;
  LApplyResult: Integer;
begin
  LApplyUpdates := TFDJSONDeltasApplyUpdates.Create(ADeltas);
  try
    for LIndex := 0 to TFDJSONDeltasReader.GetListCount(ADeltas) - 1 do
    begin
      LDelta := TFDJSONDeltasReader.GetListItem(ADeltas, LIndex);
      LDataSet := Self.FindComponent(LDelta.Key) as TFDCustomQuery;

      BeforeApplyUpdates(ADeltas, LDelta.Value, LDataSet);
      LApplyResult := LApplyUpdates.ApplyUpdates(LDelta.Key, LDataSet.Command);
      if Assigned(AOnApplyUpdates) then
        AOnApplyUpdates(LDataSet.Name, LApplyResult, LApplyUpdates);
    end;
  finally
    LApplyUpdates := nil; // it's an interface
  end;
end;

procedure TWiRLFDDataModuleResource.BeforeApplyUpdates(ADeltas: TFDJSONDeltas;
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

function TWiRLFDDataModuleResource.Update: TJSONArray;
var
  LJSONDeltas: TJSONObject;
  LDeltas: TFDJSONDeltas;
  LResult: TJSONArray;
begin
  Result := nil;
  // parse JSON content
  LJSONDeltas := TJSONObject.ParseJSONValue(Request.Content) as TJSONObject;

  LDeltas := TFDJSONDeltas.Create;
  try
    // build FireDAC delta objects
    if not TFDJSONInterceptor.JSONObjectToDataSets(LJSONDeltas, LDeltas) then
      raise Exception.Create('Error de-serializing deltas');

    // apply updates
    LResult := TJSONArray.Create;
    try
      ApplyUpdates(LDeltas,
        procedure(ADatasetName: string; AApplyResult: Integer; AApplyUpdates: IFDJSONDeltasApplyUpdates)
        var
          LResultObj: TJSONObject;
        begin
          LResultObj := TJSONObject.Create;
          try
            LResultObj.AddPair('dataset', ADatasetName);
            LResultObj.AddPair('result', TJSONNumber.Create(AApplyResult));
            LResultObj.AddPair('errors', TJSONNumber.Create(AApplyUpdates.Errors.Count));
            LResultObj.AddPair('errorText', AApplyUpdates.Errors.Strings.Text);
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
  finally
    LDeltas.Free;
  end;
end;

{ RESTIncludeDefault }

constructor RESTIncludeDefault.Create(ADefaultValue: Boolean);
begin
  inherited Create;
  FDefaultValue := ADefaultValue;
end;

end.
