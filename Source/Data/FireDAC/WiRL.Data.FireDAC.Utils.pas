{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Data.FireDAC.Utils;

interface

uses
  System.Classes, System.SysUtils,
  FireDACJSONReflect,
  FireDAC.Comp.Client,
  FireDAC.Stan.StorageBin,
  WiRL.Core.JSON;

type
  TFireDACUtils = class
    class procedure FDJSONToMemTable(const AJSONContent: string;
      ADataSetName: string; AMemTable: TFDMemTable); static;
  end;

implementation

class procedure TFireDACUtils.FDJSONToMemTable(const AJSONContent: string;
  ADataSetName: string; AMemTable: TFDMemTable);
var
  LJSONObj: TJSONObject;
  LDataSets: TFDJSONDataSets;
begin
  LJSONObj := TJSONObject.ParseJSONValue(AJSONContent) as TJSONObject;

  LDataSets := TFDJSONDataSets.Create;
  try
    if not TFDJSONInterceptor.JSONObjectToDataSets(LJSONObj, LDataSets) then
      raise Exception.Create('Error deserializing data');

    AMemTable.Close;
    AMemTable.Data := TFDJSONDataSetsReader.GetListValueByName(LDataSets, ADataSetName);
    AMemTable.ApplyUpdates;
  finally
    LDataSets.Free;
  end;
end;


end.
