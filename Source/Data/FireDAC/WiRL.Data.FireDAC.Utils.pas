{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Data.FireDAC.Utils;

interface

uses
  System.Classes, System.SysUtils,
  FireDAC.Comp.Client,
  FireDAC.Stan.StorageBin,
  FireDAC.Stan.StorageJSON,
  WiRL.Data.FireDAC.Persistence,
  WiRL.Core.JSON;

type
  TFireDACUtils = class
    class procedure FDJSONToMemTable(const AJSONContent: string;
      const ADataSetName: string; AMemTable: TFDMemTable); static;
  end;

implementation

class procedure TFireDACUtils.FDJSONToMemTable(const AJSONContent: string;
  const ADataSetName: string; AMemTable: TFDMemTable);
var
  LJSONObj: TJSONObject;
  LDataSets: TFireDACDataSets;
begin
  LJSONObj := TJSONObject.ParseJSONValue(AJSONContent) as TJSONObject;

  LDataSets := TFireDACDataSets.Create;
  try
    TFireDACJSONPersistor.JSONToDataSets(LJSONObj, LDataSets);

    AMemTable.Close;
    AMemTable.Data := LDataSets.DataSet[ADataSetName];
    AMemTable.ApplyUpdates;
  finally
    LDataSets.Free;
  end;
end;


end.
