(*
  Copyright 2015-2016, WiRL - REST Library

  Home: https://github.com/WiRL-library

*)
unit Server.MainData;

interface

uses
  System.SysUtils, System.Classes,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, Data.DB, FireDAC.Comp.Client, FireDAC.Phys.FB,
  FireDAC.Phys.FBDef, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, FireDAC.Comp.DataSet, FireDAC.VCLUI.Wait, FireDAC.Comp.UI,

  WiRL.Data.FireDAC.DataModule,
  WiRL.Core.Attributes,
  WiRL.Core.URL,
  WiRL.Core.Token, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs;

type
  [Path('/maindata')]
  TMainDataResource = class(TWiRLFDDataModuleResource)
    FDConnection1: TFDConnection;
    employee: TFDQuery;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
  private
  public
//    [GET, Path('/standard')]
//    function StandardDataSet: TArray<TDataset>;

//    [GET, Path('/employee')]
//    function EmployeeDataSet: TDataSet;
  end;

implementation

{$R *.dfm}

uses
  WiRL.Core.Registry;

{ TMainDataResource }

//function TMainDataResource.EmployeeDataSet: TDataSet;
//begin
//  Result := Employee;
//end;
//
//function TMainDataResource.StandardDataSet: TArray<TDataset>;
//begin
//  Result := [employee];
//end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TMainDataResource>(
    function: TObject
    begin
      Result := TMainDataResource.Create(nil);
    end
  );

end.
