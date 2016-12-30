{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Data.FireDAC.Editor;

interface

uses
  System.Classes, System.SysUtils
  ,  DesignEditors
  , WiRL.Client.CustomResource.Editor
  , WiRL.Client.FireDAC;

type
  TWiRLFDResourceEditor = class(TWiRLClientCustomResourceEditor)
  private
    function CurrentObj: TWiRLFDResource;
  protected
    procedure SetDesignTimePosition(AComponent: TComponent; AIndex: Integer = 0);
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure Register;

implementation

uses
  Windows
  , VCL.Dialogs
  , DesignIntf

  , FireDAC.Comp.Client
;

procedure Register;
begin
  RegisterComponentEditor(TWiRLFDResource, TWiRLFDResourceEditor);
end;

{ TWiRLFDResourceEditor }

function TWiRLFDResourceEditor.CurrentObj: TWiRLFDResource;
begin
  Result := Component as TWiRLFDResource;
end;

procedure TWiRLFDResourceEditor.ExecuteVerb(Index: Integer);
var
  LIndex: Integer;
  LMemTable: TFDMemTable;
  LOwner: TComponent;
  LCreated: Integer;
begin
  inherited;
  LIndex := GetVerbCount - 1;
  if Index = LIndex then
  begin
    LCreated := 0;
    LOwner := CurrentObj.Owner;
    CurrentObj.GET;

    CurrentObj.ResourceDataSets.ForEach(
      procedure(AItem: TWiRLFDResourceDatasetsItem)
      begin
        if (not Assigned(AItem.DataSet)) and (AItem.DataSetName <> '') then
        begin
          LMemTable := TFDMemTable.Create(LOwner);
          try
            LMemTable.Name := Designer.UniqueName(AItem.DataSetName);
            AItem.DataSet := LMemTable;

            SetDesignTimePosition(LMemTable, LCreated);
            Inc(LCreated);
          except
            LMemTable.Free;
            raise;
          end;
        end;
      end
    );

    CurrentObj.GET();
  end;

  Designer.Modified;
end;

function TWiRLFDResourceEditor.GetVerb(Index: Integer): string;
var
  LIndex: Integer;
begin
  Result := inherited GetVerb(Index);

  LIndex := GetVerbCount - 1;
  if Index = LIndex then
  begin
    Result := 'Create datasets';
  end;
end;

function TWiRLFDResourceEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 1;
end;

procedure TWiRLFDResourceEditor.SetDesignTimePosition(AComponent: TComponent; AIndex: Integer);
var
  LRec: LongRec;
begin
  LRec := LongRec(CurrentObj.DesignInfo);

  LRec.Hi := LRec.Hi + 48; // top
  LRec.Lo := LRec.Lo + (AIndex * 48); // left
  AComponent.DesignInfo := Integer(LRec);
  Designer.Modified;
end;

end.


