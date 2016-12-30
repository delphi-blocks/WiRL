{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Resources.Datamodule;

interface

uses
  System.SysUtils, System.Classes, WiRL.Data.FireDAC.DataModule
  , WiRL.Core.Attributes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.FB,
  FireDAC.Phys.FBDef, Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.DataSet
  ;

type
  [Path('/datamodule')]
  TDataResource = class(TWiRLFDDataModuleResource)
    FDConnection1: TFDConnection;
    QueryItems: TFDQuery;
    QueryAccounts: TFDQuery;
    procedure QueryItemsBeforeOpen(DataSet: TDataSet);
  private
  public
    [GET, Path('/standard')]
    function Standard: TArray<TDataSet>;
  end;

var
  DataResource: TDataResource;

implementation

uses
    WiRL.Core.Registry
    , WiRL.Data.FireDAC.MessageBodyWriters
  ;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

{ TDataResource }

procedure TDataResource.QueryItemsBeforeOpen(DataSet: TDataSet);
var
  LOwner: string;
begin
  inherited;
  LOwner := '';
  URL.QueryTokens.TryGetValue('username', LOwner);
  QueryItems.ParamByName('OWNER').AsString := LOwner;
end;

function TDataResource.Standard: TArray<TDataSet>;
begin
  Result := [QueryItems];
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TDataResource>(
    function: TObject
    begin
      Result := TDataResource.Create(nil);
    end
  );

end.
