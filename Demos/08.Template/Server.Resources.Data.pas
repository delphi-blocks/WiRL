{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Resources.Data;

interface

uses
  System.SysUtils, System.Classes
  , Data.DB

  , FireDAC.Stan.Intf, FireDAC.Stan.Option
  , FireDAC.Stan.Error, FireDAC.UI.Intf
  , FireDAC.Phys.Intf, FireDAC.Stan.Def
  , FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys
  , FireDAC.Comp.Client
  , FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt
  , FireDAC.Comp.DataSet

  , WiRL.Data.FireDAC.DataModule
  , WiRL.Core.Attributes
  , WiRL.Core.URL
  ;

type
  [Path('data')]
  TDataResource = class(TWiRLFDDataModuleResource)
    DBConnection: TFDConnection;
  private
  public
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
    WiRL.Core.Registry
  ;


initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TDataResource>(
    function:TObject
    begin
      Result := TDataResource.Create(nil);
    end
  );

end.
