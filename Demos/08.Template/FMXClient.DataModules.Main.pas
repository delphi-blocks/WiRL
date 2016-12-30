{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit FMXClient.DataModules.Main;

interface

uses
  System.SysUtils, System.Classes,
  WiRL.Client.Application,
  WiRL.Client.Client;

type
  TMainDataModule = class(TDataModule)
    WiRLClient: TWiRLClient;
    WiRLApplication: TWiRLClientApplication;
  private
  public
  end;

var
  MainDataModule: TMainDataModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
