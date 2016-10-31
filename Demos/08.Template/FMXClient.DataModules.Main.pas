(*
  Copyright 2015-2016, WiRL - REST Library

  Home: https://github.com/WiRL-library

*)
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
