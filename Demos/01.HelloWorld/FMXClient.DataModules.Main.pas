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
  System.SysUtils, System.Classes, WiRL.Client.CustomResource,
  WiRL.Client.Resource, WiRL.Client.Resource.JSON, WiRL.Client.Application,
  WiRL.Client.Client, WiRL.Client.SubResource, WiRL.Client.SubResource.JSON,
  WiRL.Client.Messaging.Resource, System.JSON;

type
  TJobMessageSubscriber = TProc<string,Integer>;

  TMainDataModule = class(TDataModule)
    WiRLClient1: TWiRLClient;
    WiRLClientApplication1: TWiRLClientApplication;
    HelloWorldResource: TWiRLClientResource;
    EchoStringResource: TWiRLClientSubResource;
    ReverseStringResource: TWiRLClientSubResource;
    PostExampleResource: TWiRLClientSubResourceJSON;
  private
    { Private declarations }
  public
    function ExecuteHelloWorld: string;
    function EchoString(AString: string): string;
    function ReverseString(AString: string): string;
  end;

var
  MainDataModule: TMainDataModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

uses
  FMXClient.Forms.Main,

  WiRL.Rtti.Utils,
  WiRL.Core.JSON;

{$R *.dfm}

{ TMainDataModule }

function TMainDataModule.EchoString(AString: string): string;
begin
  EchoStringResource.PathParamsValues.Text := AString;
  Result := EchoStringResource.GETAsString();
end;

function TMainDataModule.ExecuteHelloWorld: string;
begin
  Result := HelloWorldResource.GETAsString();
end;

function TMainDataModule.ReverseString(AString: string): string;
begin
  ReverseStringResource.PathParamsValues.Text := AString;
  Result := ReverseStringResource.GETAsString();
end;

end.
