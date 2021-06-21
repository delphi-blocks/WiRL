{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit FMXClient.DataModules.Main;

interface

uses
  System.SysUtils, System.Classes, System.TypInfo,
  WiRL.Client.CustomResource,
  WiRL.Client.Resource, WiRL.Client.Application,
  WiRL.http.Client,
  WiRL.http.Request, WiRL.http.Response,
  System.JSON, System.Net.HttpClient.Win,
  Demo.Entities,
  System.Net.HttpClientComponent, FMX.Types, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  WiRL.http.Client.Interfaces;

type
  TJobMessageSubscriber = TProc<string,Integer>;

  TMainDataModule = class(TDataModule)
    WiRLClient1: TWiRLClient;
    WiRLClientApplication1: TWiRLClientApplication;
    HelloWorldResource: TWiRLClientResource;
    EchoStringResource: TWiRLClientResource;
    ReverseStringResource: TWiRLClientResource;
    PostStreamResource: TWiRLClientResource;
    procedure DataModuleCreate(Sender: TObject);
    procedure WiRLClient1BeforeCommand(ASender: TObject; ARequest: IWiRLRequest);
  private
    { Private declarations }
  public
    function ExecuteHelloWorld: string;
    function EchoString(AString: string): string;
    function ReverseString(AString: string): string;
    function GetPerson(Id: Integer): TPerson;
    function PostOrder(AOrderProposal: TOrderProposal): TOrder;
  end;

var
  MainDataModule: TMainDataModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

uses
  FMXClient.Forms.Main,

  {$IFDEF HAS_NETHTTP_CLIENT}
  WiRL.http.Client.NetHttp,
  {$ELSE}
  WiRL.http.Client.Indy,
  {$ENDIF}

  WiRL.Configuration.Neon,
  WiRL.Rtti.Utils,
  WiRL.Core.JSON,
  WiRL.Core.MessageBody.Default,

  Neon.Core.Types;

{$R *.dfm}

{ TMainDataModule }

procedure TMainDataModule.DataModuleCreate(Sender: TObject);
begin
  WiRLClientApplication1.SetReaders('*.*');
  WiRLClientApplication1.SetWriters('*.*');

//  WiRLClientApplication1
//    .Plugin.Configure<IWiRLConfigurationNeon>
//      .SetUseUTCDate(True)
//      .SetVisibility([mvPublic, mvPublished])
//      .SetMemberCase(TNeonCase.PascalCase);

end;

function TMainDataModule.EchoString(AString: string): string;
begin
  EchoStringResource.PathParam('AString', AString);
  Result := EchoStringResource.GET<string>;
end;

function TMainDataModule.ExecuteHelloWorld: string;
begin
  Result := HelloWorldResource.GET<string>;
end;

function TMainDataModule.GetPerson(Id: Integer): TPerson;
begin
  Result := WiRLClientApplication1
    .Resource('helloworld/person')
    .Accept('application/json')
    .AcceptLanguage('it_IT')
    .Header('x-author', 'Luca')
    .QueryParam('Id', Id.ToString)
    .Get<TPerson>;
end;

function TMainDataModule.PostOrder(AOrderProposal: TOrderProposal): TOrder;
begin
  Result := WiRLClientApplication1
    .Resource('helloworld/order')
    .Accept('application/json')
    .ContentType('application/json')
    .Header('x-author', 'Luca')
    .Post<TOrderProposal, TOrder>(AOrderProposal);
end;

function TMainDataModule.ReverseString(AString: string): string;
begin
  ReverseStringResource.PathParam('AString', AString);
  Result := ReverseStringResource.GET<string>;
end;

procedure TMainDataModule.WiRLClient1BeforeCommand(ASender: TObject; ARequest:
    IWiRLRequest);
begin
  ARequest.Headers['X-App-Params'] := 'TestCustomHeader';
end;

end.
