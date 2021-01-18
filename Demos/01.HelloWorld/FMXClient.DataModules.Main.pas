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
  WiRL.Client.Resource, WiRL.Client.Resource.JSON, WiRL.Client.Application,
  WiRL.http.Client, WiRL.Client.SubResource,
  WiRL.Client.SubResource.JSON, WiRL.Client.Messaging.Resource,
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
    EchoStringResource: TWiRLClientSubResource;
    ReverseStringResource: TWiRLClientSubResource;
    PostExampleResource: TWiRLClientSubResourceJSON;
    PersonResource: TWiRLClientSubResource;
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
    procedure GetDBData(AMemTable: TFDMemTable);
    function PostDBData(AMemTable: TFDMemTable): Integer;
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
  EchoStringResource.PathParamsValues.Values['AString'] := AString;
  Result := EchoStringResource.GETAsString();
end;

function TMainDataModule.ExecuteHelloWorld: string;
begin
  Result := HelloWorldResource.GETAsString();
end;

procedure TMainDataModule.GetDBData(AMemTable: TFDMemTable);
var
  LStream: TStream;
begin
  LStream := WiRLClientApplication1
    .Resource('helloworld/db')
    .Accept('application/json')
    .Get<TStream>;

  try
    LStream.Position := 0;
    AMemTable.LoadFromStream(LStream, sfJSON);
  finally
    LStream.Free;
  end;

end;

function TMainDataModule.GetPerson(Id: Integer): TPerson;
begin
  Result := WiRLClientApplication1
    .Resource('helloworld/person')
    .Accept('application/json')
    .AcceptLanguage('it_IT')
//    .AcceptEncoding('application/json')
//    .Cookie('name', 'value')
    .Header('x-author', 'Luca')
//    .QueryParam('start', 'now - 5 minutes')
    .QueryParam('Id', Id.ToString)
    .Get<TPerson>;
end;

function TMainDataModule.PostDBData(AMemTable: TFDMemTable): Integer;
var
  LStream: TStream;
begin
  LStream := TMemoryStream.Create;
  try

    AMemTable.SaveToStream(LStream, sfJSON);
    LStream.Position := 0;

    Result := WiRLClientApplication1
      .Resource('helloworld/db')
      .Accept('text/plain')
      .ContentType('application/json')
      .Post<TStream, Integer>(LStream);

  finally
    LStream.Free;
  end;
end;

function TMainDataModule.PostOrder(AOrderProposal: TOrderProposal): TOrder;
begin
  Result := WiRLClientApplication1
    .Resource('helloworld/order')
    .Accept('application/json')
    .ContentType('application/json')
//    .AcceptEncoding('application/json')
//    .Cookie('name', 'value')
    .Header('x-author', 'Luca')
//    .QueryParam('start', 'now - 5 minutes')
//    .QueryParam('Id', Id.ToString)
    .Post<TOrderProposal, TOrder>(AOrderProposal);
end;

function TMainDataModule.ReverseString(AString: string): string;
begin
  ReverseStringResource.PathParamsValues.Values['AString'] := AString;
  Result := ReverseStringResource.GETAsString();
end;

procedure TMainDataModule.WiRLClient1BeforeCommand(ASender: TObject; ARequest:
    IWiRLRequest);
begin
  ARequest.Headers['X-App-Params'] := 'TestCustomHeader';
end;

end.
