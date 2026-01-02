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

{$I '..\Core\WiRL.inc'}

{.$UNDEF HAS_NETHTTP_CLIENT}

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
  WiRL.http.Core, WiRL.http.Client.Interfaces, Generics.Collections;

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
    procedure FillOrder(AOrderProposal: TOrderProposal; AOrder: TOrder);
    function TestException(): string;
    function GetImageStreamResource: TStream;
    procedure FillImageStreamResource(AStream: TStream);
    function PostStream(AStream: TStream): string;
    function GetRawResponse: string;
    function GetStringAndStream(AStream: TStream): string;
    function GetPersonAndHeader(Id: Integer): TPair<TPerson, string>;
    function GetPersonOrError(Id: Integer): TPerson;
    function PostMultiPart(const AFileName: string; LObject: TSimpleParam; const AString: string): string;
  end;

var
  MainDataModule: TMainDataModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

uses
  System.Net.Mime,
  FMXClient.Forms.Main,

  {$IFDEF HAS_NETHTTP_CLIENT}
  WiRL.http.Client.NetHttp,
  System.Net.HttpClient,
  {$ELSE}
  WiRL.http.Client.Indy,
  IdMultipartFormData,
  {$ENDIF}

  WiRL.Configuration.Neon,
  WiRL.Rtti.Utils,
  WiRL.Core.JSON,
  WiRL.Core.MessageBody.Default,

  Neon.Core.Types,
  Neon.Core.Persistence.JSON;

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

procedure TMainDataModule.FillImageStreamResource(AStream: TStream);
begin
  WiRLClientApplication1
    .Resource('entity/image')
    .Accept('image/png')
    .Get(AStream);
end;

function TMainDataModule.GetStringAndStream(AStream: TStream): string;
begin
  Result := WiRLClientApplication1
    .Resource('helloworld')
    .SetContentStream(AStream)
    .Accept('text/plain')
    .Get<string>;
end;

procedure TMainDataModule.FillOrder(AOrderProposal: TOrderProposal;
  AOrder: TOrder);
begin
  WiRLClientApplication1
    .Resource('helloworld/order')
    .Accept('application/json')
    .ContentType('application/json')
    .Header('x-author', 'Luca')
    .Post<TOrderProposal>(AOrderProposal, AOrder);
end;

function TMainDataModule.GetImageStreamResource: TStream;
begin
  Result := WiRLClientApplication1
    .Resource('entity/image')
    .Accept('image/png')
    .Get<TStream>;
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

function TMainDataModule.GetPersonAndHeader(Id: Integer): TPair<TPerson, string>;
var
  LResponse: IWiRLResponse;
  LPerson: TPerson;
  LContentType: string;
  LCustomHeader: string;
begin
  // If I want an object but I need the raw response too
  // I can get the object from the response

  // 1. Get the raw response (IWiRLResponse)
  LResponse := WiRLClientApplication1
    .Resource('helloworld/person')
    .Accept('application/json')
    .AcceptLanguage('it_IT')
    .Header('x-author', 'Luca')
    .QueryParam('Id', Id.ToString)
    .Get<IWiRLResponse>;

  // 2. Read the header(s) from the response
  LContentType := LResponse.ContentType;
  LCustomHeader := LResponse.Headers.Values['X-Custom-Header'];

  // 3. Read the object (TPerson) from the response
  LPerson := LResponse.Content.AsType<TPerson>;

  Result := TPair<TPerson, string>.Create(LPerson, LContentType);
end;

function TMainDataModule.GetPersonOrError(Id: Integer): TPerson;
var
  LReponse: IWiRLResponse;
begin
  LReponse := WiRLClientApplication1
    .Resource('helloworld/person')
    .Accept('application/json')
    .QueryParam('Id', Id.ToString)
    // Doesn't raise any axception in case of HTTP errors (400, 500)
    .DisableProtocolException
    .Get<IWiRLResponse>;

  case LReponse.Status of
    TWiRLResponseStatus.Success:
      Exit(LReponse.Content.AsType<TPerson>);
    TWiRLResponseStatus.ClientError:
      raise Exception.CreateFmt('Client Error: %d', [LReponse.StatusCode]);
    TWiRLResponseStatus.ServerError:
      raise Exception.CreateFmt('Server Error: %d', [LReponse.StatusCode]);
    else
      raise Exception.CreateFmt('Unknwn Error (Status: [%s])', [LReponse.Status.ToString]);
  end;
end;

function TMainDataModule.GetRawResponse: string;
var
  LResponse: IWiRLResponse;
begin
  ReverseStringResource.PathParam('AString', 'Hello, World!');
  LResponse := ReverseStringResource.GET<IWiRLResponse>;
  Result :=
    'StatusCode: ' + LResponse.StatusCode.ToString + sLineBreak +
    'ReasonString: ' + LResponse.StatusText + sLineBreak +
    'Content: ' + LResponse.ContentText;
end;

function TMainDataModule.PostMultiPart(const AFileName: string;
  LObject: TSimpleParam; const AString: string): string;
{$IFDEF HAS_NETHTTP_CLIENT}
var
  LFormData: TMultipartFormData;
begin
  LFormData := TMultipartFormData.Create();

  try
    LFormData.AddFile('AContent', AFileName);
    LFormData.AddField('AValue', AString);
    {$IFDEF HAS_FORM_DATA_FIELD_WITH_MEDIA_TYPE}
    LFormData.AddField('AJSON', TNeon.ObjectToJSONString(LObject), 'application/json');
    {$ELSE}
    LFormData.AddField('AJSON', TNeon.ObjectToJSONString(LObject));
    {$ENDIF}

    Result := WiRLClientApplication1
      .Resource('helloworld/multipart')
      .Accept('application/json')
      .ContentType(LFormData.MimeTypeHeader)
      .Header('x-author', 'Luca')
      .Post<TMultipartFormData, string>(LFormData);

  finally
    LFormData.Free;
  end;
end;
{$ELSE}
var
  LFormData: TIdMultiPartFormDataStream;
begin
  LFormData := TIdMultiPartFormDataStream.Create();
  try
    LFormData.AddFile('AContent', AFileName);
    LFormData.AddFormField('AValue', AString);
    LFormData.AddFormField('AJSON', TNeon.ObjectToJSONString(LObject), '', 'application/json');

    Result := WiRLClientApplication1
      .Resource('helloworld/multipart')
      .Accept('application/json')
      .ContentType('multipart/form-data; boundary=' + LFormData.Boundary)
      .Header('x-author', 'Luca')
      .Post<TIdMultiPartFormDataStream, string>(LFormData);

  finally
    LFormData.Free;
  end;
end;
{$ENDIF}

function TMainDataModule.PostOrder(AOrderProposal: TOrderProposal): TOrder;
begin
  Result := WiRLClientApplication1
    .Resource('helloworld/order')
    .Accept('application/json')
    .ContentType('application/json')
    .Header('x-author', 'Luca')
    .Post<TOrderProposal, TOrder>(AOrderProposal);
end;

function TMainDataModule.PostStream(AStream: TStream): string;
begin
  Result := PostStreamResource.Post<TStream, string>(AStream);
end;

function TMainDataModule.ReverseString(AString: string): string;
begin
  ReverseStringResource.PathParam('AString', AString);
  Result := ReverseStringResource.GET<string>;
end;

function TMainDataModule.TestException: string;
begin
  try
    Result := WiRLClientApplication1
      .Resource('helloworld/exception')
      .Accept('application/json')
      .Get<string>();
  except
    on E: EWiRLClientProtocolException do
    begin
      Result := E.Response.ContentText;
    end;
  end;
end;

procedure TMainDataModule.WiRLClient1BeforeCommand(ASender: TObject; ARequest:
    IWiRLRequest);
begin
  ARequest.Headers['X-App-Params'] := 'TestCustomHeader';
end;

end.
