{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Client.Interfaces;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,

  WiRL.Rtti.Utils,
  WiRL.Core.Exceptions,
  WiRL.Core.Singleton,
  WiRL.http.Request,
  WiRL.http.Response;

type
  EWiRLClientException = class(EWiRLException);

  EWiRLSocketException = class(EWiRLClientException);

  EWiRLClientProtocolException = class(EWiRLClientException)
  private
    FStatusCode: Integer;
  public
    constructor Create(const AStatusCode: Integer; const AMessage: string); reintroduce; virtual;
    property StatusCode: Integer read FStatusCode write FStatusCode;
  end;

  TWiRLProxyConnectionInfo = class(TPersistent)
  private
    FBasicByDefault: boolean;
    FProxyPort: Integer;
    FPassword: string;
    FUsername: string;
    FProxyServer: string;
  protected
    procedure AssignTo(Destination: TPersistent); override;
  published
    property BasicAuthentication: boolean read FBasicByDefault write FBasicByDefault;
    property ProxyPassword: string read FPassword write FPassword;
    property ProxyPort: Integer read FProxyPort write FProxyPort;
    property ProxyServer: string read FProxyServer write FProxyServer;
    property ProxyUsername: string read FUsername write FUserName;
  end;

  IWiRLClient = interface
  ['{A42C26F5-8B8B-4FE8-A3D4-EF12107F240B}']
    // Setters and getters
    function GetRequest: TWiRLRequest;
    function GetResponse: TWiRLResponse;
    function GetConnectTimeout: Integer;
    procedure SetConnectTimeout(Value: Integer);
    function GetReadTimeout: Integer;
    procedure SetReadTimeout(Value: Integer);
    function GetProxyParams: TWiRLProxyConnectionInfo;
    procedure SetProxyParams(Value: TWiRLProxyConnectionInfo);
    function GetMaxRedirects: Integer;
    procedure SetMaxRedirects(const Value: Integer);
    function GetClientImplementation: TObject;

    // Http methods
    procedure Delete(const AURL: string; AResponseContent: TStream);
    procedure Get(const AURL: string; AResponseContent: TStream);
    procedure Options(const AURL: string; AResponseContent: TStream);
    procedure Head(const AURL: string);
    procedure Patch(const AURL: string; AContent, AResponse: TStream);
    procedure Post(const AURL: string; AContent, AResponse: TStream);
    procedure Put(const AURL: string; AContent, AResponse: TStream);

    // Http properties
    property Request: TWiRLRequest read GetRequest;
    property Response: TWiRLResponse read GetResponse;
    property ConnectTimeout: Integer read GetConnectTimeout write SetConnectTimeout;
    property ReadTimeout: Integer read GetReadTimeout write SetReadTimeout;
    property ProxyParams: TWiRLProxyConnectionInfo read GetProxyParams write SetProxyParams;
    property MaxRedirects: Integer read GetMaxRedirects write SetMaxRedirects;

    property ClientImplementation: TObject read GetClientImplementation;
  end;

  TWiRLClientRegistry = class(TDictionary<string, TClass>)
  private type
    TWiRLClientRegistrySingleton = TWiRLSingleton<TWiRLClientRegistry>;
  protected
    class function GetInstance: TWiRLClientRegistry; static; inline;
  protected
    FDefaultClass: TClass;
    function GetDefaultClass: TClass;
    procedure SetDefaultClass(AClass: TClass);
  public
    constructor Create; virtual;
    class property Instance: TWiRLClientRegistry read GetInstance;

    function CreateClient(const AName: string): IWiRLClient;
    procedure RegisterClient<T: class>(const AName: string; ADefault: Boolean =  False);
  end;

implementation

{ TWiRLClientRegistry }

constructor TWiRLClientRegistry.Create;
begin
  inherited Create;
end;

function TWiRLClientRegistry.CreateClient(const AName: string): IWiRLClient;
var
  LObject: TObject;
  AClientClass: TClass;
begin
  if Self.Count < 1 then
    raise EWiRLException.CreateFmt('CreateClient: no client registered (add "WiRL.http.Client.*" unit to the project)', [AName]);

  if AName = '' then
    AClientClass := GetDefaultClass()
  else if not Self.TryGetValue(AName, AClientClass) then
    raise EWiRLException.CreateFmt('CreateClient: http client [%s] not registered (add "WiRL.http.Client.*" unit to the project)', [AName]);

  LObject := TRttiHelper.CreateInstance(AClientClass);
  if not Supports(LObject, IWiRLClient, Result) then
    raise EWiRLException.CreateFmt('CreateClient: can''t create a http client with class [%s]', [AClientClass.ClassName]);
end;

function TWiRLClientRegistry.GetDefaultClass: TClass;
var
  LClassList: TArray<TPair<string,TClass>>;
begin
  if Assigned(FDefaultClass) then
    Exit(FDefaultClass);
  LClassList := Self.ToArray;
  Result := LClassList[0].Value;
end;

class function TWiRLClientRegistry.GetInstance: TWiRLClientRegistry;
begin
  Result := TWiRLClientRegistrySingleton.Instance;
end;

procedure TWiRLClientRegistry.RegisterClient<T>(const AName: string; ADefault: Boolean = False);
begin
  if not Supports(TClass(T), IWiRLClient) then
    raise EWiRLException.Create(
      Format('Client registration error: [%s] is not a valid client', [TClass(T).QualifiedClassName])
    );

  Self.Add(AName, TClass(T));
  if ADefault then
    SetDefaultClass(TClass(T));
end;

procedure TWiRLClientRegistry.SetDefaultClass(AClass: TClass);
begin
  FDefaultClass := AClass;
end;

{ TWiRLProxyConnectionInfo }

procedure TWiRLProxyConnectionInfo.AssignTo(Destination: TPersistent);
var
  LDest: TWiRLProxyConnectionInfo;
begin
  if Destination is TWiRLProxyConnectionInfo then
  begin
    LDest := TWiRLProxyConnectionInfo(Destination);
    LDest.FPassword := FPassword;
    LDest.FProxyPort := FProxyPort;
    LDest.FProxyServer := FProxyServer;
    LDest.FUsername := FUsername;
    LDest.FBasicByDefault := FBasicByDefault;
  end else
  begin
    inherited AssignTo(Destination);
  end;
end;

{ EWiRLClientProtocolException }

constructor EWiRLClientProtocolException.Create(const AStatusCode: Integer;
  const AMessage: string);
begin
  inherited Create(AMessage);
end;

end.
