{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Client.Interfaces;

interface

uses
  System.Classes, System.SysUtils,

  WiRL.Rtti.Utils,
  WiRL.Core.Singleton,
  WiRL.http.Request,
  WiRL.http.Response;

type
  THttpProxyConnectionInfo = class(TPersistent)
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
    function GetProxyParams: THttpProxyConnectionInfo;
    procedure SetProxyParams(Value: THttpProxyConnectionInfo);

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
    property ProxyParams: THttpProxyConnectionInfo read GetProxyParams write SetProxyParams;
  end;

  TWiRLClientRegistry = class
  private type
    TWiRLClientRegistrySingleton = TWiRLSingleton<TWiRLClientRegistry>;
  private
    FClientClass: TClass;
  protected
    class function GetInstance: TWiRLClientRegistry; static; inline;
  public
    function CreateClient: IWiRLClient;
    class property Instance: TWiRLClientRegistry read GetInstance;

    procedure RegisterClient<T: class>;
  end;

implementation

{ TWiRLClientRegistry }

function TWiRLClientRegistry.CreateClient: IWiRLClient;
var
  LObject: TObject;
begin
  if not Assigned(FClientClass) then
    raise Exception.Create('CreateClient: http client class not registered (add "WiRL.Client.Indy" unit to the project)');

  LObject := TRttiHelper.CreateInstance(FClientClass);
  if not Supports(LObject, IWiRLClient, Result) then
    raise Exception.CreateFmt('CreateClient: can''t create a http client with class [%s]', [FClientClass.ClassName]);
end;

class function TWiRLClientRegistry.GetInstance: TWiRLClientRegistry;
begin
  Result := TWiRLClientRegistrySingleton.Instance;
end;

procedure TWiRLClientRegistry.RegisterClient<T>;
begin
  if not Supports(TClass(T), IWiRLClient) then
    raise Exception.Create(
      Format('Client registration error: [%s] is not a valid client', [TClass(T).QualifiedClassName])
    );

  FClientClass := TClass(T);
end;

{ THttpProxyConnectionInfo }

procedure THttpProxyConnectionInfo.AssignTo(Destination: TPersistent);
var
  LDest: THttpProxyConnectionInfo;
begin
  if Destination is THttpProxyConnectionInfo then
  begin
    LDest := THttpProxyConnectionInfo(Destination);
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

end.
