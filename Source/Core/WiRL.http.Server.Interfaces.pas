{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2023 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Server.Interfaces;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,

  WiRL.Rtti.Utils,
  WiRL.Core.Classes,
  WiRL.Core.Exceptions,
  WiRL.Core.Singleton,
  WiRL.Core.Context,
  WiRL.Core.Context.Server,
  WiRL.http.Request,
  WiRL.http.Response;

type
  IWiRLListener = interface
  ['{04C4895A-23EB-46A5-98F0-B25292D7E6FC}']
    procedure HandleRequest(AContext: TWiRLContext; LRequest: TWiRLRequest; LResponse: TWiRLResponse);
  end;

  IWiRLServer = interface
  ['{7306FD2F-C6E3-472E-AF67-DF4E41583720}']
    procedure Startup;
    procedure Shutdown;

    // setter and getter
    function GetPort: Word;
    procedure SetPort(AValue: Word);
    function GetThreadPoolSize: Integer;
    procedure SetThreadPoolSize(AValue: Integer);
    function GetListener: IWiRLListener;
    procedure SetListener(AValue: IWiRLListener);
    function GetServerImplementation: TObject;

    // property
    property Port: Word read GetPort write SetPort;
    property ThreadPoolSize: Integer read GetThreadPoolSize write SetThreadPoolSize;
    property ServerImplementation: TObject read GetServerImplementation;
    property Listener: IWiRLListener read GetListener write SetListener;
  end;

  IWiRLServerFactory = interface
  ['{360F2B44-A164-4C34-A8AA-8E4B11C360B9}']
    function CreateRequest(AContext, ARequest: TObject): TWiRLRequest;
    function CreateResponse(AContext, AResponse: TObject): TWiRLResponse;
  end;

  TWiRLServerRegistry = class(TDictionary<string, TClass>)
  private type
    TWiRLServerRegistrySingleton = TWiRLSingleton<TWiRLServerRegistry>;
  protected
    class function GetInstance: TWiRLServerRegistry; static; inline;
  protected
    function GetDefaultClass: TClass;
  public
    constructor Create; virtual;
    function CreateServer(const AName: string): IWiRLServer;
    class property Instance: TWiRLServerRegistry read GetInstance;

    procedure RegisterServer<T: class>(const AName: string);
  end;

implementation

{ TWiRLServerRegistry }

constructor TWiRLServerRegistry.Create;
begin
  inherited Create;
end;

function TWiRLServerRegistry.CreateServer(const AName: string): IWiRLServer;
var
  LObject: TObject;
  AServerClass: TClass;
begin
  if Self.Count < 1 then
    raise EWiRLException.CreateFmt('CreateServer: no server registered (add "WiRL.http.Server.*" units to the project)', [AName]);

  if AName = '' then
    AServerClass := GetDefaultClass()
  else if not Self.TryGetValue(AName, AServerClass) then
    raise EWiRLException.CreateFmt('CreateServer: http server [%s] not registered (add "WiRL.http.Server.*" units to the project)', [AName]);

  LObject := TRttiHelper.CreateInstance(AServerClass);
  if not Supports(LObject, IWiRLServer, Result) then
    raise EWiRLException.CreateFmt('CreateServer: can''t create an http server of class [%s]', [AServerClass.ClassName]);
end;

function TWiRLServerRegistry.GetDefaultClass: TClass;
var
  LClassList: TArray<TPair<string,TClass>>;
begin
  LClassList := Self.ToArray;
  Result := LClassList[0].Value;
end;

class function TWiRLServerRegistry.GetInstance: TWiRLServerRegistry;
begin
  Result := TWiRLServerRegistrySingleton.Instance;
end;

procedure TWiRLServerRegistry.RegisterServer<T>(const AName: string);
begin
  if not Supports(TClass(T), IWiRLServer) then
    raise EWiRLException.Create(
      Format('Server registration error: [%s] is not a valid server', [TClass(T).QualifiedClassName])
    );

  Self.Add(AName, TClass(T));
end;


end.
