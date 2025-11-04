{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Tests.Mock.Server;

interface

uses
  System.Classes, System.SysUtils, System.RegularExpressions,
  System.Json, System.NetEncoding,

  WiRL.http.Server,
  WiRL.http.Server.Interfaces,
  WiRL.http.Core,
  WiRL.http.Headers,
  WiRL.http.Accept.MediaType,
  WiRL.Engine.REST,
  WiRL.http.Cookie,
  WiRL.http.Response,
  WiRL.http.Request,
  WiRL.Core.Context;

type
  TWiRLResponseError = class(TObject)
  private
    FMessage: string;
    FStatus: string;
    FException: string;
  public
    property Message: string read FMessage write FMessage;
    property Status: string read FStatus write FStatus;
    property Exception: string read FException write FException;
  end;

  TWiRLTestServer = class(TInterfacedObject, IWiRLServer)
  public
    { IWiRLServer }
    procedure Startup;
    procedure Shutdown;
    function GetPort: Word;
    procedure SetPort(AValue: Word);
    function GetThreadPoolSize: Integer;
    procedure SetThreadPoolSize(AValue: Integer);
    function GetListener: IWiRLListener;
    procedure SetListener(AValue: IWiRLListener);
    function GetServerImplementation: TObject;


    constructor Create;
    destructor Destroy; override;
  end;

  {
  TWiRLTestServer = class(TObject)
  private
    FEngine: TWiRLRESTEngine;
    FActive: Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure DoCommand(ARequest: TWiRLRequest; AResponse: TWiRLResponse);
    function ConfigureEngine(const ABasePath: string): TWiRLRESTEngine;
    property Engine: TWiRLRESTEngine read FEngine;
    property Active: Boolean read FActive write FActive;
  end;
  }

  TWiRLTestResponse = class(TWiRLResponse)
  private
    FContentStream: TStream;
    FStatusCode: Integer;
    FReasonString: string;
    FResponseError: TWiRLResponseError;
    FHeadersSent: Boolean;
    FHeader: IWiRLHeaders;
    FConnection: TWiRLConnection;
  protected
    function GetContentStream: TStream; override;
    procedure SetContentStream(const Value: TStream); override;
    function GetStatusCode: Integer; override;
    procedure SetStatusCode(const Value: Integer); override;
    function GetReasonString: string; override;
    procedure SetReasonString(const Value: string); override;
    function GetHeaders: IWiRLHeaders; override;
    function GetConnection: TWiRLConnection; override;
  public
    procedure SendHeaders(AImmediate: Boolean = False); override;
    property HeadersSent: Boolean read FHeadersSent;
    constructor Create;
    destructor Destroy; override;
  end;

  TWiRLTestRequest = class(TWiRLRequest)
  private
    FCookieFields: TWiRLCookies;
    FQueryFields: TWiRLParam;
    FContentFields: TWiRLParam;
    FUrl: string;
    FProtocol: string;
    FHost: string;
    FPathInfo: string;
    FRawPathInfo: string;
    FQuery: string;
    FServerPort: Integer;
    FContentStream: TStream;
    FHeaders: IWiRLHeaders;
    FConnection: TWiRLConnection;
    procedure ParseQueryParams;
    procedure SetUrl(const Value: string);
  protected
    function GetHttpPathInfo: string; override;
    function GetHttpQuery: string; override;
    function GetServerPort: Integer; override;
    function GetQueryFields: TWiRLParam; override;
    function GetContentFields: TWiRLParam; override;
    function GetCookieFields: TWiRLCookies; override;
    function GetContentStream: TStream; override;
    procedure SetContentStream(const Value: TStream); override;
    function GetHeaders: IWiRLHeaders; override;
    function GetRemoteIP: string; override;
    function GetConnection: TWiRLConnection; override;
  public
    property Url: string read FUrl write SetUrl;
    constructor Create;
    destructor Destroy; override;
  end;

  TWiRLTestConnection = class(TWiRLConnection)
  private
    //FConnection: TXXXConnection;
  public
    procedure Write(AValue: TBytes; const ALength: Integer = -1; const AOffset: Integer = 0); overload; override;
    procedure Write(const AValue: string; AEncoding: TEncoding = nil); overload; override;
    procedure WriteLn(const AValue: string); override;
    procedure WriteLn(); override;
    function Connected: Boolean; override;

    constructor Create;
  end;

implementation

{ TWiRLTestServer }

{
function TWiRLTestServer.ConfigureEngine(const ABasePath: string): TWiRLRESTEngine;
begin
  FEngine.SetBasePath(ABasePath);
  Result := FEngine;
end;

constructor TWiRLTestServer.Create;
begin
  FEngine := TWiRLRESTEngine.Create(nil);
end;

destructor TWiRLTestServer.Destroy;
begin
  FEngine.Free;
  inherited;
end;

procedure TWiRLTestServer.DoCommand(ARequest: TWiRLRequest;
  AResponse: TWiRLResponse);
var
  LContext: TWiRLContext;
  LContentJson: TJSONValue;
begin
  inherited;

  LContext := TWiRLContext.Create;
  try
    LContext.Engine := FEngine;
    LContext.Request := ARequest;
    LContext.Response := AResponse;

    ARequest.ContentStream.Position := 0;

    FEngine.HandleRequest(LContext);
//    AResponseInfo.CustomHeaders.AddStrings(LContext.Response.CustomHeaders);
    if AResponse.StatusCode <> 200 then
    begin
      if AResponse.ContentType = TMediaType.APPLICATION_JSON then
      begin
        LContentJson := TJSONObject.ParseJSONValue(AResponse.Content);
        try
          (AResponse as TWiRLTestResponse).Error.Message := LContentJson.GetValue<string>('message');
          (AResponse as TWiRLTestResponse).Error.Status := LContentJson.GetValue<string>('status');
          (AResponse as TWiRLTestResponse).Error.Exception := LContentJson.GetValue<string>('exception');
        finally
          LContentJson.Free;
        end;
        //raise TWiRLTestException.Create(LMessage, LStatus, LException);
      end
      else
        raise Exception.Create(IntToStr(AResponse.StatusCode) + ' - ' + AResponse.ReasonString);
    end;

  finally
    LContext.Free;
  end;
end;
}

{ TWiRLTestServer }

constructor TWiRLTestServer.Create;
begin

end;

destructor TWiRLTestServer.Destroy;
begin

  inherited;
end;

function TWiRLTestServer.GetListener: IWiRLListener;
begin

end;

function TWiRLTestServer.GetPort: Word;
begin
  Result := 80;
end;

function TWiRLTestServer.GetServerImplementation: TObject;
begin
  Result := nil;
end;

function TWiRLTestServer.GetThreadPoolSize: Integer;
begin
  Result := 15;
end;

procedure TWiRLTestServer.SetListener(AValue: IWiRLListener);
begin

end;

procedure TWiRLTestServer.SetPort(AValue: Word);
begin

end;

procedure TWiRLTestServer.SetThreadPoolSize(AValue: Integer);
begin

end;

procedure TWiRLTestServer.Shutdown;
begin

end;

procedure TWiRLTestServer.Startup;
begin

end;

{ TWiRLTestRequest }

constructor TWiRLTestRequest.Create;
begin
  inherited;
  FContentStream := TMemoryStream.Create;
  FCookieFields := TWiRLCookies.Create;
  FQueryFields := TWiRLParam.Create;
  FContentFields := TWiRLParam.Create;
  FConnection := TWiRLTestConnection.Create;
  FMethod := 'GET';
  FServerPort := 80;
end;

destructor TWiRLTestRequest.Destroy;
begin
  FCookieFields.Free;
  FQueryFields.Free;
  FContentFields.Free;
  FContentStream.Free;
  FConnection.Free;
  inherited;
end;

function TWiRLTestRequest.GetConnection: TWiRLConnection;
begin
  Result := FConnection;
end;

function TWiRLTestRequest.GetContentFields: TWiRLParam;
begin
  Result := FContentFields;
end;

function TWiRLTestRequest.GetContentStream: TStream;
begin
  Result := FContentStream;
end;

function TWiRLTestRequest.GetCookieFields: TWiRLCookies;
begin
  Result := FCookieFields;
end;

function TWiRLTestRequest.GetHeaders: IWiRLHeaders;
begin
  if not Assigned(FHeaders) then
  begin
    FHeaders := TWiRLHeaders.Create;
  end;
  Result := FHeaders;
end;

function TWiRLTestRequest.GetHttpPathInfo: string;
begin
  Result := FPathInfo;
end;

function TWiRLTestRequest.GetHttpQuery: string;
begin
  Result := FQuery;
end;

function TWiRLTestRequest.GetQueryFields: TWiRLParam;
begin
  Result := FQueryFields;
end;

function TWiRLTestRequest.GetRemoteIP: string;
begin
  Result := '127.0.0.1';
end;

function TWiRLTestRequest.GetServerPort: Integer;
begin
  Result := FServerPort;
end;

procedure TWiRLTestRequest.ParseQueryParams;
var
  Params: TArray<string>;
  Param: string;
  EqualIndex: Integer;
begin
  FQueryFields.Clear;
  if FQuery <> '' then
  begin
    Params := FQuery.Split(['&']);
    for Param in Params do
    begin
      // I can't use split: I need only the first equal symbol
      EqualIndex := Param.IndexOf('=');
      if EqualIndex > 0 then
      begin
        {$IFDEF CompilerVersion >=28} //XE7
          FQueryFields.AddPair(TNetEncoding.URL.Decode(Param.Substring(0, EqualIndex)), TNetEncoding.URL.Decode(Param.Substring(EqualIndex + 1)));
        {$ELSE}
          FQueryFields.Add(
            TNetEncoding.URL.Decode(Param.Substring(0, EqualIndex)) + '=' +
            TNetEncoding.URL.Decode(Param.Substring(EqualIndex + 1))
          );
        {$ENDIF}
      end;
    end;
  end;

end;

procedure TWiRLTestRequest.SetContentStream(const Value: TStream);
begin
  inherited;
  if Assigned(FContentStream) then
    FContentStream.Free;
  FContentStream := Value;
end;

procedure TWiRLTestRequest.SetUrl(const Value: string);
const
  Pattern = '(https{0,1}):\/\/([^\/]+)(\/[^?\n]*)\?*(.*)';
var
  LRegEx: TRegEx;
  LMatch: TMatch;
  LPortIndex: Integer;
begin
  FUrl := Value;
  LRegEx := TRegEx.Create(Pattern, [roIgnoreCase, roMultiLine]);
  LMatch := LRegEx.Match(FUrl);
  if LMatch.Groups.Count > 1 then
    FProtocol := LMatch.Groups[1].Value;
  if LMatch.Groups.Count > 2 then
    FHost := LMatch.Groups[2].Value;
  if LMatch.Groups.Count > 3 then
  begin
    FPathInfo := LMatch.Groups[3].Value;
    FRawPathInfo := LMatch.Groups[3].Value;
  end;
  if LMatch.Groups.Count > 4 then
    FQuery := LMatch.Groups[4].Value;

  LPortIndex := FHost.IndexOf(':');
  if LPortIndex >= 0 then
    FServerPort := FHost.Substring(LPortIndex + 1).ToInteger
  else
    FServerPort := 80;

  ParseQueryParams;
end;

{ TWiRLTestResponse }

constructor TWiRLTestResponse.Create;
begin
  inherited;
  FResponseError := TWiRLResponseError.Create;
  FHeader := TWiRLHeaders.Create;
  FConnection := TWiRLTestConnection.Create;

  FStatusCode := 200;
  FReasonString := 'OK';
  FHeadersSent := False;
end;

destructor TWiRLTestResponse.Destroy;
begin
  FResponseError.Free;
  FContentStream.Free;
  FConnection.Free;
  inherited;
end;

function TWiRLTestResponse.GetConnection: TWiRLConnection;
begin
  Result := FConnection;
end;

function TWiRLTestResponse.GetContentStream: TStream;
begin
  if not Assigned(FContentStream) then
    FContentStream := TMemoryStream.Create;

  Result := FContentStream;
end;

function TWiRLTestResponse.GetHeaders: IWiRLHeaders;
begin
  Result := FHeader;
end;

function TWiRLTestResponse.GetReasonString: string;
begin
  Result := FReasonString;
end;

function TWiRLTestResponse.GetStatusCode: Integer;
begin
  Result := FStatusCode;
end;

procedure TWiRLTestResponse.SendHeaders(AImmediate: Boolean);
begin
  inherited;
  FHeadersSent := True;
end;

procedure TWiRLTestResponse.SetContentStream(const Value: TStream);
begin
  inherited;
  if Assigned(FContentStream) then
    FContentStream.Free;

  FContentStream := Value;
end;

procedure TWiRLTestResponse.SetReasonString(const Value: string);
begin
  inherited;
  FReasonString := Value;
end;

procedure TWiRLTestResponse.SetStatusCode(const Value: Integer);
begin
  inherited;
  FStatusCode := Value;
end;

{ TWiRLTestConnection }

function TWiRLTestConnection.Connected: Boolean;
begin
  Result := True;
end;

constructor TWiRLTestConnection.Create;
begin

end;

procedure TWiRLTestConnection.Write(const AValue: string; AEncoding: TEncoding);
begin

end;

procedure TWiRLTestConnection.Write(AValue: TBytes; const ALength,
  AOffset: Integer);
begin

end;

procedure TWiRLTestConnection.WriteLn(const AValue: string);
begin

end;

procedure TWiRLTestConnection.WriteLn;
begin

end;

initialization

  TWiRLServerRegistry.Instance.RegisterServer<TWiRLTestServer>('TWiRLTestServer (Test)');

end.
