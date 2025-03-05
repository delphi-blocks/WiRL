{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Headers;

interface

uses
  System.Classes, System.SysUtils, Generics.Collections;

type
  TWiRLHeader = class(TObject)
  public
    const ACCEPT = 'Accept';
    const ACCEPT_LANGUAGE = 'Accept-Language';
    const ACCEPT_CHARSET = 'Accept-Charset';
    const ACCEPT_ENCODING = 'Accept-Encoding';
    const CONTENT_TYPE = 'Content-Type';
    const AUTHORIZATION = 'Authorization';
    const USER_AGENT = 'User-Agent';
    const CONTENT_LENGTH = 'Content-Length';
    const HOST = 'Host';
    const ALLOW = 'Allow';
    const CONNECTION = 'Connection';
    const CONTENT_ENCODING = 'Content-Encoding';
    const CONTENT_LANGUAGE = 'Content-Language';
    const LOCATION = 'Location';
    const WWW_AUTHENTICATE = 'WWW-Authenticate';
    const TRANSFER_ENCODING = 'Transfer-Encoding';
  public
    Name: string;
    Value: string;
    constructor Create(const AName, AValue: string);
  end;

  IWiRLHeaders = interface
  ['{090D3CBF-D3FE-49B5-BB57-7DDD7271E765}']
    function GetValue(const AName: string): string;
    procedure SetValue(const AName, AValue: string);
    function GetAccept: string;
    function GetAcceptCharSet: string;
    function GetAcceptEncoding: string;
    function GetAcceptLanguage: string;
    function GetUserAgent: string;
    function GetContentType: string;
    function GetAuthorization: string;
    function GetAllow: string;
    function GetConnection: string;
    function GetContentEncoding: string;
    function GetContentLanguage: string;
    function GetContentLength: Int64;
    function GetLocation: string;
    function GetWWWAuthenticate: string;
    function GetTransferEncoding: string;
    procedure SetAccept(const AValue: string);
    procedure SetAcceptCharSet(const AValue: string);
    procedure SetAcceptEncoding(const AValue: string);
    procedure SetAcceptLanguage(const AValue: string);
    procedure SetAuthorization(const AValue: string);
    procedure SetContentType(const AValue: string);
    procedure SetUserAgent(const AValue: string);
    procedure SetAllow(const AValue: string);
    procedure SetConnection(const AValue: string);
    procedure SetContentEncoding(const AValue: string);
    procedure SetContentLanguage(const AValue: string);
    procedure SetContentLength(const AValue: Int64);
    procedure SetLocation(const AValue: string);
    procedure SetWWWAuthenticate(const AValue: string);
    procedure SetTransferEncoding(const AValue: string);

    procedure Clear;
    function Count: Integer;
    procedure AddHeader(AHeader: TWiRLHeader);
    function GetEnumerator: TEnumerator<TWiRLHeader>;
    property Values[const AName: string]: string read GetValue write SetValue; default;
    procedure Assign(AHeaders: IWiRLHeaders);

    /// <summary>Media type(s) that is/are acceptable for the response</summary>
    property Accept: string read GetAccept write SetAccept;
    /// <summary>Character sets that are acceptable</summary>
    property AcceptCharSet: string read GetAcceptCharSet write SetAcceptCharSet;
    /// <summary>List of acceptable encodings</summary>
    property AcceptEncoding: string read GetAcceptEncoding write SetAcceptEncoding;
    /// <summary>List of acceptable human languages for response</summary>
    property AcceptLanguage: string read GetAcceptLanguage write SetAcceptLanguage;
    /// <summary>The user agent string of the user agent</summary>
    property UserAgent: string read GetUserAgent write SetUserAgent;
    /// <summary>The MIME type of this content</summary>
    property ContentType: string read GetContentType write SetContentType;
    /// <summary>Authentication credentials for HTTP authentication</summary>
    property Authorization: string read GetAuthorization write SetAuthorization;
    /// <summary>Valid methods for a specified resource. To be used for a 405 Method not allowed</summary>
    property Allow: string read GetAllow write SetAllow;
    /// <summary>Control options for the current connection and list of hop-by-hop response fields</summary>
    property Connection: string read GetConnection write SetConnection;
    /// <summary>The type of encoding used on the data</summary>
    property ContentEncoding: string read GetContentEncoding write SetContentEncoding;
    /// <summary>The natural language or languages of the intended audience for the enclosed content</summary>
    property ContentLanguage: string read GetContentLanguage write SetContentLanguage;
    /// <summary>The length of the response body in octets (8-bit bytes)</summary>
    property ContentLength: Int64 read GetContentLength write SetContentLength;
    /// <summary>Used in redirection, or when a new resource has been created</summary>
    property Location: string read GetLocation write SetLocation;
    /// <summary>Indicates the authentication scheme that should be used to access the requested entity</summary>
    property WWWAuthenticate: string read GetWWWAuthenticate write SetWWWAuthenticate;
    /// <summary>Specifies the form of encoding used to transfer messages between nodes on the network</summary>
    property TransferEncoding: string read GetTransferEncoding write SetTransferEncoding;
  end;

  TWiRLHeaders = class(TObjectList<TWiRLHeader>, IWiRLHeaders)
  private
    [Volatile] FRefCount: Integer;
    function FindHeader(const AName: string): TWiRLHeader;

    { IWiRLHeader }
    function Count: Integer;
    function GetAccept: string;
    function GetAcceptCharSet: string;
    function GetAcceptEncoding: string;
    function GetAcceptLanguage: string;
    function GetAllow: string;
    function GetAuthorization: string;
    function GetConnection: string;
    function GetContentEncoding: string;
    function GetContentLanguage: string;
    function GetContentLength: Int64;
    function GetContentType: string;
    function GetLocation: string;
    function GetUserAgent: string;
    function GetWWWAuthenticate: string;
    function GetTransferEncoding: string;

    procedure SetAccept(const AValue: string);
    procedure SetAcceptCharSet(const AValue: string);
    procedure SetAcceptEncoding(const AValue: string);
    procedure SetAcceptLanguage(const AValue: string);
    procedure SetAllow(const AValue: string);
    procedure SetAuthorization(const AValue: string);
    procedure SetConnection(const AValue: string);
    procedure SetContentEncoding(const AValue: string);
    procedure SetContentLanguage(const AValue: string);
    procedure SetContentLength(const AValue: Int64);
    procedure SetContentType(const AValue: string);
    procedure SetLocation(const AValue: string);
    procedure SetUserAgent(const AValue: string);
    procedure SetWWWAuthenticate(const AValue: string);
    procedure SetTransferEncoding(const AValue: string);

    procedure AddHeader(AHeader: TWiRLHeader);
    procedure Assign(AHeaders: IWiRLHeaders);

    { IInterface }
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;

  protected
    procedure SetValue(const AName, AValue: string); virtual;
    function GetValue(const AName: string): string; virtual;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function GetEnumerator: TEnumerator<TWiRLHeader>; reintroduce;

    constructor Create;
  public
    class function NewInstance: TObject; override;
    class function FromArray(AHeaders: TArray<TWiRLHeader>): IWiRLHeaders;
  end;

implementation

{ TWiRLHeader }

constructor TWiRLHeader.Create(const AName, AValue: string);
begin
  inherited Create;
  Name := AName;
  Value := AValue;
end;

{ TWiRLHeaders }

procedure TWiRLHeaders.AddHeader(AHeader: TWiRLHeader);
begin
  Add(AHeader);
end;

procedure TWiRLHeaders.AfterConstruction;
begin
  inherited;
  AtomicDecrement(FRefCount);
end;

procedure TWiRLHeaders.Assign(AHeaders: IWiRLHeaders);
var
  LHeader: TWiRLHeader;
begin
  Clear;
  for LHeader in AHeaders do
  begin
    AddHeader(TWiRLHeader.Create(LHeader.Name, LHeader.Value));
  end;
end;

procedure TWiRLHeaders.BeforeDestruction;
begin
  inherited;
  if FRefCount <> 0 then
    System.Error(reInvalidPtr);
end;

function TWiRLHeaders.Count: Integer;
begin
  Result := inherited Count;
end;

constructor TWiRLHeaders.Create;
begin
  inherited Create(True);
end;

class function TWiRLHeaders.FromArray(
  AHeaders: TArray<TWiRLHeader>): IWiRLHeaders;
var
  LHeader: TWiRLHeader;
begin
  Result := TWiRLHeaders.Create;
  for LHeader in AHeaders do
    Result.AddHeader(LHeader);
end;

function TWiRLHeaders.GetAccept: string;
begin
  Result := GetValue(TWiRLHeader.ACCEPT);
end;

function TWiRLHeaders.GetAcceptCharSet: string;
begin
  Result := GetValue(TWiRLHeader.ACCEPT_CHARSET);
end;

function TWiRLHeaders.GetAcceptEncoding: string;
begin
  Result := GetValue(TWiRLHeader.ACCEPT_ENCODING);
end;

function TWiRLHeaders.GetAcceptLanguage: string;
begin
  Result := GetValue(TWiRLHeader.ACCEPT_LANGUAGE);
end;

function TWiRLHeaders.GetAllow: string;
begin
  Result := GetValue(TWiRLHeader.ALLOW);
end;

function TWiRLHeaders.GetAuthorization: string;
begin
  Result := GetValue(TWiRLHeader.AUTHORIZATION);
end;

function TWiRLHeaders.GetConnection: string;
begin
  Result := GetValue(TWiRLHeader.CONNECTION);
end;

function TWiRLHeaders.GetContentEncoding: string;
begin
  Result := GetValue(TWiRLHeader.CONTENT_ENCODING);
end;

function TWiRLHeaders.GetContentLanguage: string;
begin
  Result := GetValue(TWiRLHeader.CONTENT_LANGUAGE);
end;

function TWiRLHeaders.GetContentLength: Int64;
begin
  Result := StrToInt64Def(GetValue(TWiRLHeader.CONTENT_LENGTH), -1);
end;

function TWiRLHeaders.GetContentType: string;
begin
  Result := GetValue(TWiRLHeader.CONTENT_TYPE);
end;

function TWiRLHeaders.GetEnumerator: TEnumerator<TWiRLHeader>;
begin
  Result := inherited GetEnumerator;
end;

function TWiRLHeaders.FindHeader(const AName: string): TWiRLHeader;
var
  LHeader: TWiRLHeader;
begin
  Result := nil;
  for LHeader in Self do
    if SameText(LHeader.Name, AName) then
      Exit(LHeader);
end;

function TWiRLHeaders.GetLocation: string;
begin
  Result := GetValue(TWiRLHeader.LOCATION);
end;

function TWiRLHeaders.GetTransferEncoding: string;
begin
  Result := GetValue(TWiRLHeader.TRANSFER_ENCODING);
end;

function TWiRLHeaders.GetUserAgent: string;
begin
  Result := GetValue(TWiRLHeader.USER_AGENT);
end;

function TWiRLHeaders.GetValue(const AName: string): string;
var
  LHeader: TWiRLHeader;
begin
  Result := '';
  LHeader := FindHeader(AName);
  if Assigned(LHeader) then
      Exit(LHeader.Value);
end;

function TWiRLHeaders.GetWWWAuthenticate: string;
begin
  Result := GetValue(TWiRLHeader.WWW_AUTHENTICATE);
end;

class function TWiRLHeaders.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TWiRLHeaders(Result).FRefCount := 1;
end;

function TWiRLHeaders.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TWiRLHeaders.SetAccept(const AValue: string);
begin
  SetValue(TWiRLHeader.ACCEPT, AValue);
end;

procedure TWiRLHeaders.SetAcceptCharSet(const AValue: string);
begin
  SetValue(TWiRLHeader.ACCEPT_CHARSET, AValue);
end;

procedure TWiRLHeaders.SetAcceptEncoding(const AValue: string);
begin
  SetValue(TWiRLHeader.ACCEPT_ENCODING, AValue);
end;

procedure TWiRLHeaders.SetAcceptLanguage(const AValue: string);
begin
  SetValue(TWiRLHeader.ACCEPT_LANGUAGE, AValue);
end;

procedure TWiRLHeaders.SetAllow(const AValue: string);
begin
  SetValue(TWiRLHeader.ALLOW, AValue);
end;

procedure TWiRLHeaders.SetAuthorization(const AValue: string);
begin
  SetValue(TWiRLHeader.AUTHORIZATION, AValue);
end;

procedure TWiRLHeaders.SetConnection(const AValue: string);
begin
  SetValue(TWiRLHeader.CONNECTION, AValue);
end;

procedure TWiRLHeaders.SetContentEncoding(const AValue: string);
begin
  SetValue(TWiRLHeader.CONTENT_ENCODING, AValue);
end;

procedure TWiRLHeaders.SetContentLanguage(const AValue: string);
begin
  SetValue(TWiRLHeader.CONTENT_LANGUAGE, AValue);
end;

procedure TWiRLHeaders.SetContentLength(const AValue: Int64);
begin
  SetValue(TWiRLHeader.CONTENT_LENGTH, IntToStr(AValue));
end;

procedure TWiRLHeaders.SetContentType(const AValue: string);
begin
  SetValue(TWiRLHeader.CONTENT_TYPE, AValue);
end;

procedure TWiRLHeaders.SetLocation(const AValue: string);
begin
  SetValue(TWiRLHeader.LOCATION, AValue);
end;

procedure TWiRLHeaders.SetTransferEncoding(const AValue: string);
begin
  SetValue(TWiRLHeader.TRANSFER_ENCODING, AValue);
end;

procedure TWiRLHeaders.SetUserAgent(const AValue: string);
begin
  SetValue(TWiRLHeader.USER_AGENT, AValue);
end;

procedure TWiRLHeaders.SetValue(const AName, AValue: string);
var
  LIndex: Integer;
begin
  for LIndex := 0 to Count - 1 do
  begin
    if SameText(Items[LIndex].Name, AName) then
    begin
      if AValue = '' then
        Delete(LIndex)
      else
        Items[LIndex].Value := AValue;
      Exit;
    end;
  end;
  Add(TWiRLHeader.Create(AName, AValue));
end;

procedure TWiRLHeaders.SetWWWAuthenticate(const AValue: string);
begin
  SetValue(TWiRLHeader.WWW_AUTHENTICATE, AValue);
end;

function TWiRLHeaders._AddRef: Integer;
begin
  Result := AtomicIncrement(FRefCount);
end;

function TWiRLHeaders._Release: Integer;
begin
  Result := AtomicDecrement(FRefCount);
  if Result = 0 then
  begin
    Destroy;
  end;
end;

end.
