{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Classes;

{$I WiRL.inc}

interface

uses
  System.SysUtils, System.Generics.Collections, System.Classes, System.NetEncoding,
  WiRL.Core.Declarations;

type
  EWiRLException = class(Exception);

  TNonInterfacedObject = class(TObject, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

  TUTF8EncodingNoBOM = class(TUTF8Encoding)
  public
    function GetPreamble: TBytes; override;
  end;

  TUnicodeLEEncodingNoBOM = class(TUnicodeEncoding)
  public
    function GetPreamble: TBytes; override;
  end;

  TUnicodeBEEncodingNoBOM = class(TBigEndianUnicodeEncoding)
  public
    function GetPreamble: TBytes; override;
  end;

  // Basic authentication helper
  TBasicAuth = record
  private const
    AUTH_BASIC = 'Basic ';
  private
    FUser: string;
    FPassword: string;
  public
    constructor Create(const AUser, APassword: string);
    property User: string read FUser;
    property Password: string read FPassword;

    class operator Implicit(AAuth: TBasicAuth): string;
    class operator Implicit(AAuth: string): TBasicAuth;
  end;

  // Bearer authentication helper
  TBearerAuth = record
  private const
    AUTH_BEARER = 'Bearer ';
  private
    FToken: string;
  public
    constructor Create(const AToken: string);
    property Token: string read FToken;

    class operator Implicit(AAuth: TBearerAuth): string;
    class operator Implicit(AAuth: string): TBearerAuth;
  end;

implementation

uses
  WiRL.Core.Utils, System.IOUtils;

{ TNonInterfacedObject }

function TNonInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TNonInterfacedObject._AddRef: Integer;
begin
  Result := -1;
end;

function TNonInterfacedObject._Release: Integer;
begin
  Result := -1;
end;

{ TUTF8EncodingNoBOM }

function TUTF8EncodingNoBOM.GetPreamble: TBytes;
begin
  SetLength(Result, 0);
end;

{ TUnicodeLEEncodingNoBOM }

function TUnicodeLEEncodingNoBOM.GetPreamble: TBytes;
begin
  SetLength(Result, 0);
end;

{ TUnicodeBEEncodingNoBOM }

function TUnicodeBEEncodingNoBOM.GetPreamble: TBytes;
begin
  SetLength(Result, 0);
end;

{ TBasicAuth }

constructor TBasicAuth.Create(const AUser, APassword: string);
begin
  FUser := AUser;
  FPassword := APassword;
end;

class operator TBasicAuth.Implicit(AAuth: TBasicAuth): string;
var
  LBase64Enc: TBase64Encoding;
begin
  LBase64Enc := TBase64Encoding.Create(0);
  try
    Result := AUTH_BASIC + LBase64Enc.Encode(AAuth.FUser + ':' + AAuth.FPassword);
  finally
    LBase64Enc.Free;
  end;
end;

class operator TBasicAuth.Implicit(AAuth: string): TBasicAuth;
var
  LAuthField: string;
  LColonIdx: Integer;
begin
  if not AAuth.StartsWith(AUTH_BASIC) then
    raise EWiRLException.Create('Authentication header error: wrong authentication type');

  LAuthField := AAuth.Substring(AUTH_BASIC.Length);
  LAuthField := TNetEncoding.Base64.Decode(LAuthField);
  LColonIdx := LAuthField.IndexOf(':');

  if LColonIdx <= 0 then
    raise EWiRLException.Create('Basic auth header error: wrong format');

  Result.FUser := LAuthField.Substring(0, LColonIdx);
  Result.FPassword := LAuthField.Substring(LColonIdx + 1, MaxInt);

end;

{ TBearerAuth }

constructor TBearerAuth.Create(const AToken: string);
begin
  FToken := AToken;
end;

class operator TBearerAuth.Implicit(AAuth: TBearerAuth): string;
begin
  Result := AUTH_BEARER + AAuth.FToken;
end;

class operator TBearerAuth.Implicit(AAuth: string): TBearerAuth;
begin
  if not AAuth.StartsWith(AUTH_BEARER) then
    raise EWiRLException.Create('Authentication header error: wrong authentication type');

  Result.FToken := AAuth.Substring(AUTH_BEARER.Length);
end;

end.
