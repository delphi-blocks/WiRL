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

  TGCMemoryStream = class(TMemoryStream)
  end;

  // Basic authentication helper
  TBasicAuth = record
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
  private
    FToken: string;
  public
    constructor Create(const AToken: string);

    class operator Implicit(AAuth: TBearerAuth): string;
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
begin
  Result := 'Basic ' + TNetEncoding.Base64.Encode(AAuth.FUser + ':' + AAuth.FPassword);
end;

class operator TBasicAuth.Implicit(AAuth: string): TBasicAuth;
const
  AUTH_BASIC = 'Basic ';
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
  Result := 'Bearer ' + AAuth.FToken;
end;

end.
