{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2022 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Auth.Context;

{$I WiRL.inc}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.SyncObjs, System.Rtti,

  WiRL.Core.JSON,
  JOSE.Types.Bytes,
  JOSE.Types.JSON,
  JOSE.Core.JWT,
  JOSE.Core.JWS,
  JOSE.Core.JWA,
  JOSE.Core.JWK,
  JOSE.Core.Builder;

type
  TWiRLSubject = class(TJWTClaims)
  private
    //const CLAIM_PREFIX = 'WiRL_';
    const CLAIM_APPID = 'aid';
    const CLAIM_USERID = 'uid';
    const CLAIM_ROLES = 'roles';
  private
    function GetRoles: string;
    function GetUserID: string;
    procedure SetRoles(const Value: string);
    procedure SetUserID(const Value: string);
    function GetAppID: string;
    procedure SetAppID(const Value: string);
  public
    constructor Create; override;

    procedure Clear; virtual;
    function HasRole(const ARole: string): Boolean; virtual;

    property Roles: string read GetRoles write SetRoles;
    property AppID: string read GetAppID write SetAppID;
    property UserID: string read GetUserID write SetUserID;
  end;

  TWiRLSubjectClass = class of TWiRLSubject;

  TWiRLAuthContext = class
  private
    FSubjectClass: TWiRLSubjectClass;
    FCompactToken: string;
    FVerified: Boolean;
    FSubject: TWiRLSubject;
    function GetExpired: Boolean;
  public
    {$IFNDEF HAS_HMAC_HASH}
    class constructor Create;
    {$ENDIF}

    constructor Create; overload;
    constructor Create(ASubjectClass: TWiRLSubjectClass); overload;
    destructor Destroy; override;

    procedure Clear;
    procedure Generate(const ASecret: TBytes);
    procedure Verify(const ACompactToken: string; ASecret: TBytes);
    procedure DeserializeOnly(const ACompactToken: string);

    property CompactToken: string read FCompactToken;
    property Subject: TWiRLSubject read FSubject write FSubject;
    /// <summary>
    ///   Indicates that the authentication object (es: jwt token) has been verified (signature)
    /// </summary>
    /// <remarks>
    ///   Beware: The claims has not been validated!
    /// </remarks>
    property Verified: Boolean read FVerified;
    property Expired: Boolean read GetExpired;
  end;

implementation

uses
  System.DateUtils,
  WiRL.Core.Exceptions;

{ TWiRLAuthContext }

{$IFNDEF HAS_HMAC_HASH}
class constructor TWiRLAuthContext.Create;
var
  LToken: TWiRLAuthContext;
  LBytes: TBytes;
begin
  LToken := TWiRLAuthContext.Create;
  try
    SetLength(LBytes, 5);
    LBytes[0] := 10;
    LBytes[1] := 20;
    LBytes[2] := 30;
    LBytes[3] := 40;
    LBytes[4] := 50;

    LToken.Generate(LBytes);
  finally
    LToken.Free;
  end;
end;
{$ENDIF}

procedure TWiRLAuthContext.Clear;
begin
  FVerified := False;
  FSubject.Clear;
end;

constructor TWiRLAuthContext.Create(ASubjectClass: TWiRLSubjectClass);
begin
  FSubjectClass := ASubjectClass;
  FSubject := FSubjectClass.Create;
end;

constructor TWiRLAuthContext.Create;
begin
  Create(TWiRLSubject);
end;

procedure TWiRLAuthContext.Generate(const ASecret: TBytes);
var
  LJWT: TJWT;
  LSigner: TJWS;
  LKey: TJWK;
begin
  LJWT := TJWT.Create(FSubjectClass);
  try
    TJSONHelper.JSONCopyFrom(FSubject.JSON, LJWT.Claims.JSON);

    LSigner := TJWS.Create(LJWT);
    LKey := TJWK.Create(ASecret);
    try
      // If you get an error here, please update the JWT library
      LSigner.SkipKeyValidation := True;
      LSigner.Sign(LKey, TJOSEAlgorithmId.HS256);

      FCompactToken := LSigner.CompactToken;
      FVerified := True;
    finally
      LKey.Free;
      LSigner.Free;
    end;
  finally
    LJWT.Free;
  end;
end;

function TWiRLAuthContext.GetExpired: Boolean;
begin
  Result := False;
  if Subject.HasExpiration then
    if Subject.Expiration < Now() then
      Result := True;
end;

procedure TWiRLAuthContext.Verify(const ACompactToken: string; ASecret: TBytes);
var
  LKey: TJWK;
  LJWT: TJWT;
begin
  Clear;
  if ACompactToken <> '' then
  begin
    FCompactToken := ACompactToken;
    LKey := TJWK.Create(ASecret);
    try
      LJWT := TJOSE.Verify(LKey, FCompactToken, FSubjectClass);
      if Assigned(LJWT) then
      begin
        try
          FVerified := LJWT.Verified;
          if FVerified then
            TJSONHelper.JSONCopyFrom(LJWT.Claims.JSON, FSubject.JSON);
        finally
          LJWT.Free;
        end;
      end;
    finally
      LKey.Free;
    end;
  end;
end;

procedure TWiRLAuthContext.DeserializeOnly(const ACompactToken: string);
var
  LJWT: TJWT;
begin
  Clear;
  if ACompactToken <> '' then
  begin
    FCompactToken := ACompactToken;
    LJWT := TJOSE.DeserializeOnly(FCompactToken, FSubjectClass);
    if Assigned(LJWT) then
    begin
      try
        TJSONHelper.JSONCopyFrom(LJWT.Claims.JSON, FSubject.JSON);
      finally
        LJWT.Free;
      end;
    end;
  end;
end;

destructor TWiRLAuthContext.Destroy;
begin
  FSubject.Free;
  inherited;
end;

procedure TWiRLSubject.Clear;
begin
  Roles := '';
  UserID := '';
  AppID := '';
end;

constructor TWiRLSubject.Create;
begin
  inherited;
end;

function TWiRLSubject.GetAppID: string;
begin
  Result := TJSONUtils.GetJSONValue(CLAIM_APPID, FJSON).AsString;
end;

function TWiRLSubject.GetRoles: string;
begin
  Result := TJSONUtils.GetJSONValue(CLAIM_ROLES, FJSON).AsString;
end;

function TWiRLSubject.GetUserID: string;
begin
  Result := TJSONUtils.GetJSONValue(CLAIM_USERID, FJSON).AsString;
end;

function TWiRLSubject.HasRole(const ARole: string): Boolean;
var
  LRoles: TArray<string>;
  LRole: string;
begin
  Result := False;
  LRoles := Roles.Split([',']);
  for LRole in LRoles do
    if SameText(ARole, LRole) then
    begin
      Result := True;
      Break;
    end;
end;

procedure TWiRLSubject.SetAppID(const Value: string);
begin
  if Value = '' then
    TJSONUtils.RemoveJSONNode(CLAIM_APPID, FJSON)
  else
    TJSONUtils.SetJSONValueFrom<string>(CLAIM_APPID, Value, FJSON);
end;

procedure TWiRLSubject.SetRoles(const Value: string);
begin
  if Value = '' then
    TJSONUtils.RemoveJSONNode(CLAIM_ROLES, FJSON)
  else
    TJSONUtils.SetJSONValueFrom<string>(CLAIM_ROLES, Value, FJSON);
end;

procedure TWiRLSubject.SetUserID(const Value: string);
begin
  if Value = '' then
    TJSONUtils.RemoveJSONNode(CLAIM_USERID, FJSON)
  else
    TJSONUtils.SetJSONValueFrom<string>(CLAIM_USERID, Value, FJSON);
end;

end.
