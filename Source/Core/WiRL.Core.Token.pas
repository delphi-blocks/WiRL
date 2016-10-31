(*
  Copyright 2015-2016, WiRL - REST Library

  Home: https://github.com/WiRL-library

*)
unit WiRL.Core.Token;

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
    const CLAIM_NAME = 'name';
    const CLAIM_USERNAME = 'username';
    const CLAIM_ROLES = 'roles';
  private
    function GetRoles: string;
    function GetUserName: string;
    procedure SetRoles(const Value: string);
    procedure SetUserName(const Value: string);
    function GetDisplayName: string;
    procedure SetDisplayName(const Value: string);
  public
    constructor Create; override;

    procedure Clear; virtual;

    function HasRole(const ARole: string): Boolean; virtual;
    procedure SetUserAndRoles(const AUserName: string; const ARoles: TArray<string>); virtual;

    property Roles: string read GetRoles write SetRoles;
    property UserName: string read GetUserName write SetUserName;
    property DisplayName: string read GetDisplayName write SetDisplayName;
  end;

  TWiRLSubjectClass = class of TWiRLSubject;

  TWiRLAuthContext = class
  private
    FAuthenticated: Boolean;
    FSubjectClass: TWiRLSubjectClass;
    FCompactToken: string;
    FVerified: Boolean;
    FSubject: TWiRLSubject;
  public
    {$IF CompilerVersion < 30}
    class constructor Create;
    {$ENDIF}

    constructor Create; overload;
    constructor Create(ASubjectClass: TWiRLSubjectClass); overload;
    destructor Destroy; override;

    procedure Clear;
    procedure Generate(const ASecret: TBytes);
    procedure Verify(const ACompactToken: string; ASecret: TBytes);

    property CompactToken: string read FCompactToken;
    property Authenticated: Boolean read FAuthenticated write FAuthenticated;
    property Subject: TWiRLSubject read FSubject write FSubject;
  end;

implementation

uses
  {$IFDEF DelphiXE8_UP}
  System.NetEncoding,
  {$ENDIF}
  System.DateUtils;

{ TWiRLAuthContext }

{$IF CompilerVersion < 30}
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
  FAuthenticated := False;
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
      LSigner.Sign(LKey, HS256);

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

destructor TWiRLAuthContext.Destroy;
begin
  FSubject.Free;
  inherited;
end;

procedure TWiRLSubject.Clear;
begin
  Roles := '';
  UserName := '';
  DisplayName := '';
end;

constructor TWiRLSubject.Create;
begin
  inherited;
end;

function TWiRLSubject.GetDisplayName: string;
begin
  Result := TJSONUtils.GetJSONValue(CLAIM_NAME, FJSON).AsString;
end;

function TWiRLSubject.GetRoles: string;
begin
  Result := TJSONUtils.GetJSONValue(CLAIM_ROLES, FJSON).AsString;
end;

function TWiRLSubject.GetUserName: string;
begin
  Result := TJSONUtils.GetJSONValue(CLAIM_USERNAME, FJSON).AsString;
end;

function TWiRLSubject.HasRole(const ARole: string): Boolean;
var
  LRoles: TArray<string>;
  LRole: string;
begin
  Result := False;
  LRoles := Roles.Split([',']);
  for LRole in LRoles do
    Result := SameText(ARole, LRole);
end;

procedure TWiRLSubject.SetDisplayName(const Value: string);
begin
  if Value = '' then
    TJSONUtils.RemoveJSONNode(CLAIM_NAME, FJSON)
  else
    TJSONUtils.SetJSONValueFrom<string>(CLAIM_NAME, Value, FJSON);
end;

procedure TWiRLSubject.SetRoles(const Value: string);
begin
  if Value = '' then
    TJSONUtils.RemoveJSONNode(CLAIM_ROLES, FJSON)
  else
    TJSONUtils.SetJSONValueFrom<string>(CLAIM_ROLES, Value, FJSON);
end;

procedure TWiRLSubject.SetUserName(const Value: string);
begin
  if Value = '' then
    TJSONUtils.RemoveJSONNode(CLAIM_USERNAME, FJSON)
  else
    TJSONUtils.SetJSONValueFrom<string>(CLAIM_USERNAME, Value, FJSON);
end;

procedure TWiRLSubject.SetUserAndRoles(const AUserName: string; const ARoles: TArray<string>);
begin
  UserName := AUserName;
  Roles := Roles.Join(',', ARoles);
end;

end.
