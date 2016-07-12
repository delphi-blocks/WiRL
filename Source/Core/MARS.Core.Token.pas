(*
  Copyright 2015-2016, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit MARS.Core.Token;

{$I MARS.inc}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.SyncObjs, System.Rtti, Web.HTTPApp, IdGlobal,
  MARS.Core.JSON,
  MARS.Core.Singleton,

  JOSE.Types.Bytes,
  JOSE.Types.JSON,
  JOSE.Core.JWT,
  JOSE.Core.JWS,
  JOSE.Core.JWA,
  JOSE.Core.JWK,
  JOSE.Core.Builder;

type
  TMARSSubject = class(TJWTClaims)
  private
    //const CLAIM_PREFIX = 'mars_';
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
    function HasRole(const ARole: string): Boolean; virtual;
    procedure SetUserAndRoles(const AUserName: string; const ARoles: TArray<string>); virtual;

    property Roles: string read GetRoles write SetRoles;
    property UserName: string read GetUserName write SetUserName;
    property DisplayName: string read GetDisplayName write SetDisplayName;
  end;

  TMARSSubjectClass = class of TMARSSubject;

  TMARSAuthContext = class
  private
    FAuthenticated: Boolean;
    FSubjectClass: TMARSSubjectClass;
    FCompactToken: string;
    FVerified: Boolean;
    FSubject: TMARSSubject;
  public
    {$IF CompilerVersion < 30}
    class constructor Create;
    {$ENDIF}

    constructor Create; overload;
    constructor Create(ASubjectClass: TMARSSubjectClass); overload;
    destructor Destroy; override;

    procedure Clear;
    procedure Generate(const ASecret: TBytes);
    procedure Verify(const ACompactToken: string; ASecret: TBytes);

    property Authenticated: Boolean read FAuthenticated write FAuthenticated;
    property Subject: TMARSSubject read FSubject write FSubject;
  end;

implementation

uses
  System.DateUtils, System.NetEncoding,
  MARS.Core.Utils;

{ TMARSAuthContext }

{$IF CompilerVersion < 30}
class constructor TMARSAuthContext.Create;
var
  LToken: TMARSAuthContext;
begin
  LToken := TMARSAuthContext.Create;
  try
    LToken.Generate([10, 20, 30, 40]);
  finally
    LToken.Free;
  end;
end;
{$ENDIF}

procedure TMARSAuthContext.Clear;
begin
  FVerified := False;
end;

constructor TMARSAuthContext.Create(ASubjectClass: TMARSSubjectClass);
begin
  FSubjectClass := ASubjectClass;
  FSubject := FSubjectClass.Create;
end;

constructor TMARSAuthContext.Create;
begin
  Create(TMARSSubject);
end;

procedure TMARSAuthContext.Generate(const ASecret: TBytes);
var
  LJWT: TJWT;
  LSigner: TJWS;
  LKey: TJWK;
begin
  LJWT := TJWT.Create(FSubjectClass);
  try
    LJWT.Claims.JSON.CopyFrom(FSubject.JSON);

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

procedure TMARSAuthContext.Verify(const ACompactToken: string; ASecret: TBytes);
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
            FSubject.JSON.CopyFrom(LJWT.Claims.JSON);
        finally
          LJWT.Free;
        end;
      end;
    finally
      LKey.Free;
    end;
  end;
end;

destructor TMARSAuthContext.Destroy;
begin
  FSubject.Free;
  inherited;
end;

constructor TMARSSubject.Create;
begin
  inherited;
end;

function TMARSSubject.GetDisplayName: string;
begin
  Result := TJSONUtils.GetJSONValue(CLAIM_NAME, FJSON).AsString;
end;

function TMARSSubject.GetRoles: string;
begin
  Result := TJSONUtils.GetJSONValue(CLAIM_ROLES, FJSON).AsString;
end;

function TMARSSubject.GetUserName: string;
begin
  Result := TJSONUtils.GetJSONValue(CLAIM_USERNAME, FJSON).AsString;
end;

function TMARSSubject.HasRole(const ARole: string): Boolean;
var
  LRoles: TArray<string>;
  LRole: string;
begin
  Result := False;
  LRoles := Roles.Split([',']);
  for LRole in LRoles do
    Result := SameText(ARole, LRole);
end;

procedure TMARSSubject.SetDisplayName(const Value: string);
begin
  if Value = '' then
    TJSONUtils.RemoveJSONNode(CLAIM_NAME, FJSON)
  else
    TJSONUtils.SetJSONValueFrom<string>(CLAIM_NAME, Value, FJSON);
end;

procedure TMARSSubject.SetRoles(const Value: string);
begin
  if Value = '' then
    TJSONUtils.RemoveJSONNode(CLAIM_ROLES, FJSON)
  else
    TJSONUtils.SetJSONValueFrom<string>(CLAIM_ROLES, Value, FJSON);
end;

procedure TMARSSubject.SetUserName(const Value: string);
begin
  if Value = '' then
    TJSONUtils.RemoveJSONNode(CLAIM_USERNAME, FJSON)
  else
    TJSONUtils.SetJSONValueFrom<string>(CLAIM_USERNAME, Value, FJSON);
end;

procedure TMARSSubject.SetUserAndRoles(const AUserName: string; const ARoles: TArray<string>);
begin
  UserName := AUserName;
  Roles.Join(',', ARoles);
end;

end.
