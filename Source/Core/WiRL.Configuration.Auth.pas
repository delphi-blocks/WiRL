{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2022 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Configuration.Auth;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Defaults,
  WiRL.Configuration.Core;

{$SCOPEDENUMS ON}

type
  TAuthChallenge = (Basic, Digest, Bearer, Form);

  TAuthChallengeHelper = record helper for TAuthChallenge
    function ToString: string;
  end;

  TAuthTokenType = (JWT, Session, SASL);

  TAuthTokenLocation = (Bearer, Cookie, Header);

  IWiRLConfigurationAuth = interface(IWiRLConfiguration)
  ['{0C449E50-5CE6-43C0-9DA6-AB773EC1792F}']
    //function Configure: IWiRLConfigurationAuth;
    function SetAuthChallenge(AChallenge: TAuthChallenge; const ARealm: string): IWiRLConfigurationAuth;
    function SetAuthChallengeHeader(const AHeader: string): IWiRLConfigurationAuth;
    function SetTokenType(ALocation: TAuthTokenType): IWiRLConfigurationAuth;
    function SetTokenLocation(ALocation: TAuthTokenLocation): IWiRLConfigurationAuth;
    function SetTokenCustomHeader(const ACustomHeader: string): IWiRLConfigurationAuth;
  end;

  [Implements(IWiRLConfigurationAuth)]
  TWiRLConfigurationAuth = class sealed(TWiRLConfiguration, IWiRLConfigurationAuth)
  public
    function Configure: IWiRLConfigurationAuth;
    function SetAuthChallenge(AChallenge: TAuthChallenge; const ARealm: string): IWiRLConfigurationAuth;
    function SetAuthChallengeHeader(const AHeader: string): IWiRLConfigurationAuth;
    function SetTokenType(ATokenType: TAuthTokenType): IWiRLConfigurationAuth;
    function SetTokenLocation(ALocation: TAuthTokenLocation): IWiRLConfigurationAuth;
    function SetTokenCustomHeader(const ACustomHeader: string): IWiRLConfigurationAuth;
  private
    FTokenCustomHeader: string;
    FAuthChallenge: TAuthChallenge;
    FTokenLocation: TAuthTokenLocation;
    FRealm: string;
    FTokenType: TAuthTokenType;
    FAuthChallengeHeader: string;
  public
    class function Default: TWiRLConfigurationAuth; static;
  public
    constructor Create; override;
  published
    property Realm: string read FRealm write FRealm;
    property AuthChallenge: TAuthChallenge read FAuthChallenge write FAuthChallenge;
    property AuthChallengeHeader: string read FAuthChallengeHeader write FAuthChallengeHeader;
    property TokenType: TAuthTokenType read FTokenType write FTokenType;
    property TokenLocation: TAuthTokenLocation read FTokenLocation write FTokenLocation;
    property TokenCustomHeader: string read FTokenCustomHeader write FTokenCustomHeader;
  end;

implementation

{ TAuthChallengeHelper }

function TAuthChallengeHelper.ToString: string;
begin
  case Self of
    TAuthChallenge.Basic:  Result := 'Basic';
    TAuthChallenge.Digest: Result := 'Digest';
    TAuthChallenge.Bearer: Result := 'Bearer';
    TAuthChallenge.Form:   Result := 'Form';
  end;
end;

{ TWiRLConfigurationAuth }

constructor TWiRLConfigurationAuth.Create;
begin
  inherited;
  FTokenType := TAuthTokenType.JWT;
end;

class function TWiRLConfigurationAuth.Default: TWiRLConfigurationAuth;
begin
  Result := TWiRLConfigurationAuth.Create;
  Result.TokenType := TAuthTokenType.JWT;
  Result.TokenLocation := TAuthTokenLocation.Bearer;
  Result.AuthChallenge := TAuthChallenge.Basic;
  Result.Realm := 'wirl';
end;

function TWiRLConfigurationAuth.Configure: IWiRLConfigurationAuth;
begin
  Result := Self;
end;

function TWiRLConfigurationAuth.SetAuthChallenge(AChallenge: TAuthChallenge; const ARealm: string): IWiRLConfigurationAuth;
begin
  FAuthChallenge := AChallenge;
  FRealm := ARealm;
  Result := Self;
end;

function TWiRLConfigurationAuth.SetAuthChallengeHeader(const AHeader: string): IWiRLConfigurationAuth;
begin
  FAuthChallengeHeader := AHeader;
  Result := Self;
end;

function TWiRLConfigurationAuth.SetTokenCustomHeader(const ACustomHeader: string): IWiRLConfigurationAuth;
begin
  FTokenCustomHeader := ACustomHeader;
  Result := Self;
end;

function TWiRLConfigurationAuth.SetTokenLocation(ALocation: TAuthTokenLocation): IWiRLConfigurationAuth;
begin
  FTokenLocation := ALocation;
  Result := Self;
end;

function TWiRLConfigurationAuth.SetTokenType(ATokenType: TAuthTokenType): IWiRLConfigurationAuth;
begin
  FTokenType := ATokenType;
  Result := Self;
end;

initialization
  TWiRLConfigClassRegistry.Instance.RegisterConfigClass(TWiRLConfigurationAuth);

end.
