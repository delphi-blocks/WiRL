{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2022 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Configuration.JWT;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Defaults,

  JOSE.Core.JWT,
  JOSE.Core.JWK,
  JOSE.Core.JWA,
  WiRL.Configuration.Core,
  WiRL.Core.Auth.Context;

{$SCOPEDENUMS ON}

type
  TSecretGenerator = reference to function(): TBytes;
  TJWTVerificationMode = (Verify, Deserialize);

  IWiRLConfigurationJWT = interface(IWiRLConfiguration)
  ['{BF13669E-7B9C-4D56-AE9C-C9EF6EE733DA}']
    function SetCheckExpiration(ACheck: Boolean): IWiRLConfigurationJWT;
    function SetVerificationMode(AVerificationMode: TJWTVerificationMode = TJWTVerificationMode.Verify): IWiRLConfigurationJWT;
    function SetClaimClass(AClaimClass: TWiRLSubjectClass): IWiRLConfigurationJWT;
    function SetAlgorithm(AAlgorithm: TJOSEAlgorithmId): IWiRLConfigurationJWT;
    function SetSecret(const ASecret: TBytes): IWiRLConfigurationJWT; overload;
    function SetSecret(ASecretGen: TSecretGenerator): IWiRLConfigurationJWT; overload; deprecated;
    function SetPublicKey(const APublicKey: TBytes): IWiRLConfigurationJWT;
    function SetPrivateKey(const APrivateKey: TBytes): IWiRLConfigurationJWT;
  end;

  TConfigurator = reference to procedure(AJWTConf: IWiRLConfigurationJWT);

  [Implements(IWiRLConfigurationJWT)]
  TWiRLConfigurationJWT = class sealed(TWiRLConfiguration, IWiRLConfigurationJWT)
  private
    const SCRT_SGN = 'd2lybC5zdXBlcnNlY3JldC5zZWVkLmZvci5zaWduaW5n';
  private
    FCheckExpiration: Boolean;
    FVerificationMode: TJWTVerificationMode;
    FClaimClass: TWiRLSubjectClass;
    FAlgorithm: TJOSEAlgorithmId;
    FKeyPair: TKeyPair;
  public
    class function Default: IWiRLConfigurationJWT; static;
    class function SHA256(const ASecret: TBytes): IWiRLConfigurationJWT; static;
    class function RSA256(const APublicKey, APrivateKey: TBytes): IWiRLConfigurationJWT; static;
  public
    constructor Create; override;
    destructor Destroy; override;

    // Interface IWiRLConfigurationJWT implementation
    function SetCheckExpiration(ACheckExpiration: Boolean): IWiRLConfigurationJWT;
    function SetVerificationMode(AVerificationMode: TJWTVerificationMode): IWiRLConfigurationJWT;
    function SetClaimClass(AClaimClass: TWiRLSubjectClass): IWiRLConfigurationJWT;
    function SetAlgorithm(AAlgorithm: TJOSEAlgorithmId): IWiRLConfigurationJWT;
    function SetSecret(const ASecret: TBytes): IWiRLConfigurationJWT; overload;
    function SetSecret(ASecretGen: TSecretGenerator): IWiRLConfigurationJWT; overload;
    function SetPublicKey(const APublicKey: TBytes): IWiRLConfigurationJWT;
    function SetPrivateKey(const APrivateKey: TBytes): IWiRLConfigurationJWT;
  published
    property CheckExpiration: Boolean read FCheckExpiration write FCheckExpiration;
    property VerificationMode: TJWTVerificationMode read FVerificationMode write FVerificationMode;
    property ClaimClass: TWiRLSubjectClass read FClaimClass write FClaimClass;
    property Algorithm: TJOSEAlgorithmId read FAlgorithm write FAlgorithm;
    property KeyPair: TKeyPair read FKeyPair write FKeyPair;
  end;

implementation

constructor TWiRLConfigurationJWT.Create;
begin
  inherited;
  FVerificationMode := TJWTVerificationMode.Verify;
  FKeyPair := TKeyPair.Create;
end;

destructor TWiRLConfigurationJWT.Destroy;
begin
  FKeyPair.Free;
  inherited;
end;

class function TWiRLConfigurationJWT.Default: IWiRLConfigurationJWT;
begin
  Result := TWiRLConfigurationJWT.Create
    .SetAlgorithm(TJOSEAlgorithmId.HS256)
    .SetSecret(TEncoding.ANSI.GetBytes(SCRT_SGN))
  ;
end;

class function TWiRLConfigurationJWT.SHA256(const ASecret: TBytes): IWiRLConfigurationJWT;
begin
  Result := TWiRLConfigurationJWT.Create
    .SetAlgorithm(TJOSEAlgorithmId.HS256)
    .SetSecret(ASecret)
  ;
end;

class function TWiRLConfigurationJWT.RSA256(const APublicKey, APrivateKey: TBytes): IWiRLConfigurationJWT;
begin
  Result := TWiRLConfigurationJWT.Create
    .SetAlgorithm(TJOSEAlgorithmId.RS256)
    .SetPublicKey(APublicKey)
    .SetPrivateKey(APrivateKey)
  ;
end;

function TWiRLConfigurationJWT.SetAlgorithm(AAlgorithm: TJOSEAlgorithmId): IWiRLConfigurationJWT;
begin
  FAlgorithm := AAlgorithm;
  Result := Self;
end;

function TWiRLConfigurationJWT.SetCheckExpiration(ACheckExpiration: Boolean): IWiRLConfigurationJWT;
begin
  FCheckExpiration := ACheckExpiration;
  Result := Self;
end;

function TWiRLConfigurationJWT.SetClaimClass(AClaimClass: TWiRLSubjectClass): IWiRLConfigurationJWT;
begin
  FClaimClass := AClaimClass;
  Result := Self;
end;

function TWiRLConfigurationJWT.SetPrivateKey(const APrivateKey: TBytes): IWiRLConfigurationJWT;
begin
  FKeyPair.PrivateKey.Key := APrivateKey;
  Result := Self;
end;

function TWiRLConfigurationJWT.SetPublicKey(const APublicKey: TBytes): IWiRLConfigurationJWT;
begin
  FKeyPair.PublicKey.Key := APublicKey;
  Result := Self;
end;

function TWiRLConfigurationJWT.SetSecret(ASecretGen: TSecretGenerator): IWiRLConfigurationJWT;
begin
  FKeyPair.SetSymmetricKey(ASecretGen);
  Result := Self;
end;

function TWiRLConfigurationJWT.SetVerificationMode(AVerificationMode: TJWTVerificationMode): IWiRLConfigurationJWT;
begin
  FVerificationMode := AVerificationMode;
  Result := Self;
end;

function TWiRLConfigurationJWT.SetSecret(const ASecret: TBytes): IWiRLConfigurationJWT;
begin
  KeyPair.SetSymmetricKey(ASecret);
  Result := Self;
end;

initialization
  TWiRLConfigClassRegistry.Instance.RegisterConfigClass(TWiRLConfigurationJWT);

end.
