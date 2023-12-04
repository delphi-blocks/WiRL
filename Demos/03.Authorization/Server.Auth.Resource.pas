{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Auth.Resource;

interface

uses
  System.Classes, System.SysUtils,

  WiRL.Core.Registry,
  WiRL.Core.Exceptions,
  WiRL.Core.Attributes,
  WiRL.Core.Auth.Resource,
  WiRL.http.Accept.MediaType,

  Neon.Core.Attributes;

type
  THardwareID = class
  private
    FID: Integer;
    FToken: string;
  public
    [NeonProperty('id')]
    property ID: Integer read FID write FID;
    [NeonProperty('token')]
    property Token: string read FToken write FToken;
  end;

  /// <summary>
  /// Custom authentication (body) resource
  /// </summary>
  /// <remarks>
  /// The field are named "username" and "password". If you want custom field names you must inherit the base class
  /// </remarks>
  TBodyAuthResource = class(TWiRLAuthResource)
  public
    [POST, Produces(TMediaType.APPLICATION_JSON)]
    function DoLogin([BodyParam] AHardware: THardwareID): TWiRLLoginResponse;
  end;

implementation

uses
  WiRL.Configuration.JWT;

{ TBodyAuthResource }

function TBodyAuthResource.DoLogin(AHardware: THardwareID): TWiRLLoginResponse;
begin
  FAuthContext.Clear;
  // Custom Validation of the hardware token (simulated)
  if (AHardware.ID > 100) and (AHardware.Token = 'qwerty') then
  begin
    FAuthContext.Subject.Roles := 'admin,manager';
  end
  else
    raise EWiRLNotAuthorizedException.Create('Invalid credentials', 'TWiRLAuthFormResource', 'DoLogin');

  FAuthContext.Generate(FApplication.GetConfiguration<TWiRLConfigurationJWT>.KeyPair.PrivateKey.Key);

  Result := TWiRLLoginResponse.Create(True, FAuthContext.CompactToken);
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TBodyAuthResource>;

end.
