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
  WiRL.Core.JSON,
  WiRL.Core.Registry,
  WiRL.http.Accept.MediaType,
  WiRL.Core.Attributes,
  WiRL.Core.Exceptions,
  WiRL.Core.Auth.Resource;

type
  /// <summary>
  /// Custom authentication (body) resource
  /// </summary>
  /// <remarks>
  /// The field are named "username" and "password". If you want custom field names you must inherit the base class
  /// </remarks>
  TBodyAuthResource = class(TWiRLAuthResource)
  public
    [POST, Produces(TMediaType.APPLICATION_JSON)]
    function DoLogin([BodyParam] ABody: TJSONObject): TJSONObject;
  end;


implementation

uses
  WiRL.Configuration.JWT;

{ TBodyAuthResource }

function TBodyAuthResource.DoLogin(ABody: TJSONObject): TJSONObject;
var
  LAuthOperation: TWiRLAuthResult;
  LHardwareToken: string;
begin
  FAuthContext.Clear;

  LHardwareToken := ABody.GetValue('hardware_token').Value;

  // Validation of the hardware token (simulated)
  FAuthContext.Subject.Roles := 'admin,manager';

  if not LAuthOperation.Success then
    raise EWiRLNotAuthorizedException.Create('Invalid credentials', 'TWiRLAuthFormResource', 'DoLogin');

  FAuthContext.Generate(FApplication.GetConfiguration<TWiRLConfigurationJWT>.KeyPair.PrivateKey.Key);
  Result := GetGeneratedToken;
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TBodyAuthResource>;


end.
