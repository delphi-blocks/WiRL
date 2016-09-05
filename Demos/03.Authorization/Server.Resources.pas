(*
  Copyright 2015-2016, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit Server.Resources;

interface

uses
  System.Classes, System.SysUtils, System.Rtti,

  MARS.Core.JSON,
  MARS.Core.Registry,
  MARS.Core.Attributes,
  MARS.Core.MediaType,
  MARS.Core.URL,
  MARS.Core.MessageBodyWriters,
  MARS.Core.Request,
  MARS.Core.Response,
  MARS.Core.Token,
  MARS.Core.Token.Resource;

type
  [Path('user')]
  TSecuredResource = class
  private
  protected
    [Context] Auth: TMARSAuthContext;
    [Context] URL: TMARSURL;
    [Context] Request: TMARSRequest;
    [Context] Response: TMARSResponse;
  public
    [GET, PermitAll]
    [Produces(TMediaType.TEXT_PLAIN)]
    function PublicInfo: string;

    [GET, Path('/details'), RolesAllowed('admin')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function DetailsInfo: TJSONObject;
  end;

  [Path('auth')]
  TAuthResource = class(TMARSAuthResource)
  protected
    function Authenticate(const AUserName, APassword: string): Boolean; override;
  end;

implementation

{ TSecuredResource }

function TSecuredResource.DetailsInfo: TJSONObject;
begin

  Result := TJSONObject.Create;
  Result.AddPair('custom', TJSONString.Create('Admin-level access informations here!'));
  Result.AddPair('subject', Auth.Subject.Clone);
end;

function TSecuredResource.PublicInfo: string;
begin
  Result := 'User public informations!' + Auth.Subject.UserName;
end;

{ TAuthResource }

function TAuthResource.Authenticate(const AUserName, APassword: string): Boolean;
begin
  Result := SameText(APassword, 'mypassword');
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<TSecuredResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TAuthResource>;

end.
