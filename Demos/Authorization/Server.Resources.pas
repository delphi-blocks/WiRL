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
  [Path('first')]
  TFirstResource = class
  private
  protected
    [Context] URL: TMARSURL;
    [Context] Request: TMARSRequest;
    [Context] Response: TMARSResponse;
  public
    [GET, PermitAll]
    [Produces(TMediaType.TEXT_PLAIN)]
    function PublicInfo: string;


    [GET, Path('/details'), RolesAllowed('admin')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function DetailsInfo: string;
  end;

  [Path('token')]
  TAuthResource = class(TMARSAuthResource)
  private
  protected
  public
  end;

implementation

{ TFirstResource }

function TFirstResource.DetailsInfo: string;
begin
  Result := 'Admin-level access informations here!';
end;

function TFirstResource.PublicInfo: string;
begin
  Result := 'Public informations here!';
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<TFirstResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TAuthResource>;

end.
