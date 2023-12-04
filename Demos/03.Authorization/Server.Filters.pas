{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Filters;

interface

uses
  WinApi.Windows, System.SysUtils, System.Classes,

  WiRL.Core.Registry,
  WiRL.http.Filters,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.Core.Attributes,
  WiRL.Core.Exceptions,
  WiRL.Core.Auth.Context;

type
  /// <summary>
  ///   This filter is an example of how to ckeck the JWT claims
  /// </summary>
  TAuthCheckerFilter = class(TInterfacedObject, IWiRLContainerRequestFilter)
  public
    procedure Filter(ARequestContext: TWiRLContainerRequestContext);
  end;


implementation

uses
  WiRL.Core.Metadata;

{ TAuthCheckerFilter }

procedure TAuthCheckerFilter.Filter(ARequestContext: TWiRLContainerRequestContext);
var
  LClaims: TWiRLSubject;
  LMethod: TWiRLProxyMethod;
begin
  LClaims := ARequestContext.Context.AuthContext.Subject;
  LMethod := ARequestContext.Context.ResourceMethod as TWiRLProxyMethod;

  // If the method has no restrictions (DenyAll, PermitAll, Roles) then there is no point
  // in checking the (possible) token... But you can, of course!
  if not LMethod.Auth.HasAuth then
    Exit;

  // Here you can check every claim with your validation algorithm
  if LClaims.Expiration < Now() then
    raise EWiRLNotAuthorizedException.Create('Token expired!');

end;

initialization
  TWiRLFilterRegistry.Instance.RegisterFilter<TAuthCheckerFilter>;

end.
