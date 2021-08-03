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
  ///   This filter shows how to ckeck the JWT claims
  /// </summary>
  TAuthFilter = class(TInterfacedObject, IWiRLContainerRequestFilter)
  public
    procedure Filter(ARequestContext: TWiRLContainerRequestContext);
  end;


implementation

{ TAuthFilter }

procedure TAuthFilter.Filter(ARequestContext: TWiRLContainerRequestContext);
var
  LClaims: TWiRLSubject;
begin
  if ARequestContext.Context.AuthContext.CompactToken = '' then
    Exit;

  LClaims := ARequestContext.Context.AuthContext.Subject;

  if LClaims.Expiration < Now() then
    raise EWiRLNotAuthorizedException.Create('Token expired!');

end;

//initialization
  //TWiRLFilterRegistry.Instance.RegisterFilter<TAuthFilter>;

end.
