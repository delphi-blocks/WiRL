{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Resources.Customer;

interface

uses
  System.Classes, System.SysUtils, System.JSON,

  WiRL.Core.OpenAPI.Resource,
  WiRL.Core.Engine,
  WiRL.Core.Application,
  WiRL.Core.Registry,
  WiRL.Core.Attributes,
  WiRL.Core.Application.Worker,
  WiRL.Core.MessageBody.Default,
  WiRL.Core.Auth.Context,
  WiRL.Core.Auth.Resource,
  WiRL.http.Accept.MediaType,
  WiRL.http.URL,
  WiRL.http.Request,
  WiRL.http.Response,

  Server.Entities.Customer;

type
  [Path('addressbook')]
  TAddressBookResource = class

  end;


implementation

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TAddressBookResource>;

end.
