{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Resources;

interface

uses
  Classes, SysUtils

  , System.Rtti

  , WiRL.Core.JSON
  , WiRL.Core.Registry
  , WiRL.Core.Attributes
  , WiRL.http.Accept.MediaType

  , WiRL.Core.Token
  , WiRL.Core.Token.Resource

  , WiRL.Data.FireDAC
  , WiRL.Data.MessageBodyWriters
  , FireDAC.Phys.FB
  ;

type
  [  Connection('Firebird_Employee_Pooled')
   , Path('nodm_helloworld')
   , SQLStatement('employee', 'select * from EMPLOYEE order by EMP_NO')
   , Produces(TMediaType.APPLICATION_JSON)
  ]
  THelloWorldResource = class(TWiRLFDDatasetResource)
  protected
  public
  end;

  [Path('token')]
  TTokenResource = class(TWiRLTokenResource);

implementation


initialization
  TWiRLResourceRegistry.Instance.RegisterResource<THelloWorldResource>;
  TWiRLResourceRegistry.Instance.RegisterResource<TTokenResource>;

end.
