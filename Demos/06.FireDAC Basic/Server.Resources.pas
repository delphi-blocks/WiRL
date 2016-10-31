(*
  Copyright 2015-2016, WiRL - REST Library

  Home: https://github.com/WiRL-library

*)
unit Server.Resources;

interface

uses
  Classes, SysUtils

  , System.Rtti

  , WiRL.Core.JSON
  , WiRL.Core.Registry
  , WiRL.Core.Attributes
  , WiRL.Core.MediaType

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
