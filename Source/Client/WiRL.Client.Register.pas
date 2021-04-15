{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Client.Register;

interface

uses
  System.SysUtils, System.Classes,

  WiRL.Client.Application,
  WiRL.Client.FireDAC,
  //WiRL.Client.Messaging.Resource,
  WiRL.Client.Resource.JSON,
  WiRL.Client.Resource.Obj,
  WiRL.Client.Resource,
  WiRL.Client.Resource.Stream,
  WiRL.Client.SubResource.JSON,
  WiRL.Client.SubResource,
  WiRL.Client.SubResource.Stream,
  WiRL.Client.Token,
  WiRL.http.Client,
  WiRL.http.Client.Indy,
  WiRL.http.Client.NetHttp;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('WiRL Client', [TWiRLClientApplication]);
//  RegisterComponents('WiRL Client', [TWiRLFDResource]);
//  RegisterComponents('WiRL Client', [TWiRLClientMessagingResource]);
//  RegisterComponents('WiRL Client', [TWiRLClientResourceJSON]);
//  RegisterComponents('WiRL Client', [TWiRLClientResourceObject]);
//  RegisterComponents('WiRL Client', [TWiRLClientResource]);
//  RegisterComponents('WiRL Client', [TWiRLClientResourceStream]);
//  RegisterComponents('WiRL Client', [TWiRLClientSubResourceJSON]);
//  RegisterComponents('WiRL Client', [TWiRLClientSubResource]);
//  RegisterComponents('WiRL Client', [TWiRLClientSubResourceStream]);
  RegisterComponents('WiRL Client', [TWiRLClientToken]);
  RegisterComponents('WiRL Client', [TWiRLClient]);

  RegisterNoIcon([TWiRLClientResource]);

end;


end.
