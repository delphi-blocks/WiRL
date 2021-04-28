{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Client.Register;

interface

uses
  System.SysUtils, System.Classes,

  WiRL.Client.Application,
  WiRL.http.Headers,
  WiRL.Client.Resource,
  WiRL.Client.Application.Editor,
  WiRL.Client.CustomResource.Editor,
  WiRL.http.Client,
  WiRL.http.Client.Indy,
  WiRL.http.Client.NetHttp;

procedure Register;

implementation

uses
  DesignIntf;

procedure Register;
begin
  RegisterComponents('WiRL Client', [TWiRLClientApplication]);
  RegisterComponents('WiRL Client', [TWiRLClient]);
  RegisterNoIcon([TWiRLClientResource]);
  RegisterClass(TWiRLClientResource);

  RegisterComponentEditor(TWiRLClientApplication, TWiRLClientAppEditor);
  RegisterPropertyEditor(TypeInfo(IWiRLHeaders), nil, '', THeadersProperty);
  RegisterPropertyEditor(TypeInfo(TWiRLClient), TWiRLClientApplication, 'Client', TWiRLClientAppClientProperty);
end;


end.
