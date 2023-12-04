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
  System.SysUtils, System.Classes, System.TypInfo, DesignEditors,

  WiRL.Client.Application,
  WiRL.http.Headers,
  WiRL.Client.Resource,
  WiRL.Client.Application.Editor,
  WiRL.Client.CustomResource.Editor,
  WiRL.http.Client,
  WiRL.http.Client.Indy,
  WiRL.http.Client.NetHttp;

type
  TWiRLClientSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TWiRLClientApplicationSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;


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

  RegisterSelectionEditor(TWiRLClient, TWiRLClientSelectionEditor);
  RegisterSelectionEditor(TWiRLClientApplication, TWiRLClientApplicationSelectionEditor);
end;


{ TWiRLClientSelectionEditor }

procedure TWiRLClientSelectionEditor.RequiresUnits(Proc: TGetStrProc);
var
  I: Integer;
  LClient: TWiRLClient;
begin
  inherited;
  for i := 0 to Designer.Root.ComponentCount - 1 do
  begin
    if Designer.Root.Components[i] is TWiRLClient then
    begin
      LClient := TWiRLClient(Designer.Root.Components[i]);

      if LClient.ClientVendor = IndyVendorName then
        Proc('WiRL.http.Client.Indy');
      if LClient.ClientVendor = HttpClientVendorName then
        Proc('WiRL.http.Client.NetHttp');

    end;
  end;
end;

{ TWiRLClientApplicationSelectionEditor }

procedure TWiRLClientApplicationSelectionEditor.RequiresUnits(
  Proc: TGetStrProc);
var
  I: Integer;
  LApp: TWiRLClientApplication;
  LClient: TWiRLClient;
begin
  inherited;
  for i := 0 to Designer.Root.ComponentCount - 1 do
  begin
    if Designer.Root.Components[i] is TWiRLClientApplication then
    begin
      LApp := TWiRLClientApplication(Designer.Root.Components[i]);
      LClient := LApp.Client;

      if Assigned(LClient) then
      begin
        if LClient.ClientVendor = IndyVendorName then
          Proc('WiRL.http.Client.Indy');
        if LClient.ClientVendor = HttpClientVendorName then
          Proc('WiRL.http.Client.NetHttp');
      end;
    end;
  end;
end;

end.
