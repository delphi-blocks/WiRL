{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Configuration.CORS;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Defaults,

  WiRL.Configuration.Core;

{$SCOPEDENUMS ON}

type
  TCORSResponseType = (Simple);

  IWiRLConfigurationCORS = interface(IWiRLConfiguration)
  ['{282202BA-0AEE-417C-82A8-8BD318B5CA46}']
    function SetOrigin(const AOrigin: string): IWiRLConfigurationCORS;
    function SetMethods(const AMethods: string = 'HEAD,GET,PUT,POST,DELETE,OPTIONS'): IWiRLConfigurationCORS;
    function SetHeaders(const AHeaders: string): IWiRLConfigurationCORS;
    function SetExposeHeaders(const AHeaders: string): IWiRLConfigurationCORS;
    function SetCredentials(ACredentials: Boolean): IWiRLConfigurationCORS;
    function SetMaxAge(AMaxAge: Integer): IWiRLConfigurationCORS;
  end;

  TConfigurator = reference to procedure(AConf: IWiRLConfigurationCORS);

  [Implements(IWiRLConfigurationCORS)]
  TWiRLConfigurationCORS = class sealed(TWiRLConfiguration, IWiRLConfigurationCORS)
  private
    FEnabled: Boolean;
    FHeaders: string;
    FOrigin: string;
    FMethods: string;
    FExposeHeaders: string;
    FCredentials: Boolean;
    FMaxAge: Integer;
  public
    class function Default: IWiRLConfigurationCORS; static;
  public
    procedure DoAfterCreate; override;

    // Interface IWiRLConfigurationCORS implementation
    function SetOrigin(const AOrgin: string): IWiRLConfigurationCORS;
    function SetMethods(const AMethods: string): IWiRLConfigurationCORS;
    function SetHeaders(const AHeaders: string): IWiRLConfigurationCORS;
    function SetExposeHeaders(const AHeaders: string): IWiRLConfigurationCORS;
    function SetCredentials(ACredentials: Boolean): IWiRLConfigurationCORS;
    function SetMaxAge(AMaxAge: Integer): IWiRLConfigurationCORS;
  published
    property Origin: string read FOrigin write FOrigin;
    property Methods: string read FMethods write FMethods;
    property Headers: string read FHeaders write FHeaders;
    property ExposeHeaders: string read FExposeHeaders write FExposeHeaders;
    property Credentials: Boolean read FCredentials write FCredentials;
    property MaxAge: Integer read FMaxAge write FMaxAge;
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

implementation

uses
  WiRL.http.Filters,
  WiRL.http.Filters.CORS;

class function TWiRLConfigurationCORS.Default: IWiRLConfigurationCORS;
begin
  Result := TWiRLConfigurationCORS.Create
    .SetOrigin('*')
    .SetMethods('HEAD, GET, PUT, POST, DELETE, OPTIONS')
    .SetHeaders('Accept, Content-Type, Content-Encoding')
  ;
end;

procedure TWiRLConfigurationCORS.DoAfterCreate;
begin
  TWiRLFilterRegistry.Instance.RegisterFilter<TCORSFilter>;
  FApplication.SetFilters(TCORSFilter.QualifiedClassName);
end;

function TWiRLConfigurationCORS.SetCredentials(
  ACredentials: Boolean): IWiRLConfigurationCORS;
begin
  FCredentials := ACredentials;
  Result := Self;
end;

function TWiRLConfigurationCORS.SetExposeHeaders(
  const AHeaders: string): IWiRLConfigurationCORS;
begin
  FExposeHeaders := AHeaders;
  Result := Self;
end;

function TWiRLConfigurationCORS.SetHeaders(const AHeaders: string): IWiRLConfigurationCORS;
begin
  FHeaders := AHeaders;
  Result := Self;
end;

function TWiRLConfigurationCORS.SetMaxAge(
  AMaxAge: Integer): IWiRLConfigurationCORS;
begin
  FMaxAge := AMaxAge;
  Result := Self;
end;

function TWiRLConfigurationCORS.SetMethods(const AMethods: string): IWiRLConfigurationCORS;
begin
  FMethods := AMethods;
  Result := Self;
end;

function TWiRLConfigurationCORS.SetOrigin(const AOrgin: string): IWiRLConfigurationCORS;
begin
  FOrigin := AOrgin;
  Result := Self;
end;

initialization
  TWiRLConfigClassRegistry.Instance.RegisterConfigClass(TWiRLConfigurationCORS);

end.
