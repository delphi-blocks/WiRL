{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2025 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Configuration.Errors;

interface

uses
  System.SysUtils, System.Classes,

  WiRL.Core.Registry,
  WiRL.Core.Exceptions,
  WiRL.http.Accept.MediaType,
  WiRL.Configuration.Core,
  Neon.Core.Types;

{$SCOPEDENUMS ON}

type
  IWiRLConfigurationErrors = interface(IWiRLConfiguration)
  ['{858B4BBB-9698-469E-9476-604C86A07DB0}']
    function SetErrorClass(AClass: ExceptClass): IWiRLConfigurationErrors;
    function SetErrorCase(ACase: TNeonCase): IWiRLConfigurationErrors;
    function SetErrorMediaType(const AType: string): IWiRLConfigurationErrors;
    function SetErrorDebugInfo(ADebug: Boolean): IWiRLConfigurationErrors;
  end;

  TConfigurator = reference to procedure (AConf: IWiRLConfigurationErrors);

  [Implements(IWiRLConfigurationErrors)]
  TWiRLConfigurationErrors = class sealed(TWiRLConfiguration, IWiRLConfigurationErrors)
  private
    FErrorClass: TClass;
    FErrorCase: TNeonCase;
    FErrorMediaType: string;
    FErrorDebugInfo: Boolean;
  public
    class function Default: IWiRLConfigurationErrors; static;
  public
    constructor Create; override;
    destructor Destroy; override;

    function SetErrorClass(AClass: ExceptClass): IWiRLConfigurationErrors;
    function SetErrorCase(ACase: TNeonCase): IWiRLConfigurationErrors;
    function SetErrorMediaType(const AType: string): IWiRLConfigurationErrors;
    function SetErrorDebugInfo(ADebug: Boolean): IWiRLConfigurationErrors;
  public
    property ErrorCase: TNeonCase read FErrorCase write FErrorCase;
    property ErrorClass: TClass read FErrorClass write FErrorClass;
    property ErrorMediaType: string read FErrorMediaType write FErrorMediaType;
    property ErrorDebugInfo: Boolean read FErrorDebugInfo write FErrorDebugInfo;
  end;

implementation

constructor TWiRLConfigurationErrors.Create;
begin
  inherited;
  FErrorClass := EWiRLWebApplicationException;
  FErrorCase := TNeonCase.CamelCase;
  FErrorMediaType := TMediaType.APPLICATION_JSON;
end;

class function TWiRLConfigurationErrors.Default: IWiRLConfigurationErrors;
begin
  Result := TWiRLConfigurationErrors.Create;
end;

destructor TWiRLConfigurationErrors.Destroy;
begin
  inherited;
end;

function TWiRLConfigurationErrors.SetErrorCase(ACase: TNeonCase): IWiRLConfigurationErrors;
begin
  FErrorCase := ACase;
  Result := Self;
end;

function TWiRLConfigurationErrors.SetErrorClass(AClass: ExceptClass): IWiRLConfigurationErrors;
begin
  FErrorClass := AClass;
  Result := Self;
end;

function TWiRLConfigurationErrors.SetErrorDebugInfo(ADebug: Boolean): IWiRLConfigurationErrors;
begin
  FErrorDebugInfo := True;
  Result := Self;
end;

function TWiRLConfigurationErrors.SetErrorMediaType(const AType: string): IWiRLConfigurationErrors;
begin
  FErrorMediaType := AType;
  Result := Self;
end;

initialization
  TWiRLConfigClassRegistry.Instance.RegisterConfigClass(TWiRLConfigurationErrors);

end.