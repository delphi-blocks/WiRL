{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Configuration.Compression;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Defaults,

  WiRL.Core.Classes,
  WiRL.Configuration.Core,
  WiRL.http.Filters.Compression;

{$SCOPEDENUMS ON}

const
  ONE_KB = 1024;
  ONE_MB = ONE_KB * ONE_KB;

type
  IWiRLConfigurationCompression = interface(IWiRLConfiguration)
  ['{7CCFEED2-69E6-4A39-9403-69B2E3F471B9}']
    function SetMinSize(AMinSize: Cardinal): IWiRLConfigurationCompression;
    function SetMediaTypes(const AMediaTypes: string): IWiRLConfigurationCompression;
  end;

  TConfigurator = reference to procedure(AConf: IWiRLConfigurationCompression);

  [Implements(IWiRLConfigurationCompression)]
  TWiRLConfigurationCompression = class sealed(TWiRLConfiguration, IWiRLConfigurationCompression)
  private
    FMinimumSize: Integer;
    FMediaTypes: string;
    FMIME: TArray<Integer>;
  public
    class function Default: IWiRLConfigurationCompression; static;
  public
    procedure DoAfterCreate; override;

    function SetMinSize(AMinimumSize: Cardinal): IWiRLConfigurationCompression;
    function SetMediaTypes(const AMediaTypes: string): IWiRLConfigurationCompression;
  published
    property MinimumSize: Integer read FMinimumSize write FMinimumSize;
    property MediaTypes: string read FMediaTypes write FMediaTypes;
    property MIME: TArray<Integer> read FMIME write FMIME;
  end;

implementation

uses
  WiRL.http.Filters,
  WiRL.http.Filters.CORS;

class function TWiRLConfigurationCompression.Default: IWiRLConfigurationCompression;
begin
  Result := TWiRLConfigurationCompression.Create
    .SetMinSize(ONE_KB)
    .SetMediaTypes('application/xml, application/json, text/csv')
  ;
end;

procedure TWiRLConfigurationCompression.DoAfterCreate;
begin
  TWiRLFilterRegistry.Instance.RegisterFilter<TRequestDecodingFilter>;
  FApplication.SetFilters(TRequestDecodingFilter.QualifiedClassName);
  TWiRLFilterRegistry.Instance.RegisterFilter<TResponseEncodingFilter>;
  FApplication.SetFilters(TResponseEncodingFilter.QualifiedClassName);
end;

function TWiRLConfigurationCompression.SetMediaTypes(const AMediaTypes: string): IWiRLConfigurationCompression;
begin
  FMediaTypes := AMediaTypes;
  Result := Self;
end;

function TWiRLConfigurationCompression.SetMinSize(AMinimumSize: Cardinal): IWiRLConfigurationCompression;
begin
  FMinimumSize := AMinimumSize;
  Result := Self;
end;

initialization
  TWiRLConfigClassRegistry.Instance.RegisterConfigClass(TWiRLConfigurationCompression);

end.
