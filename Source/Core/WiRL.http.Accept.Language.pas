{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Accept.Language;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Defaults, System.Generics.Collections,

  WiRL.Core.Declarations,
  WiRL.Http.Accept.Parser;

type
  TAcceptLanguage = class(TAcceptItem)
  private
    FCountry: string;
    FLanguage: string;
    procedure ParseLanguage(const AFullLanguage: string);
  public
    const DELIM_LANGUAGE = '-';
    const EN_US = 'en-us';
    const EN_GB = 'en-gb';
    const IT_IT = 'it-it';
    const WILDCARD = '*';
  public
    class function GetWildcard: string; override;
    procedure Parse(const AAcceptItem: string); override;

    property Country: string read FCountry write FCountry;
    property Language: string read FLanguage write FLanguage;
  end;

  TAcceptLanguageList = class(TAcceptItemList<TAcceptLanguage>);

implementation

class function TAcceptLanguage.GetWildcard: string;
begin
  Result := WILDCARD;
end;

procedure TAcceptLanguage.Parse(const AAcceptItem: string);
begin
  inherited;
  ParseLanguage(FValue);
end;

procedure TAcceptLanguage.ParseLanguage(const AFullLanguage: string);
var
  LParts: TStringArray;
begin
  FLanguage := '';
  FCountry := '';

  LParts := AFullLanguage.Split([DELIM_LANGUAGE]);

  if Length(LParts) > 0 then
    FLanguage := LParts[0];

  if Length(LParts) > 1 then
    FCountry := LParts[1];
end;

end.
