{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Accept.MediaType;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Defaults, System.Generics.Collections,

  WiRL.http.Accept.Parser,
  WiRL.Core.Declarations;

type
  TMediaType = class(TAcceptItem)
  private
    FMainType: string;
    FSubType: string;
    FVersion: Integer;
    FCharset: string;
    FDialect: string;
    procedure ParseMediaType(const AFullMediaType: string);
    procedure ParseMediaTypeParams;
  public
    const DELIM_MEDIA = '/';
    const VERSION_NAME = 'version';
    const DIALECT_NAME = 'dialect';
    const CHARSET_NAME = 'charset';

    const CHARSET_ISO_8859_1 = 'iso-8859-1';
    const CHARSET_UTF8 = 'utf-8';
    const CHARSET_UTF16 = 'utf-16';
    const CHARSET_UTF16LE = 'utf-16le';
    const CHARSET_UTF16BE = 'utf-16be';

    const WITH_CHARSET = TAcceptItem.DELIM_PARAMS + CHARSET_NAME + TAcceptItem.DELIM_VALUE;
    const WITH_CHARSET_ISO_8859_1 = WITH_CHARSET + CHARSET_ISO_8859_1;
    const WITH_CHARSET_UTF8 = WITH_CHARSET + CHARSET_UTF8;
    const WITH_CHARSET_UTF16 = WITH_CHARSET + CHARSET_UTF16;

    const TEXT_PLAIN = 'text/plain';
    const TEXT_XML = 'text/xml';
    const TEXT_CSV = 'text/csv';
    const TEXT_HTML = 'text/html';
    const APPLICATION_PDF = 'application/pdf';
    const APPLICATION_XML = 'application/xml';
    const APPLICATION_JSON = 'application/json';
    const APPLICATION_XHTML_XML = 'application/xhtml+xml';
    const APPLICATION_SVG_XML = 'application/svg+xml';
    const APPLICATION_ATOM_XML = 'application/atom+xml';
    const APPLICATION_OCTET_STREAM = 'application/octet-stream';
    const APPLICATION_FORM_URLENCODED_TYPE = 'application/x-www-form-urlencoded';
    const MULTIPART_FORM_DATA = 'multipart/form-data';
    const WILDCARD = '*/*';
  public
    constructor Create(const AAcceptItem: string); override;
    constructor CreateEx(const AType, ASubType: string); overload;
    constructor CreateEx(const AType, ASubType, AParams: string); overload;
    constructor CreateEx(const AType, ASubType: string; AParams: TStringList); overload;

    class function GetWildcard: string; override;

    property MainType: string read FMainType;
    property SubType: string read FSubType;
    property Version: Integer read FVersion write FVersion;
    property Dialect: string read FDialect write FDialect;
    property Charset: string read FCharset write FCharset;
  end;

  TMediaTypeList = class(TAcceptItemList<TMediaType>)
  public
    function IntersectionWithDefault(AList: TMediaTypeList): TArray<string>;
  end;

implementation

uses
  System.StrUtils;

{ TMediaType }

constructor TMediaType.Create(const AAcceptItem: string);
begin
  inherited;
  ParseMediaType(FAcceptItemOnly);
  ParseMediaTypeParams;
end;

constructor TMediaType.CreateEx(const AType, ASubType: string);
begin

end;

constructor TMediaType.CreateEx(const AType, ASubType, AParams: string);
begin

end;

constructor TMediaType.CreateEx(const AType, ASubType: string; AParams: TStringList);
begin

end;

class function TMediaType.GetWildcard: string;
begin
  Result := WILDCARD;
end;

procedure TMediaType.ParseMediaType(const AFullMediaType: string);
var
  LParts: TStringArray;
begin
  LParts := AFullMediaType.Split([DELIM_MEDIA]);

  if LParts.Size > 0 then
    FMainType := LParts[0];

  if LParts.Size > 1 then
    FSubType := LParts[1];
end;

procedure TMediaType.ParseMediaTypeParams;
var
  LPosition: Integer;
begin
  for LPosition := 0 to FParameters.Count - 1 do
  begin
    if FParameters.Names[LPosition] = VERSION_NAME then
      FVersion := StrToInt(FParameters.ValueFromIndex[LPosition])
    else if FParameters.Names[LPosition] = DIALECT_NAME then
      FDialect := FParameters.ValueFromIndex[LPosition]
    else if FParameters.Names[LPosition] = CHARSET_NAME then
      FCharset := FParameters.ValueFromIndex[LPosition];
  end;
end;

function TMediaTypeList.IntersectionWithDefault(AList: TMediaTypeList): TArray<string>;
begin
  // AList: AMediaTypeList
  // Self: LMethodProducesMediaTypes
  if Self.Count > 0 then
    Result := Self.Intersection(AList)
  else
    Result := AList.ToArrayOfString;

  if (Length(Result) = 0) or
     ((Length(Result) = 1) and (Result[0] = TMediaType.GetWildcard))
  then // defaults
  begin
    if Self.Count > 0 then
      Result := Self.ToArrayOfString
    else
    begin
      SetLength(Result, 2);
      Result[0] := TMediaType.APPLICATION_JSON;
      Result[1] := TMediaType.WILDCARD;
    end;
  end;
end;

end.
