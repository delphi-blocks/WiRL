{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2025 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Accept.MediaType;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Defaults, System.Generics.Collections,

  WiRL.http.Accept.Parser,
  WiRL.Core.Declarations,
  WiRL.Core.Classes;

type
  TMediaType = class(TAcceptItem)
  private
    FMainType: string;
    FSubType: string;
    FVersion: string;
    FCharset: string;
    FDialect: string;
    FBoundary: string;
    procedure ParseMediaType(const AFullMediaType: string);
    procedure ParseMediaTypeParams;
    function GetMediaType: string;
  public
    const DELIM_MEDIA = '/';
    const VERSION_NAME = 'version';
    const DIALECT_NAME = 'dialect';
    const CHARSET_NAME = 'charset';
    const BOUNDARY_NAME = 'boundary';

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
    const TEXT_EVENT_STREAM = 'text/event-stream';
    const IMAGE_PNG = 'image/png';
    const IMAGE_JPEG = 'image/jpeg';
    const APPLICATION_PDF = 'application/pdf';
    const APPLICATION_XML = 'application/xml';
    const APPLICATION_JSON = 'application/json';
    const APPLICATION_PROBLEM_XML = 'application/problem+xml';
    const APPLICATION_PROBLEM_JSON = 'application/problem+json';
    const APPLICATION_JAVASCRIPT = 'application/javascript';
    const APPLICATION_XHTML_XML = 'application/xhtml+xml';
    const APPLICATION_SVG_XML = 'application/svg+xml';
    const APPLICATION_ATOM_XML = 'application/atom+xml';
    const APPLICATION_OCTET_STREAM = 'application/octet-stream';
    const APPLICATION_FORM_URLENCODED_TYPE = 'application/x-www-form-urlencoded';
    const APPLICATION_FIREDAC_JSON = 'application/vnd.embarcadero.firedac+json';
    const APPLICATION_FIREDAC_XML = 'application/vnd.embarcadero.firedac+xml';
    const APPLICATION_FIREDAC_BIN = 'application/vnd.embarcadero.firedac+bin';
    const MULTIPART_FORM_DATA = 'multipart/form-data';
    const WILDCARD = '*/*';
  public
    constructor Create(const AAcceptItem: string); override;
    constructor CreateEx(const AType, ASubType: string); overload;
    constructor CreateEx(const AType, ASubType, AParams: string); overload;
    constructor CreateEx(const AType, ASubType: string; AParams: TStringList); overload;

    function Clone: TMediaType;
    procedure Assign(ASource: TMediaType);
    function IsType(const AType: string): Boolean;
    function GetDelphiEncoding: TEncoding;

    class function GetWildcard: string; override;

    property MediaType: string read GetMediaType;
    property MainType: string read FMainType;
    property SubType: string read FSubType;
    property Version: string read FVersion write FVersion;
    property Dialect: string read FDialect write FDialect;
    property Charset: string read FCharset write FCharset;
    property Boundary: string read FBoundary write FBoundary;
  end;

  TMediaTypeList = class(TAcceptItemList<TMediaType>)
  public
    function CloneList: TMediaTypeList;
    procedure Assign(ASource: TMediaTypeList);
  public
    function IsWildCard: Boolean;
    function HasWildCard: Boolean;
    function IntersectionList(const AList: TMediaTypeList): TMediaTypeList;  overload;
  end;

implementation

uses
  System.StrUtils;

{ TMediaType }

procedure TMediaType.Assign(ASource: TMediaType);
begin
  inherited Assign(ASource);
  Self.FMainType := ASource.MainType;
  Self.FSubType  := ASource.SubType;
  Self.FVersion  := ASource.Version;
  Self.FDialect  := ASource.Dialect;
  Self.FCharset  := ASource.Charset;
  Self.FBoundary  := ASource.Boundary;
end;

function TMediaType.Clone: TMediaType;
begin
  Result := TMediaType.Create('');
  Result.Assign(Self);
end;

constructor TMediaType.Create(const AAcceptItem: string);
begin
  inherited;
  ParseMediaType(FValue);
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

function TMediaType.GetDelphiEncoding: TEncoding;
begin
  if Self.Charset = CHARSET_UTF8 then
    Result := TUTF8EncodingNoBOM.Create
  else if Self.Charset = CHARSET_UTF16BE then
    Result := TUnicodeBEEncodingNoBOM.Create
  else if Self.Charset = CHARSET_UTF16LE then
    Result := TUnicodeLEEncodingNoBOM.Create
  else if Self.Charset = CHARSET_UTF16 then
    Result := TUnicodeLEEncodingNoBOM.Create
  else if Self.Charset = CHARSET_ISO_8859_1 then
    Result := TEncoding.GetEncoding(CHARSET_ISO_8859_1)
  else
    Result := TMBCSEncoding.Create;
end;

class function TMediaType.GetWildcard: string;
begin
  Result := WILDCARD;
end;

function TMediaType.IsType(const AType: string): Boolean;
begin
  Result := SameText(MediaType, AType);
end;

function TMediaType.GetMediaType: string;
begin
  Result := MainType + DELIM_MEDIA + SubType;
end;

procedure TMediaType.ParseMediaType(const AFullMediaType: string);
var
  LParts: TStringArray;
begin
  LParts := AFullMediaType.Split([DELIM_MEDIA]);

  if Length(LParts) > 0 then
    FMainType := LParts[0];

  if Length(LParts) > 1 then
    FSubType := LParts[1];
end;

procedure TMediaType.ParseMediaTypeParams;
var
  LPosition: Integer;
begin
  for LPosition := 0 to FParameters.Count - 1 do
  begin
    if FParameters.Names[LPosition] = VERSION_NAME then
      FVersion := FParameters.ValueFromIndex[LPosition]
    else if FParameters.Names[LPosition] = DIALECT_NAME then
      FDialect := FParameters.ValueFromIndex[LPosition]
    else if FParameters.Names[LPosition] = CHARSET_NAME then
      FCharset := FParameters.ValueFromIndex[LPosition]
    else if FParameters.Names[LPosition] = BOUNDARY_NAME then
      FBoundary := FParameters.ValueFromIndex[LPosition];
  end;
end;

procedure TMediaTypeList.Assign(ASource: TMediaTypeList);
var
  LItem: TMediaType;
begin
  Clear;
  for LItem in ASource do
    Self.Add(LItem.Clone);
end;

function TMediaTypeList.CloneList: TMediaTypeList;
begin
  Result := TMediaTypeList.Create;
  Result.Assign(Self);
end;

function TMediaTypeList.IsWildCard: Boolean;
begin
  Result := (Count = 1) and Self.Contains(TMediaType.WILDCARD);
end;

function TMediaTypeList.HasWildCard: Boolean;
begin
  Result := Self.Contains(TMediaType.WILDCARD);
end;

function TMediaTypeList.IntersectionList(const AList: TMediaTypeList): TMediaTypeList;
var
  LItem: TMediaType;
begin
  Result := TMediaTypeList.Create;
  try
    for LItem in AList do
      if Self.Contains(LItem) then
        Result.Add(LItem.Clone);
  except
    Result.Free;
  end;
end;

end.
