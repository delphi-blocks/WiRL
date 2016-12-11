(*
  Copyright 2015-2016, WiRL - REST Library

  Home: https://github.com/WiRL-library

*)
unit WiRL.Core.MediaType;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Defaults, System.Generics.Collections,
  WiRL.Core.Declarations;

type
(*
  {$SCOPEDENUMS ON}
  MediaType = (
    Text_Plain,
    Text_XML,
    Text_HTML,
    Application_XML,
    Application_JSON,
    Application_XHTML_XML,
    Application_SVG_XML,
    Application_Atom_XML,
    Application_Octet_Stream,
    Application_Form_Encoded,
    Multipart_Form_Data,
    Wildcard
  );
  {$SCOPEDENUMS OFF}
*)

  TMediaTypeParams = TDictionary<string, string>;

  TMediaType = class
  private
    FPFactor: Integer;
    FMediaType: string;
    FMediaSubType: string;
    FMediaParameters: TStringList;
    FVersion: Integer;
    FQFactor: Double;
    FDialect: string;
    FCharset: string;
    function GetWeigth: Integer;
    function GetIsWildcard: Boolean;
  public
    const DELIM_MEDIA = '/';
    const DELIM_PARAMS = ';';
    const DELIM_VALUE = '=';
    const QFACTOR_NAME = 'q';
    const VERSION_NAME = 'version';
    const DIALECT_NAME = 'dialect';
    const CHARSET_NAME = 'charset';

    const CHARSET_ISO_8859_1 = 'charset=iso-8859-1';
    const CHARSET_UTF8 = 'charset=utf-8';
    const CHARSET_UTF16 = 'charset=utf-16';

    const TEXT_PLAIN = 'text/plain';
    const TEXT_XML = 'text/xml';
    const TEXT_CSV = 'text/csv';
    const TEXT_HTML = 'text/html';
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
    constructor Create; overload;
    constructor Create(const AMediaType: string); overload;
    constructor Create(const AType, ASubType: string); overload;
    constructor Create(const AType, ASubType, AParams: string); overload;
    constructor Create(const AType, ASubType: string; AParams: TStringList); overload;
    destructor Destroy; override;

    function ToString: string; override;
    function ToStringDebug: string;

    function Matches(const AMediaTypeStr: string): Boolean; overload;
    function Matches(const AMediaType: TMediaType): Boolean; overload;

    property MediaType: string read FMediaType;
    property MediaSubType: string read FMediaSubType;
    property MediaParameters: TStringList read FMediaParameters;

    property QFactor: Double read FQFactor write FQFactor;
    property PFactor: Integer read FPFactor write FPFactor;
    property Version: Integer read FVersion write FVersion;
    property Dialect: string read FDialect write FDialect;
    property Charset: string read FCharset write FCharset;
    property Weight: Integer read GetWeigth;
    property IsWildcard: Boolean read GetIsWildcard;
  end;

  TMediaTypeList = class(TObjectList<TMediaType>)
  public
    constructor Create; virtual;

    function ToArrayOfString: TArray<string>;

    function Contains(const AMediaType: string): Boolean; overload;
    function Contains(const AMediaType: TMediaType): Boolean; overload;

    function GetQualityFactor(const AMediaType: string): Double;

    function Intersect(const AList: TMediaTypeList): TArray<string>; overload;
    function Intersect(const AList: TArray<string>): TArray<string>; overload;
    function IntersectList(const AList: TMediaTypeList): TMediaTypeList; overload;
    function IntersectList(const AList: TArray<string>): TMediaTypeList; overload;

    function IntersectDefault(AList: TMediaTypeList): TArray<string>;
  end;

  TAcceptParser = class
  private
    const DELIM_ACCEPT = ',';
  public
    class function ParseAccept(const AAcceptHeader: string): TMediaTypeList; static;
  end;


implementation

uses
  System.StrUtils;

{ TMediaType }

constructor TMediaType.Create(const AType, ASubType: string);
begin
  Create;
  FMediaType := AType;
  FMediaSubType := ASubType;
end;

constructor TMediaType.Create(const AType, ASubType: string; AParams: TStringList);
begin
  Create;
  FMediaType := AType;
  FMediaSubType := ASubType;
  FMediaParameters.Assign(AParams);
end;

constructor TMediaType.Create;
begin
  FQFactor := 1;
  FMediaParameters := TStringList.Create;
  FMediaParameters.Delimiter := DELIM_PARAMS;
end;

constructor TMediaType.Create(const AMediaType: string);
var
  LSplitted: TArray<string>;

  procedure ParseMediaType(AMediaTypeStr: string);
  var
    LParsed: TArray<string>;
  begin
    LParsed := TArray<string>(SplitString(AMediaTypeStr, DELIM_MEDIA));
    case Length(LParsed) of
      0: ; // Error ;
      1:
      begin
        FMediaType := Trim(LParsed[0]);
        FMediaSubType := '';
      end;
      2:
      begin
        FMediaType := Trim(LParsed[0]);
        FMediaSubType := Trim(LParsed[1]);
      end;
    end;
  end;

  procedure ParseMediaParams(AParams: TArray<string>);
  var
    LUSFormat: TFormatSettings;
    LIndex, LPosition: Integer;
  begin
    LUSFormat := TFormatSettings.Create('en-US');
    for LIndex := 1 to High(LSplitted) do
    begin
      LPosition := FMediaParameters.Add(Trim(LSplitted[LIndex]));

      if FMediaParameters.Names[LPosition] = QFACTOR_NAME then
        FQFactor := StrToFloat(FMediaParameters.ValueFromIndex[LPosition], LUSFormat)
      else if FMediaParameters.Names[LPosition] = VERSION_NAME then
        FVersion := StrToInt(FMediaParameters.ValueFromIndex[LPosition])
      else if FMediaParameters.Names[LPosition] = DIALECT_NAME then
        FDialect := FMediaParameters.ValueFromIndex[LPosition]
      else if FMediaParameters.Names[LPosition] = CHARSET_NAME then
        FCharset := FMediaParameters.ValueFromIndex[LPosition];
    end;
  end;

begin
  Create;
  // Example: text/html;q=0.5;dialect=extjs
  LSplitted := TArray<string>(SplitString(AMediaType, DELIM_PARAMS));

  case Length(LSplitted) of
    0: ; // Error
    1: ParseMediaType(LSplitted[0]);
    else
    begin
      ParseMediaType(LSplitted[0]);
      ParseMediaParams(LSplitted);
    end;
  end;
end;

constructor TMediaType.Create(const AType, ASubType, AParams: string);
begin
  Create;
  FMediaType := AType;
  FMediaSubType := ASubType;
  FMediaParameters.Text := AParams;
end;

destructor TMediaType.Destroy;
begin
  FMediaParameters.Free;
  inherited;
end;

function TMediaType.GetIsWildcard: Boolean;
begin
  Result := ToString = WILDCARD;
end;

function TMediaType.GetWeigth: Integer;
begin
  Result := Trunc(FQFactor * 10) + (FPFactor * 1);
end;

function TMediaType.Matches(const AMediaTypeStr: string): Boolean;
begin
  Result := SameText(ToString, AMediaTypeStr) or IsWildcard;
end;

function TMediaType.Matches(const AMediaType: TMediaType): Boolean;
begin
  Result := Matches(AMediaType.ToString);
end;

function TMediaType.ToString: string;
begin
  Result := FMediaType + DELIM_MEDIA + FMediaSubType;
  if FDialect <> '' then
    Result := Result + DELIM_PARAMS + DIALECT_NAME + '=' + FDialect;
  if FCharset <> '' then
    Result := Result + DELIM_PARAMS + CHARSET_NAME + '=' + FCharset;
end;

function TMediaType.ToStringDebug: string;
const
  DEBUG_STR = '%s [QFactor:%f] [PFactor:%f] [Weight:%f]';
begin
  Result := Format(DEBUG_STR, [ToString, QFactor, PFactor, Weight]);
end;

{ TAcceptParser }

class function TAcceptParser.ParseAccept(const AAcceptHeader: string): TMediaTypeList;
var
  LMediaArray: TArray<string>;
  LMediaStr: string;
  LMediaType: TMediaType;
  LIndex, LLength: Integer;
begin
  Result := TMediaTypeList.Create;
  try
    LMediaArray := TArray<string>(SplitString(AAcceptHeader, DELIM_ACCEPT));
    LLength := Length(LMediaArray);

    for LIndex := Low(LMediaArray) to High(LMediaArray) do
    begin
      LMediaStr := LMediaArray[LIndex];
      LMediaType := TMediaType.Create(LMediaStr);
      LMediaType.PFactor := LLength - LIndex;
      Result.Add(LMediaType);
    end;

    if Result.Count = 0 then
      Result.Add(TMediaType.Create(TMediaType.WILDCARD));

    Result.Sort;
  except
    Result.Free;
  end;
end;

{ TMediaTypeList }

function TMediaTypeList.Contains(const AMediaType: string): Boolean;
var
  LItem: TMediaType;
begin
  Result := False;
  for LItem in Self do
    if LItem.ToString = AMediaType then
    begin
      Result := True;
      Break;
    end;
end;

function TMediaTypeList.Contains(const AMediaType: TMediaType): Boolean;
begin
  Result := Contains(AMediaType.ToString);
end;

constructor TMediaTypeList.Create;
begin
  inherited Create(
    TComparer<TMediaType>.Construct(
      function(const Left, Right: TMediaType): Integer
      begin
        Result := Right.Weight - Left.Weight;
      end
    ), True
  );
end;

function TMediaTypeList.GetQualityFactor(const AMediaType: string): Double;
var
  LItem: TMediaType;
begin
  Result := 0.0;
  for LItem in Self do
    if LItem.ToString = AMediaType then
    begin
      Result := LItem.QFactor;
      Break;
    end;
end;

function TMediaTypeList.IntersectDefault(AList: TMediaTypeList): TArray<string>;
begin
  // AList: AMediaTypeList
  // Self: LMethodProducesMediaTypes
  if Self.Count > 0 then
    Result := Self.Intersect(AList)
  else
    Result := AList.ToArrayOfString;

  if (Length(Result) = 0) or
     ((Length(Result) = 1) and (Result[0] = TMediaType.WILDCARD))
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

function TMediaTypeList.Intersect(const AList: TArray<string>): TArray<string>;
var
  LMediaType: string;
begin
  SetLength(Result, 0);
  for LMediaType in AList do
  begin
    if Self.Contains(LMediaType) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) -1 ] := LMediaType;
    end;
  end;
end;

function TMediaTypeList.Intersect(const AList: TMediaTypeList): TArray<string>;
begin
  Result := Intersect(AList.ToArrayOfString);
end;

function TMediaTypeList.IntersectList(const AList: TMediaTypeList): TMediaTypeList;
var
  LMediaType: TMediaType;
begin
  Result := TMediaTypeList.Create;
  try
    for LMediaType in AList do
      if Self.Contains(LMediaType) then
        Result.Add(LMediaType);
  except
    Result.Free;
  end;
end;

function TMediaTypeList.IntersectList(const AList: TArray<string>): TMediaTypeList;
var
  LMediaType: string;
begin
  Result := TMediaTypeList.Create;
  try
    for LMediaType in AList do
      if Self.Contains(LMediaType) then
        Result.Add(TMediaType.Create(LMediaType));
  except
    Result.Free;
  end;
end;

function TMediaTypeList.ToArrayOfString: TArray<string>;
var
  LIndex: Integer;
begin
  SetLength(Result, Count);
  for LIndex := 0 to Count - 1 do
    Result[LIndex] := Items[LIndex].ToString;
end;

end.
