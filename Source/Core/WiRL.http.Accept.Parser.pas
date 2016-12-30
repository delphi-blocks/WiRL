{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Accept.Parser;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Defaults,
  System.Generics.Collections,

  WiRL.Core.Declarations;

type
  TAcceptItem = class abstract
  protected
    const DELIM_PARAMS = ';';
    const DELIM_VALUE = '=';
    const QFACTOR_NAME = 'q';
  protected
    FOriginalItem: string;
    FAcceptItemOnly: string;
    FParameters: TStringList;
    FPFactor: Integer;
    FQFactor: Double;
    function GetIsWildcard: Boolean;
    function GetWeigth: Integer; virtual;
  public
    constructor Create(const AAcceptItem: string); virtual;
    destructor Destroy; override;

    function ToString: string; override;
    function ToStringDebug: string; virtual;
    procedure Parse(const AAcceptItem: string); virtual;

    function Matches(const AAcceptItem: string): Boolean; overload;

    class function GetWildcard: string; virtual;

    property AcceptItemOnly: string read FAcceptItemOnly write FAcceptItemOnly;
    property Parameters: TStringList read FParameters;
    property PFactor: Integer read FPFactor write FPFactor;
    property QFactor: Double read FQFactor write FQFactor;
    property Weight: Integer read GetWeigth;
    property IsWildcard: Boolean read GetIsWildcard;
  end;

  TAcceptItemList<T: TAcceptItem> = class(TObjectList<T>)
  public
    constructor Create; virtual;

    function ToArrayOfString: TArray<string>;
    function GetQualityFactor(const AItem: string): Double;

    function Contains(const AAcceptItem: string): Boolean; overload;
    function Contains(const AAcceptItem: T): Boolean; overload;

    function Intersection(const AList: TAcceptItemList<T>): TArray<string>; overload;
    function Intersection(const AList: TArray<string>): TArray<string>; overload;
    function IntersectionList(const AList: TAcceptItemList<T>): TAcceptItemList<T>; overload;
    function IntersectionList(const AList: TArray<string>): TAcceptItemList<T>; overload;

    function Intersected(AList: TAcceptItemList<T>): Boolean;
  end;

  TAcceptHeaderParser<T: TAcceptItem> = class
  private
    const DELIM_ACCEPT = ',';
  public
    class procedure Parse(const AAcceptHeader: string; AList: TAcceptItemList<T>); static;
  end;

implementation

uses
  System.StrUtils;

constructor TAcceptItem.Create(const AAcceptItem: string);
begin
  FQFactor := 1;
  FParameters := TStringList.Create;
  FParameters.Delimiter := DELIM_PARAMS;

  Parse(AAcceptItem);
end;

destructor TAcceptItem.Destroy;
begin
  FParameters.Free;
  inherited;
end;

procedure TAcceptItem.Parse(const AAcceptItem: string);
var
  LUSFormat: TFormatSettings;
  LIndex: Integer;
  LParts: TStringArray;
  LPosition: Integer;
begin
  FOriginalItem := AAcceptItem;
  FParameters.Clear;

  LUSFormat := TFormatSettings.Create('en-US');

  LParts := AAcceptItem.Split([DELIM_PARAMS]);

  if LParts.Size > 0 then
    FAcceptItemOnly := Trim(LParts[0]);

  for LIndex := 1 to High(LParts) do
  begin
    LPosition := FParameters.Add(Trim(LParts[LIndex]));
    if FParameters.Names[LPosition] = QFACTOR_NAME then
      FQFactor := StrToFloat(FParameters.ValueFromIndex[LPosition], LUSFormat)
  end;
end;

function TAcceptItem.GetIsWildcard: Boolean;
begin
  Result := FAcceptItemOnly = GetWildcard;
end;

function TAcceptItem.GetWeigth: Integer;
begin
  Result := Trunc(FQFactor * 100) + (FPFactor * 1);
end;

class function TAcceptItem.GetWildcard: string;
begin
  Result := '*';
end;

function TAcceptItem.Matches(const AAcceptItem: string): Boolean;
begin
  Result := SameText(AcceptItemOnly, AAcceptItem) or IsWildcard;
end;

function TAcceptItem.ToString: string;
var
  LParam: string;
begin
  Result := FAcceptItemOnly;

  for LParam in FParameters do
    Result := Result + DELIM_PARAMS + LParam
end;

function TAcceptItem.ToStringDebug: string;
const
  DEBUG_STR = '%s [QFactor:%f] [PFactor:%f] [Weight:%f]';
begin
  Result := Format(DEBUG_STR, [ToString, QFactor, PFactor, Weight]);
end;

function TAcceptItemList<T>.Contains(const AAcceptItem: string): Boolean;
var
  LTempItem: T;
begin
  LTempItem := T.Create(AAcceptItem);
  try
    Result := Contains(LTempItem);
  finally
    LTempItem.Free;
  end;
end;

function TAcceptItemList<T>.Contains(const AAcceptItem: T): Boolean;
var
  LItem: T;
begin
  Result := False;
  for LItem in Self do
    if LItem.AcceptItemOnly = AAcceptItem.AcceptItemOnly then
    begin
      Result := True;
      Break;
    end;
end;

constructor TAcceptItemList<T>.Create;
begin
  inherited Create(
    TComparer<T>.Construct(
      function(const Left, Right: T): Integer
      begin
        Result := Right.Weight - Left.Weight;
      end
    ), True
  );
end;

function TAcceptItemList<T>.GetQualityFactor(const AItem: string): Double;
var
  LItem: T;
begin
  Result := 0.0;
  for LItem in Self do
    if LItem.ToString = AItem then
    begin
      Result := LItem.QFactor;
      Break;
    end;
end;

function TAcceptItemList<T>.Intersected(AList: TAcceptItemList<T>): Boolean;
var
  LIntersection: TStringArray;
begin
  if Self.Count = 0 then
    Self.Add(T.Create(T.GetWildcard));

  LIntersection := Self.Intersection(AList);

  Result := not LIntersection.IsEmpty;
end;

function TAcceptItemList<T>.Intersection(const AList: TArray<string>): TArray<string>;
var
  LItem: string;
begin
  SetLength(Result, 0);
  for LItem in AList do
  begin
    if Contains(LItem) or Contains(T.GetWildcard) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := LItem;
    end;
  end;
end;

function TAcceptItemList<T>.Intersection(const AList: TAcceptItemList<T>): TArray<string>;
begin
  Result := Intersection(AList.ToArrayOfString);
end;

function TAcceptItemList<T>.IntersectionList(const AList: TArray<string>): TAcceptItemList<T>;
var
  LItem: string;
begin
  Result := TAcceptItemList<T>.Create;
  try
    for LItem in AList do
      if Self.Contains(LItem) then
        Result.Add(T.Create(LItem));
  except
    Result.Free;
  end;
end;

function TAcceptItemList<T>.IntersectionList(const AList: TAcceptItemList<T>): TAcceptItemList<T>;
var
  LItem: T;
begin
  Result := TAcceptItemList<T>.Create;
  try
    for LItem in AList do
      if Self.Contains(LItem) then
        Result.Add(LItem);
  except
    Result.Free;
  end;
end;

function TAcceptItemList<T>.ToArrayOfString: TArray<string>;
var
  LIndex: Integer;
begin
  SetLength(Result, Count);
  for LIndex := 0 to Count - 1 do
    Result[LIndex] := Items[LIndex].ToString;
end;

{ TAcceptHeaderParser }

class procedure TAcceptHeaderParser<T>.Parse(const AAcceptHeader: string; AList: TAcceptItemList<T>);
var
  LItems: TStringArray;
  LItemString: string;
  LItem: TAcceptItem;
  LIndex, LLength: Integer;
begin
  LItems := AAcceptHeader.Split([DELIM_ACCEPT]);
  LLength := LItems.Size;

  for LIndex := Low(LItems) to High(LItems) do
  begin
    LItemString := Trim(LItems[LIndex]);
    LItem := T.Create(LItemString);
    LItem.PFactor := LLength - LIndex;
    AList.Add(LItem);
  end;

  AList.Sort;
end;

end.