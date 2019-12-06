{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Accept.Parser;

{$I WiRL.inc}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Defaults,
  System.Generics.Collections,

  WiRL.Core.Declarations;

type
  THeaderItem = class abstract
  protected
    const DELIM_PARAMS = ';';
    const DELIM_VALUE = '=';
  protected
    FValue: string;
    FOriginalItem: string;
    FParameters: TStringList;
    procedure Assign(ASource: THeaderItem);
  public
    constructor Create(const AHeaderItem: string); virtual;
    destructor Destroy; override;
    procedure Parse(const AHeaderItem: string); virtual;
    property Parameters: TStringList read FParameters;
    property Value: string read FValue write FValue;
  end;

  TAcceptItem = class abstract(THeaderItem)
  protected
    const QFACTOR_NAME = 'q';
  protected
    FPFactor: Integer;
    FQFactor: Double;
    function GetIsWildcard: Boolean;
    function GetWeigth: Integer; virtual;
    procedure Assign(ASource: TAcceptItem);
  public
    constructor Create(const AHeaderItem: string); override;

    function ToString: string; override;
    function ToStringDebug: string; virtual;
    procedure Parse(const AHeaderItem: string); override;

    function Matches(const AAcceptItem: string): Boolean; overload;

    class function GetWildcard: string; virtual;

    property AcceptItemOnly: string read FValue write FValue;
    property PFactor: Integer read FPFactor write FPFactor;
    property QFactor: Double read FQFactor write FQFactor;
    property Weight: Integer read GetWeigth;
    property IsWildcard: Boolean read GetIsWildcard;
  end;

  TAcceptItemClass = class of TAcceptItem;

  TAcceptItemFactory<T: TAcceptItem> = class
  public
    class function CreateItem(const AAcceptItem: string): T;
  end;

  TAcceptItemList<T: TAcceptItem> = class(TObjectList<T>)
  private
    function GetEmpty: Boolean;
  public
    constructor Create; virtual;

    function ToString: string; override;

    function ToArrayOfString: TArray<string>;
    function GetWeight(const AItem: string): Integer;
    function GetQualityFactor(const AItem: string): Double;

    function Contains(const AAcceptItem: string): Boolean; overload;
    function Contains(const AAcceptItem: T): Boolean; overload;

    function Intersection(const AList: TAcceptItemList<T>): TArray<string>; overload;
    function Intersection(const AList: TArray<string>): TArray<string>; overload;
    function IntersectionList(const AList: TArray<string>): TAcceptItemList<T>; overload;

    function Intersected(AList: TAcceptItemList<T>): Boolean;

    property Empty: Boolean read GetEmpty;
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

procedure TAcceptItem.Assign(ASource: TAcceptItem);
begin
  inherited Assign(ASource);
  Self.AcceptItemOnly := ASource.AcceptItemOnly;
  Self.PFactor := ASource.PFactor;
  Self.QFactor := ASource.QFactor;
end;

constructor TAcceptItem.Create(const AHeaderItem: string);
begin
  inherited Create(AHeaderItem);
  FQFactor := 1;
end;

procedure TAcceptItem.Parse(const AHeaderItem: string);
var
  LUSFormat: TFormatSettings;
  LPosition: Integer;
begin
  inherited Parse(AHeaderItem);
  LUSFormat := TFormatSettings.Create('en-US');

  for LPosition := 0 to Parameters.Count - 1 do
  begin
    if FParameters.Names[LPosition] = QFACTOR_NAME then
      FQFactor := StrToFloat(FParameters.ValueFromIndex[LPosition], LUSFormat)
  end;
end;

function TAcceptItem.GetIsWildcard: Boolean;
begin
  Result := FValue = GetWildcard;
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
  Result := FValue;

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
  LTempItem := TAcceptItemFactory<T>.CreateItem(AAcceptItem);
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

function TAcceptItemList<T>.GetEmpty: Boolean;
begin
  Result := Count = 0;
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

function TAcceptItemList<T>.GetWeight(const AItem: string): Integer;
var
  LItem: T;
begin
  Result := 0;
  for LItem in Self do
    if LItem.ToString = AItem then
    begin
      Result := LItem.Weight;
      Break;
    end;
end;

function TAcceptItemList<T>.Intersected(AList: TAcceptItemList<T>): Boolean;
var
  LIntersection: TStringArray;
begin
  if Self.Count = 0 then
    Exit(True);

  LIntersection := Self.Intersection(AList);

  Result := Length(LIntersection) > 0;
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
        Result.Add(TAcceptItemFactory<T>.CreateItem(LItem))
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

function TAcceptItemList<T>.ToString: string;
var
  LItem: T;
begin
  Result := '';
  for LItem in Self do
    Result := Result + LItem.ToString + ',';
  if Result.Length > 0 then
    Result := Result.Substring(0, Result.Length - 1);
end;

class procedure TAcceptHeaderParser<T>.Parse(const AAcceptHeader: string; AList: TAcceptItemList<T>);
var
  LItems: TStringArray;
  LItemString: string;
  LItem: T;
  LIndex, LLength: Integer;
begin
  LItems := AAcceptHeader.Split([DELIM_ACCEPT]);
  LLength := Length(LItems);

  for LIndex := Low(LItems) to High(LItems) do
  begin
    LItemString := Trim(LItems[LIndex]);
    LItem := TAcceptItemFactory<T>.CreateItem(LItemString);
    LItem.PFactor := LLength - LIndex;
    AList.Add(LItem);
  end;

  AList.Sort;
end;

{ TAcceptItemFactory<T> }

class function TAcceptItemFactory<T>.CreateItem(const AAcceptItem: string): T;
begin
{$IFDEF HAS_GENERIC_CREATE}
  Result := T.Create(AAcceptItem);
{$ELSE}
  Result := T(TAcceptItemClass(T).Create(AAcceptItem));
{$ENDIF}
end;

constructor THeaderItem.Create(const AHeaderItem: string);
begin
  FParameters := TStringList.Create;
  FParameters.Delimiter := DELIM_PARAMS;

  Parse(AHeaderItem);
end;

destructor THeaderItem.Destroy;
begin
  FParameters.Free;
  inherited;
end;

procedure THeaderItem.Assign(ASource: THeaderItem);
begin
  Self.Parameters.Assign(ASource.Parameters);
end;

procedure THeaderItem.Parse(const AHeaderItem: string);
var
  LIndex: Integer;
  LParts: TStringArray;
begin
  FOriginalItem := AHeaderItem;
  FParameters.Clear;

  LParts := AHeaderItem.Split([DELIM_PARAMS]);

  if Length(LParts) > 0 then
    FValue := Trim(LParts[0]);

  for LIndex := 1 to High(LParts) do
  begin
    FParameters.Add(Trim(LParts[LIndex]));
  end;
end;

end.