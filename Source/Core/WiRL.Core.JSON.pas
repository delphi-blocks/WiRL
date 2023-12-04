{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.JSON;

interface

uses
  System.JSON, System.SysUtils, System.Classes, System.Generics.Collections;

type
  TJSONAncestor = System.JSON.TJSONAncestor;
  TJSONPair     = System.JSON.TJSONPair;
  TJSONValue    = System.JSON.TJSONValue;
  TJSONTrue     = System.JSON.TJSONTrue;
  TJSONFalse    = System.JSON.TJSONFalse;
  TJSONString   = System.JSON.TJSONString;
  TJSONNumber   = System.JSON.TJSONNumber;
  TJSONObject   = System.JSON.TJSONObject;
  TJSONNull     = System.JSON.TJSONNull;
  TJSONArray    = System.JSON.TJSONArray;

  TJSONHelper = class
  public
    class function Print(AJSONValue: TJSONValue; APretty: Boolean): string; static;
    class procedure PrintToWriter(AJSONValue: TJSONValue; AWriter: TTextWriter; APretty: Boolean); overload; static;
    class procedure PrintToWriter(const AJSONString: string; AWriter: TTextWriter; APretty: Boolean); overload; static;
    class function PrettyPrint(const AJSONString: string): string; static;

    class function ToJSON(AJSONValue: TJSONValue): string; static;
    class function StringArrayToJsonArray(const values: TArray<string>): string; static;
    class procedure JSONCopyFrom(ASource, ADestination: TJSONObject); static;

    class function BooleanToTJSON(AValue: Boolean): TJSONValue;
    class function DateToJSON(ADate: TDateTime; AInputIsUTC: Boolean = True): string; static;
    class function JSONToDate(const ADate: string; AReturnUTC: Boolean = True): TDateTime; static;
  end;

implementation

uses
  System.DateUtils,
  System.Variants,
  WiRL.Core.Utils;

{ TJSONHelper }

class function TJSONHelper.BooleanToTJSON(AValue: Boolean): TJSONValue;
begin
  if AValue then
    Result := TJSONTrue.Create
  else
    Result := TJSONFalse.Create;
end;

class function TJSONHelper.DateToJSON(ADate: TDateTime; AInputIsUTC: Boolean = True): string;
begin
  Result := '';
  if ADate <> 0 then
    Result := DateToISO8601(ADate, AInputIsUTC);
end;

class function TJSONHelper.JSONToDate(const ADate: string; AReturnUTC: Boolean = True): TDateTime;
begin
  Result := 0.0;
  if ADate<>'' then
    Result := ISO8601ToDate(ADate, AReturnUTC);
end;

class function TJSONHelper.ToJSON(AJSONValue: TJSONValue): string;
var
  LBytes: TBytes;
begin
  SetLength(LBytes, AJSONValue.ToString.Length * 6);
  SetLength(LBytes, AJSONValue.ToBytes(LBytes, 0));
  Result := TEncoding.Default.GetString(LBytes);
end;

class function TJSONHelper.StringArrayToJsonArray(const values: TArray<string>): string;
var
  LArray: TJSONArray;
  LIndex: Integer;
begin
  LArray := TJSONArray.Create;
  try
    for LIndex := 0 to High(values) do
      LArray.Add(values[LIndex]);
    Result := ToJSON(LArray);
  finally
    LArray.Free;
  end;
end;

class procedure TJSONHelper.JSONCopyFrom(ASource, ADestination: TJSONObject);
var
  LPairSrc, LPairDst: TJSONPair;
begin
  for LPairSrc in ASource do
  begin
    LPairDst := ADestination.Get(LPairSrc.JsonString.Value);
    if Assigned(LPairDst) then
      // Replace the JSON Value (the previous is freed by the TJSONPair object)
      LPairDst.JsonValue := TJSONValue(LPairSrc.JsonValue.Clone)
    else
      ADestination.AddPair(TJSONPair(LPairSrc.Clone));
  end;
end;

class function TJSONHelper.PrettyPrint(const AJSONString: string): string;
var
  LWriter: TStringWriter;
begin
  LWriter := TStringWriter.Create;
  try
    PrintToWriter(AJSONString, LWriter, True);
    Result := LWriter.ToString;
  finally
    LWriter.Free;
  end;
end;

class function TJSONHelper.Print(AJSONValue: TJSONValue; APretty: Boolean): string;
var
  LWriter: TStringWriter;
begin
  LWriter := TStringWriter.Create;
  try
    PrintToWriter(AJSONValue, LWriter, APretty);
    Result := LWriter.ToString;
  finally
    LWriter.Free;
  end;
end;

class procedure TJSONHelper.PrintToWriter(AJSONValue: TJSONValue;
  AWriter: TTextWriter; APretty: Boolean);
begin
  PrintToWriter(AJSONValue.ToJSON, AWriter, APretty);
end;

class procedure TJSONHelper.PrintToWriter(const AJSONString: string; AWriter:
    TTextWriter; APretty: Boolean);
var
  LChar: Char;
  LOffset: Integer;
  LIndex: Integer;
  LOutsideString: Boolean;

  function Spaces(AOffset: Integer): string;
  begin
    Result := StringOfChar(#32, AOffset * 2);
  end;

begin
  if not APretty then
  begin
    AWriter.Write(AJSONString);
    Exit;
  end;

  LOffset := 0;
  LOutsideString := True;

  for LIndex := 0 to Length(AJSONString) - 1 do
  begin
    LChar := AJSONString.Chars[LIndex];

    if LChar = '"' then
      LOutsideString := not LOutsideString;

    if LOutsideString and (LChar = '{') then
    begin
      Inc(LOffset);
      AWriter.Write(LChar);
      AWriter.Write(sLineBreak);
      AWriter.Write(Spaces(LOffset));
    end
    else if LOutsideString and (LChar = '}') then
    begin
      Dec(LOffset);
      AWriter.Write(sLineBreak);
      AWriter.Write(Spaces(LOffset));
      AWriter.Write(LChar);
    end
    else if LOutsideString and (LChar = ',') then
    begin
      AWriter.Write(LChar);
      AWriter.Write(sLineBreak);
      AWriter.Write(Spaces(LOffset));
    end
    else if LOutsideString and (LChar = '[') then
    begin
      Inc(LOffset);
      AWriter.Write(LChar);
      AWriter.Write(sLineBreak);
      AWriter.Write(Spaces(LOffset));
    end
    else if LOutsideString and (LChar = ']') then
    begin
      Dec(LOffset);
      AWriter.Write(sLineBreak);
      AWriter.Write(Spaces(LOffset));
      AWriter.Write(LChar);
    end
    else if LOutsideString and (LChar = ':') then
    begin
      AWriter.Write(LChar);
      AWriter.Write(' ');
    end
    else
      AWriter.Write(LChar);
  end;
end;

end.
