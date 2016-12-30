{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.JSON;

{$I WiRL.inc}

interface

uses
  {$IFDEF DelphiXE6_UP} // XE6 and higher
  System.JSON,
  {$ELSE}
  Data.DBXJSON,
  {$ENDIF}
  System.SysUtils, System.Generics.Collections;

type
  TJSONAncestor = {$IFDEF DelphiXE6_UP}System.JSON.TJSONAncestor{$ELSE}Data.DBXJSON.TJSONAncestor{$ENDIF};
  TJSONPair = {$IFDEF DelphiXE6_UP}System.JSON.TJSONPair{$ELSE}Data.DBXJSON.TJSONPair{$ENDIF};
  TJSONValue = {$IFDEF DelphiXE6_UP}System.JSON.TJSONValue{$ELSE}Data.DBXJSON.TJSONValue{$ENDIF};
  TJSONTrue = {$IFDEF DelphiXE6_UP}System.JSON.TJSONTrue{$ELSE}Data.DBXJSON.TJSONTrue{$ENDIF};
  TJSONString = {$IFDEF DelphiXE6_UP}System.JSON.TJSONString{$ELSE}Data.DBXJSON.TJSONString{$ENDIF};
  TJSONNumber = {$IFDEF DelphiXE6_UP}System.JSON.TJSONNumber{$ELSE}Data.DBXJSON.TJSONNumber{$ENDIF};
  TJSONObject = {$IFDEF DelphiXE6_UP}System.JSON.TJSONObject{$ELSE}Data.DBXJSON.TJSONObject{$ENDIF};
  TJSONNull = {$IFDEF DelphiXE6_UP}System.JSON.TJSONNull{$ELSE}Data.DBXJSON.TJSONNull{$ENDIF};
  TJSONFalse = {$IFDEF DelphiXE6_UP}System.JSON.TJSONFalse{$ELSE}Data.DBXJSON.TJSONFalse{$ENDIF};
  TJSONArray = {$IFDEF DelphiXE6_UP}System.JSON.TJSONArray{$ELSE}Data.DBXJSON.TJSONArray{$ENDIF};

  TJSONHelper = class
  public
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
  LPair: TJSONPair;
begin
  for LPair in ASource do
    ADestination.AddPair(TJSONPair(LPair.Clone));
end;

end.
