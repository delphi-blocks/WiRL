{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Core;

interface

uses
  System.SysUtils, System.Classes;

type
  TWiRLHeaderList = class(TStringList)
  private
    function GetName(AIndex: Integer): string;
    function GetValue(const AName: string): string;
    procedure SetValue(const AName, AValue: string);
    function GetValueFromLine(AIndex: Integer): string;
  public
    function IndexOfName(const AName: string): Integer; reintroduce;
    property Names[Index: Integer]: string read GetName;
    property Values[const Name: string]: string read GetValue write SetValue; default;
  end;

  TWiRLParam = class(TStringList)
  private
    function GetValue(const Name: string): string;
    procedure SetValue(const Name, Value: string);
  public
    property Values[const Name: string]: string read GetValue write SetValue; default;
  end;

var
  GetDefaultCharSetEncoding: TEncoding = nil;

function EncodingFromCharSet(const ACharset: string): TEncoding;

implementation

function DefaultCharSetEncoding: TEncoding;
begin
  Result := nil;
  if Assigned(GetDefaultCharSetEncoding) then
    Result := GetDefaultCharSetEncoding;
  if Result = nil then
    Result := TEncoding.UTF8;
end;

function EncodingFromCharSet(const ACharset: string): TEncoding;
begin
  if CompareText('utf-8', ACharset) = 0 then
    Result := TEncoding.UTF8
  else if CompareText('ISO-8859-1', ACharset) = 0 then
    Result := TEncoding.ANSI
  else if CompareText('ANSI', ACharset) = 0 then
    Result := TEncoding.ANSI
  else if CompareText('ASCII', ACharset) = 0 then
    Result := TEncoding.ASCII
  else
    Result := DefaultCharSetEncoding;
end;

{ TWiRLHeaderList }

const
  HeaderNameValueSeparator = ': ';

function TWiRLHeaderList.GetName(AIndex: Integer): string;
var
  LLine: string;
  LTrimmedSeparator: string;
  LSepIndex: Integer;
begin
  if (AIndex >= 0) and (AIndex < Count) then
  begin
    LLine := Get(AIndex);
    LTrimmedSeparator := Trim(HeaderNameValueSeparator); // Sometimes the space is not present
    LSepIndex := LLine.IndexOf(LTrimmedSeparator);
    Result := LLine.Substring(0, LSepIndex).Trim;
  end
  else
  begin
    Result := '';
  end;
end;

function TWiRLHeaderList.GetValueFromLine(AIndex: Integer): string;
var
  LLine: string;
  LTrimmedSeparator: string;
  LSepIndex: Integer;
begin
  if (AIndex >= 0) and (AIndex < Count) then
  begin
    LLine := Get(AIndex);
    LTrimmedSeparator := Trim(HeaderNameValueSeparator); // Sometimes the space is not present
    LSepIndex := LLine.IndexOf(LTrimmedSeparator);
    Result := LLine.Substring(LSepIndex + 1).Trim;
  end
  else
  begin
    Result := '';
  end;
end;

function TWiRLHeaderList.GetValue(const AName: string): string;
var
  LIndex: Integer;
begin
  LIndex := IndexOfName(AName);
  Result := GetValueFromLine(LIndex);
end;

function TWiRLHeaderList.IndexOfName(const AName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if CompareText(GetName(i), AName) = 0 then
    begin
      Exit(i);
    end;
  end;
end;

procedure TWiRLHeaderList.SetValue(const AName, AValue: string);
var
  LIndex: Integer;
begin
  LIndex := IndexOfName(AName);
  if AValue <> '' then
  begin
    if LIndex < 0 then
      LIndex := Add('');
    Put(LIndex, AName + HeaderNameValueSeparator + AValue);
  end
  else if LIndex >= 0 then
    Delete(LIndex);
end;

{ TWiRLParam }

function TWiRLParam.GetValue(const Name: string): string;
begin
  Result := inherited Values[Name];
end;

procedure TWiRLParam.SetValue(const Name, Value: string);
begin
  inherited Values[Name] := Value;
end;

end.
