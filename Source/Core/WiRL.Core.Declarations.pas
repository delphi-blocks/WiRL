{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2023 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Declarations;

{$I WiRL.inc}

interface

uses
  System.SysUtils, System.Rtti;

type
  {$IFNDEF HAS_UTF8CHAR}
  UTF8Char = AnsiChar;
  {$ENDIF}

  TMethodParamType = (Path, Query, Form, Header, Cookie, Body, FormData, MultiPart);

  TAttributeArray = TArray<TCustomAttribute>;
  TArgumentArray = array of TValue;
  TStringArray = TArray<string>;

  { TODO -opaolo -c : Transform in a record 13/01/2021 17:02:50 }
  TWiRLStringArray = TArray<string>;
  {$IFDEF NO_ARRAY_HELPER_BUG}
  TWiRLStringArrayHelper = record helper for TWiRLStringArray
  public
    function Length: Integer;
    function Contains(const AValue: string): Boolean;
    procedure FromString(const AValue: string);
    function ToString: string;
    function Size: Integer;
    function IsEmpty: Boolean;

    class function New(const AValue: string): TWiRLStringArray;
  end;
  {$ENDIF}


const
  WiRLVersion = 450;
  WIRL_VERSION_STR = '4.5.0';


implementation

{$IFDEF NO_ARRAY_HELPER_BUG}

{ TWiRLStringArrayHelper }

function TWiRLStringArrayHelper.Contains(const AValue: string): Boolean;
var
  LItem: string;
begin
  Result := False;
  for LItem in Self do
  begin
    if SameText(LItem, AValue) then
      Exit(True);
  end;
end;

procedure TWiRLStringArrayHelper.FromString(const AValue: string);
begin
  Self := AValue.Split([', ', ',']);
end;

function TWiRLStringArrayHelper.Length: Integer;
begin
  Result := System.Length(Self);
end;

class function TWiRLStringArrayHelper.New(const AValue: string): TWiRLStringArray;
begin
  Result.FromString(AValue);
end;

function TWiRLStringArrayHelper.ToString: string;
begin
  Result := string.Join(', ', Self);
end;

function TWiRLStringArrayHelper.Size: Integer;
begin
  Result := System.Length(Self);
end;

function TWiRLStringArrayHelper.IsEmpty: Boolean;
begin
  Result := System.Length(Self) = 0;
end;

{$ENDIF}

end.

