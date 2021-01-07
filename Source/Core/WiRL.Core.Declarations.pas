{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
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

  TAttributeArray = TArray<TCustomAttribute>;
  TArgumentArray = array of TValue;

  TStringArray = TArray<string>;
  {$IFDEF NO_ARRAY_HELPER_BUG}
  TStringArrayHelper = record helper for TStringArray
  public
    function Size: Integer;
    function IsEmpty: Boolean;
  end;
  {$ENDIF}


implementation

{$IFDEF NO_ARRAY_HELPER_BUG}

{ TStringArrayHelper }

function TStringArrayHelper.IsEmpty: Boolean;
begin
  Result := Length(Self) = 0;
end;

function TStringArrayHelper.Size: Integer;
begin
  Result := Length(Self);
end;
{$ENDIF}

end.

