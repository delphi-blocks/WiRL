{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Declarations;

interface

uses
  System.SysUtils, System.Rtti;

type
  TAttributeArray = TArray<TCustomAttribute>;
  TArgumentArray = array of TValue;

  TStringArray = TArray<string>;
  TStringArrayHelper = record helper for TStringArray
  public
    function Size: Integer;
    function IsEmpty: Boolean;
  end;

implementation

{ TStringArrayHelper }

function TStringArrayHelper.IsEmpty: Boolean;
begin
  Result := Length(Self) = 0;
end;

function TStringArrayHelper.Size: Integer;
begin
  Result := Length(Self);
end;

end.
