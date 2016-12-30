{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Classes;

interface

uses
  System.SysUtils, System.Generics.Collections, System.Classes,
  WiRL.Core.Declarations;

type
  TNonInterfacedObject = class(TObject, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

  TUTF8EncodingNoBOM = class(TUTF8Encoding)
  public
    function GetPreamble: TBytes; override;
  end;

  TUnicodeLEEncodingNoBOM = class(TUnicodeEncoding)
  public
    function GetPreamble: TBytes; override;
  end;

  TUnicodeBEEncodingNoBOM = class(TBigEndianUnicodeEncoding)
  public
    function GetPreamble: TBytes; override;
  end;


implementation

uses
  WiRL.Core.Utils;

{ TNonInterfacedObject }

function TNonInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TNonInterfacedObject._AddRef: Integer;
begin
  Result := -1;
end;

function TNonInterfacedObject._Release: Integer;
begin
  Result := -1;
end;

{ TUTF8EncodingNoBOM }

function TUTF8EncodingNoBOM.GetPreamble: TBytes;
begin
  SetLength(Result, 0);
end;

{ TUnicodeLEEncodingNoBOM }

function TUnicodeLEEncodingNoBOM.GetPreamble: TBytes;
begin
  SetLength(Result, 0);
end;

{ TUnicodeBEEncodingNoBOM }

function TUnicodeBEEncodingNoBOM.GetPreamble: TBytes;
begin
  SetLength(Result, 0);
end;

end.
