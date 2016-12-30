{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Accept.Charset;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Defaults, System.Generics.Collections,

  WiRL.Core.Declarations,
  WiRL.http.Accept.Parser;

type
  TAcceptCharset = class(TAcceptItem)
  public
    const ISO_8859_1 = 'iso-8859-1';
    const UTF8 = 'utf-8';
    const UTF16 = 'utf-16';
    const WILDCARD = '*';
  public
    class function GetWildcard: string; override;
  end;

  TAcceptCharsetList = class(TAcceptItemList<TAcceptCharset>)
  end;

implementation

class function TAcceptCharset.GetWildcard: string;
begin
  Result := WILDCARD;
end;

end.
