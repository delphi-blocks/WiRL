{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Accept.Encoding;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Defaults, System.Generics.Collections,

  WiRL.Core.Declarations,
  WiRL.http.Accept.Parser;

type
  TAcceptEncoding = class(TAcceptItem)
  public
    const GZIP = 'gzip';
    const BROTLI = 'br';
    const DEFLATE = 'deflate';
    const IDENTITY = 'identity';
    const WILDCARD = '*';
  public
    constructor Create(const AAcceptItem: string); override;
    class function GetWildcard: string; override;
  end;

  TAcceptEncodingList = class(TAcceptItemList<TAcceptEncoding>)
  public
  end;

implementation

{ TAcceptEncoding }

constructor TAcceptEncoding.Create(const AAcceptItem: string);
begin
  inherited Create(AAcceptItem);
end;

class function TAcceptEncoding.GetWildcard: string;
begin
  Result := WILDCARD;
end;

end.
