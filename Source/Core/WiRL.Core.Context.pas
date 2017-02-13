{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Context;

interface

uses
  System.Classes, System.SysUtils, System.Rtti,
  WiRL.Core.Request,
  WiRL.Core.Response,
  WiRL.Core.URL,
  WiRL.Core.Auth.Context;

type
  TWiRLContext = class
  private
    FEngine: TObject;
    FRequest: TWiRLRequest;
    FResponse: TWiRLResponse;
    FURL: TWiRLURL;
    FApplication: TObject;
    function GetURL: TWiRLURL;
  public
    destructor Destroy; override;

    property Engine: TObject read FEngine write FEngine;
    property Application: TObject read FApplication write FApplication;
    property Request: TWiRLRequest read FRequest write FRequest;
    property Response: TWiRLResponse read FResponse write FResponse;
    property URL: TWiRLURL read GetURL write FURL;
  end;

implementation

destructor TWiRLContext.Destroy;
begin
  FURL.Free;
  inherited;
end;

function TWiRLContext.GetURL: TWiRLURL;
begin
  if not Assigned(FURL) then
    FURL := TWiRLURL.Create(FRequest);
  Result := FURL;
end;

end.
