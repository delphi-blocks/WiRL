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
    procedure SetRequest(const Value: TWiRLRequest);
  public
    destructor Destroy; override;

    property Engine: TObject read FEngine write FEngine;
    property Request: TWiRLRequest read FRequest write SetRequest;
    property Response: TWiRLResponse read FResponse write FResponse;
    property URL: TWiRLURL read FURL write FURL;
  end;

implementation

destructor TWiRLContext.Destroy;
begin
  FRequest.Free;
  FResponse.Free;
  FURL.Free;
  inherited;
end;

procedure TWiRLContext.SetRequest(const Value: TWiRLRequest);
begin
  if not Assigned(FURL) then
    FURL := TWiRLURL.Create(Value);
  FRequest := Value;
end;

end.
