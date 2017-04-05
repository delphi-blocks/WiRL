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
  System.Classes, System.SysUtils, System.Rtti, System.Contnrs,
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
    FAuthContext: TWiRLAuthContext;
    FOwnedObjects: TObjectList;
    function GetURL: TWiRLURL;
    function GetOwnedObjects: TObjectList;
  public
    destructor Destroy; override;

    property Engine: TObject read FEngine write FEngine;
    property Application: TObject read FApplication write FApplication;
    property Request: TWiRLRequest read FRequest write FRequest;
    property Response: TWiRLResponse read FResponse write FResponse;
    property AuthContext: TWiRLAuthContext read FAuthContext write FAuthContext;
    property URL: TWiRLURL read GetURL write FURL;
    property OwnedObjects: TObjectList read GetOwnedObjects;
  end;

implementation

destructor TWiRLContext.Destroy;
begin
  FURL.Free;
  FOwnedObjects.Free;
  inherited;
end;

function TWiRLContext.GetOwnedObjects: TObjectList;
begin
  if not Assigned(FOwnedObjects) then
    FOwnedObjects := TObjectList.Create(True);
  Result := FOwnedObjects;
end;

function TWiRLContext.GetURL: TWiRLURL;
begin
  if not Assigned(FURL) then
    FURL := TWiRLURL.Create(FRequest);
  Result := FURL;
end;

end.
