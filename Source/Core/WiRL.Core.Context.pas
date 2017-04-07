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
  TCustomContextEnumerator = class
  private
    FIndex: Integer;
    FList: TList;
  public
    constructor Create(AList: TList);
    function GetCurrent: TObject; inline;
    function MoveNext: Boolean;
    property Current: TObject read GetCurrent;
  end;

  TWiRLCustomContext = class
  private
    FList: TObjectList;
    FOwnedObjects: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    function GetEnumerator: TCustomContextEnumerator;
    procedure Add(AValue: TObject; AOwned: Boolean);
  end;

  TWiRLContext = class
  private
    FEngine: TObject;
    FRequest: TWiRLRequest;
    FResponse: TWiRLResponse;
    FURL: TWiRLURL;
    FApplication: TObject;
    FAuthContext: TWiRLAuthContext;
    FCustomContext: TWiRLCustomContext;
    function GetURL: TWiRLURL;
    function GetCustomContext: TWiRLCustomContext;
  public
    destructor Destroy; override;

    property Engine: TObject read FEngine write FEngine;
    property Application: TObject read FApplication write FApplication;
    property Request: TWiRLRequest read FRequest write FRequest;
    property Response: TWiRLResponse read FResponse write FResponse;
    property AuthContext: TWiRLAuthContext read FAuthContext write FAuthContext;
    property URL: TWiRLURL read GetURL write FURL;
    property CustomContext: TWiRLCustomContext read GetCustomContext;
  end;

implementation

destructor TWiRLContext.Destroy;
begin
  FURL.Free;
  FCustomContext.Free;
  inherited;
end;

function TWiRLContext.GetCustomContext: TWiRLCustomContext;
begin
  if not Assigned(FCustomContext) then
    FCustomContext := TWiRLCustomContext.Create;
  Result := FCustomContext;
end;

function TWiRLContext.GetURL: TWiRLURL;
begin
  if not Assigned(FURL) then
    FURL := TWiRLURL.Create(FRequest);
  Result := FURL;
end;

{ TWiRLCustomContext }

procedure TWiRLCustomContext.Add(AValue: TObject; AOwned: Boolean);
begin
  FList.Add(AValue);
  if AOwned then
    FOwnedObjects.Add(AValue);
end;

constructor TWiRLCustomContext.Create;
begin
  inherited;
  FList := TObjectList.Create(False);
  FOwnedObjects := TObjectList.Create(True);
end;

destructor TWiRLCustomContext.Destroy;
begin
  FList.Free;
  FOwnedObjects.Free;
  inherited;
end;

function TWiRLCustomContext.GetEnumerator: TCustomContextEnumerator;
begin
  Result := TCustomContextEnumerator.Create(FList);
end;

{ TCustomContextEnumerator }

constructor TCustomContextEnumerator.Create(AList: TList);
begin
  FIndex := -1;
  FList := AList;
end;

function TCustomContextEnumerator.GetCurrent: TObject;
begin
  Result := FList[FIndex];
end;

function TCustomContextEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FList.Count - 1;
  if Result then
    Inc(FIndex);
end;

end.
