{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Context;

interface

uses
  System.Classes, System.SysUtils, System.Rtti,
  System.Generics.Collections,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.http.URL,
  WiRL.Core.Auth.Context;

type
  TCustomContextEnumerator<T> = class(TEnumerator<T>)
  private
    FList: TList<T>;
    FIndex: Integer;
    function GetCurrent: T;
  protected
    function DoGetCurrent: T; override;
    function DoMoveNext: Boolean; override;
  public
    constructor Create(const AList: TList<T>);
    property Current: T read GetCurrent;
    function MoveNext: Boolean;
  end;

  TWiRLCustomContext = class
  private
    FList: TObjectList<TObject>;
    FOwnedObjects: TObjectList<TObject>;
  public
    constructor Create;
    destructor Destroy; override;

    function GetEnumerator: TCustomContextEnumerator<TObject>;
    procedure Add(AValue: TObject; AOwned: Boolean);
  end;

  TWiRLContext = class
  private
    FServer: TObject;
    FEngine: TObject;
    FApplication: TObject;
    FResource: TObject;
    FRequest: TWiRLRequest;
    FResponse: TWiRLResponse;
    FRequestURL: TWiRLURL;
    FAuthContext: TWiRLAuthContext;
    FCustomContext: TWiRLCustomContext;
    FResourceURL: TWiRLURL;
    function GetRequestURL: TWiRLURL;
    function GetCustomContext: TWiRLCustomContext;
    function GetResourceURL: TWiRLURL;
  public
    destructor Destroy; override;

    property Server: TObject read FServer write FServer;
    property Engine: TObject read FEngine write FEngine;
    property Application: TObject read FApplication write FApplication;
    property Resource: TObject read FResource write FResource;
    property Request: TWiRLRequest read FRequest write FRequest;
    property Response: TWiRLResponse read FResponse write FResponse;
    property AuthContext: TWiRLAuthContext read FAuthContext write FAuthContext;
    property RequestURL: TWiRLURL read GetRequestURL write FRequestURL;
    property ResourceURL: TWiRLURL read GetResourceURL write FResourceURL;
    property CustomContext: TWiRLCustomContext read GetCustomContext;
  end;

implementation

uses
  WiRL.http.Server;

destructor TWiRLContext.Destroy;
begin
  FRequestURL.Free;
  FCustomContext.Free;
  inherited;
end;

function TWiRLContext.GetCustomContext: TWiRLCustomContext;
begin
  if not Assigned(FCustomContext) then
    FCustomContext := TWiRLCustomContext.Create;
  Result := FCustomContext;
end;

function TWiRLContext.GetRequestURL: TWiRLURL;
begin
  if not Assigned(FRequestURL) then
    FRequestURL := TWiRLURL.Create(FRequest);
  Result := FRequestURL;
end;

function TWiRLContext.GetResourceURL: TWiRLURL;
begin
  if not Assigned(FResourceURL) then
    FResourceURL := TWiRLURL.Create(FRequest);
  Result := FResourceURL;
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
  FList := TObjectList<TObject>.Create(False);
  FOwnedObjects := TObjectList<TObject>.Create(True);
end;

destructor TWiRLCustomContext.Destroy;
begin
  FList.Free;
  FOwnedObjects.Free;
  inherited;
end;

function TWiRLCustomContext.GetEnumerator: TCustomContextEnumerator<TObject>;
begin
  Result := TCustomContextEnumerator<TObject>.Create(FList);
end;

{ TCustomContextEnumerator<T> }

constructor TCustomContextEnumerator<T>.Create(const AList: TList<T>);
begin
  inherited Create;
  FList := AList;
  FIndex := -1;
end;

function TCustomContextEnumerator<T>.DoGetCurrent: T;
begin
  Result := GetCurrent;
end;

function TCustomContextEnumerator<T>.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

function TCustomContextEnumerator<T>.GetCurrent: T;
begin
  Result := FList[FIndex];
end;

function TCustomContextEnumerator<T>.MoveNext: Boolean;
begin
  if FIndex >= FList.Count then
    Exit(False);
  Inc(FIndex);
  Result := FIndex < FList.Count;
end;

end.
