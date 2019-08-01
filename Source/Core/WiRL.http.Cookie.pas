{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Cookie;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.Generics.Defaults;

type
  // This class store both client and server cookie

  TWiRLCookie = class
  private
    FName: string;
    FValue: string;
    FPath: String;
    FDomain: String;
    FSecure: Boolean;
    FHttpOnly: Boolean;
    FCreatedAt: TDateTime;
    FLastAccessed: TDateTime;
    FExpires: TDateTime;
    FHostOnly: Boolean;
    FPersistent: Boolean;
    procedure SetClientCookie(const Value: string);
    function GetClientCookie: string;
    function GetServerCookie: string;
    function GetIsExpired: Boolean;
    function GetMaxAge: Int64;
  public
    property CookieName: string read FName write FName;
    property Value: string read FValue write FValue;
    property Domain: String read FDomain write FDomain;
    property Expires: TDateTime read FExpires write FExpires;
    property HttpOnly: Boolean read FHttpOnly write FHttpOnly;
    property Path: String read FPath write FPath;
    property Secure: Boolean read FSecure write FSecure;
    property MaxAge: Int64 read GetMaxAge;
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
    property IsExpired: Boolean read GetIsExpired;
    property HostOnly: Boolean read FHostOnly write FHostOnly;
    property LastAccessed: TDateTime read FLastAccessed write FLastAccessed;
    property Persistent: Boolean read FPersistent write FPersistent;

    // Client representation of a cookie (an entry of the Cookie header)
    property ClientCookie: string read GetClientCookie write SetClientCookie;
    // Server representation of a cookie (the whole Set-Cookie header)
    property ServerCookie: string read GetServerCookie;
  end;

  TWiRLCookies = class(TObjectList<TWiRLCookie>)
  private
    function GetCookieByName(const AName: string): TWiRLCookie;
    function GetValueByName(const AName: string): string;
  public
    function AddClientCookie(const ACookie: string): TWiRLCookie;
    function Add(const ACookieName, ACookieValue: string): TWiRLCookie; overload;

    property Cookie[const AName: string]: TWiRLCookie read GetCookieByName;
    property Values[const AName: string]: string read GetValueByName; default;
  end;


implementation

uses
  IdGlobal;

{ TWiRLCookies }

function TWiRLCookies.Add(const ACookieName, ACookieValue: string): TWiRLCookie;
begin
  Result := TWiRLCookie.Create;
  try
    Result.CookieName := ACookieName;
    Result.Value := ACookieValue;
    Add(Result);
  except
    Result.Free;
  end;
end;

function TWiRLCookies.AddClientCookie(const ACookie: string): TWiRLCookie;
begin
  Result := TWiRLCookie.Create;
  try
    Result.ClientCookie := ACookie;
    Add(Result);
  except
    Result.Free;
  end;
end;

function TWiRLCookies.GetCookieByName(const AName: string): TWiRLCookie;
var
  LCookie: TWiRLCookie;
begin
  Result := nil;
  for LCookie in Self do
  begin
    // Cookie are case sensitive
    if LCookie.CookieName = AName then
      Exit(LCookie);
  end;  
end;

function TWiRLCookies.GetValueByName(const AName: string): string;
var
  LCookie: TWiRLCookie;
begin
  Result := '';
  LCookie := Cookie[AName];
  if Assigned(LCookie) then
    Result := LCookie.Value;
end;

{ TWiRLCookie }

function TWiRLCookie.GetClientCookie: string;
begin
  Result := FName + '=' + FValue;
end;

function TWiRLCookie.GetIsExpired: Boolean;
begin
  Result := (FExpires <> 0.0) and (FExpires < Now);
end;

function TWiRLCookie.GetMaxAge: Int64;
begin
  if FExpires <> 0.0 then 
  begin
    Result := Trunc( (FExpires - Now) * MSecsPerDay / 1000 );
  end 
  else 
  begin
    Result := -1;
  end;
end;

function TWiRLCookie.GetServerCookie: string;

  procedure AddCookieProperty(var ACookie: string; const AProperty, AValue: string);
  begin
    if Length(AValue) > 0 then
    begin
      if Length(ACookie) > 0 then 
      begin
        ACookie := ACookie + '; ';
      end;
                                       
      ACookie := ACookie + AProperty + '=' + AValue;
    end;
  end;

  procedure AddCookieFlag(var ACookie: string; const AFlag: String);
  begin
    if Length(ACookie) > 0 then 
    begin
      ACookie := ACookie + '; ';
    end;
    ACookie := ACookie + AFlag;
  end;

var
  LExpires: TDateTime;
  LMaxAge: Int64;
begin
  Result := FName + '=' + FValue;

  AddCookieProperty(Result, 'Path', FPath);
  AddCookieProperty(Result, 'Domain', FDomain);
  if FSecure then begin
    AddCookieFlag(Result, 'Secure');
  end;
  if FHttpOnly then begin
    AddCookieFlag(Result, 'HttpOnly');
  end;
  LMaxAge := MaxAge;
  if LMaxAge >= 0 then begin
    AddCookieProperty(Result, 'Max-Age', IntToStr(LMaxAge));
  end;
  LExpires := Expires;
  if LExpires <> 0.0 then begin
    AddCookieProperty(Result, 'Expires', LocalDateTimeToCookieStr(LExpires));
  end;
end;

procedure TWiRLCookie.SetClientCookie(const Value: string);
var
  LPair: TArray<string>;
begin
  LPair := Value.Split(['=']);

  if Length(LPair) > 0 then  
    FName := LPair[0];
  if Length(LPair) > 1 then  
    FValue := LPair[1];
end;

end.
