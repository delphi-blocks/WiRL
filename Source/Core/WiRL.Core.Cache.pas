{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Cache;

interface

uses
  System.Classes, System.SysUtils, Generics.Collections, System.Rtti, 
  System.SyncObjs;

type
  TWiRLCacheItem = class
  private
    FCriticalSection: TCriticalSection;
    FLastReadAccess: TDateTime;
    FLastWriteAccess: TDateTime;
    FDuration: TDateTime;
    FValue: TValue;
    function GetExpiration: TDateTime;
    function GetIsExpired: Boolean;
    function GetValue: TValue;
    procedure SetValue(const Value: TValue);
  protected
  public
    constructor Create;
    destructor Destroy; override;

    property LastReadAccess: TDateTime read FLastReadAccess;
    property LastWriteAccess: TDateTime read FLastWriteAccess;
    property Duration: TDateTime read FDuration write FDuration;
    property Expiration: TDateTime read GetExpiration;
    property IsExpired: Boolean read GetIsExpired;
    property Value: TValue read GetValue write SetValue;
    property CriticalSection: TCriticalSection read FCriticalSection;
  end;

  TWiRLCache = class
  private
    FStorage: TDictionary<string, TWiRLCacheItem>;
    FCriticalSection: TCriticalSection;
  protected
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure SetValue(const AName: string; const AValue: TValue);
    function Contains(const AName: string): Boolean;
    function GetValue(const AName: string): TValue;

    function Use(const AName: string; const ADoSomething: TProc<TValue>): Boolean;
  end;

  function CacheManager: TWiRLCache;

implementation

uses
  System.DateUtils, System.Math;

var
  _Cache: TWiRLCache;

procedure AcquireAndDo(ACRiticalSection: TCriticalSection; const ADoSomething: TProc);
begin
  ACRiticalSection.Enter;
  try
    ADoSomething();
  finally
    ACRiticalSection.Leave;
  end;
end;


function CacheManager: TWiRLCache;
begin
  if not Assigned(_Cache) then
    _Cache := TWiRLCache.Create;
  Result := _Cache;
end;

{ TCache }

procedure TWiRLCache.SetValue(const AName: string; const AValue: TValue);
var
  LValue: TValue;
begin
  LValue := AValue;

  AcquireAndDo(FCriticalSection,
    procedure
    var
      LItem: TWiRLCacheItem;
    begin
      if FStorage.TryGetValue(AName, LItem) then
      begin
        AcquireAndDo(LItem.CriticalSection,
          procedure
          begin
            LItem.Value := LValue;
          end
        );
      end
      else
      begin
        LItem := TWiRLCacheItem.Create;
        try
          LItem.Value := LValue;
          FStorage.Add(AName, LItem);
        except
          LItem.Free;
          raise;
        end;
      end;
    end);
end;

function TWiRLCache.Use(const AName: string;
  const ADoSomething: TProc<TValue>): Boolean;
var
  LItem: TWiRLCacheItem;
  LFound: Boolean;
begin
  Result := False;

  FCriticalSection.Enter;
  try
    LFound := FStorage.TryGetValue(AName, LItem);
  finally
    FCriticalSection.Leave;
  end;

  if LFound then
  begin
    Result := True;

    AcquireAndDo(LItem.CriticalSection,
      procedure
      begin
        ADoSomething(LItem.Value);
      end);
  end;
end;

function TWiRLCache.Contains(const AName: string): Boolean;
var
  LResult: Boolean;
begin
  AcquireAndDo(FCriticalSection,
    procedure
    begin
      LResult := FStorage.ContainsKey(AName);
    end);
  Result := LResult;
end;

constructor TWiRLCache.Create;
begin
  inherited Create;
  FStorage := TDictionary<string, TWiRLCacheItem>.Create;
  FCriticalSection := TCriticalSection.Create;
end;

destructor TWiRLCache.Destroy;
begin
  FCriticalSection.Free;
  FStorage.Free;
  inherited;
end;

function TWiRLCache.GetValue(const AName: string): TValue;
var
  LItem: TWiRLCacheItem;
  LResult: TValue;
begin
  Result := TValue.Empty;

  AcquireAndDo(FCriticalSection,
    procedure
    begin
      if FStorage.TryGetValue(AName, LItem) then
      begin
        AcquireAndDo(LItem.CriticalSection,
          procedure
          begin
            LResult := LItem.Value;
          end
        );
      end;
    end);
  Result := LResult;
end;

{ TCacheItem }

constructor TWiRLCacheItem.Create;
begin
  inherited Create;
  FCriticalSection := TCriticalSection.Create;
  FLastReadAccess := Now;
  FLastWriteAccess := Now;
  FDuration := 1 / HoursPerDay;
  FValue := TValue.Empty;
end;

destructor TWiRLCacheItem.Destroy;
begin
  FCriticalSection.Free;
  inherited;
end;

function TWiRLCacheItem.GetExpiration: TDateTime;
begin
  Result := Max(LastReadAccess, LastWriteAccess) + Duration;
end;

function TWiRLCacheItem.GetIsExpired: Boolean;
begin
  Result := Now > Expiration;
end;

function TWiRLCacheItem.GetValue: TValue;
begin
  Result := FValue;
  FLastReadAccess := Now;
end;

procedure TWiRLCacheItem.SetValue(const Value: TValue);
begin
  FValue := Value;
  FLastWriteAccess := Now;
end;

end.
