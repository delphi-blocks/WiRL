{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Stateful.Dictionary;

interface

uses
  System.Classes, System.SysUtils, System.SyncObjs,
  System.Generics.Collections,
  WiRL.Core.Classes,
  WiRL.Core.Attributes,
  WiRL.Core.Auth.Context,
  WiRL.Core.Singleton,
  WiRL.Core.Utils;

type
  TWiRLStatefulDictionary = class
  private
    FDictionary: TDictionary<string, TPair<TCriticalSection, TObject>>;
    FDictionaryCS: TCriticalSection;
  protected
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Use<T: class>(const AKey: string; const ADoSomething: TProc<T>);
    procedure Add(const AKey: string; AObject: TObject);
    function Contains(const AKey: string): Boolean;

  end;

  TWiRLStatefulDictionaryRegistry = class(TNonInterfacedObject)
  private
    type TWiRLStatefulDictionaryRegistrySingleton = TWiRLSingleton<TWiRLStatefulDictionaryRegistry>;
  private
    FCriticalSection: TCriticalSection;
    FDictionary: TObjectDictionary<string, TWiRLStatefulDictionary>;
  protected
    class function GetInstance: TWiRLStatefulDictionaryRegistry; static; inline;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    // Returns the TWiRLStatefulDictionary associated to AToken. Create if not exists yet.
    function GetDictionaryForToken(AToken: TWiRLAuthContext): TWiRLStatefulDictionary; virtual;

    procedure OnTokenStart(const AToken: string);
    procedure OnTokenEnd(const AToken: string);

    class property Instance: TWiRLStatefulDictionaryRegistry read GetInstance;
  end;


implementation

{ TWiRLStatefulDictionary }

procedure TWiRLStatefulDictionary.Add(const AKey: string; AObject: TObject);
var
  LValue: TPair<TCriticalSection, TObject>;
begin
  FDictionaryCS.Enter;
  try
    if FDictionary.ContainsKey(AKey) then // già presente
    begin
      if FDictionary.TryGetValue(AKey, LValue) then
      begin
        LValue.Key.Enter;
        try
          LValue.Value := AObject; // memory management???
        finally
          LValue.Key.Leave;
        end;
      end;
    end
    else
    begin // aggiunge al dizionario
      FDictionary.Add(AKey, TPair<TCriticalSection, TObject>.Create(TCriticalSection.Create, AObject));
    end;
  finally
    FDictionaryCS.Leave
  end;
end;

function TWiRLStatefulDictionary.Contains(const AKey: string): Boolean;
begin
  FDictionaryCS.Enter;
  try
    Result := FDictionary.ContainsKey(AKey);
  finally
    FDictionaryCS.Leave;
  end;
end;

constructor TWiRLStatefulDictionary.Create;
begin
  FDictionaryCS := TCriticalSection.Create;
  FDictionary := TDictionary<string, TPair<TCriticalSection, TObject>>.Create;

  inherited Create;
end;

destructor TWiRLStatefulDictionary.Destroy;
var
  LKey: string;
  LValue: TPair<string, TPair<TCriticalSection, TObject>>;
  LContainedValue: TPair<TCriticalSection, TObject>;
  LObj: TObject;
begin
  // empty dictionary!!!
  FDictionaryCS.Enter;
  try
    while FDictionary.Count > 0 do
    begin
      LKey := FDictionary.Keys.ToArray[0];
      LValue := FDictionary.ExtractPair(LKey);
      LContainedValue := LValue.Value;

      LContainedValue.Key.Enter;
      try
        LObj := LContainedValue.Value;
        LContainedValue.Value := nil;

        TThread.Synchronize(nil,
          procedure begin
            LObj.Free;
          end
        );

      finally
        LContainedValue.Key.Leave;
      end;

      LContainedValue.Key.Free;
      LContainedValue.Key := nil;
    end; // loop dictionary
  finally
    FDictionaryCS.Leave;
  end;
  FDictionary.Free;
  FDictionaryCS.Free;

  inherited;
end;

procedure TWiRLStatefulDictionary.Use<T>(const AKey: string; const ADoSomething: TProc<T>);
var
  LPair: TPair<TCriticalSection, TObject>;
  LFound: Boolean;
begin
  LFound := False;
  try
    FDictionaryCS.Enter;
    try
      LFound := FDictionary.TryGetValue(AKey, LPair);
      if LFound then
        LPair.Key.Enter;
    finally
      FDictionaryCS.Leave;
    end;
  except
    if LFound then
      LPair.Key.Leave;
    raise;
  end;

  if LFound then
    try
      ADoSomething(LPair.Value as T);
    finally
      LPair.Key.Leave;
    end;
end;


{ TWiRLStatefulDictionaryRegistry }

constructor TWiRLStatefulDictionaryRegistry.Create;
begin
  TWiRLStatefulDictionaryRegistrySingleton.CheckInstance(Self);
  inherited Create;
  FCriticalSection := TCriticalSection.Create;
  FDictionary := TObjectDictionary<string, TWiRLStatefulDictionary>.Create([doOwnsValues]);
end;

destructor TWiRLStatefulDictionaryRegistry.Destroy;
begin
  FCriticalSection.Free;
  FDictionary.Free;
  inherited;
end;

function TWiRLStatefulDictionaryRegistry.GetDictionaryForToken(AToken: TWiRLAuthContext): TWiRLStatefulDictionary;
begin
  Result := nil;
  FCriticalSection.Enter;
  try
    if not FDictionary.TryGetValue(AToken.CompactToken, Result) then
    begin
      Result := TWiRLStatefulDictionary.Create;
      FDictionary.Add(AToken.CompactToken, Result);
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

class function TWiRLStatefulDictionaryRegistry.GetInstance: TWiRLStatefulDictionaryRegistry;
begin
  Result := TWiRLStatefulDictionaryRegistrySingleton.Instance;
end;

procedure TWiRLStatefulDictionaryRegistry.OnTokenEnd(const AToken: string);
begin
  FCriticalSection.Enter;
  try
    FDictionary.Remove(AToken);
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TWiRLStatefulDictionaryRegistry.OnTokenStart(const AToken: string);
begin

end;

end.
