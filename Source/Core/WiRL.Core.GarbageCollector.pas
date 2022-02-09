{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.GarbageCollector;

interface

uses
  System.SysUtils, System.Rtti, System.Types, System.Classes,
  System.Generics.Defaults, System.Generics.Collections;

type
  TDisposeAction = reference to procedure;

  TWiRLGarbageCollector = class(TComponent)
  private
    FGarbage: TDictionary<TValue, TDisposeAction>;
    procedure CollectGarbageValue(const AValue: TValue);
    procedure CollectSingleGarbage(AGarbage: TPair<TValue, TDisposeAction>);
  public
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;

    procedure AddGarbage(const AValue: TValue); overload;
    procedure AddGarbage(const AValue: TValue; AAction: TDisposeAction); overload;
    procedure CollectGarbage();
  end;

implementation

uses
  WiRL.Core.Attributes,
  WiRL.Rtti.Utils;

procedure TWiRLGarbageCollector.AddGarbage(const AValue: TValue);
begin
  AddGarbage(AValue, nil);
end;

procedure TWiRLGarbageCollector.AddGarbage(const AValue: TValue; AAction: TDisposeAction);
begin
  if FGarbage.ContainsKey(AValue) then
    Exit;
  FGarbage.Add(AValue, AAction);
end;

procedure TWiRLGarbageCollector.CollectGarbage;
var
  LGarbage: TPair<TValue, TDisposeAction>;
begin
  for LGarbage in FGarbage do
    CollectSingleGarbage(LGarbage);
  FGarbage.Clear;
end;

procedure TWiRLGarbageCollector.CollectSingleGarbage(AGarbage: TPair<TValue, TDisposeAction>);
begin
  if Assigned(AGarbage.Value) then
    AGarbage.Value()
  else
    CollectGarbageValue(AGarbage.Key);
end;

procedure TWiRLGarbageCollector.CollectGarbageValue(const AValue: TValue);
var
  LIndex: Integer;
begin
  case AValue.Kind of
    tkClass:
    begin
      if (AValue.AsObject <> nil) then
        if not TRttiHelper.HasAttribute<SingletonAttribute>(AValue.AsObject.ClassType) then
          AValue.AsObject.Free;
    end;

    tkArray,
    tkDynArray:
    begin
      for LIndex := 0 to AValue.GetArrayLength - 1 do
        CollectGarbageValue(AValue.GetArrayElement(LIndex));
    end;
  end;
end;

constructor TWiRLGarbageCollector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGarbage := TDictionary<TValue, TDisposeAction>.Create;
end;

destructor TWiRLGarbageCollector.Destroy;
begin
  CollectGarbage;
  FGarbage.Free;
  inherited;
end;

end.
