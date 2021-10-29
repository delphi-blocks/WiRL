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
  System.SysUtils, System.Rtti, System.Classes,
  System.Generics.Defaults, System.Generics.Collections;

type
  TDisposeAction = reference to procedure;

  TGarbageInfo = record
    Value: TValue;
    Action: TDisposeAction;
  end;

  TWiRLGarbageCollector = class(TComponent)
  private
    FGarbage: TArray<TGarbageInfo>;
    procedure CollectSingleGarbage(const AValue: TValue); overload;
    procedure CollectSingleGarbage(const AGarbage: TGarbageInfo); overload;
  public
    constructor Create(AOwner: TComponent); overload; override;

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
var
  LItem: TGarbageInfo;
begin
  LItem.Value := AValue;
  LItem.Action := AAction;
  FGarbage := FGarbage + [LItem];
end;

procedure TWiRLGarbageCollector.CollectGarbage;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FGarbage) do
    CollectSingleGarbage(FGarbage[LIndex]);
  FGarbage := [];
end;

procedure TWiRLGarbageCollector.CollectSingleGarbage(const AGarbage: TGarbageInfo);
begin
  if Assigned(AGarbage.Action) then
    AGarbage.Action()
  else
    CollectSingleGarbage(AGarbage.Value);
end;

procedure TWiRLGarbageCollector.CollectSingleGarbage(const AValue: TValue);
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
        CollectSingleGarbage(AValue.GetArrayElement(LIndex));
    end;
  end;
end;

constructor TWiRLGarbageCollector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGarbage := [];
end;

end.
