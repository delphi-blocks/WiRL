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
  System.SysUtils, System.Rtti, System.Generics.Collections;

type
  TWiRLGarbageCollector = class
  private
    FGarbage: TArray<TValue>;
  public
    constructor Create;
    procedure AddGarbage(const AValue: TValue);
    procedure CollectSingleGarbage(const AValue: TValue);

    procedure CollectGarbage();
  end;

implementation

uses
  WiRL.Core.Attributes,
  WiRL.Rtti.Utils;

{ TWiRLGarbageCollector }

constructor TWiRLGarbageCollector.Create;
begin
  FGarbage := [];
end;

procedure TWiRLGarbageCollector.AddGarbage(const AValue: TValue);
begin
  FGarbage := FGarbage + [AValue];
end;

procedure TWiRLGarbageCollector.CollectGarbage;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FGarbage) do
    CollectSingleGarbage(FGarbage[LIndex]);
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

    //tkInterface: TObject(AValue.AsInterface).Free;

    tkArray,
    tkDynArray:
    begin
      for LIndex := 0 to AValue.GetArrayLength - 1 do
        CollectSingleGarbage(AValue.GetArrayElement(LIndex));
    end;
  end;
end;

end.
