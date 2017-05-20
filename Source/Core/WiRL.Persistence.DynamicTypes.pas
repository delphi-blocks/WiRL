unit WiRL.Persistence.DynamicTypes;

interface

uses
  System.Classes, System.SysUtils, Data.DB, System.Rtti, System.TypInfo,
  System.Generics.Collections;

type
  IDynamicType = interface
  ['{DD163E75-134C-4035-809C-D9E1EEEC4225}']
  end;

  IDynamicStreamable = interface(IDynamicType)
  ['{968D03E7-273F-4E94-A3EA-ECB7A73F0715}']
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
  end;

  IDynamicList = interface(IDynamicType)
  ['{9F4A2D72-078B-4EA2-B86E-068206AD0F16}']
    function Add: TObject;
    procedure AddObject(AObject: TObject);
    procedure AddValue(const AValue: TValue);
    procedure Clear;
    function Count: Integer;
    function GetEnumerator: IEnumerator;
    function GetObject(AIndex: Integer): TObject;
    function GetValue(AIndex: Integer): TValue;
  end;

  TDynamicStreamable = class(TInterfacedObject, IDynamicStreamable)
  private
    FInstance: TObject;
    FLoadMethod: TRttiMethod;
    FSaveMethod: TRttiMethod;
  public
    constructor Create(AInstance: TObject; ALoadMethod, ASaveMethod: TRttiMethod);
  public
    class function GuessType(AInstance: TObject): IDynamicStreamable;
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
  end;

  TDynamicList = class(TInterfacedObject, IDynamicList)
  public
    function Add: TObject;
    procedure AddObject(AObject: TObject);
    procedure AddValue(const AValue: TValue);
    procedure Clear;
    function Count: Integer;
    function GetEnumerator: IEnumerator;
    function GetObject(AIndex: Integer): TObject;
    function GetValue(AIndex: Integer): TValue;
  end;

implementation

uses
  WiRL.Rtti.Utils;

{ TDynamicStreamable }

constructor TDynamicStreamable.Create(AInstance: TObject; ALoadMethod,
  ASaveMethod: TRttiMethod);
begin
  FInstance := AInstance;
  FLoadMethod := ALoadMethod;
  FSaveMethod := ASaveMethod;
end;

class function TDynamicStreamable.GuessType(AInstance: TObject): IDynamicStreamable;
var
  LType: TRttiType;
  LLoadMethod, LSaveMethod: TRttiMethod;
begin
  LType := TRttiHelper.Context.GetType(AInstance.ClassType);

  if not Assigned(LType) then
    Exit(nil);

  LLoadMethod := LType.GetMethod('LoadFromStream');
  if not Assigned(LLoadMethod) then
    Exit(nil);

  LSaveMethod := LType.GetMethod('SaveToStream');
  if not Assigned(LSaveMethod) then
    Exit(nil);

  Result := Self.Create(AInstance, LLoadMethod, LSaveMethod);
end;

procedure TDynamicStreamable.LoadFromStream(AStream: TStream);
begin
  FLoadMethod.Invoke(FInstance, [AStream]);
end;

procedure TDynamicStreamable.SaveToStream(AStream: TStream);
begin
  FSaveMethod.Invoke(FInstance, [AStream]);
end;

{ TDynamicList }

function TDynamicList.Add: TObject;
begin

end;

procedure TDynamicList.AddObject(AObject: TObject);
begin

end;

procedure TDynamicList.AddValue(const AValue: TValue);
begin

end;

procedure TDynamicList.Clear;
begin

end;

function TDynamicList.Count: Integer;
begin

end;

function TDynamicList.GetEnumerator: IEnumerator;
begin

end;

function TDynamicList.GetObject(AIndex: Integer): TObject;
begin

end;

function TDynamicList.GetValue(AIndex: Integer): TValue;
begin

end;

end.
