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
    procedure Add(AObject: TObject);
    procedure AddValue(const AValue: TValue);
    procedure Clear;
    procedure SetObject(AObject: TObject);
    function Count: Integer;
    function GetEnumerator: IEnumerator;
    function GetItem(AIndex: Integer): TObject;
    function GetItemValue(AIndex: Integer): TValue;
    function GetItemTypeInfo: PTypeInfo;
    // OwnsObjects property
    procedure SetOwnsObjects(AOwnsObjects: Boolean);
    function GetOwnsObjects: Boolean;
  end;

  TDynamicStreamable = class(TInterfacedObject, IDynamicStreamable)
  private
    FInstance: TObject;
    FLoadMethod: TRttiMethod;
    FSaveMethod: TRttiMethod;
  public
    constructor Create(AInstance: TObject; ALoadMethod, ASaveMethod: TRttiMethod);
  public
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
  end;


implementation

{ TDynamicStreamable }

constructor TDynamicStreamable.Create(AInstance: TObject; ALoadMethod,
  ASaveMethod: TRttiMethod);
begin

end;

procedure TDynamicStreamable.LoadFromStream(AStream: TStream);
begin

end;

procedure TDynamicStreamable.SaveToStream(AStream: TStream);
begin

end;

end.
