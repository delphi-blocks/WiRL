unit Server.Converter;

interface

uses
  System.SysUtils, System.Classes, System.TypInfo, System.Rtti,
  WiRL.Core.Converter, WiRL.Core.Declarations;

type
  TCustomEnumConverter = class(TWiRLConverter)
  public
    function ValueFromString(const AValue: string): TValue; override;
    function ValueToString(const AValue: TValue): string; override;
  end;

implementation

{ TCustomEnumConverter }

uses
  Server.Entities;

function TCustomEnumConverter.ValueFromString(const AValue: string): TValue;
var
  LCustomEnum: TCustomEnum;
begin
  if SameText(AValue, 'destra') then
    LCustomEnum := TCustomEnum.Right
  else if SameText(AValue, 'sinistra') then
    LCustomEnum := TCustomEnum.Left
  else
    LCustomEnum := TCustomEnum.None;
    
  Result := TValue.From(LCustomEnum);
end;

function TCustomEnumConverter.ValueToString(const AValue: TValue): string;
var
  LCustomEnum: TCustomEnum;
begin
  LCustomEnum := TCustomEnum(AValue.AsOrdinal);
  case LCustomEnum of
    TCustomEnum.Left: Result := 'sinistra';
    TCustomEnum.Right: Result := 'destra';
    else
      Result := '';
  end;
end;

procedure RegisterCustomConverters;
begin
  TWiRLConverterRegistry.Instance.RegisterConverter(TCustomEnumConverter,
    function (ARttiType: TRttiType; const AAttributes: TAttributeArray; var AAffinity: Integer; const AFormat: TWiRLFormatSetting): Boolean
    begin
      Result := False;
      if (ARttiType.TypeKind = tkEnumeration) and
         (ARttiType.Handle = TypeInfo(TCustomEnum)) then
      begin
        AAffinity := TWiRLConverterRegistry.AFFINITY_HIGH;
        Exit(True);
      end;
    end
  );
end;

initialization
  RegisterCustomConverters;

end.
