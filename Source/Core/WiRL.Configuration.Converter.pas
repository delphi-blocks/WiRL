{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Configuration.Converter;

interface

uses
  System.Classes, System.SysUtils, System.TypInfo, System.Generics.Collections,
  System.Generics.Defaults, System.JSON,

  WiRL.Configuration.Core;

type
  TWiRLFomatSettingDictionary = class(TDictionary<Pointer,string>)
  public
    procedure AddFormat(ATypeInfo: PTypeInfo; const AFormat: string);
  end;

  IWiRLFormatSetting = interface(IWiRLConfiguration)
  ['{A162F42E-2AD2-45B0-AFE0-6D6C07458F13}']
    function GetFormatSettingFor(ATypeInfo: PTypeInfo): string;
    function AddFormat(ATypeInfo: PTypeInfo; const AFormat: string): IWiRLFormatSetting;
  end;

  [Implements(IWiRLFormatSetting)]
  TWiRLFormatSettingConfig = class(TWiRLConfiguration, IWiRLFormatSetting)
  private
    FFomatSettingDictionary: TWiRLFomatSettingDictionary;
  public
    function GetFormatSettingFor(ATypeInfo: PTypeInfo): string;
    function AddFormat(ATypeInfo: PTypeInfo; const AFormat: string): IWiRLFormatSetting;

    constructor Create; override;
    destructor Destroy; override;
//  public
//    function Add<T>(const AFormat: string): TWiRLFormatSettingConfig;
  end;

implementation

uses
  WiRL.Core.Converter;

{ TWiRLFormatSettingConfig }

//function TWiRLFormatSettingConfig.Add<T>(
//  const AFormat: string): TWiRLFormatSettingConfig;
//begin
//
//end;

function TWiRLFormatSettingConfig.GetFormatSettingFor(ATypeInfo: PTypeInfo): string;
begin
  if not FFomatSettingDictionary.TryGetValue(ATypeInfo, Result) then
    Result := TWiRLFormatSetting.DEFAULT;
end;

function TWiRLFormatSettingConfig.AddFormat(ATypeInfo: PTypeInfo;
  const AFormat: string): IWiRLFormatSetting;
begin
  FFomatSettingDictionary.AddFormat(ATypeInfo, AFormat);
  Result := Self;
end;


//function TWiRLFormatSettingConfig.Add<T>(
//  const AFormat: string): TWiRLFormatSettingConfig;
//begin
//
//end;

constructor TWiRLFormatSettingConfig.Create;
begin
  inherited;
  FFomatSettingDictionary := TWiRLFomatSettingDictionary.Create;
end;

destructor TWiRLFormatSettingConfig.Destroy;
begin
  FFomatSettingDictionary.Free;
  inherited;
end;

{ TWiRLFomatSettingDictionary }

procedure TWiRLFomatSettingDictionary.AddFormat(ATypeInfo: PTypeInfo;
  const AFormat: string);
begin
  Add(ATypeInfo, AFormat);
end;

initialization

  TWiRLConfigClassRegistry.Instance.RegisterConfigClass(TWiRLFormatSettingConfig);

end.
