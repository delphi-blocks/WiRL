unit Server.Config;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Defaults,

  WiRL.Core.Application,
  WiRL.Configuration.Core;

type
  ITestConfig = interface(IWiRLConfiguration)
    ['{83B5A6E2-6B7A-40B2-B072-63F2D3E14D0E}']
    function SetStringValue(const AValue: string): ITestConfig;
    function SetIntValue(const AValue: Integer): ITestConfig;
  end;

  [Implements(ITestConfig)]
  TTestConfig = class(TWiRLConfiguration, ITestConfig)
  private
    FStringValue: string;
    FIntValue: Integer;
  public
    function SetStringValue(const AValue: string): ITestConfig;
    function SetIntValue(const AValue: Integer): ITestConfig;

    property StringValue: string read FStringValue;
    property IntValue: Integer read FIntValue;
  end;

implementation

{ TTestConfig }

function TTestConfig.SetIntValue(const AValue: Integer): ITestConfig;
begin
  FIntValue := AValue;
  Result := Self;
end;

function TTestConfig.SetStringValue(const AValue: string): ITestConfig;
begin
  FStringValue := AValue;
  Result := Self;
end;

initialization

  TWiRLConfigClassRegistry.Instance.RegisterConfigClass(TTestConfig);

end.
