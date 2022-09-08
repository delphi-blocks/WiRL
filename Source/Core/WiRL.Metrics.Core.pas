{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2022 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Metrics.Core;

interface

uses
  System.Classes, System.SysUtils, System.Rtti, Generics.Collections,

  WiRL.Core.Attributes,
  WiRL.Core.Injection,
  WiRL.Core.Context,
  WiRL.Configuration.Core;

type
  {$SCOPEDENUMS ON}
  TWiRLStandardMetric = (NumRequest, RequestSize);
  TWiRLStandardMetrics = set of TWiRLStandardMetric;

  TWiRLMetricLabel = record
  public
    Name: string;
    Value: string;
    function ToString: string;
    class operator Implicit(const AValue: string): TWiRLMetricLabel;
  end;

  TWiRLMetricSample = class(TObject)
    Name: string;
    Labels: TArray<TWiRLMetricLabel>;
    Value: Double;
  end;

  [Singleton]
  TWiRLMetrics = class abstract
  public
    class function GenerateKey(const AName: string; ALabels: TArray<TWiRLMetricLabel>): string; static;
  public
    procedure AcquireMetric(const AName: string; ALabels: TArray<TWiRLMetricLabel>; AValue: Double); overload; virtual; abstract;
    procedure AcquireMetric(const AName: string; AValue: Double); overload; virtual;
    procedure IncMetric(const AName: string; ALabels: TArray<TWiRLMetricLabel>); overload; virtual; abstract;
    procedure IncMetric(const AName: string); overload; virtual;
  end;

  [Singleton]
  TInMemoryMetrics = class(TWiRLMetrics)
  private
    FItems: TObjectDictionary<string, TWiRLMetricSample>;
  public
    function GetEnumerator: TEnumerator<TWiRLMetricSample>; inline;

    procedure AcquireMetric(const AName: string; ALabels: TArray<TWiRLMetricLabel>; AValue: Double); overload; override;
    procedure IncMetric(const AName: string; ALabels: TArray<TWiRLMetricLabel>); overload; override;
    constructor Create;
    destructor Destroy; override;
  end;

  IWiRLMetricsSetting = interface(IWiRLConfiguration)
    ['{899169C3-A970-4E09-870B-2D63428B0C8A}']
    function InMemoryStorage: IWiRLMetricsSetting;
    function EnableMetric(AStandardMetrics: TWiRLStandardMetrics): IWiRLMetricsSetting;
    function AddCustomMetric(const AName, ADescription: string): IWiRLMetricsSetting;
  end;

  [Implements(IWiRLMetricsSetting)]
  TWiRLMetricsSetting = class(TWiRLConfiguration, IWiRLMetricsSetting)
  public
    { IWiRLMetricsSetting }
    function InMemoryStorage: IWiRLMetricsSetting;
    function EnableMetric(AStandardMetrics: TWiRLStandardMetrics): IWiRLMetricsSetting;
    function AddCustomMetric(const AName, ADescription: string): IWiRLMetricsSetting;
  end;

  TMetricsFactory = class(TInterfacedObject, IContextHttpFactory)
  public
    function CreateContextObject(const AObject: TRttiObject;
      AContext: TWiRLContextHttp): TValue;
  end;

const
  MetricAll = [TWiRLStandardMetric.NumRequest, TWiRLStandardMetric.RequestSize];

implementation

var
  WiRLMetrics: TWiRLMetrics;

{ TWiRLMetricsSetting }

function TWiRLMetricsSetting.AddCustomMetric(const AName,
  ADescription: string): IWiRLMetricsSetting;
begin
  Result := Self;
end;

function TWiRLMetricsSetting.EnableMetric(
  AStandardMetrics: TWiRLStandardMetrics): IWiRLMetricsSetting;
begin
  Result := Self;
end;

function TWiRLMetricsSetting.InMemoryStorage: IWiRLMetricsSetting;
begin
  // TODO: only the first time!
  WiRLMetrics := TInMemoryMetrics.Create;
  Result := Self;
end;

{ TWiRLMetrics }

procedure TWiRLMetrics.AcquireMetric(const AName: string; AValue: Double);
begin
  AcquireMetric(AName, [], AValue);
end;

class function TWiRLMetrics.GenerateKey(const AName: string;
  ALabels: TArray<TWiRLMetricLabel>): string;
var
  LLabel: TWiRLMetricLabel;
begin
  Result := AName + '||';
  for LLabel in ALabels do
    Result := Result + LLabel.ToString + '||';
end;

procedure TWiRLMetrics.IncMetric(const AName: string);
begin
  IncMetric(AName, []);
end;

{ TInMemoryMetrics }

procedure TInMemoryMetrics.AcquireMetric(const AName: string;
  ALabels: TArray<TWiRLMetricLabel>; AValue: Double);
var
  LKey: string;
  LSample: TWiRLMetricSample;
begin
  inherited;
  LKey := TWiRLMetrics.GenerateKey(AName, ALabels);
  if FItems.TryGetValue(LKey, LSample) then
  begin
    LSample.Value := AValue;
  end
  else
  begin
    LSample := TWiRLMetricSample.Create;
    LSample.Name := AName;
    LSample.Labels := ALabels;
    LSample.Value := AValue;
    FItems.Add(LKey, LSample);
  end;

end;

constructor TInMemoryMetrics.Create;
begin
  FItems := TObjectDictionary<string, TWiRLMetricSample>.Create([doOwnsValues]);
end;

destructor TInMemoryMetrics.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TInMemoryMetrics.GetEnumerator: TEnumerator<TWiRLMetricSample>;
begin
  Result := FItems.Values.GetEnumerator;
end;

procedure TInMemoryMetrics.IncMetric(const AName: string;
  ALabels: TArray<TWiRLMetricLabel>);
var
  LKey: string;
  LSample: TWiRLMetricSample;
begin
  inherited;
  LKey := TWiRLMetrics.GenerateKey(AName, ALabels);
  if FItems.TryGetValue(LKey, LSample) then
  begin
    LSample.Value := LSample.Value + 1;
  end
  else
  begin
    LSample := TWiRLMetricSample.Create;
    LSample.Name := AName;
    LSample.Labels := ALabels;
    LSample.Value := 1;
    FItems.Add(LKey, LSample);
  end;

end;

{ TMetricsFactory }

function TMetricsFactory.CreateContextObject(const AObject: TRttiObject;
  AContext: TWiRLContextHttp): TValue;
begin
  Result := WiRLMetrics;
end;

procedure InitMetrics;
begin
  WiRLMetrics := nil;
end;

procedure  FreeMetrics;
begin
  if Assigned(WiRLMetrics) then
    WiRLMetrics.Free;
end;

{ TWiRLMetricLabel }

class operator TWiRLMetricLabel.Implicit(const AValue: string): TWiRLMetricLabel;
var
  LPair: TArray<string>;
begin
  LPair := AValue.Split([':', '=']);
  if Length(LPair) > 1 then
  begin
    Result.Name := LPair[0];
    Result.Value := LPair[1];
  end
  else if Length(LPair) > 0 then
  begin
    Result.Name := LPair[0];
    Result.Value := '';
  end
  else
  begin
    Result.Name := '';
    Result.Value := '';
  end
end;

function TWiRLMetricLabel.ToString: string;
begin
  Result := Name + ':' + Value;
end;

initialization

  TWiRLContextInjectionRegistry.Instance.RegisterFactory<TWiRLMetrics>(TMetricsFactory);
  TWiRLConfigClassRegistry.Instance.RegisterConfigClass(TWiRLMetricsSetting);

  InitMetrics;

finalization

  FreeMetrics;

end.
