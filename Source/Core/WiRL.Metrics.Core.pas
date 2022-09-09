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
  System.SyncObjs,

  WiRL.Rtti.Utils,
  WiRL.Core.Attributes,
  WiRL.Core.Injection,
  WiRL.Core.Context,
  WiRL.Core.Exceptions,
  WiRL.http.Filters,
  WiRL.Configuration.Core;

type
  {$SCOPEDENUMS ON}
  TWiRLStandardMetric = (NumRequest, MemoryUsage, EnabledFilters);
  TWiRLStandardMetrics = set of TWiRLStandardMetric;

  TWiRLMetricsSetting = class;

  ICustomMetricsRetriever = interface;

  // A label (name/value) attached to a metric sample
  TWiRLMetricLabel = record
  public
    Name: string;
    Value: string;
    function ToString: string;
    class operator Implicit(const AValue: string): TWiRLMetricLabel;
  end;

  // The object that save a single metric sample
  TWiRLMetricSample = class(TObject)
    Name: string;
    Labels: TArray<TWiRLMetricLabel>;
    Value: Double;
  end;

  // The abstract object that keep all the metrics
  // Sub-class should handle the metrics in same way:
  // - Save to some place
  // - Send to a metrics logger
  // - ...
  [Singleton]
  TWiRLMetrics = class abstract
  protected
    FContext: TWiRLContextHttp;
    FSetting: TWiRLMetricsSetting;
  public
    class function GenerateKey(const AName: string; ALabels: TArray<TWiRLMetricLabel>): string; static;
  public
    function GetDescription(const AName: string): string;
    procedure AcquireMetric(const AName: string; ALabels: TArray<TWiRLMetricLabel>; AValue: Double); overload; virtual; abstract;
    procedure AcquireMetric(const AName: string; AValue: Double); overload; virtual;
    procedure IncMetric(const AName: string; ALabels: TArray<TWiRLMetricLabel>); overload; virtual; abstract;
    procedure IncMetric(const AName: string); overload; virtual;

    constructor Create(AContext: TWiRLContextHttp); virtual;
  end;

  [Singleton]
  TInMemoryMetrics = class(TWiRLMetrics)
  private
    FItems: TObjectDictionary<string, TWiRLMetricSample>;
    procedure AcquireStandardMetrics;
    procedure AcquireCustomMetrics;
  public
    function GetEnumerator: TEnumerator<TWiRLMetricSample>; inline;

    procedure AcquireMetric(const AName: string; ALabels: TArray<TWiRLMetricLabel>; AValue: Double); overload; override;
    procedure IncMetric(const AName: string; ALabels: TArray<TWiRLMetricLabel>); overload; override;
    constructor Create(AContext: TWiRLContextHttp); override;
    destructor Destroy; override;
  end;

  TWiRLMetricsClass = class of TWiRLMetrics;

  IWiRLMetricsSetting = interface(IWiRLConfiguration)
    ['{899169C3-A970-4E09-870B-2D63428B0C8A}']
    function InMemoryStorage: IWiRLMetricsSetting;
    function EnableMetric(AStandardMetrics: TWiRLStandardMetrics): IWiRLMetricsSetting;
    function AddCustomMetric(const AName, ADescription: string): IWiRLMetricsSetting;
    function AddMetricRetriever(AClass: TClass): IWiRLMetricsSetting;
  end;

  TCustomMetric = record
    Name: string;
    Description: string;
  end;

  TCustomMetrics = class(TList<TCustomMetric>)
  end;

  TMetricsRetrievers = class(TList<TClass>)
  end;

  [Implements(IWiRLMetricsSetting)]
  TWiRLMetricsSetting = class(TWiRLConfiguration, IWiRLMetricsSetting)
  private
    FWiRLMetricsClass: TWiRLMetricsClass;
    FStandardMetrics: TWiRLStandardMetrics;
    FCustomMetrics: TCustomMetrics;
    FMetricsRetrievers: TMetricsRetrievers;
  public
    { IWiRLMetricsSetting }
    function InMemoryStorage: IWiRLMetricsSetting;
    function EnableMetric(AStandardMetrics: TWiRLStandardMetrics): IWiRLMetricsSetting;
    function AddCustomMetric(const AName, ADescription: string): IWiRLMetricsSetting; overload;
    function AddMetricRetriever(AClass: TClass): IWiRLMetricsSetting;

    function GetDescription(const AName: string): string;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TMetricsFactory = class(TInterfacedObject, IContextHttpFactory)
  private
    FLock: TCriticalSection;
  public
    function CreateContextObject(const AObject: TRttiObject;
      AContext: TWiRLContextHttp): TValue;

    constructor Create;
    destructor Destroy; override;
  end;

  TResponseMetricFilter = class(TInterfacedObject, IWiRLContainerResponseFilter)
  private
    [Context] Metrics: TWiRLMetrics;
    [Context] Setting: TWiRLMetricsSetting;
  public
    procedure Filter(ARequestContext: TWiRLContainerResponseContext);
  end;

  ICustomMetricsRetriever = interface
  ['{060C88AF-2B4A-4FE0-9606-E333E145F803}']
    procedure AcquireMetrics(AMetrics: TWiRLMetrics; ASetting: TWiRLMetricsSetting);
  end;

const
  MetricAll = [TWiRLStandardMetric.EnabledFilters, TWiRLStandardMetric.NumRequest, TWiRLStandardMetric.MemoryUsage];

implementation

var
  WiRLMetrics: TWiRLMetrics;

const
  MetricFilterEnabledName = 'wirl_filter_enabled';
  MetricMemoryUsageName = 'wirl_memory_usage';
  MetricNumOfRequestName = 'wirl_num_of_request';


function GetMemoryUsed: UInt64;
var
  st: TMemoryManagerState;
  sb: TSmallBlockTypeState;
begin
  GetMemoryManagerState(st);
  result :=  st.TotalAllocatedMediumBlockSize
           + st.TotalAllocatedLargeBlockSize;
  for sb in st.SmallBlockTypeStates do begin
    result := result + sb.UseableBlockSize * sb.AllocatedBlockCount;
  end;
end;

{ TWiRLMetricsSetting }

function TWiRLMetricsSetting.AddCustomMetric(const AName, ADescription: string): IWiRLMetricsSetting;
var
  LCustomMetric: TCustomMetric;
begin
  LCustomMetric.Name := AName;
  LCustomMetric.Description := ADescription;
  FCustomMetrics.Add(LCustomMetric);
  Result := Self;
end;

function TWiRLMetricsSetting.AddMetricRetriever(AClass: TClass): IWiRLMetricsSetting;
begin
  FMetricsRetrievers.Add(AClass);
  Result := Self;
end;

constructor TWiRLMetricsSetting.Create;
begin
  inherited;
  FCustomMetrics := TCustomMetrics.Create;
  FMetricsRetrievers := TMetricsRetrievers.Create;
end;

destructor TWiRLMetricsSetting.Destroy;
begin
  FCustomMetrics.Free;
  FMetricsRetrievers.Free;
  inherited;
end;

function TWiRLMetricsSetting.EnableMetric(
  AStandardMetrics: TWiRLStandardMetrics): IWiRLMetricsSetting;
begin
  FStandardMetrics := AStandardMetrics;
  Result := Self;
end;

function TWiRLMetricsSetting.GetDescription(const AName: string): string;
var
  LMetric: TCustomMetric;
begin
  if AName = MetricFilterEnabledName then
    Exit('This filter is enabled');
  if AName = MetricMemoryUsageName then
    Exit('Memory used by this process');
  if AName = MetricNumOfRequestName then
    Exit('Total number of request');

  for LMetric in FCustomMetrics do
  begin
    if LMetric.Name = AName then
      Exit(LMetric.Description);
  end;
  Result := AName;
end;

function TWiRLMetricsSetting.InMemoryStorage: IWiRLMetricsSetting;
begin
  FWiRLMetricsClass := TInMemoryMetrics;
  Result := Self;
end;

{ TWiRLMetrics }

procedure TWiRLMetrics.AcquireMetric(const AName: string; AValue: Double);
begin
  AcquireMetric(AName, [], AValue);
end;

constructor TWiRLMetrics.Create(AContext: TWiRLContextHttp);
begin
  FContext := AContext;
  FSetting := FContext.GetContextDataAs<TWiRLMetricsSetting>;
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

function TWiRLMetrics.GetDescription(const AName: string): string;
begin
  Result := FSetting.GetDescription(AName);
end;

procedure TWiRLMetrics.IncMetric(const AName: string);
begin
  IncMetric(AName, []);
end;

{ TInMemoryMetrics }

procedure TInMemoryMetrics.AcquireCustomMetrics;

  function GetRetriever(LRetrieverClass: TClass): ICustomMetricsRetriever;
  var
    LObject: TObject;
  begin
    LObject := TRttiHelper.CreateInstance(LRetrieverClass);
    if not Supports(LObject, ICustomMetricsRetriever, Result) then
      raise Exception.CreateFmt('Custom metrics retriever [%s] must implements [ICustomMetricsRetriever] interface', [LObject.ClassName]);
  end;

var
  LRetrieverClass: TClass;
  LRetriever: ICustomMetricsRetriever;
begin
  for LRetrieverClass in FSetting.FMetricsRetrievers do
  begin
    LRetriever := GetRetriever(LRetrieverClass);
    LRetriever.AcquireMetrics(Self, FSetting);
  end;
end;

procedure TInMemoryMetrics.AcquireMetric(const AName: string;
  ALabels: TArray<TWiRLMetricLabel>; AValue: Double);
var
  LKey: string;
  LSample: TWiRLMetricSample;
begin
  inherited;
  TMonitor.Enter(FItems);
  try
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
  finally
    TMonitor.Exit(FItems);
  end;

end;

procedure TInMemoryMetrics.AcquireStandardMetrics;
var
  LFilter: TWiRLFilterConstructorProxy;
begin
  if TWiRLStandardMetric.EnabledFilters in FSetting.FStandardMetrics then
  begin
    for LFilter in TWiRLFilterRegistry.Instance do
    begin
      AcquireMetric(MetricFilterEnabledName, ['name:' + LFilter.TypeTClass.ClassName], 1);
    end;
  end;

  if TWiRLStandardMetric.MemoryUsage in FSetting.FStandardMetrics then
  begin
    AcquireMetric(MetricMemoryUsageName, GetMemoryUsed);
  end;
end;

constructor TInMemoryMetrics.Create(AContext: TWiRLContextHttp);
begin
  inherited;
  FItems := TObjectDictionary<string, TWiRLMetricSample>.Create([doOwnsValues]);
end;

destructor TInMemoryMetrics.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TInMemoryMetrics.GetEnumerator: TEnumerator<TWiRLMetricSample>;
begin
  TMonitor.Enter(FItems);
  try
    AcquireStandardMetrics;
    AcquireCustomMetrics;
    Result := FItems.Values.GetEnumerator;
  finally
    TMonitor.Exit(FItems);
  end;
end;

procedure TInMemoryMetrics.IncMetric(const AName: string;
  ALabels: TArray<TWiRLMetricLabel>);
var
  LKey: string;
  LSample: TWiRLMetricSample;
begin
  inherited;
  TMonitor.Enter(FItems);
  try
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
  finally
    TMonitor.Exit(FItems);
  end;
end;

{ TMetricsFactory }

constructor TMetricsFactory.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
end;

function TMetricsFactory.CreateContextObject(const AObject: TRttiObject;
  AContext: TWiRLContextHttp): TValue;
var
  LMetricsSetting: TWiRLMetricsSetting;
begin
  // Double checked locking
  // I prefer this over  "Compare and swap" because I'm unsure of the
  // side effects of the LMetricsClass (connection to a log server?)
  if not Assigned(WiRLMetrics) then
  begin
    FLock.Acquire;
    try
      if not Assigned(WiRLMetrics) then
      begin
        LMetricsSetting := AContext.GetContextDataAs<TWiRLMetricsSetting>;
        if not Assigned(LMetricsSetting.FWiRLMetricsClass) then
          raise EWiRLServerException.Create('Metrics class not defined');
        WiRLMetrics := LMetricsSetting.FWiRLMetricsClass.Create(AContext);
      end;
    finally
      FLock.Release;
    end;
  end;
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

destructor TMetricsFactory.Destroy;
begin
  FLock.Free;
  inherited;
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

{ TResponseMetricFilter }

procedure TResponseMetricFilter.Filter(
  ARequestContext: TWiRLContainerResponseContext);
var
  LPath: string;
  LMethod: string;
begin
  if TWiRLStandardMetric.NumRequest in Setting.FStandardMetrics then
  begin
    LPath := ARequestContext.Request.PathInfo;
    LMethod := ARequestContext.Request.Method;
    Metrics.IncMetric(MetricNumOfRequestName, ['path:' + LPath,'method:' + LMethod]);
  end;
end;

initialization

  TWiRLContextInjectionRegistry.Instance.RegisterFactory<TWiRLMetrics>(TMetricsFactory);
  TWiRLConfigClassRegistry.Instance.RegisterConfigClass(TWiRLMetricsSetting);
  TWiRLFilterRegistry.Instance.RegisterFilter<TResponseMetricFilter>;

  InitMetrics;

finalization

  FreeMetrics;

end.
