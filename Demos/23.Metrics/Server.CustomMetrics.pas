unit Server.CustomMetrics;

interface

uses
  System.Classes, System.SysUtils,

  WiRL.Metrics.Core;

type
  TMyCustomMetrics = class(TInterfacedObject, ICustomMetricsRetriever)
  public
    procedure AcquireMetrics(AMetrics: TWiRLMetrics; ASetting: TWiRLMetricsSetting);
  end;

implementation

{ TMyCustomMetrics }

procedure TMyCustomMetrics.AcquireMetrics(AMetrics: TWiRLMetrics;
  ASetting: TWiRLMetricsSetting);
begin
  AMetrics.AcquireMetric('custom_test', 1);
end;

end.
