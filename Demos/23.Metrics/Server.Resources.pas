{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Resources;

interface

uses
  System.SysUtils, System.Classes,

  WiRL.Core.Registry,
  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  WiRL.Core.MessageBody.Default,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.Metrics.Core,
  WiRL.Core.Exceptions;

type
  [Path('demo')]
  TDemoResource = class
  private
    [Context] Metrics: TWiRLMetrics;
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SampleText: string;

    [Path('metrics')]
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function GetMetrics: TWiRLMetrics;
  end;

implementation

{ TDemoResource }

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

function TDemoResource.GetMetrics: TWiRLMetrics;
begin
  Metrics.AcquireMetric('test_memory', GetMemoryUsed);
  Metrics.IncMetric('test_num_request', ['api:get_metric','host:myserver']);

  Result := Metrics;
end;

function TDemoResource.SampleText: string;
begin
  Result := 'Hello, World!'
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TDemoResource>;

end.
