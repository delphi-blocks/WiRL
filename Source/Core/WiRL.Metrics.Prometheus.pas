{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2022 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Metrics.Prometheus;

interface

uses
  System.Classes, System.SysUtils, System.Rtti,

  WiRL.Core.Attributes,
  WiRL.Core.Exceptions,
  WiRL.Core.MessageBodyWriter,
  WiRL.Core.MessageBody.Classes,
  WiRL.Configuration.Core,
  WiRL.Metrics.Core,
  WiRL.http.Accept.MediaType,
  WiRL.http.Headers;

type
  IWiRLPrometheusSetting = interface(IWiRLConfiguration)
    ['{46DA9A5B-4ABF-45FE-825E-50884CEA6BD4}']
    function SetEnabled(AValue: Boolean): IWiRLPrometheusSetting;
  end;

  [Implements(IWiRLPrometheusSetting)]
  TWiRLPrometheusSetting = class(TWiRLConfiguration, IWiRLPrometheusSetting)
  public
    { IWiRLPrometheusSetting }
    function SetEnabled(AValue: Boolean): IWiRLPrometheusSetting;
  end;

  [Produces(TMediaType.TEXT_PLAIN)]
  TPrometheusMetricsBodyWriter = class(TInterfacedObject, IMessageBodyWriter)
  private
    function GeneratePrometheusMetrics(AMetrics: TWiRLMetrics): string;
   public
     procedure WriteTo(const AValue: TValue; const AAttributes: System.TArray<System.TCustomAttribute>; AMediaType: TMediaType; AHeaders: IWiRLHeaders; AContentStream: TStream);
   end;

implementation

function DoubleQuote(const AValue: string): string;
begin
  Result := '"' + StringReplace(AValue, '"', '\"', [rfReplaceAll]) + '"';
end;

{ TPrometheusMetricsBodyWriter }

function TPrometheusMetricsBodyWriter.GeneratePrometheusMetrics(AMetrics: TWiRLMetrics): string;
var
  LSample: TWiRLMetricSample;
  FS: TFormatSettings;
  LLabelString: string;
  LLabel: TWiRLMetricLabel;
begin
  if not (AMetrics is TInMemoryMetrics) then
    raise EWiRLServerException.CreateFmt('Cannot read metrics from [%s]', [AMetrics.ClassName]);

  FS := TFormatSettings.Invariant;
  Result := '';
  for LSample in TInMemoryMetrics(AMetrics) do
  begin
    LLabelString := '';
    if Length(LSample.Labels) > 0 then
    begin
      LLabelString := LLabelString + '{';
      for LLabel in LSample.Labels do
      begin
        LLabelString := LLabelString + LLabel.Name + '=' + DoubleQuote(LLabel.Value) + ',';
      end;
      LLabelString := Copy(LLabelString, 1, Length(LLabelString) - 1) + '}';
    end;

    Result := Result +
      '# HELP ' + LSample.Name + ' Total memory used in the node in bytes' + #10 +
      '# TYPE ' + LSample.Name + ' gauge' + #10 +
      LSample.Name + LLabelString + ' ' + FloatToStr(LSample.Value, FS) + #10;
  end;
end;

procedure TPrometheusMetricsBodyWriter.WriteTo(const AValue: TValue;
  const AAttributes: System.TArray<System.TCustomAttribute>;
  AMediaType: TMediaType; AHeaders: IWiRLHeaders; AContentStream: TStream);
var
  LEncoding: TEncoding;
  LStringValue: string;
  LBytes: TBytes;
begin
  LEncoding := AMediaType.GetDelphiEncoding;
  try
    LStringValue := GeneratePrometheusMetrics(AValue.AsObject as TWiRLMetrics);
    LBytes := LEncoding.GetBytes(LStringValue);
    AContentStream.Write(LBytes[0], Length(LBytes));
  finally
    LEncoding.Free;
  end;
end;

{ TWiRLPrometheusSetting }

function TWiRLPrometheusSetting.SetEnabled(
  AValue: Boolean): IWiRLPrometheusSetting;
begin
  if AValue then
  begin
    TMessageBodyWriterRegistry.Instance.RegisterWriter<TWiRLMetrics>(TPrometheusMetricsBodyWriter);
  end
  else
  begin
    TMessageBodyWriterRegistry.Instance.UnregisterWriter(TPrometheusMetricsBodyWriter);
  end;
  Result := Self;
end;

initialization

  TWiRLConfigClassRegistry.Instance.RegisterConfigClass(TWiRLPrometheusSetting);

end.
