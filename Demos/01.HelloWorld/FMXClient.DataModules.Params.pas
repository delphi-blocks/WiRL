unit FMXClient.DataModules.Params;

interface

uses
  System.SysUtils, System.Classes, WiRL.Client.CustomResource,
  WiRL.Client.Resource, WiRL.Client.Application,
  WiRL.http.Client, WiRL.http.Accept.MediaType,
  Demo.Entities;

type
  TParamsModule = class(TDataModule)
    WiRLClient1: TWiRLClient;
    WiRLClientApplication1: TWiRLClientApplication;
    HelloWorldResource: TWiRLClientResource;
    EchoStringResource: TWiRLClientResource;
    ReverseStringResource: TWiRLClientResource;
    PostStreamResource: TWiRLClientResource;
  private
    { Private declarations }
  public
    function GetString(const AString: string): string;
    function GetInteger(AInt: Integer): Integer;
    function GetFloat(AFloat: Double): Double;
    function GetStringFromObject(const AString: string): string;
    function GetBoolean(ABool: Boolean): Boolean;
    function GetDate(ADate: TDate): TDate;
    function GetDateTime(ADateTime: TDateTime): TDateTime;
    function GetTime(ATime: TTime): TTime;
    function GetEnum(AEnum: TMyEnum): TMyEnum;
    function GetArray(AArray: TArrayInt): TArrayInt;
  end;

var
  ParamsModule: TParamsModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ TParamsModule }

function TParamsModule.GetArray(AArray: TArrayInt): TArrayInt;
begin
  Result := WiRLClientApplication1
    .Resource('params/array/{AArray}')
    .Accept(TMediaType.APPLICATION_JSON)
    .PathParam('AArray', AArray)
    .Get<TArrayInt>;
end;

function TParamsModule.GetBoolean(ABool: Boolean): Boolean;
begin
  Result := WiRLClientApplication1
    .Resource('params/bool/{ABool}')
    .PathParam('ABool', ABool)
    .Get<Boolean>;
end;

function TParamsModule.GetDate(ADate: TDate): TDate;
begin
  Result := WiRLClientApplication1
    .Resource('params/date/{ADate}')
    .PathParam('ADate', ADate)
    .Get<TDate>;
end;

function TParamsModule.GetDateTime(ADateTime: TDateTime): TDateTime;
begin
  Result := WiRLClientApplication1
    .Resource('params/datetime/{ADateTime}')
    .PathParam('ADateTime', ADateTime)
    .Get<TDateTime>;
end;

function TParamsModule.GetEnum(AEnum: TMyEnum): TMyEnum;
begin
  Result := WiRLClientApplication1
    .Resource('params/enum/{AEnum}')
    .PathParam('AEnum', AEnum)
    .Get<TMyEnum>;
end;

function TParamsModule.GetFloat(AFloat: Double): Double;
begin
  Result := WiRLClientApplication1
    .Resource('params/float/{AFloat}')
    .PathParam('AFloat', AFloat)
    .Get<Double>;
end;

function TParamsModule.GetInteger(AInt: Integer): Integer;
begin
  Result := WiRLClientApplication1
    .Resource('params/int/{AInt}')
    .PathParam('AInt', AInt)
    .Get<Integer>;
end;

function TParamsModule.GetString(const AString: string): string;
begin
  Result := WiRLClientApplication1
    .Resource('params/str/{AString}')
    .PathParam('AString', AString)
    .Get<string>;
end;

function TParamsModule.GetStringFromObject(const AString: string): string;
begin
  Result := WiRLClientApplication1
    .Resource('params/object/{AString}')
    .PathParam('AString', AString)
    .Get<string>;
end;

function TParamsModule.GetTime(ATime: TTime): TTime;
begin
  Result := WiRLClientApplication1
    .Resource('params/time/{ATime}')
    .PathParam('ATime', ATime)
    .Get<TTime>;
end;

end.
