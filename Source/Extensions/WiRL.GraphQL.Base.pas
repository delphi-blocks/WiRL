{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2022 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
{ See GraphQL.md for some help                                                 }
{******************************************************************************}
unit WiRL.GraphQL.Base;

interface

uses
  System.Classes, System.SysUtils, System.JSON, System.Rtti,

  WiRL.http.Accept.MediaType,
  WiRL.http.Headers,
  WiRL.http.Core,
  WiRL.Core.MessageBodyWriter,
  WiRL.Core.MessageBodyReader,
  WiRL.Core.MessageBody.Classes,
  WiRL.Core.Attributes,
  WiRL.Core.Declarations,
  WiRL.Core.Context,
  WiRL.Core.Context.Server,
  WiRL.Core.Injection,

  WiRL.Configuration.Core,

  Neon.Core.Attributes,

  GraphQL.Query,
  GraphQL.Resolver.Core;

type
  TGraphQLPostQuery = class
  private
    FQuery: string;
    //FOperationName: string;
    FVariables: IGraphQLVariables;
  public
    class function ParseJSON(AJSONValue: TJSONValue): TGraphQLPostQuery;
  public
    constructor Create(const AQuery: string; AVariables: IGraphQLVariables);
    destructor Destroy; override;

    property Query: string read FQuery;
    property Variables: IGraphQLVariables read FVariables;
  end;

  ///////////////////////////////////////////////////////////////////////
  // Configuration object
  ///////////////////////////////////////////////////////////////////////
  IWiRLGraphQLSetting = interface(IWiRLConfiguration)
  ['{ADD73B5E-827C-4A93-919A-B83911828739}']
    function SetGraphQLQuery(AQuery: TGraphQLQuery): IWiRLGraphQLSetting;
    function GetGraphQLQuery: TGraphQLQuery;
  end;

  [Implements(IWiRLGraphQLSetting)]
  TWiRLFormatSettingConfig = class(TWiRLConfiguration, IWiRLGraphQLSetting)
  private
    FQuery: TGraphQLQuery;
  public
    function SetGraphQLQuery(AQuery: TGraphQLQuery): IWiRLGraphQLSetting;
    function GetGraphQLQuery: TGraphQLQuery;
  end;

  ///////////////////////////////////////////////////////////////////////
  // Message body reader
  ///////////////////////////////////////////////////////////////////////
  [Consumes(TMediaType.APPLICATION_JSON)]
  TWiRLGraphQLPostQueryReader = class(TInterfacedObject, IMessageBodyReader)
  public
    function ReadFrom(AType: TRttitype; AMediaType: TMediaType;
  	  AHeaders: IWiRLHeaders; AContentStream: TStream): TValue; overload;

    procedure ReadFrom(AObject: TObject; AType: TRttitype; AMediaType: TMediaType;
	    AHeaders: IWiRLHeaders; AContentStream: TStream); overload;

  end;

  ///////////////////////////////////////////////////////////////////////
  // Context Provider
  ///////////////////////////////////////////////////////////////////////
  TGraphQLQueryFactory = class(TInterfacedObject, IContextHttpFactory)
  public
    function CreateContextObject(const AObject: TRttiObject;
      AContext: TWiRLContextHttp): TValue;
  end;

implementation

procedure RegisterGraphQLConfig;
begin
  TWiRLConfigClassRegistry.Instance.RegisterConfigClass(TWiRLFormatSettingConfig);
end;

procedure RegisterGraphQLContext;
begin
  TWiRLContextInjectionRegistry.Instance.RegisterFactory<TGraphQLQuery>(TGraphQLQueryFactory);
end;

{ TGraphQLPostQuery }

constructor TGraphQLPostQuery.Create(const AQuery: string; AVariables: IGraphQLVariables);
begin
  inherited Create;
  FQuery := AQuery;
  FVariables := AVariables;
end;

destructor TGraphQLPostQuery.Destroy;
begin
  //FVariables.Free;
  inherited;
end;

class function TGraphQLPostQuery.ParseJSON(
  AJSONValue: TJSONValue): TGraphQLPostQuery;
var
  LQuery: string;
  LVariables: IGraphQLVariables;
  LVariablesJson: TJSONValue;
  LValuePair: TJSONPair;
begin
  if not Assigned(AJSONValue) then
    raise Exception.Create('Not a valid GraphQL request');

  LQuery := AJSONValue.GetValue<string>('query');
  LVariablesJson := AJSONValue.GetValue<TJSONValue>('variables');

  LVariables := TGraphQLVariables.Create;
  for LValuePair in LVariablesJson as TJSONObject do
  begin
    if LValuePair.JsonValue is TJSONNumber then
    begin
      LVariables.SetVariable(LValuePair.JsonString.Value, TJSONNumber(LValuePair.JsonValue).AsInt);
    end
    else if LValuePair.JsonValue is TJSONString then
    begin
      LVariables.SetVariable(LValuePair.JsonString.Value, LValuePair.JsonValue.ToString);
    end
    else
      raise Exception.CreateFmt('Unhandled json "%s" value', [LValuePair.JsonString.Value]);
  end;

  Result := TGraphQLPostQuery.Create(LQuery, LVariables);

end;

procedure RegisterMessageBodyClasses;
begin
  TMessageBodyReaderRegistry.Instance.RegisterReader<TGraphQLPostQuery>(TWiRLGraphQLPostQueryReader, TMessageBodyReaderRegistry.AFFINITY_VERY_HIGH);

  (*
  TMessageBodyReaderRegistry.Instance.RegisterReader(
    TWiRLGraphQLPostQueryReader,
    function(AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Boolean
    begin
      Result := AType.IsInstance and (TRttiInstanceType(AType).MetaclassType = TGraphQLPostQuery);
    end,
    function(AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Integer
    begin
      Result := TMessageBodyWriterRegistry.AFFINITY_VERY_HIGH;
    end
  );
  *)
end;

{ TWiRLGraphQLPostQueryReader }

procedure TWiRLGraphQLPostQueryReader.ReadFrom(AObject: TObject;
  AType: TRttitype; AMediaType: TMediaType; AHeaders: IWiRLHeaders;
  AContentStream: TStream);
begin
  raise Exception.Create('TWiRLGraphQLPostQueryReader.ReadFrom: Not implemented');
end;

function TWiRLGraphQLPostQueryReader.ReadFrom(AType: TRttitype;
  AMediaType: TMediaType; AHeaders: IWiRLHeaders;
  AContentStream: TStream): TValue;
var
  LJSONValue: TJSONValue;
begin
  LJSONValue := TJSONObject.ParseJSONValue(ContentStreamToString(AMediaType.Charset, AContentStream));
  try
    Result := TGraphQLPostQuery.ParseJSON(LJSONValue);
  finally
    LJSONValue.Free;
  end;

end;

{ TWiRLFormatSettingConfig }

function TWiRLFormatSettingConfig.GetGraphQLQuery: TGraphQLQuery;
begin
  if not Assigned(FQuery) then
    raise Exception.Create('TGraphQLQuery not registered');
  Result := FQuery;
end;

function TWiRLFormatSettingConfig.SetGraphQLQuery(
  AQuery: TGraphQLQuery): IWiRLGraphQLSetting;
begin
  FQuery := AQuery;
  Result := Self;
end;

{ TGraphQLQueryFactory }

function TGraphQLQueryFactory.CreateContextObject(const AObject: TRttiObject;
  AContext: TWiRLContextHttp): TValue;
var
  LGraphQLSetting: IWiRLGraphQLSetting;
begin
  LGraphQLSetting := AContext.FindContextDataAsByIntf(IWiRLGraphQLSetting) as IWiRLGraphQLSetting;

  Result := LGraphQLSetting.GetGraphQLQuery;
end;

initialization

RegisterMessageBodyClasses;
RegisterGraphQLConfig;
RegisterGraphQLContext;


end.
