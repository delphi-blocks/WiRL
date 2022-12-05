{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Client.CustomResource;

{$I ..\Core\WiRL.inc}

interface

uses
  System.SysUtils, System.Classes, System.Rtti, System.Contnrs, System.Types, System.TypInfo,
  System.Generics.Collections, Data.DB,
  WiRL.Core.Context,
  WiRL.Client.Application,
  WiRL.Data.Utils,
  WiRL.http.Core,
  WiRL.http.Headers,
  WiRL.http.Client.Interfaces,
  WiRL.http.Client;

type
  TBeforeRequestEvent = procedure (Sender: TObject; const AHttpMethod: string; ARequestStream: TStream; out AResponse: IWiRLResponse) of object;
  TAfterRequestEvent = procedure (Sender: TObject; const AHttpMethod: string; ARequestStream: TStream; AResponse: IWiRLResponse) of object;
  TRequestErrorEvent = procedure (Sender: TObject; const AHttpMethod: string; ARequestStream: TStream; AResponse: IWiRLResponse) of object;

  {$IF DEFINED(HAS_NEW_ANDROID_PID)}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator32 or pidiOSDevice32 or pidAndroidArm32)]
  {$ELSEIF DEFINED(HAS_NEW_PIDS)}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator32 or pidiOSDevice32 or pidAndroid32Arm)]
  {$ELSE}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  {$ENDIF}
  TWiRLClientCustomResource = class(TComponent)
  private
    FContext: TWiRLContextHttp;
    FResource: string;
    FApplication: TWiRLClientApplication;
    FPathParams: TStrings;
    FQueryParams: TStrings;
    FHeaders: IWiRLHeaders;
    FBeforeRequest: TBeforeRequestEvent;
    FAfterRequest: TAfterRequestEvent;
    FOnRequestError: TRequestErrorEvent;
    FFilters: TStringDynArray;
    procedure SetPathParams(const Value: TStrings);
    procedure SetQueryParams(const Value: TStrings);

    procedure ContextInjection(AInstance: TObject);
    function MergeHeaders(const AHttpMethod: string): IWiRLHeaders;
    function StreamToObject<T>(AHeaders: IWiRLHeaders; AStream: TStream): T; overload;
    procedure StreamToObject(AObject: TObject; AHeaders: IWiRLHeaders; AStream: TStream); overload;
    procedure ObjectToStream<T>(AHeaders: IWiRLHeaders; AObject: T; AStream: TStream); overload;
    function SameObject<T>(AGeneric: T; AObject: TObject): Boolean;
    procedure SetApplication(const Value: TWiRLClientApplication);
    function ValueToString(const AValue: TValue): string;
    procedure StreamToEntity<T>(AEntity: T; AHeaders: IWiRLHeaders;
      AStream: TStream);
    procedure StreamToArray(AArray: TValue; AHeaders: IWiRLHeaders;
      AStream: TStream);
  protected
    function GetClient: TWiRLClient; virtual;
    function GetPath: string; virtual;
    function GetURL: string; virtual;
    function GetApplication: TWiRLClientApplication; virtual;
    function GetAccept: string;
    function GetContentType: string;
    procedure DoBeforeRequest(const AHttpMethod: string; ARequestStream: TStream; out AResponse: IWiRLResponse); virtual;
    procedure DoAfterRequest(const AHttpMethod: string; ARequestStream: TStream; AResponse: IWiRLResponse); virtual;
    procedure DoRequestError(const AHttpMethod: string; ARequestStream: TStream; AResponse: IWiRLResponse); virtual;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    // Handles the parent/child relationship for the designer
    procedure SetParentComponent(AParent: TComponent); override;

    procedure InitHttpRequest; virtual;
    function InternalHttpRequest(const AHttpMethod: string; ARequestStream, AResponseStream: TStream): IWiRLResponse; virtual;

    procedure DefineProperties(Filer: TFiler); override;
    procedure LoadHeadersProperty(Reader: TReader);
    procedure StoreHeadersProperty(Writer: TWriter);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetFilters(const AFilters: TStringDynArray);
    function HasFilter(AAttribute: TCustomAttribute): Boolean;

    // Handles the parent/child relationship for the designer
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;

    // http verbs
    function Get<T>: T; overload;
    procedure Get(AResponseEntity: TObject); overload;
    procedure Get<T>(AResponseEntity: T); overload;
    function Post<T, V>(const ARequestEntity: T): V; overload;
    procedure Post<T>(const ARequestEntity: T; AResponseEntity: TObject); overload;
    function Put<T, V>(const ARequestEntity: T): V; overload;
    procedure Put<T>(const ARequestEntity: T; AResponseEntity: TObject); overload;
    function Delete<T>: T; overload;
    procedure Delete(AResponseEntity: TObject); overload;
    function Patch<T, V>(const ARequestEntity: T): V; overload;
    procedure Patch<T>(const ARequestEntity: T; AResponseEntity: TObject); overload;

    function GenericHttpRequest<T, V>(const AHttpMethod: string; const ARequestEntity: T): V; overload;
    procedure GenericHttpRequest<T, V>(const AHttpMethod: string; const ARequestEntity: T; AResponseEntity: V); overload;

  public
    procedure QueryParam(const AName: string; const AValue: TValue);
    procedure PathParam(const AName: string; const AValue: TValue);

    property Accept: string read GetAccept;
    property ContentType: string read GetContentType;
    property Application: TWiRLClientApplication read GetApplication write SetApplication;
    property Client: TWiRLClient read GetClient;
    property Resource: string read FResource write FResource;
    property Path: string read GetPath;
    property PathParams: TStrings read FPathParams write SetPathParams;
    property QueryParams: TStrings read FQueryParams write SetQueryParams;
    property URL: string read GetURL;
    property Headers: IWiRLHeaders read FHeaders write FHeaders;
    property AfterRequest: TAfterRequestEvent read FAfterRequest write FAfterRequest;
    property BeforeRequest: TBeforeRequestEvent read FBeforeRequest write FBeforeRequest;
    property OnRequestError: TRequestErrorEvent read FOnRequestError write FOnRequestError;
  end;

  TWiRLResourceHeaders = class(TWiRLHeaders)
  private
    FCustomResource: TWiRLClientCustomResource;
  protected
    procedure SetValue(const AName: string; const AValue: string); override;
  public
    constructor Create(ACustomResource: TWiRLClientCustomResource);
  end;

implementation

uses
  WiRL.Configuration.Core,
  WiRL.Configuration.Neon,
  WiRL.Configuration.Converter,
  WiRL.http.Accept.MediaType,
  WiRL.Core.Classes,
  WiRL.Core.Injection,
  WiRL.Core.Converter,
  WiRL.Core.MessageBodyReader,
  WiRL.Core.MessageBodyWriter,
  WiRL.Client.Utils,
  WiRL.http.URL,
  WiRL.Rtti.Utils,
  WiRL.Core.Utils;

type
  THttpMethodImplementation = reference to function (
    AResource: TWiRLClientCustomResource;
    ARequestStream, AResponseStream: TStream;
    ACustomHeaders: IWiRLHeaders): IWiRLResponse;

  THttpMethodImplementations = array [TWiRLHttpMethod] of THttpMethodImplementation;

var
  HttpMethodImplementations: THttpMethodImplementations;

procedure RegisterHttpMethodImplementations;
begin
  HttpMethodImplementations[TWiRLHttpMethod.GET] :=
    function (AResource: TWiRLClientCustomResource; ARequestStream, AResponseStream: TStream; ACustomHeaders: IWiRLHeaders): IWiRLResponse
    begin
      Result := AResource.Client.Get(AResource.URL, AResponseStream, ACustomHeaders);
    end;

  HttpMethodImplementations[TWiRLHttpMethod.POST] :=
    function (AResource: TWiRLClientCustomResource; ARequestStream, AResponseStream: TStream; ACustomHeaders: IWiRLHeaders): IWiRLResponse
    begin
      Result := AResource.Client.Post(AResource.URL, ARequestStream, AResponseStream, ACustomHeaders);
    end;

  HttpMethodImplementations[TWiRLHttpMethod.PUT] :=
    function (AResource: TWiRLClientCustomResource; ARequestStream, AResponseStream: TStream; ACustomHeaders: IWiRLHeaders): IWiRLResponse
    begin
      Result := AResource.Client.Put(AResource.URL, ARequestStream, AResponseStream, ACustomHeaders);
    end;

  HttpMethodImplementations[TWiRLHttpMethod.DELETE] :=
    function (AResource: TWiRLClientCustomResource; ARequestStream, AResponseStream: TStream; ACustomHeaders: IWiRLHeaders): IWiRLResponse
    begin
      Result := AResource.Client.Delete(AResource.URL, AResponseStream, ACustomHeaders);
    end;

  HttpMethodImplementations[TWiRLHttpMethod.PATCH] :=
    function (AResource: TWiRLClientCustomResource; ARequestStream, AResponseStream: TStream; ACustomHeaders: IWiRLHeaders): IWiRLResponse
    begin
      Result := AResource.Client.Patch(AResource.URL, ARequestStream, AResponseStream, ACustomHeaders);
    end;

end;

{ TWiRLClientCustomResource }

procedure TWiRLClientCustomResource.ContextInjection(AInstance: TObject);
begin
  TWiRLContextInjectionRegistry.Instance.
    ContextInjection(AInstance, FContext);
end;

constructor TWiRLClientCustomResource.Create(AOwner: TComponent);
begin
  inherited;
  FResource := 'main';
//  if TWiRLComponentHelper.IsDesigning(Self) then
//    Application := TWiRLComponentHelper.FindDefault<TWiRLClientApplication>(Self);
  FPathParams := TStringList.Create;
  FQueryParams := TStringList.Create;
  FContext := TWiRLContextHttp.Create;
  FHeaders := TWiRLResourceHeaders.Create(Self);
end;

function TWiRLClientCustomResource.GetClient: TWiRLClient;
begin
  Result := nil;
  if Assigned(FApplication) then
    Result := FApplication.Client;
end;

function TWiRLClientCustomResource.GetContentType: string;
begin
  Result := Headers.ContentType;
  if (Result = '') and Assigned(Application) then
    Result := Application.DefaultMediaType;
end;

function TWiRLClientCustomResource.GetParentComponent: TComponent;
begin
  Result := Application;
end;

function TWiRLClientCustomResource.GetPath: string;
var
  LEngine: string;
  LApplication: string;
begin
  LEngine := '';
  if Assigned(Client) then
    LEngine := Client.WiRLEngineURL;

  LApplication := '';
  if Assigned(Application) then
    LApplication := Application.AppName;


  Result := TWiRLURL.CombinePath([LEngine, LApplication, Resource]);
end;


function TWiRLClientCustomResource.GetURL: string;
var
  LIndex: Integer;
begin
  Result := Path;
  for LIndex := 0 to FPathParams.Count - 1 do
  begin
    Result := StringReplace(Result, '{' + FPathParams.Names[LIndex] + '}', TWiRLURL.URLEncode(FPathParams.ValueFromIndex[LIndex]), [rfReplaceAll, rfIgnoreCase]);
  end;

//  Result := TWiRLURL.CombinePath([
//    Path,
//    TWiRLURL.CombinePath(TWiRLURL.URLEncode(FPathParamsValues.ToStringArray))
//  ]);

  if FQueryParams.Count > 0 then
    Result := Result + '?' + SmartConcat(TWiRLURL.URLEncode(FQueryParams.ToStringArray), '&');
end;

function TWiRLClientCustomResource.HasFilter(
  AAttribute: TCustomAttribute): Boolean;
var
  LFilterName: string;
  LAttributeName: string;
begin
  Result := False;
  LAttributeName := AAttribute.ClassName;
  for LFilterName in FFilters do
  begin
    if SameText(LFilterName, LAttributeName) or SameText(LFilterName + 'Attribute', LAttributeName) then
      Exit(True);
  end;
end;

function TWiRLClientCustomResource.HasParent: Boolean;
begin
  Result := Assigned(FApplication);
end;

procedure TWiRLClientCustomResource.InitHttpRequest;
var
  LPair: TPair<TWiRLConfigurationClass, TWiRLConfiguration>;
begin
  // Fill the context
  FContext.AddContainerOnce(FApplication, False);

  for LPair in FApplication.Configs do
    FContext.AddContainerOnce(LPair.Value, False);
end;

function TWiRLClientCustomResource.InternalHttpRequest(
  const AHttpMethod: string; ARequestStream, AResponseStream: TStream): IWiRLResponse;
var
  LHttpMethodImplementation: THttpMethodImplementation;
begin
  LHttpMethodImplementation := HttpMethodImplementations[TWiRLHttpMethod.ConvertFromString(AHttpMethod)];
  if not Assigned(LHttpMethodImplementation) then
    raise EWiRLClientException.CreateFmt('Implementation not found for method [%s]', [AHttpMethod]);

  try
    Result := LHttpMethodImplementation(Self, ARequestStream, AResponseStream, MergeHeaders(AHttpMethod));
  except
    on E: EWiRLClientProtocolException do
    begin
      raise EWiRLClientResourceException.Create(E.Response);
    end;
  end;
end;

procedure TWiRLClientCustomResource.LoadHeadersProperty(Reader: TReader);
var
  LRowHeader: string;
  LHeaderPair: TArray<string>;
begin
  Reader.ReadListBegin;
  while not Reader.EndOfList do
  begin
    LRowHeader := Reader.ReadString;
    LHeaderPair := LRowHeader.Split(['=']);
    if Length(LHeaderPair) > 1 then
    begin
      FHeaders.AddHeader(TWiRLHeader.Create(LHeaderPair[0], LHeaderPair[1]));
    end;
  end;
  Reader.ReadListEnd;
end;

function TWiRLClientCustomResource.MergeHeaders(const AHttpMethod: string): IWiRLHeaders;

  function HasRequestBody(const AHttpMethod: string): Boolean;
  begin
    if (AHttpMethod = 'POST') or (AHttpMethod = 'PUT') then
      Result := True
    else
      Result := False;
  end;

var
  LHeader: TWiRLHeader;
begin
  Result := TWiRLHeaders.Create;
  if Accept <> '' then
    Result.Accept := Accept;
  if HasRequestBody(AHttpMethod) and (ContentType <> '') then
    Result.ContentType := ContentType;

  for LHeader in FHeaders do
  begin
    Result.Values[LHeader.Name] := LHeader.Value;
  end;
end;

procedure TWiRLClientCustomResource.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FApplication then
      FApplication := nil;
  end;
end;

procedure TWiRLClientCustomResource.StoreHeadersProperty(Writer: TWriter);
var
  LHeader: TWiRLHeader;
begin
  Writer.WriteListBegin;
  for LHeader in FHeaders do
  begin
    Writer.WriteString(LHeader.Name + '=' + LHeader.Value);
  end;
  Writer.WriteListEnd;
end;

procedure TWiRLClientCustomResource.StreamToObject(AObject: TObject;
  AHeaders: IWiRLHeaders; AStream: TStream);
var
  LType: TRttiType;
  LMediaType: TMediaType;
  LReader: IMessageBodyReader;
begin
  LType := TRttiHelper.Context.GetType(AObject.ClassInfo);
  LMediaType := TMediaType.Create(AHeaders.Values[TWiRLHeader.CONTENT_TYPE]);
  try
    LReader := Application.ReaderRegistry.FindReader(LType, LMediaType);
    if not Assigned(LReader) then
      raise EWiRLClientException.CreateFmt('Reader not found for [%s] content type: [%s]', [LType.Name, LMediaType.MediaType]);
    ContextInjection(LReader as TObject);

    LReader.ReadFrom(AObject, LType, LMediaType, AHeaders, AStream);
  finally
    LMediaType.Free;
  end;
end;

procedure TWiRLClientCustomResource.StreamToArray(AArray: TValue;
  AHeaders: IWiRLHeaders; AStream: TStream);
var
  LList: TDataSetList;
  LIndex: Integer;
  LItem: TValue;
begin
  LList := TDataSetList.Create(False);
  try
    for LIndex := 0 to AArray.GetArrayLength - 1 do
    begin
      LItem := AArray.GetArrayElement(LIndex);
      if not LItem.IsObject then
        raise EWiRLClientException.Create('Array of primitive type not supported');

      if not (LItem.AsObject is TDataSet) then
        raise EWiRLClientException.Create('Error Message');

      LList.Add(TDataSet(LItem.AsObject));
    end;
    StreamToObject(LList, AHeaders, AStream);
  finally
    LList.Free;
  end;
end;

procedure TWiRLClientCustomResource.StreamToEntity<T>(AEntity: T;
  AHeaders: IWiRLHeaders; AStream: TStream);
var
  LValue: TValue;
begin
  LValue := TValue.From<T>(AEntity);

  if LValue.IsObject then
    StreamToObject(LValue.AsObject, AHeaders, AStream)
  else if LValue.IsArray then
    StreamToArray(LValue, AHeaders, AStream)
  else
    raise EWiRLClientException.Create('Not supported');
end;

function TWiRLClientCustomResource.StreamToObject<T>(AHeaders: IWiRLHeaders; AStream: TStream): T;
var
  LType: TRttiType;
  LMediaType: TMediaType;
  LReader: IMessageBodyReader;
  LValue: TValue;
begin
  LType := TRttiHelper.Context.GetType(TypeInfo(T));
  LMediaType := TMediaType.Create(AHeaders.ContentType);
  try
    LReader := Application.ReaderRegistry.FindReader(LType, LMediaType);
    if not Assigned(LReader) then
      raise EWiRLClientException.CreateFmt('Reader not found for [%s] content type: [%s]', [LType.Name, LMediaType.MediaType]);
    ContextInjection(LReader as TObject);

    LValue := LReader.ReadFrom(LType, LMediaType, AHeaders, AStream);
    Result := LValue.AsType<T>;
  finally
    LMediaType.Free;
  end;
end;

procedure TWiRLClientCustomResource.ObjectToStream<T>(AHeaders: IWiRLHeaders;
  AObject: T; AStream: TStream);
var
  LType: TRttiType;
  LMediaType: TMediaType;
  LWriter: IMessageBodyWriter;
  LValue: TValue;
begin
  LType := TRttiHelper.Context.GetType(TypeInfo(T));
  LMediaType := TMediaType.Create(ContentType);
  try
    LValue := TValue.From<T>(AObject);
    LWriter := Application.WriterRegistry.FindWriter(LType, LMediaType);
    if not Assigned(LWriter) then
      raise EWiRLClientException.CreateFmt('Writer not found for [%s] content type: [%s]', [LType.Name, LMediaType.MediaType]);

    ContextInjection(LWriter as TObject);
    LWriter.WriteTo(LValue, nil, LMediaType, AHeaders, AStream);
    AStream.Position := soFromBeginning;

  finally
    LMediaType.Free;
  end;
end;

procedure TWiRLClientCustomResource.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('CustomHeaders', LoadHeadersProperty, StoreHeadersProperty, FHeaders.Count > 0);
end;

procedure TWiRLClientCustomResource.Delete(AResponseEntity: TObject);
begin
  GenericHttpRequest<string, TObject>('DELETE', '', AResponseEntity);
end;

function TWiRLClientCustomResource.Delete<T>: T;
begin
  Result := GenericHttpRequest<string, T>('DELETE', '');
end;

function TWiRLClientCustomResource.GenericHttpRequest<T, V>(
  const AHttpMethod: string; const ARequestEntity: T): V;
var
  LRequestStream, LResponseStream: TMemoryStream;
  LResponse: IWiRLResponse;
begin
  if not Assigned(Client) then
    Exit;

  Result := default(V);
  LResponse := nil;
  InitHttpRequest;

  LRequestStream := TMemoryStream.Create;
  try
    LResponseStream := TGCMemoryStream.Create;
    try
      ObjectToStream<T>(MergeHeaders(AHttpMethod), ARequestEntity, LRequestStream);

      DoBeforeRequest(AHttpMethod, LRequestStream, LResponse);
      try
        if Assigned(LResponse) then
        begin
          if LResponse.StatusCode >= 400 then
            raise EWiRLClientProtocolException.Create(LResponse);
        end
        else
          LResponse := InternalHttpRequest(AHttpMethod, LRequestStream, LResponseStream);
      except
        on E: EWiRLClientProtocolException do
        begin
          DoRequestError(AHttpMethod, LRequestStream, E.Response);
          raise;
        end;
      end;
      DoAfterRequest(AHttpMethod, LRequestStream, LResponse);

      if Assigned(LResponse.ContentStream) then
        Result := StreamToObject<V>(LResponse.Headers, LResponse.ContentStream)
      else
        Result := StreamToObject<V>(LResponse.Headers, LResponseStream);
    finally
      if not SameObject<V>(Result, LResponseStream) then
        LResponseStream.Free;
    end;
  finally
    LRequestStream.Free;
  end;
end;

procedure TWiRLClientCustomResource.GenericHttpRequest<T, V>(
  const AHttpMethod: string; const ARequestEntity: T; AResponseEntity: V);
var
  LRequestStream, LResponseStream: TMemoryStream;
  LResponse: IWiRLResponse;
begin
  if not Assigned(Client) then
    Exit;

  InitHttpRequest;

  LRequestStream := TMemoryStream.Create;
  try
    LResponseStream := TGCMemoryStream.Create;
    try
      ObjectToStream<T>(MergeHeaders(AHttpMethod), ARequestEntity, LRequestStream);

      DoBeforeRequest(AHttpMethod, LRequestStream, LResponse);
      try
        if Assigned(LResponse) then
        begin
          if LResponse.StatusCode >= 400 then
            raise EWiRLClientProtocolException.Create(LResponse);
        end
        else
          LResponse := InternalHttpRequest(AHttpMethod, LRequestStream, LResponseStream);
      except
        on E: EWiRLClientProtocolException do
        begin
          DoRequestError(AHttpMethod, LRequestStream, E.Response);
          raise;
        end;
      end;
      DoAfterRequest(AHttpMethod, LRequestStream, LResponse);

      if Assigned(LResponse.ContentStream) then
        StreamToEntity<V>(AResponseEntity, LResponse.Headers, LResponse.ContentStream)
      else
        StreamToEntity<V>(AResponseEntity, LResponse.Headers, LResponseStream);

    finally
      LResponseStream.Free;
    end;
  finally
    LRequestStream.Free;
  end;
end;

procedure TWiRLClientCustomResource.Get(AResponseEntity: TObject);
begin
  GenericHttpRequest<string, TObject>('GET', '', AResponseEntity);
end;

procedure TWiRLClientCustomResource.Get<T>(AResponseEntity: T);
begin
  GenericHttpRequest<string, T>('GET', '', AResponseEntity);
end;

function TWiRLClientCustomResource.Get<T>: T;
begin
  Result := GenericHttpRequest<string, T>('GET', '');
end;

function TWiRLClientCustomResource.Patch<T, V>(const ARequestEntity: T): V;
begin
  Result := GenericHttpRequest<T, V>('PATCH', ARequestEntity);
end;

procedure TWiRLClientCustomResource.Patch<T>(const ARequestEntity: T;
  AResponseEntity: TObject);
begin
  GenericHttpRequest<T, TObject>('PATCH', ARequestEntity, AResponseEntity);
end;

function TWiRLClientCustomResource.ValueToString(const AValue: TValue): string;
var
  LConfig: TWiRLFormatSettingConfig;
begin
  LConfig := FApplication.GetConfigByClassRef(TWiRLFormatSettingConfig) as TWiRLFormatSettingConfig;
  Result := TWiRLConvert.From(AValue, AValue.TypeInfo, LConfig.GetFormatSettingFor(AValue.TypeInfo));
end;

procedure TWiRLClientCustomResource.PathParam(const AName: string;
  const AValue: TValue);
begin
  PathParams.Values[AName] := ValueToString(AValue);
end;

function TWiRLClientCustomResource.Post<T, V>(const ARequestEntity: T): V;
begin
  Result := GenericHttpRequest<T, V>('POST', ARequestEntity);
end;

procedure TWiRLClientCustomResource.Post<T>(const ARequestEntity: T;
  AResponseEntity: TObject);
begin
  GenericHttpRequest<T, TObject>('POST', ARequestEntity, AResponseEntity);
end;

function TWiRLClientCustomResource.Put<T, V>(const ARequestEntity: T): V;
begin
  Result := GenericHttpRequest<T, V>('PUT', ARequestEntity);
end;

procedure TWiRLClientCustomResource.Put<T>(const ARequestEntity: T;
  AResponseEntity: TObject);
begin
  GenericHttpRequest<T, TObject>('PUT', ARequestEntity, AResponseEntity);
end;

procedure TWiRLClientCustomResource.QueryParam(const AName: string;
  const AValue: TValue);
begin
  QueryParams.Values[AName] := ValueToString(AValue);
end;

destructor TWiRLClientCustomResource.Destroy;
begin
  if Assigned(FApplication) then
    FApplication.Resources.Extract(Self);

  FPathParams.Free;
  FQueryParams.Free;
  FContext.Free;
  inherited;
end;

procedure TWiRLClientCustomResource.DoAfterRequest(const AHttpMethod: string;
  ARequestStream: TStream; AResponse: IWiRLResponse);
var
  LRequestPosition: Integer;
  LResponsePosition: Integer;
begin
  // Call filters
  LRequestPosition := 0;
  LResponsePosition := 0;
  if Assigned(ARequestStream) then
    LRequestPosition := ARequestStream.Position;
  if Assigned(AResponse.ContentStream) then
    LResponsePosition := AResponse.ContentStream.Position;

  FApplication.ApplyResponseFilter(Self, AHttpMethod, ARequestStream, AResponse);

  if Assigned(ARequestStream) then
    ARequestStream.Position := LRequestPosition;
  if Assigned(AResponse.ContentStream) then
    AResponse.ContentStream.Position := LResponsePosition;

  // Manager event handlers

  if Assigned(FAfterRequest) then
  begin
    FAfterRequest(Self, AHttpMethod, ARequestStream, AResponse);
    if Assigned(ARequestStream) then
      ARequestStream.Position := LRequestPosition;
    if Assigned(AResponse.ContentStream) then
      AResponse.ContentStream.Position := LResponsePosition;
  end;
end;

procedure TWiRLClientCustomResource.DoBeforeRequest(const AHttpMethod: string;
  ARequestStream: TStream; out AResponse: IWiRLResponse);
var
  LPosition: Integer;
begin
  // Call filters
  LPosition := 0;
  if Assigned(ARequestStream) then
    LPosition := ARequestStream.Position;
  FApplication.ApplyRequestFilter(Self, AHttpMethod, ARequestStream, AResponse);
  if Assigned(ARequestStream) then
    ARequestStream.Position := LPosition;

  // Manage event handlers

  if Assigned(FBeforeRequest) then
  begin
    FBeforeRequest(Self, AHttpMethod, ARequestStream, AResponse);
    if Assigned(ARequestStream) then
      ARequestStream.Position := LPosition;
  end;
end;

procedure TWiRLClientCustomResource.DoRequestError(const AHttpMethod: string;
  ARequestStream: TStream; AResponse: IWiRLResponse);
var
  LRequestPosition: Integer;
  LResponsePosition: Integer;
begin
  LRequestPosition := 0;
  LResponsePosition := 0;
  if Assigned(FAfterRequest) then
  begin
    if Assigned(ARequestStream) then
      LRequestPosition := ARequestStream.Position;
    if Assigned(AResponse.ContentStream) then
      LResponsePosition := AResponse.ContentStream.Position;
    FOnRequestError(Self, AHttpMethod, ARequestStream, AResponse);
    if Assigned(ARequestStream) then
      ARequestStream.Position := LRequestPosition;
    if Assigned(AResponse.ContentStream) then
      AResponse.ContentStream.Position := LResponsePosition;
  end;
end;

function TWiRLClientCustomResource.GetAccept: string;
begin
  Result := FHeaders.Accept;
  if (Result = '') and Assigned(Application) then
    Result := Application.DefaultMediaType;
end;

function TWiRLClientCustomResource.GetApplication: TWiRLClientApplication;
begin
  Result := FApplication;
end;

function TWiRLClientCustomResource.SameObject<T>(AGeneric: T;
  AObject: TObject): Boolean;
var
  LValue: TValue;
begin
  Result := False;
  if not Assigned(AObject) then
    Exit;

  LValue := TValue.From<T>(AGeneric);
  if LValue.IsEmpty then
    Exit;

  if LValue.IsObject and (LValue.AsObject = AObject) then
    Result := True;
end;

procedure TWiRLClientCustomResource.SetApplication(
  const Value: TWiRLClientApplication);
begin
  if FApplication <> Value then
  begin
    if Assigned(FApplication) then
      FApplication.Resources.Remove(Self);

    FApplication := Value;
    if Assigned(FApplication) and (FApplication.Resources.IndexOf(Self) < 0) then
      FApplication.Resources.Add(Self);
  end;
end;

procedure TWiRLClientCustomResource.SetFilters(const AFilters: TStringDynArray);
begin
  FFilters := AFilters;
end;

procedure TWiRLClientCustomResource.SetParentComponent(AParent: TComponent);
begin
  inherited;
  if AParent is TWiRLClientApplication then
    Application := AParent as TWiRLClientApplication;
end;

procedure TWiRLClientCustomResource.SetPathParams(const Value: TStrings);
begin
  FPathParams.Assign(Value);
end;

procedure TWiRLClientCustomResource.SetQueryParams(const Value: TStrings);
begin
  FQueryParams.Assign(Value);
end;

{ TWiRLResourceHeaders }

constructor TWiRLResourceHeaders.Create(
  ACustomResource: TWiRLClientCustomResource);
begin
  inherited Create;
  FCustomResource := ACustomResource;
end;

procedure TWiRLResourceHeaders.SetValue(const AName, AValue: string);
begin
  if Assigned(FCustomResource.Application) then
  begin
    if SameText(AName, TWiRLHeader.ACCEPT) and SameText(AValue, FCustomResource.Application.DefaultMediaType) then
      Exit;
    if SameText(AName, TWiRLHeader.CONTENT_TYPE) and SameText(AValue, FCustomResource.Application.DefaultMediaType) then
      Exit;
  end;
  inherited SetValue(AName, AValue);
end;

initialization

RegisterHttpMethodImplementations;

end.
