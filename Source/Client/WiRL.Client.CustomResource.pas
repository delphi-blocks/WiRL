{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Client.CustomResource;

{$I ..\Core\WiRL.inc}

interface

uses
  System.SysUtils, System.Classes, System.Rtti, System.Contnrs, System.Types,
  System.Generics.Collections,
  WiRL.Core.Context,
  WiRL.Client.Application,
  WiRL.http.Core,
  WiRL.http.Headers,
  WiRL.http.Client.Interfaces,
  WiRL.http.Client;

type
  TBeforeRequestEvent = procedure (Sender: TObject; const AHttpMethod: string; ARequestStream: TStream) of object;
  TAfterRequestEvent = procedure (Sender: TObject; const AHttpMethod: string; ARequestStream: TStream; AResponse: IWiRLResponse) of object;

  {$IFDEF HAS_NEW_PIDS}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator32 or pidiOSDevice32 or pidAndroid32Arm)]
  {$ELSE}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  {$ENDIF}
  TWiRLClientCustomResource = class(TComponent)
  private
    FContext: TWiRLContextHttp;
    FResource: string;
    FApplication: TWiRLClientApplication;
    FSpecificClient: TWiRLClient;
    FPathParams: TStrings;
    FQueryParams: TStrings;
    FHeaders: IWiRLHeaders;
    FBeforeRequest: TBeforeRequestEvent;
    FAfterRequest: TAfterRequestEvent;
    procedure SetPathParams(const Value: TStrings);
    procedure SetQueryParams(const Value: TStrings);

    procedure ContextInjection(AInstance: TObject);
    function MergeHeaders(const AHttpMethod: string): IWiRLHeaders;
    function StreamToObject<T>(AHeaders: IWiRLHeaders; AStream: TStream): T; overload;
    procedure StreamToObject(AObject: TObject; AHeaders: IWiRLHeaders; AStream: TStream); overload;
    procedure ObjectToStream<T>(AHeaders: IWiRLHeaders; AObject: T; AStream: TStream); overload;
    function SameObject<T>(AGeneric: T; AObject: TObject): Boolean;
    procedure SetApplication(const Value: TWiRLClientApplication);
  protected
    function GetClient: TWiRLClient; virtual;
    function GetPath: string; virtual;
    function GetURL: string; virtual;
    function GetApplication: TWiRLClientApplication; virtual;
    function GetAccept: string;
    function GetContentType: string;
    procedure DoBeforeRequest(const AHttpMethod: string; ARequestStream: TStream);
    procedure DoAfterRequest(const AHttpMethod: string; ARequestStream: TStream; AResponse: IWiRLResponse);

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    // Handles the parent/child relationship for the designer
    procedure SetParentComponent(AParent: TComponent); override;

    procedure BeforeGET; virtual;
    procedure AfterGET(AResponse: IWiRLResponse); virtual;

    procedure BeforePOST(AContent: TMemoryStream); virtual;
    procedure AfterPOST(AResponse: IWiRLResponse); virtual;

    procedure BeforePUT(AContent: TMemoryStream); virtual;
    procedure AfterPUT(AResponse: IWiRLResponse); virtual;

    procedure BeforePATCH(AContent: TMemoryStream); virtual;
    procedure AfterPATCH(AResponse: IWiRLResponse); virtual;

    procedure BeforeHEAD; virtual;
    procedure AfterHEAD; virtual;

    procedure BeforeDELETE; virtual;
    procedure AfterDELETE(AResponse: IWiRLResponse); virtual;

    procedure BeforeOPTIONS; virtual;
    procedure AfterOPTIONS(AResponse: IWiRLResponse); virtual;

    procedure InitHttpRequest; virtual;
    function InternalHttpRequest(const AHttpMethod: string; ARequestStream, AResponseStream: TStream): IWiRLResponse; virtual;

    procedure DefineProperties(Filer: TFiler); override;
    procedure LoadHeadersProperty(Reader: TReader);
    procedure StoreHeadersProperty(Writer: TWriter);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Handles the parent/child relationship for the designer
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;

    // http verbs
    function Get<T>: T; overload;
    procedure Get(AResponseEntity: TObject); overload;
    function Post<T, V>(const ARequestEntity: T): V; overload;
    procedure Post<T>(const ARequestEntity: T; AResponseEntity: TObject); overload;
    function Put<T, V>(const ARequestEntity: T): V; overload;
    procedure Put<T>(const ARequestEntity: T; AResponseEntity: TObject); overload;
    function Delete<T>: T; overload;
    procedure Delete(AResponseEntity: TObject); overload;
    function Patch<T, V>(const ARequestEntity: T): V; overload;
    procedure Patch<T>(const ARequestEntity: T; AResponseEntity: TObject); overload;

    function GenericHttpRequest<T, V>(const AHttpMethod: string; const ARequestEntity: T): V; overload;
    procedure GenericHttpRequest<T>(const AHttpMethod: string; const ARequestEntity: T; AResponseEntity: TObject); overload;

  public
    property Accept: string read GetAccept;
    property ContentType: string read GetContentType;
    property Application: TWiRLClientApplication read GetApplication write SetApplication;
    property Client: TWiRLClient read GetClient;
    property SpecificClient: TWiRLClient read FSpecificClient write FSpecificClient;
    property Resource: string read FResource write FResource;
    property Path: string read GetPath;
    property PathParams: TStrings read FPathParams write SetPathParams;
    property QueryParams: TStrings read FQueryParams write SetQueryParams;
    property URL: string read GetURL;
    property Headers: IWiRLHeaders read FHeaders write FHeaders;
    property AfterRequest: TAfterRequestEvent read FAfterRequest write FAfterRequest;
    property BeforeRequest: TBeforeRequestEvent read FBeforeRequest write FBeforeRequest;
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
  WiRL.http.Accept.MediaType,
  WiRL.Core.Classes,
  WiRL.Core.Injection,
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

procedure TWiRLClientCustomResource.AfterDELETE(AResponse: IWiRLResponse);
begin

end;

procedure TWiRLClientCustomResource.AfterGET(AResponse: IWiRLResponse);
begin

end;

procedure TWiRLClientCustomResource.AfterHEAD;
begin

end;

procedure TWiRLClientCustomResource.AfterOPTIONS(AResponse: IWiRLResponse);
begin

end;

procedure TWiRLClientCustomResource.AfterPATCH(AResponse: IWiRLResponse);
begin

end;

procedure TWiRLClientCustomResource.AfterPOST(AResponse: IWiRLResponse);
begin

end;

procedure TWiRLClientCustomResource.AfterPUT(AResponse: IWiRLResponse);
begin

end;

procedure TWiRLClientCustomResource.BeforeDELETE;
begin

end;

procedure TWiRLClientCustomResource.BeforeGET;
begin

end;

procedure TWiRLClientCustomResource.BeforeHEAD;
begin

end;

procedure TWiRLClientCustomResource.BeforeOPTIONS;
begin

end;

procedure TWiRLClientCustomResource.BeforePATCH(AContent: TMemoryStream);
begin

end;

procedure TWiRLClientCustomResource.BeforePOST(AContent: TMemoryStream);
begin

end;

procedure TWiRLClientCustomResource.BeforePUT(AContent: TMemoryStream);
begin

end;

procedure TWiRLClientCustomResource.ContextInjection(AInstance: TObject);
begin
  TWiRLContextInjectionRegistry.Instance.
    ContextInjection(AInstance, FContext);
end;

constructor TWiRLClientCustomResource.Create(AOwner: TComponent);
begin
  inherited;
  FResource := 'main';
  if TWiRLComponentHelper.IsDesigning(Self) then
    Application := TWiRLComponentHelper.FindDefault<TWiRLClientApplication>(Self);
  FPathParams := TStringList.Create;
  FQueryParams := TStringList.Create;
  FContext := TWiRLContextHttp.Create;
  FHeaders := TWiRLResourceHeaders.Create(Self);
end;

function TWiRLClientCustomResource.GetClient: TWiRLClient;
begin
  Result := nil;
  if Assigned(FSpecificClient) then
    Result := FSpecificClient
  else
  begin
    if Assigned(FApplication) then
      Result := FApplication.Client;
  end;
end;

function TWiRLClientCustomResource.GetContentType: string;
begin
  Result := Headers.ContentType;
  if (Result = '') and Assigned(Application) then
    Result := Application.DefaultMediaType;
end;

function TWiRLClientCustomResource.GetParentComponent: TComponent;
begin
  Result := FApplication;
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
    if LHeaderPair.Length > 1 then
    begin
      FHeaders.AddHeader(TWiRLHeader.Create(LHeaderPair[0], LHeaderPair[1]));
    end;
  end;
  Reader.ReadListEnd;
end;

function TWiRLClientCustomResource.MergeHeaders(const AHttpMethod: string): IWiRLHeaders;

  function HasResponseBody(const AHttpMethod: string): Boolean;
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
  if HasResponseBody(AHttpMethod) and (ContentType <> '') then
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
  GenericHttpRequest<string>('DELETE', '', AResponseEntity);
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
  LNoProtocolErrorException: Boolean;
begin
  if not Assigned(Client) then
    Exit;

  Result := default(V);
  InitHttpRequest;

  LNoProtocolErrorException := Client.NoProtocolErrorException;
  Client.NoProtocolErrorException := True;
  LRequestStream := TMemoryStream.Create;
  try
    LResponseStream := TGCMemoryStream.Create;
    try
      ObjectToStream<T>(MergeHeaders(AHttpMethod), ARequestEntity, LRequestStream);

      DoBeforeRequest(AHttpMethod, LRequestStream);
      LResponse := InternalHttpRequest(AHttpMethod, LRequestStream, LResponseStream);
      DoAfterRequest(AHttpMethod, LRequestStream, LResponse);

      Result := StreamToObject<V>(LResponse.Headers, LResponseStream);
    finally
      if not SameObject<V>(Result, LResponseStream) then
        LResponseStream.Free;
    end;
  finally
    LRequestStream.Free;
    Client.NoProtocolErrorException := LNoProtocolErrorException;
  end;
end;

procedure TWiRLClientCustomResource.GenericHttpRequest<T>(
  const AHttpMethod: string; const ARequestEntity: T; AResponseEntity: TObject);
var
  LRequestStream, LResponseStream: TMemoryStream;
  LResponse: IWiRLResponse;
  LNoProtocolErrorException: Boolean;
begin
  if not Assigned(Client) then
    Exit;

  InitHttpRequest;

  LNoProtocolErrorException := Client.NoProtocolErrorException;
  Client.NoProtocolErrorException := True;
  LRequestStream := TMemoryStream.Create;
  try
    LResponseStream := TGCMemoryStream.Create;
    try
      ObjectToStream<T>(MergeHeaders(AHttpMethod), ARequestEntity, LRequestStream);

      DoBeforeRequest(AHttpMethod, LRequestStream);
      LResponse := InternalHttpRequest(AHttpMethod, LRequestStream, LResponseStream);
      DoAfterRequest(AHttpMethod, LRequestStream, LResponse);

      if Assigned(AResponseEntity) then
        StreamToObject(AResponseEntity, LResponse.Headers, LResponseStream);
    finally
      LResponseStream.Free;
    end;
  finally
    LRequestStream.Free;
    Client.NoProtocolErrorException := LNoProtocolErrorException;
  end;
end;

procedure TWiRLClientCustomResource.Get(AResponseEntity: TObject);
begin
  GenericHttpRequest<string>('GET', '', AResponseEntity);
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
  GenericHttpRequest<T>('PATCH', ARequestEntity, AResponseEntity);
end;

function TWiRLClientCustomResource.Post<T, V>(const ARequestEntity: T): V;
begin
  Result := GenericHttpRequest<T, V>('POST', ARequestEntity);
end;

procedure TWiRLClientCustomResource.Post<T>(const ARequestEntity: T;
  AResponseEntity: TObject);
begin
  GenericHttpRequest<T>('POST', ARequestEntity, AResponseEntity);
end;

function TWiRLClientCustomResource.Put<T, V>(const ARequestEntity: T): V;
begin
  Result := GenericHttpRequest<T, V>('PUT', ARequestEntity);
end;

procedure TWiRLClientCustomResource.Put<T>(const ARequestEntity: T;
  AResponseEntity: TObject);
begin
  GenericHttpRequest<T>('PUT', ARequestEntity, AResponseEntity);
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
  LRequestPosition := 0;
  LResponsePosition := 0;
  if Assigned(FAfterRequest) then
  begin
    if Assigned(ARequestStream) then
      LRequestPosition := ARequestStream.Position;
    if Assigned(AResponse.ContentStream) then
      LResponsePosition := AResponse.ContentStream.Position;
    FAfterRequest(Self, AHttpMethod, ARequestStream, AResponse);
    if Assigned(ARequestStream) then
      ARequestStream.Position := LRequestPosition;
    if Assigned(AResponse.ContentStream) then
      AResponse.ContentStream.Position := LResponsePosition;
  end;
end;

procedure TWiRLClientCustomResource.DoBeforeRequest(const AHttpMethod: string;
  ARequestStream: TStream);
var
  LPosition: Integer;
begin
  LPosition := 0;
  if Assigned(FBeforeRequest) then
  begin
    if Assigned(ARequestStream) then
      LPosition := ARequestStream.Position;
    FBeforeRequest(Self, AHttpMethod, ARequestStream);
    if Assigned(ARequestStream) then
      ARequestStream.Position := LPosition;
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

procedure TWiRLClientCustomResource.SetParentComponent(AParent: TComponent);
begin
  inherited;
  if AParent is TWiRLClientApplication then
    FApplication := AParent as TWiRLClientApplication;
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
