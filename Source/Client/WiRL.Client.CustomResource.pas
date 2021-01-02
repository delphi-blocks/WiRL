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
  System.SysUtils, System.Classes, System.Rtti, System.Generics.Collections,
  WiRL.Core.Context,
  WiRL.Client.Application,
  WiRL.http.Core,
  WiRL.http.Response,
  WiRL.http.Request,
  WiRL.http.Client.Interfaces,
  WiRL.http.Client;

type
  TWiRLClientProc = TProc;
  TWiRLClientResponseProc = TProc<TStream>;
  TWiRLClientExceptionProc = TProc<Exception>;

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
    FPathParamsValues: TStrings;
    FQueryParams: TStrings;
    FSpecificAccept: string;
    FSpecificContentType: string;
    FHeaderFields: TWiRLHeaderList;
    procedure SetPathParamsValues(const Value: TStrings);
    procedure SetQueryParams(const Value: TStrings);

    procedure ContextInjection(AInstance: TObject);
    function CustomHeaders: TWiRLHeaders;
    function StreamToObject<T>(AResponse: TWiRLResponse; AStream: TStream): T; overload;
    procedure StreamToObject(AObject: TObject; AResponse: TWiRLResponse; AStream: TStream); overload;
    procedure ObjectToStream<T>(ARequest: TWiRLRequest; AObject: T; AStream: TStream);
    function SameObject<T>(AGeneric: T; AObject: TObject): Boolean;
  protected
    function GetClient: TWiRLClient; virtual;
    function GetPath: string; virtual;
    function GetURL: string; virtual;
    function GetApplication: TWiRLClientApplication; virtual;
    function GetAccept: string;
    function GetContentType: string;

    procedure BeforeGET; virtual;
    procedure AfterGET; virtual;

    procedure BeforePOST(AContent: TMemoryStream); virtual;
    procedure AfterPOST; virtual;

    procedure BeforePUT(AContent: TMemoryStream); virtual;
    procedure AfterPUT; virtual;

    procedure BeforePATCH(AContent: TMemoryStream); virtual;
    procedure AfterPATCH; virtual;

    procedure BeforeHEAD; virtual;
    procedure AfterHEAD; virtual;

    procedure BeforeDELETE; virtual;
    procedure AfterDELETE; virtual;

    procedure BeforeOPTIONS; virtual;
    procedure AfterOPTIONS; virtual;

    procedure InitHttpRequest; virtual;
    procedure InternalHttpRequest(const AHttpMethod: string; ARequestStream, AResponseStream: TStream; ACustomHeaders: TWiRLHeaders); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GenericGet<T>: T; overload;
    procedure GenericGet(AResponseEntity: TObject); overload;
    function GenericPost<T, V>(const ARequestEntity: T): V; overload;
    procedure GenericPost<T>(const ARequestEntity: T; AResponseEntity: TObject); overload;
    function GenericPut<T, V>(const ARequestEntity: T): V; overload;
    procedure GenericPut<T>(const ARequestEntity: T; AResponseEntity: TObject); overload;
    function GenericDelete<T>: T; overload;
    procedure GenericDelete(AResponseEntity: TObject); overload;
    function GenericPatch<T, V>(const ARequestEntity: T): V; overload;
    procedure GenericPatch<T>(const ARequestEntity: T; AResponseEntity: TObject); overload;

    function GenericHttpRequest<T, V>(const AHttpMethod: string; const ARequestEntity: T): V; overload;
    procedure GenericHttpRequest<T>(const AHttpMethod: string; const ARequestEntity: T; AResponseEntity: TObject); overload;

    // http verbs
    procedure GET(const ABeforeExecute: TWiRLClientProc = nil;
      const AAfterExecute: TWiRLClientResponseProc = nil;
      const AOnException: TWiRLClientExceptionProc = nil); overload;

    function GETAsString(AEncoding: TEncoding = nil;
      const ABeforeExecute: TWiRLClientProc = nil;
      const AOnException: TWiRLClientExceptionProc = nil): string; overload;

    procedure GETAsync(const ACompletionHandler: TWiRLClientProc = nil;
      const AOnException: TWiRLClientExceptionProc = nil;
      ASynchronize: Boolean = True);

    procedure POST(const ABeforeExecute: TProc<TMemoryStream> = nil;
      const AAfterExecute: TWiRLClientResponseProc = nil;
      const AOnException: TWiRLClientExceptionProc = nil); overload;

    procedure POSTAsync(const ACompletionHandler: TWiRLClientProc = nil;
      const AOnException: TWiRLClientExceptionProc = nil;
      ASynchronize: Boolean = True);

    procedure PUT(const ABeforeExecute: TProc<TMemoryStream> = nil;
      const AAfterExecute: TWiRLClientResponseProc = nil;
      const AOnException: TWiRLClientExceptionProc = nil);

    procedure DELETE(const ABeforeExecute: TWiRLClientProc = nil;
      const AAfterExecute: TWiRLClientProc = nil;
      const AOnException: TWiRLClientExceptionProc = nil);

    procedure PATCH(const ABeforeExecute: TProc<TMemoryStream> = nil;
      const AAfterExecute: TWiRLClientResponseProc = nil;
      const AOnException: TWiRLClientExceptionProc = nil);

    procedure HEAD(const ABeforeExecute: TWiRLClientProc = nil;
      const AAfterExecute: TWiRLClientProc = nil;
      const AOnException: TWiRLClientExceptionProc = nil);

    procedure OPTIONS(const ABeforeExecute: TWiRLClientProc = nil;
      const AAfterExecute: TWiRLClientResponseProc = nil;
      const AOnException: TWiRLClientExceptionProc = nil);
  public
    property Accept: string read GetAccept;
    property ContentType: string read GetContentType;
    property Application: TWiRLClientApplication read GetApplication write FApplication;
    property Client: TWiRLClient read GetClient;
    property SpecificAccept: string read FSpecificAccept write FSpecificAccept;
    property SpecificContentType: string read FSpecificContentType write FSpecificContentType;
    property SpecificClient: TWiRLClient read FSpecificClient write FSpecificClient;
    property Resource: string read FResource write FResource;
    property Path: string read GetPath;
    property PathParamsValues: TStrings read FPathParamsValues write SetPathParamsValues;
    property QueryParams: TStrings read FQueryParams write SetQueryParams;
    property URL: string read GetURL;
    property HeaderFields: TWiRLHeaderList read FHeaderFields;
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
  THttpMethodImplementation = reference to procedure (
    AResource: TWiRLClientCustomResource;
    ARequestStream, AResponseStream: TStream;
    ACustomHeaders: TWiRLHeaders);

  THttpMethodImplementations = array [TWiRLHttpMethod] of THttpMethodImplementation;

var
  HttpMethodImplementations: THttpMethodImplementations;

procedure RegisterHttpMethodImplementations;
begin
  HttpMethodImplementations[TWiRLHttpMethod.GET] :=
    procedure (AResource: TWiRLClientCustomResource; ARequestStream, AResponseStream: TStream; ACustomHeaders: TWiRLHeaders)
    begin
      AResource.Client.Get(AResource.URL, AResponseStream, ACustomHeaders);
    end;

  HttpMethodImplementations[TWiRLHttpMethod.POST] :=
    procedure (AResource: TWiRLClientCustomResource; ARequestStream, AResponseStream: TStream; ACustomHeaders: TWiRLHeaders)
    begin
      AResource.Client.Post(AResource.URL, ARequestStream, AResponseStream, ACustomHeaders);
    end;

  HttpMethodImplementations[TWiRLHttpMethod.PUT] :=
    procedure (AResource: TWiRLClientCustomResource; ARequestStream, AResponseStream: TStream; ACustomHeaders: TWiRLHeaders)
    begin
      AResource.Client.Put(AResource.URL, ARequestStream, AResponseStream, ACustomHeaders);
    end;

  HttpMethodImplementations[TWiRLHttpMethod.DELETE] :=
    procedure (AResource: TWiRLClientCustomResource; ARequestStream, AResponseStream: TStream; ACustomHeaders: TWiRLHeaders)
    begin
      AResource.Client.Delete(AResource.URL, AResponseStream, ACustomHeaders);
    end;

  HttpMethodImplementations[TWiRLHttpMethod.PATCH] :=
    procedure (AResource: TWiRLClientCustomResource; ARequestStream, AResponseStream: TStream; ACustomHeaders: TWiRLHeaders)
    begin
      AResource.Client.Patch(AResource.URL, ARequestStream, AResponseStream, ACustomHeaders);
    end;

end;

{ TWiRLClientCustomResource }

procedure TWiRLClientCustomResource.AfterDELETE;
begin

end;

procedure TWiRLClientCustomResource.AfterGET;
begin

end;

procedure TWiRLClientCustomResource.AfterHEAD;
begin

end;

procedure TWiRLClientCustomResource.AfterOPTIONS;
begin

end;

procedure TWiRLClientCustomResource.AfterPATCH;
begin

end;

procedure TWiRLClientCustomResource.AfterPOST;
begin

end;

procedure TWiRLClientCustomResource.AfterPUT;
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
    FApplication := TWiRLComponentHelper.FindDefault<TWiRLClientApplication>(Self);
  FPathParamsValues := TStringList.Create;
  FQueryParams := TStringList.Create;
  FContext := TWiRLContextHttp.Create;
  FHeaderFields := TWiRLHeaderList.Create;
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
  Result := FSpecificContentType;
  if (Result = '') and Assigned(Application) then
    Result := Application.DefaultMediaType;
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
begin
  Result := TWiRLURL.CombinePath([
    Path,
    TWiRLURL.CombinePath(TWiRLURL.URLEncode(FPathParamsValues.ToStringArray))
  ]);

  if FQueryParams.Count > 0 then
    Result := Result + '?' + SmartConcat(TWiRLURL.URLEncode(FQueryParams.ToStringArray), '&');
end;

procedure TWiRLClientCustomResource.HEAD(const ABeforeExecute,
  AAfterExecute: TWiRLClientProc; const AOnException: TWiRLClientExceptionProc);
begin
  try
    BeforeHEAD;

    if Assigned(ABeforeExecute) then
      ABeforeExecute();

    Client.Request.Accept := Accept;
    Client.Request.ContentType := ContentType;
    Client.Head(URL);

    AfterHEAD();

    if Assigned(AAfterExecute) then
      AAfterExecute();
  except
    on E: Exception do
    begin
      if Assigned(AOnException) then
        AOnException(E)
      else
        raise;
    end;
  end;
end;

procedure TWiRLClientCustomResource.InitHttpRequest;
var
  LPair: TPair<TWiRLConfigurationClass, TWiRLConfiguration>;
begin
  // Fill the context
  FContext.AddContainerOnce(Client.Request, False);
  FContext.AddContainerOnce(Client.Response, False);
  FContext.AddContainerOnce(FApplication, False);

  for LPair in FApplication.Configs do
    FContext.AddContainerOnce(LPair.Value, False);
end;


procedure TWiRLClientCustomResource.InternalHttpRequest(
  const AHttpMethod: string; ARequestStream, AResponseStream: TStream;
  ACustomHeaders: TWiRLHeaders);
var
  LHttpMethodImplementation: THttpMethodImplementation;
begin
  LHttpMethodImplementation := HttpMethodImplementations[TWiRLHttpMethod.ConvertFromString(AHttpMethod)];
  if not Assigned(LHttpMethodImplementation) then
    raise EWiRLClientException.CreateFmt('Implementation not found for method [%s]', [AHttpMethod]);

  LHttpMethodImplementation(Self, ARequestStream, AResponseStream, ACustomHeaders);
end;

function TWiRLClientCustomResource.CustomHeaders: TWiRLHeaders;
var
  I: Integer;
begin
  Result := [];
  if Accept <> '' then
    Result := Result + [TWiRLHeader.Create('Accept', Accept)];
  if ContentType <> '' then
    Result := Result + [TWiRLHeader.Create('Content-Type', ContentType)];

  for I := 0 to FHeaderFields.Count - 1 do
  begin
    Result := Result + [TWiRLHeader.Create(FHeaderFields.Names[I], FHeaderFields.ValueFromIndex[I])];
  end;
end;

function TWiRLClientCustomResource.StreamToObject<T>(AResponse: TWiRLResponse; AStream: TStream): T;
var
  LType: TRttiType;
  LContext: TRttiContext;
  LMediaType: TMediaType;
  LReader: IMessageBodyReader;
  LValue: TValue;
begin
  LType := LContext.GetType(TypeInfo(T));
  LMediaType := TMediaType.Create(AResponse.ContentType);
  try
    LReader := Application.ReaderRegistry.FindReader(LType, LMediaType);
    if not Assigned(LReader) then
      raise EWiRLClientException.CreateFmt('Reader not found for [%s] content type: [%s]', [LType.Name, LMediaType.MediaType]);
    ContextInjection(LReader as TObject);

    LValue := LReader.ReadFrom(LType, LMediaType, AResponse.HeaderFields, AResponse.ContentStream);
    Result := LValue.AsType<T>;
  finally
    LMediaType.Free;
  end;
end;

procedure TWiRLClientCustomResource.ObjectToStream<T>(ARequest: TWiRLRequest;
  AObject: T; AStream: TStream);
var
  LType: TRttiType;
  LContext: TRttiContext;
  LMediaType: TMediaType;
  LWriter: IMessageBodyWriter;
  LValue: TValue;
begin
  LType := LContext.GetType(TypeInfo(T));
  LMediaType := TMediaType.Create(ContentType);
  try
    LValue := TValue.From<T>(AObject);
    LWriter := Application.WriterRegistry.FindWriter(LType, LMediaType);
    if not Assigned(LWriter) then
      raise EWiRLClientException.CreateFmt('Writer not found for [%s] content type: [%s]', [LType.Name, LMediaType.MediaType]);

    ContextInjection(LWriter as TObject);
    LWriter.WriteTo(LValue, nil, LMediaType, ARequest.HeaderFields, AStream);
    AStream.Position := soFromBeginning;

  finally
    LMediaType.Free;
  end;
end;

procedure TWiRLClientCustomResource.GenericDelete(AResponseEntity: TObject);
begin
  GenericHttpRequest<string>('DELETE', '', AResponseEntity);
end;

function TWiRLClientCustomResource.GenericDelete<T>: T;
begin
  Result := GenericHttpRequest<string, T>('DELETE', '');
end;

function TWiRLClientCustomResource.GenericHttpRequest<T, V>(
  const AHttpMethod: string; const ARequestEntity: T): V;
var
  LRequestStream, LResponseStream: TMemoryStream;
begin
  Result := default(V);
  InitHttpRequest;

  LRequestStream := TMemoryStream.Create;
  try
    LResponseStream := TGCMemoryStream.Create;
    try
      ObjectToStream<T>(Client.Request, ARequestEntity, LRequestStream);
      InternalHttpRequest(AHttpMethod, LRequestStream, LResponseStream, CustomHeaders);
      Result := StreamToObject<V>(Client.Response, LResponseStream);
    finally
      if not SameObject<V>(Result, LResponseStream) then
        LResponseStream.Free;
    end;
  finally
    LRequestStream.Free;
  end;
end;

procedure TWiRLClientCustomResource.GenericHttpRequest<T>(
  const AHttpMethod: string; const ARequestEntity: T; AResponseEntity: TObject);
var
  LRequestStream, LResponseStream: TMemoryStream;
begin
  InitHttpRequest;

  LRequestStream := TMemoryStream.Create;
  try
    LResponseStream := TGCMemoryStream.Create;
    try
      ObjectToStream<T>(Client.Request, ARequestEntity, LRequestStream);
      InternalHttpRequest(AHttpMethod, LRequestStream, LResponseStream, CustomHeaders);
      StreamToObject(AResponseEntity, Client.Response, LResponseStream);
    finally
      LResponseStream.Free;
    end;
  finally
    LRequestStream.Free;
  end;
end;

procedure TWiRLClientCustomResource.GenericGet(AResponseEntity: TObject);
begin
  GenericHttpRequest<string>('GET', '', AResponseEntity);
end;

function TWiRLClientCustomResource.GenericGet<T>: T;
begin
  Result := GenericHttpRequest<string, T>('GET', '');
end;

function TWiRLClientCustomResource.GenericPatch<T, V>(const ARequestEntity: T): V;
begin
  Result := GenericHttpRequest<T, V>('PATCH', ARequestEntity);
end;

procedure TWiRLClientCustomResource.GenericPatch<T>(const ARequestEntity: T;
  AResponseEntity: TObject);
begin
  GenericHttpRequest<T>('PATCH', ARequestEntity, AResponseEntity);
end;

function TWiRLClientCustomResource.GenericPost<T, V>(const ARequestEntity: T): V;
begin
  Result := GenericHttpRequest<T, V>('POST', ARequestEntity);
end;

procedure TWiRLClientCustomResource.GenericPost<T>(const ARequestEntity: T;
  AResponseEntity: TObject);
begin
  GenericHttpRequest<T>('POST', ARequestEntity, AResponseEntity);
end;

function TWiRLClientCustomResource.GenericPut<T, V>(const ARequestEntity: T): V;
begin
  Result := GenericHttpRequest<T, V>('PUT', ARequestEntity);
end;

procedure TWiRLClientCustomResource.GenericPut<T>(const ARequestEntity: T;
  AResponseEntity: TObject);
begin
  GenericHttpRequest<T>('PUT', ARequestEntity, AResponseEntity);
end;

procedure TWiRLClientCustomResource.OPTIONS(const ABeforeExecute: TWiRLClientProc = nil;
  const AAfterExecute: TWiRLClientResponseProc = nil;
  const AOnException: TWiRLClientExceptionProc = nil);
var
  LResponseStream: TMemoryStream;
begin
  LResponseStream := TMemoryStream.Create;
  try
    try
      BeforeOPTIONS();

      if Assigned(ABeforeExecute) then
        ABeforeExecute();

      Client.Request.Accept := Accept;
      Client.Request.ContentType := ContentType;
      Client.Options(URL, LResponseStream);

      AfterOPTIONS();

      if Assigned(AAfterExecute) then
        AAfterExecute(LResponseStream);
    except
      on E: Exception do
      begin
        if Assigned(AOnException) then
          AOnException(E)
        else
          raise;
      end;
    end;
  finally
    LResponseStream.Free;
  end;
end;

procedure TWiRLClientCustomResource.DELETE(const ABeforeExecute,
  AAfterExecute: TWiRLClientProc; const AOnException: TWiRLClientExceptionProc);
var
  LResponseStream: TMemoryStream;
begin
  LResponseStream := TMemoryStream.Create;
  try
    try
      BeforeDELETE();

      if Assigned(ABeforeExecute) then
        ABeforeExecute();

      Client.Request.Accept := Accept;
      Client.Request.ContentType := ContentType;
      Client.Delete(URL, LResponseStream);

      AfterDELETE();

      if Assigned(AAfterExecute) then
        AAfterExecute();
    except
      on E: Exception do
      begin
        if Assigned(AOnException) then
          AOnException(E)
        else
          raise;
      end;
    end;
  finally
    LResponseStream.Free;
  end;
end;

destructor TWiRLClientCustomResource.Destroy;
begin
  FPathParamsValues.Free;
  FQueryParams.Free;
  FContext.Free;
  FHeaderFields.Free;
  inherited;
end;

procedure TWiRLClientCustomResource.GET(const ABeforeExecute: TWiRLCLientProc;
  const AAfterExecute: TWiRLClientResponseProc;
  const AOnException: TWiRLClientExceptionProc);
var
  LResponseStream: TMemoryStream;
begin
  LResponseStream := TMemoryStream.Create;
  try
    try
      BeforeGET();

      if Assigned(ABeforeExecute) then
        ABeforeExecute();

        Client.Request.Accept := Accept;
        Client.Request.ContentType := ContentType;
        Client.Get(URL, LResponseStream);

        AfterGET();

        if Assigned(AAfterExecute) then
          AAfterExecute(LResponseStream);
    except
      on E: Exception do
      begin
        if Assigned(AOnException) then
          AOnException(E)
        else
          raise;
      end;
    end;
  finally
    LResponseStream.Free;
  end;
end;

function TWiRLClientCustomResource.GETAsString(AEncoding: TEncoding;
  const ABeforeExecute: TWiRLClientProc;
  const AOnException: TWiRLClientExceptionProc): string;
var
  LResult: string;
  LEncoding: TEncoding;
begin
  LResult := '';
  LEncoding := AEncoding;
  if not Assigned(LEncoding) then
    LEncoding := TEncoding.Default;

  GET(ABeforeExecute,
    procedure (AResponse: TStream)
    var
      LStreamReader: TStreamReader;
    begin
      AResponse.Position := 0;
      LStreamReader := TStreamReader.Create(AResponse, LEncoding);
      try
        LResult := LStreamReader.ReadToEnd;
      finally
        LStreamReader.Free;
      end;
    end,
    AOnException
  );
  Result := LResult;
end;

function TWiRLClientCustomResource.GetAccept: string;
begin
  Result := FSpecificAccept;
  if (Result = '') and Assigned(Application) then
    Result := Application.DefaultMediaType;
end;

function TWiRLClientCustomResource.GetApplication: TWiRLClientApplication;
begin
  Result := FApplication;
end;

procedure TWiRLClientCustomResource.GETAsync(
  const ACompletionHandler: TWiRLClientProc;
  const AOnException: TWiRLClientExceptionProc;
  ASynchronize: Boolean);
begin
  Client.ExecuteAsync(
    procedure
    begin
      GET(nil, nil, AOnException);
      if Assigned(ACompletionHandler) then
      begin
        if ASynchronize then
          TThread.Queue(nil, TThreadProcedure(ACompletionHandler))
        else
          ACompletionHandler();
      end;
    end
  );
end;

procedure TWiRLClientCustomResource.PATCH(const ABeforeExecute: TProc<TMemoryStream> = nil;
  const AAfterExecute: TWiRLClientResponseProc = nil;
  const AOnException: TWiRLClientExceptionProc = nil);
var
  LResponseStream: TMemoryStream;
  LContent: TMemoryStream;
begin
  LContent := TMemoryStream.Create;
  try
    LResponseStream := TMemoryStream.Create;
    try
      try
        BeforePATCH(LContent);

        if Assigned(ABeforeExecute) then
          ABeforeExecute(LContent);

        Client.Request.Accept := Accept;
        Client.Request.ContentType := ContentType;
        Client.Patch(URL, LContent, LResponseStream);

        AfterPATCH();

        if Assigned(AAfterExecute) then
          AAfterExecute(LResponseStream);
      except
        on E: Exception do
        begin
          if Assigned(AOnException) then
            AOnException(E)
          else
            raise;
        end;
      end;
    finally
      LResponseStream.Free;
    end;
  finally
    LContent.Free;
  end;
end;

procedure TWiRLClientCustomResource.POST(
  const ABeforeExecute: TProc<TMemoryStream>;
  const AAfterExecute: TWiRLClientResponseProc;
  const AOnException: TWiRLClientExceptionProc);
var
  LResponseStream: TMemoryStream;
  LContent: TMemoryStream;
begin
  LContent := TMemoryStream.Create;
  try
    LResponseStream := TMemoryStream.Create;
    try
      try
        BeforePOST(LContent);

        if Assigned(ABeforeExecute) then
          ABeforeExecute(LContent);

        Client.Request.Accept := Accept;
        Client.Request.ContentType := ContentType;
        Client.Post(URL, LContent, LResponseStream);

        AfterPOST();

        if Assigned(AAfterExecute) then
          AAfterExecute(LResponseStream);
      except
        on E: Exception do
        begin
          if Assigned(AOnException) then
            AOnException(E)
          else
            raise;
        end;
      end;
    finally
      LResponseStream.Free;
    end;
  finally
    LContent.Free;
  end;
end;

procedure TWiRLClientCustomResource.POSTAsync(
  const ACompletionHandler: TWiRLClientProc;
  const AOnException: TWiRLClientExceptionProc;
  ASynchronize: Boolean);
begin
  Client.ExecuteAsync(
    procedure
    begin
      POST(nil, nil, AOnException);
      if Assigned(ACompletionHandler) then
      begin
        if ASynchronize then
          TThread.Queue(nil, TThreadProcedure(ACompletionHandler))
        else
          ACompletionHandler();
      end;
    end
  );
end;

procedure TWiRLClientCustomResource.PUT(const ABeforeExecute: TProc<TMemoryStream> = nil;
  const AAfterExecute: TWiRLClientResponseProc = nil;
  const AOnException: TWiRLClientExceptionProc = nil);
var
  LResponseStream: TMemoryStream;
  LContent: TMemoryStream;
begin
  LContent := TMemoryStream.Create;
  try
    LResponseStream := TMemoryStream.Create;
    try
      try
        BeforePUT(LContent);

        if Assigned(ABeforeExecute) then
          ABeforeExecute(LContent);

        Client.Request.Accept := Accept;
        Client.Request.ContentType := ContentType;
        Client.Put(URL, LContent, LResponseStream);

        AfterPUT();

        if Assigned(AAfterExecute) then
          AAfterExecute(LResponseStream);
      except
        on E: Exception do
        begin
          if Assigned(AOnException) then
            AOnException(E)
          else
            raise;
        end;
      end;
    finally
      LResponseStream.Free;
    end;
  finally
    LContent.Free;
  end;
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

procedure TWiRLClientCustomResource.SetPathParamsValues(const Value: TStrings);
begin
  FPathParamsValues.Assign(Value);
end;

procedure TWiRLClientCustomResource.SetQueryParams(const Value: TStrings);
begin
  FQueryParams.Assign(Value);
end;

procedure TWiRLClientCustomResource.StreamToObject(AObject: TObject;
  AResponse: TWiRLResponse; AStream: TStream);
var
  LType: TRttiType;
  LContext: TRttiContext;
  LMediaType: TMediaType;
  LReader: IMessageBodyReader;
begin
  LType := LContext.GetType(AObject.ClassInfo);
  LMediaType := TMediaType.Create(AResponse.ContentType);
  try
    LReader := Application.ReaderRegistry.FindReader(LType, LMediaType);
    if not Assigned(LReader) then
      raise EWiRLClientException.CreateFmt('Reader not found for [%s] content type: [%s]', [LType.Name, LMediaType.MediaType]);
    ContextInjection(LReader as TObject);

    LReader.ReadFrom(AObject, LType, LMediaType, AResponse.HeaderFields, AResponse.ContentStream);
  finally
    LMediaType.Free;
  end;
end;

initialization

RegisterHttpMethodImplementations;

end.
