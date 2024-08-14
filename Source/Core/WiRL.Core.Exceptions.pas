{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2023 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Exceptions;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.Generics.Collections,
  WiRL.Rtti.Utils,
  WiRL.Core.Classes,
  WiRL.Core.Singleton,
  WiRL.Core.JSON,
  WiRL.Core.Context.Server,
  WiRL.http.Request,
  WiRL.http.Response;

type
  /// <entity>Error</entity>
  TWebExceptionSchema = class
  private
    Fdata: TDictionary<string,string>;
    Fexception: string;
    Fmessage: string;
    Fstatus: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    property status: Integer read Fstatus write Fstatus;
    property exception: string read Fexception write Fexception;
    property message: string read Fmessage write Fmessage;
    property data: TDictionary<string,string> read Fdata write Fdata;
  end;

  Pair = record
  public
    Name: string;
    Value: TValue;

    class function S(const AName: string; const AValue: string): Pair; static;
    class function B(const AName: string; AValue: Boolean): Pair; static;
    class function N(const AName: string; AValue: Integer): Pair; static;
    class function F(const AName: string; AValue: Currency): Pair; static;
    class function D(const AName: string; AValue: TDateTime): Pair; static;

    function ToJSONValue: TJSONValue;
    function ToJSONPair: TJSONPair;
  end;
  TExceptionValues = array of Pair;

  TValuesUtil = class
    class function MakeValueArray(APair1: Pair): TExceptionValues; overload; static;
    class function MakeValueArray(APair1, APair2: Pair): TExceptionValues; overload; static;
    class function MakeValueArray(APair1, APair2, APair3: Pair): TExceptionValues; overload; static;
    class function AddValuePair(APairArray: TExceptionValues; APair: Pair): TExceptionValues;
  end;

  /// <summary>
  ///   This exception may be thrown by a resource method if a specific HTTP error response needs to be produced.
  /// </summary>
  EWiRLWebApplicationException = class(EWiRLException)
  private
    class function HandleCustomException(AContext: TWiRLContext; E: Exception): Boolean; static;
    class procedure BuildReponse(E: Exception; const AErrorMediaType: string; AResponse: TWiRLResponse);
  private
    FValues: TJSONObject;
    FStatus: Integer;

    procedure CreateAndFillValues;
    function GetStatus: Integer; virtual;
    procedure SetStatus(const Value: Integer); virtual;
  public
    /// <summary>
    ///   Construct an exception with a blank message and default HTTP status code of 500.
    /// </summary>
    constructor Create; overload; virtual;

    /// <summary>
    ///   Construct an exception with specified message and default HTTP status code of 500.
    /// </summary>
    constructor Create(const AMessage: string); overload;

    /// <summary>
    ///   Construct an exception with a format string
    /// </summary>
    constructor CreateFmt(const Msg: string; const Args: array of const);

    /// <summary>
    ///   Construct an exception with specified message and specified HTTP status code.
    /// </summary>
    constructor Create(const AMessage: string; AStatus: Integer); overload;

    /// <summary>
    ///   Construct a web exception with optional values
    /// </summary>
    /// <param name="AMessage">The exception's message</param>
    /// <param name="AStatus">The HTTP status</param>
    /// <param name="AValues">Optional values that will go in the data part</param>
    constructor Create(const AMessage: string; AStatus: Integer; AValues: TExceptionValues); overload;

    /// <summary>
    ///   Construct a web exception with an inner exception already trapped
    /// </summary>
    /// <param name="AInnerException">The inner exception object</param>
    /// <param name="AStatus">The HTTP status</param>
    /// <param name="AValues">Optional values (will be put in "data" sub-section)</param>
    constructor Create(AInnerException: Exception; AStatus: Integer; AValues: TExceptionValues); overload;

    /// <summary>
    ///   Construct a web exception with optional values
    /// </summary>
    /// <param name="AMessage">The exception's message</param>
    /// <param name="AStatus">The HTTP status</param>
    /// <param name="AJObject">Optional JSON object (will be put in "data" sub-section)</param>
    constructor Create(const AMessage: string; AStatus: Integer; AJObject: TJSONObject); overload;

    destructor Destroy; override;

    class function ExceptionToJSON(E: Exception): string; overload;
    class procedure ExceptionToJSON(E: Exception; StatusCode: Integer; AJSONObject: TJSONObject); overload;
    class procedure HandleException(AContext: TWiRLContext; E: Exception); static;

    function ToJSON: string;
    property Status: Integer read GetStatus write SetStatus;
  end;

  StatusCodeAttribute = class(TCustomAttribute)
  private
    FStatusCode: Integer;
  public
    property StatusCode: Integer read FStatusCode;
    constructor Create(AStatusCode: Integer);
  end;

  // Client errors (40x)

  EWiRLHttpStatusException = class(EWiRLWebApplicationException)
  public
    /// <summary>
    ///   Construct a web exception with an issuer and a method
    /// </summary>
    /// <param name="AMessage">The exception's message</param>
    /// <param name="AIssuer">The issuer, for examples the class generating the exception</param>
    /// <param name="AMethod">The method name, for examples the method generating the exception</param>
    constructor Create(const AMessage: string; const AIssuer: string; const AMethod: string = ''); overload;
    /// <summary>
    ///   Construct a web exception reading the StatusCode from an attribute
    /// </summary>
    constructor Create; override;
  end;

  [StatusCode(400)]
  EWiRLBadRequestException = class(EWiRLHttpStatusException)
  end;

  [StatusCode(401)]
  EWiRLNotAuthorizedException = class(EWiRLHttpStatusException)
  end;

  [StatusCode(404)]
  EWiRLNotFoundException = class(EWiRLHttpStatusException)
  end;

  [StatusCode(406)]
  EWiRLNotAcceptableException = class(EWiRLHttpStatusException)
  end;

  [StatusCode(415)]
  EWiRLUnsupportedMediaTypeException = class(EWiRLHttpStatusException)
  end;

  // Server errors (50x)

  [StatusCode(500)]
  EWiRLServerException = class(EWiRLHttpStatusException)
  end;

  [StatusCode(501)]
  EWiRLNotImplementedException = class(EWiRLHttpStatusException)
  end;

  TWiRLExceptionContext = class(TObject)
  private
    FRequest: TWiRLRequest;
    FResponse: TWiRLResponse;
    FContext: TWiRLContext;
    FError: Exception;
  public
    property Context: TWiRLContext read FContext;
    property Request: TWiRLRequest read FRequest;
    property Response: TWiRLResponse read FResponse;
    property Error: Exception read FError;
    constructor Create(AContext: TWiRLContext; AError: Exception);
  end;

  IWiRLExceptionMapper = interface
    ['{CD2233A7-F5CE-4D9F-AA0A-0C42C6C7F6DE}']
    procedure HandleException(AExceptionContext: TWiRLExceptionContext);
  end;

  TWiRLExceptionMapper = class(TInterfacedObject, IWiRLExceptionMapper)
  public
    procedure HandleException(AExceptionContext: TWiRLExceptionContext); virtual; abstract;
  end;

  TWiRLExceptionMapperConstructorInfo = class
  private
    FConstructorFunc: TFunc<TObject>;
    FExceptionMapperClass: TClass;
    FExceptionClass: ExceptClass;
  public
    property ExceptionMapperClass: TClass read FExceptionMapperClass;
    property ExceptionClass: ExceptClass read FExceptionClass;
    property ConstructorFunc: TFunc<TObject> read FConstructorFunc write FConstructorFunc;

    procedure HandleException(AContext: TWiRLContext; E: Exception);

    constructor Create(AExceptionMapperClass: TClass; AExceptionClass: ExceptClass; AConstructorFunc: TFunc<TObject>);
  end;

  TWiRLExceptionMapperRegistry = class(TObjectList<TWiRLExceptionMapperConstructorInfo>)
  private
    type
      TWiRLExceptionMapperRegistrySingleton = TWiRLSingleton<TWiRLExceptionMapperRegistry>;
  protected
    class function GetInstance: TWiRLExceptionMapperRegistry; static; inline;
  public
    constructor Create; virtual;

    function RegisterExceptionMapper<TMapper: class; TException: Exception>: TWiRLExceptionMapperConstructorInfo; overload;
    function RegisterExceptionMapper<TMapper: class; TException: Exception>(const AConstructorFunc: TFunc<TObject>): TWiRLExceptionMapperConstructorInfo; overload;

    class property Instance: TWiRLExceptionMapperRegistry read GetInstance;
  end;


implementation

uses
  System.TypInfo,
  WiRL.Configuration.Auth,
  WiRL.http.Accept.MediaType,
  WiRL.Engine.REST,
  WiRL.Core.Application;

{ Pair }

class function Pair.B(const AName: string; AValue: Boolean): Pair;
begin
  Result.Name := AName;
  Result.Value := TValue.From<Boolean>(AValue);
end;

class function Pair.D(const AName: string; AValue: TDateTime): Pair;
begin
  Result.Name := AName;
  Result.Value := TValue.From<TDateTime>(AValue);
end;

class function Pair.F(const AName: string; AValue: Currency): Pair;
begin
  Result.Name := AName;
  Result.Value := TValue.From<Currency>(AValue);
end;

class function Pair.N(const AName: string; AValue: Integer): Pair;
begin
  Result.Name := AName;
  Result.Value := TValue.From<Integer>(AValue);
end;

class function Pair.S(const AName: string; const AValue: string): Pair;
begin
  Result.Name := AName;
  Result.Value := TValue.From<string>(AValue);
end;

function Pair.ToJSONPair: TJSONPair;
begin
  Result := TJSONPair.Create(Name, ToJSONValue);
end;

function Pair.ToJSONValue: TJSONValue;

  function KindEnumeration: TJSONValue;
  begin
    if Value.TypeInfo.Name = 'Boolean' then
      if Value.AsBoolean then
        Result := TJSONTrue.Create
      else
        Result := TJSONFalse.Create
    else
      Result := TJSONString.Create('type:enumeration');
  end;

  function KindFloat: TJSONValue;
  var
    LDate: Double;
  begin
    if Value.TypeInfo.Name = 'TDateTime' then
    begin
      LDate := Value.AsCurrency;
      if Trunc(LDate) = 0 then
        Result := TJSONString.Create(FormatDateTime('hh:nn:ss:zzz', LDate))
      else if Frac(LDate) = 0 then
        Result := TJSONString.Create(FormatDateTime('yyyy-mm-dd', LDate))
      else
        Result := TJSONString.Create(FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', LDate))
    end
    else
      Result := TJSONNumber.Create(Value.AsCurrency);
  end;

begin
  Result := nil;
  case Value.Kind of
    tkUnknown:     Result := TJSONString.Create('type:unknown');
    tkInteger:     Result := TJSONNumber.Create(Value.AsInteger);
    tkChar:        Result := TJSONString.Create(Value.AsString);
    tkEnumeration: Result := KindEnumeration;
    tkFloat:       Result := KindFloat;
    tkString:      Result := TJSONString.Create(Value.AsString);
    tkSet:         Result := TJSONString.Create(Value.AsString);
    tkClass:       Result := TJSONString.Create(Value.AsObject.ToString);
    tkMethod:      Result := TJSONString.Create('type:method');
    tkWChar:       Result := TJSONString.Create(Value.AsString);
    tkLString:     Result := TJSONString.Create(Value.AsString);
    tkWString:     Result := TJSONString.Create(Value.AsString);
    tkVariant:     Result := TJSONString.Create(Value.AsString);
    tkArray:       Result := TJSONString.Create('type:array');
    tkRecord:      Result := TJSONString.Create('type:record');
    tkInterface:   Result := TJSONString.Create('type:interface');
    tkInt64:       Result := TJSONNumber.Create(Value.AsInt64);
    tkDynArray:    Result := TJSONString.Create('type:dynarray');
    tkUString:     Result := TJSONString.Create(Value.AsString);
    tkClassRef:    Result := TJSONString.Create(Value.AsClass.ClassName);
    tkPointer:     Result := TJSONNumber.Create(Value.AsInteger);
    tkProcedure:   Result := TJSONString.Create('type:procedure');
    {$IFDEF HAS_MANAGED_RECORD}
    tkMRecord:     Result := TJSONString.Create('type:mrecord');
    {$ENDIF}
  end;
end;

{ EWiRLWebApplicationException }

constructor EWiRLWebApplicationException.Create;
begin
  Create('', 0);
end;

constructor EWiRLWebApplicationException.Create(const AMessage: string);
begin
  Create(AMessage, 0);
end;

constructor EWiRLWebApplicationException.Create(const AMessage: string; AStatus: Integer);
begin
  inherited Create(AMessage);
  FStatus := AStatus;

  CreateAndFillValues;
end;

constructor EWiRLWebApplicationException.Create(const AMessage: string;
    AStatus: Integer; AValues: TExceptionValues);
var
  LPair: Pair;
  LData: TJSONObject;
begin
  Create(AMessage, AStatus);
  if Length(AValues) > 0 then
  begin
    LData := TJSONObject.Create;
    FValues.AddPair('data', LData);
    for LPair in AValues do
      if not LPair.Value.IsEmpty then
        LData.AddPair(LPair.ToJSONPair);
  end;
end;

class procedure EWiRLWebApplicationException.BuildReponse(E: Exception; const AErrorMediaType: string; AResponse: TWiRLResponse);
var
  LWebException: EWiRLWebApplicationException;
begin
  if E is EWiRLWebApplicationException then
  begin
    LWebException := E as EWiRLWebApplicationException;

    AResponse.StatusCode := LWebException.Status;
    AResponse.SetNonStandardReasonString(LWebException.Message);
    AResponse.Content := LWebException.ToJSON;
    AResponse.ContentType := TMediaType.APPLICATION_JSON;
  end
  else if E is Exception then
  begin
    AResponse.StatusCode := 500;
    AResponse.SetNonStandardReasonString(E.Message);
    AResponse.Content := EWiRLWebApplicationException.ExceptionToJSON(E);
    AResponse.ContentType := TMediaType.APPLICATION_JSON;
  end;
end;

constructor EWiRLWebApplicationException.Create(const AMessage: string;
    AStatus: Integer; AJObject: TJSONObject);
var
  LData: TJSONObject;
begin
  Create(AMessage, AStatus);
  if Assigned(AJObject) then
  begin
    LData := (AJObject.Clone as TJSONObject);
    FValues.AddPair('data', LData);
  end;
end;

constructor EWiRLWebApplicationException.Create(AInnerException: Exception;
    AStatus: Integer; AValues: TExceptionValues);
begin
  Create(AInnerException.Message, AStatus, AValues);
end;

destructor EWiRLWebApplicationException.Destroy;
begin
  FValues.Free;
  inherited;
end;

class procedure EWiRLWebApplicationException.ExceptionToJSON(E: Exception; StatusCode: Integer; AJSONObject: TJSONObject);
begin
  AJSONObject.AddPair(TJSONPair.Create('status', TJSONNumber.Create(StatusCode)));
  AJSONObject.AddPair(TJSONPair.Create('exception', TJSONString.Create(E.ClassName)));
  AJSONObject.AddPair(TJSONPair.Create('message', TJSONString.Create(E.Message)));
end;

function EWiRLWebApplicationException.GetStatus: Integer;
var
  LAttr: StatusCodeAttribute;
begin
  inherited;
  if FStatus = 0 then
  begin
    LAttr := TRttiHelper.FindAttribute<StatusCodeAttribute>(Self.ClassType);
    if Assigned(LAttr) then
      FStatus := LAttr.StatusCode
    else
      FStatus := 500;
  end;
  Result := FStatus;
end;

class function EWiRLWebApplicationException.ExceptionToJSON(E: Exception): string;
const
  InternalServerError = 500;
var
  LJSON: TJSONObject;
begin
  LJSON := TJSONObject.Create;
  try
    ExceptionToJSON(E, InternalServerError, LJSON);
    Result := TJSONHelper.ToJSON(LJSON);
  finally
    LJSON.Free;
  end;
end;

class function EWiRLWebApplicationException.HandleCustomException(AContext: TWiRLContext; E: Exception): Boolean;
var
  LCtorInfo: TWiRLExceptionMapperConstructorInfo;
begin
  Result := False;

  for LCtorInfo in TWiRLExceptionMapperRegistry.Instance do
  begin
    if E.ClassType.InheritsFrom(LCtorInfo.ExceptionClass) then
    begin
      LCtorInfo.HandleException(AContext, E);
      Exit(True);
    end;
  end;
end;

class procedure EWiRLWebApplicationException.HandleException(AContext: TWiRLContext; E: Exception);
var
  LAuthChallengeHeader: string;
  LErrorMediaType: string;
  LApplication: TWiRLApplication;
begin
  LApplication := nil;
  if Assigned(AContext.Application) and (AContext.Application is TWiRLApplication) then
  begin
    LApplication := TWiRLApplication(AContext.Application);
    LAuthChallengeHeader := LApplication.GetConfiguration<TWiRLConfigurationAuth>.AuthChallengeHeader;
    LErrorMediaType := LApplication.ErrorMediaType;
  end
  else
    LAuthChallengeHeader := '';

  if HandleCustomException(AContext, E) then
    Exit;

  if LErrorMediaType = TMediaType.WILDCARD then
  begin
    if AContext.Request.Accept <> '' then
      LErrorMediaType := AContext.Request.Accept
    else
      LErrorMediaType := TMediaType.APPLICATION_JSON;
  end;

  EWiRLWebApplicationException.BuildReponse(E, LErrorMediaType, AContext.Response);
  if (AContext.Response.StatusCode = 401) and (LAuthChallengeHeader <> '') then
    AContext.Response.WWWAuthenticate := LAuthChallengeHeader;

  if Assigned(LApplication) then
  begin
    if Assigned(AContext.Engine) then
      TWiRLRESTEngine(AContext.Engine).HandleException(AContext, E);
  end;
end;

procedure EWiRLWebApplicationException.SetStatus(const Value: Integer);
begin
  FStatus := Value;
end;

procedure EWiRLWebApplicationException.CreateAndFillValues;
begin
  FValues := TJSONObject.Create;
  FValues.AddPair(TJSONPair.Create('status', TJSONNumber.Create(Status)));
  FValues.AddPair(TJSONPair.Create('exception', TJSONString.Create(Self.ClassName)));
  FValues.AddPair(TJSONPair.Create('message', TJSONString.Create(Self.Message)));
end;

constructor EWiRLWebApplicationException.CreateFmt(const Msg: string;
  const Args: array of const);
begin
  Create(Format(Msg, Args));
end;

function EWiRLWebApplicationException.ToJSON: string;
begin
  if not Assigned(FValues) then
    CreateAndFillValues;

  Result := TJSONHelper.ToJSON(FValues)
end;

{ TValuesUtil }

class function TValuesUtil.MakeValueArray(APair1: Pair): TExceptionValues;
begin
  SetLength(Result, 1);
  Result[0] := APair1;
end;

class function TValuesUtil.MakeValueArray(APair1, APair2: Pair): TExceptionValues;
begin
  SetLength(Result, 2);
  Result[0] := APair1;
  Result[1] := APair2;
end;

class function TValuesUtil.AddValuePair(APairArray: TExceptionValues;
  APair: Pair): TExceptionValues;
begin
  SetLength(APairArray, Length(APairArray) + 1);
  APairArray[Length(APairArray) - 1] := APair;
  Result := APairArray;
end;

class function TValuesUtil.MakeValueArray(APair1, APair2, APair3: Pair): TExceptionValues;
begin
  SetLength(Result, 3);
  Result[0] := APair1;
  Result[1] := APair2;
  Result[2] := APair3;
end;

{ TWiRLExceptionContext<E> }

constructor TWiRLExceptionContext.Create(AContext: TWiRLContext; AError: Exception);
begin
  inherited Create;
  FContext := AContext;
  FRequest := AContext.Request;
  FResponse := AContext.Response;
  FError := AError;
end;

{ TWiRLExceptionMapperRegistry }

constructor TWiRLExceptionMapperRegistry.Create;
begin
  inherited Create;
end;

class function TWiRLExceptionMapperRegistry.GetInstance: TWiRLExceptionMapperRegistry;
begin
  Result := TWiRLExceptionMapperRegistrySingleton.Instance;
end;

function TWiRLExceptionMapperRegistry.RegisterExceptionMapper<TMapper, TException>: TWiRLExceptionMapperConstructorInfo;
begin
  Result := RegisterExceptionMapper<TMapper, TException>(nil);
end;

function TWiRLExceptionMapperRegistry.RegisterExceptionMapper<TMapper, TException>(
  const AConstructorFunc: TFunc<TObject>): TWiRLExceptionMapperConstructorInfo;
begin
  if not Supports(TClass(TMapper), IWiRLExceptionMapper) then
    raise EWiRLServerException.Create(
      Format('Exception mapper registration error: [%s] should be a valid exception mapper', [TClass(TMapper).QualifiedClassName]),
      Self.ClassName,
      'RegisterExceptionMapper'
    );

  if not TClass(TException).InheritsFrom(Exception) then
    raise EWiRLServerException.Create(
      Format('Exception mapper registration error: [%s] should be a valid exception', [TClass(TException).QualifiedClassName]),
      Self.ClassName,
      'RegisterExceptionMapper'
    );

  Result := TWiRLExceptionMapperConstructorInfo.Create(TClass(TMapper), ExceptClass(TException), AConstructorFunc);
  Add(Result);
end;

{ TWiRLExceptionMapperConstructorInfo }

constructor TWiRLExceptionMapperConstructorInfo.Create(
  AExceptionMapperClass: TClass; AExceptionClass: ExceptClass;
  AConstructorFunc: TFunc<TObject>);
begin
  inherited Create;
  FExceptionMapperClass := AExceptionMapperClass;
  FExceptionClass := AExceptionClass;
  FConstructorFunc := AConstructorFunc;
end;

procedure TWiRLExceptionMapperConstructorInfo.HandleException(AContext: TWiRLContext; E: Exception);
var
  LObject: TObject;
  LExceptionMapper: IWiRLExceptionMapper;
  LExceptionContext: TWiRLExceptionContext;
begin
  if Assigned(ConstructorFunc) then
  begin
    LObject := ConstructorFunc();
    if not Supports(LObject, IWiRLExceptionMapper, LExceptionMapper) then
      raise EWiRLServerException.Create(
        Format('Constructor for [%s] should create a valid exception mapper', [FExceptionMapperClass.ClassName]),
        Self.ClassName,
        'HandleException'
      );
  end
  else
  begin
    LObject := TRttiHelper.CreateInstance(FExceptionMapperClass);
    if not Supports(LObject, IWiRLExceptionMapper, LExceptionMapper) then
      raise EWiRLServerException.Create(
        Format('Class [%s] should implements an exception mapper interface', [FExceptionMapperClass.ClassName]),
        Self.ClassName,
        'HandleException'
      );
  end;

  LExceptionContext := TWiRLExceptionContext.Create(AContext, E);
  try
    LExceptionMapper.HandleException(LExceptionContext);
  finally
    LExceptionContext.Free;
  end;
end;

{ EWiRLHttpStatusException }

constructor EWiRLHttpStatusException.Create(const AMessage, AIssuer, AMethod: string);
var
  LPairArray: TExceptionValues;
begin
  if not AIssuer.IsEmpty then
  begin
    LPairArray := TValuesUtil.AddValuePair(LPairArray, Pair.S('issuer', AIssuer));
  end;
  if not AMethod.IsEmpty then
  begin
    LPairArray := TValuesUtil.AddValuePair(LPairArray, Pair.S('method', AMethod));
  end;

  Create(AMessage, 0, LPairArray);
end;

constructor EWiRLHttpStatusException.Create;
begin
  inherited Create;
end;

{ StatusCodeAttribute }

constructor StatusCodeAttribute.Create(AStatusCode: Integer);
begin
  inherited Create;
  FStatusCode := AStatusCode;
end;

{ TWebExceptionSchema }

constructor TWebExceptionSchema.Create;
begin
  fdata := TDictionary<string,string>.Create;
end;

destructor TWebExceptionSchema.Destroy;
begin
  fdata.Free;
  inherited;
end;

end.
