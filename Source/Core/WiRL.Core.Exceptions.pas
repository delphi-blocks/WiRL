{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
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
  WiRL.Core.Singleton,
  WiRL.Core.JSON,
  WiRL.Core.Context,
  WiRL.http.Request,
  WiRL.http.Response;

type
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
  end;

  EWiRLException = class(Exception);

  /// <summary>
  ///   This exception may be thrown by a resource method if a specific HTTP error response needs to be produced.
  /// </summary>
  EWiRLWebApplicationException = class(EWiRLException)
  private
    class function HandleCustomException(AContext: TWiRLContext; E: Exception): Boolean; static;
  private
    FValues: TJSONObject;
    FStatus: Integer;

    procedure CreateAndFillValues;
  public
    /// <summary>
    ///   Construct an exception with a blank message and default HTTP status code of 500.
    /// </summary>
    constructor Create; overload;

    /// <summary>
    ///   Construct an exception with specified message and default HTTP status code of 500.
    /// </summary>
    constructor Create(const AMessage: string); overload;

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
    property Status: Integer read FStatus write FStatus;
  end;

  // Client errors (400)

  EWiRLNotFoundException = class(EWiRLWebApplicationException)
  public
    constructor Create(const AMessage: string; const AIssuer: string = ''; const AMethod: string = '');
  end;

  EWiRLNotAuthorizedException = class(EWiRLWebApplicationException)
  public
    constructor Create(const AMessage: string; const AIssuer: string = ''; const AMethod: string = '');
  end;

  EWiRLNotAcceptableException = class(EWiRLWebApplicationException)
  public
    constructor Create(const AMessage: string; const AIssuer: string = ''; const AMethod: string = '');
  end;

  EWiRLUnsupportedMediaTypeException = class(EWiRLWebApplicationException)
  public
    constructor Create(const AMessage: string; const AIssuer: string = ''; const AMethod: string = '');
  end;

  // Server errors (500)

  EWiRLServerException = class(EWiRLWebApplicationException)
  public
    constructor Create(const AMessage: string; const AIssuer: string = ''; const AMethod: string = '');
  end;

  EWiRLNotImplementedException = class(EWiRLWebApplicationException)
  public
    constructor Create(const AMessage: string; const AIssuer: string = ''; const AMethod: string = '');
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

//    function FilterByClassName(const AClassName: string; out AConstructorInfo: TWiRLFilterConstructorInfo) :Boolean;
    function RegisterExceptionMapper<TMapper: class; TException: Exception>: TWiRLExceptionMapperConstructorInfo; overload;
    function RegisterExceptionMapper<TMapper: class; TException: Exception>(const AConstructorFunc: TFunc<TObject>): TWiRLExceptionMapperConstructorInfo; overload;

//    function ApplyPreMatchingRequestFilters(AContext: TWiRLContext): Boolean;
//    procedure ApplyPreMatchingResponseFilters(AContext: TWiRLContext);
//
//    procedure FetchRequestFilter(const PreMatching: Boolean; ARequestProc: TProc<TWiRLFilterConstructorInfo>);
//    procedure FetchResponseFilter(const PreMatching: Boolean; AResponseProc: TProc<TWiRLFilterConstructorInfo>);

    class property Instance: TWiRLExceptionMapperRegistry read GetInstance;
  end;


implementation

uses
  System.TypInfo,
  WiRL.Configuration.Auth,
  WiRL.http.Accept.MediaType,
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
var
  LDate: Double;
begin
  Result := nil;
  if Value.IsType<TDateTime> then
  begin
    LDate := Value.AsCurrency;
    if Trunc(LDate) = 0 then
      Result := TJSONString.Create(FormatDateTime('hh:nn:ss:zzz', LDate))
    else if Frac(LDate) = 0 then
      Result := TJSONString.Create(FormatDateTime('yyyy-mm-dd', LDate))
    else
      Result := TJSONString.Create(FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', LDate))
  end
  else if Value.IsType<Boolean> then
  begin
    if Value.AsBoolean then
      Result := TJSONTrue.Create
    else
      Result := TJSONFalse.Create
  end
  else
  case Value.Kind of
    tkUnknown:     Result := TJSONString.Create('type:unknown');
    tkInteger:     Result := TJSONNumber.Create(Value.AsCurrency);
    tkChar:        Result := TJSONString.Create(Value.AsString);
    tkEnumeration: Result := TJSONString.Create('type:enumeration');
    tkFloat:       Result := TJSONNumber.Create(Value.AsCurrency);
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
  end;
end;

{ EWiRLWebApplicationException }

constructor EWiRLWebApplicationException.Create;
begin
  Create('', 500);
end;

constructor EWiRLWebApplicationException.Create(const AMessage: string);
begin
  Create(AMessage, 500);
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

class function EWiRLWebApplicationException.HandleCustomException(
  AContext: TWiRLContext; E: Exception): Boolean;
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
  LWebException: EWiRLWebApplicationException;
  LAuthChallengeHeader: string;
begin
  if Assigned(AContext.Application) and (AContext.Application is TWiRLApplication) then
    LAuthChallengeHeader := TWiRLApplication(AContext.Application).GetConfiguration<TWiRLConfigurationAuth>.AuthChallengeHeader
  else
    LAuthChallengeHeader := '';

  if HandleCustomException(AContext, E) then
    Exit;

  if E is EWiRLWebApplicationException then
  begin
    LWebException := E as EWiRLWebApplicationException;

    AContext.Response.StatusCode := LWebException.Status;
    AContext.Response.SetNonStandardReasonString(LWebException.Message);
    AContext.Response.Content := LWebException.ToJSON;
    AContext.Response.ContentType := TMediaType.APPLICATION_JSON;

    // Set the Authorization challenge
    if (LWebException.Status = 401) and (LAuthChallengeHeader <> '') then
      AContext.Response.HeaderFields['WWW-Authenticate'] := LAuthChallengeHeader;
  end
  else if E is Exception then
  begin
    AContext.Response.StatusCode := 500;
    AContext.Response.SetNonStandardReasonString(E.Message);
    AContext.Response.Content := EWiRLWebApplicationException.ExceptionToJSON(E);
    AContext.Response.ContentType := TMediaType.APPLICATION_JSON;
  end;
end;

procedure EWiRLWebApplicationException.CreateAndFillValues;
begin
  FValues := TJSONObject.Create;
  FValues.AddPair(TJSONPair.Create('status', TJSONNumber.Create(FStatus)));
  FValues.AddPair(TJSONPair.Create('exception', TJSONString.Create(Self.ClassName)));
  FValues.AddPair(TJSONPair.Create('message', TJSONString.Create(Self.Message)));
end;

function EWiRLWebApplicationException.ToJSON: string;
begin
  if not Assigned(FValues) then
    CreateAndFillValues;

  Result := TJSONHelper.ToJSON(FValues)
end;

{ EWiRLNotFoundException }

constructor EWiRLNotFoundException.Create(const AMessage: string; const
    AIssuer: string = ''; const AMethod: string = '');
var
  LPairArray: TExceptionValues;
begin
  if not AIssuer.IsEmpty then
  begin
    SetLength(LPairArray, Length(LPairArray) + 1);
    LPairArray[Length(LPairArray) - 1] := Pair.S('issuer', AIssuer);
  end;
  if not AMethod.IsEmpty then
  begin
    SetLength(LPairArray, Length(LPairArray) + 1);
    LPairArray[Length(LPairArray) - 1] := Pair.S('method', AMethod);
  end;

  inherited Create(AMessage, 404, LPairArray);
end;

{ EWiRLServerException }

constructor EWiRLServerException.Create(const AMessage: string; const AIssuer:
    string = ''; const AMethod: string = '');
var
  LPairArray: TExceptionValues;
begin
  if not AIssuer.IsEmpty then
  begin
    SetLength(LPairArray, Length(LPairArray) + 1);
    LPairArray[Length(LPairArray) - 1] := Pair.S('issuer', AIssuer);
  end;
  if not AMethod.IsEmpty then
  begin
    SetLength(LPairArray, Length(LPairArray) + 1);
    LPairArray[Length(LPairArray) - 1] := Pair.S('method', AMethod);
  end;

  inherited Create(AMessage, 500, LPairArray);
end;

{ EWiRLNotAuthorizedException }

constructor EWiRLNotAuthorizedException.Create(const AMessage: string; const
    AIssuer: string = ''; const AMethod: string = '');
var
  LPairArray: TExceptionValues;
begin
  if not AIssuer.IsEmpty then
  begin
    SetLength(LPairArray, Length(LPairArray) + 1);
    LPairArray[Length(LPairArray) - 1] := Pair.S('issuer', AIssuer);
  end;
  if not AMethod.IsEmpty then
  begin
    SetLength(LPairArray, Length(LPairArray) + 1);
    LPairArray[Length(LPairArray) - 1] := Pair.S('method', AMethod);
  end;

  inherited Create(AMessage, 401, LPairArray);
end;

{ EWiRLNotImplementedException }

constructor EWiRLNotImplementedException.Create(
  const AMessage, AIssuer, AMethod: string);
var
  LPairArray: TExceptionValues;
begin
  if not AIssuer.IsEmpty then
  begin
    SetLength(LPairArray, Length(LPairArray) + 1);
    LPairArray[Length(LPairArray) - 1] := Pair.S('issuer', AIssuer);
  end;
  if not AMethod.IsEmpty then
  begin
    SetLength(LPairArray, Length(LPairArray) + 1);
    LPairArray[Length(LPairArray) - 1] := Pair.S('method', AMethod);
  end;

  inherited Create(AMessage, 501, LPairArray);
end;

{ EWiRLUnsupportedMediaTypeException }

constructor EWiRLUnsupportedMediaTypeException.Create(
  const AMessage, AIssuer, AMethod: string);
var
  LPairArray: TExceptionValues;
begin
  if not AIssuer.IsEmpty then
  begin
    SetLength(LPairArray, Length(LPairArray) + 1);
    LPairArray[Length(LPairArray) - 1] := Pair.S('issuer', AIssuer);
  end;
  if not AMethod.IsEmpty then
  begin
    SetLength(LPairArray, Length(LPairArray) + 1);
    LPairArray[Length(LPairArray) - 1] := Pair.S('method', AMethod);
  end;

  inherited Create(AMessage, 415, LPairArray);
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

class function TValuesUtil.MakeValueArray(APair1, APair2, APair3: Pair): TExceptionValues;
begin
  SetLength(Result, 3);
  Result[0] := APair1;
  Result[1] := APair2;
  Result[2] := APair3;
end;

{ EWiRLNotAcceptableException }

constructor EWiRLNotAcceptableException.Create(const AMessage, AIssuer, AMethod: string);
var
  LPairArray: TExceptionValues;
begin
  if not AIssuer.IsEmpty then
  begin
    SetLength(LPairArray, Length(LPairArray) + 1);
    LPairArray[Length(LPairArray) - 1] := Pair.S('issuer', AIssuer);
  end;
  if not AMethod.IsEmpty then
  begin
    SetLength(LPairArray, Length(LPairArray) + 1);
    LPairArray[Length(LPairArray) - 1] := Pair.S('method', AMethod);
  end;

  inherited Create(AMessage, 406, LPairArray);
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

procedure TWiRLExceptionMapperConstructorInfo.HandleException(
  AContext: TWiRLContext; E: Exception);
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

end.
