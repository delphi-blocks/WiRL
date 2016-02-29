(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit MARS.Core.Exceptions;

interface

uses
  System.SysUtils,
  System.Rtti,
  MARS.Core.JSON,
  MARS.Core.Response;

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

  EMARSException = class(Exception);

  /// <summary>
  ///   This exception may be thrown by a resource method if a specific HTTP error response needs to be produced.
  /// </summary>
  EMARSWebApplicationException = class(EMARSException)
  private
    FValues: TJSONObject;
    FStatus: Integer;
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
    constructor Create(AInnerException: Exception; AStatus: Integer; AValues:
        TExceptionValues); overload;

    /// <summary>
    ///   Construct a web exception with optional values
    /// </summary>
    /// <param name="AMessage">The exception's message</param>
    /// <param name="AStatus">The HTTP status</param>
    /// <param name="AJObject">Optional JSON object (will be put in "data" sub-section)</param>
    constructor Create(const AMessage: string; AStatus: Integer; AJObject: TJSONObject); overload;

    destructor Destroy; override;

    function ToJSON: string;
    property Status: Integer read FStatus write FStatus;
  end;

  EMARSNotFoundException = class(EMARSWebApplicationException)
  public
    constructor Create(const AMessage: string; const AIssuer: string = ''; const AMethod: string = '');
  end;

  EMARSServerException = class(EMARSWebApplicationException)
  public
    constructor Create(const AMessage: string; const AIssuer: string = ''; const AMethod: string = '');
  end;

  EMARSNotAuthorizedException = class(EMARSWebApplicationException)
  public
    constructor Create(const AMessage: string; const AIssuer: string = ''; const AMethod: string = '');
  end;

  EMARSNotSupportedException = class(EMARSWebApplicationException)
  public
    constructor Create(const AMessage: string; const AIssuer: string = ''; const AMethod: string = '');
  end;

implementation

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
  LData: TJSONObject;
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

{ EMARSWebApplicationException }

constructor EMARSWebApplicationException.Create;
begin
  Create('', 500);
end;

constructor EMARSWebApplicationException.Create(const AMessage: string);
begin
  Create(AMessage, 500);
end;

constructor EMARSWebApplicationException.Create(const AMessage: string; AStatus: Integer);
begin
  inherited Create(AMessage);
  FValues := TJSONObject.Create;
  FStatus := AStatus;

  FValues.AddPair(TJSONPair.Create('status', TJSONNumber.Create(AStatus)));
  FValues.AddPair(TJSONPair.Create('exception', TJSONString.Create(Self.ClassName)));
  FValues.AddPair(TJSONPair.Create('message', TJSONString.Create(AMessage)));
end;

constructor EMARSWebApplicationException.Create(const AMessage: string;
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

constructor EMARSWebApplicationException.Create(const AMessage: string;
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

constructor EMARSWebApplicationException.Create(AInnerException: Exception;
    AStatus: Integer; AValues: TExceptionValues);
begin
  Create(AInnerException.Message, AStatus, AValues);
end;

destructor EMARSWebApplicationException.Destroy;
begin
  FValues.Free;
  inherited;
end;

function EMARSWebApplicationException.ToJSON: string;
begin
  Result := FValues.ToJSON;
end;

{ EMARSNotFoundException }

constructor EMARSNotFoundException.Create(const AMessage: string; const
    AIssuer: string = ''; const AMethod: string = '');
begin
  inherited Create(AMessage, 404, [Pair.S('issuer', AIssuer), Pair.S('method', AMethod)]);
end;

{ EMARSServerException }

constructor EMARSServerException.Create(const AMessage: string; const AIssuer:
    string = ''; const AMethod: string = '');
begin
  inherited Create(AMessage, 500, [Pair.S('issuer', AIssuer), Pair.S('method', AMethod)]);
end;

{ EMARSNotAuthorizedException }

constructor EMARSNotAuthorizedException.Create(const AMessage: string; const
    AIssuer: string = ''; const AMethod: string = '');
begin
  inherited Create(AMessage, 401, [Pair.S('issuer', AIssuer), Pair.S('method', AMethod)]);
end;

{ EMARSNotSupportedException }

constructor EMARSNotSupportedException.Create(const AMessage: string; const
    AIssuer: string = ''; const AMethod: string = '');
begin
  inherited Create(AMessage, 500, [Pair.S('issuer', AIssuer), Pair.S('method', AMethod)]);
end;

end.
