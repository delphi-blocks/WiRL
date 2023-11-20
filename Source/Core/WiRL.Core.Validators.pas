{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2023 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Validators;

interface

uses
  System.SysUtils, System.Classes, System.Rtti, System.RegularExpressions,
  System.Generics.Collections, System.Generics.Defaults, System.TypInfo,
  WiRL.Core.Classes,
  WiRL.Core.Singleton,
  WiRL.Core.Context.Server,
  WiRL.Core.Exceptions,
  WiRL.Rtti.Utils;

type
  EWiRLValidationError = class(EWiRLException);

  // Use this attribute to mark the validation attributes
  // that should be evaluated before the parsing of
  // the parameters
  RawConstraintAttribute = class(TCustomAttribute);

  // Every constraint attribute should inherited from this class
  TCustomConstraintAttribute = class(TCustomAttribute)
  private
    FValidator: IInterface;
    function GetRawConstraint: Boolean;
  protected
    FErrorMessage: string;
  public
    function GetValidator: IInterface;
    property ErrorMessage: string read FErrorMessage;
    property RawConstraint: Boolean read GetRawConstraint;
  end;

  MinAttribute = class(TCustomConstraintAttribute)
  private
    FMinValue: Integer;
  public
    constructor Create(AMinValue: Integer; const AMessage: string = '');
    property MinValue: Integer read FMinValue;
  end;

  MaxAttribute = class(TCustomConstraintAttribute)
  private
    FMaxValue: Integer;
  public
    constructor Create(AMaxValue: Integer; const AMessage: string = '');
    property MaxValue: Integer read FMaxValue;
  end;

  [RawConstraint]
  NotNullAttribute = class(TCustomConstraintAttribute)
  public
    constructor Create(const AMessage: string = '');
  end;

  [RawConstraint]
  PatternAttribute = class(TCustomConstraintAttribute)
  private
    FPattern: string;
  public
    constructor Create(const APattern :string; const AMessage: string = '');
    property Pattern: string read FPattern;
  end;

  [RawConstraint]
  SizeAttribute = class(TCustomConstraintAttribute)
  private
    FMax: Integer;
    FMin: Integer;
  public
    constructor Create(AMin, AMax: Integer; const AMessage: string = '');
    property Min: Integer read FMin;
    property Max: Integer read FMax;
  end;


  IConstraintValidator<A: TCustomAttribute> = interface
    ['{3996A923-1F21-49AF-91F0-01992DB6E25B}']
    procedure Initialize(ConstraintAnnotation: A);
    function IsValid(AValue: TValue; Context: TWiRLContext): Boolean;
  end;

  // Validators registry

  TWiRLValidatorConstructorInfo = class
  private
    FValidatorClass: TClass;
    FAttributeInfo: PTypeInfo;
    FConstructorFunc: TFunc<TObject>;
  public
    constructor Create(AValidatorClass: TClass; AConstructorFunc: TFunc<TObject>; AAttributeInfo: PTypeInfo);

    property ValidatorClass: TClass read FValidatorClass;
    property AttributeInfo: PTypeInfo read FAttributeInfo;
    property ConstructorFunc: TFunc<TObject> read FConstructorFunc write FConstructorFunc;
  end;

  TWiRLValidatorRegistry = class(TObjectList<TWiRLValidatorConstructorInfo>)
  private
    type
      TWiRLValidatorRegistrySingleton = TWiRLSingleton<TWiRLValidatorRegistry>;
    function GetConstraintAttribute(AClass: TClass): PTypeInfo;
  protected
    class function GetInstance: TWiRLValidatorRegistry; static; inline;
  public
    function RegisterValidator<T: class>(const AConstructorFunc: TFunc<TObject>): TWiRLValidatorConstructorInfo; overload;
    function RegisterValidator<T: class>: TWiRLValidatorConstructorInfo; overload;

    function FindValidator(AAttrib: TCustomConstraintAttribute): TObject;

    class property Instance: TWiRLValidatorRegistry read GetInstance;

    constructor Create;
  end;

  ////////////////////////////////////////////////////////////////////
  // Built-in validators
  ////////////////////////////////////////////////////////////////////

  TMaxValidator = class(TInterfacedObject, IConstraintValidator<MaxAttribute>)
  private
    FMaxValue :Integer;
  public
    procedure Initialize(AMaxAttribute: MaxAttribute);
    function IsValid(AValue: TValue; Context: TWiRLContext): Boolean;
  end;

  TMinValidator = class(TInterfacedObject, IConstraintValidator<MinAttribute>)
  private
    FMinValue :Integer;
  public
    procedure Initialize(AMinAttribute: MinAttribute);
    function IsValid(AValue: TValue; Context: TWiRLContext): Boolean;
  end;

  TNotNullValidator = class(TInterfacedObject, IConstraintValidator<NotNullAttribute>)
  public
    procedure Initialize(ANotNullAttribute: NotNullAttribute);
    function IsValid(AValue: TValue; Context: TWiRLContext): Boolean;
  end;

  TPatternValidator = class(TInterfacedObject, IConstraintValidator<PatternAttribute>)
  private
    FPattern: string;
  public
    procedure Initialize(APatternAttribute: PatternAttribute);
    function IsValid(AValue: TValue; Context: TWiRLContext): Boolean;
  end;

  TSizeValidator = class(TInterfacedObject, IConstraintValidator<SizeAttribute>)
  private
    FMin: Integer;
    FMax: Integer;
  public
    procedure Initialize(APatternAttribute: SizeAttribute);
    function IsValid(AValue: TValue; Context: TWiRLContext): Boolean;
  end;

implementation

{ TCustomConstraintAttrubute }

function TCustomConstraintAttribute.GetRawConstraint: Boolean;
begin
  Result := TRttiHelper.HasAttribute<RawConstraintAttribute>(TRttiHelper.Context.GetType(ClassType));
end;

function TCustomConstraintAttribute.GetValidator: IInterface;
var
  LValidator :IConstraintValidator<TCustomAttribute>;
  LObj: TObject;
begin
  if not Assigned(FValidator) then // Should it be inside a critical section?
  begin
    LObj := TWiRLValidatorRegistry.Instance.FindValidator(Self);
    if not Assigned(LObj) then
      raise Exception.CreateFmt('Validator not found for [%s]', [Self.ClassName]);
    if not Supports(LObj, IConstraintValidator<TCustomAttribute>, LValidator) then
      raise EWiRLValidationError.Create('Validation class not valid');

    FValidator := LValidator;
    LValidator.Initialize(Self);
  end;
  Result := FValidator;
end;

{ MaxAttribute }

constructor MaxAttribute.Create(AMaxValue: Integer; const AMessage: string);
begin
  inherited Create;
  FMaxValue := AMaxValue;
  FErrorMessage := AMessage;
end;

{ TMaxValidator }

procedure TMaxValidator.Initialize(AMaxAttribute: MaxAttribute);
begin
  FMaxValue := AMaxAttribute.MaxValue;
end;

function TMaxValidator.IsValid(AValue: TValue; Context: TWiRLContext): Boolean;
begin
  Result := AValue.AsInteger <= FMaxValue;
end;

{ MinAttribute }

constructor MinAttribute.Create(AMinValue: Integer; const AMessage: string);
begin
  FMinValue := AMinValue;
  FErrorMessage := AMessage;
end;

{ TMinValidator }

procedure TMinValidator.Initialize(AMinAttribute: MinAttribute);
begin
  FMinValue := AMinAttribute.MinValue;
end;

function TMinValidator.IsValid(AValue: TValue; Context: TWiRLContext): Boolean;
begin
  Result := AValue.AsInteger >= FMinValue;
end;

{ NotNullAttribute }

constructor NotNullAttribute.Create(const AMessage: string);
begin
  FErrorMessage := AMessage;
end;

{ TNotNullValidator }

procedure TNotNullValidator.Initialize(ANotNullAttribute: NotNullAttribute);
begin
end;

function TNotNullValidator.IsValid(AValue: TValue; Context: TWiRLContext): Boolean;
begin
  Result := AValue.ToString <> '';
end;

{ TPatternValidator }

procedure TPatternValidator.Initialize(APatternAttribute: PatternAttribute);
begin
  FPattern := APatternAttribute.Pattern;
end;

function TPatternValidator.IsValid(AValue: TValue; Context: TWiRLContext): Boolean;
begin
  Result := TRegEx.IsMatch(AValue.ToString, FPattern);
end;

{ PatternAttribute }

constructor PatternAttribute.Create(const APattern, AMessage: string);
begin
  FErrorMessage := AMessage;
  FPattern := APattern;
end;

{ TWiRLValidatorRegistry }

constructor TWiRLValidatorRegistry.Create;
begin
  inherited Create(True);
end;

function TWiRLValidatorRegistry.FindValidator(AAttrib: TCustomConstraintAttribute): TObject;
var
  LItem: TWiRLValidatorConstructorInfo;
begin
  Result := nil;
  for LItem in Self do
  begin
    if TRttiHelper.Context.GetType(AAttrib.ClassType).Handle = LItem.FAttributeInfo then
      Exit(LItem.FValidatorClass.Create);
  end;
end;

function TWiRLValidatorRegistry.GetConstraintAttribute(AClass: TClass): PTypeInfo;
var
  LInitMethod: TRttiMethod;
  LInitParams: TArray<TRttiParameter>;
begin
  LInitMethod := TRttiHelper.Context.GetType(AClass).GetMethod('Initialize');
  if not Assigned(LInitMethod) then
    raise EWiRLException.CreateFmt('Invalid validator class [%s] (Initialize method not found)', [AClass.ClassName]);

  LInitParams := LInitMethod.GetParameters;
  if Length(LInitParams) <> 1 then
    raise EWiRLException.CreateFmt('Invalid validator class [%s] (Initialize wrong number of parameters)', [AClass.ClassName]);
  Result := LInitParams[0].ParamType.Handle;
end;

class function TWiRLValidatorRegistry.GetInstance: TWiRLValidatorRegistry;
begin
  Result := TWiRLValidatorRegistrySingleton.Instance;
end;

function TWiRLValidatorRegistry.RegisterValidator<T>(
  const AConstructorFunc: TFunc<TObject>): TWiRLValidatorConstructorInfo;
var
  LValidator: TClass;
begin
  LValidator := TClass(T);
  Result := TWiRLValidatorConstructorInfo.Create(LValidator, AConstructorFunc, GetConstraintAttribute(LValidator));
  Add(Result);
end;

function TWiRLValidatorRegistry.RegisterValidator<T>: TWiRLValidatorConstructorInfo;
begin
  Result := RegisterValidator<T>(nil);
end;

{ TWiRLValidatorConstructorInfo }

constructor TWiRLValidatorConstructorInfo.Create(AValidatorClass: TClass;
  AConstructorFunc: TFunc<TObject>; AAttributeInfo: PTypeInfo);
begin
  FValidatorClass := AValidatorClass;
  FConstructorFunc := AConstructorFunc;
  FAttributeInfo := AAttributeInfo;
end;

{ SizeAttribute }

constructor SizeAttribute.Create(AMin, AMax: Integer; const AMessage: string);
begin
  FErrorMessage := AMessage;
  FMin := AMin;
  FMax := AMax;
end;

{ TSizeValidator }

procedure TSizeValidator.Initialize(APatternAttribute: SizeAttribute);
begin
  FMin := APatternAttribute.Min;
  FMax := APatternAttribute.Max;
end;

function TSizeValidator.IsValid(AValue: TValue; Context: TWiRLContext): Boolean;
var
  ValueSize: Integer;
begin
  ValueSize := AValue.ToString.Length;
  Result := (ValueSize >= FMin) and (ValueSize <= FMax)
end;

initialization
  TWiRLValidatorRegistry.Instance.RegisterValidator<TNotNullValidator>;
  TWiRLValidatorRegistry.Instance.RegisterValidator<TSizeValidator>;
  TWiRLValidatorRegistry.Instance.RegisterValidator<TPatternValidator>;

  TWiRLValidatorRegistry.Instance.RegisterValidator<TMaxValidator>;
  TWiRLValidatorRegistry.Instance.RegisterValidator<TMinValidator>;

end.
