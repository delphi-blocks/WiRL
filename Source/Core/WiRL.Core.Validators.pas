{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Validators;

interface

uses
  System.SysUtils, System.Classes, System.Rtti, System.RegularExpressions,
  WiRL.Core.Context,
  WiRL.Core.Exceptions,
  WiRL.Rtti.Utils;

type
  EWiRLValidationError = class(EWiRLException);

  ConstraintAttribute = class(TCustomAttribute)
  private
    FValidatedBy: TClass;
  public
    constructor Create(AValidatedBy: TClass);
    property ValidatedBy: TClass read FValidatedBy;
  end;

  // A should be a TCustomAttribute but I can't enforce
  // that constraint due to a circular reference
  IConstraintValidator<A: class; T> = interface
    ['{3996A923-1F21-49AF-91F0-01992DB6E25B}']
    procedure Initialize(ConstraintAnnotation: A);
    function IsValid(AValue: T; Context: TWiRLContext): Boolean;
  end;

  // Every constraint attribute should inherited from this class
  TCustomConstraintAttribute = class(TCustomAttribute)
  private
    FValidator: IInterface;
  protected
    FErrorMessage: string;
  public
    function GetValidator<T>: IInterface;
    property ErrorMessage: string read FErrorMessage;
  end;

  // Built-in validators

  MaxAttribute = class;

  TMaxValidator = class(TInterfacedObject, IConstraintValidator<MaxAttribute, Integer>)
  private
    FMaxValue :Integer;
  public
    procedure Initialize(AMaxAttribute: MaxAttribute);
    function IsValid(AValue: Integer; Context: TWiRLContext): Boolean;
  end;

  [Constraint(TMaxValidator)]
  MaxAttribute = class(TCustomConstraintAttribute)
  private
    FMaxValue: Integer;
  public
    constructor Create(AMaxValue: Integer; const AMessage: string = '');
    property MaxValue: Integer read FMaxValue;
  end;

  MinAttribute = class;

  TMinValidator = class(TInterfacedObject, IConstraintValidator<MinAttribute, Integer>)
  private
    FMinValue :Integer;
  public
    procedure Initialize(AMinAttribute: MinAttribute);
    function IsValid(AValue: Integer; Context: TWiRLContext): Boolean;
  end;

  [Constraint(TMinValidator)]
  MinAttribute = class(TCustomConstraintAttribute)
  private
    FMinValue: Integer;
  public
    constructor Create(AMinValue: Integer; const AMessage: string = '');
    property MinValue: Integer read FMinValue;
  end;

  NotNullAttribute = class;

  TNotNullValidator = class(TInterfacedObject, IConstraintValidator<NotNullAttribute, string>)
  public
    procedure Initialize(ANotNullAttribute: NotNullAttribute);
    function IsValid(AValue: string; Context: TWiRLContext): Boolean;
  end;

  [Constraint(TNotNullValidator)]
  NotNullAttribute = class(TCustomConstraintAttribute)
  public
    constructor Create(const AMessage: string = '');
  end;

  PatternAttribute = class;

  TPatternValidator = class(TInterfacedObject, IConstraintValidator<PatternAttribute, string>)
  private
    FPattern: string;
  public
    procedure Initialize(APatternAttribute: PatternAttribute);
    function IsValid(AValue: string; Context: TWiRLContext): Boolean;
  end;

  [Constraint(TPatternValidator)]
  PatternAttribute = class(TCustomConstraintAttribute)
  private
    FPattern: string;
  public
    constructor Create(const APattern :string; const AMessage: string = '');
    property Pattern: string read FPattern;
  end;


implementation

{ Constraint }

constructor ConstraintAttribute.Create(AValidatedBy: TClass);
begin
  inherited Create;
  FValidatedBy := AValidatedBy;
end;

{ TCustomConstraintAttrubute }

function TCustomConstraintAttribute.GetValidator<T>: IInterface;
var
  LValidatorClass: TClass;
  LObj: TObject;
  LValidator :IConstraintValidator<TCustomAttribute, T>;
begin
  if not Assigned(FValidator) then // Should it be inside a critical section?
  begin
    TRttiHelper.HasAttribute<ConstraintAttribute>(TRttiHelper.Context.GetType(Self.ClassType),
      procedure (LAttr: ConstraintAttribute)
      begin
        LValidatorClass := LAttr.ValidatedBy;
      end);
    if LValidatorClass = nil then
      raise EWiRLValidationError.Create('Validation class not assigned');
    LObj := LValidatorClass.Create;
    if not Supports(LObj, IConstraintValidator<TCustomAttribute, T>, LValidator) then
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

function TMaxValidator.IsValid(AValue: Integer; Context: TWiRLContext): Boolean;
begin
  Result := AValue < FMaxValue;
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

function TMinValidator.IsValid(AValue: Integer; Context: TWiRLContext): Boolean;
begin
  Result := AValue > FMinValue;
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

function TNotNullValidator.IsValid(AValue: string;
  Context: TWiRLContext): Boolean;
begin
  Result := AValue <> '';
end;

{ TPatternValidator }

procedure TPatternValidator.Initialize(APatternAttribute: PatternAttribute);
begin
  FPattern := APatternAttribute.Pattern;
end;

function TPatternValidator.IsValid(AValue: string;
  Context: TWiRLContext): Boolean;
begin
  Result := TRegEx.IsMatch(AValue, FPattern);
end;

{ PatternAttribute }

constructor PatternAttribute.Create(const APattern, AMessage: string);
begin
  FErrorMessage := AMessage;
  FPattern := APattern;
end;

end.
