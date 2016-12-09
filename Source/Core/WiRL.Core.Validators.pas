unit WiRL.Core.Validators;

interface

uses
  System.SysUtils, System.Classes, System.Rtti,
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
    function GetErrorMessage: string; virtual;
  public
    function GetValidator<T>: IInterface;
    property ErrorMessage: string read GetErrorMessage;
  end;

implementation

{ Constraint }

constructor ConstraintAttribute.Create(AValidatedBy: TClass);
begin
  inherited Create;
  FValidatedBy := AValidatedBy;
end;

{ TCustomConstraintAttrubute }

function TCustomConstraintAttribute.GetErrorMessage: string;
begin
  Result := '';
end;

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

end.
