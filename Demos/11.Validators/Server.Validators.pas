unit Server.Validators;

interface

uses
  System.SysUtils, System.Classes,
  WiRL.Core.Context,
  WiRL.Core.Attributes,
  WiRL.Core.Validators, Server.Consts;

type
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
    FErrorMessage: string;
  protected
    function GetErrorMessage: string; override;
  public
    constructor Create(AMaxValue: Integer; const AMessage: string = '');
    property MaxValue: Integer read FMaxValue;
  end;

implementation

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

function MaxAttribute.GetErrorMessage: string;
begin
  // Direct use of the error message:
  // Result := FErrorMessage;
  // Get the message through a transated dictionary:
  Result := StringResources.GetString(FErrorMessage);
end;

end.
