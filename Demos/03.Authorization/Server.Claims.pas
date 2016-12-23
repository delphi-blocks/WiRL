unit Server.Claims;

interface

uses
  System.Classes,
  WiRL.Core.JSON,
  WiRL.Core.Auth.Context;

type
  // Custom Claims Class
  TServerClaims = class(TWiRLSubject)
  private
    const CLAIM_GROUP = 'group';
    const CLAIM_LANGUAGE = 'language';
  private
    function GetGroup: string;
    function GetLanguage: string;
    procedure SetLanguage(const Value: string);
    procedure SetGroup(const Value: string);
  public
    property Group: string read GetGroup write SetGroup;
    property Language: string read GetLanguage write SetLanguage;
  end;

implementation

uses
  JOSE.Types.JSON,
  JOSE.Core.Base;

{ TServerClaims }

function TServerClaims.GetGroup: string;
begin
  Result := TJSONUtils.GetJSONValue(CLAIM_GROUP, FJSON).AsString;
end;

function TServerClaims.GetLanguage: string;
begin
  Result := TJSONUtils.GetJSONValue(CLAIM_LANGUAGE, FJSON).AsString;
end;

procedure TServerClaims.SetGroup(const Value: string);
begin
  if Value = '' then
    TJSONUtils.RemoveJSONNode(CLAIM_GROUP, FJSON)
  else
    TJSONUtils.SetJSONValueFrom<string>(CLAIM_GROUP, Value, FJSON);
end;

procedure TServerClaims.SetLanguage(const Value: string);
begin
  if Value = '' then
    TJSONUtils.RemoveJSONNode(CLAIM_LANGUAGE, FJSON)
  else
    TJSONUtils.SetJSONValueFrom<string>(CLAIM_LANGUAGE, Value, FJSON);
end;

end.
