unit WiRL.http.Engines;

interface

uses
  System.SysUtils, System.Classes, System.StrUtils,
  WiRL.Core.Context;

type
  TWiRLCustomEngine = class abstract
  private
    FBasePath: string;
    procedure SetBasePath(const Value: string);
  public
    constructor Create(const ABasePath: string); virtual;

    procedure HandleRequest(AContext: TWiRLContext); virtual; abstract;
    procedure Startup; virtual;
    procedure Shutdown; virtual;

    property BasePath: string read FBasePath write SetBasePath;
  end;

implementation

{ TWiRLCustomEngine }

constructor TWiRLCustomEngine.Create(const ABasePath: string);
begin
  inherited Create;
  FBasePath := ABasePath;
end;

procedure TWiRLCustomEngine.SetBasePath(const Value: string);
begin
  if StartsText('/', Value) then
    BasePath := Value
  else
    BasePath := '/' + Value;
end;

procedure TWiRLCustomEngine.Shutdown;
begin
end;

procedure TWiRLCustomEngine.Startup;
begin
end;

end.
