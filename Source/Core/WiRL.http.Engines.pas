{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Engines;

interface

uses
  System.SysUtils, System.Classes,
  WiRL.http.Server,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.Core.Context;

type
  THttpEngineExecuteEvent = procedure (AContext: TWiRLContext; ARequest: TWiRLRequest; AResponse: TWiRLResponse) of object;

  TWiRLhttpEngine = class(TWiRLCustomEngine)
  private
    FOnExecute: THttpEngineExecuteEvent;
  public
    procedure HandleRequest(AContext: TWiRLContext); override;
  published
    property OnExecute: THttpEngineExecuteEvent read FOnExecute write FOnExecute;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('WiRL Server', [TWiRLhttpEngine]);
end;

{ TWiRLhttpEngine }

procedure TWiRLhttpEngine.HandleRequest(AContext: TWiRLContext);
begin
  inherited;
  if Assigned(FOnExecute) then
    FOnExecute(AContext, AContext.Request, AContext.Response);
end;

end.
