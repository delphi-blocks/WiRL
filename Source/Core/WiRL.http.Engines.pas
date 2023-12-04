{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
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
  WiRL.Core.Context.Server;

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

implementation

{ TWiRLhttpEngine }

procedure TWiRLhttpEngine.HandleRequest(AContext: TWiRLContext);
begin
  inherited;
  if Assigned(FOnExecute) then
    FOnExecute(AContext, AContext.Request, AContext.Response);
end;

end.
