{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2024 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Engine.HTTP;

interface

uses
  System.SysUtils, System.Classes,

  WiRL.Engine.Core,
  WiRL.http.Server,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.Core.Context.Server;

type
  THTTPEngineExecuteEvent = procedure(AContext: TWiRLContext; ARequest:
      TWiRLRequest; AResponse: TWiRLResponse) of object;

  TWiRLHTTPEngine = class(TWiRLCustomEngine)
  private const
    DefaultEngineName = 'WiRL Simple HTTP Engine';
  private
    FOnExecute: THTTPEngineExecuteEvent;
  public
    constructor Create(AOwner: TComponent); override;
  public
    function SetEngineName(const AEngineName: string): TWiRLHTTPEngine;

    procedure HandleRequest(AContext: TWiRLContext); override;
  published
    property OnExecute: THTTPEngineExecuteEvent read FOnExecute write FOnExecute;
  end;

implementation

{ TWiRLHTTPEngine }

constructor TWiRLHTTPEngine.Create(AOwner: TComponent);
begin
  inherited;
  FEngineName := DefaultEngineName;
end;

procedure TWiRLHTTPEngine.HandleRequest(AContext: TWiRLContext);
begin
  inherited;
  if Assigned(FOnExecute) then
    FOnExecute(AContext, AContext.Request, AContext.Response);
end;

function TWiRLHTTPEngine.SetEngineName(const AEngineName: string): TWiRLHTTPEngine;
begin
  FEngineName := AEngineName;
  Result := Self;
end;

end.
