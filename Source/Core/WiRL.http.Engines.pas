{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2023 WiRL Team                                      }
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
  private const
    DefaultEngineName = 'WiRL Http Engine';
  private
    FOnExecute: THttpEngineExecuteEvent;
  public
    constructor Create(AOwner: TComponent); override;
  public
    function SetEngineName(const AEngineName: string): TWiRLhttpEngine;

    procedure HandleRequest(AContext: TWiRLContext); override;
  published
    property OnExecute: THttpEngineExecuteEvent read FOnExecute write FOnExecute;
  end;

implementation

{ TWiRLhttpEngine }

constructor TWiRLhttpEngine.Create(AOwner: TComponent);
begin
  inherited;
  FEngineName := DefaultEngineName;
end;

procedure TWiRLhttpEngine.HandleRequest(AContext: TWiRLContext);
begin
  inherited;
  if Assigned(FOnExecute) then
    FOnExecute(AContext, AContext.Request, AContext.Response);
end;

function TWiRLhttpEngine.SetEngineName(const AEngineName: string): TWiRLhttpEngine;
begin
  FEngineName := AEngineName;
  Result := Self;
end;

end.
