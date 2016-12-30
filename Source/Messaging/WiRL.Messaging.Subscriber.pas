{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Messaging.Subscriber;

interface

uses
  Classes, SysUtils

  , WiRL.Messaging.Message
  , WiRL.Core.Classes

  ;

type
  IWiRLMessageSubscriber = interface
    procedure OnMessage(AMessage: TWiRLMessage);
  end;

  TWiRLAnonymousSubscriber<T: TWiRLCustomMessage> = class(TNonInterfacedObject, IWiRLMessageSubscriber)
  private
    FProc: TProc<T>;
  protected
  public
    constructor Create(const AProc: TProc<T>); virtual;

    procedure OnMessage(AMessage: TWiRLMessage);
  end;


implementation

{ TWiRLAnonymousSubscriber }

constructor TWiRLAnonymousSubscriber<T>.Create(const AProc: TProc<T>);
begin
  inherited Create;
  FProc := AProc;
end;

procedure TWiRLAnonymousSubscriber<T>.OnMessage(AMessage: TWiRLMessage);
begin
  if Assigned(FProc) and (AMessage is T) then
  begin
    FProc(AMessage as T);
  end;
end;

end.
