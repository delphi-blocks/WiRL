(*
  Copyright 2015-2016, WiRL - REST Library

  Home: https://github.com/WiRL-library

*)
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
