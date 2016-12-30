{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Messaging.Dispatcher;

{$I WiRL.inc}

interface

uses
  Classes, SysUtils
  , Rtti
  , Generics.Collections
{$ifdef DelphiXE7_UP}
  , Threading
{$endif}
  , SyncObjs

  , WiRL.Core.Singleton
  , WiRL.Core.Utils
  , WiRL.Messaging.Message
  , WiRL.Messaging.Subscriber
  ;

type
  TWiRLMessageDispatcher = class
  private
    type
      TWiRLMessageDispatcherSingleton = TWiRLSingleton<TWiRLMessageDispatcher>;
  private
    FSubscribers: TList<IWiRLMessageSubscriber>;
    FQueue: TThreadedQueue<TWiRLMessage>;
    FCriticalSection: TCriticalSection;
{$ifdef DelphiXE7_UP}
    FWorkerTask: ITask;
{$endif}
  protected
    class function GetInstance: TWiRLMessageDispatcher; static; inline;

    procedure DoRegisterSubscriber(const ASubscriber: IWiRLMessageSubscriber); virtual;
    procedure DoUnRegisterSubscriber(const ASubscriber: IWiRLMessageSubscriber); virtual;

    property Subscribers: TList<IWiRLMessageSubscriber> read FSubscribers;
  public
    const MESSAGE_QUEUE_DEPTH = 100;
    constructor Create; virtual;
    destructor Destroy; override;

    function Enqueue(AMessage: TWiRLMessage): Integer;
    procedure RegisterSubscriber(const ASubscriber: IWiRLMessageSubscriber);
    procedure UnRegisterSubscriber(const ASubscriber: IWiRLMessageSubscriber);

    class property Instance: TWiRLMessageDispatcher read GetInstance;
  end;


implementation

{ TWiRLMessageDispatcher }

constructor TWiRLMessageDispatcher.Create;
begin
  TWiRLMessageDispatcherSingleton.CheckInstance(Self);

  inherited Create();

  FSubscribers := TList<IWiRLMessageSubscriber>.Create;
  FQueue := TThreadedQueue<TWiRLMessage>.Create(MESSAGE_QUEUE_DEPTH);
  FCriticalSection := TCriticalSection.Create;

{$ifdef DelphiXE7_UP}
  FWorkerTask := TTask.Create(
    procedure
    var
      LMessage: TWiRLMessage;
      LSubscriber: IWiRLMessageSubscriber;
    begin
      while TTask.CurrentTask.Status = TTaskStatus.Running do
      begin
        // pop
        LMessage := FQueue.PopItem;

        if Assigned(LMessage) then // this can be async
        try
          // dispatch
          for LSubscriber in Subscribers do
          begin
            try
              LSubscriber.OnMessage(LMessage);
            except
              // handle errors? ignore errors?
            end;
          end;
        finally
          LMessage.Free;
        end;

      end;
    end
  );

  FWorkerTask.Start;
{$endif}
end;

destructor TWiRLMessageDispatcher.Destroy;
begin
{$ifdef DelphiXE7_UP}
  if Assigned(FWorkerTask) and (FWorkerTask.Status < TTaskStatus.Canceled) then
    FWorkerTask.Cancel;
{$endif}

  FCriticalSection.Free;
  FSubscribers.Free;
  FQueue.Free;

  inherited;
end;

procedure TWiRLMessageDispatcher.DoRegisterSubscriber(
  const ASubscriber: IWiRLMessageSubscriber);
begin
  FSubscribers.Add(ASubscriber);
end;

procedure TWiRLMessageDispatcher.DoUnRegisterSubscriber(
  const ASubscriber: IWiRLMessageSubscriber);
begin
  FSubscribers.Remove(ASubscriber);
end;

function TWiRLMessageDispatcher.Enqueue(AMessage: TWiRLMessage): Integer;
begin
  FQueue.PushItem(AMessage, Result);
end;

class function TWiRLMessageDispatcher.GetInstance: TWiRLMessageDispatcher;
begin
  Result := TWiRLMessageDispatcherSingleton.Instance;
end;

procedure TWiRLMessageDispatcher.RegisterSubscriber(
  const ASubscriber: IWiRLMessageSubscriber);
begin
  DoRegisterSubscriber(ASubscriber);
end;

procedure TWiRLMessageDispatcher.UnRegisterSubscriber(
  const ASubscriber: IWiRLMessageSubscriber);
begin
  DoUnRegisterSubscriber(ASubscriber);
end;

end.
