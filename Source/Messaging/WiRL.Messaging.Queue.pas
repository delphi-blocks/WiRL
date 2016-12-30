{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Messaging.Queue;

interface

uses
    Classes, SysUtils

  , Generics.Collections

  , WiRL.Messaging.Dispatcher
  , WiRL.Messaging.Subscriber
  , WiRL.Messaging.Message

  , WiRL.Stateful.Dictionary

  , WiRL.Core.Token
  ;

type
  TWiRLMessagingQueueForToken = class
  private
  protected
    class function GetQueueName<T: TWiRLCustomMessage>(const AQueueName: string = ''): string;
  public
    class procedure Create<T: TWiRLCustomMessage, constructor>(AToken: TWiRLToken; const AQueueName: string = '');
    class procedure Use<T: TWiRLCustomMessage>(AToken: TWiRLToken; const ADoSomething: TProc<TQueue<T>>; const AQueueName: string = '');
  end;

implementation

{ TWiRLMessagingQueue }

class procedure TWiRLMessagingQueueForToken.Create<T>(AToken: TWiRLToken;
  const AQueueName: string);
var
  LSubscriber: IWiRLMessageSubscriber;
  LDictionary: TWiRLStatefulDictionary;
  LQueueName: string;
begin
  LQueueName := GetQueueName<T>(AQueueName);

  LDictionary := TWiRLStatefulDictionaryRegistry.Instance.GetDictionaryForToken(AToken);
  LDictionary.Add(LQueueName, TQueue<T>.Create());

  LSubscriber := TWiRLAnonymousSubscriber<T>.Create(
    procedure(AMessage: T)
    begin
      LDictionary.Use<TQueue<T>>(LQueueName,
        procedure (AQueue: TQueue<T>)
        begin
          AQueue.Enqueue(T.Clone<T>(AMessage));
        end
      );
    end
  );

  TWiRLMessageDispatcher.Instance.RegisterSubscriber(LSubscriber);
end;

class function TWiRLMessagingQueueForToken.GetQueueName<T>(
  const AQueueName: string): string;
begin
  Result := AQueueName;
  if Result = '' then
    Result := 'MessageQueue.' + T.ClassName;
end;

class procedure TWiRLMessagingQueueForToken.Use<T>(AToken: TWiRLToken;
  const ADoSomething: TProc<TQueue<T>>; const AQueueName: string);
var
  LDictionary: TWiRLStatefulDictionary;
begin
  LDictionary := TWiRLStatefulDictionaryRegistry.Instance.GetDictionaryForToken(AToken);

  LDictionary.Use<TQueue<T>>(GetQueueName<T>(AQueueName), ADoSomething);
end;

end.
