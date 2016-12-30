{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Messaging.Resource;

{$I WiRL.inc}

interface

uses
  SysUtils, Classes

  , WiRL.Core.JSON
  , WiRL.Core.Attributes
  , WiRL.Core.Token
  , WiRL.http.Accept.MediaType

  , WiRL.Messaging.Message
  , WiRL.Messaging.Queue

  ;

type
  TWiRLMessagingResourceForToken<T: TWiRLCustomMessage, constructor> = class
  private
  protected
  [Context] Token: TWiRLToken;
  public
    [Path('listen'), GET]
    [Produces(TMediaType.APPLICATION_JSON)]
    procedure Subscribe;

    [Path('myqueue'), GET]
    [Produces(TMediaType.APPLICATION_JSON)]
    function Consume: TJSONObject;
  end;


implementation

uses
  Generics.Collections;

{ TWiRLMessagingResourceForToken<T> }

function TWiRLMessagingResourceForToken<T>.Consume: TJSONObject;
var
  LCount: Integer;
  LArrayMessaggi: TJSONArray;
begin
  LCount := -1;

  LArrayMessaggi := TJSONArray.Create;
  try

    TWiRLMessagingQueueForToken.Use<T>(Token,
      procedure (AQueue: TQueue<T>)
      var
        LMessage: T;
      begin
        LCount := AQueue.Count;
        while AQueue.Count > 0 do
        begin
          LMessage := AQueue.Dequeue;
          try
            LArrayMessaggi.Add(LMessage.ToJSON);
          finally
            LMessage.Free;
          end;
        end;
      end
    );

    Result := TJSONObject.Create;
    try
      Result.AddPair('Count', TJSONNumber.Create(LCount));
      Result.AddPair('Messages', LArrayMessaggi);
    except
      Result.Free;
      raise;
    end;
  except
    LArrayMessaggi.Free;
    raise;
  end;

end;

procedure TWiRLMessagingResourceForToken<T>.Subscribe;
begin
  TWiRLMessagingQueueForToken.Create<T>(Token);
end;

end.
