{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Model.Persistence;

interface

uses
  Model;

type
  IPersistor<T, K> = interface
    function New(const AValue: T): K;
    function Retrieve(const AID: K): T;
    procedure Update(const AValue: T);
    procedure Delete(const AID: K);
  end;

implementation

end.
