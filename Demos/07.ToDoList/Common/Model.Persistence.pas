(*
  Copyright 2015-2016, WiRL - REST Library

  Home: https://github.com/WiRL-library

*)
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
