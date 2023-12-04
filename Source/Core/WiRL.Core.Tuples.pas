{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2023 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Tuples;

interface

uses
  System.SysUtils, System.Rtti,
  System.Generics.Defaults, System.Generics.Collections;

type
  /// <summary>
  ///   Simple interface for a Tuple used in REST (HTTP) communication. It's
  ///   not a general purpose Tuple but it's used to get Entities and Errors in
  ///   a GET/POST/PUT/PATCH command
  /// </summary>
  IWiRLTuple<T, E> = interface
    function GetEntity: T;
    procedure SetEntity(Value: T);
    function GetError: E;
    procedure SetError(Value: E);

    property Entity: T read GetEntity write SetEntity;
    property Error: E read GetError write SetError;
  end;

  /// <summary>
  ///   Default IWiRLTuple implementation
  /// </summary>
  TWiRLTuple<T, E> = class(TInterfacedObject, IWiRLTuple<T, E>)
  private
    FEntity: T;
    FError: E;
    FOwnObjects: Boolean;
  protected
    function GetEntity: T;
    procedure SetEntity(AValue: T);
    function GetError: E;
    procedure SetError(AValue: E);
  public
    constructor Create(); overload;
    constructor Create(AOwnObjects: Boolean); overload;
    constructor Create(AEntity: T; AError: E; AOwnObjects: Boolean); overload;
    destructor Destroy; override;

    property Entity: T read GetEntity write SetEntity;
    property Error: E read GetError write SetError;
  end;

implementation

{ TWiRLTuple<T, E> }

constructor TWiRLTuple<T, E>.Create(AOwnObjects: Boolean);
begin
  FOwnObjects := AOwnObjects;
end;

constructor TWiRLTuple<T, E>.Create(AEntity: T; AError: E; AOwnObjects: Boolean);
begin
  FEntity := AEntity;
  FError := AError;
  FOwnObjects := AOwnObjects;
end;

constructor TWiRLTuple<T, E>.Create;
begin
  Create(False);
end;

destructor TWiRLTuple<T, E>.Destroy;
var
  LEntityHolder, LErrorHolder: TValue;
begin
  if FOwnObjects then
  begin
    LEntityHolder := TValue.From<T>(FEntity);
    if LEntityHolder.IsObject then
      LEntityHolder.AsObject.Free;

    LErrorHolder := TValue.From<E>(FError);
    if LErrorHolder.IsObject then
      LErrorHolder.AsObject.Free;
  end;

  inherited;
end;

function TWiRLTuple<T, E>.GetEntity: T;
begin
  Result := FEntity;
end;

function TWiRLTuple<T, E>.GetError: E;
begin
  Result := FError;
end;

procedure TWiRLTuple<T, E>.SetEntity(AValue: T);
begin
  FEntity := AValue;
end;

procedure TWiRLTuple<T, E>.SetError(AValue: E);
begin
  FError := AValue;
end;

end.
