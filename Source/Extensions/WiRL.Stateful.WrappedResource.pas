{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Stateful.WrappedResource;

interface

uses
  System.Classes, System.SysUtils,
  WiRL.Core.Auth.Context,
  WiRL.Core.Attributes,
  WiRL.Stateful.Dictionary;

type
  PerSessionAttribute = class(TCustomAttribute)
  private
    FIdentifier: string;
  public
    constructor Create(const APerSessionIdentifier: string);
    property Identifier: string read FIdentifier;
  end;

  PerRequestAttribute = class(TCustomAttribute);
  SingletonAttribute = class(TCustomAttribute);

  TWrappedResource<T: class, constructor> = class
  protected
    [Context]
    Token: TWiRLAuthContext;

    FInstance: T;
    function CreateInstance: T; virtual;
    function Dictionary: TWiRLStatefulDictionary;
    procedure EnsureInstance(const AIdentifier: string); virtual;
    procedure WithInstance(const ADoSomething: TProc<T>); virtual;
  public

  end;


implementation

uses
  WiRL.Rtti.Utils;

{ TWrappedResource<T> }

function TWrappedResource<T>.CreateInstance: T;
begin
  Result := T.Create;
end;

function TWrappedResource<T>.Dictionary: TWiRLStatefulDictionary;
begin
  Result := TWiRLStatefulDictionaryRegistry.Instance.GetDictionaryForToken(Token);
end;

procedure TWrappedResource<T>.EnsureInstance(const AIdentifier: string);
var
  LDictionary: TWiRLStatefulDictionary;
begin
  LDictionary := Dictionary;
  if LDictionary.Contains(AIdentifier) = False then
    LDictionary.Add(AIdentifier, CreateInstance);
end;

procedure TWrappedResource<T>.WithInstance(const ADoSomething: TProc<T>);
begin
  // PerSession handling
  TRttiHelper.IfHasAttribute<PerSessionAttribute>(
    Self,
    procedure (AAttrib: PerSessionAttribute)
    begin
      EnsureInstance(AAttrib.Identifier);

      Dictionary.Use<T>(
        AAttrib.Identifier,
        procedure (AInstance: T)
        begin
          if Assigned(ADoSomething) then
            ADoSomething(AInstance);
        end
      );
    end
  );

  // Singleton handling
  {TODO}
end;

{ PerSessionAttribute }

constructor PerSessionAttribute.Create(const APerSessionIdentifier: string);
begin
  FIdentifier := APerSessionIdentifier;
  inherited Create;
end;

end.
