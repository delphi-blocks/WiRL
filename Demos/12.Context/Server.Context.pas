{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Context;

interface

uses
  System.SysUtils, System.Classes, System.Rtti,
  WiRL.Rtti.Utils,
  WiRL.Core.Context,
  WiRL.Core.Context.Server,
  WiRL.Core.Injection;

type
  // Class to be injected
  TMyClass = class
  private
    FValue: Integer;
    FInfo: string;
  public
    property Value: Integer read FValue write FValue;
    property Info: string read FInfo write FInfo;
  end;

  // Attribute to check inside the factory
  ValueAttribute = class(TCustomAttribute)
  private
    FValue: Integer;
  public
    property Value: Integer read FValue;
    constructor Create(AValue: Integer);
  end;

  // The class factory is responsable to create the context.
  // It will be released by the system unless it's annotated
  // with the Singleton attribute
  TMyClassFactory = class(TInterfacedObject, IContextHttpFactory)
  public
    function CreateContextObject(const AObject: TRttiObject;
      AContext: TWiRLContextHttp): TValue;
  end;


implementation

uses
  WiRL.http.Request;

{ TMyClassFactory }

function TMyClassFactory.CreateContextObject(const AObject: TRttiObject;
  AContext: TWiRLContextHttp): TValue;
var
  LInstance: TMyClass;
begin
  LInstance := TMyClass.Create;
  LInstance.Value := 0;
  LInstance.Info := AContext.Request.PathInfo;
  TRttiHelper.HasAttribute<ValueAttribute>(AObject,
    procedure (LAttr: ValueAttribute)
    begin
      LInstance.Value := LAttr.Value;
    end
  );

  Result := LInstance;
end;

{ ValueAttribute }

constructor ValueAttribute.Create(AValue: Integer);
begin
  inherited Create;
  FValue := AValue;
end;

initialization
  TWiRLContextInjectionRegistry.Instance.RegisterFactory<TMyClass>(TMyClassFactory);

end.
