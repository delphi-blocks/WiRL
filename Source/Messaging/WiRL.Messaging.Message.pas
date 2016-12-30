{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Messaging.Message;

{$I WiRL.inc}

interface

uses
  Classes, SysUtils
  , Rtti
  , WiRL.Core.JSON
  ;

type
  TWiRLMessage = class
  private
    FCreationDateTime: TDateTime;
  public
    constructor Create(); virtual;
    procedure Assign(ASource: TWiRLMessage); virtual;
    function ToJSON: TJSONObject; virtual;

    property CreationDateTime: TDateTime read FCreationDateTime;
  end;

  TWiRLCustomMessage = class(TWiRLMessage)
  private
  public
    class function Clone<T: TWiRLMessage, constructor>(ASource: T): T;
  end;

  TWiRLStringMessage = class(TWiRLCustomMessage)
  private
    FValue: string;
  public
    constructor Create(const AValue: string); reintroduce;
    procedure Assign(ASource: TWiRLMessage); override;
    function ToJSON: TJSONObject; override;

    property Value: string read FValue write FValue;
  end;

  TWiRLJSONObjectMessage = class(TWiRLCustomMessage)
  private
    FValue: TJSONObject;
    procedure SetValue(const AValue: TJSONObject);
  public
    constructor Create(AValue: TJSONObject); reintroduce;
    destructor Destroy; override;

    procedure Assign(ASource: TWiRLMessage); override;
    function ToJSON: TJSONObject; override;

    property Value: TJSONObject read FValue write SetValue;
  end;


implementation

uses
  DateUtils
  , WiRL.Core.Utils
  ;

{ TWiRLMessage }

procedure TWiRLMessage.Assign(ASource: TWiRLMessage);
begin
  FCreationDateTime := ASource.FCreationDateTime;
end;

constructor TWiRLMessage.Create();
begin
  inherited Create;
  FCreationDateTime := Now;
end;

function TWiRLMessage.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('MessageType', ClassName);
  Result.AddPair('CreationDateTime', DateToISO8601(CreationDateTime));
end;

{ TWiRLCustomMessage }

class function TWiRLCustomMessage.Clone<T>(ASource: T): T;
begin
  Result := T.Create;
  Result.Assign(ASource);
end;


{ TWiRLStringMessage }

procedure TWiRLStringMessage.Assign(ASource: TWiRLMessage);
begin
  inherited;
  if ASource is TWiRLStringMessage then
    Self.FValue := TWiRLStringMessage(ASource).FValue;
end;

constructor TWiRLStringMessage.Create(const AValue: string);
begin
  inherited Create;
  FValue := AValue;
end;

function TWiRLStringMessage.ToJSON: TJSONObject;
begin
  Result := inherited ToJSON;
  Result.AddPair('Value', Value);
end;

{ TWiRLJSONObjectMessage }

procedure TWiRLJSONObjectMessage.Assign(ASource: TWiRLMessage);
begin
  inherited;
  if ASource is TWiRLJSONObjectMessage then
  begin
    Value := TWiRLJSONObjectMessage(ASource).Value;
  end;
end;

constructor TWiRLJSONObjectMessage.Create(AValue: TJSONObject);
begin
  inherited Create;
  Value := AValue;
end;

destructor TWiRLJSONObjectMessage.Destroy;
begin
  FValue.Free;
  inherited;
end;

procedure TWiRLJSONObjectMessage.SetValue(const AValue: TJSONObject);
begin
  if FValue <> AValue then
    FValue := AValue.Clone as TJSONObject;
end;

function TWiRLJSONObjectMessage.ToJSON: TJSONObject;
begin
  Result := inherited ToJSON;
  Result.AddPair('Value', Value);
end;

end.
