{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Persistence.Core;

interface

uses
  System.SysUtils, System.Classes, System.Rtti, System.SyncObjs;

type
  ENeonException = class(Exception);

  TNeonConfiguration = class

  end;

  TNeonBase = class
  protected
    FErrors: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LogError(const AMessage: string);
  public
    property Errors: TStrings read FErrors write FErrors;
  end;

implementation

{ TNeonBase }

constructor TNeonBase.Create;
begin
  FErrors := TStringList.Create;
end;

destructor TNeonBase.Destroy;
begin
  FErrors.Free;
  inherited;
end;

procedure TNeonBase.LogError(const AMessage: string);
begin
  FErrors.Add(AMessage);
end;

end.
