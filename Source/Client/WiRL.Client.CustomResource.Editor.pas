{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Client.CustomResource.Editor;

interface

uses
  Classes, SysUtils
  , DesignEditors
  , WiRL.Client.CustomResource;

type
  TWiRLClientCustomResourceEditor = class(TComponentEditor)
  private
    function CurrentObj: TWiRLClientCustomResource;
  protected
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure Register;

implementation

uses
  Dialogs
  , DesignIntf
  , Windows;

procedure Register;
begin
  RegisterComponentEditor(TWiRLClientCustomResource, TWiRLClientCustomResourceEditor);
end;

{ TWiRLClientCustomResourceEditor }

function TWiRLClientCustomResourceEditor.CurrentObj: TWiRLClientCustomResource;
begin
  Result := Component as TWiRLClientCustomResource;
end;

procedure TWiRLClientCustomResourceEditor.ExecuteVerb(Index: Integer);
begin
  inherited;

  case Index of
    0: CurrentObj.GET(nil, nil, nil);
    1: CurrentObj.POST(nil, nil, nil);
    2: CurrentObj.DELETE(nil, nil, nil);
//    3: CurrentObj.PUT;
//    4: CurrentObj.PATCH;
//    5: CurrentObj.HEAD;
//    6: CurrentObj.OPTIONS;
  end;

  if (GetKeyState(VK_LSHIFT) < 0) then
    ShowMessage(CurrentObj.Client.ResponseText);

  Designer.Modified;
end;

function TWiRLClientCustomResourceEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'GET';
    1: Result := 'POST';
    2: Result := 'DELETE';
//    3: Result := 'PUT';
//    4: Result := 'PATCH';
//    5: Result := 'HEAD';
//    6: Result := 'OPTIONS';
  end;
end;

function TWiRLClientCustomResourceEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

end.


