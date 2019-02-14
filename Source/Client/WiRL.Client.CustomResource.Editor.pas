{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Client.CustomResource.Editor;

interface

uses
  System.Classes, System.SysUtils,
  DesignEditors,
  WiRL.Client.CustomResource;

type
  TWiRLClientCustomResourceEditor = class(TComponentEditor)
  private
    function CurrentObj: TWiRLClientCustomResource;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure Register;

implementation

uses
  Vcl.Dialogs, DesignIntf, Winapi.Windows;

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
    3: CurrentObj.PUT(nil, nil, nil);
    4: CurrentObj.PATCH(nil, nil, nil);
    5: CurrentObj.HEAD(nil, nil, nil);
    6: CurrentObj.OPTIONS(nil, nil, nil);
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
    3: Result := 'PUT';
    4: Result := 'PATCH';
    5: Result := 'HEAD';
    6: Result := 'OPTIONS';
  end;
end;

function TWiRLClientCustomResourceEditor.GetVerbCount: Integer;
begin
  Result := 7;
end;

end.


