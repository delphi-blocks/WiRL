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
  WiRL.http.Client.Interfaces,
  WiRL.Core.MessageBody.Default,
  WiRL.Client.CustomResource,
  WiRL.Client.Resource.Obj;

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
var
  LRequestObject, LResponseObject: TComponent;
begin
  inherited;

  if Assigned(CurrentObj.Application) then
  begin
    CurrentObj.Application.SetWriters('*.*');
    CurrentObj.Application.SetReaders('*.*');
  end;

  if CurrentObj is TWiRLClientResourceObject then
  begin
    LResponseObject := TWiRLClientResourceObject(CurrentObj).ResponseObject;
    LRequestObject := TWiRLClientResourceObject(CurrentObj).RequestObject;

    case Index of
      0: CurrentObj.GenericGet(LResponseObject);
      1: CurrentObj.GenericPost(LRequestObject, LResponseObject);
      2: CurrentObj.GenericDelete(LResponseObject);
      3: CurrentObj.GenericPut(LRequestObject, LResponseObject);
      4: CurrentObj.GenericPatch(LRequestObject, LResponseObject);
      5: CurrentObj.GenericHttpRequest('HEAD', LRequestObject, LResponseObject);
      6: CurrentObj.GenericHttpRequest('OPTIONS', LRequestObject, LResponseObject);
    end;
  end
  else
  begin
    case Index of
      0: CurrentObj.GET(nil, nil, nil);
      1: CurrentObj.POST(nil, nil, nil);
      2: CurrentObj.DELETE(nil, nil, nil);
      3: CurrentObj.PUT(nil, nil, nil);
      4: CurrentObj.PATCH(nil, nil, nil);
      5: CurrentObj.HEAD(nil, nil, nil);
      6: CurrentObj.OPTIONS(nil, nil, nil);
    end;
  end;

  if (GetKeyState(VK_LSHIFT) < 0) then
    //ShowMessage(LResponse.StatusText);
    ShowMessage('Done');

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


