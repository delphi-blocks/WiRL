{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2023 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Client.CustomResource.Editor;

interface

uses
  System.Classes, System.SysUtils,
  DesignEditors, DesignIntf,

  WiRL.http.Headers,
  WiRL.http.Client.Interfaces,
  WiRL.Core.MessageBody.Default,
  WiRL.Client.CustomResource;

type
  TWiRLClientCustomResourceEditor = class(TComponentEditor)
  private
    function CurrentObj: TWiRLClientCustomResource;
    procedure RunResourceEditor;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure Edit; override;
  end;

  THeadersProperty = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    function GetValue: string; override;
  end;

procedure Register;

implementation

uses
  Vcl.Dialogs, Winapi.Windows,
  WiRL.Client.ResourceDebugger,
  WiRL.Client.ResourceHeaderEditor;

procedure Register;
begin
  RegisterComponentEditor(TWiRLClientCustomResource, TWiRLClientCustomResourceEditor);
end;

{ TWiRLClientCustomResourceEditor }

function TWiRLClientCustomResourceEditor.CurrentObj: TWiRLClientCustomResource;
begin
  Result := Component as TWiRLClientCustomResource;
end;

procedure TWiRLClientCustomResourceEditor.Edit;
begin
  inherited;
  RunResourceEditor;
end;

procedure TWiRLClientCustomResourceEditor.ExecuteVerb(Index: Integer);
begin
  RunResourceEditor;
end;

procedure TWiRLClientCustomResourceEditor.RunResourceEditor;
begin
  inherited;
//  if Assigned(CurrentObj.Application) then
//  begin
//    CurrentObj.Application.SetWriters('*.*');
//    CurrentObj.Application.SetReaders('*.*');
//  end;

  TWiRLResourceRunnerForm.Edit(CurrentObj);

  Designer.Modified;
end;

function TWiRLClientCustomResourceEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Resource debugger';
  end;
end;

function TWiRLClientCustomResourceEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ THeadersProperty }

procedure THeadersProperty.Edit;
var
  LHeaders: IWiRLHeaders;
begin
  inherited;
  LHeaders := GetIntfValue() as IWiRLHeaders;
  TFormHeadersEditor.Execute(Designer, LHeaders);
end;

function THeadersProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function THeadersProperty.GetValue: string;
const
  HeadersValue = '<Headers>';
var
  LHeaders: IWiRLHeaders;
begin
  LHeaders := GetIntfValue() as IWiRLHeaders;
  if LHeaders.Count > 0 then
    Result := HeadersValue.ToUpper
  else
    Result := HeadersValue;
end;

end.


