{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Diagnostics.Resources;

{$I WiRL.inc}

interface

uses
  System.Classes, System.SysUtils,

  WiRL.Core.JSON,
  WiRL.Core.Registry,
  WiRL.Core.Classes,
  WiRL.Core.Application,
  WiRL.Core.Declarations,
  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  WiRL.Core.MessageBodyWriter,
  WiRL.Core.Auth.Context,
  WiRL.Core.URL,
  WiRL.Core.Engine,
  WiRL.Diagnostics.Manager;

type
  [Path('manager')]
  TDiagnosticsResource = class
  private
  protected
    [Context] URL: TWiRLURL;
  public
    [GET]
    [Produces(TMediaType.APPLICATION_JSON)]
    function RetrieveAll: TJSONObject;

    [GET][Path('app')]
    [Produces(TMediaType.APPLICATION_JSON)]
    function RetrieveApp: TJSONObject;
  end;

  [Path('resources')]
  TResourcesResource = class
  private
  protected
    [Context] Engine: TWiRLEngine;
  public
    [GET]
    [Produces(TMediaType.APPLICATION_JSON)]
    function RetrieveAll: TJSONValue;
  end;

implementation

{ TDiagnosticsResource }

function TDiagnosticsResource.RetrieveAll: TJSONObject;
begin
  Result := TWiRLDiagnosticsManager.Instance.ToJSON;
end;

function TDiagnosticsResource.RetrieveApp: TJSONObject;

  function ToString(const AArray: TArray<string>): string;
  var
    LString: string;
  begin
    Result := '';
    for LString in AArray do
    begin
      if Result <> '' then
        Result := Result + ', ';
      Result := Result + LString;
    end;
  end;

var
  LObj: TJSONObject;
  LAppName: string;
begin
  if URL.HasSubResources then
    LAppName := URL.SubResources[0]
  else
    raise Exception.Create('No app name provided');

  LObj := nil;
  TWiRLDiagnosticsManager.Instance.RetrieveAppInfo(LAppName,
    procedure(AInfo: TWiRLDiagnosticAppInfo)
    begin
      LObj := AInfo.ToJSON;
    end
  );

  LObj.AddPair('app', LAppName);
  Result := LObj;
end;

{ TResourcesResource }

function TResourcesResource.RetrieveAll: TJSONValue;
var
  LApplications: TJSONArray;
begin
  LApplications := TJSONArray.Create;
  Engine.EnumerateApplications(
    procedure(AName: string; AApplication: TWiRLApplication)
    var
      LObj: TJSONObject;
      LResources: TJSONArray;
      LResource: string;
    begin
      LResources := TJSONArray.Create;

      for LResource in AApplication.Resources do
        LResources.Add(LResource);

      LObj := TJSONObject.Create;
      LObj.AddPair(AName, LResources);
      LApplications.AddElement(LObj);
    end
  );

  Result := LApplications;
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TDiagnosticsResource>(nil);
  TWiRLResourceRegistry.Instance.RegisterResource<TResourcesResource>(nil);

end.
