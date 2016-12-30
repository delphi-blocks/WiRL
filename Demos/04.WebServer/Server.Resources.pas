{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Resources;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils,

  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  WiRL.Core.Response,
  WiRL.WebServer.Resources;

type
  [Path('helloworld')]
  THelloWorldResource = class(TFileSystemResource)
  public
    constructor Create; override;
  end;

implementation

uses
  WiRL.Core.Registry;

{ THelloWorldResource }

constructor THelloWorldResource.Create;
begin
  inherited;
  RootFolder := TDirectory.GetParent(
    TDirectory.GetParent(
      TPath.GetDirectoryName(ParamStr(0))
    )
  ) + PathDelim + 'www';
  IncludeSubFolders := True;
end;


initialization
  TWiRLResourceRegistry.Instance.RegisterResource<THelloWorldResource>;

end.
