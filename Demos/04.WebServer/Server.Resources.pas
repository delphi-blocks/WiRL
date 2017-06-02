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
  WiRL.Core.MessageBody.Default,
  WiRL.WebServer.Resources;

type
  [Path('home')]
  TStaticWebResource = class(TFileSystemResource)
  public
    constructor Create; override;
  end;

implementation

uses
  WiRL.Core.Registry;

{ TStaticWebResource }

constructor TStaticWebResource.Create;
begin
  inherited;
  RootFolder := TDirectory.GetParent(
    TPath.GetDirectoryName(ParamStr(0))
  ) + PathDelim + 'www';
  IncludeSubFolders := True;
end;


initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TStaticWebResource>;

end.
