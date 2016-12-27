(*
  Copyright 2015-2016, WiRL - REST Library

  Home: https://github.com/WiRL-library

*)
unit Server.Resources.StaticFiles;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils,

  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  WiRL.Core.Response,
  WiRL.WebServer.Resources;


type
  [Path('static')]
  TStaticFileResources = class(TFileSystemResource)
  public
    constructor Create; override;
  end;

implementation

uses
  WiRL.Core.Registry;

{ THelloWorldResource }

constructor TStaticFileResources.Create;
begin
  inherited;
  RootFolder := TPath.GetDirectoryName(ParamStr(0)) + PathDelim + 'www';
  IncludeSubFolders := True;
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TStaticFileResources>(
    function: TObject
    begin
      Result := TStaticFileResources.Create;
    end
  );

end.
