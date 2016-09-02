(*
  Copyright 2015-2016, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit Server.Resources.StaticFiles;

interface

uses
  SysUtils, Classes, IOUtils

  , MARS.Core.Attributes
  , MARS.Core.MediaType
  , MARS.Core.Response

  , MARS.WebServer.Resources
  ;

type
  [Path('static')]
  TStaticFileResources = class(TFileSystemResource)
  public
    constructor Create; override;
  end;

implementation

uses
    MARS.Core.Registry;

{ THelloWorldResource }

constructor TStaticFileResources.Create;
begin
  inherited;
  RootFolder := TPath.GetDirectoryName(ParamStr(0)) + PathDelim + 'www';
  IncludeSubFolders := True;
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<TStaticFileResources>(
    function: TObject
    begin
      Result := TStaticFileResources.Create;
    end
  );

end.
