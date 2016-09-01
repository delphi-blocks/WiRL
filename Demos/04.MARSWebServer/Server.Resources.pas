(*
  Copyright 2015-2016, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit Server.Resources;

interface

uses
  SysUtils, Classes, IOUtils

  , MARS.Core.Attributes
  , MARS.Core.MediaType
  , MARS.Core.Response

  , MARS.WebServer.Resources
  ;

type
  [Path('helloworld')]
  THelloWorldResource = class(TFileSystemResource)
  public
    constructor Create; override;
  end;

implementation

uses
    MARS.Core.Registry;

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
  TMARSResourceRegistry.Instance.RegisterResource<THelloWorldResource>(
    function: TObject
    begin
      Result := THelloWorldResource.Create;
    end
  );

end.
