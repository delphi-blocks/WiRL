{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Resources;

{$I WiRL.inc}

interface

uses
  System.SysUtils, System.Classes,

  WiRL.Core.Attributes,
  WiRL.Core.Validators,
  WiRL.Core.MessageBody.Default,
  WiRL.http.Core,
  WiRL.http.Response,
  WiRL.http.Accept.MediaType;

type
  [Path('demo_resource')]
  TDemoResource = class
  private
    // Inject the Response only if you need RunTime redirection
    [Context] Response: TWiRLResponse;
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    [ResponseRedirection(TWiRLHttpStatus.MOVED_PERMANENTLY, '/rest/app/demo_resource/destination')]
    function DemoRedirectMethod: string;

    [POST, Produces(TMediaType.TEXT_PLAIN)]
    [ResponseStatus(TWiRLHttpStatus.CREATED)]
    function InsertEntity: string;

    [GET, Path('destination'), Produces(TMediaType.TEXT_PLAIN)]
    function DemoDestinationMethod: string;

    [GET, Path('runtime'), Produces(TMediaType.TEXT_PLAIN)]
    function RunTimeRedirect: string;

   [GET, Path('/browse')]
   [Produces(TMediaType.TEXT_PLAIN)]
   function MaxwellBrowse([QueryParam('id'), NotNull('ID parameter required.')] id,
       [QueryParam('root'), NotNull('root parameter required.')] root,
       [QueryParam('item'), NotNull('item parameter required.')] item: string): string;

  end;

implementation

uses
  System.IOUtils,
  WiRL.Core.Registry;

function TDemoResource.InsertEntity: string;
begin
  Result := 'Entity created (Status 201)';
end;

function TDemoResource.MaxwellBrowse(id, root, item: string): string;
begin
  Result := Format('%s, %s, %s', [id, root, item]);
end;

function TDemoResource.RunTimeRedirect: string;
begin
  Result := 'RunTime Redirection';

  // Write a real condition here!!
  if True then
    Response.Redirect(301, '/rest/app/demo_resource/destination');
end;

function TDemoResource.DemoDestinationMethod: string;
begin
  Result := 'New endpoint'
end;

function TDemoResource.DemoRedirectMethod: string;
begin
  Result := 'Old endpoint (redirect to new)';
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TDemoResource>;

end.
