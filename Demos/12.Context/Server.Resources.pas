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

interface

uses
  System.SysUtils, System.Classes,

  WiRL.Core.Registry,
  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  WiRL.Core.MessageBody.Default,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.Core.Exceptions,

  Server.Context,
  Server.Session;


type
  [Path('context')]
  TContextDemoResource = class
  private
    [Context] Request: TWiRLRequest;
    [Context][Value(42)] MyClass: TMyClass;
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SampleText: string;
    [GET, Path('inner'), Produces(TMediaType.TEXT_PLAIN)]
    function InnerContext([Context] ParamClass: TMyClass): string;
  end;

  [Path('session')]
  TSessionResource = class
  private
    [Context] Session: TSession;
  public
    [CheckSession]
    [GET]
    function ReadSession: TSession;
    [POST]
    function Login([FormParam('username')] const AUserName: string): TSession;
    [CheckSession('admin')]
    [PUT]
    function ChangeSession([FormParam('role')] const ARole: string): TSession;
    [CheckSession]
    [DELETE]
    procedure Logout;
  end;

implementation

{ TContextDemoResource }

function TContextDemoResource.SampleText: string;
begin
  Result :=
    'Hello context injection!' + sLineBreak + sLineBreak +
    'Request.Host: ' + Request.Host + sLineBreak +
    'MyClass.Info: ' + MyClass.Info + sLineBreak +
    'MyClass.Value: ' + MyClass.Value.ToString;
end;

function TContextDemoResource.InnerContext(ParamClass: TMyClass): string;
begin
  Result := ParamClass.ClassName;
end;

{ TSessionResource }

function TSessionResource.ChangeSession(const ARole: string): TSession;
begin
  Session.Role := ARole;
  Result := Session;
end;

function TSessionResource.Login(const AUserName: string): TSession;
begin
  // check for a valid user
  // ...
  Session.UserName := AUserName;
  Session.Role := 'admin';
  Session.StartSession;

  Result := Session;
end;

procedure TSessionResource.Logout;
begin
  Session.StopSession;
end;

function TSessionResource.ReadSession: TSession;
begin
  Result := Session;
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TContextDemoResource>;
  TWiRLResourceRegistry.Instance.RegisterResource<TSessionResource>;

end.
