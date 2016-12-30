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
  System.SysUtils, System.Classes,

  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  WiRL.Core.JSON,
  WiRL.Core.Auth.Context,
  WiRL.Core.Auth.Resource,
  WiRL.WebServer.Resources,
  Model;

type
  [Path('/item')]
  TItemResource = class
  private
  protected
    [Context] Token: TWiRLAuthContext;
  public
    [GET, Path('/{id}'), RolesAllowed('user')]
    function Retrieve([PathParam] id: Integer): TToDoItem;

    [GET, RolesAllowed('user')]
    function RetrieveAll(): TArray<TToDoItem>;

    [POST, RolesAllowed('user')]
    function Add([FormParam('text')] AText: string): TToDoItem;

    [PUT, Path('/{id}'), RolesAllowed('user')]
    function Update([PathParam('id')] AId: Integer; [FormParam('text')] AText: string): TToDoItem;

    [DELETE, Path('/{id}'), RolesAllowed('user')]
    procedure Delete([PathParam('id')] AId: Integer);
  end;

  [Path('/token')]
  TTokenResource = class(TWiRLAuthResource)
  private
  protected
    function Authenticate(const AUserName, APassword: string): TWiRLAuthResult; override;
  public
  end;

  [Path('/webapp'), RootFolder('Z:\WiRL\Demos\ToDoList\www\todo_angular', True) ]
  TWebAppResource = class(TFileSystemResource)
  end;

implementation

uses
  Data.DB,
  WiRL.Core.Registry,
  WiRL.Core.Exceptions,
  FireDAC.Comp.Client,
  Model.Persistence.FDAC;

{ TItemResource }

function TItemResource.Add(AText: string): TToDoItem;
var
  LAccessor: TDBAccessor;
begin
  if AText = '' then
    raise EWiRLWebApplicationException.Create('Text cannot be empty', 500);

  LAccessor := TDBAccessor.Create;
  try
    Result := TToDoItem.Create;
    try
      Result.Owner := Token.Subject.UserID;
      Result.Text := AText;
      LAccessor.New(Result);
    except
      Result.Free;
      Result := nil;
      raise;
    end;
  finally
    LAccessor.Free;
  end;
end;

procedure TItemResource.Delete(AId: Integer);
var
  LAccessor: TDBAccessor;
begin
  LAccessor := TDBAccessor.Create;
  try
    LAccessor.Delete(AId);
  finally
    LAccessor.Free;
  end;
end;

function TItemResource.Retrieve(id: Integer): TToDoItem;
var
  LAccessor: TDBAccessor;
begin
  LAccessor := TDBAccessor.Create;
  try
    Result := LAccessor.Retrieve(id);
    if not Assigned(Result) then
      raise EWiRLWebApplicationException.Create(
        Format('Item not found: %d', [id]), 404);
  finally
    LAccessor.Free;
  end;
end;

function TItemResource.RetrieveAll: TArray<TToDoItem>;
var
  LAccessor: TDBAccessor;
  LResult: TArray<TToDoItem>;
begin
  SetLength(LResult, 0);

  LAccessor := TDBAccessor.Create;
  try
    LAccessor.Select(
        'OWNER = :OWNER'
      , procedure(AQuery: {$if CompilerVersion > 24}TFDQuery{$else}TSQLQuery{$ifend})
        begin
          AQuery.ParamByName('OWNER').AsString := Token.Subject.UserID;
        end
      , procedure(AQuery: {$if CompilerVersion > 24}TFDQuery{$else}TSQLQuery{$ifend})
        begin
          SetLength(LResult, Length(LResult) + 1);
          LResult[Length(LResult) -1] := TToDoItem.CreateFromRecord(AQuery);
        end
    );
  finally
    LAccessor.Free;
  end;

  Result := LResult;
end;


function TItemResource.Update(AId: Integer; AText: string): TToDoItem;
var
  LAccessor: TDBAccessor;
begin
  LAccessor := TDBAccessor.Create;
  try
    Result := LAccessor.Retrieve(AId);
    try
      Result.Text := AText;
      LAccessor.Update(Result);
    except
      Result.Free;
      Result := nil;
      raise;
    end;
  finally
    LAccessor.Free;
  end;
end;

{ TTokenResource }

function TTokenResource.Authenticate(const AUserName, APassword: string):
    TWiRLAuthResult;
var
  LAccessor: TDBAccessor;
  LRoles: string;
  LUserName: string;
begin
  LAccessor := TDBAccessor.Create;
  try
    LUserName := AUserName;
    Result.Success := LAccessor.Authenticate(LUserName, APassword, LRoles);
    Result.Roles := LRoles.Split([',']);
    FAuthContext.Subject.Roles := LRoles;
  finally
    LAccessor.Free;
  end;
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TItemResource>;
  TWiRLResourceRegistry.Instance.RegisterResource<TTokenResource>;
  TWiRLResourceRegistry.Instance.RegisterResource<TWebAppResource>(
    function: TObject
    begin
      Result := TWebAppResource.Create;
    end
  );

end.
