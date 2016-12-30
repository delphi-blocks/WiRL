{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Data.ToDo;

interface

uses
  System.SysUtils, System.Classes, WiRL.Client.CustomResource,
  WiRL.Client.Resource, WiRL.Client.Token, WiRL.Client.Application,
  WiRL.Client.Client, WiRL.Client.SubResource, WiRL.Client.SubResource.JSON,
  WiRL.Client.Resource.JSON, WiRL.Core.JSON, System.JSON;

type
  TTodoDM = class(TDataModule)
    ToDoClient: TWiRLClient;
    ToDoApplication: TWiRLClientApplication;
    Token: TWiRLClientToken;
    ItemResource: TWiRLClientResourceJSON;
    AllItemsSubResource: TWiRLClientSubResourceJSON;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Login(AUserName, APassword: string; const AOnSuccess: TProc);
    procedure GetList(const AOnSuccess: TProc);
    procedure Delete(AID: Integer);
    procedure Add(AText: string; const AOnSuccess: TProc);
    procedure Update(AID: Integer; AText: string; const AOnSuccess: TProc);
  end;

var
  TodoDM: TTodoDM;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ TDataModule1 }

procedure TTodoDM.Add(AText: string; const AOnSuccess: TProc);
begin
  ItemResource.PathParamsValues.Clear;
  ItemResource.POST(
    procedure (AStream: TMemoryStream)
    var
      LWriter: TStreamWriter;
    begin
      LWriter := TStreamWriter.Create(AStream, TEncoding.Default);
      try
        LWriter.Write('Text=' + AText);
      finally
        LWriter.Free;
      end;
    end
  );
  if Assigned(AOnSuccess) then
    AOnSuccess();
end;

procedure TTodoDM.Delete(AID: Integer);
begin
  ItemResource.PathParamsValues.Clear;
  ItemResource.PathParamsValues.Add(AID.ToString);
  ItemResource.DELETE();
end;

procedure TTodoDM.GetList(const AOnSuccess: TProc);
begin
  AllItemsSubResource.GETAsync(AOnSuccess);
end;

procedure TTodoDM.Login(AUserName, APassword: string; const AOnSuccess: TProc);
begin
  Token.UserName := AUserName;
  Token.Password := APassword;
  Token.POSTAsync(AOnSuccess);
end;

procedure TTodoDM.Update(AID: Integer; AText: string; const AOnSuccess: TProc);
begin
  ItemResource.PathParamsValues.Clear;
  ItemResource.PathParamsValues.Add(AID.ToString);
  ItemResource.PUT(
    procedure (AStream: TMemoryStream)
    var
      LWriter: TStreamWriter;
    begin
      LWriter := TStreamWriter.Create(AStream, TEncoding.Default);
      try
        LWriter.Write('Text=' + AText);
      finally
        LWriter.Free;
      end;
    end
  );
  if Assigned(AOnSuccess) then
    AOnSuccess();
end;

end.
