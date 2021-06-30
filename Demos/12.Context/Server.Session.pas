{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Session;

interface

uses
  System.SysUtils, System.Classes, System.Hash,
  System.Generics.Collections, System.Rtti,

  WiRL.Core.Singleton,
  WiRL.Core.Attributes,
  WiRL.Core.Exceptions,
  WiRL.Rtti.Utils,
  WiRL.Core.Context,
  WiRL.http.Filters,
  WiRL.Core.Injection;
  
type
  TSessionStatus = (
    seTemporary, // Fake session create by the system when there isn't any session
    seNew, // A newly created session that need to be send to the client
    seDelete, // Deleted session
    seValid
  );

  [Singleton]
  TSession = class
  private
    FUserName: string;
    FNew: Boolean;
    FDelete: Boolean;
    FID: string;
    FTimeStamp: TDateTime;
    FRole: string;
    function GetStatus: TSessionStatus;
    function GetIsValid: Boolean;
    function GetIsNew: Boolean;
    function GetIsTemporary: Boolean;
    function GetIsDeleted: Boolean;
  public
    property IsNew: Boolean read GetIsNew;
    property IsDeleted: Boolean read GetIsDeleted;
    property IsValid: Boolean read GetIsValid;
    property IsTemporary: Boolean read GetIsTemporary;
    property Status: TSessionStatus read GetStatus;
    
    property ID: string read FID write FID;
    property UserName: string read FUserName write FUserName;
    property Role: string read FRole write FRole;
    property TimeStamp: TDateTime read FTimeStamp write FTimeStamp;

    procedure StartSession;
    procedure StopSession;
    constructor Create;
  end;

  TSessions = class
  private
    type
      TSessionSingleton = TWiRLSingleton<TSessions>;
  private
    class function GetInstance: TSessions; static; inline;
  private
    FList: TDictionary<string,TSession>;
    procedure ClearExpiredSessions;
  public
    procedure Add(ASession: TSession);
    // nil if not found
    function Find(const AID: string): TSession;
    // exception if not found
    function Get(const AID: string): TSession;
    procedure Remove(const AID: string);
    procedure Clear;

    class property Instance: TSessions read GetInstance;

    constructor Create;
    destructor Destroy; override;
  end;

  TSessionFactory = class(TInterfacedObject, IContextHttpFactory)
  public
    function CreateContextObject(const AObject: TRttiObject;
      AContext: TWiRLContextHttp): TValue;
  end;

  TSessionResponseFilter = class(TInterfacedObject, IWiRLContainerResponseFilter)
  private
    function GetSession(AResponseContext: TWiRLContainerResponseContext): TSession;
  public
    procedure Filter(AResponseContext: TWiRLContainerResponseContext);
  end;

  [NameBinding]
  CheckSessionAttribute = class(TCustomAttribute)
  private
    FRole: string;
  public
    constructor Create(const ARole: string = '');
    property Role: string read FRole;
  end;

  [CheckSession]
  TCheckSessionRequestFilter = class(TInterfacedObject, IWiRLContainerRequestFilter)
  public
    procedure Filter(ARequestContext: TWiRLContainerRequestContext);
  end;
  

const
  SidName = 'sid';  

implementation

uses
  WiRL.http.Request;

{ TSessions }

procedure TSessions.Add(ASession: TSession);
begin
  MonitorEnter(FList);
  try
    ClearExpiredSessions;
    FList.Add(ASession.ID, ASession);
    ASession.FNew := False;
  finally
    MonitorExit(FList);
  end;
end;

procedure TSessions.Clear;
var
  LSession: TSession;
begin
  MonitorEnter(FList);
  try
    for LSession in FList.Values do 
      LSession.Free;
    FList.Clear;      
  finally
    MonitorExit(FList);
  end;
end;

procedure TSessions.ClearExpiredSessions;
begin
  // not yet implemented
end;

constructor TSessions.Create;
begin
  inherited;
  FList := TDictionary<string,TSession>.Create;
end;

destructor TSessions.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TSessions.Find(const AID: string): TSession;
begin
  Result := nil;
  MonitorEnter(FList);
  try
    if FList.TryGetValue(AID, Result) then
      Result.TimeStamp := Now;
  finally
    MonitorExit(FList);
  end;
end;

function TSessions.Get(const AID: string): TSession;
begin
  Result := Find(AID);
  if not Assigned(Result) then
    raise Exception.CreateFmt('Session [%s] not found', [AID]);
end;

class function TSessions.GetInstance: TSessions;
begin
  Result := TSessionSingleton.Instance;
end;

procedure TSessions.Remove(const AID: string);
var
  LSession: TSession;
begin
  MonitorEnter(FList);
  try
    if FList.TryGetValue(AID, LSession) then
    begin
      FList.Remove(AID);
      LSession.Free;
    end;
  finally
    MonitorExit(FList);
  end;
end;

{ TSessionFactory }

function TSessionFactory.CreateContextObject(const AObject: TRttiObject;
  AContext: TWiRLContextHttp): TValue;
var
  LSid: string;
  LSession: TSession;
begin
  LSid := AContext.Request.CookieFields[SidName];
  LSession := TSessions.Instance.Find(LSID);
  if not Assigned(LSession) then
    LSession := TSession.Create;
  Result := LSession;
end;

{ TSessionResponseFilter }

procedure TSessionResponseFilter.Filter(
  AResponseContext: TWiRLContainerResponseContext);
var
  LSession: TSession;
begin
  LSession := GetSession(AResponseContext);
  if Assigned(LSession) then
  begin
    if LSession.IsNew then
    begin
      AResponseContext.Response.Cookies.Add(SidName, LSession.ID);
      TSessions.Instance.Add(LSession);
    end
    else if LSession.IsDeleted then
    begin
      AResponseContext.Response.Cookies.Add(SidName, '');
      TSessions.Instance.Remove(LSession.ID);
    end
    else if LSession.IsTemporary then
      LSession.Free;
  end;
end;

function TSessionResponseFilter.GetSession(AResponseContext: TWiRLContainerResponseContext): TSession;
var
  LContext: TWiRLContextBase;
  LObject: TObject;
begin
  Result := nil;
  LContext := AResponseContext.Context;
  LObject := LContext.FindContextDataAs<TSession>;
  if Assigned(LObject) then
    Exit(TSession(LObject));
end;

{ TSession }

constructor TSession.Create;
begin
  inherited;
  FNew := True;
  FTimeStamp := Now;
end;

function TSession.GetIsDeleted: Boolean;
begin
  Result := (Status = seDelete);
end;

function TSession.GetIsNew: Boolean;
begin
  Result := (Status = seNew);
end;

function TSession.GetIsTemporary: Boolean;
begin
  Result := (Status = seTemporary);
end;

function TSession.GetIsValid: Boolean;
begin
  Result := (Status = seValid);
end;

function TSession.GetStatus: TSessionStatus;
begin
  if FDelete then
    Result := seDelete
  else if FID = '' then
    Result := seTemporary
  else if FNew then
    Result := seNew
  else
    Result := seValid;
end;

procedure TSession.StartSession;
begin
  FID := THashMD5.GetHashString('WiRLDemo:' + IntToStr(Random(100000)) + ':' + FloatToStr(Now) ); // This should be change with a CSPRNG
  FTimeStamp := Now;
end;

procedure TSession.StopSession;
begin
  FDelete := True;
end;

{ TCheckSessionRequestFilter }

procedure TCheckSessionRequestFilter.Filter(
  ARequestContext: TWiRLContainerRequestContext);
var
  LSid: string;
  LSession: TSession;
  LMethod: TRttiMethod;
  LRole: string;
begin
  LSid := ARequestContext.Request.CookieFields[SidName];
  LSession := TSessions.Instance.Find(LSid);
  if not Assigned(LSession) then
    raise EWiRLNotAuthorizedException.Create('Session not found');

  LMethod := ARequestContext.Method.RttiObject;
  TRttiHelper.HasAttribute<CheckSessionAttribute>(LMethod,
    procedure (AAttr: CheckSessionAttribute)
    begin
      LRole := AAttr.Role;
    end
  );
  if (LRole <> '') and (LSession.Role <> LRole) then
    raise EWiRLNotAuthorizedException.Create(Format('Not enough privileges (role [%s])', [LSession.Role]));
end;

{ CheckSessionAttribute }

constructor CheckSessionAttribute.Create(const ARole: string);
begin
  inherited Create;
  FRole := ARole;
end;

initialization
  TWiRLContextInjectionRegistry.Instance.RegisterFactory<TSession>(TSessionFactory);
  TWiRLFilterRegistry.Instance.RegisterFilter<TSessionResponseFilter>;
  TWiRLFilterRegistry.Instance.RegisterFilter<TCheckSessionRequestFilter>;

end.
