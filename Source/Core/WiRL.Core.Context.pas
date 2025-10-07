{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Context;

interface

uses
  System.Classes, System.SysUtils, System.Rtti, System.Generics.Collections,
  WiRL.Core.Classes,
  WiRL.Rtti.Utils,
  WiRL.http.Core,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.http.URL;

type
  TWiRLContextDataListEnumerator<T> = class(TEnumerator<T>)
  private
    FList: TList<T>;
    FIndex: Integer;
    function GetCurrent: T;
  protected
    function DoGetCurrent: T; override;
    function DoMoveNext: Boolean; override;
  public
    constructor Create(const AList: TList<T>);
    property Current: T read GetCurrent;
    function MoveNext: Boolean;
  end;

  TDisposeProc = reference to procedure (AObject: TObject);

  TWiRLContextDataList = class
  private
    FList: TObjectList<TObject>;
    FDisposeProcs: TList<TDisposeProc>;
    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;

    function GetEnumerator: TWiRLContextDataListEnumerator<TObject>;
    procedure Add(AValue: TObject; ADisposeProc: TDisposeProc);
    procedure Delete(AValue: TObject);
  end;

  TWiRLContextBase = class
  private
    FContextDataList: TWiRLContextDataList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddContainer(AContainer: TObject; ADisposeProc: TDisposeProc); overload;
    procedure AddContainer(AContainer: TObject); overload;
    procedure AddContainerOnce(AContainer: TObject; ADisposeProc: TDisposeProc); overload;
    procedure AddContainerOnce(AContainer: TObject); overload;
    procedure RemoveContainer(AValue: TObject);

    // Injects context data into fields and properties marked with [Context] attribute
    procedure Inject(AInstance: TObject);

    // Look up context data by type and raise an exception if not found
    function GetContextDataAs<T: class>: T;
    // Look up context data by type and return nil if not found
    function FindContextDataAs<T: class>: T; overload;
    function FindContextDataAs(AClass: TClass): TObject; overload;

    function FindContextDataAsByIntf(const IID: TGUID): IInterface; overload;
    function FindContextDataAsByIntf<T: IInterface>: T; overload;

    property ContextData: TWiRLContextDataList read FContextDataList;
  end;

  TWiRLContextHttp = class(TWiRLContextBase)
  private
    FRequestURL: TWiRLURL;
    procedure InitRequestURL(ARequest: TWiRLRequest);

    function GetRequestURL: TWiRLURL;
    function GetRequest: TWiRLRequest;
    function GetResponse: TWiRLResponse;
    function GetConnection: TWiRLConnection;
    procedure SetRequest(const Value: TWiRLRequest);
    procedure SetResponse(const Value: TWiRLResponse);
  public
    destructor Destroy; override;

    property Connection: TWiRLConnection read GetConnection;
    property Request: TWiRLRequest read GetRequest write SetRequest;
    property Response: TWiRLResponse read GetResponse write SetResponse;
    property RequestURL: TWiRLURL read GetRequestURL;
  end;

  /// <summary>
  /// Factory interface to create context objects. The class implementing this
  /// interface must be registered in the global TWiRLContextInjectionRegistry.
  /// If a custom disposer is needed, the class must also implement IContextHttpDisposer.
  /// </summary>
  IContextHttpFactory = interface
  ['{152751D7-F806-4EF3-B095-4E0D9545F6C7}']
    function CreateContextObject(const AObject: TRttiObject; AContext: TWiRLContextHttp): TValue;
  end;

  /// <summary>
  /// Optional interface for custom disposal of context objects. 
  /// Classes implementing this interface must also implement IContextHttpFactory 
  /// and be registered in the global TWiRLContextInjectionRegistry.
  /// This allows factories to define custom cleanup logic for their context objects.
  /// </summary>
  IContextHttpDisposer = interface
  ['{5DA5F130-8B49-4874-B6D5-BD40AC8A4996}']
    procedure FreeContextObject(AObject: TObject);
  end;


implementation

uses
  WiRL.Configuration.Core, WiRL.Core.Injection;

{ TWiRLContextDataList }

procedure TWiRLContextDataList.Add(AValue: TObject; ADisposeProc: TDisposeProc);
begin
  FList.Add(AValue);
  FDisposeProcs.Add(ADisposeProc);
end;

procedure TWiRLContextDataList.Clear;
var
  LIndex: Integer;
  LDisposeProc: TDisposeProc;
begin
  for LIndex := 0 to FList.Count - 1 do
  begin
    LDisposeProc := FDisposeProcs[LIndex];
    if Assigned(LDisposeProc) then
    begin
      LDisposeProc(FList[LIndex]);
    end;
  end;
  FDisposeProcs.Clear;
  FList.Clear;
end;

constructor TWiRLContextDataList.Create;
begin
  inherited;
  FList := TObjectList<TObject>.Create(False);
  FDisposeProcs := TList<TDisposeProc>.Create();
end;

procedure TWiRLContextDataList.Delete(AValue: TObject);
var
  LIndex: Integer;
  LProc: TDisposeProc;
begin
  LIndex := FList.Remove(AValue);
  if (LIndex >= 0) then
  begin
    LProc := FDisposeProcs[LIndex];
    if Assigned(LProc) then
      LProc(AValue);
    FDisposeProcs.Delete(LIndex);
  end;
end;

destructor TWiRLContextDataList.Destroy;
begin
  Clear;
  FList.Free;
  FDisposeProcs.Free;
  inherited;
end;

function TWiRLContextDataList.GetEnumerator: TWiRLContextDataListEnumerator<TObject>;
begin
  Result := TWiRLContextDataListEnumerator<TObject>.Create(FList);
end;

{ TWiRLContextDataListEnumerator<T> }

constructor TWiRLContextDataListEnumerator<T>.Create(const AList: TList<T>);
begin
  inherited Create;
  FList := AList;
  FIndex := -1;
end;

function TWiRLContextDataListEnumerator<T>.DoGetCurrent: T;
begin
  Result := GetCurrent;
end;

function TWiRLContextDataListEnumerator<T>.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

function TWiRLContextDataListEnumerator<T>.GetCurrent: T;
begin
  Result := FList[FIndex];
end;

function TWiRLContextDataListEnumerator<T>.MoveNext: Boolean;
begin
  if FIndex >= FList.Count then
    Exit(False);
  Inc(FIndex);
  Result := FIndex < FList.Count;
end;

{ TWiRLContextBase }

procedure TWiRLContextBase.AddContainer(AContainer: TObject; ADisposeProc: TDisposeProc);
begin
  FContextDataList.Add(AContainer, ADisposeProc);
end;

procedure TWiRLContextBase.AddContainerOnce(AContainer: TObject;
  ADisposeProc: TDisposeProc);
var
  LItem: TObject;
begin
  LItem := FindContextDataAs(AContainer.ClassType);
  if Assigned(LItem) then
    FContextDataList.Delete(LItem);

  FContextDataList.Add(AContainer, ADisposeProc);
end;

procedure TWiRLContextBase.AddContainerOnce(AContainer: TObject);
begin
  AddContainerOnce(AContainer, nil);
end;

constructor TWiRLContextBase.Create;
begin
  FContextDataList := TWiRLContextDataList.Create;
  AddContainerOnce(Self);
end;

destructor TWiRLContextBase.Destroy;
begin
  FContextDataList.Free;

  inherited;
end;

function TWiRLContextBase.FindContextDataAs(AClass: TClass): TObject;
var
  LItem: TObject;
  LApp: IWiRLApplication;
begin
  Result := nil;
  for LItem in FContextDataList do
  begin
    if LItem.InheritsFrom(AClass) then
      Exit(LItem);
  end;
  if not Assigned(Result) and (AClass.InheritsFrom(TWiRLConfiguration)) then
  begin
    LApp := FindContextDataAsByIntf<IWiRLApplication>;
    Result := LApp.GetConfigByClassRef(TWiRLConfigurationClass(AClass));
  end;
end;

function TWiRLContextBase.FindContextDataAs<T>: T;
begin
  Result := FindContextDataAs(TClass(T)) as T;
end;

function TWiRLContextBase.FindContextDataAsByIntf(const IID: TGUID): IInterface;
var
  LItem: TObject;
begin
  Result := nil;
  for LItem in FContextDataList do
  begin
    if Supports(LItem, IID, Result) then
      Exit;
  end;
end;

function TWiRLContextBase.FindContextDataAsByIntf<T>: T;
var
  LIID: TGUID;
  LRttiType: TRttiType;
  LInterface: IInterface;
begin
  LRttiType := TRttiHelper.Context.GetType(TypeInfo(T));

  LIID := (LRttiType as TRttiInterfaceType).GUID;
  LInterface := FindContextDataAsByIntf(LIID);

  if not Supports(LInterface, LIID, Result) then
    raise EWiRLException.CreateFmt('Interface not supported from [%s]', [LRttiType.Name]);

end;

function TWiRLContextBase.GetContextDataAs<T>: T;
begin
  Result := FindContextDataAs<T>;
  if not Assigned(Result) then
    raise Exception.CreateFmt('Class [%s] not found', [TClass(T).ClassName]);
end;

procedure TWiRLContextBase.Inject(AInstance: TObject);
begin
  TWiRLContextInjectionRegistry.Instance.ContextInjection(AInstance, Self);
end;

procedure TWiRLContextBase.RemoveContainer(AValue: TObject);
begin
  FContextDataList.Delete(AValue);
end;

procedure TWiRLContextBase.AddContainer(AContainer: TObject);
begin
  AddContainer(AContainer, nil);
end;

{ TWiRLContextHttp }

destructor TWiRLContextHttp.Destroy;
begin
  FRequestURL.Free;

  inherited;
end;

function TWiRLContextHttp.GetConnection: TWiRLConnection;
begin
  Result := FindContextDataAs<TWiRLConnection>;
end;

function TWiRLContextHttp.GetRequest: TWiRLRequest;
begin
  Result := FindContextDataAs<TWiRLRequest>;
end;

function TWiRLContextHttp.GetRequestURL: TWiRLURL;
begin
  if not Assigned(FRequestURL) then
    raise EWiRLException.Create('RequestURL not found');
  Result := FRequestURL;
end;

function TWiRLContextHttp.GetResponse: TWiRLResponse;
begin
  Result := FindContextDataAs<TWiRLResponse>;
end;

procedure TWiRLContextHttp.InitRequestURL(ARequest: TWiRLRequest);
begin
  if Assigned(FRequestURL) then
    FRequestURL.Free;
  FRequestURL := TWiRLURL.Create(ARequest);
  AddContainerOnce(FRequestURL);
end;

procedure TWiRLContextHttp.SetRequest(const Value: TWiRLRequest);
begin
  AddContainerOnce(Value);
  AddContainerOnce(Value.Connection);
  InitRequestURL(Value);
end;

procedure TWiRLContextHttp.SetResponse(const Value: TWiRLResponse);
begin
  AddContainerOnce(Value);
  AddContainerOnce(Value.Connection);
end;

end.
