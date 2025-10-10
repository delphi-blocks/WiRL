{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2025 WiRL Team                                      }
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
  TContextEntry = record
    Value: TObject;
    Factory: IInterface;
    ContextOwned: Boolean;
  end;

  TWiRLContextDataListEnumerator = class(TEnumerator<TObject>)
  private
    FList: TList<TContextEntry>;
    FIndex: Integer;
    function GetCurrent: TObject;
  protected
    function DoGetCurrent: TObject; override;
    function DoMoveNext: Boolean; override;
  public
    constructor Create(const AList: TList<TContextEntry>);
    property Current: TObject read GetCurrent;
    function MoveNext: Boolean;
  end;

  TWiRLContextBase = class;

  TWiRLContextDataList = class
  private
    FContext: TWiRLContextBase;
    FList: TList<TContextEntry>;
    procedure DisposeValue(LEntry: TContextEntry);
  public
    constructor Create(AContext: TWiRLContextBase);
    destructor Destroy; override;

    function GetEnumerator: TWiRLContextDataListEnumerator;
    procedure Add(AValue: TObject); overload;
    procedure Add(AValue: TObject; AFactory: IInterface; AContextOwned: Boolean); overload;
    procedure Delete(AValue: TObject);
    procedure Clear;
  end;

  TWiRLContextBase = class
  private
    FContextDataList: TWiRLContextDataList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddContainer(AContainer: TObject);
    procedure AddContainerOnce(AContainer: TObject);
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
    procedure DisposeContextObject(AObject: TObject; AContext: TWiRLContextHttp);
  end;

implementation

uses
  WiRL.Configuration.Core, WiRL.Core.Injection, WiRL.Core.Exceptions;

{ TWiRLContextDataList }

procedure TWiRLContextDataList.Add(AValue: TObject);
begin
  Add(AValue, nil, False);
end;

procedure TWiRLContextDataList.Add(AValue: TObject; AFactory: IInterface;
  AContextOwned: Boolean);
var
  LContextEntry: TContextEntry;
begin
  LContextEntry.Value := AValue;
  LContextEntry.Factory := AFactory;
  LContextEntry.ContextOwned := AContextOwned;
  FList.Add(LContextEntry);
end;

procedure TWiRLContextDataList.Clear;
var
  LEntry: TContextEntry;
begin
  for LEntry in FList do
  begin
    DisposeValue(LEntry);
  end;
  FList.Clear;
end;

constructor TWiRLContextDataList.Create(AContext: TWiRLContextBase);
begin
  inherited Create;
  FContext := AContext;
  FList := TList<TContextEntry>.Create;
end;

procedure TWiRLContextDataList.Delete(AValue: TObject);
var
  LEntry: TContextEntry;
begin
  for LEntry in FList do
  begin
    if LEntry.Value = AValue then
    begin
      DisposeValue(LEntry);
      FList.Remove(LEntry);
    end;
  end;
end;

destructor TWiRLContextDataList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

procedure TWiRLContextDataList.DisposeValue(LEntry: TContextEntry);
var
  LContextDisposer: IContextObjectDisposer;
  LContextHttpDisposer: IContextHttpDisposer;
begin
  if not LEntry.ContextOwned then
    Exit;

  if not Assigned(LEntry.Factory) then
  begin
    LEntry.Value.Free;
    Exit;
  end;

  if Supports(LEntry.Factory, IContextObjectDisposer, LContextDisposer) then
  begin
    LContextDisposer.DisposeContextObject(LEntry.Value, FContext);
  end
  else if Supports(LEntry.Factory, IContextHttpDisposer, LContextHttpDisposer) then
  begin
    LContextHttpDisposer.DisposeContextObject(LEntry.Value, FContext as TWiRLContextHttp);
  end
  else
  begin
    LEntry.Value.Free;
  end;
end;

function TWiRLContextDataList.GetEnumerator: TWiRLContextDataListEnumerator;
begin
  Result := TWiRLContextDataListEnumerator.Create(FList);
end;

{ TWiRLContextDataListEnumerator }

constructor TWiRLContextDataListEnumerator.Create(const AList: TList<TContextEntry>);
begin
  inherited Create;
  FList := AList;
  FIndex := -1;
end;

function TWiRLContextDataListEnumerator.DoGetCurrent: TObject;
begin
  Result := GetCurrent;
end;

function TWiRLContextDataListEnumerator.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

function TWiRLContextDataListEnumerator.GetCurrent: TObject;
begin
  Result := FList[FIndex].Value;
end;

function TWiRLContextDataListEnumerator.MoveNext: Boolean;
begin
  if FIndex >= FList.Count then
    Exit(False);
  Inc(FIndex);
  Result := FIndex < FList.Count;
end;

{ TWiRLContextBase }

procedure TWiRLContextBase.AddContainer(AContainer: TObject);
begin
  FContextDataList.Add(AContainer);
end;

procedure TWiRLContextBase.AddContainerOnce(AContainer: TObject);
var
  LItem: TObject;
begin
  LItem := FindContextDataAs(AContainer.ClassType);
  if Assigned(LItem) then
    FContextDataList.Delete(LItem);

  FContextDataList.Add(AContainer);
end;

constructor TWiRLContextBase.Create;
begin
  FContextDataList := TWiRLContextDataList.Create(Self);
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
