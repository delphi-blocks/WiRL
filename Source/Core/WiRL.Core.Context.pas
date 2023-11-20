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

  TWiRLContextDataList = class
  private
    FList: TObjectList<TObject>;
    FOwnedObjects: TObjectList<TObject>;
  public
    constructor Create;
    destructor Destroy; override;

    function GetEnumerator: TWiRLContextDataListEnumerator<TObject>;
    procedure Add(AValue: TObject; AOwned: Boolean);
    procedure Delete(AValue: TObject);
  end;

  TWiRLContextBase = class
  private
    FContextDataList: TWiRLContextDataList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddContainer(AContainer: TObject; AOwned: Boolean);
    procedure AddContainerOnce(AContainer: TObject; AOwned: Boolean);
    procedure RemoveContainer(AValue: TObject);

    // raise an exception if not found
    function GetContextDataAs<T: class>: T;
    // return nil if not found
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
    procedure SetRequest(const Value: TWiRLRequest);
    procedure SetResponse(const Value: TWiRLResponse);
  public
    destructor Destroy; override;

    property Request: TWiRLRequest read GetRequest write SetRequest;
    property Response: TWiRLResponse read GetResponse write SetResponse;
    property RequestURL: TWiRLURL read GetRequestURL;
  end;

  IContextHttpFactory = interface
  ['{152751D7-F806-4EF3-B095-4E0D9545F6C7}']
    function CreateContextObject(const AObject: TRttiObject; AContext: TWiRLContextHttp): TValue;
  end;


implementation

uses
  WiRL.Configuration.Core;

{ TWiRLContextDataList }

procedure TWiRLContextDataList.Add(AValue: TObject; AOwned: Boolean);
begin
  FList.Add(AValue);
  if AOwned then
    FOwnedObjects.Add(AValue);
end;

constructor TWiRLContextDataList.Create;
begin
  inherited;
  FList := TObjectList<TObject>.Create(False);
  FOwnedObjects := TObjectList<TObject>.Create(True);
end;

procedure TWiRLContextDataList.Delete(AValue: TObject);
var
  LIndex: Integer;
begin
  FList.Remove(AValue);

  LIndex := FOwnedObjects.IndexOf(AValue);
  if LIndex >= 0 then
  begin
    FOwnedObjects.Delete(LIndex);
    AValue.Free;
  end;
end;

destructor TWiRLContextDataList.Destroy;
begin
  FList.Free;
  FOwnedObjects.Free;
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

procedure TWiRLContextBase.AddContainer(AContainer: TObject; AOwned: Boolean);
begin
  FContextDataList.Add(AContainer, AOwned);
end;

procedure TWiRLContextBase.AddContainerOnce(AContainer: TObject;
  AOwned: Boolean);
var
  LItem: TObject;
begin
  LItem := FindContextDataAs(AContainer.ClassType);
  if Assigned(LItem) then
    FContextDataList.Delete(LItem);

  FContextDataList.Add(AContainer, AOwned);
end;

constructor TWiRLContextBase.Create;
begin
  FContextDataList := TWiRLContextDataList.Create;
  AddContainerOnce(Self, False);
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
  AddContainerOnce(FRequestURL, False);
end;

procedure TWiRLContextHttp.SetRequest(const Value: TWiRLRequest);
begin
  AddContainerOnce(Value, False);
  InitRequestURL(Value);
end;

procedure TWiRLContextHttp.SetResponse(const Value: TWiRLResponse);
begin
  AddContainerOnce(Value, False);
end;

end.
