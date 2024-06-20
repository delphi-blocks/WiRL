{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2023 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Client.Filters;

interface

uses
  System.SysUtils, System.Generics.Collections, System.Generics.Defaults,
  System.Classes, System.Rtti,

  WiRL.http.Headers,
  WiRL.http.Client.Interfaces,
  WiRL.http.Client,
  WiRL.http.Core,
  WiRL.http.Accept.MediaType,
  WiRL.Core.Classes,
  WiRL.Core.Context,
  WiRL.Core.Singleton,
  WiRL.Core.Exceptions,
  WiRL.Core.Attributes,
  WiRL.Rtti.Utils;

type
  TWiRLClientRequestContext = class(TObject)
  private
    FRequest: IWiRLRequest;
    FResponse: IWiRLResponse;
    FAborted: Boolean;
    FResource: TObject;
    function GetResponse: IWiRLResponse;
  public
    property Request: IWiRLRequest read FRequest;
    property Response: IWiRLResponse read GetResponse write FResponse;
    property Aborted: Boolean read FAborted;
    procedure Abort;
    constructor Create(AClientResource: TObject; const AHttpMethod: string; ARequestStream: TStream);
  end;

  TWiRLClientResponseContext = class(TObject)
  private
    FRequest: IWiRLRequest;
    FResponse: IWiRLResponse;
    FResource: TObject;
  public
    property Request: IWiRLRequest read FRequest;
    property Response: IWiRLResponse read FResponse;
    constructor Create(AClientResource: TObject; const AHttpMethod: string; ARequestStream: TStream; AResponse: IWiRLResponse);
  end;

  IWiRLClientRequestFilter = interface
  ['{7116140B-76EB-42DF-B396-270C6A5FF104}']
    procedure Filter(ARequestContext: TWiRLClientRequestContext);
  end;

  IWiRLClientResponseFilter = interface
  ['{CFCA5EAB-C5A4-4F6B-AF73-039A1ACFBD6D}']
    procedure Filter(AResponseContext: TWiRLClientResponseContext);
  end;

  // the lower the number the higher the priority
  // By default, when the @Priority annotation is absent
  // on a component the USER priority value is used
  TWiRLClientPriorities = class(TObject)
  public
    const AUTHENTICATION = 1000;
    const AUTHORIZATION = 2000;
    const HEADER_DECORATOR = 3000;
    const ENTITY_CODER = 4000;
    const USER = 5000;
  end;

  TWiRLClientFilterConstructorInfo = class
  private
    FConstructorFunc: TFunc<TObject>;
    FFilterQualifiedClassName: string;
    FTypeTClass: TClass;
    FPriority: Integer;
    FAttribute: TCustomAttribute;
  public
    constructor Create(AClass: TClass; const AConstructorFunc: TFunc<TObject>; APriority: Integer); overload;
    constructor Create(const AFilterQualifiedClassName: string); overload;
//
    function GetRequestFilter: IWiRLClientRequestFilter;
    function GetResponseFilter: IWiRLClientResponseFilter;
    property Attribute: TCustomAttribute read FAttribute;
    property TypeTClass: TClass read FTypeTClass;
    property Priority: Integer read FPriority;
    property ConstructorFunc: TFunc<TObject> read FConstructorFunc write FConstructorFunc;
//    property FilterQualifiedClassName: string read FFilterQualifiedClassName;
//    function Clone: TWiRLFilterConstructorProxy;
  end;

  TWiRLClientFilterRegistry = class(TObjectList<TWiRLClientFilterConstructorInfo>)
  private
    type
      TWiRLClientFilterRegistrySingleton = TWiRLSingleton<TWiRLClientFilterRegistry>;
    var
      // True if the list has been sorted since the first item was added
      FSorted: Boolean;
    function GetPriority(FilterClass: TClass) :Integer;
  protected
    class function GetInstance: TWiRLClientFilterRegistry; static; inline;
  public
    constructor Create; virtual;

    procedure Sort;
    function AddFilterName(const AFilterName: string): TWiRLClientFilterConstructorInfo;
//
    function FilterByClassName(const AClassName: string; out AConstructorInfo: TWiRLClientFilterConstructorInfo) :Boolean;
    function RegisterFilter<T: class>: TWiRLClientFilterConstructorInfo; overload;
    function RegisterFilter<T: class>(const AConstructorFunc: TFunc<TObject>): TWiRLClientFilterConstructorInfo; overload;
//
//    function ApplyPreMatchingRequestFilters(AContext: TWiRLContext): Boolean;
//    function ApplyPreMatchingResourceFilters(AContext: TWiRLContext): Boolean;
//    procedure ApplyPreMatchingResponseFilters(AContext: TWiRLContext);
//
//    procedure FetchRequestFilter(AFilterType: TWiRLFilterType; ARequestProc: TProc<TWiRLFilterConstructorProxy>);
//    procedure FetchResponseFilter(AFilterType: TWiRLFilterType; AResponseProc: TProc<TWiRLFilterConstructorProxy>);
//
    class property Instance: TWiRLClientFilterRegistry read GetInstance;
  end;

  TWiRLVirtualResponse = class(TInterfacedObject, IWiRLResponse)
  private
    FWiRLHeaders: IWiRLHeaders;
    FMediaType: TMediaType;
    FStatusCode: Integer;
    FStatusText: string;
    FContentStream: TStream;
    FOwnStream: Boolean;
    FContext: TWiRLContextBase;
  protected
    function GetHeaderValue(const AName: string): string;
    function GetStatusCode: Integer;
    function GetStatusText: string;
    function GetStatus: TWiRLResponseStatus;
    function GetContentType: string;
    function GetContentText: string;
    function GetContentStream: TStream;
    function GetHeaders: IWiRLHeaders;
    function GetContentMediaType: TMediaType;
    function GetRawContent: TBytes;
    function GetContent: TWiRLContent;
    procedure SetStatusCode(AValue: Integer);
    procedure SetStatusText(const AValue: string);
    procedure SetContentStream(AStream: TStream; AOwnStream: Boolean);
    procedure SetOwnContentStream(const AValue: Boolean);
    procedure SetContext(AContext: TWiRLContextBase);
  public
    constructor Create;
    destructor Destroy; override;
  end;


implementation

uses
  WiRL.Client.CustomResource;

{ TWiRLClientFilterRegistry }

function TWiRLClientFilterRegistry.AddFilterName(
  const AFilterName: string): TWiRLClientFilterConstructorInfo;
begin
  Result := TWiRLClientFilterConstructorInfo.Create(AFilterName);
  Add(Result);
  FSorted := False;
end;

constructor TWiRLClientFilterRegistry.Create;
var
  LComparer: IComparer<TWiRLClientFilterConstructorInfo>;
begin
  LComparer := TDelegatedComparer<TWiRLClientFilterConstructorInfo>.Create(
    function(const Left, Right: TWiRLClientFilterConstructorInfo): Integer
    begin
      Result := Left.Priority - Right.Priority;
    end
  );

  //TWiRLFilterRegistrySingleton.CheckInstance(Self);
  inherited Create(LComparer, True);
end;

function TWiRLClientFilterRegistry.FilterByClassName(const AClassName: string;
  out AConstructorInfo: TWiRLClientFilterConstructorInfo): Boolean;
var
  LItem: TWiRLClientFilterConstructorInfo;
begin
  Result := False;
  for LItem in Self do
  begin
    if CompareText(LItem.TypeTClass.QualifiedClassName, AClassName) = 0 then
    begin
      AConstructorInfo := LItem;
      Exit(True);
    end;
  end;
end;

class function TWiRLClientFilterRegistry.GetInstance: TWiRLClientFilterRegistry;
begin
  Result := TWiRLClientFilterRegistrySingleton.Instance;
end;

function TWiRLClientFilterRegistry.GetPriority(FilterClass: TClass): Integer;
var
  LPriority: Integer;
begin
  LPriority := TWiRLClientPriorities.USER;
  TRttiHelper.HasAttribute<PriorityAttribute>(TRttiHelper.Context.GetType(FilterClass),
    procedure (Attrib: PriorityAttribute)
    begin
      LPriority := Attrib.Value;
    end
  );
  Result := LPriority;
end;

function TWiRLClientFilterRegistry.RegisterFilter<T>(
  const AConstructorFunc: TFunc<TObject>): TWiRLClientFilterConstructorInfo;
begin
  if not Supports(TClass(T), IWiRLClientRequestFilter) and not Supports(TClass(T), IWiRLClientResponseFilter) then
    raise EWiRLServerException.Create(
      Format('Filter registration error: [%s] should be a valid filter', [TClass(T).QualifiedClassName]),
      Self.ClassName,
      'RegisterFilter'
    );

  Result := TWiRLClientFilterConstructorInfo.Create(TClass(T), AConstructorFunc, GetPriority(TClass(T)));
  Add(Result);
  FSorted := False;
end;

procedure TWiRLClientFilterRegistry.Sort;
begin
  if not FSorted then
  begin
    inherited Sort;
    FSorted := True;
  end;
end;

function TWiRLClientFilterRegistry.RegisterFilter<T>: TWiRLClientFilterConstructorInfo;
begin
  Result := RegisterFilter<T>(nil);
end;

{ TWiRLClientFilterConstructorInfo }

constructor TWiRLClientFilterConstructorInfo.Create(const AFilterQualifiedClassName: string);
begin
  inherited Create;
  FFilterQualifiedClassName := AFilterQualifiedClassName;
end;

function TWiRLClientFilterConstructorInfo.GetRequestFilter: IWiRLClientRequestFilter;
var
  LTempObj: TObject;
begin
  LTempObj := Self.ConstructorFunc();

  // The check doesn't have any sense but I must use SUPPORT and I hate using it without a check
  if not Supports(LTempObj, IWiRLClientRequestFilter, Result) then
    raise EWiRLNotImplementedException.Create(
      Format('[%s] does not implement requested interface [IWiRLContainerRequestFilter]', [TypeTClass.ClassName]),
      Self.ClassName,
      'GetRequestFilter'
    );
end;

function TWiRLClientFilterConstructorInfo.GetResponseFilter: IWiRLClientResponseFilter;
var
  LTempObj: TObject;
begin
  LTempObj := Self.ConstructorFunc();
  // The check doesn't have any sense but I must use SUPPORT and I hate using it without a check
  if not Supports(LTempObj, IWiRLClientResponseFilter, Result) then
    raise EWiRLNotImplementedException.Create(
      Format('[%s] does not implement requested interface [IWiRLContainerResponseFilter]', [TypeTClass.ClassName]),
      Self.ClassName,
      'GetResponseFilter'
    );
end;

constructor TWiRLClientFilterConstructorInfo.Create(AClass: TClass;
  const AConstructorFunc: TFunc<TObject>; APriority: Integer);
var
  LFilterType: TRttiType;
begin
  inherited Create;

  FConstructorFunc := AConstructorFunc;
  FTypeTClass := AClass;
  FFilterQualifiedClassName := AClass.QualifiedClassName;
  FPriority := APriority;

  LFilterType := TRttiHelper.Context.GetType(FTypeTClass);

  TRttiHelper.ForEachAttribute<NameBindingAttribute>(LFilterType,
    procedure (AFilterAttrib: NameBindingAttribute)
    begin
      FAttribute := AFilterAttrib;
    end
  );

  if not Assigned(FAttribute) then
    TRttiHelper.ForEachAttribute<TCustomAttribute>(LFilterType,
      procedure (AFilterAttrib: TCustomAttribute)
      begin
        if TRttiHelper.HasAttribute<NameBindingAttribute>(
          TRttiHelper.Context.GetType(AFilterAttrib.ClassType)) then
          FAttribute := AFilterAttrib;
      end
    );

  // Default constructor function
  if not Assigned(FConstructorFunc) then
    FConstructorFunc :=
      function: TObject
      var
        LType: TRttiType;
        LValue: TValue;
      begin
        LType := TRttiHelper.Context.GetType(FTypeTClass);
        LValue := LType.GetMethod('Create').Invoke(LType.AsInstance.MetaclassType, []);
        Result := LValue.AsObject;
      end
    ;
end;

{ TWiRLClientRequestContext }

procedure TWiRLClientRequestContext.Abort;
begin
  FAborted := True;
end;

constructor TWiRLClientRequestContext.Create(AClientResource: TObject;
  const AHttpMethod: string; ARequestStream: TStream);
var
  LClientResource: TWiRLClientCustomResource;
begin
  inherited Create;
  LClientResource := AClientResource as TWiRLClientCustomResource;

  FResource := AClientResource;
  FRequest := TWiRLClientRequest.Create(
    AHttpMethod,
    LClientResource.URL,
    ARequestStream,
    LClientResource.Headers
  );
end;

function TWiRLClientRequestContext.GetResponse: IWiRLResponse;
begin
  if not Assigned(FResponse) then
  begin
    FResponse := TWiRLVirtualResponse.Create;
  end;
  Result := FResponse;
end;

{ TWiRLClientResponseContext }

constructor TWiRLClientResponseContext.Create(AClientResource: TObject;
  const AHttpMethod: string; ARequestStream: TStream; AResponse: IWiRLResponse);
var
  LClientResource: TWiRLClientCustomResource;
begin
  inherited Create;
  LClientResource := AClientResource as TWiRLClientCustomResource;

  FResource := AClientResource;
  FRequest := TWiRLClientRequest.Create(
    AHttpMethod,
    LClientResource.URL,
    ARequestStream,
    LClientResource.Headers
  );
  FResponse := AResponse;
end;

{ TWiRLVirtualResponse }

constructor TWiRLVirtualResponse.Create;
begin
  FWiRLHeaders := TWiRLHeaders.Create;
  FContentStream := TMemoryStream.Create;
  FMediaType := nil;
end;

destructor TWiRLVirtualResponse.Destroy;
begin
  FMediaType.Free;
  if FOwnStream then
    FContentStream.Free;
  inherited;
end;

function TWiRLVirtualResponse.GetContentText: string;
begin
  Result := EncodingFromCharSet(GetContentMediaType.Charset).GetString(GetRawContent);
end;

function TWiRLVirtualResponse.GetContent: TWiRLContent;
begin
  raise EWiRLServerException.Create('Not Implemented');
end;

function TWiRLVirtualResponse.GetContentMediaType: TMediaType;
begin
  if not Assigned(FMediaType) then
    FMediaType := TMediaType.Create(GetContentType);
  Result := FMediaType;
end;

function TWiRLVirtualResponse.GetContentStream: TStream;
begin
  Result := FContentStream;
end;

function TWiRLVirtualResponse.GetContentType: string;
begin
  Result := FWiRLHeaders.ContentType;
end;

function TWiRLVirtualResponse.GetHeaders: IWiRLHeaders;
begin
  Result := FWiRLHeaders;
end;

function TWiRLVirtualResponse.GetHeaderValue(const AName: string): string;
begin
  Result := FWiRLHeaders.Values[AName];
end;

function TWiRLVirtualResponse.GetRawContent: TBytes;
begin
  if (GetContentStream <> nil) and (GetContentStream.Size > 0) then
  begin
    GetContentStream.Position := 0;
    SetLength(Result, GetContentStream.Size);
    GetContentStream.ReadBuffer(Result[0], GetContentStream.Size);
  end;
end;

function TWiRLVirtualResponse.GetStatus: TWiRLResponseStatus;
begin
  Result := TWiRLResponseStatus.FromStatusCode(FStatusCode);
end;

function TWiRLVirtualResponse.GetStatusCode: Integer;
begin
  Result := FStatusCode;
end;

function TWiRLVirtualResponse.GetStatusText: string;
begin
  Result := FStatusText;
end;

procedure TWiRLVirtualResponse.SetContentStream(AStream: TStream; AOwnStream: Boolean);
begin
  if AStream <> FContentStream then
  begin
    if Assigned(FContentStream) and FOwnStream then
    begin
      FContentStream.Free;
    end;
    FContentStream := AStream;
  end;
  FOwnStream := AOwnStream;
end;

procedure TWiRLVirtualResponse.SetContext(AContext: TWiRLContextBase);
begin
  FContext := AContext;
end;

procedure TWiRLVirtualResponse.SetOwnContentStream(const AValue: Boolean);
begin
  FOwnStream := AValue;
end;

procedure TWiRLVirtualResponse.SetStatusCode(AValue: Integer);
begin
  FStatusCode := AValue;
end;

procedure TWiRLVirtualResponse.SetStatusText(const AValue: string);
begin
  FStatusText := AValue;
end;

end.
