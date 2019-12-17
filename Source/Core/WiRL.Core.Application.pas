{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Application;

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Classes, System.Rtti, System.Generics.Collections,

  WiRL.Configuration.JWT,
  WiRL.Configuration.Auth,
  WiRL.Configuration.Neon,

  WiRL.Core.Declarations,
  WiRL.Core.Classes,
  WiRL.Core.MessageBodyReader,
  WiRL.Core.MessageBodyWriter,
  WiRL.Core.Registry,
  WiRL.Core.Context,
  WiRL.Core.Auth.Context,
  WiRL.http.Filters,
  WiRL.Core.Injection,
  Neon.Core.Persistence;

type
  TWiRLApplication = class(TComponent)
  private
    class var FRttiContext: TRttiContext;
  private
    FResourceRegistry: TWiRLResourceRegistry;
    FFilterRegistry: TWiRLFilterRegistry;
    FWriterRegistry: TWiRLWriterRegistry;
    FReaderRegistry: TWiRLReaderRegistry;
    FBasePath: string;
    FAppName: string;
    FSystemApp: Boolean;
    FConfigJWT: TWiRLConfigurationJWT;
    FConfigAuth: TWiRLConfigurationAuth;
    FConfigNeon: TWiRLConfigurationNeon;
    FEngine: TComponent;
    FUseUTCDate: Boolean;
    function AddResource(const AResource: string): Boolean;
    function AddFilter(const AFilter: string): Boolean;
    function AddWriter(const AWriter: string): Boolean;
    function AddReader(const AReader: string): Boolean;
    function GetConfigJWT: TWiRLConfigurationJWT;
    function GetConfigAuth: TWiRLConfigurationAuth;
    function GetConfigNeon: TWiRLConfigurationNeon;

    procedure SetEngine(const Value: TComponent);
    function GetPath: string;
    procedure ReadFilters(Reader: TReader);
    procedure WriteFilters(Writer: TWriter);
    procedure ReadResources(Reader: TReader);
    procedure WriteResources(Writer: TWriter);
    procedure ReadWriters(Reader: TReader);
    procedure WriteWriters(Writer: TWriter);
    procedure ReadReaders(Reader: TReader);
    procedure WriteReaders(Writer: TWriter);
  protected
    procedure SetParentComponent(AParent: TComponent); override;
    procedure DefineProperties(Filer: TFiler); override;
  public
    class procedure InitializeRtti;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Startup;
    procedure Shutdown;

    // Fluent-like configuration methods
    function SetResources(const AResources: TArray<string>): TWiRLApplication; overload;
    function SetResources(const AResources: string): TWiRLApplication; overload;
    function SetFilters(const AFilters: TArray<string>): TWiRLApplication; overload;
    function SetFilters(const AFilters: string): TWiRLApplication; overload;
    function SetWriters(const AWriters: TArray<string>): TWiRLApplication; overload;
    function SetWriters(const AWriters: string): TWiRLApplication; overload;
    function SetReaders(const AReaders: TArray<string>): TWiRLApplication; overload;
    function SetReaders(const AReaders: string): TWiRLApplication; overload;
    function SetBasePath(const ABasePath: string): TWiRLApplication;
    function SetAppName(const AAppName: string): TWiRLApplication;
    function SetUseUTCDate(AValue: Boolean): TWiRLApplication;
    function SetSystemApp(ASystem: Boolean): TWiRLApplication;


    function ConfigureJWT: IWiRLConfigurationJWT;
    function ConfigureAuth: IWiRLConfigurationAuth;
    function ConfigureNeon: IWiRLConfigurationNeon;

    function GetResourceInfo(const AResourceName: string): TWiRLConstructorInfo;

    // Handles the parent/child relationship for the designer
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;

    property SystemApp: Boolean read FSystemApp;
    property FilterRegistry: TWiRLFilterRegistry read FFilterRegistry write FFilterRegistry;
    property WriterRegistry: TWiRLWriterRegistry read FWriterRegistry write FWriterRegistry;
    property ReaderRegistry: TWiRLReaderRegistry read FReaderRegistry write FReaderRegistry;
    property Engine: TComponent read FEngine write SetEngine;

    class property RttiContext: TRttiContext read FRttiContext;
  published
    property Path: string read GetPath;
    property AppName: string read FAppName write FAppName;
    property BasePath: string read FBasePath write FBasePath;
    property UseUTCDate: Boolean read FUseUTCDate write FUseUTCDate;

    // Fake property to display the right property editors
    property Resources: TWiRLResourceRegistry read FResourceRegistry write FResourceRegistry;
    property Filters: TWiRLFilterRegistry read FFilterRegistry write FFilterRegistry;
    property Writers: TWiRLWriterRegistry read FWriterRegistry write FWriterRegistry;
    property Readers: TWiRLReaderRegistry read FReaderRegistry write FReaderRegistry;

    property ConfigNeon: TWiRLConfigurationNeon read GetConfigNeon;
    property ConfigAuth: TWiRLConfigurationAuth read GetConfigAuth;
    property ConfigJWT: TWiRLConfigurationJWT read GetConfigJWT;
  end;

implementation

uses
  System.StrUtils, System.TypInfo,
  WiRL.Core.Exceptions,
  WiRL.Core.Utils,
  WiRL.Rtti.Utils,
  WiRL.http.URL,
  WiRL.Core.Attributes,
  WiRL.Core.Engine;

function ExtractToken(const AString: string; const ATokenIndex: Integer; const ADelimiter: Char = '/'): string;
var
  LTokens: TArray<string>;
begin
  LTokens := TArray<string>(SplitString(AString, ADelimiter));

  Result := '';
  if ATokenIndex < Length(LTokens) then
    Result := LTokens[ATokenIndex]
  else
    raise EWiRLServerException.Create(
      Format('ExtractToken, index: %d from %s', [ATokenIndex, AString]), 'ExtractToken');
end;

{ TWiRLApplication }

function TWiRLApplication.AddFilter(const AFilter: string): Boolean;
var
  LRegistry: TWiRLFilterRegistry;
  LInfo: TWiRLFilterConstructorInfo;
begin
  if csDesigning in ComponentState then
  begin
    FFilterRegistry.AddFilterName(AFilter);
    Exit(True);
  end;

  Result := False;
  LRegistry := TWiRLFilterRegistry.Instance;

  if IsMask(AFilter) then // has wildcards and so on...
  begin
    for LInfo in LRegistry do
    begin
      if MatchesMask(LInfo.TypeTClass.QualifiedClassName, AFilter) then
      begin
        FFilterRegistry.Add(LInfo);
        Result := True;
      end;
    end;
  end
  else // exact match
  begin
    if LRegistry.FilterByClassName(AFilter, LInfo) then
    begin
      FFilterRegistry.Add(LInfo);
      Result := True;
    end;
  end;
end;

function TWiRLApplication.AddReader(const AReader: string): Boolean;
var
  LGlobalRegistry: TWiRLReaderRegistry;
  LReader: TWiRLReaderRegistry.TReaderInfo;
begin
  if csDesigning in ComponentState then
  begin
    FReaderRegistry.AddReaderName(AReader);
    Exit(True);
  end;

  Result := False;
  LGlobalRegistry := TMessageBodyReaderRegistry.Instance;

  if IsMask(AReader) then // has wildcards and so on...
  begin
    FReaderRegistry.Assign(LGlobalRegistry);
    Result := True;
  end
  else // exact match
  begin
    LReader := LGlobalRegistry.GetReaderByName(AReader);
    if Assigned(LReader) then
    begin
      FReaderRegistry.Add(LReader);
      Result := True;
    end;
  end;
end;

function TWiRLApplication.AddResource(const AResource: string): Boolean;

  function AddResourceToApplicationRegistry(const AInfo: TWiRLConstructorInfo): Boolean;
  var
    LClass: TClass;
    LResult: Boolean;
  begin
    LResult := False;
    LClass := AInfo.TypeTClass;
    TRttiHelper.HasAttribute<PathAttribute>(FRttiContext.GetType(LClass),
      procedure (AAttribute: PathAttribute)
      var
        LURL: TWiRLURL;
      begin
        LURL := TWiRLURL.MockURL(AAttribute.Value);
        try
          if not FResourceRegistry.ContainsKey(LURL.PathTokens[0]) then
          begin
            FResourceRegistry.Add(LURL.PathTokens[0], AInfo.Clone);
            LResult := True;
          end;
        finally
          LURL.Free;
        end;
      end
    );
    Result := LResult;
  end;

var
  LRegistry: TWiRLResourceRegistry;
  LInfo: TWiRLConstructorInfo;
  LKey: string;
begin
  if csDesigning in ComponentState then
  begin
    FResourceRegistry.AddResourceName(AResource);
    Exit(True);
  end;

  Result := False;
  LRegistry := TWiRLResourceRegistry.Instance;

  if IsMask(AResource) then // has wildcards and so on...
  begin
    for LKey in LRegistry.Keys.ToArray do
    begin
      if MatchesMask(LKey, AResource) then
      begin
        if LRegistry.TryGetValue(LKey, LInfo) and AddResourceToApplicationRegistry(LInfo) then
          Result := True;
      end;
    end;
  end
  else // exact match
    if LRegistry.TryGetValue(AResource, LInfo) then
      Result := AddResourceToApplicationRegistry(LInfo);
end;

function TWiRLApplication.AddWriter(const AWriter: string): Boolean;
var
  LGlobalRegistry: TWiRLWriterRegistry;
  LWriter: TWiRLWriterRegistry.TWriterInfo;
begin
  if csDesigning in ComponentState then
  begin
    FWriterRegistry.AddWriterName(AWriter);
    Exit(True);
  end;

  Result := False;
  LGlobalRegistry := TMessageBodyWriterRegistry.Instance;

  if IsMask(AWriter) then // has wildcards and so on...
  begin
    FWriterRegistry.Assign(LGlobalRegistry);
    Result := True;
  end
  else // exact match
  begin
    LWriter := LGlobalRegistry.GetWriterByName(AWriter);
    if Assigned(LWriter) then
    begin
      FWriterRegistry.Add(LWriter);
      Result := True;
    end;
  end;
end;

function TWiRLApplication.SetBasePath(const ABasePath: string): TWiRLApplication;
begin
  FBasePath := ABasePath;
  Result := Self;
end;

function TWiRLApplication.SetAppName(const AAppName: string): TWiRLApplication;
begin
  FAppName := AAppName;
  Result := Self;
end;

procedure TWiRLApplication.SetEngine(const Value: TComponent);
begin
  if FEngine <> Value then
  begin
    if Assigned(FEngine) then
      (FEngine as TWiRLEngine).RemoveApplication(Self);
    FEngine := Value;
    if Assigned(FEngine) then
      (FEngine as TWiRLEngine).AddApplication(Self);
  end;
end;

function TWiRLApplication.SetReaders(const AReaders: TArray<string>): TWiRLApplication;
var
  LReader: string;
begin
  Result := Self;
  for LReader in AReaders do
    Self.AddReader(LReader);
end;

function TWiRLApplication.SetReaders(const AReaders: string): TWiRLApplication;
begin
  Result := SetReaders(AReaders.Split([',']));
end;

function TWiRLApplication.SetResources(const AResources: string): TWiRLApplication;
begin
  Result := SetResources(AResources.Split([',']));
end;

function TWiRLApplication.SetFilters(const AFilters: string): TWiRLApplication;
begin
  Result := SetFilters(AFilters.Split([',']));
end;

procedure TWiRLApplication.SetParentComponent(AParent: TComponent);
begin
  inherited;
  if AParent is TWiRLEngine then
    Engine := AParent as TWiRLEngine;
end;

function TWiRLApplication.SetWriters(const AWriters: TArray<string>): TWiRLApplication;
var
  LWriter: string;
begin
  Result := Self;
  for LWriter in AWriters do
    Self.AddWriter(LWriter);
end;

function TWiRLApplication.SetWriters(const AWriters: string): TWiRLApplication;
begin
  Result := SetWriters(AWriters.Split([',']));
end;

procedure TWiRLApplication.Shutdown;
begin

end;

procedure TWiRLApplication.Startup;
begin
  if FWriterRegistry.Count = 0 then
    FWriterRegistry.Assign(TMessageBodyWriterRegistry.Instance);

  if FReaderRegistry.Count = 0 then
    FReaderRegistry.Assign(TMessageBodyReaderRegistry.Instance);

  if not Assigned(FConfigNeon) then
    FConfigNeon := TWiRLConfigurationNeon.Create;
end;

procedure TWiRLApplication.WriteFilters(Writer: TWriter);
var
  LFilter: TWiRLFilterConstructorInfo;
begin
  Writer.WriteListBegin;
  for LFilter in FFilterRegistry do
    Writer.WriteString(LFilter.FilterQualifiedClassName);
  Writer.WriteListEnd;
end;

procedure TWiRLApplication.WriteReaders(Writer: TWriter);
var
  LReaderInfo: TWiRLReaderRegistry.TReaderInfo;
begin
  Writer.WriteListBegin;
  for LReaderInfo in FReaderRegistry do
    Writer.WriteString(LReaderInfo.ReaderName);
  Writer.WriteListEnd;
end;

procedure TWiRLApplication.WriteResources(Writer: TWriter);
var
  LResourceName: string;
begin
  Writer.WriteListBegin;
  for LResourceName in FResourceRegistry.Keys do
    Writer.WriteString(LResourceName);
  Writer.WriteListEnd;
end;

procedure TWiRLApplication.WriteWriters(Writer: TWriter);
var
  LWriterInfo: TWiRLWriterRegistry.TWriterInfo;
begin
  Writer.WriteListBegin;
  for LWriterInfo in FWriterRegistry do
    Writer.WriteString(LWriterInfo.WriterName);
  Writer.WriteListEnd;
end;

function TWiRLApplication.SetFilters(const AFilters: TArray<string>): TWiRLApplication;
var
  LFilter: string;
begin
  Result := Self;
  for LFilter in AFilters do
    Self.AddFilter(LFilter);
end;

function TWiRLApplication.SetResources(const AResources: TArray<string>): TWiRLApplication;
var
  LResource: string;
begin
  Result := Self;
  for LResource in AResources do
    Self.AddResource(LResource);
end;

function TWiRLApplication.ConfigureAuth: IWiRLConfigurationAuth;
begin
  Result := ConfigAuth;
end;

function TWiRLApplication.ConfigureJWT: IWiRLConfigurationJWT;
begin
  Result := ConfigJWT;
end;

function TWiRLApplication.ConfigureNeon: IWiRLConfigurationNeon;
begin
  Result := GetConfigNeon;
end;

constructor TWiRLApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FResourceRegistry := TWiRLResourceRegistry.Create;
  FFilterRegistry := TWiRLFilterRegistry.Create;
  FFilterRegistry.OwnsObjects := False;
  FWriterRegistry := TWiRLWriterRegistry.Create(False);
  FReaderRegistry := TWiRLReaderRegistry.Create(False);

  if csDesigning in ComponentState then
  begin
    AddResource('*');
    AddFilter('*');
  end;
end;

procedure TWiRLApplication.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Resource.List', ReadResources, WriteResources, FResourceRegistry.Count > 0);
  Filer.DefineProperty('Filter.List', ReadFilters, WriteFilters, FFilterRegistry.Count > 0);
  Filer.DefineProperty('Reader.List', ReadReaders, WriteReaders, FReaderRegistry.Count > 0);
  Filer.DefineProperty('Writer.List', ReadWriters, WriteWriters, FWriterRegistry.Count > 0);
end;

destructor TWiRLApplication.Destroy;
begin
  Engine := nil;
  FReaderRegistry.Free;
  FWriterRegistry.Free;
  FFilterRegistry.Free;
  FResourceRegistry.Free;
  FConfigJWT.Free;

  inherited;
end;

function TWiRLApplication.GetConfigAuth: TWiRLConfigurationAuth;
begin
  if not Assigned(FConfigAuth) then
    FConfigAuth := TWiRLConfigurationAuth.Default;
  Result := FConfigAuth;
end;

function TWiRLApplication.GetConfigJWT: TWiRLConfigurationJWT;
begin
  if not Assigned(FConfigJWT) then
    FConfigJWT := TWiRLConfigurationJWT.Create;
  Result := FConfigJWT;
end;

function TWiRLApplication.GetParentComponent: TComponent;
begin
  Result := FEngine;
end;

function TWiRLApplication.GetPath: string;
begin
  if not Assigned(FEngine) then
    Result := BasePath
  else
    Result := TWiRLURL.CombinePath([(FEngine as TWiRLEngine).BasePath, BasePath]);
end;

function TWiRLApplication.GetResourceInfo(const AResourceName: string): TWiRLConstructorInfo;
begin
  FResourceRegistry.TryGetValue(AResourceName, Result);
end;

function TWiRLApplication.GetConfigNeon: TWiRLConfigurationNeon;
begin
  if not Assigned(FConfigNeon) then
    FConfigNeon := TWiRLConfigurationNeon.Create;
  Result := FConfigNeon;
end;

function TWiRLApplication.HasParent: Boolean;
begin
  Result := Assigned(FEngine);
end;

class procedure TWiRLApplication.InitializeRtti;
begin
  FRttiContext := TRttiContext.Create;
end;

procedure TWiRLApplication.ReadFilters(Reader: TReader);
begin
  Reader.ReadListBegin;
  FFilterRegistry.Clear;
  while not Reader.EndOfList do
    AddFilter(Reader.ReadString);
  Reader.ReadListEnd;
end;

procedure TWiRLApplication.ReadReaders(Reader: TReader);
begin
  Reader.ReadListBegin;
  FReaderRegistry.Clear;
  while not Reader.EndOfList do
    AddReader(Reader.ReadString);
  Reader.ReadListEnd;
end;

procedure TWiRLApplication.ReadResources(Reader: TReader);
begin
  Reader.ReadListBegin;
  FResourceRegistry.Clear;
  while not Reader.EndOfList do
    AddResource(Reader.ReadString);
  Reader.ReadListEnd;
end;

procedure TWiRLApplication.ReadWriters(Reader: TReader);
begin
  Reader.ReadListBegin;
  FWriterRegistry.Clear;
  while not Reader.EndOfList do
  begin
    AddWriter(Reader.ReadString);
  end;
  Reader.ReadListEnd;
end;

function TWiRLApplication.SetSystemApp(ASystem: Boolean): TWiRLApplication;
begin
  FSystemApp := ASystem;
  Result := Self;
end;

function TWiRLApplication.SetUseUTCDate(AValue: Boolean): TWiRLApplication;
begin
  FUseUTCDate := AValue;
  Result := Self;
end;

initialization
  TWiRLApplication.InitializeRtti;

end.


