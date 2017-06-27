{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Application;

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Classes, System.Rtti, System.Generics.Collections,

  WiRL.Core.Classes,
  WiRL.Core.MessageBodyReader,
  WiRL.Core.MessageBodyWriter,
  WiRL.Core.Registry,
  WiRL.Core.Context,
  WiRL.Core.Auth.Context,
  WiRL.http.Filters,
  WiRL.Core.Injection;

type
  TAuthChallenge = (Basic, Digest, Bearer, Forms);

  TAuthChallengeHelper = record helper for TAuthChallenge
    function ToString: string;
  end;

  TAuthTokenLocation = (Bearer, Cookie, Header);
  TSecretGenerator = reference to function(): TBytes;
  TAttributeArray = TArray<TCustomAttribute>;
  TArgumentArray = array of TValue;

  TWiRLApplication = class
  private
    //256bit encoding key
    const SCRT_SGN = 'd2lybC5zdXBlcnNlY3JldC5zZWVkLmZvci5zaWduaW5n';
  private
    class var FRttiContext: TRttiContext;
  private
    FSecret: TBytes;
    FResourceRegistry: TObjectDictionary<string, TWiRLConstructorInfo>;
    FFilterRegistry: TWiRLFilterRegistry;
    FWriterRegistry: TWiRLWriterRegistry;
    FReaderRegistry: TWiRLReaderRegistry;
    FBasePath: string;
    FName: string;
    FClaimClass: TWiRLSubjectClass;
    FSystemApp: Boolean;
    FAuthChallenge: TAuthChallenge;
    FRealmChallenge: string;
    FTokenLocation: TAuthTokenLocation;
    FTokenCustomHeader: string;
    function GetResources: TArray<string>;
    function AddResource(const AResource: string): Boolean;
    function AddFilter(const AFilter: string): Boolean;
    function AddWriter(const AWriter: string): Boolean;
    function AddReader(const AReader: string): Boolean;
    function GetSecret: TBytes;
    function GetAuthChallengeHeader: string;
  public
    class procedure InitializeRtti;

    constructor Create;
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
    function SetSecret(const ASecret: TBytes): TWiRLApplication; overload;
    function SetSecret(ASecretGen: TSecretGenerator): TWiRLApplication; overload;
    function SetBasePath(const ABasePath: string): TWiRLApplication;
    function SetAuthChallenge(AChallenge: TAuthChallenge; const ARealm: string): TWiRLApplication;
    function SetTokenLocation(ALocation: TAuthTokenLocation): TWiRLApplication;
    function SetTokenCustomHeader(const ACustomHeader: string): TWiRLApplication;
    function SetName(const AName: string): TWiRLApplication;
    function SetClaimsClass(AClaimClass: TWiRLSubjectClass): TWiRLApplication;
    function SetSystemApp(ASystem: Boolean): TWiRLApplication;

    function GetResourceInfo(const AResourceName: string): TWiRLConstructorInfo;

    property Name: string read FName;
    property BasePath: string read FBasePath;
    property SystemApp: Boolean read FSystemApp;
    property ClaimClass: TWiRLSubjectClass read FClaimClass;
    property FilterRegistry: TWiRLFilterRegistry read FFilterRegistry write FFilterRegistry;
    property WriterRegistry: TWiRLWriterRegistry read FWriterRegistry write FWriterRegistry;
    property ReaderRegistry: TWiRLReaderRegistry read FReaderRegistry write FReaderRegistry;
    property Resources: TArray<string> read GetResources;
    property Secret: TBytes read GetSecret;
    property AuthChallengeHeader: string read GetAuthChallengeHeader;
    property TokenLocation: TAuthTokenLocation read FTokenLocation;
    property TokenCustomHeader: string read FTokenCustomHeader;

    class property RttiContext: TRttiContext read FRttiContext;
  end;

  TWiRLApplicationDictionary = class(TObjectDictionary<string, TWiRLApplication>)
  end;


implementation

uses
  System.StrUtils, System.TypInfo,
  WiRL.Core.Exceptions,
  WiRL.Core.Utils,
  WiRL.Rtti.Utils,
  WiRL.Core.URL,
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
        LURL := TWiRLURL.CreateDummy(AAttribute.Value);
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

function TWiRLApplication.SetAuthChallenge(AChallenge: TAuthChallenge;
  const ARealm: string): TWiRLApplication;
begin
  FAuthChallenge := AChallenge;
  FRealmChallenge := ARealm;
  Result := Self;
end;

function TWiRLApplication.SetBasePath(const ABasePath: string): TWiRLApplication;
begin
  FBasePath := ABasePath;
  Result := Self;
end;

function TWiRLApplication.SetName(const AName: string): TWiRLApplication;
begin
  FName := AName;
  Result := Self;
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

function TWiRLApplication.SetClaimsClass(AClaimClass: TWiRLSubjectClass): TWiRLApplication;
begin
  FClaimClass := AClaimClass;
  Result := Self;
end;

function TWiRLApplication.SetFilters(const AFilters: string): TWiRLApplication;
begin
  Result := SetFilters(AFilters.Split([',']));
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

constructor TWiRLApplication.Create;
begin
  inherited Create;
  FResourceRegistry := TObjectDictionary<string, TWiRLConstructorInfo>.Create([doOwnsValues]);
  FFilterRegistry := TWiRLFilterRegistry.Create;
  FFilterRegistry.OwnsObjects := False;
  FWriterRegistry := TWiRLWriterRegistry.Create(False);
  FReaderRegistry := TWiRLReaderRegistry.Create(False);
  FSecret := TEncoding.ANSI.GetBytes(SCRT_SGN);
end;

destructor TWiRLApplication.Destroy;
begin
  FReaderRegistry.Free;
  FWriterRegistry.Free;
  FFilterRegistry.Free;
  FResourceRegistry.Free;
  inherited;
end;

function TWiRLApplication.GetAuthChallengeHeader: string;
begin
  if FRealmChallenge.IsEmpty then
    Result := FAuthChallenge.ToString
  else
    Result := Format('%s realm="%s"', [FAuthChallenge.ToString, FRealmChallenge])
end;

function TWiRLApplication.GetResourceInfo(const AResourceName: string): TWiRLConstructorInfo;
begin
  FResourceRegistry.TryGetValue(AResourceName, Result);
end;

function TWiRLApplication.GetResources: TArray<string>;
begin
  Result := FResourceRegistry.Keys.ToArray;
end;

function TWiRLApplication.GetSecret: TBytes;
begin
  Result := FSecret;
end;

class procedure TWiRLApplication.InitializeRtti;
begin
  FRttiContext := TRttiContext.Create;
end;

function TWiRLApplication.SetSecret(ASecretGen: TSecretGenerator): TWiRLApplication;
begin
  if Assigned(ASecretGen) then
    FSecret := ASecretGen;
  Result := Self;
end;

function TWiRLApplication.SetSecret(const ASecret: TBytes): TWiRLApplication;
begin
  FSecret := ASecret;
  Result := Self;
end;

function TWiRLApplication.SetSystemApp(ASystem: Boolean): TWiRLApplication;
begin
  FSystemApp := ASystem;
  Result := Self;
end;

function TWiRLApplication.SetTokenCustomHeader(const ACustomHeader: string): TWiRLApplication;
begin
  FTokenCustomHeader := ACustomHeader;
  Result := Self;
end;

function TWiRLApplication.SetTokenLocation(ALocation: TAuthTokenLocation): TWiRLApplication;
begin
  FTokenLocation := ALocation;
  Result := Self;
end;

{ TAuthChallengeHelper }

function TAuthChallengeHelper.ToString: string;
begin
  case Self of
    TAuthChallenge.Basic:  Result := 'Basic';
    TAuthChallenge.Digest: Result := 'Digest';
    TAuthChallenge.Bearer: Result := 'Bearer';
    TAuthChallenge.Forms:  Result := 'Forms';
  end;
end;

initialization
  TWiRLApplication.InitializeRtti;

end.
