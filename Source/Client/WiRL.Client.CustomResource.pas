{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Client.CustomResource;

{$I WiRL.inc}

interface

uses
  System.SysUtils, System.Classes,
  WiRL.Client.Application,
  WiRL.Client.Client;

type
  TWiRLClientProc = TProc;
  TWiRLClientResponseProc = TProc<TStream>;
  TWiRLClientExceptionProc = TProc<Exception>;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  TWiRLClientCustomResource = class(TComponent)
  private
    FResource: string;
    FApplication: TWiRLClientApplication;
    FSpecificClient: TWiRLClient;
    FPathParamsValues: TStrings;
    FQueryParams: TStrings;
    FSpecificAccept: string;
    FSpecificContentType: string;
    procedure SetPathParamsValues(const Value: TStrings);
    procedure SetQueryParams(const Value: TStrings);
  protected
    function GetClient: TWiRLClient; virtual;
    function GetPath: string; virtual;
    function GetURL: string; virtual;
    function GetApplication: TWiRLClientApplication; virtual;
    function GetAccept: string;
    function GetContentType: string;

    procedure BeforeGET; virtual;
    procedure AfterGET; virtual;

    procedure BeforePOST(AContent: TMemoryStream); virtual;
    procedure AfterPOST; virtual;

    procedure BeforePUT(AContent: TMemoryStream); virtual;
    procedure AfterPUT; virtual;

    procedure BeforePATCH(AContent: TMemoryStream); virtual;
    procedure AfterPATCH; virtual;

    procedure BeforeHEAD; virtual;
    procedure AfterHEAD; virtual;

    procedure BeforeDELETE; virtual;
    procedure AfterDELETE; virtual;

    procedure BeforeOPTIONS; virtual;
    procedure AfterOPTIONS; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // http verbs
    procedure GET(const ABeforeExecute: TWiRLClientProc = nil;
      const AAfterExecute: TWiRLClientResponseProc = nil;
      const AOnException: TWiRLClientExceptionProc = nil); overload;

    function GETAsString(AEncoding: TEncoding = nil;
      const ABeforeExecute: TWiRLClientProc = nil;
      const AOnException: TWiRLClientExceptionProc = nil): string; overload;

    procedure GETAsync(const ACompletionHandler: TWiRLClientProc = nil;
      const AOnException: TWiRLClientExceptionProc = nil;
      ASynchronize: Boolean = True);

    procedure POST(const ABeforeExecute: TProc<TMemoryStream> = nil;
      const AAfterExecute: TWiRLClientResponseProc = nil;
      const AOnException: TWiRLClientExceptionProc = nil);

    procedure POSTAsync(const ACompletionHandler: TWiRLClientProc = nil;
      const AOnException: TWiRLClientExceptionProc = nil;
      ASynchronize: Boolean = True);

    procedure PUT(const ABeforeExecute: TProc<TMemoryStream> = nil;
      const AAfterExecute: TWiRLClientResponseProc = nil;
      const AOnException: TWiRLClientExceptionProc = nil);

    procedure DELETE(const ABeforeExecute: TWiRLClientProc = nil;
      const AAfterExecute: TWiRLClientProc = nil;
      const AOnException: TWiRLClientExceptionProc = nil);

    procedure PATCH(const ABeforeExecute: TProc<TMemoryStream> = nil;
      const AAfterExecute: TWiRLClientResponseProc = nil;
      const AOnException: TWiRLClientExceptionProc = nil);

    procedure HEAD(const ABeforeExecute: TWiRLClientProc = nil;
      const AAfterExecute: TWiRLClientProc = nil;
      const AOnException: TWiRLClientExceptionProc = nil);

    procedure OPTIONS(const ABeforeExecute: TWiRLClientProc = nil;
      const AAfterExecute: TWiRLClientResponseProc = nil;
      const AOnException: TWiRLClientExceptionProc = nil);
  public
    property Accept: string read GetAccept;
    property ContentType: string read GetContentType;
    property Application: TWiRLClientApplication read GetApplication write FApplication;
    property Client: TWiRLClient read GetClient;
    property SpecificAccept: string read FSpecificAccept write FSpecificAccept;
    property SpecificContentType: string read FSpecificContentType write FSpecificContentType;
    property SpecificClient: TWiRLClient read FSpecificClient write FSpecificClient;
    property Resource: string read FResource write FResource;
    property Path: string read GetPath;
    property PathParamsValues: TStrings read FPathParamsValues write SetPathParamsValues;
    property QueryParams: TStrings read FQueryParams write SetQueryParams;
    property URL: string read GetURL;
  end;


implementation

uses
  WiRL.Client.Utils,
  WiRL.Core.URL,
  WiRL.Core.Utils;

{ TWiRLClientCustomResource }

procedure TWiRLClientCustomResource.AfterDELETE;
begin

end;

procedure TWiRLClientCustomResource.AfterGET;
begin

end;

procedure TWiRLClientCustomResource.AfterHEAD;
begin

end;

procedure TWiRLClientCustomResource.AfterOPTIONS;
begin

end;

procedure TWiRLClientCustomResource.AfterPATCH;
begin

end;

procedure TWiRLClientCustomResource.AfterPOST;
begin

end;

procedure TWiRLClientCustomResource.AfterPUT;
begin

end;

procedure TWiRLClientCustomResource.BeforeDELETE;
begin

end;

procedure TWiRLClientCustomResource.BeforeGET;
begin

end;

procedure TWiRLClientCustomResource.BeforeHEAD;
begin

end;

procedure TWiRLClientCustomResource.BeforeOPTIONS;
begin

end;

procedure TWiRLClientCustomResource.BeforePATCH(AContent: TMemoryStream);
begin

end;

procedure TWiRLClientCustomResource.BeforePOST(AContent: TMemoryStream);
begin

end;

procedure TWiRLClientCustomResource.BeforePUT(AContent: TMemoryStream);
begin

end;

constructor TWiRLClientCustomResource.Create(AOwner: TComponent);
begin
  inherited;
  FResource := 'main';
  if TWiRLComponentHelper.IsDesigning(Self) then
    FApplication := TWiRLComponentHelper.FindDefault<TWiRLClientApplication>(Self);
  FPathParamsValues := TStringList.Create;
  FQueryParams := TStringList.Create;
end;

function TWiRLClientCustomResource.GetClient: TWiRLClient;
begin
  Result := nil;
  if Assigned(FSpecificClient) then
    Result := FSpecificClient
  else
  begin
    if Assigned(FApplication) then
      Result := FApplication.Client;
  end;
end;

function TWiRLClientCustomResource.GetContentType: string;
begin
  Result := FSpecificContentType;
  if (Result = '') and Assigned(Application) then
    Result := Application.DefaultMediaType;
end;

function TWiRLClientCustomResource.GetPath: string;
var
  LEngine: string;
  LApplication: string;
begin
  LEngine := '';
  if Assigned(Client) then
    LEngine := Client.WiRLEngineURL;

  LApplication := '';
  if Assigned(Application) then
    LApplication := Application.AppName;


  Result := TWiRLURL.CombinePath([LEngine, LApplication, Resource]);
end;


function TWiRLClientCustomResource.GetURL: string;
begin
  Result := TWiRLURL.CombinePath([
    Path,
    TWiRLURL.CombinePath(TWiRLURL.URLEncode(FPathParamsValues.ToStringArray))
  ]);

  if FQueryParams.Count > 0 then
    Result := Result + '?' + SmartConcat(TWiRLURL.URLEncode(FQueryParams.ToStringArray), '&');
end;

procedure TWiRLClientCustomResource.HEAD(const ABeforeExecute,
  AAfterExecute: TWiRLClientProc; const AOnException: TWiRLClientExceptionProc);
begin
  try
    BeforeHEAD;

    if Assigned(ABeforeExecute) then
      ABeforeExecute();

    Client.Head(URL, Accept, ContentType);

    AfterHEAD();

    if Assigned(AAfterExecute) then
      AAfterExecute();
  except
    on E: Exception do
    begin
      if Assigned(AOnException) then
        AOnException(E)
      else
        raise Exception.Create(E.Message);
    end;
  end;
end;

procedure TWiRLClientCustomResource.OPTIONS(const ABeforeExecute: TWiRLClientProc = nil;
  const AAfterExecute: TWiRLClientResponseProc = nil;
  const AOnException: TWiRLClientExceptionProc = nil);
var
  LResponseStream: TMemoryStream;
begin
  try
    BeforeOPTIONS();

    if Assigned(ABeforeExecute) then
      ABeforeExecute();

    LResponseStream := TMemoryStream.Create;
    try
      Client.Options(URL, Accept, ContentType, LResponseStream);

      AfterOPTIONS();

      if Assigned(AAfterExecute) then
        AAfterExecute(LResponseStream);
    finally
      LResponseStream.Free;
    end;
  except
    on E: Exception do
    begin
      if Assigned(AOnException) then
        AOnException(E)
      else
        raise Exception.Create(E.Message);
    end;
  end;
end;

procedure TWiRLClientCustomResource.DELETE(const ABeforeExecute,
  AAfterExecute: TWiRLClientProc; const AOnException: TWiRLClientExceptionProc);
var
  LResponseStream: TMemoryStream;
begin
  try
    BeforeDELETE();

    if Assigned(ABeforeExecute) then
      ABeforeExecute();

    LResponseStream := TMemoryStream.Create;
    try
      Client.Delete(URL, Accept, ContentType, LResponseStream);

      AfterDELETE();

      if Assigned(AAfterExecute) then
        AAfterExecute();
    finally
      LResponseStream.Free;
    end;
  except
    on E: Exception do
    begin
      if Assigned(AOnException) then
        AOnException(E)
      else
        raise Exception.Create(E.Message);
    end;
  end;
end;

destructor TWiRLClientCustomResource.Destroy;
begin
  FPathParamsValues.Free;
  FQueryParams.Free;
  inherited;
end;

procedure TWiRLClientCustomResource.GET(const ABeforeExecute: TWiRLCLientProc;
  const AAfterExecute: TWiRLClientResponseProc;
  const AOnException: TWiRLClientExceptionProc);
var
  LResponseStream: TMemoryStream;
begin
  try
    BeforeGET();

    if Assigned(ABeforeExecute) then
      ABeforeExecute();

    LResponseStream := TMemoryStream.Create;
    try
      Client.Get(URL, Accept, ContentType, LResponseStream);

      AfterGET();

      if Assigned(AAfterExecute) then
        AAfterExecute(LResponseStream);
    finally
      LResponseStream.Free;
    end;
  except
    on E:Exception do
    begin
      if Assigned(AOnException) then
        AOnException(E)
      else
        raise Exception.Create(E.Message);
    end;
  end;
end;

function TWiRLClientCustomResource.GETAsString(AEncoding: TEncoding;
  const ABeforeExecute: TWiRLClientProc;
  const AOnException: TWiRLClientExceptionProc): string;
var
  LResult: string;
  LEncoding: TEncoding;
begin
  LResult := '';
  LEncoding := AEncoding;
  if not Assigned(LEncoding) then
    LEncoding := TEncoding.Default;

  GET(ABeforeExecute,
    procedure (AResponse: TStream)
    var
      LStreamReader: TStreamReader;
    begin
      AResponse.Position := 0;
      LStreamReader := TStreamReader.Create(AResponse, LEncoding);
      try
        LResult := LStreamReader.ReadToEnd;
      finally
        LStreamReader.Free;
      end;
    end,
    AOnException
  );
  Result := LResult;
end;

function TWiRLClientCustomResource.GetAccept: string;
begin
  Result := FSpecificAccept;
  if (Result = '') and Assigned(Application) then
    Result := Application.DefaultMediaType;
end;

function TWiRLClientCustomResource.GetApplication: TWiRLClientApplication;
begin
  Result := FApplication;
end;

procedure TWiRLClientCustomResource.GETAsync(
  const ACompletionHandler: TWiRLClientProc;
  const AOnException: TWiRLClientExceptionProc;
  ASynchronize: Boolean);
begin
  Client.ExecuteAsync(
    procedure
    begin
      GET(nil, nil, AOnException);
      if Assigned(ACompletionHandler) then
      begin
        if ASynchronize then
          TThread.Queue(nil, TThreadProcedure(ACompletionHandler))
        else
          ACompletionHandler();
      end;
    end
  );
end;

procedure TWiRLClientCustomResource.PATCH(const ABeforeExecute: TProc<TMemoryStream> = nil;
  const AAfterExecute: TWiRLClientResponseProc = nil;
  const AOnException: TWiRLClientExceptionProc = nil);
var
  LResponseStream: TMemoryStream;
  LContent: TMemoryStream;
begin
  try
    LContent := TMemoryStream.Create;
    try
      BeforePATCH(LContent);

      if Assigned(ABeforeExecute) then
        ABeforeExecute(LContent);

      LResponseStream := TMemoryStream.Create;
      try
        Client.Patch(URL, Accept, ContentType, LContent, LResponseStream);

        AfterPATCH();

        if Assigned(AAfterExecute) then
          AAfterExecute(LResponseStream);
      finally
        LResponseStream.Free;
      end;
    finally
      LContent.Free;
    end;
  except
    on E: Exception do
    begin
      if Assigned(AOnException) then
        AOnException(E)
      else
        raise Exception.Create(E.Message);
    end;
  end;
end;

procedure TWiRLClientCustomResource.POST(
  const ABeforeExecute: TProc<TMemoryStream>;
  const AAfterExecute: TWiRLClientResponseProc;
  const AOnException: TWiRLClientExceptionProc);
var
  LResponseStream: TMemoryStream;
  LContent: TMemoryStream;
begin
  try
    LContent := TMemoryStream.Create;
    try
      BeforePOST(LContent);

      if Assigned(ABeforeExecute) then
        ABeforeExecute(LContent);

      LResponseStream := TMemoryStream.Create;
      try
        Client.Post(URL, Accept, ContentType, LContent, LResponseStream);

        AfterPOST();

        if Assigned(AAfterExecute) then
          AAfterExecute(LResponseStream);
      finally
        LResponseStream.Free;
      end;
    finally
      LContent.Free;
    end;
  except
    on E:Exception do
    begin
      if Assigned(AOnException) then
        AOnException(E)
      else
        raise Exception.Create(E.Message);
    end;
  end;
end;

procedure TWiRLClientCustomResource.POSTAsync(
  const ACompletionHandler: TWiRLClientProc;
  const AOnException: TWiRLClientExceptionProc;
  ASynchronize: Boolean);
begin
  Client.ExecuteAsync(
    procedure
    begin
      POST(nil, nil, AOnException);
      if Assigned(ACompletionHandler) then
      begin
        if ASynchronize then
          TThread.Queue(nil, TThreadProcedure(ACompletionHandler))
        else
          ACompletionHandler();
      end;
    end
  );
end;

procedure TWiRLClientCustomResource.PUT(const ABeforeExecute: TProc<TMemoryStream> = nil;
  const AAfterExecute: TWiRLClientResponseProc = nil;
  const AOnException: TWiRLClientExceptionProc = nil);
var
  LResponseStream: TMemoryStream;
  LContent: TMemoryStream;
begin
  try
    LContent := TMemoryStream.Create;
    try
      BeforePUT(LContent);

      if Assigned(ABeforeExecute) then
        ABeforeExecute(LContent);

      LResponseStream := TMemoryStream.Create;
      try
        Client.Put(URL, Accept, ContentType, LContent, LResponseStream);

        AfterPUT();

        if Assigned(AAfterExecute) then
          AAfterExecute(LResponseStream);
      finally
        LResponseStream.Free;
      end;
    finally
      LContent.Free;
    end;
  except
    on E:Exception do
    begin
      if Assigned(AOnException) then
        AOnException(E)
      else
        raise Exception.Create(E.Message);
    end;
  end;
end;

procedure TWiRLClientCustomResource.SetPathParamsValues(const Value: TStrings);
begin
  FPathParamsValues.Assign(Value);
end;

procedure TWiRLClientCustomResource.SetQueryParams(const Value: TStrings);
begin
  FQueryParams.Assign(Value);
end;

end.
