{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Filters;

interface

uses
  WinApi.Windows, System.SysUtils, System.Classes,

  WiRL.Core.Registry,
  WiRL.http.Filters,
  WiRL.Core.Request,
  WiRL.Core.Response,
  WiRL.Core.Attributes,
  WiRL.Core.Exceptions,
  WiRL.Core.Auth.Context,
  WiRL.Core.URL,
  WiRL.Core.Application,
  WiRL.http.Accept.MediaType,

  Server.Filters.Attributes, Server.Forms.Main;

type
  [PreMatching]
  TRequestLoggerFilter = class(TInterfacedObject, IWiRLContainerRequestFilter)
  private
    FMainForm: TMainForm;
  public
    procedure Filter(ARequestContext: TWiRLContainerRequestContext);
    constructor Create(MainForm: TMainForm);
  end;

  [PreMatching]
  TAbortTest = class(TInterfacedObject, IWiRLContainerRequestFilter)
  public
    procedure Filter(ARequestContext: TWiRLContainerRequestContext);
  end;

  [Priority(TWiRLPriorities.USER)] // Default priority
  TRequestCheckerFilter = class(TInterfacedObject, IWiRLContainerRequestFilter)
  private
    [Context] FAuth: TWiRLAuthContext;
    [Context] FApplication: TWiRLApplication;
  public
    procedure Filter(ARequestContext: TWiRLContainerRequestContext);
  end;

  [PoweredByWiRL]
  TResponsePoweredByFilter = class(TInterfacedObject, IWiRLContainerResponseFilter)
  public
    procedure Filter(AResponseContext: TWiRLContainerResponseContext);
  end;

  [ContentEncoding]
  TResponseEncodingFilter = class(TInterfacedObject, IWiRLContainerResponseFilter)
  private
    const ENC_GZIP = 'gzip';
    const ENC_DEFLATE = 'deflate';
    const ENC_IDENTITY = 'identity';
  public
    procedure Filter(AResponseContext: TWiRLContainerResponseContext);
  end;


implementation

uses
  System.ZLib;

{ TRequestLoggerFilter }

procedure TRequestCheckerFilter.Filter(ARequestContext: TWiRLContainerRequestContext);
begin
  if Pos('error', ARequestContext.Request.Query) > 0 then
    raise EWiRLWebApplicationException.Create(Format('Filter error test [%s]', [FApplication.Name]), 400);
end;

{ TRequestLoggerFilter }

constructor TRequestLoggerFilter.Create(MainForm: TMainForm);
begin
  FMainForm := MainForm;
end;

procedure TRequestLoggerFilter.Filter(ARequestContext: TWiRLContainerRequestContext);
var
  LMessage: string;
begin
  LMessage := DateTimeToStr(Now) + ' - ' + ARequestContext.Request.Method + ' ' + ARequestContext.Request.PathInfo;
  if ARequestContext.Request.Query <> '' then
    LMessage := LMessage + '?' + ARequestContext.Request.Query;
  FMainForm.Log(LMessage);
end;

{ TResponsePoweredByFilter }

procedure TResponsePoweredByFilter.Filter(AResponseContext: TWiRLContainerResponseContext);
begin
  AResponseContext.Response.HeaderFields['X-Powered-By'] := 'WiRL';
end;

{ TResponseEncodingFilter }

procedure TResponseEncodingFilter.Filter(AResponseContext: TWiRLContainerResponseContext);
var
  LStrStream: TStringStream;
  LMemStream: TMemoryStream;

  procedure DoCompress(ASource, ADestination: TStream);
  var
    LCompressor: TZCompressionStream;
  begin
    ASource.Seek(0, TSeekOrigin.soBeginning);

    LCompressor := TZCompressionStream.Create(clDefault, ADestination);
    try
      LCompressor.CopyFrom(ASource, ASource.Size);
    finally
      LCompressor.Free;
    end;
  end;

begin
  if AResponseContext.Request.AcceptableEncodings.Contains(ENC_DEFLATE) then
  begin
    if Assigned(AResponseContext.Response.ContentStream) then
    begin
      LMemStream := TStringStream.Create;
      try
        DoCompress(AResponseContext.Response.ContentStream, LMemStream);
        LMemStream.Position := soFromBeginning;
        AResponseContext.Response.ContentStream.Free;
        AResponseContext.Response.ContentStream := LMemStream;
      except
        LMemStream.Free;
      end;
      AResponseContext.Response.ContentEncoding := ENC_DEFLATE;
    end
    else if AResponseContext.Response.Content <> '' then
    begin
      LStrStream := TStringStream.Create(AResponseContext.Response.Content);
      LStrStream.Position := soFromBeginning;
      try
        LMemStream := TMemoryStream.Create;
        try
          DoCompress(LStrStream, LMemStream);
          LMemStream.Position := soFromBeginning;
          AResponseContext.Response.Content := '';
          AResponseContext.Response.ContentStream := LMemStream;
        except
          FreeAndNil(LMemStream);
        end;
      finally
        LStrStream.Free;
      end;
      AResponseContext.Response.ContentEncoding := ENC_DEFLATE;
    end;
  end;
end;

{ TAbortTest }

procedure TAbortTest.Filter(ARequestContext: TWiRLContainerRequestContext);
begin
  if not ARequestContext.Request.PathInfo.StartsWith('/rest') then
  begin
    ARequestContext.Response.ContentType := TMediaType.TEXT_PLAIN;
    ARequestContext.Response.Content := Format('[%s] is not a valid API URL',
      [ARequestContext.Request.PathInfo]);
    ARequestContext.Response.StatusCode := 200;
    ARequestContext.Abort;
  end;

end;

initialization
  TWiRLFilterRegistry.Instance.RegisterFilter<TRequestLoggerFilter>(
    function (): TObject
    begin
      Result := TRequestLoggerFilter.Create(MainForm);
    end
  );
  TWiRLFilterRegistry.Instance.RegisterFilter<TRequestCheckerFilter>;
  TWiRLFilterRegistry.Instance.RegisterFilter<TResponsePoweredByFilter>;
  TWiRLFilterRegistry.Instance.RegisterFilter<TResponseEncodingFilter>;
  TWiRLFilterRegistry.Instance.RegisterFilter<TAbortTest>;

end.
