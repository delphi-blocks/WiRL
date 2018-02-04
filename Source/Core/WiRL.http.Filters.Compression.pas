{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Filters.Compression;

interface

uses
  WinApi.Windows, System.SysUtils, System.Classes,

  WiRL.Core.Registry,
  WiRL.http.Filters,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.Core.Attributes,
  WiRL.Core.Exceptions,
  WiRL.http.URL,
  WiRL.Core.Application,
  WiRL.http.Accept.MediaType;

type
  [NameBinding]
  ContentEncodingAttribute = class(TCustomAttribute);

  [NameBinding]
  ContentDecodingAttribute = class(TCustomAttribute);

  /// <summary>
  ///   Base class for the encoding/decoding filters, only to introduce Encoding header
  ///   values
  /// </summary>
  TCompressionFilter = class(TInterfacedObject)
  protected
    const ENC_GZIP = 'gzip';
    const ENC_DEFLATE = 'deflate';
    const ENC_IDENTITY = 'identity';
  end;

  /// <summary>
  ///   HTTP Request filter. If the request has the "deflate" ContentEncoding then the
  ///   filter decompress the body.
  /// </summary>
  /// <remarks>
  ///   Only the "deflate" ContentEncoding is supported
  /// </remarks>
  [ContentDecoding]
  TRequestDecodingFilter = class(TCompressionFilter, IWiRLContainerRequestFilter)
  public
    procedure Filter(ARequestContext: TWiRLContainerRequestContext);
  end;

  /// <summary>
  ///   HTTP Response filter. If the request has the "deflate" encoding in the Accept-Encoding header
  ///   this filter compresses the body stream
  /// </summary>
  /// <remarks>
  ///   Only the "deflate" ContentEncoding is supported
  /// </remarks>
  [ContentEncoding]
  TResponseEncodingFilter = class(TCompressionFilter, IWiRLContainerResponseFilter)
  public
    procedure Filter(AResponseContext: TWiRLContainerResponseContext);
  end;

  /// <summary>
  ///   HTTP Response filter. Useful to save the compressed stream
  /// </summary>
  /// <remarks>
  ///   Not registered by default
  /// </remarks>
  [ContentEncoding]
  TResponseEncodingFilterDebug = class(TCompressionFilter, IWiRLContainerResponseFilter)
  public
    procedure Filter(AResponseContext: TWiRLContainerResponseContext);
  end;

implementation

uses
  System.ZLib, System.IOUtils;

{ TRequestDecodingFilter }

procedure TRequestDecodingFilter.Filter(ARequestContext: TWiRLContainerRequestContext);
var
  LStrStream: TStringStream;
  LMemStream: TMemoryStream;

  procedure DoDecompress(ASource, ADestination: TStream);
  var
    LDecompressor: TZDecompressionStream;
  begin
    ASource.Seek(0, TSeekOrigin.soBeginning);

    LDecompressor := TZDecompressionStream.Create(ASource);
    try
      ADestination.Seek(0, TSeekOrigin.soBeginning);
      ADestination.CopyFrom(LDecompressor, 0);
    finally
      LDecompressor.Free;
    end;
  end;

begin
  if ARequestContext.Request.ContentEncoding = ENC_DEFLATE then
  begin
    if Assigned(ARequestContext.Request.ContentStream) then
    begin
      LMemStream := TMemoryStream.Create;
      try
        DoDecompress(ARequestContext.Request.ContentStream, LMemStream);
        LMemStream.Position := soFromBeginning;
        ARequestContext.Request.ContentStream := LMemStream;
      except
        LMemStream.Free;
      end;
      //ARequestContext.Request.ContentEncoding := ENC_IDENTITY;
    end
    else if ARequestContext.Request.Content <> '' then
    begin
      LStrStream := TStringStream.Create(ARequestContext.Request.Content);
      LStrStream.Position := soFromBeginning;
      try
        LMemStream := TMemoryStream.Create;
        try
          DoDecompress(LStrStream, LMemStream);
          LMemStream.Position := soFromBeginning;
          ARequestContext.Request.Content := '';
          ARequestContext.Request.ContentStream := LMemStream;
        except
          FreeAndNil(LMemStream);
        end;
      finally
        LStrStream.Free;
      end;
      //ARequestContext.Request.ContentEncoding := ENC_IDENTITY;
    end;
  end;
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
      LMemStream := TMemoryStream.Create;
      try
        DoCompress(AResponseContext.Response.ContentStream, LMemStream);
        LMemStream.Position := soFromBeginning;
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

{ TResponseEncodingFilterDebug }

procedure TResponseEncodingFilterDebug.Filter(AResponseContext: TWiRLContainerResponseContext);
var
  LStrStream: TStringStream;
  LMemStream: TFileStream;

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
      LMemStream := TFileStream.Create(TPath.Combine(TPath.GetDocumentsPath, 'sample.' + ENC_DEFLATE), fmCreate);
      try
        DoCompress(AResponseContext.Response.ContentStream, LMemStream);
        LMemStream.Position := soFromBeginning;
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
        LMemStream := TFileStream.Create(TPath.Combine(TPath.GetDocumentsPath, 'sample.' + ENC_DEFLATE), fmCreate);
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

initialization
  TWiRLFilterRegistry.Instance.RegisterFilter<TRequestDecodingFilter>;
  TWiRLFilterRegistry.Instance.RegisterFilter<TResponseEncodingFilter>;
  //TWiRLFilterRegistry.Instance.RegisterFilter<TResponseEncodingFilterDebug>;

end.
