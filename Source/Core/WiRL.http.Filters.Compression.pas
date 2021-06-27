{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.Filters.Compression;

interface

uses
  System.SysUtils, System.Classes,

  WiRL.Core.Classes,
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

  [NameBinding]
  CompressionAttribute = class(TCustomAttribute);

  /// <summary>
  ///   Base class for the encoding/decoding filters, only to introduce Encoding header
  ///   values
  /// </summary>
  TCompressionFilter = class(TInterfacedObject)
  protected
    const ENC_BROTLI = 'br';
    const ENC_GZIP = 'gzip';
    const ENC_DEFLATE = 'deflate';
    const ENC_IDENTITY = 'identity';
  end;

  /// <summary>
  ///   HTTP Request filter. If the request has the "gzip" or "deflate"
  ///   ContentEncoding then the filter decompress the body.
  /// </summary>
  /// <remarks>
  ///   Brotli ContentEncoding (br) is not currently supported
  /// </remarks>
  [ContentDecoding, Compression]
  TRequestDecodingFilter = class(TCompressionFilter, IWiRLContainerRequestFilter)
  public
    procedure Filter(ARequestContext: TWiRLContainerRequestContext);
  end;

  /// <summary>
  ///   HTTP Response filter. If the request has the "gzip" or "deflate" encoding
  ///   in the Accept-Encoding header this filter compresses the body stream
  /// </summary>
  /// <remarks>
  ///   Brotli ContentEncoding (br) is not currently supported
  /// </remarks>
  [ContentEncoding, Compression]
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
  [ContentEncoding, Compression]
  TResponseEncodingFilterDebug = class(TCompressionFilter, IWiRLContainerResponseFilter)
  public
    procedure Filter(AResponseContext: TWiRLContainerResponseContext);
  end;

implementation

uses
  WiRL.Configuration.Compression,
  WiRL.Core.Declarations,
  System.ZLib, System.IOUtils;

{ TRequestDecodingFilter }

procedure TRequestDecodingFilter.Filter(ARequestContext: TWiRLContainerRequestContext);
var
  LStrStream: TStringStream;
  LMemStream: TMemoryStream;

  procedure DoDecompress(ASource, ADestination: TStream; const AContentEncoding: string);
  var
    LWindowBits: Integer;
    LDecompressor: TZDecompressionStream;
  begin
    LWindowBits := 15;
    if AContentEncoding = ENC_GZIP then
	  Inc(LWindowBits, 16); //REF: https://stackoverflow.com/a/52815667/8018798

    ASource.Seek(0, TSeekOrigin.soBeginning);

    LDecompressor := TZDecompressionStream.Create(ASource, LWindowBits);
    try
      ADestination.Seek(0, TSeekOrigin.soBeginning);
      ADestination.CopyFrom(LDecompressor, 0);
    finally
      LDecompressor.Free;
    end;
  end;

begin
  if (ARequestContext.Request.ContentEncoding = ENC_GZIP)  or
     (ARequestContext.Request.ContentEncoding = ENC_DEFLATE) then
  begin
    if Assigned(ARequestContext.Request.ContentStream) then
    begin
      LMemStream := TMemoryStream.Create;
      try
        DoDecompress(ARequestContext.Request.ContentStream, LMemStream, ARequestContext.Request.ContentEncoding);
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
          DoDecompress(LStrStream, LMemStream, ARequestContext.Request.ContentEncoding);
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
  LConf: TWiRLConfigurationCompression;
  LStrStream: TStringStream;
  LMemStream: TMemoryStream;
  LContentEncoding: string;
  LContentLength: Integer;

  procedure DoCompress(ASource, ADestination: TStream; const AContentEncoding: string);
  var
    LWindowBits: Integer;
    LCompressor: TZCompressionStream;
  begin
    LWindowBits := 15;
    if AContentEncoding = ENC_GZIP then
	  Inc(LWindowBits, 16); //REF: https://stackoverflow.com/a/52815667/8018798

    ASource.Seek(0, TSeekOrigin.soBeginning);
    LCompressor := TZCompressionStream.Create(ADestination, zcDefault, LWindowBits);
    try
      LCompressor.CopyFrom(ASource, ASource.Size);
    finally
      LCompressor.Free;
    end;
  end;

begin
  LConf := (AResponseContext.Context.Application as TWiRLApplication).GetConfiguration<TWiRLConfigurationCompression>;

  if Assigned(AResponseContext.Response.ContentStream) then
    LContentLength := AResponseContext.Response.ContentStream.Size
  else
    LContentLength := Length(AResponseContext.Response.Content);

  if LContentLength <= LConf.MinimumSize then
    Exit;

  if not TWiRLStringArray.New(LConf.MediaTypes).Contains(AResponseContext.Response.ContentType) then
    Exit;

  if AResponseContext.Request.AcceptableEncodings.Contains(ENC_GZIP) then
    LContentEncoding := ENC_GZIP
  else if AResponseContext.Request.AcceptableEncodings.Contains(ENC_DEFLATE) then
    LContentEncoding := ENC_DEFLATE
  else
    LContentEncoding := ENC_IDENTITY;

  if LContentEncoding <> ENC_IDENTITY then
  begin
    if Assigned(AResponseContext.Response.ContentStream) then
    begin
      LMemStream := TMemoryStream.Create;
      try
        DoCompress(AResponseContext.Response.ContentStream, LMemStream, LContentEncoding);
        LMemStream.Position := soFromBeginning;
        AResponseContext.Response.ContentStream := LMemStream;
      except
        LMemStream.Free;
      end;
      AResponseContext.Response.ContentEncoding := LContentEncoding;
    end
    else if AResponseContext.Response.Content <> '' then
    begin
      LStrStream := TStringStream.Create(AResponseContext.Response.Content);
      LStrStream.Position := soFromBeginning;
      try
        LMemStream := TMemoryStream.Create;
        try
          DoCompress(LStrStream, LMemStream, LContentEncoding);
          LMemStream.Position := soFromBeginning;
          AResponseContext.Response.Content := '';
          AResponseContext.Response.ContentStream := LMemStream;
        except
          FreeAndNil(LMemStream);
        end;
      finally
        LStrStream.Free;
      end;
      AResponseContext.Response.ContentEncoding := LContentEncoding;
    end;
  end;
end;

{ TResponseEncodingFilterDebug }

procedure TResponseEncodingFilterDebug.Filter(AResponseContext: TWiRLContainerResponseContext);
var
  LStrStream: TStringStream;
  LMemStream: TFileStream;
  LContentEncoding: string;

  procedure DoCompress(ASource, ADestination: TStream;
    const AContentEncoding: string);
  var
    LWindowBits: Integer;
    LCompressor: TZCompressionStream;
  begin
    LWindowBits := 15;
    if AContentEncoding = ENC_GZIP then 
	  Inc(LWindowBits, 16); //REF: https://stackoverflow.com/a/52815667/8018798

    ASource.Seek(0, TSeekOrigin.soBeginning);
    LCompressor := TZCompressionStream.Create(ADestination, zcDefault, LWindowBits);
    try
      LCompressor.CopyFrom(ASource, ASource.Size);
    finally
      LCompressor.Free;
    end;
  end;

begin
  if AResponseContext.Request.AcceptableEncodings.Contains(ENC_GZIP) then 
    LContentEncoding := ENC_GZIP
  else if AResponseContext.Request.AcceptableEncodings.Contains(ENC_DEFLATE) then
	  LContentEncoding := ENC_DEFLATE
  else
	  LContentEncoding := ENC_IDENTITY;

  if LContentEncoding <> ENC_IDENTITY then
  begin
    if Assigned(AResponseContext.Response.ContentStream) then
    begin
      LMemStream := TFileStream.Create(TPath.Combine(TPath.GetDocumentsPath, 'sample.' + LContentEncoding), fmCreate);
      try
        DoCompress(AResponseContext.Response.ContentStream, LMemStream, LContentEncoding);
        LMemStream.Position := soFromBeginning;
        AResponseContext.Response.ContentStream := LMemStream;
      except
        LMemStream.Free;
      end;
      AResponseContext.Response.ContentEncoding := LContentEncoding;
    end
    else if AResponseContext.Response.Content <> '' then
    begin
      LStrStream := TStringStream.Create(AResponseContext.Response.Content);
      LStrStream.Position := soFromBeginning;
      try
        LMemStream := TFileStream.Create(TPath.Combine(TPath.GetDocumentsPath, 'sample.' + LContentEncoding), fmCreate);
        try
          DoCompress(LStrStream, LMemStream, LContentEncoding);
          LMemStream.Position := soFromBeginning;
          AResponseContext.Response.Content := '';
          AResponseContext.Response.ContentStream := LMemStream;
        except
          FreeAndNil(LMemStream);
        end;
      finally
        LStrStream.Free;
      end;
      AResponseContext.Response.ContentEncoding := LContentEncoding;
    end;
  end;
end;

end.
