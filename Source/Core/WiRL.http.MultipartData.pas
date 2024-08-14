{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2023 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.MultipartData;

interface

uses
  System.SysUtils, System.Classes, System.NetEncoding,
  System.Generics.Collections, System.Generics.Defaults,

  WiRL.Core.Classes,
  WiRL.Core.Attributes,
  WiRL.http.Core,
  WiRL.http.Headers,
  WiRL.http.Accept.MediaType,
  WiRL.http.Accept.Parser;

type
  TWiRLContentDisposition = class(THeaderItem)
  private
    FModificationDate: TDateTime;
    FFileName: string;
    FName: string;
    FSize: Int64;
    FReadDate: TDateTime;
    FCreationDate: TDateTime;
    FVoice: string;
    FHandling: string;
    FPreviewType: string;
  protected
    const FILENAME_NAME = 'filename';
    const CREATIONDATE_NAME = 'creation-date';
    const MODIFICATIONDATE_NAME = 'modification-date';
    const READDATE_NAME = 'read-date';
    const SIZE_NAME = 'size';
    const NAME_NAME = 'name';
    const VOICE_NAME = 'voice';
    const HANDLING_NAME = 'handling';
    const PREVIEWTYPE_NAME = 'preview-type';
  public
    procedure Parse(const AHeaderItem: string); override;

    property Name: string read FName write FName;
    property FileName: string read FFileName write FFileName;
    property Size: Int64 read FSize write FSize;
    property ReadDate: TDateTime read FReadDate write FReadDate;
    property CreationDate: TDateTime read FCreationDate write FCreationDate;
    property ModificationDate: TDateTime read FModificationDate write FModificationDate;
    property DispositionType: string read FValue write FValue;
    property Voice: string read FVoice write FVoice;
    property Handling: string read FHandling write FHandling;
    property PreviewType: string read FPreviewType write FPreviewType;
  end;

  [Singleton]
  TWiRLFormDataPart = class
  private
    FContentTransferEncoding: string;
    FContentMediaType: TMediaType;
    FContentStream: TStream;
    FContentDisposition: TWiRLContentDisposition;
    FHeaders: IWiRLHeaders;
    FContentType: string;
    function GetContent: string;
    function GetContentStream: TStream;
    function GetRawContent: TBytes;
    function GetCharset: string;
    function GetFileName: string;
    function GetName: string;
    function GetContentMediaType: TMediaType;
  public
    constructor Create;
    destructor Destroy; override;

    property Name: string read GetName;
    property FileName: string read GetFileName;
    property ContentTransferEncoding: string read FContentTransferEncoding write FContentTransferEncoding;
    property ContentMediaType: TMediaType read GetContentMediaType;
    property ContentType: string read FContentType write FContentType;
    property Charset: string read GetCharset;
    property Headers: IWiRLHeaders read FHeaders;

    property Content: string read GetContent;
    property RawContent: TBytes read GetRawContent;
    property ContentStream: TStream read GetContentStream;
    property ContentDisposition: TWiRLContentDisposition read FContentDisposition write FContentDisposition;
  end;

  TWiRLFormDataMultiPart = class
  private
    FMIMEBoundary: string;
    FFormDataList: TObjectList<TWiRLFormDataPart>;
    FEpilogue: string;
    FPreamble: string;
    function GetPart(const AName: string): TWiRLFormDataPart; overload;
    procedure Parse(AStream: TStream);
    function AddPart(AHeaders: IWiRLHeaders; const ABody: string): TWiRLFormDataPart;
    function GetCount: Integer;
  public
    constructor Create(AStream: TStream; const AMIMEBoundary: string);
    destructor Destroy; override;
    function FindPart(const AName: string): TWiRLFormDataPart;
    function GetPart(AIndex: Integer): TWiRLFormDataPart; overload;
    property Parts[const AName: string]: TWiRLFormDataPart read GetPart; default;
    property Count: Integer read GetCount;
    property Preamble: string read FPreamble;
    property Epilogue: string read FEpilogue;
  end;

implementation

uses
  IdGlobalProtocols,
  WiRL.RTTI.Utils,
  WiRL.Core.Exceptions;

type
  TMimeStreamReader = class(TStreamReader)
  private
    FLastLineBreak: string;
  public
    function ReadLine: string; override;
    property LastLineBreak: string read FLastLineBreak;
  end;

  // Dirty hack to access some private member in Delphi Tokyo or less
  TStreamReaderHelper = class helper for TStreamReader
    function GetBufferedData: TStringBuilder;
    function GetNoDataInStream: Boolean;
    procedure DoFillBuffer(var Encoding: TEncoding);
  end;

{ TWiRLContentDisposition }

procedure TWiRLContentDisposition.Parse(const AHeaderItem: string);
var
  LPosition: Integer;
begin
  inherited Parse(AHeaderItem);

  for LPosition := 0 to Parameters.Count - 1 do
  begin
    if FParameters.Names[LPosition] = FILENAME_NAME then
      FFileName := AnsiDequotedStr(FParameters.ValueFromIndex[LPosition], '"')
    else if FParameters.Names[LPosition] = CREATIONDATE_NAME then
      FCreationDate := CookieStrToLocalDateTime(FParameters.ValueFromIndex[LPosition])
    else if FParameters.Names[LPosition] = MODIFICATIONDATE_NAME then
      FModificationDate := CookieStrToLocalDateTime(FParameters.ValueFromIndex[LPosition])
    else if FParameters.Names[LPosition] = READDATE_NAME then
      FReadDate := CookieStrToLocalDateTime(FParameters.ValueFromIndex[LPosition])
    else if FParameters.Names[LPosition] = SIZE_NAME then
      FSize := StrToInt64(FParameters.ValueFromIndex[LPosition])
    else if FParameters.Names[LPosition] = NAME_NAME then
      FName := AnsiDequotedStr(FParameters.ValueFromIndex[LPosition], '"')
    else if FParameters.Names[LPosition] = VOICE_NAME then
      FVoice := FParameters.ValueFromIndex[LPosition]
    else if FParameters.Names[LPosition] = HANDLING_NAME then
      FHandling := FParameters.ValueFromIndex[LPosition]
    else if FParameters.Names[LPosition] = PREVIEWTYPE_NAME then
      FPreviewType := FParameters.ValueFromIndex[LPosition];
  end;
end;

{ TWiRLFormDataPart }

constructor TWiRLFormDataPart.Create;
begin
  inherited;
  FContentStream := TMemoryStream.Create;
  FContentDisposition := TWiRLContentDisposition.Create('');
  FHeaders := TWiRLHeaders.Create;
end;

destructor TWiRLFormDataPart.Destroy;
begin
  FContentDisposition.Free;
  FContentMediaType.Free;
  FContentStream.Free;
  inherited;
end;

function TWiRLFormDataPart.GetCharset: string;
begin
  Result := FContentMediaType.Charset;
end;

function TWiRLFormDataPart.GetContent: string;
begin
  Result := EncodingFromCharSet(Charset).GetString(RawContent);
end;

function TWiRLFormDataPart.GetContentMediaType: TMediaType;
begin
  if not Assigned(FContentMediaType) then
    FContentMediaType := TMediaType.Create(ContentType);
  Result := FContentMediaType;
end;

function TWiRLFormDataPart.GetContentStream: TStream;
begin
  Result := FContentStream;
end;

function TWiRLFormDataPart.GetFileName: string;
begin
  Result := FContentDisposition.FileName;
end;

function TWiRLFormDataPart.GetName: string;
begin
  Result := FContentDisposition.Name;
end;

function TWiRLFormDataPart.GetRawContent: TBytes;
var
  LPos :Int64;
begin
  if (GetContentStream <> nil) and (GetContentStream.Size > 0) then
  begin
    LPos := GetContentStream.Position;
    try
      GetContentStream.Position := 0;
      SetLength(Result, GetContentStream.Size);
      GetContentStream.ReadBuffer(Result[0], GetContentStream.Size);
    finally
      GetContentStream.Position := LPos;
    end;
  end;
end;

{ TWiRLFormDataMultiPart }

function TWiRLFormDataMultiPart.AddPart(AHeaders: IWiRLHeaders; const ABody: string): TWiRLFormDataPart;
var
  LRawBody: TBytes;
begin
  Result := TWiRLFormDataPart.Create;
  try
    Result.FHeaders.Assign(AHeaders);
    Result.FContentDisposition.Parse(AHeaders.Values['Content-Disposition']);
    Result.FContentTransferEncoding := AHeaders.Values['Content-Transfer-Encoding'];
    Result.FContentType := AHeaders.ContentType;

    if Result.ContentMediaType.MediaType = TMediaType.APPLICATION_OCTET_STREAM then
    begin
      if (Result.FContentTransferEncoding = '') or (CompareText(Result.FContentTransferEncoding, 'BINARY') = 0) then
      begin
        LRawBody := TEncoding.ANSI.GetBytes(ABody);
      end
      else if CompareText(Result.FContentTransferEncoding, 'BASE64') = 0 then
      begin
        LRawBody := TNetEncoding.Base64.DecodeStringToBytes(ABody);
      end
      else
        raise EWiRLException.CreateFmt('[%s] Encoding not supported', [Result.FContentTransferEncoding]);
    end
    else
    begin
      LRawBody := TEncoding.ANSI.GetBytes(ABody);
    end;
    Result.FContentStream.WriteData(LRawBody, Length(LRawBody));
    Result.FContentStream.Position := 0;
    FFormDataList.Add(Result);
  except
    Result.Free;
    raise;
  end;
end;

constructor TWiRLFormDataMultiPart.Create(AStream: TStream; const AMIMEBoundary: string);
begin
  inherited Create;

  FFormDataList := TObjectList<TWiRLFormDataPart>.Create(True);
  FMIMEBoundary := AMIMEBoundary;
  Parse(AStream);
end;

destructor TWiRLFormDataMultiPart.Destroy;
begin
  FFormDataList.Free;
  inherited;
end;

function TWiRLFormDataMultiPart.FindPart(const AName: string): TWiRLFormDataPart;
var
  LPart: TWiRLFormDataPart;
begin
  Result := nil;
  for LPart in FFormDataList do
  begin
    if LPart.Name = AName then
      Exit(LPart);
  end;
end;

function TWiRLFormDataMultiPart.GetCount: Integer;
begin
  Result := FFormDataList.Count;
end;

function TWiRLFormDataMultiPart.GetPart(AIndex: Integer): TWiRLFormDataPart;
begin
  Result := FFormDataList[AIndex];
end;

function TWiRLFormDataMultiPart.GetPart(const AName: string): TWiRLFormDataPart;
begin
  Result := FindPart(AName);
  if not Assigned(Result) then
    raise EWiRLException.CreateFmt('Part named [%s] not found', [AName]);
end;

procedure TWiRLFormDataMultiPart.Parse(AStream: TStream);
type
  TPartSearchStatus = (ssPreamble, ssHeader, ssBody, ssEpilogue);
var
  LStreamReader: TMimeStreamReader;
  LLine, LBody: string;
  LHeaders: IWiRLHeaders;
  LLastLineBreak: string;
  LSearchStatus: TPartSearchStatus;

  procedure AddHeader(const HeaderLine: string);
  var
    LSepIndex: Integer;
    LName, LValue: string;
  begin
    LSepIndex := Pos(':', HeaderLine);
    if LSepIndex > 0 then
    begin
      LName := Trim(Copy(HeaderLine, 1, LSepIndex - 1));
      LValue := Trim(Copy(HeaderLine, LSepIndex + 1, Length(HeaderLine) - LSepIndex));
      LHeaders.Values[LName] := LValue;
    end;
  end;

begin
  LSearchStatus := ssPreamble;
  LBody := '';
  FPreamble := '';
  FEpilogue := '';
  LLastLineBreak := '';
  LStreamReader := TMimeStreamReader.Create(AStream, TEncoding.ANSI);
  try
    LHeaders := TWiRLHeaders.Create;
    while not LStreamReader.EndOfStream do
    begin
      LLine := LStreamReader.ReadLine;
      // Boundary found
      if CompareStr(LLine, '--' + FMIMEBoundary) = 0 then
      begin
        if LSearchStatus = ssBody then
        begin
          SetLength(LBody, Length(LBody) - Length(LLastLineBreak));
          AddPart(LHeaders, LBody);
        end
        else if LSearchStatus = ssPreamble then
        begin
          SetLength(LBody, Length(LBody) - Length(LLastLineBreak));
          FPreamble := LBody;
        end;
        LBody := '';
        LHeaders.Clear;
        LSearchStatus := ssHeader;
      end
      // Last boundary found
      else if CompareStr(LLine, '--' + FMIMEBoundary + '--') = 0 then
      begin
        SetLength(LBody, Length(LBody) - Length(LLastLineBreak));
        AddPart(LHeaders, LBody);
        LSearchStatus := ssEpilogue;
        LBody := '';
      end
      else if LSearchStatus = ssHeader then
      begin
        if LLine <> '' then
        begin
          AddHeader(LLine);
        end
        else
        begin
          LSearchStatus := ssBody;
        end;
      end
      else //if SearchStatus = ssBody then
      begin
        LBody := LBody + LLine + LStreamReader.LastLineBreak;
        LLastLineBreak := LStreamReader.LastLineBreak;
      end;
    end;

    if LSearchStatus = ssEpilogue then
    begin
      FEpilogue := LBody;
    end;
  finally
    LStreamReader.Free;
  end;
end;

{ TMimeStreamReader }

function TMimeStreamReader.ReadLine: string;
var
  NewLineIndex: Integer;
  PostNewLineIndex: Integer;
  LChar: Char;
  LOriginalEncoding: TEncoding;
begin
  LOriginalEncoding := CurrentEncoding;
  FLastLineBreak := '';
  Result := '';
  if GetBufferedData = nil then
    Exit;
  NewLineIndex := 0;
  PostNewLineIndex := 0;

  while True do
  begin
    if (NewLineIndex + 2 > GetBufferedData.Length) and (not GetNoDataInStream) then
      DoFillBuffer(LOriginalEncoding);

    if NewLineIndex >= GetBufferedData.Length then
    begin
      if GetNoDataInStream then
      begin
        PostNewLineIndex := NewLineIndex;
        Break;
      end
      else
      begin
        DoFillBuffer(LOriginalEncoding);
        if GetBufferedData.Length = 0 then
          Break;
      end;
    end;
    LChar := GetBufferedData[NewLineIndex];
    if LChar = #10 then
    begin
      PostNewLineIndex := NewLineIndex + 1;
      FLastLineBreak := #10;
      Break;
    end
    else
    if (LChar = #13) and (NewLineIndex + 1 < GetBufferedData.Length) and (GetBufferedData[NewLineIndex + 1] = #10) then
    begin
      PostNewLineIndex := NewLineIndex + 2;
      FLastLineBreak := #13 + #10;
      Break;
    end
    else
    if LChar = #13 then
    begin
      PostNewLineIndex := NewLineIndex + 1;
      FLastLineBreak := #13;
      Break;
    end;

    Inc(NewLineIndex);
  end;

  Result := GetBufferedData.ToString;
  SetLength(Result, NewLineIndex);
  GetBufferedData.Remove(0, PostNewLineIndex);
end;

{ TStreamReaderHelper }

procedure TStreamReaderHelper.DoFillBuffer(var Encoding: TEncoding);
begin
  with Self do
    FillBuffer(Encoding);
end;

function TStreamReaderHelper.GetBufferedData: TStringBuilder;
begin
  with Self do
    Result := FBufferedData;
end;

function TStreamReaderHelper.GetNoDataInStream: Boolean;
begin
  with Self do
    Result := FNoDataInStream;
end;

end.
