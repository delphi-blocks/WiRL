{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.http.MultipartData;

interface

uses
  System.SysUtils, System.Classes,
  System.Generics.Collections, System.Generics.Defaults,

  WiRL.http.Core,
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
    FValue: string;
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

  TWiRLFormDataPart = class
  private
    FContentTransfer: string;
    FHeaderEncoding: Char;
    FCharset: string;
    FContentType: string;
    FContentStream: TStream;
    FHeaderCharSet: string;
    FContentDisposition: TWiRLContentDisposition;
    FName: string;
    FFileName: string;
    FHeaders: TStrings;
    function GetContent: string;
    function GetContentStream: TStream;
    function GetRawContent: TBytes;
  public
    constructor Create;
    destructor Destroy; override;

    property Name: string read FName write FName;
    property FileName: string read FFileName write FFileName;
    property ContentTransfer: string read FContentTransfer write FContentTransfer;
    property ContentType: string read FContentType write FContentType;
    property Charset: string read FCharset write FCharset;
    property HeaderCharSet: string read FHeaderCharSet write FHeaderCharSet;
    property HeaderEncoding: Char read FHeaderEncoding write FHeaderEncoding;
    property Headers: TStrings read FHeaders;

    property Content: string read GetContent;
    property RawContent: TBytes read GetRawContent;
    property ContentStream: TStream read GetContentStream;
    property ContentDisposition: TWiRLContentDisposition read FContentDisposition write FContentDisposition;
  end;

  TWiRLFormDataMultiPart = class
  private
    FMIMEBoundary: string;
    FFormDataList: TObjectList<TWiRLFormDataPart>;
    function GetMIMEBoundary(const AContentType: string): string;
    function GetPart(const AName: string): TWiRLFormDataPart;
    procedure Parse(AStream: TStream; HeaderFields: TWiRLHeaderList);
    function AddPart(const FileName, Name: string; Headers: TStrings): TWiRLFormDataPart;
  public
    constructor Create(AStream: TStream; HeaderFields: TWiRLHeaderList);
    destructor Destroy; override;
    function FindPart(const AName: string): TWiRLFormDataPart;
    property Parts[const AName: string]: TWiRLFormDataPart read GetPart; default;
  end;

implementation

uses
  IdBaseComponent,
  IdMessageCoder,
  IdMessage,
  IdGlobal,
  IdGlobalProtocols,
  IdTCPStream,
  IdBuffer,
  IdMessageCoderMIME,

  WiRL.Core.Exceptions;

{ TWiRLContentDisposition }

procedure TWiRLContentDisposition.Parse(const AHeaderItem: string);
var
  LPosition: Integer;
begin
  inherited Parse(AHeaderItem);

  for LPosition := 0 to Parameters.Count - 1 do
  begin
    if FParameters.Names[LPosition] = FILENAME_NAME then
      FFileName := UnquotedStr(FParameters.ValueFromIndex[LPosition])
    else if FParameters.Names[LPosition] = CREATIONDATE_NAME then
      FCreationDate := CookieStrToLocalDateTime(FParameters.ValueFromIndex[LPosition])
    else if FParameters.Names[LPosition] = MODIFICATIONDATE_NAME then
      FModificationDate := CookieStrToLocalDateTime(FParameters.ValueFromIndex[LPosition])
    else if FParameters.Names[LPosition] = READDATE_NAME then
      FReadDate := CookieStrToLocalDateTime(FParameters.ValueFromIndex[LPosition])
    else if FParameters.Names[LPosition] = SIZE_NAME then
      FSize := StrToInt64(FParameters.ValueFromIndex[LPosition])
    else if FParameters.Names[LPosition] = NAME_NAME then
      FName := UnquotedStr(FParameters.ValueFromIndex[LPosition])
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
  FHeaders := TStringList.Create;
end;

destructor TWiRLFormDataPart.Destroy;
begin
  FContentDisposition.Free;
  FContentStream.Free;
  FHeaders.Free;
  inherited;
end;

function TWiRLFormDataPart.GetContent: string;
begin
  Result := EncodingFromCharSet(Charset).GetString(RawContent);
end;

function TWiRLFormDataPart.GetContentStream: TStream;
begin
  Result := FContentStream;
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

function TWiRLFormDataMultiPart.AddPart(const FileName, Name: string; Headers: TStrings): TWiRLFormDataPart;
begin
  Result := TWiRLFormDataPart.Create;
  Result.FFileName := FileName;
  Result.FName := Name;
  Result.FHeaders.Assign(Headers);
  Result.FContentDisposition.Parse(Headers.Values['Content-Disposition']);
  Result.FContentType := Headers.Values['Content-Type'];
  FFormDataList.Add(Result);
end;

constructor TWiRLFormDataMultiPart.Create(AStream: TStream; HeaderFields: TWiRLHeaderList);
begin
  inherited Create;
  FFormDataList := TObjectList<TWiRLFormDataPart>.Create(True);
  FMIMEBoundary := GetMIMEBoundary(HeaderFields['content-type']);
  Parse(AStream, HeaderFields);
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

function TWiRLFormDataMultiPart.GetMIMEBoundary(
  const AContentType: string): string;
var
  LContentTypeParser: TContentTypeItem;
begin
  LContentTypeParser := TContentTypeItem.Create(AContentType);
  try
    Result := LContentTypeParser.Boundary;
  finally
    LContentTypeParser.Free;
  end;
end;

function TWiRLFormDataMultiPart.GetPart(const AName: string): TWiRLFormDataPart;
begin
  Result := FindPart(AName);
  if not Assigned(Result) then
    raise EWiRLException.CreateFmt('Part named [%s] not found', [AName]);
end;

procedure TWiRLFormDataMultiPart.Parse(AStream: TStream; HeaderFields: TWiRLHeaderList);
var
  Line: string;
  Decoder: TIdMessageDecoder;
  MsgEnd: Boolean;
  NewPart :TWiRLFormDataPart;
  PartName :string;
  ContentType :string;
  ContentDisposition :string;

  procedure ProcessTextPart(var VDecoder: TIdMessageDecoder; APart :TWiRLFormDataPart);
  var
    MStream: TMemoryStream;
    NewDecoder: TIdMessageDecoder;
  begin
    NewDecoder := nil;
    MStream := TMemoryStream.Create;
    try
      NewDecoder := VDecoder.ReadBody(MStream, MsgEnd);
      try
        MStream.Position := 0;
        MStream.SaveToStream(APart.ContentStream);
      except
        FreeAndNil(NewDecoder);
        raise;
      end;
    finally
      MStream.Free;
    end;
    VDecoder.Free;
    VDecoder := NewDecoder;
  end;

  procedure ProcessAttachment(var VDecoder: TIdMessageDecoder; APart :TWiRLFormDataPart);
  var
    DestStream: TMemoryStream;
    NewDecoder: TIdMessageDecoder;
  begin
    NewDecoder := nil;
    DestStream := TMemoryStream.Create;
    try
      NewDecoder := VDecoder.ReadBody(DestStream, MsgEnd);
      try
        DestStream.SaveToStream(APart.ContentStream);
      except
        FreeAndNil(NewDecoder);
        raise;
      end;
    finally
      DestStream.Free;
    end;
    VDecoder.Free;
    VDecoder := NewDecoder;
  end;

begin
  Decoder := nil;
  try
    repeat
      Line := ReadLnFromStream(AStream, -1, True);
      if TextIsSame(Line, '--' + FMIMEBoundary) then
      begin
        Decoder := TIdMessageDecoderMIME.Create(nil);
        TIdMessageDecoderMIME(Decoder).MIMEBoundary := FMIMEBoundary;
        Decoder.SourceStream := AStream;
        Decoder.FreeSourceStream := False;
        Break;
      end;
      if TextIsSame(Line, '--' + FMIMEBoundary + '--') then
      begin
        Break;
      end;
    until False;

    if Decoder <> nil then begin
      MsgEnd := False;
      repeat
        Decoder.ReadHeader;
        ContentDisposition := Decoder.Headers.Values['Content-Disposition'];
        ContentType := Decoder.Headers.Values['Content-Type'];
        PartName := ExtractHeaderSubItem(ContentDisposition, 'name'{$IFNDEF VER210}, QuoteMIME{$ENDIF});
        NewPart := AddPart(Decoder.Filename, PartName, Decoder.Headers);

        case Decoder.PartType of
          mcptText:       ProcessTextPart(Decoder, NewPart);
          mcptAttachment: ProcessAttachment(Decoder, NewPart);
          mcptIgnore:     begin
                            FreeAndNil(Decoder);
                            Decoder := TIdMessageDecoderMIME.Create(nil);
                            TIdMessageDecoderMIME(Decoder).MIMEBoundary := FMIMEBoundary;
                            Decoder.SourceStream := AStream;
                            Decoder.FreeSourceStream := False;
                          end;
//          mcptEOF:        begin
//                            FreeAndNil(Decoder);
//                            MsgEnd := True;
//                          end;
        end;
        if NewPart.ContentStream.Size > 0 then
        begin
          NewPart.ContentStream.Position := 0;
        end;
        (*********)
        if Assigned(Decoder) then
        begin
          TIdMessageDecoderMIME(Decoder).MIMEBoundary := FMIMEBoundary;
          Decoder.SourceStream := AStream;
          Decoder.FreeSourceStream := False;
        end;
        (*********)
      until (Decoder = nil) or MsgEnd;
    end;
  finally
    Decoder.Free;
  end;
end;

end.
