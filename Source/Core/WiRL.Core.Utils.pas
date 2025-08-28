{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Utils;

{$I WiRL.inc}

interface

uses
  System.SysUtils, System.Classes, System.Rtti, System.SyncObjs, System.JSON,
  System.Generics.Collections,
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  {$IFDEF HAS_NET_ENCODING}
  System.NetEncoding,
  {$ELSE}
  IdCoder, IdCoderMIME, IdGlobal,
  {$ENDIF}
  WiRL.Core.JSON;

type
  TStringFunc = TFunc<string>;

  TBase64 = class
    class function Encode(const ASource: TStream): string; overload;
    class procedure Decode(const ASource: string; ADest: TStream); overload;
  end;

  /// <summary>
  ///   Class for a (very) simple Template Engine based on a template
  /// </summary>
  TWiRLTemplateEngine = class
  private
    FMacros: TDictionary<string, TStringFunc>;
  protected
    FStartChars: string;
    FEndChars: string;
    procedure InitMacros; virtual;
  public
    class function Render(const ATemplate: string): string;
  public
    constructor Create(AStartChars: string = '{'; AEndChars: string = '}');
    destructor Destroy; override;
    function RenderTemplate(const ATemplate: string): string;

    property Macros: TDictionary<string, TStringFunc> read FMacros write FMacros;
  end;

  TWiRLTemplatePaths = class(TWiRLTemplateEngine)
  protected
    procedure InitMacros; override;
  end;

  TWiRLTemplateHTML = class(TWiRLTemplateEngine)
  protected
    procedure InitMacros; override;
  public
    constructor Create(AStartChars: string = '{%'; AEndChars: string = '%}');
  end;

  TCustomAttributeClass = class of TCustomAttribute;

  function CreateCompactGuidStr: string;

  /// <summary>
  ///   Returns the file name without the extension
  /// </summary>
  function ExtractFileNameOnly(const AFileName: string): string;

  /// <summary>
  ///   Returns the directory up (1) level
  /// </summary>
  function DirectoryUp(const APath: string; ALevel: Integer = 1): string;

  function SmartConcat(const AArgs: array of string; const ADelimiter: string = ',';
    const AAvoidDuplicateDelimiter: Boolean = True; const ATrim: Boolean = True;
    const ACaseInsensitive: Boolean = True): string;

  function EnsurePrefix(const AString, APrefix: string; const AIgnoreCase: Boolean = True): string;
  function EnsureSuffix(const AString, ASuffix: string; const AIgnoreCase: Boolean = True): string;
  function StripPrefix(const APrefix, AString: string): string;
  function StripSuffix(const ASuffix, AString: string): string;

  function IsAbsoluteUrl(const APath: string): Boolean;

  function StringArrayToString(const AArray: TArray<string>): string;

  function StreamToJSONValue(const AStream: TStream; const AEncoding: TEncoding = nil): TJSONValue;
  function StreamToString(AStream: TStream): string;
  procedure CopyStream(ASourceStream, ADestStream: TStream;
    AOverWriteDest: Boolean = True; AThenResetDestPosition: Boolean = True);

  function IsMask(const AString: string): Boolean;
  function MatchesMask(const AString, AMask: string): Boolean;

  function ExistsInArray(AArray: TJSONArray; const AValue: string): Boolean;
  function IncludeLeadingSlash(const AValue: string): string;
  function IncludeTrailingSlash(const AValue: string): string;
  function ExcludeLeadingSlash(const AValue: string): string;
  function ExcludeTrailingSlash(const AValue: string): string;
  function CombineURL(const APathLeft, APathRight: string): string;

implementation

uses
  System.TypInfo, System.StrUtils, System.DateUtils, System.Masks, System.IOUtils;

function ExistsInArray(AArray: TJSONArray; const AValue: string): Boolean;
var
  LValue: TJSONValue;
begin
  for LValue in AArray do
  begin
    if LValue.Value.Equals(AValue) then
      Exit(True);
  end;
  Result := False;
end;

function IncludeLeadingSlash(const AValue: string): string;
begin
  if not AValue.StartsWith('/') then
    Result := '/' + AValue
  else
    Result := AValue;
end;

function IncludeTrailingSlash(const AValue: string): string;
begin
  if not AValue.EndsWith('/') then
    Result := AValue + '/'
  else
    Result := AValue;
end;

function ExcludeLeadingSlash(const AValue: string): string;
begin
  if AValue.StartsWith('/') then
    Result := AValue.Substring(1)
  else
    Result := AValue;
end;

function ExcludeTrailingSlash(const AValue: string): string;
begin
  if AValue.EndsWith('/') then
    Result := AValue.Substring(0, AValue.Length - 2)
  else
    Result := AValue;
end;

function CombineURL(const APathLeft, APathRight: string): string;
begin
  Result := IncludeTrailingSlash(APathLeft) + ExcludeLeadingSlash(APathRight);
end;

function StreamToString(AStream: TStream): string;
var
  LStream: TStringStream;
begin
  LStream := TStringStream.Create;
  try
    LStream.CopyFrom(AStream, 0);
    Result := LStream.DataString;
  finally
    LStream.Free;
  end;
end;

function IsMask(const AString: string): Boolean;
begin
  Result := ContainsStr(AString, '*') // wildcard
    or ContainsStr(AString, '?') // jolly
    or (ContainsStr(AString, '[') and ContainsStr(AString, ']')); // range
end;

function MatchesMask(const AString, AMask: string): Boolean;
begin
  Result := System.Masks.MatchesMask(AString, AMask);
end;

procedure CopyStream(ASourceStream, ADestStream: TStream;
  AOverWriteDest: Boolean = True; AThenResetDestPosition: Boolean = True);
begin
  if AOverWriteDest then
    ADestStream.Size := 0;
  ADestStream.CopyFrom(ASourceStream, 0);
  if AThenResetDestPosition then
    ADestStream.Position := 0;
end;

function StreamToJSONValue(const AStream: TStream; const AEncoding: TEncoding): TJSONValue;
var
  LStreamReader: TStreamReader;
  LEncoding: TEncoding;
begin
  LEncoding := AEncoding;
  if not Assigned(LEncoding) then
    LEncoding := TEncoding.Default;

  AStream.Position := 0;
  LStreamReader := TStreamReader.Create(AStream, LEncoding);
  try
    Result := TJSONObject.ParseJSONValue(LStreamReader.ReadToEnd);
  finally
    LStreamReader.Free;
  end;
end;

function StringArrayToString(const AArray: TArray<string>): string;
begin
  Result := SmartConcat(AArray);
end;

function IsAbsoluteUrl(const APath: string): Boolean;
const
  ValidProtocolChars = [
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
    'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
    'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
  ];
var
  LIndex: Integer;
begin
  Result := False;
  for LIndex := 1 to Length(APath) do
  begin
    if APath[LIndex] = ':' then
      Exit(True);
    if CharInSet(APath[LIndex], ValidProtocolChars) then

  end;
end;

function EnsurePrefix(const AString, APrefix: string; const AIgnoreCase: Boolean = True): string;
begin
  Result := AString;
  if Result <> '' then
  begin
    if (AIgnoreCase and not StartsText(APrefix, Result))
      or not StartsStr(APrefix, Result) then
      Result := APrefix + Result;
  end;
end;

function EnsureSuffix(const AString, ASuffix: string; const AIgnoreCase: Boolean = True): string;
begin
  Result := AString;
  if Result <> '' then
  begin
    if (AIgnoreCase and not EndsText(ASuffix, Result))
      or not EndsStr(ASuffix, Result) then
      Result := Result + ASuffix;
  end;
end;

function StripPrefix(const APrefix, AString: string): string;
begin
  Result := AString;
  if APrefix <> '' then
    while StartsStr(APrefix, Result) do
      Result := RightStr(Result, Length(Result) - Length(APrefix));
end;

function StripSuffix(const ASuffix, AString: string): string;
begin
  Result := AString;
  if ASuffix <> '' then
    while EndsStr(ASuffix, Result) do
      Result := LeftStr(Result, Length(Result) - Length(ASuffix));
end;

function SmartConcat(const AArgs: array of string; const ADelimiter: string = ',';
  const AAvoidDuplicateDelimiter: Boolean = True; const ATrim: Boolean = True;
  const ACaseInsensitive: Boolean = True): string;
var
  LIndex: Integer;
  LValue: string;
begin
  Result := '';
  for LIndex := 0 to Length(AArgs) - 1 do
  begin
    LValue := AArgs[LIndex];
    if ATrim then
      LValue := Trim(LValue);
    if AAvoidDuplicateDelimiter then
      LValue := StripPrefix(ADelimiter, StripSuffix(ADelimiter, LValue));

    if (Result <> '') and (LValue <> '') then
      Result := Result + ADelimiter;

    if IsAbsoluteUrl(LValue) then
      Result := LValue
    else
      Result := Result + LValue;
  end;
end;

function DateToString(ADate: TDateTime; const AZeroDateAsEmptyString: Boolean = True): string;
begin
  Result := DateToStr(ADate);
  if AZeroDateAsEmptyString and (ADate = 0) then
    Result := '';
end;

function DateTimeToString(ADate: TDateTime; const AZeroDateAsEmptyString: Boolean = True): string;
begin
  Result := DateTimeToStr(ADate);
  if AZeroDateAsEmptyString and (ADate = 0) then
    Result := '';
end;

function TimeToString(ADate: TDateTime; const AZeroDateAsEmptyString: Boolean = True): string;
begin
  Result := TimeToStr(ADate);
  if AZeroDateAsEmptyString and (ADate = 0) then
    Result := '';
end;

function CreateCompactGuidStr: string;
var
  LIndex: Integer;
  LBuffer: array[0..15] of Byte;
begin
  CreateGUID(TGUID(LBuffer));
  Result := '';
  for LIndex := 0 to 15 do
    Result := Result + IntToHex(LBuffer[LIndex], 2);
end;

function ExtractFileNameOnly(const AFileName: string): string;
begin
  Result := ExtractFileName(ChangeFileExt(AFileName, ''));
end;

function DirectoryUp(const APath: string; ALevel: Integer = 1): string;
var
  LIndexLevel: Integer;
begin
  if APath = '' then
    Exit;
  Result := APath;
  for LIndexLevel := 0 to ALevel - 1 do
    Result := ExtractFilePath(ExcludeTrailingPathDelimiter(Result));
end;

class function TBase64.Encode(const ASource: TStream): string;
{$IFDEF HAS_NET_ENCODING}
var
  LBase64Stream: TStringStream;
{$ENDIF}
begin
{$IFDEF HAS_NET_ENCODING}
  LBase64Stream := TStringStream.Create;
  try
    TNetEncoding.Base64.Encode(ASource, LBase64Stream);
    Result := LBase64Stream.DataString;
  finally
    LBase64Stream.Free;
  end;
{$ELSE}
  Result := TIdEncoderMIME.EncodeStream(ASource);
{$ENDIF}
end;

class procedure TBase64.Decode(const ASource: string; ADest: TStream);
{$IFDEF HAS_NET_ENCODING}
var
  LBase64Stream: TStringStream;
{$ENDIF}
begin
{$IFDEF HAS_NET_ENCODING}
  LBase64Stream := TStringStream.Create(ASource);
  LBase64Stream.Position := soFromBeginning;
  try
    TNetEncoding.Base64.Decode(LBase64Stream, ADest);
  finally
    LBase64Stream.Free;
  end;
{$ELSE}
  TIdDecoderMIME.DecodeStream(ASource, ADest);
{$ENDIF}
end;

{ TWiRLTemplateEngine }

constructor TWiRLTemplateEngine.Create(AStartChars: string = '{'; AEndChars: string = '}');
begin
  FStartChars := AStartChars;
  FEndChars := AEndChars;
  FMacros := TDictionary<string, TStringFunc>.Create;

  InitMacros;
end;

destructor TWiRLTemplateEngine.Destroy;
begin
  FMacros.Free;
  inherited;
end;

function TWiRLTemplateEngine.RenderTemplate(const ATemplate: string): string;
var
  LMacroPair: TPair<string,TFunc<string>>;
begin
  Result := ATemplate;

  for LMacroPair in FMacros do
    Result := Result.Replace(FStartChars + LMacroPair.Key + FEndChars,
      ExcludeTrailingPathDelimiter(LMacroPair.Value()), [rfIgnoreCase]);
end;

procedure TWiRLTemplateEngine.InitMacros;
begin

end;

class function TWiRLTemplateEngine.Render(const ATemplate: string): string;
var
  LMacroEngine: TWiRLTemplateEngine;
begin
  LMacroEngine := Self.Create();
  try
    Result := LMacroEngine.RenderTemplate(ATemplate);
  finally
    LMacroEngine.Free;
  end;
end;

{ TWiRLTemplatePaths }

procedure TWiRLTemplatePaths.InitMacros;
begin
  inherited;

  Macros.Add('AppPath', function(): string
    begin
      Result := ExtractFilePath(ParamStr(0));
    end
  );

  Macros.Add('CurrPath', function(): string
    begin
      Result := TDirectory.GetCurrentDirectory;
    end
  );

  Macros.Add('TempPath', function(): string
    begin
      Result := TPath.GetTempPath;
    end
  );

  Macros.Add('HomePath', function(): string
    begin
      Result := TPath.GetHomePath;
    end
  );

  Macros.Add('DocumentsPath', function(): string
    begin
      Result := TPath.GetDocumentsPath;
    end
  );

  Macros.Add('PublicPath', function(): string
    begin
      Result := TPath.GetPublicPath;
    end
  );
end;

{ TWiRLTemplateHTML }

constructor TWiRLTemplateHTML.Create(AStartChars, AEndChars: string);
begin
  inherited Create(AStartChars, AEndChars);
end;

procedure TWiRLTemplateHTML.InitMacros;
begin
  inherited;

  Macros.Add('AppPath', function(): string
    begin
      Result := ExtractFilePath(ParamStr(0));
    end
  );

end;

end.
