{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.MessageBodyReader;

{$I WiRL.inc}

interface

uses
  System.Classes, System.SysUtils, System.Rtti, System.Generics.Defaults,
  System.Generics.Collections,
  WiRL.Core.Singleton,
  WiRL.http.Accept.MediaType,
  WiRL.Core.Request,
  WiRL.Core.Declarations,
  WiRL.Core.Classes;

type
  IMessageBodyReader = interface
  ['{472A6C22-F4AF-4E77-B6BB-B1085A63504D}']
    function ReadFrom(AParam: TRttiParameter;
      AMediaType: TMediaType; ARequest: TWiRLRequest): TValue;
  end;

  TGetAffinityFunction = reference to function(AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer;

  TMessageBodyReaderRegistry = class
  private
    type
      TWiRLRegistrySingleton = TWiRLSingleton<TMessageBodyReaderRegistry>;
      TEntryInfo = record
        TypeMetadata: TRttiType;
        CreateInstance: TFunc<IMessageBodyReader>;
        IsReadable: TFunc<TRttiType, TAttributeArray, TMediaType, Boolean>;
        GetAffinity: TGetAffinityFunction;
      end;
  private
    FRegistry: TList<TEntryInfo>;
    class function GetInstance: TMessageBodyReaderRegistry; static; inline;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterReader(const ACreateInstance: TFunc<IMessageBodyReader>;
      const AIsReadable: TFunc<TRttiType, TAttributeArray, TMediaType, Boolean>;
      const AGetAffinity: TGetAffinityFunction; AReaderRttiType: TRttiType); overload;

    procedure RegisterReader(const AReaderClass: TClass;
      const AIsWritable: TFunc<TRttiType, TAttributeArray, TMediaType, Boolean>;
      const AGetAffinity: TGetAffinityFunction); overload;

    procedure RegisterReader(const AReaderClass, ASubjectClass: TClass;
      const AGetAffinity: TGetAffinityFunction); overload;

    procedure RegisterReader<T: class>(const AReaderClass: TClass); overload;

    function FindReader(AType: TRttiType; AMediaType: TMediaType): IMessageBodyReader;

    class property Instance: TMessageBodyReaderRegistry read GetInstance;
    class function GetDefaultClassAffinityFunc<T: class>: TGetAffinityFunction;
  end;


implementation

uses
  WiRL.Rtti.Utils,
  WiRL.Core.Attributes;

{ TMessageBodyReaderRegistry }

constructor TMessageBodyReaderRegistry.Create;
begin
  FRegistry := TList<TEntryInfo>.Create;
end;

destructor TMessageBodyReaderRegistry.Destroy;
begin
  FRegistry.Free;
  inherited;
end;

function TMessageBodyReaderRegistry.FindReader(AType: TRttiType; AMediaType: TMediaType): IMessageBodyReader;
var
  LEntry: TEntryInfo;
  LFound: Boolean;
  LCompatibleEntries: TArray<TEntryInfo>;
  LCurrentAffinity, LCandidateAffinity: Integer;
  LCandidate: TEntryInfo;
begin
  for LEntry in FRegistry do
  begin
    LFound := False;

    TRttiHelper.ForEachAttribute<ConsumesAttribute>(LEntry.TypeMetadata,
      procedure (AAttrib: ConsumesAttribute)
      begin
        if AMediaType.ToString = AAttrib.Value then
          LFound := True;
      end
    );
    if LFound and LEntry.IsReadable(AType, AType.GetAttributes, AMediaType) then
    begin
      {$ifndef DelphiXE7_UP}
      SetLength(LCompatibleEntries, Length(LCompatibleEntries) + 1);
      LCompatibleEntries[High(LCompatibleEntries)] := LEntry;
      {$else}
      LCompatibleEntries := LCompatibleEntries + [LEntry];
      {$endif}
    end;
  end;

  case Length(LCompatibleEntries) of
    0: Result := nil;
    1: Result := LCompatibleEntries[0].CreateInstance();
    else
    begin  // devo scegliere quello migliore fra quelli compatibili
      LCandidate := LCompatibleEntries[0];
      LCandidateAffinity := LCandidate.GetAffinity(AType, AType.GetAttributes, AMediaType.ToString);

      for LEntry in LCompatibleEntries do
      begin
        LCurrentAffinity := LCandidate.GetAffinity(AType, AType.GetAttributes, AMediaType.ToString);

        if LCurrentAffinity >= LCandidateAffinity then
        begin
          LCandidate := LEntry;
          LCandidateAffinity := LCurrentAffinity;
        end;
      end;

      Result := LCandidate.CreateInstance();
    end;
  end;
end;

class function TMessageBodyReaderRegistry.GetDefaultClassAffinityFunc<T>: TGetAffinityFunction;
begin
  Result :=
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
    begin
      if Assigned(AType) and TRttiHelper.IsObjectOfType<T>(AType, False) then
        Result := 100
      else if Assigned(AType) and TRttiHelper.IsObjectOfType<T>(AType) then
        Result := 99
      else
        Result := 0;
    end
end;

class function TMessageBodyReaderRegistry.GetInstance: TMessageBodyReaderRegistry;
begin
  Result := TWiRLRegistrySingleton.Instance;
end;

procedure TMessageBodyReaderRegistry.RegisterReader(const AReaderClass: TClass;
  const AIsWritable: TFunc<TRttiType, TAttributeArray, TMediaType, Boolean>;
  const AGetAffinity: TGetAffinityFunction);
begin
  RegisterReader(
    function : IMessageBodyReader
    var LInstance: TObject;
    begin
      LInstance := TRttiHelper.CreateInstance(AReaderClass);
      //LInstance := AReaderClass.Create;
      if not Supports(LInstance, IMessageBodyReader, Result) then
        raise Exception.Create('Interface IMessageBodyReader not implemented');
    end
    , AIsWritable
    , AGetAffinity
    , TRttiContext.Create.GetType(AReaderClass)
  );
end;

procedure TMessageBodyReaderRegistry.RegisterReader(const AReaderClass,
  ASubjectClass: TClass; const AGetAffinity: TGetAffinityFunction);
begin
  RegisterReader(
    AReaderClass,
    function (AType: TRttiType; AAttributes: TAttributeArray; AMediaType: TMediaType): Boolean
    begin
      Result := Assigned(AType) and TRttiHelper.IsObjectOfType(AType, ASubjectClass);
    end,
    AGetAffinity
  );
end;

procedure TMessageBodyReaderRegistry.RegisterReader<T>(const AReaderClass:
    TClass);
begin
  RegisterReader(
    AReaderClass
    , function (AType: TRttiType; AAttributes: TAttributeArray; AMediaType: TMediaType): Boolean
      begin
        Result := Assigned(AType) and TRttiHelper.IsObjectOfType<T>(AType);
      end
    , Self.GetDefaultClassAffinityFunc<T>()
  );
end;

procedure TMessageBodyReaderRegistry.RegisterReader(
  const ACreateInstance: TFunc<IMessageBodyReader>;
  const AIsReadable: TFunc<TRttiType, TAttributeArray, TMediaType, Boolean>;
  const AGetAffinity: TGetAffinityFunction; AReaderRttiType: TRttiType);
var
  LEntryInfo: TEntryInfo;
begin
  LEntryInfo.CreateInstance := ACreateInstance;
  LEntryInfo.IsReadable := AIsReadable;
  LEntryInfo.TypeMetadata := AReaderRttiType;
  LEntryInfo.GetAffinity := AGetAffinity;

  FRegistry.Add(LEntryInfo)
end;

end.
