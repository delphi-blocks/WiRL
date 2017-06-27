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

interface

uses
  System.Classes, System.SysUtils, System.Rtti, System.Generics.Collections,

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

  TGetAffinityFunction = reference to function(AType: TRttiType;
    const AAttributes: TAttributeArray; AMediaType: string): Integer;

  TWiRLReaderRegistry = class
  public
    const AFFINITY_VERY_HIGH = 50;
    const AFFINITY_HIGH = 30;
    const AFFINITY_LOW = 10;
    const AFFINITY_VERY_LOW = 1;
    const AFFINITY_ZERO = 0;
  public type
    TReaderInfo = class
    private
      FReaderType: TRttiType;
      FReaderName: string;
      FConsumes: TMediaTypeList;
      function GetConsumesMediaTypes(AType: TRttiType): TMediaTypeList;
    public
      CreateInstance: TFunc<IMessageBodyReader>;
      IsReadable: TFunc<TRttiType, TAttributeArray, TMediaType, Boolean>;
      GetAffinity: TGetAffinityFunction;
    public
      constructor Create(AType: TRttiType);
      destructor Destroy; override;

      property Consumes: TMediaTypeList read FConsumes;
      property ReaderName: string read FReaderName;
      property ReaderType: TRttiType read FReaderType write FReaderType;
    end;
  private
    function GetCount: Integer;
  protected
    FRegistry: TObjectList<TReaderInfo>;
  public
    constructor Create; overload;
    constructor Create(AOwnsObjects: Boolean); overload;
    destructor Destroy; override;

    function GetReaderByName(const AQualifiedClassName: string): TReaderInfo;
    function Add(AReader: TReaderInfo): Integer;
    procedure Assign(ARegistry: TWiRLReaderRegistry);
    procedure Enumerate(const AProc: TProc<TReaderInfo>);
    function FindReader(AParam: TRttiType; AMediaType: TMediaType): IMessageBodyReader;

    class function GetDefaultClassAffinityFunc<T: class>: TGetAffinityFunction;

    property Count: Integer read GetCount;
  end;

  TMessageBodyReaderRegistry = class(TWiRLReaderRegistry)
  private type
    TWiRLRegistrySingleton = TWiRLSingleton<TMessageBodyReaderRegistry>;
  private
    class function GetInstance: TMessageBodyReaderRegistry; static; inline;
  public
    procedure RegisterReader(const ACreateInstance: TFunc<IMessageBodyReader>;
      const AIsReadable: TFunc<TRttiType, TAttributeArray, TMediaType, Boolean>;
      const AGetAffinity: TGetAffinityFunction; AReaderRttiType: TRttiType); overload;

    procedure RegisterReader(const AReaderClass: TClass; const AIsWritable: TFunc<TRttiType,
      TAttributeArray, TMediaType, Boolean>; const AGetAffinity: TGetAffinityFunction); overload;

    procedure RegisterReader(const AReaderClass, ASubjectClass: TClass;
      const AGetAffinity: TGetAffinityFunction); overload;

    procedure RegisterReader<T: class>(const AReaderClass: TClass); overload;

    class property Instance: TMessageBodyReaderRegistry read GetInstance;
  end;


implementation

uses
  WiRL.Rtti.Utils,
  WiRL.Core.Attributes;

{ TWiRLReaderRegistry }

constructor TWiRLReaderRegistry.Create;
begin
  Create(True);
end;

function TWiRLReaderRegistry.Add(AReader: TReaderInfo): Integer;
begin
  Result := FRegistry.Add(AReader);
end;

procedure TWiRLReaderRegistry.Assign(ARegistry: TWiRLReaderRegistry);
var
  LReaderInfo: TReaderInfo;
begin
  for LReaderInfo in ARegistry.FRegistry do
    FRegistry.Add(LReaderInfo);
end;

constructor TWiRLReaderRegistry.Create(AOwnsObjects: Boolean);
begin
  FRegistry := TObjectList<TReaderInfo>.Create(AOwnsObjects);
end;

destructor TWiRLReaderRegistry.Destroy;
begin
  FRegistry.Free;
  inherited;
end;

procedure TWiRLReaderRegistry.Enumerate(const AProc: TProc<TReaderInfo>);
begin

end;

function TWiRLReaderRegistry.FindReader(AParam: TRttiType; AMediaType: TMediaType): IMessageBodyReader;
var
  LEntry: TReaderInfo;
  LCompatibleEntries: TArray<TReaderInfo>;
  LCurrentAffinity, LCandidateAffinity: Integer;
  LCandidate: TReaderInfo;
begin
  SetLength(LCompatibleEntries, 0);

  for LEntry in FRegistry do
  begin
    if LEntry.Consumes.Contains(AMediaType) and
       LEntry.IsReadable(AParam, AParam.GetAttributes, AMediaType) then
    begin
      {$IFNDEF DelphiXE7_UP}
      SetLength(LCompatibleEntries, Length(LCompatibleEntries) + 1);
      LCompatibleEntries[High(LCompatibleEntries)] := LEntry;
      {$ELSE}
      LCompatibleEntries := LCompatibleEntries + [LEntry];
      {$ENDIF}
    end;
  end;

  case Length(LCompatibleEntries) of
    0: Result := nil;
    1: Result := LCompatibleEntries[0].CreateInstance();
  else
    begin
      LCandidate := LCompatibleEntries[0];
      LCandidateAffinity := LCandidate.GetAffinity(AParam, AParam.GetAttributes, AMediaType.ToString);

      for LEntry in LCompatibleEntries do
      begin
        LCurrentAffinity := LCandidate.GetAffinity(AParam, AParam.GetAttributes, AMediaType.ToString);

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

function TWiRLReaderRegistry.GetCount: Integer;
begin
  Result := FRegistry.Count;
end;

class function TWiRLReaderRegistry.GetDefaultClassAffinityFunc<T>: TGetAffinityFunction;
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

function TWiRLReaderRegistry.GetReaderByName(const AQualifiedClassName: string): TReaderInfo;
var
  LReaderInfo: TReaderInfo;
begin
  Result := nil;
  for LReaderInfo in FRegistry do
    if SameText(LReaderInfo.FReaderName, AQualifiedClassName) then
      Exit(LReaderInfo);
end;

class function TMessageBodyReaderRegistry.GetInstance: TMessageBodyReaderRegistry;
begin
  Result := TWiRLRegistrySingleton.Instance;
end;

procedure TMessageBodyReaderRegistry.RegisterReader(const ACreateInstance:
    TFunc<IMessageBodyReader>; const AIsReadable: TFunc<TRttiType,
    TAttributeArray, TMediaType, Boolean>; const AGetAffinity:
    TGetAffinityFunction; AReaderRttiType: TRttiType);
var
  LEntryInfo: TReaderInfo;
begin
  LEntryInfo := TReaderInfo.Create(AReaderRttiType);

  LEntryInfo.CreateInstance := ACreateInstance;
  LEntryInfo.IsReadable := AIsReadable;
  LEntryInfo.GetAffinity := AGetAffinity;

  FRegistry.Add(LEntryInfo)
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
    end,
    AIsWritable,
    AGetAffinity,
    TRttiContext.Create.GetType(AReaderClass)
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

procedure TMessageBodyReaderRegistry.RegisterReader<T>(const AReaderClass: TClass);
begin
  RegisterReader(
    AReaderClass,
    function (AType: TRttiType; AAttributes: TAttributeArray; AMediaType: TMediaType): Boolean
    begin
      Result := Assigned(AType) and TRttiHelper.IsObjectOfType<T>(AType);
    end,
    Self.GetDefaultClassAffinityFunc<T>()
  );
end;

{ TWiRLReaderRegistry.TReaderInfo }

constructor TWiRLReaderRegistry.TReaderInfo.Create(AType: TRttiType);
begin
  FReaderType := AType;
  FReaderName := AType.QualifiedName;
  FConsumes := GetConsumesMediaTypes(AType);
end;

destructor TWiRLReaderRegistry.TReaderInfo.Destroy;
begin
  FConsumes.Free;
  inherited;
end;

function TWiRLReaderRegistry.TReaderInfo.GetConsumesMediaTypes(AType: TRttiType): TMediaTypeList;
var
  LList: TMediaTypeList;
begin
  LList := TMediaTypeList.Create;

  TRttiHelper.ForEachAttribute<ConsumesAttribute>(AType,
    procedure (AConsumes: ConsumesAttribute)
    var
      LMediaList: TArray<string>;
      LMedia: string;
    begin
      LMediaList := AConsumes.Value.Split([',']);

      for LMedia in LMediaList do
        LList.Add(TMediaType.Create(LMedia));
    end
  );

  Result := LList;
end;

end.
