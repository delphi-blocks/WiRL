{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.MessageBodyReader;

{$I WiRL.inc}

interface

uses
  System.Classes, System.SysUtils, System.Rtti, System.Generics.Collections,

  WiRL.Core.Singleton,
  WiRL.http.Core,
  WiRL.http.Headers,
  WiRL.http.Accept.MediaType,
  WiRL.http.Request,
  WiRL.Core.Declarations,
  WiRL.Core.Classes;

type
  /// <summary>
  ///   Interface for a provider that supports the conversion of a stream to a
  ///   Delphi type. A IMessageBodyReader implementation may be annotated with [Consumes] to
  ///   restrict the media types for which it will be considered suitable
  /// </summary>
  /// <remarks>
  ///   Providers implementing IMessageBodyReader interface must be registered in the
  ///   WiRL IMessageBodyReader Registry at runtime.
  /// </remarks>
  IMessageBodyReader = interface
  ['{472A6C22-F4AF-4E77-B6BB-B1085A63504D}']

    /// <summary>
    ///   Read a type from the HTTP Request stream
    /// </summary>
    function ReadFrom(AType: TRttitype; AMediaType: TMediaType;
      AHeaders: IWiRLHeaders; AContentStream: TStream): TValue; overload;

    /// <summary>
    ///   Read a type from the HTTP Request stream.
    /// </summary>
    /// <remarks>
    ///   Use this overloaded methods when the object to be deserialized
    ///   already exists!
    /// </remarks>
    procedure ReadFrom(AObject: TObject; AType: TRttitype; AMediaType: TMediaType;
      AHeaders: IWiRLHeaders; AContentStream: TStream); overload;
  end;

  TIsReadableFunction = reference to function(AType: TRttiType;
    const AAttributes: TAttributeArray; AMediaType: TMediaType): Boolean;

  TGetAffinityFunction = reference to function(AType: TRttiType;
    const AAttributes: TAttributeArray; AMediaType: TMediaType): Integer;

  /// <summary>
  ///   Global registry for classes that implements the IMessageBodyReader interface
  /// </summary>
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
      IsReadable: TIsReadableFunction;
      GetAffinity: TGetAffinityFunction;
    public
      constructor Create(AType: TRttiType); overload;
      constructor Create(const AReaderName: string); overload;
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
    procedure Clear;
    function GetEnumerator: TObjectList<TReaderInfo>.TEnumerator;
    function Add(AReader: TReaderInfo): Integer;
    function AddReaderName(const AReaderName: string): TReaderInfo;
    procedure Assign(ARegistry: TWiRLReaderRegistry);
    procedure Enumerate(const AProc: TProc<TReaderInfo>);
    function FindReader(AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): IMessageBodyReader;

    class function GetDefaultClassAffinityFunc<T: class>: TGetAffinityFunction;

    property Count: Integer read GetCount;
  end;

  TMessageBodyReaderRegistry = class(TWiRLReaderRegistry)
  private type
    TMessageBodyReaderRegistrySingleton = TWiRLSingleton<TMessageBodyReaderRegistry>;
  private
    class function GetInstance: TMessageBodyReaderRegistry; static; inline;
  public
    procedure RegisterReader(const ACreateInstance: TFunc<IMessageBodyReader>;
      const AIsReadable: TIsReadableFunction;
      const AGetAffinity: TGetAffinityFunction; AReaderRttiType: TRttiType); overload;

    procedure RegisterReader(const AReaderClass: TClass;
      const AIsReadable: TIsReadableFunction;
      const AGetAffinity: TGetAffinityFunction); overload;

    procedure RegisterReader(const AReaderClass, ASubjectClass: TClass;
      const AGetAffinity: TGetAffinityFunction); overload;

    procedure RegisterReader<T: class>(const AReaderClass: TClass; AAffinity: Integer = 0); overload;

    procedure RegisterReader(const AReaderClass: TClass; ASubjectIntf: TGUID;
      const AGetAffinity: TGetAffinityFunction); overload;

    class property Instance: TMessageBodyReaderRegistry read GetInstance;
  end;


implementation

uses
  WiRL.Core.Exceptions,
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

function TWiRLReaderRegistry.AddReaderName(const AReaderName: string): TReaderInfo;
begin
  Result := TReaderInfo.Create(AReaderName);
  FRegistry.Add(Result);
end;

procedure TWiRLReaderRegistry.Assign(ARegistry: TWiRLReaderRegistry);
var
  LReaderInfo: TReaderInfo;
begin
  for LReaderInfo in ARegistry.FRegistry do
    FRegistry.Add(LReaderInfo);
end;

procedure TWiRLReaderRegistry.Clear;
begin
  FRegistry.Clear;
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

function TWiRLReaderRegistry.FindReader(AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): IMessageBodyReader;
var
  LEntry: TReaderInfo;
  LCompatibleEntries: TArray<TReaderInfo>;
  LCurrentAffinity, LCandidateAffinity: Integer;
  LCandidate: TReaderInfo;
begin
  SetLength(LCompatibleEntries, 0);

  for LEntry in FRegistry do
  begin
    if (LEntry.Consumes.Contains(AMediaType) or LEntry.Consumes.Contains(TMediaType.WILDCARD)) and
       LEntry.IsReadable(AType, AAttributes, AMediaType) then
      LCompatibleEntries := LCompatibleEntries + [LEntry];
  end;

  case Length(LCompatibleEntries) of
    0: Result := nil;
    1: Result := LCompatibleEntries[0].CreateInstance();
  else
    begin
      LCandidate := LCompatibleEntries[0];
      LCandidateAffinity := LCandidate.GetAffinity(AType, AAttributes, AMediaType);

      for LEntry in LCompatibleEntries do
      begin
        LCurrentAffinity := LEntry.GetAffinity(AType, AAttributes, AMediaType);

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
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Integer
    begin
      if Assigned(AType) and TRttiHelper.IsObjectOfType<T>(AType, False) then
        Result := AFFINITY_HIGH
      else if Assigned(AType) and TRttiHelper.IsObjectOfType<T>(AType) then
        Result := AFFINITY_LOW
      else
        Result := AFFINITY_ZERO;
    end
end;

function TWiRLReaderRegistry.GetEnumerator: TObjectList<TReaderInfo>.TEnumerator;
begin
  Result := FRegistry.GetEnumerator;
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
  Result := TMessageBodyReaderRegistrySingleton.Instance;
end;

procedure TMessageBodyReaderRegistry.RegisterReader(
    const ACreateInstance: TFunc<IMessageBodyReader>;
    const AIsReadable: TIsReadableFunction;
    const AGetAffinity: TGetAffinityFunction; AReaderRttiType: TRttiType);
var
  LEntryInfo: TReaderInfo;
begin
  LEntryInfo := TReaderInfo.Create(AReaderRttiType);

  LEntryInfo.CreateInstance := ACreateInstance;
  LEntryInfo.IsReadable := AIsReadable;
  LEntryInfo.GetAffinity := AGetAffinity;

  FRegistry.Add(LEntryInfo)
end;

procedure TMessageBodyReaderRegistry.RegisterReader(
  const AReaderClass: TClass;
  const AIsReadable: TIsReadableFunction;
  const AGetAffinity: TGetAffinityFunction);
begin
  RegisterReader(
    function : IMessageBodyReader
    var LInstance: TObject;
    begin
      LInstance := TRttiHelper.CreateInstance(AReaderClass);
      //LInstance := AReaderClass.Create;
      if not Supports(LInstance, IMessageBodyReader, Result) then
        raise EWiRLServerException.Create('Interface IMessageBodyReader not implemented');
    end,
    AIsReadable,
    AGetAffinity,
    TRttiHelper.Context.GetType(AReaderClass)
  );
end;

procedure TMessageBodyReaderRegistry.RegisterReader(const AReaderClass,
    ASubjectClass: TClass; const AGetAffinity: TGetAffinityFunction);
begin
  RegisterReader(
    AReaderClass,
    function(AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Boolean
    begin
      Result := Assigned(AType) and TRttiHelper.IsObjectOfType(AType, ASubjectClass);
    end,
    AGetAffinity
  );
end;

procedure TMessageBodyReaderRegistry.RegisterReader<T>(const AReaderClass: TClass; AAffinity: Integer = 0);
var
  LAffinity: TGetAffinityFunction;
begin
  if AAffinity = 0 then
    LAffinity := Self.GetDefaultClassAffinityFunc<T>()
  else
    LAffinity :=
      function(AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Integer
      begin
        Result := AAffinity;
      end;

  RegisterReader(
    AReaderClass,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Boolean
    begin
      Result := Assigned(AType) and TRttiHelper.IsObjectOfType<T>(AType);
    end,
    LAffinity
  );
end;

{ TWiRLReaderRegistry.TReaderInfo }

constructor TWiRLReaderRegistry.TReaderInfo.Create(AType: TRttiType);
begin
  FReaderType := AType;
  FReaderName := AType.QualifiedName;
  FConsumes := GetConsumesMediaTypes(AType);
end;

constructor TWiRLReaderRegistry.TReaderInfo.Create(const AReaderName: string);
begin
  FReaderName := AReaderName;
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

procedure TMessageBodyReaderRegistry.RegisterReader(const AReaderClass: TClass;
  ASubjectIntf: TGUID; const AGetAffinity: TGetAffinityFunction);
begin
  RegisterReader(
    AReaderClass,
    function(AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Boolean
    begin
      Result := Assigned(AType) and TRttiHelper.IsInterfaceOfType(AType, ASubjectIntf);
    end,
    AGetAffinity
  );
end;

end.
