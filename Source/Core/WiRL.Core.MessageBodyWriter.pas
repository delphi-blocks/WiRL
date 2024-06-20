{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.MessageBodyWriter;

interface

uses
  System.Classes, System.SysUtils, System.Rtti, System.Generics.Collections,

  WiRL.Core.Singleton,
  WiRL.http.Core,
  WiRL.http.Headers,
  WiRL.http.Response,
  WiRL.Core.Metadata,
  WiRL.http.Accept.MediaType,
  WiRL.Core.Declarations,
  WiRL.Core.Classes,
  WiRL.Core.Attributes;

type
  /// <summary>
  ///   Interface for a provider that supports the conversion of a Delphi type to a
  ///   stream. A IMessageBodyWriter implementation may be annotated with [Produces] to
  ///   restrict the media types for which it will be considered suitable
  /// </summary>
  /// <remarks>
  ///   Providers implementing IMessageBodyWriter interface must be registered in the
  ///   WiRL MessageBodyWriter Registry at runtime.
  /// </remarks>
  IMessageBodyWriter = interface
  ['{C22068E1-3085-482D-9EAB-4829C7AE87C0}']

    /// <summary>
    ///   Write a type to an HTTP message (body)
    /// </summary>
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaders: IWiRLHeaders; AContentStream: TStream);
  end;

  TIsWritableFunction = reference to function(AType: TRttiType;
    const AAttributes: TAttributeArray; AMediaType: TMediaType): Boolean;

  TGetAffinityFunction = reference to function(AType: TRttiType;
    const AAttributes: TAttributeArray; AMediaType: TMediaType): Integer;

  /// <summary>
  ///   Global registry for classes that implements the IMessageBodyWriter interface
  /// </summary>
  TWiRLWriterRegistry = class
  public
    const AFFINITY_VERY_HIGH = 50;
    const AFFINITY_HIGH = 30;
    const AFFINITY_LOW = 10;
    const AFFINITY_VERY_LOW = 1;
    const AFFINITY_ZERO = 0;
  public type
    TWriterInfo = class
    private
      FWriterType: TRttiType;
      FWriterName: string;
      FProduces: TMediaTypeList;
      function GetProducesMediaTypes(AObject: TRttiObject): TMediaTypeList;
    public
      CreateInstance: TFunc<IMessageBodyWriter>;
      IsWritable: TIsWritableFunction;
      GetAffinity: TGetAffinityFunction;

      constructor Create(AType: TRttiType); overload;
      constructor Create(const AWriterName: string); overload;
      destructor Destroy; override;

      property Produces: TMediaTypeList read FProduces;
      property WriterName: string read FWriterName;
      property WriterType: TRttiType read FWriterType write FWriterType;
    end;
  private
    function GetCount: Integer;
  protected
    FRegistry: TObjectList<TWriterInfo>;
    function ProducesAcceptIntersection(AProduces, AAccept: TMediaTypeList): TMediaTypeList;
    function InternalFindWriter(AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): IMessageBodyWriter; overload;
  public
    class function GetDefaultClassAffinityFunc<T: class>: TGetAffinityFunction;
  public
    constructor Create; overload;
    constructor Create(AOwnsObjects: Boolean); overload;
    destructor Destroy; override;

    procedure Clear;
    function GetEnumerator: TObjectList<TWriterInfo>.TEnumerator;
    function GetWriterByName(const AQualifiedClassName: string): TWriterInfo;
    function AddWriterName(const AWriterName: string): TWriterInfo;
    function Add(AWriter: TWriterInfo): Integer;
    procedure Assign(ARegistry: TWiRLWriterRegistry);
    procedure Enumerate(const AProc: TProc<TWriterInfo>);

    procedure FindWriter(AMethod: TWiRLProxyMethod; AAcceptMediaTypes: TMediaTypeList;
      out AWriter: IMessageBodyWriter; out AMediaType: TMediaType); overload;

    function FindWriter(AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): IMessageBodyWriter; overload;

    property Count: Integer read GetCount;
  end;

  TMessageBodyWriterRegistry = class(TWiRLWriterRegistry)
  private type
    TMessageBodyWriterRegistrySingleton = TWiRLSingleton<TMessageBodyWriterRegistry>;
  private
    class function GetInstance: TMessageBodyWriterRegistry; static; inline;
  public
    procedure RegisterWriter(const ACreateInstance: TFunc<IMessageBodyWriter>;
        const AIsWritable: TIsWritableFunction; const AGetAffinity:
        TGetAffinityFunction; AWriterRttiType: TRttiType); overload;

    procedure RegisterWriter(const AWriterClass: TClass; const AIsWritable:
        TIsWritableFunction; const AGetAffinity: TGetAffinityFunction); overload;

    procedure RegisterWriter(const AWriterClass: TClass; const ASubjectClass:
        TClass; const AGetAffinity: TGetAffinityFunction); overload;

    procedure RegisterWriter<T: class>(const AWriterClass: TClass; AAffinity: Integer = 0); overload;

    procedure RegisterWriter(const AWriterClass: TClass; ASubjectIntf:
        TGUID; const AGetAffinity: TGetAffinityFunction); overload;

    function UnregisterWriter(const AWriterClass: TClass): Integer; overload;
    function UnregisterWriter(const AQualifiedClassName: string): Integer; overload;
  public
    class property Instance: TMessageBodyWriterRegistry read GetInstance;
  end;


implementation

uses
  WiRL.Core.Exceptions,
  WiRL.Core.Utils,
  WiRL.Rtti.Utils;

{ TWiRLWriterRegistry }

function TWiRLWriterRegistry.Add(AWriter: TWriterInfo): Integer;
begin
  Result := FRegistry.Add(AWriter);
end;

function TWiRLWriterRegistry.AddWriterName(
  const AWriterName: string): TWriterInfo;
begin
  Result := TWriterInfo.Create(AWriterName);
  FRegistry.Add(Result);
end;

procedure TWiRLWriterRegistry.Assign(ARegistry: TWiRLWriterRegistry);
var
  LWriterInfo: TWriterInfo;
begin
  for LWriterInfo in ARegistry.FRegistry do
    FRegistry.Add(LWriterInfo);
end;

constructor TWiRLWriterRegistry.Create;
begin
  Create(True);
end;

procedure TWiRLWriterRegistry.Clear;
begin
  FRegistry.Clear;
end;

constructor TWiRLWriterRegistry.Create(AOwnsObjects: Boolean);
begin
  FRegistry := TObjectList<TWriterInfo>.Create(AOwnsObjects);
end;

destructor TWiRLWriterRegistry.Destroy;
begin
  FRegistry.Free;
  inherited;
end;

procedure TWiRLWriterRegistry.Enumerate(const AProc: TProc<TWriterInfo>);
var
  LEntry: TWriterInfo;
begin
  for LEntry in FRegistry do
    AProc(LEntry);
end;

function TWiRLWriterRegistry.FindWriter(AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): IMessageBodyWriter;
begin
  if FRegistry.Count = 0 then
    raise EWiRLServerException.Create('MessageBodyWriters registry is empty. Please include the MBW''s units in your project');

  Result := InternalFindWriter(AType, AAttributes, AMediaType);
end;

procedure TWiRLWriterRegistry.FindWriter(AMethod: TWiRLProxyMethod; AAcceptMediaTypes: TMediaTypeList;
  out AWriter: IMessageBodyWriter; out AMediaType: TMediaType);
var
  LMediaType: TMediaType;
  LAllowedMediaList: TMediaTypeList;
begin
  if FRegistry.Count = 0 then
    raise EWiRLServerException.Create('MessageBodyWriters registry is empty. Please include the MBW''s units in your project');

  AWriter := nil;
  AMediaType := nil;

  if not AMethod.IsFunction then
    Exit; // no serialization (it's a procedure!)

  LAllowedMediaList := ProducesAcceptIntersection(AMethod.Produces, AAcceptMediaTypes);
  try
    for LMediaType in LAllowedMediaList do
    begin
      AWriter := InternalFindWriter(AMethod.RttiObject.ReturnType, AMethod.RttiObject.GetAttributes, LMediaType);
      if Assigned(AWriter) then
      begin
        AMediaType := LMediaType.Clone;
        Break;
      end;
    end;
  finally
    LAllowedMediaList.Free;
  end;
end;

function TWiRLWriterRegistry.GetCount: Integer;
begin
  Result := FRegistry.Count;
end;

class function TWiRLWriterRegistry.GetDefaultClassAffinityFunc<T>: TGetAffinityFunction;
begin
  Result :=
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Integer
    begin
      if Assigned(AType) and TRttiHelper.IsObjectOfType<T>(AType, False) then
        Result := 100
      else if Assigned(AType) and TRttiHelper.IsObjectOfType<T>(AType) then
        Result := 99
      else
        Result := 0;
    end
end;

function TWiRLWriterRegistry.GetEnumerator: TObjectList<TWriterInfo>.TEnumerator;
begin
  Result := FRegistry.GetEnumerator;
end;

function TWiRLWriterRegistry.GetWriterByName(const AQualifiedClassName: string): TWriterInfo;
var
  LWriterInfo: TWriterInfo;
begin
  Result := nil;
  for LWriterInfo in FRegistry do
    if SameText(LWriterInfo.FWriterName, AQualifiedClassName) then
      Exit(LWriterInfo);
end;

function TWiRLWriterRegistry.InternalFindWriter(AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): IMessageBodyWriter;
var
  LEntry: TWriterInfo;
  LFound: Boolean;
  LCandidateAffinity: Integer;
  LCandidate: TWriterInfo;
begin
  Result := nil;
  LFound := False;
  LCandidate := nil;
  LCandidateAffinity := -1;

  for LEntry in FRegistry do
  begin
    if not Assigned(LEntry.Produces) then
      raise EWiRLServerException.CreateFmt('Attribute [Produce] required for [%s]', [LEntry.WriterName]);

    if LEntry.IsWritable(AType, AAttributes, AMediaType) and
       (AMediaType.IsWildcard or LEntry.Produces.Contains(TMediaType.WILDCARD) or LEntry.Produces.Contains(AMediaType)) then
    begin
      if not LFound or (LCandidateAffinity < LEntry.GetAffinity(AType, AAttributes, AMediaType)) then
      begin
        LCandidate := LEntry;
        LCandidateAffinity := LCandidate.GetAffinity(AType, AType.GetAttributes, AMediaType);
        LFound := True;
      end;
    end;
  end;

  if LFound then
    Result := LCandidate.CreateInstance();
end;

function TWiRLWriterRegistry.ProducesAcceptIntersection(AProduces, AAccept: TMediaTypeList): TMediaTypeList;
begin
  if AProduces.Empty then
    Result := AAccept.CloneList
  else if AAccept.Empty or AAccept.IsWildCard then
    Result := AProduces.CloneList
  else
    Result := AAccept.IntersectionList(AProduces);

  if Result.Empty and AAccept.HasWildCard then
    Result.Assign(AProduces);

  { TODO -opaolo -c : This have to be configuration-related 02/06/2017 18:00:25 }
  if Result.Empty then
  begin
    Result.Add(TMediaType.Create(TMediaType.APPLICATION_JSON));
    Result.Add(TMediaType.Create(TMediaType.WILDCARD));
  end;
end;

{ TWiRLWriterRegistry.TWriterInfo }

constructor TWiRLWriterRegistry.TWriterInfo.Create(AType: TRttiType);
begin
  FWriterType := AType;
  FWriterName := AType.QualifiedName;
  FProduces := GetProducesMediaTypes(AType);
end;

constructor TWiRLWriterRegistry.TWriterInfo.Create(const AWriterName: string);
begin
  FWriterName := AWriterName;
end;

destructor TWiRLWriterRegistry.TWriterInfo.Destroy;
begin
  FProduces.Free;
  inherited;
end;

function TWiRLWriterRegistry.TWriterInfo.GetProducesMediaTypes(AObject: TRttiObject): TMediaTypeList;
var
  LList: TMediaTypeList;
begin
  LList := TMediaTypeList.Create;

  TRttiHelper.ForEachAttribute<ProducesAttribute>(AObject,
    procedure (AProduces: ProducesAttribute)
    var
      LMediaList: TArray<string>;
      LMedia: string;
    begin
      LMediaList := AProduces.Value.Split([',']);

      for LMedia in LMediaList do
        LList.Add(TMediaType.Create(LMedia));
    end
  );

  Result := LList;
end;

class function TMessageBodyWriterRegistry.GetInstance: TMessageBodyWriterRegistry;
begin
  Result := TMessageBodyWriterRegistrySingleton.Instance;
end;

procedure TMessageBodyWriterRegistry.RegisterWriter(const ACreateInstance:
    TFunc<IMessageBodyWriter>; const AIsWritable: TIsWritableFunction; const
    AGetAffinity: TGetAffinityFunction; AWriterRttiType: TRttiType);
var
  LEntryInfo: TWriterInfo;
begin
  LEntryInfo := TWriterInfo.Create(AWriterRttiType);

  LEntryInfo.CreateInstance := ACreateInstance;
  LEntryInfo.IsWritable := AIsWritable;
  LEntryInfo.GetAffinity := AGetAffinity;

  FRegistry.Add(LEntryInfo)
end;

procedure TMessageBodyWriterRegistry.RegisterWriter(const AWriterClass: TClass;
    const AIsWritable: TIsWritableFunction; const AGetAffinity: TGetAffinityFunction);
begin
  RegisterWriter(
    function : IMessageBodyWriter
    var
      LInstance: TObject;
    begin
      LInstance := TRttiHelper.CreateInstance(AWriterClass);
      //LInstance := AWriterClass.Create;
      if not Supports(LInstance, IMessageBodyWriter, Result) then
        raise EWiRLServerException.Create('Interface IMessageBodyWriter not implemented');
    end,
    AIsWritable,
    AGetAffinity,
    TRttiContext.Create.GetType(AWriterClass)
  );
end;

procedure TMessageBodyWriterRegistry.RegisterWriter(const AWriterClass,
    ASubjectClass: TClass; const AGetAffinity: TGetAffinityFunction);
begin
  RegisterWriter(
    AWriterClass,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Boolean
    begin
      Result := Assigned(AType) and TRttiHelper.IsObjectOfType(AType, ASubjectClass);
    end,
    AGetAffinity
  );
end;

procedure TMessageBodyWriterRegistry.RegisterWriter<T>(const AWriterClass: TClass; AAffinity: Integer = 0);
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

  RegisterWriter(
    AWriterClass,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Boolean
    begin
      Result := Assigned(AType) and TRttiHelper.IsObjectOfType<T>(AType);
    end,
    LAffinity
  );
end;

function TMessageBodyWriterRegistry.UnregisterWriter(const AWriterClass: TClass): Integer;
begin
  Result := UnregisterWriter(AWriterClass.QualifiedClassName);
end;

function TMessageBodyWriterRegistry.UnregisterWriter(const AQualifiedClassName: string): Integer;
var
  LIndex: Integer;
begin
  Result := -1;
  for LIndex := 0 to FRegistry.Count - 1 do
    if FRegistry[LIndex].WriterName = AQualifiedClassName then
    begin
      FRegistry.Delete(LIndex);
      Result := LIndex;
      Break;
    end;
end;

procedure TMessageBodyWriterRegistry.RegisterWriter(const AWriterClass: TClass;
  ASubjectIntf: TGUID; const AGetAffinity: TGetAffinityFunction);
begin
  RegisterWriter(
    AWriterClass,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Boolean
    begin
      Result := Assigned(AType) and TRttiHelper.IsInterfaceOfType(AType, ASubjectIntf);
    end,
    AGetAffinity
  );
end;

end.
