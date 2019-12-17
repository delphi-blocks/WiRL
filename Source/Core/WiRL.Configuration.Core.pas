unit WiRL.Configuration.Core;

interface

uses
  System.Classes, System.SysUtils, System.TypInfo,
  WiRL.Core.JSON,

  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON, System.JSON;

type
  // A non-reference-counted IInterface implementation.
  TWiRLConfigurationNRef = class(TPersistent, IInterface)
  private
    FNeonConfig: TNeonConfiguration;
    function GetAsJSON: TJSONObject;
    function GetAsString: string;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(AStream: TStream);
    procedure SaveToJSONObject(const AName: string; AJSON: TJSONObject);

    property AsString: string read GetAsString;
    property AsJSON: TJSONObject read GetAsJSON;
  end;



implementation

{ TWiRLConfigurationNRef }

constructor TWiRLConfigurationNRef.Create;
begin
  FNeonConfig := TNeonConfiguration.Create;
  FNeonConfig.SetVisibility([mvPublished]);
  FNeonConfig.SetPrettyPrint(False);
end;

destructor TWiRLConfigurationNRef.Destroy;
begin
  FNeonConfig.Free;
  inherited;
end;

function TWiRLConfigurationNRef.GetAsJSON: TJSONObject;
begin
  Result := TNeon.ObjectToJSON(Self, FNeonConfig) as TJSONObject;
end;

function TWiRLConfigurationNRef.GetAsString: string;
begin
  Result := TNeon.ObjectToJSONString(Self, FNeonConfig);
end;

procedure TWiRLConfigurationNRef.SaveToFile(const AFileName: string);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(AFileName, fmCreate or fmOpenReadWrite);
  try
    SaveToStream(LStream);
  finally
    LStream.Free;
  end;
end;

procedure TWiRLConfigurationNRef.SaveToJSONObject(const AName: string; AJSON: TJSONObject);
begin
  AJSON.AddPair(AName, GetAsJSON);
end;

procedure TWiRLConfigurationNRef.SaveToStream(AStream: TStream);
begin
  TNeon.PrintToStream(GetAsJSON, AStream, True);
end;

function TWiRLConfigurationNRef.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TWiRLConfigurationNRef._AddRef: Integer;
begin
  Result := -1;
end;

function TWiRLConfigurationNRef._Release: Integer;
begin
  Result := -1;
end;

end.
