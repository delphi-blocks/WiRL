unit Server.Listener;

interface

uses
  System.Classes, System.SysUtils,

  WiRL.Configuration.Core,
  WiRL.Configuration.Compression,
  WiRL.Core.Classes,
  WiRL.Engine.Core,
  WiRL.Engine.REST,
  WiRL.Core.Application,
  WiRL.Core.MessageBody.Default,
  WiRL.Data.MessageBody.Default,
  WiRL.http.Server,
  WiRL.http.Server.Indy;

type
  TListener = class(TObject)
  private
    FServer: TWiRLServer;
    FPort: Integer;
    FName: string;
    FDisplayName: string;
    function GetActive: Boolean;
    procedure SetPort(const Value: Integer);
  public
    property Name: string read FName;
    property DisplayName: string read FDisplayName;
    property Port: Integer read FPort write SetPort;

    procedure Start;
    procedure Stop;
    property Active: Boolean read GetActive;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  WiRL.Core.JSON,
  WiRL.Rtti.Utils;


{ TListener }

constructor TListener.Create;
begin
  FPort := 8080;
  FName := 'WiRLDemo';
  FDisplayName := 'WIRL - Multiplatform demo';
end;

destructor TListener.Destroy;
begin
  Stop;
  inherited;
end;

function TListener.GetActive: Boolean;
begin
  Result := Assigned(FServer);
end;

procedure TListener.SetPort(const Value: Integer);
begin
  if Active then
    raise Exception.Create('Stop before change port');
  FPort := Value;
end;

procedure TListener.Start;
begin
  if Assigned(FServer) then
    Exit;

  FServer := TWiRLServer.Create(nil);

  // Server configuration
  FServer
    .SetPort(FPort)
    // Engine configuration
    .AddEngine<TWiRLRESTEngine>('/rest')
      .SetEngineName('WiRL Demo')

      // Application configuration
      .AddApplication('/app')
        .SetAppName('App')
        .SetFilters('*')
        .SetResources('*')

      .Plugin.Configure<IWiRLConfigurationCompression>
        .SetMinSize(300)
        .SetMediaTypes('application/xml,application/json,text/plain')
        .ApplyConfig
  ;

  if not FServer.Active then
    FServer.Active := True;
end;

procedure TListener.Stop;
begin
  if not Assigned(FServer) then
    Exit;

  FServer.Active := False;
  FreeAndNil(FServer);
end;

end.
