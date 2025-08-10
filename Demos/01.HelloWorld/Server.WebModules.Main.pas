unit Server.WebModules.Main;

interface

uses
  System.SysUtils, System.Classes, System.TypInfo,
  Web.HTTPApp,
  Soap.SOAPHTTPDisp, Soap.WebBrokerSOAP,

  Neon.Core.Types,

  WiRL.http.Server,
  WiRL.http.Server.WebBroker,
  WiRL.Engine.Core,
  WiRL.Engine.REST,
  WiRL.Core.Converter,
  WiRL.Configuration.Converter,
  WiRL.Configuration.Neon;

type
  TMainWebModule = class(TWebModule)
    procedure WebModule1DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
    RESTServer: TWiRLServer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  WebModuleClass: TComponentClass = TMainWebModule;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

constructor TMainWebModule.Create(AOwner: TComponent);
begin
  inherited;
  RESTServer := TWiRLServer.Create(Self);

  RESTServer.AddEngine<TWiRLRESTEngine>('/rest')
    .SetEngineName('RESTEngine')
    .AddApplication('/app')
      .SetResources('*')
      .SetFilters('*')

      .Plugin.Configure<IWiRLFormatSetting>
        .AddFormat(TypeInfo(TDateTime), TWiRLFormatSetting.ISODATE_UTC)
        .ApplyConfig

      .Plugin.Configure<IWiRLConfigurationNeon>
        .SetUseUTCDate(True)
        .SetVisibility([mvPublic, mvPublished])
        .SetMemberCase(TNeonCase.PascalCase);

  RESTServer.Active := True;
end;

destructor TMainWebModule.Destroy;
begin
  RESTServer.Free;
  inherited;
end;

procedure TMainWebModule.WebModule1DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Response.Content :=
    '<html>' +
    '<head><title>Web Server Application</title></head>' +
    '<body>Web Server Application</body>' +
    '</html>';
end;

end.
