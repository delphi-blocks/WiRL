unit Server.WebModule;

interface

{$I WiRL.inc}

uses
  System.SysUtils, System.Classes, System.IOUtils, System.TypInfo,

  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}

  Neon.Core.Types,

  WiRL.http.Server,
  WiRL.http.Server.WebBroker,
  WiRL.Engine.Core,
  WiRL.Engine.REST,
  WiRL.Engine.WebServer,
  WiRL.Core.Converter,
  WiRL.Configuration.Converter,
  WiRL.Configuration.Neon,

  Web.HTTPApp;

type
  TWebModule2 = class(TWebModule)
    procedure WebModule2DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
    procedure WebModule2WebActionItem1Action(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModule2WebActionItem2Action(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
    FWiRLServer: TWiRLServer;
    FDispatcher: TWiRLDispatcher;
    function GetContentType(const AURL: string): string;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule2;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

function TWebModule2.GetContentType(const AURL: string): string;
begin
  if AURL.EndsWith('.js') then
    Result := 'application/javascript'
  else if AURL.EndsWith('.css') then
    Result := 'text/css'
  else if AURL.EndsWith('.txt') then
    Result := 'text/plain'
  else
    Result := 'text/html';
end;

procedure TWebModule2.WebModule2DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  LURL: string;
begin
  if Request.PathInfo.StartsWith('/.well-known') then
    Exit;
  if Request.PathInfo.StartsWith('/favicon') then
    Exit;

  if Request.PathInfo = '/' then
    LURL := 'index.html'
  else
    LURL := Copy(Request.PathInfo, 2, Length(Request.PathInfo));

  Response.ContentType := GetContentType(LURL);
  Response.Content := TFile.ReadAllText(TPath.Combine(TDirectory.GetCurrentDirectory, '..\..\www', LURL));
end;

procedure TWebModule2.WebModule2WebActionItem1Action(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  {$IFDEF HAS_WEBBROKER_SSE}
  //var LStream := TWebResponseStream.BeginEventsStream(Response);
  var LStream := TWebResponseStream.BeginStream(Response, 'text/event-stream; charset=utf-8');
  for var I := 1 to 5 do
  begin
    LStream.WriteData('test: ' + I.ToString);
    LStream.EndEvent;
    Sleep(1000);
  end;
  {$ENDIF}
end;

procedure TWebModule2.WebModule2WebActionItem2Action(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  {$IFDEF HAS_WEBBROKER_SSE}
  Response.CustomHeaders.Values['Transfer-Encoding'] := 'chunked';
  var EOL := TEncoding.UTF8.GetBytes(#13#10);
  // I don't need the stream but "BeginStream" forces the headers to be sent to
  // the client
  TWebResponseStream.BeginStream(Response, 'text/plain');
  for var I := 1 to 5 do
  begin
    var LBytes: TBytes;
    var LValue := 'Chunk #' + I.ToString;

    // Chunk size
    LBytes :=
      // Chunk size
      TEncoding.UTF8.GetBytes(IntToHex(Length(LValue))) + EOL +
      // Chunk content
      TEncoding.UTF8.GetBytes(LValue) + EOL;

    //LStream.Write(LBytes, Length(LBytes));
    Request.WriteClient(LBytes[0], Length(LBytes));

    Sleep(1000);
  end;
  var LBytes := TEncoding.UTF8.GetBytes('0') + EOL + EOL;
  Request.WriteClient(LBytes[0], Length(LBytes));
  {$ENDIF}
end;

procedure TWebModule2.WebModuleCreate(Sender: TObject);
begin
  FWiRLServer := TWiRLServer.Create(nil);

  FWiRLServer.AddEngine<TWiRLRESTEngine>('/rest')
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

//  FWiRLServer.AddEngine<TWiRLWebServerEngine>('/')
//    .SetEngineName('FileSystemEngine')
//    .SetRootFolder('..\..\www');

  FWiRLServer.Active := True;

  // Create the dispatcher for WebBroker
  FDispatcher := TWiRLDispatcher.Create(Self);
  FDispatcher.Server := FWiRLServer;
end;

procedure TWebModule2.WebModuleDestroy(Sender: TObject);
begin
  FDispatcher.Free;
  FWiRLServer.Free;
end;

end.
