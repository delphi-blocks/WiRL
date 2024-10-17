unit Server.WinService;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils,
  System.Classes, System.IOUtils, Vcl.SvcMgr,

  Server.Filters,
  Server.Listener;

type
  TmodService = class(TService, ILogger)
    procedure ServiceExecute(Sender: TService);
  private
    FListener: TListener;

    { ILogger }
    procedure Log(const AMessage: string);
  public
    function GetServiceController: TServiceController; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  modService: TmodService;

implementation

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  modService.Controller(CtrlCode);
end;

constructor TmodService.Create(AOwner: TComponent);
begin
  inherited;
  FListener := TListener.Create;
  Name := FListener.Name;
  DisplayName := FListener.DisplayName;

  RegisterLogger(Self);
end;

destructor TmodService.Destroy;
begin
  UnregisterLogger(Self);

  FListener.Free;
  inherited;
end;

function TmodService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TmodService.Log(const AMessage: string);
var
  LLogPath: string;
begin
  LLogPath := TPath.Combine(ExtractFileDir(ParamStr(0)), 'log.txt');

  TMonitor.Enter(Self);
  try
    TFile.AppendAllText(LLogPath, AMessage + sLineBreak, TEncoding.UTF8);
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TmodService.ServiceExecute(Sender: TService);
begin
  FListener.Start;
  while not Terminated do
  begin
    if Assigned(ServiceThread) then
      ServiceThread.ProcessRequests(False);
    Sleep(1000);
  end;
  FListener.Stop;
end;

end.
