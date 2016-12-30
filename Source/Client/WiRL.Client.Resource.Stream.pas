{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Client.Resource.Stream;

{$I WiRL.inc}

interface

uses
  SysUtils, Classes

  , WiRL.Client.Resource
  , WiRL.Client.Client
  ;

type
  {$ifdef DelphiXE2_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  {$endif}
  TWiRLClientResourceStream = class(TWiRLClientResource)
  private
    FResponse: TStream;
  protected
    procedure AfterGET(); override;
    procedure AfterPOST(); override;
    function GetResponseSize: Int64; virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Response: TStream read FResponse;
    property ResponseSize: Int64 read GetResponseSize;
  end;

procedure Register;

implementation

uses
  WiRL.Core.Utils;

procedure Register;
begin
  RegisterComponents('WiRL Client', [TWiRLClientResourceStream]);
end;

{ TWiRLClientResourceStream }

procedure TWiRLClientResourceStream.AfterGET();
begin
  inherited;
  CopyStream(Client.Response.ContentStream, FResponse);
end;

procedure TWiRLClientResourceStream.AfterPOST;
begin
  inherited;
  CopyStream(Client.Response.ContentStream, FResponse);
end;

constructor TWiRLClientResourceStream.Create(AOwner: TComponent);
begin
  inherited;
  SpecificAccept := '*/*';
  FResponse := TMemoryStream.Create;
end;

destructor TWiRLClientResourceStream.Destroy;
begin
  FResponse.Free;
  inherited;
end;

function TWiRLClientResourceStream.GetResponseSize: Int64;
begin
  Result := FResponse.Size;
end;

end.
