{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Client.SubResource.Stream;

{$I WiRL.inc}

interface

uses
  SysUtils, Classes

  , WiRL.Client.SubResource
  , WiRL.Client.Client
  ;

type
  {$ifdef DelphiXE2_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  {$endif}
  TWiRLClientSubResourceStream = class(TWiRLClientSubResource)
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
  RegisterComponents('WiRL Client', [TWiRLClientSubResourceStream]);
end;

{ TWiRLClientResourceJSON }

procedure TWiRLClientSubResourceStream.AfterGET();
begin
  inherited;
  CopyStream(Client.Response.ContentStream, FResponse);
end;

procedure TWiRLClientSubResourceStream.AfterPOST;
begin
  inherited;
  CopyStream(Client.Response.ContentStream, FResponse);
end;

constructor TWiRLClientSubResourceStream.Create(AOwner: TComponent);
begin
  inherited;
  FResponse := TMemoryStream.Create;
end;

destructor TWiRLClientSubResourceStream.Destroy;
begin
  FResponse.Free;
  inherited;
end;


function TWiRLClientSubResourceStream.GetResponseSize: Int64;
begin
  Result := FResponse.Size;
end;

end.
