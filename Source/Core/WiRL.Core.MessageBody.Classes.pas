{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.MessageBody.Classes;

interface

uses
  System.Classes, System.SysUtils, System.Rtti,

  WiRL.Core.JSON,
  WiRL.Core.Declarations,
  WiRL.Core.Classes,
  WiRL.http.Core,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.Core.Attributes,
  WiRL.Core.Application,
  WiRL.http.Accept.MediaType,
  WiRL.Core.MessageBodyWriter,
  WiRL.Core.MessageBodyReader;

type
  TMessageBodyWriter = class(TInterfacedObject, IMessageBodyWriter)
  protected
    [Context] WiRLApplication: TWiRLApplication;
  public
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaderFields: TWiRLHeaderList; AContentStream: TStream); virtual; abstract;
  end;

  TMessageBodyReader = class(TInterfacedObject, IMessageBodyReader)
  protected
    [Context] WiRLApplication: TWiRLApplication;
  public
    function ReadFrom(AParam: TRttiParameter; AMediaType: TMediaType;
      AHeaderFields: TWiRLHeaderList; AContentStream: TStream): TValue; virtual; abstract;
  end;

  TMessageBodyProvider = class(TInterfacedObject, IMessageBodyReader, IMessageBodyWriter)
  protected
    [Context] FRequest: TWiRLRequest;
    [Context] WiRLApplication: TWiRLApplication;

    procedure WriteJSONToStream(AJSON: TJSONValue; AStream: TStream);
    procedure WriteJSONPToStream(AJSON: TJSONValue; AStream: TStream);
  public
    function ReadFrom(AParam: TRttiParameter; AMediaType: TMediaType;
      AHeaderFields: TWiRLHeaderList; AContentStream: TStream): TValue; virtual; abstract;

    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaderFields: TWiRLHeaderList; AContentStream: TStream); virtual; abstract;
  end;

implementation

uses
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON;

{ TMessageBodyProvider }

procedure TMessageBodyProvider.WriteJSONPToStream(AJSON: TJSONValue; AStream: TStream);
var
  LCallback: string;
  LBytes: TBytes;
begin
  LCallback := FRequest.QueryFields.Values['callback'];
  if LCallback.IsEmpty then
    LCallback := 'callback';

  LBytes := TEncoding.UTF8.GetBytes(LCallback + '(');
  AStream.Write(LBytes[0], Length(LBytes));

  TNeon.PrintToStream(AJSON, AStream, WiRLApplication.SerializerConfig.GetPrettyPrint);

  LBytes := TEncoding.UTF8.GetBytes(');');
  AStream.Write(LBytes[0], Length(LBytes));
end;

procedure TMessageBodyProvider.WriteJSONToStream(AJSON: TJSONValue; AStream: TStream);
begin
  TNeon.PrintToStream(AJSON, AStream, WiRLApplication.SerializerConfig.GetPrettyPrint);
end;

end.
