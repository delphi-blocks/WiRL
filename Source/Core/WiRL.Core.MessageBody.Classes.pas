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
    [Context] FRequest: TWiRLRequest;
    [Context] WiRLApplication: TWiRLApplication;
  public
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaderFields: TWiRLHeaderList; AContentStream: TStream); virtual; abstract;
  end;

  TMessageBodyReader = class(TInterfacedObject, IMessageBodyReader)
  protected
    [Context] FRequest: TWiRLRequest;
    [Context] WiRLApplication: TWiRLApplication;
  public
    function ReadFrom(AParam: TRttiParameter; AMediaType: TMediaType;
      AHeaderFields: TWiRLHeaderList; AContentStream: TStream): TValue; virtual; abstract;
  end;

  TMessageBodyProvider = class(TInterfacedObject, IMessageBodyReader, IMessageBodyWriter)
  protected
    [Context] FRequest: TWiRLRequest;
    [Context] WiRLApplication: TWiRLApplication;
  public
    function ReadFrom(AParam: TRttiParameter; AMediaType: TMediaType;
      AHeaderFields: TWiRLHeaderList; AContentStream: TStream): TValue; virtual; abstract;

    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaderFields: TWiRLHeaderList; AContentStream: TStream); virtual; abstract;
  end;

implementation

end.
