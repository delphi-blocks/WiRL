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
  WiRL.http.Headers,
//  WiRL.http.Request,
//  WiRL.http.Response,
  WiRL.Core.Attributes,
//  WiRL.Core.Application,
  WiRL.http.Accept.MediaType,
  WiRL.Core.MessageBodyWriter,
  WiRL.Core.MessageBodyReader;

type
  TMessageBodyWriter = class(TInterfacedObject, IMessageBodyWriter)
  public
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaders: IWiRLHeaders; AContentStream: TStream); virtual; abstract;
  end;

  TMessageBodyReader = class(TInterfacedObject, IMessageBodyReader)
  public
    function ReadFrom(AType: TRttiType; AMediaType: TMediaType;
      AHeaders: IWiRLHeaders; AContentStream: TStream): TValue; overload; virtual; abstract;

    procedure ReadFrom(AObject: TObject; AType: TRttitype; AMediaType: TMediaType;
	    AHeaders: IWiRLHeaders; AContentStream: TStream); overload; virtual;
  end;

  TMessageBodyProvider = class(TInterfacedObject, IMessageBodyReader, IMessageBodyWriter)
  protected
  public
    function ReadFrom(AType: TRttiType; AMediaType: TMediaType;
      AHeaders: IWiRLHeaders; AContentStream: TStream): TValue; overload; virtual; abstract;

    procedure ReadFrom(AObject: TObject; AType: TRttitype; AMediaType: TMediaType;
	    AHeaders: IWiRLHeaders; AContentStream: TStream); overload; virtual;

    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaders: IWiRLHeaders; AContentStream: TStream); virtual; abstract;
  end;

implementation

{ TMessageBodyProvider }

procedure TMessageBodyProvider.ReadFrom(AObject: TObject; AType: TRttitype;
  AMediaType: TMediaType; AHeaders: IWiRLHeaders; AContentStream: TStream);
begin
  raise EWiRLException.Create('ReadFrom on TObject not implemented');
end;

{ TMessageBodyReader }

procedure TMessageBodyReader.ReadFrom(AObject: TObject; AType: TRttitype;
  AMediaType: TMediaType; AHeaders: IWiRLHeaders; AContentStream: TStream);
begin
  raise EWiRLException.Create('ReadFrom on TObject not implemented');
end;

end.
