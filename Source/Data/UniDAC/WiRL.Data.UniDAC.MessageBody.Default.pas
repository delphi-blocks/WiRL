{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2024 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Data.UniDAC.MessageBody.Default;

interface

uses
  System.Classes, System.SysUtils, System.Rtti, Data.DB,

  WiRL.Core.Attributes,
  WiRL.Core.Declarations,
  WiRL.http.Accept.MediaType,
  WiRL.http.Headers,
  WiRL.Core.MessageBody.Classes,
  WiRL.Core.MessageBodyReader,
  WiRL.Core.MessageBodyWriter;

type
  /// <summary>
  ///   This is the standard provider (MessageBodyReader/Writer) for the
  ///   TMemDataSet class, the base class of the UniDAC (ODAC, MSDAC,
  ///   PgDAC, MyDAC, etc...) datasets
  /// </summary>
  /// <remarks>
  ///   This Provider supports XML data format
  /// </remarks>
  [Consumes(TMediaType.APPLICATION_XML), Produces(TMediaType.APPLICATION_XML)]
  TWiRLMemDataSetProvider = class(TMessageBodyProvider)
  public
    function ReadFrom(AType: TRttitype; AMediaType: TMediaType;
      AHeaders: IWiRLHeaders; AContentStream: TStream): TValue; overload; override;

    procedure ReadFrom(AObject: TObject; AType: TRttitype; AMediaType: TMediaType;
	    AHeaders: IWiRLHeaders; AContentStream: TStream); overload; override;

    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaders: IWiRLHeaders; AContentStream: TStream); override;
  end;

  /// <summary>
  ///   This is the standard provider (MessageBodyReader/Writer) for the
  ///   TVirtualTable class, the memory dataset in the UniDAC (ODAC, MSDAC,
  ///   PgDAC, MyDAC, etc...) framework.
  /// </summary>
  /// <remarks>
  ///   This Provider supports binary format
  /// </remarks>
  [Consumes(TMediaType.APPLICATION_OCTET_STREAM), Produces(TMediaType.APPLICATION_OCTET_STREAM)]
  TWiRLVirtualTableProvider = class(TMessageBodyProvider)
  public
    function ReadFrom(AType: TRttitype; AMediaType: TMediaType;
      AHeaders: IWiRLHeaders; AContentStream: TStream): TValue; overload; override;

    procedure ReadFrom(AObject: TObject; AType: TRttitype; AMediaType: TMediaType;
	    AHeaders: IWiRLHeaders; AContentStream: TStream); overload; override;

    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaders: IWiRLHeaders; AContentStream: TStream); override;
  end;

implementation

uses
  VirtualTable, MemDS,
  WiRL.Core.Exceptions;

function TWiRLMemDataSetProvider.ReadFrom(AType: TRttitype; AMediaType: TMediaType;
      AHeaders: IWiRLHeaders; AContentStream: TStream): TValue;
var
  LDataSet: TVirtualTable;
begin
  LDataSet := TVirtualTable.Create(nil);
  try
    LDataSet.LoadFromStream(AContentStream);
    Result := TValue.From<TVirtualTable>(LDataSet);
  except
    LDataSet.Free;
  end;
end;

procedure TWiRLMemDataSetProvider.ReadFrom(AObject: TObject; AType: TRttitype;
      AMediaType: TMediaType; AHeaders: IWiRLHeaders; AContentStream: TStream);
var
  LDataSet: TVirtualTable;
begin
  if not (AObject is TVirtualTable) then
    raise EWiRLWebApplicationException.Create('Deserialization only with a TVirtualTable', 501);

  LDataSet := AObject as TVirtualTable;
  AContentStream.Position := 0;
  LDataSet.LoadFromStream(AContentStream);
end;

procedure TWiRLMemDataSetProvider.WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaders: IWiRLHeaders; AContentStream: TStream);
var
  LDataSet: TMemDataSet;
begin
  LDataSet := AValue.AsType<TMemDataSet>;
  LDataSet.Active := True;
  LDataSet.SaveToXML(AContentStream);
end;

{ TWiRLVirtualTableProvider }

function TWiRLVirtualTableProvider.ReadFrom(AType: TRttitype;
  AMediaType: TMediaType; AHeaders: IWiRLHeaders;
  AContentStream: TStream): TValue;
var
  LDataSet: TVirtualTable;
begin
  LDataSet := TVirtualTable.Create(nil);
  try
    LDataSet.LoadFromStream(AContentStream);
    Result := TValue.From<TVirtualTable>(LDataSet);
  except
    LDataSet.Free;
  end;
end;

procedure TWiRLVirtualTableProvider.ReadFrom(AObject: TObject; AType: TRttitype;
  AMediaType: TMediaType; AHeaders: IWiRLHeaders; AContentStream: TStream);
var
  LDataSet: TVirtualTable;
begin
  if not (AObject is TVirtualTable) then
    raise EWiRLWebApplicationException.Create('Deserialization only with a TVirtualTable', 501);

  LDataSet := AObject as TVirtualTable;
  AContentStream.Position := 0;
  LDataSet.LoadFromStream(AContentStream);
end;

procedure TWiRLVirtualTableProvider.WriteTo(const AValue: TValue;
  const AAttributes: TAttributeArray; AMediaType: TMediaType;
  AHeaders: IWiRLHeaders; AContentStream: TStream);
var
  LDataSet: TVirtualTable;
begin
  LDataSet := AValue.AsType<TVirtualTable>;
  LDataSet.Active := True;
  LDataSet.SaveToStream(AContentStream, True, True);
end;

initialization
  // MBR/W for TMemDataSet
  TMessageBodyReaderRegistry.Instance.RegisterReader<TMemDataSet>(TWiRLMemDataSetProvider);
  TMessageBodyWriterRegistry.Instance.RegisterWriter<TMemDataSet>(TWiRLMemDataSetProvider);

  // MBR/W for TVirtualTable
  TMessageBodyReaderRegistry.Instance.RegisterReader<TVirtualTable>(TWiRLVirtualTableProvider);
  TMessageBodyWriterRegistry.Instance.RegisterWriter<TVirtualTable>(TWiRLVirtualTableProvider);

end.
