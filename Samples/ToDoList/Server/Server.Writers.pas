{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Writers;

interface

uses
  System.Classes, System.SysUtils, System.Rtti,

  WiRL.Core.Declarations,
  WiRL.Core.Classes,
  WiRL.Core.Response,
  WiRL.Core.Attributes,
  WiRL.Core.MessageBodyWriter,
  WiRL.http.Accept.MediaType;

type
  [Produces(TMediaType.APPLICATION_JSON)]
  TToDoItemWriter = class(TNonInterfacedObject, IMessageBodyWriter)
  public
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponse: TWiRLResponse);
  end;

  [Produces(TMediaType.APPLICATION_XML)]
  TToDoItemWriterXML = class(TNonInterfacedObject, IMessageBodyWriter)
  public
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponse: TWiRLResponse);
  end;


  [Produces(TMediaType.APPLICATION_JSON)]
  TArrayToDoItemWriter = class(TNonInterfacedObject, IMessageBodyWriter)
  public
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponse: TWiRLResponse);
  end;

  [Produces(TMediaType.APPLICATION_XML)]
  TArrayToDoItemWriterXML = class(TNonInterfacedObject, IMessageBodyWriter)
  public
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponse: TWiRLResponse);
  end;


implementation

uses
  WiRL.Rtti.Utils,
  WiRL.Core.JSON,
  Model;

{ TToDoItemWriter }

procedure TToDoItemWriter.WriteTo(const AValue: TValue; const AAttributes:
    TAttributeArray; AMediaType: TMediaType; AResponse: TWiRLResponse);
var
  LWriter: TStreamWriter;
  LItem: TToDoItem;
begin
  LWriter := TStreamWriter.Create(AResponse.ContentStream);
  try
    LItem := AValue.AsType<TToDoItem>;
    LWriter.Write(LItem.ToJson.ToJSON);
  finally
    LWriter.Free;
  end;
end;

{ TArrayToDoItemWriter }

procedure TArrayToDoItemWriter.WriteTo(const AValue: TValue; const AAttributes:
    TAttributeArray; AMediaType: TMediaType; AResponse: TWiRLResponse);
var
  LArray: TArray<TToDoItem>;
  LItem: TToDoItem;
  LWriter: TStreamWriter;
  LJSONArray: TJSONArray;
begin
  LWriter := TStreamWriter.Create(AResponse.ContentStream);
  try
    LJSONArray := TJSONArray.Create;
    try
      LArray := AValue.AsType<TArray<TToDoItem>>;
      for LItem in LArray do
        LJSONArray.Add(LItem.ToJson);

      LWriter.Write(LJSONArray.ToJSON);
    finally
      LJSONArray.Free;
    end;
  finally
    LWriter.Free;
  end;
end;

{ TToDoItemWriterXML }

procedure TToDoItemWriterXML.WriteTo(const AValue: TValue; const AAttributes:
    TAttributeArray; AMediaType: TMediaType; AResponse: TWiRLResponse);
var
  LWriter: TStreamWriter;
  LItem: TToDoItem;
begin
  LWriter := TStreamWriter.Create(AResponse.ContentStream);
  try
    LItem := AValue.AsType<TToDoItem>;
    LWriter.Write(LItem.ToXML);
  finally
    LWriter.Free;
  end;
end;

{ TArrayToDoItemWriterXML }

procedure TArrayToDoItemWriterXML.WriteTo(const AValue: TValue; const
    AAttributes: TAttributeArray; AMediaType: TMediaType; AResponse:
    TWiRLResponse);
var
  LArray: TArray<TToDoItem>;
  LItem: TToDoItem;
  LWriter: TStreamWriter;
begin
  LWriter := TStreamWriter.Create(AResponse.ContentStream);
  try
    LWriter.Write('<items>');
    LArray := AValue.AsType<TArray<TToDoItem>>;
    for LItem in LArray do
      LWriter.Write(LItem.ToXML);

    LWriter.Write('</items>');
  finally
    LWriter.Free;
  end;
end;

initialization
  TWiRLMessageBodyRegistry.Instance.RegisterWriter<TToDoItem>(TToDoItemWriter);
  TWiRLMessageBodyRegistry.Instance.RegisterWriter<TToDoItem>(TToDoItemWriterXML);

  TWiRLMessageBodyRegistry.Instance.RegisterWriter(
    TArrayToDoItemWriter
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := Assigned(AType) and TRttiHelper.IsDynamicArrayOf<TToDoItem>(AType);
      end
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
      begin
        Result := TWiRLMessageBodyRegistry.AFFINITY_HIGH
      end
  );

  TWiRLMessageBodyRegistry.Instance.RegisterWriter(
    TArrayToDoItemWriterXML
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := Assigned(AType) and TRttiHelper.IsDynamicArrayOf<TToDoItem>(AType);
      end
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
      begin
        Result := TWiRLMessageBodyRegistry.AFFINITY_HIGH
      end
  );


end.
