{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Data.Utils;

interface

uses
  System.Classes, System.SysUtils, Data.DB;

type
  TDataUtils = class
  private
    class function RecordToXML(const ADataSet: TDataSet; const ARootPath: string = ''): string; static;
    class function RecordToCSV(const ADataSet: TDataSet): string; static;
  public
    class function DataSetToXML(const ADataSet: TDataSet): string; overload; static;
    class function DataSetToXML(const ADataSet: TDataSet; const AAcceptFunc: TFunc<Boolean>): string; overload; static;

    class function DataSetToCSV(const ADataSet: TDataSet): string; static;
  end;


implementation

uses
  System.Rtti, System.StrUtils, System.DateUtils,

  WiRL.Rtti.Utils,
  WiRL.Core.Utils;

type
  TJSONFieldType = (NestedObject, NestedArray, SimpleValue);

class function TDataUtils.RecordToCSV(const ADataSet: TDataSet): string;
var
  LField: TField;
begin
  Result := '';
  for LField in ADataSet.Fields do
  begin
    { TODO -opaolo -c : Check field types 02/09/2021 15:33:33 }
    Result := Result + LField.AsString + ',';
  end;
  Result := Result.TrimRight([',']);
end;

class function TDataUtils.RecordToXML(const ADataSet: TDataSet; const ARootPath: string = ''): string;
var
  LField: TField;
begin
  Result := '';
  for LField in ADataSet.Fields do
  begin
    { TODO -opaolo -c : Check field types 02/09/2021 15:33:33 }
    Result := Result
      + Format('<%s>%s</%s>', [LField.FieldName, LField.AsString, LField.FieldName]);
  end;
end;

class function TDataUtils.DataSetToCSV(const ADataSet: TDataSet): string;
var
  LBookmark: TBookmark;
begin
  Result := '';
  if not Assigned(ADataSet) then
    Exit;

  if not ADataSet.Active then
    ADataSet.Open;

  ADataSet.DisableControls;
  try
    LBookmark := ADataSet.Bookmark;
    try
      ADataSet.First;
      while not ADataSet.Eof do
      try
        Result := Result + TDataUtils.RecordToCSV(ADataSet) + sLineBreak;
      finally
        ADataSet.Next;
      end;
    finally
      ADataSet.GotoBookmark(LBookmark);
    end;
  finally
    ADataSet.EnableControls;
  end;
end;

class function TDataUtils.DataSetToXML(const ADataSet: TDataSet): string;
begin
  Result := DataSetToXML(ADataSet, nil);
end;

class function TDataUtils.DataSetToXML(const ADataSet: TDataSet; const AAcceptFunc: TFunc<Boolean>): string;
var
  LBookmark: TBookmark;
begin
  Result := '';
  if not Assigned(ADataSet) then
    Exit;

  if not ADataSet.Active then
    ADataSet.Open;

  ADataSet.DisableControls;
  try
    LBookmark := ADataSet.Bookmark;
    try
      ADataSet.First;
      while not ADataSet.Eof do
      try
        if (not Assigned(AAcceptFunc)) or (AAcceptFunc()) then
          Result := Result + '<row>' + RecordToXML(ADataSet) + '</row>';
      finally
        ADataSet.Next;
      end;
    finally
      ADataSet.GotoBookmark(LBookmark);
    end;
  finally
    ADataSet.EnableControls;
  end;
end;

end.
