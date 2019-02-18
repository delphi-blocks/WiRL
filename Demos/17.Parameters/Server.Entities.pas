unit Server.Entities;

interface

uses
  System.SysUtils;

type
  TMyEnum = (First, Second);

  TRecordParam = record
    Name: string;
    City: string;
    Age: Integer;
    Enum: TMyEnum;
  end;

  TArrayParam = TArray<TRecordParam>;


implementation

end.
