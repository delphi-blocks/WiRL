unit Server.Entities;

interface

uses
  System.SysUtils;

type
  TMyEnum = (First, Second);

  TCustomEnum = (None, Left, Right);

  TRecordParam = record
    Name: string;
    City: string;
    Age: Integer;
    Enum: TMyEnum;
  end;

  TArrayParam = TArray<TRecordParam>;
  TArrayInt = TArray<Integer>;


implementation

end.
