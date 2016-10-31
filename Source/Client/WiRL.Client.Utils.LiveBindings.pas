(*
  Copyright 2015-2016, WiRL - REST Library

  Home: https://github.com/WiRL-library

*)
unit WiRL.Client.Utils.LiveBindings;

interface

uses
  System.Classes, System.SysUtils

  , Data.Bind.DBScope
  , Data.Bind.Components

  ;

function BindListControl(const ABindSourceDB: TBindSourceDB; const AControl: TComponent;
  const AAfterCreateProc: TProc<TLinkListControlToField> = nil): TLinkListControlToField;

implementation

function BindListControl(const ABindSourceDB: TBindSourceDB; const AControl: TComponent;
  const AAfterCreateProc: TProc<TLinkListControlToField> = nil): TLinkListControlToField;
begin
  Result := TLinkListControlToField.Create(nil);
  Result.Category := 'Runtime Bindings';
  Result.DataSource := ABindSourceDB;
  Result.Control := AControl;
  Result.Active := True;
  if Assigned(AAfterCreateProc) then
    AAfterCreateProc(Result);
end;


end.
