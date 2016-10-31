(*
  Copyright 2015-2016, WiRL - REST Library

  Home: https://github.com/WiRL-library

*)
unit Frames.Diagnostics;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFrame1 = class(TFrame)
    LabelAppName: TLabel;
    Label2: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
