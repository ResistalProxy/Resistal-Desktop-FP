unit frm_about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  HtmlView;

type

  { TFrmAbout }

  TFrmAbout = class(TForm)
    BtnOK: TButton;
    HtmlViewer1: THtmlViewer;
    procedure BtnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
  private

  public

  end;

var
  FrmAbout: TFrmAbout;

implementation

{$R *.lfm}

{ TFrmAbout }

procedure TFrmAbout.FormCreate(Sender: TObject);
begin
  //HtmlViewer1.LoadFromString('<h1>Resistal</h1><p>Resistal is an Vidalia alternative that supports devices running the Windows. Resistal protects network traffic using The Onion Router (Tor) network. Tor encrypts the data and sends it through random points across the world to hide where the connection started. For example, while using Resistal, a website you visit might think you''re looking at it from another country.</p>','');
  HtmlViewer1.LoadFromString(
    '<h2 style="text-align: center;">Resistal</h2><p style="text-align: justify;">Resistal is abbreviation of Digital Resistance. Resistal is a Vidalia alternative that supports devices running the Windows. Resistal protects network traffic using The Onion Router (TOR) network. Tor encrypts the data and sends it through random points across the world to hide where the connection started. For example, while using Resistal, a website you visit might think you''re looking at it from another country.</p>', '');
end;

procedure TFrmAbout.FormKeyPress(Sender: TObject; var Key: char);
begin
  {
  Backspace  8
  Tab  9
  Enter  13
  Shift  16
  Control  17
  Alt  18
  Caps Lock  20
  Escape  27
  Space  32
  Page up  33
  Page down  34
  End  35
  Home  36
  Left  37
  Up  38
  Right  39
  Down  40
  Delete  46
  }
  if Key = #27 then
    Close;
end;

procedure TFrmAbout.BtnOKClick(Sender: TObject);
begin
  Close;
end;

end.
