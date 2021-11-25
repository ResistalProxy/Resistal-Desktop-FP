unit frm_contact;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLIntf;

type

  { TFrmContact }

  TFrmContact = class(TForm)
    BtnOK: TButton;
    Label1: TLabel;
    Label2: TLabel;
    UrlLblGithub: TLabel;
    Label4: TLabel;
    UrlLblTelegram: TLabel;
    UrlLblEmail: TLabel;
    procedure BtnOKClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure UrlLblEmailClick(Sender: TObject);
    procedure UrlLblGithubClick(Sender: TObject);
    procedure UrlLblTelegramClick(Sender: TObject);
  private

  public

  end;

var
  FrmContact: TFrmContact;

implementation

{$R *.lfm}

{ TFrmContact }

procedure TFrmContact.BtnOKClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmContact.FormKeyPress(Sender: TObject; var Key: char);
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

procedure TFrmContact.UrlLblGithubClick(Sender: TObject);
begin
  OpenURL(UrlLblGithub.Caption);
end;

procedure TFrmContact.UrlLblEmailClick(Sender: TObject);
begin
  OpenURL('mailto:' + UrlLblEmail.Caption + '?subject=Resistal Proxy&body=');
end;

procedure TFrmContact.UrlLblTelegramClick(Sender: TObject);
begin
  OpenURL(UrlLblTelegram.Caption);
end;

end.
