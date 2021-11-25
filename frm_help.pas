unit frm_help;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLIntf;

type

  { TFrmHelp }

  TFrmHelp = class(TForm)
    BtnOK: TButton;
    Label2: TLabel;
    UrlLblGithub: TLabel;
    procedure BtnOKClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    //procedure FormCreate(Sender: TObject);
    procedure UrlLblGithubClick(Sender: TObject);
  private

  public

  end;

var
  FrmHelp: TFrmHelp;

implementation

{$R *.lfm}

{ TFrmHelp }

//procedure TFrmHelp.FormCreate(Sender: TObject);
//begin
//  HtmlViewer1.LoadFromString(
//    '<p>Please visit <a href="https://github.com/ResistalProxy/resistal">https://github.com/ResistalProxy/resistal</a></p><p>&nbsp;</p>', '');
//end;

procedure TFrmHelp.UrlLblGithubClick(Sender: TObject);
begin
  OpenURL(UrlLblGithub.Caption);
end;

procedure TFrmHelp.BtnOKClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmHelp.FormKeyPress(Sender: TObject; var Key: char);
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

end.
