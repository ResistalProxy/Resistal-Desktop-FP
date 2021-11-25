program Resistal;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  frm_main,
  frm_about,
  frm_contact,
  frm_help,
  tor,
  sha256,
  internet,
  useRoutingTable,
  readFileInfo,
  ProjectFilesManager,
  UniqueInstanceRaw,
  indylaz, FrameViewer09;

{$R *.res}

begin
  if not InstanceRunning() then
  begin
    RequireDerivedFormResource := True;
    Application.Scaled := True;
    Application.Initialize;
    Application.CreateForm(TFrmMain, FrmMain);
    Application.CreateForm(TFrmAbout, FrmAbout);
    Application.CreateForm(TFrmContact, FrmContact);
    Application.CreateForm(TFrmHelp, FrmHelp);
    Application.Run;
  end;
end.
