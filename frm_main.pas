unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Menus, process, strutils, LCLProc, Zipper,
  IdHTTP, IdSSLOpenSSL, tor, internet, sha256, useRoutingTable, readFileInfo,
  frm_about, frm_contact, frm_help, Windows, WinInet, ProjectFilesManager,
  JPP.Panel, IdComponent, IdIOHandlerStack, RegExpr, LCLIntf, Win32Proc
  {$IFDEF Windows}
  , registry
  {$ENDIF}
  {$IFDEF UNIX}
  {$ENDIF}
  , IniFiles, fpjson, jsonparser, telnetsshclient;

const
  BUF_SIZE = 2048; // Buffer size for reading the output in chunks

var
  AppVersion: string;
  AppPath: string;

  TorProcess: TProcess;
  PrivoxyProcess: TProcess;
  //Aria2cProcess: TProcess;

  OutputStream: TMemoryStream;
  OutputStringStream: TStringStream;
  BytesRead: longint;
  Buffer: array[1..BUF_SIZE] of byte;

  //RunProgram: TProcess;
  UpdateChecked: boolean = False;
  StopThreadInternet: boolean = False;

  DownloadUrl: string;

  IsOpenBrowser: boolean = False;
  IsFirstRun: boolean = True;
  IsUpdateBridge: boolean = False;
  IsUpdateApplication: boolean = False;

type
  { ThreadTor }
  TThreadTor = class(TThread)
  private
    function MemoryStreamToString(M: TMemoryStream): ansistring;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: boolean);
  end;

  { ThreadRestartTor }
  TThreadRestartTor = class(TThread)
  private
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: boolean);
  end;

  { ThreadInternet }
  TThreadInternet = class(TThread)
  private
    procedure CheckUpdate;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: boolean);
  end;

  { ThreadCheckUpdate }
  TThreadCheckUpdate = class(TThread)
  private
    procedure UpdateBridge;
    procedure UpdateApplication;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: boolean);
  end;

  { ThreadTelnetBridge }
  TThreadTelnetBridge = class(TThread)
  private
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: boolean);
  end;


  { TFrmMain }

  TFrmMain = class(TForm)
    BtnConnect: TButton;
    BtnSetSocksTelegram: TButton;
    BtnSetSystemProxy: TButton;
    ChkBxUseBridges: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    TxtTorVersion: TEdit;
    Edit8: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    IdHttpCheckUpdate: TIdHTTP;
    IdIOHandlerStack1: TIdIOHandlerStack;
    IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL;
    JppPanelMain: TJppPanel;
    LblStatus: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MainMenu: TMainMenu;
    MainTray: TTrayIcon;
    N2: TMenuItem;
    MenuItemAutoConnect: TMenuItem;
    MenuOptions: TMenuItem;
    MenuItemStartup: TMenuItem;
    MenuItemAutostart: TMenuItem;
    MenuItemStartMinimized: TMenuItem;
    MenuView: TMenuItem;
    MenuItemHideMain: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuHelp: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemContact: TMenuItem;
    N1: TMenuItem;
    StatusBarMain: TStatusBar;
    TrayMenuItemOpen: TMenuItem;
    MenuItem2: TMenuItem;
    TrayMenuItemExit: TMenuItem;
    TrayPopup: TPopupMenu;
    ProgressBar1: TProgressBar;
    procedure BtnConnectClick(Sender: TObject);
    procedure BtnSetSocksTelegramClick(Sender: TObject);
    procedure BtnSetSystemProxyClick(Sender: TObject);
    procedure ChkBxUseBridgesChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure IdHttpCheckUpdateWork(ASender: TObject; AWorkMode: TWorkMode;
      AWorkCount: int64);
    procedure MainTrayDblClick(Sender: TObject);
    procedure MenuItemAutoConnectClick(Sender: TObject);
    procedure MenuItemAutostartClick(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemContactClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure MenuItemHelpClick(Sender: TObject);
    procedure MenuItemHideMainClick(Sender: TObject);
    procedure MenuItemStartMinimizedClick(Sender: TObject);
    procedure TrayMenuItemOpenClick(Sender: TObject);
    procedure TrayMenuItemExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    ThreadTor: TThreadTor;
    ThreadRestartTor: TThreadRestartTor;
    ThreadInternet: TThreadInternet;
    ThreadCheckUpdate: TThreadCheckUpdate;
    ThreadTelnetBridge: TThreadTelnetBridge;
  public

  end;

var
  FrmMain: TFrmMain;

procedure StartingBrowser;
procedure RestartApp;
procedure Aria2cDownload(DownloadPath: string; URL: string);
function SetProxy(Address: string): boolean;
function RemoveProxy(): boolean;
function ReadRegistry(Key: string; Name: string): string;
procedure WriteRegistry(Key: string; Name: string; Value: string);
function DeleteRegistry(Key: string; Name: string): boolean;
function TelnetConnect(Host: string): boolean;

implementation

{$R *.lfm}

{ TFrmMain }

procedure TFrmMain.FormCreate(Sender: TObject);
var
  Ini: TIniFile;
  strTorVersion: TStringArray;
  TorVersion: string;
begin
  inherited;

  AppVersion := readFileInfo.getAppVersion;
  AppPath := ExtractFilePath(Application.ExeName);

  Ini := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    FrmMain.MenuItemAutoConnect.Checked := Ini.ReadBool('App', 'AutoConnect', False);
  finally
    Ini.Free;
  end;

  Self.Caption := Self.Caption + ' ' + AppVersion;
  MainTray.Hint := Self.Caption;

  LblStatus.Caption := '...';
  BtnSetSystemProxy.Enabled := False;

  ProjectFilesManager.UpdateApp;

  if ProjectFilesManager.CheckFiles then
  begin
    ProjectFilesManager.DeleteBackup;

    RemoveProxy();

    if tor.ConfigureTor and tor.ConfigToral and tor.ConfigTorav then
    begin
      ChkBxUseBridges.Checked := tor.ReadUseBridgesCfg;

      ThreadInternet := TThreadInternet.Create(True);
      ThreadInternet.Start;
    end
    else
    begin
      Application.MessageBox('Tor Config Error.' + sLineBreak +
        'Resistal will be closed.', 'Error', MB_OK or MB_ICONERROR);
      Application.Terminate;
    end;
  end
  else
  begin
    Application.MessageBox('Some File or Directory NOT Found.' +
      sLineBreak + 'Resistal will be closed.', 'Error', MB_OK or MB_ICONERROR);
    Application.Terminate;
  end;


  if ReadRegistry('\SOFTWARE\Microsoft\Windows\CurrentVersion\Run',
    Application.Title) <> '' then
  begin
    FrmMain.MenuItemAutostart.Checked := True;
    FrmMain.MenuItemStartMinimized.Enabled := True;

    FrmMain.MenuItemStartMinimized.Checked :=
      ReadRegistry('\SOFTWARE\Microsoft\Windows\CurrentVersion\Run',
      Application.Title).Contains('-minimized');
  end
  else
  begin
    FrmMain.MenuItemAutostart.Checked := False;
    FrmMain.MenuItemStartMinimized.Enabled := False;
  end;


  if FrmMain.MenuItemAutoConnect.Checked then
    BtnConnect.Click;

  if RunCommand(ProjectFilesManager.TorExePath, ['--version'],
    TorVersion, [poRunIdle], swoHIDE) then
  begin
    strTorVersion := TorVersion.Split(' ');
    if Trim(strTorVersion[2]).Length > 0 then
      TxtTorVersion.Caption := strTorVersion[2]
    else
      TxtTorVersion.Caption := ProjectFilesManager.TorVersion;
  end
  else
    TxtTorVersion.Caption := ProjectFilesManager.TorVersion;

end;

procedure TFrmMain.FormShow(Sender: TObject);
begin
  if Application.HasOption('minimized') and IsFirstRun then
  begin
    Hide;
    WindowState := wsMinimized;
    IsFirstRun := False;
  end;
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  inherited;

  if ThreadInternet <> nil then
    ThreadInternet.Terminate;

  if ThreadCheckUpdate <> nil then
    ThreadCheckUpdate.Terminate;

  if ThreadTelnetBridge <> nil then
    ThreadTelnetBridge.Terminate;

  if ThreadTor <> nil then
    ThreadTor.Terminate;

  if TorProcess <> nil then
  begin
    TorProcess.Terminate(0);
    TorProcess.Free;
  end;

  if PrivoxyProcess <> nil then
  begin
    PrivoxyProcess.Terminate(0);
    PrivoxyProcess.Free;
  end;

  if BtnSetSystemProxy.Caption = 'Disable System Proxy' then
  begin
    RemoveProxy();
  end;
end;


constructor TThreadInternet.Create(CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;

procedure TThreadInternet.CheckUpdate;
begin
  FrmMain.ThreadCheckUpdate := TThreadCheckUpdate.Create(True);
  FrmMain.ThreadCheckUpdate.Start;
end;

procedure TThreadInternet.Execute;
var
  status: string;
  Flags: Windows.DWORD; // flags to pass to API function
begin
  //while not StopThreadInternet do
  while True do
  begin
    Flags := 0;
    status := FrmMain.StatusBarMain.Panels[0].Text;

    if InternetGetConnectedState(@Flags, 0) and
      (not status.Contains('IP') or status.Contains('Error') or
      status.Contains('invalid')) then
    begin
      FrmMain.StatusBarMain.Panels[0].Text := 'IP: ' + internet.GetExternalIPAddress;

      if not UpdateChecked then
      begin
        UpdateChecked := True;
        Synchronize(@CheckUpdate);
      end;
    end
    else if (not InternetGetConnectedState(@Flags, 0)) then
    begin
      FrmMain.StatusBarMain.Panels[0].Text := 'Internet NOT Available.';
    end;

    Sleep(3000);
  end;

end;


constructor TThreadCheckUpdate.Create(CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;

procedure TThreadCheckUpdate.UpdateBridge;
var
  bridge: string = '';
  bridgeArray: TStringArray;
  i, j: integer;
  BridgeRegex: TRegExpr;
  BridgeIp: string;

  slToral: TStringList;
  toralHaveBridge: boolean = False;
  toralBridgeIp: string;
begin
  FrmMain.StatusBarMain.Panels[1].Text := 'Update Bridges...';

  try
    DownloadUrl :=
      'https://resistalproxy.github.io/resistal/bridge.txt';
    bridge := FrmMain.IdHttpCheckUpdate.Get(DownloadUrl);
  except
    FrmMain.StatusBarMain.Panels[1].Text := 'Error in Update Bridges.';
    Sleep(1000);
  end;

  if not bridge.IsEmpty then
  begin
    bridgeArray := bridge.Split(sLineBreak);
    BridgeRegex := TRegExpr.Create('\b\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}\:\d{1,5}\b');

    // Update toral  (all)
    slToral := TStringList.Create;
    slToral.LoadFromFile(AppPath + ProjectFilesManager.toralPath);
    for i := 0 to (Length(bridgeArray) - 1) do
    begin
      toralHaveBridge := False;
      if bridgeArray[i].StartsWith('Bridge obfs4') then
      begin
        if BridgeRegex.Exec(bridgeArray[i]) then
        begin
          BridgeIp := BridgeRegex.Match[0];
        end;

        for j := 0 to slToral.Count - 1 do
        begin
          if slToral[j].StartsWith('Bridge obfs4') then
          begin

            if BridgeRegex.Exec(slToral[j]) then
            begin
              toralBridgeIp := BridgeRegex.Match[0];

              if toralBridgeIp = BridgeIp then
              begin
                toralHaveBridge := True;
                Break;
              end;
            end;
          end;
        end;

        if not toralHaveBridge then
        begin
          slToral.Add(bridgeArray[i]);
        end;
      end;
    end;
    slToral.SaveToFile(AppPath + ProjectFilesManager.toralPath);
    slToral.Free;

    FrmMain.StatusBarMain.Panels[1].Text := 'Bridges Updated.';
    IsUpdateBridge := True;
    Sleep(1000);

    // Run Telnet Thread
    FrmMain.ThreadTelnetBridge := TThreadTelnetBridge.Create(True);
    FrmMain.ThreadTelnetBridge.Start;
  end;
end;

procedure TThreadCheckUpdate.UpdateApplication;
var
  version: string = '';
  AppHash: string = '';
  MS: TMemoryStream;
  UnZipper: TUnZipper;
begin
  // read version.info
  FrmMain.StatusBarMain.Panels[1].Text := 'Checking for updates...';

  try
    DownloadUrl :=
      'https://resistalproxy.github.io/resistal/version.info';
    version := FrmMain.IdHttpCheckUpdate.Get(DownloadUrl);
  except
    FrmMain.StatusBarMain.Panels[1].Text := 'Error in Update Application.';
    Sleep(1000);
  end;

  if not version.IsEmpty then
  begin
    try
      if version <> readFileInfo.getAppVersion then
      begin
        Sleep(1000);
        FrmMain.StatusBarMain.Panels[1].Text := 'New version available.';

        Sleep(1000);
        FrmMain.StatusBarMain.Panels[1].Text := 'Downloading...';
        Sleep(500);

        // read sha256
        try
          DownloadUrl :=
            'https://resistalproxy.github.io/resistal/resistal.sha256';
          AppHash := FrmMain.IdHttpCheckUpdate.Get(DownloadUrl);
        except
          FrmMain.StatusBarMain.Panels[1].Text := 'Error in Update Application.';
          Sleep(1000);
        end;

        if not AppHash.IsEmpty then
        begin
          // download
          ProjectFilesManager.DeleteDownload;
          ProjectFilesManager.DeleteBackup;

          try
            DownloadUrl :=
              'https://resistalproxy.github.io/resistal/ResistalProxy.zip';

            if (WindowsVersion = wv95) or (WindowsVersion = wvNT4) or
              (WindowsVersion = wv98) or (WindowsVersion = wvMe) or
              (WindowsVersion = wv2000) or (WindowsVersion = wvServer2003) or
              (WindowsVersion = wvXP) then
            begin
              // Download the file
              // Save the memorystream to disk
              MS := TMemoryStream.Create;
              FrmMain.IdHttpCheckUpdate.Get(DownloadUrl, MS);
              if not DirectoryExists(AppPath + 'Download') then
              begin
                if CreateDir(AppPath + 'Download') then
                  MS.SaveToFile(AppPath + 'Download\ResistalProxy.zip')
                else
                  FrmMain.StatusBarMain.Panels[1].Text :=
                    'Failed to create Download directory!';
              end
              else
                MS.SaveToFile(AppPath + 'Download\ResistalProxy.zip');

              // Free the memorystream
              MS.Free;
            end
            else
            begin
              // download with aria2c
              if not DirectoryExists(AppPath + 'Download') then
              begin
                if CreateDir(AppPath + 'Download') then
                  Aria2cDownload(AppPath + 'Download', DownloadUrl)
                else
                  FrmMain.StatusBarMain.Panels[1].Text :=
                    'Failed to create Download directory!';
              end
              else
                Aria2cDownload(AppPath + 'Download', DownloadUrl);
            end;
          finally
            // check exist file
            if FileExists(AppPath + 'Download\ResistalProxy.zip') then
            begin
              Sleep(1000);
              // check hash
              if AppHash = sha256.getsha256_file(AppPath +
                'Download\ResistalProxy.zip') then
              begin
                FrmMain.StatusBarMain.Panels[1].Text := 'Download Completed.';

                // extract
                try
                  UnZipper := TUnZipper.Create;
                  try
                    UnZipper.FileName := AppPath + 'Download\ResistalProxy.zip';
                    UnZipper.OutputPath := AppPath + 'Download\';
                    UnZipper.Examine;
                    UnZipper.UnZipAllFiles;
                  finally
                    UnZipper.Free;
                  end;

                  SysUtils.DeleteFile(AppPath + 'Download\ResistalProxy.zip');

                  // install
                  if ProjectFilesManager.UpdateResistal then
                  begin
                    Sleep(500);
                    FrmMain.StatusBarMain.Panels[1].Text := 'Update Completed.';

                    // restart
                    Sleep(1000);
                    FrmMain.StatusBarMain.Panels[1].Text :=
                      'Restart App to Complete Update.';
                    IsUpdateApplication := True;
                  end;
                except
                  on E: Exception do
                    FrmMain.StatusBarMain.Panels[1].Text := E.Message;
                end;

              end
              else
              begin
                FrmMain.StatusBarMain.Panels[1].Text := 'Downloading with Error.';
              end;
            end
            else
              FrmMain.StatusBarMain.Panels[1].Text := 'Download NOT Complete.';
          end;
        end;
      end
      else
      begin
        Sleep(1000);
        FrmMain.StatusBarMain.Panels[1].Text := 'You have the latest version.';
        IsUpdateApplication := True;
      end;
    except
      FrmMain.StatusBarMain.Panels[1].Text := 'Error in Update Application.';
      Sleep(1000);
    end;
  end;
end;

procedure TThreadCheckUpdate.Execute;
begin
  try
    { Update Bridge }
    UpdateBridge;

    { Update App }
    UpdateApplication;

  except
  end;
end;


constructor TThreadTelnetBridge.Create(CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;

procedure TThreadTelnetBridge.Execute;
var
  BridgeRegex: TRegExpr;
  i, j: integer;

  slTorav: TStringList;
  toravHaveBridge: boolean = False;
  toravBridgeIp: string;

  slToral: TStringList;
  toralBridgeIp: string;

begin
  BridgeRegex := TRegExpr.Create('\b\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}\:\d{1,5}\b');

  // Telnet torav
  slTorav := TStringList.Create;
  slTorav.LoadFromFile(AppPath + ProjectFilesManager.toravPath);
  if slTorav.Count > 0 then
  begin
    for i := slTorav.Count - 1 downto 0 do
    begin
      if slTorav[i].StartsWith('Bridge obfs4') then
      begin
        if BridgeRegex.Exec(slTorav[i]) then
        begin
          toravBridgeIp := BridgeRegex.Match[0];

          if not TelnetConnect(toravBridgeIp) then
          begin
            // Delete Bridge
            slTorav.Delete(i);
          end;
        end;
      end;
    end;

    // Telnet toral
    slToral := TStringList.Create;
    slToral.LoadFromFile(AppPath + ProjectFilesManager.toralPath);
    if slToral.Count > 0 then
    begin
      for i := 0 to slToral.Count - 1 do
      begin
        toravHaveBridge := False;
        if slToral[i].StartsWith('Bridge obfs4') then
        begin
          if BridgeRegex.Exec(slToral[i]) then
          begin
            toralBridgeIp := BridgeRegex.Match[0];

            for j := 0 to slTorav.Count - 1 do
            begin
              if slTorav[j].StartsWith('Bridge obfs4') then
              begin
                if BridgeRegex.Exec(slTorav[j]) then
                begin
                  toravBridgeIp := BridgeRegex.Match[0];

                  if toravBridgeIp = toralBridgeIp then
                  begin
                    toravHaveBridge := True;
                    Break;
                  end;
                end;
              end;
            end;

            if not toravHaveBridge then
            begin
              if TelnetConnect(toralBridgeIp) then
              begin
                slTorav.Add(slToral[i]);
              end;
            end;

          end;
        end;
      end;
    end
    else
    begin
      //ShowMessage('toral is empty!');
    end;

    slToral.Free;

  end
  else
  begin
    // Telnet toral
    slToral := TStringList.Create;
    slToral.LoadFromFile(AppPath + ProjectFilesManager.toralPath);
    if slToral.Count > 0 then
    begin
      for i := 0 to slToral.Count - 1 do
      begin
        toravHaveBridge := False;
        if slToral[i].StartsWith('Bridge obfs4') then
        begin
          if BridgeRegex.Exec(slToral[i]) then
          begin
            toralBridgeIp := BridgeRegex.Match[0];
            if TelnetConnect(toralBridgeIp) then
            begin

              for j := 0 to slTorav.Count - 1 do
              begin
                if slTorav[j].StartsWith('Bridge obfs4') then
                begin
                  if BridgeRegex.Exec(slTorav[j]) then
                  begin
                    toravBridgeIp := BridgeRegex.Match[0];

                    if toravBridgeIp = toralBridgeIp then
                    begin
                      toravHaveBridge := True;
                      Break;
                    end;
                  end;
                end;
              end;

              if not toravHaveBridge then
              begin
                slTorav.Add(slToral[i]);
              end;

            end;
          end;
        end;
      end;
    end
    else
    begin
      //ShowMessage('toral is empty!');
    end;

    slToral.Free;

  end;

  slTorav.SaveToFile(AppPath + ProjectFilesManager.toravPath);
  slTorav.Free;

  FrmMain.StatusBarMain.Panels[1].Text := 'Checked All Bridges.';
end;


constructor TThreadTor.Create(CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;

procedure TThreadTor.Execute;
var
  TorOutput: string;
  StrArray: TStringArray;
  index: integer;
  posStrArray: integer;
  strBootstrapped: string;
  strPct: TStringArray;
  intPct: integer;
  strMsg: string;
  strWarn: string;
  strBridge: string;
begin
  try
    SetCurrentDir(AppPath);
    TorProcess := TProcess.Create(nil);
    TorProcess.Executable := ProjectFilesManager.TorExePath;
    TorProcess.Parameters.Add('-f');
    TorProcess.Parameters.Add(AppPath + ProjectFilesManager.torrcPath);
    TorProcess.Options := [poUsePipes];
    TorProcess.ShowWindow := swoHIDE;

    // Start the process (run the dir/ls command)
    TorProcess.Execute;

    // Create a stream object to store the generated output in. This could
    // also be a file stream to directly save the output to disk.
    OutputStream := TMemoryStream.Create;

    while TorProcess.Running and (not Terminated) do
    begin
      // Get the new data from the process to a maximum of the buffer size that was allocated.
      // Note that all read(...) calls will block except for the last one, which returns 0 (zero).
      BytesRead := TorProcess.Output.Read(Buffer, BUF_SIZE);

      // Add the bytes that were read to the stream for later usage
      OutputStream.Write(Buffer, BytesRead);

      TorOutput := MemoryStreamToString(OutputStream).Trim;
      OutputStream.Clear;

      StrArray := TorOutput.Split(sLineBreak);

      for index := 0 to (Length(StrArray) - 1) do
      begin
        if AnsiContainsStr(StrArray[index], '[notice]') then
        begin
          if AnsiContainsStr(StrArray[index], ' running') then
          begin
            FrmMain.LblStatus.Caption := 'Running';
          end;

          if AnsiContainsStr(StrArray[index], 'Read configuration file') then
          begin
            FrmMain.LblStatus.Caption := 'Read configuration file';
          end;

          if AnsiContainsStr(StrArray[index], 'Opening Socks listener on ') then
          begin
            FrmMain.LblStatus.Caption := 'Opening Socks listener';
          end;

          if AnsiContainsStr(StrArray[index], 'Opened Socks listener on ') then
          begin
            FrmMain.LblStatus.Caption := 'Opened Socks listener';
          end;

          if AnsiContainsStr(StrArray[index], 'Bootstrapped') then
          begin
            posStrArray := Pos('Bootstrapped', StrArray[index]);
            if posStrArray <> 0 then
            begin
              strBootstrapped := StrArray[index].Substring(posStrArray - 1);

              posStrArray := Pos(':', strBootstrapped);
              strMsg := strBootstrapped.Substring(posStrArray + 1);
              FrmMain.LblStatus.Caption := strMsg;

              strPct := strBootstrapped.Split(' ');
              intPct := StrToInt(ReplaceStr(strPct[1], '%', ''));
              FrmMain.ProgressBar1.Position := intPct;

              if intPct >= 1 then
              begin
                FrmMain.ChkBxUseBridges.Enabled := True;
              end;

              if intPct = 100 then
              begin
                FrmMain.BtnConnect.Caption := 'Disconnect';
                FrmMain.BtnConnect.Enabled := True;

                FrmMain.BtnSetSystemProxy.Enabled := True;
                FrmMain.ChkBxUseBridges.Enabled := True;

                if TorProcess.Running then
                begin
                  // Run Privoxy
                  SetCurrentDir(AppPath + ProjectFilesManager.PrivoxyPath);
                  PrivoxyProcess := TProcess.Create(nil);
                  PrivoxyProcess.Executable := 'privoxy.exe';
                  PrivoxyProcess.Parameters.Add('config.txt');
                  PrivoxyProcess.Options := [poUsePipes];
                  PrivoxyProcess.ShowWindow := swoHIDE;

                  if (PrivoxyProcess <> nil) and (not PrivoxyProcess.Running) then
                  begin
                    PrivoxyProcess.Execute;

                    StartingBrowser;

                    if not UpdateChecked then
                    begin
                      UpdateChecked := True;
                      FrmMain.ThreadInternet.CheckUpdate;
                    end;

                    if (not IsUpdateBridge) or (not IsUpdateApplication) then
                    begin
                      // Create Torified URL
                    end;

                  end;

                end;
              end;
            end;
          end;
        end
        else if AnsiContainsStr(StrArray[index], '[warn]') then
        begin
          if AnsiContainsStr(StrArray[index], 'Proxy Client: unable to connect to') and
            AnsiContainsStr(StrArray[index], '("general SOCKS server failure")') and
            useRoutingTable.IsInternetAvailable() then
          begin
            posStrArray := Pos('Proxy Client: unable to connect to', StrArray[index]);
            if posStrArray <> 0 then
            begin
              strWarn := StrArray[index].Substring(posStrArray - 1);

              strWarn := ReplaceStr(strWarn, 'Proxy Client: unable to connect to ', '');
              strBridge := ReplaceStr(strWarn, ' ("general SOCKS server failure")', '');

              if (FrmMain.ProgressBar1.Position = 100) and
                useRoutingTable.IsInternetAvailable() then
              begin

                // Delete Bridge in torrc
                tor.DeleteBridge(AppPath, strBridge);
              end
              else
              begin
                tor.DeleteBridge(AppPath, strBridge);

                // restart tor
                FrmMain.ThreadRestartTor := TThreadRestartTor.Create(True);
                FrmMain.ThreadRestartTor.Start;
              end;
            end;
          end;
        end;
      end;
    end;

  except

  end;
end;

function TThreadTor.MemoryStreamToString(M: TMemoryStream): ansistring;
begin
  SetString(Result, PAnsiChar(M.Memory), M.Size);
end;


constructor TThreadRestartTor.Create(CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;

procedure TThreadRestartTor.Execute;
begin
  FrmMain.ThreadTor.Terminate;

  if TorProcess <> nil then
  begin
    TorProcess.Terminate(0);
    TorProcess.Free;
  end;

  if PrivoxyProcess <> nil then
  begin
    PrivoxyProcess.Terminate(0);
    PrivoxyProcess.Free;
  end;

  FrmMain.LblStatus.Caption := 'Restart Tor...';
  FrmMain.ProgressBar1.Position := 0;

  Sleep(1000);

  if tor.GetTorrcBridgeCount = 0 then
  begin
    tor.ConfigureTor;
    FrmMain.ThreadCheckUpdate.UpdateBridge;
  end;

  FrmMain.ThreadTor := TThreadTor.Create(True);
  FrmMain.ThreadTor.Start;
end;

procedure Aria2cDownload(DownloadPath: string; URL: string);
var
  Aria2cProcess: TProcess;
  CharBuffer: array [0..2048] of char;
  ReadCount: integer;
  wout: string;
  percent: string = '';
begin
  try
    Aria2cProcess := TProcess.Create(nil);
    SetCurrentDir(AppPath);
    Aria2cProcess.Executable := ProjectFilesManager.Aria2cPath;

    Aria2cProcess.Parameters.Add('--dir="' + DownloadPath + '"');
    Aria2cProcess.Parameters.Add('-x8');
    Aria2cProcess.Parameters.Add('-k1M');
    Aria2cProcess.Parameters.Add('-s4');
    Aria2cProcess.Parameters.Add('--check-certificate=false');
    Aria2cProcess.Parameters.Add('--auto-file-renaming=false');
    Aria2cProcess.Parameters.Add('--allow-overwrite=true');
    Aria2cProcess.Parameters.Add('--file-allocation=none');
    Aria2cProcess.Parameters.Add('--download-result=full');
    Aria2cProcess.Parameters.Add(URL);

    Aria2cProcess.Options := [poUsePipes];
    Aria2cProcess.ShowWindow := swoHIDE;
    Aria2cProcess.Execute;

    while Aria2cProcess.Running do
    begin
      ReadCount := Min(2048, Aria2cProcess.Output.NumBytesAvailable);
      Aria2cProcess.Output.Read(CharBuffer, ReadCount);
      wout := Copy(CharBuffer, 0, ReadCount);

      if Pos('%) ', wout) > 0 then
      begin
        percent := Copy(wout, Pos('B(', wout) + 2, length(wout));
        percent := Copy(percent, 0, Pos('%)', percent));
      end;

      if (Pos('(OK):download completed.', wout) > 0) then
      begin
        percent := '100%';
      end;

      FrmMain.StatusBarMain.Panels[1].Text := 'Downloading ' + percent;
    end;

  except

  end;

  if Aria2cProcess <> nil then
  begin
    Aria2cProcess.Terminate(0);
    Aria2cProcess.Free;
  end;

end;

procedure TFrmMain.FormWindowStateChange(Sender: TObject);
begin
  if WindowState = wsMinimized then
    Hide;
end;

procedure TFrmMain.IdHttpCheckUpdateWork(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCount: int64);
var
  ContentLength: int64;
begin
  ContentLength := IdHttpCheckUpdate.Response.ContentLength;

  if AnsiContainsStr(DownloadUrl, 'ResistalProxy.zip') then
  begin
    if (Pos('chunked', LowerCase(IdHttpCheckUpdate.Response.ContentEncoding)) = 0) and
      (ContentLength > 0) then
    begin
      FrmMain.StatusBarMain.Panels[1].Text :=
        'Downloading ' + IntToStr(100 * AWorkCount div ContentLength) + '%';
    end;
  end;
end;

procedure TFrmMain.BtnConnectClick(Sender: TObject);
begin
  if BtnConnect.Caption = 'Connect' then
  begin
    if tor.GetTorrcBridgeCount = 0 then
    begin
      tor.ConfigureTor;
      ThreadCheckUpdate := TThreadCheckUpdate.Create(True);
      ThreadCheckUpdate.Start;
    end;

    ThreadTor := TThreadTor.Create(True);
    ThreadTor.Start;
    BtnConnect.Enabled := False;
    LblStatus.Caption := 'Initializing';
    ChkBxUseBridges.Enabled := False;
  end
  else if BtnConnect.Caption = 'Disconnect' then
  begin
    ThreadTor.Terminate;

    if TorProcess <> nil then
    begin
      TorProcess.Terminate(0);
      TorProcess.Free;
    end;

    if PrivoxyProcess <> nil then
    begin
      PrivoxyProcess.Terminate(0);
      PrivoxyProcess.Free;
    end;

    BtnConnect.Caption := 'Connect';
    LblStatus.Caption := '...';
    ProgressBar1.Position := 0;

    if BtnSetSystemProxy.Caption = 'Disable System Proxy' then
    begin
      RemoveProxy();
      BtnSetSystemProxy.Caption := 'Enable System Proxy';
    end;
    BtnSetSystemProxy.Enabled := False;

  end;
end;

procedure TFrmMain.BtnSetSocksTelegramClick(Sender: TObject);
begin
  OpenURL('tg://socks?server=127.0.0.1&port=9050');
end;

procedure TFrmMain.BtnSetSystemProxyClick(Sender: TObject);
var
  bReturn: boolean;
begin
  if BtnSetSystemProxy.Caption = 'Enable System Proxy' then
  begin
    bReturn := SetProxy('127.0.0.1:8118');

    if bReturn then
      BtnSetSystemProxy.Caption := 'Disable System Proxy';
  end
  else if BtnSetSystemProxy.Caption = 'Disable System Proxy' then
  begin
    bReturn := RemoveProxy();

    if bReturn then
      BtnSetSystemProxy.Caption := 'Enable System Proxy';
  end;
end;

procedure TFrmMain.ChkBxUseBridgesChange(Sender: TObject);
begin
  tor.UseBridges(ChkBxUseBridges.Checked);
end;

procedure StartingBrowser;
begin
  if not IsOpenBrowser then
  begin
    OpenURL('https://resistalproxy.github.io/');
    IsOpenBrowser := True;
  end;
end;

procedure TFrmMain.MainTrayDblClick(Sender: TObject);
begin
  if WindowState = wsMinimized then
  begin
    WindowState := wsNormal;
    Show;
  end;
end;

procedure TFrmMain.MenuItemAutoConnectClick(Sender: TObject);
var
  AutoConnect: boolean;
  Ini: TIniFile;
begin
  AutoConnect := not FrmMain.MenuItemAutoConnect.Checked;
  FrmMain.MenuItemAutoConnect.Checked := AutoConnect;

  Ini := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    Ini.WriteBool('App', 'AutoConnect', AutoConnect);
  finally
    Ini.Free;
  end;

end;

procedure TFrmMain.MenuItemAutostartClick(Sender: TObject);
var
  Autostart: boolean;
begin
  Autostart := not FrmMain.MenuItemAutostart.Checked;
  FrmMain.MenuItemAutostart.Checked := Autostart;

  if Autostart then
  begin
    WriteRegistry('\SOFTWARE\Microsoft\Windows\CurrentVersion\Run',
      Application.Title,
      Application.ExeName);

    FrmMain.MenuItemStartMinimized.Enabled := True;
  end
  else
  begin
    FrmMain.MenuItemStartMinimized.Checked := False;
    FrmMain.MenuItemStartMinimized.Enabled := False;
    DeleteRegistry('\SOFTWARE\Microsoft\Windows\CurrentVersion\Run', Application.Title);
  end;

end;

procedure TFrmMain.MenuItemStartMinimizedClick(Sender: TObject);
var
  StartMinimized: boolean;
begin
  StartMinimized := not FrmMain.MenuItemStartMinimized.Checked;

  if StartMinimized and FrmMain.MenuItemAutostart.Checked then
  begin
    FrmMain.MenuItemStartMinimized.Checked := StartMinimized;

    WriteRegistry('\SOFTWARE\Microsoft\Windows\CurrentVersion\Run',
      Application.Title,
      Application.ExeName + ' -minimized');
  end
  else if not StartMinimized then
  begin
    FrmMain.MenuItemStartMinimized.Checked := StartMinimized;

    WriteRegistry('\SOFTWARE\Microsoft\Windows\CurrentVersion\Run',
      Application.Title,
      Application.ExeName);
  end;

end;

procedure TFrmMain.MenuItemAboutClick(Sender: TObject);
begin
  FrmAbout.ShowModal;
end;

procedure TFrmMain.MenuItemContactClick(Sender: TObject);
begin
  FrmContact.ShowModal;
end;

procedure TFrmMain.MenuItemExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmMain.MenuItemHelpClick(Sender: TObject);
begin
  FrmHelp.ShowModal;
end;

procedure TFrmMain.MenuItemHideMainClick(Sender: TObject);
begin
  Hide;
  WindowState := wsMinimized;
end;

procedure TFrmMain.TrayMenuItemOpenClick(Sender: TObject);
begin
  if WindowState = wsMinimized then
  begin
    WindowState := wsNormal;
    Show;
  end;
end;

procedure TFrmMain.TrayMenuItemExitClick(Sender: TObject);
begin
  Close;
end;

procedure RestartApp;
var
  aProcess: TProcess;
begin
  aProcess := TProcess.Create(nil);
  aProcess.Executable := Application.ExeName;
  aProcess.Execute;
  aProcess.Free;
  Application.Terminate;
end;


function SetProxy(Address: string): boolean;
var
  list: INTERNET_PER_CONN_OPTION_LIST;
  dwBufSize: DWORD;
  Options: array[1..3] of INTERNET_PER_CONN_OPTION;
begin
  try
    dwBufSize := sizeof(list);
    list.dwSize := sizeof(list);
    list.pszConnection := nil;
    list.dwOptionCount := High(Options);

    Options[1].dwOption := INTERNET_PER_CONN_FLAGS;
    Options[1].Value.dwValue := PROXY_TYPE_PROXY;

    Options[2].dwOption := INTERNET_PER_CONN_PROXY_SERVER;
    Options[2].Value.pszValue := PChar(Address);

    Options[3].dwOption := INTERNET_PER_CONN_PROXY_BYPASS;
    Options[3].Value.pszValue := '<local>';

    list.pOptions := @Options;

    Result := InternetSetOption(nil, INTERNET_OPTION_PER_CONNECTION_OPTION,
      @list, dwBufSize);

    InternetSetOption(nil, INTERNET_OPTION_SETTINGS_CHANGED, nil, 0);
    InternetSetOption(nil, INTERNET_OPTION_REFRESH, nil, 0);
  except
    Result := False;
  end;
end;

function RemoveProxy(): boolean;
var
  list: INTERNET_PER_CONN_OPTION_LIST;
  dwBufSize: DWORD;
  Options: array[1..3] of INTERNET_PER_CONN_OPTION;
begin
  try
    dwBufSize := sizeof(list);
    list.dwSize := sizeof(list);
    list.pszConnection := nil;
    list.dwOptionCount := High(Options);

    Options[1].dwOption := INTERNET_PER_CONN_FLAGS;
    Options[1].Value.dwValue := PROXY_TYPE_DIRECT;

    Options[2].dwOption := INTERNET_PER_CONN_PROXY_SERVER;
    Options[2].Value.pszValue := PChar('');

    Options[3].dwOption := INTERNET_PER_CONN_PROXY_BYPASS;
    Options[3].Value.pszValue := '<global>';

    list.pOptions := @Options;

    Result := InternetSetOption(nil, INTERNET_OPTION_PER_CONNECTION_OPTION,
      @list, dwBufSize);

    InternetSetOption(nil, INTERNET_OPTION_SETTINGS_CHANGED, nil, 0);
    InternetSetOption(nil, INTERNET_OPTION_REFRESH, nil, 0);
  except
    Result := False;
  end;
end;


function ReadRegistry(Key: string; Name: string): string;
var
  {$IFDEF Windows}
  Registry: TRegistry;
  {$ENDIF}
begin
  {$IFDEF Windows}
  Registry := TRegistry.Create;
  try
    Registry.Access := Registry.Access or KEY_READ or KEY_WOW64_64KEY;

    // Navigate to proper "directory":
    Registry.RootKey := HKEY_CURRENT_USER; //Current User only

    if Registry.OpenKey(Key, False) then
      Result := Registry.ReadString(Name);

  finally
    Registry.Free;
  end;
  {$ENDIF}
end;

procedure WriteRegistry(Key: string; Name: string; Value: string);
var
  {$IFDEF Windows}
  Registry: TRegistry;
  {$ENDIF}
begin
  {$IFDEF Windows}
  Registry := TRegistry.Create;
  try
    Registry.Access := Registry.Access or KEY_READ or KEY_WRITE or KEY_WOW64_64KEY;

    // Navigate to proper "directory":
    Registry.RootKey := HKEY_CURRENT_USER; //Current User only

    if Registry.OpenKey(Key, True) then
      Registry.WriteString(Name, Value);

  finally
    Registry.Free;
  end;
  {$ENDIF}
end;

function DeleteRegistry(Key: string; Name: string): boolean;
var
  {$IFDEF Windows}
  Registry: TRegistry;
  {$ENDIF}
begin
  {$IFDEF Windows}
  Registry := TRegistry.Create;
  try
    Registry.Access := Registry.Access or KEY_READ or KEY_WRITE or KEY_WOW64_64KEY;

    // Navigate to proper "directory":
    Registry.RootKey := HKEY_CURRENT_USER; //Current User only

    if Registry.OpenKey(Key, True) then
      Result := Registry.DeleteValue(Name);

  finally
    Registry.Free;
  end;
  {$ENDIF}
end;


function TelnetConnect(Host: string): boolean;
var
  comm: TTelnetSSHClient;
  server: string;
  Address: TStringArray;
  HostName: string;
  TargetPort: string;
begin
  try
    server := Host;
    Address := server.Split(':');
    HostName := Address[0];
    TargetPort := Address[1];

    comm := TTelnetSSHClient.Create;
    comm.HostName := HostName;
    comm.TargetPort := TargetPort;
    comm.ProtocolType := Telnet;
    comm.Connect;

    if comm.Connected then
    begin
      comm.Disconnect;
      Result := True;
    end
    else
    begin
      Result := False;
    end;
    comm.Free;

  except
    Result := False;
  end;
end;

end.
