unit ProjectFilesManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, fileutil,
  sha256, readFileInfo, ShellAPI;

const
  Dirs: array[1..7] of string =
    (
    'Bin',
    'Privoxy',
    'Tor',
    'Tor\Data',
    'Tor\Data\Tor',
    'Tor\Tor',
    'Tor\Tor\PluggableTransports'
    );

  Files: array[1..11] of string =
    (
    'Resistal.exe',
    'libeay32.dll',
    'ssleay32.dll',

    'Bin\aria2c.exe',

    'Privoxy\config.txt',
    'Privoxy\privoxy.exe',

    'Tor\Data\Tor\geoip',
    'Tor\Data\Tor\geoip6',
    'Tor\Tor\PluggableTransports\obfs4proxy.exe',
    'Tor\Tor\PluggableTransports\snowflake-client.exe',
    'Tor\Tor\tor.exe'
    );

  TorPath: string = 'Tor';
  TorExePath: string = 'Tor\Tor\tor.exe';
  DataPath: string = 'Tor\Data\Tor';
  torrcPath: string = 'Tor\Data\Tor\torrc';
  obfs4Path: string = 'Tor\Tor\PluggableTransports\obfs4proxy.exe';
  PrivoxyPath: string = 'Privoxy';
  PrivoxyExePath: string = 'Privoxy\privoxy.exe';
  Aria2cPath: string = 'Bin\aria2c.exe';
  toralPath: string = 'Tor\Data\Tor\toral';
  toravPath: string = 'Tor\Data\Tor\torav';
  TorVersion: string = '0.4.7.13';

var
  AppPath: string;
  AppVersion: string;

function CheckFiles: boolean;
function UpdateApp: boolean;
function UpdateAppDirectory: boolean;
function UpdateResistal: boolean;
function UpdateFile(FilePath: string): boolean;
function CheckFile(FilePath: string): boolean;
procedure DeleteBackupFile(FilePath: string);
procedure DeleteBackup;
procedure DeleteDownload;
procedure DeleteDirectory(DirName: string);
procedure DeleteFiles(APath, AFileSpec: string);
procedure DeleteFileAndDirectory(const DirName: string);

implementation

function CheckFile(FilePath: string): boolean;
begin
  Result := FileExists(AppPath + FilePath);
end;

function CheckFiles: boolean;
var
  i: integer;
begin
  Result := True;

  for i := 1 to Length(Files) do
  begin
    Result := Result and CheckFile(Files[i]);

    if not Result then
    begin
      ShowMessage(Files[i] + ' NOT Found.');
      break;
    end;
  end;
end;

function UpdateApp: boolean;
var
  i: integer;
begin
  Result := True;

  Result := Result and UpdateAppDirectory;

  for i := 2 to Length(Files) do
  begin
    Result := Result and UpdateFile(Files[i]);

    if not Result then
    begin
      ShowMessage(Files[i] + ' NO Update.');
      break;
    end;
  end;

  if Result then
    DeleteDownload;
end;

function UpdateAppDirectory: boolean;
var
  i: integer;
begin
  Result := True;

  for i := 1 to Length(Dirs) do
  begin
    try
      if not DirectoryExists(AppPath + Dirs[i]) then
        CreateDir(AppPath + Dirs[i]);
    except
      Result := False;
    end;
  end;
end;

function UpdateResistal: boolean;
begin
  Result := True;

  UpdateFile('Resistal.exe');
end;

function UpdateFile(FilePath: string): boolean;
var
  IsRenameFile: boolean;
  IsCopyFile: boolean;
begin
  Result := True;

  if FileExists(AppPath + 'Download\' + FilePath) then
  begin
    if FileExists(AppPath + FilePath) then
    begin
      if sha256.getsha256_file(AppPath + FilePath) <>
        sha256.getsha256_file(AppPath + 'Download\' + FilePath) then
      begin
        try
          // rename
          if FileExists(AppPath + FilePath + '.bak') then
            DeleteFileAndDirectory(AppPath + FilePath + '.bak');
          IsRenameFile := RenameFile(AppPath + FilePath, AppPath + FilePath + '.bak');

          if IsRenameFile then
          begin
            IsCopyFile := CopyFile(AppPath + 'Download\' + FilePath,
              AppPath + FilePath, [cffPreserveTime, cffOverwriteFile]);

            if IsCopyFile then
            begin
              // delete download file if updated
              DeleteFileAndDirectory(AppPath + 'Download\' + FilePath);
            end;
          end;
        except
          Result := False;
        end;
      end
      else
      begin
        // remove download file if same hash
        DeleteFileAndDirectory(AppPath + 'Download\' + FilePath);
      end;
    end
    else
    begin
      try
        IsCopyFile := CopyFile(AppPath + 'Download\' + FilePath,
          AppPath + FilePath, [cffPreserveTime, cffOverwriteFile]);

        if IsCopyFile then
          DeleteFileAndDirectory(AppPath + 'Download\' + FilePath);
      except
        Result := False;
      end;
    end;
  end;
end;

procedure DeleteBackup;
begin
  DeleteBackupFile(AppPath + 'Resistal.exe.bak');
  DeleteBackupFile(AppPath + 'libeay32.dll.bak');
  DeleteBackupFile(AppPath + 'ssleay32.dll.bak');

  DeleteBackupFile(AppPath + 'Bin\SetProxy.exe.bak');
  DeleteBackupFile(AppPath + 'Bin\aria2c.exe.bak');

  DeleteBackupFile(AppPath + 'Tor\Tor\PluggableTransports\obfs4proxy.exe.bak');
  DeleteBackupFile(AppPath + 'Tor\Tor\libcrypto-1_1.dll.bak');
  DeleteBackupFile(AppPath + 'Tor\Tor\libevent_core-2-1-7.dll.bak');
  DeleteBackupFile(AppPath + 'Tor\Tor\libevent_extra-2-1-7.dll.bak');
  DeleteBackupFile(AppPath + 'Tor\Tor\libevent-2-1-7.dll.bak');
  DeleteBackupFile(AppPath + 'Tor\Tor\libgcc_s_dw2-1.dll.bak');
  DeleteBackupFile(AppPath + 'Tor\Tor\libssl-1_1.dll.bak');
  DeleteBackupFile(AppPath + 'Tor\Tor\libssp-0.dll.bak');
  DeleteBackupFile(AppPath + 'Tor\Tor\libwinpthread-1.dll.bak');
  DeleteBackupFile(AppPath + 'Tor\Tor\tor.exe.bak');
  DeleteBackupFile(AppPath + 'Tor\Tor\zlib1.dll.bak');
end;

procedure DeleteDownload;
begin
  DeleteFileAndDirectory(AppPath + 'Download\*');
end;

procedure DeleteDirectory(DirName: string);
var
  lSearchRec: TSearchRec;
begin
  if FindFirst(DirName + '\*', faAnyFile, lSearchRec) = 0 then
  begin
    try
      repeat
        if (lSearchRec.Attr and faDirectory <> 0) then
        begin
          if (lSearchRec.Name <> '.') and (lSearchRec.Name <> '..') then
          begin
            DeleteDirectory(DirName + '\' + lSearchRec.Name);
          end;
        end
        else
        begin
          SysUtils.DeleteFile(DirName + '\' + lSearchRec.Name);
        end;
      until FindNext(lSearchRec) <> 0;
    finally
      SysUtils.FindClose(lSearchRec);
    end;
    RemoveDir(DirName);
  end;
end;

procedure DeleteFiles(APath, AFileSpec: string);
var
  lSearchRec: TSearchRec;
  lPath: string;
begin
  lPath := IncludeTrailingPathDelimiter(APath);

  if FindFirst(lPath + AFileSpec, faAnyFile, lSearchRec) = 0 then
  begin
    try
      repeat
        SysUtils.DeleteFile(lPath + lSearchRec.Name);
      until SysUtils.FindNext(lSearchRec) <> 0;
    finally
      SysUtils.FindClose(lSearchRec);  // Free resources on successful find
    end;
  end;
end;

procedure DeleteFileAndDirectory(const DirName: string);
var
  FileOp: TSHFileOpStruct;
begin
  // dir: path
  // content dir: path\*
  // file: path\file
  FillChar(FileOp, SizeOf(FileOp), 0);
  FileOp.wFunc := FO_DELETE;
  FileOp.pFrom := PChar(DirName + #0);//double zero-terminated
  FileOp.fFlags := FOF_SILENT or FOF_NOERRORUI or FOF_NOCONFIRMATION;
  SHFileOperation(FileOp);
end;

procedure DeleteBackupFile(FilePath: string);
begin
  if FileExists(FilePath) then
    DeleteFileAndDirectory(FilePath);
end;

initialization
  // optional initialization part
  AppPath := ExtractFilePath(Application.ExeName);
  AppVersion := readFileInfo.getAppVersion;

finalization
  // optional clean-up code

end.
