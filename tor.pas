unit tor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, LCLProc, strutils, RegExpr, ProjectFilesManager;

const
  Obfs4Bridges: array[1..13] of string =
    (
    'Bridge obfs4 85.31.186.98:443 011F2599C0E9B27EE74B353155E244813763C3E5 cert=ayq0XzCwhpdysn5o0EyDUbmSOx3X/oTEbzDMvczHOdBJKlvIdHHLJGkZARtT4dcBFArPPg iat-mode=0',
    'Bridge obfs4 38.229.1.78:80 C8CBDB2464FC9804A69531437BCF2BE31FDD2EE4 cert=Hmyfd2ev46gGY7NoVxA9ngrPF2zCZtzskRTzoWXbxNkzeVnGFPWmrTtILRyqCTjHR+s9dg iat-mode=1',
    'Bridge obfs4 37.218.245.14:38224 D9A82D2F9C2F65A18407B1D2B764F130847F8B5D cert=bjRaMrr1BRiAW8IE9U5z27fQaYgOhX1UCmOpg2pFpoMvo6ZgQMzLsaTzzQNTlm7hNcb+Sg iat-mode=0',
    'Bridge obfs4 192.95.36.142:443 CDF2E852BF539B82BD10E27E9115A31734E378C2 cert=qUVQ0srL1JI/vO6V6m/24anYXiJD3QP2HgzUKQtQ7GRqqUvs7P+tG43RtAqdhLOALP7DJQ iat-mode=1',
    'Bridge obfs4 38.229.33.83:80 0BAC39417268B96B9F514E7F63FA6FBA1A788955 cert=VwEFpk9F/UN9JED7XpG1XOjm/O8ZCXK80oPecgWnNDZDv5pdkhq1OpbAH0wNqOT6H6BmRQ iat-mode=1',
    'Bridge obfs4 193.11.166.194:27025 1AE2C08904527FEA90C4C4F8C1083EA59FBC6FAF cert=ItvYZzW5tn6v3G4UnQa6Qz04Npro6e81AP70YujmK/KXwDFPTs3aHXcHp4n8Vt6w/bv8cA iat-mode=0',
    'Bridge obfs4 85.31.186.26:443 91A6354697E6B02A386312F68D82CF86824D3606 cert=PBwr+S8JTVZo6MPdHnkTwXJPILWADLqfMGoVvhZClMq/Urndyd42BwX9YFJHZnBB3H0XCw iat-mode=0',
    'Bridge obfs4 193.11.166.194:27015 2D82C2E354D531A68469ADF7F878FA6060C6BACA cert=4TLQPJrTSaDffMK7Nbao6LC7G9OW/NHkUwIdjLSS3KYf0Nv4/nQiiI8dY2TcsQx01NniOg iat-mode=0',
    'Bridge obfs4 51.222.13.177:80 5EDAC3B810E12B01F6FD8050D2FD3E277B289A08 cert=2uplIpLQ0q9+0qMFrK5pkaYRDOe460LL9WHBvatgkuRr/SL31wBOEupaMMJ6koRE6Ld0ew iat-mode=0',
    'Bridge obfs4 45.145.95.6:27015 C5B7CD6946FF10C5B3E89691A7D3F2C122D2117C cert=TD7PbUO0/0k6xYHMPW3vJxICfkMZNdkRrb63Zhl5j9dW3iRGiCx0A7mPhe5T2EDzQ35+Zw iat-mode=0',
    'Bridge obfs4 146.57.248.225:22 10A6CD36A537FCE513A322361547444B393989F0 cert=K1gDtDAIcUfeLqbstggjIw2rtgIKqdIhUlHp82XRqNSq/mtAjp1BIC9vHKJ2FAEpGssTPw iat-mode=0',
    'Bridge obfs4 209.148.46.65:443 74FAD13168806246602538555B5521A0383A1875 cert=ssH+9rP8dG2NLDN2XuFw63hIO/9MNNinLmxQDpVa+7kTOa9/m+tGWT1SmSYpQ9uTBGa6Hw iat-mode=0',
    'Bridge obfs4 193.11.166.194:27020 86AC7B8D430DAC4117E9F42C9EAED18133863AAF cert=0LDeJH4JzMDtkJJrFphJCiPqKx7loozKN7VNfuukMGfHO0Z8OGdzHVkhVAOfo1mUdv9cMg iat-mode=0'
    );

  SnowflakeBridges: array[1..1] of string =
    ('Bridge snowflake 192.0.2.3:1 2B280B23E1107BB62ABFC40DDCC8824814F80A72');

  MeekBridges: array[1..1] of string =
    ('Bridge meek_lite 192.0.2.2:2 97700DFE9F483596DDA6264C4D7DF7641E1E39CE url=https://meek.azureedge.net/ front=ajax.aspnetcdn.com');

var
  AppPath: string;

function ConfigureTor(bridgeType: integer): boolean;
function ConfigToral: boolean;
function ConfigTorav: boolean;
function GetTorrcBridgeCount(bridgeType: integer): integer;
function DeleteBridge(AppPath: string; Bridge: string): boolean;
function ReadUseBridgesCfg: boolean;
procedure UseBridges(useBridges: boolean);
function DeleteAllBridge(AppPath: string): boolean;

implementation

// 0: obfs4
// 1: snowflake
// 2: meek
function ConfigureTor(bridgeType: integer): boolean;
var
  torrcPath: string;
  torrcFile: TextFile;
  slTorrc: TStringList;
  slTorav: TStringList;
  i: integer;
  haveCfg1: boolean = False;
  haveCfg2: boolean = False;
  haveCfg3: boolean = False;
  haveCfg4: boolean = False;
  haveCfg5: boolean = False;
  haveCfg6: boolean = False;
  haveCfg7: boolean = False;
  haveCfg8: boolean = False;
  haveCfg9: boolean = False;
  haveBridge: boolean = False;
begin
  torrcPath := AppPath + ProjectFilesManager.torrcPath;

  if FileExists(torrcPath) then
  begin
    // Create an instance of the string list to handle the textfile
    slTorrc := TStringList.Create;

    slTorav := TStringList.Create;
    slTorav.LoadFromFile(AppPath + ProjectFilesManager.toravPath);

    // Embed the file handling in a try/except block to handle errors gracefully
    try
      // Load the contents of the textfile completely in memory
      slTorrc.LoadFromFile(torrcPath);

      // AvoidDiskWrites
      for i := 0 to slTorrc.Count - 1 do
      begin
        if slTorrc[i].StartsWith('AvoidDiskWrites') then
        begin
          haveCfg1 := True;
          Break;
        end;
      end;
      if not haveCfg1 then
        slTorrc.Add('AvoidDiskWrites 1');

      // CircuitBuildTimeout
      for i := 0 to slTorrc.Count - 1 do
      begin
        if slTorrc[i].StartsWith('CircuitBuildTimeout') then
        begin
          haveCfg2 := True;
          Break;
        end;
      end;
      if not haveCfg2 then
        slTorrc.Add('CircuitBuildTimeout 2');

      // SocksPort
      for i := 0 to slTorrc.Count - 1 do
      begin
        if slTorrc[i].StartsWith('SocksPort') then
        begin
          haveCfg3 := True;
          Break;
        end;
      end;
      if not haveCfg3 then
        slTorrc.Add('SocksPort 9050');

      // DataDirectory
      for i := 0 to slTorrc.Count - 1 do
      begin
        if slTorrc[i].StartsWith('DataDirectory') then
        begin
          slTorrc[i] := 'DataDirectory ' + AppPath + ProjectFilesManager.DataPath;
          haveCfg4 := True;
          Break;
        end;
      end;
      if not haveCfg4 then
        slTorrc.Add('DataDirectory ' + AppPath + ProjectFilesManager.DataPath);

      // GeoIPFile
      for i := 0 to slTorrc.Count - 1 do
      begin
        if slTorrc[i].StartsWith('GeoIPFile') then
        begin
          slTorrc[i] := 'GeoIPFile ' + AppPath + ProjectFilesManager.DataPath + '\geoip';
          haveCfg5 := True;
          Break;
        end;
      end;
      if not haveCfg5 then
        slTorrc.Add('GeoIPFile ' + AppPath + ProjectFilesManager.DataPath + '\geoip');

      // GeoIPv6File
      for i := 0 to slTorrc.Count - 1 do
      begin
        if slTorrc[i].StartsWith('GeoIPv6File') then
        begin
          slTorrc[i] := 'GeoIPv6File ' + AppPath +
            ProjectFilesManager.DataPath + '\geoip6';
          haveCfg6 := True;
          Break;
        end;
      end;
      if not haveCfg6 then
        slTorrc.Add('GeoIPv6File ' + AppPath + ProjectFilesManager.DataPath + '\geoip6');

      // UseBridges
      for i := 0 to slTorrc.Count - 1 do
      begin
        if slTorrc[i].StartsWith('UseBridges') then
        begin
          haveCfg7 := True;
          Break;
        end;
      end;
      if not haveCfg7 then
        slTorrc.Add('UseBridges 1');

      // ClientTransportPlugin
      for i := 0 to slTorrc.Count - 1 do
      begin
        if slTorrc[i].StartsWith('ClientTransportPlugin') then
        begin
          if AnsiContainsStr(slTorrc[i], 'meek_lite') then
          begin
            slTorrc[i] :=
              'ClientTransportPlugin meek_lite,obfs2,obfs3,obfs4,scramblesuit exec PluggableTransports\obfs4proxy.exe';

            haveCfg8 := True;
            Break;
          end;
        end;
      end;
      if not haveCfg8 then
      begin
        slTorrc.Add(
          'ClientTransportPlugin meek_lite,obfs2,obfs3,obfs4,scramblesuit exec PluggableTransports\obfs4proxy.exe');
      end;

      // ClientTransportPlugin
      for i := 0 to slTorrc.Count - 1 do
      begin
        if slTorrc[i].StartsWith('ClientTransportPlugin') then
        begin
          if AnsiContainsStr(slTorrc[i], 'snowflake') then
          begin
            slTorrc[i] :=
              'ClientTransportPlugin snowflake exec PluggableTransports\snowflake-client.exe -url https://snowflake-broker.torproject.net.global.prod.fastly.net/ -front cdn.sstatic.net -ice stun:stun.l.google.com:19302,stun:stun.voip.blackberry.com:3478,stun:stun.altar.com.pl:3478,stun:stun.antisip.com:3478,stun:stun.bluesip.net:3478,stun:stun.dus.net:3478,stun:stun.epygi.com:3478,stun:stun.sonetel.com:3478,stun:stun.sonetel.net:3478,stun:stun.stunprotocol.org:3478,stun:stun.uls.co.za:3478,stun:stun.voipgate.com:3478,stun:stun.voys.nl:3478';

            haveCfg9 := True;
            Break;
          end;
        end;
      end;
      if not haveCfg9 then
      begin
        slTorrc.Add(
          'ClientTransportPlugin snowflake exec PluggableTransports\snowflake-client.exe -url https://snowflake-broker.torproject.net.global.prod.fastly.net/ -front cdn.sstatic.net -ice stun:stun.l.google.com:19302,stun:stun.voip.blackberry.com:3478,stun:stun.altar.com.pl:3478,stun:stun.antisip.com:3478,stun:stun.bluesip.net:3478,stun:stun.dus.net:3478,stun:stun.epygi.com:3478,stun:stun.sonetel.com:3478,stun:stun.sonetel.net:3478,stun:stun.stunprotocol.org:3478,stun:stun.uls.co.za:3478,stun:stun.voipgate.com:3478,stun:stun.voys.nl:3478');
      end;

      // have any bridge in torrc?
      for i := 0 to slTorrc.Count - 1 do
      begin
        if (bridgeType = 0) and slTorrc[i].StartsWith('Bridge obfs4') then
        begin
          haveBridge := True;
          Break;
        end;

        if (bridgeType = 1) and slTorrc[i].StartsWith('Bridge snowflake') then
        begin
          haveBridge := True;
          Break;
        end;

        if (bridgeType = 2) and slTorrc[i].StartsWith('Bridge meek_lite') then
        begin
          haveBridge := True;
          Break;
        end;
      end;

      // if torav have no bridges -> write from Bridges
      // else write from torav
      if not haveBridge then
      begin
        if (bridgeType = 0) and (slTorav.Count > 0) then
        begin
          for i := 0 to slTorav.Count - 1 do
          begin
            if slTorav[i].StartsWith('Bridge obfs4') then
            begin
              slTorrc.Add(slTorav[i]);
            end;
          end;
        end
        else if bridgeType = 0 then
        begin
          for i := 1 to Length(Obfs4Bridges) do
          begin
            slTorrc.Add(Obfs4Bridges[i]);
          end;
        end
        else if bridgeType = 1 then
        begin
          for i := 1 to Length(SnowflakeBridges) do
          begin
            slTorrc.Add(SnowflakeBridges[i]);
          end;
        end
        else if bridgeType = 2 then
        begin
          for i := 1 to Length(MeekBridges) do
          begin
            slTorrc.Add(MeekBridges[i]);
          end;
        end;
      end;

      // And write the contents back to disk, replacing the original contents
      slTorrc.SaveToFile(torrcPath);

      Result := True;
    except
      Result := False;
    end;

    // Clean up
    slTorrc.Free;
    slTorav.Free;

  end
  else
  begin
    // Set the name of the file that will be created
    AssignFile(torrcFile, torrcPath);

    try
      // Create the file, write some text and close it.
      Rewrite(torrcFile);
      WriteLn(torrcFile, 'AvoidDiskWrites 1');
      WriteLn(torrcFile, '');
      WriteLn(torrcFile, 'CircuitBuildTimeout 2');
      WriteLn(torrcFile, '');
      WriteLn(torrcFile, 'SocksPort 9050');
      WriteLn(torrcFile, '');
      WriteLn(torrcFile, 'DataDirectory ' + AppPath + ProjectFilesManager.DataPath);
      WriteLn(torrcFile, 'GeoIPFile ' + AppPath +
        ProjectFilesManager.DataPath + '\geoip');
      WriteLn(torrcFile, 'GeoIPv6File ' + AppPath + ProjectFilesManager.DataPath +
        '\geoip6');
      WriteLn(torrcFile, '');
      WriteLn(torrcFile, 'UseBridges 1');

      {$IFDEF Windows}
      WriteLn(torrcFile,
        'ClientTransportPlugin meek_lite,obfs2,obfs3,obfs4,scramblesuit exec PluggableTransports\obfs4proxy.exe');
      WriteLn(torrcFile,
        'ClientTransportPlugin snowflake exec PluggableTransports\snowflake-client.exe -url https://snowflake-broker.torproject.net.global.prod.fastly.net/ -front cdn.sstatic.net -ice stun:stun.l.google.com:19302,stun:stun.voip.blackberry.com:3478,stun:stun.altar.com.pl:3478,stun:stun.antisip.com:3478,stun:stun.bluesip.net:3478,stun:stun.dus.net:3478,stun:stun.epygi.com:3478,stun:stun.sonetel.com:3478,stun:stun.sonetel.net:3478,stun:stun.stunprotocol.org:3478,stun:stun.uls.co.za:3478,stun:stun.voipgate.com:3478,stun:stun.voys.nl:3478');
      {$ENDIF}

      {$IFDEF UNIX}
      WriteLn(torrcFile,
        'ClientTransportPlugin meek_lite,obfs2,obfs3,obfs4,scramblesuit exec PluggableTransports/obfs4proxy');
      {$ENDIF}

      WriteLn(torrcFile, '');

      if bridgeType = 0 then
      begin
        for i := 1 to Length(Obfs4Bridges) do
        begin
          WriteLn(torrcFile, Obfs4Bridges[i]);
        end;
      end
      else if bridgeType = 1 then
      begin
        for i := 1 to Length(SnowflakeBridges) do
        begin
          WriteLn(torrcFile, SnowflakeBridges[i]);
        end;
      end
      else if bridgeType = 2 then
      begin
        for i := 1 to Length(MeekBridges) do
        begin
          WriteLn(torrcFile, MeekBridges[i]);
        end;
      end;

      CloseFile(torrcFile);

      Result := True;
    except
      Result := False;
    end;
  end;

end;

function ConfigToral: boolean;
var
  toralFile: TextFile;
  slToral: TStringList;
  toralHaveBridge: boolean = False;
  toralBridgeIp: string;

  i, j: integer;
  BridgeRegex: TRegExpr;
  BridgeIp: string;
begin
  BridgeRegex := TRegExpr.Create('\b\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}\:\d{1,5}\b');

  if FileExists(AppPath + ProjectFilesManager.toralPath) then
  begin
    // Update toral  (all)
    try
      slToral := TStringList.Create;
      slToral.LoadFromFile(AppPath + ProjectFilesManager.toralPath);

      for i := 1 to Length(Obfs4Bridges) do
      begin
        toralHaveBridge := False;
        if Obfs4Bridges[i].StartsWith('Bridge obfs4') then
        begin
          if BridgeRegex.Exec(Obfs4Bridges[i]) then
          begin
            BridgeIp := BridgeRegex.Match[0];

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
              slToral.Add(Obfs4Bridges[i]);
            end;
          end;
        end;
      end;

      // And write the contents back to disk, replacing the original contents
      slToral.SaveToFile(AppPath + ProjectFilesManager.toralPath);

      Result := True;
    except
      Result := False;
    end;

    // Clean up
    slToral.Free;

  end
  else
  begin
    // Set the name of the file that will be created
    AssignFile(toralFile, AppPath + ProjectFilesManager.toralPath);

    try
      // Create the file, write some text and close it.
      Rewrite(toralFile);

      for i := 1 to Length(Obfs4Bridges) do
      begin
        WriteLn(toralFile, Obfs4Bridges[i]);
      end;

      CloseFile(toralFile);

      Result := True;
    except
      Result := False;
    end;

  end;

end;

function ConfigTorav: boolean;
var
  F: longint;
begin
  if not FileExists(AppPath + ProjectFilesManager.toravPath) then
  begin
    try
      // If a file with name FileName already existed on the disk, it is overwritten.
      // If an error occurs (e.g. disk full or non-existent path), the function returns THandle(-1).
      F := FileCreate(AppPath + ProjectFilesManager.toravPath);
      // Halt stops program execution and returns control to the calling program.
      // The optional argument Errnum specifies an exit value. If omitted, zero is returned.
      // Note that this skips any try/finally (implicit or explicit) or try/except blocks,
      // thus may result in memory leaks.
      // Finalization sections of units will be executed.
      //if F = -1 then
      //  Halt(1); { Stop with exit code 1 }
      FileClose(f);

      Result := True;
    except
      Result := False;
    end;
  end
  else
    Result := True;

end;

function GetTorrcBridgeCount(bridgeType: integer): integer;
var
  torrcPath: string;
  slTorrc: TStringList;
  i: integer;
  torrcBridgeCount: integer;
begin
  torrcPath := AppPath + ProjectFilesManager.torrcPath;

  if FileExists(torrcPath) then
  begin
    torrcBridgeCount := 0;

    slTorrc := TStringList.Create;
    try
      slTorrc.LoadFromFile(torrcPath);

      if bridgeType = 0 then
      begin
        for i := 0 to slTorrc.Count - 1 do
        begin
          if slTorrc[i].StartsWith('Bridge obfs4') then
            Inc(torrcBridgeCount);
        end;
      end
      else if bridgeType = 1 then
      begin
        for i := 0 to slTorrc.Count - 1 do
        begin
          if slTorrc[i].StartsWith('Bridge snowflake') then
            Inc(torrcBridgeCount);
        end;
      end
      else if bridgeType = 2 then
      begin
        for i := 0 to slTorrc.Count - 1 do
        begin
          if slTorrc[i].StartsWith('Bridge meek_lite') then
            Inc(torrcBridgeCount);
        end;
      end;

      Result := torrcBridgeCount;
    except
      Result := -1;
    end;

    // Clean up
    slTorrc.Free;

  end
  else
  begin
    Result := 0;
  end;

end;

function DeleteBridge(AppPath: string; Bridge: string): boolean;
var
  torrcPath: string;
  slTorrc: TStringList;
  i: integer;
begin
  torrcPath := AppPath + ProjectFilesManager.torrcPath;
  if FileExists(torrcPath) then
  begin
    slTorrc := TStringList.Create;
    try
      slTorrc.LoadFromFile(torrcPath);
      for i := 0 to slTorrc.Count - 1 do
      begin
        if slTorrc[i].StartsWith('Bridge obfs4') and
          AnsiContainsStr(slTorrc[i], Bridge) then
        begin
          slTorrc.Delete(i);
          Break;
        end;
      end;
      slTorrc.SaveToFile(torrcPath);
      Result := True;
    except
      on E: EInOutError do
      begin
        Result := False;
      end;
    end;
    slTorrc.Free;
  end
  else
  begin
    Result := False;
  end;
end;

function ReadUseBridgesCfg: boolean;
var
  torrcPath: string;
  slTorrc: TStringList;
  i: integer;
  strUseBridges: TStringArray;
begin
  torrcPath := AppPath + ProjectFilesManager.torrcPath;

  if FileExists(torrcPath) then
  begin
    slTorrc := TStringList.Create;
    try
      slTorrc.LoadFromFile(torrcPath);

      for i := 0 to slTorrc.Count - 1 do
      begin
        if slTorrc[i].StartsWith('UseBridges') then
        begin
          strUseBridges := slTorrc[i].Split(' ');
          Result := StrToBoolDef(strUseBridges[1], True);
          Break;
        end;
      end;

      slTorrc.SaveToFile(torrcPath);
      slTorrc.Free;
    except
      Result := False;
    end;
  end;
end;

procedure UseBridges(useBridges: boolean);
var
  torrcPath: string;
  slTorrc: TStringList;
  i: integer;
begin
  torrcPath := AppPath + ProjectFilesManager.torrcPath;

  if FileExists(torrcPath) then
  begin
    slTorrc := TStringList.Create;
    try
      slTorrc.LoadFromFile(torrcPath);

      for i := 0 to slTorrc.Count - 1 do
      begin
        if slTorrc[i].StartsWith('UseBridges') then
        begin
          slTorrc[i] := 'UseBridges ' + BoolToStr(useBridges, '1', '0');
          Break;
        end;
      end;

      slTorrc.SaveToFile(torrcPath);
      slTorrc.Free;
    except
    end;
  end;
end;

function DeleteAllBridge(AppPath: string): boolean;
var
  torrcPath: string;
  slTorrc: TStringList;
  i: integer;
begin
  torrcPath := AppPath + ProjectFilesManager.torrcPath;

  if FileExists(torrcPath) then
  begin
    slTorrc := TStringList.Create;
    try
      slTorrc.LoadFromFile(torrcPath);
      for i := slTorrc.Count - 1 downto 0 do
      begin
        if slTorrc[i].StartsWith('Bridge ') then
        begin
          slTorrc.Delete(i);
        end;
      end;

      slTorrc.SaveToFile(torrcPath);
      Result := True;
    except
      on E: EInOutError do
      begin
        Result := False;
      end;
    end;
    slTorrc.Free;
  end
  else
  begin
    Result := False;
  end;
end;

initialization
  // optional initialization part
  AppPath := ExtractFilePath(Application.ExeName);

finalization
  // optional clean-up code

end.
