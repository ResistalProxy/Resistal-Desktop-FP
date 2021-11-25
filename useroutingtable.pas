unit useRoutingTable;

{$mode objfpc}{$H+}

interface

{
Pointed out by @440bx:
https://forum.lazarus.freepascal.org/index.php/topic,23943.msg321556.html#msg321556
}
function IsInternetAvailable(): boolean;

implementation
uses
  Windows;

//constant and types from JwaIpHlpApi and JwaIpRtrMib
const
  ANY_SIZE = 1;

type
{$PUSH}
{$PACKENUM 4} // jediapilib.inc

  MIB_IPFORWARDROW = record
    dwForwardDest: DWORD;
    dwForwardMask: DWORD;
    dwForwardPolicy: DWORD;
    dwForwardNextHop: DWORD;
    dwForwardIfIndex: DWORD;
    dwForwardType: DWORD;
    dwForwardProto: DWORD;
    dwForwardAge: DWORD;
    dwForwardNextHopAS: DWORD;
    dwForwardMetric1: DWORD;
    dwForwardMetric2: DWORD;
    dwForwardMetric3: DWORD;
    dwForwardMetric4: DWORD;
    dwForwardMetric5: DWORD;
  end;
  //PMIB_IPFORWARDROW = ^MIB_IPFORWARDROW;

  MIB_IPFORWARDTABLE = record
    dwNumEntries: DWORD;
    table: array [0..ANY_SIZE - 1] of MIB_IPFORWARDROW;
  end;
  PMIB_IPFORWARDTABLE = ^MIB_IPFORWARDTABLE;
{$POP}

function GetIpForwardTable(pIpForwardTable: PMIB_IPFORWARDTABLE; var pdwSize: ULONG;
  bOrder: BOOL): DWORD; stdcall; external 'iphlpapi.dll';

function IsInternetAvailable(): boolean;
var
  bIsInternetAvailable: boolean = false;
  dwBufferSize: DWORD = 0;
  dwIndex: DWORD;
  pRoutingTable: PMIB_IPFORWARDTABLE;
  pByte: Windows.PBYTE;
  dwRowCount: DWORD;
begin
  bIsInternetAvailable := False;
  // Get the required buffer size
  if (ERROR_INSUFFICIENT_BUFFER = GetIpForwardTable(nil, &dwBufferSize, false)) then
  begin
    pByte := GetMem(dwBufferSize);
    if (pByte<>nil) then
    begin
      pRoutingTable := PMIB_IPFORWARDTABLE(pByte);
      // Attempt to fill buffer with routing table information
      if (NO_ERROR = GetIpForwardTable(pRoutingTable, dwBufferSize, false)) then
      begin
        dwRowCount := pRoutingTable^.dwNumEntries; // Get row count
        // Look for default route to gateway
        for dwIndex := 0 to  dwRowCount-1 do
        begin
          if (pRoutingTable^.table[dwIndex].dwForwardDest = 0) then
          begin // Default route designated by 0.0.0.0 in table
            bIsInternetAvailable := true; // Found it
            break; // Short circuit loop
          end;
        end;
      end;
      FreeMem(pByte); // Clean up. Just say "No" to memory leaks
    end;
  end;
  Result := bIsInternetAvailable;
end;

end.