unit sha256;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, DCPsha256;

function getsha256_file(S: string): string;
function getsha256(S: string): string;

implementation

function getsha256_file(S: string): string;
var
  Hash: TDCP_sha256;
  Digest: array[0..31] of byte;  // sha256 produces a 256bit digest (32bytes)
  Source: TFileStream;
  i: integer;
  str1: string;
begin
  Result := '';
  Source := nil;
  try
    Source := TFileStream.Create(s, fmOpenRead);  // open the file specified by Edit1
  except
    MessageDlg('Unable to open file', mtError, [mbOK], 0);
  end;

  if Source <> nil then
  begin
    Hash := TDCP_sha256.Create(nil);  // create the hash
    Hash.Init;                        // initialize it
    Hash.UpdateStream(Source, Source.Size);
    Hash.Final(Digest);               // produce the digest
    Source.Free;
    str1 := '';
    for i := 0 to 31 do
      str1 := str1 + IntToHex(Digest[i], 2);

    Result := UpperCase(str1);         // display the digest in capital letter
  end;
end;


function getsha256(S: string): string;
var
  Hash: TDCP_sha256;
  Digest: array[0..31] of byte;  // sha256 produces a 256bit digest (32bytes)
  Source: string;
  i: integer;
  str1: string;
begin
  Result := '';
  Source := S;  // here your string for get sha256

  if Source <> '' then
  begin
    Hash := TDCP_sha256.Create(nil);  // create the hash
    Hash.Init;                        // initialize it
    Hash.UpdateStr(Source);
    Hash.Final(Digest);               // produce the digest
    str1 := '';
    for i := 0 to 31 do
      str1 := str1 + IntToHex(Digest[i], 2);

    Result := UpperCase(str1);         // display the digest in capital letter
  end;
end;


end.
