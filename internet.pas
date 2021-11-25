unit internet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, RegexPr;

function GetExternalIPAddress: string;

implementation

function GetExternalIPAddress: string;
var
  HTTPClient: TFPHTTPClient;
  IPRegex: TRegExpr;
  RawData: string;
begin
  try
    HTTPClient := TFPHTTPClient.Create(nil);
    IPRegex := TRegExpr.Create;
    try
      RawData := HTTPClient.Get('http://httpbin.org/ip');
      IPRegex.Expression := RegExprString('\b\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}\b');
      if IPRegex.Exec(RawData) then
      begin
        Result := IPRegex.Match[0];
      end
      else
      begin
        Result := 'Got invalid results getting external IP address.';
      end;
    except
      on E: Exception do
      begin
        Result := 'Error retrieving external IP address';
      end;
    end;
  finally
    HTTPClient.Free;
    IPRegex.Free;
  end;
end;

end.
