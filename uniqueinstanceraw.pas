unit UniqueInstanceRaw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function InstanceRunning(const Identifier: string; SendParameters: boolean = False; DoInitServer: boolean = True): boolean;
function InstanceRunning: boolean;

implementation

uses
  SimpleIpc, UniqueInstanceBase;

function InstanceRunning(const Identifier: string; SendParameters: boolean; DoInitServer: boolean): boolean;
var
  Client: TSimpleIPCClient;
begin
  Client := TSimpleIPCClient.Create(nil);
  with Client do
    try
      ServerId := GetServerId(Identifier);
      Result := Client.ServerRunning;
      if not Result then
      begin
        if DoInitServer then
          InitializeUniqueServer(ServerID);
      end
      else
      // an instance already exists
      if SendParameters then
      begin
        Active := True;
        SendStringMessage(ParamCount, GetFormattedParams);
      end;
    finally
      Free;
    end;
end;

function InstanceRunning: boolean;
begin
  Result := InstanceRunning('');
end;

end.

