unit JTD.Utils;

interface

function DownloadText(const Url: string): string;

implementation

uses
  System.Net.HttpClient, System.NetConsts, System.Net.URLClient, System.Classes,
  System.SysUtils;

function DownloadText(const Url: string): string;
begin
  var HTTP := THTTPClient.Create;
  try
    HTTP.Accept := 'application/json';
    var Response := TStringStream.Create('', TEncoding.UTF8);
    try
      var RespInfo := HTTP.Get(Url, Response);
      if RespInfo.StatusCode <> 200 then
        raise ENetHTTPClientException.Create(RespInfo.StatusText + '. Code ' + RespInfo.StatusCode.ToString);
      Result := Response.DataString;
    finally
      Response.Free;
    end;
  finally
    HTTP.Free;
  end;
end;

end.

