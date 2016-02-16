unit uGitHub;

//  *************************************************
//    Generated By: JsonToDelphiClass - 0.64
//    Generated On: 2015-01-03 15:32:27
//  *************************************************
//    Created By  : Petar Georgiev - 2014
//    Web Site    : http://pgeorgiev.com
//  *************************************************

interface

uses Generics.Collections, Rest.Json, IdUri, IdHttp,
  IdSSLOpenSSL, System.JSON, SysUtils, Classes;

type

//  Represents a serializable object with HTTP/REST capabilities (via Indy)
//  HTTPS connections require OpenSSL binaries!
//  Use the "AOnBeforeRequest" event to setup HTTP client's parameters like timeout, encoding etc.
//  
TUGitHubSerializableObject = class abstract
protected
  //  As per http://www.restapitutorial.com/lessons/httpmethods.html
  class procedure EnsureHttpResponseCode(AHttpResponseCode: integer; AUrl: string; AValidValues: array of integer);
  class procedure EnsureHttpContentType(AHttp: TIdHttp);
public
  //  Generic Web Request method
  class function  WebRequest(AUrl: string; AOnRequest: TProc<TIdHttp>): integer;
  //  Returns an instance of T from a JSON string via GET request. AArrayProperty is intended for internal use only!
  //  HttpGet is reintroduced in descendant classes to return concrete instance
  class function  HttpGet<T: class, constructor>
                            (AUrl: string; AOnBeforeRequest: TProc<TIdHttp> = nil; AArrayProperty: string = ''): T;
  //  Performs POST request, sends the current object as JSON string and returns server's response as text.
  function        HttpPost  (AUrl: string; AOnBeforeRequest: TProc<TIdHttp> = nil): string;
  //  Performs PUT request, sends the current object as JSON string and returns server's response as text.
  function        HttpPut   (AUrl: string; AOnBeforeRequest: TProc<TIdHttp> = nil): string;
  //  Performs DELETE request and returns server's response as text. This method exists just REST compliance.
  function        HttpDelete(AUrl: string; AOnBeforeRequest: TProc<TIdHttp> = nil): string;
end;

TUploaderClass = class
private
  FAvatar_url: String;
  FEvents_url: String;
  FFollowers_url: String;
  FFollowing_url: String;
  FGists_url: String;
  FGravatar_id: String;
  FHtml_url: String;
  FId: Extended;
  FLogin: String;
  FOrganizations_url: String;
  FReceived_events_url: String;
  FRepos_url: String;
  FSite_admin: Boolean;
  FStarred_url: String;
  FSubscriptions_url: String;
  FType: String;
  FUrl: String;
public
  property avatar_url: String read FAvatar_url write FAvatar_url;
  property events_url: String read FEvents_url write FEvents_url;
  property followers_url: String read FFollowers_url write FFollowers_url;
  property following_url: String read FFollowing_url write FFollowing_url;
  property gists_url: String read FGists_url write FGists_url;
  property gravatar_id: String read FGravatar_id write FGravatar_id;
  property html_url: String read FHtml_url write FHtml_url;
  property id: Extended read FId write FId;
  property login: String read FLogin write FLogin;
  property organizations_url: String read FOrganizations_url write FOrganizations_url;
  property received_events_url: String read FReceived_events_url write FReceived_events_url;
  property repos_url: String read FRepos_url write FRepos_url;
  property site_admin: Boolean read FSite_admin write FSite_admin;
  property starred_url: String read FStarred_url write FStarred_url;
  property subscriptions_url: String read FSubscriptions_url write FSubscriptions_url;
  property &type: String read FType write FType;
  property url: String read FUrl write FUrl;
  function ToJsonString: string;
  class function FromJsonString(AJsonString: string): TUploaderClass;
end;

TAssetsClass = class
private
  FBrowser_download_url: String;
  FContent_type: String;
  FCreated_at: String;
  FDownload_count: Extended;
  FId: Extended;
  FName: String;
  FSize: Extended;
  FState: String;
  FUpdated_at: String;
  FUploader: TUploaderClass;
  FUrl: String;
public
  property browser_download_url: String read FBrowser_download_url write FBrowser_download_url;
  property content_type: String read FContent_type write FContent_type;
  property created_at: String read FCreated_at write FCreated_at;
  property download_count: Extended read FDownload_count write FDownload_count;
  property id: Extended read FId write FId;
  property name: String read FName write FName;
  property size: Extended read FSize write FSize;
  property state: String read FState write FState;
  property updated_at: String read FUpdated_at write FUpdated_at;
  property uploader: TUploaderClass read FUploader write FUploader;
  property url: String read FUrl write FUrl;
  constructor Create;
  destructor Destroy; override;
  function ToJsonString: string;
  class function FromJsonString(AJsonString: string): TAssetsClass;
end;

TAuthorClass = class
private
  FAvatar_url: String;
  FEvents_url: String;
  FFollowers_url: String;
  FFollowing_url: String;
  FGists_url: String;
  FGravatar_id: String;
  FHtml_url: String;
  FId: Extended;
  FLogin: String;
  FOrganizations_url: String;
  FReceived_events_url: String;
  FRepos_url: String;
  FSite_admin: Boolean;
  FStarred_url: String;
  FSubscriptions_url: String;
  FType: String;
  FUrl: String;
public
  property avatar_url: String read FAvatar_url write FAvatar_url;
  property events_url: String read FEvents_url write FEvents_url;
  property followers_url: String read FFollowers_url write FFollowers_url;
  property following_url: String read FFollowing_url write FFollowing_url;
  property gists_url: String read FGists_url write FGists_url;
  property gravatar_id: String read FGravatar_id write FGravatar_id;
  property html_url: String read FHtml_url write FHtml_url;
  property id: Extended read FId write FId;
  property login: String read FLogin write FLogin;
  property organizations_url: String read FOrganizations_url write FOrganizations_url;
  property received_events_url: String read FReceived_events_url write FReceived_events_url;
  property repos_url: String read FRepos_url write FRepos_url;
  property site_admin: Boolean read FSite_admin write FSite_admin;
  property starred_url: String read FStarred_url write FStarred_url;
  property subscriptions_url: String read FSubscriptions_url write FSubscriptions_url;
  property &type: String read FType write FType;
  property url: String read FUrl write FUrl;
  function ToJsonString: string;
  class function FromJsonString(AJsonString: string): TAuthorClass;
end;

TReleaseClass = class(TUGitHubSerializableObject)
private
  FAssets: TArray<TAssetsClass>;
  FAssets_url: String;
  FAuthor: TAuthorClass;
  FBody: String;
  FCreated_at: String;
  FDraft: Boolean;
  FHtml_url: String;
  FId: Extended;
  FName: String;
  FPrerelease: Boolean;
  FPublished_at: String;
  FTag_name: String;
  FTarball_url: String;
  FTarget_commitish: String;
  FUpload_url: String;
  FUrl: String;
  FZipball_url: String;
public
  property assets: TArray<TAssetsClass> read FAssets write FAssets;
  property assets_url: String read FAssets_url write FAssets_url;
  property author: TAuthorClass read FAuthor write FAuthor;
  property body: String read FBody write FBody;
  property created_at: String read FCreated_at write FCreated_at;
  property draft: Boolean read FDraft write FDraft;
  property html_url: String read FHtml_url write FHtml_url;
  property id: Extended read FId write FId;
  property name: String read FName write FName;
  property prerelease: Boolean read FPrerelease write FPrerelease;
  property published_at: String read FPublished_at write FPublished_at;
  property tag_name: String read FTag_name write FTag_name;
  property tarball_url: String read FTarball_url write FTarball_url;
  property target_commitish: String read FTarget_commitish write FTarget_commitish;
  property upload_url: String read FUpload_url write FUpload_url;
  property url: String read FUrl write FUrl;
  property zipball_url: String read FZipball_url write FZipball_url;
  constructor Create;
  destructor Destroy; override;
  function ToJsonString: string;
  class function FromJsonString(AJsonString: string): TReleaseClass;
  class function HttpGet(AUrl: string; AOnBeforeRequest: TProc<TIdHttp> = nil): TReleaseClass;
end;

TGitReleasesClass = class
private
  FReleases: TArray<TReleaseClass>;
public
  property Releases: TArray<TReleaseClass> read FReleases write FReleases;
  constructor Create;
  destructor Destroy; override;
  function ToJsonString: string;
  class function FromJsonString(AJsonString: string): TGitReleasesClass;
  class function FromUrl(AUrl: string; ATimeout: integer): TGitReleasesClass;
end;

TErrorClass = class
private
  FDocumentation_url: String;
  FMessage: String;
public
  property documentation_url: String read FDocumentation_url write FDocumentation_url;
  property message: String read FMessage write FMessage;
  function ToJsonString: string;
  class function FromJsonString(AJsonString: string): TErrorClass;
end;

implementation

{TUploaderClass}


function TUploaderClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TUploaderClass.FromJsonString(AJsonString: string): TUploaderClass;
begin
  result := TJson.JsonToObject<TUploaderClass>(AJsonString)
end;

{TAssetsClass}

constructor TAssetsClass.Create;
begin
  inherited;
  FUploader := TUploaderClass.Create();
end;

destructor TAssetsClass.Destroy;
begin
  FUploader.free;
  inherited;
end;

function TAssetsClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TAssetsClass.FromJsonString(AJsonString: string): TAssetsClass;
begin
  result := TJson.JsonToObject<TAssetsClass>(AJsonString)
end;

{TAuthorClass}


function TAuthorClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TAuthorClass.FromJsonString(AJsonString: string): TAuthorClass;
begin
  result := TJson.JsonToObject<TAuthorClass>(AJsonString)
end;

{TReleaseClass}

constructor TReleaseClass.Create;
begin
  inherited;
  FAuthor := TAuthorClass.Create();
end;

destructor TReleaseClass.Destroy;
var
  LassetsItem: TAssetsClass;
begin

 for LassetsItem in FAssets do
   LassetsItem.free;

  FAuthor.free;
  inherited;
end;

function TReleaseClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TReleaseClass.FromJsonString(AJsonString: string): TReleaseClass;
begin
  result := TJson.JsonToObject<TReleaseClass>(AJsonString)
end;

class function TReleaseClass.HttpGet(AUrl: string; AOnBeforeRequest: TProc<TIdHttp>): TReleaseClass;
begin
  result := inherited HttpGet<TReleaseClass>(AUrl, AOnBeforeRequest);
end;

{TGitReleasesClass}

constructor TGitReleasesClass.Create;
begin
  inherited;
end;

destructor TGitReleasesClass.Destroy;
var
  LReleasesItem: TReleaseClass;
begin

 for LReleasesItem in FReleases do
   LReleasesItem.free;

  inherited;
end;

function TGitReleasesClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TGitReleasesClass.FromJsonString(AJsonString: string): TGitReleasesClass;
begin
  result := TJson.JsonToObject<TGitReleasesClass>(AJsonString)
end;


class function TGitReleasesClass.FromUrl(AUrl: string;
  ATimeout: integer): TGitReleasesClass;
var
  LUri: TIdUri;
  LHttp: TIdHttp;
  LSslIoHandler: TIdSSLIOHandlerSocketOpenSSL;
  LString: string;
  LJsonValue: TJsonValue;
  LJsonObject: TJsonObject;
begin
  result := nil;
  LUri := TIdURI.Create(AUrl);
  try
    LHttp := TIdHTTP.Create;
    try
      LHttp.ConnectTimeout := ATimeout;
      LHttp.ReadTimeout := ATimeout;
      LHttp.HandleRedirects := true;

      if LUri.Protocol.ToLower = 'https' then
      begin
        LSslIoHandler := TIdSSLIOHandlerSocketOpenSSL.Create(LHttp);
        LHttp.IOHandler := LSslIoHandler;
      end;

      LString := LHttp.Get(AUrl);

      if LHttp.ResponseCode <> 200 then
        raise Exception.CreateFmt('Error getting JSON string from %s. Http error code: %d', [AUrl, LHttp.ResponseCode]);

      LJsonValue := TJSONObject.ParseJSONValue(LString);

      if LJsonValue = nil then
        raise Exception.Create('Unable to parse JSON string!');

      try
        LJsonValue.Owned := false;
        if LJsonValue is TJSONArray then
        begin
          LJsonObject := TJSONObject.Create;
          try
            LJsonObject.AddPair('Releases', LJsonValue);
            LString := LJsonObject.ToJSON;
          finally
            LJsonObject.Free;
          end;
        end;

        result := TGitReleasesClass.FromJsonString(LString);

      finally
        LJsonValue.Free;
      end;
    finally
      LHttp.Free;
    end;
  finally
    LUri.Free;
  end;
end;

{TErrorClass}

function TErrorClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TErrorClass.FromJsonString(AJsonString: string): TErrorClass;
begin
  result := TJson.JsonToObject<TErrorClass>(AJsonString)
end;

{ TUGitHubSerializableObject }

class procedure TUGitHubSerializableObject.EnsureHttpContentType(
  AHttp: TIdHttp);
begin
  if AHttp.Response.ContentType <> 'application/json' then
    raise Exception.CreateFmt('Invalid content type %s!', [AHttp.Response.ContentType]);
end;

class procedure TUGitHubSerializableObject.EnsureHttpResponseCode(
  AHttpResponseCode: integer; AUrl: string; AValidValues: array of integer);
var
  LValue: integer;
begin
  for LValue in AValidValues do
    if LValue = AHttpResponseCode then exit;

  raise Exception.CreateFmt('The request to %s has failed with code %d', [AUrl, AHttpResponseCode]);
end;

function TUGitHubSerializableObject.HttpDelete(AUrl: string;
  AOnBeforeRequest: TProc<TIdHttp>): string;
var
  LResult: string;
begin

  WebRequest(AUrl,
    procedure(LHttp: TIdHttp)
    begin
    
      //  Allow HTTP client pre-configuration
      if assigned(AOnBeforeRequest) then
        AOnBeforeRequest(LHttp);

      LResult := LHttp.Delete(AUrl); 
      EnsureHttpResponseCode(LHttp.ResponseCode, AUrl, [200, 204]);

    end
  );

  result := LResult;
end;

class function TUGitHubSerializableObject.HttpGet<T>(AUrl: string; AOnBeforeRequest: TProc<TIdHttp>; AArrayProperty: string): T;
var
  LResult: T;
begin

  WebRequest(AUrl, 
    procedure(LHttp: TIdHttp)
    var
      LString: string;
      LJsonValue: TJsonValue;
      LJsonObject: TJsonObject;
    begin
    
      //  Allow HTTP client pre-configuration
      if assigned(AOnBeforeRequest) then
        AOnBeforeRequest(LHttp);

      LString := LHttp.Get(AUrl);
      EnsureHttpResponseCode(LHttp.ResponseCode, AUrl, [200, 304]);
      EnsureHttpContentType(LHttp);

      LJsonValue := TJSONObject.ParseJSONValue(LString);
      
      if LJsonValue = nil then
        raise Exception.Create('Unable to parse JSON string!');      

      try
        LJsonValue.Owned := false;
        if LJsonValue is TJSONArray then
          if (AArrayProperty <> '') then
          begin
            LJsonObject := TJSONObject.Create;
            try
              LJsonObject.AddPair(AArrayProperty, LJsonValue);
              LString := LJsonObject.ToJSON;
            finally
              LJsonObject.Free;
            end;
          end
          else
            raise Exception.CreateFmt('The class %s does not accept array values!', [LResult.className]);
      finally
        LJsonValue.Free;
      end;   

      LResult := TJson.JsonToObject<T>(LString);     

    end
  );

  result := LResult;
end;

function TUGitHubSerializableObject.HttpPost(AUrl: string;
  AOnBeforeRequest: TProc<TIdHttp>): string;
var
  LResult: string;
begin

  WebRequest(AUrl,
    procedure(LHttp: TIdHttp)
    var
      LStringStream: TStringStream;
    begin
    
      //  Allow HTTP client pre-configuration
      if assigned(AOnBeforeRequest) then
        AOnBeforeRequest(LHttp);

      LResult := TJson.ObjectToJsonString(self);  

      LStringStream := TStringStream.Create(LResult, TEncoding.GetEncoding(LHttp.Request.ContentEncoding));
      try
        LResult := LHttp.Post(AUrl, LStringStream);
        EnsureHttpResponseCode(LHttp.ResponseCode, AUrl, [200, 201, 202, 204]);
        EnsureHttpContentType(LHttp);
      finally
        LStringStream.Free;
      end;

    end
  );

  result := LResult;
end;

function TUGitHubSerializableObject.HttpPut(AUrl: string;
  AOnBeforeRequest: TProc<TIdHttp>): string;
var
  LResult: string;
begin

  WebRequest(AUrl,
    procedure(LHttp: TIdHttp)
    var
      LStringStream: TStringStream;
    begin
    
      //  Allow HTTP client pre-configuration
      if assigned(AOnBeforeRequest) then
        AOnBeforeRequest(LHttp);

      LResult := TJson.ObjectToJsonString(self);  

      LStringStream := TStringStream.Create(LResult, TEncoding.GetEncoding(LHttp.Request.ContentEncoding));
      try
        LResult := LHttp.Put(AUrl, LStringStream);
        EnsureHttpResponseCode(LHttp.ResponseCode, AUrl, [200, 204]);
        EnsureHttpContentType(LHttp);
      finally
        LStringStream.Free;
      end;

    end
  );

  result := LResult;
end;


class function TUGitHubSerializableObject.WebRequest(AUrl: string; AOnRequest: TProc<TIdHttp>): integer;
var
  LUri: TIdUri;
  LHttp: TIdHttp;
  LSslIoHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  LUri := TIdURI.Create(AUrl);
  try
    LHttp := TIdHTTP.Create;
    try
      LHttp.HandleRedirects := true;
      //  Default encoding
      LHttp.Request.ContentEncoding := 'utf-8';
      //  Specify Content-Type header
      LHttp.Request.ContentType := 'application/json';

      //  Replace default IOHandler with TIdSSLIOHandlerSocketOpenSSL if the connection is SSL based
      if LUri.Protocol.ToLower = 'https' then
      begin
        LSslIoHandler := TIdSSLIOHandlerSocketOpenSSL.Create(LHttp);
        LHttp.IOHandler := LSslIoHandler;
      end;

      try
        AOnRequest(LHttp);
      finally
        result := LHttp.ResponseCode;
      end;

    finally
      LHttp.Free;
    end;
  finally
    LUri.Free;
  end;
end;

end.

