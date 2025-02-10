unit TidyKit.Request;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fphttpclient, opensslsockets, base64,
  fpjson, jsonparser, URIParser, HTTPDefs, TidyKit.Core;

type
  { HTTP Methods }
  TRequestMethod = (rmGet, rmPost, rmPut, rmDelete, rmPatch, rmHead, rmOptions);
  
  { HTTP Response }
  TResponse = class(TObject)
  private
    FStatusCode: Integer;
    FHeaders: TStringList;
    FContent: TMemoryStream;
    FContentText: string;
    FContentJSON: TJSONData;
    function GetContentText: string;
    function GetContentJSON: TJSONData;
  public
    constructor Create;
    destructor Destroy; override;
    property StatusCode: Integer read FStatusCode write FStatusCode;
    property Headers: TStringList read FHeaders;
    property Content: TMemoryStream read FContent;
    property Text: string read GetContentText;
    property JSON: TJSONData read GetContentJSON;
  end;

  { Request Options }
  TRequestOptions = record
    Headers: array of string;
    ParamString: string;  // Format: 'param1=value1&param2=value2'
    Data: string;
    JSON: string;
    Auth: array[0..1] of string;
    Timeout: Integer;
    VerifySSL: Boolean;
  end;

  { Main Request Class }
  TRequestKit = class(TKitBase)
  private
    FClient: TFPHTTPClient;
    procedure InitializeClient;
    procedure SetRequestOptions(const Options: TRequestOptions);
    function BuildURL(const BaseURL: string; const ParamString: string): string;
    function ExecuteRequest(const Method: TRequestMethod; const URL: string; 
      const Options: TRequestOptions): TResponse;
    function StringToStream(const AString: string): TStream;
  public
    constructor Create;
    destructor Destroy; override;
    
    { Main HTTP Methods }
    function Get(const URL: string; const Options: TRequestOptions): TResponse;
    function Post(const URL: string; const Options: TRequestOptions): TResponse;
    function Put(const URL: string; const Options: TRequestOptions): TResponse;
    function Delete(const URL: string; const Options: TRequestOptions): TResponse;
    function Patch(const URL: string; const Options: TRequestOptions): TResponse;
    function Head(const URL: string; const Options: TRequestOptions): TResponse;
    function SendOptions(const URL: string; const Options: TRequestOptions): TResponse;
    
    { Helper Methods }
    class function CreateDefaultOptions: TRequestOptions;
  end;

implementation

{ TResponse }

constructor TResponse.Create;
begin
  inherited Create;
  FHeaders := TStringList.Create;
  FContent := TMemoryStream.Create;
  FContentJSON := nil;
end;

destructor TResponse.Destroy;
begin
  FHeaders.Free;
  FContent.Free;
  if Assigned(FContentJSON) then
    FContentJSON.Free;
  inherited Destroy;
end;

function TResponse.GetContentText: string;
var
  Stream: TStringStream;
begin
  if FContentText = '' then
  begin
    Stream := TStringStream.Create('');
    try
      FContent.Position := 0;
      Stream.LoadFromStream(FContent);
      FContentText := Stream.DataString;
    finally
      Stream.Free;
    end;
  end;
  Result := FContentText;
end;

function TResponse.GetContentJSON: TJSONData;
var
  Parser: TJSONParser;
begin
  if not Assigned(FContentJSON) then
  begin
    Parser := TJSONParser.Create(GetContentText);
    try
      FContentJSON := Parser.Parse;
    finally
      Parser.Free;
    end;
  end;
  Result := FContentJSON;
end;

{ TRequestKit }

constructor TRequestKit.Create;
begin
  inherited Create;
  InitializeClient;
end;

destructor TRequestKit.Destroy;
begin
  FClient.Free;
  inherited Destroy;
end;

procedure TRequestKit.InitializeClient;
begin
  FClient := TFPHTTPClient.Create(nil);
  FClient.AllowRedirect := True;
end;

class function TRequestKit.CreateDefaultOptions: TRequestOptions;
begin
  Result.Headers := [];
  Result.ParamString := '';
  Result.Data := '';
  Result.JSON := '';
  Result.Auth[0] := '';
  Result.Auth[1] := '';
  Result.Timeout := 0;
  Result.VerifySSL := True;
end;

function TRequestKit.BuildURL(const BaseURL: string; const ParamString: string): string;
var
  URI: TURI;
begin
  URI := ParseURI(BaseURL);
  Result := BaseURL;
  
  if ParamString <> '' then
  begin
    if Pos('?', Result) > 0 then
      Result := Result + '&' + ParamString
    else
      Result := Result + '?' + ParamString;
  end;
end;

function TRequestKit.StringToStream(const AString: string): TStream;
begin
  Result := TStringStream.Create(AString);
end;

procedure TRequestKit.SetRequestOptions(const Options: TRequestOptions);
var
  I: Integer;
  AuthStr: string;
begin
  // Set headers
  FClient.RequestHeaders.Clear;
  for I := 0 to Length(Options.Headers) - 1 do
    FClient.RequestHeaders.Add(Options.Headers[I]);
    
  // Set timeout
  if Options.Timeout > 0 then
    FClient.ConnectTimeout := Options.Timeout;
    
  // Set basic auth if provided
  if (Options.Auth[0] <> '') then
  begin
    AuthStr := EncodeStringBase64(Options.Auth[0] + ':' + Options.Auth[1]);
    FClient.RequestHeaders.Add('Authorization: Basic ' + AuthStr);
  end;
end;

function TRequestKit.ExecuteRequest(const Method: TRequestMethod; const URL: string;
  const Options: TRequestOptions): TResponse;
var
  ResponseStream: TMemoryStream;
  MethodStr: string;
  RequestBodyStream: TStream;
  Parser: TJSONParser;
  TempJSON: TJSONData;
begin
  Result := TResponse.Create;
  ResponseStream := TMemoryStream.Create;
  RequestBodyStream := nil;
  try
    SetRequestOptions(Options);
    
    case Method of
      rmGet: MethodStr := 'GET';
      rmPost: MethodStr := 'POST';
      rmPut: MethodStr := 'PUT';
      rmDelete: MethodStr := 'DELETE';
      rmPatch: MethodStr := 'PATCH';
      rmHead: MethodStr := 'HEAD';
      rmOptions: MethodStr := 'OPTIONS';
    end;
    
    try
      if Options.JSON <> '' then
      begin
        // Validate JSON before sending
        Parser := TJSONParser.Create(Options.JSON);
        try
          TempJSON := Parser.Parse;
          TempJSON.Free;
        finally
          Parser.Free;
        end;
        
        RequestBodyStream := StringToStream(Options.JSON);
        FClient.AddHeader('Content-Type', 'application/json');
      end
      else if Options.Data <> '' then
      begin
        RequestBodyStream := StringToStream(Options.Data);
      end;
      
      if Assigned(RequestBodyStream) then
        FClient.RequestBody := RequestBodyStream;
      
      FClient.HTTPMethod(MethodStr, BuildURL(URL, Options.ParamString), ResponseStream, []);
      
      Result.StatusCode := FClient.ResponseStatusCode;
      Result.Headers.Assign(FClient.ResponseHeaders);
      Result.Content.LoadFromStream(ResponseStream);
      
    except
      on E: Exception do
      begin
        FreeAndNil(Result);
        raise ETidyKitException.Create('HTTP Request Error: ' + E.Message);
      end;
    end;
  finally
    ResponseStream.Free;
    if Assigned(RequestBodyStream) then
      RequestBodyStream.Free;
  end;
end;

function TRequestKit.Get(const URL: string; const Options: TRequestOptions): TResponse;
begin
  Result := ExecuteRequest(rmGet, URL, Options);
end;

function TRequestKit.Post(const URL: string; const Options: TRequestOptions): TResponse;
begin
  Result := ExecuteRequest(rmPost, URL, Options);
end;

function TRequestKit.Put(const URL: string; const Options: TRequestOptions): TResponse;
begin
  Result := ExecuteRequest(rmPut, URL, Options);
end;

function TRequestKit.Delete(const URL: string; const Options: TRequestOptions): TResponse;
begin
  Result := ExecuteRequest(rmDelete, URL, Options);
end;

function TRequestKit.Patch(const URL: string; const Options: TRequestOptions): TResponse;
begin
  Result := ExecuteRequest(rmPatch, URL, Options);
end;

function TRequestKit.Head(const URL: string; const Options: TRequestOptions): TResponse;
begin
  Result := ExecuteRequest(rmHead, URL, Options);
end;

function TRequestKit.SendOptions(const URL: string; const Options: TRequestOptions): TResponse;
begin
  Result := ExecuteRequest(rmOptions, URL, Options);
end;

end. 
