unit TidyKit.Request.Simple;

{$mode objfpc}{$H+}{$J-}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, fphttpclient, opensslsockets, base64,
  fpjson, jsonparser, URIParser, HTTPDefs, TidyKit.Core;

type
  { Response record with automatic cleanup }
  TResponse = record
  private
    FContent: TMemoryStream;
    FHeaders: TStringList;
    FJSON: TJSONData;
    FText: string;
    
    function GetText: string;
    function GetJSON: TJSONData;
  public
    StatusCode: Integer;
    
    property Headers: TStringList read FHeaders;
    property Content: TMemoryStream read FContent;
    property Text: string read GetText;
    property JSON: TJSONData read GetJSON;
    
    procedure Initialize;
    procedure Cleanup;
  end;
  
  { Result pattern for error handling }
  TRequestResult = record
    Success: Boolean;
    Response: TResponse;
    Error: string;
  end;
  
  { Request builder for fluent interface }
  TRequestBuilder = record
  private
    FURL: string;
    FMethod: string;
    FHeaders: TStringList;
    FParams: TStringList;
    FTimeout: Integer;
    FUsername: string;
    FPassword: string;
    FJSON: string;
    FData: string;
    
    function Execute: TResponse;
  public
    procedure Initialize;
    procedure Cleanup;
    
    function Get: TRequestBuilder;
    function Post: TRequestBuilder;
    function Put: TRequestBuilder;
    function Delete: TRequestBuilder;
    function Patch: TRequestBuilder;
    
    function URL(const AUrl: string): TRequestBuilder;
    function AddHeader(const Name, Value: string): TRequestBuilder;
    function AddParam(const Name, Value: string): TRequestBuilder;
    function WithTimeout(const Milliseconds: Integer): TRequestBuilder;
    function BasicAuth(const Username, Password: string): TRequestBuilder;
    function WithJSON(const JsonStr: string): TRequestBuilder;
    function WithData(const Data: string): TRequestBuilder;
    
    function Send: TResponse;
  end;
  
  { Global HTTP functions }
  THttp = record
  public
    class function NewRequest: TRequestBuilder; static;
    class function Get(const URL: string): TResponse; static;
    class function Post(const URL: string; const Data: string = ''): TResponse; static;
    class function Put(const URL: string; const Data: string = ''): TResponse; static;
    class function Delete(const URL: string): TResponse; static;
    class function PostJSON(const URL: string; const JSON: string): TResponse; static;
    
    // Result pattern versions
    class function TryGet(const URL: string): TRequestResult; static;
    class function TryPost(const URL: string; const Data: string = ''): TRequestResult; static;
  end;

const
  Http: THttp = ();
  
implementation

{ TResponse }

procedure TResponse.Initialize;
begin
  FContent := TMemoryStream.Create;
  FHeaders := TStringList.Create;
  FJSON := nil;
  FText := '';
  StatusCode := 0;
end;

procedure TResponse.Cleanup;
begin
  FContent.Free;
  FHeaders.Free;
  if Assigned(FJSON) then
    FJSON.Free;
end;

function TResponse.GetText: string;
var
  Stream: TStringStream;
begin
  if FText = '' then
  begin
    Stream := TStringStream.Create('');
    try
      FContent.Position := 0;
      Stream.LoadFromStream(FContent);
      FText := Stream.DataString;
    finally
      Stream.Free;
    end;
  end;
  Result := FText;
end;

function TResponse.GetJSON: TJSONData;
var
  Parser: TJSONParser;
begin
  if not Assigned(FJSON) then
  begin
    Parser := TJSONParser.Create(GetText);
    try
      FJSON := Parser.Parse;
    finally
      Parser.Free;
    end;
  end;
  Result := FJSON;
end;

{ TRequestBuilder }

procedure TRequestBuilder.Initialize;
begin
  FHeaders := TStringList.Create;
  FParams := TStringList.Create;
  FTimeout := 0;
  FJSON := '';
  FData := '';
end;

procedure TRequestBuilder.Cleanup;
begin
  FHeaders.Free;
  FParams.Free;
end;

function TRequestBuilder.Get: TRequestBuilder;
begin
  FMethod := 'GET';
  Result := Self;
end;

function TRequestBuilder.Post: TRequestBuilder;
begin
  FMethod := 'POST';
  Result := Self;
end;

function TRequestBuilder.Put: TRequestBuilder;
begin
  FMethod := 'PUT';
  Result := Self;
end;

function TRequestBuilder.Delete: TRequestBuilder;
begin
  FMethod := 'DELETE';
  Result := Self;
end;

function TRequestBuilder.Patch: TRequestBuilder;
begin
  FMethod := 'PATCH';
  Result := Self;
end;

function TRequestBuilder.URL(const AUrl: string): TRequestBuilder;
begin
  FURL := AUrl;
  Result := Self;
end;

function TRequestBuilder.AddHeader(const Name, Value: string): TRequestBuilder;
begin
  FHeaders.Add(Format('%s: %s', [Name, Value]));
  Result := Self;
end;

function TRequestBuilder.AddParam(const Name, Value: string): TRequestBuilder;
begin
  if FParams.Count > 0 then
    FParams.Add('&');
  FParams.Add(Format('%s=%s', [Name, Value]));
  Result := Self;
end;

function TRequestBuilder.WithTimeout(const Milliseconds: Integer): TRequestBuilder;
begin
  FTimeout := Milliseconds;
  Result := Self;
end;

function TRequestBuilder.BasicAuth(const Username, Password: string): TRequestBuilder;
begin
  FUsername := Username;
  FPassword := Password;
  Result := Self;
end;

function TRequestBuilder.WithJSON(const JsonStr: string): TRequestBuilder;
begin
  FJSON := JsonStr;
  Result := Self;
end;

function TRequestBuilder.WithData(const Data: string): TRequestBuilder;
begin
  FData := Data;
  Result := Self;
end;

function TRequestBuilder.Send: TResponse;
begin
  Result := Execute;
end;

function TRequestBuilder.Execute: TResponse;
var
  Client: TFPHTTPClient;
  RequestStream: TStringStream;
  AuthStr: string;
  I: Integer;
  FinalURL: string;
begin
  Result.Initialize;
  Client := TFPHTTPClient.Create(nil);
  RequestStream := nil;
  try
    // Set headers
    for I := 0 to FHeaders.Count - 1 do
      Client.RequestHeaders.Add(FHeaders[I]);
      
    // Set timeout
    if FTimeout > 0 then
      Client.ConnectTimeout := FTimeout;
      
    // Set auth
    if (FUsername <> '') then
    begin
      AuthStr := EncodeStringBase64(FUsername + ':' + FPassword);
      Client.RequestHeaders.Add('Authorization: Basic ' + AuthStr);
    end;
    
    // Set content type for JSON
    if FJSON <> '' then
    begin
      Client.RequestHeaders.Add('Content-Type: application/json');
      RequestStream := TStringStream.Create(FJSON);
    end
    else if FData <> '' then
    begin
      RequestStream := TStringStream.Create(FData);
    end;
    
    // Build URL with params
    FinalURL := FURL;
    if (FParams.Count > 0) and (FParams.Text <> '') then
    begin
      if Pos('?', FinalURL) > 0 then
        FinalURL := FinalURL + '&' + FParams.Text
      else
        FinalURL := FinalURL + '?' + FParams.Text;
    end;
    
    // Execute request
    try
      if Assigned(RequestStream) then
        Client.RequestBody := RequestStream;
        
      Client.HTTPMethod(FMethod, FinalURL, Result.Content, []);
      
      Result.StatusCode := Client.ResponseStatusCode;
      Result.Headers.Assign(Client.ResponseHeaders);
      
    except
      on E: Exception do
      begin
        Result.Cleanup;
        raise ETidyKitException.Create('HTTP Request Error: ' + E.Message);
      end;
    end;
    
  finally
    Client.Free;
    if Assigned(RequestStream) then
      RequestStream.Free;
  end;
end;

{ THttp }

class function THttp.NewRequest: TRequestBuilder;
begin
  Result.Initialize;
end;

class function THttp.Get(const URL: string): TResponse;
begin
  Result := NewRequest.Get.URL(URL).Send;
end;

class function THttp.Post(const URL: string; const Data: string): TResponse;
begin
  Result := NewRequest.Post.URL(URL).WithData(Data).Send;
end;

class function THttp.Put(const URL: string; const Data: string): TResponse;
begin
  Result := NewRequest.Put.URL(URL).WithData(Data).Send;
end;

class function THttp.Delete(const URL: string): TResponse;
begin
  Result := NewRequest.Delete.URL(URL).Send;
end;

class function THttp.PostJSON(const URL: string; const JSON: string): TResponse;
begin
  Result := NewRequest.Post.URL(URL).WithJSON(JSON).Send;
end;

class function THttp.TryGet(const URL: string): TRequestResult;
begin
  try
    Result.Response := Get(URL);
    Result.Success := True;
    Result.Error := '';
  except
    on E: Exception do
    begin
      Result.Success := False;
      Result.Error := E.Message;
    end;
  end;
end;

class function THttp.TryPost(const URL: string; const Data: string): TRequestResult;
begin
  try
    Result.Response := Post(URL, Data);
    Result.Success := True;
    Result.Error := '';
  except
    on E: Exception do
    begin
      Result.Success := False;
      Result.Error := E.Message;
    end;
  end;
end;

end. 
