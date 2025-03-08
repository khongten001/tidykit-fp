unit TidyKit.Request;

{$mode objfpc}{$H+}{$J-}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, fphttpclient, opensslsockets, base64,
  URIParser, HTTPDefs, TidyKit.Core, TidyKit.JSON;

type
  { Exception class for HTTP request operations }
  ERequestError = class(Exception);

  { Response record with automatic memory management }
  TResponse = record
  private
    FContent: string;
    FHeaders: string;
    FJSON: IJSONValue;  // Changed to IJSONValue interface
    
    function GetText: string;
    function GetJSON: IJSONValue;  // Changed return type
  public
    StatusCode: Integer;
    
    property Text: string read GetText;
    property JSON: IJSONValue read GetJSON;  // Changed property type
    
    { Management operators for automatic initialization/cleanup }
    
    { Initialize is called automatically when:
      - A variable of this type is declared
      - Memory for this type is allocated
      - This type is used as a field in another record
      No need to call this manually }
    class operator Initialize(var Response: TResponse);
    
    { Finalize is called automatically when:
      - A variable goes out of scope
      - Memory for this type is freed
      - The containing record is finalized
      This ensures FJSON is properly freed }
    class operator Finalize(var Response: TResponse);
  end;
  
  { Result pattern for error handling }
  TRequestResult = record
    Success: Boolean;
    Response: TResponse;  // Will be automatically initialized and finalized
    Error: string;
  end;
  
  { HTTP request with fluent interface and automatic memory management }
  THttpRequest = record
  private
    FURL: string;
    FMethod: string;
    FHeaders: string;
    FParams: string;
    FTimeout: Integer;
    FUsername: string;
    FPassword: string;
    FJSON: string;
    FData: string;
    
    function Execute: TResponse;
  public
    { Management operators for automatic initialization/cleanup }
    
    { Initialize is called automatically when a request is created.
      Sets all fields to their default empty state.
      This happens automatically for local variables and fields }
    class operator Initialize(var Request: THttpRequest);
    
    { Finalize is called automatically when a request goes out of scope.
      Currently empty as all fields are managed types }
    class operator Finalize(var Request: THttpRequest);
    
    function Get: THttpRequest;
    function Post: THttpRequest;
    function Put: THttpRequest;
    function Delete: THttpRequest;
    function Patch: THttpRequest;
    
    function URL(const AUrl: string): THttpRequest;
    function AddHeader(const Name, Value: string): THttpRequest;
    function AddParam(const Name, Value: string): THttpRequest;
    function WithTimeout(const Milliseconds: Integer): THttpRequest;
    function BasicAuth(const Username, Password: string): THttpRequest;
    function WithJSON(const JsonStr: string): THttpRequest;
    function WithData(const Data: string): THttpRequest;
    
    function Send: TResponse;
  end;
  
  { Global HTTP functions }
  THttp = record
    { Simple one-line request methods }
    class function Get(const URL: string): TResponse; static;
    class function Post(const URL: string; const Data: string = ''): TResponse; static;
    class function Put(const URL: string; const Data: string = ''): TResponse; static;
    class function Delete(const URL: string): TResponse; static;
    class function PostJSON(const URL: string; const JSON: string): TResponse; static;
    
    { Error handling variants }
    class function TryGet(const URL: string): TRequestResult; static;
    class function TryPost(const URL: string; const Data: string = ''): TRequestResult; static;
  end;

const
  Http: THttp = ();
  
implementation

{ TResponse }

class operator TResponse.Initialize(var Response: TResponse);
begin
  // Called automatically when a TResponse is created
  Response.FContent := '';
  Response.FHeaders := '';
  Response.FJSON := nil;  // Interface will be created on-demand in GetJSON
  Response.StatusCode := 0;
end;

class operator TResponse.Finalize(var Response: TResponse);
begin
  // Called automatically when a TResponse goes out of scope
  // No need to free FJSON - interface reference counting handles cleanup
  Response.FJSON := nil;
end;

function TResponse.GetText: string;
begin
  Result := FContent;
end;

function TResponse.GetJSON: IJSONValue;
begin
  // Lazy initialization of JSON - only parse when needed
  if not Assigned(FJSON) and (FContent <> '') then
  begin
    try
      FJSON := TJSON.Parse(FContent);  // Use our JSON parser
    except
      on E: Exception do
        raise ERequestError.Create('JSON Parse Error: ' + E.Message);
    end;
  end;
  Result := FJSON;
end;

{ THttpRequest }

class operator THttpRequest.Initialize(var Request: THttpRequest);
begin
  // Called automatically when a THttpRequest is created
  Request.FHeaders := '';
  Request.FParams := '';
  Request.FTimeout := 0;
  Request.FJSON := '';
  Request.FData := '';
end;

class operator THttpRequest.Finalize(var Request: THttpRequest);
begin
  // Called automatically when a THttpRequest goes out of scope
  // All fields are managed types (string), so no manual cleanup needed
end;

function THttpRequest.Get: THttpRequest;
begin
  FMethod := 'GET';
  Result := Self;
end;

function THttpRequest.Post: THttpRequest;
begin
  FMethod := 'POST';
  Result := Self;
end;

function THttpRequest.Put: THttpRequest;
begin
  FMethod := 'PUT';
  Result := Self;
end;

function THttpRequest.Delete: THttpRequest;
begin
  FMethod := 'DELETE';
  Result := Self;
end;

function THttpRequest.Patch: THttpRequest;
begin
  FMethod := 'PATCH';
  Result := Self;
end;

function THttpRequest.URL(const AUrl: string): THttpRequest;
begin
  FURL := AUrl;
  Result := Self;
end;

function THttpRequest.AddHeader(const Name, Value: string): THttpRequest;
begin
  if FHeaders <> '' then
    FHeaders := FHeaders + #13#10;
  FHeaders := FHeaders + Format('%s: %s', [Name, Value]);
  Result := Self;
end;

function THttpRequest.AddParam(const Name, Value: string): THttpRequest;
begin
  if FParams <> '' then
    FParams := FParams + '&';
  FParams := FParams + Format('%s=%s', [Name, Value]);
  Result := Self;
end;

function THttpRequest.WithTimeout(const Milliseconds: Integer): THttpRequest;
begin
  FTimeout := Milliseconds;
  Result := Self;
end;

function THttpRequest.BasicAuth(const Username, Password: string): THttpRequest;
begin
  FUsername := Username;
  FPassword := Password;
  Result := Self;
end;

function THttpRequest.WithJSON(const JsonStr: string): THttpRequest;
begin
  FJSON := JsonStr;
  Result := Self;
end;

function THttpRequest.WithData(const Data: string): THttpRequest;
begin
  FData := Data;
  Result := Self;
end;

function THttpRequest.Send: TResponse;
begin
  Result := Execute;
end;

function THttpRequest.Execute: TResponse;
var
  Client: TFPHTTPClient;
  RequestStream: TStringStream;
  ResponseStream: TMemoryStream;
  ContentStream: TStringStream;
  AuthStr: string;
  HeaderLines: TStringList;
  I: Integer;
  FinalURL: string;
begin
  // Result is automatically initialized by the class operator
  Client := TFPHTTPClient.Create(nil);
  RequestStream := nil;
  ResponseStream := TMemoryStream.Create;
  HeaderLines := TStringList.Create;
  try
    // Set headers
    if FHeaders <> '' then
    begin
      HeaderLines.Text := FHeaders;
      for I := 0 to HeaderLines.Count - 1 do
        Client.RequestHeaders.Add(HeaderLines[I]);
    end;
      
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
      Client.RequestHeaders.Add('Content-Type: application/x-www-form-urlencoded');
      RequestStream := TStringStream.Create(FData);
    end;
    
    // Build URL with params
    FinalURL := FURL;
    if FParams <> '' then
    begin
      if Pos('?', FinalURL) > 0 then
        FinalURL := FinalURL + '&' + FParams
      else
        FinalURL := FinalURL + '?' + FParams;
    end;
    
    // Execute request
    try
      if Assigned(RequestStream) then
        Client.RequestBody := RequestStream;
        
      Client.HTTPMethod(FMethod, FinalURL, ResponseStream, []);
      
      Result.StatusCode := Client.ResponseStatusCode;
      Result.FHeaders := Client.ResponseHeaders.Text;
      
      // Convert response to string
      ContentStream := TStringStream.Create('');
      try
        ResponseStream.Position := 0;
        ContentStream.LoadFromStream(ResponseStream);
        Result.FContent := ContentStream.DataString;
      finally
        ContentStream.Free;
      end;
      
    except
      on E: Exception do
      begin
        // Result is already initialized, just clear it
        Result.FContent := '';
        Result.FHeaders := '';
        Result.FJSON := nil;
        Result.StatusCode := 0;
        raise ERequestError.Create('HTTP Request Error: ' + E.Message);
      end;
    end;
    
  finally
    HeaderLines.Free;
    ResponseStream.Free;
    Client.Free;
    if Assigned(RequestStream) then
      RequestStream.Free;
  end;
end;

{ THttp }

class function THttp.Get(const URL: string): TResponse;
var
  Builder: THttpRequest;  // Initialize is called automatically
begin
  Result := Builder.Get.URL(URL).Send;
end;

class function THttp.Post(const URL: string; const Data: string): TResponse;
var
  Builder: THttpRequest;  // Initialize is called automatically
begin
  Result := Builder.Post.URL(URL).WithData(Data).Send;
end;

class function THttp.Put(const URL: string; const Data: string): TResponse;
var
  Builder: THttpRequest;  // Initialize is called automatically
begin
  Result := Builder.Put.URL(URL).WithData(Data).Send;
end;

class function THttp.Delete(const URL: string): TResponse;
var
  Builder: THttpRequest;  // Initialize is called automatically
begin
  Result := Builder.Delete.URL(URL).Send;
end;

class function THttp.PostJSON(const URL: string; const JSON: string): TResponse;
var
  Builder: THttpRequest;  // Initialize is called automatically
begin
  Result := Builder.Post.URL(URL).WithJSON(JSON).Send;
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
