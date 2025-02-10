# TidyKit.Request Documentation

TidyKit.Request is a simple yet powerful HTTP client for Free Pascal. It provides an easy-to-use interface for making HTTP requests with support for various HTTP methods, request options, and response handling.

## Table of Contents
- [Basic Usage](#basic-usage)
- [HTTP Methods](#http-methods)
- [Request Options](#request-options)
- [Response Handling](#response-handling)
- [Examples](#examples)

## Basic Usage

```pascal
uses
  TidyKit;

var
  RequestKit: TRequestKit;
  Response: TResponse;
  Options: TRequestOptions;
begin
  RequestKit := TRequestKit.Create;
  try
    Options := Default(TRequestOptions);
    Response := RequestKit.Get('https://api.example.com/data', Options);
    try
      WriteLn('Status Code: ', Response.StatusCode);
      WriteLn('Response Body: ', Response.Text);
    finally
      Response.Free;
    end;
  finally
    RequestKit.Free;
  end;
end;
```

## HTTP Methods

TidyKit.Request supports the following HTTP methods:
- `Get` - HTTP GET request
- `Post` - HTTP POST request
- `Put` - HTTP PUT request
- `Delete` - HTTP DELETE request
- `Patch` - HTTP PATCH request
- `Head` - HTTP HEAD request
- `SendOptions` - HTTP OPTIONS request

## Request Options

The `TRequestOptions` record provides various options to customize your HTTP requests:

```pascal
TRequestOptions = record
  Headers: array of string;      // Custom headers
  ParamString: string;          // Query parameters
  Data: string;                 // Request body for form data
  JSON: string;                 // Request body for JSON data
  Auth: array[0..1] of string;  // Basic auth credentials [username, password]
  Timeout: Integer;             // Request timeout in milliseconds
  VerifySSL: Boolean;          // SSL verification flag
end;
```

### Setting Headers
```pascal
Options := Default(TRequestOptions);
SetLength(Options.Headers, 2);
Options.Headers[0] := 'X-API-Key: your-api-key';
Options.Headers[1] := 'User-Agent: TidyKit-Client';
```

### Query Parameters
```pascal
Options := Default(TRequestOptions);
Options.ParamString := 'page=1&limit=10';  // Will be appended to URL
```

### Form Data
```pascal
Options := Default(TRequestOptions);
SetLength(Options.Headers, 1);
Options.Headers[0] := 'Content-Type: application/x-www-form-urlencoded';
Options.Data := 'name=John&age=30';
```

### JSON Data
```pascal
Options := Default(TRequestOptions);
Options.JSON := '{"name": "John", "age": 30}';  // Content-Type is set automatically
```

### Basic Authentication
```pascal
Options := Default(TRequestOptions);
Options.Auth[0] := 'username';
Options.Auth[1] := 'password';
```

### Timeout
```pascal
Options := Default(TRequestOptions);
Options.Timeout := 5000;  // 5 seconds timeout
```

## Response Handling

The `TResponse` class provides access to the HTTP response:

- `StatusCode: Integer` - HTTP status code
- `Headers: TStringList` - Response headers
- `Text: string` - Response body as text
- `JSON: TJSONData` - Response body parsed as JSON
- `Content: TMemoryStream` - Raw response body

```pascal
Response := RequestKit.Get(URL, Options);
try
  if Response.StatusCode = 200 then
  begin
    // Access response as text
    WriteLn(Response.Text);
    
    // Access response as JSON
    if Response.JSON <> nil then
      WriteLn(Response.JSON.FormatJSON);
      
    // Access specific header
    WriteLn(Response.Headers.Values['Content-Type']);
  end;
finally
  Response.Free;
end;
```

## Examples

### GET Request with Query Parameters
```pascal
var
  RequestKit: TRequestKit;
  Response: TResponse;
  Options: TRequestOptions;
begin
  RequestKit := TRequestKit.Create;
  try
    Options := Default(TRequestOptions);
    Options.ParamString := 'page=1&limit=10';
    
    Response := RequestKit.Get('https://api.example.com/users', Options);
    try
      if Response.StatusCode = 200 then
        WriteLn(Response.JSON.FormatJSON);
    finally
      Response.Free;
    end;
  finally
    RequestKit.Free;
  end;
end;
```

### POST Request with JSON Body
```pascal
var
  RequestKit: TRequestKit;
  Response: TResponse;
  Options: TRequestOptions;
begin
  RequestKit := TRequestKit.Create;
  try
    Options := Default(TRequestOptions);
    Options.JSON := '{"name": "John Doe", "email": "john@example.com"}';
    
    Response := RequestKit.Post('https://api.example.com/users', Options);
    try
      if Response.StatusCode = 201 then
        WriteLn('User created successfully');
    finally
      Response.Free;
    end;
  finally
    RequestKit.Free;
  end;
end;
```

### Authenticated Request
```pascal
var
  RequestKit: TRequestKit;
  Response: TResponse;
  Options: TRequestOptions;
begin
  RequestKit := TRequestKit.Create;
  try
    Options := Default(TRequestOptions);
    Options.Auth[0] := 'username';
    Options.Auth[1] := 'password';
    SetLength(Options.Headers, 1);
    Options.Headers[0] := 'X-API-Key: your-api-key';
    
    Response := RequestKit.Get('https://api.example.com/secure', Options);
    try
      if Response.StatusCode = 200 then
        WriteLn(Response.Text);
    finally
      Response.Free;
    end;
  finally
    RequestKit.Free;
  end;
end;
```

### Error Handling
```pascal
var
  RequestKit: TRequestKit;
  Response: TResponse;
  Options: TRequestOptions;
begin
  RequestKit := TRequestKit.Create;
  try
    Options := Default(TRequestOptions);
    Response := nil;
    
    try
      Response := RequestKit.Get('https://api.example.com/data', Options);
      case Response.StatusCode of
        200..299: WriteLn('Success: ', Response.Text);
        401: WriteLn('Unauthorized');
        404: WriteLn('Not Found');
        500..599: WriteLn('Server Error');
      else
        WriteLn('Unexpected Status: ', Response.StatusCode);
      end;
    except
      on E: ETidyKitException do
        WriteLn('Request Error: ', E.Message);
    end;
  finally
    Response.Free;
    RequestKit.Free;
  end;
end;
```

Remember to always free your `TRequestKit` and `TResponse` objects to prevent memory leaks. Using a try-finally block is recommended. 