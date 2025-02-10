# TidyKit.Request.Simple Documentation

A lightweight, memory-safe HTTP client for Free Pascal that uses advanced records for automatic cleanup. This module provides a fluent interface for making HTTP requests with built-in memory management.

## Features

- Zero-setup memory management using advanced records
- Fluent builder interface for requests
- Support for common HTTP methods (GET, POST, PUT, DELETE, PATCH)
- JSON and form data handling
- Query parameters and custom headers
- Basic authentication
- Timeout configuration
- Error handling with result pattern

## Memory Safety

This module uses Free Pascal's advanced records feature to provide automatic memory management:

- No manual initialization or cleanup needed
- Objects are automatically initialized when declared
- Memory is automatically freed when variables go out of scope
- Safe to use in try-except blocks - cleanup still happens if an exception occurs

## Basic Usage

### Simple GET Request
```pascal
var
  Response := Http.Get('https://api.example.com/data');
if Response.StatusCode = 200 then
  WriteLn(Response.Text);
// Response is automatically cleaned up when it goes out of scope
```

### POST with JSON
```pascal
var
  Response := Http.PostJSON('https://api.example.com/users',
    '{"name": "John", "age": 30}');
if Response.StatusCode = 200 then
  WriteLn(Response.JSON.FormatJSON);
// Response is automatically cleaned up
```

### Using the Builder Pattern
```pascal
var
  Response := Http.NewRequest  // Builder is automatically initialized
    .Post
    .URL('https://api.example.com/data')
    .AddHeader('X-API-Key', 'your-key')
    .AddParam('version', '1.0')
    .WithJSON('{"data": "value"}')
    .Send;  // Builder is automatically cleaned up
    
if Response.StatusCode = 200 then
  WriteLn(Response.Text);
// Response is automatically cleaned up
```

## Error Handling

### Using Try-Pattern
```pascal
var
  Result := Http.TryGet('https://api.example.com/data');
if Result.Success then
  WriteLn(Result.Response.Text)
else
  WriteLn('Error: ', Result.Error);
// Both Result and Response are automatically cleaned up
```

## API Reference

### TResponse Record
```pascal
TResponse = record
  StatusCode: Integer;
  property Text: string;              // Response body as text
  property JSON: TJSONData;           // Response parsed as JSON
  
  // These are called automatically:
  class operator Initialize(var Response: TResponse);  // Called when variable is created
  class operator Finalize(var Response: TResponse);    // Called when variable goes out of scope
end;
```

### TRequestBuilder Record
```pascal
TRequestBuilder = record
  // HTTP Methods
  function Get: TRequestBuilder;
  function Post: TRequestBuilder;
  function Put: TRequestBuilder;
  function Delete: TRequestBuilder;
  function Patch: TRequestBuilder;
  
  // Request Configuration
  function URL(const AUrl: string): TRequestBuilder;
  function AddHeader(const Name, Value: string): TRequestBuilder;
  function AddParam(const Name, Value: string): TRequestBuilder;
  function WithTimeout(const Milliseconds: Integer): TRequestBuilder;
  function BasicAuth(const Username, Password: string): TRequestBuilder;
  function WithJSON(const JsonStr: string): TRequestBuilder;
  function WithData(const Data: string): TRequestBuilder;
  
  function Send: TResponse;
  
  // These are called automatically:
  class operator Initialize(var Builder: TRequestBuilder);
  class operator Finalize(var Builder: TRequestBuilder);
end;
```

### Global HTTP Functions
```pascal
THttp = record
  class function NewRequest: TRequestBuilder;
  class function Get(const URL: string): TResponse;
  class function Post(const URL: string; const Data: string = ''): TResponse;
  class function Put(const URL: string; const Data: string = ''): TResponse;
  class function Delete(const URL: string): TResponse;
  class function PostJSON(const URL: string; const JSON: string): TResponse;
  
  class function TryGet(const URL: string): TRequestResult;
  class function TryPost(const URL: string; const Data: string = ''): TRequestResult;
end;
```

## Advanced Usage Examples

### Complex Request with Multiple Headers and Parameters
```pascal
var
  Response := Http.NewRequest
    .Post
    .URL('https://api.example.com/users')
    .AddHeader('X-API-Key', 'your-key')
    .AddHeader('Accept', 'application/json')
    .AddParam('version', '2.0')
    .AddParam('format', 'detailed')
    .WithJSON('{"name": "John", "email": "john@example.com"}')
    .WithTimeout(5000)
    .Send;

if Response.StatusCode = 201 then
  WriteLn('User created: ', Response.JSON.FindPath('id').AsString);
```

### Authenticated Request with Error Handling
```pascal
var
  Result := Http.NewRequest
    .Get
    .URL('https://api.example.com/secure')
    .BasicAuth('username', 'password')
    .WithTimeout(3000)
    .Send;

case Result.StatusCode of
  200: WriteLn('Success: ', Result.Text);
  401: WriteLn('Authentication failed');
  403: WriteLn('Access denied');
  else WriteLn('Error: ', Result.StatusCode);
end;
```

### Form Data Submission
```pascal
var
  Response := Http.NewRequest
    .Post
    .URL('https://api.example.com/submit')
    .AddHeader('Content-Type', 'application/x-www-form-urlencoded')
    .WithData('name=John&age=30&email=john@example.com')
    .Send;

if Response.StatusCode = 200 then
  WriteLn('Form submitted successfully');
```

## Best Practices

1. Let the automatic memory management work for you - don't try to manually manage memory
2. Always check StatusCode before accessing response data
3. Use TryGet/TryPost for better error handling
4. Set appropriate timeouts for your use case
5. Use the builder pattern for complex requests
6. Take advantage of the fluent interface for better readability 