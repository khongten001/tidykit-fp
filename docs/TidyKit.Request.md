# TidyKit.Request Documentation

A lightweight, memory-safe HTTP client for Free Pascal that uses advanced records for automatic cleanup. This module provides a fluent interface for making HTTP requests with built-in memory management.

## Features

- Zero-setup memory management using advanced records
- Fluent interface for expressive request building
- Support for common HTTP methods (GET, POST, PUT, DELETE, PATCH)
- JSON and form data handling
- Query parameters and custom headers
- Basic authentication
- Timeout configuration
- Error handling with result pattern

## Design Philosophy

### Fluent Interface Pattern
This module implements a fluent interface pattern, which is a specific form of the builder pattern that emphasizes:

1. **Method Chaining**: Each method returns the request itself, allowing for a chain of method calls:
```pascal
var
  Request: TRequestBuilder;  // Automatically initialized
  Response := Request
    .Post                                // Chain HTTP method
    .URL('https://api.example.com')      // Chain URL
    .AddHeader('X-API-Key', 'your-key')  // Chain headers
    .WithJSON('{"name": "John"}')        // Chain body
    .Send;                               // Execute
```

2. **Readable, SQL-like Syntax**: The API reads almost like English:
```pascal
var
  Request: TRequestBuilder;  // Automatically initialized
  Response := Request
    .Get
    .URL('https://api.example.com/secure')
    .BasicAuth('username', 'password')
    .WithTimeout(5000)
    .Send;
```

3. **Context Preservation**: Each method call preserves and adds to the context, with the final state resolved only when `Send` is called.

4. **Self-Documenting Code**: The fluent interface makes the intent clear and reduces the need for additional documentation.

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
```

### POST with JSON
```pascal
var
  Response := Http.PostJSON('https://api.example.com/users',
    '{"name": "John", "age": 30}');
if Response.StatusCode = 200 then
  WriteLn(Response.JSON.FormatJSON);
```

### Using the Fluent Interface
```pascal
var
  Request: TRequestBuilder;  // Automatically initialized
  Response := Request       // Start building the request
    .Post
    .URL('https://api.example.com/data')
    .AddHeader('X-API-Key', 'your-key')
    .AddParam('version', '1.0')
    .WithJSON('{"data": "value"}')
    .Send;                 // Execute the request
    
if Response.StatusCode = 200 then
  WriteLn(Response.Text);
```

The fluent interface above makes the request construction both readable and maintainable:
- The request is automatically initialized when declared
- Each method call is chained to the next
- The code reads like a natural language description
- The state is built up step by step
- All cleanup is handled automatically by Free Pascal's advanced records feature

## Error Handling

### Using Try-Pattern
```pascal
var
  Result := Http.TryGet('https://api.example.com/data');
if Result.Success then
  WriteLn(Result.Response.Text)
else
  WriteLn('Error: ', Result.Error);
```

## API Reference

### TResponse Record
```pascal
TResponse = record
  StatusCode: Integer;
  property Text: string;              // Response body as text
  property JSON: TJSONData;           // Response parsed as JSON
  
  // Memory management (called automatically)
  class operator Initialize(var Response: TResponse);  // Called when variable is created
  class operator Finalize(var Response: TResponse);    // Called when variable goes out of scope
end;
```

### TRequestBuilder Record
```pascal
TRequestBuilder = record
  // HTTP Methods (each returns Self for chaining)
  function Get: TRequestBuilder;
  function Post: TRequestBuilder;
  function Put: TRequestBuilder;
  function Delete: TRequestBuilder;
  function Patch: TRequestBuilder;
  
  // Request Configuration (each returns Self for chaining)
  function URL(const AUrl: string): TRequestBuilder;
  function AddHeader(const Name, Value: string): TRequestBuilder;
  function AddParam(const Name, Value: string): TRequestBuilder;
  function WithTimeout(const Milliseconds: Integer): TRequestBuilder;
  function BasicAuth(const Username, Password: string): TRequestBuilder;
  function WithJSON(const JsonStr: string): TRequestBuilder;
  function WithData(const Data: string): TRequestBuilder;
  
  // Execute the request
  function Send: TResponse;
  
  // Memory management (called automatically)
  class operator Initialize(var Request: TRequestBuilder);
  class operator Finalize(var Request: TRequestBuilder);
end;
```

### Global HTTP Functions
```pascal
THttp = record
  // Simple one-line request methods
  class function Get(const URL: string): TResponse;
  class function Post(const URL: string; const Data: string = ''): TResponse;
  class function Put(const URL: string; const Data: string = ''): TResponse;
  class function Delete(const URL: string): TResponse;
  class function PostJSON(const URL: string; const JSON: string): TResponse;
  
  // Error handling variants
  class function TryGet(const URL: string): TRequestResult;
  class function TryPost(const URL: string; const Data: string = ''): TRequestResult;
end;
```

The global `Http` constant of type `THttp` provides convenient one-liner methods for simple requests. For more complex requests, declare a `TRequestBuilder` variable and use the fluent interface.

## Advanced Usage Examples

### Complex Request with Multiple Headers and Parameters
```pascal
var
  Request: TRequestBuilder;
  Response := Request
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
  Request: TRequestBuilder;
  Response := Request
    .Get
    .URL('https://api.example.com/secure')
    .BasicAuth('username', 'password')
    .WithTimeout(3000)
    .Send;

case Response.StatusCode of
  200: WriteLn('Success: ', Response.Text);
  401: WriteLn('Authentication failed');
  403: WriteLn('Access denied');
  else WriteLn('Error: ', Response.StatusCode);
end;
```

### Form Data Submission
```pascal
var
  Request: TRequestBuilder;
  Response := Request
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
5. Take advantage of the fluent interface for complex requests
6. Let the code read like natural language descriptions 