# TidyKit.Request Documentation

A lightweight, memory-safe HTTP client for Free Pascal that uses advanced records for automatic cleanup. This module provides a fluent interface for making HTTP requests with built-in memory management and seamless JSON integration through TidyKit.JSON.

## Table of Contents

- [TidyKit.Request Documentation](#tidykitrequest-documentation)
  - [Table of Contents](#table-of-contents)
  - [Features](#features)
  - [Design Philosophy](#design-philosophy)
    - [Fluent Interface Pattern](#fluent-interface-pattern)
  - [Memory Safety](#memory-safety)
  - [Basic Usage](#basic-usage)
    - [Simple GET Request](#simple-get-request)
    - [Working with JSON](#working-with-json)
    - [Using the Fluent Interface](#using-the-fluent-interface)
  - [Error Handling](#error-handling)
    - [Using Try-Pattern](#using-try-pattern)
  - [API Reference](#api-reference)
    - [TResponse Record](#tresponse-record)
    - [TRequestBuilder Record](#trequestbuilder-record)
    - [Global HTTP Functions](#global-http-functions)
  - [Advanced Usage Examples](#advanced-usage-examples)
    - [Complex Request with Multiple Headers and Parameters](#complex-request-with-multiple-headers-and-parameters)
    - [Authenticated Request with Error Handling](#authenticated-request-with-error-handling)
    - [Form Data Submission](#form-data-submission)
  - [Best Practices](#best-practices)


## Features

- Zero-setup memory management using advanced records
- Fluent interface for expressive request building
- Support for common HTTP methods (GET, POST, PUT, DELETE, PATCH)
- Seamless JSON integration with TidyKit.JSON
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
  Request: TRequestBuilder;  // Automatically initialized when declared
  UserData: IJSONObject;
  Response: TResponse;
begin
  UserData := TJSON.Obj;
  UserData.Add('name', 'John');
  UserData.Add('email', 'john@example.com');

  Response := Request
    .Post                                // Chain HTTP method
    .URL('https://api.example.com')      // Chain URL
    .AddHeader('X-API-Key', 'your-key')  // Chain headers
    .WithJSON(UserData.ToString)         // Chain JSON body
    .Send;                               // Execute
end;
```

2. **Readable, SQL-like Syntax**: The API reads almost like English:
```pascal
var
  Request: TRequestBuilder;  // Automatically initialized when declared
  Response: TResponse;
begin
  Response := Request
    .Get
    .URL('https://api.example.com/secure')
    .BasicAuth('username', 'password')
    .WithTimeout(5000)
    .Send;
end;
```

3. **Context Preservation**: Each method call preserves and adds to the context, with the final state resolved only when `Send` is called.

4. **Self-Documenting Code**: The fluent interface makes the intent clear and reduces the need for additional documentation.

## Memory Safety

This module uses Free Pascal's advanced records feature to provide automatic memory management:

- No manual initialization or cleanup needed
- Objects are automatically initialized when declared
- Memory is automatically freed when variables go out of scope
- Safe to use in try-except blocks - cleanup still happens if an exception occurs
- JSON responses are managed through interface references

## Basic Usage

### Simple GET Request
```pascal
var
  Response: TResponse;
  Data: IJSONObject;
begin
  Response := Http.Get('https://api.example.com/data');
  if Response.StatusCode = 200 then
  begin
    Data := Response.JSON.AsObject;
    WriteLn('ID: ', Data['id'].AsInteger);
    WriteLn('Name: ', Data['name'].AsString);
  end;
end;
```

### Working with JSON
```pascal
var
  Response: TResponse;
  UserData: IJSONObject;
begin
  // Create JSON request data
  UserData := TJSON.Obj;
  UserData.Add('name', 'John Smith');
  UserData.Add('age', 30);
  UserData.Add('email', 'john@example.com');

  // Send JSON request
  Response := Http.PostJSON('https://api.example.com/users',
    UserData.ToString);

  // Handle JSON response
  if Response.StatusCode = 201 then
  begin
    WriteLn('User created with ID: ', Response.JSON.AsObject['id'].AsString);
    WriteLn('Created at: ', Response.JSON.AsObject['created_at'].AsString);
  end;
end;
```

### Using the Fluent Interface
```pascal
var
  Request: TRequestBuilder;  // Automatically initialized when declared
  Response: TResponse;
  UserData, ResponseData: IJSONObject;
begin
  // Create request JSON
  UserData := TJSON.Obj;
  UserData.Add('name', 'John');
  UserData.Add('email', 'john@example.com');

  Response := Request
    .Post                                // Chain HTTP method
    .URL('https://api.example.com')      // Chain URL
    .AddHeader('X-API-Key', 'your-key')  // Chain headers
    .WithJSON(UserData.ToString)         // Chain JSON body
    .Send;                               // Execute

  if Response.StatusCode = 200 then
  begin
    ResponseData := Response.JSON.AsObject;
    WriteLn('Success: ', ResponseData['success'].AsBoolean);
    WriteLn('Message: ', ResponseData['message'].AsString);
  end;
end;
```

## Error Handling

The TidyKit.Request module uses a dedicated exception class, `ERequestError`, for handling HTTP-related errors. This allows you to specifically catch HTTP request errors while letting other types of exceptions propagate as normal.

```pascal
try
  Response := Http.Get('https://api.example.com/data');
  // Use the response...
except
  on E: ERequestError do
    // Handle HTTP-specific errors (timeouts, connection problems, etc.)
    WriteLn('HTTP Error: ', E.Message);
  on E: Exception do
    // Handle other types of errors
    WriteLn('General Error: ', E.Message);
end;
```

### Using Try-Pattern
For a more functional approach, you can use the built-in Try-pattern methods that handle exceptions for you:

```pascal
var
  Result: TRequestResult;
begin
  Result := Http.TryGet('https://api.example.com/data');
  if Result.Success then
  begin
    WriteLn('Status: ', Result.Response.JSON.AsObject['status'].AsString);
    WriteLn('Data: ', Result.Response.JSON.AsObject['data'].AsString);
  end
  else
    WriteLn('Error: ', Result.Error);
```

## API Reference

### TResponse Record
```pascal
TResponse = record
  StatusCode: Integer;
  property Text: string;              // Response body as text
  property JSON: IJSONValue;          // Response parsed as JSON
  
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
  Response: TResponse;
  UserData: IJSONObject;
  CreatedUser: IJSONObject;
begin
  // Create request data
  UserData := TJSON.Obj;
  UserData.Add('name', 'John');
  UserData.Add('email', 'john@example.com');

  Response := Request
    .Post
    .URL('https://api.example.com/users')
    .AddHeader('X-API-Key', 'your-key')
    .AddHeader('Accept', 'application/json')
    .AddParam('version', '2.0')
    .AddParam('format', 'detailed')
    .WithJSON(UserData.ToString)
    .WithTimeout(5000)
    .Send;

  if Response.StatusCode = 201 then
  begin
    CreatedUser := Response.JSON.AsObject;
    WriteLn('User created with ID: ', CreatedUser['id'].AsString);
    WriteLn('Created at: ', CreatedUser['created_at'].AsString);
  end;
end;
```

### Authenticated Request with Error Handling
```pascal
var
  Request: TRequestBuilder;
  Response: TResponse;
  SecureData: IJSONObject;
begin
  Response := Request
    .Get
    .URL('https://api.example.com/secure')
    .BasicAuth('username', 'password')
    .WithTimeout(3000)
    .Send;

  if Response.StatusCode = 200 then
  begin
    SecureData := Response.JSON.AsObject;
    WriteLn('Access granted to: ', SecureData['resource'].AsString);
  end;
end;
```

### Form Data Submission
```pascal
var
  Request: TRequestBuilder;
  Response: TResponse;
begin
  Response := Request
    .Post
    .URL('https://api.example.com/submit')
    .AddHeader('Content-Type', 'application/x-www-form-urlencoded')
    .WithData('name=John&age=30&email=john@example.com')
    .Send;

  if Response.StatusCode = 200 then
    WriteLn('Form submitted successfully');
end;
```

## Best Practices

1. Let the automatic memory management work for you - don't try to manually manage memory
2. Always check StatusCode before accessing response data
3. Use TryGet/TryPost for better error handling
4. Set appropriate timeouts for your use case
5. Take advantage of the fluent interface for complex requests
6. Let the code read like natural language descriptions 