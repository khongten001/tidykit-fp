# TidyKit.Request Simplified Design Proposal

This is a proposal for a more developer-friendly version of TidyKit.Request that uses advanced records for automatic cleanup.

## Key Improvements

1. Use advanced records for automatic cleanup
2. Provide a global request function for one-liners
3. Add builder pattern for request options
4. Simplify response handling

## Proposed Design

### Simple One-liner Requests
```pascal
// Simple GET request
var Response := Http.Get('https://api.example.com/data');
WriteLn(Response.Text);

// Simple POST with JSON
var Response := Http.Post('https://api.example.com/users', '{"name": "John"}');
WriteLn(Response.StatusCode);
```

### Advanced Record for Response
```pascal
TResponse = record
private
  FContent: TMemoryStream;
  FHeaders: TStringList;
  FJSON: TJSONData;
  
  procedure Initialize;
  procedure Cleanup;
public
  StatusCode: Integer;
  
  // These properties handle cleanup automatically
  property Headers: TStringList read FHeaders;
  property Content: TMemoryStream read FContent;
  property Text: string read GetText;
  property JSON: TJSONData read GetJSON;
  
  class operator Initialize(var Response: TResponse);
  class operator Finalize(var Response: TResponse);
end;
```

### Builder Pattern for Options
```pascal
var
  Response := Http.NewRequest
    .Get
    .URL('https://api.example.com/data')
    .AddHeader('X-API-Key', 'your-key')
    .AddParam('page', '1')
    .AddParam('limit', '10')
    .WithTimeout(5000)
    .Execute;
    
WriteLn(Response.Text);  // Response freed automatically
```

### Simplified Authentication
```pascal
var
  Response := Http.NewRequest
    .Get
    .URL('https://api.example.com/secure')
    .BasicAuth('username', 'password')
    .Execute;
```

### JSON Requests Made Simple
```pascal
type
  TUser = record
    Name: string;
    Age: Integer;
  end;

var
  User: TUser;
  Response: TResponse;
  
  // Sending JSON
  User.Name := 'John';
  User.Age := 30;
  Response := Http.PostJSON('https://api.example.com/users', User);
  
  // Reading JSON
  if Response.StatusCode = 200 then
    User := Response.JSON.Parse<TUser>;
```

### Error Handling with Result Pattern
```pascal
type
  TRequestResult = record
    Success: Boolean;
    Response: TResponse;
    Error: string;
  end;

// Using result pattern
var
  Result := Http.TryGet('https://api.example.com/data');
  if Result.Success then
    WriteLn(Result.Response.Text)
  else
    WriteLn('Error: ', Result.Error);
```

## Implementation Benefits

1. **Automatic Cleanup**
   - No need to manually free responses
   - Memory management handled by record finalizers
   - Safer for new developers

2. **Simplified Usage**
   - One-liner methods for common operations
   - Fluent interface for complex requests
   - Type-safe JSON serialization

3. **Better Error Handling**
   - Result pattern prevents exceptions
   - Clear error messages
   - Type-safe responses

## Example Usage Comparison

### Before:
```pascal
var
  RequestKit: TRequestKit;
  Response: TResponse;
  Options: TRequestOptions;
begin
  RequestKit := TRequestKit.Create;
  try
    Options := Default(TRequestOptions);
    Options.JSON := '{"name": "John"}';
    Response := RequestKit.Post('https://api.example.com/users', Options);
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

### After:
```pascal
var
  Response := Http.Post('https://api.example.com/users', '{"name": "John"}');
  if Response.StatusCode = 200 then
    WriteLn(Response.Text);
```

## Implementation Notes

1. Use `TMemoryStream` and `TStringList` as fields in the record
2. Implement proper Initialize/Finalize operators
3. Use generics for JSON serialization
4. Implement thread-safe request handling
5. Consider connection pooling for performance
6. Add request/response logging capabilities

## Migration Strategy

1. Create new unit `TidyKit.Request.Simple`
2. Keep existing implementation for backward compatibility
3. Gradually deprecate class-based implementation
4. Provide migration guide and tools 