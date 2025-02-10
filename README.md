# TidyKit

A collection of utility libraries for Free Pascal, designed for simplicity, safety, and ease of use.

## Features

- **HTTP Client**: Memory-safe HTTP client with fluent interface ([docs](docs/TidyKit.Request.Simple.md))
- **File System**: File and directory operations
- **String Utilities**: String manipulation and pattern matching
- **Date/Time**: Date and time handling with timezone support
- **Cryptography**: Encryption, hashing, and encoding utilities

## Installation

1. Clone the repository:
```bash
git clone https://github.com/yourusername/TidyKit.git
```

2. Add the source directory to your project's search path.

## Quick Start

### HTTP Client
```pascal
uses
  TidyKit;

// Simple GET request
var
  Response: TResponse;
begin
  Response := Http.Get('https://api.example.com/data');
  try
    if Response.StatusCode = 200 then
      WriteLn(Response.Text);
  finally
    Response.Cleanup;
  end;
end;

// POST with JSON using builder pattern
var
  Builder: TRequestBuilder;
  Response: TResponse;
begin
  Builder.Initialize;
  try
    Builder := Http.NewRequest
      .Post
      .URL('https://api.example.com/users')
      .AddHeader('X-API-Key', 'your-key')
      .WithJSON('{"name": "John"}');
      
    Response := Builder.Send;
    try
      if Response.StatusCode = 200 then
        WriteLn(Response.JSON.FormatJSON);
    finally
      Response.Cleanup;
    end;
  finally
    Builder.Cleanup;
  end;
end;
```

### File Operations
```pascal
uses
  TidyKit;

var
  FileKit: TFileKit;
begin
  FileKit := TFileKit.Create;
  try
    FileKit.WriteText('file.txt', 'Hello World');
    WriteLn(FileKit.ReadText('file.txt'));
  finally
    FileKit.Free;
  end;
end;
```

## Documentation

- [HTTP Client](docs/TidyKit.Request.Simple.md)
- [File System](docs/TidyKit.FS.md)
- [String Utilities](docs/TidyKit.Strings.md)
- [Date/Time](docs/TidyKit.DateTime.md)
- [Cryptography](docs/TidyKit.Crypto.md)

## Memory Management

TidyKit uses both classes and advanced records for different purposes:

### Classes (Traditional Objects)
- Must be created with Create and freed with Free
- Always use try-finally blocks
- Example: TFileKit, TCryptoKit

### Advanced Records (HTTP Client)
- Must call Initialize before use
- Must call Cleanup after use
- Always use try-finally blocks
- Example: TResponse, TRequestBuilder

## Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add some amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.