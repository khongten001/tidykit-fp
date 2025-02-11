# TidyKit

A collection of utility libraries for Free Pascal, designed for simplicity, safety, and ease of use.

> [!WARNING]
> 
> This library is in active development. The API is not stable and breaking changes may occur until version 1.0.0 is released.

## Features

- **HTTP Client**: Memory-safe HTTP client with fluent interface ([docs](docs/TidyKit.Request.md))
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

var
  Response: TResponse;
  Request: THttpRequest;
begin
  // Simple GET request
  Response := Http.Get('https://api.example.com/data');
  if Response.StatusCode = 200 then
    WriteLn(Response.Text);

  // POST with JSON using fluent interface
  Response := Request
    .Post
    .URL('https://api.example.com/users')
    .AddHeader('X-API-Key', 'your-key')
    .WithJSON('{"name": "John"}')
    .Send;
    
  if Response.StatusCode = 200 then
    WriteLn(Response.JSON.FormatJSON);
end;

### File System Operations
```pascal
uses
  TidyKit;

// Basic file operations
var
  FileKit: TFileKit;
begin
  FileKit := TFileKit.Create;
  try
    // Write and read text files
    FileKit.WriteText('file.txt', 'Hello World');
    WriteLn(FileKit.ReadText('file.txt'));
    
    // Directory operations
    FileKit.CreateDirectory('new_dir');
    FileKit.CopyFile('source.txt', 'dest.txt');
    
    // File search with sorting
    var Files := FileKit.ListFiles('.', '*.txt', False, fsDate);
    for var File in Files do
      WriteLn(File);
      
    // Path manipulation
    WriteLn(FileKit.GetFileName('path/to/file.txt'));    // file.txt
    WriteLn(FileKit.GetDirectory('path/to/file.txt'));   // path/to
    WriteLn(FileKit.GetExtension('file.txt'));           // .txt
  finally
    FileKit.Free;
  end;
end;
```

### DateTime Operations
```pascal
uses
  TidyKit;

// Basic date/time operations
var
  DateTimeKit: TDateTimeKit;
begin
  DateTimeKit := TDateTimeKit.Create;
  try
    // Get current date/time
    var Now := DateTimeKit.GetNow;
    var Today := DateTimeKit.GetToday;
    
    // Format dates
    WriteLn(DateTimeKit.GetAsString(Now, 'yyyy-mm-dd hh:nn:ss'));
    
    // Date arithmetic
    var NextWeek := DateTimeKit.AddDays(Now, 7);
    var LastMonth := DateTimeKit.AddMonths(Now, -1);
    
    // Date components
    WriteLn('Year: ', DateTimeKit.GetYear(Now));
    WriteLn('Month: ', DateTimeKit.GetMonth(Now));
    WriteLn('Day: ', DateTimeKit.GetDay(Now));
    
    // Period calculations
    var StartDate := DateTimeKit.FromString('2024-01-01');
    var EndDate := DateTimeKit.FromString('2024-12-31');
    var Period := DateTimeKit.SpanBetween(StartDate, EndDate, dskPeriod);
  finally
    DateTimeKit.Free;
  end;
end;
```

### String Operations
```pascal
uses
  TidyKit;

// String manipulation
var
  StringKit: TStringKit;
begin
  StringKit := TStringKit.Create;
  try
    // Basic operations
    WriteLn(StringKit.Trim('  Hello World  '));
    WriteLn(StringKit.ToUpper('hello'));
    WriteLn(StringKit.ToLower('WORLD'));
    
    // Advanced operations
    WriteLn(StringKit.PadLeft('123', 5, '0'));     // 00123
    WriteLn(StringKit.ReverseText('Hello'));       // olleH
    WriteLn(StringKit.CapitalizeText('hello world')); // Hello World
    
    // Pattern matching
    if StringKit.MatchesPattern('test@email.com', '^[\w\.-]+@[\w\.-]+\.\w+$') then
      WriteLn('Valid email');
      
    // String analysis
    WriteLn('Words: ', StringKit.GetWords('Hello World').Count);
    WriteLn('Contains: ', StringKit.Contains('Hello World', 'World'));
  finally
    StringKit.Free;
  end;
end;
```

## Documentation

- [HTTP Client](docs/TidyKit.Request.md)
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
- Automatic initialization and cleanup
- No manual memory management needed
- Example: TResponse, TRequestBuilder

## Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add some amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.