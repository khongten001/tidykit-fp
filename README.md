# 🧰 TidyKit: Comprehensive Free Pascal Toolkit for Development

[![FPC](https://img.shields.io/badge/Free%20Pascal-3.2.2-blue.svg)](https://www.freepascal.org/)
[![Lazarus](https://img.shields.io/badge/Lazarus-3.6+-blue.svg)](https://www.lazarus-ide.org/)
[![License](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE.md)
[![Documentation](https://img.shields.io/badge/Docs-Available-brightgreen.svg)](docs/)
[![Tests](https://img.shields.io/badge/Tests-Passing-brightgreen.svg)](tests/)
[![Status](https://img.shields.io/badge/Status-Development-yellow.svg)]()
[![Version](https://img.shields.io/badge/Version-0.1.8-blueviolet.svg)]()

**Providing utilities and development tools without external dependencies.**

> [!WARNING]
> ⚠️ This library is under active development and is not yet ready for production use. APIs may change without notice.

## 🌟 Why TidyKit?

- **All-in-One Solution**: Stop hunting for separate libraries - TidyKit provides everything in one package
- **Modern Pascal**: Designed with modern programming practices while maintaining FPC 3.2.2 compatibility
- **Thoroughly Tested**: Extensive test suite ensures reliability and stability
- **Cross-Platform**: Tested on both Windows and Ubuntu Linux environments
- **Well-Documented**: Every component has detailed documentation with examples
- **Memory Management**: Mixed approach with interface-based automatic reference counting for complex objects and traditional memory management for simpler operations
- **Easy to use API**: Currently transitioning toward a simpler API design (see [Roadmap](#️-roadmap))

## 📑 Table of Contents 

- [🧰 TidyKit: Comprehensive Free Pascal Toolkit for Development](#-tidykit-comprehensive-free-pascal-toolkit-for-development)
  - [🌟 Why TidyKit?](#-why-tidykit)
  - [📑 Table of Contents](#-table-of-contents)
  - [🏗️ Architectural Patterns](#️-architectural-patterns)
  - [✨ Features](#-features)
  - [💻 Installation (Lazarus IDE)](#-installation-lazarus-ide)
  - [💻 Installation (General)](#-installation-general)
  - [📝 Library Usage](#-library-usage)
  - [🚀 Quick Start](#-quick-start)
    - [📝 String Operations](#-string-operations)
    - [📂 File System Operations](#-file-system-operations)
    - [📅 DateTime Operations](#-datetime-operations)
    - [🔄 JSON Operations](#-json-operations)
    - [📝 Logging Operations](#-logging-operations)
    - [🌐 HTTP Request Operations](#-http-request-operations)
    - [🔐 Crypto Operations](#-crypto-operations)
    - [📦 Archive Operations](#-archive-operations)
    - [📚 Collections Operations](#-collections-operations)
  - [📖 System Requirements](#-system-requirements)
    - [Tested Environments](#tested-environments)
    - [Dependencies](#dependencies)
    - [Build Requirements](#build-requirements)
  - [📚 Documentation](#-documentation)
  - [📊 Real-World Examples](#-real-world-examples)
  - [💬 Community \& Support](#-community--support)
  - [⚠️ Known Limitations](#️-known-limitations)
  - [✅ Testing](#-testing)
  - [🤝 Contributing](#-contributing)
  - [⚖️ License](#️-license)
  - [🙏 Acknowledgments](#-acknowledgments)
  - [🗺️ Roadmap](#️-roadmap)
    - [Planned for v0.2.0 (Q3 2025) - Simplify API](#planned-for-v020-q3-2025---simplify-api)
    - [Planned for v0.3.0 (Q4 2025) - Examples \& Refinements](#planned-for-v030-q4-2025---examples--refinements)
    - [Future Goals](#future-goals)

## 🏗️ Architectural Patterns

TidyKit currently uses a mix of architectural patterns. We are actively working towards a more simplified API for release **v0.2.0**.

| Pattern | Modules | Characteristics | Memory Management |
|---------|-------------------------|-----------------|-------------------|
| **Factory/Interface** | `TidyKit.JSON`, `TidyKit.Logger` | <ul><li>Object-oriented design</li><li>Fluent API</li></ul> | Automatic reference counting via interfaces |
| **Advanced Records** | `TidyKit.Request` | <ul><li>Record-based design</li><li>Fluent API</li><li>Value semantics</li></ul> | No manual management needed |
| **Static Class Functions** | `TidyKit.FS`, `TidyKit.DateTime`, `TidyKit.Strings`, `TidyKit.Archive`, `TidyKit.Crypto.*` | <ul><li>Procedural-style API</li><li>Simple usage for stateless utilities</li><li>No instance creation</li></ul> | No manual management needed |

> [!IMPORTANT]
> The goal for **v0.2.0** is to finalise this structure, providing a clear distinction: Factory/Interface for components interacting with external state or benefiting from mocking, and Static Methods for stateless utilities.

## ✨ Features

- 📝 **String Operations**
  - String manipulation and transformations
  - Case conversion and formatting
  - Pattern matching and validation
  - Unicode support
  - String comparison and searching
  - Text encoding/decoding

- 📂 **File System Operations**
  - File and directory manipulation
  - Path operations
  - File searching and filtering

- 📦 **Archive Operations**
  - ZIP file compression and decompression
  - TAR file creation and extraction
  - Pattern-based file filtering
  - Recursive directory handling

- 🔐 **Cryptography**
  - SHA3 implementation
  - SHA2 family (SHA-256, SHA-512, SHA-512/256)
  - AES-256 encryption
    - CBC and CTR modes
    - High-level interface with automatic Base64 encoding
    - Low-level interface with raw binary operations
    - Configurable padding modes (PKCS7 or None)
  - Secure hashing
  - Encryption utilities
  - Base64 encoding/decoding
  - Legacy support (MD5, SHA1, Blowfish)

- 🌐 **Network Operations**
  - HTTP client
  - Request handling
  - Response parsing

- 🔄 **JSON Operations**
  - Interface-based JSON manipulation with automatic memory management
  - Property order preservation in JSON objects
  - Full Unicode support
    - Unicode escape sequence parsing (\uXXXX)
    - UTF-8/16 character handling
  - Support for all JSON data types
    - Objects with ordered properties
    - Arrays with type-safe elements
    - Strings with proper escaping
    - Numbers (both integer and floating-point)
    - Booleans (true/false)
    - Null (singleton implementation)
  - Output formatting
    - Pretty printing with configurable indentation
    - Compact output for storage/transmission
  - Memory safety
    - Automatic reference counting through interfaces
    - Safe singleton management for null values
    - Proper cleanup of nested structures
  - Thoroughly tested with 17 comprehensive test cases

- 📝 **Logging Operations**
  - Easy to use logging system with multiple output destinations
  - Configurable log levels (Debug, Info, Warning, Error, Fatal)
  - Console and file output with automatic coloring
  - File rotation based on size
  - Multiple log file support
  - Category-based logging for better organization
  - Automatic context management with reference counting
  - Format string support for convenient message formatting
  - Thread-safety considerations for multi-threaded applications
  - Singleton pattern with unique instance tracking
  - Method chaining for fluent configuration
  - Error recovery to prevent logging failures from crashing the application
  - Default log directory creation
  - Extensible sink architecture with built-in implementations
  - Pattern-based message formatting
  - Structured logging for key-value data
  - Performance timing capabilities
  - Batch logging for improved performance
  - Environment and file-based configuration
  - Specialized logger factory methods
  - Thoroughly tested with 34 comprehensive test cases

- 📚 **Collections**
  - Generic List<T> implementation with dynamic array backing
  - Strong typing with generics
  - Automatic memory management through interfaces
  - Common collection operations:
    - Add, Insert, Remove, and Clear methods
    - Item access by index
    - Search and filter capabilities
    - Sorting with custom comparers
    - ForEach iteration
    - ToArray conversion
  - Specialized list types:
    - StringList for string collections
    - ObjectList for TObject descendants
  - Value semantics for primitive types
  - Object lifecycle management for complex types
  - Interface compatibility with standard Pascal enumerators
  - Thoroughly tested with comprehensive test cases

## 💻 Installation (Lazarus IDE)

1. Clone the repository:

```bash
git clone https://github.com/ikelaiah/tidykit-fp
```

2. Open / start a new project in Lazarus IDE

3. Go to `Package` → `Open Package File (.lpk)...`

4. Navigate to the TidyKit packages in the `packages/lazarus/` folder and select `TidyKit.lpk`

5. In the package window that opens, click `Compile`

6. Click `Use → Add to Project` to install the package

The TidyKit package is now ready to use in your Lazarus project.

## 💻 Installation (General)

1. Clone the repository:

```bash
git clone https://github.com/ikelaiah/tidykit-fp
```

2. Add the source directory to your project's search path.


## 📝 Library Usage

```pascal
uses
  // String manipulation unit
  TidyKit.Strings,           // String operations

  // File system operations
  TidyKit.FS,                // File system operations

  // Date Time manipulation unit
  TidyKit.DateTime,          // Date Time operations

  // JSON functionality
  TidyKit.JSON,              // All JSON functionality

  // Logging functionality
  TidyKit.Logger,            // Easy to use logging system

  // Network units
  TidyKit.Request,           // HTTP client with simple and advanced features

  // Cryptography units
  TidyKit.Crypto,            // Base crypto operations
  TidyKit.Crypto.SHA2,       // SHA2 implementation
  TidyKit.Crypto.SHA3,       // SHA3 implementation
  TidyKit.Crypto.AES256,     // AES-256 implementation

  // Archive operations
  TidyKit.Archive,           // Archive operations

  // Command-line argument parsing
  TidyKit.ParseArgs,         // Simple argument parser

  // Collections
  TidyKit.Collections.List;  // Generic List implementation
```

## 🚀 Quick Start

### 📝 String Operations

```pascal
var
  Text: string;
begin
  // Email validation
  if TStringKit.MatchesPattern('user@example.com', '^[\w\.-]+@[\w\.-]+\.\w+$') then
    WriteLn('Valid email');
    
  // Format phone number
  Text := TStringKit.PadLeft('5551234', 10, '0');  // Returns '0005551234'
  
  // Clean user input
  Text := TStringKit.CollapseWhitespace('  Hello    World  ');  // Returns 'Hello World'
  
  // Format product code
  Text := TStringKit.PadCenter('A123', 8, '-');  // Returns '--A123---'
end;
```

### 📂 File System Operations

```pascal
var
  Files: TFilePathArray;
  Attrs: TFileAttributes;
  Content: string;
begin
  // Basic file operations
  TFileKit.WriteFile('example.txt', 'Hello World');
  Content := TFileKit.ReadFile('example.txt');
  
  // Directory operations
  TFileKit.CreateDirectory('new_folder');
  TFileKit.EnsureDirectory('path/to/folder');
  
  // List files with pattern matching
  Files := TFileKit.ListFiles('src', '*.pas', True, fsName);
  
  // Get file attributes
  Attrs := TFileKit.GetAttributes('example.txt');
  WriteLn(Format('Read-only: %s', [BoolToStr(Attrs.ReadOnly, True)]));
  
  // File manipulation
  TFileKit.CopyFile('source.txt', 'dest.txt');
  TFileKit.MoveFile('old.txt', 'new.txt');
  TFileKit.DeleteFile('temp.txt');
  
  // Path operations
  WriteLn(TFileKit.GetFileName('path/to/file.txt')); // Returns 'file.txt'
  WriteLn(TFileKit.GetExtension('script.pas')); // Returns '.pas'
  WriteLn(TFileKit.ChangeExtension('test.txt', '.bak')); // Returns 'test.bak'
end;
```

### 📅 DateTime Operations

```pascal
var
  CurrentTime: TDateTime;
  NextWorkday: TDateTime;
  TZInfo: TTimeZoneInfo;
  OriginalTZ: string;
  DSTTransitionDate: TDateTime;
begin
  // Get next business day for delivery date
  CurrentTime := TDateTimeKit.GetNow;
  NextWorkday := TDateTimeKit.NextBusinessDay(CurrentTime);
  
  // Format for display
  WriteLn(TDateTimeKit.GetAsString(NextWorkday, 'yyyy-mm-dd'));
  
  // Check if within business hours (9 AM - 5 PM)
  if TDateTimeKit.IsWithinInterval(CurrentTime, 
     TDateTimeKit.CreateInterval(
       TDateTimeKit.StartOfDay(CurrentTime) + EncodeTime(9, 0, 0, 0),
       TDateTimeKit.StartOfDay(CurrentTime) + EncodeTime(17, 0, 0, 0)
     )) then
    WriteLn('Within business hours');
    
  // Cross-platform timezone handling
  TZInfo := TDateTimeKit.GetTimeZone(CurrentTime);
  WriteLn('Current timezone: ', TZInfo.Name);
  WriteLn('Offset from UTC: ', TZInfo.Offset, ' minutes');
  WriteLn('DST active: ', BoolToStr(TZInfo.IsDST, True));
  
  // Convert between timezones
  WriteLn('UTC time: ', TDateTimeKit.GetAsString(
    TDateTimeKit.WithTimeZone(CurrentTime, 'UTC'), 
    'yyyy-mm-dd hh:nn:ss'));
    
  // Cross-platform environment variables for testing
  OriginalTZ := GetEnvVar('TZ');
  try
    // Change timezone for testing
    SetEnvVar('TZ', 'America/New_York');
    
    // Create a datetime value near the US DST transition 
    // (2:00 AM on second Sunday in March 2024)
    DSTTransitionDate := EncodeDateTime(2024, 3, 10, 2, 0, 0, 0);
    
    // Check if this time is in DST using GetTimeZone
    TZInfo := TDateTimeKit.GetTimeZone(DSTTransitionDate);
    WriteLn('DST active: ', BoolToStr(TZInfo.IsDST, True));
    WriteLn('Timezone offset: ', TZInfo.Offset, ' minutes');
  finally
    // Restore original timezone
    SetEnvVar('TZ', OriginalTZ);
  end;
end;
```

### 🔄 JSON Operations

```pascal
var
  Person: IJSONObject;
  Address: IJSONObject;
  Hobbies: IJSONArray;
  JSON: string;
  Value: IJSONValue;
begin
  // Create a person object
  Person := TJSON.Obj;
  Person.Add('name', 'John Smith');
  Person.Add('age', 30);
  Person.Add('isActive', True);
  
  // Create and add an address object
  Address := TJSON.Obj;
  Address.Add('street', '123 Main St');
  Address.Add('city', 'Springfield');
  Address.Add('zipCode', '12345');
  Person.Add('address', Address);
  
  // Create and add a hobbies array
  Hobbies := TJSON.Arr;
  Hobbies.Add('reading');
  Hobbies.Add('cycling');
  Hobbies.Add('swimming');
  Person.Add('hobbies', Hobbies);
  
  // Convert to JSON string with pretty printing
  JSON := Person.ToString(True);
  WriteLn(JSON);
  
  // Parse JSON string
  JSON := '{"name":"Jane Doe","age":25,"skills":["Pascal","Python"]}';
  Value := TJSON.Parse(JSON);
  Person := Value.AsObject;
  
  // Access values
  WriteLn('Name: ', Person['name'].AsString);
  WriteLn('Age: ', Person['age'].AsInteger);
  WriteLn('First Skill: ', Person['skills'].AsArray[0].AsString);
  
  // Error handling
  try
    Value := TJSON.Parse('{invalid json}');
  except
    on E: EJSONException do
      WriteLn('Error: ', E.Message);
  end;
end;
```

### 📝 Logging Operations

```pascal
// Simple one-line setup for console and file logging
TLogger.CreateConsoleAndFileLogger('application.log', llInfo);

// Log messages with different levels
Logger.Debug('Processing started'); // Only shown if minimum level is Debug
Logger.Info('User %s logged in', ['JohnDoe']);
Logger.Warning('Disk space is low: %d%% remaining', [5]);
Logger.Error('Failed to save file: %s', ['Access denied']);
Logger.Fatal('Application crashed: %s', ['Segmentation fault']);

// Create category-based loggers for better organization
var
  UILogger, DBLogger: ILogContext;
begin
  UILogger := Logger.CreateContext('UI');
  DBLogger := Logger.CreateContext('DB');
  
  UILogger.Info('Window created');
  DBLogger.Warning('Slow query detected: %s', ['SELECT * FROM large_table']);
end;

// Time operations and log their duration
var
  Timer: ITimedOperation;
begin
  Timer := Logger.TimedBlock('Data processing');
  // ... perform long operation ...
  // Timer automatically logs completion with duration when it goes out of scope
end;

// IMPORTANT: Always close log files when shutting down
try
  // Your application logic with logging...
finally
  Logger.CloseLogFiles;  // Ensures all data is written to disk
end;
```

### 🌐 HTTP Request Operations

```pascal
var
  Response: TResponse;
  UserData: IJSONObject;
  ApiResponse: IJSONObject;
  Result: TRequestResult;
begin
  // Simple GET request with JSON response
  Response := Http.Get('https://api.example.com/data');
  if Response.StatusCode = 200 then
  begin
    ApiResponse := Response.JSON.AsObject;
    WriteLn('User ID: ', ApiResponse['id'].AsInteger);
    WriteLn('Username: ', ApiResponse['username'].AsString);
  end;
    
  // POST with JSON data
  UserData := TJSON.Obj;
  UserData.Add('name', 'John Smith');
  UserData.Add('email', 'john@example.com');
  UserData.Add('age', 30);
  
  Response := Http.PostJSON('https://api.example.com/users',
    UserData.ToString);
    
  if Response.StatusCode = 201 then
    WriteLn('User created with ID: ', Response.JSON.AsObject['id'].AsString);
    
  // Error handling with TryGet pattern
  Result := Http.TryGet('https://api.example.com/users');
  if Result.Success then
    WriteLn('Found ', Result.Response.JSON.AsObject['count'].AsInteger, ' users')
  else
    WriteLn('Error: ', Result.Error);
    
  // Download file with custom headers
  Response := Http.Get('https://example.com/large-file.zip');
  TFileKit.WriteFile('download.zip', Response.Text);
end;
```

### 🔐 Crypto Operations

```pascal
var
  Hash: string;
  Encrypted: string;
begin
  // Hash password for storage
  Hash := TCryptoKit.SHA512Hash('user_password');
  
  // Secure configuration data
  Encrypted := TCryptoKit.BlowfishCrypt(
    '{"api_key": "secret123"}',
    'encryption_key',
    bmEncrypt
  );
  
  // Verify file integrity
  Hash := TCryptoKit.SHA256Hash(TFileKit.ReadFile('important.dat'));
end;
```

### 📦 Archive Operations

```pascal
var
  SourceDir, DestDir: string;
begin
  // Create ZIP archive
  SourceDir := 'path/to/source';
  TArchiveKit.CompressToZip(SourceDir, 'archive.zip', True);  // Recursive
  
  // Extract specific files
  DestDir := 'path/to/extract';
  TArchiveKit.DecompressFromZip('archive.zip', DestDir, '*.txt');  // Only .txt files
  
  // Create TAR archive with specific files
  TArchiveKit.CompressToTar(SourceDir, 'backup.tar', True, '*.pas');  // Only .pas files
  
  // Extract entire TAR archive
  TArchiveKit.DecompressFromTar('backup.tar', DestDir);
end;
```

### 📚 Collections Operations

```pascal
uses
  TidyKit.Collections.List;

var
  Numbers: IList<Integer>;
  FilteredNumbers: IList<Integer>;
  Strings: IStringList;
  I: Integer;
  Person: TPerson;
  People: IObjectList<TPerson>;
begin
  // Create a generic list of integers
  Numbers := TList<Integer>.Create;
  
  // Add items
  Numbers.Add(1);
  Numbers.Add(2);
  Numbers.Add(3);
  Numbers.Add(4);
  Numbers.Add(5);
  
  // Access items by index
  WriteLn('Third number: ', Numbers[2]); // Zero-based index
  
  // Modify items
  Numbers[1] := 20; // Change 2 to 20
  
  // Iterate over all items
  Numbers.ForEach(
    procedure(const Item: Integer)
    begin
      Write(Item, ' ');
    end
  );
  WriteLn; // Output: 1 20 3 4 5
  
  // Find items
  if Numbers.Contains(20) then
    WriteLn('Found 20 in the list');
  
  // Filter items
  FilteredNumbers := Numbers.Where(
    function(const Item: Integer): Boolean
    begin
      Result := Item > 3;
    end
  );
  
  // Convert to array
  for I in FilteredNumbers.ToArray do
    Write(I, ' ');
  WriteLn; // Output: 4 5
  
  // Specialized string list
  Strings := TStringList.Create;
  Strings.Add('Apple');
  Strings.Add('Banana');
  Strings.Add('Cherry');
  
  // Case-insensitive search
  if Strings.ContainsText('apple') then
    WriteLn('Found apple in the list');
    
  // Working with objects (with automatic memory management)
  People := TObjectList<TPerson>.Create(True); // True = own objects
  
  Person := TPerson.Create;
  Person.Name := 'John';
  Person.Age := 30;
  People.Add(Person); // List takes ownership
  
  Person := TPerson.Create;
  Person.Name := 'Jane';
  Person.Age := 25;
  People.Add(Person);
  
  // Find object by criteria
  Person := People.FirstOrDefault(
    function(const Item: TPerson): Boolean
    begin
      Result := Item.Age < 30;
    end
  );
  
  if Assigned(Person) then
    WriteLn('Found person under 30: ', Person.Name);
    
  // No need to free objects - handled automatically when list goes out of scope
end;
```

## 📖 System Requirements

### Tested Environments

| Module                | Windows 11 | Ubuntu 24.04.2 |
|-----------------------|------------|----------------|
| TidyKit.Strings       | ✅         | ✅            |
| TidyKit.FS            | ✅         | ✅            |
| TidyKit.DateTime      | ✅         | ✅            |
| TidyKit.JSON          | ✅         | ✅            |
| TidyKit.Logger        | ✅         | ✅            |
| TidyKit.Request       | ✅         | ✅            |
| TidyKit.Crypto        | ✅         | ✅            |
| TidyKit.Archive       | ✅         | ✅            |
| TidyKit.Collections   | ✅         | ✅            |

### Dependencies

- Windows
  - No external dependencies required
- Linux
  - Ubuntu/Debian: `sudo apt-get install libssl-dev` (needed for HTTPS in `TidyKit.Request`)
  - Fedora/RHEL: `sudo dnf install openssl-devel` (needed for HTTPS in `TidyKit.Request`)
- Uses only standard Free Pascal RTL units

### Build Requirements

- Free Pascal Compiler (FPC) 3.2.2+
- Lazarus 3.6+
- Basic development tools (git, terminal, etc)

## 📚 Documentation

For detailed documentation, see:

- 📋 [Cheat Sheet](docs/cheat-sheet.md)
- ❓ [FAQ](docs/FAQ.md) - Common questions about design decisions and patterns
- 📝 [Strings](docs/TidyKit.Strings.md)
- 📂 [File System](docs/TidyKit.FS.md)
- 📅 [DateTime](docs/TidyKit.DateTime.md)
- 🔄 [JSON](docs/TidyKit.JSON.md)
- 📝 [Logger](docs/TidyKit.Logger.md)
- 🌐 [Network](docs/TidyKit.Request.md)
- 🔐 [Crypto](docs/TidyKit.Crypto.md)
- 📦 [Archive](docs/TidyKit.Archive.md)
- 📚 [Collections](docs/TidyKit.Collections.md)

## 📊 Real-World Examples

TidyKit can be used to build a wide variety of applications quickly:

| Example Project | Description | Source Code |
|-----------------|-------------|-------------|
| File System Utility | Batch operations on files with pattern matching | [View Example](examples/FileKitExample/) |
| Simple File Explorer | Navigate directories, view files, and perform file operations with an interactive console UI | [View Example](examples/SimpleFileExplorer/) |
| File Analyzer | Analyze text files, count word frequencies, and gather directory statistics | [View Example](examples/FileAnalyzer/) |
| File Backup | Backup files and folders, supports simulate run | [View Example](examples/FileBackup/) |
| Simple Data Logger | Record sensor readings with timestamp and export as CSV | [View Example](examples/LoggerExample/) |
| Configuration Manager | Load, parse, and validate JSON configuration files | [View Example](examples/ConfigKitExample/) |
| Secure Password Storage | Hash and verify passwords with SHA-256 | [View Example](examples/CryptoKitExample/) |
| Date Calculator | Business day calculator with timezone handling | [View Example](examples/DateTimeExample/) |


## 💬 Community & Support

- **Questions?** [Open a discussion](https://github.com/ikelaiah/tidykit-fp/discussions)
- **Found a bug?** [Report an issue](https://github.com/ikelaiah/tidykit-fp/issues)

## ⚠️ Known Limitations

- **Platform Support**: Currently only tested on Windows 11 and Ubuntu 24.04. MacOS and FreeBSD are not officially supported.
- **Threading**: Batch logging mode is not thread-safe. Use in single-threaded contexts or implement your own synchronization.
- **HTTP SSL**: HTTPS requests require OpenSSL libraries on Linux systems (see Dependencies section).
- **Timezone Support**: Limited timezone support on Unix-like systems.
- **Language Features**: Designed for FPC 3.2.2, so no use of inline var declarations or anonymous functions.
- **Library Integration**: Currently no integration with package managers.

## ✅ Testing

1. Open the `TestRunner.lpi` using Lazarus IDE
2. Compile the project
3. Run the Test Runner:

```bash
$ cd tests
$ ./TestRunner.exe -a --format=plain
```

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request. For major changes, please open an issue first to discuss what you would like to change.

1. Fork the Project
2. Create your Feature Branch (git checkout -b feature/AmazingFeature)
3. Commit your Changes (git commit -m 'Add some AmazingFeature')
4. Push to the Branch (git push origin feature/AmazingFeature)
5. Open a Pull Request

## ⚖️ License

This project is licensed under the MIT License - see the [LICENSE](LICENSE.md) file for details.

## 🙏 Acknowledgments

- FPC Team for Free Pascal
- Contributors and maintainers

## 🗺️ Roadmap

TidyKit is under active development. Here's what's planned:

### Planned for v0.2.0 (Q3 2025) - Simplify API

- Provide a simpler API.

### Planned for v0.3.0 (Q4 2025) - Examples & Refinements

- Add **more real-world examples** and tutorials demonstrating common use cases.
- Performance optimizations based on profiling.
- Expand unit test coverage, especially for edge cases identified during API simplification.

### Future Goals

- Command line argument parser module.
- Thread-safe collections and data structures module.
- Concurrency module.
- Integration with package managers.

---

*Feedback and suggestions are welcome! See the [issues](https://github.com/ikelaiah/tidykit-fp/issues) page to contribute ideas or track progress.*