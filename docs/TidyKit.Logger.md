# TidyKit.Logger Documentation

## Overview

`TidyKit.Logger` is a lightweight, flexible logging component for Free Pascal applications. It provides formatted log messages with timestamps, log levels, and multiple output destinations including console and file-based logging.

## Quick Start Guide

### One-Line Setup

The simplest way to start using TidyKit.Logger is with one of the factory methods:

```pascal
uses
  TidyKit.Logger;

// Console and file logging with Info level
TLogger.CreateConsoleAndFileLogger('application.log', llInfo);

// Now you can start logging immediately
Logger.Info('Application started');
```

### Basic Logging

```pascal
// Log messages with different levels
Logger.Debug('Detailed information for troubleshooting'); // Only shown if minimum level is Debug
Logger.Info('Normal operational messages');
Logger.Warning('Something might be wrong');
Logger.Error('Something failed but application can continue');
Logger.Fatal('Critical error, application cannot continue');

// Log with format strings (both styles supported)
Logger.InfoFmt('User %s logged in from %s', ['john', '192.168.1.10']);
Logger.Info('User %s logged in from %s', ['john', '192.168.1.10']); // Same result
```

### Category-Based Logging

Organize your logs by component or feature:

```pascal
var
  UILogger, DBLogger: TLogContext;
begin
  // Create loggers for different parts of your application
  UILogger := Logger.CreateContext('UI');
  DBLogger := Logger.CreateContext('DB');
  
  // Log with categories
  UILogger.Info('Window created');      // Outputs: [UI] Window created
  DBLogger.Warning('Slow query');       // Outputs: [DB] Slow query
  
  // No need to free context objects - they're automatically managed
end;
```

### Measuring Performance

Easily track how long operations take:

```pascal
procedure ImportData;
var
  Timer: ITimedOperation;
begin
  Timer := Logger.TimedBlock('Data import');
  // ... perform operation ...
  // Timer automatically logs completion with duration when it goes out of scope
end;
```

## Features

- **Log Levels**: Debug, Info, Warning, Error, and Fatal levels with appropriate console colors
- **Multiple Destinations**: Log to console, files, or both simultaneously
- **File Rotation**: Automatic log file rotation when size limits are reached
- **Multiple Log Files**: Support for logging to multiple files with individual control
- **Singleton Pattern**: Easy global access through the `Logger` function
- **Custom DateTime Format**: Configurable timestamp format
- **Thread-Safe**: Basic thread safety through file open/close on each write
- **Error Handling**: Robust error recovery to prevent logging failures from crashing applications
- **Log Level Filtering**: Filter out messages below a specified level
- **Format String Support**: Convenient format string overloads for all log methods
- **Method Chaining**: Fluent API for configuration methods
- **Default Log File Paths**: Automatic creation of log directories and files
- **Simple One-Line Setup**: Set up the logger with a single method call
- **Category Support**: Group logs by categories for better organization
- **Extensible Sink Architecture**: Easily add custom output destinations
- **Pattern-Based Formatting**: Customize log message format with patterns
- **Structured Logging**: Log structured data as key-value pairs
- **Performance Timing**: Measure and log operation durations
- **Batch Logging**: Batch multiple log messages for better performance
- **Configuration Options**: Load settings from environment variables or config files
- **Specialized Logger Types**: Purpose-built loggers for specific scenarios

## Advanced Usage

### Simplified Initialization

```pascal
// Create different types of pre-configured loggers
var
  ConsoleLogger: TLogger;
  FileLogger: TLogger;
  BothLogger: TLogger;
  DebugLogger: TLogger;
  AuditLogger: TLogger;
begin
  // Console-only logger with minimum level Debug
  ConsoleLogger := TLogger.CreateConsoleLogger(llDebug);
  
  // File-only logger with Info level
  FileLogger := TLogger.CreateFileLogger('application.log', llInfo);
  
  // Console and file logger
  BothLogger := TLogger.CreateConsoleAndFileLogger('application.log', llInfo);
  
  // Debug logger with special format showing file and line
  DebugLogger := TLogger.CreateDebugLogger;
  
  // Audit logger for security events
  AuditLogger := TLogger.CreateAuditLogger('audit.log');
end;
```

### Structured Logging

```pascal
// Log structured data
Logger.LogStructured(llInfo, 'User login', [
  NameValuePair('username', 'john_doe'),
  NameValuePair('ip_address', '192.168.1.10'),
  NameValuePair('success', True),
  NameValuePair('attempt', 3)
]);
```

### Method Chaining

```pascal
// Configure multiple settings with method chaining
Logger
  .SetLogDestinations([ldConsole, ldFile])
  .SetMinLogLevel(llInfo)
  .SetDateTimeFormat('yyyy-mm-dd hh:nn')
  .SetFormat('[%time] [%level] %message');
  
// Then add a log file
Logger.AddLogFile('application.log');
```

### Custom Message Format

```pascal
// Set custom message format pattern
Logger.SetFormat('[%time] [%level] [%file:%line] %message');

// Available pattern tokens:
// %time - Formatted timestamp
// %level - Log level
// %message - Log message
// %category - Log category (if any)
// %file - Source file (if available)
// %line - Line number (if available)
```

### Batch Logging

```pascal
// Batch multiple messages for better performance
Logger.BeginBatch;
try
  for i := 1 to 1000 do
    Logger.Info('Processing item ' + IntToStr(i));
finally
  Logger.EndBatch; // Writes all messages at once
end;
```

### Extensible Sink Architecture

```pascal
// Use built-in sinks
Logger.AddSink(TConsoleSink.Create);
Logger.AddSink(TFileSink.Create('app.log'));
Logger.AddSink(TRotatingFileSink.Create('app.log', 1024*1024, 5)); // 1MB, keep 5 files
Logger.AddSink(TDailyFileSink.Create('app.log')); // Rotates daily
Logger.AddSink(TMemorySink.Create(100)); // Keep last 100 messages in memory
```

### Direct Category Logging

```pascal
// Log with categories without creating context objects
Logger.InfoWithCategory('UI', 'Window created');
Logger.ErrorWithCategory('Database', 'Connection failed');
```

### Configuration

```pascal
// Load from environment variables
Logger.ConfigureFromEnvironment;
// Environment variables:
// LOGGER_DESTINATIONS (CONSOLE,FILE)
// LOGGER_LEVEL (DEBUG,INFO,WARNING,ERROR,FATAL)
// LOGGER_DATETIME_FORMAT
// LOGGER_FORMAT_PATTERN
// LOGGER_DEFAULT_FILE
// LOGGER_MAX_FILE_SIZE

// Load from configuration file
Logger.LoadConfiguration('logger.ini');
// Example file format:
// [Logger]
// Destinations=Console,File
// MinLevel=Info
// DateTimeFormat=yyyy-mm-dd hh:nn:ss.zzz
// FormatPattern=[%time] [%level] %message
// DefaultLogFile=application.log
// MaxFileSize=26214400
```

## API Reference

### Functions

#### `Logger: TLogger`

Returns the singleton instance of the TLogger class. Use this function for global access to the logger.

```pascal
var
  MyLogger: TLogger;
begin
  MyLogger := Logger;
  // or directly use Logger.Method()
end;
```

#### Helper Functions

```pascal
// Create name-value pairs for structured logging
function NameValuePair(const AName: string; const AValue: string): TNameValuePair; overload;
function NameValuePair(const AName: string; AValue: Integer): TNameValuePair; overload;
function NameValuePair(const AName: string; AValue: Boolean): TNameValuePair; overload;
function NameValuePair(const AName: string; AValue: Double): TNameValuePair; overload;
```

### TLogger Class

#### Factory Methods (Simplified Initialization)

```pascal
class function CreateConsoleLogger(AMinLevel: TLogLevel = llDebug): TLogger;
class function CreateFileLogger(const AFilename: string; AMinLevel: TLogLevel = llDebug): TLogger;
class function CreateConsoleAndFileLogger(const AFilename: string; AMinLevel: TLogLevel = llDebug): TLogger;
class function CreateDebugLogger: TLogger;
class function CreateAuditLogger(const AFilename: string): TLogger;
class function CreateDefaultLogger(ADestinations: TLogDestinations = [ldConsole, ldFile]; 
  const ALogFileName: string = ''; AMinLogLevel: TLogLevel = llDebug): TLogger;
```

#### Properties and Configuration

##### `function SetLogDestinations(ADestinations: TLogDestinations): TLogger`

Sets the active log destinations. Can be a set of:
- `ldConsole`: Log to console (stdout)
- `ldFile`: Log to file(s)

Returns the logger instance for method chaining.

```pascal
// Log to console only
Logger.SetLogDestinations([ldConsole]);

// Log to both console and file
Logger.SetLogDestinations([ldConsole, ldFile]);

// Disable all logging
Logger.SetLogDestinations([]);
```

##### `function SetDateTimeFormat(const AFormat: string): TLogger`

Sets the datetime format string used for timestamps in log messages.
Returns the logger instance for method chaining.

```pascal
// Default format
Logger.SetDateTimeFormat('yyyy-mm-dd hh:nn:ss.zzz');

// Custom format
Logger.SetDateTimeFormat('yyyy-mm-dd hh:nn');
```

##### `function SetMinLogLevel(ALevel: TLogLevel): TLogger`

Sets the minimum log level. Messages below this level will be filtered out.
Returns the logger instance for method chaining.

```pascal
// Only show Info level and above
Logger.SetMinLogLevel(llInfo);

// Show all messages including Debug
Logger.SetMinLogLevel(llDebug);
```

##### `function SetFormat(const APattern: string): TLogger`

Sets the message format pattern. Available tokens:
- `%time`: Formatted timestamp
- `%level`: Log level
- `%message`: Log message
- `%category`: Log category (if any)
- `%file`: Source file (if available)
- `%line`: Line number (if available)

Returns the logger instance for method chaining.

```pascal
// Default format
Logger.SetFormat('[%time] [%level] %message');

// Custom format with file and line info
Logger.SetFormat('[%time] [%level] [%file:%line] %message');
```

##### `function AddLogFile(const AFileName: string; AMaxSize: Int64 = 25 * 1024 * 1024): Integer`

Adds a log file to the logger. Returns the index of the added log file.
- `AFileName`: The path and name of the log file
- `AMaxSize`: Maximum file size in bytes (default 25MB)

```pascal
var
  LogIndex: Integer;
begin
  // Add a log file with default size limit (25MB)
  LogIndex := Logger.AddLogFile('application.log');
  
  // Add a log file with 1MB size limit
  LogIndex := Logger.AddLogFile('small_log.log', 1024 * 1024);
end;
```

##### `function AddDefaultLogFile(const ABaseName: string = 'application'; AMaxSize: Int64 = 25 * 1024 * 1024): Integer`

Adds a log file with a default path in a 'logs' subdirectory of the application.
Returns the index of the added log file.
- `ABaseName`: Base name for the log file (default 'application')
- `AMaxSize`: Maximum file size in bytes (default 25MB)

```pascal
var
  LogIndex: Integer;
begin
  // Creates logs/application.log in the application directory
  LogIndex := Logger.AddDefaultLogFile();
  
  // Creates logs/system.log
  LogIndex := Logger.AddDefaultLogFile('system');
end;
```

##### `procedure CloseLogFiles`

Closes all log files and clears the log file list.

```pascal
Logger.CloseLogFiles;
```

##### `function AddSink(ASink: ILogSink): TLogger`

Adds a custom sink for log output. Returns the logger instance for method chaining.

```pascal
var
  Sink: ILogSink;
begin
  Sink := TFileSink.Create('app.log');
  Logger.AddSink(Sink);
end;
```

##### `procedure RemoveSink(ASink: ILogSink)`

Removes a previously added sink.

##### `procedure ClearSinks`

Removes all sinks.

##### `function GetInstanceID: Int64`

Returns the unique identifier of the current logger instance. This can be used to track logger instances across resets.

```pascal
var
  CurrentID: Int64;
begin
  CurrentID := Logger.GetInstanceID;
  WriteLn('Current logger instance ID: ', CurrentID);
end;
```

#### Logging Methods

##### Standard Logging

```pascal
procedure Log(const AMessage: string; ALogLevel: TLogLevel = llInfo; const AFileIndex: Integer = -1);
procedure Debug(const AMessage: string; const AFileIndex: Integer = -1);
procedure Info(const AMessage: string; const AFileIndex: Integer = -1);
procedure Warning(const AMessage: string; const AFileIndex: Integer = -1);
procedure Error(const AMessage: string; const AFileIndex: Integer = -1);
procedure Fatal(const AMessage: string; const AFileIndex: Integer = -1);
```

##### Format String Overloads (Original Style)

```pascal
procedure DebugFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1);
procedure InfoFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1);
procedure WarningFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1);
procedure ErrorFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1);
procedure FatalFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1);
```

##### Format String Overloads (Streamlined)

```pascal
procedure Debug(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1); overload;
procedure Info(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1); overload;
procedure Warning(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1); overload;
procedure Error(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1); overload;
procedure Fatal(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1); overload;
```

##### Type-Specific Logging

```pascal
procedure LogValue(const AName: string; const AValue: Integer; ALevel: TLogLevel = llInfo);
procedure LogValue(const AName: string; const AValue: Boolean; ALevel: TLogLevel = llInfo);
procedure LogValue(const AName: string; const AValue: Double; ALevel: TLogLevel = llInfo);
procedure LogValue(const AName: string; const AValue: string; ALevel: TLogLevel = llInfo);
```

##### Structured Logging

```pascal
procedure LogStructured(ALevel: TLogLevel; const AMessage: string; const AFields: array of TNameValuePair);
```

##### Direct Category Logging

```pascal
procedure LogWithCategory(const ACategory: string; ALevel: TLogLevel; const AMessage: string);
procedure DebugWithCategory(const ACategory, AMessage: string);
procedure InfoWithCategory(const ACategory, AMessage: string);
procedure WarningWithCategory(const ACategory, AMessage: string);
procedure ErrorWithCategory(const ACategory, AMessage: string);
procedure FatalWithCategory(const ACategory, AMessage: string);
```

#### Performance and Batching

##### `function TimedBlock(const AName: string): ITimedOperation`

Creates a timed operation block that logs start and end times with duration.

```pascal
var
  Timer: ITimedOperation;
begin
  Timer := Logger.TimedBlock('File processing');
  // ... do work ...
  // When Timer goes out of scope, it logs completion time and duration
end;
```

##### `procedure BeginBatch`

Starts batch mode. Messages are stored but not actually logged until EndBatch is called.

##### `procedure EndBatch`

Ends batch mode and processes all stored messages.

#### Configuration

##### `procedure Configure(const AConfig: TLoggerConfig)`

Configures the logger with the specified settings.

##### `procedure ConfigureFromEnvironment`

Configures the logger based on environment variables.

##### `procedure LoadConfiguration(const AConfigFile: string)`

Loads logger configuration from a file.

#### Singleton Management

##### `class function GetInstance: TLogger`

Returns the singleton instance of the TLogger class. This is the method called by the `Logger` function.

##### `class procedure ResetInstance`

Resets (destroys and recreates) the singleton instance.

#### Category Support

##### `function CreateContext(const ACategory: string): TLogContext`

Creates a new logging context with the specified category name.

### TLogContext Class

The TLogContext class provides category-based logging. Each logging method automatically prefixes log messages with the category name.

```pascal
procedure Debug(const AMessage: string; const AFileIndex: Integer = -1);
procedure Info(const AMessage: string; const AFileIndex: Integer = -1);
procedure Warning(const AMessage: string; const AFileIndex: Integer = -1);
procedure Error(const AMessage: string; const AFileIndex: Integer = -1);
procedure Fatal(const AMessage: string; const AFileIndex: Integer = -1);

// Original format string methods
procedure DebugFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1);
procedure InfoFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1);
procedure WarningFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1);
procedure ErrorFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1);
procedure FatalFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1);

// Streamlined format string methods
procedure Debug(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1); overload;
procedure Info(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1); overload;
procedure Warning(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1); overload;
procedure Error(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1); overload;
procedure Fatal(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1); overload;
```

### Built-in Sinks

TidyKit.Logger comes with several built-in sink implementations:

#### `TConsoleSink`

Writes log messages to the console with colored output based on log level.

```pascal
var
  Sink: ILogSink;
begin
  Sink := TConsoleSink.Create;
  Logger.AddSink(Sink);
end;
```

#### `TFileSink`

Writes log messages to a file, with optional size-based rotation.

```pascal
var
  Sink: ILogSink;
begin
  Sink := TFileSink.Create('app.log', 10 * 1024 * 1024); // 10MB max
  Logger.AddSink(Sink);
end;
```

#### `TRotatingFileSink`

Extends TFileSink with the ability to keep a limited number of rotated files.

```pascal
var
  Sink: ILogSink;
begin
  // 5MB max size, keep up to 10 old files
  Sink := TRotatingFileSink.Create('app.log', 5 * 1024 * 1024, 10);
  Logger.AddSink(Sink);
end;
```

#### `TDailyFileSink`

Creates a new log file each day with the date in the filename.

```pascal
var
  Sink: ILogSink;
begin
  Sink := TDailyFileSink.Create('app.log');
  Logger.AddSink(Sink);
  // Creates files like: app_20250301.log, app_20250302.log, etc.
end;
```

#### `TMemorySink`

Keeps log messages in memory for later retrieval.

```pascal
var
  Sink: TMemorySink;
begin
  Sink := TMemorySink.Create(100); // Keep last 100 messages
  Logger.AddSink(Sink);
  
  // Later, retrieve messages
  Memo1.Lines.Assign(Sink.GetMessages);
end;
```

## Examples

### Basic Application Logging

```pascal
procedure InitLogger;
begin
  // Easy one-line setup
  TLogger.CreateConsoleAndFileLogger('application.log', llInfo);
  Logger.Info('Application started');
end;

procedure ShutdownLogger;
begin
  Logger.Info('Application shutting down');
  Logger.CloseLogFiles;
end;
```

### Logging with Custom Format

```pascal
TLogger.CreateConsoleLogger(llDebug);
Logger.SetFormat('[%time] [%level] [Thread %threadid] %message');
```

### Performance Monitoring

```pascal
procedure ProcessFiles(const APath: string);
var
  Timer: ITimedOperation;
  Files: TStringList;
  i: Integer;
begin
  Timer := Logger.TimedBlock('File processing');
  
  Files := TStringList.Create;
  try
    FindAllFiles(APath, '*.txt', Files);
    Logger.Info('Found %d files to process', [Files.Count]);
    
    for i := 0 to Files.Count - 1 do
    begin
      Logger.Debug('Processing file: %s', [Files[i]]);
      // Process file...
    end;
  finally
    Files.Free;
  end;
  // Timer automatically logs completion when it goes out of scope
end;
```

### High-Volume Logging with Batching

```pascal
procedure ProcessLargeDataset(const ADataset: TDataset);
begin
  Logger.BeginBatch;
  try
    while not ADataset.EOF do
    begin
      Logger.Debug('Processing record %d', [ADataset.RecNo]);
      // Process record
      ADataset.Next;
    end;
  finally
    Logger.EndBatch; // Writes all messages at once
  end;
end;
```

### Creating Custom Sinks

```pascal
type
  TEmailSink = class(TInterfacedObject, ILogSink)
  private
    FRecipient: string;
    FSubject: string;
    FMessages: TStringList;
  public
    constructor Create(const ARecipient, ASubject: string);
    destructor Destroy; override;
    procedure Write(const AFormattedMessage: string; ALevel: TLogLevel);
    procedure Flush;
  end;

constructor TEmailSink.Create(const ARecipient, ASubject: string);
begin
  inherited Create;
  FRecipient := ARecipient;
  FSubject := ASubject;
  FMessages := TStringList.Create;
end;

destructor TEmailSink.Destroy;
begin
  Flush; // Send any remaining messages
  FMessages.Free;
  inherited;
end;

procedure TEmailSink.Write(const AFormattedMessage: string; ALevel: TLogLevel);
begin
  // Only store errors and fatal errors
  if ALevel >= llError then
    FMessages.Add(AFormattedMessage);
    
  // Send immediately for fatal errors
  if ALevel = llFatal then
    Flush;
end;

procedure TEmailSink.Flush;
begin
  if FMessages.Count > 0 then
  begin
    // Send email with FMessages.Text as body
    // ...email sending code...
    FMessages.Clear;
  end;
end;

// Usage:
Logger.AddSink(TEmailSink.Create('admin@example.com', 'Error Report'));
```

## Best Practices

1. **Use Appropriate Log Levels**: Debug for detailed troubleshooting, Info for general progress, Warning for potential issues, Error for failures, Fatal for critical problems.

2. **Set Appropriate Minimum Log Level**: Use Debug during development, Info or Warning in production to reduce noise and improve performance.

3. **Use Category Logging**: Organize logs by component or feature using category contexts or direct category methods.

4. **Choose Appropriate Sinks**: Console for development, file-based for production, possibly with rotation settings to manage disk space.

5. **Use Structured Logging**: For machine-readable logs or when integrating with log analysis tools, use structured logging with field data.

6. **Use Performance Timing**: Track operation durations to identify bottlenecks.

7. **Use Batch Logging**: When logging large numbers of messages in a loop, use batch mode to improve performance.

8. **Add Context**: Include relevant context in log messages to make troubleshooting easier.

9. **Configure from Environment**: Use environment variables for configuration to easily adjust settings in different environments.

10. **Clean Up Resources**: Call `CloseLogFiles` when shutting down your application.

## Limitations

1. The logger is primarily designed for single-process applications.
2. While basic file operations are thread-safe, heavy multi-threaded use might benefit from additional synchronization.
3. The batch mode is not thread-safe. Use it in single-threaded contexts or provide your own synchronization.
4. Some features might not be compatible with all Pascal compilers or platforms. Adjust as needed for your environment. 