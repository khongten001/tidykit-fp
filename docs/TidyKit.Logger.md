# TidyKit.Logger Documentation

## Overview

`TidyKit.Logger` is a lightweight, flexible logging component for Free Pascal applications. It provides formatted log messages with timestamps, log levels, and multiple output destinations including console and file-based logging.

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

## Getting Started

### Installation

Add the `TidyKit.Logger.pas` unit to your project. Make sure to include it in your uses clause:

```pascal
uses
  // other units...
  TidyKit.Logger;
```

### Basic Usage

```pascal
// Get logger instance
Logger.SetLogDestinations([ldConsole, ldFile]);
Logger.AddLogFile('application.log');

// Log messages with different levels
Logger.Debug('This is a debug message');
Logger.Info('This is an informational message');
Logger.Warning('This is a warning');
Logger.Error('This is an error');
Logger.Fatal('This is a fatal error');
```

### Quick Setup (One-Line)

```pascal
// Create a default logger with console and file logging
TLogger.CreateDefaultLogger();

// Or with custom settings
TLogger.CreateDefaultLogger([ldConsole, ldFile], 'myapp.log', llInfo);
```

### Method Chaining

```pascal
// Configure multiple settings with method chaining
Logger
  .SetLogDestinations([ldConsole, ldFile])
  .SetMinLogLevel(llInfo)
  .SetDateTimeFormat('yyyy-mm-dd hh:nn');
  
// Then add a log file
Logger.AddLogFile('application.log');
```

### Category-Based Logging

```pascal
var
  UILogger, DBLogger, NetworkLogger: TLogContext;
begin
  // Create different contexts for different parts of the application
  UILogger := Logger.CreateContext('UI');
  DBLogger := Logger.CreateContext('DB');
  NetworkLogger := Logger.CreateContext('Network');
  
  // Log with categories
  UILogger.Info('Application window created');
  DBLogger.Info('Connected to database');
  NetworkLogger.Warning('Connection latency high');
  
  // Use format strings with categories
  DBLogger.ErrorFmt('Query failed: %s', ['Syntax error in SQL statement']);
  
  // No need to free context objects - they are automatically managed
end;
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

### TLogger Class

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

##### `procedure Log(const AMessage: string; ALogLevel: TLogLevel = llInfo; const AFileIndex: Integer = -1)`

Main logging method. Logs a message with the specified level and optionally to a specific file index.
- `AMessage`: The message to log
- `ALogLevel`: The log level (default Info)
- `AFileIndex`: Optional specific log file index (default -1, all files)

##### `procedure Debug(const AMessage: string; const AFileIndex: Integer = -1)`

Logs a message with Debug level.

##### `procedure Info(const AMessage: string; const AFileIndex: Integer = -1)`

Logs a message with Info level.

##### `procedure Warning(const AMessage: string; const AFileIndex: Integer = -1)`

Logs a message with Warning level.

##### `procedure Error(const AMessage: string; const AFileIndex: Integer = -1)`

Logs a message with Error level.

##### `procedure Fatal(const AMessage: string; const AFileIndex: Integer = -1)`

Logs a message with Fatal level.

#### Format String Overloads

##### `procedure DebugFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1)`

Logs a formatted message with Debug level.
- `AFormat`: Format string
- `AArgs`: Arguments for the format string
- `AFileIndex`: Optional specific log file index

```pascal
Logger.DebugFmt('Processing item %d of %d', [CurrentItem, TotalItems]);
```

##### `procedure InfoFmt, WarningFmt, ErrorFmt, FatalFmt`

Similar to DebugFmt but for different log levels.

#### Singleton Management

##### `class function GetInstance: TLogger`

Returns the singleton instance of the TLogger class. This is the method called by the `Logger` function. Each logger instance has a unique identifier accessible via `GetInstanceID`.

##### `class procedure ResetInstance`

Resets (destroys and recreates) the singleton instance. Useful for testing or when complete reset is needed.

```pascal
// Reset the logger instance
TLogger.ResetInstance;
```

##### `class function CreateDefaultLogger(ADestinations: TLogDestinations = [ldConsole, ldFile]; const ALogFileName: string = ''; AMinLogLevel: TLogLevel = llDebug): TLogger`

Creates a default logger instance with the specified settings. This is a convenience method for quick setup.
- `ADestinations`: Log destinations (default console and file)
- `ALogFileName`: Custom log file name (if empty, a default log file will be created)
- `AMinLogLevel`: Minimum log level (default Debug)

```pascal
// Create a default logger
TLogger.CreateDefaultLogger();

// Create a custom logger
TLogger.CreateDefaultLogger([ldFile], 'myapp.log', llWarning);
```

#### Category Support

##### `function CreateContext(const ACategory: string): TLogContext`

Creates a new logging context with the specified category name.
- `ACategory`: Name of the category

The returned `TLogContext` object is reference-counted and automatically managed by the logger. No manual cleanup is required.

```pascal
var
  DBLogger: TLogContext;
begin
  DBLogger := Logger.CreateContext('Database');
  DBLogger.Info('Connected to database');
  // Logs: [Database] Connected to database
  
  // No need to free the context - it's automatically managed
end;
```

### TLogContext Class

The TLogContext class provides category-based logging. Each logging method automatically prefixes log messages with the category name. Context objects are automatically managed by the logger and do not need to be freed manually.

#### Logging Methods

All the same logging methods as TLogger (Debug, Info, Warning, Error, Fatal) plus the format string overloads (DebugFmt, InfoFmt, etc.).

## Log Levels

The logger supports the following log levels, in order of increasing severity:

1. **Debug** (`llDebug`): Detailed information for debugging purposes
2. **Info** (`llInfo`): General informational messages about application progress
3. **Warning** (`llWarning`): Potential issues that aren't errors but might need attention
4. **Error** (`llError`): Error conditions that don't stop the application
5. **Fatal** (`llFatal`): Critical errors that may cause the application to terminate

Each log level has an associated color when displayed in the console:
- Debug: Gray
- Info: White (default)
- Warning: Yellow
- Error: Red
- Fatal: White text on red background

## Log File Rotation

When a log file reaches its maximum size limit, it is automatically rotated. The rotation process:

1. Renames the current log file by appending a timestamp to its name
2. Creates a new empty log file with the original name
3. Continues logging to the new file

Example of rotated file names:
```
application.log          # Current log file
application_20250305_123045.log  # Rotated log file with timestamp
```

## Examples

### Basic Application Logging

```pascal
procedure InitLogger;
begin
  Logger.SetLogDestinations([ldConsole, ldFile]);
  Logger.AddLogFile(ExtractFilePath(ParamStr(0)) + 'app.log');
  Logger.Info('Application started');
end;

procedure ShutdownLogger;
begin
  Logger.Info('Application shutting down');
  Logger.CloseLogFiles;
end;
```

### Logging to Multiple Files

```pascal
var
  GeneralLogIndex, ErrorLogIndex: Integer;
begin
  Logger.SetLogDestinations([ldFile]);
  
  // General log for all messages
  GeneralLogIndex := Logger.AddLogFile('general.log');
  
  // Error log for errors only
  ErrorLogIndex := Logger.AddLogFile('errors.log');
  
  // Log to general log
  Logger.Info('This goes to the general log');
  
  // Log to error log only
  Logger.Error('This is an error', ErrorLogIndex);
  
  // Log to both
  Logger.Fatal('This is a critical error');
end;
```

### Using Method Chaining and Format Strings

```pascal
Logger
  .SetLogDestinations([ldConsole, ldFile])
  .SetMinLogLevel(llInfo)
  .SetDateTimeFormat('yyyy-mm-dd hh:nn:ss');

Logger.AddDefaultLogFile('application');

// Use format string
Logger.InfoFmt('User %s logged in from %s', ['john_doe', '192.168.1.10']);
```

### Using Category-Based Logging

```pascal
var
  UILogger, DBLogger, NetworkLogger: TLogContext;
begin
  // Create different contexts for different parts of the application
  UILogger := Logger.CreateContext('UI');
  DBLogger := Logger.CreateContext('DB');
  NetworkLogger := Logger.CreateContext('Network');
  
  // Log with categories
  UILogger.Info('Application window created');
  DBLogger.Info('Connected to database');
  NetworkLogger.Warning('Connection latency high');
  
  // Use format strings with categories
  DBLogger.ErrorFmt('Query failed: %s', ['Syntax error in SQL statement']);
  
  // No need to free context objects - they are automatically managed
end;
```

## Best Practices

1. **Initialization**: Set up the logger early in your application initialization
2. **Log Levels**: Use appropriate log levels and set minimum log level based on your deployment environment (e.g., Debug for development, Info or Warning for production)
3. **Categories**: Use categories to organize logs from different parts of your application
4. **Format Strings**: Use format string overloads instead of manually concatenating strings
5. **Method Chaining**: Use method chaining to configure the logger with a clean, fluent syntax
6. **Default Log Paths**: Use AddDefaultLogFile for simple setup
7. **Cleanup**: Call `CloseLogFiles` when shutting down to ensure proper cleanup
8. **Contexts**: Create logger contexts for different components of your application - they are automatically managed and don't need to be freed

## Limitations

1. The logger is primarily designed for single-process applications
2. While basic file operations are thread-safe, heavy multi-threaded use might benefit from additional synchronization
3. The file rotation is based on size checks before each write, not continuous monitoring

## Contributing

We welcome contributions to the TidyKit.Logger component. Please feel free to submit bug reports, feature requests, or pull requests. 