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

##### `procedure SetLogDestinations(ADestinations: TLogDestinations)`

Sets the active log destinations. Can be a set of:
- `ldConsole`: Log to console (stdout)
- `ldFile`: Log to file(s)

```pascal
// Log to console only
Logger.SetLogDestinations([ldConsole]);

// Log to both console and file
Logger.SetLogDestinations([ldConsole, ldFile]);

// Disable all logging
Logger.SetLogDestinations([]);
```

##### `procedure SetDateTimeFormat(const AFormat: string)`

Sets the datetime format string used for timestamps in log messages.

```pascal
// Default format
Logger.SetDateTimeFormat('yyyy-mm-dd hh:nn:ss.zzz');

// Custom format
Logger.SetDateTimeFormat('yyyy-mm-dd hh:nn');
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

##### `procedure CloseLogFiles`

Closes all log files and clears the log file list.

```pascal
Logger.CloseLogFiles;
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

#### Singleton Management

##### `class function GetInstance: TLogger`

Returns the singleton instance of the TLogger class. This is the method called by the `Logger` function.

##### `class procedure ResetInstance`

Resets (destroys and recreates) the singleton instance. Useful for testing or when complete reset is needed.

```pascal
// Reset the logger instance
TLogger.ResetInstance;
```

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

## Best Practices

1. **Initialization**: Set up the logger early in your application initialization
2. **Error Handling**: The logger contains internal error handling to prevent crashes, but it's good practice to check file operation results
3. **Log Levels**: Use appropriate log levels for different message types
4. **File Rotation**: Set realistic maximum file sizes to prevent excessive disk usage
5. **Cleanup**: Call `CloseLogFiles` when shutting down to ensure proper cleanup

## Limitations

1. The logger is primarily designed for single-process applications
2. While basic file operations are thread-safe, heavy multi-threaded use might benefit from additional synchronization
3. The file rotation is based on size checks before each write, not continuous monitoring

## Contributing

We welcome contributions to the TidyKit.Logger component. Please feel free to submit bug reports, feature requests, or pull requests. 