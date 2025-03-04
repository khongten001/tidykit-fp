# TidyKit.Log Module Documentation

The TidyKit.Log module provides a powerful, flexible, and easy-to-use logging system for Free Pascal applications. It features thread-safe operation, multiple output targets, log rotation, and colored console output.

## Features

- Thread-safe logging with non-blocking operations
- Multiple log targets (file and console) with easy extensibility
- Automatic log file rotation based on size
- Colored console output (Windows and ANSI terminals)
- Interface-based design with automatic memory management
- Fluent interface for easy configuration
- Support for formatted messages
- Multiple log levels (Debug, Info, Warning, Error, Fatal)
- Category-based logging support

## Installation

1. Add the following units to your project:
   - `TidyKit.Log.pas`
   - `TidyKit.Log.Targets.pas`

2. Add the units to your uses clause:
```pascal
uses
  TidyKit.Log, TidyKit.Log.Targets;
```

## Basic Usage

### Quick Start
```pascal
var
  Logger: ILogger;
begin
  // Console logging
  Logger := ConsoleLogger;
  Logger.Info('Application started');
  Logger.Warning('Resource usage high');
  
  // File logging
  Logger := FileLogger('app.log');
  Logger.Info('Starting process');
  Logger.Debug('Config loaded: %s', ['config.ini']);
  
  // Multi-target logging
  Logger := MultiLogger
    .AddTarget(TFileTarget.Create('app.log'))
    .AddTarget(TConsoleTarget.Create)
    .Enable;
  Logger.Info('Logging to both file and console');
end;
```

### Log Levels

The module supports five log levels, from lowest to highest priority:

```pascal
// Debug: Detailed information for troubleshooting
Logger.Debug('Connection pool size: %d', [Pool.Size]);

// Info: General operational messages
Logger.Info('Server started on port %d', [Port]);

// Warning: Potential issues that aren't errors
Logger.Warning('High memory usage: %d MB', [MemUsage]);

// Error: Error conditions that allow continued operation
Logger.Error('Failed to connect to %s: %s', [Server, E.Message]);

// Fatal: Severe errors that prevent proper execution
Logger.Fatal('Database connection lost - shutting down');
```

### Formatted Messages

```pascal
// Simple string formatting
Logger.Info('User %s logged in from %s', [Username, IPAddress]);

// Number formatting
Logger.Info('Memory usage: %.2f MB', [GetMemoryUsage / 1024 / 1024]);

// Multiple parameters
Logger.Info('Session %d: User=%s, Role=%s, Status=%s',
  [SessionID, User, Role, Status]);
```

## Advanced Usage

### Custom Configuration

```pascal
var
  Logger: ILogger;
  LogKit: TLogKit;
begin
  // Create with direct reference for configuration
  LogKit := TLogKit.Create;
  Logger := LogKit;  // Interface takes ownership
  
  // Configure multiple aspects
  LogKit.AddTarget(TFileTarget.Create('app.log')
         .SetMaxSize(50 * 1024 * 1024)
         .SetRotateCount(10))
       .AddTarget(TConsoleTarget.Create
         .EnableColors)
       .SetMinLevel(llWarning)
       .Enable;
       
  // Use interface for logging
  Logger.Warning('Important message');
  
  // Proper cleanup
  LogKit.Shutdown;
  Logger := nil;
  LogKit := nil;
end;
```

### File Rotation

```pascal
var
  Target: TFileTarget;
  Logger: ILogger;
begin
  // Create file target with rotation
  Target := TFileTarget.Create('app.log');
  Target.SetMaxSize(10 * 1024 * 1024)  // 10MB
        .SetRotateCount(5);             // Keep 5 backups
  
  // When size limit is reached:
  // - app.log -> app.log.1
  // - app.log.1 -> app.log.2
  // - etc...
  
  Logger := TLogKit.Create
    .AddTarget(Target)
    .Enable;
    
  // Log until rotation occurs
  for I := 1 to 1000000 do
    Logger.Info('Log message %d', [I]);
end;
```

### Thread Safety

The logging system is designed to be thread-safe by default:

```pascal
var
  Logger: ILogger;
  Threads: array[1..5] of TThread;
begin
  Logger := ConsoleLogger;
  
  // Create multiple logging threads
  for I := 1 to 5 do
    Threads[I] := TThread.CreateAnonymousThread(
      procedure
      var
        J: Integer;
      begin
        for J := 1 to 1000 do
          Logger.Info('Thread %d: Message %d', [I, J]);
      end
    );
    
  // Start all threads
  for I := 1 to 5 do
    Threads[I].Start;
    
  // Wait for completion
  for I := 1 to 5 do
    Threads[I].WaitFor;
end;
```

### Custom Targets

Creating a custom log target:

```pascal
type
  TDatabaseTarget = class(TInterfacedObject, ILogTarget)
  private
    FConnection: TSQLConnection;
    FLock: TCriticalSection;
  public
    constructor Create(const AConnectionString: string);
    destructor Destroy; override;
    
    { ILogTarget implementation }
    function GetName: string;
    procedure WriteLog(const AEntry: TLogEntry);
    procedure Flush;
  end;

constructor TDatabaseTarget.Create(const AConnectionString: string);
begin
  inherited Create;
  FConnection := TSQLConnection.Create(AConnectionString);
  FLock := TCriticalSection.Create;
end;

destructor TDatabaseTarget.Destroy;
begin
  FLock.Free;
  FConnection.Free;
  inherited;
end;

procedure TDatabaseTarget.WriteLog(const AEntry: TLogEntry);
begin
  FLock.Enter;
  try
    FConnection.ExecuteSQL(
      'INSERT INTO Logs (Level, Message, TimeStamp, ThreadID) ' +
      'VALUES (?, ?, ?, ?)',
      [Ord(AEntry.Level), AEntry.Message, 
       AEntry.TimeStamp, AEntry.ThreadID]
    );
  finally
    FLock.Leave;
  end;
end;
```

## Memory Management

### Interface-Based Design

The logging system uses interfaces for automatic memory management:

```pascal
procedure UseLogger;
var
  Logger: ILogger;         // Interface - automatically managed
  Target: ILogTarget;      // Interface - automatically managed
begin
  Logger := ConsoleLogger;
  Target := TFileTarget.Create('app.log');
  
  Logger.Info('Message');
  // Both Logger and Target automatically freed when they go out of scope
end;
```

### Direct References

When you need to access implementation-specific methods:

```pascal
var
  Logger: ILogger;
  LogKit: TLogKit;
  Target: ILogTarget;
  FileTarget: TFileTarget;
begin
  // Create objects
  LogKit := TLogKit.Create;
  Logger := LogKit;  // Interface takes ownership
  
  FileTarget := TFileTarget.Create('app.log');
  Target := FileTarget;  // Interface takes ownership
  
  // Use implementation methods
  FileTarget.SetMaxSize(10 * 1024 * 1024);
  LogKit.AddTarget(Target);
  LogKit.Enable;
  
  // Use interface for logging
  Logger.Info('Message');
  
  // Cleanup
  LogKit.Shutdown;
  Logger := nil;
  LogKit := nil;
  Target := nil;
  FileTarget := nil;
end;
```

## Error Handling

### Logging Exceptions

```pascal
procedure ProcessData;
begin
  try
    // Complex operation
    DoSomethingRisky;
  except
    on E: EDatabaseError do
    begin
      Logger.Error('Database error: %s', [E.Message]);
      raise;  // Re-raise if needed
    end;
    on E: Exception do
    begin
      Logger.Fatal('Unexpected error: %s', [E.Message]);
      raise;
    end;
  end;
end;
```

### Target Error Handling

Log targets should handle errors gracefully:

```pascal
procedure TFileTarget.WriteLog(const AEntry: TLogEntry);
begin
  try
    // Attempt to write log
    EnsureFileOpen;
    WriteLn(FFile, FormatLogEntry(AEntry));
    Flush;
  except
    // Silently continue - don't let logging errors
    // affect the main application
  end;
end;
```

## Performance Optimization

### Minimum Log Levels

```pascal
// Only process Warning and above
Logger := TLogKit.Create
  .SetMinLevel(llWarning)
  .Enable;

Logger.Debug('Not processed');  // Skipped
Logger.Info('Not processed');   // Skipped
Logger.Warning('Processed');    // Logged
Logger.Error('Processed');      // Logged
```

### Efficient Formatting

```pascal
// WRONG: String concatenation
Logger.Info('User ' + Username + ' logged in from ' + IPAddress);

// CORRECT: Format string
Logger.Info('User %s logged in from %s', [Username, IPAddress]);

// WRONG: Complex calculation in all cases
Logger.Debug('Memory stats: %s', [CalculateComplexStats]);

// CORRECT: Only calculate if needed
if Logger.IsDebugEnabled then
  Logger.Debug('Memory stats: %s', [CalculateComplexStats]);
```

## Testing

### Memory Target

```pascal
procedure TestLogging;
var
  Target: TMemoryTarget;
  Logger: ILogger;
begin
  Target := TMemoryTarget.Create;
  try
    Logger := TLogKit.Create
      .AddTarget(Target)
      .Enable;
      
    Logger.Info('Test message');
    Logger.Warning('Test warning');
    
    AssertEquals(2, Target.GetEntryCount);
    AssertTrue(Pos('INFO', Target.GetEntry(0)) > 0);
    AssertTrue(Pos('WARNING', Target.GetEntry(1)) > 0);
  finally
    Target.Free;
  end;
end;
```

## Best Practices

1. Use interface references by default
2. Set appropriate minimum log levels in production
3. Include context in log messages
4. Use formatted messages instead of concatenation
5. Handle target errors gracefully
6. Follow proper cleanup sequences
7. Test logging in multi-threaded scenarios
8. Consider log rotation for long-running applications
9. Use categories for better log organization
10. Include timestamps and thread IDs in production logs 