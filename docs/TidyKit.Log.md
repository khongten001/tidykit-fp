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

### File Logger

```pascal
var
  Log: ILogger;
begin
  Log := FileLogger('application.log');
  Log.Info('Application started');
  Log.Debug('Configuration loaded: %s', ['config.ini']);
  // Logger automatically freed when it goes out of scope
end;
```

### Console Logger

```pascal
var
  Log: ILogger;
begin
  Log := ConsoleLogger;
  Log.Info('Starting process');
  Log.Warning('Resource usage high');
  // Logger automatically freed when it goes out of scope
end;
```

### Multi-Target Logger

```pascal
var
  Log: ILogger;
begin
  Log := MultiLogger
    .AddTarget(TFileTarget.Create('app.log').SetMaxSize(1024 * 1024))
    .AddTarget(TConsoleTarget.Create.EnableColors)
    .SetMinLevel(llDebug)
    .Enable;

  Log.Info('Logging to both file and console');
  // Logger automatically freed when it goes out of scope
end;
```

## Log Levels

The module supports five log levels:

- `llDebug`: Detailed information for debugging
- `llInfo`: General information about program execution
- `llWarning`: Potentially harmful situations
- `llError`: Error events that might still allow the application to continue
- `llFatal`: Severe errors that prevent proper program execution

## File Target Features

### Log Rotation

The file target supports automatic log rotation based on file size:

```pascal
var
  Target: TFileTarget;
begin
  Target := TFileTarget.Create('app.log')
    .SetMaxSize(10 * 1024 * 1024)  // 10 MB
    .SetRotateCount(5);            // Keep 5 backup files
end;
```

When the log file reaches the specified size:
1. Current file is renamed to `app.log.1`
2. Previous backup files are shifted (`.1` becomes `.2`, etc.)
3. Oldest backup file is deleted if it exceeds `RotateCount`
4. A new empty log file is created

## Console Target Features

### Colored Output

The console target supports colored output based on log level:

- Debug: Cyan
- Info: White
- Warning: Yellow
- Error: Red
- Fatal: Magenta

Colors can be enabled or disabled:

```pascal
var
  Target: TConsoleTarget;
begin
  Target := TConsoleTarget.Create
    .EnableColors;   // or .DisableColors
end;
```

## Thread Safety

The logging module is designed to be thread-safe:

- Log entries are queued in a thread-safe buffer (capacity: 1000 entries)
- A background thread processes the queue and writes to targets
- Synchronized queue operations using critical sections
- Buffer overflow protection prevents memory issues

## Memory Management

The module uses interface-based reference counting for automatic memory management:

- No manual `Free` calls needed
- Resources are automatically cleaned up
- No memory leaks
- Thread-safe cleanup

## Performance Considerations

1. Log entries are buffered and processed asynchronously
2. File operations are batched for better performance
3. Lock-free queue minimizes thread contention
4. Automatic log rotation prevents unbounded file growth

## Message Categories

The logging system supports categorized messages:

```pascal
var
  Entry: TLogEntry;
begin
  Entry.Level := llInfo;
  Entry.Message := 'Database connection established';
  Entry.Category := 'Database';
  Target.WriteLog(Entry);
end;
```

Categories allow you to:
- Group related messages
- Filter logs by component or subsystem
- Add context to log entries
- Improve log analysis and debugging

## Example Program

See `log_example.pas` in the project root for a complete example demonstrating all features.

## Best Practices

1. Use appropriate log levels:
   - Debug for detailed troubleshooting
   - Info for general operational events
   - Warning for potential issues
   - Error for actual problems
   - Fatal for severe failures

2. Include relevant context in log messages:
   - Timestamps are added automatically
   - Thread IDs are included
   - Add transaction IDs or user IDs when relevant

3. Use formatted messages for better readability:
   ```pascal
   Log.Info('User %s logged in from %s', [Username, IPAddress]);
   ```

4. Configure appropriate log rotation for production:
   ```pascal
   Target := TFileTarget.Create('app.log')
     .SetMaxSize(50 * 1024 * 1024)  // 50 MB
     .SetRotateCount(10);           // Keep 10 backups
   ```

5. Handle logging errors gracefully:
   ```pascal
   try
     // Application code
   except
     on E: Exception do
       Log.Error('Error processing request: %s', [E.Message]);
   end;
   ```

## Contributing

1. Follow the Free Pascal coding style
2. Maintain thread safety in new features
3. Add appropriate documentation
4. Include unit tests for new functionality
5. Submit pull requests with clear descriptions

## License

This module is part of the TidyKit library and is available under the same license terms as the main project.

## Testing

### Memory Target

The `TMemoryTarget` class is provided for testing purposes. It stores log entries in memory, making it easy to verify logging behavior in unit tests:

```pascal
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
    
    AssertEquals(1, Target.GetEntryCount);
    AssertTrue(Pos('INFO', Target.GetEntry(0)) > 0);
  finally
    Target.Free;
  end;
end;
```

The memory target is not intended for production use - it's specifically designed for testing scenarios where you need to:
- Verify log message content
- Check log levels
- Count logged messages
- Test message formatting
- Avoid file system or console dependencies

## Performance Considerations

1. Log entries are buffered and processed asynchronously
2. File operations are batched for better performance
3. Lock-free queue minimizes thread contention
4. Automatic log rotation prevents unbounded file growth 