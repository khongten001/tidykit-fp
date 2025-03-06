program LoggerExample;

{$mode objfpc}{$H+}

uses
  SysUtils,
  Classes,
  TidyKit.Logger;

procedure ShowHeader(const AMessage: string);
begin
  WriteLn;
  WriteLn('--------------------------------------------------');
  WriteLn(' ', AMessage);
  WriteLn('--------------------------------------------------');
end;

procedure DemoQuickStart;
begin
  ShowHeader('Quick Start Demo');
  
  // Reset instance
  TLogger.ResetInstance;
  
  // One-line setup for console and file logging
  TLogger.CreateConsoleAndFileLogger('quickstart.log', llInfo);
  WriteLn('Logger created with one-line setup method');
  
  // Use the logger immediately
  Logger.Info('Application started');
  Logger.Warning('Disk space is low: %d%% remaining', [5]);
  Logger.Error('Failed to save file: %s', ['Access denied']);
  
  // Clean up
  Logger.CloseLogFiles;
  if FileExists('quickstart.log') then
    DeleteFile('quickstart.log');
end;

procedure DemoPerformanceTiming;
var
  Timer: ITimedOperation;
  i: Integer;
begin
  ShowHeader('Performance Timing Demo');
  
  // Reset instance
  TLogger.ResetInstance;
  Logger.SetLogDestinations([ldConsole]);
  
  // Create a timed operation
  Timer := Logger.TimedBlock('Data processing operation');
  
  // Simulate work
  WriteLn('Performing work...');
  for i := 1 to 5 do
  begin
    Sleep(100); // Simulate work
    Write('.');
  end;
  WriteLn;
  
  // Timer will automatically log when it goes out of scope
  WriteLn('Timer will log completion message when it goes out of scope');
end;

procedure DemoStructuredLogging;
begin
  ShowHeader('Structured Logging Demo');
  
  // Reset instance
  TLogger.ResetInstance;
  Logger.SetLogDestinations([ldConsole]);
  
  // Log structured data
  Logger.LogStructured(llInfo, 'User login', [
    NameValuePair('username', 'john_doe'),
    NameValuePair('ip_address', '192.168.1.10'),
    NameValuePair('success', True),
    NameValuePair('attempt', 3)
  ]);
  
  Logger.LogStructured(llWarning, 'Database query slow', [
    NameValuePair('query', 'SELECT * FROM large_table'),
    NameValuePair('duration_ms', 1250),
    NameValuePair('rows_returned', 5000)
  ]);
end;

procedure DemoFactoryMethods;
var
  ConsoleLogger, FileLogger, BothLogger: TLogger;
begin
  ShowHeader('Factory Methods Demo');
  
  // Create different types of loggers
  WriteLn('Creating console-only logger with Debug level...');
  ConsoleLogger := TLogger.CreateConsoleLogger(llDebug);
  ConsoleLogger.Debug('This is a console-only debug message');
  
  WriteLn('Creating file-only logger with Info level...');
  FileLogger := TLogger.CreateFileLogger('file_only.log', llInfo);
  FileLogger.Info('This is a file-only info message');
  FileLogger.Debug('This debug message should NOT appear in the file');
  
  WriteLn('Creating console and file logger...');
  BothLogger := TLogger.CreateConsoleAndFileLogger('console_and_file.log', llWarning);
  BothLogger.Warning('This warning should appear in console and file');
  BothLogger.Info('This info message should NOT appear anywhere');
  
  // Clean up
  FileLogger.CloseLogFiles;
  BothLogger.CloseLogFiles;
  
  if FileExists('file_only.log') then
    DeleteFile('file_only.log');
    
  if FileExists('console_and_file.log') then
    DeleteFile('console_and_file.log');
end;

procedure DemoBatchLogging;
var
  i: Integer;
begin
  ShowHeader('Batch Logging Demo');
  
  // Reset instance
  TLogger.ResetInstance;
  Logger.SetLogDestinations([ldConsole]);
  
  WriteLn('Logging 10 messages individually:');
  for i := 1 to 10 do
    Logger.Info('Individual log message #' + IntToStr(i));
    
  WriteLn;
  WriteLn('Logging 10 messages in batch:');
  
  Logger.BeginBatch;
  try
    for i := 1 to 10 do
      Logger.Info('Batch log message #' + IntToStr(i));
  finally
    Logger.EndBatch; // Writes all messages at once
  end;
  
  WriteLn('Batch of 10 messages was written all at once');
end;

procedure DemoBasicLogging;
begin
  ShowHeader('Basic Logging Demo');
  
  // First, make sure we're starting fresh
  TLogger.ResetInstance;
  
  // Configure logger for console output
  Logger.SetLogDestinations([ldConsole]);
  Logger.SetMinLogLevel(llDebug);
  
  // Demonstrate different log levels
  Logger.Debug('This is a DEBUG message - detailed debugging information');
  Logger.Info('This is an INFO message - general information about program flow');
  Logger.Warning('This is a WARNING message - potential issue that might need attention');
  Logger.Error('This is an ERROR message - something went wrong but execution continues');
  Logger.Fatal('This is a FATAL message - critical error that might cause termination');

  // Demonstrate format string usage
  Logger.DebugFmt('Processing item %d of %d: "%s"', [1, 10, 'Example Item']);
  
  // Show effect of minimum log level
  WriteLn;
  WriteLn('Setting minimum log level to WARNING...');
  Logger.SetMinLogLevel(llWarning);
  
  WriteLn('These messages should appear:');
  Logger.Warning('This warning message should appear');
  Logger.Error('This error message should appear');
  Logger.Fatal('This fatal message should appear');
  
  WriteLn('These messages should NOT appear:');
  Logger.Debug('This debug message should not appear');
  Logger.Info('This info message should not appear');
end;

procedure DemoFileLogging;
var
  LogFilePath: string;
  LogIndex: Integer;
  FileContents: TStringList;
begin
  ShowHeader('File Logging Demo');
  
  // Reset and configure logger for file output
  TLogger.ResetInstance;
  Logger.SetLogDestinations([ldFile]);
  
  // Create a log file in the current directory
  LogFilePath := 'example.log';
  if FileExists(LogFilePath) then
    DeleteFile(LogFilePath);
    
  LogIndex := Logger.AddLogFile(LogFilePath);
  WriteLn('Created log file: ', LogFilePath);
  
  // Write some log messages
  Logger.Info('Starting file logging demo');
  Logger.Warning('This is a test warning');
  Logger.ErrorFmt('Error code: %d - %s', [404, 'Not Found']);
  
  // Close log files to ensure all content is written
  Logger.CloseLogFiles;
  
  // Display log file contents
  if FileExists(LogFilePath) then
  begin
    WriteLn('Log file contents:');
    WriteLn('-----------------');
    
    FileContents := TStringList.Create;
    try
      FileContents.LoadFromFile(LogFilePath);
      WriteLn(FileContents.Text);
    finally
      FileContents.Free;
    end;
    
    WriteLn('-----------------');
  end
  else
    WriteLn('Error: Log file not found!');
end;

procedure DemoCategoryBasedLogging;
var
  UILogger, DBLogger, NetworkLogger: TLogContext;
begin
  ShowHeader('Category-Based Logging Demo');
  
  // Reset and configure logger
  TLogger.ResetInstance;
  Logger.SetLogDestinations([ldConsole]);
  
  // Create specialized loggers for different categories
  UILogger := Logger.CreateContext('UI');
  DBLogger := Logger.CreateContext('Database');
  NetworkLogger := Logger.CreateContext('Network');
  
  // Use category-specific logging
  UILogger.Info('Application window created');
  DBLogger.Info('Connected to database');
  NetworkLogger.Warning('Network latency high: 350ms');
  
  // Use format strings with categories
  DBLogger.ErrorFmt('Query failed: %s', ['Syntax error in SQL statement']);
  NetworkLogger.InfoFmt('Received %d bytes from %s', [1024, '192.168.1.10']);
  
  // No need to free context objects - they are automatically managed
  WriteLn('Context objects are automatically managed by the logger');
end;

procedure DemoMethodChaining;
begin
  ShowHeader('Method Chaining Demo');
  
  // Reset instance
  TLogger.ResetInstance;
  
  // Configure with method chaining
  Logger
    .SetLogDestinations([ldConsole])
    .SetMinLogLevel(llInfo)
    .SetDateTimeFormat('yyyy-mm-dd hh:nn:ss');
    
  WriteLn('Logger configured with method chaining');
  Logger.Info('Using custom datetime format');
  Logger.Warning('All in one fluent configuration');
end;

procedure DemoInstanceManagement;
var
  FirstID, SecondID: Int64;
begin
  ShowHeader('Instance Management Demo');
  
  // Reset to start fresh
  TLogger.ResetInstance;
  
  // Get the instance ID of the current logger
  FirstID := Logger.GetInstanceID;
  WriteLn('First logger instance ID: ', FirstID);
  
  // Log something
  Logger.Info('Using first logger instance');
  
  // Reset the instance
  WriteLn('Resetting logger instance...');
  TLogger.ResetInstance;
  
  // Get the new instance ID
  SecondID := Logger.GetInstanceID;
  WriteLn('Second logger instance ID: ', SecondID);
  
  // Log something with new instance
  Logger.Info('Using second logger instance');
  
  // Verify instance has changed
  if FirstID <> SecondID then
    WriteLn('SUCCESS: Logger instance was changed (', FirstID, ' -> ', SecondID, ')')
  else
    WriteLn('ERROR: Logger instance did not change!');
end;

procedure DemoErrorRecovery;
var
  InvalidPath: string;
  ValidPath: string;
begin
  ShowHeader('Error Recovery Demo');
  
  // Reset instance
  TLogger.ResetInstance;
  
  // Try to log to an invalid location
  InvalidPath := '/invalid/directory/that/does/not/exist/error.log';
  Logger.SetLogDestinations([ldConsole, ldFile]);
  
  WriteLn('Attempting to add invalid log file: ', InvalidPath);
  try
    Logger.AddLogFile(InvalidPath);
    WriteLn('NOTE: If you see this message, the logger recovered from the error');
  except
    on E: Exception do
      WriteLn('ERROR: ', E.Message);
  end;
  
  // Now let's log to a valid location
  ValidPath := 'valid.log';
  WriteLn('Adding valid log file: ', ValidPath);
  Logger.AddLogFile(ValidPath);
  Logger.Info('This message should be logged to both console and file');
  Logger.CloseLogFiles;
  
  // Clean up
  if FileExists(ValidPath) then
    DeleteFile(ValidPath);
end;

procedure DemoDefaultLoggerSetup;
begin
  ShowHeader('Default Logger Setup Demo');
  
  // Reset instance
  TLogger.ResetInstance;
  
  // Use the one-line setup method
  TLogger.CreateDefaultLogger([ldConsole], '', llInfo);
  WriteLn('Default logger created with console output only');
  
  Logger.Info('This info message should appear');
  Logger.Debug('This debug message should NOT appear');
  
  // Clean up
  Logger.CloseLogFiles;
end;

// Main program
begin
  WriteLn('TidyKit.Logger Demo Application');
  WriteLn('==============================');

  try
    // New demos
    DemoQuickStart;
    DemoPerformanceTiming;
    DemoStructuredLogging;
    DemoFactoryMethods;
    DemoBatchLogging;
    
    // Original demos
    DemoBasicLogging;
    DemoFileLogging;
    DemoCategoryBasedLogging;
    DemoMethodChaining;
    DemoInstanceManagement;
    DemoErrorRecovery;
    DemoDefaultLoggerSetup;
    
    // Clean up
    TLogger.ResetInstance;
    WriteLn;
    WriteLn('All demos completed successfully');
    
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.Message);
      ExitCode := 1;
    end;
  end;
  
  // Wait for user input before closing
  WriteLn;
  Write('Press Enter to exit...');
  ReadLn;
end. 