program LoggerExample;

{$mode objfpc}{$H+}

(*******************************************************************************
 * TidyKit.Logger Example Program
 *
 * This example program demonstrates how to use TidyKit.Logger through a series
 * of practical examples. The examples progress from basic to advanced usage,
 * showing you step-by-step how to use the logger effectively.
 *
 * To run this example:
 * 1. Compile this program using Free Pascal Compiler
 * 2. Execute the resulting binary
 * 3. Follow the console output to see how each feature works
 *
 * Each example is self-contained and includes clear comments explaining what
 * is happening and why you would use a particular feature.
 *******************************************************************************)

(*******************************************************************************
 * NOTE ABOUT TLogger.ResetInstance:
 * 
 * This example program uses TLogger.ResetInstance frequently to ensure each
 * demo starts with a fresh logger configuration. In a real application, you
 * would typically call ResetInstance much less frequently - usually only when:
 * - Making major configuration changes
 * - Transitioning between application phases
 * - Cleaning up resources at shutdown
 * - Ensuring isolation between test cases
 *******************************************************************************)

uses
  SysUtils,
  Classes,
  TidyKit.Logger;

  //------------------------------------------------------------------------------
  // HELPER PROCEDURES
  //------------------------------------------------------------------------------

  // Displays a section header to organize the console output
  procedure ShowHeader(const AMessage: string);
  begin
    WriteLn;
    WriteLn('--------------------------------------------------');
    WriteLn(' ', AMessage);
    WriteLn('--------------------------------------------------');
  end;

  // Displays a subsection header for related examples
  procedure ShowSubHeader(const AMessage: string);
  begin
    WriteLn;
    WriteLn(' * ', AMessage, ' *');
    WriteLn;
  end;

  //------------------------------------------------------------------------------
  // BASIC EXAMPLES - Start here if you're new to TidyKit.Logger
  //------------------------------------------------------------------------------

  // This demo shows the simplest way to get started with TidyKit.Logger
  procedure DemoQuickStart;
  begin
    ShowHeader('QUICK START - The Simplest Way to Use TidyKit.Logger');

    // Step 1: Reset any existing logger (for demo purposes only)
    TLogger.ResetInstance;

    // Step 2: One-line setup - Configure logger for both console and file output
    // This is usually the first thing you do in your application's initialization
    TLogger.CreateConsoleAndFileLogger('quickstart.log', llInfo);
    WriteLn('Logger created with one-line setup - ready to use immediately');

    // Step 3: Use the global Logger function to log messages
    Logger.Info('Application started');                         // Basic message
    Logger.Warning('Disk space is low: %d%% remaining', [5]);   // With formatting
    Logger.Error('Failed to save file: %s', ['Access denied']); // With error details

    // Step 4: Clean up when finished (important!)
    WriteLn('Closing log files...');
    Logger.CloseLogFiles;

    // For this example only: clean up the log file we created
    if FileExists('quickstart.log') then
      DeleteFile('quickstart.log');
  end;

  // This demo shows how to use different log levels and when to use each one
  procedure DemoBasicLogging;
  begin
    ShowHeader('BASIC LOGGING - Using Different Log Levels');

    // Reset logger and configure it for console output with Debug level
    TLogger.ResetInstance;
    Logger.SetLogDestinations([ldConsole]);
    Logger.SetMinLogLevel(llDebug);

    // Demonstrate different log levels
    ShowSubHeader('Log Levels (All showing with llDebug minimum level)');
    Logger.Debug('DEBUG: Detailed information for development/troubleshooting');
    Logger.Info('INFO: General information about normal operation');
    Logger.Warning('WARNING: Something unusual that might need attention');
    Logger.Error('ERROR: Something failed but execution can continue');
    Logger.Fatal('FATAL: Critical error that might cause program termination');

    // Demonstrate format string usage for including variable data
    ShowSubHeader('Including Variables in Log Messages');
    Logger.DebugFmt('Processing item %d of %d: "%s"', [1, 10, 'Example Item']);

    // Show how minimum log level filtering works
    ShowSubHeader('Minimum Log Level Filtering');
    WriteLn('Setting minimum log level to WARNING...');
    Logger.SetMinLogLevel(llWarning);

    WriteLn('These messages WILL appear (WARNING and above):');
    Logger.Warning('A warning message');
    Logger.Error('An error message');
    Logger.Fatal('A fatal message');

    WriteLn;
    WriteLn('These messages will NOT appear (below WARNING):');
    Logger.Debug('A debug message - filtered out');
    Logger.Info('An info message - filtered out');

    // Reset to show all messages for subsequent demos
    Logger.SetMinLogLevel(llDebug);
  end;

  // This demo shows how to use category-based logging to organize your logs
  procedure DemoCategoryBasedLogging;
  var
    UILogger: ILogContext;
    DBLogger: ILogContext;
    NetworkLogger: ILogContext;
  begin
    ShowHeader('CATEGORY-BASED LOGGING - Organizing Logs by Component');

    // Reset the logger to start fresh
    TLogger.ResetInstance;

    // Create a basic console logger
    TLogger.CreateConsoleLogger(llDebug);

    WriteLn('Creating category loggers for different parts of the application...');

    // Create specialized loggers for different categories
    UILogger := Logger.CreateContext('UI');
    DBLogger := Logger.CreateContext('Database');
    NetworkLogger := Logger.CreateContext('Network');

    // Each category logger adds its category name to the message
    UILogger.Info('Application window created');
    // Shows: [UI] Application window created
    DBLogger.Info('Connected to database');
    // Shows: [Database] Connected to database
    NetworkLogger.Warning('Network latency high: 350ms');
    // Shows: [Network] Network latency high: 350ms

    // Use format strings with categories
    DBLogger.ErrorFmt('Query failed: %s', ['Syntax error in SQL statement']);
    NetworkLogger.InfoFmt('Received %d bytes from %s', [1024, '192.168.1.10']);

    // No need to manually release contexts - they will be automatically freed
    // when they go out of scope thanks to interface reference counting
  end;

  // This demo shows how to log to files instead of or in addition to the console
  procedure DemoFileLogging;
  var
    LogFilePath: string;
    LogIndex: integer;
    FileContents: TStringList;
    UILogger, DBLogger, NetworkLogger: ILogContext;
  begin
    ShowHeader('FILE LOGGING - Writing Logs to Files');

    // Reset and configure logger for file output only
    TLogger.ResetInstance;
    Logger.SetLogDestinations([ldFile]);

    // Create a log file in the current directory
    LogFilePath := 'example.log';
    if FileExists(LogFilePath) then
      DeleteFile(LogFilePath);

    // Add a log file and get its index
    LogIndex := Logger.AddLogFile(LogFilePath);
    WriteLn('Created log file: ', LogFilePath);

    // Write some log messages to the file
    Logger.Info('Starting file logging demo');
    Logger.Warning('This is a test warning - written to file only');
    Logger.ErrorFmt('Error code: %d - %s', [404, 'Not Found']);

    // Close log files to ensure all content is written
    Logger.CloseLogFiles;

    // Display log file contents to see what was written
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

    // Demonstrate category-based logging in files
    ShowSubHeader('Category-Based Logging in Files');

    // Configure logger for file output and console for this demo
    TLogger.ResetInstance;
    Logger.SetLogDestinations([ldConsole, ldFile]);
    LogFilePath := 'categories.log';
    Logger.AddLogFile(LogFilePath);

    // Create category contexts
    UILogger := Logger.CreateContext('UI');
    DBLogger := Logger.CreateContext('Database');
    NetworkLogger := Logger.CreateContext('Network');

    // Log with categories
    UILogger.Info('Application window created');
    DBLogger.Info('Connected to database');
    NetworkLogger.Warning('Network latency high: 350ms');

    // Use format strings with categories
    DBLogger.ErrorFmt('Query failed: %s', ['Syntax error in SQL statement']);
    NetworkLogger.InfoFmt('Received %d bytes from %s', [1024, '192.168.1.10']);

    // No need to free context objects - they are automatically managed by interface reference counting
    WriteLn('Note: Context objects are automatically managed by interface reference counting');

    // Direct category logging without context objects
    ShowSubHeader('Direct Category Logging');
    WriteLn('Logging with categories without creating ILogContext objects:');

    // Alternative: Use direct category methods without creating context objects
    Logger.InfoWithCategory('System', 'System initialization complete');
    Logger.ErrorWithCategory('Security', 'Invalid login attempt from IP: 192.168.1.100');

    // Clean up log files for this demo
    Logger.CloseLogFiles;
    if FileExists('categories.log') then
      DeleteFile('categories.log');
  end;

  //------------------------------------------------------------------------------
  // INTERMEDIATE EXAMPLES - Once you understand the basics
  //------------------------------------------------------------------------------

  // This demo shows how to measure the performance of operations
  procedure DemoPerformanceTiming;
  var
    Timer: ITimedOperation;
    i: integer;
    OuterTimer: ITimedOperation;
    InnerTimer: ITimedOperation;
  begin
    ShowHeader('PERFORMANCE TIMING - Measuring Operation Duration');

    // Reset instance and configure
    TLogger.ResetInstance;
    Logger.SetLogDestinations([ldConsole]);

    // Create a timed operation block
    ShowSubHeader('Basic Timing Block');
    WriteLn('Creating a timer that will log when it goes out of scope...');

    begin
      // Start the timer - this logs the beginning of the operation
      Timer := Logger.TimedBlock('Data processing operation');

      // Simulate work
      WriteLn('Performing work...');
      for i := 1 to 5 do
      begin
        Sleep(100); // Simulate work
        Write('.');
      end;
      WriteLn;

      // When Timer goes out of scope at the end of this block,
      // it automatically logs completion with duration
      WriteLn('Timer will log completion message when it goes out of scope');
    end;

    ShowSubHeader('Nested Timing Blocks');
    WriteLn('You can nest timing blocks for more detailed performance analysis:');
    
    OuterTimer := Logger.TimedBlock('Outer operation');

    // Simulate some initial work
    Sleep(100);

    // Create a nested timer
    InnerTimer := Logger.TimedBlock('Inner operation');
    Sleep(200); // Simulate inner work

    // InnerTimer logs completion here (when it goes out of scope)

    // More outer work
    Sleep(100);

    // OuterTimer logs completion here (when it goes out of scope)
  end;

  // This demo shows how to use method chaining for fluent configuration
  procedure DemoMethodChaining;
  begin
    ShowHeader('METHOD CHAINING - Configuring with Fluent Syntax');

    // Reset instance
    TLogger.ResetInstance;

    // Configure with method chaining
    // This allows you to configure multiple settings in a single statement
    WriteLn('Configuring logger with method chaining...');
    Logger
      .SetLogDestinations([ldConsole])
      .SetMinLogLevel(llInfo)
      .SetDateTimeFormat('yyyy-mm-dd hh:nn:ss')
      .SetFormat('[%time] [%level] %message');

    WriteLn('Logger configured with method chaining');
    Logger.Info('Using custom datetime format');
    Logger.Warning('All settings configured in one fluent statement');
  end;

  // This demo shows how to improve performance with batch logging
  procedure DemoBatchLogging;
  var
    i: integer;
    StartTime: TDateTime;
    EndTime: TDateTime;
  begin
    ShowHeader('BATCH LOGGING - Improving Performance for Multiple Messages');

    // Reset instance
    TLogger.ResetInstance;
    Logger.SetLogDestinations([ldConsole]);
    
    // First, log messages individually (slower)
    ShowSubHeader('Individual Logging (Slower)');
    WriteLn('Logging 10 messages individually:');

    StartTime := Now;
    for i := 1 to 10 do
      Logger.Info('Individual log message #' + IntToStr(i));
    EndTime := Now;

    WriteLn(Format('Time taken: %.6f seconds',
      [(EndTime - StartTime) * 24 * 60 * 60]));

    // Then, log messages in batch (faster)
    ShowSubHeader('Batch Logging (Faster)');
    WriteLn('Logging 10 messages in batch:');

    StartTime := Now;
    Logger.BeginBatch;
    try
      for i := 1 to 10 do
        Logger.Info('Batch log message #' + IntToStr(i));
    finally
      Logger.EndBatch; // Writes all messages at once
    end;
    EndTime := Now;

    WriteLn(Format('Time taken: %.6f seconds',
      [(EndTime - StartTime) * 24 * 60 * 60]));

    WriteLn('Note: The performance difference becomes more significant');
    WriteLn('      with larger numbers of messages or when writing to files.');
  end;

  // This demo shows how to use factory methods to create specialized loggers
  procedure DemoFactoryMethods;
  var
    ConsoleLogger: TLogger;
    FileLogger: TLogger;
    BothLogger: TLogger;
    DebugLogger: TLogger;
    AuditLogger: TLogger;
  begin
    ShowHeader('FACTORY METHODS - Creating Specialized Loggers');
    
    // Various factory methods for different use cases

    ShowSubHeader('Basic Logger Types');
    WriteLn('Creating console-only logger with Debug level...');
    ConsoleLogger := TLogger.CreateConsoleLogger(llDebug);
    ConsoleLogger.Debug('This is a console-only debug message');

    WriteLn('Creating file-only logger with Info level...');
    FileLogger := TLogger.CreateFileLogger('file_only.log', llInfo);
    FileLogger.Info('This is a file-only info message');
    FileLogger.Debug('This debug message should NOT appear in the file');

    WriteLn('Creating console and file logger...');
    BothLogger := TLogger.CreateConsoleAndFileLogger('console_and_file.log',
      llWarning);
    BothLogger.Warning('This warning should appear in console and file');
    BothLogger.Info('This info message should NOT appear anywhere');

    ShowSubHeader('Specialized Logger Types');
    WriteLn('Creating debug logger (shows file and line information)...');
    DebugLogger := TLogger.CreateDebugLogger;
    DebugLogger.Debug('This debug message includes source file location');

    WriteLn('Creating audit logger (for security events)...');
    AuditLogger := TLogger.CreateAuditLogger('audit.log');
    AuditLogger.Info('User logged in: admin');
    AuditLogger.Warning('Failed login attempt: unknown_user');

    // Clean up
    WriteLn('Cleaning up log files...');
    FileLogger.CloseLogFiles;
    BothLogger.CloseLogFiles;
    DebugLogger.CloseLogFiles;
    AuditLogger.CloseLogFiles;

    if FileExists('file_only.log') then
      DeleteFile('file_only.log');

    if FileExists('console_and_file.log') then
      DeleteFile('console_and_file.log');

    if FileExists('audit.log') then
      DeleteFile('audit.log');

    if FileExists('debug.log') then
      DeleteFile('debug.log');
  end;

  //------------------------------------------------------------------------------
  // ADVANCED EXAMPLES - For more complex logging needs
  //------------------------------------------------------------------------------

  // This demo shows how to log structured data with key-value pairs
  procedure DemoStructuredLogging;
  begin
    ShowHeader('STRUCTURED LOGGING - Logging Key-Value Data');

    // Reset instance
    TLogger.ResetInstance;
    Logger.SetLogDestinations([ldConsole]);

    // Show how to log structured data with key-value pairs
    ShowSubHeader('Key-Value Pair Logging');
    WriteLn('Logging structured data for a user login event:');

    // Log structured data as key-value pairs
    Logger.LogStructured(llInfo, 'User login',
      [NameValuePair('username', 'john_doe'), NameValuePair('ip_address',
      '192.168.1.10'), NameValuePair('success', True), NameValuePair('attempt', 3)]);

    WriteLn('Logging structured data for a database query:');
    Logger.LogStructured(llWarning, 'Database query slow',
      [NameValuePair('query', 'SELECT * FROM large_table'),
      NameValuePair('duration_ms', 1250), NameValuePair('rows_returned', 5000)]);

    // Show type-specific value logging
    ShowSubHeader('Type-Specific Value Logging');
    WriteLn('Logging values with their native types:');

    Logger.LogValue('user_count', 42, llInfo);
    Logger.LogValue('temperature', 98.6, llInfo);
    Logger.LogValue('is_active', True, llInfo);
    Logger.LogValue('username', 'john_doe', llInfo);
  end;

  // This demo shows how to manage logger instances
  procedure DemoInstanceManagement;
  var
    FirstID: int64;
    SecondID: int64;
  begin
    ShowHeader('INSTANCE MANAGEMENT - Working with Logger Instances');

    // Reset to start fresh
    TLogger.ResetInstance;

    // Get the instance ID of the current logger
    FirstID := Logger.GetInstanceID;
    WriteLn('First logger instance ID: ', FirstID);

    // Log something with first instance
    Logger.Info('Using first logger instance');

    // Reset the instance - this destroys the current instance and creates a new one
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

    ShowSubHeader('When to Use Instance Management');
    WriteLn('Use instance management when:');
    WriteLn('1. You need to completely reconfigure the logger');
    WriteLn('2. You want to clear all sinks and start fresh');
    WriteLn('3. During testing to ensure isolation between test cases');
  end;

  // This demo shows how the logger handles and recovers from errors
  procedure DemoErrorRecovery;
  var
    InvalidPath: string;
    ValidPath: string;
  begin
    ShowHeader('ERROR RECOVERY - How the Logger Handles Errors');

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

  // This demo shows the default logger setup (simplest way to get started)
  procedure DemoDefaultLoggerSetup;
  begin
    ShowHeader('DEFAULT LOGGER SETUP - One-Line Configuration');

    // Reset instance
    TLogger.ResetInstance;

    // Use the one-line setup method
    TLogger.CreateDefaultLogger([ldConsole], '', llInfo);
    WriteLn('Default logger created with console output only');

    Logger.Info('This info message should appear');
    Logger.Debug('This debug message should NOT appear (below minimum level)');

    // Clean up
    Logger.CloseLogFiles;
  end;

  //------------------------------------------------------------------------------
  // MAIN PROGRAM
  //------------------------------------------------------------------------------

begin
  WriteLn('TidyKit.Logger Demo Application');
  WriteLn('==============================');
  WriteLn('This program demonstrates how to use TidyKit.Logger effectively.');
  WriteLn('The examples progress from basic to advanced usage.');
  WriteLn;
  WriteLn('Press Enter after each demo to continue to the next one...');

  try
    //----------------------------------------------------------------------
    // BASIC DEMOS - Start here for beginners
    //----------------------------------------------------------------------
    DemoQuickStart;
    ReadLn;

    DemoBasicLogging;
    ReadLn;

    DemoCategoryBasedLogging;
    ReadLn;

    DemoFileLogging;
    ReadLn;

    //----------------------------------------------------------------------
    // INTERMEDIATE DEMOS - More advanced features
    //----------------------------------------------------------------------
    DemoPerformanceTiming;
    ReadLn;

    DemoMethodChaining;
    ReadLn;

    DemoBatchLogging;
    ReadLn;

    DemoFactoryMethods;
    ReadLn;

    //----------------------------------------------------------------------
    // ADVANCED DEMOS - For more complex logging needs
    //----------------------------------------------------------------------
    DemoStructuredLogging;
    ReadLn;

    DemoInstanceManagement;
    ReadLn;

    DemoErrorRecovery;
    ReadLn;

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
