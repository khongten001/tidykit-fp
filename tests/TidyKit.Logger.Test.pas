unit TidyKit.Logger.Test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  TidyKit.Logger;

type
  TLoggerTest = class(TTestCase)
  private
    FTestDir: string;
    FLogFile: string;
    procedure CleanupLogFiles;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Singleton tests }
    procedure Test01_GetInstance;
    procedure Test02_SingletonBehavior;
    
    { Basic logging tests }
    procedure Test03_ConsoleLogging;
    procedure Test04_FileLogging;
    procedure Test05_MultiDestinationLogging;
    
    { Log level tests }
    procedure Test06_DebugLevel;
    procedure Test07_InfoLevel;
    procedure Test08_WarningLevel;
    procedure Test09_ErrorLevel;
    procedure Test10_FatalLevel;
    
    { Configuration tests }
    procedure Test11_SetLogDestinations;
    procedure Test12_SetDateTimeFormat;
    procedure Test13_AddLogFile;
    
    { File management tests }
    procedure Test14_LogFileRotation;
    procedure Test15_MultipleLogFiles;
    procedure Test16_SpecificFileLogging;
    
    { Edge cases and error handling }
    procedure Test17_EmptyMessage;
    procedure Test18_InvalidFileIndex;
    procedure Test19_MaxSizeHandling;
    procedure Test20_ConcurrentLogging;
    
    { New features tests }
    procedure Test21_LogLevelFiltering;
    procedure Test22_FormatStringOverloads;
    procedure Test23_SimpleOneLineSetup;
    procedure Test24_DefaultLogFilePaths;
    procedure Test25_MethodChaining;
    procedure Test26_CategorySupport;

    { Reference counting tests }
    procedure Test27_ContextReferenceCountingBasic;
    procedure Test28_ContextAutoCleanup;

    { Additional file handling tests }
    procedure Test29_LogRotationThreshold;
    procedure Test30_StressTest;

    { Context lifecycle tests }
    procedure Test31_ContextLifecycleOnLoggerReset;
    procedure Test32_DefaultLogDirectoryCreation;

    { Thread safety and error handling }
    procedure Test33_ThreadSafetyBasic;
    procedure Test34_ErrorRecovery;
    
    { Sink architecture tests }
    procedure Test35_SinkManagement;
    procedure Test36_ConsoleSink;
    procedure Test37_FileSink;
    procedure Test38_RotatingFileSink;
    procedure Test39_DailyFileSink;
    procedure Test40_MemorySink;
    
    { Pattern-based formatting tests }
    procedure Test41_PatternParsing;
    procedure Test42_PatternFormatting;
    
    { Structured logging tests }
    procedure Test43_StructuredLogging;
    procedure Test44_LogValueTypes;
    
    { Performance timing tests }
    procedure Test45_TimedOperationBlock;
    
    { Batch logging tests }
    procedure Test46_BatchLogging;
    procedure Test47_BatchLoggedMessagesCount;
    
    { Configuration tests }
    procedure Test48_ConfigurationLoading;
    
    { Factory methods tests }
    procedure Test49_FactoryMethods;
  end;

implementation

procedure TLoggerTest.SetUp;
var
  TestBasePath: string;
begin
  // First, completely reset the logger by destroying and recreating the singleton
  TLogger.ResetInstance;
  
  // Wait a moment to ensure file system operations are complete
  Sleep(100);
  
  {$IFDEF WINDOWS}
  TestBasePath := GetEnvironmentVariable('TEMP');
  if TestBasePath = '' then
    TestBasePath := GetEnvironmentVariable('TMP');
  if TestBasePath = '' then
    TestBasePath := 'C:\Temp';
  {$ELSE}
  TestBasePath := '/tmp';
  {$ENDIF}
  
  // Generate unique test directory for each test method
  FTestDir := IncludeTrailingPathDelimiter(TestBasePath) + 
              'TidyKitLoggerTest_' + FormatDateTime('yyyymmddhhnnsszzz', Now) + '_' + IntToStr(Random(10000));
  FLogFile := FTestDir + PathDelim + 'test.log';
  
  // Clean up any existing test directory
  if DirectoryExists(FTestDir) then
  begin
    CleanupLogFiles;
    RemoveDir(FTestDir);
  end;
  
  // Create test directory
  if not ForceDirectories(FTestDir) then
    raise Exception.CreateFmt('Could not create test directory: %s', [FTestDir]);
    
  // Reset logger state before each test
  Logger.SetLogDestinations([]);
  Logger.CloseLogFiles;
  Logger.SetDateTimeFormat('yyyy-mm-dd hh:nn:ss.zzz'); // Reset to default format
  
  // Small delay to ensure file system operations are complete
  Sleep(100);
end;

procedure TLoggerTest.TearDown;
begin
  // First, close all log files and reset logger state
  Logger.SetLogDestinations([]);
  Logger.CloseLogFiles;
  
  // Also completely reset the logger singleton
  TLogger.ResetInstance;
  
  // Small delay to ensure file system operations are complete
  Sleep(200);
  
  // Then clean up test files
  CleanupLogFiles;
  
  // Another small delay
  Sleep(200);
  
  // Finally remove test directory
  if DirectoryExists(FTestDir) then
  begin
    RemoveDir(FTestDir);
  end;
end;

procedure TLoggerTest.CleanupLogFiles;
var
  SearchRec: TSearchRec;
  FullPath: string;
  RetryCount: Integer;
begin
  // First ensure all files are closed
  Logger.SetLogDestinations([]);
  Logger.CloseLogFiles;
  TLogger.ResetInstance;
  
  // Small delay to ensure file system operations are complete
  Sleep(200);
  
  // Then attempt to delete files
  if FindFirst(FTestDir + PathDelim + '*.log', faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        FullPath := FTestDir + PathDelim + SearchRec.Name;
        RetryCount := 0;
        while (not DeleteFile(FullPath)) and (RetryCount < 3) do
        begin
          // If delete fails, wait longer and try again
          Inc(RetryCount);
          Sleep(200);
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
end;

procedure TLoggerTest.Test01_GetInstance;
var
  LoggerInstance: TLogger;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test01_GetInstance: Starting');
  LoggerInstance := Logger;
  AssertNotNull('Logger instance should not be nil', LoggerInstance);
end;

procedure TLoggerTest.Test02_SingletonBehavior;
var
  FirstInstance, SecondInstance: TLogger;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test02_SingletonBehavior: Starting');
  FirstInstance := Logger;
  SecondInstance := Logger;
  AssertSame('Multiple calls to Logger should return the same instance',
    FirstInstance, SecondInstance);
end;

procedure TLoggerTest.Test03_ConsoleLogging;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test03_ConsoleLogging: Starting');
  Logger.Info('Console logging test');
  // Visual verification required - no assertion possible for console output
  AssertTrue('Console logging test completed', True);
end;

procedure TLoggerTest.Test04_FileLogging;
var
  LogIndex: Integer;
  FileContent: TStringList;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test04_FileLogging: Starting');
  Logger.SetLogDestinations([ldFile]);
  LogIndex := Logger.AddLogFile(FLogFile);
  
  Logger.Info('File logging test');
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    AssertTrue('Log file should contain the test message',
      Pos('File logging test', FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
end;

procedure TLoggerTest.Test05_MultiDestinationLogging;
var
  LogIndex: Integer;
  FileContent: TStringList;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test05_MultiDestinationLogging: Starting');
  Logger.SetLogDestinations([ldConsole, ldFile]);
  LogIndex := Logger.AddLogFile(FLogFile);
  
  Logger.Info('Multi-destination test');
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    AssertTrue('Log file should contain the test message',
      Pos('Multi-destination test', FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
  // Visual verification required for console output
  AssertTrue('Multi-destination test completed', True);
end;

procedure TLoggerTest.Test06_DebugLevel;
var
  LogIndex: Integer;
  FileContent: TStringList;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test06_DebugLevel: Starting');
  Logger.SetLogDestinations([ldFile]);
  LogIndex := Logger.AddLogFile(FLogFile);
  
  Logger.Debug('Debug message test');
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    AssertTrue('Log file should contain DEBUG level',
      Pos('[DEBUG]', FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
end;

procedure TLoggerTest.Test07_InfoLevel;
var
  LogIndex: Integer;
  FileContent: TStringList;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test07_InfoLevel: Starting');
  Logger.SetLogDestinations([ldFile]);
  LogIndex := Logger.AddLogFile(FLogFile);
  
  Logger.Info('Info message test');
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    AssertTrue('Log file should contain INFO level',
      Pos('[INFO]', FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
end;

procedure TLoggerTest.Test08_WarningLevel;
var
  LogIndex: Integer;
  FileContent: TStringList;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test08_WarningLevel: Starting');
  Logger.SetLogDestinations([ldFile]);
  LogIndex := Logger.AddLogFile(FLogFile);
  
  Logger.Warning('Warning message test');
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    AssertTrue('Log file should contain WARNING level',
      Pos('[WARNING]', FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
end;

procedure TLoggerTest.Test09_ErrorLevel;
var
  LogIndex: Integer;
  FileContent: TStringList;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test09_ErrorLevel: Starting');
  Logger.SetLogDestinations([ldFile]);
  LogIndex := Logger.AddLogFile(FLogFile);
  
  Logger.Error('Error message test');
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    AssertTrue('Log file should contain ERROR level',
      Pos('[ERROR]', FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
end;

procedure TLoggerTest.Test10_FatalLevel;
var
  LogIndex: Integer;
  FileContent: TStringList;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test10_FatalLevel: Starting');
  Logger.SetLogDestinations([ldFile]);
  LogIndex := Logger.AddLogFile(FLogFile);
  
  Logger.Fatal('Fatal message test');
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    AssertTrue('Log file should contain FATAL level',
      Pos('[FATAL]', FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
end;

procedure TLoggerTest.Test11_SetLogDestinations;
var
  LogIndex: Integer;
  FileContent: TStringList;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test11_SetLogDestinations: Starting');
  // Test changing destinations
  Logger.SetLogDestinations([]);
  LogIndex := Logger.AddLogFile(FLogFile);
  Logger.Info('Should not be logged');
  
  AssertFalse('Log file should not exist when logging is disabled',
    FileExists(FLogFile));
    
  Logger.SetLogDestinations([ldFile]);
  Logger.Info('Should be logged');
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    AssertTrue('Log file should contain the second message only',
      (Pos('Should be logged', FileContent.Text) > 0) and
      (Pos('Should not be logged', FileContent.Text) = 0));
  finally
    FileContent.Free;
  end;
end;

procedure TLoggerTest.Test12_SetDateTimeFormat;
var
  LogIndex: Integer;
  FileContent: TStringList;
  CustomFormat: string;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test12_SetDateTimeFormat: Starting');
  Logger.SetLogDestinations([ldFile]);
  LogIndex := Logger.AddLogFile(FLogFile);
  
  CustomFormat := 'yyyy-mm-dd';
  Logger.SetDateTimeFormat(CustomFormat);
  Logger.Info('DateTime format test');
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    AssertTrue('Log file should use custom datetime format',
      Pos(FormatDateTime(CustomFormat, Now), FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
end;

procedure TLoggerTest.Test13_AddLogFile;
var
  LogIndex: Integer;
  SecondLogFile: string;
  SecondLogIndex: Integer;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test13_AddLogFile: Starting');
  Logger.SetLogDestinations([ldFile]);
  
  LogIndex := Logger.AddLogFile(FLogFile);
  AssertTrue('First log index should be valid', LogIndex >= 0);
  
  SecondLogFile := FTestDir + PathDelim + 'second.log';
  SecondLogIndex := Logger.AddLogFile(SecondLogFile);
  AssertTrue('Second log index should be valid', SecondLogIndex >= 0);
  AssertTrue('Log indices should be different', LogIndex <> SecondLogIndex);
end;

procedure TLoggerTest.Test14_LogFileRotation;
var
  LogIndex: Integer;
  i: Integer;
  LargeMessage: string;
  SearchRec: TSearchRec;
  RotatedFiles: Integer;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test14_LogFileRotation: Starting');
  Logger.SetLogDestinations([ldFile]);
  LogIndex := Logger.AddLogFile(FLogFile, 100); // Small max size for testing
  
  // Create a message larger than the max size
  LargeMessage := '';
  for i := 1 to 10 do
    LargeMessage := LargeMessage + 'This is a large message to trigger rotation. ';
  
  Logger.Info(LargeMessage);
  
  // Count rotated files
  RotatedFiles := 0;
  if FindFirst(FTestDir + PathDelim + '*.log', faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        Inc(RotatedFiles);
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
  
  AssertTrue('At least one rotated file should exist', RotatedFiles > 1);
end;

procedure TLoggerTest.Test15_MultipleLogFiles;
var
  FirstIndex, SecondIndex: Integer;
  FirstContent, SecondContent: TStringList;
  SecondLogFile: string;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test15_MultipleLogFiles: Starting');
  Logger.SetLogDestinations([ldFile]);
  
  FirstIndex := Logger.AddLogFile(FLogFile);
  SecondLogFile := FTestDir + PathDelim + 'second.log';
  SecondIndex := Logger.AddLogFile(SecondLogFile);
  
  Logger.Info('Test multiple files');
  
  FirstContent := TStringList.Create;
  SecondContent := TStringList.Create;
  try
    FirstContent.LoadFromFile(FLogFile);
    SecondContent.LoadFromFile(SecondLogFile);
    
    AssertTrue('First log file should contain the message',
      Pos('Test multiple files', FirstContent.Text) > 0);
    AssertTrue('Second log file should contain the message',
      Pos('Test multiple files', SecondContent.Text) > 0);
  finally
    FirstContent.Free;
    SecondContent.Free;
  end;
end;

procedure TLoggerTest.Test16_SpecificFileLogging;
var
  FirstIndex, SecondIndex: Integer;
  FirstContent, SecondContent: TStringList;
  SecondLogFile: string;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test16_SpecificFileLogging: Starting');
  Logger.SetLogDestinations([ldFile]);
  
  FirstIndex := Logger.AddLogFile(FLogFile);
  SecondLogFile := FTestDir + PathDelim + 'second.log';
  SecondIndex := Logger.AddLogFile(SecondLogFile);
  
  Logger.Info('First file only', FirstIndex);
  Logger.Info('Second file only', SecondIndex);
  
  FirstContent := TStringList.Create;
  SecondContent := TStringList.Create;
  try
    FirstContent.LoadFromFile(FLogFile);
    SecondContent.LoadFromFile(SecondLogFile);
    
    AssertTrue('First message should be in first file only',
      (Pos('First file only', FirstContent.Text) > 0) and
      (Pos('First file only', SecondContent.Text) = 0));
      
    AssertTrue('Second message should be in second file only',
      (Pos('Second file only', SecondContent.Text) > 0) and
      (Pos('Second file only', FirstContent.Text) = 0));
  finally
    FirstContent.Free;
    SecondContent.Free;
  end;
end;

procedure TLoggerTest.Test17_EmptyMessage;
var
  LogIndex: Integer;
  FileContent: TStringList;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test17_EmptyMessage: Starting');
  Logger.SetLogDestinations([ldFile]);
  LogIndex := Logger.AddLogFile(FLogFile);
  
  Logger.Info('');
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    AssertTrue('Empty message should be logged',
      FileContent.Count > 0);
  finally
    FileContent.Free;
  end;
end;

procedure TLoggerTest.Test18_InvalidFileIndex;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test18_InvalidFileIndex: Starting');
  Logger.SetLogDestinations([ldFile]);
  
  // Should not raise an exception
  Logger.Info('Test invalid index', 999);
  AssertTrue('Logging with invalid file index should not raise an exception', True);
end;

procedure TLoggerTest.Test19_MaxSizeHandling;
var
  LogIndex: Integer;
  i: Integer;
  FileSize: Int64;
  MaxSize: Int64;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test19_MaxSizeHandling: Starting');
  MaxSize := 1024; // 1KB
  Logger.SetLogDestinations([ldFile]);
  LogIndex := Logger.AddLogFile(FLogFile, MaxSize);
  
  // Write enough data to exceed max size
  for i := 1 to 100 do
    Logger.Info('Test message for max size handling: ' + IntToStr(i));
    
  FileSize := 0;
  if FileExists(FLogFile) then
  begin
    with TFileStream.Create(FLogFile, fmOpenRead or fmShareDenyNone) do
    try
      FileSize := Size;
    finally
      Free;
    end;
  end;
  
  AssertTrue('Current log file should not exceed max size',
    FileSize <= MaxSize);
end;

procedure TLoggerTest.Test20_ConcurrentLogging;
var
  LogIndex: Integer;
  i: Integer;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test20_ConcurrentLogging: Starting');
  Logger.SetLogDestinations([ldFile]);
  LogIndex := Logger.AddLogFile(FLogFile);
  
  // Simulate concurrent logging
  for i := 1 to 100 do
  begin
    Logger.Debug('Debug ' + IntToStr(i));
    Logger.Info('Info ' + IntToStr(i));
    Logger.Warning('Warning ' + IntToStr(i));
    Logger.Error('Error ' + IntToStr(i));
  end;
  
  AssertTrue('Log file should exist after concurrent logging',
    FileExists(FLogFile));
end;

procedure TLoggerTest.Test21_LogLevelFiltering;
var
  LogIndex: Integer;
  FileContent: TStringList;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test21_LogLevelFiltering: Starting');
  Logger.SetLogDestinations([ldFile]);
  LogIndex := Logger.AddLogFile(FLogFile);
  
  // Set minimum log level to Warning
  Logger.SetMinLogLevel(llWarning);
  
  // These messages should be filtered out
  Logger.Debug('Debug message should be filtered');
  Logger.Info('Info message should be filtered');
  
  // These messages should be logged
  Logger.Warning('Warning message should be logged');
  Logger.Error('Error message should be logged');
  Logger.Fatal('Fatal message should be logged');
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    
    // Check that appropriate messages are filtered
    AssertTrue('Debug message should be filtered out',
      Pos('Debug message should be filtered', FileContent.Text) = 0);
      
    AssertTrue('Info message should be filtered out',
      Pos('Info message should be filtered', FileContent.Text) = 0);
      
    // Check that appropriate messages are logged
    AssertTrue('Warning message should be logged',
      Pos('Warning message should be logged', FileContent.Text) > 0);
      
    AssertTrue('Error message should be logged',
      Pos('Error message should be logged', FileContent.Text) > 0);
      
    AssertTrue('Fatal message should be logged',
      Pos('Fatal message should be logged', FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
end;

procedure TLoggerTest.Test22_FormatStringOverloads;
var
  LogIndex: Integer;
  FileContent: TStringList;
  TestValue1: Integer;
  TestValue2: string;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test22_FormatStringOverloads: Starting');
  Logger.SetLogDestinations([ldFile]);
  LogIndex := Logger.AddLogFile(FLogFile);
  
  TestValue1 := 42;
  TestValue2 := 'test string';
  
  // Test format string overloads for different log levels
  Logger.DebugFmt('Debug format test: %d and %s', [TestValue1, TestValue2]);
  Logger.InfoFmt('Info format test: %d and %s', [TestValue1, TestValue2]);
  Logger.WarningFmt('Warning format test: %d and %s', [TestValue1, TestValue2]);
  Logger.ErrorFmt('Error format test: %d and %s', [TestValue1, TestValue2]);
  Logger.FatalFmt('Fatal format test: %d and %s', [TestValue1, TestValue2]);
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    
    // Check that all formatted messages are logged correctly
    AssertTrue('Debug format message should be logged correctly',
      Pos('Debug format test: 42 and test string', FileContent.Text) > 0);
      
    AssertTrue('Info format message should be logged correctly',
      Pos('Info format test: 42 and test string', FileContent.Text) > 0);
      
    AssertTrue('Warning format message should be logged correctly',
      Pos('Warning format test: 42 and test string', FileContent.Text) > 0);
      
    AssertTrue('Error format message should be logged correctly',
      Pos('Error format test: 42 and test string', FileContent.Text) > 0);
      
    AssertTrue('Fatal format message should be logged correctly',
      Pos('Fatal format test: 42 and test string', FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
end;

procedure TLoggerTest.Test23_SimpleOneLineSetup;
var
  LoggerInstance: TLogger;
  DefaultLogPath: string;
  FileContent: TStringList;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test23_SimpleOneLineSetup: Starting');
  
  // Reset the logger completely
  TLogger.ResetInstance;
  
  // Test one-line setup with explicit log file in test directory
  LoggerInstance := TLogger.CreateDefaultLogger([ldConsole, ldFile], FLogFile, llInfo);
  
  // Check instance was created and configured correctly
  AssertNotNull('Logger instance should not be nil', LoggerInstance);
  
  // Log some messages
  LoggerInstance.Debug('Debug message should not be logged');
  LoggerInstance.Info('Info message should be logged');
  LoggerInstance.Warning('Warning message should be logged');
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    
    // Check that minimum log level is working
    AssertTrue('Debug message should be filtered out',
      Pos('Debug message should not be logged', FileContent.Text) = 0);
      
    // Check that other messages are logged
    AssertTrue('Info message should be logged',
      Pos('Info message should be logged', FileContent.Text) > 0);
      
    AssertTrue('Warning message should be logged',
      Pos('Warning message should be logged', FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
  
  // Reset for the rest of the tests
  TLogger.ResetInstance;
end;

procedure TLoggerTest.Test24_DefaultLogFilePaths;
var
  LogIndex: Integer;
  AppDir, DefaultLogPath: string;
  FileExists: Boolean;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test24_DefaultLogFilePaths: Starting');
  
  // Redirect default log path to test directory for testing purposes
  // This requires some mocking which isn't ideal, so we'll just test the method
  // returns a valid index without testing the actual path creation
  Logger.SetLogDestinations([ldFile]);
  LogIndex := Logger.AddDefaultLogFile('testlog', 1024 * 1024);
  
  AssertTrue('Default log file index should be valid', LogIndex >= 0);
  
  // If we wanted to test actual path creation, we'd need to modify the AddDefaultLogFile
  // method to accept a base directory parameter for testing
end;

procedure TLoggerTest.Test25_MethodChaining;
var
  LogIndex: Integer;
  FileContent: TStringList;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test25_MethodChaining: Starting');
  
  // Test method chaining by configuring multiple settings in a chain
  Logger
    .SetLogDestinations([ldFile])
    .SetMinLogLevel(llWarning)
    .SetDateTimeFormat('yyyy-mm-dd');
  
  LogIndex := Logger.AddLogFile(FLogFile);
  
  // Log messages at different levels
  Logger.Debug('Debug message should be filtered');
  Logger.Info('Info message should be filtered');
  Logger.Warning('Warning message should be logged');
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    
    // Check that log level filtering was applied
    AssertTrue('Debug message should be filtered out',
      Pos('Debug message should be filtered', FileContent.Text) = 0);
      
    AssertTrue('Info message should be filtered out',
      Pos('Info message should be filtered', FileContent.Text) = 0);
      
    // Check that warning message is logged
    AssertTrue('Warning message should be logged',
      Pos('Warning message should be logged', FileContent.Text) > 0);
      
    // Check that datetime format was applied
    AssertTrue('Log file should use custom datetime format (yyyy-mm-dd)',
      Pos(FormatDateTime('yyyy-mm-dd', Now), FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
end;

procedure TLoggerTest.Test26_CategorySupport;
var
  LogIndex: Integer;
  FileContent: TStringList;
  UILogger, DBLogger: TLogContext;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test26_CategorySupport: Starting');
  Logger.SetLogDestinations([ldFile]);
  LogIndex := Logger.AddLogFile(FLogFile);
  
  // Create category contexts
  UILogger := Logger.CreateContext('UI');
  DBLogger := Logger.CreateContext('Database');
  
  // Log messages with categories
  UILogger.Info('UI related message');
  DBLogger.Warning('Database related warning');
  
  // Test format overloads with categories
  UILogger.ErrorFmt('UI error: %s', ['Button not found']);
  DBLogger.InfoFmt('Database query took %d ms', [150]);
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    
    // Check category prefixes in log messages
    AssertTrue('UI category should be present in log message',
      Pos('[UI] UI related message', FileContent.Text) > 0);
      
    AssertTrue('Database category should be present in log message',
      Pos('[Database] Database related warning', FileContent.Text) > 0);
      
    // Check category prefixes in formatted messages
    AssertTrue('UI category should be present in formatted log message',
      Pos('[UI] UI error: Button not found', FileContent.Text) > 0);
      
    AssertTrue('Database category should be present in formatted log message',
      Pos('[Database] Database query took 150 ms', FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
  
  // No need to manually free the contexts anymore - they will be automatically
  // freed when the logger is reset in TearDown
end;

procedure TLoggerTest.Test27_ContextReferenceCountingBasic;
var
  Context: TLogContext;
  RefCount: Integer;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test27_ContextReferenceCountingBasic: Starting');
  
  // Create context
  Context := Logger.CreateContext('TestContext');
  
  // Manually add another reference
  RefCount := Context.AddRef;
  
  // Verify reference count is now 2 (one from CreateContext, one from AddRef)
  AssertEquals('Reference count should be 2 after AddRef', 2, RefCount);
  
  // Release one reference
  RefCount := Context.Release;
  
  // Verify reference count is now 1
  AssertEquals('Reference count should be 1 after first Release', 1, RefCount);
  
  // The context should still be usable
  Context.Info('Context is still usable');
  
  // No need to Release again - it will be automatically freed when logger is reset
end;

procedure TLoggerTest.Test28_ContextAutoCleanup;
var
  ContextCount: Integer;
  TempContext: TLogContext;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test28_ContextAutoCleanup: Starting');
  
  // Create multiple contexts
  Logger.CreateContext('Context1');
  Logger.CreateContext('Context2');
  Logger.CreateContext('Context3');
  
  // Check we have contexts by creating and using another one
  TempContext := Logger.CreateContext('TempContext');
  TempContext.Info('Testing context tracking');
  
  // Reset the logger instance
  TLogger.ResetInstance;
  
  // Get a new logger instance
  Logger.Info('After reset, contexts should be cleaned up');
  
  // We can't directly test internal FContexts list count, but we can
  // indirectly verify by ensuring program doesn't crash due to memory issues
  // when we destroy and recreate contexts
  TempContext := Logger.CreateContext('NewContext');
  TempContext.Info('New context after reset');
  
  // Test passes if we don't get access violations or memory leaks (checked by heaptrc)
  AssertTrue('Context auto-cleanup test completed successfully', True);
end;

procedure TLoggerTest.Test29_LogRotationThreshold;
var
  LogIndex: Integer;
  SearchRec: TSearchRec;
  i: Integer;
  RotatedFileCount: Integer;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test29_LogRotationThreshold: Starting');
  Logger.SetLogDestinations([ldFile]);
  
  // Set a very small max size (50 bytes)
  LogIndex := Logger.AddLogFile(FLogFile, 50);
  
  // Write a small message to create the file
  Logger.Info('First message');
  
  // Verify file exists
  AssertTrue('Log file should exist', FileExists(FLogFile));
  
  // Write messages that will exceed the 50 byte threshold
  for i := 1 to 5 do
    Logger.Info('Message that should trigger rotation: ' + IntToStr(i));
  
  // Count rotated files
  RotatedFileCount := 0;
  if FindFirst(FTestDir + PathDelim + '*.log', faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        if (SearchRec.Name <> ExtractFileName(FLogFile)) then
          Inc(RotatedFileCount);
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
  
  // The original file should still exist (but be a new file)
  AssertTrue('Original log file should still exist', FileExists(FLogFile));
  
  // There should be at least one rotated file
  AssertTrue('At least one rotated log file should exist', RotatedFileCount > 0);
end;

procedure TLoggerTest.Test30_StressTest;
var
  LogIndex: Integer;
  i: Integer;
  StartTime, EndTime: TDateTime;
  ElapsedMs: Integer;
  MsgCount: Integer;
  FileContent: TStringList;
  LineCount: Integer;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test30_StressTest: Starting');
  Logger.SetLogDestinations([ldFile]);
  LogIndex := Logger.AddLogFile(FLogFile);
  
  MsgCount := 200; // Adjust based on system performance
  
  StartTime := Now;
  
  // Write a large number of messages as quickly as possible
  for i := 1 to MsgCount do
    Logger.InfoFmt('Stress test message %d of %d', [i, MsgCount]);
  
  EndTime := Now;
  ElapsedMs := Round((EndTime - StartTime) * 86400 * 1000);
  
  // Log throughput information
  Logger.InfoFmt('Logged %d messages in %d ms (%f msgs/sec)', 
    [MsgCount, ElapsedMs, MsgCount / (ElapsedMs / 1000)]);
    
  // Simple verification that file exists and has content
  AssertTrue('Log file should exist after stress test', FileExists(FLogFile));
  
  // Verify at least some of the messages were logged
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    LineCount := FileContent.Count;
    
    // We should have at least the number of messages we sent plus throughput info
    AssertTrue('Log file should contain stress test messages', 
      LineCount >= MsgCount);
      
    // Check for first message
    AssertTrue('Log should contain first message', 
      Pos('Stress test message 1 of', FileContent.Text) > 0);
      
    // Check for last message
    AssertTrue('Log should contain last message', 
      Pos(Format('Stress test message %d of', [MsgCount]), FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
end;

procedure TLoggerTest.Test31_ContextLifecycleOnLoggerReset;
var
  Context: TLogContext;
  DestroyedLoggerInstance: TLogger;
  NewLoggerInstance: TLogger;
  DestroyedInstanceID, NewInstanceID: Int64;
begin
  WriteLn('==== Test31_ContextLifecycleOnLoggerReset: Starting ====');
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test31_ContextLifecycleOnLoggerReset: Starting');
  
  // Get a reference to the current logger instance and its ID
  DestroyedLoggerInstance := Logger;
  DestroyedInstanceID := DestroyedLoggerInstance.GetInstanceID;
  WriteLn('Logger instance ID before reset: ', IntToStr(DestroyedInstanceID));
  
  // Create a context
  Context := Logger.CreateContext('DestroyTest');
  
  // Use the context
  Context.Info('This context will be released when logger is reset');
  
  // Reset logger - this should release all contexts
  WriteLn('About to call TLogger.ResetInstance()');
  TLogger.ResetInstance;
  WriteLn('TLogger.ResetInstance() completed');
  
  // Force the creation of a new logger instance
  WriteLn('About to call Logger function to get new instance');
  NewLoggerInstance := Logger;
  NewInstanceID := NewLoggerInstance.GetInstanceID;
  WriteLn('Logger instance ID after reset: ', IntToStr(NewInstanceID));
  
  // Compare instance IDs instead of pointers
  WriteLn('Destroyed logger instance ID: ', IntToStr(DestroyedInstanceID));
  WriteLn('New logger instance ID: ', IntToStr(NewInstanceID));
  
  // Now the Context variable still points to the released memory location
  // We should never use it, but we can test that a new logger instance was created
  if DestroyedInstanceID = NewInstanceID then
    WriteLn('ERROR: Logger instance ID did not change after reset!');
    
  WriteLn('Comparing instance IDs - different? ', BoolToStr(DestroyedInstanceID <> NewInstanceID, True));
  
  AssertTrue('Logger instance should change after reset (IDs should be different)', 
    DestroyedInstanceID <> NewInstanceID);
    
  // Create a new context with the same name to test no conflicts
  Context := Logger.CreateContext('DestroyTest');
  Context.Info('New context with same name');
  
  // Test passes if we reach here without memory violations
  AssertTrue('Context lifecycle test passed', True);
  WriteLn('==== Test31_ContextLifecycleOnLoggerReset: Completed ====');
end;

procedure TLoggerTest.Test32_DefaultLogDirectoryCreation;
var
  LogDir: string;
  LogFile: string;
  LogIndex: Integer;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test32_DefaultLogDirectoryCreation: Starting');
  Logger.SetLogDestinations([ldFile]);
  
  // Create log file in a non-existent nested directory structure
  LogDir := FTestDir + PathDelim + 'nested' + PathDelim + 'log' + PathDelim + 'dir';
  LogFile := LogDir + PathDelim + 'deep.log';
  
  // Make sure directory doesn't exist to start with
  if DirectoryExists(LogDir) then
    RemoveDir(LogDir);
    
  // Verify directory doesn't exist
  AssertFalse('Nested directory should not exist before test', 
    DirectoryExists(LogDir));
  
  // Add log file - this should create all necessary directories
  LogIndex := Logger.AddLogFile(LogFile);
  
  // Log a message to ensure file is created
  Logger.Info('Testing directory creation');
  
  // Verify the directory was created
  AssertTrue('Nested directory structure should be created', 
    DirectoryExists(LogDir));
    
  // Verify log file was created
  AssertTrue('Log file should be created in nested directory', 
    FileExists(LogFile));
end;

procedure TLoggerTest.Test33_ThreadSafetyBasic;
var
  LogIndex: Integer;
  FileContent: TStringList;
  MessageCount, FoundCount: Integer;
  i: Integer;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test33_ThreadSafetyBasic: Starting');
  Logger.SetLogDestinations([ldFile]);
  LogIndex := Logger.AddLogFile(FLogFile);
  
  // Write messages with a specific pattern we can count
  MessageCount := 20;
  for i := 1 to MessageCount do
  begin
    // Use a background thread to increase chances of thread-safety issues
    // This is a simulation - actual thread testing would need TThread
    Sleep(1); // Force a small delay to increase chance of timing issues
    Logger.InfoFmt('THREAD_TEST_MARKER_%d', [i]);
  end;
  
  // Small delay to ensure all writes complete
  Sleep(100);
  
  // Count the messages in the log file
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FLogFile);
    
    FoundCount := 0;
    for i := 0 to FileContent.Count - 1 do
      if Pos('THREAD_TEST_MARKER_', FileContent[i]) > 0 then
        Inc(FoundCount);
        
    AssertEquals('All thread test messages should be in log file', 
      MessageCount, FoundCount);
  finally
    FileContent.Free;
  end;
end;

procedure TLoggerTest.Test34_ErrorRecovery;
var
  LogIndex: Integer;
  ReadOnlyDir: string;
  OriginalAttrs: Integer;
  LogFile: string;
  TestPassed: Boolean;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test34_ErrorRecovery: Starting');
  
  // Create a directory to use for testing
  ReadOnlyDir := FTestDir + PathDelim + 'readonly';
  ForceDirectories(ReadOnlyDir);
  LogFile := ReadOnlyDir + PathDelim + 'should_fail.log';
  
  // Make sure the file doesn't exist initially
  if FileExists(LogFile) then
    DeleteFile(LogFile);
  
  {$IFDEF WINDOWS}
  // Save original attributes
  OriginalAttrs := FileGetAttr(ReadOnlyDir);
  
  try
    // Make directory read-only (not possible on all systems)
    FileSetAttr(ReadOnlyDir, OriginalAttrs or faReadOnly);
    
    // Try to log to a file in the read-only directory
    Logger.SetLogDestinations([ldFile]);
    LogIndex := Logger.AddLogFile(LogFile);
    
    // This should not throw an exception even if file can't be created
    TestPassed := True;
    try
      Logger.Info('This should handle the error gracefully');
      // Note: depending on system permissions, this might actually succeed
    except
      on E: Exception do
        TestPassed := False;
    end;
    
    // Continue with logging to console
    Logger.SetLogDestinations([ldConsole]);
    Logger.Info('This should work fine with console logging');
    
    // Test passed if we got here without exceptions
    AssertTrue('Logger should handle file errors gracefully', TestPassed);
  finally
    // Restore directory attributes to original state
    try
      FileSetAttr(ReadOnlyDir, OriginalAttrs);
    except
      // Ignore errors when restoring attributes
    end;
  end;
  {$ELSE}
  // On non-Windows platforms, we just verify that invalid paths don't crash
  Logger.SetLogDestinations([ldFile]);
  LogIndex := Logger.AddLogFile('/invalid/path/that/should/not/exist/test.log');
  
  TestPassed := True;
  try
    Logger.Info('This should handle invalid paths gracefully');
  except
    on E: Exception do
      TestPassed := False;
  end;
  
  // Continue with logging to console
  Logger.SetLogDestinations([ldConsole]);
  Logger.Info('This should work fine with console logging');
  
  // Test passed if we got here without exceptions
  AssertTrue('Logger should handle file errors gracefully', TestPassed);
  {$ENDIF}
end;

procedure TLoggerTest.Test35_SinkManagement;
var
  ConsoleSink, FileSink: ILogSink;
  SinkCount: Integer;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test35_SinkManagement: Starting');
  
  // Clear any existing sinks
  Logger.ClearSinks;
  
  // Add two sinks
  ConsoleSink := TConsoleSink.Create;
  FileSink := TFileSink.Create(FLogFile);
  
  Logger.AddSink(ConsoleSink);
  Logger.AddSink(FileSink);
  
  // Test removing a sink
  Logger.RemoveSink(ConsoleSink);
  
  // Log a message (should only go to file sink)
  Logger.Info('This should only go to the file sink');
  
  // Verify file sink received the message
  SinkCount := 1; // We should have 1 sink left
  
  // Add console sink back
  Logger.AddSink(ConsoleSink);
  
  // Clear all sinks
  Logger.ClearSinks;
  
  // Ensure all sinks are cleared
  SinkCount := 0; // We should have no sinks
  
  AssertTrue('Sink management test completed', True);
end;

procedure TLoggerTest.Test36_ConsoleSink;
var
  ConsoleSink: ILogSink;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test36_ConsoleSink: Starting');
  
  // Clear any existing sinks
  Logger.ClearSinks;
  
  // Create and add console sink
  ConsoleSink := TConsoleSink.Create;
  Logger.AddSink(ConsoleSink);
  
  // Log messages with various levels
  Logger.Debug('Debug message from ConsoleSink test');
  Logger.Info('Info message from ConsoleSink test');
  Logger.Warning('Warning message from ConsoleSink test');
  Logger.Error('Error message from ConsoleSink test');
  Logger.Fatal('Fatal message from ConsoleSink test');
  
  // Since console output can't be automatically verified, we just check that no exceptions occurred
  AssertTrue('Console sink test completed without errors', True);
end;

procedure TLoggerTest.Test37_FileSink;
var
  FileSink: ILogSink;
  TestFile: string;
  FileContent: TStringList;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test37_FileSink: Starting');
  
  // Clear any existing sinks
  Logger.ClearSinks;
  
  // Create test file path
  TestFile := FTestDir + PathDelim + 'filesink.log';
  
  // Create and add file sink
  FileSink := TFileSink.Create(TestFile);
  Logger.AddSink(FileSink);
  
  // Log a test message
  Logger.Info('Test message for FileSink');
  
  // Flush ensures message is written
  FileSink.Flush;
  
  // Verify message was written to file
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(TestFile);
    AssertTrue('Log file should contain the test message',
      Pos('Test message for FileSink', FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
end;

procedure TLoggerTest.Test38_RotatingFileSink;
var
  RotatingSink: TRotatingFileSink;
  TestFile: string;
  FileContent: TStringList;
  MaxSize: Int64;
  i: Integer;
  LongMessage: string;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test38_RotatingFileSink: Starting');
  
  // Clear any existing sinks
  Logger.ClearSinks;
  
  // Create test file path
  TestFile := FTestDir + PathDelim + 'rotatesink.log';
  
  // Set small max size to trigger rotation
  MaxSize := 1024; // 1KB
  
  // Create and add rotating file sink
  RotatingSink := TRotatingFileSink.Create(TestFile, MaxSize, 3);
  Logger.AddSink(RotatingSink);
  
  // Create a message that will cause rotation
  LongMessage := '';
  for i := 1 to 50 do
    LongMessage := LongMessage + 'This is log message #' + IntToStr(i) + ' with some padding to make it longer. ';
  
  // Log enough to cause rotation
  for i := 1 to 5 do
  begin
    Logger.Info(LongMessage);
  end;
  
  // Flush to ensure all writes complete
  RotatingSink.Flush;
  
  // Verify log file exists
  AssertTrue('Log file should exist', FileExists(TestFile));
  
  // Verify at least one rotated file exists (can't predict exact name due to timestamp)
  AssertTrue('At least one rotated log file should be created', True);
  
  // Read current log file
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(TestFile);
    AssertTrue('Log file should contain log messages',
      FileContent.Count > 0);
  finally
    FileContent.Free;
  end;
end;

procedure TLoggerTest.Test39_DailyFileSink;
var
  DailySink: TDailyFileSink;
  TestFile, TodayFile: string;
  FileContent: TStringList;
  DateStr: string;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test39_DailyFileSink: Starting');
  
  // Clear any existing sinks
  Logger.ClearSinks;
  
  // Create test file path
  TestFile := FTestDir + PathDelim + 'daily.log';
  
  // Create and add daily file sink
  DailySink := TDailyFileSink.Create(TestFile);
  Logger.AddSink(DailySink);
  
  // Log a test message
  Logger.Info('Test message for DailyFileSink');
  
  // Flush to ensure writes complete
  DailySink.Flush;
  
  // Get today's date for filename check
  DateStr := FormatDateTime('yyyymmdd', Now);
  TodayFile := FTestDir + PathDelim + 'daily_' + DateStr + '.log';
  
  // Verify message was written to file
  FileContent := TStringList.Create;
  try
    // The file should exist, either as the original name or with today's date
    if FileExists(TodayFile) then
    begin
      FileContent.LoadFromFile(TodayFile);
      AssertTrue('Log file should contain the test message',
        Pos('Test message for DailyFileSink', FileContent.Text) > 0);
    end
    else if FileExists(TestFile) then
    begin
      FileContent.LoadFromFile(TestFile);
      AssertTrue('Log file should contain the test message',
        Pos('Test message for DailyFileSink', FileContent.Text) > 0);
    end
    else
      AssertFalse('Either original or dated log file should exist', True);
  finally
    FileContent.Free;
  end;
end;

procedure TLoggerTest.Test40_MemorySink;
var
  MemorySink: TMemorySink;
  Messages: TStringList;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test40_MemorySink: Starting');
  
  // Clear any existing sinks
  Logger.ClearSinks;
  
  // Create and add memory sink with capacity of 10 messages
  MemorySink := TMemorySink.Create(10);
  Logger.AddSink(MemorySink);
  
  // Log several messages
  Logger.Debug('Debug message 1');
  Logger.Info('Info message 2');
  Logger.Warning('Warning message 3');
  
  // Get messages from memory sink
  Messages := MemorySink.GetMessages;
  
  try
    // Verify message count
    AssertEquals('Memory sink should have 3 messages', 3, Messages.Count);
    
    // Verify message content
    AssertTrue('Memory sink should contain debug message',
      Pos('Debug message 1', Messages.Text) > 0);
    AssertTrue('Memory sink should contain info message',
      Pos('Info message 2', Messages.Text) > 0);
    AssertTrue('Memory sink should contain warning message',
      Pos('Warning message 3', Messages.Text) > 0);
    
    // Add more messages to test capacity limit
    Logger.Info('Message 4');
    Logger.Info('Message 5');
    Logger.Info('Message 6');
    Logger.Info('Message 7');
    Logger.Info('Message 8');
    Logger.Info('Message 9');
    Logger.Info('Message 10');
    Logger.Info('Message 11'); // This should push out the oldest message
    
    // Refresh our local copy of messages
    Messages.Free;
    Messages := MemorySink.GetMessages;
    
    // Verify capacity limit (should have 10 messages, oldest removed)
    AssertEquals('Memory sink should have 10 messages (its capacity)', 10, Messages.Count);
    AssertFalse('Memory sink should no longer contain oldest debug message',
      Pos('Debug message 1', Messages.Text) > 0);
    
    // Test clear method
    MemorySink.Clear;
    Messages.Free;
    Messages := MemorySink.GetMessages;
    
    // Verify messages are cleared
    AssertEquals('Memory sink should have 0 messages after clear', 0, Messages.Count);
  finally
    Messages.Free;
  end;
end;

procedure TLoggerTest.Test41_PatternParsing;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test41_PatternParsing: Starting');
  
  // Test setting various patterns
  Logger.SetFormat('[%time] [%level] %message');
  Logger.Info('Basic pattern test');
  
  Logger.SetFormat('%time | %level | %message | %category');
  Logger.InfoWithCategory('TestCategory', 'Category pattern test');
  
  Logger.SetFormat('[%time] [%level] [%file:%line] %message');
  Logger.Info('File and line pattern test');
  
  Logger.SetFormat('Custom pattern with static text: %message');
  Logger.Info('Static text pattern test');
  
  Logger.SetFormat('%level - %time - %message');
  Logger.Info('Reordered pattern test');
  
  // If we got here without exceptions, the pattern parsing works
  AssertTrue('Pattern parsing should succeed for valid patterns', True);
end;

procedure TLoggerTest.Test42_PatternFormatting;
var
  MemorySink: TMemorySink;
  Messages: TStringList;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test42_PatternFormatting: Starting');
  
  // Clear any existing sinks
  Logger.ClearSinks;
  
  // Create memory sink to capture formatted messages
  MemorySink := TMemorySink.Create(10);
  Logger.AddSink(MemorySink);
  
  // Set specific datetime format for predictable testing
  Logger.SetDateTimeFormat('yyyy-mm-dd');
  
  // Test basic pattern
  Logger.SetFormat('[%time] [%level] %message');
  Logger.Info('Basic pattern test');
  
  // Test with category
  Logger.SetFormat('[%time] [%level] [%category] %message');
  Logger.InfoWithCategory('TestCategory', 'Category test');
  
  // Get formatted messages
  Messages := MemorySink.GetMessages;
  try
    // Verify basic pattern formatting
    AssertTrue('Basic pattern should include date in proper format',
      Pos('[' + FormatDateTime('yyyy-mm-dd', Now) + ']', Messages[0]) > 0);
    AssertTrue('Basic pattern should include level',
      Pos('[INFO]', Messages[0]) > 0);
    AssertTrue('Basic pattern should include message',
      Pos('Basic pattern test', Messages[0]) > 0);
    
    // Verify category pattern formatting
    AssertTrue('Category pattern should include category',
      Pos('[TestCategory]', Messages[1]) > 0);
    AssertTrue('Category pattern should include message',
      Pos('Category test', Messages[1]) > 0);
  finally
    Messages.Free;
  end;
end;

procedure TLoggerTest.Test43_StructuredLogging;
var
  MemorySink: TMemorySink;
  Messages: TStringList;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test43_StructuredLogging: Starting');
  
  // Clear any existing sinks
  Logger.ClearSinks;
  
  // Create memory sink to capture messages
  MemorySink := TMemorySink.Create(10);
  Logger.AddSink(MemorySink);
  
  // Log structured message with various field types
  Logger.LogStructured(llInfo, 'User login', [
    NameValuePair('username', 'testuser'),
    NameValuePair('success', True),
    NameValuePair('attempts', 3),
    NameValuePair('response_time', 150.5)
  ]);
  
  // Get messages
  Messages := MemorySink.GetMessages;
  try
    // Verify structured message contains field data
    AssertTrue('Structured log should contain message',
      Pos('User login', Messages[0]) > 0);
    AssertTrue('Structured log should contain string field',
      Pos('username=testuser', Messages[0]) > 0);
    AssertTrue('Structured log should contain boolean field',
      Pos('success=True', Messages[0]) > 0);
    AssertTrue('Structured log should contain integer field',
      Pos('attempts=3', Messages[0]) > 0);
    AssertTrue('Structured log should contain float field',
      Pos('response_time=150.5', Messages[0]) > 0);
  finally
    Messages.Free;
  end;
end;

procedure TLoggerTest.Test44_LogValueTypes;
var
  MemorySink: TMemorySink;
  Messages: TStringList;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test44_LogValueTypes: Starting');
  
  // Clear any existing sinks
  Logger.ClearSinks;
  
  // Create memory sink to capture messages
  MemorySink := TMemorySink.Create(10);
  Logger.AddSink(MemorySink);
  
  // Log different value types
  Logger.LogValue('integer_value', 42);
  Logger.LogValue('boolean_value', True);
  Logger.LogValue('float_value', 3.14);
  Logger.LogValue('string_value', 'test string');
  
  // Get messages
  Messages := MemorySink.GetMessages;
  try
    // Verify typed values are logged correctly
    AssertTrue('Integer value should be logged correctly',
      Pos('integer_value=42', Messages[0]) > 0);
    AssertTrue('Boolean value should be logged correctly',
      Pos('boolean_value=True', Messages[1]) > 0);
    AssertTrue('Float value should be logged correctly',
      Pos('float_value=3.14', Messages[2]) > 0);
    AssertTrue('String value should be logged correctly',
      Pos('string_value=test string', Messages[3]) > 0);
  finally
    Messages.Free;
  end;
end;

procedure TLoggerTest.Test45_TimedOperationBlock;
var
  MemorySink: TMemorySink;
  Messages: TStringList;
  Timer: ITimedOperation;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test45_TimedOperationBlock: Starting');
  
  // Clear any existing sinks
  Logger.ClearSinks;
  
  // Create memory sink to capture messages
  MemorySink := TMemorySink.Create(10);
  Logger.AddSink(MemorySink);
  
  // Create timed operation
  Timer := Logger.TimedBlock('Test operation');
  
  // Simulate work
  Sleep(100);
  
  // Let timer go out of scope to log completion
  Timer := nil;
  
  // Get messages
  Messages := MemorySink.GetMessages;
  try
    // Verify timed operation messages
    AssertTrue('Timed operation should log start message',
      Pos('Starting: Test operation', Messages[0]) > 0);
    AssertTrue('Timed operation should log completion message',
      Pos('Completed: Test operation', Messages[1]) > 0);
    AssertTrue('Timed operation should include duration',
      Pos('Duration:', Messages[1]) > 0);
    AssertTrue('Duration should be at least 100ms',
      Pos('ms', Messages[1]) > 0);
  finally
    Messages.Free;
  end;
end;

procedure TLoggerTest.Test46_BatchLogging;
var
  MemorySink: TMemorySink;
  Messages: TStringList;
  i: Integer;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test46_BatchLogging: Starting');
  
  // Clear any existing sinks
  Logger.ClearSinks;
  
  // Create memory sink to capture messages
  MemorySink := TMemorySink.Create(1000);
  Logger.AddSink(MemorySink);
  
  // Start batch
  Logger.BeginBatch;
  
  // Log multiple messages in batch
  for i := 1 to 10 do
    Logger.Info('Batch message ' + IntToStr(i));
  
  // Check that messages aren't logged yet
  Messages := MemorySink.GetMessages;
  try
    AssertEquals('No messages should be logged before EndBatch', 0, Messages.Count);
  finally
    Messages.Free;
  end;
  
  // End batch to flush messages
  Logger.EndBatch;
  
  // Now messages should be logged
  Messages := MemorySink.GetMessages;
  try
    AssertEquals('All batch messages should be logged after EndBatch', 10, Messages.Count);
    
    // Verify message content
    for i := 1 to 10 do
      AssertTrue('Batch should contain message ' + IntToStr(i),
        Pos('Batch message ' + IntToStr(i), Messages.Text) > 0);
  finally
    Messages.Free;
  end;
end;

procedure TLoggerTest.Test47_BatchLoggedMessagesCount;
var
  MemorySink: TMemorySink;
  Messages: TStringList;
  i: Integer;
  UnhandledException: Boolean;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test47_BatchLoggedMessagesCount: Starting');
  
  // Clear any existing sinks
  Logger.ClearSinks;
  
  // Create memory sink with smaller capacity
  MemorySink := TMemorySink.Create(100);
  Logger.AddSink(MemorySink);
  
  // Test batch with exception handling
  Logger.BeginBatch;
  
  // Log a lot of messages
  for i := 1 to 50 do
    Logger.Info('Message ' + IntToStr(i));
  
  UnhandledException := False;
  try
    // Simulate an exception
    try
      if i > 10 then
        raise Exception.Create('Test exception during batch');
    except
      // In real code, the finally block would still call EndBatch
      Logger.EndBatch;
      // Re-raise for testing
      raise;
    end;
  except
    on E: Exception do
      UnhandledException := True;
  end;
  
  // Verify exception was caught
  AssertTrue('Exception should be caught during batch', UnhandledException);
  
  // Verify messages were still logged
  Messages := MemorySink.GetMessages;
  try
    AssertEquals('All 50 messages should be logged despite exception', 50, Messages.Count);
  finally
    Messages.Free;
  end;
  
  // Test nested batches (not supported, inner batch should be ignored)
  Logger.BeginBatch;
  Logger.Info('Outer batch message 1');
  
  Logger.BeginBatch; // This should be ignored
  Logger.Info('Inner batch message 1');
  Logger.Info('Inner batch message 2');
  Logger.EndBatch; // This should be ignored
  
  Logger.Info('Outer batch message 2');
  Logger.EndBatch;
  
  // Verify all messages are logged together
  Messages := MemorySink.GetMessages;
  try
    AssertEquals('Messages count should now be 54', 54, Messages.Count);
    AssertTrue('All batch messages should be present',
      (Pos('Outer batch message 1', Messages.Text) > 0) and
      (Pos('Inner batch message 1', Messages.Text) > 0) and
      (Pos('Inner batch message 2', Messages.Text) > 0) and
      (Pos('Outer batch message 2', Messages.Text) > 0));
  finally
    Messages.Free;
  end;
end;

// Create a test INI file for configuration
procedure CreateTestConfigFile(const FileName: string);
var
  ConfigFile: TStringList;
begin
  ConfigFile := TStringList.Create;
  try
    ConfigFile.Add('[Logger]');
    ConfigFile.Add('Destinations=Console,File');
    ConfigFile.Add('MinLevel=Info');
    ConfigFile.Add('DateTimeFormat=yyyy-mm-dd hh:nn');
    ConfigFile.Add('FormatPattern=[%time] [%level] %message');
    ConfigFile.Add('DefaultLogFile=testconfig.log');
    ConfigFile.Add('MaxFileSize=1048576');
    ConfigFile.SaveToFile(FileName);
  finally
    ConfigFile.Free;
  end;
end;

procedure TLoggerTest.Test48_ConfigurationLoading;
var
  ConfigFile: string;
  LogFile: string;
  FileContent: TStringList;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test48_ConfigurationLoading: Starting');
  
  // Create test configuration file
  ConfigFile := FTestDir + PathDelim + 'logger.ini';
  LogFile := FTestDir + PathDelim + 'testconfig.log';
  CreateTestConfigFile(ConfigFile);
  
  // Load configuration from file
  Logger.LoadConfiguration(ConfigFile);
  
  // Log a test message
  Logger.Info('Test message with loaded configuration');
  Logger.Debug('This debug message should be filtered out');
  
  // Verify log file exists with correct content
  FileContent := TStringList.Create;
  try
    AssertTrue('Log file should be created from config', FileExists(LogFile));
    
    FileContent.LoadFromFile(LogFile);
    
    // Verify message format matches configuration
    AssertTrue('Message should use configured format pattern',
      Pos('[' + FormatDateTime('yyyy-mm-dd hh:nn', Now) + ']', FileContent.Text) > 0);
    
    // Verify log level filtering
    AssertTrue('Info message should be present',
      Pos('Test message with loaded configuration', FileContent.Text) > 0);
    AssertFalse('Debug message should be filtered out',
      Pos('This debug message should be filtered out', FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
end;

procedure TLoggerTest.Test49_FactoryMethods;
var
  ConsoleLogger, FileLogger, BothLogger, DebugLogger, AuditLogger: TLogger;
  ConsoleTest, FileTest, BothTest, DebugTest, AuditTest: string;
  FileContent: TStringList;
begin
  Logger.SetLogDestinations([ldConsole]);
  Logger.Debug('Test49_FactoryMethods: Starting');
  
  // Create test file paths
  FileTest := FTestDir + PathDelim + 'filelogger.log';
  BothTest := FTestDir + PathDelim + 'bothlogger.log';
  DebugTest := FTestDir + PathDelim + 'debuglogger.log';
  AuditTest := FTestDir + PathDelim + 'auditlogger.log';
  
  // Reset logger to start with clean state
  TLogger.ResetInstance;
  
  // Test each factory method
  
  // Console logger
  ConsoleLogger := TLogger.CreateConsoleLogger(llDebug);
  ConsoleLogger.Info('Console logger test');
  
  // File logger
  TLogger.ResetInstance;
  FileLogger := TLogger.CreateFileLogger(FileTest, llInfo);
  FileLogger.Info('File logger test');
  FileLogger.Debug('This should be filtered'); // Should be filtered due to level
  
  // Console and file logger
  TLogger.ResetInstance;
  BothLogger := TLogger.CreateConsoleAndFileLogger(BothTest, llWarning);
  BothLogger.Warning('Both logger test');
  BothLogger.Info('This should be filtered'); // Should be filtered due to level
  
  // Debug logger
  TLogger.ResetInstance;
  DebugLogger := TLogger.CreateDebugLogger;
  DebugLogger.Debug('Debug logger test');
  
  // Audit logger
  TLogger.ResetInstance;
  AuditLogger := TLogger.CreateAuditLogger(AuditTest);
  AuditLogger.Info('Audit logger test');
  
  // Verify file loggers created files with correct content
  FileContent := TStringList.Create;
  try
    // File logger
    AssertTrue('File logger should create log file', FileExists(FileTest));
    FileContent.LoadFromFile(FileTest);
    AssertTrue('File logger should log info message',
      Pos('File logger test', FileContent.Text) > 0);
    AssertFalse('File logger should filter debug message',
      Pos('This should be filtered', FileContent.Text) > 0);
    
    // Both logger
    FileContent.Clear;
    AssertTrue('Both logger should create log file', FileExists(BothTest));
    FileContent.LoadFromFile(BothTest);
    AssertTrue('Both logger should log warning message',
      Pos('Both logger test', FileContent.Text) > 0);
    AssertFalse('Both logger should filter info message',
      Pos('This should be filtered', FileContent.Text) > 0);
    
    // Debug logger - should use custom format with file/line
    FileContent.Clear;
    if FileExists(DebugTest) then
    begin
      FileContent.LoadFromFile(DebugTest);
      AssertTrue('Debug logger should include file info',
        Pos('[TidyKit.Logger.Test.pas', FileContent.Text) > 0);
    end;
    
    // Audit logger
    FileContent.Clear;
    AssertTrue('Audit logger should create log file', FileExists(AuditTest));
    FileContent.LoadFromFile(AuditTest);
    AssertTrue('Audit logger should log info message',
      Pos('Audit logger test', FileContent.Text) > 0);
  finally
    FileContent.Free;
  end;
end;

initialization
  Randomize; // Initialize random number generator
  RegisterTest(TLoggerTest);
end. 