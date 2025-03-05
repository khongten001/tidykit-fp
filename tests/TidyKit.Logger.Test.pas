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

initialization
  Randomize; // Initialize random number generator
  RegisterTest(TLoggerTest);
end. 