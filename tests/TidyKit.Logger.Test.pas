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

initialization
  Randomize; // Initialize random number generator
  RegisterTest(TLoggerTest);
end. 