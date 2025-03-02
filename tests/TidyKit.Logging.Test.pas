unit TidyKit.Logging.Test;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  TidyKit.Logging,
  TidyKit.Logging.Console,
  TidyKit.Logging.FileLog;

type
  { Thread class for testing concurrent logging }
  TTestThread = class(TThread)
  private
    FLogger: TLogger;
    FIndex: Integer;
  public
    constructor Create(ALogger: TLogger; AIndex: Integer);
    procedure Execute; override;
  end;

  TLoggingTest = class(TTestCase)
  private
    FLogger: TLogger;
    FLogFile: string;
    FLogDir: string;
    procedure EnsureLogDirectory;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConsoleLogging;
    procedure TestFileLogging;
    procedure TestLogLevels;
    procedure TestFileRotation;
    procedure TestThreadSafety;
  end;

implementation

uses
  StrUtils;

{ TTestThread }

constructor TTestThread.Create(ALogger: TLogger; AIndex: Integer);
begin
  inherited Create(False);
  FLogger := ALogger;
  FIndex := AIndex;
  FreeOnTerminate := False;
end;

procedure TTestThread.Execute;
var
  j: Integer;
begin
  for j := 1 to 100 do
    FLogger.Info(Format('Thread %d - Message %d', [FIndex, j]));
end;

{ TLoggingTest }

procedure TLoggingTest.EnsureLogDirectory;
begin
  if not DirectoryExists(FLogDir) then
    if not ForceDirectories(FLogDir) then
      raise EInOutError.Create('Failed to create log directory: ' + FLogDir);
end;

procedure TLoggingTest.SetUp;
begin
  FLogDir := IncludeTrailingPathDelimiter(GetCurrentDir) + 'logs';
  // Create unique log file for each test using TestName
  FLogFile := IncludeTrailingPathDelimiter(FLogDir) + Format('test_%s.log', [TestName]);
  
  { Ensure clean state }
  if DirectoryExists(FLogDir) then
  begin
    if FileExists(FLogFile) then
      DeleteFile(FLogFile);
    // Don't remove directory as other tests might be using it
  end;
  
  { Create fresh log directory }
  EnsureLogDirectory;
  
  FLogger := TLogger.Create;
end;

procedure TLoggingTest.TearDown;
var
  i: Integer;
  BackupFile: string;
begin
  if Assigned(FLogger) then 
  begin
    // Wait for any pending logs to be processed
    FLogger.WaitForQueue(1000);
    
    // Free logger and its destinations which will close file handles
    FLogger.Free;
    FLogger := nil;
  end;
  
  // Give the system time to fully release file handles
  Sleep(500);
  
  { Clean up test files with retry }
  if FileExists(FLogFile) then
  begin
    // Retry deletion a few times if needed
    for i := 1 to 5 do
    begin
      try
        if DeleteFile(FLogFile) then
          Break;
      except
        if i < 5 then
          Sleep(100)
        else
          raise;
      end;
    end;
  end;
  
  { Clean up rotated files with retry }
  for i := 1 to 4 do
  begin
    BackupFile := Format('%s.%d', [FLogFile, i]);
    if FileExists(BackupFile) then
    begin
      try
        DeleteFile(BackupFile);
      except
        // Ignore errors on backup file cleanup
      end;
    end;
  end;
end;

procedure TLoggingTest.TestConsoleLogging;
var
  ConsoleDest: TConsoleDestination;
begin
  ConsoleDest := TConsoleDestination.Create(True);
  try
    FLogger.AddDestination(ConsoleDest);
    FLogger.Info('Console logging test');
    FLogger.Warning('This is a warning message');
    FLogger.Error('This is an error message');
  finally
    ConsoleDest.Free;
  end;
end;

procedure TLoggingTest.TestFileLogging;
var
  FileDest: TFileDestination;
  FileContents: string;
begin
  FileDest := TFileDestination.Create(FLogFile);
  try
    FLogger.AddDestination(FileDest);
    FLogger.Info('File logging test');
    FLogger.Warning('This is a warning message');
    
    // Wait for logs to be processed
    FLogger.WaitForQueue;
    
    // Free the destination to ensure file handle is released
    FileDest.Free;
    FileDest := nil;
    
    // Give the system time to fully release file handles
    Sleep(100);
    
    { Read file contents }
    FileContents := '';
    with TStringList.Create do
    try
      LoadFromFile(FLogFile);
      FileContents := Text;
    finally
      Free;
    end;
    
    { Verify log messages }
    AssertTrue('Log file should contain INFO message',
      Pos('INFO', FileContents) > 0);
    AssertTrue('Log file should contain warning message',
      Pos('WARN', FileContents) > 0);
  finally
    if Assigned(FileDest) then
      FileDest.Free;
  end;
end;

procedure TLoggingTest.TestLogLevels;
var
  FileDest: TFileDestination;
  FileContents: string;
begin
  FileDest := TFileDestination.Create(FLogFile);
  try
    FLogger
      .AddDestination(FileDest)
      .SetMinLogLevel(llWarning);
      
    { These should not be logged }
    FLogger.Debug('Debug message');
    FLogger.Info('Info message');
    
    { These should be logged }
    FLogger.Warning('Warning message');
    FLogger.Error('Error message');
    
    // Wait for logs to be processed
    FLogger.WaitForQueue;
    
    // Free the destination to ensure file handle is released
    FileDest.Free;
    FileDest := nil;
    
    // Give the system time to fully release file handles
    Sleep(100);
    
    { Read file contents }
    FileContents := '';
    with TStringList.Create do
    try
      LoadFromFile(FLogFile);
      FileContents := Text;
    finally
      Free;
    end;
    
    { Verify log messages }
    AssertTrue('Debug message should not be in log',
      Pos('DEBUG', FileContents) = 0);
    AssertTrue('Info message should not be in log',
      Pos('INFO', FileContents) = 0);
    AssertTrue('Warning message should be in log',
      Pos('WARN', FileContents) > 0);
    AssertTrue('Error message should be in log',
      Pos('ERROR', FileContents) > 0);
  finally
    if Assigned(FileDest) then
      FileDest.Free;
  end;
end;

procedure TLoggingTest.TestFileRotation;
var
  FileDest: TFileDestination;
  i: Integer;
  LongMessage: string;
begin
  { Create file destination with small max size }
  FileDest := TFileDestination.Create(FLogFile, 1024, 3);
  try
    FLogger.AddDestination(FileDest);
    
    { Write enough data to cause rotation }
    LongMessage := DupeString('This is a long message that will be repeated. ', 10);
    for i := 1 to 20 do
      FLogger.Info(Format('Message %d: %s', [i, LongMessage]));
      
    // Wait for logs to be processed
    FLogger.WaitForQueue;
    
    { Verify rotated files exist }
    AssertTrue('Original log file should exist',
      FileExists(FLogFile));
    AssertTrue('Backup file 1 should exist',
      FileExists(FLogFile + '.1'));
    AssertTrue('Backup file 2 should exist',
      FileExists(FLogFile + '.2'));
    AssertTrue('Backup file 3 should exist',
      FileExists(FLogFile + '.3'));
    AssertFalse('Backup file 4 should not exist',
      FileExists(FLogFile + '.4'));
  finally
    FileDest.Free;
  end;
end;

procedure TLoggingTest.TestThreadSafety;
var
  FileDest: TFileDestination;
  Threads: array[1..5] of TThread;
  i: Integer;
  FileStream: TFileStream;
begin
  FileDest := TFileDestination.Create(FLogFile);
  try
    FLogger.AddDestination(FileDest);
    
    { Create and start threads }
    for i := Low(Threads) to High(Threads) do
      Threads[i] := TTestThread.Create(FLogger, i);
      
    { Wait for all threads to finish }
    for i := Low(Threads) to High(Threads) do
    begin
      Threads[i].WaitFor;
      Threads[i].Free;
    end;
    
    // Wait for logs to be processed
    FLogger.WaitForQueue;
    
    // Free the destination to ensure file handle is released
    FileDest.Free;
    FileDest := nil;
    
    // Give the system time to fully release file handles
    Sleep(100);
    
    { Verify log file exists and has content }
    AssertTrue('Log file should exist', FileExists(FLogFile));
    
    FileStream := TFileStream.Create(FLogFile, fmOpenRead or fmShareDenyNone);
    try
      AssertTrue('Log file should have content', FileStream.Size > 0);
    finally
      FileStream.Free;
    end;
  finally
    if Assigned(FileDest) then
      FileDest.Free;
  end;
end;

initialization
  RegisterTest(TLoggingTest);
end. 