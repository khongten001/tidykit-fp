unit TidyKit.Log.Test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, TypInfo,
  TidyKit.Log, TidyKit.Log.Targets;

type
  { Test target that stores log entries in memory }
  TMemoryTarget = class(TInterfacedObject, ILogTarget)
  private
    FEntries: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    
    { ILogTarget implementation }
    function GetName: string;
    procedure WriteLog(const AEntry: TLogEntry);
    procedure Flush;
    
    { Test methods }
    function GetEntryCount: Integer;
    function GetEntry(AIndex: Integer): string;
    procedure Clear;
  end;

  { Thread for testing thread safety }
  TTestThread = class(TThread)
  private
    FLogger: ILogger;
    FCount: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(ALogger: ILogger; ACount: Integer);
  end;

  { Test cases }
  TLogTest = class(TTestCase)
  private
    FMemoryTarget: TMemoryTarget;
    FLogger: ILogger;
    FTestFilePath: string;
    procedure WaitForLogging;
    function GetTestFilePath(const AFileName: string): string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test01_CreateLogger;
    procedure Test02a_DebugLevel;
    procedure Test02b_InfoLevel;
    procedure Test02c_WarningLevel;
    procedure Test02d_ErrorLevel;
    procedure Test02e_FatalLevel;
    procedure Test03_FormattedMessages;
    procedure Test04a_BelowMinimumLevel;
    procedure Test04b_AboveMinimumLevel;
    procedure Test05_EnableDisable;
    procedure Test06_MultipleTargets;
    procedure Test07_ThreadSafeQueue;
    procedure Test08_FileTarget;
    procedure Test09_ConsoleTarget;
    procedure Test10_LogRotation;
    procedure Test11_ConsoleColors;
    procedure Test12_FactoryFunctions;
    procedure Test13_CategorySupport;
    procedure Test14_ThreadSafety;
    procedure Test15_MemoryManagement;
  end;

implementation

uses
  DateUtils;

{ TMemoryTarget implementation }

constructor TMemoryTarget.Create;
begin
  inherited Create;
  FEntries := TStringList.Create;
end;

destructor TMemoryTarget.Destroy;
begin
  FEntries.Free;
  inherited;
end;

function TMemoryTarget.GetName: string;
begin
  Result := 'Memory';
end;

procedure TMemoryTarget.WriteLog(const AEntry: TLogEntry);
var
  LogLine: string;
  LevelStr: string;
begin
  case AEntry.Level of
    llDebug: LevelStr := 'DEBUG';
    llInfo: LevelStr := 'INFO';
    llWarning: LevelStr := 'WARNING';
    llError: LevelStr := 'ERROR';
    llFatal: LevelStr := 'FATAL';
  end;
  WriteLn('Log level string: ', LevelStr);  // Debug output
  
  if AEntry.Category <> '' then
    LogLine := Format('[%s] [%s] [%d] [%s] %s',
      [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', AEntry.TimeStamp),
       LevelStr,
       AEntry.ThreadID,
       AEntry.Category,
       AEntry.Message])
  else
    LogLine := Format('[%s] [%s] [%d] %s',
      [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', AEntry.TimeStamp),
       LevelStr,
       AEntry.ThreadID,
       AEntry.Message]);
  FEntries.Add(LogLine);
end;

procedure TMemoryTarget.Flush;
begin
  // Nothing to flush in memory target
end;

function TMemoryTarget.GetEntryCount: Integer;
begin
  Result := FEntries.Count;
end;

function TMemoryTarget.GetEntry(AIndex: Integer): string;
begin
  Result := FEntries[AIndex];
end;

procedure TMemoryTarget.Clear;
begin
  FEntries.Clear;
end;

{ TTestThread implementation }

constructor TTestThread.Create(ALogger: ILogger; ACount: Integer);
begin
  inherited Create(False);
  FLogger := ALogger;
  FCount := ACount;
  FreeOnTerminate := False;
end;

procedure TTestThread.Execute;
var
  I: Integer;
begin
  for I := 1 to FCount do
    FLogger.Info('Thread message %d', [I]);
end;

{ TLogTest implementation }

procedure TLogTest.WaitForLogging;
begin
  Sleep(500);  { Increase wait time for logging operations }
end;

function TLogTest.GetTestFilePath(const AFileName: string): string;
begin
  Result := IncludeTrailingPathDelimiter(GetCurrentDir) + AFileName;
end;

procedure TLogTest.SetUp;
begin
  FMemoryTarget := TMemoryTarget.Create;
  FLogger := TLogKit.Create
    .AddTarget(FMemoryTarget)
    .Enable;  // Ensure logger is enabled
  FTestFilePath := GetTestFilePath('test.log');
end;

procedure TLogTest.TearDown;
begin
  FLogger := nil;  { Release logger }
  FMemoryTarget := nil;  { Release target }
  if FileExists(FTestFilePath) then
    DeleteFile(FTestFilePath);
end;

procedure TLogTest.Test01_CreateLogger;
begin
  WriteLn('Test01_CreateLogger is starting...');
  AssertNotNull('Logger should not be nil', FLogger);
  FLogger.Info('Test message');
  WaitForLogging;
  AssertEquals('One log entry should be written', 1, FMemoryTarget.GetEntryCount);
end;

procedure TLogTest.Test02a_DebugLevel;
var
  LogEntry: string;
begin
  WriteLn('Test02a_DebugLevel is starting...');
  FLogger.Debug('Debug message');
  WaitForLogging;
  AssertEquals('One log entry should be written', 1, FMemoryTarget.GetEntryCount);
  LogEntry := FMemoryTarget.GetEntry(0);
  WriteLn('Debug test - Log entry: ', LogEntry);
  AssertTrue('Debug level should be logged', Pos('DEBUG', LogEntry) > 0);
end;

procedure TLogTest.Test02b_InfoLevel;
var
  LogEntry: string;
begin
  WriteLn('Test02b_InfoLevel is starting...');
  FLogger.Info('Info message');
  WaitForLogging;
  AssertEquals('One log entry should be written', 1, FMemoryTarget.GetEntryCount);
  LogEntry := FMemoryTarget.GetEntry(0);
  WriteLn('Info test - Log entry: ', LogEntry);
  AssertTrue('Info level should be logged', Pos('INFO', LogEntry) > 0);
end;

procedure TLogTest.Test02c_WarningLevel;
begin
  WriteLn('Test02c_WarningLevel is starting...');
  FLogger.Warning('Warning message');
  WaitForLogging;
  AssertEquals('One log entry should be written', 1, FMemoryTarget.GetEntryCount);
  AssertTrue('Warning level should be logged', Pos('WARNING', FMemoryTarget.GetEntry(0)) > 0);
end;

procedure TLogTest.Test02d_ErrorLevel;
begin
  WriteLn('Test02d_ErrorLevel is starting...');
  FLogger.Error('Error message');
  WaitForLogging;
  AssertEquals('One log entry should be written', 1, FMemoryTarget.GetEntryCount);
  AssertTrue('Error level should be logged', Pos('ERROR', FMemoryTarget.GetEntry(0)) > 0);
end;

procedure TLogTest.Test02e_FatalLevel;
begin
  WriteLn('Test02e_FatalLevel is starting...');
  FLogger.Fatal('Fatal message');
  WaitForLogging;
  AssertEquals('One log entry should be written', 1, FMemoryTarget.GetEntryCount);
  AssertTrue('Fatal level should be logged', Pos('FATAL', FMemoryTarget.GetEntry(0)) > 0);
end;

procedure TLogTest.Test03_FormattedMessages;
begin
  WriteLn('Test03_FormattedMessages is starting...');
  FLogger.Info('Value: %d, String: %s', [42, 'test']);
  WaitForLogging;
  AssertEquals('One log entry should be written', 1, FMemoryTarget.GetEntryCount);
  AssertTrue('Formatted message should be correct',
    Pos('Value: 42, String: test', FMemoryTarget.GetEntry(0)) > 0);
end;

procedure TLogTest.Test04a_BelowMinimumLevel;
var
  Logger: ILogger;
begin
  WriteLn('Test04a_BelowMinimumLevel is starting...');
  Logger := TLogKit.Create
    .AddTarget(FMemoryTarget)
    .SetMinLevel(llWarning)
    .Enable;
    
  Logger.Debug('Debug message');  { Should not be logged }
  Logger.Info('Info message');    { Should not be logged }
  
  WaitForLogging;
  AssertEquals('Messages below minimum level should not be logged', 0, FMemoryTarget.GetEntryCount);
end;

procedure TLogTest.Test04b_AboveMinimumLevel;
var
  Logger: ILogger;
begin
  WriteLn('Test04b_AboveMinimumLevel is starting...');
  Logger := TLogKit.Create
    .AddTarget(FMemoryTarget)
    .SetMinLevel(llWarning)
    .Enable;
    
  Logger.Warning('Warning message');
  WaitForLogging;
  AssertEquals('One message should be logged', 1, FMemoryTarget.GetEntryCount);
  AssertTrue('Warning level should be logged', Pos('WARNING', FMemoryTarget.GetEntry(0)) > 0);
end;

procedure TLogTest.Test05_EnableDisable;
var
  Logger: ILogger;
begin
  WriteLn('Test05_EnableDisable is starting...');
  Logger := TLogKit.Create
    .AddTarget(FMemoryTarget)
    .Enable;

  Logger.Info('First message');
  (Logger as TLogKit).Disable;
  Logger.Info('Second message');  { Should not be logged }
  (Logger as TLogKit).Enable;
  Logger.Info('Third message');
  
  WaitForLogging;
  AssertEquals('Two entries should be logged', 2, FMemoryTarget.GetEntryCount);
end;

procedure TLogTest.Test06_MultipleTargets;
var
  SecondTarget: TMemoryTarget;
  Logger: ILogger;
begin
  WriteLn('Test06_MultipleTargets is starting...');
  SecondTarget := TMemoryTarget.Create;
  try
    Logger := TLogKit.Create
      .AddTarget(FMemoryTarget)
      .AddTarget(SecondTarget)
      .Enable;
      
    Logger.Info('Test message');
    WaitForLogging;
    
    AssertEquals('First target should have one entry', 1, FMemoryTarget.GetEntryCount);
    AssertEquals('Second target should have one entry', 1, SecondTarget.GetEntryCount);
  finally
    SecondTarget := nil;
  end;
end;

procedure TLogTest.Test07_ThreadSafeQueue;
var
  I: Integer;
  ThreadCount: Integer;
  Threads: array of TThread;
begin
  WriteLn('Test07_ThreadSafeQueue is starting...');
  ThreadCount := 5;
  SetLength(Threads, ThreadCount);
  
  for I := 0 to ThreadCount - 1 do
    Threads[I] := TTestThread.Create(FLogger, 100);
    
  for I := 0 to ThreadCount - 1 do
    Threads[I].WaitFor;
    
  for I := 0 to ThreadCount - 1 do
    Threads[I].Free;
    
  WaitForLogging;
  AssertEquals('All messages should be logged',
    ThreadCount * 100, FMemoryTarget.GetEntryCount);
end;

procedure TLogTest.Test08_FileTarget;
var
  Logger: ILogger;
  Target: TFileTarget;
begin
  WriteLn('Test08_FileTarget is starting...');
  if FileExists(FTestFilePath) then
    DeleteFile(FTestFilePath);
    
  Target := TFileTarget.Create(FTestFilePath);
  try
    Logger := TLogKit.Create
      .AddTarget(Target)
      .Enable;
      
    Logger.Info('Test message');
    WaitForLogging;
    Target.Flush;
    Logger := nil;
    Target := nil;
    
    WaitForLogging;
    AssertTrue('Log file should be created', FileExists(FTestFilePath));
  finally
    if FileExists(FTestFilePath) then
      DeleteFile(FTestFilePath);
  end;
end;

procedure TLogTest.Test09_ConsoleTarget;
var
  Logger: ILogger;
begin
  WriteLn('Test09_ConsoleTarget is starting...');
  Logger := TLogKit.Create
    .AddTarget(TConsoleTarget.Create)
    .Enable;
    
  Logger.Info('Console test message');
  Logger := nil;
end;

procedure TLogTest.Test10_LogRotation;
var
  Logger: ILogger;
  Target: TFileTarget;
  I: Integer;
  BaseFile: string;
begin
  WriteLn('Test10_LogRotation is starting...');
  BaseFile := GetTestFilePath('rotation.log');
  
  if FileExists(BaseFile) then
    DeleteFile(BaseFile);
    
  Target := TFileTarget.Create(BaseFile);
  try
    Target.SetMaxSize(100)
          .SetRotateCount(3);
          
    Logger := TLogKit.Create
      .AddTarget(Target)
      .Enable;
      
    for I := 1 to 10 do
    begin
      Logger.Info('Test message with some padding to force rotation %d', [I]);
      WaitForLogging;
      Target.Flush;
    end;
      
    WaitForLogging;
    Target.Flush;
    
    Logger := nil;
    Target := nil;
    
    WaitForLogging;
    AssertTrue('Original log file should exist', FileExists(BaseFile));
  finally
    if FileExists(BaseFile) then
      DeleteFile(BaseFile);
    if FileExists(BaseFile + '.1') then
      DeleteFile(BaseFile + '.1');
    if FileExists(BaseFile + '.2') then
      DeleteFile(BaseFile + '.2');
    if FileExists(BaseFile + '.3') then
      DeleteFile(BaseFile + '.3');
  end;
end;

procedure TLogTest.Test11_ConsoleColors;
var
  Target: TConsoleTarget;
  Entry: TLogEntry;
begin
  WriteLn('Test11_ConsoleColors is starting...');
  Target := TConsoleTarget.Create;
  try
    Target.EnableColors;
    Entry.Level := llError;
    Entry.Message := 'Test message';
    Entry.TimeStamp := Now;
    Entry.ThreadID := GetCurrentThreadId;
    Entry.Category := '';
    
    Target.WriteLog(Entry);
    
    Target.DisableColors;
    Target.WriteLog(Entry);
  finally
    Target.Free;
  end;
end;

procedure TLogTest.Test12_FactoryFunctions;
var
  Logger: ILogger;
begin
  WriteLn('Test12_FactoryFunctions is starting...');
  Logger := FileLogger('factory.log');
  AssertNotNull('FileLogger should create logger', Logger);
  Logger := nil;
  DeleteFile('factory.log');
  
  Logger := ConsoleLogger;
  AssertNotNull('ConsoleLogger should create logger', Logger);
  Logger := nil;
  
  Logger := MultiLogger;
  AssertNotNull('MultiLogger should create logger', Logger);
  Logger := nil;
end;

procedure TLogTest.Test13_CategorySupport;
var
  Entry: TLogEntry;
begin
  WriteLn('Test13_CategorySupport is starting...');
  Entry.Level := llInfo;
  Entry.Message := 'Test message';
  Entry.TimeStamp := Now;
  Entry.ThreadID := GetCurrentThreadId;
  Entry.Category := 'TestCategory';
  
  FMemoryTarget.WriteLog(Entry);
  
  AssertEquals('One log entry should be written', 1, FMemoryTarget.GetEntryCount);
  AssertTrue('Log entry should contain category name',
    Pos('TestCategory', FMemoryTarget.GetEntry(0)) > 0);
end;

procedure TLogTest.Test14_ThreadSafety;
var
  Queue: TThreadSafeQueue;
  Entry: TLogEntry;
  Success: Boolean;
begin
  WriteLn('Test14_ThreadSafety is starting...');
  Queue := TThreadSafeQueue.Create(2);
  try
    Entry.Message := 'Test';
    
    Success := Queue.TryEnqueue(Entry);
    AssertTrue('First enqueue should succeed', Success);
    
    Success := Queue.TryEnqueue(Entry);
    AssertTrue('Second enqueue should succeed', Success);
    
    Success := Queue.TryEnqueue(Entry);
    AssertFalse('Third enqueue should fail (queue full)', Success);
    
    Success := Queue.TryDequeue(Entry);
    AssertTrue('First dequeue should succeed', Success);
    
    Success := Queue.TryDequeue(Entry);
    AssertTrue('Second dequeue should succeed', Success);
    
    Success := Queue.TryDequeue(Entry);
    AssertFalse('Third dequeue should fail (queue empty)', Success);
  finally
    Queue.Free;
  end;
end;

procedure TLogTest.Test15_MemoryManagement;
var
  Logger, Logger2: ILogger;
  Target: TMemoryTarget;
begin
  WriteLn('Test15_MemoryManagement is starting...');
  Target := TMemoryTarget.Create;
  try
    Logger := TLogKit.Create
      .AddTarget(Target)
      .Enable;
      
    Logger2 := Logger;
    
    Logger.Info('Test message');
    WaitForLogging;
    AssertEquals('First message should be logged', 1, Target.GetEntryCount);
    
    Logger := nil;
    WaitForLogging;  // Wait for any pending operations
    
    Logger2.Info('Second message');
    WaitForLogging;
    AssertEquals('Both messages should be logged', 2, Target.GetEntryCount);
    
    Logger2 := nil;
    WaitForLogging;  // Wait for any pending operations before freeing target
  finally
    FLogger := nil;  // Ensure main logger is released
    FMemoryTarget := nil;  // Ensure main target is released
    WaitForLogging;  // Final wait for cleanup
    Target.Free;
  end;
end;

initialization
  RegisterTest(TLogTest);
end.