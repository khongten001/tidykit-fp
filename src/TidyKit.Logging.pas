unit TidyKit.Logging;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, SyncObjs, DateUtils;

type
  { Log levels from least to most severe }
  TLogLevel = (llDebug, llInfo, llWarning, llError, llFatal);

  { Record to store log entry details }
  TLogRecord = record
    Timestamp: TDateTime;
    Level: TLogLevel;
    Message: string;
    ThreadID: TThreadID;
    ClassName: string;
    MethodName: string;
    LineNumber: Integer;
  end;

  PLogRecord = ^TLogRecord;

  { Abstract base class for log destinations }
  TLogDestination = class abstract
  private
    FEnabled: Boolean;
    FMinLevel: TLogLevel;
  protected
    procedure DoLog(const ARecord: TLogRecord); virtual; abstract;
  public
    constructor Create;
    property Enabled: Boolean read FEnabled write FEnabled;
    property MinLevel: TLogLevel read FMinLevel write FMinLevel;
    procedure Log(const ARecord: TLogRecord);
  end;

  { Thread-safe queue for log records }
  TLogQueue = class
  private
    FLock: TCriticalSection;
    FQueue: TList;
    FMaxSize: Integer;
  public
    constructor Create(AMaxSize: Integer = 1000);
    destructor Destroy; override;
    function Enqueue(const ARecord: TLogRecord): Boolean;
    function Dequeue(out ARecord: TLogRecord): Boolean;
    function Count: Integer;
  end;

  { Background thread for asynchronous logging }
  TLogThread = class(TThread)
  private
    FQueue: TLogQueue;
    FDestinations: TList;
    FEvent: TEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(AQueue: TLogQueue; ADestinations: TList);
    destructor Destroy; override;
    procedure SignalWork;
  end;

  { Main logger class }
  TLogger = class
  private
    FLogThread: TLogThread;
    FDestinations: TList;
    FQueue: TLogQueue;
    FMinLogLevel: TLogLevel;
    FLock: TCriticalSection;
    procedure InternalLog(ALevel: TLogLevel; const AMessage: string;
      const AClassName: string = ''; const AMethodName: string = '';
      ALineNumber: Integer = 0);
  public
    constructor Create;
    destructor Destroy; override;
    
    { Fluent configuration methods }
    function SetMinLogLevel(ALevel: TLogLevel): TLogger;
    function AddDestination(ADest: TLogDestination): TLogger;
    
    { Logging methods }
    procedure Log(ALevel: TLogLevel; const AMessage: string);
    procedure Debug(const AMessage: string);
    procedure Info(const AMessage: string);
    procedure Warning(const AMessage: string);
    procedure Error(const AMessage: string);
    procedure Fatal(const AMessage: string);
    
    { Queue management }
    procedure WaitForQueue(ATimeout: Integer = 1000);
  end;

implementation

{ TLogDestination }

constructor TLogDestination.Create;
begin
  inherited Create;
  FEnabled := True;
  FMinLevel := llDebug;
end;

procedure TLogDestination.Log(const ARecord: TLogRecord);
begin
  if not FEnabled then Exit;
  if ARecord.Level < FMinLevel then Exit;
  DoLog(ARecord);
end;

{ TLogQueue }

constructor TLogQueue.Create(AMaxSize: Integer);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FQueue := TList.Create;
  FMaxSize := AMaxSize;
end;

destructor TLogQueue.Destroy;
var
  i: Integer;
  PRecord: PLogRecord;
begin
  FLock.Enter;
  try
    for i := 0 to FQueue.Count - 1 do
    begin
      PRecord := PLogRecord(FQueue[i]);
      Dispose(PRecord);
    end;
  finally
    FLock.Leave;
  end;
  FQueue.Free;
  FLock.Free;
  inherited Destroy;
end;

function TLogQueue.Enqueue(const ARecord: TLogRecord): Boolean;
var
  PRecord: PLogRecord;
begin
  Result := False;
  FLock.Enter;
  try
    if FQueue.Count < FMaxSize then
    begin
      New(PRecord);
      PRecord^ := ARecord;
      FQueue.Add(PRecord);
      Result := True;
    end;
  finally
    FLock.Leave;
  end;
end;

function TLogQueue.Dequeue(out ARecord: TLogRecord): Boolean;
var
  PRecord: PLogRecord;
begin
  Result := False;
  FLock.Enter;
  try
    if FQueue.Count > 0 then
    begin
      PRecord := PLogRecord(FQueue[0]);
      ARecord := PRecord^;
      Dispose(PRecord);
      FQueue.Delete(0);
      Result := True;
    end;
  finally
    FLock.Leave;
  end;
end;

function TLogQueue.Count: Integer;
begin
  FLock.Enter;
  try
    Result := FQueue.Count;
  finally
    FLock.Leave;
  end;
end;

{ TLogThread }

constructor TLogThread.Create(AQueue: TLogQueue; ADestinations: TList);
begin
  inherited Create(False);
  FQueue := AQueue;
  FDestinations := ADestinations;
  FEvent := TEvent.Create(nil, False, False, '');
  FreeOnTerminate := False;
end;

destructor TLogThread.Destroy;
begin
  FEvent.Free;
  inherited Destroy;
end;

procedure TLogThread.SignalWork;
begin
  FEvent.SetEvent;
end;

procedure TLogThread.Execute;
var
  LogRecord: TLogRecord;
  i: Integer;
  WaitResult: TWaitResult;
begin
  while not Terminated do
  begin
    if FQueue.Dequeue(LogRecord) then
    begin
      for i := 0 to FDestinations.Count - 1 do
        TLogDestination(FDestinations[i]).Log(LogRecord);
    end
    else
    begin
      WaitResult := FEvent.WaitFor(100);
      if WaitResult = wrTimeout then
        Sleep(1); // Give other threads a chance to run
    end;
  end;
end;

{ TLogger }

procedure TLogger.WaitForQueue(ATimeout: Integer);
var
  StartTime: TDateTime;
  ElapsedMS: Integer;
begin
  StartTime := Now;
  while (FQueue.Count > 0) and not FLogThread.Terminated do
  begin
    ElapsedMS := MilliSecondsBetween(Now, StartTime);
    if ElapsedMS >= ATimeout then
      Break;
    Sleep(1);
  end;
  // Give destinations time to flush
  Sleep(50);
end;

constructor TLogger.Create;
begin
  inherited Create;
  FDestinations := TList.Create;
  FQueue := TLogQueue.Create;
  FLock := TCriticalSection.Create;
  FLogThread := TLogThread.Create(FQueue, FDestinations);
  FMinLogLevel := llDebug;
end;

destructor TLogger.Destroy;
begin
  FLogThread.Terminate;
  WaitForQueue(1000); // Wait for queue to empty
  FLogThread.SignalWork; // Wake up thread
  FLogThread.WaitFor;
  FLogThread.Free;
  FQueue.Free;
  FDestinations.Free;
  FLock.Free;
  inherited Destroy;
end;

function TLogger.SetMinLogLevel(ALevel: TLogLevel): TLogger;
begin
  FMinLogLevel := ALevel;
  Result := Self;
end;

function TLogger.AddDestination(ADest: TLogDestination): TLogger;
begin
  FLock.Enter;
  try
    FDestinations.Add(ADest);
  finally
    FLock.Leave;
  end;
  Result := Self;
end;

procedure TLogger.InternalLog(ALevel: TLogLevel; const AMessage: string;
  const AClassName: string; const AMethodName: string; ALineNumber: Integer);
var
  LogRecord: TLogRecord;
begin
  if ALevel < FMinLogLevel then Exit;
  
  LogRecord.Timestamp := Now;
  LogRecord.Level := ALevel;
  LogRecord.Message := AMessage;
  LogRecord.ThreadID := GetCurrentThreadId;
  LogRecord.ClassName := AClassName;
  LogRecord.MethodName := AMethodName;
  LogRecord.LineNumber := ALineNumber;
  
  if not FQueue.Enqueue(LogRecord) then
    { If queue is full, try direct logging }
    TLogDestination(FDestinations[0]).Log(LogRecord);
    
  FLogThread.SignalWork;
end;

procedure TLogger.Log(ALevel: TLogLevel; const AMessage: string);
begin
  InternalLog(ALevel, AMessage);
end;

procedure TLogger.Debug(const AMessage: string);
begin
  InternalLog(llDebug, AMessage);
end;

procedure TLogger.Info(const AMessage: string);
begin
  InternalLog(llInfo, AMessage);
end;

procedure TLogger.Warning(const AMessage: string);
begin
  InternalLog(llWarning, AMessage);
end;

procedure TLogger.Error(const AMessage: string);
begin
  InternalLog(llError, AMessage);
end;

procedure TLogger.Fatal(const AMessage: string);
begin
  InternalLog(llFatal, AMessage);
end;

end. 
