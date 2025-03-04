{$mode objfpc}{$H+}
unit TidyKit.Log;

interface

uses
  Classes, SysUtils, SyncObjs;

type
  { Log levels }
  TLogLevel = (llDebug, llInfo, llWarning, llError, llFatal);

  { Log entry record }
  TLogEntry = record
    Level: TLogLevel;
    Message: string;
    TimeStamp: TDateTime;
    ThreadID: TThreadID;
    Category: string;
  end;

  { Log target interface }
  ILogTarget = interface
    ['{ABCD1234-5678-9ABC-DEF0-123456789ABC}']
    function GetName: string;
    procedure WriteLog(const AEntry: TLogEntry);
    procedure Flush;
  end;

  { Logger interface }
  ILogger = interface
    ['{12345678-ABCD-EF01-2345-6789ABCDEF01}']
    function Debug(const AMessage: string): ILogger; overload;
    function Debug(const AFormat: string; const AArgs: array of const): ILogger; overload;
    function Info(const AMessage: string): ILogger; overload;
    function Info(const AFormat: string; const AArgs: array of const): ILogger; overload;
    function Warning(const AMessage: string): ILogger; overload;
    function Warning(const AFormat: string; const AArgs: array of const): ILogger; overload;
    function Error(const AMessage: string): ILogger; overload;
    function Error(const AFormat: string; const AArgs: array of const): ILogger; overload;
    function Fatal(const AMessage: string): ILogger; overload;
    function Fatal(const AFormat: string; const AArgs: array of const): ILogger; overload;
  end;

  { Thread-safe queue }
  TThreadSafeQueue = class
  private
    FBuffer: array of TLogEntry;
    FHead: Integer;
    FTail: Integer;
    FCount: Integer;
    FCapacity: Integer;
    FLock: TCriticalSection;
    FEvent: TEvent;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    
    function TryEnqueue(const AEntry: TLogEntry): Boolean;
    function TryDequeue(out AEntry: TLogEntry): Boolean;
    function WaitForEntry(ATimeout: Cardinal): Boolean;
  end;

  { Log thread }
  TLogThread = class(TThread)
  private
    FQueue: TThreadSafeQueue;
    FTargets: array of ILogTarget;
    procedure ProcessLogEntry(const AEntry: TLogEntry);
  protected
    procedure Execute; override;
  public
    constructor Create(AQueue: TThreadSafeQueue);
    procedure AddTarget(const ATarget: ILogTarget);
  end;

  { Main logger implementation }
  TLogKit = class(TInterfacedObject, ILogger)
  private
    FQueue: TThreadSafeQueue;
    FLogThread: TLogThread;
    FEnabled: Boolean;
    FMinLevel: TLogLevel;
    procedure EnqueueEntry(const AEntry: TLogEntry);
  public
    constructor Create;
    destructor Destroy; override;
    
    { ILogger implementation }
    function Debug(const AMessage: string): ILogger; overload;
    function Debug(const AFormat: string; const AArgs: array of const): ILogger; overload;
    function Info(const AMessage: string): ILogger; overload;
    function Info(const AFormat: string; const AArgs: array of const): ILogger; overload;
    function Warning(const AMessage: string): ILogger; overload;
    function Warning(const AFormat: string; const AArgs: array of const): ILogger; overload;
    function Error(const AMessage: string): ILogger; overload;
    function Error(const AFormat: string; const AArgs: array of const): ILogger; overload;
    function Fatal(const AMessage: string): ILogger; overload;
    function Fatal(const AFormat: string; const AArgs: array of const): ILogger; overload;
    
    { Configuration methods }
    function AddTarget(const ATarget: ILogTarget): TLogKit;
    function SetMinLevel(ALevel: TLogLevel): TLogKit;
    function Enable: TLogKit;
    function Disable: TLogKit;
  end;

{ Factory functions }
function FileLogger(const AFilename: string): ILogger;
function ConsoleLogger: ILogger;
function MultiLogger: ILogger;

implementation

uses
  TidyKit.Log.Targets;  { Will contain target implementations }

{ TThreadSafeQueue implementation }

constructor TThreadSafeQueue.Create(ACapacity: Integer);
begin
  inherited Create;
  FCapacity := ACapacity;
  SetLength(FBuffer, FCapacity);
  FHead := 0;
  FTail := 0;
  FCount := 0;
  FLock := TCriticalSection.Create;
  FEvent := TEvent.Create(nil, True, False, '');
end;

destructor TThreadSafeQueue.Destroy;
begin
  FLock.Free;
  FEvent.Free;
  inherited;
end;

function TThreadSafeQueue.TryEnqueue(const AEntry: TLogEntry): Boolean;
begin
  Result := False;
  FLock.Enter;
  try
    if FCount < FCapacity then
    begin
      FBuffer[FTail] := AEntry;
      FTail := (FTail + 1) mod FCapacity;
      Inc(FCount);
      Result := True;
      FEvent.SetEvent;
    end;
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeQueue.TryDequeue(out AEntry: TLogEntry): Boolean;
begin
  Result := False;
  FLock.Enter;
  try
    if FCount > 0 then
    begin
      AEntry := FBuffer[FHead];
      FHead := (FHead + 1) mod FCapacity;
      Dec(FCount);
      Result := True;
      if FCount = 0 then
        FEvent.ResetEvent;
    end;
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeQueue.WaitForEntry(ATimeout: Cardinal): Boolean;
begin
  Result := FEvent.WaitFor(ATimeout) = wrSignaled;
end;

{ TLogThread implementation }

constructor TLogThread.Create(AQueue: TThreadSafeQueue);
begin
  inherited Create(False);
  FQueue := AQueue;
  SetLength(FTargets, 0);
  FreeOnTerminate := False;
end;

procedure TLogThread.AddTarget(const ATarget: ILogTarget);
begin
  SetLength(FTargets, Length(FTargets) + 1);
  FTargets[High(FTargets)] := ATarget;
end;

procedure TLogThread.ProcessLogEntry(const AEntry: TLogEntry);
var
  I: Integer;
begin
  for I := 0 to High(FTargets) do
    FTargets[I].WriteLog(AEntry);
end;

procedure TLogThread.Execute;
var
  Entry: TLogEntry;
begin
  while not Terminated do
  begin
    if FQueue.WaitForEntry(100) then
    begin
      while FQueue.TryDequeue(Entry) do
      begin
        ProcessLogEntry(Entry);
      end;
    end;
  end;
end;

{ TLogKit implementation }

constructor TLogKit.Create;
begin
  inherited Create;
  FQueue := TThreadSafeQueue.Create(1000);
  FLogThread := TLogThread.Create(FQueue);
  FEnabled := False;
  FMinLevel := llDebug;
end;

destructor TLogKit.Destroy;
begin
  FLogThread.Terminate;
  FLogThread.WaitFor;
  FLogThread.Free;
  FQueue.Free;
  inherited;
end;

procedure TLogKit.EnqueueEntry(const AEntry: TLogEntry);
begin
  if not FEnabled or (AEntry.Level < FMinLevel) then
    Exit;
  FQueue.TryEnqueue(AEntry);
end;

function TLogKit.Debug(const AMessage: string): ILogger;
var
  Entry: TLogEntry;
begin
  Entry.Level := llDebug;
  Entry.Message := AMessage;
  Entry.TimeStamp := Now;
  Entry.ThreadID := GetCurrentThreadId;
  Entry.Category := '';
  EnqueueEntry(Entry);
  Result := Self;
end;

function TLogKit.Debug(const AFormat: string; const AArgs: array of const): ILogger;
begin
  Result := Debug(Format(AFormat, AArgs));
end;

function TLogKit.Info(const AMessage: string): ILogger;
var
  Entry: TLogEntry;
begin
  Entry.Level := llInfo;
  Entry.Message := AMessage;
  Entry.TimeStamp := Now;
  Entry.ThreadID := GetCurrentThreadId;
  Entry.Category := '';
  EnqueueEntry(Entry);
  Result := Self;
end;

function TLogKit.Info(const AFormat: string; const AArgs: array of const): ILogger;
begin
  Result := Info(Format(AFormat, AArgs));
end;

function TLogKit.Warning(const AMessage: string): ILogger;
var
  Entry: TLogEntry;
begin
  Entry.Level := llWarning;
  Entry.Message := AMessage;
  Entry.TimeStamp := Now;
  Entry.ThreadID := GetCurrentThreadId;
  Entry.Category := '';
  EnqueueEntry(Entry);
  Result := Self;
end;

function TLogKit.Warning(const AFormat: string; const AArgs: array of const): ILogger;
begin
  Result := Warning(Format(AFormat, AArgs));
end;

function TLogKit.Error(const AMessage: string): ILogger;
var
  Entry: TLogEntry;
begin
  Entry.Level := llError;
  Entry.Message := AMessage;
  Entry.TimeStamp := Now;
  Entry.ThreadID := GetCurrentThreadId;
  Entry.Category := '';
  EnqueueEntry(Entry);
  Result := Self;
end;

function TLogKit.Error(const AFormat: string; const AArgs: array of const): ILogger;
begin
  Result := Error(Format(AFormat, AArgs));
end;

function TLogKit.Fatal(const AMessage: string): ILogger;
var
  Entry: TLogEntry;
begin
  Entry.Level := llFatal;
  Entry.Message := AMessage;
  Entry.TimeStamp := Now;
  Entry.ThreadID := GetCurrentThreadId;
  Entry.Category := '';
  EnqueueEntry(Entry);
  Result := Self;
end;

function TLogKit.Fatal(const AFormat: string; const AArgs: array of const): ILogger;
begin
  Result := Fatal(Format(AFormat, AArgs));
end;

function TLogKit.AddTarget(const ATarget: ILogTarget): TLogKit;
begin
  FLogThread.AddTarget(ATarget);
  Result := Self;
end;

function TLogKit.SetMinLevel(ALevel: TLogLevel): TLogKit;
begin
  FMinLevel := ALevel;
  Result := Self;
end;

function TLogKit.Enable: TLogKit;
begin
  FEnabled := True;
  Result := Self;
end;

function TLogKit.Disable: TLogKit;
begin
  FEnabled := False;
  Result := Self;
end;

{ Factory functions implementation }

function FileLogger(const AFilename: string): ILogger;
begin
  Result := TLogKit.Create
    .AddTarget(TFileTarget.Create(AFilename))
    .Enable;
end;

function ConsoleLogger: ILogger;
begin
  Result := TLogKit.Create
    .AddTarget(TConsoleTarget.Create)
    .Enable;
end;

function MultiLogger: ILogger;
begin
  Result := TLogKit.Create.Enable;
end;

end. 