unit TidyKit.Logging.FileLog;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, TidyKit.Logging; 

type
  { File destination for logging }
  TFileDestination = class(TLogDestination)
  private
    FFilePath: string;
    FMaxFileSize: Int64;
    FMaxBackupCount: Integer;
    FFileStream: TFileStream;
    FLock: TRTLCriticalSection;
    
    procedure RotateLogFile;
    function FormatLogRecord(const ARecord: TLogRecord): string;
    procedure EnsureFileStream;
    procedure FlushFileStream;
  protected
    procedure DoLog(const ARecord: TLogRecord); override;
  public
    constructor Create(const AFilePath: string; AMaxFileSize: Int64 = 10 * 1024 * 1024;
      AMaxBackupCount: Integer = 5);
    destructor Destroy; override;
  end;

implementation

uses
{$IFDEF WINDOWS}
  Windows;
{$ELSE}
  BaseUnix;
{$ENDIF}

constructor TFileDestination.Create(const AFilePath: string; AMaxFileSize: Int64;
  AMaxBackupCount: Integer);
begin
  inherited Create;
  FFilePath := AFilePath;
  FMaxFileSize := AMaxFileSize;
  FMaxBackupCount := AMaxBackupCount;
  InitCriticalSection(FLock);
  EnsureFileStream;
end;

destructor TFileDestination.Destroy;
begin
  EnterCriticalSection(FLock);
  try
    if Assigned(FFileStream) then
    begin
      FlushFileStream;
      FreeAndNil(FFileStream);
    end;
  finally
    LeaveCriticalSection(FLock);
    DoneCriticalSection(FLock);
    inherited Destroy;
  end;
end;

procedure TFileDestination.EnsureFileStream;
var
  FileMode: Word;
  Attempts: Integer;
  MaxAttempts: Integer;
begin
  if not Assigned(FFileStream) then
  begin
    { Ensure directory exists }
    if not ForceDirectories(ExtractFileDir(FFilePath)) then
      raise EInOutError.CreateFmt('Failed to create directory: %s', [ExtractFileDir(FFilePath)]);
    
    { Set file mode based on existence }
    if FileExists(FFilePath) then
      FileMode := fmOpenWrite or fmShareDenyNone
    else
      FileMode := fmCreate or fmShareDenyNone;
      
    MaxAttempts := 5;
    Attempts := 0;
    while Attempts < MaxAttempts do
    try
      Inc(Attempts);
      FFileStream := TFileStream.Create(FFilePath, FileMode);
      if FileMode = fmCreate then
        FileMode := fmOpenWrite or fmShareDenyNone;
      FFileStream.Seek(0, soEnd);
      Break;
    except
      on E: EFOpenError do
      begin
        if Assigned(FFileStream) then
          FreeAndNil(FFileStream);
        if Attempts = MaxAttempts then
          raise EInOutError.CreateFmt('Failed to open log file "%s" after %d attempts: %s',
            [FFilePath, MaxAttempts, E.Message]);
        Sleep(100); // Wait a bit before retrying
      end;
    end;
  end;
end;

procedure TFileDestination.FlushFileStream;
begin
  if Assigned(FFileStream) then
  begin
    {$IFDEF WINDOWS}
    FlushFileBuffers(FFileStream.Handle);
    {$ELSE}
    fpfsync(FFileStream.Handle);
    {$ENDIF}
  end;
end;

procedure TFileDestination.RotateLogFile;
var
  i: Integer;
  OldName, NewName: string;
  Attempts: Integer;
  MaxAttempts: Integer;
begin
  EnterCriticalSection(FLock);
  try
    if Assigned(FFileStream) then
    begin
      FlushFileStream;
      FreeAndNil(FFileStream);
    end;

    { Delete oldest backup if it exists }
    if FMaxBackupCount > 0 then
    begin
      MaxAttempts := 5;
      
      { Delete oldest backup }
      OldName := Format('%s.%d', [FFilePath, FMaxBackupCount]);
      if FileExists(OldName) then
      begin
        Attempts := 0;
        while (Attempts < MaxAttempts) and FileExists(OldName) do
        begin
          Inc(Attempts);
          try
            SysUtils.DeleteFile(OldName);
          except
            Sleep(100);
          end;
        end;
      end;

      { Shift existing backup files }
      for i := FMaxBackupCount - 1 downto 1 do
      begin
        OldName := Format('%s.%d', [FFilePath, i]);
        NewName := Format('%s.%d', [FFilePath, i + 1]);
        if FileExists(OldName) then
        begin
          if FileExists(NewName) then
            SysUtils.DeleteFile(NewName);
          RenameFile(OldName, NewName);
        end;
      end;

      { Rename current log file to .1 }
      if FileExists(FFilePath) then
      begin
        NewName := Format('%s.1', [FFilePath]);
        if FileExists(NewName) then
          SysUtils.DeleteFile(NewName);
        RenameFile(FFilePath, NewName);
      end;
    end;

    { Create new log file }
    EnsureFileStream;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TFileDestination.FormatLogRecord(const ARecord: TLogRecord): string;
const
  LevelNames: array[TLogLevel] of string = (
    'DEBUG', 'INFO', 'WARN', 'ERROR', 'FATAL'
  );
begin
  Result := Format('[%s] [%s] [Thread %d] %s',
    [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', ARecord.Timestamp),
     LevelNames[ARecord.Level],
     ARecord.ThreadID,
     ARecord.Message]);
     
  if (ARecord.ClassName <> '') or (ARecord.MethodName <> '') then
    Result := Result + Format(' [%s.%s:%d]',
      [ARecord.ClassName, ARecord.MethodName, ARecord.LineNumber]);
      
  Result := Result + LineEnding;
end;

procedure TFileDestination.DoLog(const ARecord: TLogRecord);
var
  LogMessage: string;
  LogBytes: TBytes;
  Attempts: Integer;
  MaxAttempts: Integer;
begin
  EnterCriticalSection(FLock);
  try
    EnsureFileStream;
    
    { Check if we need to rotate }
    if (FMaxFileSize > 0) and (FFileStream.Size >= FMaxFileSize) then
      RotateLogFile;
      
    { Format and write the log message }
    LogMessage := FormatLogRecord(ARecord);
    LogBytes := TEncoding.UTF8.GetBytes(LogMessage);
    
    MaxAttempts := 5;
    Attempts := 0;
    while Attempts < MaxAttempts do
    try
      Inc(Attempts);
      FFileStream.WriteBuffer(LogBytes[0], Length(LogBytes));
      FlushFileStream;
      Break;
    except
      on E: EWriteError do
      begin
        if Attempts = MaxAttempts then
          raise;
        Sleep(100);
      end;
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

end. 
