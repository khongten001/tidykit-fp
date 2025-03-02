unit TidyKit.Logging.File;

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
  protected
    procedure DoLog(const ARecord: TLogRecord); override;
  public
    constructor Create(const AFilePath: string; AMaxFileSize: Int64 = 10 * 1024 * 1024;
      AMaxBackupCount: Integer = 5);
    destructor Destroy; override;
  end;

implementation

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
  if Assigned(FFileStream) then
    FFileStream.Free;
  DoneCriticalSection(FLock);
  inherited Destroy;
end;

procedure TFileDestination.EnsureFileStream;
var
  FileExists: Boolean;
begin
  if not Assigned(FFileStream) then
  begin
    FileExists := FileExists(FFilePath);
    ForceDirectories(ExtractFileDir(FFilePath));
    FFileStream := TFileStream.Create(FFilePath,
      fmOpenWrite or fmShareDenyWrite);
    if not FileExists then
      FFileStream.Size := 0;
    FFileStream.Seek(0, soEnd);
  end;
end;

procedure TFileDestination.RotateLogFile;
var
  i: Integer;
  OldName, NewName: string;
begin
  FFileStream.Free;
  FFileStream := nil;

  { Delete oldest backup if it exists }
  if FMaxBackupCount > 0 then
  begin
    OldName := Format('%s.%d', [FFilePath, FMaxBackupCount]);
    if FileExists(OldName) then
      DeleteFile(OldName);

    { Shift existing backup files }
    for i := FMaxBackupCount - 1 downto 1 do
    begin
      OldName := Format('%s.%d', [FFilePath, i]);
      NewName := Format('%s.%d', [FFilePath, i + 1]);
      if FileExists(OldName) then
        RenameFile(OldName, NewName);
    end;

    { Rename current log file to .1 }
    if FileExists(FFilePath) then
      RenameFile(FFilePath, Format('%s.1', [FFilePath]));
  end;

  { Create new log file }
  EnsureFileStream;
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
    FFileStream.WriteBuffer(LogBytes[0], Length(LogBytes));
    FFileStream.Flush;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

end. 