unit TidyKit.Logger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$IFDEF WINDOWS}, Windows {$ENDIF}
  {$IFDEF UNIX}, BaseUnix {$ENDIF};

type
  { Log level enumeration }
  TLogLevel = (llDebug, llInfo, llWarning, llError, llFatal);
  
  { Log destination enumeration }
  TLogDestination = (ldConsole, ldFile);
  TLogDestinations = set of TLogDestination;

  { Log file record structure }
  TLogFile = record
    FileName: string;
    MaxSize: Int64;
    Stream: TFileStream;
  end;

  { TLogger class }
  TLogger = class
  private
    FLogDestinations: TLogDestinations;
    FLogFiles: array of TLogFile;
    FFormatDateTime: string;
    class var FInstance: TLogger;
    
    procedure SetConsoleColor(ALogLevel: TLogLevel);
    procedure ResetConsoleColor;
    procedure RotateLogFile(var ALogFile: TLogFile);
    function GetLogLevelStr(ALogLevel: TLogLevel): string;
    procedure InitLogFile(var ALogFile: TLogFile);
    procedure WriteToFile(var ALogFile: TLogFile; const AMessage: string);
  public
    constructor Create;
    destructor Destroy; override;
    
    { Logging methods }
    procedure Log(const AMessage: string; ALogLevel: TLogLevel = llInfo; const AFileIndex: Integer = -1);
    procedure Debug(const AMessage: string; const AFileIndex: Integer = -1);
    procedure Info(const AMessage: string; const AFileIndex: Integer = -1);
    procedure Warning(const AMessage: string; const AFileIndex: Integer = -1);
    procedure Error(const AMessage: string; const AFileIndex: Integer = -1);
    procedure Fatal(const AMessage: string; const AFileIndex: Integer = -1);
    
    { Configuration methods }
    procedure SetLogDestinations(ADestinations: TLogDestinations);
    procedure SetDateTimeFormat(const AFormat: string);
    function AddLogFile(const AFileName: string; AMaxSize: Int64 = 25 * 1024 * 1024): Integer;
    procedure CloseLogFiles;
    
    { Singleton access }
    class function GetInstance: TLogger;
    class procedure ResetInstance;
  end;

{ Global access function }
function Logger: TLogger;

implementation

function Logger: TLogger;
begin
  Result := TLogger.GetInstance;
end;

{ TLogger }

constructor TLogger.Create;
begin
  inherited Create;
  FLogDestinations := [ldConsole];
  FFormatDateTime := 'yyyy-mm-dd hh:nn:ss.zzz';
end;

destructor TLogger.Destroy;
begin
  CloseLogFiles;
  inherited Destroy;
end;

procedure TLogger.SetConsoleColor(ALogLevel: TLogLevel);
begin
  {$IFDEF WINDOWS}
  case ALogLevel of
    llDebug: SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 8);    // Gray
    llInfo: SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 7);     // White
    llWarning: SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 14); // Yellow
    llError: SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 12);   // Red
    llFatal: SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 79);   // White on Red
  end;
  {$ENDIF}
  {$IFDEF UNIX}
  case ALogLevel of
    llDebug: Write(#27'[90m');    // Gray
    llInfo: Write(#27'[0m');      // Default
    llWarning: Write(#27'[33m');  // Yellow
    llError: Write(#27'[31m');    // Red
    llFatal: Write(#27'[97;41m'); // White on Red
  end;
  {$ENDIF}
end;

procedure TLogger.ResetConsoleColor;
begin
  {$IFDEF WINDOWS}
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 7); // Default
  {$ENDIF}
  {$IFDEF UNIX}
  Write(#27'[0m');
  {$ENDIF}
end;

function TLogger.GetLogLevelStr(ALogLevel: TLogLevel): string;
begin
  case ALogLevel of
    llDebug: Result := 'DEBUG';
    llInfo: Result := 'INFO';
    llWarning: Result := 'WARNING';
    llError: Result := 'ERROR';
    llFatal: Result := 'FATAL';
  end;
end;

procedure TLogger.WriteToFile(var ALogFile: TLogFile; const AMessage: string);
var
  FileHandle: TextFile;
  MessageWithNewLine: string;
begin
  try
    AssignFile(FileHandle, ALogFile.FileName);
    if FileExists(ALogFile.FileName) then
      Append(FileHandle)
    else
      Rewrite(FileHandle);
      
    try
      MessageWithNewLine := AMessage + LineEnding;
      Write(FileHandle, MessageWithNewLine);
    finally
      CloseFile(FileHandle);
    end;
  except
    on E: Exception do
    begin
      // If writing failed, try again after a short delay
      Sleep(100);
      try
        AssignFile(FileHandle, ALogFile.FileName);
        if FileExists(ALogFile.FileName) then
          Append(FileHandle)
        else
          Rewrite(FileHandle);
          
        MessageWithNewLine := AMessage + LineEnding;
        Write(FileHandle, MessageWithNewLine);
        CloseFile(FileHandle);
      except
        // If it fails again, silently continue - don't crash the application
        // for logging errors
      end;
    end;
  end;
end;

procedure TLogger.InitLogFile(var ALogFile: TLogFile);
var
  FileHandle: TextFile;
begin
  try
    // Just verify we can create the file
    AssignFile(FileHandle, ALogFile.FileName);
    if not FileExists(ALogFile.FileName) then
    begin
      Rewrite(FileHandle);
      CloseFile(FileHandle);
    end;
  except
    on E: Exception do
      raise Exception.CreateFmt('Failed to initialize log file %s: %s', [ALogFile.FileName, E.Message]);
  end;
end;

procedure TLogger.RotateLogFile(var ALogFile: TLogFile);
var
  NewFileName: string;
  FileHandle: TextFile;
begin
  // Exit if file doesn't exist - nothing to rotate
  if not FileExists(ALogFile.FileName) then
    Exit;
    
  // Generate new filename with timestamp
  NewFileName := ChangeFileExt(ALogFile.FileName, 
    FormatDateTime('_yyyymmdd_hhnnss', Now) + ExtractFileExt(ALogFile.FileName));
  
  try
    // Try to rename the file
    if not RenameFile(ALogFile.FileName, NewFileName) then
    begin
      // If rename fails, try another approach - truncate the file
      AssignFile(FileHandle, ALogFile.FileName);
      Rewrite(FileHandle);
      CloseFile(FileHandle);
    end;
  except
    // If rename operations fail, just truncate the file
    AssignFile(FileHandle, ALogFile.FileName);
    Rewrite(FileHandle);
    CloseFile(FileHandle);
  end;

  // Ensure that a new log file exists
  try
    AssignFile(FileHandle, ALogFile.FileName);
    Rewrite(FileHandle);
    CloseFile(FileHandle);
  except
    // If creation fails, just continue
  end;
end;

procedure TLogger.Log(const AMessage: string; ALogLevel: TLogLevel; const AFileIndex: Integer);
var
  LogMessage: string;
  i: Integer;
  CurrentFileSize: Int64;
  F: file;
  NeedRotation: Boolean;
begin
  LogMessage := Format('[%s] [%s] %s',
    [FormatDateTime(FFormatDateTime, Now),
     GetLogLevelStr(ALogLevel),
     AMessage]);

  if ldConsole in FLogDestinations then
  begin
    SetConsoleColor(ALogLevel);
    WriteLn(LogMessage);
    ResetConsoleColor;
  end;

  if (ldFile in FLogDestinations) then
  begin
    // First check if any files need rotation before writing
    if AFileIndex = -1 then
    begin
      for i := 0 to High(FLogFiles) do
      begin
        // Check file size before writing
        NeedRotation := False;
        try
          if FileExists(FLogFiles[i].FileName) then
          begin
            AssignFile(F, FLogFiles[i].FileName);
            Reset(F, 1);
            try
              CurrentFileSize := FileSize(F);
              // If already at max size, rotate now
              if CurrentFileSize >= FLogFiles[i].MaxSize then
                NeedRotation := True;
              // Also check if this message would exceed the limit
              if (CurrentFileSize + Length(LogMessage) + 2) > FLogFiles[i].MaxSize then 
                NeedRotation := True;
            finally
              CloseFile(F);
            end;
          end;
        except
          // Ignore errors when checking file size
        end;
        
        // Rotate if needed
        if NeedRotation then
          RotateLogFile(FLogFiles[i]);
          
        // Now write the message
        WriteToFile(FLogFiles[i], LogMessage);
      end;
    end
    else if (AFileIndex >= 0) and (AFileIndex <= High(FLogFiles)) then
    begin
      // Check file size before writing
      NeedRotation := False;
      try
        if FileExists(FLogFiles[AFileIndex].FileName) then
        begin
          AssignFile(F, FLogFiles[AFileIndex].FileName);
          Reset(F, 1);
          try
            CurrentFileSize := FileSize(F);
            // If already at max size, rotate now
            if CurrentFileSize >= FLogFiles[AFileIndex].MaxSize then
              NeedRotation := True;
            // Also check if this message would exceed the limit
            if (CurrentFileSize + Length(LogMessage) + 2) > FLogFiles[AFileIndex].MaxSize then 
              NeedRotation := True;
          finally
            CloseFile(F);
          end;
        end;
      except
        // Ignore errors when checking file size
      end;
      
      // Rotate if needed
      if NeedRotation then
        RotateLogFile(FLogFiles[AFileIndex]);
        
      // Now write the message
      WriteToFile(FLogFiles[AFileIndex], LogMessage);
    end;
  end;
end;

procedure TLogger.Debug(const AMessage: string; const AFileIndex: Integer = -1);
begin
  Log(AMessage, llDebug, AFileIndex);
end;

procedure TLogger.Info(const AMessage: string; const AFileIndex: Integer = -1);
begin
  Log(AMessage, llInfo, AFileIndex);
end;

procedure TLogger.Warning(const AMessage: string; const AFileIndex: Integer = -1);
begin
  Log(AMessage, llWarning, AFileIndex);
end;

procedure TLogger.Error(const AMessage: string; const AFileIndex: Integer = -1);
begin
  Log(AMessage, llError, AFileIndex);
end;

procedure TLogger.Fatal(const AMessage: string; const AFileIndex: Integer = -1);
begin
  Log(AMessage, llFatal, AFileIndex);
end;

procedure TLogger.SetLogDestinations(ADestinations: TLogDestinations);
begin
  // If disabling file logging, close all files
  if (ldFile in FLogDestinations) and not (ldFile in ADestinations) then
    CloseLogFiles;
    
  FLogDestinations := ADestinations;
end;

procedure TLogger.SetDateTimeFormat(const AFormat: string);
begin
  FFormatDateTime := AFormat;
end;

function TLogger.AddLogFile(const AFileName: string; AMaxSize: Int64): Integer;
var
  NewLogFile: TLogFile;
begin
  NewLogFile.FileName := AFileName;
  NewLogFile.MaxSize := AMaxSize;
  NewLogFile.Stream := nil;

  // Create directory if it doesn't exist
  ForceDirectories(ExtractFilePath(AFileName));
  
  // Only initialize the file if file logging is enabled
  if ldFile in FLogDestinations then
    InitLogFile(NewLogFile);

  SetLength(FLogFiles, Length(FLogFiles) + 1);
  FLogFiles[High(FLogFiles)] := NewLogFile;
  Result := High(FLogFiles);
end;

procedure TLogger.CloseLogFiles;
var
  i: Integer;
begin
  for i := 0 to High(FLogFiles) do
  begin
    if Assigned(FLogFiles[i].Stream) then
    begin
      FLogFiles[i].Stream.Free;
      FLogFiles[i].Stream := nil;
    end;
  end;
  SetLength(FLogFiles, 0);
end;

class function TLogger.GetInstance: TLogger;
begin
  if FInstance = nil then
    FInstance := TLogger.Create;
  Result := FInstance;
end;

class procedure TLogger.ResetInstance;
begin
  if FInstance <> nil then
  begin
    FInstance.CloseLogFiles;
    FInstance.Free;
    FInstance := nil;
  end;
end;

initialization
  TLogger.FInstance := nil;

finalization
  if TLogger.FInstance <> nil then
    TLogger.FInstance.Free;

end. 
