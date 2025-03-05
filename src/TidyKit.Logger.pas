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

  { Forward declaration for TLogContext }
  TLogContext = class;

  { TLogger class }
  TLogger = class
  private
    FLogDestinations: TLogDestinations;
    FLogFiles: array of TLogFile;
    FFormatDateTime: string;
    FMinLogLevel: TLogLevel;
    FContexts: TList; // List to track created contexts
    FInstanceID: Int64; // Unique identifier for this instance
    class var FInstance: TLogger;
    class var FNextInstanceID: Int64; // Counter for generating unique IDs
    
    procedure SetConsoleColor(ALogLevel: TLogLevel);
    procedure ResetConsoleColor;
    procedure RotateLogFile(var ALogFile: TLogFile);
    function GetLogLevelStr(ALogLevel: TLogLevel): string;
    procedure InitLogFile(var ALogFile: TLogFile);
    procedure WriteToFile(var ALogFile: TLogFile; const AMessage: string);
    procedure FreeContexts; // New method to free all contexts
  public
    constructor Create;
    destructor Destroy; override;
    
    { Instance identification }
    function GetInstanceID: Int64;
    
    { Logging methods }
    procedure Log(const AMessage: string; ALogLevel: TLogLevel = llInfo; const AFileIndex: Integer = -1);
    procedure Debug(const AMessage: string; const AFileIndex: Integer = -1);
    procedure Info(const AMessage: string; const AFileIndex: Integer = -1);
    procedure Warning(const AMessage: string; const AFileIndex: Integer = -1);
    procedure Error(const AMessage: string; const AFileIndex: Integer = -1);
    procedure Fatal(const AMessage: string; const AFileIndex: Integer = -1);
    
    { Format string overloads }
    procedure DebugFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1);
    procedure InfoFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1);
    procedure WarningFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1);
    procedure ErrorFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1);
    procedure FatalFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1);
    
    { Configuration methods with method chaining }
    function SetLogDestinations(ADestinations: TLogDestinations): TLogger;
    function SetDateTimeFormat(const AFormat: string): TLogger;
    function SetMinLogLevel(ALevel: TLogLevel): TLogger;
    function AddLogFile(const AFileName: string; AMaxSize: Int64 = 25 * 1024 * 1024): Integer;
    function AddDefaultLogFile(const ABaseName: string = 'application'; AMaxSize: Int64 = 25 * 1024 * 1024): Integer;
    procedure CloseLogFiles;
    
    { Category support }
    function CreateContext(const ACategory: string): TLogContext;
    
    { Singleton access }
    class function GetInstance: TLogger;
    class procedure ResetInstance;
    
    { Simple one-line setup }
    class function CreateDefaultLogger(ADestinations: TLogDestinations = [ldConsole, ldFile]; 
      const ALogFileName: string = ''; AMinLogLevel: TLogLevel = llDebug): TLogger;
  end;

{ TLogContext class for category-based logging }
TLogContext = class
private
  FLogger: TLogger;
  FCategory: string;
  FRefCount: Integer;  // Reference count
public
  constructor Create(ALogger: TLogger; const ACategory: string);
  destructor Destroy; override;
  
  { Reference counting methods }
  function AddRef: Integer;
  function Release: Integer;
  
  procedure Debug(const AMessage: string; const AFileIndex: Integer = -1);
  procedure Info(const AMessage: string; const AFileIndex: Integer = -1);
  procedure Warning(const AMessage: string; const AFileIndex: Integer = -1);
  procedure Error(const AMessage: string; const AFileIndex: Integer = -1);
  procedure Fatal(const AMessage: string; const AFileIndex: Integer = -1);
  
  { Format string overloads }
  procedure DebugFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1);
  procedure InfoFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1);
  procedure WarningFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1);
  procedure ErrorFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1);
  procedure FatalFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1);
end;

{ Global access function }
function Logger: TLogger;

implementation

function Logger: TLogger;
begin
  WriteLn('Logger function called');
  Result := TLogger.GetInstance;
  WriteLn('Logger function returning instance at address: ', IntToStr(PtrInt(Result)));
end;

{ TLogger }

constructor TLogger.Create;
begin
  inherited Create;
  FLogDestinations := [ldConsole];
  FFormatDateTime := 'yyyy-mm-dd hh:nn:ss.zzz';
  FMinLogLevel := llDebug; // Default to most verbose
  FContexts := TList.Create; // Initialize context list
  FInstanceID := FNextInstanceID;
  Inc(FNextInstanceID);
  WriteLn('TLogger.Create: Created new instance with ID: ', IntToStr(FInstanceID));
end;

destructor TLogger.Destroy;
begin
  CloseLogFiles;
  FreeContexts; // Release all contexts before destroying logger
  FContexts.Free;
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
  // Skip logging if below minimum log level
  if ALogLevel < FMinLogLevel then
    Exit;
    
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

// Format string overloads
procedure TLogger.DebugFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1);
begin
  Debug(Format(AFormat, AArgs), AFileIndex);
end;

procedure TLogger.InfoFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1);
begin
  Info(Format(AFormat, AArgs), AFileIndex);
end;

procedure TLogger.WarningFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1);
begin
  Warning(Format(AFormat, AArgs), AFileIndex);
end;

procedure TLogger.ErrorFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1);
begin
  Error(Format(AFormat, AArgs), AFileIndex);
end;

procedure TLogger.FatalFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1);
begin
  Fatal(Format(AFormat, AArgs), AFileIndex);
end;

// Method chaining configuration
function TLogger.SetLogDestinations(ADestinations: TLogDestinations): TLogger;
begin
  // If disabling file logging, close all files
  if (ldFile in FLogDestinations) and not (ldFile in ADestinations) then
    CloseLogFiles;
    
  FLogDestinations := ADestinations;
  Result := Self;
end;

function TLogger.SetDateTimeFormat(const AFormat: string): TLogger;
begin
  FFormatDateTime := AFormat;
  Result := Self;
end;

function TLogger.SetMinLogLevel(ALevel: TLogLevel): TLogger;
begin
  FMinLogLevel := ALevel;
  Result := Self;
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

function TLogger.AddDefaultLogFile(const ABaseName: string = 'application'; AMaxSize: Int64 = 25 * 1024 * 1024): Integer;
var
  LogDir, LogPath: string;
begin
  LogDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)) + 'logs');
  LogPath := LogDir + ABaseName + '.log';
  
  // Ensure log directory exists
  ForceDirectories(LogDir);
  
  Result := AddLogFile(LogPath, AMaxSize);
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

function TLogger.CreateContext(const ACategory: string): TLogContext;
begin
  Result := TLogContext.Create(Self, ACategory);
  Result.AddRef; // AddRef for the reference we're returning
  FContexts.Add(Result); // Track the context
end;

class function TLogger.GetInstance: TLogger;
begin
  WriteLn('GetInstance: FInstance address before check: ', IntToStr(PtrInt(FInstance)));
  if FInstance = nil then
  begin
    FInstance := TLogger.Create;
    WriteLn('GetInstance: Created new instance, address: ', IntToStr(PtrInt(FInstance)), ', ID: ', IntToStr(FInstance.FInstanceID));
  end
  else
    WriteLn('GetInstance: Returning existing instance, address: ', IntToStr(PtrInt(FInstance)), ', ID: ', IntToStr(FInstance.FInstanceID));
  Result := FInstance;
end;

class procedure TLogger.ResetInstance;
var
  OldInstanceID: Int64;
begin
  WriteLn('ResetInstance: FInstance address before: ', IntToStr(PtrInt(FInstance)));
  if FInstance <> nil then
  begin
    OldInstanceID := FInstance.FInstanceID;
    FInstance.CloseLogFiles;
    FInstance.FreeContexts; // Free all contexts
    FInstance.Free;
    FInstance := nil;
    WriteLn('ResetInstance: FInstance has been set to nil (destroyed ID: ', IntToStr(OldInstanceID), ')');
  end;
  WriteLn('ResetInstance: FInstance address after: ', IntToStr(PtrInt(FInstance)));
end;

class function TLogger.CreateDefaultLogger(ADestinations: TLogDestinations; 
  const ALogFileName: string; AMinLogLevel: TLogLevel): TLogger;
begin
  ResetInstance;
  Result := GetInstance;
  Result.SetLogDestinations(ADestinations).SetMinLogLevel(AMinLogLevel);
  
  if (ldFile in ADestinations) then
  begin
    if ALogFileName <> '' then
      Result.AddLogFile(ALogFileName)
    else
      Result.AddDefaultLogFile;
  end;
end;

procedure TLogger.FreeContexts;
var
  i: Integer;
begin
  if Assigned(FContexts) then
  begin
    for i := FContexts.Count - 1 downto 0 do
    begin
      if Assigned(FContexts[i]) then
        TLogContext(FContexts[i]).Release;
    end;
    FContexts.Clear;
  end;
end;

{ TLogContext }

constructor TLogContext.Create(ALogger: TLogger; const ACategory: string);
begin
  FLogger := ALogger;
  FCategory := ACategory;
  FRefCount := 0; // Start with 0, AddRef will be called explicitly
end;

destructor TLogContext.Destroy;
begin
  // Remove from logger's list if still there
  if Assigned(FLogger) and Assigned(FLogger.FContexts) then
    FLogger.FContexts.Remove(Self);
  inherited Destroy;
end;

function TLogContext.AddRef: Integer;
begin
  Inc(FRefCount);
  Result := FRefCount;
end;

function TLogContext.Release: Integer;
begin
  Dec(FRefCount);
  Result := FRefCount;
  if Result <= 0 then
  begin
    // Don't immediately free here to avoid issues if Release is called from within a method
    // Schedule for destruction instead
    Result := 0; // Ensure we don't get negative values
    Free;
  end;
end;

procedure TLogContext.Debug(const AMessage: string; const AFileIndex: Integer);
begin
  FLogger.Debug(Format('[%s] %s', [FCategory, AMessage]), AFileIndex);
end;

procedure TLogContext.Info(const AMessage: string; const AFileIndex: Integer);
begin
  FLogger.Info(Format('[%s] %s', [FCategory, AMessage]), AFileIndex);
end;

procedure TLogContext.Warning(const AMessage: string; const AFileIndex: Integer);
begin
  FLogger.Warning(Format('[%s] %s', [FCategory, AMessage]), AFileIndex);
end;

procedure TLogContext.Error(const AMessage: string; const AFileIndex: Integer);
begin
  FLogger.Error(Format('[%s] %s', [FCategory, AMessage]), AFileIndex);
end;

procedure TLogContext.Fatal(const AMessage: string; const AFileIndex: Integer);
begin
  FLogger.Fatal(Format('[%s] %s', [FCategory, AMessage]), AFileIndex);
end;

procedure TLogContext.DebugFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer);
begin
  Debug(Format(AFormat, AArgs), AFileIndex);
end;

procedure TLogContext.InfoFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer);
begin
  Info(Format(AFormat, AArgs), AFileIndex);
end;

procedure TLogContext.WarningFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer);
begin
  Warning(Format(AFormat, AArgs), AFileIndex);
end;

procedure TLogContext.ErrorFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer);
begin
  Error(Format(AFormat, AArgs), AFileIndex);
end;

procedure TLogContext.FatalFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer);
begin
  Fatal(Format(AFormat, AArgs), AFileIndex);
end;

function TLogger.GetInstanceID: Int64;
begin
  Result := FInstanceID;
end;

initialization
  WriteLn('TidyKit.Logger initialization: Setting FInstance to nil');
  TLogger.FInstance := nil;
  TLogger.FNextInstanceID := 1; // Initialize the instance ID counter

finalization
  WriteLn('TidyKit.Logger finalization: FInstance address: ', IntToStr(PtrInt(TLogger.FInstance)));
  if TLogger.FInstance <> nil then
  begin
    WriteLn('TidyKit.Logger finalization: Freeing FInstance');
    TLogger.FInstance.Free;
    WriteLn('TidyKit.Logger finalization: FInstance freed');
  end;

end. 
