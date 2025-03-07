unit TidyKit.Logger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, StrUtils, Types
  {$IFDEF WINDOWS}, Windows {$ENDIF}
  {$IFDEF UNIX}, BaseUnix {$ENDIF};

const
  ENABLE_CONSOLE_LOG = False; // Set to True to enable console output, False to disable

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
  
  { Format token types }
  TFormatToken = (ftText, ftTime, ftLevel, ftCategory, ftMessage, ftFile, ftLine);
  
  { Format token information }
  TFormatTokenInfo = record
    TokenType: TFormatToken;
    TextValue: string; // Used for ftText
  end;
  
  { Format pattern array }
  TFormatPattern = array of TFormatTokenInfo;
  
  { Log sink interface }
  ILogSink = interface
    ['{A4F2B3C1-D5E6-4F78-8A9B-0C1D2E3F4A5B}']
    procedure Write(const AFormattedMessage: string; ALevel: TLogLevel);
    procedure Flush;
  end;
  
  { Name-value pair for structured logging }
  TNameValuePair = record
    Name: string;
    ValueType: (nvtString, nvtInteger, nvtBoolean, nvtFloat);
    StringValue: string;  // Used when ValueType = nvtString
    IntValue: Integer;    // Used when ValueType = nvtInteger  
    BoolValue: Boolean;   // Used when ValueType = nvtBoolean
    FloatValue: Double;   // Used when ValueType = nvtFloat
  end;
  
  { Logger configuration record }
  TLoggerConfig = record
    Destinations: TLogDestinations;
    MinLevel: TLogLevel;
    DateTimeFormat: string;
    FormatPattern: string;
    DefaultLogFile: string;
    MaxFileSize: Int64;
  end;
  
  { Timed operation interface }
  ITimedOperation = interface
    ['{A4F2B3C1-D5E6-4F78-8A9B-0C1D2E3F4A5C}']
    function GetName: string;
    property Name: string read GetName;
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
    FFormatPattern: TFormatPattern;
    FFormatString: string;
    FSinks: TInterfaceList; // List of ILogSink
    FBatchMode: Boolean;
    FBatchBuffer: TStringList;
    class var FInstance: TLogger;
    class var FNextInstanceID: Int64; // Counter for generating unique IDs
    
    procedure SetConsoleColor(ALogLevel: TLogLevel);
    procedure ResetConsoleColor;
    procedure RotateLogFile(var ALogFile: TLogFile);
    function GetLogLevelStr(ALogLevel: TLogLevel): string;
    procedure InitLogFile(var ALogFile: TLogFile);
    procedure WriteToFile(var ALogFile: TLogFile; const AMessage: string);
    procedure FreeContexts; // Method to free all contexts
    function ParseFormatPattern(const APattern: string): TFormatPattern;
    function FormatLogMessage(const ATime: TDateTime; ALevel: TLogLevel; 
                              const ACategory, AMessage, AFile: string; 
                              ALine: Integer): string;
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
    
    { Format string overloads - Original }
    procedure DebugFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1);
    procedure InfoFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1);
    procedure WarningFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1);
    procedure ErrorFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1);
    procedure FatalFmt(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1);
    
    { New format string overloads (streamlined syntax) }
    procedure Debug(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1); overload;
    procedure Info(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1); overload;
    procedure Warning(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1); overload;
    procedure Error(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1); overload;
    procedure Fatal(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1); overload;
    
    { Specialized logging functions for common data types }
    procedure LogValue(const AName: string; const AValue: Integer; ALevel: TLogLevel = llInfo); overload;
    procedure LogValue(const AName: string; const AValue: Boolean; ALevel: TLogLevel = llInfo); overload;
    procedure LogValue(const AName: string; const AValue: Double; ALevel: TLogLevel = llInfo); overload;
    procedure LogValue(const AName: string; const AValue: string; ALevel: TLogLevel = llInfo); overload;
    
    { Structured logging }
    procedure LogStructured(ALevel: TLogLevel; const AMessage: string; const AFields: array of TNameValuePair);
    
    { Category direct support }
    procedure LogWithCategory(const ACategory: string; ALevel: TLogLevel; const AMessage: string);
    procedure DebugWithCategory(const ACategory, AMessage: string);
    procedure InfoWithCategory(const ACategory, AMessage: string);
    procedure WarningWithCategory(const ACategory, AMessage: string);
    procedure ErrorWithCategory(const ACategory, AMessage: string);
    procedure FatalWithCategory(const ACategory, AMessage: string);
    
    { Timing functionality }
    function TimedBlock(const AName: string): ITimedOperation;
    
    { Batch logging }
    procedure BeginBatch;
    procedure EndBatch;
    
    { Configuration methods with method chaining }
    function SetLogDestinations(ADestinations: TLogDestinations): TLogger;
    function SetDateTimeFormat(const AFormat: string): TLogger;
    function SetMinLogLevel(ALevel: TLogLevel): TLogger;
    function SetFormat(const APattern: string): TLogger;
    function AddLogFile(const AFileName: string; AMaxSize: Int64 = 25 * 1024 * 1024): Integer;
    function AddDefaultLogFile(const ABaseName: string = 'application'; AMaxSize: Int64 = 25 * 1024 * 1024): Integer;
    procedure CloseLogFiles;
    
    { Sink management }
    function AddSink(ASink: ILogSink): TLogger;
    procedure RemoveSink(ASink: ILogSink);
    procedure ClearSinks;
    
    { Category support }
    function CreateContext(const ACategory: string): TLogContext;
    
    { Global configuration }
    procedure LoadConfiguration(const AConfigFile: string);
    procedure ConfigureFromEnvironment;
    procedure Configure(const AConfig: TLoggerConfig);
    
    { Singleton access }
    class function GetInstance: TLogger;
    class procedure ResetInstance;
    
    { Factory methods for simplified initialization }
    class function CreateConsoleLogger(AMinLevel: TLogLevel = llDebug): TLogger;
    class function CreateFileLogger(const AFilename: string; AMinLevel: TLogLevel = llDebug): TLogger;
    class function CreateConsoleAndFileLogger(const AFilename: string; AMinLevel: TLogLevel = llDebug): TLogger;
    class function CreateDebugLogger: TLogger;
    class function CreateAuditLogger(const AFilename: string): TLogger;
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
  
  { New format string overloads (streamlined syntax) }
  procedure Debug(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1); overload;
  procedure Info(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1); overload;
  procedure Warning(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1); overload;
  procedure Error(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1); overload;
  procedure Fatal(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer = -1); overload;
end;

{ Helper functions }
function Logger: TLogger;
function NameValuePair(const AName: string; const AValue: string): TNameValuePair; overload;
function NameValuePair(const AName: string; AValue: Integer): TNameValuePair; overload;
function NameValuePair(const AName: string; AValue: Boolean): TNameValuePair; overload;
function NameValuePair(const AName: string; AValue: Double): TNameValuePair; overload;

{ Standard sink implementations }
type
  TConsoleSink = class(TInterfacedObject, ILogSink)
  public
    procedure Write(const AFormattedMessage: string; ALevel: TLogLevel); 
    procedure Flush;
  end;
  
  TFileSink = class(TInterfacedObject, ILogSink)
  private
    FFilename: string;
    FMaxSize: Int64;
  public
    constructor Create(const AFilename: string; AMaxSize: Int64 = 25 * 1024 * 1024);
    procedure Write(const AFormattedMessage: string; ALevel: TLogLevel); virtual;
    procedure Flush;
  end;
  
  TRotatingFileSink = class(TFileSink)
  private
    FMaxFiles: Integer;
  public
    constructor Create(const AFilename: string; AMaxSize: Int64 = 25 * 1024 * 1024; AMaxFiles: Integer = 5);
    procedure Write(const AFormattedMessage: string; ALevel: TLogLevel); override;
  end;
  
  TDailyFileSink = class(TInterfacedObject, ILogSink)
  private
    FBaseFilename: string;
    FCurrentDay: Word;
    FCurrentFilename: string;
  public
    constructor Create(const ABaseFilename: string);
    procedure Write(const AFormattedMessage: string; ALevel: TLogLevel);
    procedure Flush;
  end;
  
  TMemorySink = class(TInterfacedObject, ILogSink)
  private
    FMessages: TStringList;
    FMaxMessages: Integer;
  public
    constructor Create(AMaxMessages: Integer = 1000);
    destructor Destroy; override;
    procedure Write(const AFormattedMessage: string; ALevel: TLogLevel);
    procedure Flush;
    function GetMessages: TStringList;
    procedure Clear;
  end;
  
  // Callback procedure types
  TSimpleLogCallback = procedure of object;
  TMessageLogCallback = procedure(const AMessage: string; ALevel: TLogLevel) of object;

  TCallbackSink = class(TInterfacedObject, ILogSink)
  private
    FSimpleCallback: TSimpleLogCallback;
    FMessageCallback: TMessageLogCallback;
  public
    constructor Create(ASimpleCallback: TSimpleLogCallback); overload;
    constructor Create(AMessageCallback: TMessageLogCallback); overload;
    procedure Write(const AFormattedMessage: string; ALevel: TLogLevel);
    procedure Flush;
  end;

{ Timer implementation }
TTimedOperation = class(TInterfacedObject, ITimedOperation)
private
  FLogger: TLogger;
  FName: string;
  FStartTime: TDateTime;
  FActive: Boolean;
public
  constructor Create(ALogger: TLogger; const AName: string);
  destructor Destroy; override;
  function GetName: string;
end;

implementation

{ Helper functions implementation }
function NameValuePair(const AName: string; const AValue: string): TNameValuePair;
begin
  Result.Name := AName;
  Result.ValueType := nvtString;
  Result.StringValue := AValue;
end;

function NameValuePair(const AName: string; AValue: Integer): TNameValuePair;
begin
  Result.Name := AName;
  Result.ValueType := nvtInteger;
  Result.IntValue := AValue;
  Result.StringValue := '';  // Initialize other fields
  Result.BoolValue := False;
  Result.FloatValue := 0.0;
end;

function NameValuePair(const AName: string; AValue: Boolean): TNameValuePair;
begin
  Result.Name := AName;
  Result.ValueType := nvtBoolean;
  Result.BoolValue := AValue;
  Result.StringValue := '';  // Initialize other fields
  Result.IntValue := 0;
  Result.FloatValue := 0.0;
end;

function NameValuePair(const AName: string; AValue: Double): TNameValuePair;
begin
  Result.Name := AName;
  Result.ValueType := nvtFloat;
  Result.FloatValue := AValue;
  Result.StringValue := '';  // Initialize other fields
  Result.IntValue := 0;
  Result.BoolValue := False;
end;

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
  FMinLogLevel := llDebug; // Default to most verbose
  FContexts := TList.Create; // Initialize context list
  FSinks := TInterfaceList.Create; // Initialize sink list
  FBatchBuffer := TStringList.Create; // Initialize batch buffer
  FBatchMode := False;
  
  // Set default format pattern
  SetFormat('[%time] [%level] %message');
  
  // Set instance ID
  FInstanceID := FNextInstanceID;
  Inc(FNextInstanceID);
end;

destructor TLogger.Destroy;
begin
  CloseLogFiles;
  FreeContexts; // Release all contexts before destroying logger
  FContexts.Free;
  
  // Explicitly free FSinks instead of just setting to nil
  if Assigned(FSinks) then
  begin
    FSinks.Clear; // Clear all interfaces first
    FSinks.Free;  // Then free the list itself
  end;
  
  FBatchBuffer.Free;
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

  // If in batch mode, just store the message and return
  if FBatchMode and Assigned(FBatchBuffer) then
  begin
    FBatchBuffer.Add(LogMessage);
    Exit;
  end;

  if ldConsole in FLogDestinations then
  begin
    SetConsoleColor(ALogLevel);
    
    if ENABLE_CONSOLE_LOG then WriteLn(LogMessage);
    
    ResetConsoleColor;
  end;

  // Process sinks
  if Assigned(FSinks) then
  begin
    for i := 0 to FSinks.Count - 1 do
    begin
      if Assigned(FSinks[i]) then
      begin
        try
          (FSinks[i] as ILogSink).Write(LogMessage, ALogLevel);
        except
          // Ignore errors in sinks
        end;
      end;
    end;
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
              CurrentFileSize := System.FileSize(F);
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
            CurrentFileSize := System.FileSize(F);
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
  DirPath: string;
begin
  NewLogFile.FileName := AFileName;
  NewLogFile.MaxSize := AMaxSize;
  NewLogFile.Stream := nil;

  // Create directory if it doesn't exist
  DirPath := ExtractFilePath(AFileName);
  if (DirPath <> '') then
  begin
    try
      ForceDirectories(DirPath);
    except
      // Continue even if directory creation fails
      // We'll attempt to create the file anyway
    end;
  end;
  
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
  if (LogDir <> '') then
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
  if ENABLE_CONSOLE_LOG then WriteLn('GetInstance: FInstance address before check: ', IntToStr(PtrInt(FInstance)));
  if FInstance = nil then
  begin
    FInstance := TLogger.Create;
    if ENABLE_CONSOLE_LOG then WriteLn('GetInstance: Created new instance, address: ', IntToStr(PtrInt(FInstance)), ', ID: ', IntToStr(FInstance.FInstanceID));
  end
  else
    if ENABLE_CONSOLE_LOG then WriteLn('GetInstance: Returning existing instance, address: ', IntToStr(PtrInt(FInstance)), ', ID: ', IntToStr(FInstance.FInstanceID));
  Result := FInstance;
end;

class procedure TLogger.ResetInstance;
var
  OldInstanceID: Int64;
begin
  if ENABLE_CONSOLE_LOG then WriteLn('ResetInstance: FInstance address before: ', IntToStr(PtrInt(FInstance)));
  if FInstance <> nil then
  begin
    OldInstanceID := FInstance.FInstanceID;
    
    // Make sure all resources are properly released
    FInstance.CloseLogFiles;
    FInstance.FreeContexts; // Free all contexts
    
    // Clear all sinks before destroying
    if Assigned(FInstance.FSinks) then
      FInstance.FSinks.Clear;
    
    FInstance.Free;
    FInstance := nil;

    if ENABLE_CONSOLE_LOG then WriteLn('ResetInstance: FInstance has been set to nil (destroyed ID: ', IntToStr(OldInstanceID), ')');
  end;

  if ENABLE_CONSOLE_LOG then WriteLn('ResetInstance: FInstance address after: ', IntToStr(PtrInt(FInstance)));
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

procedure TLogContext.Debug(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer);
begin
  Debug(Format(AFormat, AArgs), AFileIndex);
end;

procedure TLogContext.Info(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer);
begin
  Info(Format(AFormat, AArgs), AFileIndex);
end;

procedure TLogContext.Warning(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer);
begin
  Warning(Format(AFormat, AArgs), AFileIndex);
end;

procedure TLogContext.Error(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer);
begin
  Error(Format(AFormat, AArgs), AFileIndex);
end;

procedure TLogContext.Fatal(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer);
begin
  Fatal(Format(AFormat, AArgs), AFileIndex);
end;

function TLogger.GetInstanceID: Int64;
begin
  Result := FInstanceID;
end;

function TLogger.SetFormat(const APattern: string): TLogger;
begin
  FFormatString := APattern;
  FFormatPattern := ParseFormatPattern(APattern);
  Result := Self;
end;

function TLogger.ParseFormatPattern(const APattern: string): TFormatPattern;
var
  Pattern: string;
  i, StartPos, CurPos: Integer;
  InToken: Boolean;
  Token: string;
  TokenInfo: TFormatTokenInfo;
begin
  SetLength(Result, 0);
  
  Pattern := APattern;
  if Pattern = '' then
    Pattern := '[%time] [%level] %message'; // Default pattern
    
  CurPos := 1;
  StartPos := 1;
  InToken := False;
  
  while CurPos <= Length(Pattern) do
  begin
    if Pattern[CurPos] = '%' then
    begin
      // Add text before token if any
      if CurPos > StartPos then
      begin
        SetLength(Result, Length(Result) + 1);
        TokenInfo.TokenType := ftText;
        TokenInfo.TextValue := Copy(Pattern, StartPos, CurPos - StartPos);
        Result[High(Result)] := TokenInfo;
      end;
      
      // Start reading token
      InToken := True;
      StartPos := CurPos;
    end
    else if InToken and ((Pattern[CurPos] = ' ') or (Pattern[CurPos] = ']') or (CurPos = Length(Pattern))) then
    begin
      // End of token found
      if Pattern[CurPos] = ' ' then
        Token := Copy(Pattern, StartPos, CurPos - StartPos)
      else if Pattern[CurPos] = ']' then
        Token := Copy(Pattern, StartPos, CurPos - StartPos)
      else
        Token := Copy(Pattern, StartPos, CurPos - StartPos + 1);
      
      SetLength(Result, Length(Result) + 1);
      
      if Token = '%time' then
        TokenInfo.TokenType := ftTime
      else if Token = '%level' then
        TokenInfo.TokenType := ftLevel
      else if Token = '%category' then
        TokenInfo.TokenType := ftCategory
      else if Token = '%message' then
        TokenInfo.TokenType := ftMessage
      else if Token = '%file' then
        TokenInfo.TokenType := ftFile
      else if Token = '%line' then
        TokenInfo.TokenType := ftLine
      else
      begin
        // Unknown token, treat as text
        TokenInfo.TokenType := ftText;
        TokenInfo.TextValue := Token;
      end;
      
      Result[High(Result)] := TokenInfo;
      
      InToken := False;
      StartPos := CurPos;
    end;
    
    Inc(CurPos);
  end;
  
  // Add any remaining text
  if StartPos <= Length(Pattern) then
  begin
    SetLength(Result, Length(Result) + 1);
    TokenInfo.TokenType := ftText;
    TokenInfo.TextValue := Copy(Pattern, StartPos, Length(Pattern) - StartPos + 1);
    Result[High(Result)] := TokenInfo;
  end;
end;

function TLogger.FormatLogMessage(const ATime: TDateTime; ALevel: TLogLevel; 
                          const ACategory, AMessage, AFile: string; 
                          ALine: Integer): string;
var
  i: Integer;
  TokenInfo: TFormatTokenInfo;
begin
  Result := '';
  
  for i := 0 to High(FFormatPattern) do
  begin
    TokenInfo := FFormatPattern[i];
    
    case TokenInfo.TokenType of
      ftText: Result := Result + TokenInfo.TextValue;
      ftTime: Result := Result + FormatDateTime(FFormatDateTime, ATime);
      ftLevel: Result := Result + GetLogLevelStr(ALevel);
      ftCategory: Result := Result + ACategory;
      ftMessage: Result := Result + AMessage;
      ftFile: Result := Result + AFile;
      ftLine: Result := Result + IntToStr(ALine);
    end;
  end;
end;

// Category direct support
procedure TLogger.LogWithCategory(const ACategory: string; ALevel: TLogLevel; const AMessage: string);
begin
  Log(Format('[%s] %s', [ACategory, AMessage]), ALevel);
end;

procedure TLogger.DebugWithCategory(const ACategory, AMessage: string);
begin
  LogWithCategory(ACategory, llDebug, AMessage);
end;

procedure TLogger.InfoWithCategory(const ACategory, AMessage: string);
begin
  LogWithCategory(ACategory, llInfo, AMessage);
end;

procedure TLogger.WarningWithCategory(const ACategory, AMessage: string);
begin
  LogWithCategory(ACategory, llWarning, AMessage);
end;

procedure TLogger.ErrorWithCategory(const ACategory, AMessage: string);
begin
  LogWithCategory(ACategory, llError, AMessage);
end;

procedure TLogger.FatalWithCategory(const ACategory, AMessage: string);
begin
  LogWithCategory(ACategory, llFatal, AMessage);
end;

// Sink management
function TLogger.AddSink(ASink: ILogSink): TLogger;
begin
  if Assigned(ASink) then
    FSinks.Add(ASink);
  Result := Self;
end;

procedure TLogger.RemoveSink(ASink: ILogSink);
begin
  if Assigned(FSinks) and Assigned(ASink) then
    FSinks.Remove(ASink);
end;

procedure TLogger.ClearSinks;
begin
  if Assigned(FSinks) then
    FSinks.Clear;
end;

// Global configuration
procedure TLogger.Configure(const AConfig: TLoggerConfig);
begin
  SetLogDestinations(AConfig.Destinations)
    .SetMinLogLevel(AConfig.MinLevel)
    .SetDateTimeFormat(AConfig.DateTimeFormat);
    
  if AConfig.FormatPattern <> '' then
    SetFormat(AConfig.FormatPattern);
    
  if (ldFile in AConfig.Destinations) and (AConfig.DefaultLogFile <> '') then
    AddLogFile(AConfig.DefaultLogFile, AConfig.MaxFileSize);
end;

function GetEnvVar(const Name: string): string;
var
  Buffer: array[0..1023] of Char;
begin
  Result := '';
  if GetEnvironmentVariable(PChar(Name), Buffer, SizeOf(Buffer)) > 0 then
    Result := string(Buffer);
end;

procedure TLogger.ConfigureFromEnvironment;
var
  Config: TLoggerConfig;
  EnvValue: string;
begin
  // Initialize with defaults
  Config.Destinations := [ldConsole];
  Config.MinLevel := llInfo;
  Config.DateTimeFormat := 'yyyy-mm-dd hh:nn:ss.zzz';
  Config.FormatPattern := '[%time] [%level] %message';
  Config.DefaultLogFile := '';
  Config.MaxFileSize := 25 * 1024 * 1024;
  
  // Read from environment
  EnvValue := GetEnvVar('LOGGER_DESTINATIONS');
  if EnvValue <> '' then
  begin
    Config.Destinations := [];
    if Pos('CONSOLE', UpperCase(EnvValue)) > 0 then
      Include(Config.Destinations, ldConsole);
    if Pos('FILE', UpperCase(EnvValue)) > 0 then
      Include(Config.Destinations, ldFile);
  end;
  
  EnvValue := GetEnvVar('LOGGER_LEVEL');
  if EnvValue <> '' then
  begin
    if UpperCase(EnvValue) = 'DEBUG' then
      Config.MinLevel := llDebug
    else if UpperCase(EnvValue) = 'INFO' then
      Config.MinLevel := llInfo
    else if UpperCase(EnvValue) = 'WARNING' then
      Config.MinLevel := llWarning
    else if UpperCase(EnvValue) = 'ERROR' then
      Config.MinLevel := llError
    else if UpperCase(EnvValue) = 'FATAL' then
      Config.MinLevel := llFatal;
  end;
  
  EnvValue := GetEnvVar('LOGGER_DATETIME_FORMAT');
  if EnvValue <> '' then
    Config.DateTimeFormat := EnvValue;
    
  EnvValue := GetEnvVar('LOGGER_FORMAT_PATTERN');
  if EnvValue <> '' then
    Config.FormatPattern := EnvValue;
    
  EnvValue := GetEnvVar('LOGGER_DEFAULT_FILE');
  if EnvValue <> '' then
    Config.DefaultLogFile := EnvValue;
    
  EnvValue := GetEnvVar('LOGGER_MAX_FILE_SIZE');
  if EnvValue <> '' then
  begin
    try
      Config.MaxFileSize := StrToInt64(EnvValue);
    except
      // Keep default if parse fails
    end;
  end;
  
  Configure(Config);
end;

procedure TLogger.LoadConfiguration(const AConfigFile: string);
var
  ConfigList: TStringList;
  i: Integer;
  Key, Value, Section: string;
  Config: TLoggerConfig;
  ConfigDir: string;
begin
  // Initialize with defaults
  Config.Destinations := [ldConsole];
  Config.MinLevel := llInfo;
  Config.DateTimeFormat := 'yyyy-mm-dd hh:nn:ss.zzz';
  Config.FormatPattern := '[%time] [%level] %message';
  Config.DefaultLogFile := '';
  Config.MaxFileSize := 25 * 1024 * 1024;

  // Get the directory of the config file for relative paths
  ConfigDir := ExtractFilePath(AConfigFile);

  // Only load if file exists
  if not FileExists(AConfigFile) then
  begin
    Configure(Config);
    Exit;
  end;
  
  // Load and parse configuration
  ConfigList := TStringList.Create;
  try
    ConfigList.LoadFromFile(AConfigFile);
    
    Section := '';
    for i := 0 to ConfigList.Count - 1 do
    begin
      if (ConfigList[i] = '') or (ConfigList[i][1] = ';') then
        Continue; // Skip empty lines and comments
        
      if (ConfigList[i][1] = '[') and (ConfigList[i][Length(ConfigList[i])] = ']') then
      begin
        // Section header
        Section := Copy(ConfigList[i], 2, Length(ConfigList[i]) - 2);
        Continue;
      end;
      
      if Pos('=', ConfigList[i]) > 0 then
      begin
        // Key=Value pair
        Key := Trim(Copy(ConfigList[i], 1, Pos('=', ConfigList[i]) - 1));
        Value := Trim(Copy(ConfigList[i], Pos('=', ConfigList[i]) + 1, Length(ConfigList[i])));
        
        if (Section = 'Logger') or (Section = '') then
        begin
          if SameText(Key, 'Destinations') then
          begin
            Config.Destinations := [];
            if Pos('CONSOLE', UpperCase(Value)) > 0 then
              Include(Config.Destinations, ldConsole);
            if Pos('FILE', UpperCase(Value)) > 0 then
              Include(Config.Destinations, ldFile);
          end
          else if SameText(Key, 'MinLevel') then
          begin
            if SameText(Value, 'Debug') then
              Config.MinLevel := llDebug
            else if SameText(Value, 'Info') then
              Config.MinLevel := llInfo
            else if SameText(Value, 'Warning') then
              Config.MinLevel := llWarning
            else if SameText(Value, 'Error') then
              Config.MinLevel := llError
            else if SameText(Value, 'Fatal') then
              Config.MinLevel := llFatal;
          end
          else if SameText(Key, 'DateTimeFormat') then
            Config.DateTimeFormat := Value
          else if SameText(Key, 'FormatPattern') then
            Config.FormatPattern := Value
          else if SameText(Key, 'DefaultLogFile') then
          begin
            // Handle relative paths - if no path specified, use config file directory
            if (Value <> '') and (ExtractFilePath(Value) = '') then
              Config.DefaultLogFile := ConfigDir + Value
            else
              Config.DefaultLogFile := Value;
          end
          else if SameText(Key, 'MaxFileSize') then
          begin
            try
              Config.MaxFileSize := StrToInt64(Value);
            except
              // Keep default if parse fails
            end;
          end;
        end;
      end;
    end;
  finally
    ConfigList.Free;
  end;
  
  Configure(Config);
end;

// Factory methods
class function TLogger.CreateConsoleLogger(AMinLevel: TLogLevel): TLogger;
begin
  ResetInstance;
  Result := GetInstance;
  Result.SetLogDestinations([ldConsole]).SetMinLogLevel(AMinLevel);
end;

class function TLogger.CreateFileLogger(const AFilename: string; AMinLevel: TLogLevel): TLogger;
begin
  ResetInstance;
  Result := GetInstance;
  Result.SetLogDestinations([ldFile]).SetMinLogLevel(AMinLevel);
  Result.AddLogFile(AFilename);
end;

class function TLogger.CreateConsoleAndFileLogger(const AFilename: string; AMinLevel: TLogLevel): TLogger;
begin
  ResetInstance;
  Result := GetInstance;
  Result.SetLogDestinations([ldConsole, ldFile]).SetMinLogLevel(AMinLevel);
  Result.AddLogFile(AFilename);
end;

class function TLogger.CreateDebugLogger: TLogger;
begin
  ResetInstance;
  Result := GetInstance;
  Result.SetLogDestinations([ldConsole, ldFile])
        .SetMinLogLevel(llDebug)
        .SetFormat('[%time] [%level] [%file:%line] %message');
  Result.AddDefaultLogFile('debug');
end;

class function TLogger.CreateAuditLogger(const AFilename: string): TLogger;
begin
  ResetInstance;
  Result := GetInstance;
  Result.SetLogDestinations([ldFile])
        .SetMinLogLevel(llInfo)
        .SetFormat('[%time] [AUDIT] %message');
  Result.AddLogFile(AFilename);
end;

// Streamlined format string overloads
procedure TLogger.Debug(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer);
begin
  Debug(Format(AFormat, AArgs), AFileIndex);
end;

procedure TLogger.Info(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer);
begin
  Info(Format(AFormat, AArgs), AFileIndex);
end;

procedure TLogger.Warning(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer);
begin
  Warning(Format(AFormat, AArgs), AFileIndex);
end;

procedure TLogger.Error(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer);
begin
  Error(Format(AFormat, AArgs), AFileIndex);
end;

procedure TLogger.Fatal(const AFormat: string; const AArgs: array of const; const AFileIndex: Integer);
begin
  Fatal(Format(AFormat, AArgs), AFileIndex);
end;

// Type-specific logging
procedure TLogger.LogValue(const AName: string; const AValue: Integer; ALevel: TLogLevel);
begin
  Log(Format('%s=%d', [AName, AValue]), ALevel);
end;

procedure TLogger.LogValue(const AName: string; const AValue: Boolean; ALevel: TLogLevel);
begin
  if AValue then
    Log(Format('%s=True', [AName]), ALevel)
  else
    Log(Format('%s=False', [AName]), ALevel);
end;

procedure TLogger.LogValue(const AName: string; const AValue: Double; ALevel: TLogLevel);
begin
  Log(Format('%s=%f', [AName, AValue]), ALevel);
end;

procedure TLogger.LogValue(const AName: string; const AValue: string; ALevel: TLogLevel);
begin
  Log(Format('%s=%s', [AName, AValue]), ALevel);
end;

// Structured logging
procedure TLogger.LogStructured(ALevel: TLogLevel; const AMessage: string; const AFields: array of TNameValuePair);
var
  i: Integer;
  StructuredMessage: string;
begin
  StructuredMessage := AMessage;
  
  for i := 0 to High(AFields) do
  begin
    StructuredMessage := StructuredMessage + ' | ';
    
    case AFields[i].ValueType of
      nvtString: StructuredMessage := StructuredMessage + Format('%s=%s', [AFields[i].Name, AFields[i].StringValue]);
      nvtInteger: StructuredMessage := StructuredMessage + Format('%s=%d', [AFields[i].Name, AFields[i].IntValue]);
      nvtBoolean: 
        if AFields[i].BoolValue then
          StructuredMessage := StructuredMessage + Format('%s=True', [AFields[i].Name])
        else
          StructuredMessage := StructuredMessage + Format('%s=False', [AFields[i].Name]);
      nvtFloat: StructuredMessage := StructuredMessage + Format('%s=%f', [AFields[i].Name, AFields[i].FloatValue]);
    end;
  end;
  
  Log(StructuredMessage, ALevel);
end;

// Batch logging
procedure TLogger.BeginBatch;
begin
  if not FBatchMode then
  begin
    FBatchMode := True;
    if Assigned(FBatchBuffer) then
      FBatchBuffer.Clear;
  end;
end;

procedure TLogger.EndBatch;
var
  i, j: Integer;
  Level: TLogLevel;
begin
  if FBatchMode and Assigned(FBatchBuffer) then
  begin
    FBatchMode := False;
    
    // Process all buffered messages
    for i := 0 to FBatchBuffer.Count - 1 do
    begin
      // Write to console if enabled
      if ldConsole in FLogDestinations then
      begin
        WriteLn(FBatchBuffer[i]);
      end;
      
      // Process through sinks
      if Assigned(FSinks) then
      begin
        // Since we don't store the log level with the message, use Info level for sinks
        Level := llInfo;
        for j := 0 to FSinks.Count - 1 do
        begin
          if Assigned(FSinks[j]) then
          begin
            try
              (FSinks[j] as ILogSink).Write(FBatchBuffer[i], Level);
            except
              // Ignore errors in sinks
            end;
          end;
        end;
      end;
    end;
    
    FBatchBuffer.Clear;
  end;
end;

// Timing functionality
function TLogger.TimedBlock(const AName: string): ITimedOperation;
begin
  Result := TTimedOperation.Create(Self, AName);
end;

{ TTimedOperation }

constructor TTimedOperation.Create(ALogger: TLogger; const AName: string);
begin
  inherited Create;
  FLogger := ALogger;
  FName := AName;
  FStartTime := Now;
  FActive := True;
  
  FLogger.Debug(Format('Starting: %s', [FName]));
end;

destructor TTimedOperation.Destroy;
var
  Duration: TDateTime;
  DurationMs: Int64;
begin
  if FActive then
  begin
    Duration := Now - FStartTime;
    DurationMs := Round(Duration * 24 * 60 * 60 * 1000); // Convert to milliseconds
    FLogger.Debug(Format('Completed: %s (Duration: %d ms)', [FName, DurationMs]));
    FActive := False;
  end;
  inherited Destroy;
end;

function TTimedOperation.GetName: string;
begin
  Result := FName;
end;

{ TConsoleSink }

procedure TConsoleSink.Write(const AFormattedMessage: string; ALevel: TLogLevel);
const
  {$IFDEF WINDOWS}
  // Windows console colors
  FOREGROUND_BLUE = 1;
  FOREGROUND_GREEN = 2;
  FOREGROUND_RED = 4;
  FOREGROUND_INTENSITY = 8;
  BACKGROUND_BLUE = 16;
  BACKGROUND_GREEN = 32;
  BACKGROUND_RED = 64;
  BACKGROUND_INTENSITY = 128;
  
  // Color combinations
  COLOR_GRAY = FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_RED;
  COLOR_WHITE = COLOR_GRAY or FOREGROUND_INTENSITY;
  COLOR_YELLOW = FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_INTENSITY;
  COLOR_RED = FOREGROUND_RED or FOREGROUND_INTENSITY;
  COLOR_FATAL = FOREGROUND_RED or FOREGROUND_BLUE or FOREGROUND_INTENSITY or 
                BACKGROUND_RED or BACKGROUND_INTENSITY;
  {$ENDIF}
begin
  {$IFDEF WINDOWS}
  case ALevel of
    llDebug: SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), COLOR_GRAY);
    llInfo: SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), COLOR_WHITE);
    llWarning: SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), COLOR_YELLOW);
    llError: SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), COLOR_RED);
    llFatal: SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), COLOR_FATAL);
  end;
  {$ENDIF}
  
  {$IFDEF UNIX}
  case ALevel of
    llDebug: Write(#27'[90m');    // Gray
    llInfo: Write(#27'[0m');      // Default
    llWarning: Write(#27'[33m');  // Yellow
    llError: Write(#27'[31m');    // Red
    llFatal: Write(#27'[97;41m'); // White on Red
  end;
  {$ENDIF}
  
  WriteLn(AFormattedMessage);
  
  {$IFDEF WINDOWS}
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), COLOR_WHITE); // Reset to default
  {$ENDIF}
  
  {$IFDEF UNIX}
  Write(#27'[0m'); // Reset to default
  {$ENDIF}
end;

procedure TConsoleSink.Flush;
begin
  // Console output is immediate, no need to flush
end;

{ TFileSink }

constructor TFileSink.Create(const AFilename: string; AMaxSize: Int64);
var
  Dir: string;
begin
  inherited Create;
  FFilename := AFilename;
  FMaxSize := AMaxSize;
  
  // Create directory if it doesn't exist
  Dir := ExtractFilePath(AFilename);
  if (Dir <> '') then
    ForceDirectories(Dir);
end;

procedure TFileSink.Write(const AFormattedMessage: string; ALevel: TLogLevel);
var
  F: TextFile;
  CurrentFileSize: Int64;
  BackupFilename: string;
  NeedRotation: Boolean;
  FileHandle: file;
begin
  NeedRotation := False;
  
  // Check if file exists and needs rotation
  if FileExists(FFilename) then
  begin
    AssignFile(FileHandle, FFilename);
    Reset(FileHandle, 1);
    try
      CurrentFileSize := FileSize(FileHandle);
      if CurrentFileSize >= FMaxSize then
        NeedRotation := True;
    finally
      CloseFile(FileHandle);
    end;
  end;
  
  // Rotate if needed
  if NeedRotation then
  begin
    BackupFilename := ChangeFileExt(FFilename, 
      FormatDateTime('_yyyymmdd_hhnnss', Now) + ExtractFileExt(FFilename));
    
    try
      if not RenameFile(FFilename, BackupFilename) then
      begin
        // If rename fails, just truncate the file
        AssignFile(F, FFilename);
        Rewrite(F);
        CloseFile(F);
      end;
    except
      // If rename operations fail, just truncate the file
      AssignFile(F, FFilename);
      Rewrite(F);
      CloseFile(F);
    end;
  end;
  
  // Write to file
  try
    AssignFile(F, FFilename);
    if FileExists(FFilename) then
      Append(F)
    else
      Rewrite(F);
      
    try
      WriteLn(F, AFormattedMessage);
    finally
      CloseFile(F);
    end;
  except
    on E: Exception do
    begin
      // If writing failed, try again after a short delay
      Sleep(100);
      try
        AssignFile(F, FFilename);
        if FileExists(FFilename) then
          Append(F)
        else
          Rewrite(F);
          
        WriteLn(F, AFormattedMessage);
        CloseFile(F);
      except
        // If it fails again, silently continue - don't crash the application
        // for logging errors
      end;
    end;
  end;
end;

procedure TFileSink.Flush;
begin
  // File operations are flushed immediately
end;

{ TRotatingFileSink }

constructor TRotatingFileSink.Create(const AFilename: string; AMaxSize: Int64; AMaxFiles: Integer);
begin
  inherited Create(AFilename, AMaxSize);
  FMaxFiles := AMaxFiles;
end;

procedure TRotatingFileSink.Write(const AFormattedMessage: string; ALevel: TLogLevel);
var
  SearchRec: TSearchRec;
  FileList: TStringList;
  BasePath, FileMask: string;
  i: Integer;
begin
  // First let the parent class handle the writing and basic rotation
  inherited Write(AFormattedMessage, ALevel);
  
  // Now check if we need to delete old files
  BasePath := ExtractFilePath(FFilename);
  FileMask := ChangeFileExt(ExtractFileName(FFilename), '_*' + ExtractFileExt(FFilename));
  
  if FindFirst(BasePath + FileMask, faAnyFile, SearchRec) = 0 then
  begin
    try
      FileList := TStringList.Create;
      try
        // Collect all backup files
        repeat
          FileList.Add(BasePath + SearchRec.Name);
        until FindNext(SearchRec) <> 0;
        
        // Sort by name (which includes the timestamp)
        FileList.Sort;
        
        // Delete oldest files if we have too many
        if FileList.Count > FMaxFiles then
        begin
          for i := 0 to FileList.Count - FMaxFiles - 1 do
          begin
            try
              SysUtils.DeleteFile(FileList[i]);
            except
              // Ignore errors when deleting old files
            end;
          end;
        end;
      finally
        FileList.Free;
      end;
    finally
      SysUtils.FindClose(SearchRec);
    end;
  end;
end;

{ TDailyFileSink }

constructor TDailyFileSink.Create(const ABaseFilename: string);
var
  Year, Month, Day: Word;
  Dir: string;
begin
  inherited Create;
  FBaseFilename := ABaseFilename;
  
  // Get current day
  DecodeDate(Date, Year, Month, Day);
  FCurrentDay := Day;
  
  // Set current filename
  FCurrentFilename := ChangeFileExt(FBaseFilename, 
    FormatDateTime('_yyyymmdd', Date) + ExtractFileExt(FBaseFilename));
    
  // Create directory if it doesn't exist
  Dir := ExtractFilePath(FBaseFilename);
  if (Dir <> '') then
    ForceDirectories(Dir);
end;

procedure TDailyFileSink.Write(const AFormattedMessage: string; ALevel: TLogLevel);
var
  F: TextFile;
  Year, Month, Day: Word;
begin
  // Check if day has changed
  DecodeDate(Date, Year, Month, Day);
  if Day <> FCurrentDay then
  begin
    FCurrentDay := Day;
    FCurrentFilename := ChangeFileExt(FBaseFilename, 
      FormatDateTime('_yyyymmdd', Date) + ExtractFileExt(FBaseFilename));
  end;
  
  // Write to current day's file
  try
    AssignFile(F, FCurrentFilename);
    if FileExists(FCurrentFilename) then
      Append(F)
    else
      Rewrite(F);
      
    try
      WriteLn(F, AFormattedMessage);
    finally
      CloseFile(F);
    end;
  except
    on E: Exception do
    begin
      // If writing failed, try again after a short delay
      Sleep(100);
      try
        AssignFile(F, FCurrentFilename);
        if FileExists(FCurrentFilename) then
          Append(F)
        else
          Rewrite(F);
          
        WriteLn(F, AFormattedMessage);
        CloseFile(F);
      except
        // If it fails again, silently continue
      end;
    end;
  end;
end;

procedure TDailyFileSink.Flush;
begin
  // File operations are flushed immediately
end;

{ TMemorySink }

constructor TMemorySink.Create(AMaxMessages: Integer);
begin
  inherited Create;
  FMaxMessages := AMaxMessages;
  FMessages := TStringList.Create;
end;

destructor TMemorySink.Destroy;
begin
  FMessages.Free;
  inherited Destroy;
end;

procedure TMemorySink.Write(const AFormattedMessage: string; ALevel: TLogLevel);
begin
  if Assigned(FMessages) then
  begin
    // Add message to list
    FMessages.Add(AFormattedMessage);
    
    // Trim if exceeding max messages
    while (FMessages.Count > FMaxMessages) and (FMessages.Count > 0) do
      FMessages.Delete(0);
  end;
end;

procedure TMemorySink.Flush;
begin
  // Memory sink doesn't need flushing
end;

function TMemorySink.GetMessages: TStringList;
begin
  Result := TStringList.Create;
  if Assigned(FMessages) then
    Result.Assign(FMessages);
end;

procedure TMemorySink.Clear;
begin
  FMessages.Clear;
end;

{ TCallbackSink }

constructor TCallbackSink.Create(ASimpleCallback: TSimpleLogCallback);
begin
  inherited Create;
  FSimpleCallback := ASimpleCallback;
  FMessageCallback := nil;
end;

constructor TCallbackSink.Create(AMessageCallback: TMessageLogCallback);
begin
  inherited Create;
  FSimpleCallback := nil;
  FMessageCallback := AMessageCallback;
end;

procedure TCallbackSink.Write(const AFormattedMessage: string; ALevel: TLogLevel);
begin
  if Assigned(FSimpleCallback) then
    FSimpleCallback
  else if Assigned(FMessageCallback) then
    FMessageCallback(AFormattedMessage, ALevel);
end;

procedure TCallbackSink.Flush;
begin
  // Nothing to flush
end;

initialization
  TLogger.FInstance := nil;
  TLogger.FNextInstanceID := 1;

finalization
  if Assigned(TLogger.FInstance) then
    TLogger.FInstance.Free;

end.
