{$mode objfpc}{$H+}
unit TidyKit.Log.Targets;

interface

uses
  Classes, SysUtils, TidyKit.Log, TidyKit.Core;

type
  { File target implementation }
  TFileTarget = class(TInterfacedObject, ILogTarget)
  private
    FFilename: string;
    FMaxSize: Integer;
    FRotateCount: Integer;
    FFile: TextFile;
    FCurrentSize: Int64;
    FIsOpen: Boolean;
    procedure EnsureFileOpen;
    procedure CheckRotation;
    procedure RotateFiles;
    function GetFileSize(const AFileName: string): Int64;
  public
    constructor Create(const AFilename: string);
    destructor Destroy; override;
    
    { ILogTarget implementation }
    function GetName: string;
    procedure WriteLog(const AEntry: TLogEntry);
    procedure Flush;
    
    { Configuration methods }
    function SetMaxSize(ASize: Integer): TFileTarget;
    function SetRotateCount(ACount: Integer): TFileTarget;
  end;

  { Console target implementation }
  TConsoleTarget = class(TInterfacedObject, ILogTarget)
  private
    FUseColors: Boolean;
    procedure WriteWithColor(const AText: string; AColor: Word);
  public
    constructor Create;
    
    { ILogTarget implementation }
    function GetName: string;
    procedure WriteLog(const AEntry: TLogEntry);
    procedure Flush;
    
    { Configuration methods }
    function EnableColors: TConsoleTarget;
    function DisableColors: TConsoleTarget;
  end;

implementation

uses
  {$IFDEF WINDOWS}Windows{$ELSE}termio{$ENDIF}, TypInfo;

const
  DEFAULT_MAX_SIZE = 10 * 1024 * 1024;  { 10 MB }
  DEFAULT_ROTATE_COUNT = 5;

{ TFileTarget implementation }

constructor TFileTarget.Create(const AFilename: string);
begin
  inherited Create;
  FFilename := AFilename;
  FMaxSize := DEFAULT_MAX_SIZE;
  FRotateCount := DEFAULT_ROTATE_COUNT;
  FIsOpen := False;
  FCurrentSize := 0;
end;

destructor TFileTarget.Destroy;
begin
  if FIsOpen then
    CloseFile(FFile);
  inherited;
end;

function TFileTarget.GetFileSize(const AFileName: string): Int64;
var
  F: file;
begin
  AssignFile(F, AFileName);
  try
    Reset(F, 1);  // Open with record size 1 (byte)
    Result := FileSize(F);
    CloseFile(F);
  except
    Result := 0;
  end;
end;

procedure TFileTarget.EnsureFileOpen;
begin
  if not FIsOpen then
  begin
    AssignFile(FFile, FFilename);
    try
      if FileExists(FFilename) then
      begin
        Append(FFile);
        FCurrentSize := GetFileSize(FFilename);
      end
      else
      begin
        ForceDirectories(ExtractFilePath(FFilename));  { Create directory if needed }
        Rewrite(FFile);
        FCurrentSize := 0;
      end;
      FIsOpen := True;
    except
      on E: Exception do
      begin
        FIsOpen := False;
        raise ETidyKitException.CreateFmt('Failed to open log file "%s": %s', [FFilename, E.Message]);
      end;
    end;
  end;
end;

procedure TFileTarget.CheckRotation;
begin
  if (FMaxSize > 0) and (FCurrentSize >= FMaxSize) then
    RotateFiles;
end;

procedure TFileTarget.RotateFiles;
var
  I: Integer;
  OldName, NewName: string;
begin
  if FIsOpen then
  begin
    CloseFile(FFile);
    FIsOpen := False;
  end;

  for I := FRotateCount - 1 downto 1 do
  begin
    OldName := Format('%s.%d', [FFilename, I]);
    NewName := Format('%s.%d', [FFilename, I + 1]);
    if FileExists(OldName) then
      RenameFile(OldName, NewName);
  end;

  if FileExists(FFilename) then
    RenameFile(FFilename, FFilename + '.1');

  FCurrentSize := 0;
end;

function TFileTarget.GetName: string;
begin
  Result := 'File';
end;

procedure TFileTarget.WriteLog(const AEntry: TLogEntry);
var
  LogLine: string;
begin
  EnsureFileOpen;
  CheckRotation;

  LogLine := Format('[%s] [%s] [%d] %s',
    [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', AEntry.TimeStamp),
     GetEnumName(TypeInfo(TLogLevel), Ord(AEntry.Level)),
     AEntry.ThreadID,
     AEntry.Message]);

  WriteLn(FFile, LogLine);
  Inc(FCurrentSize, Length(LogLine) + 2);  { +2 for line ending }
  Flush;  { Ensure the line is written immediately }
end;

procedure TFileTarget.Flush;
begin
  if FIsOpen then
    System.Flush(FFile);
end;

function TFileTarget.SetMaxSize(ASize: Integer): TFileTarget;
begin
  FMaxSize := ASize;
  Result := Self;
end;

function TFileTarget.SetRotateCount(ACount: Integer): TFileTarget;
begin
  FRotateCount := ACount;
  Result := Self;
end;

{ TConsoleTarget implementation }

const
  {$IFDEF WINDOWS}
  COLOR_DEBUG   = FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_INTENSITY;
  COLOR_INFO    = FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE;
  COLOR_WARNING = FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_INTENSITY;
  COLOR_ERROR   = FOREGROUND_RED or FOREGROUND_INTENSITY;
  COLOR_FATAL   = FOREGROUND_RED or FOREGROUND_BLUE or FOREGROUND_INTENSITY;
  {$ELSE}
  COLOR_DEBUG   = 36;  { Cyan }
  COLOR_INFO    = 37;  { White }
  COLOR_WARNING = 33;  { Yellow }
  COLOR_ERROR   = 31;  { Red }
  COLOR_FATAL   = 35;  { Magenta }
  {$ENDIF}

constructor TConsoleTarget.Create;
begin
  inherited Create;
  FUseColors := True;
end;

procedure TConsoleTarget.WriteWithColor(const AText: string; AColor: Word);
{$IFDEF WINDOWS}
var
  Handle: THandle;
  ConsoleInfo: TConsoleScreenBufferInfo;
  OldAttrs: Word;
begin
  if FUseColors then
  begin
    Handle := GetStdHandle(STD_OUTPUT_HANDLE);
    if GetConsoleScreenBufferInfo(Handle, ConsoleInfo) then
    begin
      OldAttrs := ConsoleInfo.wAttributes;
      SetConsoleTextAttribute(Handle, AColor);
      Write(AText);
      SetConsoleTextAttribute(Handle, OldAttrs);
    end
    else
      Write(AText);  { Fallback if can't get console info }
  end
  else
    Write(AText);
end;
{$ELSE}
begin
  if FUseColors then
    Write(Format(#27'[%dm%s'#27'[0m', [AColor, AText]))
  else
    Write(AText);
end;
{$ENDIF}

function TConsoleTarget.GetName: string;
begin
  Result := 'Console';
end;

procedure TConsoleTarget.WriteLog(const AEntry: TLogEntry);
var
  LogLine: string;
  Color: Word;
begin
  LogLine := Format('[%s] [%s] [%d] %s'#13#10,
    [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', AEntry.TimeStamp),
     GetEnumName(TypeInfo(TLogLevel), Ord(AEntry.Level)),
     AEntry.ThreadID,
     AEntry.Message]);

  case AEntry.Level of
    llDebug:   Color := COLOR_DEBUG;
    llInfo:    Color := COLOR_INFO;
    llWarning: Color := COLOR_WARNING;
    llError:   Color := COLOR_ERROR;
    llFatal:   Color := COLOR_FATAL;
  end;

  WriteWithColor(LogLine, Color);
end;

procedure TConsoleTarget.Flush;
begin
  System.Flush(Output);
end;

function TConsoleTarget.EnableColors: TConsoleTarget;
begin
  FUseColors := True;
  Result := Self;
end;

function TConsoleTarget.DisableColors: TConsoleTarget;
begin
  FUseColors := False;
  Result := Self;
end;

end. 
