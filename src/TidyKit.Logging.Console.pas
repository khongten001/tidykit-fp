unit TidyKit.Logging.Console;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, TidyKit.Logging;

type
  { Console destination for logging }
  TConsoleDestination = class(TLogDestination)
  private
    FUseColors: Boolean;
    function GetColorForLevel(ALevel: TLogLevel): Word;
    function FormatLogRecord(const ARecord: TLogRecord): string;
  protected
    procedure DoLog(const ARecord: TLogRecord); override;
  public
    constructor Create(AUseColors: Boolean = True);
  end;

implementation

uses
{$IFDEF WINDOWS}
  Windows;
{$ELSE}
  BaseUnix;
{$ENDIF}

const
  { ANSI color codes }
  ANSI_RESET = #27'[0m';
  ANSI_RED = #27'[31m';
  ANSI_GREEN = #27'[32m';
  ANSI_YELLOW = #27'[33m';
  ANSI_BLUE = #27'[34m';
  ANSI_MAGENTA = #27'[35m';

  { Windows console colors }
  WIN_BLACK = 0;
  WIN_BLUE = 1;
  WIN_GREEN = 2;
  WIN_CYAN = 3;
  WIN_RED = 4;
  WIN_MAGENTA = 5;
  WIN_YELLOW = 6;
  WIN_WHITE = 7;
  WIN_BRIGHT = 8;

constructor TConsoleDestination.Create(AUseColors: Boolean);
begin
  inherited Create;
  FUseColors := AUseColors;
end;

function TConsoleDestination.GetColorForLevel(ALevel: TLogLevel): Word;
begin
  case ALevel of
    llDebug: Result := WIN_CYAN;
    llInfo: Result := WIN_GREEN;
    llWarning: Result := WIN_YELLOW;
    llError: Result := WIN_RED;
    llFatal: Result := WIN_MAGENTA;
    else Result := WIN_WHITE;
  end;
end;

function TConsoleDestination.FormatLogRecord(const ARecord: TLogRecord): string;
const
  LevelNames: array[TLogLevel] of string = (
    'DEBUG', 'INFO', 'WARN', 'ERROR', 'FATAL'
  );
begin
  Result := Format('[%s] [%s] %s',
    [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', ARecord.Timestamp),
     LevelNames[ARecord.Level],
     ARecord.Message]);
     
  if (ARecord.ClassName <> '') or (ARecord.MethodName <> '') then
    Result := Result + Format(' [%s.%s:%d]',
      [ARecord.ClassName, ARecord.MethodName, ARecord.LineNumber]);
end;

procedure TConsoleDestination.DoLog(const ARecord: TLogRecord);
{$IFDEF WINDOWS}
var
  OldColor: Word;
  Handle: THandle;
  ConsoleInfo: TConsoleScreenBufferInfo;
{$ENDIF}
begin
  if FUseColors then
  begin
    {$IFDEF WINDOWS}
    Handle := GetStdHandle(STD_OUTPUT_HANDLE);
    OldColor := 0;
    if Handle <> INVALID_HANDLE_VALUE then
    begin
      GetConsoleScreenBufferInfo(Handle, ConsoleInfo);
      OldColor := ConsoleInfo.wAttributes;
      SetConsoleTextAttribute(Handle, GetColorForLevel(ARecord.Level));
    end;
    WriteLn(FormatLogRecord(ARecord));
    if Handle <> INVALID_HANDLE_VALUE then
      SetConsoleTextAttribute(Handle, OldColor);
    {$ELSE}
    case ARecord.Level of
      llDebug: Write(ANSI_BLUE);
      llInfo: Write(ANSI_GREEN);
      llWarning: Write(ANSI_YELLOW);
      llError: Write(ANSI_RED);
      llFatal: Write(ANSI_MAGENTA);
    end;
    WriteLn(FormatLogRecord(ARecord));
    Write(ANSI_RESET);
    {$ENDIF}
  end
  else
    WriteLn(FormatLogRecord(ARecord));
end;

end. 