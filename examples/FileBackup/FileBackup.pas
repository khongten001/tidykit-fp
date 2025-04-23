program FileBackup;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, DateUtils,
  TidyKit.Core, TidyKit.FS;

const
  VERSION = '1.0.0';

type
  TBackupOptions = record
    SourceDir: string;
    DestDir: string;
    FilePatterns: TStringArray;
    ExcludePatterns: TStringArray;
    LogFile: string;
    Recursive: Boolean;
    Verbose: Boolean;
    Simulate: Boolean;
  end;

var
  Options: TBackupOptions;
  FileKit: IFileKit;
  StartTime: TDateTime;
  
{ Prints help information }
procedure ShowHelp;
begin
  WriteLn('File Backup Utility v', VERSION);
  WriteLn('Usage: FileBackup [options]');
  WriteLn('');
  WriteLn('Options:');
  WriteLn('  -s, --source=DIR    Source directory (required)');
  WriteLn('  -d, --dest=DIR      Destination directory (required)');
  WriteLn('  -p, --pattern=GLOB  File pattern to include (can be used multiple times)');
  WriteLn('  -e, --exclude=GLOB  File pattern to exclude (can be used multiple times)');
  WriteLn('  -l, --log=FILE      Log file path');
  WriteLn('  -r, --recursive     Recursively process subdirectories');
  WriteLn('  -v, --verbose       Display detailed information');
  WriteLn('  --simulate          Simulate backup without copying files');
  WriteLn('  -h, --help          Display this help message');
  WriteLn('');
  WriteLn('Example:');
  WriteLn('  FileBackup --source=c:\documents --dest=d:\backup --pattern=*.docx --pattern=*.xlsx --recursive');
end;

{ Parse command line parameters }
function ParseCommandLine: Boolean;
var
  i: Integer;
  Param, Value: string;
  EqualPos: Integer;
begin
  Result := True;
  
  // Set default values
  Options.SourceDir := '';
  Options.DestDir := '';
  Options.LogFile := '';
  Options.Recursive := False;
  Options.Verbose := False;
  Options.Simulate := False;
  SetLength(Options.FilePatterns, 0);
  SetLength(Options.ExcludePatterns, 0);
  
  // Parse command line parameters
  i := 1;
  while i <= ParamCount do
  begin
    Param := ParamStr(i);
    Inc(i);
    
    // Check for help
    if (Param = '-h') or (Param = '--help') then
    begin
      ShowHelp;
      Exit(False);
    end
    
    // Check for parameters with values
    else if (Param.StartsWith('-s=') or Param.StartsWith('--source=')) then
    begin
      EqualPos := Pos('=', Param);
      Options.SourceDir := Copy(Param, EqualPos + 1, Length(Param));
    end
    else if (Param.StartsWith('-d=') or Param.StartsWith('--dest=')) then
    begin
      EqualPos := Pos('=', Param);
      Options.DestDir := Copy(Param, EqualPos + 1, Length(Param));
    end
    else if (Param.StartsWith('-p=') or Param.StartsWith('--pattern=')) then
    begin
      EqualPos := Pos('=', Param);
      Value := Copy(Param, EqualPos + 1, Length(Param));
      
      SetLength(Options.FilePatterns, Length(Options.FilePatterns) + 1);
      Options.FilePatterns[High(Options.FilePatterns)] := Value;
    end
    else if (Param.StartsWith('-e=') or Param.StartsWith('--exclude=')) then
    begin
      EqualPos := Pos('=', Param);
      Value := Copy(Param, EqualPos + 1, Length(Param));
      
      SetLength(Options.ExcludePatterns, Length(Options.ExcludePatterns) + 1);
      Options.ExcludePatterns[High(Options.ExcludePatterns)] := Value;
    end
    else if (Param.StartsWith('-l=') or Param.StartsWith('--log=')) then
    begin
      EqualPos := Pos('=', Param);
      Options.LogFile := Copy(Param, EqualPos + 1, Length(Param));
    end
    else if (Param = '-r') or (Param = '--recursive') then
      Options.Recursive := True
    else if (Param = '-v') or (Param = '--verbose') then
      Options.Verbose := True
    else if (Param = '--simulate') then
      Options.Simulate := True
    else
    begin
      WriteLn('Unknown parameter: ', Param);
      ShowHelp;
      Exit(False);
    end;
  end;
  
  // Check required parameters
  if Options.SourceDir = '' then
  begin
    WriteLn('Error: Source directory not specified.');
    ShowHelp;
    Exit(False);
  end;
  
  if Options.DestDir = '' then
  begin
    WriteLn('Error: Destination directory not specified.');
    ShowHelp;
    Exit(False);
  end;
  
  // If no file patterns specified, include all files
  if Length(Options.FilePatterns) = 0 then
  begin
    SetLength(Options.FilePatterns, 1);
    Options.FilePatterns[0] := '*';
  end;
  
  // Create a default log file if not specified
  if Options.LogFile = '' then
  begin
    Options.LogFile := FileKit.CombinePaths(Options.DestDir, 
                         'backup_log_' + FormatDateTime('yyyy-mm-dd_hh-nn-ss', Now) + '.log');
  end;
end;

{ Logs a message to the console and optionally to a log file }
procedure LogMessage(const Msg: string; ToFile: Boolean = True);
begin
  WriteLn(Msg);
  
  if ToFile and (Options.LogFile <> '') then
    FileKit.AppendText(Options.LogFile, Msg + sLineBreak);
end;

{ Checks if a file should be excluded based on patterns }
function ShouldExclude(const FilePath: string): Boolean;
var
  FileName: string;
  Pattern: string;
begin
  Result := False;
  FileName := ExtractFileName(FilePath);
  
  for Pattern in Options.ExcludePatterns do
  begin
    if FileKit.MatchesPattern(FileName, Pattern) then
      Exit(True);
  end;
end;

{ Performs the actual backup }
procedure PerformBackup;
var
  FileCount, DirCount: Integer;
  TotalSize: Int64;
  BackupStartTime: TDateTime;
  Files: TSearchResults;
  SearchResult: TSearchResult; // Renamed variable 'File' to 'SearchResult'
  TargetPath, RelativePath: string;
  Pattern: string;
  ElapsedTime: TDateTime;
  BackupDirName: string;
begin
  FileCount := 0;
  DirCount := 0;
  TotalSize := 0;
  BackupStartTime := Now;
  
  // Create the destination directory with timestamp
  BackupDirName := FileKit.CombinePaths(Options.DestDir, 
                     'backup_' + FormatDateTime('yyyy-mm-dd_hh-nn-ss', Now));
  
  if not Options.Simulate then
    FileKit.CreateDirectory(BackupDirName);
  
  // Initialize log file
  if (Options.LogFile <> '') and not Options.Simulate then
  begin
    FileKit.WriteTextFile(Options.LogFile, 
      'Backup started at: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', BackupStartTime) + sLineBreak + 
      'Source: ' + Options.SourceDir + sLineBreak +
      'Destination: ' + BackupDirName + sLineBreak +
      'Patterns: ' + string.Join(', ', Options.FilePatterns) + sLineBreak +
      'Exclude: ' + string.Join(', ', Options.ExcludePatterns) + sLineBreak +
      'Recursive: ' + BoolToStr(Options.Recursive, 'Yes', 'No') + sLineBreak +
      '---------------------------------------------------' + sLineBreak);
  end;
  
  LogMessage('Starting backup from ' + Options.SourceDir + ' to ' + BackupDirName);
  
  // Process each pattern
  for Pattern in Options.FilePatterns do
  begin
    LogMessage('Processing pattern: ' + Pattern, False);
    
    // Find all files matching the pattern
    Files := FileKit.SearchFiles(Options.SourceDir, Pattern, Options.Recursive);
    
    // Process each file
    for SearchResult in Files do // Use renamed variable
    begin
      // Skip directories
      if SearchResult.IsDirectory then // Use renamed variable
      begin
        Inc(DirCount);
        continue;
      end;
      
      // Check if file should be excluded
      if ShouldExclude(SearchResult.FullPath) then // Use renamed variable
      begin
        if Options.Verbose then
          LogMessage('Excluding: ' + SearchResult.FullPath); // Use renamed variable
        Continue;
      end;
      
      // Get relative path for preserving directory structure
      RelativePath := FileKit.GetRelativePath(Options.SourceDir, SearchResult.FullPath); // Use renamed variable
      if RelativePath = '.' then  // File is directly in source dir
        RelativePath := SearchResult.FileName; // Use renamed variable
        
      TargetPath := FileKit.CombinePaths(BackupDirName, RelativePath);
      
      if Options.Verbose then
        LogMessage('Copying: ' + SearchResult.FullPath + ' -> ' + TargetPath); // Use renamed variable
      
      // Copy the file unless we're in simulation mode
      if not Options.Simulate then
      begin
        FileKit.EnsureDirectory(ExtractFilePath(TargetPath));
        FileKit.CopyFile(SearchResult.FullPath, TargetPath); // Use renamed variable
      end;
      
      Inc(FileCount);
      Inc(TotalSize, SearchResult.Size); // Use renamed variable
    end;
  end;
  
  // Log summary
  ElapsedTime := Now - BackupStartTime;
  
  LogMessage('');
  LogMessage('Backup completed in ' + FormatDateTime('hh:nn:ss', ElapsedTime));
  LogMessage('Files processed: ' + IntToStr(FileCount));
  LogMessage('Directories processed: ' + IntToStr(DirCount));
  LogMessage('Total size: ' + IntToStr(TotalSize) + ' bytes (' + 
             Format('%.2f MB', [TotalSize / (1024*1024)]) + ')');
              
  if Options.Simulate then
    LogMessage('Note: This was a simulation. No files were actually copied.');
end;

begin
  try
    // Create a FileKit instance
    FileKit := TFSFactory.CreateFileKit;
    
    // Parse command line parameters
    if not ParseCommandLine then
      Exit;
      
    // Validate source directory
    if not FileKit.DirectoryExists(Options.SourceDir) then
    begin
      WriteLn('Error: Source directory does not exist: ', Options.SourceDir);
      Exit;
    end;
    
    StartTime := Now;
    
    // Perform the backup
    PerformBackup;
    
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
