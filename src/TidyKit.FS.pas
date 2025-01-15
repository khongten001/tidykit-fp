unit TidyKit.FS;

{$mode objfpc}{$H+}{$J-}

interface

uses
  {$IFDEF UNIX}
  BaseUnix,
  Unix,
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  Classes, SysUtils, DateUtils, TidyKit.Core;

type
  { TFileAttributes - Platform-independent file attributes record
    Provides a unified way to access file attributes across different operating systems.
    
    Fields:
    - ReadOnly: Whether the file is read-only
    - Hidden: Whether the file is hidden from normal directory listings
    - System: Whether the file is a system file
    - Directory: Whether the path points to a directory
    - Archive: Whether the file has been modified since last backup
    - SymLink: Whether the path points to a symbolic link
    - Owner: Owner of the file (more relevant on Unix systems)
    - Group: Group owner of the file (more relevant on Unix systems)
    - Permissions: Unix-style permissions string (e.g., 'rwxr-xr-x') }
  TFileAttributes = record
    ReadOnly: Boolean;
    Hidden: Boolean;
    System: Boolean;
    Directory: Boolean;
    Archive: Boolean;
    SymLink: Boolean;
    Owner: string;
    Group: string;
    Permissions: string;  // Unix-style permissions string
  end;

  { TSearchResult - Record containing detailed information about a found file
    Used by search operations to return comprehensive file information.
    
    Fields:
    - FileName: Name of the file without path
    - FullPath: Complete path to the file
    - Size: File size in bytes
    - LastModified: Last modification date/time
    - IsDirectory: Whether this is a directory
    - Attributes: Detailed file attributes }
  TSearchResult = record
    FileName: string;
    FullPath: string;
    Size: Int64;
    LastModified: TDateTime;
    IsDirectory: Boolean;
    Attributes: TFileAttributes;
  end;
  
  { Array of search results used by search operations }
  TSearchResults = array of TSearchResult;

  { TFileKit - Main class providing file system operations
    All methods are class methods (static) for functional programming style.
    Methods are grouped by functionality for easier navigation.
    All paths are handled in a platform-independent way. }
  TFileKit = class
  private
    { Creates a TSearchResult record for a given path with comprehensive file information }
    class function CreateSearchResult(const APath: string): TSearchResult; static;
  public
    { Basic file operations }
    
    { Reads entire file content as string.
      Returns file content as string.
      If file doesn't exist, raises EFileNotFoundException. }
    class function ReadFile(const APath: string): string; static;
    
    { Writes string content to file, creating path if needed.
      If write fails, raises EInOutError. }
    class procedure WriteFile(const APath: string; const AContent: string); static;
    
    { Appends string content to existing file or creates new if file doesn't exist. }
    class procedure AppendFile(const APath: string; const AContent: string); static;
    
    { Deletes file if it exists. }
    class procedure DeleteFile(const APath: string); static;
    
    { Copies file from source to destination.
      Creates destination directory if needed.
      If source doesn't exist, raises EFileNotFoundException. }
    class procedure CopyFile(const ASourcePath, ADestPath: string); static;
    
    { Moves file from source to destination.
      Tries rename first, falls back to copy+delete if needed.
      If source doesn't exist, raises EFileNotFoundException. }
    class procedure MoveFile(const ASourcePath, ADestPath: string); static;
    
    { Content manipulation operations }
    
    { Appends text to end of file. }
    class procedure AppendText(const APath, AText: string); static;
    
    { Prepends text to beginning of file. }
    class procedure PrependText(const APath, AText: string); static;
    
    { Replaces all occurrences of text in file. }
    class procedure ReplaceText(const APath, OldText, NewText: string); static;
    
    { Directory operations }
    
    { Creates directory and all parent directories. }
    class procedure CreateDirectory(const APath: string); static;
    
    { Deletes directory and optionally its contents.
      If Recursive is False, fails if directory is not empty. }
    class procedure DeleteDirectory(const APath: string; const Recursive: Boolean = True); static;
    
    { Ensures directory exists, creating it if needed. }
    class procedure EnsureDirectory(const APath: string); static;
    
    { Path operations }
    
    { Changes file extension.
      Returns path with new extension. }
    class function ChangeExtension(const APath, NewExt: string): string; static;
    
    { Gets file name from path.
      Returns file name with extension. }
    class function GetFileName(const APath: string): string; static;
    
    { Gets file name without extension.
      Returns file name without extension. }
    class function GetFileNameWithoutExt(const APath: string): string; static;
    
    { Gets directory name from path.
      Returns name of the directory. }
    class function GetDirectory(const APath: string): string; static;
    
    { Gets file extension including dot.
      Returns file extension with dot. }
    class function GetExtension(const APath: string): string; static;
    
    { File information }
    
    { Checks if file exists.
      Returns True if file exists. }
    class function Exists(const APath: string): Boolean; static;
    
    { Checks if directory exists.
      Returns True if directory exists. }
    class function DirectoryExists(const APath: string): Boolean; static;
    
    { Gets file size in bytes.
      Returns file size in bytes.
      If file doesn't exist, raises EFileNotFoundException. }
    class function GetSize(const APath: string): Int64; static;
    
    { Gets file creation time.
      Returns creation time or 0 if error.
      On Unix, returns last status change time. }
    class function GetCreationTime(const APath: string): TDateTime; static;
    
    { Gets file last access time.
      Returns last access time or 0 if error. }
    class function GetLastAccessTime(const APath: string): TDateTime; static;
    
    { Gets file last write time.
      Returns last write time or 0 if error. }
    class function GetLastWriteTime(const APath: string): TDateTime; static;
    
    { Gets detailed file attributes.
      Returns TFileAttributes record. }
    class function GetAttributes(const APath: string): TFileAttributes; static;
    
    { Checks if file appears to be text.
      Returns True if file appears to be text. }
    class function IsTextFile(const APath: string): Boolean; static;
    
    { Detects file encoding from BOM.
      Returns encoding name ('UTF-8', 'UTF-16LE', etc.) or 'ASCII' if no BOM. }
    class function GetFileEncoding(const APath: string): string; static;
    
    { Search operations }
    
    { Searches for files matching pattern.
      Returns array of matching files. }
    class function SearchFiles(const APath, APattern: string; const Recursive: Boolean = False): TSearchResults; static;
    
    { Internal search implementation.
      Returns array of matching files. }
    class function SearchFilesIn(const ADirectory, APattern: string; const Recursive: Boolean = False): TSearchResults; static;
    
    { Finds most recently modified file.
      Returns name of newest file or empty if none found. }
    class function FindLastModifiedFile(const APath, APattern: string; const Recursive: Boolean = False): string; static;
    
    { Finds oldest file.
      Returns name of oldest file or empty if none found. }
    class function FindFirstModifiedFile(const APath, APattern: string; const Recursive: Boolean = False): string; static;
    
    { Finds largest file.
      Returns name of largest file or empty if none found. }
    class function FindLargestFile(const APath, APattern: string; const Recursive: Boolean = False): string; static;
    
    { Finds smallest file.
      Returns name of smallest file or empty if none found. }
    class function FindSmallestFile(const APath, APattern: string; const Recursive: Boolean = False): string; static;
    
    { Directory information }
    
    { Gets user's home directory.
      Returns user's home directory path. }
    class function GetUserDir: string; static;
    
    { Gets current working directory.
      Returns current directory path. }
    class function GetCurrentDir: string; static;
    
    { Gets system temporary directory.
      Returns temporary directory path. }
    class function GetTempDir: string; static;
    
    { Gets parent directory of path.
      Returns parent directory path. }
    class function GetParentDir(const APath: string): string; static;
    
    { Path manipulation }
    
    { Combines two paths safely.
      Returns combined path. }
    class function CombinePaths(const APath1, APath2: string): string; static;
    
    { Checks if path is absolute.
      Returns True if path is absolute. }
    class function IsAbsolutePath(const APath: string): Boolean; static;
    
    { Normalizes path separators for current platform.
      Returns normalized path. }
    class function NormalizePath(const APath: string): string; static;
    
    { File system operations }
    
    { Creates temporary file.
      Returns path to new temporary file.
      If creation fails, raises ETidyKitException. }
    class function CreateTempFile(const APrefix: string = ''): string; static;
    
    { Creates temporary directory.
      Returns path to new temporary directory.
      If creation fails, raises ETidyKitException. }
    class function CreateTempDirectory(const APrefix: string = ''): string; static;
  end;

implementation

{ Platform-specific helper functions }

{ GetFileAttributes - Internal helper to get file attributes in a platform-independent way.
  Handles both Windows and Unix attribute systems, converting them to our unified format.
  
  Windows: Uses GetFileAttributes API to read standard Windows attributes
  Unix: Uses FpStat to read file mode and convert to equivalent attributes
  
  Returns TFileAttributes record with platform-independent attributes }
function GetFileAttributes(const APath: string): TFileAttributes;
{$IFDEF UNIX}
var
  Info: BaseUnix.Stat;
{$ENDIF}
{$IFDEF WINDOWS}
var
  Attrs: DWord;
{$ENDIF}
begin
  // Initialize record fields directly
  Result.ReadOnly := False;
  Result.Hidden := False;
  Result.System := False;
  Result.Directory := False;
  Result.Archive := False;
  Result.SymLink := False;
  Result.Owner := '';
  Result.Group := '';
  Result.Permissions := '';
  
  {$IFDEF UNIX}
  if FpStat(APath, Info) = 0 then
  begin
    Result.ReadOnly := (Info.Mode and S_IWUSR) = 0;
    Result.Directory := S_ISDIR(Info.Mode);
    Result.SymLink := S_ISLNK(Info.Mode);
    
    // Convert mode to string (e.g., 'rwxr-xr-x')
    Result.Permissions := '';
    Result.Permissions := Result.Permissions + IfThen((Info.Mode and S_IRUSR) <> 0, 'r', '-');
    Result.Permissions := Result.Permissions + IfThen((Info.Mode and S_IWUSR) <> 0, 'w', '-');
    Result.Permissions := Result.Permissions + IfThen((Info.Mode and S_IXUSR) <> 0, 'x', '-');
    Result.Permissions := Result.Permissions + IfThen((Info.Mode and S_IRGRP) <> 0, 'r', '-');
    Result.Permissions := Result.Permissions + IfThen((Info.Mode and S_IWGRP) <> 0, 'w', '-');
    Result.Permissions := Result.Permissions + IfThen((Info.Mode and S_IXGRP) <> 0, 'x', '-');
    Result.Permissions := Result.Permissions + IfThen((Info.Mode and S_IROTH) <> 0, 'r', '-');
    Result.Permissions := Result.Permissions + IfThen((Info.Mode and S_IWOTH) <> 0, 'w', '-');
    Result.Permissions := Result.Permissions + IfThen((Info.Mode and S_IXOTH) <> 0, 'x', '-');
  end;
  {$ENDIF}
  
  {$IFDEF WINDOWS}
  Attrs := Windows.GetFileAttributes(PChar(APath));
  if Attrs <> $FFFFFFFF then
  begin
    Result.ReadOnly := (Attrs and FILE_ATTRIBUTE_READONLY) <> 0;
    Result.Hidden := (Attrs and FILE_ATTRIBUTE_HIDDEN) <> 0;
    Result.System := (Attrs and FILE_ATTRIBUTE_SYSTEM) <> 0;
    Result.Directory := (Attrs and FILE_ATTRIBUTE_DIRECTORY) <> 0;
    Result.Archive := (Attrs and FILE_ATTRIBUTE_ARCHIVE) <> 0;
    Result.SymLink := (Attrs and FILE_ATTRIBUTE_REPARSE_POINT) <> 0;
  end;
  {$ENDIF}
end;

{ NormalizePath - Internal helper to normalize path separators
  Converts all separators to platform-specific format (\ for Windows, / for Unix)
  Also expands relative paths to absolute paths
  
  @param APath Path to normalize
  @returns Normalized absolute path }
function NormalizePath(const APath: string): string;
var
  TempPath: string;
begin
  TempPath := APath;
  {$IFDEF WINDOWS}
  TempPath := StringReplace(TempPath, '/', '\', [rfReplaceAll]);
  {$ELSE}
  TempPath := StringReplace(TempPath, '\', '/', [rfReplaceAll]);
  {$ENDIF}
  Result := ExpandFileName(TempPath);
  TempPath := '';
end;

{ FileTimeToDateTime - Internal helper to convert Windows FILETIME to TDateTime
  Handles the conversion through LocalFileTime and SystemTime structures
  
  @param FileTime Windows FILETIME structure
  @returns TDateTime value or 0 if conversion fails }
function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;
var
  LocalFileTime: TFileTime;
  SystemTime: TSystemTime;
begin
  Result := 0;
  LocalFileTime.dwLowDateTime := 0;
  LocalFileTime.dwHighDateTime := 0;
  SystemTime.wYear := 0;
  SystemTime.wMonth := 0;
  SystemTime.wDay := 0;
  SystemTime.wHour := 0;
  SystemTime.wMinute := 0;
  SystemTime.wSecond := 0;
  SystemTime.wMilliseconds := 0;
  
  if FileTimeToLocalFileTime(FileTime, LocalFileTime) and
     FileTimeToSystemTime(LocalFileTime, SystemTime) then
  begin
    Result := EncodeDate(SystemTime.wYear, SystemTime.wMonth, SystemTime.wDay) +
              EncodeTime(SystemTime.wHour, SystemTime.wMinute, SystemTime.wSecond, SystemTime.wMilliseconds);
  end;
end;

{ Helper functions }

{ LoadFromFile - Internal helper to read entire file into string
  Uses TFileStream and TStringStream for efficient reading
  Handles sharing for read access
  
  @param APath Path to the file to read
  @returns File contents as string or empty if file doesn't exist }
function LoadFromFile(const APath: string): string;
var
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  Result := '';
  if FileExists(APath) then
  begin
    FileStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
    try
      StringStream := TStringStream.Create('');
      try
        StringStream.CopyFrom(FileStream, FileStream.Size);
        Result := StringStream.DataString;
      finally
        StringStream.Free;
      end;
    finally
      FileStream.Free;
    end;
  end;
end;

{ SaveToFile - Internal helper to write string to file
  Creates all necessary directories in the path
  Uses TFileStream and TStringStream for efficient writing
  
  @param APath Path where to write
  @param AContent Content to write }
procedure SaveToFile(const APath: string; const AContent: string);
var
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  ForceDirectories(ExtractFilePath(APath));
  FileStream := TFileStream.Create(APath, fmCreate);
  try
    StringStream := TStringStream.Create(AContent);
    try
      FileStream.CopyFrom(StringStream, StringStream.Size);
    finally
      StringStream.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

{ TFileKit implementation }

{ CreateSearchResult - Creates detailed search result for a file
  Gathers all available information about the file including:
  - Basic properties (name, path)
  - Size and modification time
  - Detailed attributes
  
  @param APath Path to analyze
  @returns TSearchResult with comprehensive file information }
class function TFileKit.CreateSearchResult(const APath: string): TSearchResult;
var
  SearchRec: TSearchRec;
  Found: Boolean;
  NormalPath: string;
begin
  // Initialize record fields directly
  Result.FileName := '';
  Result.FullPath := '';
  Result.Size := 0;
  Result.LastModified := 0;
  Result.IsDirectory := False;
  with Result.Attributes do
  begin
    ReadOnly := False;
    Hidden := False;
    System := False;
    Directory := False;
    Archive := False;
    SymLink := False;
    Owner := '';
    Group := '';
    Permissions := '';
  end;
  
  NormalPath := NormalizePath(APath);
  Result.FullPath := NormalPath;
  Result.FileName := ExtractFileName(NormalPath);
  Result.IsDirectory := DirectoryExists(NormalPath);
  Result.Attributes := GetFileAttributes(NormalPath);
  
  WriteLn('CreateSearchResult: Processing file: ', NormalPath);
  
  Found := FindFirst(NormalPath, faAnyFile, SearchRec) = 0;
  try
    if Found then
    begin
      WriteLn('CreateSearchResult: Found file with size: ', SearchRec.Size);
      Result.Size := SearchRec.Size;
      Result.LastModified := FileDateToDateTime(SearchRec.Time);
    end
    else
    begin
      WriteLn('CreateSearchResult: Could not find file: ', NormalPath);
    end;
  finally
    if Found then
      FindClose(SearchRec);
  end;
  
  WriteLn('CreateSearchResult: Final result - Name: ', Result.FileName, ' Size: ', Result.Size);
  NormalPath := '';
end;

{ ReadFile - Reads entire file content
  Simple wrapper around LoadFromFile
  
  @param APath Path to the file
  @returns File content as string }
class function TFileKit.ReadFile(const APath: string): string;
begin
  Result := LoadFromFile(APath);
end;

{ WriteFile - Writes content to file
  Creates all necessary directories
  
  @param APath Target file path
  @param AContent Content to write }
class procedure TFileKit.WriteFile(const APath: string; const AContent: string);
begin
  if APath <> '' then
  begin
    ForceDirectories(ExtractFilePath(APath));
    SaveToFile(APath, AContent);
  end;
end;

{ AppendFile - Appends content to existing file
  If file doesn't exist, creates it
  If file exists, reads current content and appends new content
  
  @param APath Target file path
  @param AContent Content to append }
class procedure TFileKit.AppendFile(const APath: string; const AContent: string);
var
  ExistingContent: string;
begin
  if APath <> '' then
  begin
    if FileExists(APath) then
    begin
      ExistingContent := LoadFromFile(APath);
      SaveToFile(APath, ExistingContent + AContent);
    end
    else
      SaveToFile(APath, AContent);
  end;
end;

{ DeleteFile - Safely deletes file if it exists
  Uses SysUtils.DeleteFile for the actual deletion
  
  @param APath Path to the file to delete }
class procedure TFileKit.DeleteFile(const APath: string);
begin
  if FileExists(APath) then
    SysUtils.DeleteFile(APath);
end;

{ CopyFile - Copies file using streams
  Creates destination directory if needed
  Uses TFileStream for efficient copying
  
  @param ASourcePath Source file path
  @param ADestPath Destination file path }
class procedure TFileKit.CopyFile(const ASourcePath, ADestPath: string);
var
  SourceStream, DestStream: TFileStream;
begin
  if FileExists(ASourcePath) then
  begin
    ForceDirectories(ExtractFilePath(ADestPath));
    SourceStream := TFileStream.Create(ASourcePath, fmOpenRead or fmShareDenyWrite);
    try
      DestStream := TFileStream.Create(ADestPath, fmCreate);
      try
        DestStream.CopyFrom(SourceStream, SourceStream.Size);
      finally
        DestStream.Free;
      end;
    finally
      SourceStream.Free;
    end;
  end;
end;

{ MoveFile - Moves file, trying rename first
  If rename fails (e.g., across devices), falls back to copy+delete
  Creates destination directory if needed
  
  @param ASourcePath Source file path
  @param ADestPath Destination file path }
class procedure TFileKit.MoveFile(const ASourcePath, ADestPath: string);
var
  DestDir: string;
begin
  if FileExists(ASourcePath) then
  begin
    DestDir := ExtractFilePath(ADestPath);
    if DestDir <> '' then
      ForceDirectories(DestDir);
      
    if not RenameFile(ASourcePath, ADestPath) then
    begin
      CopyFile(ASourcePath, ADestPath);
      if FileExists(ADestPath) then
        SysUtils.DeleteFile(ASourcePath);
    end;
  end;
end;

{ SearchFiles - Main search function
  Determines actual search directory from input path
  Delegates to SearchFilesIn for implementation
  
  @param APath Base path (can be file or directory)
  @param APattern Search pattern
  @param Recursive Whether to search subdirectories
  @returns Array of found files }
class function TFileKit.SearchFiles(const APath, APattern: string; const Recursive: Boolean = False): TSearchResults;
var
  SearchDir: string;
  I: Integer;
begin
  SetLength(Result, 0);
  
  WriteLn('SearchFiles: Starting search');
  WriteLn('SearchFiles: Input path = ', APath);
  WriteLn('SearchFiles: Pattern = ', APattern);
  WriteLn('SearchFiles: Recursive = ', Recursive);
  
  if DirectoryExists(APath) then
  begin
    SearchDir := APath;
    WriteLn('SearchFiles: Using directory path = ', SearchDir);
  end
  else if APath <> '' then
  begin
    SearchDir := ExtractFilePath(ExpandFileName(APath));
    WriteLn('SearchFiles: Using extracted path = ', SearchDir);
  end
  else
  begin
    SearchDir := GetCurrentDir;
    WriteLn('SearchFiles: Using current directory = ', SearchDir);
  end;
    
  Result := SearchFilesIn(SearchDir, APattern, Recursive);
  WriteLn('SearchFiles: Found ', Length(Result), ' files');
  
  // Debug output for found files
  for I := 0 to High(Result) do
  begin
    WriteLn('SearchFiles: File[', I, '] = ', Result[I].FileName);
    WriteLn('SearchFiles: Path[', I, '] = ', Result[I].FullPath);
    WriteLn('SearchFiles: Size[', I, '] = ', Result[I].Size);
  end;
end;

{ SearchFilesIn - Internal search implementation
  Uses recursive helper function to handle directory traversal
  Maintains visited directory list to prevent cycles
  
  @param ADirectory Directory to search
  @param APattern Search pattern
  @param Recursive Whether to search subdirectories
  @returns Array of found files }
class function TFileKit.SearchFilesIn(const ADirectory, APattern: string; const Recursive: Boolean = False): TSearchResults;
  function DoSearch(const RawDir, Pat: string; Rec: Boolean; Visited: TStrings): TSearchResults;
  var
    NDir: string;
    SearchRec: TSearchRec;
    SubDirs: TStringList;
    SubResults: TSearchResults;
    FullPath: string;
    I, J, OldLen: Integer;
    Found: Boolean;
  begin
    SetLength(Result, 0);
    NDir := NormalizePath(RawDir);
    
    WriteLn('SearchFilesIn: Searching in directory: ', NDir);
    WriteLn('SearchFilesIn: Pattern: ', Pat);
    WriteLn('SearchFilesIn: Recursive: ', Rec);
    
    if Visited.IndexOf(NDir) >= 0 then
    begin
      WriteLn('SearchFilesIn: Directory already visited: ', NDir);
      Exit;
    end;
    Visited.Add(NDir);

    if not DirectoryExists(NDir) then
    begin
      WriteLn('SearchFilesIn: Directory does not exist: ', NDir);
      Exit;
    end;
    
    SubDirs := TStringList.Create;
    try
      // First find all matching files in current directory
      Found := FindFirst(IncludeTrailingPathDelimiter(NDir) + Pat, faAnyFile, SearchRec) = 0;
      try
        while Found do
        begin
          if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
          begin
            FullPath := IncludeTrailingPathDelimiter(NDir) + SearchRec.Name;
            if (SearchRec.Attr and faDirectory) = 0 then
            begin
              WriteLn('SearchFilesIn: Found file: ', SearchRec.Name, ' in ', NDir);
              WriteLn('SearchFilesIn: File size: ', SearchRec.Size);
              OldLen := Length(Result);
              SetLength(Result, OldLen + 1);
              Result[OldLen] := CreateSearchResult(FullPath);
              WriteLn('SearchFilesIn: Added file with size: ', Result[OldLen].Size);
            end
            else if Rec then
            begin
              WriteLn('SearchFilesIn: Found subdirectory: ', SearchRec.Name);
              SubDirs.Add(FullPath);
            end;
          end;
          Found := FindNext(SearchRec) = 0;
        end;
      finally
        FindClose(SearchRec);
      end;

      // Then find all subdirectories if recursive
      if Rec then
      begin
        Found := FindFirst(IncludeTrailingPathDelimiter(NDir) + '*', faDirectory, SearchRec) = 0;
        try
          while Found do
          begin
            if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and 
               ((SearchRec.Attr and faDirectory) <> 0) and
               ((SearchRec.Attr and faSymLink) = 0) then
            begin
              FullPath := IncludeTrailingPathDelimiter(NDir) + SearchRec.Name;
              WriteLn('SearchFilesIn: Found directory for recursive search: ', SearchRec.Name);
              SubDirs.Add(FullPath);
            end;
            Found := FindNext(SearchRec) = 0;
          end;
        finally
          FindClose(SearchRec);
        end;

        // Process subdirectories
        WriteLn('SearchFilesIn: Processing ', SubDirs.Count, ' subdirectories');
        for I := 0 to SubDirs.Count - 1 do
        begin
          WriteLn('SearchFilesIn: Recursing into subdirectory: ', SubDirs[I]);
          SubResults := DoSearch(SubDirs[I], Pat, True, Visited);
          if Length(SubResults) > 0 then
          begin
            WriteLn('SearchFilesIn: Found ', Length(SubResults), ' files in subdirectory');
            OldLen := Length(Result);
            SetLength(Result, OldLen + Length(SubResults));
            for J := 0 to High(SubResults) do
            begin
              Result[OldLen + J] := SubResults[J];
              WriteLn('SearchFilesIn: Added file from subdirectory: ', Result[OldLen + J].FileName, ' Size: ', Result[OldLen + J].Size);
            end;
          end;
          SetLength(SubResults, 0);
        end;
      end;
    finally
      SubDirs.Free;
      NDir := '';
      FullPath := '';
    end;
  end;

var
  VisitedDirs: TStringList;
  I: Integer;
begin
  SetLength(Result, 0);
  VisitedDirs := TStringList.Create;
  try
    VisitedDirs.Sorted := True;
    VisitedDirs.Duplicates := dupIgnore;
    Result := DoSearch(ADirectory, APattern, Recursive, VisitedDirs);
    WriteLn('SearchFilesIn: Total files found: ', Length(Result));
    for I := 0 to High(Result) do
    begin
      WriteLn('SearchFilesIn: Final file[', I, '] = ', Result[I].FileName);
      WriteLn('SearchFilesIn: Final path[', I, '] = ', Result[I].FullPath);
      WriteLn('SearchFilesIn: Final size[', I, '] = ', Result[I].Size);
    end;
  finally
    VisitedDirs.Free;
  end;
end;

{ FindSmallestFile - Finds file with smallest size
  Handles both recursive and non-recursive search
  For equal sizes, uses alphabetical order as tiebreaker
  
  @param APath Base path to search
  @param APattern Search pattern
  @param Recursive Whether to search subdirectories
  @returns Name of smallest file or empty if none found }
class function TFileKit.FindSmallestFile(const APath, APattern: string; const Recursive: Boolean = False): string;
var
  Files: TSearchResults;
  I: Integer;
  SmallestSize: Int64;
  SearchRec: TSearchRec;
  FullPath: string;
begin
  Result := '';
  SmallestSize := High(Int64);
  
  WriteLn('FindSmallestFile: Starting search in ', APath, ' with pattern ', APattern);
  WriteLn('FindSmallestFile: Recursive = ', Recursive);
  
  if not Recursive then
  begin
    // Non-recursive search
    if FindFirst(IncludeTrailingPathDelimiter(APath) + APattern, faAnyFile, SearchRec) = 0 then
    try
      repeat
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and 
           ((SearchRec.Attr and faDirectory) = 0) then
        begin
          FullPath := IncludeTrailingPathDelimiter(APath) + SearchRec.Name;
          WriteLn('FindSmallestFile: Checking file: ', SearchRec.Name, ' Size: ', GetSize(FullPath));
          if (Result = '') or (GetSize(FullPath) < SmallestSize) then
          begin
            Result := SearchRec.Name;
            SmallestSize := GetSize(FullPath);
            WriteLn('FindSmallestFile: New smallest file found: ', Result, ' Size: ', SmallestSize);
          end;
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end
  else
  begin
    // Recursive search using SearchFiles
    Files := SearchFiles(APath, APattern, True);
    try
      WriteLn('FindSmallestFile: Found ', Length(Files), ' files in recursive search');
      for I := 0 to High(Files) do
      begin
        if not Files[I].IsDirectory then
        begin
          WriteLn('FindSmallestFile: Checking file (recursive): ', Files[I].FileName, 
                 ' Path: ', Files[I].FullPath,
                 ' Size: ', Files[I].Size);
          if (Result = '') or (Files[I].Size < SmallestSize) then
          begin
            Result := ExtractFileName(Files[I].FullPath);
            SmallestSize := Files[I].Size;
            WriteLn('FindSmallestFile: New smallest file found: ', Result, ' Size: ', SmallestSize);
          end
          else if (Files[I].Size = SmallestSize) and (ExtractFileName(Files[I].FullPath) < Result) then
          begin
            // If sizes are equal, use alphabetical order
            Result := ExtractFileName(Files[I].FullPath);
            WriteLn('FindSmallestFile: Found equal size file, using alphabetical order: ', Result);
          end;
        end;
      end;
    finally
      SetLength(Files, 0);
    end;
  end;
  
  WriteLn('FindSmallestFile: Final result = ', Result, ' with size = ', SmallestSize);
end;

{ IsTextFile - Checks if file appears to be text
  Reads first 512 bytes and checks for binary characters
  A file is considered text if it contains only:
  - ASCII control chars (0-31) that are valid in text (CR, LF, tab, etc.)
  - Printable ASCII chars (32-127)
  - Extended ASCII chars (128-255)
  
  @param APath Path to the file
  @returns True if file appears to be text }
class function TFileKit.IsTextFile(const APath: string): Boolean;
const
  MaxBytesToCheck = 512;
var
  F: file;
  Buffer: array[0..MaxBytesToCheck-1] of Byte;
  BytesRead: Integer;
  I: Integer;
begin
  Result := False;
  if not FileExists(APath) then
    Exit;
    
  AssignFile(F, APath);
  try
    Reset(F, 1);  // Open in binary mode
    BlockRead(F, Buffer, MaxBytesToCheck, BytesRead);
    
    // Check for binary characters
    for I := 0 to BytesRead - 1 do
      if (Buffer[I] < 7) or ((Buffer[I] > 14) and (Buffer[I] < 32) and (Buffer[I] <> 27)) then
        Exit;
        
    Result := True;
  finally
    CloseFile(F);
  end;
end;

{ GetFileEncoding - Detects file encoding from BOM
  Reads first 4 bytes to check for Unicode BOMs:
  - EF BB BF: UTF-8
  - FE FF: UTF-16BE
  - FF FE: UTF-16LE
  - 00 00 FE FF: UTF-32BE
  - FF FE 00 00: UTF-32LE
  
  @param APath Path to the file
  @returns Encoding name or 'ASCII' if no BOM found }
class function TFileKit.GetFileEncoding(const APath: string): string;
const
  MaxBytesToCheck = 4;
var
  F: file;
  Buffer: array[0..MaxBytesToCheck-1] of Byte;
  BytesRead: Integer;
begin
  Result := 'ASCII';  // Default
  
  if not FileExists(APath) then
    Exit;
    
  AssignFile(F, APath);
  try
    Reset(F, 1);  // Open in binary mode
    BlockRead(F, Buffer, MaxBytesToCheck, BytesRead);
    
    if BytesRead >= 2 then
    begin
      // Check BOM
      if (BytesRead >= 3) and (Buffer[0] = $EF) and (Buffer[1] = $BB) and (Buffer[2] = $BF) then
        Result := 'UTF-8'
      else if (Buffer[0] = $FE) and (Buffer[1] = $FF) then
        Result := 'UTF-16BE'
      else if (Buffer[0] = $FF) and (Buffer[1] = $FE) then
        Result := 'UTF-16LE'
      else if (BytesRead >= 4) and (Buffer[0] = 0) and (Buffer[1] = 0) and 
              (Buffer[2] = $FE) and (Buffer[3] = $FF) then
        Result := 'UTF-32BE'
      else if (BytesRead >= 4) and (Buffer[0] = $FF) and (Buffer[1] = $FE) and 
              (Buffer[2] = 0) and (Buffer[3] = 0) then
        Result := 'UTF-32LE';
    end;
  finally
    CloseFile(F);
  end;
end;

end. 
