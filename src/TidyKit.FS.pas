unit TidyKit.FS;

{$mode objfpc}{$H+}{$J-}

interface

uses
  {$IFDEF UNIX}
  BaseUnix,  // Unix-specific system calls
  Unix,      // Unix-specific types and constants
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows,   // Windows-specific API
  {$ENDIF}
  Classes, SysUtils, DateUtils, TidyKit.Core;

type
  { TFileAttributes provides a platform-independent way to access file attributes.
    This record normalizes the differences between Windows and Unix file systems. }
  TFileAttributes = record
    ReadOnly: Boolean;    // True if file cannot be modified
    Hidden: Boolean;      // True if file is hidden from normal listings
    System: Boolean;      // True if file is a system file
    Directory: Boolean;   // True if path points to a directory
    Archive: Boolean;     // True if file has been modified since last backup
    SymLink: Boolean;     // True if path is a symbolic link
    Owner: string;        // Name/ID of file owner (more relevant on Unix)
    Group: string;        // Name/ID of file group (more relevant on Unix)
    Permissions: string;  // Unix-style permissions string (e.g. 'rwxr-xr--')
  end;

  { Dynamic array of strings, used for file listings }
  TStringArray = array of string;

  { TFileSortOrder defines how files/directories should be sorted in listings.
    This provides flexible sorting options for directory contents. }
  TFileSortOrder = (
    fsNone,           // No sorting (default)
    fsName,           // Sort by name
    fsNameDesc,       // Sort by name descending
    fsDate,           // Sort by modification date
    fsDateDesc,       // Sort by modification date descending
    fsSize,           // Sort by size
    fsSizeDesc       // Sort by size descending
  );

  { TSearchResult contains detailed information about a file or directory.
    Used by search operations to return comprehensive file information. }
  TSearchResult = record
    FileName: string;      // Name of the file without path
    FullPath: string;      // Complete path to the file
    Size: Int64;          // File size in bytes
    LastModified: TDateTime; // Last modification timestamp
    IsDirectory: Boolean;  // True if item is a directory
    Attributes: TFileAttributes; // Detailed file attributes
  end;
  
  { Array of search results for operations that return multiple files }
  TSearchResults = array of TSearchResult;

  { TFileKit provides a comprehensive set of file system operations.
    All operations are static (class functions) for ease of use and memory safety.
    The kit handles platform-specific differences internally. }
  TFileKit = class
  private
    { Creates a TSearchResult record for a given path }
    class function CreateSearchResult(const APath: string): TSearchResult; static;
  public
    { Basic file operations }
    class function ReadFile(const APath: string): string; static;
    class procedure WriteFile(const APath: string; const AContent: string); static;
    class procedure AppendFile(const APath: string; const AContent: string); static;
    class procedure DeleteFile(const APath: string); static;
    class procedure CopyFile(const ASourcePath, ADestPath: string); static;
    class procedure MoveFile(const ASourcePath, ADestPath: string); static;
    
    { Text content manipulation }
    class procedure AppendText(const APath, AText: string); static;
    class procedure PrependText(const APath, AText: string); static;
    class procedure ReplaceText(const APath, OldText, NewText: string); static;
    
    { Directory management }
    class procedure CreateDirectory(const APath: string); static;
    class procedure DeleteDirectory(const APath: string; const Recursive: Boolean = True); static;
    class procedure EnsureDirectory(const APath: string); static;
    class function ListDirectories(const APath: string; 
      const Pattern: string = '*'; 
      const Recursive: Boolean = False;
      const SortOrder: TFileSortOrder = fsNone): TStringArray; static;
    class function ListFiles(const APath: string; 
      const Pattern: string = '*'; 
      const Recursive: Boolean = False;
      const SortOrder: TFileSortOrder = fsNone): TStringArray; static;
    
    { Path manipulation and information }
    class function ChangeExtension(const APath, NewExt: string): string; static;
    class function GetFileName(const APath: string): string; static;
    class function GetFileNameWithoutExt(const APath: string): string; static;
    class function GetDirectory(const APath: string): string; static;
    class function GetExtension(const APath: string): string; static;
    
    { File information and attributes }
    class function Exists(const APath: string): Boolean; static;
    class function DirectoryExists(const APath: string): Boolean; static;
    class function GetSize(const APath: string): Int64; static;
    class function GetCreationTime(const APath: string): TDateTime; static;
    class function GetLastAccessTime(const APath: string): TDateTime; static;
    class function GetLastWriteTime(const APath: string): TDateTime; static;
    class function GetAttributes(const APath: string): TFileAttributes; static;
    class function IsTextFile(const APath: string): Boolean; static;
    class function GetFileEncoding(const APath: string): string; static;
    
    { File search and filtering }
    class function SearchFiles(const APath, APattern: string; const Recursive: Boolean = False): TSearchResults; static;
    class function SearchFilesIn(const ADirectory, APattern: string; const Recursive: Boolean = False): TSearchResults; static;
    class function FindLastModifiedFile(const APath, APattern: string; const Recursive: Boolean = False): string; static;
    class function FindFirstModifiedFile(const APath, APattern: string; const Recursive: Boolean = False): string; static;
    class function FindLargestFile(const APath, APattern: string; const Recursive: Boolean = False): string; static;
    class function FindSmallestFile(const APath, APattern: string; const Recursive: Boolean = False): string; static;
    
    { Standard directory paths }
    class function GetUserDir: string; static;
    class function GetCurrentDir: string; static;
    class function GetTempDir: string; static;
    class function GetParentDir(const APath: string): string; static;
    
    { Path utilities }
    class function CombinePaths(const APath1, APath2: string): string; static;
    class function IsAbsolutePath(const APath: string): Boolean; static;
    class function NormalizePath(const APath: string): string; static;
    
    { Temporary file operations }
    class function CreateTempFile(const APrefix: string = ''): string; static;
    class function CreateTempDirectory(const APrefix: string = ''): string; static;
  end;

implementation

{ Forward declarations }
function CompareByDate(List: TStringList; Index1, Index2: Integer): Integer; forward;
function CompareBySize(List: TStringList; Index1, Index2: Integer): Integer; forward;

{ Platform-specific helper functions }

// Add comparison functions for sorting
function CompareByDate(List: TStringList; Index1, Index2: Integer): Integer;
var
  Time1, Time2: TDateTime;
begin
  Time1 := TFileKit.GetLastWriteTime(List[Index1]);
  Time2 := TFileKit.GetLastWriteTime(List[Index2]);
  Result := CompareDateTime(Time1, Time2);
end;

function CompareBySize(List: TStringList; Index1, Index2: Integer): Integer;
var
  Size1, Size2: Int64;
begin
  Size1 := TFileKit.GetSize(List[Index1]);
  Size2 := TFileKit.GetSize(List[Index2]);
  if Size1 < Size2 then
    Result := -1
  else if Size1 > Size2 then
    Result := 1
  else
    Result := 0;
end;

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

{ TFileKit }

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

class function TFileKit.ReadFile(const APath: string): string;
begin
  Result := LoadFromFile(APath);
end;

class procedure TFileKit.WriteFile(const APath: string; const AContent: string);
begin
  if APath <> '' then
  begin
    ForceDirectories(ExtractFilePath(APath));
    SaveToFile(APath, AContent);
  end;
end;

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

class procedure TFileKit.DeleteFile(const APath: string);
begin
  if FileExists(APath) then
    SysUtils.DeleteFile(APath);
end;

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

class procedure TFileKit.AppendText(const APath, AText: string);
var
  Content: string;
begin
  if APath <> '' then
  begin
    if FileExists(APath) then
    begin
      Content := LoadFromFile(APath);
      SaveToFile(APath, Content + AText);
    end
    else
      SaveToFile(APath, AText);
  end;
end;

class procedure TFileKit.PrependText(const APath, AText: string);
var
  Content: string;
begin
  if APath <> '' then
  begin
    if FileExists(APath) then
    begin
      Content := LoadFromFile(APath);
      SaveToFile(APath, AText + Content);
    end
    else
      SaveToFile(APath, AText);
  end;
end;

class procedure TFileKit.ReplaceText(const APath, OldText, NewText: string);
var
  Content: string;
begin
  if APath <> '' then
  begin
    if FileExists(APath) then
    begin
      Content := LoadFromFile(APath);
      Content := StringReplace(Content, OldText, NewText, [rfReplaceAll]);
      SaveToFile(APath, Content);
    end;
  end;
end;

class procedure TFileKit.CreateDirectory(const APath: string);
begin
  if not SysUtils.DirectoryExists(APath) then
    ForceDirectories(APath);
end;

class procedure TFileKit.DeleteDirectory(const APath: string; const Recursive: Boolean = True);
var
  SearchRec: TSearchRec;
  FullPath: string;
begin
  if DirectoryExists(APath) then
  begin
    if Recursive then
    begin
      if FindFirst(IncludeTrailingPathDelimiter(APath) + '*', faAnyFile, SearchRec) = 0 then
      begin
        try
          repeat
            if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
            begin
              FullPath := IncludeTrailingPathDelimiter(APath) + SearchRec.Name;
              if (SearchRec.Attr and faDirectory) <> 0 then
                DeleteDirectory(FullPath, True)
              else
                SysUtils.DeleteFile(FullPath);
            end;
          until FindNext(SearchRec) <> 0;
        finally
          FindClose(SearchRec);
        end;
      end;
    end;
    RemoveDir(APath);
  end;
end;

class procedure TFileKit.EnsureDirectory(const APath: string);
begin
  ForceDirectories(ExtractFilePath(APath));
end;

class function TFileKit.ChangeExtension(const APath, NewExt: string): string;
begin
  Result := ChangeFileExt(APath, NewExt);
end;

class function TFileKit.GetFileName(const APath: string): string;
begin
  Result := ExtractFileName(APath);
end;

class function TFileKit.GetFileNameWithoutExt(const APath: string): string;
begin
  Result := ChangeFileExt(ExtractFileName(APath), '');
end;

class function TFileKit.GetDirectory(const APath: string): string;
begin
  if DirectoryExists(APath) then
    Result := ExtractFileName(ExcludeTrailingPathDelimiter(APath))
  else if APath <> '' then
    Result := ExtractFileName(ExcludeTrailingPathDelimiter(ExtractFilePath(ExpandFileName(APath))))
  else
    Result := ExtractFileName(ExcludeTrailingPathDelimiter(GetCurrentDir));
end;

class function TFileKit.GetExtension(const APath: string): string;
begin
  Result := ExtractFileExt(APath);
end;

class function TFileKit.Exists(const APath: string): Boolean;
begin
  Result := FileExists(APath);
end;

class function TFileKit.DirectoryExists(const APath: string): Boolean;
begin
  Result := SysUtils.DirectoryExists(APath);
end;

class function TFileKit.GetSize(const APath: string): Int64;
var
  SearchRec: TSearchRec;
  Found: Boolean;
  NormalPath: string;
begin
  Result := 0;
  if APath = '' then
    Exit;
    
  NormalPath := NormalizePath(APath);
  WriteLn('GetSize: Getting size for file: ', NormalPath);
  
  Found := FindFirst(NormalPath, faAnyFile, SearchRec) = 0;
  try
    if Found then
    begin
      Result := SearchRec.Size;
      WriteLn('GetSize: Found file with size: ', Result);
    end
    else
    begin
      WriteLn('GetSize: Could not find file: ', NormalPath);
    end;
  finally
    if Found then
      FindClose(SearchRec);
  end;
  
  NormalPath := '';
end;

class function TFileKit.GetCreationTime(const APath: string): TDateTime;
{$IFDEF WINDOWS}
var
  Handle: THandle;
  FindData: TWin32FindData;
{$ENDIF}
begin
  Result := 0;
  {$IFDEF WINDOWS}
  FillChar(FindData, SizeOf(FindData), 0);
  Handle := FindFirstFile(PChar(APath), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    Result := FileTimeToDateTime(FindData.ftCreationTime);
  end;
  {$ENDIF}
  {$IFDEF UNIX}
  Result := FileDateToDateTime(FileAge(APath));
  {$ENDIF}
end;

class function TFileKit.GetLastAccessTime(const APath: string): TDateTime;
{$IFDEF WINDOWS}
var
  Handle: THandle;
  FindData: TWin32FindData;
{$ENDIF}
begin
  Result := 0;
  {$IFDEF WINDOWS}
  FillChar(FindData, SizeOf(FindData), 0);
  Handle := FindFirstFile(PChar(APath), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    Result := FileTimeToDateTime(FindData.ftLastAccessTime);
  end;
  {$ENDIF}
  {$IFDEF UNIX}
  Result := FileDateToDateTime(FileAge(APath));
  {$ENDIF}
end;

class function TFileKit.GetLastWriteTime(const APath: string): TDateTime;
{$IFDEF WINDOWS}
var
  Handle: THandle;
  FindData: TWin32FindData;
{$ENDIF}
begin
  Result := 0;
  {$IFDEF WINDOWS}
  FillChar(FindData, SizeOf(FindData), 0);
  Handle := FindFirstFile(PChar(APath), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    Result := FileTimeToDateTime(FindData.ftLastWriteTime);
  end;
  {$ENDIF}
  {$IFDEF UNIX}
  Result := FileDateToDateTime(FileAge(APath));
  {$ENDIF}
end;

class function TFileKit.GetAttributes(const APath: string): TFileAttributes;
begin
  Result := GetFileAttributes(APath);
end;

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

class function TFileKit.FindLastModifiedFile(const APath, APattern: string; const Recursive: Boolean = False): string;
var
  Files: TSearchResults;
  I: Integer;
  NewestTime: TDateTime;
  SearchRec: TSearchRec;
begin
  Result := '';
  NewestTime := 0;
  
  if not Recursive then
  begin
    // Non-recursive search
    if FindFirst(IncludeTrailingPathDelimiter(APath) + APattern, faAnyFile, SearchRec) = 0 then
    try
      repeat
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and 
           ((SearchRec.Attr and faDirectory) = 0) then
        begin
          WriteLn('Checking file: ', SearchRec.Name, ' Time: ', DateTimeToStr(FileDateToDateTime(SearchRec.Time)));
          if (Result = '') or (FileDateToDateTime(SearchRec.Time) > NewestTime) then
          begin
            Result := SearchRec.Name;
            NewestTime := FileDateToDateTime(SearchRec.Time);
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
      for I := 0 to High(Files) do
      begin
        if not Files[I].IsDirectory then
        begin
          WriteLn('Checking file (recursive): ', Files[I].FileName, ' Time: ', DateTimeToStr(Files[I].LastModified));
          if (Result = '') or (Files[I].LastModified > NewestTime) then
          begin
            Result := ExtractFileName(Files[I].FullPath);
            NewestTime := Files[I].LastModified;
          end;
        end;
      end;
    finally
      SetLength(Files, 0);
    end;
  end;
end;

class function TFileKit.FindFirstModifiedFile(const APath, APattern: string; const Recursive: Boolean = False): string;
var
  Files: TSearchResults;
  I: Integer;
  OldestTime: TDateTime;
  SearchRec: TSearchRec;
begin
  Result := '';
  OldestTime := MaxDateTime;
  
  if not Recursive then
  begin
    // Non-recursive search
    if FindFirst(IncludeTrailingPathDelimiter(APath) + APattern, faAnyFile, SearchRec) = 0 then
    try
      repeat
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and 
           ((SearchRec.Attr and faDirectory) = 0) then
        begin
          WriteLn('Checking file: ', SearchRec.Name, ' Time: ', DateTimeToStr(FileDateToDateTime(SearchRec.Time)));
          if (Result = '') or (FileDateToDateTime(SearchRec.Time) < OldestTime) then
          begin
            Result := SearchRec.Name;
            OldestTime := FileDateToDateTime(SearchRec.Time);
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
      for I := 0 to High(Files) do
      begin
        if not Files[I].IsDirectory then
        begin
          WriteLn('Checking file (recursive): ', Files[I].FileName, ' Time: ', DateTimeToStr(Files[I].LastModified));
          if (Result = '') or (Files[I].LastModified < OldestTime) then
          begin
            Result := ExtractFileName(Files[I].FullPath);
            OldestTime := Files[I].LastModified;
          end;
        end;
      end;
    finally
      SetLength(Files, 0);
    end;
  end;
end;

class function TFileKit.FindLargestFile(const APath, APattern: string; const Recursive: Boolean = False): string;
var
  Files: TSearchResults;
  I: Integer;
  LargestSize: Int64;
  SearchRec: TSearchRec;
  FullPath: string;
begin
  Result := '';
  LargestSize := -1;
  
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
          WriteLn('Checking file: ', SearchRec.Name, ' Size: ', GetSize(FullPath));
          if (Result = '') or (GetSize(FullPath) > LargestSize) then
          begin
            Result := SearchRec.Name;
            LargestSize := GetSize(FullPath);
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
      for I := 0 to High(Files) do
      begin
        if not Files[I].IsDirectory then
        begin
          WriteLn('Checking file (recursive): ', Files[I].FileName, ' Size: ', Files[I].Size);
          if (Result = '') or (Files[I].Size > LargestSize) then
          begin
            Result := ExtractFileName(Files[I].FullPath);
            LargestSize := Files[I].Size;
          end;
        end;
      end;
    finally
      SetLength(Files, 0);
    end;
  end;
end;

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

class function TFileKit.GetUserDir: string;
begin
  {$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('USERPROFILE');
  {$ELSE}
  Result := GetEnvironmentVariable('HOME');
  {$ENDIF}
  if Result = '' then
    Result := GetCurrentDir;
end;

class function TFileKit.GetCurrentDir: string;
begin
  Result := SysUtils.GetCurrentDir;
end;

class function TFileKit.GetTempDir: string;
begin
  Result := SysUtils.GetTempDir;
end;

class function TFileKit.GetParentDir(const APath: string): string;
begin
  Result := ExtractFileDir(ExcludeTrailingPathDelimiter(ExpandFileName(APath)));
end;

class function TFileKit.CombinePaths(const APath1, APath2: string): string;
begin
  if APath1 = '' then
    Result := APath2
  else if APath2 = '' then
    Result := APath1
  else
    Result := IncludeTrailingPathDelimiter(APath1) + ExcludeTrailingPathDelimiter(APath2);
    
  // Only normalize if both paths are non-empty
  if (APath1 <> '') and (APath2 <> '') then
    Result := NormalizePath(Result);
end;

class function TFileKit.IsAbsolutePath(const APath: string): Boolean;
begin
  {$IFDEF WINDOWS}
  Result := (Length(APath) >= 2) and
            (APath[1] in ['A'..'Z', 'a'..'z']) and
            (APath[2] = ':');
  {$ELSE}
  Result := (Length(APath) > 0) and (APath[1] = '/');
  {$ENDIF}
end;

class function TFileKit.NormalizePath(const APath: string): string;
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
end;

class function TFileKit.CreateTempFile(const APrefix: string = ''): string;
var
  TempPath: string;
  GUID: TGUID;
  GuidStr: string;
begin
  TempPath := GetTempDir;
  if CreateGUID(GUID) = 0 then
  begin
    GuidStr := GUIDToString(GUID);
    if APrefix <> '' then
      Result := CombinePaths(TempPath, APrefix + '_' + GuidStr + '.tmp')
    else
      Result := CombinePaths(TempPath, 'tmp_' + GuidStr + '.tmp');
    WriteFile(Result, ''); // Create empty file
  end
  else
    raise ETidyKitException.Create('Failed to create GUID for temporary file');
end;

class function TFileKit.CreateTempDirectory(const APrefix: string = ''): string;
var
  TempPath: string;
  GUID: TGUID;
  GuidStr: string;
begin
  TempPath := GetTempDir;
  if CreateGUID(GUID) = 0 then
  begin
    GuidStr := GUIDToString(GUID);
    if APrefix <> '' then
      Result := CombinePaths(TempPath, APrefix + '_' + GuidStr)
    else
      Result := CombinePaths(TempPath, 'tmp_' + GuidStr);
    CreateDirectory(Result);
  end
  else
    raise ETidyKitException.Create('Failed to create GUID for temporary directory');
end;

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

class function TFileKit.ListDirectories(const APath: string; 
  const Pattern: string = '*'; 
  const Recursive: Boolean = False;
  const SortOrder: TFileSortOrder = fsNone): TStringArray;
var
  SearchRec: TSearchRec;
  DirList: TStringList;
  SubDirs: TStringArray;
  NormalizedPath: string;
  FullPath: string;
  I: Integer;
begin
  DirList := TStringList.Create;
  try
    NormalizedPath := IncludeTrailingPathDelimiter(NormalizePath(APath));
    
    // First find directories matching the pattern
    if FindFirst(NormalizedPath + Pattern, faDirectory, SearchRec) = 0 then
    try
      repeat
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and 
           ((SearchRec.Attr and faDirectory) <> 0) and
           ((SearchRec.Attr and faSymLink) = 0) then
        begin
          FullPath := NormalizedPath + SearchRec.Name;
          DirList.Add(FullPath);
          
          if Recursive then
          begin
            SubDirs := ListDirectories(FullPath, Pattern, True, SortOrder);
            for I := 0 to High(SubDirs) do
              DirList.Add(SubDirs[I]);
            SetLength(SubDirs, 0); // Free the array
          end;
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
    
    // Apply sorting if requested
    case SortOrder of
      fsName: DirList.Sort;
      fsNameDesc: begin
        DirList.Sort;
        for I := 0 to (DirList.Count div 2) - 1 do
          DirList.Exchange(I, DirList.Count - 1 - I);
      end;
      fsDate: DirList.CustomSort(@CompareByDate);
      fsDateDesc: begin
        DirList.CustomSort(@CompareByDate);
        for I := 0 to (DirList.Count div 2) - 1 do
          DirList.Exchange(I, DirList.Count - 1 - I);
      end;
      // Size sorting not applicable for directories
    end;
    
    SetLength(Result, DirList.Count);
    for I := 0 to DirList.Count - 1 do
      Result[I] := DirList[I];
  finally
    DirList.Free;
  end;
end;

class function TFileKit.ListFiles(const APath: string; 
  const Pattern: string = '*'; 
  const Recursive: Boolean = False;
  const SortOrder: TFileSortOrder = fsNone): TStringArray;
var
  SearchRec: TSearchRec;
  FileList: TStringList;
  SubDirs: TStringArray;
  SubFiles: TStringArray;
  NormalizedPath: string;
  FullPath: string;
  I: Integer;
begin
  FileList := TStringList.Create;
  try
    NormalizedPath := IncludeTrailingPathDelimiter(NormalizePath(APath));
    
    // First find all files in current directory matching the pattern
    if FindFirst(NormalizedPath + Pattern, faAnyFile - faDirectory, SearchRec) = 0 then
    try
      repeat
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and
           ((SearchRec.Attr and faDirectory) = 0) then
        begin
          FullPath := NormalizedPath + SearchRec.Name;
          FileList.Add(FullPath);
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
    
    // Then find subdirectories if recursive
    if Recursive then
    begin
      if FindFirst(NormalizedPath + '*', faDirectory, SearchRec) = 0 then
      try
        repeat
          if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and
             ((SearchRec.Attr and faDirectory) <> 0) and
             ((SearchRec.Attr and faSymLink) = 0) then
          begin
            FullPath := NormalizedPath + SearchRec.Name;
            SubFiles := ListFiles(FullPath, Pattern, True, SortOrder);
            for I := 0 to High(SubFiles) do
              FileList.Add(SubFiles[I]);
            SetLength(SubFiles, 0); // Free the array
          end;
        until FindNext(SearchRec) <> 0;
      finally
        FindClose(SearchRec);
      end;
    end;
    
    // Apply sorting if requested
    case SortOrder of
      fsName: FileList.Sort;
      fsNameDesc: begin
        FileList.Sort;
        for I := 0 to (FileList.Count div 2) - 1 do
          FileList.Exchange(I, FileList.Count - 1 - I);
      end;
      fsDate: FileList.CustomSort(@CompareByDate);
      fsDateDesc: begin
        FileList.CustomSort(@CompareByDate);
        for I := 0 to (FileList.Count div 2) - 1 do
          FileList.Exchange(I, FileList.Count - 1 - I);
      end;
      fsSize: FileList.CustomSort(@CompareBySize);
      fsSizeDesc: begin
        FileList.CustomSort(@CompareBySize);
        for I := 0 to (FileList.Count div 2) - 1 do
          FileList.Exchange(I, FileList.Count - 1 - I);
      end;
    end;
    
    SetLength(Result, FileList.Count);
    for I := 0 to FileList.Count - 1 do
      Result[I] := FileList[I];
  finally
    FileList.Free;
  end;
end;

end. 
