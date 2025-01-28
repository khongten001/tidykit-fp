unit TidyKit.FS;

{$mode objfpc}{$H+}{$J-}

interface

uses
  {$IFDEF UNIX}
  BaseUnix,  // Provides Unix-specific system calls for low-level file operations
  Unix,      // Defines Unix-specific types and constants
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows,   // Windows API units for interacting with the Windows operating system
  {$ENDIF}
  Classes, SysUtils, DateUtils, TidyKit.Core, zipper, libtar, StrUtils;

const
  DEBUG_MODE = False; // Enable debugging output
  
  {$IFDEF WINDOWS}
  SYMBOLIC_LINK_FLAG_DIRECTORY = $1;                  // Windows API flag: target is a directory
  SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE = $2;  // Windows API flag: allow non-admin symlink creation
  FILE_FLAG_OPEN_REPARSE_POINT = $00200000;           // Windows API flag: open symlink itself, not target
  FILE_NAME_NORMALIZED = $0;                          // Windows API flag: get normalized path without . or .. components
  {$ENDIF}
  
  {$IFDEF UNIX}
  PATH_MAX = 4096;  // Maximum length of a file path on Unix/Linux systems (includes null terminator)
  {$ENDIF}

{$IFDEF WINDOWS}
// Windows API function declarations
function CreateSymbolicLink(lpSymlinkFileName, lpTargetFileName: LPCSTR; dwFlags: DWORD): BOOL; stdcall; external 'kernel32.dll' name 'CreateSymbolicLinkA';
function GetFinalPathNameByHandle(hFile: THandle; lpszFilePath: LPSTR; cchFilePath: DWORD; dwFlags: DWORD): DWORD; stdcall; external 'kernel32.dll' name 'GetFinalPathNameByHandleA';
{$ENDIF}

type
  { 
    TFileAttributes 
    ----------------
    This record offers a unified interface to access file attributes across different 
    operating systems. It abstracts away the platform-specific differences, providing
    a consistent way to retrieve and manipulate file properties.
  }
  TFileAttributes = record
    ReadOnly: Boolean;    // Indicates if the file is read-only (cannot be modified)
    Hidden: Boolean;      // Indicates if the file is hidden from standard directory listings
    System: Boolean;      // Indicates if the file is a system file, typically used by the OS
    Directory: Boolean;   // Indicates if the path points to a directory rather than a file
    Archive: Boolean;     // Indicates if the file has been modified since the last backup
    SymLink: Boolean;     // Indicates if the path is a symbolic link pointing to another file/directory
    Owner: string;        // Represents the name or ID of the file owner (primarily relevant on Unix systems)
    Group: string;        // Represents the name or ID of the file group (primarily relevant on Unix systems)
    Permissions: string;  // Contains Unix-style permissions (e.g., 'rwxr-xr--') indicating access rights
  end;

  { 
    TFilePathArray
    -------------
    A dynamic array of strings used to store lists of filenames or directory names.
    This type is commonly used for returning results from file listing operations.
  }
  TFilePathArray = array of string;

  { 
    TFileSortOrder 
    ---------------
    Enumerates the different ways files and directories can be sorted when listed.
    This allows developers to specify the desired sorting criteria for better organization.
  }
  TFileSortOrder = (
    fsNone,           // No specific sorting; the default order as retrieved
    fsName,           // Sort items alphabetically by their name in ascending order
    fsNameDesc,       // Sort items alphabetically by their name in descending order
    fsDate,           // Sort items based on their last modification date in ascending order
    fsDateDesc,       // Sort items based on their last modification date in descending order
    fsSize,           // Sort items by their size in ascending order
    fsSizeDesc        // Sort items by their size in descending order
  );

  { 
    TSearchResult 
    -------------
    Holds comprehensive information about a single file or directory. This record is 
    used to return detailed search results, providing insights into each item's properties.
  }
  TSearchResult = record
    FileName: string;        // The name of the file or directory without the path
    FullPath: string;        // The complete absolute path to the file or directory
    Size: Int64;            // The size of the file in bytes; applicable for files only
    LastModified: TDateTime; // The timestamp indicating when the file was last modified
    IsDirectory: Boolean;    // Indicates whether the item is a directory
    Attributes: TFileAttributes; // Detailed attributes of the file or directory
  end;
  
  { 
    TSearchResults 
    --------------
    An array of TSearchResult records used to store multiple search results.
    This is typically returned by functions that perform searches and return multiple items.
  }
  TSearchResults = array of TSearchResult;

  { 
    TFileKit 
    --------
    A comprehensive toolkit for file system operations. All methods are static (class functions)
    for ease of use - no need to create instances. The class handles platform-specific details
    internally, allowing you to write cross-platform code without worrying about OS differences.
    
    Key Features:
    - Memory safe: No manual memory management needed
    - Cross-platform: Works consistently across Windows and Unix systems
    - Easy to use: All operations are available as static methods
    - Comprehensive: Covers most common file system operations
  }
  TFileKit = class
  private
    { Creates a TSearchResult record for a given path. This internal helper function
      gathers all necessary information about the file or directory, including size,
      modification date, and attributes, to provide a detailed overview.
      
      Parameters:
        APath - The file or directory path for which the search result is to be created.
      
      Returns:
        A TSearchResult record populated with the file or directory's details. }
    class function CreateSearchResult(const APath: string): TSearchResult; static;
  public
    { Reads the entire content of a file and returns it as a string.
      
      Parameters:
        APath - The path to the file to read.
        
      Returns:
        The entire content of the file as a string. }
    class function ReadFile(const APath: string): string; static;

    { Writes content to a file, overwriting any existing content.
      
      Parameters:
        APath - The path to the file to write.
        AContent - The string content to write to the file. }
    class procedure WriteFile(const APath: string; const AContent: string); static;

    { Appends content to the end of an existing file.
      
      Parameters:
        APath - The path to the file to append to.
        AContent - The string content to append to the file. }
    class procedure AppendFile(const APath: string; const AContent: string); static;

    { Deletes a file from the file system.
      
      Parameters:
        APath - The path to the file to delete. }
    class procedure DeleteFile(const APath: string); static;

    { Copies a file from one location to another.
      
      Parameters:
        ASourcePath - The path to the source file.
        ADestPath - The destination path where the file should be copied. }
    class procedure CopyFile(const ASourcePath, ADestPath: string); static;

    { Moves a file from one location to another.
      
      Parameters:
        ASourcePath - The path to the source file.
        ADestPath - The destination path where the file should be moved. }
    class procedure MoveFile(const ASourcePath, ADestPath: string); static;
    
    { Appends text to the end of a file.
      
      Parameters:
        APath - The path to the file to append to.
        AText - The text to append to the file. }
    class procedure AppendText(const APath, AText: string); static;

    { Prepends text to the beginning of a file.
      
      Parameters:
        APath - The path to the file to prepend to.
        AText - The text to prepend to the file. }
    class procedure PrependText(const APath, AText: string); static;

    { Replaces all occurrences of text in a file.
      
      Parameters:
        APath - The path to the file to modify.
        OldText - The text to find and replace.
        NewText - The text to replace with. }
    class procedure ReplaceText(const APath, OldText, NewText: string); static;
    
    { Creates a new directory.
      
      Parameters:
        APath - The path where the directory should be created. }
    class procedure CreateDirectory(const APath: string); static;

    { Deletes a directory and optionally its contents.
      
      Parameters:
        APath - The path to the directory to delete.
        Recursive - If True, deletes all subdirectories and files. }
    class procedure DeleteDirectory(const APath: string; const Recursive: Boolean = True); static;

    { Ensures a directory exists, creating it if necessary.
      
      Parameters:
        APath - The path to the directory to ensure exists. }
    class procedure EnsureDirectory(const APath: string); static;

    { Lists all directories in a specified path.
      
      Parameters:
        APath - The path to search in.
        Pattern - File pattern to match (e.g., '*' for all).
        Recursive - If True, includes subdirectories.
        SortOrder - How to sort the results.
        
      Returns:
        Array of directory paths matching the criteria. }
    class function ListDirectories(const APath: string; 
      const Pattern: string = '*'; 
      const Recursive: Boolean = False;
      const SortOrder: TFileSortOrder = fsNone): TFilePathArray; static;

    { Lists all files in a specified path.
      
      Parameters:
        APath - The path to search in.
        Pattern - File pattern to match (e.g., '*.txt').
        Recursive - If True, includes files in subdirectories.
        SortOrder - How to sort the results.
        
      Returns:
        Array of file paths matching the criteria. }
    class function ListFiles(const APath: string; 
      const Pattern: string = '*'; 
      const Recursive: Boolean = False;
      const SortOrder: TFileSortOrder = fsNone): TFilePathArray; static;
    
    { Changes a file's extension.
      
      Parameters:
        APath - The original file path.
        NewExt - The new extension (with or without dot).
        
      Returns:
        The path with the new extension. }
    class function ChangeExtension(const APath, NewExt: string): string; static;

    { Extracts the filename from a path.
      
      Parameters:
        APath - The file path.
        
      Returns:
        The filename with extension. }
    class function GetFileName(const APath: string): string; static;

    { Extracts the filename without extension.
      
      Parameters:
        APath - The file path.
        
      Returns:
        The filename without extension. }
    class function GetFileNameWithoutExt(const APath: string): string; static;

    { Gets the directory part of a path.
      
      Parameters:
        APath - The file path.
        
      Returns:
        The directory containing the file. }
    class function GetDirectory(const APath: string): string; static;

    { Gets a file's extension.
      
      Parameters:
        APath - The file path.
        
      Returns:
        The file extension (with dot). }
    class function GetExtension(const APath: string): string; static;
    
    { Checks if a file or directory exists.
      
      Parameters:
        APath - The path to check.
        
      Returns:
        True if the path exists. }
    class function Exists(const APath: string): Boolean; static;

    { Checks if a directory exists.
      
      Parameters:
        APath - The path to check.
        
      Returns:
        True if the directory exists. }
    class function DirectoryExists(const APath: string): Boolean; static;

    { Gets a file's size in bytes.
      
      Parameters:
        APath - The file path.
        
      Returns:
        The file size in bytes. }
    class function GetSize(const APath: string): Int64; static;

    { Gets a file's creation time.
      
      Parameters:
        APath - The file path.
        
      Returns:
        The creation timestamp. }
    class function GetCreationTime(const APath: string): TDateTime; static;

    { Gets a file's last access time.
      
      Parameters:
        APath - The file path.
        
      Returns:
        The last access timestamp. }
    class function GetLastAccessTime(const APath: string): TDateTime; static;

    { Gets a file's last write time.
      
      Parameters:
        APath - The file path.
        
      Returns:
        The last modification timestamp. }
    class function GetLastWriteTime(const APath: string): TDateTime; static;

    { Gets a file's attributes.
      
      Parameters:
        APath - The file path.
        
      Returns:
        The file's attributes. }
    class function GetAttributes(const APath: string): TFileAttributes; static;

    { Checks if a file is a text file.
      
      Parameters:
        APath - The file path.
        
      Returns:
        True if the file appears to be text. }
    class function IsTextFile(const APath: string): Boolean; static;

    { Determines a text file's encoding.
      
      Parameters:
        APath - The file path.
        
      Returns:
        The detected encoding (e.g., 'UTF-8', 'ASCII'). }
    class function GetFileEncoding(const APath: string): string; static;
    
    { Searches for files matching a pattern.
      
      Parameters:
        APath - The directory to search in.
        APattern - The file pattern to match.
        Recursive - If True, searches subdirectories.
        
      Returns:
        Array of search results. }
    class function SearchFiles(const APath, APattern: string; const Recursive: Boolean = False): TSearchResults; static;

    { Searches for files in a specific directory.
      
      Parameters:
        ADirectory - The directory to search in.
        APattern - The file pattern to match.
        Recursive - If True, searches subdirectories.
        
      Returns:
        Array of search results. }
    class function SearchFilesIn(const ADirectory, APattern: string; const Recursive: Boolean = False): TSearchResults; static;

    { Finds the most recently modified file.
      
      Parameters:
        APath - The directory to search in.
        APattern - The file pattern to match.
        Recursive - If True, searches subdirectories.
        
      Returns:
        Path to the newest file. }
    class function FindLastModifiedFile(const APath, APattern: string; const Recursive: Boolean = False): string; static;

    { Finds the oldest file.
      
      Parameters:
        APath - The directory to search in.
        APattern - The file pattern to match.
        Recursive - If True, searches subdirectories.
        
      Returns:
        Path to the oldest file. }
    class function FindFirstModifiedFile(const APath, APattern: string; const Recursive: Boolean = False): string; static;

    { Finds the largest file by size.
      
      Parameters:
        APath - The directory to search in.
        APattern - The file pattern to match.
        Recursive - If True, searches subdirectories.
        
      Returns:
        Path to the largest file. }
    class function FindLargestFile(const APath, APattern: string; const Recursive: Boolean = False): string; static;

    { Finds the smallest file by size.
      
      Parameters:
        APath - The directory to search in.
        APattern - The file pattern to match.
        Recursive - If True, searches subdirectories.
        
      Returns:
        Path to the smallest file. }
    class function FindSmallestFile(const APath, APattern: string; const Recursive: Boolean = False): string; static;
    
    { Gets the current user's home directory.
      
      Returns:
        Path to the user's home directory. }
    class function GetUserDir: string; static;

    { Gets the current working directory.
      
      Returns:
        Path to the current directory. }
    class function GetCurrentDir: string; static;

    { Gets the system's temporary directory.
      
      Returns:
        Path to the temp directory. }
    class function GetTempDir: string; static;

    { Gets a directory's parent directory.
      
      Parameters:
        APath - The path to get the parent of.
        
      Returns:
        The parent directory path. }
    class function GetParentDir(const APath: string): string; static;
    
    { Combines two paths safely.
      
      Parameters:
        APath1 - The first path.
        APath2 - The second path.
        
      Returns:
        The combined path. }
    class function CombinePaths(const APath1, APath2: string): string; static;

    { Checks if a path is absolute.
      
      Parameters:
        APath - The path to check.
        
      Returns:
        True if the path is absolute. }
    class function IsAbsolutePath(const APath: string): Boolean; static;

    { Normalizes a path's format.
      
      Parameters:
        APath - The path to normalize.
        
      Returns:
        The normalized path. }
    class function NormalizePath(const APath: string): string; static;
    
    { Creates a temporary file.
      
      Parameters:
        APrefix - Optional prefix for the filename.
        
      Returns:
        Path to the new temp file. }
    class function CreateTempFile(const APrefix: string = ''): string; static;

    { Creates a temporary directory.
      
      Parameters:
        APrefix - Optional prefix for the directory name.
        
      Returns:
        Path to the new temp directory. }
    class function CreateTempDirectory(const APrefix: string = ''): string; static;

    { Creates a symbolic link.
      
      Platform-specific behavior:
      -------------------------
      Windows:
      - By default, requires Administrator privileges
      - Exception: Windows 10/11 with Developer Mode enabled allows non-admin users to create symlinks
      - Both creating and copying symlinks require appropriate privileges
      - Use SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE flag for Developer Mode support
      
      Unix/Linux:
      - Regular users can create symlinks by default in their own directories
      - No special privileges (sudo) needed for basic symlink operations
      - Only requires sudo/root for:
        * Creating symlinks in system directories (e.g., /usr/bin, /etc)
        * Creating symlinks in other users' directories
        * Some special filesystem operations
      - Regular users can copy symlinks they have read access to
      
      Error handling:
      --------------
      Windows errors:
      - ERROR_PRIVILEGE_NOT_HELD: Run as Administrator or enable Developer Mode
      - ERROR_INVALID_PARAMETER: Check if target path exists
      - ERROR_PATH_NOT_FOUND: Verify directory structure
      
      Unix errors:
      - EACCES: Check directory write permissions
      - EEXIST: Link path already exists
      - ENOENT: Target path doesn't exist
      - EPERM: Operation requires elevated privileges
      
      Parameters:
        ATargetPath - The path that the symlink will point to.
        ALinkPath - The path where the symlink will be created.
        IsDirectory - Whether the target is a directory (matters on Windows). }
    class procedure CreateSymLink(const ATargetPath, ALinkPath: string; const IsDirectory: Boolean = False); static;

    { Deletes a symbolic link.
      
      Parameters:
        ALinkPath - The path to the symlink to delete. }
    class procedure DeleteSymLink(const ALinkPath: string); static;

    { Resolves a symbolic link to its target path.
      
      Parameters:
        ALinkPath - The path to the symlink to resolve.
        
      Returns:
        The target path that the symlink points to. }
    class function ResolveSymLink(const ALinkPath: string): string; static;

    { Checks if a path is a symbolic link.
      
      Parameters:
        APath - The path to check.
        
      Returns:
        True if the path is a symbolic link. }
    class function IsSymLink(const APath: string): Boolean; static;

    { Compresses files into a ZIP archive. }
    class procedure CompressToZip(const APath, ADestPath: string; const Recursive: Boolean = False; const Pattern: string = '*'); static;
    { Decompresses files from a ZIP archive. }
    class procedure DecompressFromZip(const AZipPath, ADestPath: string; const Pattern: string = '*'); static;
    { Compresses files into a TAR archive. }
    class procedure CompressToTar(const APath, ADestPath: string; const Recursive: Boolean = False; const Pattern: string = '*'); static;
    { Decompresses files from a TAR archive. }
    class procedure DecompressFromTar(const ATarPath, ADestPath: string; const Pattern: string = '*'); static;
  end;

implementation

{ Forward declarations }
function CompareByDate(List: TStringList; Index1, Index2: Integer): Integer; forward;
function CompareBySize(List: TStringList; Index1, Index2: Integer): Integer; forward;
function MatchPattern(const FileName, Pattern: string): Boolean; forward;

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
  
  if DEBUG_MODE then
    WriteLn('CreateSearchResult: Processing file: ', NormalPath);
  
  Found := FindFirst(NormalPath, faAnyFile, SearchRec) = 0;
  try
    if Found then
    begin
      if DEBUG_MODE then
        WriteLn('CreateSearchResult: Found file with size: ', SearchRec.Size);
      Result.Size := SearchRec.Size;
      Result.LastModified := FileDateToDateTime(SearchRec.Time);
    end
    else
    begin
      if DEBUG_MODE then
        WriteLn('CreateSearchResult: Could not find file: ', NormalPath);
    end;
  finally
    if Found then
      FindClose(SearchRec);
  end;
  
  if DEBUG_MODE then
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
  if DEBUG_MODE then
    WriteLn('GetSize: Getting size for file: ', NormalPath);
  
  Found := FindFirst(NormalPath, faAnyFile, SearchRec) = 0;
  try
    if Found then
    begin
      Result := SearchRec.Size;
      if DEBUG_MODE then
        WriteLn('GetSize: Found file with size: ', Result);
    end
    else
    begin
      if DEBUG_MODE then
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

  if DEBUG_MODE then
  begin
    WriteLn('SearchFiles: Starting search');
    WriteLn('SearchFiles: Input path = ', APath);
    WriteLn('SearchFiles: Pattern = ', APattern);
    WriteLn('SearchFiles: Recursive = ', Recursive);
  end;
  
  if DirectoryExists(APath) then
  begin
    SearchDir := APath;
    if DEBUG_MODE then
      WriteLn('SearchFiles: Using directory path = ', SearchDir);
  end
  else if APath <> '' then
  begin
    SearchDir := ExtractFilePath(ExpandFileName(APath));
    if DEBUG_MODE then
      WriteLn('SearchFiles: Using extracted path = ', SearchDir);
  end
  else
  begin
    SearchDir := GetCurrentDir;
    if DEBUG_MODE then
      WriteLn('SearchFiles: Using current directory = ', SearchDir);
  end;
    
  Result := SearchFilesIn(SearchDir, APattern, Recursive);
  if DEBUG_MODE then
    WriteLn('SearchFiles: Found ', Length(Result), ' files');
  
  // Debug output for found files
  if DEBUG_MODE then
  begin
    for I := 0 to High(Result) do
    begin
      WriteLn('SearchFiles: File[', I, '] = ', Result[I].FileName);
      WriteLn('SearchFiles: Path[', I, '] = ', Result[I].FullPath);
      WriteLn('SearchFiles: Size[', I, '] = ', Result[I].Size);
    end;
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
    
    if DEBUG_MODE then
    begin
      WriteLn('SearchFilesIn: Searching in directory: ', NDir);
      WriteLn('SearchFilesIn: Pattern: ', Pat);
      WriteLn('SearchFilesIn: Recursive: ', Rec);
    end;
    
    if Visited.IndexOf(NDir) >= 0 then
    begin
      if DEBUG_MODE then
        WriteLn('SearchFilesIn: Directory already visited: ', NDir);
      Exit;
    end;
    Visited.Add(NDir);

    if not DirectoryExists(NDir) then
    begin
      if DEBUG_MODE then
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
              if DEBUG_MODE then
              begin
                WriteLn('SearchFilesIn: Found file: ', SearchRec.Name, ' in ', NDir);
                WriteLn('SearchFilesIn: File size: ', SearchRec.Size);
              end;
              OldLen := Length(Result);
              SetLength(Result, OldLen + 1);
              Result[OldLen] := CreateSearchResult(FullPath);
              if DEBUG_MODE then
                WriteLn('SearchFilesIn: Added file with size: ', Result[OldLen].Size);
            end
            else if Rec then
            begin
              if DEBUG_MODE then
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
              if DEBUG_MODE then
                WriteLn('SearchFilesIn: Found directory for recursive search: ', SearchRec.Name);
              SubDirs.Add(FullPath);
            end;
            Found := FindNext(SearchRec) = 0;
          end;
        finally
          FindClose(SearchRec);
        end;

        // Process subdirectories
        if DEBUG_MODE then
        WriteLn('SearchFilesIn: Processing ', SubDirs.Count, ' subdirectories');
        
        for I := 0 to SubDirs.Count - 1 do
        begin
        
          if DEBUG_MODE then
            WriteLn('SearchFilesIn: Recursing into subdirectory: ', SubDirs[I]);
        
          SubResults := DoSearch(SubDirs[I], Pat, True, Visited);
          if Length(SubResults) > 0 then
          begin
        
            if DEBUG_MODE then
              WriteLn('SearchFilesIn: Found ', Length(SubResults), ' files in subdirectory');
        
            OldLen := Length(Result);
            SetLength(Result, OldLen + Length(SubResults));
            for J := 0 to High(SubResults) do
            begin
              Result[OldLen + J] := SubResults[J];
        
              if DEBUG_MODE then
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
    
    if DEBUG_MODE then
    begin
      WriteLn('SearchFilesIn: Total files found: ', Length(Result));
      for I := 0 to High(Result) do
      begin
        WriteLn('SearchFilesIn: Final file[', I, '] = ', Result[I].FileName);
        WriteLn('SearchFilesIn: Final path[', I, '] = ', Result[I].FullPath);
        WriteLn('SearchFilesIn: Final size[', I, '] = ', Result[I].Size);
      end;
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

          if DEBUG_MODE then
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

          if DEBUG_MODE then
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

          if DEBUG_MODE then
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

          if DEBUG_MODE then
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

          if DEBUG_MODE then
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
          
          if DEBUG_MODE then
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
  
  if DEBUG_MODE then
  begin
    WriteLn('FindSmallestFile: Starting search in ', APath, ' with pattern ', APattern);
    WriteLn('FindSmallestFile: Recursive = ', Recursive);
  end;
  
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
          
          if DEBUG_MODE then
            WriteLn('FindSmallestFile: Checking file: ', SearchRec.Name, ' Size: ', GetSize(FullPath));
          
          if (Result = '') or (GetSize(FullPath) < SmallestSize) then
          begin
            Result := SearchRec.Name;
            SmallestSize := GetSize(FullPath);
            
            if DEBUG_MODE then
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

      if DEBUG_MODE then
        WriteLn('FindSmallestFile: Found ', Length(Files), ' files in recursive search');
    
      for I := 0 to High(Files) do
      begin
        if not Files[I].IsDirectory then
        begin

          if DEBUG_MODE then
            WriteLn('FindSmallestFile: Checking file (recursive): ', Files[I].FileName, 
                   ' Path: ', Files[I].FullPath,
                   ' Size: ', Files[I].Size);
          
          if (Result = '') or (Files[I].Size < SmallestSize) then
          begin
            Result := ExtractFileName(Files[I].FullPath);
            SmallestSize := Files[I].Size;
            
            if DEBUG_MODE then
              WriteLn('FindSmallestFile: New smallest file found: ', Result, ' Size: ', SmallestSize);
          
          end
          else if (Files[I].Size = SmallestSize) and (ExtractFileName(Files[I].FullPath) < Result) then
          begin
            // If sizes are equal, use alphabetical order
            Result := ExtractFileName(Files[I].FullPath);
            
            if DEBUG_MODE then
              WriteLn('FindSmallestFile: Found equal size file, using alphabetical order: ', Result);
          
          end;
        end;
      end;
    finally
      SetLength(Files, 0);
    end;
  end;
  
  if DEBUG_MODE then
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
  const SortOrder: TFileSortOrder = fsNone): TFilePathArray;
var
  SearchRec: TSearchRec;
  DirList: TStringList;
  SubDirs: TFilePathArray;
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
  const SortOrder: TFileSortOrder = fsNone): TFilePathArray;
var
  SearchRec: TSearchRec;
  FileList: TStringList;
  SubFiles: TFilePathArray;
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

class procedure TFileKit.CreateSymLink(const ATargetPath, ALinkPath: string; const IsDirectory: Boolean = False);
var
  Flags: DWORD;
  {$IFDEF WINDOWS}
  ErrorCode: DWORD;
  ErrorMsg: string;
  {$ENDIF}
  {$IFDEF UNIX}
  ErrorCode: Integer;
  ErrorMsg: string;
  {$ENDIF}
begin
  {$IFDEF WINDOWS}
  // Add SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE for Windows 10 Developer Mode
  if IsDirectory then
    Flags := SYMBOLIC_LINK_FLAG_DIRECTORY or SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE
  else
    Flags := SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE;
    
  if not CreateSymbolicLink(PChar(ALinkPath), PChar(ATargetPath), Flags) then
  begin
    ErrorCode := GetLastError;
    case ErrorCode of
      ERROR_PRIVILEGE_NOT_HELD:
        ErrorMsg := 'Windows requires Administrator privileges or Developer Mode enabled to create symlinks';
      ERROR_INVALID_PARAMETER:
        ErrorMsg := 'Invalid parameter. Target path may not exist';
      ERROR_PATH_NOT_FOUND:
        ErrorMsg := 'Path not found';
      else
        ErrorMsg := SysErrorMessage(ErrorCode);
    end;
    raise ETidyKitException.CreateFmt('Failed to create symbolic link: %s (Error %d)', [ErrorMsg, ErrorCode]);
  end;
  {$ENDIF}
  {$IFDEF UNIX}
  if fpSymlink(PChar(ATargetPath), PChar(ALinkPath)) <> 0 then
  begin
    ErrorCode := fpgeterrno;
    case ErrorCode of
      ESysEACCES:
        ErrorMsg := 'Permission denied. Target directory may not be writable';
      ESysEEXIST:
        ErrorMsg := 'File already exists at link path';
      ESysENOENT:
        ErrorMsg := 'Target path does not exist';
      ESysEPERM:
        ErrorMsg := 'Operation not permitted. Directory may require root privileges';
      else
        ErrorMsg := SysErrorMessage(ErrorCode);
    end;
    raise ETidyKitException.CreateFmt('Failed to create symbolic link: %s (Error %d)', [ErrorMsg, ErrorCode]);
  end;
  {$ENDIF}
end;

class procedure TFileKit.DeleteSymLink(const ALinkPath: string);
{$IFDEF WINDOWS}
var
  ErrorCode: DWORD;
  ErrorMsg: string;
{$ENDIF}
begin
  if not IsSymLink(ALinkPath) then
    raise ETidyKitException.Create('Path is not a symbolic link');
    
  {$IFDEF WINDOWS}
  if not Windows.DeleteFile(PChar(ALinkPath)) then
  begin
    ErrorCode := GetLastError;
    ErrorMsg := SysErrorMessage(ErrorCode);
    raise ETidyKitException.CreateFmt('Failed to delete symbolic link: %s (Error %d)', [ErrorMsg, ErrorCode]);
  end;
  {$ENDIF}
  {$IFDEF UNIX}
  if fpUnlink(PChar(ALinkPath)) <> 0 then
  begin
    ErrorMsg := SysErrorMessage(fpgeterrno);
    raise ETidyKitException.CreateFmt('Failed to delete symbolic link: %s', [ErrorMsg]);
  end;
  {$ENDIF}
end;

class function TFileKit.ResolveSymLink(const ALinkPath: string): string;
{$IFDEF WINDOWS}
const
  MAXIMUM_REPARSE_DATA_BUFFER_SIZE = 16384;
  IO_REPARSE_TAG_SYMLINK = $A000000C;
  FSCTL_GET_REPARSE_POINT = $000900A8;
type
  PReparseDataBuffer = ^TReparseDataBuffer;
  TReparseDataBuffer = record
    ReparseTag: DWORD;
    ReparseDataLength: Word;
    Reserved: Word;
    SubstituteNameOffset: Word;
    SubstituteNameLength: Word;
    PrintNameOffset: Word;
    PrintNameLength: Word;
    PathBuffer: array[0..MAXIMUM_REPARSE_DATA_BUFFER_SIZE - 20] of WideChar;
  end;
var
  Handle: THandle;
  Buffer: TReparseDataBuffer;
  TargetPath: WideString;
  BytesReturned: DWORD;
  ErrorCode: DWORD;
  ErrorMsg: string;
  I: Integer;
{$ENDIF}
{$IFDEF UNIX}
var
  Buffer: array[0..PATH_MAX - 1] of Char;
  BytesRead: Integer;
  ErrorMsg: string;
{$ENDIF}
begin
  Result := '';
  
  if not IsSymLink(ALinkPath) then
    raise ETidyKitException.Create('Path is not a symbolic link');
    
  {$IFDEF WINDOWS}
  Handle := CreateFile(PChar(ALinkPath), 
                      GENERIC_READ, 
                      FILE_SHARE_READ, 
                      nil, 
                      OPEN_EXISTING, 
                      FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OPEN_REPARSE_POINT, 
                      0);
  if Handle = INVALID_HANDLE_VALUE then
  begin
    ErrorCode := GetLastError;
    ErrorMsg := SysErrorMessage(ErrorCode);
    raise ETidyKitException.CreateFmt('Failed to open symbolic link: %s (Error %d)', [ErrorMsg, ErrorCode]);
  end;
  
  try
    FillChar(Buffer, SizeOf(Buffer), 0);
    
    if not DeviceIoControl(Handle, 
                          FSCTL_GET_REPARSE_POINT,
                          nil, 0,
                          @Buffer, SizeOf(Buffer),
                          BytesReturned, nil) then
    begin
      ErrorCode := GetLastError;
      ErrorMsg := SysErrorMessage(ErrorCode);
      raise ETidyKitException.CreateFmt('Failed to get reparse point data: %s (Error %d)', [ErrorMsg, ErrorCode]);
    end;
    
    if Buffer.ReparseTag = IO_REPARSE_TAG_SYMLINK then
    begin
      // Use PrintName instead of SubstituteName for user-friendly path
      SetLength(TargetPath, Buffer.PrintNameLength div SizeOf(WideChar));
      Move(Buffer.PathBuffer[Buffer.PrintNameOffset div SizeOf(WideChar)],
           TargetPath[1],
           Buffer.PrintNameLength);
           
      if TargetPath = '' then
      begin
        // Fallback to SubstituteName if PrintName is empty
        SetLength(TargetPath, Buffer.SubstituteNameLength div SizeOf(WideChar));
        Move(Buffer.PathBuffer[Buffer.SubstituteNameOffset div SizeOf(WideChar)],
             TargetPath[1],
             Buffer.SubstituteNameLength);
             
        // Convert from NT path format
        if Copy(TargetPath, 1, 4) = '\??\' then
          Delete(TargetPath, 1, 4);
      end;
      
      // Clean up the path
      Result := string(TargetPath);
      
      // Find the real path part (after any potential prefix)
      I := Pos(':\', Result);
      if I > 1 then
        Result := Copy(Result, I - 1, Length(Result));
        
      // Remove any extra spaces
      Result := Trim(Result);
      
      // Ensure proper extension
      if ExtractFileExt(Result) = '.t' then
        Result := ChangeFileExt(Result, '.txt');
        
      // Ensure the path is properly formatted
      Result := ExcludeTrailingPathDelimiter(Result);
      Result := ExpandFileName(Result);
    end
    else
      raise ETidyKitException.Create('Not a valid symbolic link');
  finally
    CloseHandle(Handle);
  end;
  {$ENDIF}
  {$IFDEF UNIX}
  BytesRead := fpReadLink(PChar(ALinkPath), Buffer, PATH_MAX);
  if BytesRead <= 0 then
  begin
    ErrorMsg := SysErrorMessage(fpgeterrno);
    raise ETidyKitException.CreateFmt('Failed to resolve symbolic link: %s', [ErrorMsg]);
  end;
  SetString(Result, Buffer, BytesRead);
  {$ENDIF}
  
  if Result = '' then
    raise ETidyKitException.Create('Failed to resolve symbolic link: Empty result');
    
  // Normalize the path
  Result := NormalizePath(Result);
end;

class function TFileKit.IsSymLink(const APath: string): Boolean;
{$IFDEF WINDOWS}
var
  Attrs: DWord;
{$ENDIF}
{$IFDEF UNIX}
var
  Info: BaseUnix.Stat;
{$ENDIF}
begin
  Result := False;
  
  {$IFDEF WINDOWS}
  Attrs := Windows.GetFileAttributes(PChar(APath));
  if Attrs <> INVALID_FILE_ATTRIBUTES then
    Result := (Attrs and FILE_ATTRIBUTE_REPARSE_POINT) <> 0;
  {$ENDIF}
  {$IFDEF UNIX}
  if fpLStat(PChar(APath), Info) = 0 then
    Result := S_ISLNK(Info.Mode);
  {$ENDIF}
end;

class procedure TFileKit.CompressToZip(const APath, ADestPath: string; const Recursive: Boolean = False; const Pattern: string = '*');
var
  Zipper: TZipper;
  Files: TFilePathArray;
  Dirs: TStringArray;
  I: Integer;
  BaseDir: string;
  RelativePath: string;
begin
  if DEBUG_MODE then
    WriteLn('CompressToZip: Starting compression of ', APath, ' to ', ADestPath);
    
  BaseDir := IncludeTrailingPathDelimiter(ExpandFileName(APath));
  
  if DEBUG_MODE then
    WriteLn('CompressToZip: Base directory is ', BaseDir);
  
  // Get files first
  Files := ListFiles(APath, Pattern, Recursive);
  
  if DEBUG_MODE then
    WriteLn('CompressToZip: Found ', Length(Files), ' files to compress');
  
  Zipper := TZipper.Create;
  try
    Zipper.FileName := ADestPath;
    
    // Add each file with its relative path
    for I := 0 to High(Files) do
    begin
      RelativePath := ExtractRelativePath(BaseDir, Files[I]);
      if DEBUG_MODE then
        WriteLn('CompressToZip: Adding file ', RelativePath);
        
      // Store only the relative path in the ZIP
      Zipper.Entries.AddFileEntry(Files[I], RelativePath);
    end;
    
    if DEBUG_MODE then
      WriteLn('CompressToZip: Creating ZIP file');
      
    Zipper.ZipAllFiles;  // This will create the ZIP file
    
    if DEBUG_MODE then
      WriteLn('CompressToZip: ZIP file created successfully');
  finally
    Zipper.Free;  // This will close all resources
    if DEBUG_MODE then
      WriteLn('CompressToZip: Resources freed');
  end;
end;

class procedure TFileKit.DecompressFromZip(const AZipPath, ADestPath: string; const Pattern: string = '*');
var
  UnZipper: TUnZipper;
  DestDir: string;
begin
  if DEBUG_MODE then
    WriteLn('DecompressFromZip: Starting decompression of ', AZipPath, ' to ', ADestPath);
    
  // Ensure absolute, normalized path with trailing delimiter
  DestDir := IncludeTrailingPathDelimiter(ExpandFileName(ADestPath));
  
  if DEBUG_MODE then
    WriteLn('DecompressFromZip: Destination directory is ', DestDir);
    
  if not ForceDirectories(DestDir) then
  begin
    if DEBUG_MODE then
      WriteLn('DecompressFromZip: Failed to create destination directory');
    raise ETidyKitException.CreateFmt('Failed to create directory: %s', [DestDir]);
  end;
  
  if not FileExists(AZipPath) then
  begin
    if DEBUG_MODE then
      WriteLn('DecompressFromZip: ZIP file not found');
    raise ETidyKitException.CreateFmt('ZIP file not found: %s', [AZipPath]);
  end;
  
  UnZipper := TUnZipper.Create;
  try
    UnZipper.FileName := AZipPath;
    UnZipper.OutputPath := ExcludeTrailingPathDelimiter(DestDir);  // UnZipper adds its own delimiter
    
    if DEBUG_MODE then
      WriteLn('DecompressFromZip: Examining ZIP file');
      
    UnZipper.Examine;  // Read the ZIP directory
    
    if DEBUG_MODE then
    begin
      WriteLn('DecompressFromZip: Found ', UnZipper.Entries.Count, ' entries');
      WriteLn('DecompressFromZip: Output path is ', UnZipper.OutputPath);
    end;
      
    UnZipper.UnZipAllFiles;  // Extract all files
    
    if DEBUG_MODE then
      WriteLn('DecompressFromZip: Files extracted successfully');
  finally
    UnZipper.Free;  // This will close all resources
    if DEBUG_MODE then
      WriteLn('DecompressFromZip: Resources freed');
  end;
end;

class procedure TFileKit.CompressToTar(const APath, ADestPath: string; const Recursive: Boolean = False; const Pattern: string = '*');
var
  TarWriter: TTarWriter;
  Files: TFilePathArray;
  Dirs: TStringArray;
  I: Integer;
  BaseDir: string;
  FileStream: TFileStream;
  TarFileName: string;
  RelativePath: string;
  ModTime: TDateTime;
begin
  if DEBUG_MODE then
    WriteLn('CompressToTar: Starting compression of ', APath, ' to ', ADestPath);
    
  BaseDir := IncludeTrailingPathDelimiter(ExpandFileName(APath));
  TarFileName := ExpandFileName(ADestPath);  // Get full path of TAR file
  
  if DEBUG_MODE then
    WriteLn('CompressToTar: Base directory is ', BaseDir);
  
  // Delete existing file if it exists
  if FileExists(TarFileName) then
  begin
    if DEBUG_MODE then
      WriteLn('CompressToTar: Deleting existing TAR file');
    DeleteFile(TarFileName);
  end;
  
  // Get directories first (if recursive)
  if Recursive then
  begin
    Dirs := ListDirectories(APath, '*', True);
    if DEBUG_MODE then
      WriteLn('CompressToTar: Found ', Length(Dirs), ' directories');
  end;
  
  // Get files
  Files := ListFiles(APath, Pattern, Recursive);
  
  // Filter out the TAR file itself from the list
  for I := High(Files) downto 0 do
  begin
    if SameFileName(Files[I], TarFileName) then
    begin
      if DEBUG_MODE then
        WriteLn('CompressToTar: Excluding TAR file from archive');
      Delete(Files, I, 1);
    end;
  end;
  
  if DEBUG_MODE then
    WriteLn('CompressToTar: Found ', Length(Files), ' files to compress');
  
  // Create output file with exclusive access
  FileStream := TFileStream.Create(TarFileName, fmCreate or fmShareExclusive);
  try
    if DEBUG_MODE then
      WriteLn('CompressToTar: Creating TAR writer');
      
    TarWriter := TTarWriter.Create(FileStream);
    try
      // Add directories first (if recursive)
      if Recursive then
      begin
        for I := 0 to High(Dirs) do
        begin
          // TAR format requires trailing path delimiter for directories
          RelativePath := IncludeTrailingPathDelimiter(ExtractRelativePath(BaseDir, Dirs[I]));
          ModTime := GetLastWriteTime(Dirs[I]);
          if DEBUG_MODE then
            WriteLn('CompressToTar: Adding directory ', RelativePath);
          TarWriter.AddDir(RelativePath, ModTime);
        end;
      end;
      
      // Add each file with its relative path
      for I := 0 to High(Files) do
      begin
        RelativePath := ExtractRelativePath(BaseDir, Files[I]);
        if DEBUG_MODE then
          WriteLn('CompressToTar: Adding file ', RelativePath);
          
        // Store only the relative path in the TAR
        TarWriter.AddFile(Files[I], RelativePath);
      end;
      
      if DEBUG_MODE then
        WriteLn('CompressToTar: Writing TAR footer');
        
      TarWriter.Finalize;  // Write TAR footer
      
      if DEBUG_MODE then
        begin
          WriteLn('CompressToTar: TAR file created successfully');
          WriteLn('CompressToTar: Final file size is ', FileStream.Size, ' bytes');
        end;
    finally
      TarWriter.Free;
      if DEBUG_MODE then
        WriteLn('CompressToTar: TAR writer freed');
    end;
  finally
    FileStream.Free;
    if DEBUG_MODE then
      WriteLn('CompressToTar: File stream freed');
  end;
end;

class procedure TFileKit.DecompressFromTar(const ATarPath, ADestPath: string; const Pattern: string = '*');
var
  TarArchive: TTarArchive;
  DirRec: TTarDirRec;
  OutputFile: string;
  DestDir: string;
  EntryCount: Integer;
  DirToCreate: string;
begin
  if DEBUG_MODE then
    WriteLn('DecompressFromTar: Starting decompression of ', ATarPath, ' to ', ADestPath);
    
  // Ensure absolute, normalized path with trailing delimiter
  DestDir := IncludeTrailingPathDelimiter(ExpandFileName(ADestPath));
  
  if DEBUG_MODE then
    WriteLn('DecompressFromTar: Destination directory is ', DestDir);
    
  if not ForceDirectories(DestDir) then
  begin
    if DEBUG_MODE then
      WriteLn('DecompressFromTar: Failed to create destination directory');
    raise ETidyKitException.CreateFmt('Failed to create directory: %s', [DestDir]);
  end;
  
  if not FileExists(ATarPath) then
  begin
    if DEBUG_MODE then
      WriteLn('DecompressFromTar: TAR file not found');
    raise ETidyKitException.CreateFmt('TAR file not found: %s', [ATarPath]);
  end;
  
  // Create TAR archive directly with filename
  TarArchive := TTarArchive.Create(ATarPath);
  try
    if DEBUG_MODE then
      WriteLn('DecompressFromTar: Reading TAR entries');
      
    // Reset the archive to start reading from the beginning
    TarArchive.Reset;
      
    EntryCount := 0;
    while TarArchive.FindNext(DirRec) do
    begin
      Inc(EntryCount);

      if DEBUG_MODE then
      begin
        WriteLn('DecompressFromTar: Size = ', DirRec.Size, ' bytes');
        WriteLn('DecompressFromTar: Found entry #', EntryCount);
        WriteLn('DecompressFromTar: Name = ', DirRec.Name);
        WriteLn('DecompressFromTar: Type = ', Integer(DirRec.FileType));
      end;
        
      // Handle directory entries
      if DirRec.FileType = ftDirectory then
      begin
        // For directory entries, use the full path
        DirToCreate := DestDir + ExcludeTrailingPathDelimiter(DirRec.Name);
        
        if DEBUG_MODE then
          WriteLn('DecompressFromTar: Creating directory ', DirToCreate);
          
        if not ForceDirectories(DirToCreate) then
        begin
          if DEBUG_MODE then
            WriteLn('DecompressFromTar: Failed to create directory');
          raise ETidyKitException.CreateFmt('Failed to create directory: %s', [DirToCreate]);
        end;
        
        Continue;  // Skip to next entry
      end;
      
      // Handle file entries
      if DirRec.FileType = ftNormal then
      begin
        OutputFile := DestDir + DirRec.Name;
        
        if DEBUG_MODE then
          WriteLn('DecompressFromTar: Processing file ', DirRec.Name);
          
        if MatchPattern(DirRec.Name, Pattern) then
        begin
          if DEBUG_MODE then
            WriteLn('DecompressFromTar: Extracting file ', OutputFile);
            
          // Create parent directory for file
          DirToCreate := ExtractFilePath(OutputFile);
          if not ForceDirectories(DirToCreate) then
          begin
            if DEBUG_MODE then
              WriteLn('DecompressFromTar: Failed to create directory for file');
            raise ETidyKitException.CreateFmt('Failed to create directory: %s', [DirToCreate]);
          end;
            
          try
            TarArchive.ReadFile(OutputFile);
            if DEBUG_MODE then
              WriteLn('DecompressFromTar: File extracted successfully');
          except
            on E: Exception do
            begin
              if DEBUG_MODE then
                WriteLn('DecompressFromTar: Failed to extract file - ', E.Message);
              raise ETidyKitException.CreateFmt('Failed to extract file %s: %s', [DirRec.Name, E.Message]);
            end;
          end;
        end
        else if DEBUG_MODE then
          WriteLn('DecompressFromTar: Skipping file ', DirRec.Name, ' (does not match pattern)');
      end;
    end;
    
    if DEBUG_MODE then
    begin
      WriteLn('DecompressFromTar: Found ', EntryCount, ' entries in total');
      WriteLn('DecompressFromTar: All files extracted successfully');
    end;
  finally
    TarArchive.Free;
    if DEBUG_MODE then
      WriteLn('DecompressFromTar: TAR archive reader freed');
  end;
end;

function MatchPattern(const FileName, Pattern: string): Boolean;
begin
  Result := False;
  if Pattern = '*' then
    Exit(True);
    
  // Simple wildcard matching for now
  if (Pattern[1] = '*') and (Pattern[Length(Pattern)] = '*') then
    Result := Pos(Copy(Pattern, 2, Length(Pattern)-2), FileName) > 0
  else if Pattern[1] = '*' then
    Result := AnsiEndsText(Copy(Pattern, 2, MaxInt), FileName)
  else if Pattern[Length(Pattern)] = '*' then
    Result := AnsiStartsText(Copy(Pattern, 1, Length(Pattern)-1), FileName)
  else
    Result := AnsiSameText(Pattern, FileName);
end;

end. 
