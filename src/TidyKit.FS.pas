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
  DEBUG_MODE = True; // Enable debugging output
  
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

  TDirectoryInfo = record
    FileCount: Integer;
    DirectoryCount: Integer;
    TotalSize: Int64;
    OldestFile: string;
    NewestFile: string;
    LargestFile: string;
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
    class function ReadTextFile(const APath: string): string; static;

    { Writes content to a file, overwriting any existing content.
      
      Parameters:
        APath - The path to the file to write.
        AContent - The string content to write to the file. }
    class procedure WriteTextFile(const APath: string; const AContent: string); static;

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

    { Batch file operations }

    class procedure CopyFiles(const ASourceDir, ADestDir, APattern: string); static;
    class procedure MoveFiles(const ASourceDir, ADestDir, APattern: string); static;
    class procedure DeleteFiles(const ASourceDir, APattern: string); static;

    { Simple Path Analysis }
    class function IsEmptyDirectory(const Path: string): Boolean; static;
    class function GetCommonPath(const Path1, Path2: string): string; static;
    class function GetRelativePath(const BasePath, TargetPath: string): string; static;
    class function IsSubPath(const ParentPath, ChildPath: string): Boolean; static;

    { Basic File Content Operations }
    class function CountLines(const FilePath: string): Integer; static;
    class function GetFirstLine(const FilePath: string): string; static;
    class function GetLastLine(const FilePath: string): string; static;
    class function IsFileEmpty(const FilePath: string): Boolean; static;
    class function ContainsText(const FilePath, SearchText: string; CaseSensitive: Boolean = False): Boolean; static;

    { Simple File Type Detection }
    class function IsBinaryFile(const FilePath: string): Boolean; static;
    class function GetMimeType(const FilePath: string): string; static;
    class function IsExecutable(const FilePath: string): Boolean; static;
    class function IsHidden(const FilePath: string): Boolean; static;

    { Basic Space Operations }
    class function GetDriveFreeSpace(const Path: string): Int64; static;
    class function GetDriveCapacity(const Path: string): Int64; static;
    class function HasEnoughSpace(const Path: string; RequiredBytes: Int64): Boolean; static;

    { Basic File Comparison }
    class function AreFilesIdentical(const File1, File2: string): Boolean; static;
    class function GetNewerFile(const File1, File2: string): string; static;
    class function GetFileDifferences(const File1, File2: string): TStringArray; static;

    { Simple File Locking }
    class function LockFile(const FilePath: string): Boolean; static;
    class function UnlockFile(const FilePath: string): Boolean; static;
    class function IsFileLocked(const FilePath: string): Boolean; static;

    { Path Validation and Sanitization }
    class function IsValidFileName(const FileName: string): Boolean; static;
    class function SanitizeFileName(const FileName: string): string; static;
    class function MakeValidPath(const Path: string): string; static;
    class function IsPathTooLong(const Path: string): Boolean; static;

    { Simple Directory Summary }
    class function GetDirectoryInfo(const Path: string): TDirectoryInfo; static;

    { Basic File Patterns }
    class function MatchesPattern(const FileName, Pattern: string): Boolean; static;
    class function FindFirstMatch(const Directory, Pattern: string): string; static;
    class function CountMatches(const Directory, Pattern: string): Integer; static;
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

function LoadTextFromFile(const APath: string): string;
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

procedure SaveTextToFile(const APath: string; const AContent: string);
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

class function TFileKit.ReadTextFile(const APath: string): string;
begin
  Result := LoadTextFromFile(APath);
end;

class procedure TFileKit.WriteTextFile(const APath: string; const AContent: string);
begin
  if APath <> '' then
  begin
    ForceDirectories(ExtractFilePath(APath));
    SaveTextToFile(APath, AContent);
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
  {$IFDEF WINDOWS}
  SourceAttrs: DWord;
  FileTime: TFileTime;
  Handle: THandle;
  {$ENDIF}
  {$IFDEF UNIX}
  Info: BaseUnix.Stat;
  {$ENDIF}
begin
  if FileExists(ASourcePath) then
  begin
    ForceDirectories(ExtractFilePath(ADestPath));
    
    // First copy the file content
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
    
    // Now copy file attributes and timestamps
    {$IFDEF WINDOWS}
    // Get source file attributes
    SourceAttrs := Windows.GetFileAttributes(PChar(ASourcePath));
    if SourceAttrs <> INVALID_FILE_ATTRIBUTES then
      Windows.SetFileAttributes(PChar(ADestPath), SourceAttrs);
      
    // Copy timestamps
    Handle := CreateFile(PChar(ASourcePath), GENERIC_READ, FILE_SHARE_READ, nil,
                        OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
    if Handle <> INVALID_HANDLE_VALUE then
    begin
      try
        if GetFileTime(Handle, @FileTime, nil, nil) then
        begin
          CloseHandle(Handle);
          Handle := CreateFile(PChar(ADestPath), GENERIC_WRITE, 0, nil,
                             OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
          if Handle <> INVALID_HANDLE_VALUE then
          begin
            SetFileTime(Handle, @FileTime, @FileTime, @FileTime);
          end;
        end;
      finally
        if Handle <> INVALID_HANDLE_VALUE then
          CloseHandle(Handle);
      end;
    end;
    {$ENDIF}
    
    {$IFDEF UNIX}
    // Get source file metadata
    if fpStat(PChar(ASourcePath), Info) = 0 then
    begin
      // Set permissions
      fpChmod(PChar(ADestPath), Info.Mode and $0FFF);
      
      // Set ownership if we have permissions
      if fpGetuid = 0 then  // Only try if we're root
      begin
        fpChown(PChar(ADestPath), Info.uid, Info.gid);
      end;
      
      // Set timestamps
      with Info do
      begin
        fpUtime(PChar(ADestPath), @Info.mtime);
      end;
    end;
    {$ENDIF}
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
      
    // First try a simple rename
    if not RenameFile(ASourcePath, ADestPath) then
    begin
      // If rename fails, copy and delete
      CopyFile(ASourcePath, ADestPath);
      if FileExists(ADestPath) then
      begin
        // Verify copy succeeded before deleting source
        if GetSize(ADestPath) = GetSize(ASourcePath) then
          SysUtils.DeleteFile(ASourcePath)
        else
          raise ETidyKitException.Create('Move operation failed: Size mismatch after copy');
      end;
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
      Content := LoadTextFromFile(APath);
      SaveTextToFile(APath, Content + AText);
    end
    else
      SaveTextToFile(APath, AText);
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
      Content := LoadTextFromFile(APath);
      SaveTextToFile(APath, AText + Content);
    end
    else
      SaveTextToFile(APath, AText);
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
      Content := LoadTextFromFile(APath);
      Content := StringReplace(Content, OldText, NewText, [rfReplaceAll]);
      SaveTextToFile(APath, Content);
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
begin
  {$IFDEF WINDOWS}
  Result := StringReplace(APath, '/', '\', [rfReplaceAll]);
  {$ELSE}
  Result := StringReplace(APath, '\', '/', [rfReplaceAll]);
  {$ENDIF}
  Result := ExpandFileName(Result);
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
    WriteTextFile(Result, ''); // Create empty file
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

{ Batch file operations }

class procedure TFileKit.CopyFiles(const ASourceDir, ADestDir, APattern: string);
var
  Files: TFilePathArray;
  I: Integer;
  RelativePath, DestPath: string;
begin
  if not DirectoryExists(ASourceDir) then
    Exit;
    
  // Create destination directory if it doesn't exist
  ForceDirectories(ADestDir);
  
  // Get list of files matching pattern
  Files := ListFiles(ASourceDir, APattern, False);
  
  // Copy each file
  for I := 0 to High(Files) do
  begin
    // Get relative path from source directory
    RelativePath := ExtractRelativePath(
      IncludeTrailingPathDelimiter(ASourceDir),
      Files[I]
    );
    
    // Construct destination path
    DestPath := CombinePaths(ADestDir, RelativePath);
    
    // Create destination directory if needed
    ForceDirectories(ExtractFilePath(DestPath));
    
    // Copy the file
    CopyFile(Files[I], DestPath);
  end;
end;

class procedure TFileKit.MoveFiles(const ASourceDir, ADestDir, APattern: string);
var
  Files: TFilePathArray;
  I: Integer;
  RelativePath, DestPath: string;
begin
  if not DirectoryExists(ASourceDir) then
    Exit;
    
  // Create destination directory if it doesn't exist
  ForceDirectories(ADestDir);
  
  // Get list of files matching pattern
  Files := ListFiles(ASourceDir, APattern, False);
  
  // Move each file
  for I := 0 to High(Files) do
  begin
    // Get relative path from source directory
    RelativePath := ExtractRelativePath(
      IncludeTrailingPathDelimiter(ASourceDir),
      Files[I]
    );
    
    // Construct destination path
    DestPath := CombinePaths(ADestDir, RelativePath);
    
    // Create destination directory if needed
    ForceDirectories(ExtractFilePath(DestPath));
    
    // Move the file
    MoveFile(Files[I], DestPath);
  end;
end;

class procedure TFileKit.DeleteFiles(const ASourceDir, APattern: string);
var
  Files: TFilePathArray;
  I: Integer;
begin
  if not DirectoryExists(ASourceDir) then
    Exit;
    
  // Get list of files matching pattern
  Files := ListFiles(ASourceDir, APattern, False);
  
  // Delete each file
  for I := 0 to High(Files) do
    DeleteFile(Files[I]);
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

{ Simple Path Analysis }

class function TFileKit.IsEmptyDirectory(const Path: string): Boolean;
var
  SearchRec: TSearchRec;
  FindResult: Integer;
  IsEmpty: Boolean;
begin
  if not DirectoryExists(Path) then
    raise ETidyKitException.CreateFmt('Directory does not exist: %s', [Path]);
    
  IsEmpty := True;  // Assume empty until we find a non-special entry
  
  try
    FindResult := FindFirst(IncludeTrailingPathDelimiter(Path) + '*', faAnyFile, SearchRec);
    try
      while FindResult = 0 do
      begin
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          IsEmpty := False;  // Found a real file or directory
          Break;
        end;
        FindResult := FindNext(SearchRec);
      end;
      Result := IsEmpty;
    finally
      FindClose(SearchRec);
    end;
  except
    on E: Exception do
      raise ETidyKitException.CreateFmt('Error checking if directory is empty: %s', [E.Message]);
  end;
end;

class function TFileKit.GetCommonPath(const Path1, Path2: string): string;
var
  Parts1, Parts2: TStringArray;
  I, MinLen: Integer;
  CommonParts: TStringArray;
  IsUnixStyle: Boolean;
  {$IFDEF WINDOWS}
  HasDriveLetter: Boolean;
  DriveLetter: string;
  {$ENDIF}
begin
  // Check if paths are Unix-style (starting with /)
  IsUnixStyle := (Length(Path1) > 0) and (Path1[1] = '/') and
                 (Length(Path2) > 0) and (Path2[1] = '/');

  {$IFDEF WINDOWS}
  // On Windows, if the paths are Unix-style, convert them to Windows format
  if IsUnixStyle then
  begin
    DriveLetter := 'C:';
    HasDriveLetter := True;
    // Remove leading slash and split by forward slash
    Parts1 := SplitString(Copy(Path1, 2, Length(Path1)), '/');
    Parts2 := SplitString(Copy(Path2, 2, Length(Path2)), '/');
  end
  else
  {$ENDIF}
  begin
    {$IFDEF WINDOWS}
    HasDriveLetter := (Length(Path1) >= 2) and (Path1[2] = ':') and
                      (Length(Path2) >= 2) and (Path2[2] = ':');
    if HasDriveLetter then
    begin
      DriveLetter := UpperCase(Path1[1]) + ':';
      // Remove drive letters for comparison
      Parts1 := SplitString(Copy(Path1, 3, Length(Path1)), PathDelim);
      Parts2 := SplitString(Copy(Path2, 3, Length(Path2)), PathDelim);
    end
    else
    {$ENDIF} 
    begin
      Parts1 := SplitString(NormalizePath(Path1), PathDelim);
      Parts2 := SplitString(NormalizePath(Path2), PathDelim);
    end;
  end;
  
  // If either path is empty, return empty string
  if (Length(Parts1) = 0) or (Length(Parts2) = 0) then
    Exit('');
    
  // If root paths are different, return empty string
  if Parts1[0] <> Parts2[0] then
    Exit('');
    
  MinLen := Length(Parts1);
  if Length(Parts2) < MinLen then
    MinLen := Length(Parts2);
    
  SetLength(CommonParts, 0);
  
  for I := 0 to MinLen - 1 do
  begin
    if Parts1[I] = Parts2[I] then
    begin
      SetLength(CommonParts, Length(CommonParts) + 1);
      CommonParts[High(CommonParts)] := Parts1[I];
    end
    else
      Break;
  end;
  
  if Length(CommonParts) = 0 then
    Result := ''
  else begin
    {$IFDEF WINDOWS}
    if HasDriveLetter then
    begin
      Result := DriveLetter + '\' + CommonParts[0];
      for I := 1 to High(CommonParts) do
        Result := Result + '\' + CommonParts[I];
    end
    else
    {$ENDIF}
    if IsUnixStyle then
    begin
      Result := '/' + CommonParts[0];  // Add leading slash for Unix paths
      for I := 1 to High(CommonParts) do
        Result := Result + '/' + CommonParts[I];
    end
    else
    begin
      Result := CommonParts[0];
      for I := 1 to High(CommonParts) do
        Result := Result + PathDelim + CommonParts[I];
    end;
  end;
end;

class function TFileKit.GetRelativePath(const BasePath, TargetPath: string): string;
var
  BaseNorm, TargetNorm: string;
  BaseParts, TargetParts: TStringArray;
  CommonLength, I, UpLevels: Integer;
  ResultParts: TStringArray;
  IsUnixStyle: Boolean;
  {$IFDEF WINDOWS}
  HasDriveLetter: Boolean;
  {$ENDIF}
begin
  // Check if paths are Unix-style (starting with /)
  IsUnixStyle := (Length(BasePath) > 0) and (BasePath[1] = '/') and
                 (Length(TargetPath) > 0) and (TargetPath[1] = '/');
                 
  {$IFDEF WINDOWS}
  HasDriveLetter := (Length(BasePath) >= 2) and (BasePath[2] = ':') and
                    (Length(TargetPath) >= 2) and (TargetPath[2] = ':');
  {$ENDIF}
                 
  if IsUnixStyle then
  begin
    // For Unix paths, use them as-is but remove leading slash
    BaseNorm := Copy(BasePath, 2, Length(BasePath));
    TargetNorm := Copy(TargetPath, 2, Length(TargetPath));
    BaseParts := SplitString(BaseNorm, '/');
    TargetParts := SplitString(TargetNorm, '/');
  end
  else
  begin
    BaseNorm := ExcludeTrailingPathDelimiter(NormalizePath(BasePath));
    TargetNorm := ExcludeTrailingPathDelimiter(NormalizePath(TargetPath));
    
    {$IFDEF WINDOWS}
    if HasDriveLetter then
    begin
      // Remove drive letters for comparison
      BaseParts := SplitString(Copy(BaseNorm, 3, Length(BaseNorm)), PathDelim);
      TargetParts := SplitString(Copy(TargetNorm, 3, Length(TargetNorm)), PathDelim);
    end
    else
    {$ENDIF}
    begin
      BaseParts := SplitString(BaseNorm, PathDelim);
      TargetParts := SplitString(TargetNorm, PathDelim);
    end;
  end;
  
  if BaseNorm = TargetNorm then
    Exit('.');
    
  // Find common prefix length
  CommonLength := 0;
  while (CommonLength < Length(BaseParts)) and 
        (CommonLength < Length(TargetParts)) and 
        (BaseParts[CommonLength] = TargetParts[CommonLength]) do
    Inc(CommonLength);
    
  // Calculate number of levels to go up - from the end of BaseParts to CommonLength
  UpLevels := Length(BaseParts) - CommonLength;
  
  // Create result array with correct size
  SetLength(ResultParts, UpLevels + Length(TargetParts) - CommonLength);
  
  // Add '..' for each level we need to go up
  for I := 0 to UpLevels - 1 do
    ResultParts[I] := '..';
    
  // Add the remaining path components from TargetParts
  for I := 0 to Length(TargetParts) - CommonLength - 1 do
    ResultParts[UpLevels + I] := TargetParts[CommonLength + I];
    
  if Length(ResultParts) = 0 then
    Result := '.'
  else begin
    Result := ResultParts[0];
    for I := 1 to High(ResultParts) do
      Result := Result + '/' + ResultParts[I];  // Always use forward slash for consistency
  end;
end;

class function TFileKit.IsSubPath(const ParentPath, ChildPath: string): Boolean;
var
  ParentNorm, ChildNorm: string;
begin
  ParentNorm := IncludeTrailingPathDelimiter(NormalizePath(ParentPath));
  ChildNorm := NormalizePath(ChildPath);
  
  Result := (Length(ChildNorm) > Length(ParentNorm)) and
            (Copy(ChildNorm, 1, Length(ParentNorm)) = ParentNorm);
end;

{ Basic File Content Operations }

class function TFileKit.CountLines(const FilePath: string): Integer;
var
  FileStream: TFileStream;
  Buffer: array[0..4095] of Byte;
  BytesRead: Integer;
  I: Integer;
begin
  Result := 0;
  if not FileExists(FilePath) then
    raise ETidyKitException.CreateFmt('File does not exist: %s', [FilePath]);
    
  FileStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyNone);
  try
    while FileStream.Position < FileStream.Size do
    begin
      BytesRead := FileStream.Read(Buffer, SizeOf(Buffer));
      for I := 0 to BytesRead - 1 do
        if Buffer[I] = 10 then  // LF (Line Feed)
          Inc(Result);
    end;
    
    // If file doesn't end with newline and has content, count last line
    if (FileStream.Size > 0) and (Buffer[BytesRead - 1] <> 10) then
      Inc(Result);
  finally
    FileStream.Free;
  end;
end;

class function TFileKit.GetFirstLine(const FilePath: string): string;
var
  FileStream: TFileStream;
  Buffer: array[0..4095] of Char;
  BytesRead: Integer;
  I: Integer;
begin
  Result := '';
  if not FileExists(FilePath) then
    raise ETidyKitException.CreateFmt('File does not exist: %s', [FilePath]);
    
  FileStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyNone);
  try
    BytesRead := FileStream.Read(Buffer, SizeOf(Buffer));
    if BytesRead > 0 then
    begin
      for I := 0 to BytesRead - 1 do
        if Buffer[I] in [#10, #13] then
          Break
        else
          Result := Result + Buffer[I];
    end;
  finally
    FileStream.Free;
  end;
end;

class function TFileKit.GetLastLine(const FilePath: string): string;
var
  Lines: TStringList;
begin
  Result := '';
  if not FileExists(FilePath) then
    raise ETidyKitException.CreateFmt('File does not exist: %s', [FilePath]);
    
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FilePath);
    if Lines.Count > 0 then
      Result := Lines[Lines.Count - 1];
  finally
    Lines.Free;
  end;
end;

class function TFileKit.IsFileEmpty(const FilePath: string): Boolean;
var
  FileStream: TFileStream;
begin
  if not FileExists(FilePath) then
    raise ETidyKitException.CreateFmt('File does not exist: %s', [FilePath]);
    
  FileStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyNone);
  try
    Result := FileStream.Size = 0;
  finally
    FileStream.Free;
  end;
end;

class function TFileKit.ContainsText(const FilePath, SearchText: string; CaseSensitive: Boolean = False): Boolean;
var
  FileStream: TFileStream;
  Buffer: array[0..4095] of Char;
  BytesRead: Integer;
  SearchLen: Integer;
  Content: string;
begin
  Result := False;
  Content := '';
  if not FileExists(FilePath) then
    raise ETidyKitException.CreateFmt('File does not exist: %s', [FilePath]);
    
  if SearchText = '' then
    Exit(True);
    
  SearchLen := Length(SearchText);
  FileStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Content, FileStream.Size);
    FileStream.Read(Content[1], FileStream.Size);
    
    if CaseSensitive then
      Result := Pos(SearchText, Content) > 0
    else
      Result := Pos(UpperCase(SearchText), UpperCase(Content)) > 0;
  finally
    FileStream.Free;
  end;
end;

{ Simple File Type Detection }

class function TFileKit.IsBinaryFile(const FilePath: string): Boolean;
var
  FileStream: TFileStream;
  Buffer: array[0..4095] of Byte;
  BytesRead, I: Integer;
  NonTextCount: Integer;
  MaxCheck: Integer;
begin
  Result := True;  // Assume binary by default
  
  if not FileExists(FilePath) then
    raise ETidyKitException.CreateFmt('File does not exist: %s', [FilePath]);
    
  FileStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyNone);
  try
    NonTextCount := 0;
    MaxCheck := 512; // Check first 512 bytes
    
    BytesRead := FileStream.Read(Buffer, Min(MaxCheck, SizeOf(Buffer)));
    if BytesRead = 0 then
      Exit(False); // Empty file is considered text
      
    for I := 0 to BytesRead - 1 do
    begin
      if (Buffer[I] < 32) and not (Buffer[I] in [9, 10, 13]) then // Not tab, LF, CR
        Inc(NonTextCount);
    end;
    
    // If more than 10% non-text characters, consider it binary
    Result := (NonTextCount / BytesRead) > 0.1;
  finally
    FileStream.Free;
  end;
end;

class function TFileKit.GetMimeType(const FilePath: string): string;
const
  ExtToMime: array[0..19] of array[0..1] of string = (
    ('.txt', 'text/plain'),
    ('.html', 'text/html'),
    ('.htm', 'text/html'),
    ('.css', 'text/css'),
    ('.js', 'application/javascript'),
    ('.json', 'application/json'),
    ('.xml', 'application/xml'),
    ('.jpg', 'image/jpeg'),
    ('.jpeg', 'image/jpeg'),
    ('.png', 'image/png'),
    ('.gif', 'image/gif'),
    ('.bmp', 'image/bmp'),
    ('.pdf', 'application/pdf'),
    ('.zip', 'application/zip'),
    ('.doc', 'application/msword'),
    ('.docx', 'application/vnd.openxmlformats-officedocument.wordprocessingml.document'),
    ('.xls', 'application/vnd.ms-excel'),
    ('.xlsx', 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'),
    ('.mp3', 'audio/mpeg'),
    ('.mp4', 'video/mp4')
  );
var
  Ext: string;
  I: Integer;
begin
  Result := 'application/octet-stream'; // Default MIME type
  Ext := LowerCase(ExtractFileExt(FilePath));
  
  for I := Low(ExtToMime) to High(ExtToMime) do
    if ExtToMime[I][0] = Ext then
    begin
      Result := ExtToMime[I][1];
      Break;
    end;
end;

class function TFileKit.IsExecutable(const FilePath: string): Boolean;
begin
  {$IFDEF WINDOWS}
  Result := AnsiEndsText('.exe', FilePath) or
            AnsiEndsText('.com', FilePath) or
            AnsiEndsText('.bat', FilePath) or
            AnsiEndsText('.cmd', FilePath);
  {$ELSE} 
  Result := (fpStatFS(PChar(FilePath), @Stats) = 0) and ((Stats.Mode and S_IXUSR) <> 0);
  {$ENDIF}
end;

class function TFileKit.IsHidden(const FilePath: string): Boolean;
begin
  {$IFDEF WINDOWS}
  Result := (Windows.GetFileAttributes(PChar(FilePath)) and DWORD(FILE_ATTRIBUTE_HIDDEN)) <> 0;
  {$ELSE}
  Result := (ExtractFileName(FilePath)[1] = '.');
  {$ENDIF}
end;

{ Basic Space Operations }

class function TFileKit.GetDriveFreeSpace(const Path: string): Int64;
{$IFDEF WINDOWS}
var
  FreeAvailable, TotalSpace: Int64;
  RootPath: array[0..3] of Char;
begin
  Result := -1;
  StrPCopy(RootPath, ExtractFileDrive(Path) + '\');
  if GetDiskFreeSpaceEx(RootPath, FreeAvailable, TotalSpace, nil) then
    Result := FreeAvailable;
end;
{$ELSE}
var
  Stats: TStatFs;
begin
  Result := -1;
  if fpStatFS(PChar(ExtractFilePath(Path)), @Stats) = 0 then
    Result := Int64(Stats.bsize) * Int64(Stats.bavail);
end;
{$ENDIF}

class function TFileKit.GetDriveCapacity(const Path: string): Int64;
{$IFDEF WINDOWS}
var
  FreeAvailable, TotalSpace: Int64;
  RootPath: array[0..3] of Char;
begin
  Result := -1;
  StrPCopy(RootPath, ExtractFileDrive(Path) + '\');
  if GetDiskFreeSpaceEx(RootPath, FreeAvailable, TotalSpace, nil) then
    Result := TotalSpace;
end;
{$ELSE}
var
  Stats: TStatFs;
begin
  Result := -1;
  if fpStatFS(PChar(ExtractFilePath(Path)), @Stats) = 0 then
    Result := Int64(Stats.bsize) * Int64(Stats.blocks);
end;
{$ENDIF}

class function TFileKit.HasEnoughSpace(const Path: string; RequiredBytes: Int64): Boolean;
var
  FreeSpace: Int64;
begin
  FreeSpace := GetDriveFreeSpace(Path);
  Result := (FreeSpace <> -1) and (FreeSpace >= RequiredBytes);
end;

{ Basic File Comparison }

class function TFileKit.AreFilesIdentical(const File1, File2: string): Boolean;
var
  File1Stream, File2Stream: TFileStream;
  Buffer1, Buffer2: array[0..4095] of Byte;
  BytesRead1, BytesRead2: Integer;
  I: Integer;
begin
  Result := False;
  if not FileExists(File1) or not FileExists(File2) then
    Exit;
    
  File1Stream := TFileStream.Create(File1, fmOpenRead or fmShareDenyNone);
  File2Stream := TFileStream.Create(File2, fmOpenRead or fmShareDenyNone);
  try
    while (File1Stream.Position < File1Stream.Size) and (File2Stream.Position < File2Stream.Size) do
    begin
      BytesRead1 := File1Stream.Read(Buffer1, SizeOf(Buffer1));
      BytesRead2 := File2Stream.Read(Buffer2, SizeOf(Buffer2));
      
      if BytesRead1 <> BytesRead2 then
        Exit(False);
      
      for I := 0 to BytesRead1 - 1 do
        if Buffer1[I] <> Buffer2[I] then
          Exit(False);
    end;
    
    // Check if both files are at the end
    Result := (File1Stream.Position = File1Stream.Size) and (File2Stream.Position = File2Stream.Size);
  finally
    File1Stream.Free;
    File2Stream.Free;
  end;
end;

class function TFileKit.GetNewerFile(const File1, File2: string): string;
begin
  if FileExists(File1) and FileExists(File2) then
  begin
    if FileDateToDateTime(FileAge(File1)) > FileDateToDateTime(FileAge(File2)) then
      Result := File1
    else
      Result := File2;
  end
  else if FileExists(File1) then
    Result := File1
  else if FileExists(File2) then
    Result := File2
  else
    raise ETidyKitException.Create('Both files do not exist');
end;

class function TFileKit.GetFileDifferences(const File1, File2: string): TStringArray;
var
  File1Stream, File2Stream: TFileStream;
  Buffer1, Buffer2: array[0..4095] of Byte;
  BytesRead1, BytesRead2: Integer;
  I: Integer;
begin
  SetLength(Result, 0);
  if not FileExists(File1) or not FileExists(File2) then
    Exit;
    
  File1Stream := TFileStream.Create(File1, fmOpenRead or fmShareDenyNone);
  File2Stream := TFileStream.Create(File2, fmOpenRead or fmShareDenyNone);
  try
    while (File1Stream.Position < File1Stream.Size) and (File2Stream.Position < File2Stream.Size) do
    begin
      BytesRead1 := File1Stream.Read(Buffer1, SizeOf(Buffer1));
      BytesRead2 := File2Stream.Read(Buffer2, SizeOf(Buffer2));
      
      if BytesRead1 <> BytesRead2 then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := Format('Difference at position %d: File1 has %d bytes, File2 has %d bytes', [File1Stream.Position - BytesRead1, BytesRead1, BytesRead2]);
      end
      else
      begin
        for I := 0 to BytesRead1 - 1 do
          if Buffer1[I] <> Buffer2[I] then
          begin
            SetLength(Result, Length(Result) + 1);
            Result[High(Result)] := Format('Difference at position %d: File1 has %d, File2 has %d', [File1Stream.Position - BytesRead1 + I, Buffer1[I], Buffer2[I]]);
          end;
      end;
    end;
    
    // Check if both files are at the end
    if (File1Stream.Position < File1Stream.Size) or (File2Stream.Position < File2Stream.Size) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := Format('File1 has %d bytes left, File2 has %d bytes left', [File1Stream.Size - File1Stream.Position, File2Stream.Size - File2Stream.Position]);
    end;
  finally
    File1Stream.Free;
    File2Stream.Free;
  end;
end;

{ Simple File Locking }

var
  LockedFiles: TStringList = nil;

class function TFileKit.LockFile(const FilePath: string): Boolean;
{$IFDEF WINDOWS}
var
  Handle: THandle;
begin
  Result := False;
  if not FileExists(FilePath) then
    Exit;
    
  // Initialize locked files list if needed
  if LockedFiles = nil then
    LockedFiles := TStringList.Create;
    
  // Check if file is already locked
  if LockedFiles.IndexOf(FilePath) >= 0 then
    Exit(False);
    
  Handle := CreateFile(PChar(FilePath),
                      GENERIC_READ or GENERIC_WRITE,
                      0,  // No sharing
                      nil,
                      OPEN_EXISTING,
                      FILE_ATTRIBUTE_NORMAL,
                      0);
                      
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    // Add to locked files list
    LockedFiles.Add(FilePath);
    Result := True;
  end;
end;
{$ELSE}
var
  LockFile: Text;
  LockPath: string;
begin
  Result := False;
  if not FileExists(FilePath) then
    Exit;
    
  // Initialize locked files list if needed
  if LockedFiles = nil then
    LockedFiles := TStringList.Create;
    
  // Check if file is already locked
  if LockedFiles.IndexOf(FilePath) >= 0 then
    Exit(False);
    
  LockPath := FilePath + '.lock';
  try
    AssignFile(LockFile, LockPath);
    Rewrite(LockFile);
    CloseFile(LockFile);
    LockedFiles.Add(FilePath);
    Result := True;
  except
    Result := False;
  end;
end;
{$ENDIF}

class function TFileKit.UnlockFile(const FilePath: string): Boolean;
{$IFDEF WINDOWS}
begin
  Result := False;
  if not FileExists(FilePath) then
    Exit;
    
  if (LockedFiles <> nil) and (LockedFiles.IndexOf(FilePath) >= 0) then
  begin
    LockedFiles.Delete(LockedFiles.IndexOf(FilePath));
    Result := True;
  end;
end;
{$ELSE}
var
  LockPath: string;
begin
  Result := False;
  if not FileExists(FilePath) then
    Exit;
    
  if (LockedFiles <> nil) and (LockedFiles.IndexOf(FilePath) >= 0) then
  begin
    LockPath := FilePath + '.lock';
    if FileExists(LockPath) then
    try
      DeleteFile(LockPath);
      LockedFiles.Delete(LockedFiles.IndexOf(FilePath));
      Result := True;
    except
      Result := False;
    end;
  end;
end;
{$ENDIF}

class function TFileKit.IsFileLocked(const FilePath: string): Boolean;
{$IFDEF WINDOWS}
begin
  if not FileExists(FilePath) then
    Exit(False);
    
  Result := (LockedFiles <> nil) and (LockedFiles.IndexOf(FilePath) >= 0);
end;
{$ELSE}
begin
  if not FileExists(FilePath) then
    Exit(False);
    
  Result := (LockedFiles <> nil) and (LockedFiles.IndexOf(FilePath) >= 0);
end;
{$ENDIF}

{ Path Validation and Sanitization }

class function TFileKit.IsValidFileName(const FileName: string): Boolean;
const
  InvalidChars: set of Char = ['<', '>', ':', '"', '/', '\', '|', '?', '*'];
var
  I: Integer;
begin
  Result := False;
  
  // Check for empty filename
  if (FileName = '') or (FileName = '.') or (FileName = '..') then
    Exit;
    
  // Check length
  if Length(FileName) > 255 then
    Exit;
    
  // Check for invalid characters
  for I := 1 to Length(FileName) do
    if (FileName[I] < #32) or (FileName[I] in InvalidChars) then
      Exit;
      
  Result := True;
end;

class function TFileKit.SanitizeFileName(const FileName: string): string;
const
  InvalidChars: set of Char = ['<', '>', ':', '"', '/', '\', '|', '?', '*'];
var
  I: Integer;
  LastWasUnderscore: Boolean;
  TempResult: string;
begin
  Result := '';
  
  // Handle empty or special cases
  if (FileName = '') or (FileName = '.') or (FileName = '..') then
    Exit('_');
    
  // Replace invalid characters with underscore
  LastWasUnderscore := False;
  TempResult := '';
  
  for I := 1 to Length(FileName) do
  begin
    if (FileName[I] < #32) or (FileName[I] in InvalidChars) then
    begin
      TempResult := TempResult + '_';
      LastWasUnderscore := True;
    end
    else
    begin
      TempResult := TempResult + FileName[I];
      LastWasUnderscore := (FileName[I] = '_');
    end;
  end;
  
  // Trim trailing spaces and dots
  Result := TrimRight(TempResult);
  while (Length(Result) > 0) and (Result[Length(Result)] = '.') do
    SetLength(Result, Length(Result) - 1);
    
  // If result is empty after sanitization, return underscore
  if Result = '' then
    Result := '_';
end;

class function TFileKit.MakeValidPath(const Path: string): string;
var
  Parts: TStringArray;
  ValidParts: TStringArray;
  I: Integer;
  IsAbs: Boolean;
  Drive: string;
  NormPath: string;
begin
  if Path = '' then
    Exit('');
    
  NormPath := NormalizePath(Path);
  
  // Check if path is absolute and get drive letter on Windows
  IsAbs := IsAbsolutePath(NormPath);
  Drive := '';
  {$IFDEF WINDOWS}
  if (Length(NormPath) >= 2) and (NormPath[2] = ':') then
  begin
    Drive := UpperCase(NormPath[1]) + ':';
    Delete(NormPath, 1, 2);
  end;
  {$ENDIF}
  
  // Remove leading path separator for processing
  if (Length(NormPath) > 0) and (NormPath[1] = PathDelim) then
    Delete(NormPath, 1, 1);
    
  // Split path into parts
  Parts := SplitString(NormPath, PathDelim);
  SetLength(ValidParts, 0);
  
  // Process each part
  for I := 0 to High(Parts) do
  begin
    if (Parts[I] = '') or (Parts[I] = '.') then
      Continue
    else if Parts[I] = '..' then
    begin
      if (Length(ValidParts) > 0) and (ValidParts[High(ValidParts)] <> '..') then
        SetLength(ValidParts, Length(ValidParts) - 1)
      else if not IsAbs then
        begin
          SetLength(ValidParts, Length(ValidParts) + 1);
          ValidParts[High(ValidParts)] := '..';
        end;
    end
    else
    begin
      SetLength(ValidParts, Length(ValidParts) + 1);
      ValidParts[High(ValidParts)] := SanitizeFileName(Parts[I]);
    end;
  end;
  
  // Combine parts back into path
  if Length(ValidParts) = 0 then
  begin
    if IsAbs then
    begin
      {$IFDEF WINDOWS}
      if Drive <> '' then
        Result := Drive + PathDelim
      else
        Result := PathDelim;
      {$ELSE}
      Result := PathDelim;
      {$ENDIF}
    end
    else
      Result := '.';
  end
  else
  begin
    Result := ValidParts[0];
    for I := 1 to High(ValidParts) do
      Result := Result + PathDelim + ValidParts[I];
      
    // Add drive and path separator for absolute paths
    if IsAbs then
    begin
      {$IFDEF WINDOWS}
      if Drive <> '' then
        Result := Drive + PathDelim + Result
      else
        Result := PathDelim + Result;
      {$ELSE}
      Result := PathDelim + Result;
      {$ENDIF}
    end;
  end;
end;

class function TFileKit.IsPathTooLong(const Path: string): Boolean;
begin
  {$IFDEF WINDOWS}
  Result := Length(Path) > 260;  // MAX_PATH
  {$ELSE}
  Result := Length(Path) > 4096; // PATH_MAX
  {$ENDIF}
end;

{ Simple Directory Summary }

class function TFileKit.GetDirectoryInfo(const Path: string): TDirectoryInfo;
var
  SearchRec: TSearchRec;
  OldestTime, NewestTime: TDateTime;
  LargestSize: Int64;
begin
  Result.FileCount := 0;
  Result.DirectoryCount := 0;
  Result.TotalSize := 0;
  Result.OldestFile := '';
  Result.NewestFile := '';
  Result.LargestFile := '';
  
  if not DirectoryExists(Path) then
    Exit;
    
  OldestTime := MaxDateTime;
  NewestTime := 0;
  LargestSize := 0;
  
  if FindFirst(IncludeTrailingPathDelimiter(Path) + '*', faAnyFile, SearchRec) = 0 then
  try
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        if (SearchRec.Attr and faDirectory) <> 0 then
          Inc(Result.DirectoryCount)
        else
        begin
          Inc(Result.FileCount);
          Result.TotalSize := Result.TotalSize + SearchRec.Size;
          
          // Update oldest file
          if FileDateToDateTime(SearchRec.Time) < OldestTime then
          begin
            OldestTime := FileDateToDateTime(SearchRec.Time);
            Result.OldestFile := SearchRec.Name;
          end;
          
          // Update newest file
          if FileDateToDateTime(SearchRec.Time) > NewestTime then
          begin
            NewestTime := FileDateToDateTime(SearchRec.Time);
            Result.NewestFile := SearchRec.Name;
          end;
          
          // Update largest file
          if SearchRec.Size > LargestSize then
          begin
            LargestSize := SearchRec.Size;
            Result.LargestFile := SearchRec.Name;
          end;
        end;
      end;
    until FindNext(SearchRec) <> 0;
  finally
    FindClose(SearchRec);
  end;
end;

{ Basic File Patterns }

class function TFileKit.MatchesPattern(const FileName, Pattern: string): Boolean;
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

class function TFileKit.FindFirstMatch(const Directory, Pattern: string): string;
var
  SearchRec: TSearchRec;
begin
  Result := '';
  if FindFirst(IncludeTrailingPathDelimiter(Directory) + Pattern, faAnyFile, SearchRec) = 0 then
  try
    Result := SearchRec.Name;
  finally
    FindClose(SearchRec);
  end;
end;

class function TFileKit.CountMatches(const Directory, Pattern: string): Integer;
var
  SearchRec: TSearchRec;
  DirCount: Integer;
begin
  DirCount := 0;
  if FindFirst(IncludeTrailingPathDelimiter(Directory) + Pattern, faAnyFile, SearchRec) = 0 then
  try
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        Inc(DirCount);
    until FindNext(SearchRec) <> 0;
  finally
    FindClose(SearchRec);
  end;
  Result := DirCount;
end;

finalization
  if Assigned(LockedFiles) then
    FreeAndNil(LockedFiles);
end. 
