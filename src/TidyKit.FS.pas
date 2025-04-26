unit TidyKit.FS;

{$mode objfpc}{$H+}{$J-}

interface

uses
  {$IFDEF UNIX}
  BaseUnix,  // Unix-specific system calls for low-level file operations
  Unix,      // Unix-specific types and constants
  UnixType,  // Additional Unix system types
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows,   // Windows API units
  {$ENDIF}
  Classes, SysUtils, DateUtils, TidyKit.Core, zipper, libtar, StrUtils, Math;

const
  DEBUG_MODE = False; // Enable debugging output
  
  {$IFDEF WINDOWS}
  // Windows API flag constants
  SYMBOLIC_LINK_FLAG_DIRECTORY = $1;                  // Target is a directory
  SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE = $2;  // Allow non-admin symlink creation
  FILE_FLAG_OPEN_REPARSE_POINT = $00200000;           // Open symlink itself, not target
  FILE_NAME_NORMALIZED = $0;                          // Get normalized path without . or .. components
  {$ENDIF}
  
  {$IFDEF UNIX}
  // Unix/Linux file system constants
  PATH_MAX = 4096;  // Maximum length of a file path (includes null terminator)
  
  // Linux file permission constants
  S_IFMT   = $F000;  // Bitmask for the file type bitfields
  S_IFLNK  = $A000;  // Symbolic link
  S_IFREG  = $8000;  // Regular file
  S_IFDIR  = $4000;  // Directory
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
    TDirectoryInfo
    --------------
    Contains summary information about a directory's contents, including
    count of files and directories, total size, and information about
    oldest, newest, and largest files.
  }
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
    
    @warning None
  }
  TFilePathArray = array of string;

  { 
    TFileSortOrder 
    ---------------
    Enumerates the different ways files and directories can be sorted when listed.
    This allows developers to specify the desired sorting criteria for better organization.
    
    @warning For directories, size sorting options (fsSize and fsSizeDesc) have no effect and 
             are equivalent to using fsNone
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
    
    @warning None
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
    EFileSystemError 
    ----------------
    This exception class is used to handle errors related to file system operations.
    It provides a unified way to report and manage errors that occur during file handling.
  }
  EFileSystemError = class(Exception);


// Platform-specific external function declarations
{$IFDEF WINDOWS}
// Windows API function declarations for symbolic link operations

{ @description Helper function | Creates a symbolic link file that points to another file or directory
  
  @usage Used internally by TFileKit.CreateSymLink to implement Windows symbolic link creation
  
  @param lpSymlinkFileName Path where the symbolic link should be created
  @param lpTargetFileName Path to the target file or directory that the link points to
  @param dwFlags Options (SYMBOLIC_LINK_FLAG_DIRECTORY for directory targets, 
                SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE for non-admin users in Developer Mode)
  
  @returns TRUE if the function succeeds, FALSE otherwise (use GetLastError for error details)
  
  @warning By default requires Administrator privileges on Windows
           Windows 10 with Developer Mode enabled allows non-admin symlink creation
           Returns FALSE and sets the appropriate Windows error code on failure
  
  @example
    Success := CreateSymbolicLink('C:\LinkToFile.txt', 'C:\TargetFile.txt', 0);
}
function CreateSymbolicLink(lpSymlinkFileName, lpTargetFileName: LPCSTR; dwFlags: DWORD): BOOL; 
  stdcall; external 'kernel32.dll' name 'CreateSymbolicLinkA';

{ @description Helper function | Gets the final path of a file, resolving any symbolic links in the path
  
  @usage Used internally by TFileKit.ResolveSymLink to get the target of a symbolic link
  
  @param hFile Handle to an open file (must have appropriate access rights)
  @param lpszFilePath Buffer to receive the file path
  @param cchFilePath Size of the buffer in characters
  @param dwFlags Format options (FILE_NAME_NORMALIZED to get a normalized path)
  
  @returns The length of the file path if successful, 0 otherwise (use GetLastError for error details)
  
  @warning Requires an open file handle with appropriate access rights
           Path may be returned with "\\?\" prefix on Windows
           Buffer must be large enough to hold the complete path
  
  @example
    PathLength := GetFinalPathNameByHandle(FileHandle, PathBuffer, BufferSize, FILE_NAME_NORMALIZED);
}
function GetFinalPathNameByHandle(hFile: THandle; lpszFilePath: LPSTR; cchFilePath: DWORD; dwFlags: DWORD): DWORD; 
  stdcall; external 'kernel32.dll' name 'GetFinalPathNameByHandleA';
{$ENDIF}

{$IFDEF UNIX}
// Helper functions for Unix file system operations
{ @description Helper function | Checks if a file mode represents a directory

  @usage Used internally to determine directory status from Unix file stats

  @param Mode Unix file mode value

  @returns True if the mode is for a directory

  @warning None
}
function S_ISDIR(Mode: mode_t): Boolean;

{ @description Helper function | Checks if a file mode represents a symbolic link

  @usage Used internally to determine symbolic link status from Unix file stats

  @param Mode Unix file mode value

  @returns True if the mode is for a symbolic link

  @warning None
}
function S_ISLNK(Mode: mode_t): Boolean;
{$ENDIF}


type 

  TFileKit = class
  private
    { @description Creates a detailed information record for a file or directory
      
      @usage Internal helper used by search and listing methods to collect file information
      
      @param APath The file or directory path to analyze
      
      @returns A populated TSearchResult record with details like path, size, timestamps, and attributes
      
      @warning Only retrieves basic file information; some advanced attributes may not be available
                Path is normalized which could resolve symbolic links
                Returns zero values for size/dates if file information cannot be retrieved
      
      @example
        Result := CreateSearchResult('C:\Windows\notepad.exe');
        // Returns a TSearchResult with details about notepad.exe
    }
    class function CreateSearchResult(const APath: string): TSearchResult;

    { @description Converts a platform-specific file time value to TDateTime format
      
      @usage Internal helper for normalizing timestamp formats across platforms
      
      @param FileTime A platform-specific file time structure (FILETIME on Windows, TDateTime on Unix)
      
      @returns The converted value as a TDateTime value, normalized for the local time zone
               Returns 0 if the conversion fails
      
      @warning On Windows, performs multiple conversions that may lose precision
                Windows FILETIME and Delphi TDateTime have different base dates and resolutions
                On Unix systems, simply passes through the existing TDateTime value
      
      @example
        DateTime := FileTimeToDateTime(FindData.ftLastWriteTime);
        // Converts a Windows FILETIME to TDateTime
    }
    class function FileTimeToDateTime(const FileTime: {$IFDEF WINDOWS}FILETIME{$ELSE}TDateTime{$ENDIF}): TDateTime;
    
    { @description Performs simple wildcard pattern matching for filenames
    
      @usage Internal helper used by file search and listing methods to match filenames against patterns
      
      @param FileName The filename to check against the pattern
      @param Pattern The pattern to match against (supports '*' at start, end, or both)
      
      @returns True if the filename matches the pattern, False otherwise
      
      @warning Uses case-insensitive comparison
                Limited wildcard support: only handles '*' (not '?')
                Only supports patterns with a single '*' at start, end, or both ends
                Complex patterns will not work as expected
      
      @example
        if MatchPattern('document.txt', '*.txt') then // Returns True
        if MatchPattern('image.jpg', 'img*') then // Returns False
    }
    class function MatchPattern(const FileName, Pattern: string): Boolean;

    { @description Loads the entire content of a text file into a string
    
      @usage Internal helper used by text manipulation methods (ReadTextFile, etc.)
      
      @param APath The path of the file to read
      
      @returns The complete text content of the file as a string
               Returns an empty string if the file doesn't exist or cannot be read
      
      @warning No exception handling for read errors
                Loads the entire file content into memory, which may be problematic for very large files
                No text encoding detection or conversion
      
      @example
        Content := LoadTextFromFile('C:\config.ini');
        // Returns the complete content of config.ini
    }
    class function LoadTextFromFile(const APath: string): string;
    
    { @description Saves a string to a text file, creating the directory structure if needed
    
      @usage Internal helper used by text modification methods (WriteTextFile, etc.)
      
      @param APath The path where the file should be created or overwritten
      @param AContent The string content to write to the file
      
      @warning Always creates parent directories if they don't exist
                Always overwrites existing file without confirmation
                No exception handling for write errors
                No text encoding specification
      
      @example
        SaveTextToFile('C:\logs\app.log', 'Application started');
        // Creates or overwrites app.log with the specified content
    }
    class procedure SaveTextToFile(const APath: string; const AContent: string);
  public
    { @description Reads the entire content of a file and returns it as a string
      
      @usage Use when you need to load the complete text content of a file into memory
      
      @param APath The path to the file to read
      
      @returns The entire content of the file as a string
      
      @warning May cause memory issues with extremely large files
                Returns empty string if file doesn't exist instead of raising an exception
      
      @example
        Content := TFileKit.ReadTextFile('C:\config.txt');
        // Returns the entire content of config.txt as a string
    }
    class function ReadTextFile(const APath: string): string;

    { @description Writes content to a file, overwriting any existing content
      
      @usage Use when you need to save text content to a file
      
      @param APath The path to the file to write
      @param AContent The string content to write to the file
      
      @warning Will overwrite existing files without confirmation
                Creates parent directories if they don't exist
      
      @example
        TFileKit.WriteTextFile('C:\logs\app.log', 'Application started');
        // Creates or overwrites app.log with the specified content
    }
    class procedure WriteTextFile(const APath: string; const AContent: string);

    { @description Deletes a file from the file system
      
      @usage Use when you need to remove a file permanently
      
      @param APath The path to the file to delete
      
      @warning Does not raise exceptions if file doesn't exist
                No confirmation or recovery mechanism
                Operation cannot be undone
      
      @example
        TFileKit.DeleteFile('C:\temp\temporary.tmp');
        // Removes the specified file from the file system
    }
    class procedure DeleteFile(const APath: string);

    { @description Copies a file from one location to another
      
      @usage Use when you need to duplicate a file while preserving attributes and timestamps
      
      @param ASourcePath The path to the source file
      @param ADestPath The destination path where the file should be copied
      
      @warning Will create destination directory structure if it doesn't exist
                Will overwrite destination without confirmation if it exists
                May fail if destination is read-only or in use
      
      @example
        TFileKit.CopyFile('C:\original.txt', 'D:\backup\original.txt');
        // Creates a copy of original.txt at the specified destination
    }
    class procedure CopyFile(const ASourcePath, ADestPath: string);

    { @description Moves a file from one location to another
      
      @usage Use when you need to relocate a file rather than copying it
      
      @param ASourcePath The path to the source file
      @param ADestPath The destination path where the file should be moved
      
      @warning Will create destination directory structure if it doesn't exist
                Will overwrite destination without confirmation if it exists
                Falls back to copy+delete if simple rename fails
                Verifies sizes match before deleting source in copy+delete scenario
      
      @example
        TFileKit.MoveFile('C:\temp.txt', 'D:\archive\temp.txt');
        // Moves temp.txt from C: to D:\archive
    }
    class procedure MoveFile(const ASourcePath, ADestPath: string);
    
    { @description Appends text to the end of a file
      
      @usage Use when you need to add content to the end of a file without rewriting it
             Particularly useful for log files and incremental data collection
      
      @param APath The path to the file to append to
      @param AText The text to append to the file
      
      @warning Creates new file if it doesn't exist
                May cause issues with newlines if not handled properly in AText
                Requires careful handling for non-ASCII text encodings
      
      @example
        TFileKit.AppendText('C:\logs\app.log', 'New log entry at ' + DateTimeToStr(Now));
        // Adds the text to the end of the log file
    }
    class procedure AppendText(const APath, AText: string);

    { @description Prepends text to the beginning of a file
      
      @usage Use when you need to add content to the start of a file, such as headers or initial configuration
      
      @param APath The path to the file to prepend to
      @param AText The text to prepend to the file
      
      @warning Creates new file if it doesn't exist
                Requires loading entire file into memory, potentially inefficient for large files
                May cause issues with newlines if not handled properly in AText
      
      @example
        TFileKit.PrependText('C:\logs\app.log', '=== Log started ===' + LineEnding);
        // Adds the text to the beginning of the log file
    }
    class procedure PrependText(const APath, AText: string);

    { @description Replaces all occurrences of text in a file
      
      @usage Use when you need to find and replace text across an entire file, like updating configuration values
      
      @param APath The path to the file to modify
      @param OldText The text to find and replace
      @param NewText The text to replace with
      
      @warning Requires loading entire file into memory, potentially inefficient for large files
                Case-sensitive by default (uses StringReplace)
                Does nothing if file doesn't exist
      
      @example
        TFileKit.ReplaceText('C:\config.ini', 'DEBUG=FALSE', 'DEBUG=TRUE');
        // Replaces all instances of 'DEBUG=FALSE' with 'DEBUG=TRUE'
    }
    class procedure ReplaceText(const APath, OldText, NewText: string);
    
    { @description Creates a new directory
      
      @usage Use when you need to create a directory and its parent directories if they don't exist
      
      @param APath The path where the directory should be created
      
      @warning May fail due to permissions or invalid path characters
                No error is raised if the directory already exists
      
      @example
        TFileKit.CreateDirectory('C:\Projects\MyApp\logs');
        // Creates the logs directory and any non-existent parent directories
    }
    class procedure CreateDirectory(const APath: string);

    { @description Deletes a directory and optionally its contents
      
      @usage Use when you need to remove a directory and its contents permanently
      
      @param APath The path to the directory to delete
      @param Recursive If True (default), deletes all subdirectories and files within the directory
      
      @warning No confirmation or recovery mechanism; operation is irreversible
                Without Recursive=True, will fail if directory is not empty
                With Recursive=True, performs dangerous recursive deletion; use with extreme caution
                May fail due to permissions or if files/directories are in use
      
      @example
        TFileKit.DeleteDirectory('C:\temp\cache', True);
        // Deletes the cache directory and all its contents
    }
    class procedure DeleteDirectory(const APath: string; const Recursive: Boolean = True);

    { @description Ensures a directory exists, creating it and any necessary parent directories if they don't exist
      
      @usage Use when you need to make sure a directory path exists before writing a file to it
      
      @param APath The full path to the directory to ensure exists (including potential parent directories)
      
      @warning Creates the full directory tree if needed
                No specific permissions checking before attempting creation
                Relies on underlying ForceDirectories behavior
      
      @example
        TFileKit.EnsureDirectory('C:\Users\username\AppData\Local\MyApp\data');
        // Creates the complete directory path if any part doesn't exist
    }
    class procedure EnsureDirectory(const APath: string);

    { @description Lists all directories in a specified path, optionally recursively
      
      @usage Use when you need to enumerate directories that match specific criteria (pattern, recursion)
      
      @param APath The path to search in
      @param Pattern File pattern to match directory names (e.g., '*' for all, 'temp*' for names starting with temp)
      @param Recursive If True, includes subdirectories in the search
      @param SortOrder Specifies how to sort the resulting list of directories (name, date)
      
      @returns A dynamic array (TFilePathArray) of full directory paths matching the criteria
      
      @warning May be slow on large directory trees, especially when Recursive=True
                Avoids following symbolic links to prevent infinite loops
                Returns an empty array if the base path doesn't exist or no directories match
      
      @example
        Dirs := TFileKit.ListDirectories('C:\Projects', '*', False, fsName);
        // Returns a sorted array of all top-level directories in C:\Projects
    }
    class function ListDirectories(const APath: string; 
      const Pattern: string = '*'; 
      const Recursive: Boolean = False;
      const SortOrder: TFileSortOrder = fsNone): TFilePathArray;

    { @description Lists all files in a specified path, optionally recursively
      
      @usage Use when you need to enumerate files that match a specific pattern (e.g., '*.txt', 'image?.jpg')
      
      @param APath The path to search in
      @param Pattern File pattern to match filenames (e.g., '*.log', 'data_*.csv')
      @param Recursive If True, includes files in subdirectories
      @param SortOrder Specifies how to sort the resulting list of files (name, date, size)
      
      @returns A dynamic array (TFilePathArray) of full file paths matching the criteria
      
      @warning May be slow on large directory trees, especially when Recursive=True
                Returns an empty array if the base path doesn't exist or no files match
                Pattern matching is basic wildcard matching
      
      @example
        Files := TFileKit.ListFiles('C:\Documents', '*.pdf', True, fsDateDesc);
        // Returns an array of all PDF files in C:\Documents and its subdirectories, sorted newest first
    }
    class function ListFiles(const APath: string; 
      const Pattern: string = '*'; 
      const Recursive: Boolean = False;
      const SortOrder: TFileSortOrder = fsNone): TFilePathArray;
    
    { @description Changes the extension of a file path string
      
      @usage Use when you need to modify the extension part of a path string, e.g., before renaming or creating a related file
      
      @param APath The original file path string
      @param NewExt The new extension (can include or omit the leading dot)
      
      @returns The path string with the new extension applied
      
      @warning Only changes the path string in memory; does not rename the actual file on disk
                Automatically adds a dot if NewExt doesn't start with one
                If APath has no extension, NewExt is simply appended
      
      @example
        NewPath := TFileKit.ChangeExtension('C:\document.doc', '.pdf');
        // Returns: 'C:\document.pdf'
        BackupPath := TFileKit.ChangeExtension('C:\data.csv', 'bak');
        // Returns: 'C:\data.bak'
    }
    class function ChangeExtension(const APath, NewExt: string): string;

    { @description Extracts the filename (including extension) from a full path string
      
      @usage Use when you need just the name part of a file path, discarding the directory information
      
      @param APath The full file path string
      
      @returns The filename with its extension
      
      @warning Returns an empty string if APath is empty or represents a root directory
      
      @example
        Name := TFileKit.GetFileName('C:\Users\docs\report.docx');
        // Returns: 'report.docx'
    }
    class function GetFileName(const APath: string): string;

    { @description Extracts the filename from a path string, excluding the extension
      
      @usage Use when you need the base name of a file without its type identifier
      
      @param APath The full file path string
      
      @returns The filename without its extension
      
      @warning Returns an empty string if APath is empty or represents a root directory
      
      @example
        BaseName := TFileKit.GetFileNameWithoutExt('C:\images\photo.jpeg');
        // Returns: 'photo'
    }
    class function GetFileNameWithoutExt(const APath: string): string;

    { @description Gets the directory part of a full path string
      
      @usage Use when you need the path to the folder containing a file or directory
      
      @param APath The full file or directory path string
      
      @returns The path of the containing directory
      
      @warning Behavior might differ slightly based on whether APath is a file or directory and if it has a trailing delimiter
                Uses ExpandFileName internally, so relative paths are resolved
      
      @example
        Dir := TFileKit.GetDirectory('C:\Users\docs\report.docx');
        // Returns: 'docs'
        ParentDir := TFileKit.GetDirectory('C:\Users\docs\');
        // Returns: 'Users'
    }
    class function GetDirectory(const APath: string): string;

    { @description Gets the extension part of a file path string
      
      @usage Use when you need to determine the type of a file based on its extension
      
      @param APath The full file path string
      
      @returns The file extension, including the leading dot (e.g., '.txt', '.docx')
               Returns an empty string if the path has no extension
      
      @warning Case-sensitive based on the input path string
      
      @example
        Ext := TFileKit.GetExtension('C:\archive\backup.zip');
        // Returns: '.zip'
    }
    class function GetExtension(const APath: string): string;
    
    { @description Checks if a file or directory exists at the specified path
      
      @usage Use to verify the existence of a file or directory before attempting operations on it
      
      @param APath The path to check
      
      @returns True if a file or directory exists at the path, False otherwise
      
      @warning Does not distinguish between files and directories (use DirectoryExists for that)
                May return False due to permission issues even if the path exists
      
      @example
        if TFileKit.Exists('C:\config.ini') then
          // Proceed to read the file
    }
    class function Exists(const APath: string): Boolean;

    { @description Checks if a directory exists at the specified path
      
      @usage Use specifically to verify if a path points to an existing directory
      
      @param APath The path to check
      
      @returns True if a directory exists at the path, False otherwise (including if it's a file)
      
      @warning May return False due to permission issues even if the directory exists
      
      @example
        if TFileKit.DirectoryExists('C:\Users\Public\Documents') then
          // Proceed with directory operations
    }
    class function DirectoryExists(const APath: string): Boolean;

    { @description Gets the size of a file in bytes
      
      @usage Use to determine the size of a file, e.g., for progress indicators or validation
      
      @param APath The path to the file
      
      @returns The size of the file in bytes (Int64). Returns 0 if the file does not exist or is empty.
      
      @warning Returns 0 for non-existent files instead of raising an error
                May return 0 if there are permission issues accessing the file
      
      @example
        FileSize := TFileKit.GetSize('C:\data\large_dataset.bin');
        // Returns the size of the file in bytes
    }
    class function GetSize(const APath: string): Int64;

    { @description Gets the creation timestamp of a file or directory
      
      @usage Use to find out when a file or directory was originally created
      
      @param APath The path to the file or directory
      
      @returns The creation timestamp as a TDateTime value. Returns 0 if the path doesn't exist or the timestamp cannot be retrieved.
      
      @warning Timestamp resolution and availability depend on the operating system and file system
                On Unix, often returns the last status change time (ctime) instead of true creation time
      
      @example
        Created := TFileKit.GetCreationTime('C:\boot.ini');
        // Returns the creation date and time
    }
    class function GetCreationTime(const APath: string): TDateTime;

    { @description Gets the last access timestamp of a file or directory
      
      @usage Use to find out when a file or directory was last accessed (read or executed)
      
      @param APath The path to the file or directory
      
      @returns The last access timestamp as a TDateTime value. Returns 0 if the path doesn't exist or the timestamp cannot be retrieved.
      
      @warning Last access time updates might be disabled on some systems (especially Windows) for performance reasons
                Timestamp resolution depends on the OS and file system
      
      @example
        LastRead := TFileKit.GetLastAccessTime('C:\Users\user\document.txt');
        // Returns the last access date and time
    }
    class function GetLastAccessTime(const APath: string): TDateTime;

    { @description Gets the last modification (write) timestamp of a file or directory
      
      @usage Use to find out when the content of a file or directory was last changed
      
      @param APath The path to the file or directory
      
      @returns The last modification timestamp as a TDateTime value. Returns 0 if the path doesn't exist or the timestamp cannot be retrieved.
      
      @warning Timestamp resolution depends on the OS and file system
      
      @example
        Modified := TFileKit.GetLastWriteTime('C:\config.ini');
        // Returns the last modification date and time
    }
    class function GetLastWriteTime(const APath: string): TDateTime;

    { @description Gets the attributes of a file or directory
      
      @usage Use to retrieve detailed properties like ReadOnly, Hidden, Directory, SymLink, etc.
      
      @param APath The path to the file or directory
      
      @returns A TFileAttributes record populated with the attributes. Fields are initialized to default values (False, empty strings) if attributes cannot be retrieved.
      
      @warning The specific attributes available (Owner, Group, Permissions) vary between Windows and Unix
                Retrieval might fail due to permission issues
      
      @example
        Attrs := TFileKit.GetAttributes('C:\Windows\System32\kernel32.dll');
        if Attrs.ReadOnly then WriteLn('File is read-only');
        if Attrs.System then WriteLn('File is a system file');
    }
    class function GetAttributes(const APath: string): TFileAttributes;

    { @description Attempts to determine if a file contains primarily text data
      
      @usage Use as a heuristic check to guess if a file is text-based before trying to read it as text
      
      @param APath The path to the file
      
      @returns True if the file appears to be text (based on checking a sample for control characters), False otherwise or if the file doesn't exist/can't be read.
      
      @warning This is a heuristic and not foolproof; it might misclassify some binary files as text or vice-versa
                Only checks a small initial portion of the file (MaxBytesToCheck)
                Empty files are considered text
      
      @example
        if TFileKit.IsTextFile('C:\data.log') then
          Content := TFileKit.ReadTextFile('C:\data.log');
    }
    class function IsTextFile(const APath: string): Boolean;

    { @description Attempts to detect the text encoding of a file by checking for a Byte Order Mark (BOM)
      
      @usage Use to guess the encoding (UTF-8, UTF-16LE/BE, UTF-32LE/BE) before reading a text file
      
      @param APath The path to the file
      
      @returns A string representing the detected encoding ('UTF-8', 'UTF-16LE', 'UTF-16BE', 'UTF-32LE', 'UTF-32BE'). Returns 'ASCII' as the default if no BOM is found or the file cannot be read.
      
      @warning Only detects encodings based on the presence of a BOM at the very beginning of the file
                Files saved without a BOM (like many UTF-8 files) will be reported as 'ASCII'
                Only checks the first few bytes (MaxBytesToCheck)
      
      @example
        Encoding := TFileKit.GetFileEncoding('C:\unicode_file.txt');
        // Might return 'UTF-16LE' or 'UTF-8' depending on the file's BOM
    }
    class function GetFileEncoding(const APath: string): string;
    
    { @description Searches for files matching a pattern within a directory, optionally recursively
      
      @usage Use to find files based on name patterns and retrieve detailed information about each match
      
      @param APath The directory path to start the search from
      @param APattern The file pattern to match (e.g., '*.log', 'report_*.docx')
      @param Recursive If True (default is False), searches subdirectories as well
      
      @returns A dynamic array (TSearchResults) containing detailed TSearchResult records for each matching file
      
      @warning Can be slow on large directory structures, especially with recursion
                Pattern matching is basic wildcard matching
                Returns an empty array if the path doesn't exist or no files match
      
      @example
        Results := TFileKit.SearchFiles('C:\Projects', '*.pas', True);
        for Result in Results do
          WriteLn('Found: ', Result.FullPath, ' Size: ', Result.Size);
    }
    class function SearchFiles(const APath, APattern: string; const Recursive: Boolean = False): TSearchResults;

    { @description Searches for files matching a pattern strictly within the specified directory (and optionally subdirectories)
      
      @usage Internal helper for SearchFiles, but can be used directly if the input path is guaranteed to be a directory
      
      @param ADirectory The directory path to search within
      @param APattern The file pattern to match
      @param Recursive If True, searches subdirectories
      
      @returns A dynamic array (TSearchResults) of matching files
      
      @warning Assumes ADirectory exists; behavior is undefined if it doesn't
                Avoids following symbolic links during recursion to prevent cycles
      
      @example
        // Assuming 'C:\logs' exists
        LogFiles := TFileKit.SearchFilesIn('C:\logs', '*.log', False);
    }
    class function SearchFilesIn(const ADirectory, APattern: string; const Recursive: Boolean = False): TSearchResults;

    { @description Finds the file with the most recent modification date matching a pattern
      
      @usage Use to identify the latest version of a file based on timestamp
      
      @param APath The directory path to search in
      @param APattern The file pattern to match
      @param Recursive If True, searches subdirectories
      
      @returns The full path to the most recently modified file matching the criteria. Returns an empty string if no matching files are found.
      
      @warning If multiple files have the exact same latest timestamp, the one found first (OS-dependent order) is returned
                Can be slow on large directories, especially with recursion
      
      @example
        LatestLog := TFileKit.FindLastModifiedFile('C:\App\Logs', 'app_*.log', True);
        // Returns the path to the newest log file
    }
    class function FindLastModifiedFile(const APath, APattern: string; const Recursive: Boolean = False): string;

    { @description Finds the file with the oldest modification date matching a pattern
      
      @usage Use to identify the earliest version of a file based on timestamp
      
      @param APath The directory path to search in
      @param APattern The file pattern to match
      @param Recursive If True, searches subdirectories
      
      @returns The full path to the oldest modified file matching the criteria. Returns an empty string if no matching files are found.
      
      @warning If multiple files have the exact same oldest timestamp, the one found first (OS-dependent order) is returned
                Can be slow on large directories, especially with recursion
      
      @example
        OldestBackup := TFileKit.FindFirstModifiedFile('D:\Backups', 'db_backup_*.bak', False);
        // Returns the path to the oldest backup file in that directory
    }
    class function FindFirstModifiedFile(const APath, APattern: string; const Recursive: Boolean = False): string;

    { @description Finds the largest file (by size in bytes) matching a pattern
      
      @usage Use to identify the file consuming the most disk space within a set
      
      @param APath The directory path to search in
      @param APattern The file pattern to match
      @param Recursive If True, searches subdirectories
      
      @returns The full path to the largest file matching the criteria. Returns an empty string if no matching files are found.
      
      @warning If multiple files have the exact same largest size, the one found first (OS-dependent order) is returned
                Can be slow on large directories, especially with recursion
      
      @example
        LargestVideo := TFileKit.FindLargestFile('C:\Videos', '*.mp4', True);
        // Returns the path to the largest MP4 file
    }
    class function FindLargestFile(const APath, APattern: string; const Recursive: Boolean = False): string;

    { @description Finds the smallest file (by size in bytes) matching a pattern
      
      @usage Use to identify the smallest file, potentially for finding empty or near-empty files
      
      @param APath The directory path to search in
      @param APattern The file pattern to match
      @param Recursive If True, searches subdirectories
      
      @returns The full path to the smallest file matching the criteria. Returns an empty string if no matching files are found.
      
      @warning If multiple files have the exact same smallest size, the one found first alphabetically is returned
                Can be slow on large directories, especially with recursion
      
      @example
        SmallestConfig := TFileKit.FindSmallestFile('C:\Configs', '*.ini', False);
        // Returns the path to the smallest INI file
    }
    class function FindSmallestFile(const APath, APattern: string; const Recursive: Boolean = False): string;
    
    { @description Gets the current user's home directory path
      
      @usage Use to locate user-specific configuration files, documents, or application data directories
      
      @returns The full path to the user's home directory (e.g., 'C:\Users\username' on Windows, '/home/username' on Linux). Falls back to GetCurrentDir if the home directory cannot be determined.
      
      @warning Relies on environment variables ('USERPROFILE' on Windows, 'HOME' on Unix), which might not be set in all environments
      
      @example
        UserConfigPath := TFileKit.CombinePaths(TFileKit.GetUserDir, '.myapp\config.json');
        // Constructs a path within the user's home directory
    }
    class function GetUserDir: string;

    { @description Gets the current working directory of the application
      
      @usage Use when you need to know the directory from which the application was launched or where relative paths are resolved by default
      
      @returns The full path to the current working directory
      
      @warning The current directory can be changed during runtime (e.g., using ChDir), so it might not always be the application's startup directory
      
      @example
        CurrentPath := TFileKit.GetCurrentDir;
        // Returns the application's current working directory path
    }
    class function GetCurrentDir: string;

    { @description Gets the system's designated temporary directory path
      
      @usage Use to find a standard location for creating temporary files or directories
      
      @returns The full path to the system's temporary directory (e.g., 'C:\Users\user\AppData\Local\Temp' on Windows, '/tmp' on Linux)
      
      @warning Relies on system settings or environment variables (TEMP, TMP) which might vary
                Ensure you have write permissions to this directory
      
      @example
        TempFilePath := TFileKit.CombinePaths(TFileKit.GetTempDir, 'my_temp_data.tmp');
        // Constructs a path within the system temporary directory
    }
    class function GetTempDir: string;

    { @description Gets the parent directory of a given path
      
      @usage Use to navigate up the directory hierarchy from a file or directory path
      
      @param APath The file or directory path
      
      @returns The full path of the parent directory. Returns the root or drive if APath is already a root/drive.
      
      @warning Behavior with root paths ('C:\', '/') might vary slightly
                Uses ExpandFileName and ExcludeTrailingPathDelimiter internally
      
      @example
        Parent := TFileKit.GetParentDir('C:\Users\docs\report.docx');
        // Returns: 'C:\Users\docs' 
        GrandParent := TFileKit.GetParentDir(Parent);
        // Returns: 'C:\Users'
    }
    class function GetParentDir(const APath: string): string;
    
    { @description Safely combines two path components into a single path string
      
      @usage Use to construct full paths from directory and file/subdirectory names without worrying about missing or extra path delimiters
      
      @param APath1 The first path component (typically a directory)
      @param APath2 The second path component (typically a file or subdirectory)
      
      @returns The combined path string, normalized with correct path delimiters. Returns APath2 if APath1 is empty, APath1 if APath2 is empty.
      
      @warning Normalizes the path, which might resolve '..' components or change slashes depending on the OS
      
      @example
        FullPath := TFileKit.CombinePaths('C:\Data', 'Subdir\file.txt');
        // Returns: 'C:\Data\Subdir\file.txt' (on Windows)
    }
    class function CombinePaths(const APath1, APath2: string): string;

    { @description Checks if a path string represents an absolute path
      
      @usage Use to determine if a path is fully qualified or relative to the current directory
      
      @param APath The path string to check
      
      @returns True if the path starts with a drive letter and colon (Windows) or a forward slash (Unix), False otherwise
      
      @warning Does not validate the existence or correctness of the path, only checks the format
      
      @example
        IsAbs := TFileKit.IsAbsolutePath('C:\Windows\System32'); // True on Windows
        IsRel := TFileKit.IsAbsolutePath('mydir\myfile.txt');   // False
    }
    class function IsAbsolutePath(const APath: string): Boolean;

    { @description Normalizes a path string to a standard format for the current OS
      
      @usage Use to clean up path strings, resolve relative components ('.', '..'), and ensure consistent path delimiters
      
      @param APath The path string to normalize
      
      @returns The expanded, normalized path string with OS-specific delimiters (e.g., '\' on Windows, '/' on Unix)
      
      @warning Resolves symbolic links as part of ExpandFileName
                Final path might look different if '..' components navigate above the root
      
      @example
        NormPath := TFileKit.NormalizePath('C:/Users/../Public/./Documents');
        // Returns: 'C:\Public\Documents' (on Windows)
    }
    class function NormalizePath(const APath: string): string;
    
    { @description Creates an empty temporary file with a unique name in the system's temporary directory
      
      @usage Use when you need a temporary file for intermediate data storage, ensuring a unique name to avoid conflicts
      
      @param APrefix Optional prefix string to include in the temporary filename
      
      @returns The full path to the newly created empty temporary file
      
      @warning The created file is *not* automatically deleted; requires manual cleanup (e.g., using DeleteFile)
                Relies on GUID creation, which could theoretically fail
                Requires write permissions to the temporary directory
      
      @example
        TempFile := TFileKit.CreateTempFile('myapp-');
        // Returns path like 'C:\Temp\myapp-_...GUID....tmp' and creates the empty file
        // ... use TempFile ...
        TFileKit.DeleteFile(TempFile); // Manual cleanup needed
    }
    class function CreateTempFile(const APrefix: string = ''): string;

    { @description Creates an empty temporary directory with a unique name in the system's temporary directory
      
      @usage Use when you need a temporary directory for storing multiple temporary files or complex structures
      
      @param APrefix Optional prefix string to include in the temporary directory name
      
      @returns The full path to the newly created empty temporary directory
      
      @warning The created directory and its contents are *not* automatically deleted; requires manual cleanup (e.g., using DeleteDirectory)
                Relies on GUID creation, which could theoretically fail
                Requires write permissions to the temporary directory
      
      @example
        TempDir := TFileKit.CreateTempDirectory('session-');
        // Returns path like 'C:\Temp\session-_...GUID...' and creates the empty directory
        // ... use TempDir ...
        TFileKit.DeleteDirectory(TempDir, True); // Manual cleanup needed
    }
    class function CreateTempDirectory(const APrefix: string = ''): string;

    { @description Creates a symbolic link (symlink) pointing from one path to another
      
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

      @usage Use to create shortcuts or aliases to files or directories, useful for organizing files or managing different versions
      
      @param ATargetPath The existing file or directory path that the link should point to
      @param ALinkPath The path where the new symbolic link file should be created
      @param IsDirectory Set to True if the ATargetPath is a directory (especially important on Windows)
      
      @warning Requires specific privileges on Windows (Admin or Developer Mode)
                Requires write permissions in the directory where ALinkPath is created
                Behavior and requirements differ significantly between Windows and Unix
                Raises EFileSystemError on failure with OS-specific error details
      
      @example
        // Link C:\MyLink.txt to C:\Data\RealFile.txt
        TFileKit.CreateSymLink('C:\Data\RealFile.txt', 'C:\MyLink.txt', False); 
        // Link /home/user/linked_dir to /mnt/data/real_dir
        TFileKit.CreateSymLink('/mnt/data/real_dir', '/home/user/linked_dir', True); 
    }
    class procedure CreateSymLink(const ATargetPath, ALinkPath: string; const IsDirectory: Boolean = False);

    { @description Deletes a symbolic link file itself, without affecting the target
      
      @usage Use to remove a previously created symbolic link
      
      @param ALinkPath The path to the symbolic link file to delete
      
      @warning Only deletes the link; the original target file or directory remains untouched
                Raises EFileSystemError if deletion fails (e.g., due to permissions)
                Does nothing if the path doesn't exist
      
      @example
        TFileKit.DeleteSymLink('C:\MyLink.txt');
        // Removes the link file C:\MyLink.txt
    }
    class procedure DeleteSymLink(const ALinkPath: string);

    { @description Resolves a symbolic link to find the actual path it points to
      
      @usage Use to determine the target of a symbolic link
      
      @param ALinkPath The path of the symbolic link to resolve
      
      @returns The full, normalized path of the target file or directory
      
      @warning Raises EFileSystemError if ALinkPath is not a symbolic link or if resolution fails
                The returned path is normalized
                Implementation details differ between Windows and Unix
      
      @example
        TargetPath := TFileKit.ResolveSymLink('C:\MyLink.txt');
        // Returns 'C:\Data\RealFile.txt' (or its normalized equivalent)
    }
    class function ResolveSymLink(const ALinkPath: string): string;

    { @description Checks if a given path represents a symbolic link
      
      @usage Use to distinguish symbolic links from regular files or directories
      
      @param APath The path to check
      
      @returns True if the path exists and is a symbolic link, False otherwise
      
      @warning Uses different system calls on Windows (GetFileAttributes with FILE_ATTRIBUTE_REPARSE_POINT) and Unix (lstat)
                Returns False if the path doesn't exist
      
      @example
        if TFileKit.IsSymLink('/usr/bin/python') then
          WriteLn('Python path is a symlink');
    }
    class function IsSymLink(const APath: string): Boolean;

    { @description Copies multiple files matching a pattern from one directory to another (non-recursive)
      
      @usage Use for batch copying files within a single directory level
      
      @param ASourceDir The source directory containing the files to copy
      @param ADestDir The destination directory where files should be copied
      @param APattern The file pattern to match (e.g., '*.txt')
      
      @warning Only copies files directly within ASourceDir; does not recurse into subdirectories
                Will overwrite existing files in ADestDir without confirmation
                Creates ADestDir if it doesn't exist
      
      @example
        TFileKit.CopyFiles('C:\Images\Raw', 'C:\Images\Processed', '*.jpg');
        // Copies all JPG files from Raw to Processed
    }
    class procedure CopyFiles(const ASourceDir, ADestDir, APattern: string);

    { @description Moves multiple files matching a pattern from one directory to another (non-recursive)
      
      @usage Use for batch moving files within a single directory level
      
      @param ASourceDir The source directory containing the files to move
      @param ADestDir The destination directory where files should be moved
      @param APattern The file pattern to match (e.g., '*.tmp')
      
      @warning Only moves files directly within ASourceDir; does not recurse into subdirectories
                Will overwrite existing files in ADestDir without confirmation
                Creates ADestDir if it doesn't exist
                Uses MoveFile internally, which may fall back to copy+delete
      
      @example
        TFileKit.MoveFiles('C:\Downloads', 'C:\Archive', '*.zip');
        // Moves all ZIP files from Downloads to Archive
    }
    class procedure MoveFiles(const ASourceDir, ADestDir, APattern: string);

    { @description Deletes multiple files matching a pattern within a directory (non-recursive)
      
      @usage Use for batch deleting files within a single directory level
      
      @param ASourceDir The directory containing the files to delete
      @param APattern The file pattern to match (e.g., '*.log', 'temp_*')
      
      @warning Only deletes files directly within ASourceDir; does not recurse into subdirectories
                No confirmation or recovery; deletion is permanent
                Does nothing if ASourceDir doesn't exist
      
      @example
        TFileKit.DeleteFiles('C:\Temp', '*.tmp');
        // Deletes all TMP files in C:\Temp
    }
    class procedure DeleteFiles(const ASourceDir, APattern: string);

    { @description Checks if a directory is empty (contains no files or subdirectories)
      
      @usage Use to determine if a directory is empty before deleting it or performing other actions
      
      @param Path The path to the directory to check
      
      @returns True if the directory exists and contains no files or subdirectories (other than '.' and '..'), False otherwise
      
      @warning Raises EFileSystemError if the directory does not exist
                Considers hidden files/directories when checking for emptiness
      
      @example
        if TFileKit.IsEmptyDirectory('C:\EmptyFolder') then
          TFileKit.DeleteDirectory('C:\EmptyFolder', False);
    }
    class function IsEmptyDirectory(const Path: string): Boolean;

    { @description Finds the longest common starting path between two path strings
      
      @usage Use to determine the shared base directory of two paths
      
      @param Path1 The first path string
      @param Path2 The second path string
      
      @returns The longest common path prefix shared by both Path1 and Path2. Returns an empty string if there's no common prefix (e.g., different drives on Windows).
      
      @warning Normalizes paths before comparison
                Comparison is case-insensitive (due to NormalizePath and splitting)
                Handles drive letters on Windows and leading slashes on Unix
      
      @example
        Common := TFileKit.GetCommonPath('C:\Projects\App1\Source', 'C:\Projects\App2\Data');
        // Returns: 'C:\Projects'
    }
    class function GetCommonPath(const Path1, Path2: string): string;

    { @description Calculates the relative path from a base path to a target path
      
      @usage Use to express a path relative to another, often used for configuration files or project structures
      
      @param BasePath The path to be relative from
      @param TargetPath The path to point to
      
      @returns A string representing the relative path from BasePath to TargetPath (e.g., '..\Data\file.txt', 'Subdir/image.png'). Uses '..' to navigate up directories. Returns '.' if paths are identical. Uses '/' as separator.
      
      @warning Normalizes paths before calculation
               Assumes both paths exist and are valid for calculation logic
               Handles drive letters on Windows and leading slashes on Unix
      
      @example
        Relative := TFileKit.GetRelativePath('C:\Projects\App\Source', 'C:\Projects\App\Data\config.ini');
        // Returns: '../Data/config.ini'
    }
    class function GetRelativePath(const BasePath, TargetPath: string): string;

    { @description Checks if one path is a subdirectory (or file within a subdirectory) of another path
      
      @usage Use to verify if a file or folder resides within a specific parent directory tree
      
      @param ParentPath The potential parent directory path
      @param ChildPath The path to check if it's under ParentPath
      
      @returns True if ChildPath starts with the normalized ParentPath (including trailing delimiter), False otherwise
      
      @warning Normalizes both paths before comparison
               Comparison is case-insensitive (due to NormalizePath)
      
      @example
        IsInside := TFileKit.IsSubPath('C:\Users\Public', 'C:\Users\Public\Documents\report.txt');
        // Returns: True
    }
    class function IsSubPath(const ParentPath, ChildPath: string): Boolean;

    { @description Counts the number of lines in a text file
      
      @usage Use to get a quick estimate of file length or for processing line-based data
      
      @param FilePath The path to the text file
      
      @returns The number of lines based on counting Line Feed (LF, #10) characters. Adds 1 if the file is non-empty and doesn't end with LF.
      
      @warning Raises EFileSystemError if the file does not exist
                Reads the file in chunks; performance depends on file size
                Assumes LF as the primary line ending; might miscount files with only CR endings
      
      @example
        LineCount := TFileKit.CountLines('C:\logfile.log');
        // Returns the number of lines in the log file
    }
    class function CountLines(const FilePath: string): Integer;

    { @description Reads and returns the first line of a text file
      
      @usage Use to quickly peek at the header or first record of a text file
      
      @param FilePath The path to the text file
      
      @returns The content of the first line, up to the first CR or LF character. Returns an empty string if the file is empty or doesn't exist.
      
      @warning Raises EFileSystemError if the file does not exist
                Only reads a small initial chunk (4KB) of the file
      
      @example
        Header := TFileKit.GetFirstLine('C:\data.csv');
        // Returns the first line (header row) of the CSV file
    }
    class function GetFirstLine(const FilePath: string): string;

    { @description Reads and returns the last line of a text file
      
      @usage Use to get the most recent entry in a log file or the last record in a data file
      
      @param FilePath The path to the text file
      
      @returns The content of the last line. Returns an empty string if the file is empty or doesn't exist.
      
      @warning Raises EFileSystemError if the file does not exist
                Loads the entire file into a TStringList, which can be inefficient for very large files
      
      @example
        LastLogEntry := TFileKit.GetLastLine('C:\server.log');
        // Returns the last line written to the log file
    }
    class function GetLastLine(const FilePath: string): string;

    { @description Checks if a file is empty (has a size of 0 bytes)
      
      @usage Use to quickly determine if a file contains any data
      
      @param FilePath The path to the file
      
      @returns True if the file exists and its size is 0, False otherwise
      
      @warning Raises EFileSystemError if the file does not exist
      
      @example
        if TFileKit.IsFileEmpty('C:\output.txt') then
          WriteLn('Output file is empty.');
    }
    class function IsFileEmpty(const FilePath: string): Boolean;

    { @description Checks if a file contains a specific text string
      
      @usage Use to quickly search for the presence of a keyword or marker within a file
      
      @param FilePath The path to the file
      @param SearchText The text string to search for
      @param CaseSensitive If True (default is False), the search is case-sensitive
      
      @returns True if the SearchText is found within the file content, False otherwise
      
      @warning Raises EFileSystemError if the file does not exist
                Loads the entire file content into memory, inefficient for large files
                Uses simple Pos search, not optimized for large-scale text searching
      
      @example
        HasError := TFileKit.ContainsText('C:\app.log', 'ERROR:', False);
        // Returns True if the log file contains 'ERROR:' (case-insensitive)
    }
    class function ContainsText(const FilePath, SearchText: string; CaseSensitive: Boolean = False): Boolean;

    { @description Attempts to determine if a file is binary rather than text
      
      @usage Use as a heuristic to guess if a file contains non-textual data
      
      @param FilePath The path to the file
      
      @returns True if the file appears to be binary (based on checking a sample for a high percentage of control characters), False otherwise (considered text or empty).
      
      @warning Raises EFileSystemError if the file does not exist
                This is a heuristic and not foolproof; might misclassify some files
                Only checks a small initial portion (512 bytes)
                Empty files are considered non-binary (text)
      
      @example
        if not TFileKit.IsBinaryFile('C:\image.jpg') then
          WriteLn('Warning: Expected image.jpg to be binary.');
    }
    class function IsBinaryFile(const FilePath: string): Boolean;

    { @description Guesses the MIME type of a file based on its extension
      
      @usage Use to determine the likely content type of a file for web servers or file handling logic
      
      @param FilePath The path to the file
      
      @returns A string representing the guessed MIME type (e.g., 'text/plain', 'image/jpeg', 'application/pdf'). Returns 'application/octet-stream' if the extension is unknown.
      
      @warning Relies solely on the file extension; does not inspect file content
                Only knows a limited set of common extensions defined internally
                Comparison is case-insensitive
      
      @example
        Mime := TFileKit.GetMimeType('C:\document.pdf');
        // Returns: 'application/pdf'
    }
    class function GetMimeType(const FilePath: string): string;

    { @description Checks if a file is likely executable based on extension (Windows) or permissions (Unix)
      
      @usage Use to identify files that can be executed by the operating system
      
      @param FilePath The path to the file
      
      @returns True if the file has an executable extension (.exe, .com, .bat, .cmd on Windows) or has execute permissions (Unix), False otherwise.
      
      @warning On Windows, relies only on common executable extensions
                On Unix, requires file system access to check permissions (fpStat)
                Does not guarantee the file is actually a valid executable program
      
      @example
        if TFileKit.IsExecutable('C:\MyApp\run.exe') then
          // It's likely an executable file
    }
    class function IsExecutable(const FilePath: string): Boolean;

    { @description Checks if a file or directory is hidden
      
      @usage Use to identify files or directories that are typically hidden from standard listings
      
      @param FilePath The path to the file or directory
      
      @returns True if the file/directory has the hidden attribute (Windows) or its name starts with a dot (Unix), False otherwise.
      
      @warning Definition of "hidden" differs between Windows and Unix
                On Windows, requires checking file attributes
                On Unix, relies purely on the naming convention
      
      @example
        if TFileKit.IsHidden('C:\Users\user\NTUSER.DAT') then
          WriteLn('File is hidden.');
    }
    class function IsHidden(const FilePath: string): Boolean;

    { @description Gets the amount of free space available on the drive containing the specified path
      
      @usage Use to check available disk space before writing large files or performing installations
      
      @param Path A path located on the drive to check (e.g., 'C:\', '/home/user')
      
      @returns The number of free bytes available to the current user on the drive (Int64). Returns -1 on error (e.g., path doesn't exist, permission error).
      
      @warning Requires access to the specified path to determine the drive
                Uses platform-specific API calls (GetDiskFreeSpaceEx on Windows, statfs on Unix)
      
      @example
        FreeSpace := TFileKit.GetDriveFreeSpace('D:\Data');
        if FreeSpace > 1024*1024*100 then // Check for > 100 MB
          // Proceed with operation
    }
    class function GetDriveFreeSpace(const Path: string): Int64;

    { @description Gets the total capacity of the drive containing the specified path
      
      @usage Use to determine the total size of a storage volume
      
      @param Path A path located on the drive to check (e.g., 'C:\', '/mnt/data')
      
      @returns The total size of the drive in bytes (Int64). Returns -1 on error.
      
      @warning Requires access to the specified path to determine the drive
                Uses platform-specific API calls
      
      @example
        Capacity := TFileKit.GetDriveCapacity('C:\');
        // Returns the total size of the C: drive
    }
    class function GetDriveCapacity(const Path: string): Int64;

    { @description Checks if a drive has at least a specified amount of free space
      
      @usage A convenience function combining GetDriveFreeSpace with a comparison
      
      @param Path A path located on the drive to check
      @param RequiredBytes The minimum number of free bytes required
      
      @returns True if the drive has at least RequiredBytes of free space available, False otherwise or on error.
      
      @warning Returns False if GetDriveFreeSpace returns -1 (error)
      
      @example
        if TFileKit.HasEnoughSpace('C:\InstallDir', 500 * 1024 * 1024) then // Need 500MB
          // Start installation
    }
    class function HasEnoughSpace(const Path: string; RequiredBytes: Int64): Boolean;

    { @description Compares two files byte-by-byte to check if their content is identical
      
      @usage Use for verifying file copies, detecting changes, or comparing data integrity
      
      @param File1 Path to the first file
      @param File2 Path to the second file
      
      @returns True if both files exist and contain the exact same sequence of bytes, False otherwise (including if sizes differ or content mismatches).
      
      @warning Returns False if either file does not exist
                Reads files in chunks (4KB); performance depends on file size
      
      @example
        if TFileKit.AreFilesIdentical('C:\original.dat', 'C:\backup.dat') then
          WriteLn('Files are identical.');
    }
    class function AreFilesIdentical(const File1, File2: string): Boolean;

    { @description Compares the modification times of two files and returns the path of the newer one
      
      @usage Use to determine which of two files was modified more recently
      
      @param File1 Path to the first file
      @param File2 Path to the second file
      
      @returns The path of the file with the later modification timestamp. If timestamps are equal, returns File2. If only one file exists, returns that file's path.
      
      @warning Raises EFileSystemError if neither file exists
                Relies on FileAge, which might have limited resolution depending on the file system
      
      @example
        Newer := TFileKit.GetNewerFile('config.ini', 'config.bak');
        // Returns the path of the more recently saved config file
    }
    class function GetNewerFile(const File1, File2: string): string;

    { @description Compares two files byte-by-byte and returns a list of differences
      
      @usage Use for basic file comparison to identify where two files differ
      
      @param File1 Path to the first file
      @param File2 Path to the second file
      
      @returns A TStringArray containing descriptions of the differences found (e.g., differing bytes at specific positions, size mismatches). Returns an empty array if files are identical or if either file doesn't exist.
      
      @warning Reads files in chunks (4KB)
                Only reports the first differing byte within each chunk comparison
                Can generate a large array if files differ significantly
                Not a sophisticated diff algorithm
      
      @example
        Diffs := TFileKit.GetFileDifferences('file_v1.txt', 'file_v2.txt');
        if Length(Diffs) > 0 then
          WriteLn('Files differ: ', Diffs[0]);
    }
    class function GetFileDifferences(const File1, File2: string): TStringArray;

    { @description Attempts to acquire an exclusive lock on a file (simple implementation)
      
      @usage Use to prevent other processes or parts of the application from modifying a file while it's being processed
      
      @param FilePath The path to the file to lock
      
      @returns True if the lock was successfully acquired, False otherwise (e.g., file doesn't exist, already locked by this mechanism).
      
      @warning This is a very basic, advisory locking mechanism using a global list (Windows) or a '.lock' file (Unix). It's not a robust OS-level lock.
                Does not prevent other applications unaware of this mechanism from accessing the file.
                Requires calling UnlockFile to release the lock.
                The global list `LockedFiles` is not thread-safe.
      
      @example
        if TFileKit.LockFile('C:\data.db') then
        try
          // Process the file exclusively
        finally
          TFileKit.UnlockFile('C:\data.db');
        end;
    }
    class function LockFile(const FilePath: string): Boolean;

    { @description Releases a lock previously acquired by LockFile
      
      @usage Must be called to release a lock obtained via LockFile to allow others to lock it
      
      @param FilePath The path to the file whose lock should be released
      
      @returns True if the lock was found and released, False otherwise (e.g., file wasn't locked by this mechanism).
      
      @warning Only releases locks managed by this specific LockFile/UnlockFile implementation.
                The global list `LockedFiles` is not thread-safe.
      
      @example
        // (Inside a try..finally block after LockFile)
        TFileKit.UnlockFile('C:\data.db');
    }
    class function UnlockFile(const FilePath: string): Boolean;

    { @description Checks if a file is currently locked by this TFileKit's simple locking mechanism
      
      @usage Use to check the status of the advisory lock before attempting an operation
      
      @param FilePath The path to the file to check
      
      @returns True if the file path is present in the internal list of locked files, False otherwise.
      
      @warning Only checks the internal advisory lock status; does not check for OS-level locks or locks held by other applications.
                The global list `LockedFiles` is not thread-safe.
      
      @example
        if TFileKit.IsFileLocked('C:\status.flag') then
          WriteLn('Status file is currently locked.');
    }
    class function IsFileLocked(const FilePath: string): Boolean;

    { @description Checks if a filename string contains invalid characters or violates basic naming rules
      
      @usage Use to validate user input or generated filenames before attempting to create files
      
      @param FileName The filename string to validate (just the name, not the full path)
      
      @returns True if the filename is considered valid (non-empty, within length limits, no forbidden characters), False otherwise.
      
      @warning Checks against a common set of invalid characters ('<', '>', ':', '"', '/', '\', '|', '?', '*'); OS might have additional restrictions.
                Checks for basic length limit (255); actual filesystem limits might vary.
                Does not check for reserved filenames (CON, PRN, AUX, etc. on Windows).
      
      @example
        if TFileKit.IsValidFileName('report_final.docx') then
          // Proceed to create file
    }
    class function IsValidFileName(const FileName: string): Boolean;

    { @description Replaces invalid characters in a filename string with underscores
      
      @usage Use to clean up potentially invalid filenames to make them safe for file system use
      
      @param FileName The filename string to sanitize
      
      @returns A sanitized version of the filename with invalid characters replaced by '_'. Also trims trailing spaces/dots and ensures the result is not empty.
      
      @warning Replaces invalid characters with '_'; might lead to collisions if multiple invalid names sanitize to the same result.
                Does not handle reserved filenames (CON, PRN, etc.).
                Resulting filename might still exceed path length limits when combined with a directory.
      
      @example
        SafeName := TFileKit.SanitizeFileName('My Report / Version? *final*:.txt');
        // Returns: 'My Report _ Version_ _final__.txt' (or similar)
    }
    class function SanitizeFileName(const FileName: string): string;

    { @description Attempts to create a valid, normalized path string by sanitizing components and resolving relative parts
      
      @usage Use to clean up and normalize a potentially complex or invalid path string
      
      @param Path The path string to make valid
      
      @returns A normalized path string with invalid filename components sanitized and relative parts ('.', '..') resolved.
      
      @warning Combines normalization (like NormalizePath) with filename sanitization (SanitizeFileName) for each component.
                Resolution of '..' might lead to unexpected results if the input path is unusual.
      
      @example
        ValidPath := TFileKit.MakeValidPath('C:/Temp/../My Data?/report<1>.txt');
        // Returns: 'C:\My Data_\report_1_.txt' (on Windows)
    }
    class function MakeValidPath(const Path: string): string;

    { @description Checks if a path string potentially exceeds typical operating system path length limits
      
      @usage Use as a preliminary check to avoid errors caused by excessively long paths
      
      @param Path The path string to check
      
      @returns True if the path length exceeds a predefined limit (260 for Windows, 1024 for Unix), False otherwise.
      
      @warning The actual path length limits can be complex and depend on the OS version, filesystem, and API used. This is a simplified check.
                Windows has mechanisms (like '\\?\') to handle longer paths, which this check doesn't account for.
      
      @example
        LongPath := StringOfChar('A', 300);
        if TFileKit.IsPathTooLong('C:\' + LongPath + '\file.txt') then
          WriteLn('Warning: Path might be too long.');
    }
    class function IsPathTooLong(const Path: string): Boolean;

    { @description Gathers summary information about a directory's contents (non-recursive)
      
      @usage Use to get a quick overview of a directory (file/subdir count, total size, oldest/newest/largest files)
      
      @param Path The path to the directory
      
      @returns A TDirectoryInfo record populated with the summary. Fields are zero/empty if the directory doesn't exist or is empty.
      
      @warning Only scans the immediate contents of the directory; does not recurse.
                Performance depends on the number of items in the directory.
                File times/sizes are retrieved during the scan.
      
      @example
        Info := TFileKit.GetDirectoryInfo('C:\Downloads');
        WriteLn('Files: ', Info.FileCount, ', Size: ', Info.TotalSize);
        WriteLn('Newest: ', Info.NewestFile);
    }
    class function GetDirectoryInfo(const Path: string): TDirectoryInfo;

    { @description Checks if a filename matches a simple wildcard pattern
      
      @usage A basic pattern matching function used internally by listing/searching functions
      
      @param FileName The filename to check
      @param Pattern The wildcard pattern ('*', '*ext', 'name*', '*part*')
      
      @returns True if the FileName matches the Pattern, False otherwise. Case-insensitive.
      
      @warning Very basic wildcard support: '*' anywhere, '*text', 'text*', '*text*'. Does not support '?'.
               Uses the same implementation as the private MatchPattern method.
      
      @example
        if TFileKit.MatchesPattern('document.txt', '*.txt') then // True
        if TFileKit.MatchesPattern('image.jpg', 'img*') then // False
    }
    class function MatchesPattern(const FileName, Pattern: string): Boolean;

    { @description Finds the first file or directory matching a pattern within a directory (non-recursive)
      
      @usage Use to quickly find if at least one item matches a pattern, without listing all items
      
      @param Directory The directory path to search in
      @param Pattern The file/directory pattern to match
      
      @returns The name (not full path) of the first matching item found. Returns an empty string if no match is found or the directory doesn't exist.
      
      @warning The order of items returned by FindFirst is OS-dependent.
               Returns any file or directory (including special entries, except '.' and '..').
               Does not filter by file attributes - will return both files and directories.
      
      @example
        FirstLog := TFileKit.FindFirstMatch('C:\Logs', '*.log');
        if FirstLog <> '' then WriteLn('Found log file: ', FirstLog);
    }
    class function FindFirstMatch(const Directory, Pattern: string): string;

    { @description Counts the number of files or directories matching a pattern within a directory (non-recursive)
      
      @usage Use to quickly count items matching a pattern without retrieving the full list
      
      @param Directory The directory path to search in
      @param Pattern The file/directory pattern to match
      
      @returns The number of items (excluding '.' and '..') matching the pattern in the directory.
      
      @warning Only counts items directly within the specified directory.
               Counts both files and directories matching the pattern.
               Returns 0 if the directory doesn't exist.
      
      @example
        TmpFileCount := TFileKit.CountMatches('C:\Temp', '*.tmp');
        WriteLn('Found ', TmpFileCount, ' temporary files.');
    }
    class function CountMatches(const Directory, Pattern: string): Integer;

    { @description Reads a specific chunk of bytes from a file
      
      @usage Use for reading parts of large files without loading the entire content, e.g., for partial processing or random access
      
      @param FilePath The path to the file
      @param Offset The starting byte position (0-based) to read from
      @param Size The maximum number of bytes to read
      
      @returns A TBytes dynamic array containing the bytes read. The array length might be less than Size if EOF is reached or Offset is invalid. Returns an empty array on error or if Offset is out of bounds.
      
      @warning Returns empty array if FilePath doesn't exist or Offset is invalid (negative or beyond file size)
               Actual bytes read might be less than requested Size if near EOF
               Does not raise exceptions on invalid parameters
      
      @example
        // Read 1024 bytes starting from position 4096
        Chunk := TFileKit.GetChunk('C:\largefile.bin', 4096, 1024);
        if Length(Chunk) > 0 then
          // Process the chunk
    }
    class function GetChunk(const FilePath: string; Offset, Size: Int64): TBytes;
  end;


implementation

// Add comparison functions for sorting
function CompareByDate(List: TStringList; Index1, Index2: Integer): Integer;
var
  Time1, Time2: TDateTime;
  Info1, Info2: TSearchRec;
begin
  // Use FindFirst/FindClose to get file dates for more robust handling
  if FindFirst(List[Index1], faAnyFile, Info1) = 0 then
  begin
    Time1 := FileDateToDateTime(Info1.Time);
    FindClose(Info1);
  end else
    Time1 := 0;

  if FindFirst(List[Index2], faAnyFile, Info2) = 0 then
  begin
    Time2 := FileDateToDateTime(Info2.Time);
    FindClose(Info2);
  end else
    Time2 := 0;
    
  Result := CompareDateTime(Time1, Time2);
end;

function CompareBySize(List: TStringList; Index1, Index2: Integer): Integer;
var
  Size1, Size2: Int64;
  Info1, Info2: TSearchRec;
begin
  // Use standard FindFirst/FindClose to get size
  if FindFirst(List[Index1], faAnyFile, Info1) = 0 then
  begin
    Size1 := Info1.Size;
    FindClose(Info1);
  end else
    Size1 := 0;

  if FindFirst(List[Index2], faAnyFile, Info2) = 0 then
  begin
    Size2 := Info2.Size;
    FindClose(Info2);
  end else
    Size2 := 0;

  if Size1 < Size2 then
    Result := -1
  else if Size1 > Size2 then
    Result := 1
  else
    Result := 0;
end;



{$IFDEF UNIX}
function S_ISDIR(Mode: mode_t): Boolean;
begin
  Result := ((Mode and S_IFMT) = S_IFDIR);
end;

function S_ISLNK(Mode: mode_t): Boolean;
begin
  Result := ((Mode and S_IFMT) = S_IFLNK);
end;
{$ENDIF}

class function TFileKit.GetAttributes(const APath: string): TFileAttributes;
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
    Result.ReadOnly := (Info.st_mode and S_IWUSR) = 0;
    Result.Directory := ((Info.st_mode and S_IFMT) = S_IFDIR);
    Result.SymLink := ((Info.st_mode and S_IFMT) = S_IFLNK);
    
    // Convert mode to string (e.g., 'rwxr-xr-x')
    Result.Permissions := '';
    Result.Permissions := Result.Permissions + IfThen((Info.st_mode and S_IRUSR) <> 0, 'r', '-');
    Result.Permissions := Result.Permissions + IfThen((Info.st_mode and S_IWUSR) <> 0, 'w', '-');
    Result.Permissions := Result.Permissions + IfThen((Info.st_mode and S_IXUSR) <> 0, 'x', '-');
    Result.Permissions := Result.Permissions + IfThen((Info.st_mode and S_IRGRP) <> 0, 'r', '-');
    Result.Permissions := Result.Permissions + IfThen((Info.st_mode and S_IWGRP) <> 0, 'w', '-');
    Result.Permissions := Result.Permissions + IfThen((Info.st_mode and S_IXGRP) <> 0, 'x', '-');
    Result.Permissions := Result.Permissions + IfThen((Info.st_mode and S_IROTH) <> 0, 'r', '-');
    Result.Permissions := Result.Permissions + IfThen((Info.st_mode and S_IWOTH) <> 0, 'w', '-');
    Result.Permissions := Result.Permissions + IfThen((Info.st_mode and S_IXOTH) <> 0, 'x', '-');
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

class function TFileKit.FileTimeToDateTime(const FileTime: {$IFDEF WINDOWS}FILETIME{$ELSE}TDateTime{$ENDIF}): TDateTime;
{$IFDEF WINDOWS}
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
{$ELSE}
begin
  // On Unix systems, just return the datetime directly
  Result := FileTime;
{$ENDIF}
end;


class function TFileKit.MatchPattern(const FileName, Pattern: string): Boolean;
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

class function TFileKit.LoadTextFromFile(const APath: string): string;
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

class procedure TFileKit.SaveTextToFile(const APath: string; const AContent: string);
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
  Result.Attributes := GetAttributes(NormalPath);
  
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
      fpChmod(PChar(ADestPath), Info.st_mode and $0FFF);
      
      // Set ownership if we have permissions
      if fpGetuid = 0 then  // Only try if we're root
      begin
        fpChown(PChar(ADestPath), Info.st_uid, Info.st_gid);
      end;
      
      // Set timestamps
      with Info do
      begin
        fpUtime(PChar(ADestPath), @Info.st_mtime);
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
          raise EFileSystemError.Create('Move operation failed: Size mismatch after copy');
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
    raise EFileSystemError.Create('Failed to create GUID for temporary file');
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
    raise EFileSystemError.Create('Failed to create GUID for temporary directory');
end;

class function TFileKit.IsTextFile(const APath: string): Boolean;
const
  MaxBytesToCheck = 512;
var
  Stream: TFileStream;
  Buffer: array[0..MaxBytesToCheck-1] of Byte;
  BytesRead: Integer;
  I: Integer;
  IsBinary: Boolean;
begin
  Result := False; // Assume not text by default
  if not FileExists(APath) then
    Exit;

  Stream := nil; // Initialize stream to nil
  try
    Stream := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
    try
      // Read a sample of the file
      BytesRead := Stream.Read(Buffer, Min(Stream.Size, MaxBytesToCheck));

      // If the file is empty, consider it text
      if BytesRead = 0 then
      begin
        Result := True;
        Exit;
      end;

      // Check for binary characters (control chars other than TAB, LF, CR, ESC)
      IsBinary := False;
      for I := 0 to BytesRead - 1 do
      begin
        if (Buffer[I] < 7) or ((Buffer[I] > 14) and (Buffer[I] < 32) and (Buffer[I] <> 27)) then
        begin
          IsBinary := True;
          Break; // Found a binary character, no need to check further
        end;
      end;

      Result := not IsBinary; // It's text if no binary characters were found

    except
      // Handle potential exceptions during stream reading
      Result := False; // Assume not text if reading fails
    end;
  finally
    // Ensure the stream is always freed
    if Assigned(Stream) then
      Stream.Free;
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
  {$IFDEF WINDOWS}
  Flags: DWORD;
  ErrorCode: DWORD;
  ErrorMsg: string;
  {$ENDIF}
  {$IFDEF UNIX}
  ErrorCode: Integer;
  ErrorMsg: string;
  {$ENDIF}
begin
  {$IFDEF WINDOWS}
  // Windows implementation
  // -----------------------------
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
    raise EFileSystemError.CreateFmt('Failed to create symbolic link: %s (Error %d)', [ErrorMsg, ErrorCode]);
  end;
  {$ENDIF}
  
  {$IFDEF UNIX}
  // Unix implementation
  // -----------------------------
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
    raise EFileSystemError.CreateFmt('Failed to create symbolic link: %s (Error %d)', [ErrorMsg, ErrorCode]);
  end;
  {$ENDIF}
end;

class procedure TFileKit.DeleteSymLink(const ALinkPath: string);
var
  ErrorCode: DWORD;
  ErrMsg: string;
begin
  if not FileExists(ALinkPath) and not DirectoryExists(ALinkPath) then
    Exit;
    
  {$IFDEF WINDOWS}
  if not Windows.DeleteFile(PChar(ALinkPath)) then
  begin
    ErrorCode := GetLastError;
    ErrMsg := SysErrorMessage(ErrorCode);
    raise EFileSystemError.CreateFmt('Failed to delete symbolic link: %s (Error %d)', [ErrMsg, ErrorCode]);
  end;
  {$ENDIF}
  {$IFDEF UNIX}
  if fpUnlink(PChar(ALinkPath)) <> 0 then
  begin
    ErrMsg := SysErrorMessage(fpgeterrno);
    raise EFileSystemError.CreateFmt('Failed to delete symbolic link: %s', [ErrMsg]);
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
  ErrMsg: string;
  I: Integer;
{$ENDIF}
{$IFDEF UNIX}
var
  Buffer: array[0..PATH_MAX - 1] of Char;
  BytesRead: Integer;
  ErrMsg: string;
{$ENDIF}
begin
  Result := '';
  
  if not IsSymLink(ALinkPath) then
    raise EFileSystemError.Create('Path is not a symbolic link');
    
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
    ErrMsg := SysErrorMessage(ErrorCode);
    raise EFileSystemError.CreateFmt('Failed to open symbolic link: %s (Error %d)', [ErrMsg, ErrorCode]);
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
      ErrMsg := SysErrorMessage(ErrorCode);
      raise EFileSystemError.CreateFmt('Failed to get reparse point data: %s (Error %d)', [ErrMsg, ErrorCode]);
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
      raise EFileSystemError.Create('Not a valid symbolic link');
  finally
    CloseHandle(Handle);
  end;
  {$ENDIF}
  {$IFDEF UNIX}
  BytesRead := fpReadLink(PChar(ALinkPath), Buffer, PATH_MAX);
  if BytesRead <= 0 then
  begin
    ErrMsg := SysErrorMessage(fpgeterrno);
    raise EFileSystemError.CreateFmt('Failed to resolve symbolic link: %s', [ErrMsg]);
  end;
  SetString(Result, Buffer, BytesRead);
  {$ENDIF}
  
  if Result = '' then
    raise EFileSystemError.Create('Failed to resolve symbolic link: Empty result');
    
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
    Result := ((Info.st_mode and S_IFMT) = S_IFLNK);
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
    raise EFileSystemError.CreateFmt('Directory does not exist: %s', [Path]);
    
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
      raise EFileSystemError.CreateFmt('Error checking if directory is empty: %s', [E.Message]);
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
      // For Unix paths starting with /, handle them specially
      {$IFDEF UNIX}
      if IsUnixStyle then
      begin
        // Remove leading slash for splitting
        Parts1 := SplitString(Copy(Path1, 2, Length(Path1)), '/');
        Parts2 := SplitString(Copy(Path2, 2, Length(Path2)), '/');
      end
      else
      {$ENDIF}
      begin
        Parts1 := SplitString(NormalizePath(Path1), PathDelim);
        Parts2 := SplitString(NormalizePath(Path2), PathDelim);
      end;
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
  else 
  begin
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
      Result := '/' + CommonParts[0];  // Add single leading slash for Unix paths
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
    raise EFileSystemError.CreateFmt('File does not exist: %s', [FilePath]);
    
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
    raise EFileSystemError.CreateFmt('File does not exist: %s', [FilePath]);
    
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
    raise EFileSystemError.CreateFmt('File does not exist: %s', [FilePath]);
    
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
    raise EFileSystemError.CreateFmt('File does not exist: %s', [FilePath]);
    
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
    raise EFileSystemError.CreateFmt('File does not exist: %s', [FilePath]);
    
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
    raise EFileSystemError.CreateFmt('File does not exist: %s', [FilePath]);
    
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
{$IFDEF UNIX}
var
  Info: BaseUnix.Stat;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  Result := AnsiEndsText('.exe', FilePath) or
            AnsiEndsText('.com', FilePath) or
            AnsiEndsText('.bat', FilePath) or
            AnsiEndsText('.cmd', FilePath);
  {$ELSE} 
  Result := (fpStat(PChar(FilePath), Info) = 0) and ((Info.st_mode and S_IXUSR) <> 0);
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
  StatFS: BaseUnix.TStatFS;
begin
  Result := -1;
  if fpStatFS(PChar(ExtractFilePath(Path)), @StatFS) = 0 then
    Result := Int64(StatFS.bsize) * Int64(StatFS.bavail);
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
  StatFS: BaseUnix.TStatFS;
begin
  Result := -1;
  if fpStatFS(PChar(ExtractFilePath(Path)), @StatFS) = 0 then
    Result := Int64(StatFS.bsize) * Int64(StatFS.blocks);
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
    raise EFileSystemError.Create('Both files do not exist');
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
  TextFile: Text;
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
    AssignFile(TextFile, LockPath);
    Rewrite(TextFile);
    CloseFile(TextFile);
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
  // On Unix, the PATH_MAX value of 4096 includes the null terminator
  // Also, some distros may have limitations for specific operations
  // Using a more conservative threshold
  Result := Length(Path) > 1024; // More conservative limit for cross-platform compatibility
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

class function TFileKit.GetChunk(const FilePath: string; Offset, Size: Int64): TBytes;
var
  FileStream: TFileStream;
  BytesRead: Integer;
  ChunkSize: Int64;
begin
  SetLength(Result, 0);
  if not FileExists(FilePath) then
    Exit;
    
  FileStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyNone);
  try
    // Make sure offset is within file bounds
    if (Offset < 0) or (Offset >= FileStream.Size) then
      Exit;
      
    // Calculate actual chunk size (don't read past EOF)
    ChunkSize := Min(Size, FileStream.Size - Offset);
    if ChunkSize <= 0 then
      Exit;
      
    // Set position and read chunk
    FileStream.Position := Offset;
    SetLength(Result, ChunkSize);
    BytesRead := FileStream.Read(Result[0], ChunkSize);
    
    // If we didn't read the full chunk, resize the result
    if BytesRead <> ChunkSize then
      SetLength(Result, BytesRead);
  finally
    FileStream.Free;
  end;
end;

finalization
  if Assigned(LockedFiles) then
    FreeAndNil(LockedFiles);
end.
