program FileKitExample;

{$mode objfpc}{$H+}{$J-}

{ This example demonstrates the file handling capabilities of TidyKit.FS library.
  It shows common file and directory operations like:
  - Basic file operations (read/write/copy/move/delete)
  - File content manipulation
  - Directory listing and management
  - File information and attributes
  - File searching and filtering
  - Path manipulation
  - Temporary file handling
  - Text file operations }

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, 
  TidyKit.Core, TidyKit.FS;

type
  { TFileOperations
    Main class that demonstrates various file operations.
    Each method focuses on a specific category of functionality. }
  TFileOperations = class
  private
    FBasePath: string;  // Base path for all operations
    FFileKit: IFileKit; // Our file operations interface
    procedure CreateSampleFiles;    // Creates test files and directories
    procedure CleanupSampleFiles;   // Cleans up test files when done
  public
    constructor Create(const ABasePath: string);
    destructor Destroy; override;
    
    procedure ShowBasicOperations;      // Basic file operations like read/write/copy/move
    procedure ShowFileManipulation;     // Content manipulation like append/prepend/replace
    procedure ShowDirectoryOperations;  // Directory listing and management
    procedure ShowFileInformation;      // File metadata and attributes
    procedure ShowSearchOperations;     // File searching and filtering
    procedure ShowPathOperations;       // Path manipulation and analysis
    procedure ShowTempOperations;       // Temporary file handling
    procedure ShowTextOperations;       // Text file specific operations
  end;

{ TFileOperations Implementation }

constructor TFileOperations.Create(const ABasePath: string);
begin
  // Create our FileKit instance using the factory
  FFileKit := TFSFactory.CreateFileKit;
  
  // Normalize the base path to ensure consistent path separators
  FBasePath := FFileKit.NormalizePath(ABasePath);
  CreateSampleFiles;
end;

destructor TFileOperations.Destroy;
begin
  CleanupSampleFiles;  // Clean up all test files before destroying
  inherited;
  // FileKit interface will be automatically freed by reference counting
end;

procedure TFileOperations.CreateSampleFiles;
begin
  // Create a test directory structure with subdirectories
  FFileKit.CreateDirectory(FFileKit.CombinePaths(FBasePath, 'test'));
  FFileKit.CreateDirectory(FFileKit.CombinePaths(FBasePath, 'test/subdir1'));
  FFileKit.CreateDirectory(FFileKit.CombinePaths(FBasePath, 'test/subdir2'));
  
  // Create various test files with different content types:
  
  // Text file with multiple lines
  FFileKit.WriteTextFile(
    FFileKit.CombinePaths(FBasePath, 'test/file1.txt'),
    'This is file 1' + LineEnding + 'With multiple lines'
  );
  
  // Simple text file
  FFileKit.WriteTextFile(
    FFileKit.CombinePaths(FBasePath, 'test/file2.txt'),
    'This is file 2'
  );
  
  // Text file in subdirectory
  FFileKit.WriteTextFile(
    FFileKit.CombinePaths(FBasePath, 'test/subdir1/file3.txt'),
    'This is file 3'
  );
  
  // Binary file with null characters
  FFileKit.WriteTextFile(
    FFileKit.CombinePaths(FBasePath, 'test/subdir2/file4.dat'),
    'Binary'#0'Data'#0'Here'
  );
end;

procedure TFileOperations.CleanupSampleFiles;
begin
  // Remove all test files and directories recursively
  if FFileKit.DirectoryExists(FFileKit.CombinePaths(FBasePath, 'test')) then
    FFileKit.DeleteDirectory(FFileKit.CombinePaths(FBasePath, 'test'), True);
end;

procedure TFileOperations.ShowBasicOperations;
var
  FilePath: string;
  Content: string;
begin
  WriteLn('Basic File Operations:');
  WriteLn('---------------------');
  
  // Create a test file path
  FilePath := FFileKit.CombinePaths(FBasePath, 'test/basic.txt');
  
  // Demonstrate basic file writing
  WriteLn('Writing to file...');
  FFileKit.WriteTextFile(FilePath, 'Hello, World!');
  
  // Read and display file content
  WriteLn('Reading from file...');
  Content := FFileKit.ReadTextFile(FilePath);
  WriteLn('Content: ', Content);
  
  // Append additional content to existing file
  WriteLn('Appending to file...');
  FFileKit.AppendText(FilePath, LineEnding + 'Additional content');
  Content := FFileKit.ReadTextFile(FilePath);
  WriteLn('Updated content: ', Content);
  
  // Create a backup copy of the file
  WriteLn('Copying file...');
  FFileKit.CopyFile(FilePath, FilePath + '.backup');
  
  // Move/rename the backup file
  WriteLn('Moving file...');
  FFileKit.MoveFile(FilePath + '.backup', FilePath + '.moved');
  
  // Clean up by deleting test files
  WriteLn('Deleting files...');
  FFileKit.DeleteFile(FilePath);
  FFileKit.DeleteFile(FilePath + '.moved');
  
  WriteLn;
end;

procedure TFileOperations.ShowFileManipulation;
var
  FilePath: string;
begin
  WriteLn('File Content Manipulation:');
  WriteLn('------------------------');
  
  FilePath := FFileKit.CombinePaths(FBasePath, 'test/manipulation.txt');
  
  // Create a file with initial content
  FFileKit.WriteTextFile(FilePath, 'Line 1' + LineEnding + 'Line 2' + LineEnding + 'Line 3');
  
  // Add text to end of file
  WriteLn('Appending text...');
  FFileKit.AppendText(FilePath, LineEnding + 'Appended line');
  
  // Add text to beginning of file
  WriteLn('Prepending text...');
  FFileKit.PrependText(FilePath, 'Prepended line' + LineEnding);
  
  // Replace text throughout file
  WriteLn('Replacing text...');
  FFileKit.ReplaceText(FilePath, 'Line', 'TEXT');
  
  // Display the final result
  WriteLn('Final content:');
  WriteLn(FFileKit.ReadTextFile(FilePath));
  WriteLn;
end;

procedure TFileOperations.ShowDirectoryOperations;
var
  Files: TFilePathArray;
  Dirs: TFilePathArray;
  I: Integer;
begin
  WriteLn('Directory Operations:');
  WriteLn('--------------------');
  
  // List files in test directory (non-recursive)
  WriteLn('Files in test directory:');
  Files := FFileKit.ListFiles(FFileKit.CombinePaths(FBasePath, 'test'), '*.*', False, fsName);
  for I := 0 to High(Files) do
    WriteLn('- ', FFileKit.GetFileName(Files[I]));
  WriteLn;
  
  // List all files including subdirectories
  WriteLn('All files (recursive):');
  Files := FFileKit.ListFiles(FFileKit.CombinePaths(FBasePath, 'test'), '*.*', True, fsName);
  for I := 0 to High(Files) do
    WriteLn('- ', FFileKit.GetFileName(Files[I]));
  WriteLn;
  
  // List all directories
  WriteLn('Directories:');
  Dirs := FFileKit.ListDirectories(FFileKit.CombinePaths(FBasePath, 'test'), '*', True, fsName);
  for I := 0 to High(Dirs) do
    WriteLn('- ', FFileKit.GetFileName(Dirs[I]));
  WriteLn;
  
  // Create new directory
  WriteLn('Creating new directory...');
  FFileKit.CreateDirectory(FFileKit.CombinePaths(FBasePath, 'test/newdir'));
  
  // Create directory and any missing parent directories
  WriteLn('Ensuring directory exists...');
  FFileKit.EnsureDirectory(FFileKit.CombinePaths(FBasePath, 'test/newdir/subdir'));
  
  WriteLn;
end;

procedure TFileOperations.ShowFileInformation;
var
  FilePath: string;
  Attrs: TFileAttributes;
begin
  WriteLn('File Information:');
  WriteLn('-----------------');
  
  FilePath := FFileKit.CombinePaths(FBasePath, 'test/file1.txt');
  
  // Display basic file properties
  WriteLn('File: ', FFileKit.GetFileName(FilePath));
  WriteLn('Exists: ', FFileKit.Exists(FilePath));
  WriteLn('Size: ', FFileKit.GetSize(FilePath), ' bytes');
  WriteLn('Is Text File: ', FFileKit.IsTextFile(FilePath));
  WriteLn('Encoding: ', FFileKit.GetFileEncoding(FilePath));
  
  // Show file timestamps
  WriteLn('Created: ', DateTimeToStr(FFileKit.GetCreationTime(FilePath)));
  WriteLn('Last Accessed: ', DateTimeToStr(FFileKit.GetLastAccessTime(FilePath)));
  WriteLn('Last Modified: ', DateTimeToStr(FFileKit.GetLastWriteTime(FilePath)));
  
  // Display detailed file attributes
  Attrs := FFileKit.GetAttributes(FilePath);
  WriteLn('Attributes:');
  WriteLn('- Read Only: ', Attrs.ReadOnly);
  WriteLn('- Hidden: ', Attrs.Hidden);
  WriteLn('- System: ', Attrs.System);
  WriteLn('- Directory: ', Attrs.Directory);
  WriteLn('- Archive: ', Attrs.Archive);
  WriteLn('- Symbolic Link: ', Attrs.SymLink);
  if Attrs.Permissions <> '' then
    WriteLn('- Permissions: ', Attrs.Permissions);
  
  WriteLn;
end;

procedure TFileOperations.ShowSearchOperations;
var
  Results: TSearchResults;
  I: Integer;
  LatestFile, OldestFile, LargestFile, SmallestFile: string;
begin
  WriteLn('Search Operations:');
  WriteLn('-----------------');
  
  // Search for files with pattern matching
  WriteLn('Searching for *.txt files:');
  Results := FFileKit.SearchFiles(FFileKit.CombinePaths(FBasePath, 'test'), '*.txt', True);
  for I := 0 to High(Results) do
  begin
    WriteLn('Found: ', Results[I].FileName);
    WriteLn('  Size: ', Results[I].Size, ' bytes');
    WriteLn('  Modified: ', DateTimeToStr(Results[I].LastModified));
  end;
  WriteLn;
  
  // Find files based on various criteria
  LatestFile := FFileKit.FindLastModifiedFile(
    FFileKit.CombinePaths(FBasePath, 'test'), '*.txt', True);
  OldestFile := FFileKit.FindFirstModifiedFile(
    FFileKit.CombinePaths(FBasePath, 'test'), '*.txt', True);
  LargestFile := FFileKit.FindLargestFile(
    FFileKit.CombinePaths(FBasePath, 'test'), '*.txt', True);
  SmallestFile := FFileKit.FindSmallestFile(
    FFileKit.CombinePaths(FBasePath, 'test'), '*.txt', True);
    
  WriteLn('File Analysis:');
  WriteLn('Most recently modified: ', LatestFile);
  WriteLn('Oldest file: ', OldestFile);
  WriteLn('Largest file: ', LargestFile);
  WriteLn('Smallest file: ', SmallestFile);
  WriteLn;
end;

procedure TFileOperations.ShowPathOperations;
var
  TestPath: string;
begin
  WriteLn('Path Operations:');
  WriteLn('---------------');
  
  TestPath := FFileKit.CombinePaths(FBasePath, 'test/subdir1/file.txt');
  
  // Demonstrate various path manipulation operations
  WriteLn('Test Path: ', TestPath);
  WriteLn('File Name: ', FFileKit.GetFileName(TestPath));
  WriteLn('File Name (no ext): ', FFileKit.GetFileNameWithoutExt(TestPath));
  WriteLn('Directory: ', FFileKit.GetDirectory(TestPath));
  WriteLn('Extension: ', FFileKit.GetExtension(TestPath));
  WriteLn('Parent Directory: ', FFileKit.GetParentDir(TestPath));
  WriteLn('Is Absolute Path: ', FFileKit.IsAbsolutePath(TestPath));
  WriteLn('Normalized Path: ', FFileKit.NormalizePath(TestPath));
  
  // Change file extension
  WriteLn('With new extension: ', FFileKit.ChangeExtension(TestPath, '.dat'));
  WriteLn;
end;

procedure TFileOperations.ShowTempOperations;
var
  TempFile: string;
  TempDir: string;
begin
  WriteLn('Temporary File Operations:');
  WriteLn('------------------------');
  
  // Show system directory locations
  WriteLn('System Directories:');
  WriteLn('User Directory: ', FFileKit.GetUserDir);
  WriteLn('Current Directory: ', FFileKit.GetCurrentDir);
  WriteLn('Temp Directory: ', FFileKit.GetTempDir);
  WriteLn;
  
  // Create temporary files and directories
  TempFile := FFileKit.CreateTempFile('example_');
  WriteLn('Created temp file: ', TempFile);
  
  TempDir := FFileKit.CreateTempDirectory('example_');
  WriteLn('Created temp directory: ', TempDir);
  
  // Clean up temporary items
  FFileKit.DeleteFile(TempFile);
  FFileKit.DeleteDirectory(TempDir);
  WriteLn;
end;

procedure TFileOperations.ShowTextOperations;
var
  TextPath, BinaryPath: string;
begin
  WriteLn('Text File Operations:');
  WriteLn('-------------------');
  
  // Compare text vs binary files
  TextPath := FFileKit.CombinePaths(FBasePath, 'test/file1.txt');
  BinaryPath := FFileKit.CombinePaths(FBasePath, 'test/subdir2/file4.dat');
  
  // Check text file properties
  WriteLn(TextPath);
  WriteLn('Is Text File: ', FFileKit.IsTextFile(TextPath));
  WriteLn('Encoding: ', FFileKit.GetFileEncoding(TextPath));
  WriteLn;
  
  // Check binary file properties
  WriteLn(BinaryPath);
  WriteLn('Is Text File: ', FFileKit.IsTextFile(BinaryPath));
  WriteLn('Encoding: ', FFileKit.GetFileEncoding(BinaryPath));
  WriteLn;
end;

var
  FileOps: TFileOperations;
  WorkDir: string;
  FileKit: IFileKit; // Temporary filekit for initial setup

begin
  try
    // Create a FileKit instance for initial setup
    FileKit := TFSFactory.CreateFileKit;
    
    // Create a working directory for our examples
    WorkDir := FileKit.CombinePaths(FileKit.GetCurrentDir, 'FileKitTest');
    FileKit.CreateDirectory(WorkDir);
    
    // Create our file operations demo object
    FileOps := TFileOperations.Create(WorkDir);
    try
      WriteLn('TidyKit File System Example');
      WriteLn('==========================');
      WriteLn('Working Directory: ', WorkDir);
      WriteLn;
      
      // Run through all the demos
      FileOps.ShowBasicOperations;
      FileOps.ShowFileManipulation;
      FileOps.ShowDirectoryOperations;
      FileOps.ShowFileInformation;
      FileOps.ShowSearchOperations;
      FileOps.ShowPathOperations;
      FileOps.ShowTempOperations;
      FileOps.ShowTextOperations;
      
    finally
      FileOps.Free;
    end;
    
    // Clean up our working directory
    if FileKit.DirectoryExists(WorkDir) then
      FileKit.DeleteDirectory(WorkDir, True);
      
    WriteLn('Press Enter to exit...');
    ReadLn;
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
