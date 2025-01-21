program FileKitExample;

{$mode objfpc}{$H+}{$J-}

uses
  Classes, SysUtils, TidyKit;

type
  TFileOperations = class
  private
    FBasePath: string;
    procedure CreateSampleFiles;
    procedure CleanupSampleFiles;
  public
    constructor Create(const ABasePath: string);
    destructor Destroy; override;
    
    procedure ShowBasicOperations;
    procedure ShowFileManipulation;
    procedure ShowDirectoryOperations;
    procedure ShowFileInformation;
    procedure ShowSearchOperations;
    procedure ShowPathOperations;
    procedure ShowTempOperations;
    procedure ShowTextOperations;
  end;

{ TFileOperations }

constructor TFileOperations.Create(const ABasePath: string);
begin
  FBasePath := TFileKit.NormalizePath(ABasePath);
  CreateSampleFiles;
end;

destructor TFileOperations.Destroy;
begin
  CleanupSampleFiles;
  inherited;
end;

procedure TFileOperations.CreateSampleFiles;
begin
  // Create test directory structure
  TFileKit.CreateDirectory(TFileKit.CombinePaths(FBasePath, 'test'));
  TFileKit.CreateDirectory(TFileKit.CombinePaths(FBasePath, 'test/subdir1'));
  TFileKit.CreateDirectory(TFileKit.CombinePaths(FBasePath, 'test/subdir2'));
  
  // Create some test files
  TFileKit.WriteFile(
    TFileKit.CombinePaths(FBasePath, 'test/file1.txt'),
    'This is file 1' + LineEnding + 'With multiple lines'
  );
  TFileKit.WriteFile(
    TFileKit.CombinePaths(FBasePath, 'test/file2.txt'),
    'This is file 2'
  );
  TFileKit.WriteFile(
    TFileKit.CombinePaths(FBasePath, 'test/subdir1/file3.txt'),
    'This is file 3'
  );
  TFileKit.WriteFile(
    TFileKit.CombinePaths(FBasePath, 'test/subdir2/file4.dat'),
    'Binary'#0'Data'#0'Here'
  );
end;

procedure TFileOperations.CleanupSampleFiles;
begin
  if TFileKit.DirectoryExists(TFileKit.CombinePaths(FBasePath, 'test')) then
    TFileKit.DeleteDirectory(TFileKit.CombinePaths(FBasePath, 'test'), True);
end;

procedure TFileOperations.ShowBasicOperations;
var
  FilePath: string;
  Content: string;
begin
  WriteLn('Basic File Operations:');
  WriteLn('---------------------');
  
  // File path for testing
  FilePath := TFileKit.CombinePaths(FBasePath, 'test/basic.txt');
  
  // Write file
  WriteLn('Writing to file...');
  TFileKit.WriteFile(FilePath, 'Hello, World!');
  
  // Read file
  WriteLn('Reading from file...');
  Content := TFileKit.ReadFile(FilePath);
  WriteLn('Content: ', Content);
  
  // Append to file
  WriteLn('Appending to file...');
  TFileKit.AppendFile(FilePath, LineEnding + 'Additional content');
  Content := TFileKit.ReadFile(FilePath);
  WriteLn('Updated content: ', Content);
  
  // Copy file
  WriteLn('Copying file...');
  TFileKit.CopyFile(FilePath, FilePath + '.backup');
  
  // Move file
  WriteLn('Moving file...');
  TFileKit.MoveFile(FilePath + '.backup', FilePath + '.moved');
  
  // Delete files
  WriteLn('Deleting files...');
  TFileKit.DeleteFile(FilePath);
  TFileKit.DeleteFile(FilePath + '.moved');
  
  WriteLn;
end;

procedure TFileOperations.ShowFileManipulation;
var
  FilePath: string;
begin
  WriteLn('File Content Manipulation:');
  WriteLn('------------------------');
  
  FilePath := TFileKit.CombinePaths(FBasePath, 'test/manipulation.txt');
  
  // Create initial content
  TFileKit.WriteFile(FilePath, 'Line 1' + LineEnding + 'Line 2' + LineEnding + 'Line 3');
  
  // Append text
  WriteLn('Appending text...');
  TFileKit.AppendText(FilePath, LineEnding + 'Appended line');
  
  // Prepend text
  WriteLn('Prepending text...');
  TFileKit.PrependText(FilePath, 'Prepended line' + LineEnding);
  
  // Replace text
  WriteLn('Replacing text...');
  TFileKit.ReplaceText(FilePath, 'Line', 'TEXT');
  
  // Show final content
  WriteLn('Final content:');
  WriteLn(TFileKit.ReadFile(FilePath));
  WriteLn;
end;

procedure TFileOperations.ShowDirectoryOperations;
var
  Files: TStringArray;
  Dirs: TStringArray;
  I: Integer;
begin
  WriteLn('Directory Operations:');
  WriteLn('--------------------');
  
  // List files (non-recursive)
  WriteLn('Files in test directory:');
  Files := TFileKit.ListFiles(TFileKit.CombinePaths(FBasePath, 'test'), '*.*', False, fsName);
  for I := 0 to High(Files) do
    WriteLn('- ', TFileKit.GetFileName(Files[I]));
  WriteLn;
  
  // List files (recursive)
  WriteLn('All files (recursive):');
  Files := TFileKit.ListFiles(TFileKit.CombinePaths(FBasePath, 'test'), '*.*', True, fsName);
  for I := 0 to High(Files) do
    WriteLn('- ', TFileKit.GetFileName(Files[I]));
  WriteLn;
  
  // List directories
  WriteLn('Directories:');
  Dirs := TFileKit.ListDirectories(TFileKit.CombinePaths(FBasePath, 'test'), '*', True, fsName);
  for I := 0 to High(Dirs) do
    WriteLn('- ', TFileKit.GetFileName(Dirs[I]));
  WriteLn;
  
  // Create and ensure directories
  WriteLn('Creating new directory...');
  TFileKit.CreateDirectory(TFileKit.CombinePaths(FBasePath, 'test/newdir'));
  WriteLn('Ensuring directory exists...');
  TFileKit.EnsureDirectory(TFileKit.CombinePaths(FBasePath, 'test/newdir/subdir'));
  
  WriteLn;
end;

procedure TFileOperations.ShowFileInformation;
var
  FilePath: string;
  Attrs: TFileAttributes;
begin
  WriteLn('File Information:');
  WriteLn('-----------------');
  
  FilePath := TFileKit.CombinePaths(FBasePath, 'test/file1.txt');
  
  // Basic information
  WriteLn('File: ', TFileKit.GetFileName(FilePath));
  WriteLn('Exists: ', TFileKit.Exists(FilePath));
  WriteLn('Size: ', TFileKit.GetSize(FilePath), ' bytes');
  WriteLn('Is Text File: ', TFileKit.IsTextFile(FilePath));
  WriteLn('Encoding: ', TFileKit.GetFileEncoding(FilePath));
  
  // Timestamps
  WriteLn('Created: ', DateTimeToStr(TFileKit.GetCreationTime(FilePath)));
  WriteLn('Last Accessed: ', DateTimeToStr(TFileKit.GetLastAccessTime(FilePath)));
  WriteLn('Last Modified: ', DateTimeToStr(TFileKit.GetLastWriteTime(FilePath)));
  
  // Attributes
  Attrs := TFileKit.GetAttributes(FilePath);
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
  
  // Search for files
  WriteLn('Searching for *.txt files:');
  Results := TFileKit.SearchFiles(TFileKit.CombinePaths(FBasePath, 'test'), '*.txt', True);
  for I := 0 to High(Results) do
  begin
    WriteLn('Found: ', Results[I].FileName);
    WriteLn('  Size: ', Results[I].Size, ' bytes');
    WriteLn('  Modified: ', DateTimeToStr(Results[I].LastModified));
  end;
  WriteLn;
  
  // Find specific files
  LatestFile := TFileKit.FindLastModifiedFile(
    TFileKit.CombinePaths(FBasePath, 'test'), '*.txt', True);
  OldestFile := TFileKit.FindFirstModifiedFile(
    TFileKit.CombinePaths(FBasePath, 'test'), '*.txt', True);
  LargestFile := TFileKit.FindLargestFile(
    TFileKit.CombinePaths(FBasePath, 'test'), '*.txt', True);
  SmallestFile := TFileKit.FindSmallestFile(
    TFileKit.CombinePaths(FBasePath, 'test'), '*.txt', True);
    
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
  
  TestPath := TFileKit.CombinePaths(FBasePath, 'test/subdir1/file.txt');
  
  WriteLn('Test Path: ', TestPath);
  WriteLn('File Name: ', TFileKit.GetFileName(TestPath));
  WriteLn('File Name (no ext): ', TFileKit.GetFileNameWithoutExt(TestPath));
  WriteLn('Directory: ', TFileKit.GetDirectory(TestPath));
  WriteLn('Extension: ', TFileKit.GetExtension(TestPath));
  WriteLn('Parent Directory: ', TFileKit.GetParentDir(TestPath));
  WriteLn('Is Absolute Path: ', TFileKit.IsAbsolutePath(TestPath));
  WriteLn('Normalized Path: ', TFileKit.NormalizePath(TestPath));
  
  // Change extension
  WriteLn('With new extension: ', TFileKit.ChangeExtension(TestPath, '.dat'));
  WriteLn;
end;

procedure TFileOperations.ShowTempOperations;
var
  TempFile: string;
  TempDir: string;
begin
  WriteLn('Temporary File Operations:');
  WriteLn('------------------------');
  
  // System directories
  WriteLn('System Directories:');
  WriteLn('User Directory: ', TFileKit.GetUserDir);
  WriteLn('Current Directory: ', TFileKit.GetCurrentDir);
  WriteLn('Temp Directory: ', TFileKit.GetTempDir);
  WriteLn;
  
  // Create temporary files and directories
  TempFile := TFileKit.CreateTempFile('example_');
  WriteLn('Created temp file: ', TempFile);
  
  TempDir := TFileKit.CreateTempDirectory('example_');
  WriteLn('Created temp directory: ', TempDir);
  
  // Cleanup
  TFileKit.DeleteFile(TempFile);
  TFileKit.DeleteDirectory(TempDir);
  WriteLn;
end;

procedure TFileOperations.ShowTextOperations;
var
  TextPath, BinaryPath: string;
begin
  WriteLn('Text File Operations:');
  WriteLn('-------------------');
  
  TextPath := TFileKit.CombinePaths(FBasePath, 'test/file1.txt');
  BinaryPath := TFileKit.CombinePaths(FBasePath, 'test/subdir2/file4.dat');
  
  WriteLn(TextPath);
  WriteLn('Is Text File: ', TFileKit.IsTextFile(TextPath));
  WriteLn('Encoding: ', TFileKit.GetFileEncoding(TextPath));
  WriteLn;
  
  WriteLn(BinaryPath);
  WriteLn('Is Text File: ', TFileKit.IsTextFile(BinaryPath));
  WriteLn('Encoding: ', TFileKit.GetFileEncoding(BinaryPath));
  WriteLn;
end;

var
  FileOps: TFileOperations;
  WorkDir: string;

begin
  try
    // Create working directory in the current directory
    WorkDir := TFileKit.CombinePaths(TFileKit.GetCurrentDir, 'FileKitTest');
    TFileKit.CreateDirectory(WorkDir);
    
    FileOps := TFileOperations.Create(WorkDir);
    try
      WriteLn('TFileKit Example Program');
      WriteLn('======================');
      WriteLn('Working Directory: ', WorkDir);
      WriteLn;
      
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
    
    // Cleanup working directory
    if TFileKit.DirectoryExists(WorkDir) then
      TFileKit.DeleteDirectory(WorkDir, True);
      
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
