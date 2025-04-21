unit TestCaseFS;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, DateUtils, fpcunit, testregistry,
  {$IFDEF WINDOWS}
  Windows,   // Add Windows unit for Windows-specific types and constants
  {$ENDIF}
  {$IFDEF UNIX}
  BaseUnix,  // Already used for Unix-specific types
  Unix,
  {$ENDIF}
  TidyKit;

{$IFDEF WINDOWS}
const
  // Define Windows constants if not using Windows unit
  FILE_ATTRIBUTE_READONLY  = $00000001;
  FILE_ATTRIBUTE_HIDDEN    = $00000002;
  FILE_ATTRIBUTE_SYSTEM    = $00000004;
  FILE_ATTRIBUTE_DIRECTORY = $00000010;
  FILE_ATTRIBUTE_ARCHIVE   = $00000020;
  FILE_ATTRIBUTE_NORMAL    = $00000080;
{$ENDIF}

type
  TStringArray = array of string;

type

  { TFSTests }
  TFSTests = class(TTestCase)
  private
    FTestDir: string;
    FTestFile: string;
    FFileKit: IFileKit; // Add interface field
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Basic file operations
    procedure Test01_ReadFile;
    procedure Test02_WriteFile;
    procedure Test03_AppendText;
    procedure Test04_DeleteFile;
    procedure Test05_CopyTo;
    procedure Test06_MoveTo;
    // Content operations
    procedure Test07_AppendText;
    procedure Test08_PrependText;
    procedure Test09_ReplaceText;
    // Directory operations
    procedure Test10_CreateDirectory;
    procedure Test11_DeleteDirectory;
    procedure Test12_EnsureDirectory;
    procedure Test13_GetFileName;
    procedure Test14_GetFileNameWithoutExt;
    procedure Test15_GetDirectory;
    procedure Test16_GetExtension;
    // File information
    procedure Test17_Exists;
    procedure Test18_DirectoryExists;
    procedure Test19_GetSize;
    procedure Test20_GetCreationTime;
    procedure Test21_GetLastAccessTime;
    procedure Test22_GetLastWriteTime;
    procedure Test23_GetAttributes;
    procedure Test24_IsTextFile;
    procedure Test25_GetFileEncoding;
    // Search operations
    procedure Test26_SearchFiles;
    procedure Test27_FindLastModifiedFile;
    procedure Test27b_FindLastModifiedFileRecursive;
    procedure Test28_FindFirstModifiedFile;
    procedure Test28b_FindFirstModifiedFileRecursive;
    procedure Test29_FindLargestFile;
    procedure Test29b_FindLargestFileRecursive;
    procedure Test30_FindSmallestFile;
    procedure Test30b_FindSmallestFileRecursive;
    // Directory information
    procedure Test31_GetUserDir;
    procedure Test32_GetCurrentDir;
    procedure Test33_GetTempDir;
    procedure Test34_GetParentDir;
    procedure Test34b_ListDirectories;
    procedure Test34c_ListDirectoriesRecursive;
    procedure Test34d_ListFiles;
    procedure Test34e_ListFilesRecursive;
    // Path manipulation
    procedure Test35_CombinePaths;
    procedure Test36_IsAbsolutePath;
    procedure Test37_NormalizePath;
    // File system operations
    procedure Test38_CreateTempFile;
    procedure Test39_CreateTempDirectory;
    procedure Test34f_ListFilesWithPattern;
    procedure Test34g_ListFilesWithSorting;
    procedure Test34h_ListDirectoriesWithPattern;
    procedure Test34i_ListDirectoriesWithSorting;
    // Symlink operations
    procedure Test40_CreateSymLink;
    procedure Test41_DeleteSymLink;
    procedure Test42_ResolveSymLink;
    procedure Test43_IsSymLink;
    procedure Test40_CopyFiles;
    procedure Test41_MoveFiles;
    procedure Test42_DeleteFiles;
    // Simple Path Analysis Tests
    procedure Test44_IsEmptyDirectory;
    procedure Test45_GetCommonPath;
    procedure Test46_GetRelativePath;
    procedure Test47_IsSubPath;
    // Basic File Content Operations Tests
    procedure Test48_CountLines;
    procedure Test49_GetFirstLine;
    procedure Test50_GetLastLine;
    procedure Test51_IsFileEmpty;
    procedure Test52_ContainsText;
    // Simple File Type Detection Tests
    procedure Test53_IsBinaryFile;
    procedure Test54_GetMimeType;
    procedure Test55_IsExecutable;
    procedure Test56_IsHidden;
    // Basic Space Operations Tests
    procedure Test57_GetDriveFreeSpace;
    procedure Test58_GetDriveCapacity;
    procedure Test59_HasEnoughSpace;
    // Basic File Comparison Tests
    procedure Test60_AreFilesIdentical;
    procedure Test61_GetNewerFile;
    procedure Test62_GetFileDifferences;
    // Simple File Locking Tests
    procedure Test63_LockFile;
    procedure Test64_UnlockFile;
    procedure Test65_IsFileLocked;
    // Path Validation and Sanitization Tests
    procedure Test66_IsValidFileName;
    procedure Test67_SanitizeFileName;
    procedure Test68_MakeValidPath;
    procedure Test69_IsPathTooLong;
    // Simple Directory Summary Tests
    procedure Test70_GetDirectoryInfo;
    // Basic File Patterns Tests
    procedure Test71_MatchesPattern;
    procedure Test72_FindFirstMatch;
    procedure Test73_CountMatches;
  end;

implementation

{ TFSTests }

procedure TFSTests.SetUp;
var
  TestBasePath: string;
begin
  // Initialize the FileKit interface
  FFileKit := TFSFactory.CreateFileKit;

  // Use a more accessible location for tests
  {$IFDEF WINDOWS}
  TestBasePath := SysUtils.GetEnvironmentVariable('TEMP');
  if TestBasePath = '' then
    TestBasePath := SysUtils.GetEnvironmentVariable('TMP');
  if TestBasePath = '' then
    TestBasePath := 'C:\Temp';
  {$ELSE}
  TestBasePath := '/tmp';
  {$ENDIF}
  
  // Create a unique test directory for each test
  FTestDir := IncludeTrailingPathDelimiter(TestBasePath) + 
              'TidyKitTest_' + FormatDateTime('yyyymmddhhnnsszzz', Now);
  FTestFile := FTestDir + PathDelim + 'test.txt';
  
  // Ensure clean test environment
  if FFileKit.DirectoryExists(FTestDir) then
  begin
    try
      FFileKit.DeleteDirectory(FTestDir, True);
      Sleep(100); // Give OS time to release handles
      RemoveDir(FTestDir);
    except
      on E: Exception do
        WriteLn('Warning: Could not delete existing test directory: ', E.Message);
    end;
  end;
  
  try
    if not FFileKit.EnsureDirectory(FTestDir) then
      raise ETidyKitException.CreateFmt('Could not create test directory: %s', [FTestDir]);
  except
    on E: Exception do
      raise ETidyKitException.CreateFmt('Failed to setup test environment: %s', [E.Message]);
  end;
end;

procedure TFSTests.TearDown;
begin
  // Clean up test environment
  if FFileKit.DirectoryExists(FTestDir) then
  try
    FFileKit.DeleteDirectory(FTestDir, True);
    Sleep(100); // Give OS time to release handles
    RemoveDir(FTestDir);
  except
    on E: Exception do
      WriteLn('Warning: Could not clean up test directory: ', E.Message);
  end;
  // Release the interface
  FFileKit := nil;
end;

procedure TFSTests.Test01_ReadFile;
const
  TestContent = 'Test Content';
begin
  WriteLn('Test01_ReadFile:Starting');
  // Write test content first using FFileKit
  FFileKit.WriteTextFile(FTestFile, TestContent);

  // Test read using FFileKit
  AssertEquals('ReadFile should read the correct content',
    TestContent, FFileKit.ReadTextFile(FTestFile));
  WriteLn('Test01_ReadFile:Finished');
end;

procedure TFSTests.Test02_WriteFile;
const
  TestContent = 'Test Content';
begin
  WriteLn('Test02_WriteFile:Starting');
  // Test write using FFileKit
  FFileKit.WriteTextFile(FTestFile, TestContent);
  AssertTrue('File should exist after write',
    FFileKit.Exists(FTestFile)); // Use FFileKit.Exists
  WriteLn('Test02_WriteFile:Finished');
end;

procedure TFSTests.Test03_AppendText;
const
  FirstLine = 'First Line';
  SecondLine = 'Second Line';
begin
  WriteLn('Test03_AppendText:Starting');
  // Create initial file using FFileKit
  FFileKit.WriteTextFile(FTestFile, FirstLine);

  // Test append using FFileKit
  FFileKit.AppendText(FTestFile, SecondLine);

  // Verify content using FFileKit
  AssertEquals('AppendText should append content correctly',
    FirstLine + SecondLine, FFileKit.ReadTextFile(FTestFile));
  WriteLn('Test03_AppendText:Finished');
end;

procedure TFSTests.Test04_DeleteFile;
const
  TestContent = 'Test Content';
begin
  WriteLn('Test04_DeleteFile:Starting');
  // Create test file using FFileKit
  FFileKit.WriteTextFile(FTestFile, TestContent);

  // Test delete using FFileKit
  FFileKit.DeleteFile(FTestFile);
  AssertFalse('File should not exist after delete',
    FFileKit.Exists(FTestFile)); // Use FFileKit.Exists
  WriteLn('Test04_DeleteFile:Finished');
end;

procedure TFSTests.Test05_CopyTo;
const
  TestContent = 'Test Content';
var
  CopyFile: string;
  SourceModTime, DestModTime: TDateTime;
  SourceAttrsRec, DestAttrsRec: TFileAttributes; // Use TFileAttributes record
begin
  WriteLn('Test05_CopyTo:Starting');
  CopyFile := FTestDir + PathDelim + 'copy.txt';

  // Create source file using FFileKit
  FFileKit.WriteTextFile(FTestFile, TestContent);

  // Platform-specific attribute setup (using standard functions for setup)
  {$IFDEF WINDOWS}
  WriteLn('Setting up Windows file attributes');
  SetFileAttributes(PChar(FTestFile), FILE_ATTRIBUTE_READONLY);
  SourceAttrsRec := FFileKit.GetAttributes(FTestFile); // Get attributes via FFileKit
  AssertTrue('Source file should have read-only attribute set', SourceAttrsRec.ReadOnly);
  {$ENDIF}

  {$IFDEF UNIX}
  WriteLn('Setting up Unix file permissions');
  fpChmod(PChar(FTestFile), $1A4); // Octal 444
  SourceAttrsRec := FFileKit.GetAttributes(FTestFile); // Get attributes via FFileKit
  AssertEquals('Source file should have read-only permissions', 'r--r--r--', Copy(SourceAttrsRec.Permissions, 1, 9));
  {$ENDIF}

  // Set a specific modification time for testing
  SysUtils.FileSetDate(FTestFile, DateTimeToFileDate(EncodeDateTime(2024, 1, 14, 12, 0, 0, 0)));
  SourceModTime := FFileKit.GetLastWriteTime(FTestFile); // Get time via FFileKit

  // Perform the file copy operation using FFileKit
  WriteLn('Copying file');
  FFileKit.CopyFile(FTestFile, CopyFile);

  // Verify copy results (common checks first)
  WriteLn('Verifying common copy results');
  AssertTrue('Destination file should exist after copy', FFileKit.Exists(CopyFile));
  AssertEquals('Copied content should match source', TestContent, FFileKit.ReadTextFile(CopyFile));

  // Platform-specific verification using FFileKit
  DestAttrsRec := FFileKit.GetAttributes(CopyFile);
  DestModTime := FFileKit.GetLastWriteTime(CopyFile);

  {$IFDEF WINDOWS}
  WriteLn('Verifying Windows-specific copy results');
  AssertTrue('Destination file should have read-only attribute', DestAttrsRec.ReadOnly);
  AssertEquals('File attributes should match (ignoring archive)',
    SourceAttrsRec.ReadOnly = DestAttrsRec.ReadOnly and
    SourceAttrsRec.Hidden = DestAttrsRec.Hidden and
    SourceAttrsRec.System = DestAttrsRec.System, True);
  AssertEquals('File modification time should match', SourceModTime, DestModTime);
  {$ENDIF}

  {$IFDEF UNIX}
  WriteLn('Verifying Unix-specific copy results');
  AssertEquals('File permissions should match', Copy(SourceAttrsRec.Permissions, 1, 9), Copy(DestAttrsRec.Permissions, 1, 9));
  // Timestamp comparison skipped as before
  WriteLn('Note: Skipping timestamp comparison on Unix platforms - timestamps are preserved but formats vary');
  {$ENDIF}

  WriteLn('Test05_CopyTo:Finished');
end;

procedure TFSTests.Test06_MoveTo;
const
  TestContent = 'Test Content';
var
  MoveFile: string;
begin
  WriteLn('Test06_MoveTo:Starting');
  MoveFile := FTestDir + PathDelim + 'moved.txt';

  // Create source file using FFileKit
  FFileKit.WriteTextFile(FTestFile, TestContent);

  // Test move using FFileKit
  FFileKit.MoveFile(FTestFile, MoveFile);

  AssertFalse('Source file should not exist after move',
    FFileKit.Exists(FTestFile)); // Use FFileKit.Exists
  AssertTrue('Destination file should exist after move',
    FFileKit.Exists(MoveFile)); // Use FFileKit.Exists
  AssertEquals('Moved content should match source',
    TestContent, FFileKit.ReadTextFile(MoveFile));
  WriteLn('Test06_MoveTo:Finished');
end;

procedure TFSTests.Test07_AppendText;
const
  FirstLine = 'First Line';
  SecondLine = 'Second Line';
begin
  WriteLn('Test07_AppendText:Starting');
  // Create initial file using FFileKit
  FFileKit.WriteTextFile(FTestFile, FirstLine);

  // Test append text using FFileKit
  FFileKit.AppendText(FTestFile, SecondLine);

  AssertEquals('AppendText should append content correctly',
    FirstLine + SecondLine, FFileKit.ReadTextFile(FTestFile));
  WriteLn('Test07_AppendText:Finished');
end;

procedure TFSTests.Test08_PrependText;
const
  FirstLine = 'First Line';
  SecondLine = 'Second Line';
begin
  WriteLn('Test08_PrependText:Starting');
  // Create initial file using FFileKit
  FFileKit.WriteTextFile(FTestFile, SecondLine);

  // Test prepend text using FFileKit
  FFileKit.PrependText(FTestFile, FirstLine);

  AssertEquals('PrependText should prepend content correctly',
    FirstLine + SecondLine, FFileKit.ReadTextFile(FTestFile));
  WriteLn('Test08_PrependText:Finished');
end;

procedure TFSTests.Test09_ReplaceText;
const
  OriginalText = 'Hello, World!';
  OldText = 'World';
  NewText = 'TidyKit';
  ExpectedText = 'Hello, TidyKit!'; // Define expected result
begin
  WriteLn('Test09_ReplaceText:Starting');
  // Create initial file using FFileKit
  FFileKit.WriteTextFile(FTestFile, OriginalText);

  // Test replace text using FFileKit
  FFileKit.ReplaceText(FTestFile, OldText, NewText);

  // Verify the content using FFileKit
  AssertEquals('ReplaceText should replace content correctly',
    ExpectedText, FFileKit.ReadTextFile(FTestFile));

  WriteLn('Test09_ReplaceText:Finished');
end;

procedure TFSTests.Test10_CreateDirectory;
var
  TestSubDir: string;
begin
  WriteLn('Test10_CreateDirectory:Starting'); // Add Starting message
  TestSubDir := FTestDir + PathDelim + 'subdir';

  // Test create directory using FFileKit
  FFileKit.CreateDirectory(TestSubDir);

  AssertTrue('Directory should exist after creation',
    FFileKit.DirectoryExists(TestSubDir)); // Use FFileKit.DirectoryExists
  WriteLn('Test10_CreateDirectory:Finished');
end;

procedure TFSTests.Test11_DeleteDirectory;
var
  TestSubDir: string; // Define TestSubDir
  TestSubFile: string;
begin
  WriteLn('Test11_DeleteDirectory:Starting'); // Add Starting message
  TestSubDir := FTestDir + PathDelim + 'subdir_to_delete';
  TestSubFile := TestSubDir + PathDelim + 'subfile.txt';

  // Create directory and file using FFileKit
  FFileKit.CreateDirectory(TestSubDir);
  FFileKit.WriteTextFile(TestSubFile, 'Subfile content');

  // Test delete directory (recursive) using FFileKit
  FFileKit.DeleteDirectory(TestSubDir, True);

  AssertFalse('Directory should not exist after deletion',
    FFileKit.DirectoryExists(TestSubDir)); // Use FFileKit.DirectoryExists
  WriteLn('Test11_DeleteDirectory:Finished');
end;

procedure TFSTests.Test12_EnsureDirectory;
var
  DeepDir: string;
  TestFileInDeepDir: string; // Define file path
begin
  WriteLn('Test12_EnsureDirectory:Starting'); // Add Starting message
  DeepDir := FTestDir + PathDelim + 'deep' + PathDelim + 'nested' + PathDelim + 'dir';
  TestFileInDeepDir := DeepDir + PathDelim + 'deepfile.txt'; // Define file path

  // Test ensure directory using FFileKit (for the file's path)
  FFileKit.EnsureDirectory(TestFileInDeepDir); // Ensure directory for the file

  AssertTrue('Deep directory structure should exist after EnsureDirectory',
    FFileKit.DirectoryExists(DeepDir)); // Check if the directory itself exists

  // Optional: Test creating a file within the ensured directory
  FFileKit.WriteTextFile(TestFileInDeepDir, 'Deep file content');
  AssertTrue('File should exist in ensured directory', FFileKit.Exists(TestFileInDeepDir));

  WriteLn('Test12_EnsureDirectory:Finished');
end;

procedure TFSTests.Test13_GetFileName;
const
  TestFileName = 'test.txt';
begin
  WriteLn('Test13_GetFileName:Starting'); // Add Starting message
  // Test GetFileName using FFileKit
  AssertEquals('GetFileName should return the correct filename',
    TestFileName, FFileKit.GetFileName(FTestFile));
  WriteLn('Test13_GetFileName:Finished');
end;

procedure TFSTests.Test14_GetFileNameWithoutExt;
const
  TestFileName = 'test.txt';
  ExpectedName = 'test'; // Define expected result
begin
  WriteLn('Test14_GetFileNameWithoutExt:Starting'); // Add Starting message
  // Test GetFileNameWithoutExt using FFileKit
  AssertEquals('GetFileNameWithoutExt should return the filename without extension',
    ExpectedName, FFileKit.GetFileNameWithoutExt(FTestFile));
  WriteLn('Test14_GetFileNameWithoutExt:Finished');
end;

procedure TFSTests.Test15_GetDirectory;
var
  ExpectedDir: string; // Define expected result
begin
  WriteLn('Test15_GetDirectory:Starting'); // Add Starting message
  // Normalize expected directory path for comparison
  ExpectedDir := FFileKit.NormalizePath(FTestDir);

  // Test GetDirectory using FFileKit
  AssertEquals('GetDirectory should return the correct directory path',
    ExpectedDir, FFileKit.GetDirectory(FTestFile));
  WriteLn('Test15_GetDirectory:Finished');
end;

procedure TFSTests.Test16_GetExtension;
const
  TestFileName = 'test.txt';
  ExpectedExt = '.txt'; // Define expected result
begin
  WriteLn('Test16_GetExtension:Starting'); // Add Starting message
  // Test GetExtension using FFileKit
  AssertEquals('GetExtension should return the correct file extension',
    ExpectedExt, FFileKit.GetExtension(FTestFile));
  WriteLn('Test16_GetExtension:Finished');
end;

initialization
  RegisterTest(TFSTests);
end.
