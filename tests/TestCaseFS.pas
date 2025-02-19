unit TestCaseFS;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, DateUtils, fpcunit, testregistry,
  TidyKit;

type
  TStringArray = array of string;

type

  { TFSTests }
  TFSTests = class(TTestCase)
  private
    FTestDir: string;
    FTestFile: string;
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
    // Compression and decompression tests
    procedure Test44_CompressToZip;
    procedure Test45_DecompressFromZip;
    procedure Test46_CompressToTar;
    procedure Test47_DecompressFromTar;
    procedure Test44b_CompressToZipRecursive;
    procedure Test45b_DecompressFromZipRecursive;
    procedure Test46b_CompressToTarRecursive;
    procedure Test47b_DecompressFromTarRecursive;
  end;

implementation

{ TFSTests }

procedure TFSTests.SetUp;
var
  TestBasePath: string;
begin
  // Use a more accessible location for tests
  {$IFDEF WINDOWS}
  TestBasePath := GetEnvironmentVariable('TEMP');
  if TestBasePath = '' then
    TestBasePath := GetEnvironmentVariable('TMP');
  if TestBasePath = '' then
    TestBasePath := 'C:\Temp';
  {$ELSE}
  TestBasePath := '/tmp';
  {$ENDIF}
  
  FTestDir := IncludeTrailingPathDelimiter(TestBasePath) + 'TidyKitTest_' + FormatDateTime('yyyymmddhhnnss', Now);
  FTestFile := FTestDir + PathDelim + 'test.txt';
  
  // Ensure clean test environment
  if DirectoryExists(FTestDir) then
  begin
    try
      TFileKit.DeleteDirectory(FTestDir, True);
    except
      on E: Exception do
        WriteLn('Warning: Could not delete existing test directory: ', E.Message);
    end;
  end;
  
  try
    if not ForceDirectories(FTestDir) then
      raise ETidyKitException.CreateFmt('Could not create test directory: %s', [FTestDir]);
  except
    on E: Exception do
      raise ETidyKitException.CreateFmt('Failed to setup test environment: %s', [E.Message]);
  end;
end;

procedure TFSTests.TearDown;
begin
  // Clean up test environment
  if DirectoryExists(FTestDir) then
  try
    TFileKit.DeleteDirectory(FTestDir, True);
  except
    on E: Exception do
      WriteLn('Warning: Could not clean up test directory: ', E.Message);
  end;
end;

procedure TFSTests.Test01_ReadFile;
const
  TestContent = 'Test Content';
begin
  WriteLn('Test01_ReadFile:Starting');
  // Write test content first
  TFileKit.WriteTextFile(FTestFile, TestContent);
  
  // Test read
  AssertEquals('ReadFile should read the correct content',
    TestContent, TFileKit.ReadTextFile(FTestFile));
  WriteLn('Test01_ReadFile:Finished');
end;

procedure TFSTests.Test02_WriteFile;
const
  TestContent = 'Test Content';
begin
  WriteLn('Test02_WriteFile:Starting');
  // Test write
  TFileKit.WriteTextFile(FTestFile, TestContent);
  AssertTrue('File should exist after write',
    FileExists(FTestFile));
  WriteLn('Test02_WriteFile:Finished');
end;

procedure TFSTests.Test03_AppendText;
const
  FirstLine = 'First Line';
  SecondLine = 'Second Line';
begin
  WriteLn('Test03_AppendText:Starting');
  // Create initial file
  TFileKit.WriteTextFile(FTestFile, FirstLine);
  
  // Test append
  TFileKit.AppendText(FTestFile, SecondLine);
  
  // Verify content
  AssertEquals('AppendText should append content correctly',
    FirstLine + SecondLine, TFileKit.ReadTextFile(FTestFile));
  WriteLn('Test03_AppendText:Finished');
end;

procedure TFSTests.Test04_DeleteFile;
const
  TestContent = 'Test Content';
begin
  WriteLn('Test04_DeleteFile:Starting');
  // Create test file
  TFileKit.WriteTextFile(FTestFile, TestContent);
  
  // Test delete
  TFileKit.DeleteFile(FTestFile);
  AssertFalse('File should not exist after delete',
    FileExists(FTestFile));
  WriteLn('Test04_DeleteFile:Finished');
end;

procedure TFSTests.Test05_CopyTo;
const
  TestContent = 'Test Content';
var
  CopyFile: string;
begin
  WriteLn('Test05_CopyTo:Starting');
  CopyFile := FTestDir + PathDelim + 'copy.txt';
  
  // Create source file
  TFileKit.WriteTextFile(FTestFile, TestContent);
  
  // Test copy
  TFileKit.CopyFile(FTestFile, CopyFile);
  
  AssertTrue('Destination file should exist after copy',
    FileExists(CopyFile));
  AssertEquals('Copied content should match source',
    TestContent, TFileKit.ReadTextFile(CopyFile));
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
  
  // Create source file
  TFileKit.WriteTextFile(FTestFile, TestContent);
  
  // Test move
  TFileKit.MoveFile(FTestFile, MoveFile);
  
  AssertFalse('Source file should not exist after move',
    FileExists(FTestFile));
  AssertTrue('Destination file should exist after move',
    FileExists(MoveFile));
  AssertEquals('Moved content should match source',
    TestContent, TFileKit.ReadTextFile(MoveFile));
  WriteLn('Test06_MoveTo:Finished');
end;

procedure TFSTests.Test07_AppendText;
const
  FirstLine = 'First Line';
  SecondLine = 'Second Line';
begin
  WriteLn('Test07_AppendText:Starting');
  // Create initial file
  TFileKit.WriteTextFile(FTestFile, FirstLine);
  
  // Test append text
  TFileKit.AppendText(FTestFile, SecondLine);
  
  AssertEquals('AppendText should append content correctly',
    FirstLine + SecondLine, TFileKit.ReadTextFile(FTestFile));
  WriteLn('Test07_AppendText:Finished');
end;

procedure TFSTests.Test08_PrependText;
const
  FirstLine = 'First Line';
  SecondLine = 'Second Line';
begin
  WriteLn('Test08_PrependText:Starting');
  // Create initial file
  TFileKit.WriteTextFile(FTestFile, SecondLine);
  
  // Test prepend text
  TFileKit.PrependText(FTestFile, FirstLine);
  
  AssertEquals('PrependText should prepend content correctly',
    FirstLine + SecondLine, TFileKit.ReadTextFile(FTestFile));
  WriteLn('Test08_PrependText:Finished');
end;

procedure TFSTests.Test09_ReplaceText;
const
  OriginalText = 'Hello, World!';
  OldText = 'World';
  NewText = 'TidyKit';
begin
  WriteLn('Test09_ReplaceText:Starting');
  // Create initial file
  TFileKit.WriteTextFile(FTestFile, OriginalText);
  
  // Test replace text
  TFileKit.ReplaceText(FTestFile, OldText, NewText);
  
  AssertEquals('ReplaceText should replace content correctly',
    'Hello, TidyKit!', TFileKit.ReadTextFile(FTestFile));
  WriteLn('Test09_ReplaceText:Finished');
end;

procedure TFSTests.Test10_CreateDirectory;
var
  TestSubDir: string;
begin
  WriteLn('Test10_CreateDirectory:Starting');
  TestSubDir := FTestDir + PathDelim + 'subdir';
  
  // Test create directory
  TFileKit.CreateDirectory(TestSubDir);
  
  AssertTrue('Directory should exist after creation',
    DirectoryExists(TestSubDir));
  WriteLn('Test10_CreateDirectory:Finished');
end;

procedure TFSTests.Test11_DeleteDirectory;
var
  TestSubDir: string;
  TestSubFile: string;
begin
  WriteLn('Test11_DeleteDirectory:Starting');
  TestSubDir := FTestDir + PathDelim + 'subdir';
  TestSubFile := TestSubDir + PathDelim + 'test.txt';
  
  // Create test structure
  TFileKit.CreateDirectory(TestSubDir);
  TFileKit.WriteTextFile(TestSubFile, 'Test Content');
  
  // Test delete directory
  TFileKit.DeleteDirectory(TestSubDir, True);
  
  AssertFalse('Directory should not exist after deletion',
    DirectoryExists(TestSubDir));
  WriteLn('Test11_DeleteDirectory:Finished');
end;

procedure TFSTests.Test12_EnsureDirectory;
var
  DeepDir: string;
begin
  WriteLn('Test12_EnsureDirectory:Starting');
  DeepDir := FTestDir + PathDelim + 'deep' + PathDelim + 'path';
  
  // Test ensure directory
  TFileKit.EnsureDirectory(DeepDir + PathDelim + 'file.txt');
  
  AssertTrue('Directory structure should be created',
    DirectoryExists(DeepDir));
  WriteLn('Test12_EnsureDirectory:Finished');
end;

procedure TFSTests.Test13_GetFileName;
const
  TestFileName = 'test.txt';
begin
  WriteLn('Test13_GetFileName:Starting');
  AssertEquals('GetFileName should return correct name',
    TestFileName, TFileKit.GetFileName(FTestDir + PathDelim + TestFileName));
  WriteLn('Test13_GetFileName:Finished');
end;

procedure TFSTests.Test14_GetFileNameWithoutExt;
const
  TestFileName = 'test.txt';
begin
  WriteLn('Test14_GetFileNameWithoutExt:Starting');
  AssertEquals('GetFileNameWithoutExt should return correct name',
    'test', TFileKit.GetFileNameWithoutExt(FTestDir + PathDelim + TestFileName));
  WriteLn('Test14_GetFileNameWithoutExt:Finished');
end;

procedure TFSTests.Test15_GetDirectory;
begin
  WriteLn('Test15_GetDirectory:Starting');
  AssertEquals('GetDirectory should return correct directory',
    ExtractFileName(ExcludeTrailingPathDelimiter(FTestDir)),
    TFileKit.GetDirectory(FTestFile));
  WriteLn('Test15_GetDirectory:Finished');
end;

procedure TFSTests.Test16_GetExtension;
const
  TestFileName = 'test.txt';
begin
  WriteLn('Test16_GetExtension:Starting');
  AssertEquals('GetExtension should return correct extension',
    '.txt', TFileKit.GetExtension(FTestDir + PathDelim + TestFileName));
  WriteLn('Test16_GetExtension:Finished');
end;

procedure TFSTests.Test17_Exists;
begin
  WriteLn('Test17_Exists:Starting');
  // Create test file
  TFileKit.WriteTextFile(FTestFile, 'Test Content');
  
  AssertTrue('Exists should return true for existing file',
    TFileKit.Exists(FTestFile));
  AssertFalse('Exists should return false for non-existing file',
    TFileKit.Exists(FTestDir + PathDelim + 'nonexistent.txt'));
  WriteLn('Test17_Exists:Finished');
end;

procedure TFSTests.Test18_DirectoryExists;
begin
  WriteLn('Test18_DirectoryExists:Starting');
  AssertTrue('DirectoryExists should return true for existing directory',
    TFileKit.DirectoryExists(FTestDir));
  AssertFalse('DirectoryExists should return false for non-existing directory',
    TFileKit.DirectoryExists(FTestDir + PathDelim + 'nonexistent'));
  WriteLn('Test18_DirectoryExists:Finished');
end;

procedure TFSTests.Test19_GetSize;
const
  TestContent = 'Test Content';
begin
  WriteLn('Test19_GetSize:Starting');
  // Create test file
  TFileKit.WriteTextFile(FTestFile, TestContent);
  
  AssertEquals('GetSize should return correct file size',
    Length(TestContent), TFileKit.GetSize(FTestFile));
  WriteLn('Test19_GetSize:Finished');
end;

procedure TFSTests.Test20_GetCreationTime;
begin
  WriteLn('Test20_GetCreationTime:Starting');
  // Create test file
  TFileKit.WriteTextFile(FTestFile, 'Test Content');
  
  AssertTrue('GetCreationTime should return valid timestamp',
    TFileKit.GetCreationTime(FTestFile) > 0);
  WriteLn('Test20_GetCreationTime:Finished');
end;

procedure TFSTests.Test21_GetLastAccessTime;
begin
  WriteLn('Test21_GetLastAccessTime:Starting');
  // Create test file
  TFileKit.WriteTextFile(FTestFile, 'Test Content');
  
  AssertTrue('GetLastAccessTime should return valid timestamp',
    TFileKit.GetLastAccessTime(FTestFile) > 0);
  WriteLn('Test21_GetLastAccessTime:Finished');
end;

procedure TFSTests.Test22_GetLastWriteTime;
begin
  WriteLn('Test22_GetLastWriteTime:Starting');
  // Create test file
  TFileKit.WriteTextFile(FTestFile, 'Test Content');
  
  AssertTrue('GetLastWriteTime should return valid timestamp',
    TFileKit.GetLastWriteTime(FTestFile) > 0);
  WriteLn('Test22_GetLastWriteTime:Finished');
end;

procedure TFSTests.Test23_GetAttributes;
begin
  WriteLn('Test23_GetAttributes:Starting');
  // Create test file
  TFileKit.WriteTextFile(FTestFile, 'Test Content');
  
  AssertFalse('New file should not be read-only',
    TFileKit.GetAttributes(FTestFile).ReadOnly);
  WriteLn('Test23_GetAttributes:Finished');
end;

procedure TFSTests.Test24_IsTextFile;
var
  TextFile, BinaryFile: string;
  BinStream: TFileStream;
begin
  WriteLn('Test24_IsTextFile:Starting');
  TextFile := FTestDir + PathDelim + 'test.txt';
  BinaryFile := FTestDir + PathDelim + 'test.bin';
  
  // Create a text file
  TFileKit.WriteTextFile(TextFile, 'This is a text file');
  
  // Create a binary file
  BinStream := TFileStream.Create(BinaryFile, fmCreate);
  try
    BinStream.Write(#0#1#2#3#4#5, 6);
  finally
    BinStream.Free;
  end;
  
  try
    AssertTrue('Text file should be detected as text',
      TFileKit.IsTextFile(TextFile));
    AssertFalse('Binary file should not be detected as text',
      TFileKit.IsTextFile(BinaryFile));
  finally
    if FileExists(TextFile) then
      DeleteFile(TextFile);
    if FileExists(BinaryFile) then
      DeleteFile(BinaryFile);
  end;
  WriteLn('Test24_IsTextFile:Finished');
end;

procedure TFSTests.Test25_GetFileEncoding;
var
  UTF8File: string;
  UTF8Stream: TFileStream;
  UTF8BOM: array[0..2] of Byte;
  UTF8Text: AnsiString;
begin
  WriteLn('Test25_GetFileEncoding:Starting');
  UTF8File := FTestDir + PathDelim + 'utf8.txt';
  
  // Create UTF-8 file with BOM
  UTF8Stream := TFileStream.Create(UTF8File, fmCreate);
  try
    // Write UTF-8 BOM
    UTF8BOM[0] := $EF;
    UTF8BOM[1] := $BB;
    UTF8BOM[2] := $BF;
    UTF8Stream.WriteBuffer(UTF8BOM, 3);
    
    // Write some UTF-8 text
    UTF8Text := 'Hello, UTF-8 World!';
    UTF8Stream.WriteBuffer(UTF8Text[1], Length(UTF8Text));
  finally
    UTF8Stream.Free;
  end;
  
  try
    AssertEquals('UTF-8 file should be detected correctly',
      'UTF-8', TFileKit.GetFileEncoding(UTF8File));
  finally
    if FileExists(UTF8File) then
      DeleteFile(UTF8File);
  end;
  WriteLn('Test25_GetFileEncoding:Finished');
end;

procedure TFSTests.Test26_SearchFiles;
var
  Results: TSearchResults;
  SubDir: string;
begin
  WriteLn('Test26_SearchFiles: Starting');
  
  // Clean up the entire test directory first
  if DirectoryExists(FTestDir) then
    TFileKit.DeleteDirectory(FTestDir, True);
  ForceDirectories(FTestDir);

  // Create test files in root directory
  TFileKit.WriteTextFile(FTestDir + PathDelim + 'test1.txt', 'Content 1');
  TFileKit.WriteTextFile(FTestDir + PathDelim + 'test2.txt', 'Content 2');

  // Create subdirectory with more test files
  SubDir := FTestDir + PathDelim + 'subdir';
  TFileKit.CreateDirectory(SubDir);
  TFileKit.WriteTextFile(SubDir + PathDelim + 'test3.txt', 'Content 3');
  TFileKit.WriteTextFile(SubDir + PathDelim + 'test4.txt', 'Content 4');

  // Test non-recursive search (default)
  Results := TFileKit.SearchFiles(FTestDir, '*.txt');
  try
    AssertEquals('Non-recursive SearchFiles should only find files in root directory',
      2, Length(Results));
  finally
    SetLength(Results, 0);
  end;

  // Test recursive search
  Results := TFileKit.SearchFiles(FTestDir, '*.txt', True);
  try
    AssertEquals('Recursive SearchFiles should find all files',
      4, Length(Results));
  finally
    SetLength(Results, 0);
  end;
  WriteLn('Test26_SearchFiles: Finished');
end;

procedure TFSTests.Test27_FindLastModifiedFile;
var
  NewestFile: string;
  SR: TSearchRec;
  SubDir: string;
begin
  WriteLn('Test27_FindLastModifiedFile: Starting');

  // Clean up any existing txt files
  if FindFirst(FTestDir + PathDelim + '*.txt', faAnyFile, SR) = 0 then
  try
    repeat
      DeleteFile(FTestDir + PathDelim + SR.Name);
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;

  // Create test files with delay to ensure different timestamps
  TFileKit.WriteTextFile(FTestDir + PathDelim + 'test1.txt', 'Content 1');
  Sleep(1000);
  
  // Create subdirectory with newer file
  SubDir := FTestDir + PathDelim + 'subdir';
  TFileKit.CreateDirectory(SubDir);
  Sleep(1000);
  TFileKit.WriteTextFile(SubDir + PathDelim + 'test2.txt', 'Content 2');
  Sleep(1000);
  TFileKit.WriteTextFile(FTestDir + PathDelim + 'test3.txt', 'Content 3');
  Sleep(1000);
  
  // Test non-recursive search
  NewestFile := TFileKit.FindLastModifiedFile(FTestDir, '*.txt', False);
  AssertEquals('Non-recursive FindLastModifiedFile should find newest file in root directory',
    'test3.txt', NewestFile);

  WriteLn('Test27_FindLastModifiedFile: Finished');
end;

procedure TFSTests.Test27b_FindLastModifiedFileRecursive;
var
  NewestFile: string;
  SR: TSearchRec;
  SubDir: string;
begin
  WriteLn('Test27b_FindLastModifiedFileRecursive: Starting');

  // Clean up any existing txt files
  if FindFirst(FTestDir + PathDelim + '*.txt', faAnyFile, SR) = 0 then
  try
    repeat
      DeleteFile(FTestDir + PathDelim + SR.Name);
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;

  // Create test files with delay to ensure different timestamps
  TFileKit.WriteTextFile(FTestDir + PathDelim + 'test1.txt', 'Content 1');
  Sleep(1000);
  
  // Create subdirectory with newer file
  SubDir := FTestDir + PathDelim + 'subdir';
  TFileKit.CreateDirectory(SubDir);
  Sleep(1000);
  TFileKit.WriteTextFile(SubDir + PathDelim + 'test2.txt', 'Content 2');
  Sleep(1000);
  TFileKit.WriteTextFile(FTestDir + PathDelim + 'test3.txt', 'Content 3');
  Sleep(1000);
  
  // Test recursive search
  NewestFile := TFileKit.FindLastModifiedFile(FTestDir, '*.txt', True);
  AssertEquals('Recursive FindLastModifiedFile should find newest file in any directory',
    'test3.txt', NewestFile);

  WriteLn('Test27b_FindLastModifiedFileRecursive: Finished');
end;

procedure TFSTests.Test28_FindFirstModifiedFile;
var
  OldestFile: string;
  SR: TSearchRec;
  SubDir: string;
begin
  WriteLn('Test28_FindFirstModifiedFile: Starting');
  // Clean up any existing txt files
  if FindFirst(FTestDir + PathDelim + '*.txt', faAnyFile, SR) = 0 then
  try
    repeat
      DeleteFile(FTestDir + PathDelim + SR.Name);
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;

  // Create test files with delay to ensure different timestamps
  TFileKit.WriteTextFile(FTestDir + PathDelim + 'test1.txt', 'Content 1');
  FileSetDate(FTestDir + PathDelim + 'test1.txt', DateTimeToFileDate(EncodeDateTime(2025, 1, 14, 21, 48, 32, 0)));
  
  Sleep(2000);
  
  // Create subdirectory with newer files
  SubDir := FTestDir + PathDelim + 'subdir';
  TFileKit.CreateDirectory(SubDir);
  TFileKit.WriteTextFile(SubDir + PathDelim + 'test2.txt', 'Content 2');
  FileSetDate(SubDir + PathDelim + 'test2.txt', DateTimeToFileDate(EncodeDateTime(2025, 1, 14, 21, 48, 36, 0)));
  
  Sleep(2000);
  
  TFileKit.WriteTextFile(FTestDir + PathDelim + 'test3.txt', 'Content 3');
  FileSetDate(FTestDir + PathDelim + 'test3.txt', DateTimeToFileDate(EncodeDateTime(2025, 1, 14, 21, 48, 38, 0)));
  
  Sleep(2000);
  
  // Test non-recursive search
  WriteLn('Test28_FindFirstModifiedFile: Non-recursive search');
  OldestFile := TFileKit.FindFirstModifiedFile(FTestDir, '*.txt', False);
  AssertEquals('Non-recursive FindFirstModifiedFile should find oldest file in root directory',
    'test1.txt', OldestFile);

  WriteLn('Test28_FindFirstModifiedFile: Finished');
end;

procedure TFSTests.Test28b_FindFirstModifiedFileRecursive;
var
  OldestFile: string;
  SR: TSearchRec;
  SubDir: string;
begin
  WriteLn('Test28b_FindFirstModifiedFileRecursive: Starting');
  // Clean up any existing txt files
  if FindFirst(FTestDir + PathDelim + '*.txt', faAnyFile, SR) = 0 then
  try
    repeat
      DeleteFile(FTestDir + PathDelim + SR.Name);
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;

  // Create test files with delay to ensure different timestamps
  TFileKit.WriteTextFile(FTestDir + PathDelim + 'test1.txt', 'Content 1');
  FileSetDate(FTestDir + PathDelim + 'test1.txt', DateTimeToFileDate(EncodeDateTime(2025, 1, 14, 21, 48, 32, 0)));
  
  Sleep(2000);
  
  // Create subdirectory with newer files
  SubDir := FTestDir + PathDelim + 'subdir';
  TFileKit.CreateDirectory(SubDir);
  TFileKit.WriteTextFile(SubDir + PathDelim + 'test2.txt', 'Content 2');
  FileSetDate(SubDir + PathDelim + 'test2.txt', DateTimeToFileDate(EncodeDateTime(2025, 1, 14, 21, 48, 36, 0)));
  
  Sleep(2000);
  
  TFileKit.WriteTextFile(FTestDir + PathDelim + 'test3.txt', 'Content 3');
  FileSetDate(FTestDir + PathDelim + 'test3.txt', DateTimeToFileDate(EncodeDateTime(2025, 1, 14, 21, 48, 38, 0)));
  
  Sleep(2000);
  
  // Test recursive search
  WriteLn('Test28b_FindFirstModifiedFileRecursive: Recursive search');
  OldestFile := TFileKit.FindFirstModifiedFile(FTestDir, '*.txt', True);
  AssertEquals('Recursive FindFirstModifiedFile should find oldest file in any directory',
    'test1.txt', OldestFile);

  WriteLn('Test28b_FindFirstModifiedFileRecursive: Finished');
end;

procedure TFSTests.Test29_FindLargestFile;
var
  LargestFile: string;
  SR: TSearchRec;
begin
  WriteLn('Test29_FindLargestFile: Starting');
  // Clean up any existing txt files
  if FindFirst(FTestDir + PathDelim + '*.txt', faAnyFile, SR) = 0 then
  try
    repeat
      DeleteFile(FTestDir + PathDelim + SR.Name);
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;

  // Create test files with different sizes
  TFileKit.WriteTextFile(FTestDir + PathDelim + 'test1.txt', StringOfChar('A', 100));
  TFileKit.WriteTextFile(FTestDir + PathDelim + 'test3.txt', StringOfChar('C', 300));
  
  // Test non-recursive search
  WriteLn('Test29_FindLargestFile: Non-recursive search');
  LargestFile := TFileKit.FindLargestFile(FTestDir, '*.txt', False);
  AssertEquals('Non-recursive FindLargestFile should find largest file in root directory',
    'test3.txt', LargestFile);

  WriteLn('Test29_FindLargestFile: Finished');
end;

procedure TFSTests.Test29b_FindLargestFileRecursive;
var
  LargestFile: string;
  SR: TSearchRec;
  SubDir: string;
begin
  WriteLn('Test29b_FindLargestFileRecursive: Starting');
  // Clean up any existing txt files
  if FindFirst(FTestDir + PathDelim + '*.txt', faAnyFile, SR) = 0 then
  try
    repeat
      DeleteFile(FTestDir + PathDelim + SR.Name);
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;

  // Create test files with different sizes
  TFileKit.WriteTextFile(FTestDir + PathDelim + 'test1.txt', StringOfChar('A', 100));
  
  // Create subdirectory with larger file
  SubDir := FTestDir + PathDelim + 'subdir';
  TFileKit.CreateDirectory(SubDir);
  TFileKit.WriteTextFile(SubDir + PathDelim + 'test2.txt', StringOfChar('B', 500));
  
  TFileKit.WriteTextFile(FTestDir + PathDelim + 'test3.txt', StringOfChar('C', 300));
  
  // Test recursive search
  WriteLn('Test29b_FindLargestFileRecursive: Recursive search');
  LargestFile := TFileKit.FindLargestFile(FTestDir, '*.txt', True);
  AssertEquals('Recursive FindLargestFile should find largest file in any directory',
    'test2.txt', LargestFile);

  WriteLn('Test29b_FindLargestFileRecursive: Finished');
end;

procedure TFSTests.Test30_FindSmallestFile;
var
  SmallestFile: string;
  SR: TSearchRec;
begin
  WriteLn('Test30_FindSmallestFile: Starting');
  // Clean up any existing txt files
  if FindFirst(FTestDir + PathDelim + '*.txt', faAnyFile, SR) = 0 then
  try
    repeat
      DeleteFile(FTestDir + PathDelim + SR.Name);
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;

  // Create test files with different sizes
  TFileKit.WriteTextFile(FTestDir + PathDelim + 'test1.txt', StringOfChar('A', 100));
  TFileKit.WriteTextFile(FTestDir + PathDelim + 'test3.txt', StringOfChar('C', 50));
  
  // Test non-recursive search
  WriteLn('Test30_FindSmallestFile: Non-recursive search');
  SmallestFile := TFileKit.FindSmallestFile(FTestDir, '*.txt', False);
  AssertEquals('Non-recursive FindSmallestFile should find smallest file in root directory',
    'test3.txt', SmallestFile);

  WriteLn('Test30_FindSmallestFile: Finished');
end;

procedure TFSTests.Test30b_FindSmallestFileRecursive;
var
  SmallestFile: string;
  SR: TSearchRec;
  SubDir: string;
begin
  WriteLn('Test30b_FindSmallestFileRecursive: Starting');
  // Clean up any existing files and directories first
  if DirectoryExists(FTestDir) then
  begin
    TFileKit.DeleteDirectory(FTestDir, True);
    RemoveDir(FTestDir);
  end;
  TFileKit.CreateDirectory(FTestDir);

  // Create test files with different sizes
  TFileKit.WriteTextFile(FTestDir + PathDelim + 'test1.txt', StringOfChar('A', 100));
  
  // Create subdirectory with smaller file
  SubDir := FTestDir + PathDelim + 'subdir';
  TFileKit.CreateDirectory(SubDir);
  TFileKit.WriteTextFile(SubDir + PathDelim + 'test2.txt', StringOfChar('B', 25));
  
  TFileKit.WriteTextFile(FTestDir + PathDelim + 'test3.txt', StringOfChar('C', 50));
  
  // Test recursive search
  WriteLn('Test30b_FindSmallestFileRecursive: Recursive search');
  SmallestFile := TFileKit.FindSmallestFile(FTestDir, '*.txt', True);
  AssertEquals('Recursive FindSmallestFile should find smallest file in any directory',
    'test2.txt', SmallestFile);

  WriteLn('Test30b_FindSmallestFileRecursive: Finished');
end;

procedure TFSTests.Test31_GetUserDir;
var
  UserDir: string;
begin
  WriteLn('Test31_GetUserDir: Starting');
  UserDir := TFileKit.GetUserDir;
  AssertTrue('GetUserDir should return non-empty string', UserDir <> '');
  AssertTrue('GetUserDir should return existing directory', DirectoryExists(UserDir));
  WriteLn('Test31_GetUserDir: Finished');
end;

procedure TFSTests.Test32_GetCurrentDir;
var
  CurDir: string;
begin
  WriteLn('Test32_GetCurrentDir: Starting');
  CurDir := TFileKit.GetCurrentDir;
  AssertTrue('GetCurrentDir should return non-empty string', CurDir <> '');
  AssertTrue('GetCurrentDir should return existing directory', DirectoryExists(CurDir));
  AssertEquals('GetCurrentDir should match system current directory', 
    ExcludeTrailingPathDelimiter(GetCurrentDir), 
    ExcludeTrailingPathDelimiter(CurDir));
  WriteLn('Test32_GetCurrentDir: Finished');
end;

procedure TFSTests.Test33_GetTempDir;
var
  TempDir: string;
begin
  WriteLn('Test33_GetTempDir: Starting');
  TempDir := TFileKit.GetTempDir;
  AssertTrue('GetTempDir should return non-empty string', TempDir <> '');
  AssertTrue('GetTempDir should return existing directory', DirectoryExists(TempDir));
  WriteLn('Test33_GetTempDir: Finished');
end;

procedure TFSTests.Test34_GetParentDir;
begin
  WriteLn('Test34_GetParentDir: Starting');
  AssertEquals('GetParentDir should return correct parent directory',
    TFileKit.NormalizePath(ExcludeTrailingPathDelimiter(FTestDir)),
    TFileKit.NormalizePath(ExcludeTrailingPathDelimiter(TFileKit.GetParentDir(FTestFile))));
  WriteLn('Test34_GetParentDir: Finished');
end;

procedure TFSTests.Test34b_ListDirectories;
var
  SubDir1, SubDir2: string;
  Dirs: TStringArray;
  I: Integer;
  Found: Boolean;
begin
  WriteLn('Test34b_ListDirectories: Starting');
  
  // Clean up any existing files and directories first
  if DirectoryExists(FTestDir) then
  begin
    TFileKit.DeleteDirectory(FTestDir, True);
    RemoveDir(FTestDir);
  end;
  ForceDirectories(FTestDir);

  // Create exactly two test directories
  SubDir1 := TFileKit.CombinePaths(FTestDir, 'dir1');
  SubDir2 := TFileKit.CombinePaths(FTestDir, 'dir2');
  TFileKit.CreateDirectory(SubDir1);
  TFileKit.CreateDirectory(SubDir2);
  
  // Test non-recursive directory listing
  Dirs := TFileKit.ListDirectories(FTestDir, '*', False);
  WriteLn('Test34b_ListDirectories: Found ', Length(Dirs), ' directories:');
  for I := 0 to High(Dirs) do
    WriteLn('Test34b_ListDirectories: Dir[', I, '] = ', Dirs[I]);
  
  AssertEquals('ListDirectories should find exactly 2 directories', 2, Length(Dirs));
  
  // Verify both directories are found
  Found := False;
  for I := 0 to High(Dirs) do
    if TFileKit.NormalizePath(Dirs[I]) = TFileKit.NormalizePath(SubDir1) then
    begin
      Found := True;
      Break;
    end;
  AssertTrue('dir1 should be found', Found);
  
  Found := False;
  for I := 0 to High(Dirs) do
    if TFileKit.NormalizePath(Dirs[I]) = TFileKit.NormalizePath(SubDir2) then
    begin
      Found := True;
      Break;
    end;
  AssertTrue('dir2 should be found', Found);
  WriteLn('Test34b_ListDirectories: Finished');
end;

procedure TFSTests.Test34c_ListDirectoriesRecursive;
var
  SubDir1, SubDir2, SubSubDir: string;
  Dirs: TStringArray;
  I: Integer;
  Found: Boolean;
begin
  WriteLn('Test34c_ListDirectoriesRecursive: Starting');

  // Clean up any existing files and directories first
  if DirectoryExists(FTestDir) then
  begin
    TFileKit.DeleteDirectory(FTestDir, True);
    RemoveDir(FTestDir);
  end;
  ForceDirectories(FTestDir);

  // Create test directory structure with exactly three directories
  SubDir1 := TFileKit.CombinePaths(FTestDir, 'dir1');
  SubDir2 := TFileKit.CombinePaths(FTestDir, 'dir2');
  SubSubDir := TFileKit.CombinePaths(SubDir1, 'subdir1');
  TFileKit.CreateDirectory(SubDir1);
  TFileKit.CreateDirectory(SubDir2);
  TFileKit.CreateDirectory(SubSubDir);
  
  // Test recursive directory listing
  Dirs := TFileKit.ListDirectories(FTestDir, '*', True);
  WriteLn('Test34c_ListDirectoriesRecursive: Found ', Length(Dirs), ' directories:');
  for I := 0 to High(Dirs) do
    WriteLn('Test34c_ListDirectoriesRecursive: Dir[', I, '] = ', Dirs[I]);
  
  AssertEquals('ListDirectories should find exactly 3 directories recursively', 3, Length(Dirs));
  
  // Verify all directories are found
  Found := False;
  for I := 0 to High(Dirs) do
    if TFileKit.NormalizePath(Dirs[I]) = TFileKit.NormalizePath(SubDir1) then
    begin
      Found := True;
      Break;
    end;
  AssertTrue('dir1 should be found', Found);
  
  Found := False;
  for I := 0 to High(Dirs) do
    if TFileKit.NormalizePath(Dirs[I]) = TFileKit.NormalizePath(SubDir2) then
    begin
      Found := True;
      Break;
    end;
  AssertTrue('dir2 should be found', Found);
  
  Found := False;
  for I := 0 to High(Dirs) do
    if TFileKit.NormalizePath(Dirs[I]) = TFileKit.NormalizePath(SubSubDir) then
    begin
      Found := True;
      Break;
    end;
  AssertTrue('subdir1 should be found', Found);

  WriteLn('Test34c_ListDirectoriesRecursive: Finished');
end;

procedure TFSTests.Test34d_ListFiles;
var
  File1, File2: string;
  Files: TStringArray;
  I: Integer;
  Found: Boolean;
begin
  WriteLn('Test34d_ListFiles: Starting');
  
  // Clean up any existing files and directories first
  if DirectoryExists(FTestDir) then
  begin
    TFileKit.DeleteDirectory(FTestDir, True);
    RemoveDir(FTestDir);
  end;
  ForceDirectories(FTestDir);

  // Create only test files we want to test with
  File1 := TFileKit.CombinePaths(FTestDir, 'file1.txt');
  File2 := TFileKit.CombinePaths(FTestDir, 'file2.txt');
  TFileKit.WriteTextFile(File1, 'test1');
  TFileKit.WriteTextFile(File2, 'test2');
  
  // Test non-recursive file listing
  Files := TFileKit.ListFiles(FTestDir, '*', False);
  WriteLn('Test34d_ListFiles: Found ', Length(Files), ' files:');
  for I := 0 to High(Files) do
    WriteLn('Test34d_ListFiles: File[', I, '] = ', Files[I]);
  
  AssertEquals('ListFiles should find exactly 2 files', 2, Length(Files));
  
  // Verify both files are found
  Found := False;
  for I := 0 to High(Files) do
    if TFileKit.NormalizePath(Files[I]) = TFileKit.NormalizePath(File1) then
    begin
      Found := True;
      Break;
    end;
  AssertTrue('file1.txt should be found', Found);
  
  Found := False;
  for I := 0 to High(Files) do
    if TFileKit.NormalizePath(Files[I]) = TFileKit.NormalizePath(File2) then
    begin
      Found := True;
      Break;
    end;
  AssertTrue('file2.txt should be found', Found);

  WriteLn('Test34d_ListFiles: Finished');
end;

procedure TFSTests.Test34e_ListFilesRecursive;
var
  File1, File2, SubDir, File3: string;
  Files: TStringArray;
  I: Integer;
  Found: Boolean;
begin
  WriteLn('Test34e_ListFilesRecursive: Starting');

  // Clean up any existing files and directories first
  if DirectoryExists(FTestDir) then
  begin
    TFileKit.DeleteDirectory(FTestDir, True);
    RemoveDir(FTestDir);
  end;
  ForceDirectories(FTestDir);

  // Create test directory structure with exactly the files we want to test
  File1 := TFileKit.CombinePaths(FTestDir, 'file1.txt');
  File2 := TFileKit.CombinePaths(FTestDir, 'file2.txt');
  SubDir := TFileKit.CombinePaths(FTestDir, 'subdir');
  File3 := TFileKit.CombinePaths(SubDir, 'file3.txt');
  
  TFileKit.WriteTextFile(File1, 'test1');
  TFileKit.WriteTextFile(File2, 'test2');
  TFileKit.CreateDirectory(SubDir);
  TFileKit.WriteTextFile(File3, 'test3');
  
  // Test recursive file listing
  Files := TFileKit.ListFiles(FTestDir, '*', True);
  WriteLn('Test34e_ListFilesRecursive: Found ', Length(Files), ' files:');
  for I := 0 to High(Files) do
    WriteLn('Test34e_ListFilesRecursive: File[', I, '] = ', Files[I]);
  
  AssertEquals('ListFiles should find exactly 3 files recursively', 3, Length(Files));
  
  // Verify all files are found
  Found := False;
  for I := 0 to High(Files) do
    if TFileKit.NormalizePath(Files[I]) = TFileKit.NormalizePath(File1) then
    begin
      Found := True;
      Break;
    end;
  AssertTrue('file1.txt should be found', Found);
  
  Found := False;
  for I := 0 to High(Files) do
    if TFileKit.NormalizePath(Files[I]) = TFileKit.NormalizePath(File2) then
    begin
      Found := True;
      Break;
    end;
  AssertTrue('file2.txt should be found', Found);
  
  Found := False;
  for I := 0 to High(Files) do
    if TFileKit.NormalizePath(Files[I]) = TFileKit.NormalizePath(File3) then
    begin
      Found := True;
      Break;
    end;
  AssertTrue('file3.txt should be found', Found);

  WriteLn('Test34e_ListFilesRecursive: Finished');
end;

procedure TFSTests.Test34f_ListFilesWithPattern;
var
  File1, File2, File3, File4: string;
  Files: TStringArray;
begin
  WriteLn('Test34f_ListFilesWithPattern: Starting');
  // Clean up any existing files and directories first
  if DirectoryExists(FTestDir) then
  begin
    TFileKit.DeleteDirectory(FTestDir, True);
    RemoveDir(FTestDir);
  end;
  TFileKit.CreateDirectory(FTestDir);

  // Create test files with different extensions
  File1 := TFileKit.CombinePaths(FTestDir, 'test1.txt');
  File2 := TFileKit.CombinePaths(FTestDir, 'test2.txt');
  File3 := TFileKit.CombinePaths(FTestDir, 'data1.dat');
  File4 := TFileKit.CombinePaths(FTestDir, 'data2.dat');
  
  TFileKit.WriteTextFile(File1, 'test1');
  TFileKit.WriteTextFile(File2, 'test2');
  TFileKit.WriteTextFile(File3, 'data1');
  TFileKit.WriteTextFile(File4, 'data2');
  
  // Test pattern matching for .txt files
  Files := TFileKit.ListFiles(FTestDir, '*.txt');
  AssertEquals('ListFiles should find 2 .txt files', 2, Length(Files));
  
  // Test pattern matching for .dat files
  Files := TFileKit.ListFiles(FTestDir, '*.dat');
  AssertEquals('ListFiles should find 2 .dat files', 2, Length(Files));
  
  // Test pattern matching with prefix
  Files := TFileKit.ListFiles(FTestDir, 'test*.*');
  AssertEquals('ListFiles should find 2 test files', 2, Length(Files));

  WriteLn('Test34f_ListFilesWithPattern: Finished');
end;

procedure TFSTests.Test34g_ListFilesWithSorting;
var
  File1, File2, File3: string;
  Files: TStringArray;
  begin
  WriteLn('Test34g_ListFilesWithSorting: Starting');

  // Clean up any existing files and directories first
  if DirectoryExists(FTestDir) then
  begin
    TFileKit.DeleteDirectory(FTestDir, True);
    RemoveDir(FTestDir);
  end;
  TFileKit.CreateDirectory(FTestDir);

  // Create test files with different sizes and timestamps
  File1 := TFileKit.CombinePaths(FTestDir, 'b_file.txt');  // Middle name
  File2 := TFileKit.CombinePaths(FTestDir, 'a_file.txt');  // First name
  File3 := TFileKit.CombinePaths(FTestDir, 'c_file.txt');  // Last name
  
  // Create files with different sizes
  TFileKit.WriteTextFile(File1, StringOfChar('B', 200));  // 200 bytes
  TFileKit.WriteTextFile(File2, StringOfChar('A', 100));  // 100 bytes
  TFileKit.WriteTextFile(File3, StringOfChar('C', 300));  // 300 bytes
  
  // Set different timestamps
  FileSetDate(File1, DateTimeToFileDate(EncodeDateTime(2024, 1, 2, 0, 0, 0, 0)));
  FileSetDate(File2, DateTimeToFileDate(EncodeDateTime(2024, 1, 1, 0, 0, 0, 0)));
  FileSetDate(File3, DateTimeToFileDate(EncodeDateTime(2024, 1, 3, 0, 0, 0, 0)));
  
  // Test name sorting (ascending)
  Files := TFileKit.ListFiles(FTestDir, '*', False, fsName);
  AssertEquals('First file should be a_file.txt', 'a_file.txt', ExtractFileName(Files[0]));
  AssertEquals('Last file should be c_file.txt', 'c_file.txt', ExtractFileName(Files[2]));
  
  // Test name sorting (descending)
  Files := TFileKit.ListFiles(FTestDir, '*', False, fsNameDesc);
  AssertEquals('First file should be c_file.txt', 'c_file.txt', ExtractFileName(Files[0]));
  AssertEquals('Last file should be a_file.txt', 'a_file.txt', ExtractFileName(Files[2]));
  
  // Test date sorting (ascending)
  Files := TFileKit.ListFiles(FTestDir, '*', False, fsDate);
  AssertEquals('First file should be a_file.txt', 'a_file.txt', ExtractFileName(Files[0]));
  AssertEquals('Last file should be c_file.txt', 'c_file.txt', ExtractFileName(Files[2]));
  
  // Test date sorting (descending)
  Files := TFileKit.ListFiles(FTestDir, '*', False, fsDateDesc);
  AssertEquals('First file should be c_file.txt', 'c_file.txt', ExtractFileName(Files[0]));
  AssertEquals('Last file should be a_file.txt', 'a_file.txt', ExtractFileName(Files[2]));
  
  // Test size sorting (ascending)
  Files := TFileKit.ListFiles(FTestDir, '*', False, fsSize);
  AssertEquals('First file should be a_file.txt', 'a_file.txt', ExtractFileName(Files[0]));
  AssertEquals('Last file should be c_file.txt', 'c_file.txt', ExtractFileName(Files[2]));
  
  // Test size sorting (descending)
  Files := TFileKit.ListFiles(FTestDir, '*', False, fsSizeDesc);
  AssertEquals('First file should be c_file.txt', 'c_file.txt', ExtractFileName(Files[0]));
  AssertEquals('Last file should be a_file.txt', 'a_file.txt', ExtractFileName(Files[2]));

  WriteLn('Test34g_ListFilesWithSorting: Finished');
end;

procedure TFSTests.Test34h_ListDirectoriesWithPattern;
var
  Dir1, Dir2, Dir3, Dir4: string;
  Dirs: TStringArray;
begin
  WriteLn('Test34h_ListDirectoriesWithPattern: Starting');
  // Clean up any existing files and directories first
  if DirectoryExists(FTestDir) then
  begin
    TFileKit.DeleteDirectory(FTestDir, True);
    RemoveDir(FTestDir);
  end;
  TFileKit.CreateDirectory(FTestDir);

  // Create test directories with different names
  Dir1 := TFileKit.CombinePaths(FTestDir, 'test_dir1');
  Dir2 := TFileKit.CombinePaths(FTestDir, 'test_dir2');
  Dir3 := TFileKit.CombinePaths(FTestDir, 'data_dir1');
  Dir4 := TFileKit.CombinePaths(FTestDir, 'data_dir2');
  
  TFileKit.CreateDirectory(Dir1);
  TFileKit.CreateDirectory(Dir2);
  TFileKit.CreateDirectory(Dir3);
  TFileKit.CreateDirectory(Dir4);
  
  // Test pattern matching for test directories
  Dirs := TFileKit.ListDirectories(FTestDir, 'test_*');
  AssertEquals('ListDirectories should find 2 test directories', 2, Length(Dirs));
  
  // Test pattern matching for data directories
  Dirs := TFileKit.ListDirectories(FTestDir, 'data_*');
  AssertEquals('ListDirectories should find 2 data directories', 2, Length(Dirs));
  
  // Test pattern matching with number
  Dirs := TFileKit.ListDirectories(FTestDir, '*1');
  AssertEquals('ListDirectories should find 2 directories ending with 1', 2, Length(Dirs));

  WriteLn('Test34h_ListDirectoriesWithPattern: Finished');  
end;

procedure TFSTests.Test34i_ListDirectoriesWithSorting;
var
  Dir1, Dir2, Dir3: string;
  Dirs: TStringArray;
begin
  WriteLn('Test34i_ListDirectoriesWithSorting: Starting');

  // Clean up any existing files and directories first
  if DirectoryExists(FTestDir) then
  begin
    TFileKit.DeleteDirectory(FTestDir, True);
    RemoveDir(FTestDir);
  end;
  TFileKit.CreateDirectory(FTestDir);

  // Create test directories
  Dir1 := TFileKit.CombinePaths(FTestDir, 'b_dir');  // Middle name
  Dir2 := TFileKit.CombinePaths(FTestDir, 'a_dir');  // First name
  Dir3 := TFileKit.CombinePaths(FTestDir, 'c_dir');  // Last name
  
  TFileKit.CreateDirectory(Dir1);
  TFileKit.CreateDirectory(Dir2);
  TFileKit.CreateDirectory(Dir3);
  
  // Set different timestamps
  FileSetDate(Dir1, DateTimeToFileDate(EncodeDateTime(2024, 1, 2, 0, 0, 0, 0)));
  FileSetDate(Dir2, DateTimeToFileDate(EncodeDateTime(2024, 1, 1, 0, 0, 0, 0)));
  FileSetDate(Dir3, DateTimeToFileDate(EncodeDateTime(2024, 1, 3, 0, 0, 0, 0)));
  
  // Test name sorting (ascending)
  Dirs := TFileKit.ListDirectories(FTestDir, '*', False, fsName);
  AssertEquals('First directory should be a_dir', 'a_dir', ExtractFileName(Dirs[0]));
  AssertEquals('Last directory should be c_dir', 'c_dir', ExtractFileName(Dirs[2]));
  
  // Test name sorting (descending)
  Dirs := TFileKit.ListDirectories(FTestDir, '*', False, fsNameDesc);
  AssertEquals('First directory should be c_dir', 'c_dir', ExtractFileName(Dirs[0]));
  AssertEquals('Last directory should be a_dir', 'a_dir', ExtractFileName(Dirs[2]));
  
  // Test date sorting (ascending)
  Dirs := TFileKit.ListDirectories(FTestDir, '*', False, fsDate);
  AssertEquals('First directory should be a_dir (oldest)', 'a_dir', ExtractFileName(Dirs[0]));
  AssertEquals('Last directory should be c_dir (newest)', 'c_dir', ExtractFileName(Dirs[2]));
  
  // Test date sorting (descending)
  Dirs := TFileKit.ListDirectories(FTestDir, '*', False, fsDateDesc);
  AssertEquals('First directory should be c_dir (newest)', 'c_dir', ExtractFileName(Dirs[0]));
  AssertEquals('Last directory should be a_dir (oldest)', 'a_dir', ExtractFileName(Dirs[2]));

  WriteLn('Test34i_ListDirectoriesWithSorting: Finished');
end;

procedure TFSTests.Test35_CombinePaths;
begin
  WriteLn('Test35_CombinePaths: Starting');
  AssertEquals('CombinePaths should combine paths correctly',
    TFileKit.NormalizePath(IncludeTrailingPathDelimiter(FTestDir) + 'test.txt'),
    TFileKit.NormalizePath(TFileKit.CombinePaths(FTestDir, 'test.txt')));
    
  AssertEquals('CombinePaths should handle empty first path',
    'test.txt',
    TFileKit.CombinePaths('', 'test.txt'));
    
  AssertEquals('CombinePaths should handle empty second path',
    TFileKit.NormalizePath(ExcludeTrailingPathDelimiter(FTestDir)),
    TFileKit.NormalizePath(ExcludeTrailingPathDelimiter(TFileKit.CombinePaths(FTestDir, ''))));

  WriteLn('Test35_CombinePaths: Finished');
end;

procedure TFSTests.Test36_IsAbsolutePath;
begin
  WriteLn('Test36_IsAbsolutePath: Starting');
  {$IFDEF WINDOWS}
  AssertTrue('Windows drive path should be absolute',
    TFileKit.IsAbsolutePath('C:\Windows'));
  AssertFalse('Relative Windows path should not be absolute',
    TFileKit.IsAbsolutePath('Windows\System32'));
  {$ENDIF}
  
  {$IFDEF UNIX}
  AssertTrue('Unix root path should be absolute',
    TFileKit.IsAbsolutePath('/usr/local'));
  AssertFalse('Relative Unix path should not be absolute',
    TFileKit.IsAbsolutePath('usr/local'));
  {$ENDIF}
  WriteLn('Test36_IsAbsolutePath: Finished');
end;

procedure TFSTests.Test37_NormalizePath;
var
  TestPath: string;
begin
  WriteLn('Test37_NormalizePath: Starting');
  {$IFDEF WINDOWS}
  TestPath := 'C:/Windows/System32';
  AssertEquals('NormalizePath should convert forward slashes to backslashes on Windows',
    'C:\Windows\System32',
    TFileKit.NormalizePath(TestPath));
  {$ENDIF}
  
  {$IFDEF UNIX}
  TestPath := '/usr\local\bin';
  AssertEquals('NormalizePath should convert backslashes to forward slashes on Unix',
    '/usr/local/bin',
    TFileKit.NormalizePath(TestPath));
  {$ENDIF}
  WriteLn('Test37_NormalizePath: Finished');
end;

procedure TFSTests.Test38_CreateTempFile;
var
  TempFile: string;
begin
  WriteLn('Test38_CreateTempFile: Starting');
  TempFile := TFileKit.CreateTempFile('test');
  try
    AssertTrue('CreateTempFile should create file', FileExists(TempFile));
    AssertEquals('CreateTempFile should create file with prefix',
      'test_', Copy(ExtractFileName(TempFile), 1, 5));
  finally
    if FileExists(TempFile) then
      DeleteFile(TempFile);
  end;
  WriteLn('Test38_CreateTempFile: Finished');
end;

procedure TFSTests.Test39_CreateTempDirectory;
var
  TempDir: string;
begin
  WriteLn('Test39_CreateTempDirectory: Starting');
  TempDir := TFileKit.CreateTempDirectory('test');
  try
    AssertTrue('CreateTempDirectory should create directory',
      DirectoryExists(TempDir));
    AssertEquals('CreateTempDirectory should create directory with prefix',
      'test_', Copy(ExtractFileName(TempDir), 1, 5));
  finally
    if DirectoryExists(TempDir) then
      RemoveDir(TempDir);
  end;
  WriteLn('Test39_CreateTempDirectory: Finished');
end;

procedure TFSTests.Test40_CreateSymLink;
var
  TargetFile, LinkFile: string;
begin
  WriteLn('Test40_CreateSymLink: Starting');
  TargetFile := TFileKit.CombinePaths(FTestDir, 'target.txt');
  LinkFile := TFileKit.CombinePaths(FTestDir, 'link.txt');
  
  try
    // Create a target file
    TFileKit.WriteTextFile(TargetFile, 'Test content');
    
    // Create symlink
    try
      TFileKit.CreateSymLink(TargetFile, LinkFile);
      
      // Only verify if symlink creation succeeded
      AssertTrue('Symlink should exist', TFileKit.Exists(LinkFile));
      AssertTrue('Path should be a symlink', TFileKit.IsSymLink(LinkFile));
      
      // Verify content can be read through symlink
      AssertEquals('Content should be readable through symlink', 
                 'Test content', 
                 TFileKit.ReadTextFile(LinkFile));
    except
      on E: ETidyKitException do
      begin
        // Skip test if we don't have permissions
        if (Pos('privilege', E.Message) > 0) or 
           (Pos('permission', E.Message) > 0) then
          Ignore('Skipping symlink test - insufficient privileges');
        raise;
      end;
    end;
  finally
    if TFileKit.Exists(LinkFile) then
      TFileKit.DeleteFile(LinkFile);
    if TFileKit.Exists(TargetFile) then
      TFileKit.DeleteFile(TargetFile);
  end;
  WriteLn('Test40_CreateSymLink: Finished');
end;

procedure TFSTests.Test41_DeleteSymLink;
var
  TargetFile, LinkFile: string;
begin
  WriteLn('Test41_DeleteSymLink: Starting');

  TargetFile := TFileKit.CombinePaths(FTestDir, 'target.txt');
  LinkFile := TFileKit.CombinePaths(FTestDir, 'link.txt');
  
  try
    // Create target and symlink
    TFileKit.WriteTextFile(TargetFile, 'Test content');
    try
      TFileKit.CreateSymLink(TargetFile, LinkFile);
      
      // Only proceed with delete test if create succeeded
      if TFileKit.IsSymLink(LinkFile) then
      begin
        // Delete symlink
        TFileKit.DeleteSymLink(LinkFile);
        
        // Verify symlink was deleted but target remains
        AssertFalse('Symlink should not exist', TFileKit.Exists(LinkFile));
        AssertTrue('Target file should still exist', TFileKit.Exists(TargetFile));
      end
      else
        Ignore('Skipping symlink deletion test - could not create symlink');
    except
      on E: ETidyKitException do
      begin
        if (Pos('privilege', E.Message) > 0) or 
           (Pos('permission', E.Message) > 0) then
          Ignore('Skipping symlink deletion test - insufficient privileges');
        raise;
      end;
    end;
  finally
    if TFileKit.Exists(LinkFile) then
      TFileKit.DeleteFile(LinkFile);
    if TFileKit.Exists(TargetFile) then
      TFileKit.DeleteFile(TargetFile);
  end;
  WriteLn('Test41_DeleteSymLink: Finished');
end;

procedure TFSTests.Test42_ResolveSymLink;
var
  TargetFile, LinkFile, ResolvedPath: string;
begin
  WriteLn('Test42_ResolveSymLink: Starting');

  TargetFile := TFileKit.CombinePaths(FTestDir, 'target.txt');
  LinkFile := TFileKit.CombinePaths(FTestDir, 'link.txt');
  
  try
    // Create target and symlink
    TFileKit.WriteTextFile(TargetFile, 'Test content');
    try
      TFileKit.CreateSymLink(TargetFile, LinkFile);
      
      // Only proceed with resolve test if create succeeded
      if TFileKit.IsSymLink(LinkFile) then
      begin
        // Resolve symlink
        ResolvedPath := TFileKit.ResolveSymLink(LinkFile);
        
        // Verify resolved path matches target
        AssertEquals('Resolved path should match target', 
                   TFileKit.NormalizePath(TargetFile), 
                   TFileKit.NormalizePath(ResolvedPath));
      end
      else
        Ignore('Skipping symlink resolution test - could not create symlink');
    except
      on E: ETidyKitException do
      begin
        if (Pos('privilege', E.Message) > 0) or 
           (Pos('permission', E.Message) > 0) then
          Ignore('Skipping symlink resolution test - insufficient privileges');
        raise;
      end;
    end;
  finally
    if TFileKit.Exists(LinkFile) then
      TFileKit.DeleteFile(LinkFile);
    if TFileKit.Exists(TargetFile) then
      TFileKit.DeleteFile(TargetFile);
  end;
  WriteLn('Test42_ResolveSymLink: Finished');
end;

procedure TFSTests.Test43_IsSymLink;
var
  TargetFile, LinkFile: string;
begin
  WriteLn('Test43_IsSymLink: Starting');

  TargetFile := TFileKit.CombinePaths(FTestDir, 'target.txt');
  LinkFile := TFileKit.CombinePaths(FTestDir, 'link.txt');
  
  try
    // Create target and symlink
    TFileKit.WriteTextFile(TargetFile, 'Test content');
    try
      TFileKit.CreateSymLink(TargetFile, LinkFile);
      
      // Only proceed with detection test if create succeeded
      if TFileKit.Exists(LinkFile) then
      begin
        // Verify symlink detection
        AssertTrue('Link should be detected as symlink', TFileKit.IsSymLink(LinkFile));
        AssertFalse('Target should not be detected as symlink', TFileKit.IsSymLink(TargetFile));
        AssertFalse('Non-existent file should not be detected as symlink', 
                  TFileKit.IsSymLink(TFileKit.CombinePaths(FTestDir, 'nonexistent.txt')));
      end
      else
        Ignore('Skipping symlink detection test - could not create symlink');
    except
      on E: ETidyKitException do
      begin
        if (Pos('privilege', E.Message) > 0) or 
           (Pos('permission', E.Message) > 0) then
          Ignore('Skipping symlink detection test - insufficient privileges');
        raise;
      end;
    end;
  finally
    if TFileKit.Exists(LinkFile) then
      TFileKit.DeleteFile(LinkFile);
    if TFileKit.Exists(TargetFile) then
      TFileKit.DeleteFile(TargetFile);
  end;

  WriteLn('Test43_IsSymLink: Finished');
end;

procedure TFSTests.Test44_CompressToZip;
var
  ZipFile: string;
  TestContent: string;
begin
  WriteLn('Test44_CompressToZip: Starting');
  ZipFile := FTestDir + PathDelim + 'test.zip';
  TestContent := 'Test Content';
  
  // Clean up any existing files
  if FileExists(ZipFile) then
    DeleteFile(ZipFile);
  if FileExists(FTestFile) then
    DeleteFile(FTestFile);
    
  TFileKit.WriteTextFile(FTestFile, TestContent);
  TFileKit.CompressToZip(FTestDir, ZipFile);
  AssertTrue('ZIP file should be created', FileExists(ZipFile));
  WriteLn('Test44_CompressToZip: Finished');
end;

procedure TFSTests.Test45_DecompressFromZip;
var
  ZipFile: string;
  ExtractDir: string;
  TestContent: string;
  ExtractedFile: string;
begin
  WriteLn('Test45_DecompressFromZip: Starting');
  ZipFile := FTestDir + PathDelim + 'test.zip';
  ExtractDir := FTestDir + PathDelim + 'extracted';
  TestContent := 'Test Content';
  ExtractedFile := ExtractDir + PathDelim + 'test.txt';
  
  // Clean up any existing files
  if FileExists(ZipFile) then
    DeleteFile(ZipFile);
  if DirectoryExists(ExtractDir) then
    RemoveDir(ExtractDir);
  if FileExists(FTestFile) then
    DeleteFile(FTestFile);
    
  // Create test file and compress it
  ForceDirectories(ExtractDir);
  TFileKit.WriteTextFile(FTestFile, TestContent);
  TFileKit.CompressToZip(FTestDir, ZipFile);
  
  // Verify ZIP was created
  AssertTrue('ZIP file should be created', FileExists(ZipFile));
  
  // Extract and verify
  TFileKit.DecompressFromZip(ZipFile, ExtractDir);
  AssertTrue('Extracted file should exist', FileExists(ExtractedFile));
  AssertEquals('Extracted content should match original',
    TestContent, TFileKit.ReadTextFile(ExtractedFile));
  WriteLn('Test45_DecompressFromZip: Finished');
end;

procedure TFSTests.Test46_CompressToTar;
var
  TarFile: string;
  TestContent: string;
begin
  WriteLn('Test46_CompressToTar: Starting');
  TarFile := FTestDir + PathDelim + 'test.tar';
  TestContent := 'Test Content';
  
  // Clean up any existing files
  if FileExists(TarFile) then
    DeleteFile(TarFile);
  if FileExists(FTestFile) then
    DeleteFile(FTestFile);
    
  TFileKit.WriteTextFile(FTestFile, TestContent);
  TFileKit.CompressToTar(FTestDir, TarFile);
  AssertTrue('TAR file should be created', FileExists(TarFile));
  WriteLn('Test46_CompressToTar: Finished');
end;

procedure TFSTests.Test47_DecompressFromTar;
var
  TarFile: string;
  ExtractDir: string;
  TestContent: string;
  ExtractedFile: string;
begin
  WriteLn('Test47_DecompressFromTar: Starting');
  TarFile := FTestDir + PathDelim + 'test.tar';
  ExtractDir := FTestDir + PathDelim + 'extracted';
  TestContent := 'Test Content';
  ExtractedFile := ExtractDir + PathDelim + 'test.txt';
  
  // Clean up any existing files
  if FileExists(TarFile) then
    DeleteFile(TarFile);
  if DirectoryExists(ExtractDir) then
    RemoveDir(ExtractDir);
  if FileExists(FTestFile) then
    DeleteFile(FTestFile);
    
  // Create test file and compress it
  ForceDirectories(ExtractDir);
  TFileKit.WriteTextFile(FTestFile, TestContent);
  TFileKit.CompressToTar(FTestDir, TarFile);
  
  // Verify TAR was created
  AssertTrue('TAR file should be created', FileExists(TarFile));
  
  // Extract and verify
  TFileKit.DecompressFromTar(TarFile, ExtractDir);
  AssertTrue('Extracted file should exist', FileExists(ExtractedFile));
  AssertEquals('Extracted content should match original',
    TestContent, TFileKit.ReadTextFile(ExtractedFile));
  WriteLn('Test47_DecompressFromTar: Finished');
end;

procedure TFSTests.Test44b_CompressToZipRecursive;
var
  ZipFile: string;
  SubDir1, SubDir2, DeepDir: string;
  TestFile1, TestFile2, TestFile3, TestFile4: string;
begin
  WriteLn('Test44b_CompressToZipRecursive: Starting');
  ZipFile := FTestDir + PathDelim + 'test_recursive.zip';
  
  // Clean up any existing files
  if FileExists(ZipFile) then
    DeleteFile(ZipFile);
  if DirectoryExists(FTestDir) then
  begin
    TFileKit.DeleteDirectory(FTestDir, True);
    RemoveDir(FTestDir);
  end;
  ForceDirectories(FTestDir);
  
  // Create test directory structure
  SubDir1 := FTestDir + PathDelim + 'dir1';
  SubDir2 := FTestDir + PathDelim + 'dir2';
  DeepDir := SubDir1 + PathDelim + 'deep';
  ForceDirectories(SubDir1);
  ForceDirectories(SubDir2);
  ForceDirectories(DeepDir);
  
  // Create test files in various directories
  TestFile1 := FTestDir + PathDelim + 'root1.txt';
  TestFile2 := SubDir1 + PathDelim + 'file1.txt';
  TestFile3 := SubDir2 + PathDelim + 'file2.dat';
  TestFile4 := DeepDir + PathDelim + 'deep.txt';
  
  TFileKit.WriteTextFile(TestFile1, 'Root content 1');
  TFileKit.WriteTextFile(TestFile2, 'Dir1 content');
  TFileKit.WriteTextFile(TestFile3, 'Dir2 content');
  TFileKit.WriteTextFile(TestFile4, 'Deep content');
  
  // Compress the entire directory structure
  TFileKit.CompressToZip(FTestDir, ZipFile, True);
  AssertTrue('ZIP file should be created', FileExists(ZipFile));
  
  WriteLn('Test44b_CompressToZipRecursive: Finished');
end;

procedure TFSTests.Test45b_DecompressFromZipRecursive;
var
  ZipFile: string;
  ExtractDir: string;
  SubDir1, SubDir2, DeepDir: string;
  TestFile1, TestFile2, TestFile3, TestFile4: string;
begin
  WriteLn('Test45b_DecompressFromZipRecursive: Starting');
  ZipFile := FTestDir + PathDelim + 'test_recursive.zip';
  ExtractDir := FTestDir + PathDelim + 'extracted_recursive';
  
  // Clean up and prepare directories
  if DirectoryExists(ExtractDir) then
  begin
    TFileKit.DeleteDirectory(ExtractDir, True);
    RemoveDir(ExtractDir);
  end;
  ForceDirectories(ExtractDir);
  
  // Create test directory structure and files
  SubDir1 := FTestDir + PathDelim + 'dir1';
  SubDir2 := FTestDir + PathDelim + 'dir2';
  DeepDir := SubDir1 + PathDelim + 'deep';
  ForceDirectories(SubDir1);
  ForceDirectories(SubDir2);
  ForceDirectories(DeepDir);
  
  TestFile1 := FTestDir + PathDelim + 'root1.txt';
  TestFile2 := SubDir1 + PathDelim + 'file1.txt';
  TestFile3 := SubDir2 + PathDelim + 'file2.dat';
  TestFile4 := DeepDir + PathDelim + 'deep.txt';
  
  TFileKit.WriteTextFile(TestFile1, 'Root content 1');
  TFileKit.WriteTextFile(TestFile2, 'Dir1 content');
  TFileKit.WriteTextFile(TestFile3, 'Dir2 content');
  TFileKit.WriteTextFile(TestFile4, 'Deep content');
  
  // Create ZIP file in a different directory to avoid including it in the archive
  ZipFile := GetTempDir + PathDelim + 'test_recursive.zip';
  if FileExists(ZipFile) then
    DeleteFile(ZipFile);
    
  // Compress then extract
  TFileKit.CompressToZip(FTestDir, ZipFile, True);
  TFileKit.DecompressFromZip(ZipFile, ExtractDir);
  
  // Verify directory structure was preserved
  AssertTrue('Extracted dir1 should exist', 
    DirectoryExists(ExtractDir + PathDelim + 'dir1'));
  AssertTrue('Extracted dir2 should exist', 
    DirectoryExists(ExtractDir + PathDelim + 'dir2'));
  AssertTrue('Extracted deep directory should exist', 
    DirectoryExists(ExtractDir + PathDelim + 'dir1' + PathDelim + 'deep'));
    
  // Verify all files were extracted with correct content
  AssertTrue('Root file should exist',
    FileExists(ExtractDir + PathDelim + 'root1.txt'));
  AssertTrue('Dir1 file should exist',
    FileExists(ExtractDir + PathDelim + 'dir1' + PathDelim + 'file1.txt'));
  AssertTrue('Dir2 file should exist',
    FileExists(ExtractDir + PathDelim + 'dir2' + PathDelim + 'file2.dat'));
  AssertTrue('Deep file should exist',
    FileExists(ExtractDir + PathDelim + 'dir1' + PathDelim + 'deep' + PathDelim + 'deep.txt'));
    
  // Verify content integrity
  AssertEquals('Root file content should match',
    'Root content 1', TFileKit.ReadTextFile(ExtractDir + PathDelim + 'root1.txt'));
  AssertEquals('Dir1 file content should match',
    'Dir1 content', TFileKit.ReadTextFile(ExtractDir + PathDelim + 'dir1' + PathDelim + 'file1.txt'));
  AssertEquals('Dir2 file content should match',
    'Dir2 content', TFileKit.ReadTextFile(ExtractDir + PathDelim + 'dir2' + PathDelim + 'file2.dat'));
  AssertEquals('Deep file content should match',
    'Deep content', TFileKit.ReadTextFile(ExtractDir + PathDelim + 'dir1' + PathDelim + 'deep' + PathDelim + 'deep.txt'));
    
  // Clean up
  if FileExists(ZipFile) then
    DeleteFile(ZipFile);
    
  WriteLn('Test45b_DecompressFromZipRecursive: Finished');
end;

procedure TFSTests.Test46b_CompressToTarRecursive;
var
  TarFile: string;
  SubDir1, SubDir2, DeepDir: string;
  TestFile1, TestFile2, TestFile3, TestFile4: string;
begin
  WriteLn('Test46b_CompressToTarRecursive: Starting');
  TarFile := FTestDir + PathDelim + 'test_recursive.tar';
  
  // Clean up any existing files
  if FileExists(TarFile) then
    DeleteFile(TarFile);
  if DirectoryExists(FTestDir) then
  begin
    TFileKit.DeleteDirectory(FTestDir, True);
    RemoveDir(FTestDir);
  end;
  ForceDirectories(FTestDir);
  
  // Create test directory structure
  SubDir1 := FTestDir + PathDelim + 'dir1';
  SubDir2 := FTestDir + PathDelim + 'dir2';
  DeepDir := SubDir1 + PathDelim + 'deep';
  ForceDirectories(SubDir1);
  ForceDirectories(SubDir2);
  ForceDirectories(DeepDir);
  
  // Create test files in various directories
  TestFile1 := FTestDir + PathDelim + 'root1.txt';
  TestFile2 := SubDir1 + PathDelim + 'file1.txt';
  TestFile3 := SubDir2 + PathDelim + 'file2.dat';
  TestFile4 := DeepDir + PathDelim + 'deep.txt';
  
  TFileKit.WriteTextFile(TestFile1, 'Root content 1');
  TFileKit.WriteTextFile(TestFile2, 'Dir1 content');
  TFileKit.WriteTextFile(TestFile3, 'Dir2 content');
  TFileKit.WriteTextFile(TestFile4, 'Deep content');
  
  // Compress the entire directory structure
  TFileKit.CompressToTar(FTestDir, TarFile, True);
  AssertTrue('TAR file should be created', FileExists(TarFile));
  
  WriteLn('Test46b_CompressToTarRecursive: Finished');
end;

procedure TFSTests.Test47b_DecompressFromTarRecursive;
var
  TarFile: string;
  ExtractDir: string;
  SubDir1, SubDir2, DeepDir: string;
  TestFile1, TestFile2, TestFile3, TestFile4: string;
begin
  WriteLn('Test47b_DecompressFromTarRecursive: Starting');
  ExtractDir := FTestDir + PathDelim + 'extracted_recursive';
  
  // Clean up and prepare directories
  if DirectoryExists(ExtractDir) then
  begin
    TFileKit.DeleteDirectory(ExtractDir, True);
    RemoveDir(ExtractDir);
  end;
  ForceDirectories(ExtractDir);
  
  // Create test directory structure and files
  SubDir1 := FTestDir + PathDelim + 'dir1';
  SubDir2 := FTestDir + PathDelim + 'dir2';
  DeepDir := SubDir1 + PathDelim + 'deep';
  ForceDirectories(SubDir1);
  ForceDirectories(SubDir2);
  ForceDirectories(DeepDir);
  
  TestFile1 := FTestDir + PathDelim + 'root1.txt';
  TestFile2 := SubDir1 + PathDelim + 'file1.txt';
  TestFile3 := SubDir2 + PathDelim + 'file2.dat';
  TestFile4 := DeepDir + PathDelim + 'deep.txt';
  
  TFileKit.WriteTextFile(TestFile1, 'Root content 1');
  TFileKit.WriteTextFile(TestFile2, 'Dir1 content');
  TFileKit.WriteTextFile(TestFile3, 'Dir2 content');
  TFileKit.WriteTextFile(TestFile4, 'Deep content');
  
  // Create TAR file in a different directory to avoid including it in the archive
  TarFile := GetTempDir + PathDelim + 'test_recursive.tar';
  if FileExists(TarFile) then
    DeleteFile(TarFile);
    
  // Compress then extract
  TFileKit.CompressToTar(FTestDir, TarFile, True);
  TFileKit.DecompressFromTar(TarFile, ExtractDir);
  
  // Verify directory structure was preserved
  AssertTrue('Extracted dir1 should exist', 
    DirectoryExists(ExtractDir + PathDelim + 'dir1'));
  AssertTrue('Extracted dir2 should exist', 
    DirectoryExists(ExtractDir + PathDelim + 'dir2'));
  AssertTrue('Extracted deep directory should exist', 
    DirectoryExists(ExtractDir + PathDelim + 'dir1' + PathDelim + 'deep'));
    
  // Verify all files were extracted with correct content
  AssertTrue('Root file should exist',
    FileExists(ExtractDir + PathDelim + 'root1.txt'));
  AssertTrue('Dir1 file should exist',
    FileExists(ExtractDir + PathDelim + 'dir1' + PathDelim + 'file1.txt'));
  AssertTrue('Dir2 file should exist',
    FileExists(ExtractDir + PathDelim + 'dir2' + PathDelim + 'file2.dat'));
  AssertTrue('Deep file should exist',
    FileExists(ExtractDir + PathDelim + 'dir1' + PathDelim + 'deep' + PathDelim + 'deep.txt'));
    
  // Verify content integrity
  AssertEquals('Root file content should match',
    'Root content 1', TFileKit.ReadTextFile(ExtractDir + PathDelim + 'root1.txt'));
  AssertEquals('Dir1 file content should match',
    'Dir1 content', TFileKit.ReadTextFile(ExtractDir + PathDelim + 'dir1' + PathDelim + 'file1.txt'));
  AssertEquals('Dir2 file content should match',
    'Dir2 content', TFileKit.ReadTextFile(ExtractDir + PathDelim + 'dir2' + PathDelim + 'file2.dat'));
  AssertEquals('Deep file content should match',
    'Deep content', TFileKit.ReadTextFile(ExtractDir + PathDelim + 'dir1' + PathDelim + 'deep' + PathDelim + 'deep.txt'));
    
  // Clean up
  if FileExists(TarFile) then
    DeleteFile(TarFile);
    
  WriteLn('Test47b_DecompressFromTarRecursive: Finished');
end;

initialization
  RegisterTest(TFSTests);
end.
