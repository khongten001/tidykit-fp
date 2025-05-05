unit TidyKit.Archive.Test;

{$mode objfpc}{$H+}{$J-}
{$ModeSwitch advancedrecords}

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
  TidyKit.FS, TidyKit.Archive;

type
  { TArchiveTests }
  TArchiveTests = class(TTestCase)
  private
    FTestDir: string;
    FTestFile: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // ZIP compression tests
    procedure Test01_CompressToZip;
    procedure Test02_DecompressFromZip;
    procedure Test03_CompressToZipRecursive;
    procedure Test04_DecompressFromZipRecursive;
    // TAR compression tests
    procedure Test05_CompressToTar;
    procedure Test06_DecompressFromTar;
    procedure Test07_CompressToTarRecursive;
    procedure Test08_DecompressFromTarRecursive;
  end;

implementation

{ TArchiveTests }

procedure TArchiveTests.SetUp;
var
  TestBasePath: string;
begin
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
  if DirectoryExists(FTestDir) then
  begin
    try
      TFileKit.DeleteDirectory(FTestDir, True);
      Sleep(100); // Give OS time to release handles
      RemoveDir(FTestDir);
    except
      on E: Exception do
        WriteLn('Warning: Could not delete existing test directory: ', E.Message);
    end;
  end;
  
  try
    if not ForceDirectories(FTestDir) then
      raise EFSError.CreateFmt('Could not create test directory: %s', [FTestDir]);
  except
    on E: Exception do
      raise EFSError.CreateFmt('Failed to setup test environment: %s', [E.Message]);
  end;
end;

procedure TArchiveTests.TearDown;
begin
  // Clean up test environment
  if DirectoryExists(FTestDir) then
  try
    // First try to delete any remaining files
    TFileKit.DeleteDirectory(FTestDir, True);
    Sleep(100); // Give OS time to release handles
    RemoveDir(FTestDir);
  except
    on E: Exception do
      WriteLn('Warning: Could not clean up test directory: ', E.Message);
  end;
end;

procedure TArchiveTests.Test01_CompressToZip;
var
  ZipFile: string;
  TestContent: string;
begin
  WriteLn('Test01_CompressToZip: Starting');
  ZipFile := FTestDir + PathDelim + 'test.zip';
  TestContent := 'Test Content';
  
  // Clean up any existing files
  if FileExists(ZipFile) then
    TFileKit.DeleteFile(ZipFile);
  if FileExists(FTestFile) then
    TFileKit.DeleteFile(FTestFile);
    
  TFileKit.WriteTextFile(FTestFile, TestContent);
  TArchiveKit.CompressToZip(FTestDir, ZipFile);
  AssertTrue('ZIP file should be created', FileExists(ZipFile));
  WriteLn('Test01_CompressToZip: Finished');
end;

procedure TArchiveTests.Test02_DecompressFromZip;
var
  ZipFile: string;
  ExtractDir: string;
  TestContent: string;
  ExtractedFile: string;
begin
  WriteLn('Test02_DecompressFromZip: Starting');
  ZipFile := FTestDir + PathDelim + 'test.zip';
  ExtractDir := FTestDir + PathDelim + 'extracted';
  TestContent := 'Test Content';
  ExtractedFile := ExtractDir + PathDelim + 'test.txt';
  
  // Clean up any existing files
  if FileExists(ZipFile) then
    TFileKit.DeleteFile(ZipFile);
  if DirectoryExists(ExtractDir) then
    RemoveDir(ExtractDir);
  if FileExists(FTestFile) then
    TFileKit.DeleteFile(FTestFile);
    
  // Create test file and compress it
  ForceDirectories(ExtractDir);
  TFileKit.WriteTextFile(FTestFile, TestContent);
  TArchiveKit.CompressToZip(FTestDir, ZipFile);
  
  // Verify ZIP was created
  AssertTrue('ZIP file should be created', FileExists(ZipFile));
  
  // Extract and verify
  TArchiveKit.DecompressFromZip(ZipFile, ExtractDir);
  AssertTrue('Extracted file should exist', FileExists(ExtractedFile));
  AssertEquals('Extracted content should match original',
    TestContent, TFileKit.ReadTextFile(ExtractedFile));
  WriteLn('Test02_DecompressFromZip: Finished');
end;

procedure TArchiveTests.Test03_CompressToZipRecursive;
var
  ZipFile: string;
  SubDir1, SubDir2, DeepDir: string;
  TestFile1, TestFile2, TestFile3, TestFile4: string;
begin
  WriteLn('Test03_CompressToZipRecursive: Starting');
  ZipFile := FTestDir + PathDelim + 'test_recursive.zip';
  
  // Clean up any existing files
  if FileExists(ZipFile) then
    TFileKit.DeleteFile(ZipFile);
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
  TArchiveKit.CompressToZip(FTestDir, ZipFile, True);
  AssertTrue('ZIP file should be created', FileExists(ZipFile));
  
  WriteLn('Test03_CompressToZipRecursive: Finished');
end;

procedure TArchiveTests.Test04_DecompressFromZipRecursive;
var
  ZipFile: string;
  ExtractDir: string;
  SubDir1, SubDir2, DeepDir: string;
  TestFile1, TestFile2, TestFile3, TestFile4: string;
begin
  WriteLn('Test04_DecompressFromZipRecursive: Starting');
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
    TFileKit.DeleteFile(ZipFile);
    
  // Compress then extract
  TArchiveKit.CompressToZip(FTestDir, ZipFile, True);
  TArchiveKit.DecompressFromZip(ZipFile, ExtractDir);
  
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
    TFileKit.DeleteFile(ZipFile);
    
  WriteLn('Test04_DecompressFromZipRecursive: Finished');
end;

procedure TArchiveTests.Test05_CompressToTar;
var
  TarFile: string;
  TestContent: string;
begin
  WriteLn('Test05_CompressToTar: Starting');
  TarFile := FTestDir + PathDelim + 'test.tar';
  TestContent := 'Test Content';
  
  // Clean up any existing files
  if FileExists(TarFile) then
    TFileKit.DeleteFile(TarFile);
  if FileExists(FTestFile) then
    TFileKit.DeleteFile(FTestFile);
    
  TFileKit.WriteTextFile(FTestFile, TestContent);
  TArchiveKit.CompressToTar(FTestDir, TarFile);
  AssertTrue('TAR file should be created', FileExists(TarFile));
  WriteLn('Test05_CompressToTar: Finished');
end;

procedure TArchiveTests.Test06_DecompressFromTar;
var
  TarFile: string;
  ExtractDir: string;
  TestContent: string;
  ExtractedFile: string;
begin
  WriteLn('Test06_DecompressFromTar: Starting');
  TarFile := FTestDir + PathDelim + 'test.tar';
  ExtractDir := FTestDir + PathDelim + 'extracted';
  TestContent := 'Test Content';
  ExtractedFile := ExtractDir + PathDelim + 'test.txt';
  
  // Clean up any existing files
  if FileExists(TarFile) then
    TFileKit.DeleteFile(TarFile);
  if DirectoryExists(ExtractDir) then
    RemoveDir(ExtractDir);
  if FileExists(FTestFile) then
    TFileKit.DeleteFile(FTestFile);
    
  // Create test file and compress it
  ForceDirectories(ExtractDir);
  TFileKit.WriteTextFile(FTestFile, TestContent);
  TArchiveKit.CompressToTar(FTestDir, TarFile);
  
  // Verify TAR was created
  AssertTrue('TAR file should be created', FileExists(TarFile));
  
  // Extract and verify
  TArchiveKit.DecompressFromTar(TarFile, ExtractDir);
  AssertTrue('Extracted file should exist', FileExists(ExtractedFile));
  AssertEquals('Extracted content should match original',
    TestContent, TFileKit.ReadTextFile(ExtractedFile));
  WriteLn('Test06_DecompressFromTar: Finished');
end;

procedure TArchiveTests.Test07_CompressToTarRecursive;
var
  TarFile: string;
  SubDir1, SubDir2, DeepDir: string;
  TestFile1, TestFile2, TestFile3, TestFile4: string;
begin
  WriteLn('Test07_CompressToTarRecursive: Starting');
  TarFile := FTestDir + PathDelim + 'test_recursive.tar';
  
  // Clean up any existing files
  if FileExists(TarFile) then
    TFileKit.DeleteFile(TarFile);
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
  TArchiveKit.CompressToTar(FTestDir, TarFile, True);
  AssertTrue('TAR file should be created', FileExists(TarFile));
  
  WriteLn('Test07_CompressToTarRecursive: Finished');
end;

procedure TArchiveTests.Test08_DecompressFromTarRecursive;
var
  TarFile: string;
  ExtractDir: string;
  SubDir1, SubDir2, DeepDir: string;
  TestFile1, TestFile2, TestFile3, TestFile4: string;
begin
  WriteLn('Test08_DecompressFromTarRecursive: Starting');
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
    TFileKit.DeleteFile(TarFile);
    
  // Compress then extract
  TArchiveKit.CompressToTar(FTestDir, TarFile, True);
  TArchiveKit.DecompressFromTar(TarFile, ExtractDir);
  
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
    TFileKit.DeleteFile(TarFile);
    
  WriteLn('Test08_DecompressFromTarRecursive: Finished');
end;

initialization
  RegisterTest(TArchiveTests);
end. 
