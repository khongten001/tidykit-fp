unit TestCaseArchive;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, TidyKit.Core, TidyKit.FS, TidyKit.Archive;

type
  TArchiveTests = class(TTestCase)
  private
    FTempDir: string;
    FSourceDir: string;
    FSourceFile: string;
    FZipFile: string;
    FTarFile: string;
    FExtractDir: string;
    FNestedDir: string;  // For deep directory structure tests

    procedure CreateTestFiles;
    procedure CreateNestedDirectories;
    procedure CleanupTestFiles;
    function CompareDirectoryContents(const Dir1, Dir2: string): Boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // ZIP tests - Basic operations
    procedure Test01_CreateZipFromFile;
    procedure Test02_CreateZipFromDirectory;
    procedure Test03_ExtractZipFile;
    procedure Test04_ExtractZipDirectory;
    procedure Test05_CreateZipEmptyDirectory;
    procedure Test06_CreateZipNonExistentSource;
    procedure Test07_ExtractZipNonExistentFile;

    // ZIP tests - Recursive vs Non-recursive
    procedure Test08_CreateZipRecursive;
    procedure Test09_CreateZipNonRecursive;
    procedure Test10_ExtractZipRecursive;
    procedure Test11_ExtractZipNonRecursive;
    
    // ZIP tests - Compression levels
    procedure Test12_CreateZipNoCompression;
    procedure Test13_CreateZipFastCompression;
    procedure Test14_CreateZipMaxCompression;
    procedure Test15_CompareCompressionSizes;

    // TAR tests - Basic operations
    procedure Test21_CreateTarFromFile;
    procedure Test22_CreateTarFromDirectory;
    procedure Test23_ExtractTarFile;
    procedure Test24_ExtractTarDirectory;
    procedure Test25_CreateTarEmptyDirectory;
    procedure Test26_CreateTarNonExistentSource;
    procedure Test27_ExtractTarNonExistentFile;

    // TAR tests - Recursive vs Non-recursive
    procedure Test28_CreateTarRecursive;
    procedure Test29_CreateTarNonRecursive;
    procedure Test30_ExtractTarRecursive;
    procedure Test31_ExtractTarNonRecursive;
  end;

implementation

procedure TArchiveTests.CreateTestFiles;
var
  TestContent: string;
begin
  // Create source directory with test files
  TFileKit.CreateDirectory(FSourceDir);
  
  // Create a test file
  TestContent := 'This is a test file content.' + LineEnding +
                 'Line 2 of the test file.' + LineEnding +
                 'Line 3 with some numbers: 123456789' + LineEnding;
  TFileKit.WriteFile(FSourceFile, TestContent);
  
  // Create some subdirectories and files
  TFileKit.CreateDirectory(TFileKit.CombinePaths(FSourceDir, 'subdir1'));
  TFileKit.CreateDirectory(TFileKit.CombinePaths(FSourceDir, 'subdir2'));
  
  TFileKit.WriteFile(
    TFileKit.CombinePaths(FSourceDir, 'subdir1/file1.txt'),
    'Content of file1.txt'
  );
  
  TFileKit.WriteFile(
    TFileKit.CombinePaths(FSourceDir, 'subdir2/file2.txt'),
    'Content of file2.txt'
  );
end;

procedure TArchiveTests.CreateNestedDirectories;
var
  I: Integer;
  CurrentDir, DirName: string;
begin
  CurrentDir := FNestedDir;
  
  // Create a deep directory structure with files
  for I := 1 to 5 do
  begin
    DirName := Format('level%d', [I]);
    CurrentDir := TFileKit.CombinePaths(CurrentDir, DirName);
    TFileKit.CreateDirectory(CurrentDir);
    
    // Create a file in each directory
    TFileKit.WriteFile(
      TFileKit.CombinePaths(CurrentDir, Format('file%d.txt', [I])),
      Format('Content of file at level %d', [I])
    );
  end;
end;

function TArchiveTests.CompareDirectoryContents(const Dir1, Dir2: string): Boolean;
var
  Files1, Files2: TFilePathArray;
  Content1, Content2: string;
  RelPath: string;
  I: Integer;
begin
  Result := False;
  
  // Get all files recursively
  Files1 := TFileKit.ListFiles(Dir1, '*', True);
  Files2 := TFileKit.ListFiles(Dir2, '*', True);
  
  // Compare file count
  if Length(Files1) <> Length(Files2) then
    Exit;
    
  // Compare each file's content
  for I := 0 to High(Files1) do
  begin
    RelPath := ExtractRelativePath(Dir1, Files1[I]);
    Content1 := TFileKit.ReadFile(Files1[I]);
    Content2 := TFileKit.ReadFile(TFileKit.CombinePaths(Dir2, RelPath));
    
    if Content1 <> Content2 then
      Exit;
  end;
  
  Result := True;
end;

procedure TArchiveTests.CleanupTestFiles;
begin
  if TFileKit.DirectoryExists(FTempDir) then
    TFileKit.DeleteDirectory(FTempDir, True);
end;

procedure TArchiveTests.SetUp;
begin
  inherited;
  
  // Create temporary directory for tests
  FTempDir := TFileKit.CreateTempDirectory('TidyKitTest');
  FSourceDir := TFileKit.CombinePaths(FTempDir, 'source');
  FSourceFile := TFileKit.CombinePaths(FSourceDir, 'test.txt');
  FZipFile := TFileKit.CombinePaths(FTempDir, 'test.zip');
  FTarFile := TFileKit.CombinePaths(FTempDir, 'test.tar');
  FExtractDir := TFileKit.CombinePaths(FTempDir, 'extract');
  FNestedDir := TFileKit.CombinePaths(FTempDir, 'nested');
  
  CreateTestFiles;
  CreateNestedDirectories;
end;

procedure TArchiveTests.TearDown;
begin
  CleanupTestFiles;
end;

procedure TArchiveTests.Test01_CreateZipFromFile;
begin
  TArchiveKit.CreateZip(FSourceFile, FZipFile);
  AssertTrue('ZIP file should exist', TFileKit.Exists(FZipFile));
  AssertTrue('ZIP file size should be greater than 0', TFileKit.GetSize(FZipFile) > 0);
end;

procedure TArchiveTests.Test02_CreateZipFromDirectory;
begin
  TArchiveKit.CreateZip(FSourceDir, FZipFile);
  AssertTrue('ZIP file should exist', TFileKit.Exists(FZipFile));
  AssertTrue('ZIP file size should be greater than 0', TFileKit.GetSize(FZipFile) > 0);
end;

procedure TArchiveTests.Test03_ExtractZipFile;
var
  ExtractedFile: string;
  OriginalContent, ExtractedContent: string;
begin
  TArchiveKit.CreateZip(FSourceFile, FZipFile);
  TArchiveKit.ExtractZip(FZipFile, FExtractDir);
  
  ExtractedFile := TFileKit.CombinePaths(FExtractDir, TFileKit.GetFileName(FSourceFile));
  AssertTrue('Extracted file should exist', TFileKit.Exists(ExtractedFile));
  
  OriginalContent := TFileKit.ReadFile(FSourceFile);
  ExtractedContent := TFileKit.ReadFile(ExtractedFile);
  AssertEquals('Extracted content should match original', OriginalContent, ExtractedContent);
end;

procedure TArchiveTests.Test04_ExtractZipDirectory;
begin
  TArchiveKit.CreateZip(FSourceDir, FZipFile);
  TArchiveKit.ExtractZip(FZipFile, FExtractDir);
  
  AssertTrue('Extracted directory structure should exist',
    TFileKit.DirectoryExists(TFileKit.CombinePaths(FExtractDir, 'subdir1')) and
    TFileKit.DirectoryExists(TFileKit.CombinePaths(FExtractDir, 'subdir2')));
    
  AssertTrue('Extracted files should exist',
    TFileKit.Exists(TFileKit.CombinePaths(FExtractDir, 'subdir1/file1.txt')) and
    TFileKit.Exists(TFileKit.CombinePaths(FExtractDir, 'subdir2/file2.txt')));
end;

procedure TArchiveTests.Test05_CreateZipEmptyDirectory;
var
  EmptyDir: string;
begin
  EmptyDir := TFileKit.CombinePaths(FTempDir, 'empty');
  TFileKit.CreateDirectory(EmptyDir);
  
  TArchiveKit.CreateZip(EmptyDir, FZipFile);
  AssertTrue('ZIP file should exist', TFileKit.Exists(FZipFile));
end;

procedure TArchiveTests.Test06_CreateZipNonExistentSource;
begin
  TArchiveKit.CreateZip(FTempDir + 'nonexistent', FZipFile);
  AssertFalse('ZIP file should not exist', TFileKit.Exists(FZipFile));
end;

procedure TArchiveTests.Test07_ExtractZipNonExistentFile;
begin
  TArchiveKit.ExtractZip(FTempDir + 'nonexistent.zip', FExtractDir);
  AssertFalse('Extract directory should not exist', TFileKit.DirectoryExists(FExtractDir));
end;

procedure TArchiveTests.Test08_CreateZipRecursive;
var
  ExtractDir: string;
begin
  ExtractDir := TFileKit.CombinePaths(FTempDir, 'extract_recursive');
  
  // Create ZIP with recursive option
  TArchiveKit.CreateZip(FNestedDir, FZipFile, True);
  
  // Extract and verify
  TArchiveKit.ExtractZip(FZipFile, ExtractDir, True);
  
  // Verify all levels are present
  AssertTrue('All nested directories should be present',
    TFileKit.DirectoryExists(TFileKit.CombinePaths(ExtractDir, 'level1')) and
    TFileKit.DirectoryExists(TFileKit.CombinePaths(ExtractDir, 'level1/level2')) and
    TFileKit.DirectoryExists(TFileKit.CombinePaths(ExtractDir, 'level1/level2/level3')));
    
  // Verify directory contents match
  AssertTrue('Directory contents should match', CompareDirectoryContents(FNestedDir, ExtractDir));
end;

procedure TArchiveTests.Test09_CreateZipNonRecursive;
var
  ExtractDir: string;
begin
  ExtractDir := TFileKit.CombinePaths(FTempDir, 'extract_nonrecursive');
  
  // Create ZIP without recursive option
  TArchiveKit.CreateZip(FNestedDir, FZipFile, False);
  
  // Extract and verify
  TArchiveKit.ExtractZip(FZipFile, ExtractDir, True);
  
  // Only top-level files should be present
  AssertTrue('Top level directory should exist',
    TFileKit.DirectoryExists(TFileKit.CombinePaths(ExtractDir, 'level1')));
    
  AssertFalse('Deeper directories should not exist',
    TFileKit.DirectoryExists(TFileKit.CombinePaths(ExtractDir, 'level1/level2')));
end;

procedure TArchiveTests.Test10_ExtractZipRecursive;
var
  ExtractDir: string;
begin
  ExtractDir := TFileKit.CombinePaths(FTempDir, 'extract_recursive');
  
  // Create ZIP with recursive option
  TArchiveKit.CreateZip(FNestedDir, FZipFile, True);
  
  // Extract and verify
  TArchiveKit.ExtractZip(FZipFile, ExtractDir, True);
  
  // Verify all levels are present
  AssertTrue('All nested directories should be present',
    TFileKit.DirectoryExists(TFileKit.CombinePaths(ExtractDir, 'level1')) and
    TFileKit.DirectoryExists(TFileKit.CombinePaths(ExtractDir, 'level1/level2')) and
    TFileKit.DirectoryExists(TFileKit.CombinePaths(ExtractDir, 'level1/level2/level3')));
    
  // Verify directory contents match
  AssertTrue('Directory contents should match', CompareDirectoryContents(FNestedDir, ExtractDir));
end;

procedure TArchiveTests.Test11_ExtractZipNonRecursive;
var
  ExtractDir: string;
begin
  ExtractDir := TFileKit.CombinePaths(FTempDir, 'extract_nonrecursive');
  
  // Create ZIP without recursive option
  TArchiveKit.CreateZip(FNestedDir, FZipFile, False);
  
  // Extract and verify
  TArchiveKit.ExtractZip(FZipFile, ExtractDir, True);
  
  // Only top-level files should be present
  AssertTrue('Top level directory should exist',
    TFileKit.DirectoryExists(TFileKit.CombinePaths(ExtractDir, 'level1')));
    
  AssertFalse('Deeper directories should not exist',
    TFileKit.DirectoryExists(TFileKit.CombinePaths(ExtractDir, 'level1/level2')));
end;

procedure TArchiveTests.Test12_CreateZipNoCompression;
begin
  TArchiveKit.CreateZip(FSourceDir, FZipFile, True, clNone);
  AssertTrue('ZIP file should exist', TFileKit.Exists(FZipFile));
end;

procedure TArchiveTests.Test13_CreateZipFastCompression;
begin
  TArchiveKit.CreateZip(FSourceDir, FZipFile, True, clFastest);
  AssertTrue('ZIP file should exist', TFileKit.Exists(FZipFile));
end;

procedure TArchiveTests.Test14_CreateZipMaxCompression;
begin
  TArchiveKit.CreateZip(FSourceDir, FZipFile, True, clMaximum);
  AssertTrue('ZIP file should exist', TFileKit.Exists(FZipFile));
end;

procedure TArchiveTests.Test15_CompareCompressionSizes;
var
  NoCompressionSize, FastCompressionSize, MaxCompressionSize: Int64;
  ZipFileNone, ZipFileFast, ZipFileMax: string;
begin
  // Create ZIP files with different compression levels
  ZipFileNone := TFileKit.CombinePaths(FTempDir, 'none.zip');
  ZipFileFast := TFileKit.CombinePaths(FTempDir, 'fast.zip');
  ZipFileMax := TFileKit.CombinePaths(FTempDir, 'max.zip');
  
  TArchiveKit.CreateZip(FSourceDir, ZipFileNone, True, clNone);
  TArchiveKit.CreateZip(FSourceDir, ZipFileFast, True, clFastest);
  TArchiveKit.CreateZip(FSourceDir, ZipFileMax, True, clMaximum);
  
  NoCompressionSize := TFileKit.GetSize(ZipFileNone);
  FastCompressionSize := TFileKit.GetSize(ZipFileFast);
  MaxCompressionSize := TFileKit.GetSize(ZipFileMax);
  
  // Verify compression levels have expected effect
  AssertTrue('No compression should be larger than fast compression',
    NoCompressionSize > FastCompressionSize);
  AssertTrue('Fast compression should be larger than max compression',
    FastCompressionSize > MaxCompressionSize);
end;

procedure TArchiveTests.Test21_CreateTarFromFile;
begin
  TArchiveKit.CreateTar(FSourceFile, FTarFile);
  AssertTrue('TAR file should exist', TFileKit.Exists(FTarFile));
  AssertTrue('TAR file size should be greater than 0', TFileKit.GetSize(FTarFile) > 0);
end;

procedure TArchiveTests.Test22_CreateTarFromDirectory;
begin
  TArchiveKit.CreateTar(FSourceDir, FTarFile);
  AssertTrue('TAR file should exist', TFileKit.Exists(FTarFile));
  AssertTrue('TAR file size should be greater than 0', TFileKit.GetSize(FTarFile) > 0);
end;

procedure TArchiveTests.Test23_ExtractTarFile;
var
  ExtractedFile: string;
  OriginalContent, ExtractedContent: string;
begin
  TArchiveKit.CreateTar(FSourceFile, FTarFile);
  TArchiveKit.ExtractTar(FTarFile, FExtractDir);
  
  ExtractedFile := TFileKit.CombinePaths(FExtractDir, TFileKit.GetFileName(FSourceFile));
  AssertTrue('Extracted file should exist', TFileKit.Exists(ExtractedFile));
  
  OriginalContent := TFileKit.ReadFile(FSourceFile);
  ExtractedContent := TFileKit.ReadFile(ExtractedFile);
  AssertEquals('Extracted content should match original', OriginalContent, ExtractedContent);
end;

procedure TArchiveTests.Test24_ExtractTarDirectory;
begin
  TArchiveKit.CreateTar(FSourceDir, FTarFile);
  TArchiveKit.ExtractTar(FTarFile, FExtractDir);
  
  AssertTrue('Extracted directory structure should exist',
    TFileKit.DirectoryExists(TFileKit.CombinePaths(FExtractDir, 'subdir1')) and
    TFileKit.DirectoryExists(TFileKit.CombinePaths(FExtractDir, 'subdir2')));
    
  AssertTrue('Extracted files should exist',
    TFileKit.Exists(TFileKit.CombinePaths(FExtractDir, 'subdir1/file1.txt')) and
    TFileKit.Exists(TFileKit.CombinePaths(FExtractDir, 'subdir2/file2.txt')));
end;

procedure TArchiveTests.Test25_CreateTarEmptyDirectory;
var
  EmptyDir: string;
begin
  EmptyDir := TFileKit.CombinePaths(FTempDir, 'empty');
  TFileKit.CreateDirectory(EmptyDir);
  
  TArchiveKit.CreateTar(EmptyDir, FTarFile);
  AssertTrue('TAR file should exist', TFileKit.Exists(FTarFile));
end;

procedure TArchiveTests.Test26_CreateTarNonExistentSource;
begin
  TArchiveKit.CreateTar(FTempDir + 'nonexistent', FTarFile);
  AssertFalse('TAR file should not exist', TFileKit.Exists(FTarFile));
end;

procedure TArchiveTests.Test27_ExtractTarNonExistentFile;
begin
  TArchiveKit.ExtractTar(FTempDir + 'nonexistent.tar', FExtractDir);
  AssertFalse('Extract directory should not exist', TFileKit.DirectoryExists(FExtractDir));
end;

procedure TArchiveTests.Test28_CreateTarRecursive;
var
  ExtractDir: string;
begin
  ExtractDir := TFileKit.CombinePaths(FTempDir, 'extract_recursive');
  
  // Create TAR with recursive option
  TArchiveKit.CreateTar(FNestedDir, FTarFile, True);
  
  // Extract and verify
  TArchiveKit.ExtractTar(FTarFile, ExtractDir, True);
  
  // Verify all levels are present
  AssertTrue('All nested directories should be present',
    TFileKit.DirectoryExists(TFileKit.CombinePaths(ExtractDir, 'level1')) and
    TFileKit.DirectoryExists(TFileKit.CombinePaths(ExtractDir, 'level1/level2')) and
    TFileKit.DirectoryExists(TFileKit.CombinePaths(ExtractDir, 'level1/level2/level3')));
    
  // Verify directory contents match
  AssertTrue('Directory contents should match', CompareDirectoryContents(FNestedDir, ExtractDir));
end;

procedure TArchiveTests.Test29_CreateTarNonRecursive;
var
  ExtractDir: string;
begin
  ExtractDir := TFileKit.CombinePaths(FTempDir, 'extract_nonrecursive');
  
  // Create TAR without recursive option
  TArchiveKit.CreateTar(FNestedDir, FTarFile, False);
  
  // Extract and verify
  TArchiveKit.ExtractTar(FTarFile, ExtractDir, True);
  
  // Only top-level files should be present
  AssertTrue('Top level directory should exist',
    TFileKit.DirectoryExists(TFileKit.CombinePaths(ExtractDir, 'level1')));
    
  AssertFalse('Deeper directories should not exist',
    TFileKit.DirectoryExists(TFileKit.CombinePaths(ExtractDir, 'level1/level2')));
end;

procedure TArchiveTests.Test30_ExtractTarRecursive;
var
  ExtractDir: string;
begin
  ExtractDir := TFileKit.CombinePaths(FTempDir, 'extract_recursive');
  
  // Create TAR with recursive option
  TArchiveKit.CreateTar(FNestedDir, FTarFile, True);
  
  // Extract and verify
  TArchiveKit.ExtractTar(FTarFile, ExtractDir, True);
  
  // Verify all levels are present
  AssertTrue('All nested directories should be present',
    TFileKit.DirectoryExists(TFileKit.CombinePaths(ExtractDir, 'level1')) and
    TFileKit.DirectoryExists(TFileKit.CombinePaths(ExtractDir, 'level1/level2')) and
    TFileKit.DirectoryExists(TFileKit.CombinePaths(ExtractDir, 'level1/level2/level3')));
    
  // Verify directory contents match
  AssertTrue('Directory contents should match', CompareDirectoryContents(FNestedDir, ExtractDir));
end;

procedure TArchiveTests.Test31_ExtractTarNonRecursive;
var
  ExtractDir: string;
begin
  ExtractDir := TFileKit.CombinePaths(FTempDir, 'extract_nonrecursive');
  
  // Create TAR without recursive option
  TArchiveKit.CreateTar(FNestedDir, FTarFile, False);
  
  // Extract and verify
  TArchiveKit.ExtractTar(FTarFile, ExtractDir, True);
  
  // Only top-level files should be present
  AssertTrue('Top level directory should exist',
    TFileKit.DirectoryExists(TFileKit.CombinePaths(ExtractDir, 'level1')));
    
  AssertFalse('Deeper directories should not exist',
    TFileKit.DirectoryExists(TFileKit.CombinePaths(ExtractDir, 'level1/level2')));
end;

initialization
  RegisterTest(TArchiveTests);
end. 