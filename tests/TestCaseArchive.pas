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

procedure TArchiveTests.SetUp;
begin
  inherited;
  
  // Create base temporary directory
  FTempDir := TFileKit.CreateTempDirectory('TidyKitTest');
  
  // Initialize paths
  FSourceDir := TFileKit.CombinePaths(FTempDir, 'source');
  FSourceFile := TFileKit.CombinePaths(FSourceDir, 'test.txt');
  FZipFile := TFileKit.CombinePaths(FTempDir, 'test.zip');
  FTarFile := TFileKit.CombinePaths(FTempDir, 'test.tar');
  FExtractDir := TFileKit.CombinePaths(FTempDir, 'extract');
  FNestedDir := TFileKit.CombinePaths(FTempDir, 'nested');
  
  // Create directories using ForceDirectories
  ForceDirectories(FSourceDir);
  ForceDirectories(FExtractDir);
  ForceDirectories(FNestedDir);
  
  // Create test files and nested directories
  CreateTestFiles;
  CreateNestedDirectories;
end;

procedure TArchiveTests.CreateTestFiles;
var
  TestContent: string;
  SubDir1, SubDir2: string;
begin
  // Create source directory if it doesn't exist
  ForceDirectories(FSourceDir);
  
  // Create a test file
  TestContent := 'This is a test file content.' + LineEnding +
                 'Line 2 of the test file.' + LineEnding +
                 'Line 3 with some numbers: 123456789' + LineEnding;
  TFileKit.WriteFile(FSourceFile, TestContent);
  
  // Create some subdirectories and files
  SubDir1 := TFileKit.CombinePaths(FSourceDir, 'subdir1');
  SubDir2 := TFileKit.CombinePaths(FSourceDir, 'subdir2');
  
  // Create subdirectories
  ForceDirectories(SubDir1);
  ForceDirectories(SubDir2);
  
  // Write test files in subdirectories
  TFileKit.WriteFile(
    TFileKit.CombinePaths(SubDir1, 'file1.txt'),
    'Content of file1.txt'
  );
  
  TFileKit.WriteFile(
    TFileKit.CombinePaths(SubDir2, 'file2.txt'),
    'Content of file2.txt'
  );
end;

procedure TArchiveTests.CreateNestedDirectories;
var
  I: Integer;
  CurrentDir, DirName, FilePath: string;
begin
  // Ensure nested base directory exists
  ForceDirectories(FNestedDir);
  CurrentDir := FNestedDir;
  
  // Create a deep directory structure with files
  for I := 1 to 5 do
  begin
    DirName := Format('level%d', [I]);
    CurrentDir := TFileKit.CombinePaths(CurrentDir, DirName);
    ForceDirectories(CurrentDir);
    
    // Create a file in each directory
    FilePath := TFileKit.CombinePaths(CurrentDir, Format('file%d.txt', [I]));
    TFileKit.WriteFile(
      FilePath,
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
  // Create source directory with test files
  ForceDirectories(FSourceDir);
  CreateTestFiles;
  
  // Create ZIP from directory
  TArchiveKit.CreateZip(FSourceDir, FZipFile);
  AssertTrue('ZIP file should exist', TFileKit.Exists(FZipFile));
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
  // Create source directory with test files
  ForceDirectories(FSourceDir);
  CreateTestFiles;
  
  // Create and extract ZIP
  TArchiveKit.CreateZip(FSourceDir, FZipFile);
  TArchiveKit.ExtractZip(FZipFile, FExtractDir);
  
  // Compare contents
  AssertTrue('Extracted directory structure should exist', 
    CompareDirectoryContents(FSourceDir, FExtractDir));
end;

procedure TArchiveTests.Test05_CreateZipEmptyDirectory;
var
  EmptyDir: string;
begin
  EmptyDir := TFileKit.CombinePaths(FTempDir, 'empty');
  ForceDirectories(EmptyDir);
  
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
begin
  // Create nested directory structure
  ForceDirectories(FNestedDir);
  CreateNestedDirectories;
  
  // Create ZIP with recursive option
  TArchiveKit.CreateZip(FNestedDir, FZipFile, True);
  AssertTrue('ZIP file should exist', TFileKit.Exists(FZipFile));
end;

procedure TArchiveTests.Test09_CreateZipNonRecursive;
begin
  // Create nested directory structure
  ForceDirectories(FNestedDir);
  CreateNestedDirectories;
  
  // Create ZIP without recursive option
  TArchiveKit.CreateZip(FNestedDir, FZipFile, False);
  AssertTrue('ZIP file should exist', TFileKit.Exists(FZipFile));
end;

procedure TArchiveTests.Test10_ExtractZipRecursive;
begin
  // Create nested directory structure
  ForceDirectories(FNestedDir);
  CreateNestedDirectories;
  
  // Create and extract ZIP
  TArchiveKit.CreateZip(FNestedDir, FZipFile, True);
  TArchiveKit.ExtractZip(FZipFile, FExtractDir, True);
  
  // Compare contents
  AssertTrue('Extracted directory structure should exist',
    CompareDirectoryContents(FNestedDir, FExtractDir));
end;

procedure TArchiveTests.Test11_ExtractZipNonRecursive;
begin
  // Create nested directory structure
  ForceDirectories(FNestedDir);
  CreateNestedDirectories;
  
  // Create and extract ZIP
  TArchiveKit.CreateZip(FNestedDir, FZipFile, True);
  TArchiveKit.ExtractZip(FZipFile, FExtractDir, False);
  
  // Only top-level files should exist
  AssertTrue('Top-level directory should exist',
    TFileKit.DirectoryExists(FExtractDir));
  AssertFalse('Subdirectories should not exist',
    TFileKit.DirectoryExists(TFileKit.CombinePaths(FExtractDir, 'level1')));
end;

procedure TArchiveTests.Test12_CreateZipNoCompression;
begin
  // Create source directory with test files
  ForceDirectories(FSourceDir);
  CreateTestFiles;
  
  // Create ZIP with no compression
  TArchiveKit.CreateZip(FSourceDir, FZipFile, True, alNone);
  AssertTrue('ZIP file should exist', TFileKit.Exists(FZipFile));
end;

procedure TArchiveTests.Test13_CreateZipFastCompression;
begin
  // Create source directory with test files
  ForceDirectories(FSourceDir);
  CreateTestFiles;
  
  // Create ZIP with fast compression
  TArchiveKit.CreateZip(FSourceDir, FZipFile, True, alFastest);
  AssertTrue('ZIP file should exist', TFileKit.Exists(FZipFile));
end;

procedure TArchiveTests.Test14_CreateZipMaxCompression;
begin
  // Create source directory with test files
  ForceDirectories(FSourceDir);
  CreateTestFiles;
  
  // Create ZIP with maximum compression
  TArchiveKit.CreateZip(FSourceDir, FZipFile, True, alMaximum);
  AssertTrue('ZIP file should exist', TFileKit.Exists(FZipFile));
end;

procedure TArchiveTests.Test15_CompareCompressionSizes;
var
  NoCompSize, FastCompSize, MaxCompSize: Int64;
begin
  // Create source directory with test files
  ForceDirectories(FSourceDir);
  CreateTestFiles;
  
  // Create ZIPs with different compression levels
  TArchiveKit.CreateZip(FSourceDir, FZipFile, True, alNone);
  NoCompSize := TFileKit.GetSize(FZipFile);
  
  TArchiveKit.CreateZip(FSourceDir, FZipFile, True, alFastest);
  FastCompSize := TFileKit.GetSize(FZipFile);
  
  TArchiveKit.CreateZip(FSourceDir, FZipFile, True, alMaximum);
  MaxCompSize := TFileKit.GetSize(FZipFile);
  
  // Verify compression sizes
  AssertTrue('No compression should be largest', NoCompSize >= FastCompSize);
  AssertTrue('Maximum compression should be smallest', FastCompSize >= MaxCompSize);
end;

procedure TArchiveTests.Test21_CreateTarFromFile;
begin
  // Create source directory with test files
  ForceDirectories(FSourceDir);
  CreateTestFiles;
  
  // Create TAR from file
  TArchiveKit.CreateTar(FSourceFile, FTarFile);
  AssertTrue('TAR file should exist', TFileKit.Exists(FTarFile));
end;

procedure TArchiveTests.Test22_CreateTarFromDirectory;
begin
  // Create source directory with test files
  ForceDirectories(FSourceDir);
  CreateTestFiles;
  
  // Create TAR from directory
  TArchiveKit.CreateTar(FSourceDir, FTarFile);
  AssertTrue('TAR file should exist', TFileKit.Exists(FTarFile));
end;

procedure TArchiveTests.Test23_ExtractTarFile;
begin
  // Create source directory with test files
  ForceDirectories(FSourceDir);
  CreateTestFiles;
  
  // Create and extract TAR
  TArchiveKit.CreateTar(FSourceFile, FTarFile);
  TArchiveKit.ExtractTar(FTarFile, FExtractDir);
  
  // Verify extracted file
  AssertTrue('Extracted file should exist',
    TFileKit.Exists(TFileKit.CombinePaths(FExtractDir, ExtractFileName(FSourceFile))));
end;

procedure TArchiveTests.Test24_ExtractTarDirectory;
begin
  // Create source directory with test files
  ForceDirectories(FSourceDir);
  CreateTestFiles;
  
  // Create and extract TAR
  TArchiveKit.CreateTar(FSourceDir, FTarFile);
  TArchiveKit.ExtractTar(FTarFile, FExtractDir);
  
  // Compare contents
  AssertTrue('Extracted directory structure should exist',
    CompareDirectoryContents(FSourceDir, FExtractDir));
end;

procedure TArchiveTests.Test25_CreateTarEmptyDirectory;
var
  EmptyDir: string;
begin
  EmptyDir := TFileKit.CombinePaths(FTempDir, 'empty');
  ForceDirectories(EmptyDir);
  
  TArchiveKit.CreateTar(EmptyDir, FTarFile);
  AssertTrue('TAR file should exist', TFileKit.Exists(FTarFile));
end;

procedure TArchiveTests.Test26_CreateTarNonExistentSource;
var
  NonExistentPath: string;
begin
  NonExistentPath := TFileKit.CombinePaths(FTempDir, 'nonexistent');
  TArchiveKit.CreateTar(NonExistentPath, FTarFile);
  AssertFalse('TAR file should not exist', TFileKit.Exists(FTarFile));
end;

procedure TArchiveTests.Test27_ExtractTarNonExistentFile;
var
  NonExistentTar: string;
begin
  NonExistentTar := TFileKit.CombinePaths(FTempDir, 'nonexistent.tar');
  TArchiveKit.ExtractTar(NonExistentTar, FExtractDir);
  AssertFalse('Extract directory should not exist', TFileKit.DirectoryExists(FExtractDir));
end;

procedure TArchiveTests.Test28_CreateTarRecursive;
begin
  // Create nested directory structure
  ForceDirectories(FNestedDir);
  CreateNestedDirectories;
  
  // Create TAR with recursive option
  TArchiveKit.CreateTar(FNestedDir, FTarFile, True);
  AssertTrue('TAR file should exist', TFileKit.Exists(FTarFile));
end;

procedure TArchiveTests.Test29_CreateTarNonRecursive;
begin
  // Create nested directory structure
  ForceDirectories(FNestedDir);
  CreateNestedDirectories;
  
  // Create TAR without recursive option
  TArchiveKit.CreateTar(FNestedDir, FTarFile, False);
  AssertTrue('TAR file should exist', TFileKit.Exists(FTarFile));
end;

procedure TArchiveTests.Test30_ExtractTarRecursive;
begin
  // Create nested directory structure
  ForceDirectories(FNestedDir);
  CreateNestedDirectories;
  
  // Create and extract TAR
  TArchiveKit.CreateTar(FNestedDir, FTarFile, True);
  TArchiveKit.ExtractTar(FTarFile, FExtractDir, True);
  
  // Compare contents
  AssertTrue('Extracted directory structure should exist',
    CompareDirectoryContents(FNestedDir, FExtractDir));
end;

procedure TArchiveTests.Test31_ExtractTarNonRecursive;
begin
  // Create nested directory structure
  ForceDirectories(FNestedDir);
  CreateNestedDirectories;
  
  // Create and extract TAR
  TArchiveKit.CreateTar(FNestedDir, FTarFile, True);
  TArchiveKit.ExtractTar(FTarFile, FExtractDir, False);
  
  // Only top-level files should exist
  AssertTrue('Top-level directory should exist',
    TFileKit.DirectoryExists(FExtractDir));
  AssertFalse('Subdirectories should not exist',
    TFileKit.DirectoryExists(TFileKit.CombinePaths(FExtractDir, 'level1')));
end;

initialization
  RegisterTest(TArchiveTests);
end. 