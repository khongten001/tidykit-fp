# Working with ZIP Files in Free Pascal

This tutorial demonstrates how to use the `zipper` unit in Free Pascal to compress and extract ZIP files.

## Prerequisites

```pascal
uses
  Classes,    // For TStrings, TStringList
  SysUtils,   // For file operations, path manipulation
  zipper;     // The ZIP compression/decompression unit
```

## Compressing Files

### Single File
```pascal
procedure CompressSingleFile(const SourceFile, ZipFile: string);
var
  Zipper: TZipper;  // Main class for creating ZIP files
begin
  // Create a new ZIP creator instance
  Zipper := TZipper.Create;
  try
    // Set the output ZIP file name
    // This doesn't create the file yet, just sets the name
    Zipper.FileName := ZipFile;
    
    // Add file to the ZIP entries list
    // Parameters:
    //   1. Full path to source file
    //   2. Name to use inside ZIP (here we use just the filename)
    Zipper.Entries.AddFileEntry(SourceFile, ExtractFileName(SourceFile));
    
    // Actually create the ZIP file and compress all entries
    // This is when the file is created and written
    Zipper.ZipAllFiles;
  finally
    // Always free the Zipper to release resources
    Zipper.Free;
  end;
end;
```

### Single Directory (Non-Recursive)
```pascal
procedure CompressDirectory(const SourceDir, ZipFile: string);
var
  Zipper: TZipper;
  SearchRec: TSearchRec;     // Used for directory scanning
  BaseDir: string;           // Full path to source directory
begin
  Zipper := TZipper.Create;
  try
    Zipper.FileName := ZipFile;
    
    // Ensure directory path ends with path delimiter
    // This is important for correct path handling
    BaseDir := IncludeTrailingPathDelimiter(SourceDir);
    
    // Find all files in directory (not recursive)
    // faAnyFile - faDirectory means "all files but not directories"
    if FindFirst(BaseDir + '*.*', faAnyFile - faDirectory, SearchRec) = 0 then
    begin
      repeat
        // Add each file to the ZIP
        // The file will appear at the root level in the ZIP
        Zipper.Entries.AddFileEntry(
          BaseDir + SearchRec.Name,  // Full path to actual file
          SearchRec.Name             // Name to use in ZIP (no path)
        );
      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);  // Always close directory search
    end;
    
    // Create ZIP file with all entries
    Zipper.ZipAllFiles;
  finally
    Zipper.Free;
  end;
end;
```

### Directory with Subdirectories (Recursive)
```pascal
procedure CompressRecursive(const SourceDir, ZipFile: string);
var
  Zipper: TZipper;
  BaseDir: string;  // Root directory for relative path calculation
  
  // Nested procedure to handle directory recursion
  procedure AddDirectory(const Directory: string);
  var
    SearchRec: TSearchRec;
    FullPath: string;      // Full path to current file/directory
    RelativePath: string;  // Path relative to base directory
  begin
    // Find all files and directories
    if FindFirst(Directory + '*', faAnyFile, SearchRec) = 0 then
    begin
      try
        repeat
          // Skip . and .. directory entries
          if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
          begin
            // Build full path to current item
            FullPath := Directory + SearchRec.Name;
            
            // Calculate path relative to base directory
            // This preserves directory structure in ZIP
            RelativePath := ExtractRelativePath(BaseDir, FullPath);
            
            if (SearchRec.Attr and faDirectory) <> 0 then
              // If it's a directory, recurse into it
              // Note: ZIP format doesn't need explicit directory entries
              AddDirectory(IncludeTrailingPathDelimiter(FullPath))
            else
              // If it's a file, add it with its relative path
              // This maintains the directory structure in the ZIP
              Zipper.Entries.AddFileEntry(FullPath, RelativePath);
          end;
        until FindNext(SearchRec) <> 0;
      finally
        FindClose(SearchRec);
      end;
    end;
  end;
  
begin
  Zipper := TZipper.Create;
  try
    Zipper.FileName := ZipFile;
    
    // Set up base directory for relative path calculations
    BaseDir := IncludeTrailingPathDelimiter(SourceDir);
    
    // Start the recursive process
    AddDirectory(BaseDir);
    
    // Create the final ZIP file
    Zipper.ZipAllFiles;
  finally
    Zipper.Free;
  end;
end;
```

## Extracting Files

### Extract All Files
```pascal
procedure ExtractAll(const ZipFile, DestDir: string);
var
  Unzipper: TUnZipper;  // Main class for extracting ZIP files
begin
  Unzipper := TUnZipper.Create;
  try
    // Set source ZIP file
    Unzipper.FileName := ZipFile;
    
    // Set extraction directory
    // TUnZipper will automatically:
    // - Create necessary directories
    // - Preserve directory structure
    // - Handle path separators
    Unzipper.OutputPath := DestDir;
    
    // Extract everything
    // This will:
    // - Create directories as needed
    // - Extract all files
    // - Preserve attributes where possible
    Unzipper.UnZipAllFiles;
  finally
    Unzipper.Free;
  end;
end;
```

### Extract Specific Files
```pascal
procedure ExtractFiles(const ZipFile, DestDir: string; const FileList: TStrings);
var
  Unzipper: TUnZipper;
begin
  Unzipper := TUnZipper.Create;
  try
    Unzipper.FileName := ZipFile;
    Unzipper.OutputPath := DestDir;
    
    // Extract only files in the list
    // FileList should contain exact paths as they appear in ZIP
    // Example: 'dir1/file.txt', not 'C:\dir1\file.txt'
    Unzipper.UnZipFiles(FileList);
  finally
    Unzipper.Free;
  end;
end;
```

### List ZIP Contents
```pascal
procedure ListZipContents(const ZipFile: string);
var
  Unzipper: TUnZipper;
  I: Integer;
begin
  Unzipper := TUnZipper.Create;
  try
    Unzipper.FileName := ZipFile;
    
    // Read ZIP directory without extracting
    // This populates the Entries property
    Unzipper.Examine;
    
    // List all entries
    // Entries include both files and directories
    for I := 0 to Unzipper.Entries.Count - 1 do
      WriteLn(Unzipper.Entries[I].ArchiveFileName);
  finally
    Unzipper.Free;
  end;
end;
```

## Key Points

1. **Directory Structure**
   - ZIP format automatically handles directory structure
   - No need to explicitly create directory entries
   - Just include the correct relative paths when adding files
   - Example: Adding 'dir1/file.txt' automatically handles 'dir1' directory

2. **Path Handling**
   - Use `IncludeTrailingPathDelimiter` for directory paths
   - Use `ExtractRelativePath` to get paths relative to base directory
   - Use `ExtractFileName` for entry names without paths
   - ZIP format accepts both '/' and '\' as path separators

3. **Memory Management**
   - Always free `TZipper` and `TUnZipper` instances
   - Use try-finally blocks to ensure proper cleanup
   - No need to manage file streams - the classes handle this

4. **Best Practices**
   - Check file existence before operations
   - Handle exceptions appropriately
   - Use relative paths in ZIP entries for portability
   - Don't assume path separator type - handle both '/' and '\'

## Example Usage

```pascal
// Compress a single file
CompressSingleFile('test.txt', 'output.zip');

// Compress a directory (non-recursive)
CompressDirectory('C:\MyFiles', 'output.zip');

// Compress a directory with all subdirectories
CompressRecursive('C:\MyFiles', 'output.zip');

// Extract all files
ExtractAll('archive.zip', 'C:\ExtractedFiles');

// Extract specific files
var
  Files: TStringList;
begin
  Files := TStringList.Create;
  try
    // Note: Use paths as they appear in ZIP
    Files.Add('file1.txt');           // File in root
    Files.Add('docs/file2.txt');      // File in subdirectory
    ExtractFiles('archive.zip', 'C:\ExtractedFiles', Files);
  finally
    Files.Free;
  end;
end;

// List ZIP contents
ListZipContents('archive.zip');
```

## Common Issues and Solutions

1. **File Not Found**
   - Always check if source files exist before adding
   - Use absolute paths or correct relative paths
   - Handle exceptions for missing files

2. **Path Separators**
   - ZIP format accepts both '/' and '\'
   - Be consistent in your code but handle both in input
   - Use `PathDelim` constant for current platform

3. **Memory Leaks**
   - Always use try-finally blocks
   - Free all created objects
   - Let TZipper/TUnZipper manage their own resources

4. **Permission Issues**
   - Check write permissions for output ZIP
   - Check read permissions for input files
   - Handle access denied exceptions 