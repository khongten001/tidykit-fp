# Working with TAR Files in Free Pascal

This tutorial demonstrates how to use the `libtar` unit in Free Pascal to create and extract TAR archives.

## Prerequisites

```pascal
uses
  Classes,    // For TFileStream
  SysUtils,   // For file operations, path manipulation
  libtar;     // The TAR archive handling unit
```

## Creating TAR Archives

### Single File
```pascal
procedure CompressSingleFile(const SourceFile, TarFile: string);
var
  TarWriter: TTarWriter;    // Main class for creating TAR files
  FileStream: TFileStream;  // Required for TAR output
begin
  // Create output file stream
  // Note: Unlike ZIP, we must manage the stream ourselves
  FileStream := TFileStream.Create(TarFile, fmCreate);
  try
    // Create TAR writer with our stream
    TarWriter := TTarWriter.Create(FileStream);
    try
      // Add file to TAR
      // Parameters:
      //   1. Full path to source file
      //   2. Name to use inside TAR (here we use just the filename)
      TarWriter.AddFile(SourceFile, ExtractFileName(SourceFile));
      
      // IMPORTANT: Write TAR footer
      // Without this, the TAR file will be invalid
      TarWriter.Finalize;
    finally
      TarWriter.Free;
    end;
  finally
    FileStream.Free;  // Must free the stream ourselves
  end;
end;
```

### Single Directory (Non-Recursive)
```pascal
procedure CompressDirectory(const SourceDir, TarFile: string);
var
  TarWriter: TTarWriter;
  FileStream: TFileStream;
  SearchRec: TSearchRec;
  BaseDir: string;
begin
  FileStream := TFileStream.Create(TarFile, fmCreate);
  try
    TarWriter := TTarWriter.Create(FileStream);
    try
      BaseDir := IncludeTrailingPathDelimiter(SourceDir);
      
      // IMPORTANT: Add directory entry first
      // TAR format requires explicit directory entries
      // Must end with trailing slash and include modification time
      TarWriter.AddDir(
        ExtractFileName(ExcludeTrailingPathDelimiter(SourceDir)) + '/',
        GetLastWriteTime(SourceDir)
      );
      
      // Find all files in directory
      if FindFirst(BaseDir + '*.*', faAnyFile - faDirectory, SearchRec) = 0 then
      begin
        try
          repeat
            // Add each file with relative path
            // Unlike ZIP, TAR is more strict about paths
            TarWriter.AddFile(
              BaseDir + SearchRec.Name,  // Full source path
              SearchRec.Name             // Entry name in TAR
            );
          until FindNext(SearchRec) <> 0;
        finally
          FindClose(SearchRec);
        end;
      end;
      
      // IMPORTANT: Write TAR footer
      TarWriter.Finalize;
    finally
      TarWriter.Free;
    end;
  finally
    FileStream.Free;
  end;
end;
```

### Directory with Subdirectories (Recursive)
```pascal
procedure CompressRecursive(const SourceDir, TarFile: string);
var
  TarWriter: TTarWriter;
  FileStream: TFileStream;
  BaseDir: string;
  
  // Nested procedure to handle directory recursion
  procedure AddDirectory(const Directory: string);
  var
    SearchRec: TSearchRec;
    FullPath, RelativePath: string;
  begin
    // IMPORTANT: Add directory entry first (with trailing slash)
    // This is required for TAR format
    RelativePath := IncludeTrailingPathDelimiter(
      ExtractRelativePath(BaseDir, Directory)
    );
    
    // Don't add root directory entry
    if RelativePath <> '' then
      TarWriter.AddDir(RelativePath, GetLastWriteTime(Directory));
    
    // Find all files and subdirectories
    if FindFirst(Directory + '*', faAnyFile, SearchRec) = 0 then
    begin
      try
        repeat
          if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
          begin
            FullPath := Directory + SearchRec.Name;
            RelativePath := ExtractRelativePath(BaseDir, FullPath);
            
            if (SearchRec.Attr and faDirectory) <> 0 then
              // Recurse into subdirectory
              // Note: Directory entry will be added in recursive call
              AddDirectory(IncludeTrailingPathDelimiter(FullPath))
            else
              // Add file with its relative path
              TarWriter.AddFile(FullPath, RelativePath);
          end;
        until FindNext(SearchRec) <> 0;
      finally
        FindClose(SearchRec);
      end;
    end;
  end;
  
begin
  // Create output stream
  FileStream := TFileStream.Create(TarFile, fmCreate);
  try
    TarWriter := TTarWriter.Create(FileStream);
    try
      BaseDir := IncludeTrailingPathDelimiter(SourceDir);
      
      // Start recursive process
      // This will:
      // 1. Add directory entries with trailing slashes
      // 2. Add files with correct relative paths
      // 3. Maintain directory structure
      AddDirectory(BaseDir);
      
      // IMPORTANT: Write TAR footer
      TarWriter.Finalize;
    finally
      TarWriter.Free;
    end;
  finally
    FileStream.Free;
  end;
end;
```

## Extracting TAR Archives

### Extract All Files
```pascal
procedure ExtractAll(const TarFile, DestDir: string);
var
  TarArchive: TTarArchive;  // Main class for reading TAR files
  DirRec: TTarDirRec;      // Holds entry information
  OutputFile: string;       // Full path for output
  DestPath: string;        // Base extraction path
begin
  // Create TAR reader
  // Note: Unlike ZIP, this handles its own file stream
  TarArchive := TTarArchive.Create(TarFile);
  try
    DestPath := IncludeTrailingPathDelimiter(DestDir);
    
    // IMPORTANT: Reset to start reading
    // Without this, FindNext won't work
    TarArchive.Reset;
    
    // Read each entry
    while TarArchive.FindNext(DirRec) do
    begin
      // Build full output path
      OutputFile := DestPath + DirRec.Name;
      
      // Handle different entry types
      case DirRec.FileType of
        ftDirectory:
          begin
            // Create directory
            // Note: Remove trailing slash for ForceDirectories
            if not ForceDirectories(ExcludeTrailingPathDelimiter(OutputFile)) then
              raise Exception.CreateFmt('Failed to create directory: %s', [OutputFile]);
          end;
          
        ftNormal:
          begin
            // Create parent directory for file
            if not ForceDirectories(ExtractFilePath(OutputFile)) then
              raise Exception.CreateFmt('Failed to create directory: %s', 
                [ExtractFilePath(OutputFile)]);
            
            // Extract the file
            // This reads from current position in archive
            TarArchive.ReadFile(OutputFile);
          end;
      end;
    end;
  finally
    TarArchive.Free;
  end;
end;
```

### List TAR Contents
```pascal
procedure ListTarContents(const TarFile: string);
var
  TarArchive: TTarArchive;
  DirRec: TTarDirRec;
begin
  TarArchive := TTarArchive.Create(TarFile);
  try
    // IMPORTANT: Reset before reading
    TarArchive.Reset;
    
    // Read each entry
    while TarArchive.FindNext(DirRec) do
    begin
      // Handle different entry types differently
      case DirRec.FileType of
        ftDirectory:
          WriteLn('Directory: ', DirRec.Name);
        ftNormal:
          WriteLn('File: ', DirRec.Name, ' (', DirRec.Size, ' bytes)');
      end;
    end;
  finally
    TarArchive.Free;
  end;
end;
```

## Key Points

1. **Directory Structure**
   - TAR format requires explicit directory entries
   - Directory entries must end with trailing slash ('/' or '\')
   - Add directories before their contents
   - Use `AddDir` for directories and `AddFile` for files
   - Directory entries must have modification times

2. **Path Handling**
   - Use `IncludeTrailingPathDelimiter` for directory paths in TAR
   - Use `ExcludeTrailingPathDelimiter` when creating directories on disk
   - Use `ExtractRelativePath` to get paths relative to base directory
   - Be consistent with path separators

3. **Memory Management**
   - Always free `TTarWriter` and `TTarArchive` instances
   - Always free `TFileStream` instances when you create them
   - Use try-finally blocks to ensure proper cleanup
   - TAR writer needs manual stream management

4. **Best Practices**
   - Always call `TarWriter.Finalize` before closing
   - Always call `TarArchive.Reset` before reading
   - Handle both `ftDirectory` and `ftNormal` entry types
   - Create directories before extracting files into them
   - Check file and directory permissions

## Example Usage

```pascal
// Create a TAR file with a single file
CompressSingleFile('test.txt', 'output.tar');

// Create a TAR file from a directory (non-recursive)
CompressDirectory('C:\MyFiles', 'output.tar');

// Create a TAR file from a directory with all subdirectories
CompressRecursive('C:\MyFiles', 'output.tar');

// Extract all files from a TAR archive
ExtractAll('archive.tar', 'C:\ExtractedFiles');

// List TAR contents
ListTarContents('archive.tar');
```

## Common Issues and Solutions

1. **Invalid TAR File**
   - Always call `TarWriter.Finalize`
   - Add directory entries with trailing slashes
   - Add directories before their contents
   - Handle file streams properly

2. **Directory Structure Issues**
   - Add explicit directory entries
   - Use trailing slashes for directories
   - Create parent directories before files
   - Handle path separators consistently

3. **Reading Problems**
   - Always call `TarArchive.Reset` before reading
   - Check file permissions
   - Handle both file types (`ftDirectory` and `ftNormal`)
   - Create directories before extracting files

4. **Stream Management**
   - Create file streams in `fmCreate` mode for writing
   - Free streams in finally blocks
   - Handle stream exceptions
   - Check disk space and permissions

## Important Differences from ZIP

1. **Directory Handling**
   - ZIP: Directories are implicit from file paths
   - TAR: Directories must be explicitly added with `AddDir`
   - TAR: Directory entries must have trailing slashes
   - TAR: Directory entries need modification times

2. **Path Separators**
   - ZIP: Works with both forward and backward slashes
   - TAR: Traditionally uses forward slashes
   - TAR: Our implementation handles both, but be consistent

3. **File Stream**
   - ZIP: `TZipper` manages its own file stream
   - TAR: You must create and manage the file stream
   - TAR: Stream management requires careful error handling

4. **Finalization**
   - ZIP: No explicit finalization needed
   - TAR: Must call `TarWriter.Finalize` to write footer
   - TAR: Missing footer makes archive invalid

5. **Reading Entries**
   - ZIP: Use `Examine` to read directory
   - TAR: Must use `Reset` and `FindNext`
   - TAR: Sequential access only 