# TidyKit.FS Documentation

The `TidyKit.FS` unit provides comprehensive file system operations for FreePascal applications. It offers a unified interface for working with files, directories, paths, and file attributes across different operating systems.

## Features

- Cross-platform file and directory operations
- Path manipulation and analysis
- File searching and pattern matching
- File attributes and metadata handling
- Symbolic link support
- File locking mechanisms
- Directory information and statistics
- File validation and sanitization

## Basic Usage

### File Operations

```pascal
// Reading and writing files
var
  Content: string;
begin
  // Read entire file
  Content := TFileKit.ReadTextFile('input.txt');
  
  // Write to file (creates file if it doesn't exist)
  TFileKit.WriteTextFile('output.txt', 'Hello, World!');
  
  // Append text to file
  TFileKit.AppendText('log.txt', 'New log entry');
  
  // Prepend text to file
  TFileKit.PrependText('log.txt', 'Log header');
  
  // Replace text in file
  TFileKit.ReplaceText('config.txt', 'old_value', 'new_value');
end;
```

### Directory Operations

```pascal
// Basic directory operations
TFileKit.CreateDirectory('new_dir');                // Create single directory
TFileKit.EnsureDirectory('path/to/deep/dir');      // Create directory tree
TFileKit.DeleteDirectory('old_dir', True);         // Delete with contents

// List directory contents
var
  Files: TFilePathArray;
  Dirs: TFilePathArray;
begin
  // List files with various options
  Files := TFileKit.ListFiles('.', '*.txt');                // List .txt files
  Files := TFileKit.ListFiles('.', '*', True);              // Recursive listing
  Files := TFileKit.ListFiles('.', '*', False, fsName);     // Sort by name
  
  // List directories
  Dirs := TFileKit.ListDirectories('.', '*', True);         // Recursive
  Dirs := TFileKit.ListDirectories('.', 'test_*');          // Pattern matching
end;
```

### Path Analysis

```pascal
// Path manipulation and analysis
var
  CommonPath: string;
  RelPath: string;
begin
  // Find common path between two paths
  CommonPath := TFileKit.GetCommonPath('/usr/local/bin', '/usr/local/lib');
  // Returns '/usr/local'
  
  // Get relative path
  RelPath := TFileKit.GetRelativePath('/usr/share', '/usr/local/bin');
  // Returns '../local/bin'
  
  // Check if one path is subpath of another
  if TFileKit.IsSubPath('/usr/local', '/usr/local/bin') then
    WriteLn('Is subpath');
end;
```

### File Information

```pascal
// Get file information
var
  Attrs: TFileAttributes;
  Size: Int64;
  ModTime: TDateTime;
begin
  // Get file attributes
  Attrs := TFileKit.GetAttributes('file.txt');
  WriteLn('Read-only: ', Attrs.ReadOnly);
  WriteLn('Hidden: ', Attrs.Hidden);
  
  // Get file size
  Size := TFileKit.GetSize('file.txt');
  
  // Get modification time
  ModTime := TFileKit.GetLastWriteTime('file.txt');
end;
```

### Search Operations

```pascal
// Search for files
var
  Results: TSearchResults;
  NewestFile: string;
  LargestFile: string;
begin
  // Search files recursively
  Results := TFileKit.SearchFiles('.', '*.txt', True);
  
  // Find specific files
  NewestFile := TFileKit.FindLastModifiedFile('.', '*.txt');
  LargestFile := TFileKit.FindLargestFile('.', '*.txt');
end;
```

### File Validation and Sanitization

```pascal
// Validate and sanitize file names and paths
var
  SafeName: string;
begin
  // Check if filename is valid
  if TFileKit.IsValidFileName('file*.txt') then
    WriteLn('Valid filename');
    
  // Sanitize filename
  SafeName := TFileKit.SanitizeFileName('file*.txt');
  // Returns 'file_txt'
  
  // Make path valid
  Path := TFileKit.MakeValidPath('/path//to/./file');
  // Returns '/path/to/file'
end;
```

### Directory Information

```pascal
// Get directory statistics
var
  Info: TDirectoryInfo;
begin
  Info := TFileKit.GetDirectoryInfo('dir');
  WriteLn('Files: ', Info.FileCount);
  WriteLn('Directories: ', Info.DirectoryCount);
  WriteLn('Total size: ', Info.TotalSize);
  WriteLn('Newest file: ', Info.NewestFile);
end;
```

### Symbolic Links

```pascal
// Work with symbolic links
begin
  // Create symlink
  TFileKit.CreateSymLink('target.txt', 'link.txt');
  
  // Create directory symlink
  TFileKit.CreateSymLink('target_dir', 'link_dir', True);
  
  // Get target of symlink
  Target := TFileKit.ResolveSymLink('link.txt');
  
  // Check if path is symlink
  if TFileKit.IsSymLink('link.txt') then
    WriteLn('Is symlink');
end;
```

## Platform-Specific Considerations

### Windows

- Symlink creation requires Administrator privileges or Developer Mode
- File paths use backslashes (`\`) but forward slashes (`/`) are also accepted
- Drive letters (e.g., `C:`) are supported
- Maximum path length is 260 characters by default
- File locking is implemented using Windows API exclusive file access
- Hidden files are determined by file attributes

### Unix/Linux

- Regular users can create symlinks in their own directories
- File paths use forward slashes (`/`)
- No drive letters, all paths are relative to root (`/`)
- Maximum path length is typically 4096 characters
- File locking is implemented using lock files
- Hidden files are determined by leading dot in filename

## Error Handling

The unit uses exceptions to report errors. Always wrap operations in try-except blocks:

```pascal
try
  TFileKit.CopyFile('source.txt', 'dest.txt');
except
  on E: ETidyKitException do
    WriteLn('Error: ', E.Message);
end;
```

## Best Practices

1. Always check file existence before operations:
   ```pascal
   if TFileKit.Exists(FilePath) then
     TFileKit.DeleteFile(FilePath);
   ```

2. Use path manipulation functions for cross-platform compatibility:
   ```pascal
   Path := TFileKit.CombinePaths(Dir, FileName);
   ```

3. Handle file locks properly:
   ```pascal
   if TFileKit.LockFile(FilePath) then
   try
     // Work with file
   finally
     TFileKit.UnlockFile(FilePath);
   end;
   ```

4. Use appropriate search functions:
   - `SearchFiles`: For complex searches with full file information
   - `ListFiles`: For simple directory listing
   - `FindXXXFile`: For finding specific files by criteria

5. Sanitize file names from user input:
   ```pascal
   SafeName := TFileKit.SanitizeFileName(UserInput);
   ```

## Performance Considerations

1. For large directories:
   - Avoid recursive operations unless necessary
   - Use non-recursive `ListFiles` when possible
   - Consider using pattern matching to filter files

2. For frequent operations:
   - Cache file information when appropriate
   - Reuse `TSearchResults` instead of searching repeatedly

3. For large files:
   - Consider chunked processing for very large files
   - Use appropriate buffer sizes for file operations 