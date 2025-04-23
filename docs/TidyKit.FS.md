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

The TidyKit.FS module now uses a Factory/Interface pattern. This provides several benefits:
- Automatic memory management through interface reference counting
- Ability to mock file operations for testing
- More object-oriented design
- Potential for different implementations in the future

### Creating a FileKit Instance

```pascal
var
  FileKit: IFileKit;
begin
  // Create an instance using the factory
  FileKit := TFSFactory.CreateFileKit;
  
  // Use the instance for file operations
  // When FileKit goes out of scope, memory is automatically managed
end;
```

### File Operations

```pascal
// Reading and writing files
var
  FileKit: IFileKit;
  Content: string;
begin
  FileKit := TFSFactory.CreateFileKit;
  
  // Read entire file
  Content := FileKit.ReadTextFile('input.txt');
  
  // Write to file (creates file if it doesn't exist)
  FileKit.WriteTextFile('output.txt', 'Hello, World!');
  
  // Append text to file
  FileKit.AppendText('log.txt', 'New log entry');
  
  // Prepend text to file
  FileKit.PrependText('log.txt', 'Log header');
  
  // Replace text in file
  FileKit.ReplaceText('config.txt', 'old_value', 'new_value');
end;
```

### Directory Operations

```pascal
var
  FileKit: IFileKit;
begin
  FileKit := TFSFactory.CreateFileKit;
  
  // Basic directory operations
  FileKit.CreateDirectory('new_dir');                // Create single directory
  FileKit.EnsureDirectory('path/to/deep/dir');      // Create directory tree
  FileKit.DeleteDirectory('old_dir', True);         // Delete with contents

  // List directory contents
  var
    Files: TFilePathArray;
    Dirs: TFilePathArray;
  begin
    // List files with various options
    Files := FileKit.ListFiles('.', '*.txt');                // List .txt files
    Files := FileKit.ListFiles('.', '*', True);              // Recursive listing
    Files := FileKit.ListFiles('.', '*', False, fsName);     // Sort by name
    
    // List directories
    Dirs := FileKit.ListDirectories('.', '*', True);         // Recursive
    Dirs := FileKit.ListDirectories('.', 'test_*');          // Pattern matching
  end;
end;
```

### Path Analysis

```pascal
var
  FileKit: IFileKit;
  CommonPath: string;
  RelPath: string;
begin
  FileKit := TFSFactory.CreateFileKit;
  
  // Find common path between two paths
  CommonPath := FileKit.GetCommonPath('/usr/local/bin', '/usr/local/lib');
  // Returns '/usr/local'
  
  // Get relative path
  RelPath := FileKit.GetRelativePath('/usr/share', '/usr/local/bin');
  // Returns '../local/bin'
  
  // Check if one path is subpath of another
  if FileKit.IsSubPath('/usr/local', '/usr/local/bin') then
    WriteLn('Is subpath');
end;
```

### File Information

```pascal
var
  FileKit: IFileKit;
  Attrs: TFileAttributes;
  Size: Int64;
  ModTime: TDateTime;
begin
  FileKit := TFSFactory.CreateFileKit;
  
  // Get file attributes
  Attrs := FileKit.GetAttributes('file.txt');
  WriteLn('Read-only: ', Attrs.ReadOnly);
  WriteLn('Hidden: ', Attrs.Hidden);
  
  // Get file size
  Size := FileKit.GetSize('file.txt');
  
  // Get modification time
  ModTime := FileKit.GetLastWriteTime('file.txt');
end;
```

### Search Operations

```pascal
var
  FileKit: IFileKit;
  Results: TSearchResults;
  NewestFile: string;
  LargestFile: string;
begin
  FileKit := TFSFactory.CreateFileKit;
  
  // Search files recursively
  Results := FileKit.SearchFiles('.', '*.txt', True);
  
  // Find specific files
  NewestFile := FileKit.FindLastModifiedFile('.', '*.txt');
  LargestFile := FileKit.FindLargestFile('.', '*.txt');
end;
```

### File Validation and Sanitization

```pascal
var
  FileKit: IFileKit;
  SafeName: string;
begin
  FileKit := TFSFactory.CreateFileKit;
  
  // Check if filename is valid
  if FileKit.IsValidFileName('file*.txt') then
    WriteLn('Valid filename');
    
  // Sanitize filename
  SafeName := FileKit.SanitizeFileName('file*.txt');
  // Returns 'file_txt'
  
  // Make path valid
  Path := FileKit.MakeValidPath('/path//to/./file');
  // Returns '/path/to/file'
end;
```

### Directory Information

```pascal
var
  FileKit: IFileKit;
  Info: TDirectoryInfo;
begin
  FileKit := TFSFactory.CreateFileKit;
  
  Info := FileKit.GetDirectoryInfo('dir');
  WriteLn('Files: ', Info.FileCount);
  WriteLn('Directories: ', Info.DirectoryCount);
  WriteLn('Total size: ', Info.TotalSize);
  WriteLn('Newest file: ', Info.NewestFile);
end;
```

### Symbolic Links

```pascal
var
  FileKit: IFileKit;
  Target: string;
begin
  FileKit := TFSFactory.CreateFileKit;
  
  // Create symlink
  FileKit.CreateSymLink('target.txt', 'link.txt');
  
  // Create directory symlink
  FileKit.CreateSymLink('target_dir', 'link_dir', True);
  
  // Get target of symlink
  Target := FileKit.ResolveSymLink('link.txt');
  
  // Check if path is symlink
  if FileKit.IsSymLink('link.txt') then
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

The TidyKit.FS module uses a dedicated exception class, `EFileSystemError`, for file system-related errors. This allows you to specifically catch file system errors while letting other types of exceptions propagate as normal.

```pascal
var
  FileKit: IFileKit;
begin
  FileKit := TFSFactory.CreateFileKit;
  
  try
    FileKit.CopyFile('source.txt', 'dest.txt');
  except
    on E: EFileSystemError do
      WriteLn('File System Error: ', E.Message);
    on E: Exception do
      WriteLn('Other Error: ', E.Message);
  end;
end;
```

Specific errors that might be raised include:
- File not found
- Access denied
- Invalid path
- Directory not empty
- Disk full
- Symbolic link creation failures

## Best Practices

1. Always check file existence before operations:
   ```pascal
   if FileKit.Exists(FilePath) then
     FileKit.DeleteFile(FilePath);
   ```

2. Use path manipulation functions for cross-platform compatibility:
   ```pascal
   Path := FileKit.CombinePaths(Dir, FileName);
   ```

3. Handle file locks properly:
   ```pascal
   if FileKit.LockFile(FilePath) then
   try
     // Work with file
   finally
     FileKit.UnlockFile(FilePath);
   end;
   ```

4. Use appropriate search functions:
   - `SearchFiles`: For complex searches with full file information
   - `ListFiles`: For simple directory listing
   - `FindXXXFile`: For finding specific files by criteria

5. Sanitize file names from user input:
   ```pascal
   SafeName := FileKit.SanitizeFileName(UserInput);
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