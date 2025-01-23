# 🧰 TidyKit

TidyKit is a Free Pascal library that helps you tackle common tasks faster, with clean, type-safe code.

> [!WARNING]
> This library is currently in early development stage. The API is not stable and may undergo breaking changes between versions. 
> 
> Use with caution in production environments.

## 📑 Table of Contents
- [🧰 TidyKit](#-tidykit)
  - [📑 Table of Contents](#-table-of-contents)
  - [✅ TODO](#-todo)
  - [✨ Features](#-features)
    - [🗂️ FileSystem Operations](#️-filesystem-operations)
    - [📝 String Operations](#-string-operations)
    - [📅 DateTime Operations](#-datetime-operations)
    - [🎯 Core Features](#-core-features)
  - [🌐 Platform Compatibility](#-platform-compatibility)
    - [📝 Platform-Specific Notes](#-platform-specific-notes)
      - [Windows](#windows)
      - [Unix-like Systems (Linux, macOS, FreeBSD)](#unix-like-systems-linux-macos-freebsd)
  - [🧪 Platform Testing Status](#-platform-testing-status)
  - [📦 Installation](#-installation)
    - [📚 Dependencies](#-dependencies)
    - [ℹ️ Compatibility Notes](#ℹ️-compatibility-notes)
  - [🚀 Quick Start](#-quick-start)
    - [📝 String Operations](#-string-operations-1)
    - [📅 DateTime Operations](#-datetime-operations-1)
    - [🗂️ FileSystem Operations](#️-filesystem-operations-1)
  - [🧪 Unit Testing](#-unit-testing)
  - [📚 Examples](#-examples)
  - [🤝 Contributing](#-contributing)
  - [📝 License](#-license)
  - [🙏 Acknowledgments](#-acknowledgments)
  - [📞 Contact](#-contact)
  - [📋 Cheat Sheet](#-cheat-sheet)
    - [File System Operations](#file-system-operations)
    - [String operations](#string-operations)
    - [DateTime Operations](#datetime-operations)
      - [Basic Operations](#basic-operations)
      - [Component Access](#component-access)
      - [Component Modification](#component-modification)
      - [Date Arithmetic](#date-arithmetic)
      - [Period Operations](#period-operations)
      - [Interval Operations](#interval-operations)
      - [Date Comparison](#date-comparison)
      - [Period Boundaries](#period-boundaries)
      - [Date Rounding](#date-rounding)
      - [Timezone Operations](#timezone-operations)

## ✅ TODO

- [ ] Add comprehensive documentation
  - [ ] Add detailed API reference
  - [ ] Add best practices / cookbook guide
  - [ ] Add troubleshooting guide  

- [ ] Expand test coverage
  - [ ] Add more unit tests
  - [ ] Add edge case tests
  

## ✨ Features

### 🗂️ FileSystem Operations
- File reading/writing with encoding detection
- Directory creation/deletion with recursive options
- File/directory listing with sorting options (by name, date, size)
- File searching with pattern matching
- File attributes and metadata handling
- Path manipulation and normalization
- Temporary file/directory creation
- Cross-platform path handling
- File encoding detection (UTF-8, UTF-16, UTF-32, ASCII)
- File type detection (text vs binary)

### 📝 String Operations
- Basic operations: trim, case conversion, substring
- Pattern matching with regex support
- String padding and alignment (left, right, center)
- Word operations and tokenization
- String tests (contains, starts with, ends with)
- String manipulation (replace, duplicate, reverse)
- Whitespace handling (collapse, remove)
- String metrics (length, count substrings)

### 📅 DateTime Operations
- Date/time parsing and formatting
- Component access (year, month, day, etc.)
- Date arithmetic (add/subtract periods)
- Business day calculations
- Period and interval operations
- Date rounding and boundaries
- Calendar calculations (ISO weeks, epidemiological weeks)
- Timezone handling (with enhanced Windows support)
- Date comparisons and tests
- Special date operations (rollback/forward month)

### 🎯 Core Features
- FPC 3.2.2 Compatible: No inline var, anonymous functions, or lambda
- Cross-Platform: Works on Windows, Linux, macOS, and FreeBSD
- Static Functions: No instance management or memory leaks
- Memory Safe: Proper resource management
- Exception Handling: Custom exception types for better error handling
- Consistent API: Similar patterns across all modules
- **Partial Symbolic Link Support:** Detects symbolic links but lacks full manipulation capabilities

## 🌐 Platform Compatibility

TidyKit is designed to be platform-independent and works across:
- 🪟 Windows (32/64-bit)
- 🐧 Linux
- 🍎 macOS
- 🐡 FreeBSD
- 🔄 Any platform supported by Free Pascal

All operations automatically handle platform-specific differences:
- 📁 File paths (directory separators, drive letters)
- ↩️ Line endings (CR, LF, CRLF)
- 🔒 File system permissions
- ⏰ Date/time handling (timezones, DST)

### 📝 Platform-Specific Notes

#### Windows
- ✅ Full timezone support with DST handling
- ✅ Complete file attribute support
- ✅ Advanced file system operations

#### Unix-like Systems (Linux, macOS, FreeBSD)
- ⚠️ Basic timezone support (UTC only)
- ✅ Unix-style file permissions
- ⚠️ Partial Symbolic Link Support: Detects symbolic links but lacks full manipulation capabilities
- ⚠️ Limited file attribute support

## 🧪 Platform Testing Status

⚠️ **Note**: Current testing has been performed on:
- ✅ Windows (32/64-bit)

While the library is designed to be cross-platform, the following platforms have not been tested yet:
- ⚠️ Linux
- ⚠️ macOS
- ⚠️ FreeBSD

Contributions for testing and validation on other platforms are welcome! 🤝

## 📦 Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/ikelaiah/tidykit-fp
   ```

2. Add the `src` directory to your project's unit search path:
   - 🔧 In Lazarus: Project -> Project Options -> Compiler Options -> Paths -> Other unit files
   - ⌨️ In FPC: Use `-Fu` command line option

3. Add `TidyKit` to your uses clause:
   ```pascal
   program MyProject;
   
   {$mode objfpc}{$H+}{$J-}
   
   uses
     {$IFDEF UNIX}
     cthreads,
     {$ENDIF}
     Classes, SysUtils,
     TidyKit; // Add this line to use TidyKit
   
   begin
     // Your code here
   end.
   ```

4. Verify installation by checking if you can use any of the kits:
   ```pascal
   var
     CurrentTime: TDateTime;
   begin
     CurrentTime := TDateTimeKit.GetNow;
     WriteLn('Current time: ', TDateTimeKit.GetAsString(CurrentTime));
   end.
   ```

### 📚 Dependencies

TidyKit requires:
- 🔨 Free Pascal Compiler (FPC) 3.2.2 or later
- 🆓 No external dependencies required

### ℹ️ Compatibility Notes

- 🪟 Windows: Fully tested and supported
- 🐧 Linux/macOS/FreeBSD: Designed to work but needs testing
- 🌐 Unicode: Full UTF-8 support
- 🔄 Thread Safety: Most immutable operations (like DateTime calculations) are thread-safe, but file operations should be synchronized when used across threads

## 🚀 Quick Start

### 📝 String Operations
```pascal
uses
  TidyKit;

var
  Str: string;
  Words: TStringArray;
  Matches: TStringMatches;
begin
  // Basic transformations
  Str := TStringKit.Trim('  Hello, World!  ');
  Str := TStringKit.ToLower(Str);
  Str := TStringKit.ReplaceText(Str, 'hello', 'hi');
  WriteLn(Str);  // 'hi, world!'
  
  // Advanced transformations
  Str := TStringKit.ReverseText('hello');
  Str := TStringKit.DuplicateText(Str, 2);
  WriteLn(Str);  // 'ollehollo'
  
  // Padding and alignment
  Str := TStringKit.PadCenter('title', 20, '*');
  WriteLn(Str);  // '*******title********'
  
  // Whitespace handling
  Str := '  too   many    spaces  ';
  Str := TStringKit.Trim(TStringKit.CollapseWhitespace(Str));
  WriteLn(Str);  // 'too many spaces'
  
  // Pattern matching and extraction
  Str := 'The year is 2024';
  if TStringKit.MatchesPattern(Str, '\d+') then
  begin
    // Extract matches with position information
    Matches := TStringKit.ExtractMatches(Str, '\d+');
    WriteLn('Found number at position ', Matches[0].Position);
    WriteLn('Number is: ', Matches[0].Text);
    
    // Extract just the matched strings
    Words := TStringKit.ExtractAllMatches(Str, '\d+');
    WriteLn(Words[0]);  // Prints: 2024
  end;
  
  // Word operations
  Str := 'The quick brown fox';
  Words := TStringKit.GetWords(Str);
  for Word in Words do
    WriteLn(Word);  // Prints each word on new line
  
  // String tests
  if TStringKit.Contains('Hello World', 'World') then
    WriteLn('Found!');
  if TStringKit.StartsWith('Hello', 'He') then
    WriteLn('Starts with He');
  if TStringKit.EndsWith('World', 'ld') then
    WriteLn('Ends with ld');
end;
```

### 📅 DateTime Operations
```pascal
var
  CurrentDate, NextMonth: TDateTime;
  Period: TDateSpan;
  Interval: TInterval;
  FormattedDate: string;
begin
  // Get current date/time
  CurrentDate := TDateTimeKit.GetNow;
  
  // Basic formatting
  FormattedDate := TDateTimeKit.GetAsString(CurrentDate, 'yyyy-mm-dd hh:nn:ss');
  WriteLn(FormattedDate);
  
  // Parse dates with specific formats
  CurrentDate := TDateTimeKit.FromString('2024-03-15 14:30:00', 'yyyy-mm-dd hh:nn:ss');
  CurrentDate := TDateTimeKit.YMD('2024-03-15');  // Year-Month-Day
  CurrentDate := TDateTimeKit.MDY('03-15-2024');  // Month-Day-Year
  CurrentDate := TDateTimeKit.DMY('15-03-2024');  // Day-Month-Year
  CurrentDate := TDateTimeKit.YQ('2024-1');       // Year-Quarter
  
  // Date manipulations
  NextMonth := TDateTimeKit.AddMonths(CurrentDate, 1);
  NextMonth := TDateTimeKit.SetHour(NextMonth, 9);
  NextMonth := TDateTimeKit.SetMinute(NextMonth, 0);
  
  // Period operations
  Period := TDateTimeKit.CreatePeriod(1, 2, 3);  // 1 year, 2 months, 3 days
  NextMonth := TDateTimeKit.AddSpan(CurrentDate, Period);
  
  // Interval operations
  Interval := TDateTimeKit.CreateInterval(CurrentDate, NextMonth);
  if TDateTimeKit.IsWithinInterval(TDateTimeKit.GetNow, Interval) then
    WriteLn('Current date is within interval');
    
  // Business day operations
  if TDateTimeKit.IsBusinessDay(CurrentDate) then
    WriteLn('Is a business day');
  CurrentDate := TDateTimeKit.NextBusinessDay(CurrentDate);
  CurrentDate := TDateTimeKit.AddBusinessDays(CurrentDate, 5);
  
  // Period boundaries
  CurrentDate := TDateTimeKit.StartOfYear(CurrentDate);
  CurrentDate := TDateTimeKit.EndOfMonth(CurrentDate);
  CurrentDate := TDateTimeKit.StartOfWeek(CurrentDate);
  
  // Date rounding
  CurrentDate := TDateTimeKit.RoundDate(CurrentDate, TDateUnit.duHour);
  CurrentDate := TDateTimeKit.FloorDate(CurrentDate, TDateUnit.duDay);
  CurrentDate := TDateTimeKit.CeilingDate(CurrentDate, TDateUnit.duMonth);
  
  // Calendar calculations
  WriteLn('ISO Year: ', TDateTimeKit.GetISOYear(CurrentDate));
  WriteLn('ISO Week: ', TDateTimeKit.GetISOWeek(CurrentDate));
  WriteLn('Epi Year: ', TDateTimeKit.GetEpiYear(CurrentDate));
  WriteLn('Epi Week: ', TDateTimeKit.GetEpiWeek(CurrentDate));
  
  // Timezone operations
  WriteLn('System timezone: ', TDateTimeKit.GetSystemTimeZone);
  CurrentDate := TDateTimeKit.WithTimeZone(CurrentDate, 'UTC');
  
  // Special operations
  CurrentDate := TDateTimeKit.RollbackMonth(CurrentDate);  // Last day of previous month
  CurrentDate := TDateTimeKit.RollForwardMonth(CurrentDate);  // First day of next month
  WriteLn('As decimal year: ', TDateTimeKit.GetDecimalDate(CurrentDate):0:4);
end;
```

### 🗂️ FileSystem Operations
```pascal
var
  Content: string;
  Files: TSearchResults;
  Attrs: TFileAttributes;
begin
  // File operations
  Content := TFileKit.ReadFile('input.txt');
  TFileKit.WriteFile('output.txt', Content + 'new content');
  
  // Directory operations
  TFileKit.CreateDirectory('new_dir');
  TFileKit.EnsureDirectory('path/to/file.txt');
  
  // Search operations
  Files := TFileKit.SearchFiles('.', '*.txt', True);  // True for recursive
  try
    for FileItem in Files do
    begin
      WriteLn('File: ', FileItem.FileName);
      WriteLn('Full path: ', FileItem.FullPath);
      WriteLn('Size: ', FileItem.Size);
      WriteLn('Last modified: ', DateTimeToStr(FileItem.LastModified));
      WriteLn('Is directory: ', FileItem.IsDirectory);
    end;
  finally
    SetLength(Files, 0);  // Clean up search results
  end;
  
  // File information
  if TFileKit.Exists('file.txt') then
  begin
    WriteLn('Size: ', TFileKit.GetSize('file.txt'));
    WriteLn('Created: ', DateTimeToStr(TFileKit.GetCreationTime('file.txt')));
  end;
  
  // Directory listing
  var
    Files: TStringArray;
    Dirs: TStringArray;
  begin
    // List files in current directory (non-recursive)
    Files := TFileKit.ListFiles('.', False);
    WriteLn('Files in current directory:');
    for File in Files do
      WriteLn('  ', File);
      
    // List files recursively
    Files := TFileKit.ListFiles('.', True);
    WriteLn('Files in current directory and subdirectories:');
    for File in Files do
      WriteLn('  ', File);
      
    // List directories (non-recursive)
    Dirs := TFileKit.ListDirectories('.', False);
    WriteLn('Subdirectories:');
    for Dir in Dirs do
      WriteLn('  ', Dir);
      
    // List directories recursively
    Dirs := TFileKit.ListDirectories('.', True);
    WriteLn('All directories recursively:');
    for Dir in Dirs do
      WriteLn('  ', Dir);
  end;
  
  // Directory and file listing
  Files := TFileKit.ListFiles(Path);                                   // List all files
  Files := TFileKit.ListFiles(Path, '*.txt');                          // List text files only
  Files := TFileKit.ListFiles(Path, '*', True);                        // List all files recursively
  Files := TFileKit.ListFiles(Path, '*.txt', True);                    // List text files recursively
  Files := TFileKit.ListFiles(Path, '*', False, fsName);               // List files sorted by name
  Files := TFileKit.ListFiles(Path, '*', False, fsDateDesc);           // List files newest first
  Files := TFileKit.ListFiles(Path, '*.txt', True, fsSize);            // List text files by size recursively

  Dirs := TFileKit.ListDirectories(Path);                              // List all directories
  Dirs := TFileKit.ListDirectories(Path, 'test*');                     // List directories starting with 'test'
  Dirs := TFileKit.ListDirectories(Path, '*', True);                   // List all directories recursively
  Dirs := TFileKit.ListDirectories(Path, '*', False, fsName);          // List directories sorted by name
  Dirs := TFileKit.ListDirectories(Path, '*', True, fsDateDesc);       // List directories newest first
  
  // File attributes
  Attrs := TFileKit.GetAttributes('file.txt');
  WriteLn('Read-only: ', Attrs.ReadOnly);
  WriteLn('Hidden: ', Attrs.Hidden);
  WriteLn('System: ', Attrs.System);
  WriteLn('Directory: ', Attrs.Directory);
  WriteLn('Archive: ', Attrs.Archive);

  // Symbolic link operations
  TFileKit.CreateSymLink('target.txt', 'link.txt');         // Create file symlink
  TFileKit.CreateSymLink('target_dir', 'link_dir', True);   // Create directory symlink
  TFileKit.DeleteSymLink('link.txt');                       // Delete symlink
  Path := TFileKit.ResolveSymLink('link.txt');              // Get target path
  if TFileKit.IsSymLink('link.txt') then ...                // Check if path is symlink

  // Note: On Windows, creating symlinks requires Administrator privileges or Developer Mode
  // On Unix/Linux, regular users can create symlinks in their own directories
end;
```

## 🧪 Unit Testing

To run the unit tests,

1. Open the `tests/TestRunner.lpi` file in Lazarus.
2. Compile.
3. In your terminal, 

```bash
$ ./tests/TestRunner.exe -a --format=plain
```

It may take a few seconds to run.


## 📚 Examples

You can find complete examples in the `examples` directory:

1. `examples/DateTimeExample` - Demonstrates comprehensive date/time operations including:
   - ⏰ Basic date/time parsing and formatting
   - 📊 Period and interval operations
   - 📆 Business day calculations
   - 🗓️ Calendar operations (ISO and Epidemiological weeks)
   - 🌐 Timezone handling
   - 🔄 Date rounding and special operations

2. `examples/FileKitExample` - Shows file system operations including:
   - 📄 Basic file reading/writing
   - 📁 Directory creation and manipulation 
   - 🔍 File searching and listing
   - 🛣️ Path operations
   - 🏷️ File attributes
   - 📝 Temporary file handling
   - 📋 Text file operations

3. `examples/StringKitExample` - Demonstrates string manipulation features:
   - 🔠 Case conversion and comparison
   - ✂️ Substring operations
   - 🔍 Pattern matching and replacement
   - 🔄 String splitting and joining
   - ⌨️ Whitespace handling
   - ✅ String validation
   - 🔄 Text transformation

## 🤝 Contributing
Contributions are welcome! Please feel free to submit a Pull Request. For major changes, please open an issue first to discuss what you would like to change.

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## 📝 License
This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.

## 🙏 Acknowledgments
Inspired by modern CLI frameworks
Built with Free Pascal and Lazarus IDE

## 📞 Contact
Your Name - ikelaiah

Project Link: https://github.com/ikelaiah/tidykit-fp

## 📋 Cheat Sheet

### File System Operations

```pascal
// Basic file operations
Content := TFileKit.ReadFile('input.txt');                   // Read entire file
TFileKit.WriteFile('output.txt', 'content');                 // Write to file
TFileKit.AppendFile('log.txt', 'new line');                  // Append to file
TFileKit.DeleteFile('temp.txt');                             // Delete file
TFileKit.CopyFile('source.txt', 'dest.txt');                 // Copy file
TFileKit.MoveFile('old.txt', 'new.txt');                     // Move/rename file

// Directory operations
TFileKit.CreateDirectory('new_dir');                         // Create directory
TFileKit.DeleteDirectory('old_dir', True);                   // Delete directory (True = recursive)
TFileKit.EnsureDirectory('path/to/file.txt');                // Create all parent directories

// File listing
Files := TFileKit.ListFiles('.', '*', False);                // List files in current dir
Files := TFileKit.ListFiles('.', '*', True);                 // List files recursively
Files := TFileKit.ListFiles('.', '*.txt');                   // List only .txt files
Files := TFileKit.ListFiles('.', '*', False, fsName);        // Sort by name (ascending)
Files := TFileKit.ListFiles('.', '*', False, fsNameDesc);    // Sort by name (descending)
Files := TFileKit.ListFiles('.', '*', False, fsDate);        // Sort by date (ascending)
Files := TFileKit.ListFiles('.', '*', False, fsDateDesc);    // Sort by date (descending)
Files := TFileKit.ListFiles('.', '*', False, fsSize);        // Sort by size (ascending)
Files := TFileKit.ListFiles('.', '*', False, fsSizeDesc);    // Sort by size (descending)

// Directory listing
Dirs := TFileKit.ListDirectories('.', '*', False);             // List directories in current dir
Dirs := TFileKit.ListDirectories('.', '*', True);              // List directories recursively
Dirs := TFileKit.ListDirectories('.', 'test_*');               // List dirs matching pattern
Dirs := TFileKit.ListDirectories('.', '*', False, fsName);     // Sort by name (ascending)
Dirs := TFileKit.ListDirectories('.', '*', False, fsNameDesc); // Sort by name (descending)
Dirs := TFileKit.ListDirectories('.', '*', False, fsDate);     // Sort by date (ascending)
Dirs := TFileKit.ListDirectories('.', '*', False, fsDateDesc); // Sort by date (descending)

// Path operations
Path := TFileKit.GetFileName('path/to/file.txt');         // Returns 'file.txt'
Path := TFileKit.GetFileNameWithoutExt('file.txt');       // Returns 'file'
Path := TFileKit.GetDirectory('path/to/file.txt');        // Returns 'path/to'
Path := TFileKit.GetExtension('file.txt');                // Returns '.txt'
Path := TFileKit.GetParentDir('path/to/file.txt');        // Returns 'path'
Path := TFileKit.CombinePaths('path', 'file.txt');        // Combine paths
Path := TFileKit.NormalizePath('path/./to/../file.txt');  // Normalize path

// File information
if TFileKit.Exists('file.txt') then ...                   // Check file exists
if TFileKit.DirectoryExists('dir') then ...               // Check directory exists
Size := TFileKit.GetSize('file.txt');                     // Get file size
Time := TFileKit.GetCreationTime('file.txt');             // Get creation time
Time := TFileKit.GetLastAccessTime('file.txt');           // Get last access time
Time := TFileKit.GetLastWriteTime('file.txt');            // Get last write time
Attrs := TFileKit.GetAttributes('file.txt');              // Get file attributes
if TFileKit.IsTextFile('file.txt') then ...               // Check if text file
Encoding := TFileKit.GetFileEncoding('file.txt');         // Get file encoding

// Search operations
Results := TFileKit.SearchFiles('.', '*.txt', True);      // Search files recursively
File := TFileKit.FindLastModifiedFile('.', '*.txt');      // Find newest file
File := TFileKit.FindFirstModifiedFile('.', '*.txt');     // Find oldest file
File := TFileKit.FindLargestFile('.', '*.txt');           // Find largest file
File := TFileKit.FindSmallestFile('.', '*.txt');          // Find smallest file

// System directories
Dir := TFileKit.GetUserDir;                                // Get user directory
Dir := TFileKit.GetCurrentDir;                             // Get current directory
Dir := TFileKit.GetTempDir;                                // Get temp directory

// Temporary files
TempFile := TFileKit.CreateTempFile('prefix_');           // Create temp file
TempDir := TFileKit.CreateTempDirectory('prefix_');       // Create temp directory

// Symbolic link operations
TFileKit.CreateSymLink('target.txt', 'link.txt');         // Create file symlink
TFileKit.CreateSymLink('target_dir', 'link_dir', True);   // Create directory symlink
TFileKit.DeleteSymLink('link.txt');                       // Delete symlink
Path := TFileKit.ResolveSymLink('link.txt');              // Get target path
if TFileKit.IsSymLink('link.txt') then ...                // Check if path is symlink

// Note: On Windows, creating symlinks requires Administrator privileges or Developer Mode
// On Unix/Linux, regular users can create symlinks in their own directories
```

### String operations

```pascal
// Basic string operations
Str := TStringKit.Trim(Text);                     // Trim whitespace
Str := TStringKit.TrimLeft(Text);                 // Trim left whitespace
Str := TStringKit.TrimRight(Text);                // Trim right whitespace
Str := TStringKit.ToLower(Text);                  // Convert to lowercase
Str := TStringKit.ToUpper(Text);                  // Convert to uppercase

// Advanced string operations
Str := TStringKit.PadCenter(Text, Width, Char);   // Center pad with character
Str := TStringKit.PadLeft(Text, Width, Char);     // Left pad with character
Str := TStringKit.PadRight(Text, Width, Char);    // Right pad with character

// Whitespace handling
Str := TStringKit.CollapseWhitespace(Text);       // Collapse multiple spaces
Str := TStringKit.RemoveWhitespace(Text);         // Remove all whitespace

// Text manipulation
Str := TStringKit.DuplicateText(Text, Count);     // Duplicate text
Str := TStringKit.ReverseText(Text);              // Reverse text
Str := TStringKit.CapitalizeText(Text);           // Capitalize words
Str := TStringKit.ReplaceText(Text, Old, New);    // Replace text

// Pattern matching and extraction
Matches := TStringKit.ExtractMatches(Text, Pattern);      // Extract regex matches with positions
Words := TStringKit.ExtractAllMatches(Text, Pattern);     // Extract regex matches as strings
if TStringKit.MatchesPattern(Text, Pattern) then          // Check regex pattern
Str := TStringKit.ReplaceRegEx(Text, Pattern, Replace);   // Replace using regex

// Word operations
Words := TStringKit.GetWords(Text);               // Split into words
Count := TStringKit.CountSubString(Text, SubStr); // Count occurrences

// String tests
if TStringKit.Contains(Text, SubStr) then         // Check substring
if TStringKit.StartsWith(Text, Prefix) then       // Check prefix
if TStringKit.EndsWith(Text, Suffix) then         // Check suffix
if TStringKit.IsEmpty(Text) then                  // Check if empty
Length := TStringKit.GetLength(Text);             // Get string length

// Substring operations
Str := TStringKit.SubString(Text, Start, Length); // Get substring
Str := TStringKit.LeftStr(Text, Length);          // Get left part
Str := TStringKit.RightStr(Text, Length);         // Get right part
```

### DateTime Operations

#### Basic Operations
```pascal
// Get current date/time
Now := TDateTimeKit.GetNow;                // Current date and time
Today := TDateTimeKit.GetToday;            // Current date at midnight

// Parse date strings
Date1 := TDateTimeKit.FromString('2024-01-15');                // System format
Date2 := TDateTimeKit.FromString('15/01/2024', 'dd/mm/yyyy');  // Custom format

// Format dates
Str1 := TDateTimeKit.GetAsString(Now);                        // System format
Str2 := TDateTimeKit.GetAsString(Now, 'yyyy-mm-dd hh:nn:ss'); // Custom format

// Parse with specific formats
Date3 := TDateTimeKit.YMD('2024-01-15');    // Year-Month-Day
Date4 := TDateTimeKit.MDY('01-15-2024');    // Month-Day-Year
Date5 := TDateTimeKit.DMY('15-01-2024');    // Day-Month-Year
Date6 := TDateTimeKit.YQ('2024-1');         // Year-Quarter
Date7 := TDateTimeKit.DateDecimal(2024.5);  // Decimal year
```

#### Component Access
```pascal
// Basic components
Year := TDateTimeKit.GetYear(Now);        // e.g., 2024
Month := TDateTimeKit.GetMonth(Now);      // 1-12
Day := TDateTimeKit.GetDay(Now);          // 1-31
Hour := TDateTimeKit.GetHour(Now);        // 0-23
Minute := TDateTimeKit.GetMinute(Now);    // 0-59
Second := TDateTimeKit.GetSecond(Now);    // 0-59
MS := TDateTimeKit.GetMillisecond(Now);   // 0-999

// Calendar components
DOW := TDateTimeKit.GetDayOfWeek(Now);    // 1=Sunday to 7=Saturday
DOY := TDateTimeKit.GetDayOfYear(Now);    // 1-366
Quarter := TDateTimeKit.GetQuarter(Now);   // 1-4
Semester := TDateTimeKit.GetSemester(Now); // 1-2

// ISO calendar
ISOYear := TDateTimeKit.GetISOYear(Now);   // ISO-8601 year
ISOWeek := TDateTimeKit.GetISOWeek(Now);   // ISO-8601 week (1-53)

// Epidemiological calendar
EpiYear := TDateTimeKit.GetEpiYear(Now);   // Epi year
EpiWeek := TDateTimeKit.GetEpiWeek(Now);   // Epi week (1-53)

// Time of day
IsAM := TDateTimeKit.IsAM(Now);            // Before noon
IsPM := TDateTimeKit.IsPM(Now);            // After noon
```

#### Component Modification
```pascal
// Set components (returns new TDateTime)
NewDate := TDateTimeKit.SetYear(Now, 2025);
NewDate := TDateTimeKit.SetMonth(Now, 6);
NewDate := TDateTimeKit.SetDay(Now, 15);
NewDate := TDateTimeKit.SetHour(Now, 14);
NewDate := TDateTimeKit.SetMinute(Now, 30);
NewDate := TDateTimeKit.SetSecond(Now, 45);
NewDate := TDateTimeKit.SetMilliSecond(Now, 500);
```

#### Date Arithmetic
```pascal
// Add/subtract time units
NewDate := TDateTimeKit.AddYears(Now, 1);     // Add 1 year
NewDate := TDateTimeKit.AddMonths(Now, -2);   // Subtract 2 months
NewDate := TDateTimeKit.AddDays(Now, 7);      // Add 7 days
NewDate := TDateTimeKit.AddHours(Now, 12);    // Add 12 hours
NewDate := TDateTimeKit.AddMinutes(Now, 30);  // Add 30 minutes
NewDate := TDateTimeKit.AddSeconds(Now, -15); // Subtract 15 seconds

// Business day operations
NewDate := TDateTimeKit.AddBusinessDays(Now, 5);   // Add 5 business days
NextBDay := TDateTimeKit.NextBusinessDay(Now);     // Next business day
PrevBDay := TDateTimeKit.PreviousBusinessDay(Now); // Previous business day
IsWorkDay := TDateTimeKit.IsBusinessDay(Now);      // Check if business day

// Month rolling
NewDate := TDateTimeKit.RollbackMonth(Now);     // Last day of previous month
NewDate := TDateTimeKit.RollForwardMonth(Now);  // First day of next month
```

#### Period Operations
```pascal
// Create periods and durations
Period := TDateTimeKit.CreatePeriod(1, 2, 3);     // 1 year, 2 months, 3 days
Duration := TDateTimeKit.CreateDuration(0, 0, 1); // 1 day fixed duration

// Add/subtract periods
NewDate := TDateTimeKit.AddSpan(Now, Period);
NewDate := TDateTimeKit.SubtractSpan(Now, Period);

// Calculate span between dates
Span := TDateTimeKit.SpanBetween(Date1, Date2, dskPeriod);    // As period
Span := TDateTimeKit.SpanBetween(Date1, Date2, dskDuration);  // As duration

// Convert periods
Seconds := TDateTimeKit.PeriodToSeconds(Period);
Period := TDateTimeKit.SecondsToPeriod(Seconds);
Period := TDateTimeKit.StandardizePeriod(Period);  // Normalize units
```

#### Interval Operations
```pascal
// Create and manipulate intervals
Interval := TDateTimeKit.CreateInterval(StartDate, EndDate);  // Create interval
if TDateTimeKit.IsWithinInterval(TestDate, Interval) then     // Test if date in interval
if TDateTimeKit.IntervalsOverlap(Interval1, Interval2) then   // Test if intervals overlap

// Interval calculations
Length := TDateTimeKit.IntervalLength(Interval);              // Get interval length
Gap := TDateTimeKit.IntervalGap(Interval1, Interval2);        // Get gap between intervals
Diff := TDateTimeKit.IntervalSetdiff(Interval1, Interval2);   // Set difference
Union := TDateTimeKit.IntervalUnion(Interval1, Interval2);    // Union of intervals
Inter := TDateTimeKit.IntervalIntersection(Interval1, Interval2); // Intersection

// Interval alignment
if TDateTimeKit.IntervalAlign(Interval1, Interval2) then      // Check if intervals align
```

#### Date Comparison
```pascal
// Compare dates
if TDateTimeKit.IsBefore(Date1, Date2) then      // Date1 < Date2
if TDateTimeKit.IsAfter(Date1, Date2) then       // Date1 > Date2
if TDateTimeKit.IsSameDay(Date1, Date2) then     // Same calendar day
if TDateTimeKit.IsSameMonth(Date1, Date2) then   // Same month and year
if TDateTimeKit.IsSameYear(Date1, Date2) then    // Same year
```

#### Period Boundaries
```pascal
// Start of period
Start := TDateTimeKit.StartOfYear(Now);    // First moment of year
Start := TDateTimeKit.StartOfMonth(Now);   // First moment of month
Start := TDateTimeKit.StartOfWeek(Now);    // First moment of week
Start := TDateTimeKit.StartOfDay(Now);     // First moment of day
Start := TDateTimeKit.StartOfHour(Now);    // First moment of hour

// End of period
End := TDateTimeKit.EndOfYear(Now);        // Last moment of year
End := TDateTimeKit.EndOfMonth(Now);       // Last moment of month
End := TDateTimeKit.EndOfWeek(Now);        // Last moment of week
End := TDateTimeKit.EndOfDay(Now);         // Last moment of day
End := TDateTimeKit.EndOfHour(Now);        // Last moment of hour
```

#### Date Rounding
```pascal
// Round dates to nearest unit
Round := TDateTimeKit.RoundDate(Now, duMonth);    // Round to nearest month
Floor := TDateTimeKit.FloorDate(Now, duMonth);    // Round down to month start
Ceil := TDateTimeKit.CeilingDate(Now, duMonth);   // Round up to month end

// Available units: duSecond, duMinute, duHour, duDay, duWeek,
// duMonth, duBiMonth, duQuarter, duSeason, duHalfYear, duYear
```

#### Timezone Operations
```pascal
// Get timezone information
TZ := TDateTimeKit.GetTimeZone(Now);           // Current timezone info
SystemTZ := TDateTimeKit.GetSystemTimeZone;     // System timezone name
TZNames := TDateTimeKit.GetTimeZoneNames;       // Available timezone names

// Convert between timezones
UTC := TDateTimeKit.WithTimeZone(Now, 'UTC');   // Convert to UTC
Local := TDateTimeKit.ForceTimeZone(Now, 'EST'); // Force timezone
```