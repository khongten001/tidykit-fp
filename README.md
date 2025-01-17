# TidyKit

A comprehensive toolkit for Free Pascal that provides an easy-to-use, unified interface for common operations. TidyKit brings modern functionality to Free Pascal while maintaining traditional Pascal programming style.


> [!WARNING]
> This library is currently in early development stage. The API is not stable and may undergo breaking changes between versions. Use with caution in production environments.

## TODO

- [ ] Add comprehensive documentation
  - [ ] Add detailed API reference
  - [ ] Add more code examples
  - [ ] Add best practices guide
  - [ ] Add troubleshooting guide
  - [ ] Add contribution guidelines
  
- [ ] Expand test coverage
  - [ ] Add more unit tests
  - [ ] Add edge case tests
  
- [ ] Add more examples
  - [ ] Add real-world usage examples
  - [ ] Add cookbook with common patterns
  - [ ] Add sample applications


## Features

- 🗂️ FileSystem Operations: Modern FS operations inspired by Node.js fs module
  - File reading/writing
  - Directory creation/deletion
  - File/directory listing (recursive and non-recursive)
  - File searching and attributes
  - Path manipulation and normalization
- 📝 String Operations: Modern string handling with comprehensive methods
- 📅 DateTime Operations: Modern date and time manipulation
- 🎯 FPC 3.2.2 Compatible: No inline var, anonymous functions, or lambda
- 💻 Cross-Platform: Works on Windows, Linux, macOS, and FreeBSD
- 🚀 Static Functions: No instance management or memory leaks, just call and use
- 🔒 Memory Safe: Proper resource management with no memory leaks

## Platform Compatibility

TidyKit is designed to be platform-independent and works across:
- Windows (32/64-bit)
- Linux
- macOS
- FreeBSD
- Any platform supported by Free Pascal

All operations automatically handle platform-specific differences:
- File paths (directory separators, drive letters)
- Line endings (CR, LF, CRLF)
- File system permissions
- Date/time handling (timezones, DST)

## Platform Testing Status

⚠️ **Note**: Current testing has been performed on:
- ✅ Windows (32/64-bit)

While the library is designed to be cross-platform, the following platforms have not been tested yet:
- ⚠️ Linux
- ⚠️ macOS
- ⚠️ FreeBSD

Contributions for testing and validation on other platforms are welcome!

## Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/iwan/TidyKit.git
   ```

2. Add the `src` directory to your project's unit search path:
   - In Lazarus: Project -> Project Options -> Compiler Options -> Paths -> Other unit files
   - In FPC: Use `-Fu` command line option or add to `fpc.cfg`

3. Add `TidyKit` to your uses clause:
   ```pascal
   program MyProject;
   
   {$mode objfpc}{$H+}{$J-}
   
   uses
     {$IFDEF UNIX}
     cthreads,
     {$ENDIF}
     Classes, SysUtils,
     TidyKit;  // Add this line to use TidyKit
   
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

### Dependencies

TidyKit requires:
- Free Pascal Compiler (FPC) 3.2.2 or later
- No external dependencies required

### Compatibility Notes

- Windows: Fully tested and supported
- Linux/macOS/FreeBSD: Designed to work but needs testing
- Unicode: Full UTF-8 support
- Thread Safety: All operations are thread-safe

## Quick Start

```pascal
uses
  TidyKit;

// String operations example
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

// DateTime operations example
var
  CurrentDate: TDateTime;
  NextMonth: TDateTime;
  FormattedDate: string;
begin
  // Get current date/time
  CurrentDate := TDateTimeKit.GetNow;
  
  // Date manipulations
  NextMonth := TDateTimeKit.AddMonths(CurrentDate, 1);
  NextMonth := TDateTimeKit.SetHour(NextMonth, 9);
  NextMonth := TDateTimeKit.SetMinute(NextMonth, 0);
  
  // Date parts
  CurrentDate := TDateTimeKit.SetYear(CurrentDate, 2024);
  CurrentDate := TDateTimeKit.SetMonth(CurrentDate, 3);
  CurrentDate := TDateTimeKit.SetDay(CurrentDate, 15);
  
  // Business day operations
  if TDateTimeKit.IsBusinessDay(CurrentDate) then
    WriteLn('Is a business day');
    
  // Next business day
  CurrentDate := TDateTimeKit.NextBusinessDay(CurrentDate);
  
  // Date comparisons
  if TDateTimeKit.IsAfter(CurrentDate, TDateTimeKit.GetNow) then
    WriteLn('Future date');
    
  // Date formatting
  FormattedDate := TDateTimeKit.GetAsString(CurrentDate, 'yyyy-mm-dd hh:nn:ss');
  WriteLn(FormattedDate);
  
  // Period start/end
  CurrentDate := TDateTimeKit.StartOfMonth(CurrentDate);  // Beginning of month
  CurrentDate := TDateTimeKit.EndOfMonth(CurrentDate);    // End of month
end;

// FileSystem operations example
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
  Files := TFileKit.ListFiles(Path);                                    // List all files
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
end;
```

## Cheat Sheet

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

```pascal
// Basic operations
Now := TDateTimeKit.GetNow;                       // Current date/time
Today := TDateTimeKit.GetToday;                   // Current date (time = 00:00:00)
Str := TDateTimeKit.GetAsString(Now, 'yyyy-mm-dd hh:nn:ss');  // Format date/time

// Time spans (two types)
// 1. Periods - Calendar time (respects month/year lengths)
Period := TDateTimeKit.CreatePeriod(1);           // 1 year as calendar time
NextMonth := TDateTimeKit.AddSpan(Now, TDateTimeKit.CreatePeriod(0, 1));  // Add 1 month
// Examples of period behavior:
Jan31 := EncodeDate(2024, 1, 31);
Feb28 := TDateTimeKit.AddSpan(Jan31, TDateTimeKit.CreatePeriod(0, 1));  // 2024-02-29 (leap year)
Feb29_2024 := EncodeDate(2024, 2, 29);
Feb28_2025 := TDateTimeKit.AddSpan(Feb29_2024, TDateTimeKit.CreatePeriod(1));  // 2025-02-28

// 2. Durations - Fixed time (exact number of seconds)
Duration := TDateTimeKit.CreateDuration(0, 0, 1);  // Exactly 24 hours
Tomorrow := TDateTimeKit.AddSpan(Now, Duration);   // Add exactly 24 hours

// Intervals (specific time ranges)
Q1_2024 := TDateTimeKit.CreateInterval(
  EncodeDate(2024, 1, 1),    // Start (inclusive)
  EncodeDate(2024, 4, 1)     // End (exclusive)
);
if TDateTimeKit.IsWithinInterval(Now, Q1_2024) then
  WriteLn('Date is in Q1 2024');

// Calculate spans between dates
Span := TDateTimeKit.SpanBetween(Start, End_, dskPeriod);   // As calendar time
Span := TDateTimeKit.SpanBetween(Start, End_, dskDuration); // As fixed duration

// Date components - getters
Year := TDateTimeKit.GetYear(Now);                // Get year (e.g., 2024)
Month := TDateTimeKit.GetMonth(Now);              // Get month (1-12)
Day := TDateTimeKit.GetDay(Now);                  // Get day (1-31)
DayOfWeek := TDateTimeKit.GetDayOfWeek(Now);     // Get day of week (1=Sunday)
DayOfYear := TDateTimeKit.GetDayOfYear(Now);     // Get day of year (1-366)
Hour := TDateTimeKit.GetHour(Now);               // Get hour (0-23)
Minute := TDateTimeKit.GetMinute(Now);           // Get minute (0-59)
Second := TDateTimeKit.GetSecond(Now);           // Get second (0-59)
MS := TDateTimeKit.GetMillisecond(Now);          // Get millisecond (0-999)

// Date components - setters (create new date, don't modify input)
Date := TDateTimeKit.SetYear(Now, 2024);         // Set year
Date := TDateTimeKit.SetMonth(Now, 1);           // Set month (1-12)
Date := TDateTimeKit.SetDay(Now, 1);             // Set day (1-31)
Time := TDateTimeKit.SetHour(Now, 9);            // Set hour (0-23)
Time := TDateTimeKit.SetMinute(Now, 30);         // Set minute (0-59)
Time := TDateTimeKit.SetSecond(Now, 0);          // Set second (0-59)
Time := TDateTimeKit.SetMillisecond(Now, 0);     // Set millisecond (0-999)

// Period start/end
Date := TDateTimeKit.StartOfYear(Now);           // First moment of year
Date := TDateTimeKit.StartOfMonth(Now);          // First moment of month
Date := TDateTimeKit.StartOfWeek(Now);           // First moment of week
Date := TDateTimeKit.StartOfDay(Now);            // Start of day (00:00:00)
Date := TDateTimeKit.EndOfYear(Now);             // Last moment of year
Date := TDateTimeKit.EndOfMonth(Now);            // Last moment of month
Date := TDateTimeKit.EndOfWeek(Now);             // Last moment of week
Date := TDateTimeKit.EndOfDay(Now);              // End of day (23:59:59.999)

// Comparisons
if TDateTimeKit.IsBefore(Date1, Date2) then      // Check if before
if TDateTimeKit.IsAfter(Date1, Date2) then       // Check if after
if TDateTimeKit.IsSameDay(Date1, Date2) then     // Check if same day
if TDateTimeKit.IsSameMonth(Date1, Date2) then   // Check if same month
if TDateTimeKit.IsSameYear(Date1, Date2) then    // Check if same year

// Business days
if TDateTimeKit.IsBusinessDay(Now) then          // Check if business day
Date := TDateTimeKit.NextBusinessDay(Now);       // Get next business day
Date := TDateTimeKit.AddBusinessDays(Now, 5);    // Add 5 business days
```

#### Important Notes About DateTime Operations

1. **Time Spans**: TidyKit supports two types of time spans:
   - **Periods** (`dskPeriod`): Calendar time that respects month/year lengths
     ```pascal
     // Example: Adding 1 month to January 31st
     Jan31 := EncodeDate(2024, 1, 31);
     Feb29 := TDateTimeKit.AddSpan(Jan31, TDateTimeKit.CreatePeriod(0, 1));
     // Result: February 29th (2024 is leap year)
     ```
   - **Durations** (`dskDuration`): Fixed-length time in seconds
     ```pascal
     // Example: Adding exactly 24 hours
     Now := TDateTimeKit.GetNow;
     Tomorrow := TDateTimeKit.AddSpan(Now, TDateTimeKit.CreateDuration(0, 0, 1));
     ```

2. **Corner Cases**:
   - Month-end transitions are handled automatically:
     ```pascal
     // January 31st + 1 month = February 29th (in 2024)
     // January 31st + 1 month = February 28th (in 2025)
     ```
   - Leap years are handled correctly:
     ```pascal
     // February 29th, 2024 + 1 year = February 28th, 2025
     ```
   - Time components are preserved unless explicitly changed

3. **Intervals**:
   - Start date is inclusive, end date is exclusive
   - Perfect for defining ranges like quarters, months, years:
     ```pascal
     Q1_2024 := TDateTimeKit.CreateInterval(
       EncodeDate(2024, 1, 1),    // January 1st, 2024
       EncodeDate(2024, 4, 1)     // Up to (but not including) April 1st
     );
     ```

4. **Best Practices**:
   - Use `dskPeriod` for calendar calculations (e.g., "add 1 month")
   - Use `dskDuration` for exact time differences (e.g., "exactly 24 hours")
   - Always consider timezone implications when working with dates
   - Remember that all operations return new values, never modify input
