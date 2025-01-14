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

1. Clone this repository
2. Add the `src` directory to your project's unit search path
3. Add `TidyKit` to your uses clause

### Using with FPC/Lazarus

```pascal
program MyProject;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils,
  TidyKit;  // Add this line to use TidyKit
```

## Quick Start

```pascal
uses
  TidyKit;

// String operations example
var
  Str: string;
  Words: TStringArray;
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
    // Extract all numbers from text
    Words := TStringKit.ExtractAllMatches(Str, '\d+');
    WriteLn(Words[0]);  // Prints: 2024
  end;
  
  // Word operations
  Str := 'The quick brown fox';
  Words := TStringKit.GetWords(Str);
  for Word in Words do
    WriteLn(Word);  // Prints each word on new line
  
  // Case-insensitive operations
  if TStringKit.Contains('Hello World', 'HELLO', False) then  // Case-insensitive search
    WriteLn('Found!');
end;

// DateTime operations example
var
  CurrentDate: TDateTime;
  NextMonth: TDateTime;
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
  
  // Easy date comparisons
  if TDateTimeKit.IsAfter(CurrentDate, Now) then
    WriteLn('Future date');
end;

// FileSystem operations example
var
  Content: string;
  Files: TSearchResults;
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
      WriteLn(FileItem.FullPath);
  finally
    SetLength(Files, 0);  // Clean up search results
  end;
  
  // File information
  WriteLn('Size: ', TFileKit.GetSize('file.txt'));
  WriteLn('Last Modified: ', TFileKit.GetLastWriteTime('file.txt'));
  WriteLn('Permissions: ', TFileKit.GetAttributes('file.txt').Permissions);
end;
```

## Cheat Sheet

### FileSystem Operations

TidyKit provides a comprehensive set of filesystem operations:

```pascal
// Basic file operations
Content := TFileKit.ReadFile(Path);  // Read file content
TFileKit.WriteFile(Path, Content);   // Write to file
TFileKit.AppendFile(Path, Content);  // Append to file
TFileKit.DeleteFile(Path);           // Delete file
TFileKit.CopyFile(Source, Dest);     // Copy file
TFileKit.MoveFile(Source, Dest);     // Move/rename file

// Content operations
TFileKit.AppendText(Path, Text);       // Append text
TFileKit.PrependText(Path, Text);      // Prepend text
TFileKit.ReplaceText(Path, Old, New);  // Replace in content

// Directory operations
TFileKit.CreateDirectory(Path);              // Create directory
TFileKit.DeleteDirectory(Path, Recursive);   // Delete directory
TFileKit.EnsureDirectory(Path);              // Ensure parent exists

// Path operations
Name := TFileKit.GetFileName(Path);              // Get file name
Name := TFileKit.GetFileNameWithoutExt(Path);    // Get file name without extension
Dir := TFileKit.GetDirectory(Path);              // Get directory path
Ext := TFileKit.GetExtension(Path);              // Get file extension
Parent := TFileKit.GetParentDir(Path);           // Get parent directory
Combined := TFileKit.CombinePaths(Path1, Path2); // Combine paths
Normalized := TFileKit.NormalizePath(Path);      // Normalize path separators

// Search operations
Files := TFileKit.SearchFiles(Path, Pattern);              // Search files
Files := TFileKit.SearchFiles(Path, Pattern, True);        // Search files recursively
LastModified := TFileKit.FindLastModifiedFile(Path, Pattern);    // Find most recently modified file
FirstModified := TFileKit.FindFirstModifiedFile(Path, Pattern);  // Find first modified file
LargestFile := TFileKit.FindLargestFile(Path, Pattern);          // Find largest file
SmallestFile := TFileKit.FindSmallestFile(Path, Pattern);        // Find smallest file

// To enable recursive search for any operation, add True as the last parameter:
LastModified := TFileKit.FindLastModifiedFile(Path, Pattern, True);   // Recursive
FirstModified := TFileKit.FindFirstModifiedFile(Path, Pattern, True); // Recursive
LargestFile := TFileKit.FindLargestFile(Path, Pattern, True);         // Recursive
SmallestFile := TFileKit.FindSmallestFile(Path, Pattern, True);       // Recursive

// File information
if TFileKit.Exists(Path) then             // Check if exists
if TFileKit.DirectoryExists(Path) then    // Check if directory
Size := TFileKit.GetSize(Path);           // Get file size
Time := TFileKit.GetCreationTime(Path);   // Get creation time
Time := TFileKit.GetLastAccessTime(Path); // Get last access time
Time := TFileKit.GetLastWriteTime(Path);  // Get last write time
Attrs := TFileKit.GetAttributes(Path);    // Get file attributes
IsText := TFileKit.IsTextFile(Path);      // Check if text file
Encoding := TFileKit.GetFileEncoding(Path); // Get file encoding

// Directory information
UserDir := TFileKit.GetUserDir;          // Get user directory
CurDir := TFileKit.GetCurrentDir;        // Get current directory
TempDir := TFileKit.GetTempDir;          // Get temp directory

// File system operations
TempFile := TFileKit.CreateTempFile(Prefix);      // Create temp file
TempDir := TFileKit.CreateTempDirectory(Prefix);  // Create temp directory
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

### DateTime operations

```pascal
// Basic operations
Now := TDateTimeKit.GetNow;                       // Current date/time
Today := TDateTimeKit.GetToday;                   // Current date
DateTime := TDateTimeKit.GetDateTime(Value);       // Convert to datetime
Str := TDateTimeKit.GetAsString(Value, Format);   // Format to string
DateTime := TDateTimeKit.FromString(Str, Format); // Parse from string

// Date parts - getters
Year := TDateTimeKit.GetYear(Value);              // Get year
Month := TDateTimeKit.GetMonth(Value);            // Get month
Day := TDateTimeKit.GetDay(Value);                // Get day
DayOfWeek := TDateTimeKit.GetDayOfWeek(Value);   // Get day of week
DayOfYear := TDateTimeKit.GetDayOfYear(Value);    // Get day of year
Hour := TDateTimeKit.GetHour(Value);              // Get hour
Minute := TDateTimeKit.GetMinute(Value);          // Get minute
Second := TDateTimeKit.GetSecond(Value);          // Get second
MS := TDateTimeKit.GetMillisecond(Value);         // Get millisecond

// Date parts - setters
Date := TDateTimeKit.SetYear(Value, Year);        // Set year
Date := TDateTimeKit.SetMonth(Value, Month);      // Set month
Date := TDateTimeKit.SetDay(Value, Day);          // Set day
Time := TDateTimeKit.SetHour(Value, Hour);        // Set hour
Time := TDateTimeKit.SetMinute(Value, Minute);    // Set minute
Time := TDateTimeKit.SetSecond(Value, Second);    // Set second
Time := TDateTimeKit.SetMillisecond(Value, MS);   // Set millisecond

// Date manipulation
Date := TDateTimeKit.AddYears(Value, Years);      // Add years
Date := TDateTimeKit.AddMonths(Value, Months);    // Add months
Date := TDateTimeKit.AddDays(Value, Days);        // Add days
Time := TDateTimeKit.AddHours(Value, Hours);      // Add hours
Time := TDateTimeKit.AddMinutes(Value, Minutes);  // Add minutes
Time := TDateTimeKit.AddSeconds(Value, Seconds);  // Add seconds

// Period start/end
Date := TDateTimeKit.StartOfYear(Value);          // Start of year
Date := TDateTimeKit.StartOfMonth(Value);         // Start of month
Date := TDateTimeKit.StartOfWeek(Value);          // Start of week
Date := TDateTimeKit.StartOfDay(Value);           // Start of day
Date := TDateTimeKit.StartOfHour(Value);          // Start of hour
Date := TDateTimeKit.EndOfYear(Value);            // End of year
Date := TDateTimeKit.EndOfMonth(Value);           // End of month
Date := TDateTimeKit.EndOfWeek(Value);            // End of week
```
