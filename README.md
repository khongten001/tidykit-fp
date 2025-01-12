# TidyKit

A comprehensive toolkit for Free Pascal that provides an easy-to-use, unified interface for common operations. TidyKit brings modern functionality to Free Pascal while maintaining traditional Pascal programming style.

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

### Using with MSBuild/Delphi

Add the `src` directory to your project's search path in the project options.

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

## FileSystem Operations

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
TFileKit.CreateDirectory(Path);  // Create directory
TFileKit.DeleteDirectory(Path);  // Delete directory
TFileKit.EnsureDirectory(Path);  // Ensure parent exists

// Path operations
NewPath := TFileKit.ChangeExtension(Path, Ext); // Change extension
Name := TFileKit.GetFileName(Path);             // Get file name
Dir := TFileKit.GetDirectory(Path);             // Get directory path
Ext := TFileKit.GetExtension(Path);             // Get file extension

// Search operations
Files := TFileKit.SearchFiles(Path, Pattern);    // Search files
Files := TFileKit.SearchFilesIn(Dir, Pattern);   // Search in directory
NewestFile := TFileKit.FindNewestFile(Path);     // Find newest file
OldestFile := TFileKit.FindOldestFile(Path);     // Find oldest file
LargestFile := TFileKit.FindLargestFile(Path);   // Find largest file
SmallestFile := TFileKit.FindSmallestFile(Path); // Find smallest file

// File information
if TFileKit.Exists(Path) then            // Check if exists
if TFileKit.IsDirectory(Path) then       // Check if directory
Size := TFileKit.GetSize(Path);          // Get file size
Time := TFileKit.GetCreationTime(Path);  // Get creation time
Time := TFileKit.GetLastWriteTime(Path); // Get last write time
Attrs := TFileKit.GetAttributes(Path);   // Get file attributes
```

## String Operations

TidyKit provides a rich set of string manipulation functions:

```pascal
// Basic transformations
OutStr := TStringKit.Trim(Text);      // Remove whitespace
OutStr := TStringKit.TrimLeft(Text);  // Remove left whitespace
OutStr := TStringKit.TrimRight(Text); // Remove right whitespace
OutStr := TStringKit.ToUpper(Text);   // Convert to uppercase
OutStr := TStringKit.ToLower(Text);   // Convert to lowercase

// Advanced transformations
OutStr := TStringKit.ReverseText(Text);        // Reverse string
OutStr := TStringKit.DuplicateText(Text, 2);   // Repeat string
OutStr := TStringKit.PadLeft(Text, 10, '*');   // Left padding
OutStr := TStringKit.PadRight(Text, 10, '*');  // Right padding
OutStr := TStringKit.PadCenter(Text, 10, '*'); // Center text

// Pattern matching
if TStringKit.MatchesPattern(Text, Pattern) then   // Check pattern
if TStringKit.Contains(Text, SubStr) then          // Check contains
if TStringKit.StartsWith(Text, Prefix) then        // Check starts with
if TStringKit.EndsWith(Text, Suffix) then          // Check ends with

// Extraction and splitting
Words := TStringKit.GetWords(Text);               // Get words
OutStr := TStringKit.SubString(Text, Start, Len); // Get substring
OutStr := TStringKit.LeftStr(Text, Len);          // Get left part
OutStr := TStringKit.RightStr(Text, Len);         // Get right part
```

## DateTime Operations

TidyKit provides comprehensive date and time manipulation functions:

```pascal
// Current date/time
Now := TDateTimeKit.GetNow;       // Current date and time
Today := TDateTimeKit.GetToday;   // Current date at midnight

// Date parts
Year := TDateTimeKit.GetYear(Date);     // Get year
Month := TDateTimeKit.GetMonth(Date);   // Get month
Day := TDateTimeKit.GetDay(Date);       // Get day
Hour := TDateTimeKit.GetHour(Date);     // Get hour
Minute := TDateTimeKit.GetMinute(Date); // Get minute

// Date manipulations
NewDate := TDateTimeKit.AddYears(Date, 1);  // Add years
NewDate := TDateTimeKit.AddMonths(Date, 1); // Add months
NewDate := TDateTimeKit.AddDays(Date, 1);   // Add days
NewDate := TDateTimeKit.AddHours(Date, 1);  // Add hours

// Date truncation
NewDate := TDateTimeKit.StartOfYear(Date);  // Start of year
NewDate := TDateTimeKit.StartOfMonth(Date); // Start of month
NewDate := TDateTimeKit.StartOfDay(Date);   // Start of day
NewDate := TDateTimeKit.EndOfYear(Date);    // End of year

// Business days
if TDateTimeKit.IsBusinessDay(Date) then           // Check business day
NewDate := TDateTimeKit.NextBusinessDay(Date);     // Next business day
NewDate := TDateTimeKit.PreviousBusinessDay(Date); // Previous business day
NewDate := TDateTimeKit.AddBusinessDays(Date, 5);  // Add business days

// Comparisons
if TDateTimeKit.IsBefore(Date1, Date2) then  // Compare dates
if TDateTimeKit.IsAfter(Date1, Date2) then   // Compare dates
if TDateTimeKit.IsSameDay(Date1, Date2) then // Check same day
```

## Documentation

[Detailed documentation coming soon]

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

MIT License 