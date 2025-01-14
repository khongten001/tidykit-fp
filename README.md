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

// Search operations (recursive search is off by default)
Files := TFileKit.SearchFiles(Path, Pattern);              // Search files non-recursively
Files := TFileKit.SearchFiles(Path, Pattern, True);        // Search files recursively
Files := TFileKit.SearchFilesIn(Dir, Pattern);            // Search in directory non-recursively
Files := TFileKit.SearchFilesIn(Dir, Pattern, True);      // Search in directory recursively
LastModified := TFileKit.FindLastModifiedFile(Path);      // Find most recently modified file
FirstModified := TFileKit.FindFirstModifiedFile(Path);    // Find first modified file
LargestFile := TFileKit.FindLargestFile(Path);           // Find largest file
SmallestFile := TFileKit.FindSmallestFile(Path);         // Find smallest file

// To enable recursive search for any operation, add True as the last parameter:
LastModified := TFileKit.FindLastModifiedFile(Path, Pattern, True);  // Recursive
FirstModified := TFileKit.FindFirstModifiedFile(Path, Pattern, True);  // Recursive
LargestFile := TFileKit.FindLargestFile(Path, Pattern, True);// Recursive
SmallestFile := TFileKit.FindSmallestFile(Path, Pattern, True); // Recursive

// File information
if TFileKit.Exists(Path) then            // Check if exists
if TFileKit.IsDirectory(Path) then       // Check if directory
Size := TFileKit.GetSize(Path);          // Get file size
Time := TFileKit.GetCreationTime(Path);  // Get creation time
Time := TFileKit.GetLastWriteTime(Path); // Get last write time
Attrs := TFileKit.GetAttributes(Path);   // Get file attributes
```