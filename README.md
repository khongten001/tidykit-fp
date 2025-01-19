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
Date := TDateTimeKit.FromString('2024-01-15');    // Parse date string
Date := TDateTimeKit.GetDateTime(Now);            // Validate/convert to TDateTime

// Parse specific date formats
Date := TDateTimeKit.YMD('2024-01-15');          // Parse YYYY-MM-DD
Date := TDateTimeKit.MDY('01-15-2024');          // Parse MM-DD-YYYY
Date := TDateTimeKit.DMY('15-01-2024');          // Parse DD-MM-YYYY
Date := TDateTimeKit.YQ('2024-1');               // Parse year and quarter
Date := TDateTimeKit.DateDecimal(2024.5);        // Parse decimal year

// Additional component getters
Year := TDateTimeKit.GetISOYear(Now);            // ISO week-numbering year
Week := TDateTimeKit.GetISOWeek(Now);            // ISO week number (1-53)
Year := TDateTimeKit.GetEpiYear(Now);            // Epidemiological year
Week := TDateTimeKit.GetEpiWeek(Now);            // Epidemiological week
Sem := TDateTimeKit.GetSemester(Now);            // Semester (1-2)
Dec := TDateTimeKit.GetDecimalDate(Now);         // Get as decimal year

// Time spans (two types)
// 1. Periods - Calendar time (respects month/year lengths)
Period := TDateTimeKit.CreatePeriod(1);          // 1 year period
Period := TDateTimeKit.CreatePeriod(0, 1);       // 1 month period
Period := TDateTimeKit.CreatePeriod(0, 0, 1);    // 1 day period

// 2. Durations - Fixed time (exact number of seconds)
Duration := TDateTimeKit.CreateDuration(0, 0, 1); // Exactly 24 hours
Duration := TDateTimeKit.CreateDuration(0, 0, 0, 12); // 12 hours

// Time span operations
NextYear := TDateTimeKit.AddSpan(Now, Period);    // Add time span
LastYear := TDateTimeKit.SubtractSpan(Now, Period); // Subtract time span
Span := TDateTimeKit.SpanBetween(Date1, Date2);   // Get span between dates
Secs := TDateTimeKit.PeriodToSeconds(Period);     // Convert period to seconds
Period := TDateTimeKit.SecondsToPeriod(Secs);     // Convert seconds to period
Period := TDateTimeKit.StandardizePeriod(Period);  // Normalize period units

// Intervals (specific time ranges)
Interval := TDateTimeKit.CreateInterval(
  EncodeDate(2024, 1, 1),    // Start (inclusive)
  EncodeDate(2024, 4, 1)     // End (exclusive)
);

// Interval operations
if TDateTimeKit.IsWithinInterval(Now, Interval) then    // Check if date in interval
if TDateTimeKit.IntervalsOverlap(Int1, Int2) then      // Check if intervals overlap
if TDateTimeKit.IntervalAlign(Int1, Int2) then         // Check if intervals adjacent
Span := TDateTimeKit.IntervalGap(Int1, Int2);          // Get gap between intervals
Diff := TDateTimeKit.IntervalSetdiff(Int1, Int2);      // Set difference
Union := TDateTimeKit.IntervalUnion(Int1, Int2);       // Combine intervals
Inter := TDateTimeKit.IntervalIntersection(Int1, Int2); // Get overlap

// Date components - getters
Year := TDateTimeKit.GetYear(Now);               // Get year (e.g., 2024)
Month := TDateTimeKit.GetMonth(Now);             // Get month (1-12)
Day := TDateTimeKit.GetDay(Now);                 // Get day (1-31)
DayOfWeek := TDateTimeKit.GetDayOfWeek(Now);    // Get day of week (1=Sunday)
DayOfYear := TDateTimeKit.GetDayOfYear(Now);    // Get day of year (1-366)
Hour := TDateTimeKit.GetHour(Now);              // Get hour (0-23)
Minute := TDateTimeKit.GetMinute(Now);          // Get minute (0-59)
Second := TDateTimeKit.GetSecond(Now);          // Get second (0-59)
MS := TDateTimeKit.GetMillisecond(Now);         // Get millisecond (0-999)
Quarter := TDateTimeKit.GetQuarter(Now);        // Get quarter (1-4)

// Date components - setters (create new date, don't modify input)
Date := TDateTimeKit.SetYear(Now, 2024);         // Set year
Date := TDateTimeKit.SetMonth(Now, 1);           // Set month (1-12)
Date := TDateTimeKit.SetDay(Now, 1);             // Set day (1-31)
Time := TDateTimeKit.SetHour(Now, 9);            // Set hour (0-23)
Time := TDateTimeKit.SetMinute(Now, 30);         // Set minute (0-59)
Time := TDateTimeKit.SetSecond(Now, 0);          // Set second (0-59)
Time := TDateTimeKit.SetMillisecond(Now, 0);     // Set millisecond (0-999)

// Date arithmetic
Date := TDateTimeKit.AddYears(Now, 1);           // Add years
Date := TDateTimeKit.AddMonths(Now, 1);          // Add months
Date := TDateTimeKit.AddDays(Now, 1);            // Add days
Date := TDateTimeKit.AddHours(Now, 1);           // Add hours
Date := TDateTimeKit.AddMinutes(Now, 30);        // Add minutes
Date := TDateTimeKit.AddSeconds(Now, 30);        // Add seconds

// Period start/end
Date := TDateTimeKit.StartOfYear(Now);           // First moment of year
Date := TDateTimeKit.StartOfMonth(Now);          // First moment of month
Date := TDateTimeKit.StartOfWeek(Now);           // First moment of week
Date := TDateTimeKit.StartOfDay(Now);            // Start of day (00:00:00)
Date := TDateTimeKit.StartOfHour(Now);           // Start of hour (XX:00:00)
Date := TDateTimeKit.EndOfYear(Now);             // Last moment of year
Date := TDateTimeKit.EndOfMonth(Now);            // Last moment of month
Date := TDateTimeKit.EndOfWeek(Now);             // Last moment of week
Date := TDateTimeKit.EndOfDay(Now);              // End of day (23:59:59.999)
Date := TDateTimeKit.EndOfHour(Now);             // End of hour (XX:59:59.999)

// Date rounding
Date := TDateTimeKit.FloorDate(Now, duMonth);    // Round down to unit
Date := TDateTimeKit.CeilingDate(Now, duMonth);  // Round up to unit
Date := TDateTimeKit.RoundDate(Now, duMonth);    // Round to nearest unit

// Month rolling
Date := TDateTimeKit.RollbackMonth(Now);         // Roll to last day of prev month
Date := TDateTimeKit.RollForwardMonth(Now);      // Roll to first day of next month

// Comparisons
if TDateTimeKit.IsBefore(Date1, Date2) then      // Check if before
if TDateTimeKit.IsAfter(Date1, Date2) then       // Check if after
if TDateTimeKit.IsSameDay(Date1, Date2) then     // Check if same day
if TDateTimeKit.IsSameMonth(Date1, Date2) then   // Check if same month
if TDateTimeKit.IsSameYear(Date1, Date2) then    // Check if same year
if TDateTimeKit.IsAM(Now) then                   // Check if before noon
if TDateTimeKit.IsPM(Now) then                   // Check if after noon

// Business days
if TDateTimeKit.IsBusinessDay(Now) then          // Check if business day
Date := TDateTimeKit.NextBusinessDay(Now);       // Get next business day
Date := TDateTimeKit.PreviousBusinessDay(Now);   // Get previous business day
Date := TDateTimeKit.AddBusinessDays(Now, 5);    // Add 5 business days

// Timezone operations
TZInfo := TDateTimeKit.GetTimeZone(Now);              // Get timezone info for date
WriteLn('Timezone: ', TZInfo.Name);                   // Timezone name
WriteLn('Offset: ', TZInfo.Offset);                   // Minutes from UTC
WriteLn('DST: ', TZInfo.IsDST);                       // Is Daylight Saving Time

SystemTZ := TDateTimeKit.GetSystemTimeZone;           // Get system timezone
TZNames := TDateTimeKit.GetTimeZoneNames;             // Get available timezone names

// Convert between timezones
UTC := TDateTimeKit.WithTimeZone(Now, 'UTC');         // Convert to UTC
Local := TDateTimeKit.WithTimeZone(UTC, SystemTZ);    // Convert to local time

// Force timezone (ignores current timezone)
Forced := TDateTimeKit.ForceTimeZone(Now, 'UTC');     // Force to UTC
```

#### Timezone Best Practices and Troubleshooting

1. **Working with Timezones**
   - Always store dates in UTC internally
   - Convert to local time only for display
   - Use explicit timezone conversions rather than implicit ones
   ```pascal
   // Good: Explicit timezone handling
   UTCTime := TDateTimeKit.WithTimeZone(LocalTime, 'UTC');
   DisplayTime := TDateTimeKit.WithTimeZone(UTCTime, SystemTZ);
   
   // Bad: Implicit timezone assumptions
   DisplayTime := LocalTime; // Timezone unclear
   ```

2. **Handling DST Transitions**
   - Be aware of DST transition dates
   - Handle ambiguous times during DST fallback
   - Account for skipped times during DST spring forward
   ```pascal
   // Example: Handling DST transition
   procedure HandleDSTTransition(const ADateTime: TDateTime);
   var
     TZInfo: TTimeZoneInfo;
   begin
     TZInfo := TDateTimeKit.GetTimeZone(ADateTime);
     if TZInfo.IsDST then
       WriteLn('Time is in DST period')
     else
       WriteLn('Time is in standard time');
       
     // Convert to UTC to avoid ambiguity
     UTCTime := TDateTimeKit.WithTimeZone(ADateTime, 'UTC');
   end;
   ```

3. **Working with Different Timezone Formats**
   - Windows timezone names (e.g., 'Eastern Standard Time')
   - IANA timezone names (e.g., 'America/New_York')
   - UTC offsets (e.g., 'UTC+01:00')
   ```pascal
   // Example: Working with different timezone formats
   SystemTZ := TDateTimeKit.GetSystemTimeZone;    // Get system timezone
   UTCTime := TDateTimeKit.WithTimeZone(Now, 'UTC');  // Convert to UTC
   LocalTime := TDateTimeKit.WithTimeZone(UTCTime, SystemTZ);  // Back to local
   ```

4. **Common Timezone Issues and Solutions**

   a. **Missing Timezone Information**
   ```pascal
   // Problem: Unknown timezone
   if TDateTimeKit.GetTimeZone(ADateTime).Name = '' then
     // Use system timezone as fallback
     Result := TDateTimeKit.WithTimeZone(ADateTime, TDateTimeKit.GetSystemTimeZone)
   else
     Result := ADateTime;
   ```

   b. **Invalid Timezone Names**
   ```pascal
   // Solution: Validate timezone names
   var
     ValidTZNames: TStringArray;
   begin
     ValidTZNames := TDateTimeKit.GetTimeZoneNames;
     if IndexStr(DesiredTZ, ValidTZNames) >= 0 then
       Result := TDateTimeKit.WithTimeZone(ADateTime, DesiredTZ)
     else
       raise Exception.Create('Invalid timezone name');
   end;
   ```

   c. **DST Transition Edge Cases**
   ```pascal
   // Solution: Handle ambiguous DST times
   if IsAmbiguousDSTTime(ADateTime) then
   begin
     // Convert to UTC first to avoid ambiguity
     UTCTime := TDateTimeKit.WithTimeZone(ADateTime, 'UTC');
     // Then convert back to desired timezone
     Result := TDateTimeKit.WithTimeZone(UTCTime, TargetTZ);
   end;
   ```

   d. **Cross-Platform Timezone Names**
   ```pascal
   // Solution: Use platform-independent timezone handling
   {$IFDEF WINDOWS}
   // Use Windows timezone names
   TZ := 'Eastern Standard Time';
   {$ELSE}
   // Use IANA timezone names
   TZ := 'America/New_York';
   {$ENDIF}
   Result := TDateTimeKit.WithTimeZone(ADateTime, TZ);
   ```

5. **Performance Considerations**
   - Cache timezone information for frequently used timezones
   - Batch timezone conversions when possible
   - Use `ForceTimeZone` for bulk operations where exact precision isn't required
   ```pascal
   // Example: Efficient bulk timezone conversion
   procedure ConvertDateArray(var Dates: array of TDateTime; const TargetTZ: string);
   var
     I: Integer;
   begin
     for I := Low(Dates) to High(Dates) do
       Dates[I] := TDateTimeKit.ForceTimeZone(Dates[I], TargetTZ);
   end;
   ```
