# 📋 Cheat Sheet

A comprehensive reference of TidyKit's features and usage examples.
 
## Table of Contents

- [📋 Cheat Sheet](#-cheat-sheet)
  - [Table of Contents](#table-of-contents)
  - [🔄 JSON Operations](#-json-operations)
    - [Creating Values](#creating-values)
    - [Objects](#objects)
    - [Arrays](#arrays)
    - [Type Safety](#type-safety)
    - [Parsing \& Formatting](#parsing--formatting)
    - [Error Handling](#error-handling)
  - [📁File System Operations](#file-system-operations)
  - [🧵String operations](#string-operations)
  - [🕙 DateTime Operations](#-datetime-operations)
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
  - [🔒 Cryptographic Operations](#-cryptographic-operations)
    - [Hash Functions](#hash-functions)
    - [Base64 Encoding/Decoding](#base64-encodingdecoding)
    - [XOR Encryption (Basic)](#xor-encryption-basic)
    - [Blowfish Encryption (Legacy Support)](#blowfish-encryption-legacy-support)
    - [AES-256 Encryption](#aes-256-encryption)
    - [Common Use Cases](#common-use-cases)
    - [Best Practices](#best-practices)
    - [Security Notes](#security-notes)
    - [Using the Fluent Interface](#using-the-fluent-interface)
    - [Error Handling](#error-handling-1)
    - [Working with JSON](#working-with-json)
  - [📊 Math Operations](#-math-operations)
    - [Statistics (TStatsKit)](#statistics-tstatskit)
    - [Financial Calculations (TFinanceKit)](#financial-calculations-tfinancekit)
    - [Matrix Operations (TMatrixKit)](#matrix-operations-tmatrixkit)
    - [Trigonometry (TTrigKit)](#trigonometry-ttrigkit)
  - [📝 Logging Operations](#-logging-operations)
    - [Getting Started with TidyKit.Logger](#getting-started-with-tidykitlogger)
    - [Common Scenarios](#common-scenarios)
    - [Advanced Use](#advanced-use)
  - [📁 Archive Operations](#-archive-operations)
  - [🛠️ Error Handling](#️-error-handling)

## 🔄 JSON Operations

### Creating Values
```pascal
TJSON.Obj                    // Empty object: {}
TJSON.Arr                    // Empty array: []
TJSON.Str('text')           // String: "text"
TJSON.Int(123)              // Integer: 123
TJSON.Num(123.45)           // Number: 123.45
TJSON.Bool(True)            // Boolean: true
TJSON.Null                  // Null: null
```

### Objects
```pascal
// Add values
Obj.Add('str', 'value')     // Add string
Obj.Add('num', 123)         // Add integer
Obj.Add('dec', 123.45)      // Add decimal
Obj.Add('bool', True)       // Add boolean
Obj.Add('null', TJSON.Null) // Add null

// Access values
Value := Obj['key']         // Get value
Exists := Obj.Contains('key')
Obj.Remove('key')           // Remove key
Count := Obj.Count          // Number of items
Keys := Obj.GetOrderedKeys  // Get keys in order
```

### Arrays
```pascal
// Add values
Arr.Add('text')            // Add string
Arr.Add(123)               // Add integer
Arr.Add(123.45)            // Add decimal
Arr.Add(True)              // Add boolean

// Access values
Value := Arr[0]            // Get value
Arr.Delete(0)              // Delete item
Arr.Clear                  // Remove all
Count := Arr.Count         // Number of items
```

### Type Safety
```pascal
// Safe type checking
if Value.IsString then S := Value.AsString
if Value.IsNumber then
begin
  if Frac(Value.AsNumber) = 0 then
    I := Value.AsInteger   // Only for whole numbers
  else
    D := Value.AsNumber    // For any number
end
if Value.IsBoolean then B := Value.AsBoolean
if Value.IsObject then O := Value.AsObject
if Value.IsArray then A := Value.AsArray
if Value.IsNull then ...  // Handle null
```

### Parsing & Formatting
```pascal
// Parse JSON
Value := TJSON.Parse('{"key":"value"}')
Success := TJSON.TryParse(JSON, Value)

// Format JSON
Pretty := Value.ToString(True)   // With indentation
Compact := Value.ToString(False) // Without whitespace
```

### Error Handling
```pascal
try
  Value := TJSON.Parse(JSON);
except
  on E: EJSONException do
    // Handle JSON errors
end;
```

## 📁File System Operations

```pascal
// Basic file operations
Content := TFileKit.ReadTextFile('input.txt');             // Read entire file
TFileKit.WriteTextFile('output.txt', 'content');           // Write to file
TFileKit.AppendText('file.txt', 'new content');            // Append text to file
TFileKit.PrependText('file.txt', 'prefix text');           // Add text at start
TFileKit.ReplaceText('file.txt', 'old', 'new');            // Replace text in file
TFileKit.DeleteFile('temp.txt');                           // Delete file
TFileKit.CopyFile('source.txt', 'dest.txt');               // Copy file with attributes
TFileKit.MoveFile('old.txt', 'new.txt');                   // Move/rename file

// Batch file operations
TFileKit.CopyFiles('source_dir', 'dest_dir', '*.txt');     // Copy all .txt files
TFileKit.MoveFiles('source_dir', 'dest_dir', '*.doc');     // Move all .doc files
TFileKit.DeleteFiles('temp_dir', '*.tmp');                 // Delete all .tmp files

// Directory operations
TFileKit.CreateDirectory('new_dir');                       // Create directory
TFileKit.DeleteDirectory('old_dir', True);                 // Delete directory (True = recursive)
TFileKit.EnsureDirectory('path/to/file.txt');              // Create all parent directories

// File listing with sorting options
Files := TFileKit.ListFiles('.', '*', False);              // List files in current dir
Files := TFileKit.ListFiles('.', '*', True);               // List files recursively
Files := TFileKit.ListFiles('.', '*.txt');                 // List only .txt files
Files := TFileKit.ListFiles('.', '*', False, fsName);      // Sort by name (ascending)
Files := TFileKit.ListFiles('.', '*', False, fsNameDesc);  // Sort by name (descending)
Files := TFileKit.ListFiles('.', '*', False, fsDate);      // Sort by date (ascending)
Files := TFileKit.ListFiles('.', '*', False, fsDateDesc);  // Sort by date (descending)
Files := TFileKit.ListFiles('.', '*', False, fsSize);      // Sort by size (ascending)
Files := TFileKit.ListFiles('.', '*', False, fsSizeDesc);  // Sort by size (descending)

// Directory listing with sorting options
Dirs := TFileKit.ListDirectories('.', '*', False);         // List directories in current dir
Dirs := TFileKit.ListDirectories('.', '*', True);          // List directories recursively
Dirs := TFileKit.ListDirectories('.', 'test_*');           // List dirs matching pattern
Dirs := TFileKit.ListDirectories('.', '*', False, fsName); // Sort by name (ascending)
Dirs := TFileKit.ListDirectories('.', '*', False, fsDate); // Sort by date (ascending)

// Path operations
Path := TFileKit.GetFileName('path/to/file.txt');         // Returns 'file.txt'
Path := TFileKit.GetFileNameWithoutExt('file.txt');       // Returns 'file'
Path := TFileKit.GetDirectory('path/to/file.txt');        // Returns 'path/to'
Path := TFileKit.GetExtension('file.txt');                // Returns '.txt'
Path := TFileKit.GetParentDir('path/to/file.txt');        // Returns 'path/to'
Path := TFileKit.CombinePaths('path', 'file.txt');        // Combine paths
Path := TFileKit.NormalizePath('path/./to/../file.txt');  // Normalize path
Path := TFileKit.ChangeExtension('file.txt', '.doc');     // Change file extension
if TFileKit.IsAbsolutePath('C:\file.txt') then ...        // Check if path is absolute

// Path analysis
CommonPath := TFileKit.GetCommonPath('/usr/local/bin', '/usr/local/lib');  // Returns '/usr/local'
RelPath := TFileKit.GetRelativePath('/usr/share', '/usr/local/bin');       // Returns '../local/bin'
if TFileKit.IsSubPath('/usr/local', '/usr/local/bin') then ...             // Check if path is subpath

// File information
if TFileKit.Exists('file.txt') then ...                 // Check file exists
if TFileKit.DirectoryExists('dir') then ...             // Check directory exists
Size := TFileKit.GetSize('file.txt');                   // Get file size
Time := TFileKit.GetCreationTime('file.txt');           // Get creation time
Time := TFileKit.GetLastAccessTime('file.txt');         // Get last access time
Time := TFileKit.GetLastWriteTime('file.txt');          // Get last write time
Attrs := TFileKit.GetAttributes('file.txt');            // Get file attributes
if TFileKit.IsTextFile('file.txt') then ...             // Check if text file
Encoding := TFileKit.GetFileEncoding('file.txt');       // Get file encoding
if TFileKit.IsEmptyDirectory('dir') then ...            // Check if directory is empty

// Search operations
Results := TFileKit.SearchFiles('.', '*.txt', True);    // Search files recursively
Results := TFileKit.SearchFilesIn('dir', '*.txt', True);// Search in specific dir
File := TFileKit.FindLastModifiedFile('.', '*.txt');    // Find newest file
File := TFileKit.FindFirstModifiedFile('.', '*.txt');   // Find oldest file
File := TFileKit.FindLargestFile('.', '*.txt');         // Find largest file
File := TFileKit.FindSmallestFile('.', '*.txt');        // Find smallest file

// System directories
Dir := TFileKit.GetUserDir;                             // Get user directory
Dir := TFileKit.GetCurrentDir;                          // Get current directory
Dir := TFileKit.GetTempDir;                             // Get temp directory

// Temporary files
TempFile := TFileKit.CreateTempFile('prefix_');         // Create temp file
TempDir := TFileKit.CreateTempDirectory('prefix_');     // Create temp directory

// Symbolic link operations
TFileKit.CreateSymLink('target.txt', 'link.txt');       // Create file symlink
TFileKit.CreateSymLink('target_dir', 'link_dir', True); // Create directory symlink
TFileKit.DeleteSymLink('link.txt');                     // Delete symlink
Path := TFileKit.ResolveSymLink('link.txt');            // Get target path
if TFileKit.IsSymLink('link.txt') then ...              // Check if path is symlink

// File locking
if TFileKit.LockFile('file.txt') then                   // Lock file
try
  // Work with file
finally
  TFileKit.UnlockFile('file.txt');                      // Unlock file
end;
if TFileKit.IsFileLocked('file.txt') then ...           // Check if file is locked

// File validation and sanitization
if TFileKit.IsValidFileName('file.txt') then ...        // Check if filename is valid
Name := TFileKit.SanitizeFileName('file*.txt');         // Sanitize filename
Path := TFileKit.MakeValidPath('/path//to/./file');     // Make path valid
if TFileKit.IsPathTooLong('very/long/path') then ...    // Check if path is too long

// Directory information
Info := TFileKit.GetDirectoryInfo('dir');               // Get directory statistics
WriteLn('Files: ', Info.FileCount);                     // Number of files
WriteLn('Directories: ', Info.DirectoryCount);          // Number of subdirectories
WriteLn('Total size: ', Info.TotalSize);                // Total size in bytes
WriteLn('Oldest file: ', Info.OldestFile);              // Name of oldest file
WriteLn('Newest file: ', Info.NewestFile);              // Name of newest file
WriteLn('Largest file: ', Info.LargestFile);            // Name of largest file

// Pattern matching
if TFileKit.MatchesPattern('test.txt', '*.txt') then ... // Check if filename matches pattern
File := TFileKit.FindFirstMatch('dir', '*.txt');         // Find first matching file
Count := TFileKit.CountMatches('dir', '*.txt');          // Count matching files

// Note: On Windows, creating symlinks requires Administrator privileges or Developer Mode
// On Unix/Linux, regular users can create symlinks in their own directories
```

## 🧵String operations

```pascal
// ---------- Basic String Manipulation ----------

// Trimming and Whitespace
Str := TStringKit.Trim(Text);                     // Trim whitespace from both ends
Str := TStringKit.TrimLeft(Text);                 // Trim left whitespace
Str := TStringKit.TrimRight(Text);                // Trim right whitespace
Str := TStringKit.CollapseWhitespace(Text);       // Collapse multiple spaces to one
Str := TStringKit.RemoveWhitespace(Text);         // Remove all whitespace

// Case Conversion
Str := TStringKit.ToLower(Text);                  // Convert to lowercase
Str := TStringKit.ToUpper(Text);                  // Convert to uppercase
Str := TStringKit.CapitalizeText(Text);           // Capitalize first letter of each word

// Advanced Case Conversion
Str := TStringKit.ToTitleCase(Text);              // Convert to Title Case
Str := TStringKit.ToCamelCase(Text);              // Convert to camelCase
Str := TStringKit.ToPascalCase(Text);             // Convert to PascalCase
Str := TStringKit.ToSnakeCase(Text);              // Convert to snake_case
Str := TStringKit.ToKebabCase(Text);              // Convert to kebab-case

// Padding and Alignment
Str := TStringKit.PadLeft(Text, Width, Char);     // Left pad with character
Str := TStringKit.PadRight(Text, Width, Char);    // Right pad with character
Str := TStringKit.PadCenter(Text, Width, Char);   // Center pad with character

// Text Transformation
Str := TStringKit.ReverseText(Text);              // Reverse text
Str := TStringKit.DuplicateText(Text, Count);     // Duplicate text
Str := TStringKit.Truncate(Text, MaxLen, '...');  // Truncate with ellipsis

// ---------- String Inspection ----------

// Basic Tests
if TStringKit.IsEmpty(Text) then                  // Check if empty
Length := TStringKit.GetLength(Text);             // Get string length
Count := TStringKit.CountWords(Text);             // Count words

// Content Inspection
if TStringKit.Contains(Text, SubStr) then         // Check if contains substring
if TStringKit.StartsWith(Text, Prefix) then       // Check if starts with prefix
if TStringKit.EndsWith(Text, Suffix) then         // Check if ends with suffix
Count := TStringKit.CountSubString(Text, SubStr); // Count occurrences of substring

// Validation
if TStringKit.IsValidEmail('user@example.com') then       // Validate email address
if TStringKit.IsValidURL('https://example.com') then      // Validate URL
if TStringKit.IsValidIP('192.168.1.1') then               // Validate IP address (v4 or v6)
if TStringKit.IsValidIPv4('192.168.1.1') then             // Validate IPv4 address
if TStringKit.IsValidIPv6('::1') then                     // Validate IPv6 address
if TStringKit.IsValidDate('2024-01-15', 'yyyy-mm-dd') then // Validate date format

// ---------- Substring Operations ----------

// Extraction
Str := TStringKit.SubString(Text, Start, Length); // Extract substring
Str := TStringKit.LeftStr(Text, Length);          // Get left part
Str := TStringKit.RightStr(Text, Length);         // Get right part

// Splitting and Joining
Words := TStringKit.GetWords(Text);               // Split into words
Strings := TStringKit.Split('a,b,c', ',');                // Split by delimiter
Strings := TStringKit.Split('a,b,,c', ',', 0, True);      // Split and remove empty entries
Str := TStringKit.Join(['one', 'two', 'three'], ', ');    // Join with delimiter

// ---------- Pattern Matching and Replacement ----------

// Regex Operations
Matches := TStringKit.ExtractMatches(Text, Pattern);      // Extract regex matches with positions
Words := TStringKit.ExtractAllMatches(Text, Pattern);     // Extract regex matches as strings
if TStringKit.MatchesPattern(Text, Pattern) then          // Check regex pattern
Str := TStringKit.ReplaceRegEx(Text, Pattern, Replace);   // Replace using regex

// Simple Replacement
Str := TStringKit.ReplaceText(Text, Old, New);            // Replace all occurrences

// ---------- Formatting and Conversion ----------

// Number Formatting
Str := TStringKit.FormatNumber(1234);                     // Format to "1,234"
Str := TStringKit.FormatFloat(1234.56, 2, '.', ',');      // Format to "1,234.56"
Str := TStringKit.FormatFileSize(1048576);                // Format to "1.00 MB"

// Number Conversions
Roman := TStringKit.ToRoman(1984);                        // Convert to Roman numerals (MCMLXXXIV)
Num := TStringKit.FromRoman('MMXXIV');                    // Convert from Roman numerals (2024)
Ordinal := TStringKit.ToOrdinal(21);                      // Convert to ordinal (21st)
Words := TStringKit.NumberToWords(42);                    // Convert to words (forty-two)

// ---------- String Similarity and Distance ----------

// Distance Metrics
Dist := TStringKit.LevenshteinDistance(S1, S2);        // Edit distance between strings
Dist := TStringKit.HammingDistance(S1, S2);            // Character differences at same positions

// Similarity Metrics (0-1 scale, higher is more similar)
Sim := TStringKit.LevenshteinSimilarity(S1, S2);       // Normalized similarity
Sim := TStringKit.JaroSimilarity(S1, S2);              // Jaro similarity for short strings
Sim := TStringKit.JaroWinklerSimilarity(S1, S2);       // Jaro-Winkler with prefix bonus
Sim := TStringKit.LCSSimilarity(S1, S2);               // LCS similarity ratio

// Common Subsequence
LCS := TStringKit.LongestCommonSubsequence(S1, S2);    // Longest common subsequence

// Fuzzy Matching
if TStringKit.IsFuzzyMatch(S1, S2) then               // Default: Levenshtein, threshold 0.7
if TStringKit.IsFuzzyMatch(S1, S2, 0.8) then          // Custom threshold
if TStringKit.IsFuzzyMatch(S1, S2, 0.7, 1) then       // Using Jaro-Winkler (method=1)
if TStringKit.IsFuzzyMatch(S1, S2, 0.7, 2) then       // Using LCS similarity (method=2)

// ---------- Phonetic Algorithms ----------

// Phonetic Encoding
Code := TStringKit.Soundex('Smith');                   // Get Soundex code (S530)
Code := TStringKit.Metaphone('Smith');                 // Get Metaphone code (SM0)

// ---------- Text Analysis ----------

// Readability and Analysis
Score := TStringKit.FleschKincaidReadability(Text);    // Calculate readability (0-100)
NGrams := TStringKit.GenerateNGrams(Text, 2);          // Generate bigrams

// ---------- Encoding/Decoding ----------

// HTML and URL Encoding
Encoded := TStringKit.HTMLEncode('<div>');             // HTML encoding
Decoded := TStringKit.HTMLDecode('&lt;div&gt;');       // HTML decoding
Encoded := TStringKit.URLEncode('a b');                // URL encoding (a+b)
Decoded := TStringKit.URLDecode('a+b');                // URL decoding

// Base64 and Hex Encoding
// Note: Base64 functions have been removed from TStringKit - use TCryptoKit instead
// TCryptoKit.Base64Encode('Hello World!');            // Base64 encoding (SGVsbG8gV29ybGQh)
// TCryptoKit.Base64Decode('SGVsbG8gV29ybGQh');        // Base64 decoding

HexStr := TStringKit.HexEncode('abc');                   // Hex encoding (616263)
Original := TStringKit.HexDecode('616263');              // Hex decoding
```

## 🕙 DateTime Operations

### Basic Operations
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

### Component Access
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

### Component Modification
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

### Date Arithmetic
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

### Period Operations
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

### Interval Operations
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

### Date Comparison
```pascal
// Compare dates
if TDateTimeKit.IsBefore(Date1, Date2) then      // Date1 < Date2
if TDateTimeKit.IsAfter(Date1, Date2) then       // Date1 > Date2
if TDateTimeKit.IsSameDay(Date1, Date2) then     // Same calendar day
if TDateTimeKit.IsSameMonth(Date1, Date2) then   // Same month and year
if TDateTimeKit.IsSameYear(Date1, Date2) then    // Same year
```

### Period Boundaries
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

### Date Rounding
```pascal
// Round dates to nearest unit
Round := TDateTimeKit.RoundDate(Now, duMonth);    // Round to nearest month
Floor := TDateTimeKit.FloorDate(Now, duMonth);    // Round down to month start
Ceil := TDateTimeKit.CeilingDate(Now, duMonth);   // Round up to month end

// Available units: duSecond, duMinute, duHour, duDay, duWeek,
// duMonth, duBiMonth, duQuarter, duSeason, duHalfYear, duYear
```

### Timezone Operations
```pascal
// Get timezone information
TZ := TDateTimeKit.GetTimeZone(Now);           // Current timezone info
SystemTZ := TDateTimeKit.GetSystemTimeZone;     // System timezone name
TZNames := TDateTimeKit.GetTimeZoneNames;       // Available timezone names

// Convert between timezones
UTC := TDateTimeKit.WithTimeZone(Now, 'UTC');   // Convert to UTC
Local := TDateTimeKit.ForceTimeZone(Now, 'EST'); // Force timezone
```

## 🔒 Cryptographic Operations

### Hash Functions
```pascal
// MD5 (legacy support)
Hash := TCryptoKit.MD5Hash('text');                // Returns MD5 hash

// SHA1 (legacy support)
Hash := TCryptoKit.SHA1Hash('text');               // Returns SHA1 hash

// SHA2 Family
Hash := TCryptoKit.SHA256Hash('text');             // Returns SHA-256 hash
Hash := TCryptoKit.SHA512Hash('text');             // Returns SHA-512 hash
Hash := TCryptoKit.SHA512_256Hash('text');         // Returns SHA-512/256 hash

// SHA3 Family (Keccak)
Hash := TCryptoKit.SHA3_224Hash('text');           // Returns SHA3-224 hash
Hash := TCryptoKit.SHA3_256Hash('text');           // Returns SHA3-256 hash
Hash := TCryptoKit.SHA3_384Hash('text');           // Returns SHA3-384 hash
Hash := TCryptoKit.SHA3_512Hash('text');           // Returns SHA3-512 hash
```

### Base64 Encoding/Decoding
```pascal
// Base64 encoding/decoding
Encoded := TCryptoKit.Base64Encode('text');        // Encode to Base64
Decoded := TCryptoKit.Base64Decode(Encoded);       // Decode from Base64
```

### XOR Encryption (Basic)
```pascal
// Simple XOR encryption (not secure for sensitive data)
Encrypted := TCryptoKit.XORCrypt('text', 'key');   // Encrypt text
Decrypted := TCryptoKit.XORCrypt(Encrypted, 'key'); // Decrypt text (same operation)
```

### Blowfish Encryption (Legacy Support)
```pascal
// Blowfish encryption/decryption
Encrypted := TCryptoKit.BlowfishCrypt('text', 'key', bmEncrypt); // Encrypt
Decrypted := TCryptoKit.BlowfishCrypt(Encrypted, 'key', bmDecrypt); // Decrypt
```

### AES-256 Encryption

```pascal
// High-level interface (with automatic Base64 encoding)
var
  Key: TAESKey;
  IV: TAESBlock;
  PlainText, CipherText: string;
begin
  // Generate secure key and IV
  Key := TCryptoKit.GenerateRandomKey;
  IV := TCryptoKit.GenerateIV;
  
  // CBC Mode (with PKCS7 padding)
  CipherText := TCryptoKit.AES256EncryptCBC('secret text', Key, IV);
  PlainText := TCryptoKit.AES256DecryptCBC(CipherText, Key, IV);
  
  // CTR Mode (no padding needed)
  CipherText := TCryptoKit.AES256EncryptCTR('secret text', Key, IV);
  PlainText := TCryptoKit.AES256DecryptCTR(CipherText, Key, IV);
end;

// Low-level interface (raw binary operations)
var
  Key: TAESKey;
  IV: TAESBlock;
  PlainBytes, CipherBytes: TBytes;
begin
  // Generate secure key and IV
  Key := TCryptoKit.GenerateRandomKey;
  IV := TCryptoKit.GenerateIV;
  
  // CBC Mode with configurable padding
  CipherBytes := TAES256.EncryptCBC(PlainBytes, Key, IV, apPKCS7); // With PKCS7 padding
  PlainBytes := TAES256.DecryptCBC(CipherBytes, Key, IV, apPKCS7);
  
  CipherBytes := TAES256.EncryptCBC(PlainBytes, Key, IV, apNone);  // No padding (for NIST vectors)
  PlainBytes := TAES256.DecryptCBC(CipherBytes, Key, IV, apNone);
  
  // CTR Mode (no padding needed)
  CipherBytes := TAES256.EncryptCTR(PlainBytes, Key, IV);
  PlainBytes := TAES256.DecryptCTR(CipherBytes, Key, IV);
end;

// Key derivation and generation
var
  Key: TAESKey;
  IV: TAESBlock;
begin
  // Generate random key and IV
  Key := TCryptoKit.GenerateRandomKey;
  IV := TCryptoKit.GenerateIV;
  
  // Derive key from password
  Key := TCryptoKit.DeriveKey('password', 'salt', 100000); // PBKDF2-SHA256
end;
```

### Common Use Cases

1. Password Hashing
```pascal
// Hash password before storage (use SHA-512 or SHA3-512)
HashedPassword := TCryptoKit.SHA512Hash(Password);
```

2. File Integrity Check
```pascal
// Calculate file hash
FileHash := TCryptoKit.SHA256Hash(TFileKit.ReadFile('file.txt'));
```

3. Secure Data Storage
```pascal
var
  Key: TAESKey;
  IV: TAESBlock;
  EncryptedData: string;
begin
  // Generate secure key and IV
  Key := TCryptoKit.GenerateRandomKey;
  IV := TCryptoKit.GenerateIV;
  
  // Encrypt and store (uses PKCS7 padding)
  EncryptedData := TCryptoKit.AES256EncryptCBC(SensitiveData, Key, IV);
  TFileKit.WriteFile('secure.dat', EncryptedData);
end;
```

### Best Practices

1. Key Management
   - Use `TCryptoKit.GenerateRandomKey` for secure key generation
   - Use `TCryptoKit.DeriveKey` for password-based keys
   - Never store encryption keys in source code
   - Rotate keys periodically
   - Securely erase keys from memory when done

2. IV (Initialization Vector) Handling
   - Use `TCryptoKit.GenerateIV` for secure IV generation
   - Use a unique IV for each encryption operation
   - Never reuse IVs with the same key
   - Store IV alongside encrypted data (it's not secret)

3. Mode Selection
   - Use CBC mode with PKCS7 padding for general encryption
   - Use CTR mode for streaming or random access
   - Use raw mode (apNone) only for NIST compliance testing

4. Hash Selection
   - Use SHA-256 or better for general hashing
   - Use SHA-512 for password hashing (with proper salting)
   - Avoid MD5 and SHA1 for security-critical operations

### Security Notes

1. Encryption Strength
   - AES-256 provides 256-bit security
   - Key size determines security level
   - Mode of operation affects security properties

2. Known Limitations
   - CBC mode requires padding (potential padding oracle attacks)
   - CTR mode requires unique counter values
   - XOR encryption is not cryptographically secure

3. Compliance
   - AES-256 implementation follows NIST standards
   - Supports FIPS-compliant modes of operation
   - Includes PKCS7 padding for CBC mode
   - Raw mode available for NIST test vectors
```

## HTTP Client (TidyKit.Request.Simple)

### Simple Requests
```pascal
uses TidyKit;

// GET request
var Response := Http.Get('https://api.example.com/data');
if Response.StatusCode = 200 then
  WriteLn(Response.Text);

// POST with form data
Response := Http.Post('https://api.example.com/submit', 'name=John&age=30');

// POST with JSON
Response := Http.PostJSON('https://api.example.com/users', '{"name": "John"}');

// PUT request
Response := Http.Put('https://api.example.com/users/1', 'status=active');

// DELETE request
Response := Http.Delete('https://api.example.com/users/1');
```

### Using the Fluent Interface
```pascal
// Request with headers, params, and JSON
var
  Request: THttpRequest;
  Response: TResponse;
begin
  Response := Request
    .Post
    .URL('https://api.example.com/users')
    .AddHeader('X-API-Key', 'your-key')
    .AddHeader('Accept', 'application/json')
    .AddParam('version', '2.0')
    .WithJSON('{"name": "John"}')
    .Send;
end;

// Authenticated request with timeout
var
  Request: THttpRequest;
  Response: TResponse;
begin
  Response := Request
    .Get
    .URL('https://api.example.com/secure')
    .BasicAuth('username', 'password')
    .WithTimeout(5000)  // 5 seconds
    .Send;
end;

// Form data with custom headers
var
  Request: THttpRequest;
  Response: TResponse;
begin
  Response := Request
    .Post
    .URL('https://api.example.com/submit')
    .AddHeader('Content-Type', 'application/x-www-form-urlencoded')
    .WithData('name=John&age=30')
    .Send;
end;
```

### Error Handling
```pascal
// Using try-except
var
  Response: TResponse;
begin
  try
    Response := Http.Get('https://api.example.com/data');
    if Response.StatusCode = 200 then
      WriteLn(Response.Text);
  except
    on E: ETidyKitException do
      WriteLn('Error: ', E.Message);
  end;
end;

// Using result pattern
var
  Result: TRequestResult;
begin
  Result := Http.TryGet('https://api.example.com/data');
  if Result.Success then
    WriteLn(Result.Response.Text)
  else
    WriteLn('Error: ', Result.Error);
end;
```

### Working with JSON
```pascal
var
  Request: THttpRequest;
  Response: TResponse;
  UserName: string;
  UserAge: Integer;
  Items: TJSONArray;
  I: Integer;
begin
  Response := Request
    .Get
    .URL('https://api.example.com/users')
    .Send;

  if Response.StatusCode = 200 then
  begin
    // Access JSON data
    UserName := Response.JSON.FindPath('user.name').AsString;
    UserAge := Response.JSON.FindPath('user.age').AsInteger;
    
    // Array iteration
    Items := Response.JSON.FindPath('items').AsArray;
    for I := 0 to Items.Count - 1 do
      WriteLn(Items[I].AsString);
  end;
end;
```

## 📊 Math Operations

### Statistics (TStatsKit)
```pascal
// Basic descriptive statistics
Stats := TStatsKit.Describe(Data);
// Returns comprehensive statistics with full Double precision:
// - Mean: Full precision (e.g., 5.500000)
// - StdDev: Full precision (e.g., 2.872281)
// - Variance: Full precision (e.g., 8.250000)

// Hypothesis testing with p-values
TStatsKit.ShapiroWilkTest(Data, WPValue);
if WPValue >= 0.05 then
  WriteLn('Data is normally distributed');

// Error handling
try
  Result := TStatsKit.StandardDeviation(Data);
except
  on E: EInvalidArgument do
    WriteLn('Error: Need at least 2 data points');
  on E: Exception do
    WriteLn('Unexpected error: ', E.Message);
end;
```

### Financial Calculations (TFinanceKit)
```pascal
// All financial calculations use 4 decimal precision by default
// Precision can be customized using the ADecimals parameter

// Modified Duration (Expected: 4.3009)
ModDur := TFinanceKit.ModifiedDuration(
  1000.0,  // Face value
  0.06,    // Coupon rate
  0.05,    // Yield rate
  2,       // Periods per year
  5        // Years to maturity
);

// Black-Scholes Option Pricing
// Call Option (Expected: 10.4506)
// Put Option (Expected: 5.5723)
CallPrice := TFinanceKit.BlackScholes(
  100.0,   // Spot price
  100.0,   // Strike price
  0.05,    // Risk-free rate
  0.20,    // Volatility
  1.0,     // Time to maturity
  otCall
);

// Operating Leverage (Expected DOL: 2.0000)
Leverage := TFinanceKit.OperatingLeverage(
  10000.0,  // Quantity
  50.0,     // Price per unit
  30.0,     // Variable cost per unit
  100000.0  // Fixed costs
);

// Error handling for financial calculations
try
  IRR := TFinanceKit.InternalRateOfReturn(InitialInvestment, CashFlows);
except
  on E: EInvalidOperation do
    WriteLn('Error: IRR calculation did not converge');
  on E: EArgumentException do
    WriteLn('Error: Invalid cash flow data');
end;
```

### Matrix Operations (TMatrixKit)
```pascal
// Creating matrices
M := TMatrixKit.CreateFromArray([[1.0, 2.0], [3.0, 4.0]]);  // From array
M := TMatrixKit.Identity(3);                                 // Identity matrix
M := TMatrixKit.Zeros(2, 3);                                // Zero matrix
M := TMatrixKit.Ones(3, 2);                                 // Matrix of ones
M := TMatrixKit.CreateDiagonal([1.0, 2.0, 3.0]);           // Diagonal matrix
M := TMatrixKit.CreateSymmetric([[1.0, 2.0], [2.0, 3.0]]); // Symmetric matrix
M := TMatrixKit.CreateRandom(2, 2, 0.0, 1.0);              // Random matrix

// Basic operations
C := A.Add(B);                    // Matrix addition
C := A.Subtract(B);               // Matrix subtraction
C := A.Multiply(B);               // Matrix multiplication
C := A.ScalarMultiply(2.0);       // Scalar multiplication
C := A.Transpose;                 // Matrix transpose
C := A.Inverse;                   // Matrix inverse

// Element-wise operations
C := A.ElementWiseMultiply(B);    // Element-wise multiplication
C := A.ElementWiseDivide(B);      // Element-wise division

// Matrix properties
Val := M.Determinant;             // Matrix determinant
Val := M.Trace;                   // Matrix trace
Val := M.Rank;                    // Matrix rank
Val := M.Condition;               // Condition number
Val := M.NormOne;                 // Maximum column sum norm
Val := M.NormInf;                 // Maximum row sum norm
Val := M.NormFrobenius;           // Frobenius norm

// Matrix type checks
if M.IsSquare then ...            // Check if square
if M.IsSymmetric then ...         // Check if symmetric
if M.IsDiagonal then ...          // Check if diagonal
if M.IsTriangular(True) then ...  // Check if upper triangular
if M.IsPositiveDefinite then ...  // Check if positive definite
if M.IsOrthogonal then ...        // Check if orthogonal

// Matrix decompositions
LU := M.LU;                       // LU decomposition
QR := M.QR;                       // QR decomposition
Eigen := M.EigenDecomposition;    // Eigendecomposition

// Submatrix operations
Sub := M.GetSubMatrix(0, 0, 2, 2);  // Get 2x2 submatrix from top-left
M.SetSubMatrix(1, 1, Sub);          // Set submatrix at position (1,1)

// Solving linear systems (Ax = b)
X := A.Inverse.Multiply(B);          // Using inverse
// Or using LU decomposition:
LU := A.LU;
X := LU.U.Inverse.Multiply(
     LU.L.Inverse.Multiply(B));

// Error handling
try
  M := A.Inverse;
except
  on E: EMatrixError do
    WriteLn('Matrix error: ', E.Message);
end;
```

### Trigonometry (TTrigKit)
```pascal
// Angle conversions
Rad := TTrigKit.DegToRad(Degrees);           // Convert degrees to radians
Deg := TTrigKit.RadToDeg(Radians);           // Convert radians to degrees
Rad := TTrigKit.GradToRad(Grads);            // Convert grads to radians
Grad := TTrigKit.RadToGrad(Radians);         // Convert radians to grads
Rad := TTrigKit.NormalizeAngle(Radians);     // Normalize angle to [0, 2π]
Deg := TTrigKit.NormalizeAngleDeg(Degrees);  // Normalize angle to [0, 360]

// Basic trigonometric functions
Sin := TTrigKit.Sin(X);                      // Sine of X (radians)
Cos := TTrigKit.Cos(X);                      // Cosine of X (radians)
Tan := TTrigKit.Tan(X);                      // Tangent of X (radians)
Sec := TTrigKit.Sec(X);                      // Secant of X (radians)
Csc := TTrigKit.Csc(X);                      // Cosecant of X (radians)
Cot := TTrigKit.Cot(X);                      // Cotangent of X (radians)

// Inverse trigonometric functions
ASin := TTrigKit.ArcSin(X);                  // Inverse sine
ACos := TTrigKit.ArcCos(X);                  // Inverse cosine
ATan := TTrigKit.ArcTan(X);                  // Inverse tangent
ATan2 := TTrigKit.ArcTan2(Y, X);            // Two-argument inverse tangent

// Hyperbolic functions
SinH := TTrigKit.Sinh(X);                    // Hyperbolic sine
CosH := TTrigKit.Cosh(X);                    // Hyperbolic cosine
TanH := TTrigKit.Tanh(X);                    // Hyperbolic tangent

// Inverse hyperbolic functions
ASinH := TTrigKit.ArcSinh(X);                // Inverse hyperbolic sine
ACosH := TTrigKit.ArcCosh(X);                // Inverse hyperbolic cosine
ATanH := TTrigKit.ArcTanh(X);                // Inverse hyperbolic tangent

// Triangle calculations
Area := TTrigKit.TriangleArea(Base, Height); // Triangle area from base and height
Area := TTrigKit.TriangleAreaSAS(A, Angle, B); // Triangle area from SAS
Area := TTrigKit.TriangleAreaSSS(A, B, C);   // Triangle area from three sides
Perim := TTrigKit.TrianglePerimeter(A, B, C); // Triangle perimeter
InRad := TTrigKit.TriangleInRadius(A, B, C);  // Radius of inscribed circle
CircumRad := TTrigKit.TriangleCircumRadius(A, B, C); // Radius of circumscribed circle

// Circle sector and segment calculations
SectorArea := TTrigKit.CircularSectorArea(R, Angle); // Area of circular sector
SegmentArea := TTrigKit.CircularSegmentArea(R, Angle); // Area of circular segment
ChordLen := TTrigKit.ChordLength(R, Angle);  // Length of chord

// Vector operations
Mag := TTrigKit.VectorMagnitude(X, Y);       // Vector magnitude
Angle := TTrigKit.VectorAngle(X1, Y1, X2, Y2); // Angle between vectors
```

## 📝 Logging Operations

### Getting Started with TidyKit.Logger

```pascal
// STEP 1: Choose how to set up your logger

// Option A: Use the global logger singleton (recommended for most applications)
uses
  TidyKit.Logger;
  
// Set up once at application startup
TLogger.CreateConsoleAndFileLogger('application.log', llInfo);

// Then use anywhere in your code through the Logger function
Logger.Info('Application started');


// Option B: Create your own logger instance (for specialized components)
var
  ComponentLogger: TLogger;
begin
  ComponentLogger := TLogger.CreateFileLogger('component.log', llDebug);
  ComponentLogger.Debug('Component initialized');
end;


// STEP 2: Choose the appropriate log level based on the information
Logger.Debug('Detailed debug info - only for development');     // Use during development
Logger.Info('Normal operational messages');                     // General information
Logger.Warning('Something unusual happened');                   // Potential issues
Logger.Error('An operation failed');                            // Recoverable errors
Logger.Fatal('Critical error - application cannot continue');   // Critical failures


// STEP 3: Include variables in log messages (two equivalent styles)
Logger.InfoFmt('User %s logged in from %s', ['john', '192.168.1.10']);  // Format style
Logger.Info('User %s logged in from %s', ['john', '192.168.1.10']);     // Same result


// STEP 4: Organize logs by component/feature (optional)
var
  UILogger, DBLogger: ILogContext;
begin
  // Create category-based loggers for better organization
  UILogger := Logger.CreateContext('UI');
  DBLogger := Logger.CreateContext('DB');
  
  UILogger.Info('Window created');
  DBLogger.Warning('Slow query detected: %s', ['SELECT * FROM large_table']);
end;


// STEP 5: Measure operation performance (optional)
procedure ProcessData;
var
  Timer: ITimedOperation;
begin
  // Create a timer that logs when it goes out of scope
  Timer := Logger.TimedBlock('Data processing');
  
  // Do your work...
  // When this procedure ends, it logs something like:
  // "Data processing completed in 235ms"
end;


// STEP 6: Clean up when finished (important!)
procedure Shutdown;
begin
  Logger.Info('Application shutting down');
  Logger.CloseLogFiles;  // Closes files and flushes any pending messages
end;

// Program shutdown best practices:
// - CloseLogFiles is sufficient for normal program termination
// - ResetInstance is NOT required at program shutdown
// - The logger will automatically clean up when your application terminates
// - For guaranteed data integrity, explicitly call CloseLogFiles
try
  // Your application code
finally
  Logger.CloseLogFiles;  // This is sufficient for cleanup
end;
```

### Common Scenarios

```pascal
// FOR DEVELOPMENT: Include detailed debug information
TLogger.CreateConsoleLogger(llDebug);
Logger.Debug('Connection status: %d, buffer size: %d', [status, size]);

// FOR PRODUCTION: Filter out debug noise
TLogger.CreateConsoleAndFileLogger('app.log', llInfo);
Logger.SetMinLogLevel(llWarning);  // Only show warnings and above

// FOR TROUBLESHOOTING: Show file and line information
TLogger.CreateDebugLogger;  // Special logger that includes source file info
Logger.Debug('Variable value: %d', [value]);  // Shows file:line in output

// FOR SECURITY EVENTS: Track login attempts, permission changes, etc.
AuditLogger := TLogger.CreateAuditLogger('security.log');
AuditLogger.Warning('Failed login attempt for user %s from %s', [username, ip]);

// FOR HIGH-VOLUME LOGGING: Use batch mode for performance
Logger.BeginBatch;
try
  for i := 1 to 1000 do
    Logger.Info('Processing item ' + IntToStr(i));
finally
  Logger.EndBatch;  // Writes all messages at once for better performance
end;

// FOR TESTING: Capture logs in memory
var
  MemSink: TMemorySink;
begin
  MemSink := TMemorySink.Create(100);  // Keep last 100 messages
  Logger.AddSink(MemSink);
  
  // Later, retrieve messages for assertions
  Assert(MemSink.GetMessages.Count > 0);
end;
```

### Advanced Use
```pascal
// Configure with method chaining
Logger
  .SetLogDestinations([ldConsole, ldFile])
  .SetMinLogLevel(llInfo)
  .SetDateTimeFormat('yyyy-mm-dd hh:nn:ss')
  .SetFormat('[%time] [%level] %message');
  
// Custom message format patterns
Logger.SetFormat('[%time] [%level] %message');                    // Default format
Logger.SetFormat('[%time] [%level] [%category] %message');        // With category
Logger.SetFormat('[%time] [%level] [%file:%line] %message');      // With source location
Logger.SetFormat('[%time] [%level] [Thread %threadid] %message'); // With thread ID

// Add log files with size limits
LogIndex := Logger.AddLogFile('app.log', 25 * 1024 * 1024);      // 25MB limit
LogIndex := Logger.AddDefaultLogFile('system');                   // Creates logs/system.log
LogIndex := Logger.AddDefaultLogFile('errors', 5 * 1024 * 1024); // 5MB error log

// Structured logging with key-value pairs
Logger.LogStructured(llInfo, 'User login', [
  NameValuePair('username', 'john_doe'),
  NameValuePair('ip_address', '192.168.1.10'),
  NameValuePair('success', True),
  NameValuePair('attempt', 3)
]);

// Type-specific value logging
Logger.LogValue('count', 42, llInfo);            // Integer value
Logger.LogValue('temperature', 98.6, llInfo);    // Double value
Logger.LogValue('is_active', True, llInfo);      // Boolean value
Logger.LogValue('username', 'john_doe', llInfo); // String value

// Custom sink management
Logger.AddSink(TConsoleSink.Create);
Logger.AddSink(TFileSink.Create('app.log'));
Logger.AddSink(TRotatingFileSink.Create('app.log', 1024*1024, 5)); // 1MB, keep 5 files
Logger.AddSink(TDailyFileSink.Create('app.log'));                  // Rotates daily
Logger.AddSink(TMemorySink.Create(100));                           // Keep last 100 messages

// Configuration from environment or file
Logger.ConfigureFromEnvironment;  // LOGGER_LEVEL, LOGGER_DESTINATIONS, etc.
Logger.LoadConfiguration('logger.ini');

// Instance management
ID := Logger.GetInstanceID;  // Get unique ID of current logger instance
TLogger.ResetInstance;       // Destroy and recreate singleton instance

// When to use ResetInstance:
// 1. Complete reconfiguration - when you need totally different settings
// 2. Between test cases - ensures test isolation
// 3. Application phase changes - switching from init to runtime logging
// 4. Final cleanup - to release ALL logger resources

// Error recovery - logger attempts to handle errors gracefully
try
  Logger.AddLogFile('/invalid/path/file.log');  // Will handle error and continue
except
  on E: Exception do
    WriteLn('Error handled: ', E.Message);
end;
```

## 📁 Archive Operations

```pascal
// Archive operations
TArchiveKit.CompressToZip('source.txt', 'archive.zip');                // Compress single file
TArchiveKit.CompressToZip('sourcedir', 'archive.zip', True);          // Compress recursively
TArchiveKit.CompressToZip('sourcedir', 'archive.zip', True, '*.txt'); // Compress only .txt files
TArchiveKit.DecompressFromZip('archive.zip', 'destdir');              // Extract all files
TArchiveKit.DecompressFromZip('archive.zip', 'destdir', '*.txt');     // Extract only .txt files

TArchiveKit.CompressToTar('source.txt', 'archive.tar');               // Create TAR with single file
TArchiveKit.CompressToTar('sourcedir', 'archive.tar', True);          // Create TAR recursively
TArchiveKit.CompressToTar('sourcedir', 'archive.tar', True, '*.txt'); // Create TAR with only .txt files
TArchiveKit.DecompressFromTar('archive.tar', 'destdir');              // Extract all files
TArchiveKit.DecompressFromTar('archive.tar', 'destdir', '*.txt');     // Extract only .txt files
```

## 🛠️ Error Handling

TidyKit uses module-specific exception classes for better error granularity:

```pascal
// Core exception (base class)
try
  // Generic operations
except
  on E: ETidyKitException do
    // Handle general TidyKit errors
end;

// Module-specific exceptions
try
  // JSON operations
  JsonValue := TJSON.Parse(JsonStr);
except
  on E: EJSONException do
    // Handle JSON-specific errors
end;

try
  // File operations
  TFileKit.CopyFile(Source, Dest);
except
  on E: EFileSystemError do
    // Handle filesystem-specific errors
end;

try
  // Matrix operations
  InvertedMatrix := TMatrixKit.Inverse(Matrix);
except
  on E: EMatrixError do
    // Handle matrix-specific errors (e.g., singular matrix)
end;

try
  // Statistical operations
  Mean := TStatsKit.Mean(Data);
except
  on E: EStatsError do
    // Handle statistics-specific errors (e.g., empty dataset)
end;

try
  // Financial operations
  IRR := TFinanceKit.InternalRateOfReturn(Investment, CashFlows);
except
  on E: EFinanceError do
    // Handle finance-specific errors (e.g., convergence failure)
end;

try
  // Cryptographic operations
  Encrypted := TCryptoKit.AES256EncryptCBC(Data, Key, IV);
except
  on E: ECryptoError do
    // Handle general crypto errors
  on E: EAESError do
    // Handle AES-specific errors
end;

try
  // HTTP operations
  Response := Http.Get(URL);
except
  on E: ERequestError do
    // Handle HTTP-specific errors
end;

try
  // Archive operations
  TArchiveKit.CompressToZip(Path, ZipFile);
except
  on E: EArchiveError do
    // Handle archive-specific errors
end;

try
  // Logger operations
  Logger.Info('Message');
except
  on E: ELoggerException do
    // Handle logger-specific errors
end;
```

This modular approach makes error handling more precise and maintainable.