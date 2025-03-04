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
  - [📁 Archive Operations](#-archive-operations)
  - [Logging Operations](#logging-operations)
    - [Basic Logging](#basic-logging)
      - [1. Console Logger](#1-console-logger)
      - [2. File Logger](#2-file-logger)
      - [3. Log Levels](#3-log-levels)
      - [4. Formatted Messages](#4-formatted-messages)
    - [Advanced Usage](#advanced-usage)
      - [1. Custom Configuration](#1-custom-configuration)
      - [2. Multiple Targets](#2-multiple-targets)
      - [3. File Rotation](#3-file-rotation)
    - [Memory Management](#memory-management)
      - [1. Interface References](#1-interface-references)
      - [2. Cleanup Sequence](#2-cleanup-sequence)
      - [3. NEVER Do This](#3-never-do-this)
    - [Thread Safety](#thread-safety)
      - [1. Thread-Safe Logging](#1-thread-safe-logging)
      - [2. Custom Target Thread Safety](#2-custom-target-thread-safety)
    - [Error Handling](#error-handling-2)
      - [1. Log Errors](#1-log-errors)
      - [2. Target Errors](#2-target-errors)
    - [Best Practices](#best-practices-1)
- [TidyKit.Log Quick Reference](#tidykitlog-quick-reference)
  - [Basic Logging](#basic-logging-1)
  - [Memory Management](#memory-management-1)
  - [Configuration](#configuration)
  - [Thread Safety](#thread-safety-1)
  - [Error Handling](#error-handling-3)

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
TFileKit.EnsureDirectory('path/to/file.txt');             // Create all parent directories

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
if TFileKit.IsSubPath('/usr/local', '/usr/local/bin') then ...            // Check if path is subpath

// File information
if TFileKit.Exists('file.txt') then ...                   // Check file exists
if TFileKit.DirectoryExists('dir') then ...               // Check directory exists
Size := TFileKit.GetSize('file.txt');                     // Get file size
Time := TFileKit.GetCreationTime('file.txt');             // Get creation time
Time := TFileKit.GetLastAccessTime('file.txt');           // Get last access time
Time := TFileKit.GetLastWriteTime('file.txt');            // Get last write time
Attrs := TFileKit.GetAttributes('file.txt');              // Get file attributes
if TFileKit.IsTextFile('file.txt') then ...              // Check if text file
Encoding := TFileKit.GetFileEncoding('file.txt');         // Get file encoding
if TFileKit.IsEmptyDirectory('dir') then ...             // Check if directory is empty

// Search operations
Results := TFileKit.SearchFiles('.', '*.txt', True);      // Search files recursively
Results := TFileKit.SearchFilesIn('dir', '*.txt', True);  // Search in specific dir
File := TFileKit.FindLastModifiedFile('.', '*.txt');      // Find newest file
File := TFileKit.FindFirstModifiedFile('.', '*.txt');     // Find oldest file
File := TFileKit.FindLargestFile('.', '*.txt');           // Find largest file
File := TFileKit.FindSmallestFile('.', '*.txt');          // Find smallest file

// System directories
Dir := TFileKit.GetUserDir;                               // Get user directory
Dir := TFileKit.GetCurrentDir;                            // Get current directory
Dir := TFileKit.GetTempDir;                               // Get temp directory

// Temporary files
TempFile := TFileKit.CreateTempFile('prefix_');           // Create temp file
TempDir := TFileKit.CreateTempDirectory('prefix_');       // Create temp directory

// Symbolic link operations
TFileKit.CreateSymLink('target.txt', 'link.txt');         // Create file symlink
TFileKit.CreateSymLink('target_dir', 'link_dir', True);   // Create directory symlink
TFileKit.DeleteSymLink('link.txt');                       // Delete symlink
Path := TFileKit.ResolveSymLink('link.txt');              // Get target path
if TFileKit.IsSymLink('link.txt') then ...               // Check if path is symlink

// File locking
if TFileKit.LockFile('file.txt') then                    // Lock file
try
  // Work with file
finally
  TFileKit.UnlockFile('file.txt');                       // Unlock file
end;
if TFileKit.IsFileLocked('file.txt') then ...           // Check if file is locked

// File validation and sanitization
if TFileKit.IsValidFileName('file.txt') then ...         // Check if filename is valid
Name := TFileKit.SanitizeFileName('file*.txt');          // Sanitize filename
Path := TFileKit.MakeValidPath('/path//to/./file');      // Make path valid
if TFileKit.IsPathTooLong('very/long/path') then ...     // Check if path is too long

// Directory information
Info := TFileKit.GetDirectoryInfo('dir');                // Get directory statistics
WriteLn('Files: ', Info.FileCount);                      // Number of files
WriteLn('Directories: ', Info.DirectoryCount);           // Number of subdirectories
WriteLn('Total size: ', Info.TotalSize);                 // Total size in bytes
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
// Matrix creation
M := TMatrixKit.CreateMatrix(Rows, Cols);        // Create empty matrix
I := TMatrixKit.Identity(Size);                  // Create identity matrix
Z := TMatrixKit.Zeros(Rows, Cols);               // Create zero matrix
O := TMatrixKit.Ones(Rows, Cols);                // Create matrix of ones

// Basic operations
C := TMatrixKit.Add(A, B);                       // Matrix addition
D := TMatrixKit.Subtract(A, B);                  // Matrix subtraction
E := TMatrixKit.Multiply(A, B);                  // Matrix multiplication
F := TMatrixKit.ScalarMultiply(A, 2.0);          // Scalar multiplication

// Matrix transformations
T := TMatrixKit.Transpose(A);                    // Matrix transpose

// Matrix properties
Det := TMatrixKit.Determinant(A);                // Calculate determinant
Tr := TMatrixKit.Trace(A);                       // Calculate trace

// Helper functions
Rows := TMatrixKit.GetRows(A);                   // Get number of rows
Cols := TMatrixKit.GetCols(A);                   // Get number of columns
IsSquare := TMatrixKit.IsSquare(A);              // Check if matrix is square

// Note: Features like matrix rank, inversion, LU and QR decomposition
// are planned for future implementation
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

## Logging Operations

### Basic Logging

#### 1. Console Logger
```pascal
var
  Logger: ILogger;
begin
  Logger := ConsoleLogger;
  Logger.Info('Hello, World!');
end;
```

#### 2. File Logger
```pascal
var
  Logger: ILogger;
begin
  Logger := FileLogger('app.log');
  Logger.Info('Application started');
end;
```

#### 3. Log Levels
```pascal
Logger.Debug('Debug info');
Logger.Info('Information');
Logger.Warning('Warning message');
Logger.Error('Error occurred');
Logger.Fatal('Fatal error');
```

#### 4. Formatted Messages
```pascal
Logger.Info('Count: %d, Name: %s', [42, 'Test']);
```

### Advanced Usage

#### 1. Custom Configuration
```pascal
var
  Logger: ILogger;
  LogKit: TLogKit;
begin
  // Create with direct reference
  LogKit := TLogKit.Create;
  Logger := LogKit;  // Interface takes ownership
  
  // Configure
  LogKit.AddTarget(TFileTarget.Create('app.log'))
       .SetMinLevel(llWarning)
       .Enable;
       
  // Use
  Logger.Warning('Important message');
  
  // Cleanup
  LogKit.Shutdown;
  Logger := nil;
  LogKit := nil;
end;
```

#### 2. Multiple Targets
```pascal
LogKit.AddTarget(TFileTarget.Create('app.log'))
     .AddTarget(TConsoleTarget.Create)
     .Enable;
```

#### 3. File Rotation
```pascal
var
  Target: TFileTarget;
begin
  Target := TFileTarget.Create('app.log');
  Target.SetMaxSize(10 * 1024 * 1024)  // 10MB
        .SetRotateCount(5);            // Keep 5 backups
  
  LogKit.AddTarget(Target).Enable;
end;
```

### Memory Management

#### 1. Interface References
```pascal
var
  Logger: ILogger;
  Target: ILogTarget;
  Kit: TLogKit;
```

#### 2. Cleanup Sequence
```pascal
// 1. Shutdown
LogKit.Shutdown;

// 2. Clear references in order
Logger := nil;
LogKit := nil;
Target := nil;
```

#### 3. NEVER Do This
```pascal
// DON'T: Type cast interface
TLogKit(Logger).AddTarget(Target);

// DON'T: Manual free
Logger.Free;  // WRONG!
Target.Free;  // WRONG!

// DON'T: Keep only direct reference
var
  Logger: TLogKit;  // WRONG!
```

### Thread Safety

#### 1. Thread-Safe Logging
```pascal
// Safe in any thread
Logger.Info('Message from thread');
```

#### 2. Custom Target Thread Safety
```pascal
type
  TMyTarget = class(TInterfacedObject, ILogTarget)
  private
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteLog(const AEntry: TLogEntry);
  end;

procedure TMyTarget.WriteLog(const AEntry: TLogEntry);
begin
  FLock.Enter;
  try
    // Thread-safe write
  finally
    FLock.Leave;
  end;
end;
```

### Error Handling

#### 1. Log Errors
```pascal
try
  // Some code
except
  on E: Exception do
    Logger.Error('Error: %s', [E.Message]);
end;
```

#### 2. Target Errors
```pascal
type
  TMyTarget = class(TInterfacedObject, ILogTarget)
  public
    procedure WriteLog(const AEntry: TLogEntry);
  end;

procedure TMyTarget.WriteLog(const AEntry: TLogEntry);
begin
  try
    // Risky operation
  except
    // Silently continue - don't raise in logging
  end;
end;
```

### Best Practices

1. Use interface references by default
2. Keep direct references only when needed
3. Always call Shutdown before clearing references
4. Use appropriate log levels
5. Implement thread-safe custom targets
6. Handle target errors gracefully
7. Use formatted messages for complex output
8. Consider log rotation for long-running apps

# TidyKit.Log Quick Reference

## Basic Logging
```pascal
// Quick console logging
Logger := ConsoleLogger;
Logger.Info('Message');

// Quick file logging
Logger := FileLogger('app.log');
Logger.Info('Message');

// Log levels
Logger.Debug('Debug info');
Logger.Info('Info message');
Logger.Warning('Warning');
Logger.Error('Error');
Logger.Fatal('Fatal');

// Format strings
Logger.Info('Count: %d', [42]);
```

## Memory Management
```pascal
// CORRECT: Interface vars
var
  Logger: ILogger;
  Target: ILogTarget;

// CORRECT: Cleanup order
LogKit.Shutdown;
Logger := nil;
LogKit := nil;
Target := nil;

// WRONG: Never do these
TLogKit(Logger).AddTarget(Target);  // No type casting
Logger.Free;                        // No manual free
var Logger: TLogKit;               // No direct class vars
```

## Configuration
```pascal
// Basic setup
Logger := TLogKit.Create
  .AddTarget(TFileTarget.Create('app.log'))
  .Enable;

// Multiple targets
Logger := TLogKit.Create
  .AddTarget(TFileTarget.Create('app.log'))
  .AddTarget(TConsoleTarget.Create)
  .Enable;

// Set minimum level
Logger := TLogKit.Create
  .SetMinLevel(llWarning)
  .Enable;

// File rotation (complete setup)
var
  Target: TFileTarget;
  Logger: ILogger;
begin
  // Configure target
  Target := TFileTarget.Create('app.log')
    .SetMaxSize(10 * 1024 * 1024)  // 10MB
    .SetRotateCount(5);            // 5 backups

  // Create and enable logger with target
  Logger := TLogKit.Create
    .AddTarget(Target)
    .Enable;
end;
```

## Thread Safety
```pascal
// Thread-safe by default
Logger.Info('Safe in any thread');

// Custom target thread safety
type
  TMyTarget = class(TInterfacedObject, ILogTarget)
  private
    FLock: TCriticalSection;
  public
    procedure WriteLog(const AEntry: TLogEntry);
  end;

procedure TMyTarget.WriteLog(const AEntry: TLogEntry);
begin
  FLock.Enter;
  try
    // Write log
  finally
    FLock.Leave;
  end;
end;
```

## Error Handling
```pascal
// Log exceptions
try
  // Code
except
  on E: Exception do
    Logger.Error('Error: %s', [E.Message]);
end;

// Silent target errors
procedure TMyTarget.WriteLog(const AEntry: TLogEntry);
begin
  try
    // Write log
  except
    // Silently continue
  end;
end;
```