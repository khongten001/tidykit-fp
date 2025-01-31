# 📋 Cheat Sheet

This is a quick reference for the library's features.

## Table of Contents

- [File System Operations](#file-system-operations)
  - [Basic File Operations](#basic-file-operations)
  - [Directory Operations](#directory-operations) 
  - [File Listing](#file-listing)
  - [Directory Listing](#directory-listing)
  - [Path Operations](#path-operations)
  - [File Information](#file-information)


## File System Operations

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

// Archive operations - ZIP
TFileKit.CompressToZip('source.txt', 'archive.zip');                // Compress single file
TFileKit.CompressToZip('sourcedir', 'archive.zip');                 // Compress directory (non-recursive)
TFileKit.CompressToZip('sourcedir', 'archive.zip', True);          // Compress recursively
TFileKit.CompressToZip('sourcedir', 'archive.zip', True, '*.txt'); // Compress only .txt files
TFileKit.DecompressFromZip('archive.zip', 'destdir');              // Extract all files
TFileKit.DecompressFromZip('archive.zip', 'destdir', '*.txt');     // Extract only .txt files

// Archive operations - TAR
TFileKit.CompressToTar('source.txt', 'archive.tar');               // Create TAR with single file
TFileKit.CompressToTar('sourcedir', 'archive.tar');                // Create TAR from directory (non-recursive)
TFileKit.CompressToTar('sourcedir', 'archive.tar', True);          // Create TAR recursively
TFileKit.CompressToTar('sourcedir', 'archive.tar', True, '*.txt'); // Create TAR with only .txt files
TFileKit.DecompressFromTar('archive.tar', 'destdir');              // Extract all files
TFileKit.DecompressFromTar('archive.tar', 'destdir', '*.txt');     // Extract only .txt files
```

## String operations

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

## DateTime Operations

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
// MD5 (legacy, not recommended for security)
hash := TCryptoKit.MD5Hash('text');  // 128-bit output

// SHA-1 (legacy, not recommended for security)
hash := TCryptoKit.SHA1Hash('text'); // 160-bit output

// SHA-256 (recommended for general use)
hash := TCryptoKit.SHA256Hash('text'); // 256-bit output

// SHA-512 (recommended for 64-bit systems)
hash := TCryptoKit.SHA512Hash('text'); // 512-bit output

// SHA-512/256 (FIPS 180-4 compliant, recommended for 64-bit systems)
hash := TCryptoKit.SHA512_256Hash('text'); // 256-bit output
```

### Base64 Encoding/Decoding

```pascal
// Encoding
encoded := TCryptoKit.Base64Encode('Hello, World!');
// Result: 'SGVsbG8sIFdvcmxkIQ=='

// Decoding
decoded := TCryptoKit.Base64Decode('SGVsbG8sIFdvcmxkIQ==');
// Result: 'Hello, World!'

// Error handling
if (encoded = '') or (decoded = '') then
  WriteLn('Encoding/decoding failed');
```

### XOR Encryption (Basic)

```pascal
// Basic encryption (not for sensitive data)
const
  Key = 'MySecretKey123';
var
  Text: string = 'Hello, World!';
  Encrypted, Decrypted: string;
begin
  // Encrypt
  Encrypted := TCryptoKit.XORCrypt(Text, Key);
  
  // Decrypt (same operation)
  Decrypted := TCryptoKit.XORCrypt(Encrypted, Key);
  
  // Key wrapping is automatic for long texts
  LongText := 'Very long text...';
  Encrypted := TCryptoKit.XORCrypt(LongText, 'short key');
end;
```

### Blowfish Encryption (Legacy Support)

```pascal
// Encryption
const
  Key = 'MySecretKey123456'; // Up to 56 bytes
var
  Text: string = 'Hello, World!';
  Encrypted, Decrypted: string;
begin
  // Encrypt
  Encrypted := TCryptoKit.BlowfishCrypt(Text, Key, bmEncrypt);
  
  // Decrypt
  Decrypted := TCryptoKit.BlowfishCrypt(Encrypted, Key, bmDecrypt);
  
  // Key length handling
  if Length(Key) > 56 then
    SetLength(Key, 56);  // Truncate to maximum allowed length
end;
```

### Common Use Cases

1. Password Hashing (use SHA-256 or SHA-512/256):
```pascal
userPasswordHash := TCryptoKit.SHA512_256Hash(password + salt);
```

2. File Checksum:
```pascal
fileContent := TFileKit.ReadFile('myfile.txt');
checksum := TCryptoKit.SHA256Hash(fileContent);
```

3. Data Integrity Check:
```pascal
originalHash := TCryptoKit.SHA512_256Hash(originalData);
receivedHash := TCryptoKit.SHA512_256Hash(receivedData);
isValid := originalHash = receivedHash;
```

4. Safe String Storage:
```pascal
// Store sensitive string in Base64
encoded := TCryptoKit.Base64Encode(sensitiveString);
// Later, decode it
decoded := TCryptoKit.Base64Decode(encoded);
```

### Best Practices

1. Hash Function Selection:
   - New applications: Use SHA-256 or SHA-512/256
   - Legacy compatibility: MD5 or SHA-1 only if required
   - Performance on 64-bit: Prefer SHA-512/256

2. Key Management:
   - Generate strong random keys
   - Never store keys in plaintext
   - Never reuse keys across systems
   - Rotate keys periodically

3. Algorithm Usage:
   - XOR: Only for basic data obfuscation
   - Blowfish: Only for legacy system compatibility
   - Base64: Safe string encoding, not encryption
   - Modern hashes: Always use salt for passwords

4. Error Handling:
   ```pascal
   try
     hash := TCryptoKit.SHA512_256Hash(data);
     if hash = '' then
       raise Exception.Create('Hashing failed');
   except
     on E: Exception do
       // Handle error
   end;
   ```

### Security Notes

1. **Hash Collisions**:
   - MD5: Known collisions, avoid for security
   - SHA-1: Theoretically broken, avoid for security
   - SHA-256/SHA-512: No known practical collisions

2. **Performance**:
   - SHA-512/256: Better on 64-bit systems
   - SHA-256: Better on 32-bit systems
   - Base64: Minimal overhead
   - Blowfish: Moderate CPU usage

3. **Output Sizes**:
   - MD5: 32 hex chars (128 bits)
   - SHA-1: 40 hex chars (160 bits)
   - SHA-256: 64 hex chars (256 bits)
   - SHA-512: 128 hex chars (512 bits)
   - SHA-512/256: 64 hex chars (256 bits)