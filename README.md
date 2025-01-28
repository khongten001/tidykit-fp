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
    - [🎯 Core Features](#-core-features)
    - [🗂️ FileSystem Operations](#️-filesystem-operations)
    - [📝 String Operations](#-string-operations)
    - [📅 DateTime Operations](#-datetime-operations)
    - [🔒 Cryptographic Operations](#-cryptographic-operations)
      - [Hashing Functions](#hashing-functions)
      - [Encoding](#encoding)
      - [Encryption Algorithms](#encryption-algorithms)
      - [Security Best Practices](#security-best-practices)
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
    - [🔒 Cryptographic Operations](#-cryptographic-operations-1)
  - [📖 Documentation](#-documentation)
  - [🧪 Unit Testing](#-unit-testing)
  - [📚 Examples](#-examples)
  - [🤝 Contributing](#-contributing)
  - [📝 License](#-license)
  - [🙏 Acknowledgments](#-acknowledgments)
  - [📞 Contact](#-contact)

## ✅ TODO

- [ ] Add comprehensive documentation
  - [ ] Add detailed API reference
  - [ ] Add best practices / cookbook guide
  - [ ] Add troubleshooting guide  

- [ ] Expand test coverage
  - [ ] Add more unit tests
  - [ ] Add edge case tests
  

## ✨ Features

### 🎯 Core Features
- FPC 3.2.2 Compatible: No inline var, anonymous functions, or lambda
- Cross-Platform: Works on Windows, Linux, macOS, and FreeBSD
- Static Functions: No instance management or memory leaks
- Memory Safe: Proper resource management
- Exception Handling: Custom exception types for better error handling
- Consistent API: Similar patterns across all modules
- **Partial Symbolic Link Support:** Detects symbolic links but lacks full manipulation capabilities

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
- Basic Archive operations:
  - ZIP file creation and extraction (using `zipper` unit)
    - Single file compression
    - Directory compression (recursive and non-recursive)
    - Pattern-based file selection (e.g., '*.txt', '*.dat')
    - Selective file extraction
    - Directory structure preservation
  - TAR file creation and extraction (using `libtar` unit)
    - Single file archiving
    - Directory archiving (recursive and non-recursive)
    - Pattern-based file selection (e.g., '*.txt', '*.dat')
    - Directory structure preservation with explicit entries
    - Sequential access to archive contents

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

### 🔒 Cryptographic Operations

The `TCryptoKit` module provides essential cryptographic functionality using FPC's RTL libraries:

#### Hashing Functions
- MD5 hashing
- SHA1 hashing

```pascal
hash := TCryptoKit.MD5Hash('Hello, World!');
hash := TCryptoKit.SHA1Hash('Hello, World!');
```

#### Encoding
- Base64 encoding/decoding

```pascal
encoded := TCryptoKit.Base64Encode('Hello, World!');
decoded := TCryptoKit.Base64Decode(encoded);
```

#### Encryption Algorithms

1. **Simple XOR Encryption**
   - Basic XOR-based encryption/decryption
   - Suitable for basic data obfuscation
   - Not recommended for sensitive data

```pascal
encrypted := TCryptoKit.XORCrypt(plainText, key);
decrypted := TCryptoKit.XORCrypt(encrypted, key); // Same operation decrypts
```

2. **Blowfish**
   - Block cipher with variable key length (up to 56 bytes)
   - Suitable for legacy systems
   - Includes automatic padding
   - Output is Base64 encoded for safe storage/transmission

```pascal
encrypted := TCryptoKit.BlowfishCrypt(plainText, key, bmEncrypt);
decrypted := TCryptoKit.BlowfishCrypt(encrypted, key, bmDecrypt);
```

#### Security Best Practices

1. Key Management:
   - Use strong, random keys
   - Never reuse keys
   - Store keys securely

2. Algorithm Selection:
   - Use XOR encryption only for basic data obfuscation
   - Blowfish is suitable for legacy systems
   - Consider modern alternatives for new applications

3. Additional Security:
   - Keep encryption keys separate from other application data
   - Use strong random number generators for key generation
   - Consider adding additional authentication mechanisms

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
end;
```

### 🔒 Cryptographic Operations
```pascal
uses
  TidyKit;

var
  PlainText, Encrypted, Decrypted: string;
  Key: string;
begin
  PlainText := 'Secret message';
  Key := 'MySecretKey123456';

  // Hashing
  WriteLn('MD5: ', TCryptoKit.MD5Hash(PlainText));
  WriteLn('SHA1: ', TCryptoKit.SHA1Hash(PlainText));

  // Base64 encoding/decoding
  Encrypted := TCryptoKit.Base64Encode(PlainText);
  WriteLn('Base64 encoded: ', Encrypted);
  Decrypted := TCryptoKit.Base64Decode(Encrypted);
  WriteLn('Base64 decoded: ', Decrypted);

  // Simple XOR encryption (basic, not for sensitive data)
  Encrypted := TCryptoKit.XORCrypt(PlainText, Key);
  Decrypted := TCryptoKit.XORCrypt(Encrypted, Key);
  WriteLn('XOR decrypted: ', Decrypted);

  // Blowfish encryption (legacy support)
  Encrypted := TCryptoKit.BlowfishCrypt(PlainText, Key, bmEncrypt);
  Decrypted := TCryptoKit.BlowfishCrypt(Encrypted, Key, bmDecrypt);
  WriteLn('Blowfish decrypted: ', Decrypted);

  // Error handling example
  if (Encrypted = '') or (Decrypted = '') then
    WriteLn('Encryption or decryption failed');
    
  // Key length handling for Blowfish
  if Length(Key) > 56 then
    SetLength(Key, 56);  // Truncate to maximum allowed length
end;
```

## 📖 Documentation

See the [cheat-sheet.md](docs/cheat-sheet.md) for a quick reference of the library's features.  

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
Inspired by R's tidyverse
Built with Free Pascal and Lazarus IDE

## 📞 Contact
Your Name - ikelaiah

Project Link: https://github.com/ikelaiah/tidykit-fp
