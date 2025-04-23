# 🧰 TidyKit

[![FPC](https://img.shields.io/badge/Free%20Pascal-3.2.2-blue.svg)](https://www.freepascal.org/)
[![Lazarus](https://img.shields.io/badge/Lazarus-3.6+-blue.svg)](https://www.lazarus-ide.org/)
[![License](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE.md)
[![Documentation](https://img.shields.io/badge/Docs-Available-brightgreen.svg)](docs/)
[![Tests](https://img.shields.io/badge/Tests-Passing-brightgreen.svg)](tests/)
[![Status](https://img.shields.io/badge/Status-Development-yellow.svg)]()
[![Version](https://img.shields.io/badge/Version-0.1.5-blueviolet.svg)]()


A comprehensive toolkit providing essential utilities for development in Free Pascal.  
**Streamlining your Pascal programming experience with reliable tools.**

> [!WARNING]
> ⚠️ This library is under active development and is not yet ready for production use. APIs may change without notice.

## 🌟 Why TidyKit?

- **All-in-One Solution**: Stop hunting for separate libraries - TidyKit provides everything in one package
- **Modern Pascal**: Designed with modern programming practices while maintaining FPC 3.2.2 compatibility
- **Thoroughly Tested**: Extensive test suite ensures reliability and stability
- **Cross-Platform**: Tested on both Windows and Ubuntu Linux environments
- **Well-Documented**: Every component has detailed documentation with examples
- **Memory Management**: Mixed approach with interface-based automatic reference counting for complex objects and traditional memory management for simpler operations
- **Evolving API**: Currently transitioning toward a simpler API design (see [Roadmap](#️-roadmap))

## 📑 Table of Contents 
- [🧰 TidyKit](#-tidykit)
  - [🌟 Why TidyKit?](#-why-tidykit)
  - [📑 Table of Contents](#-table-of-contents)
  - [🏗️ Architectural Patterns](#️-architectural-patterns)
    - [Example: Static Class Functions (Stable Pattern)](#example-static-class-functions-stable-pattern)
    - [Example: Factory/Interface (Target Pattern for FS, JSON, Logger, Request, Matrices)](#example-factoryinterface-target-pattern-for-fs-json-logger-request-matrices)
  - [✨ Features](#-features)
  - [💻 Installation (Lazarus IDE)](#-installation-lazarus-ide)
  - [💻 Installation (General)](#-installation-general)
  - [📝 Library Usage](#-library-usage)
  - [🚀 Quick Start](#-quick-start)
    - [📝 String Operations](#-string-operations)
    - [📂 File System Operations](#-file-system-operations)
    - [📅 DateTime Operations](#-datetime-operations)
    - [🔄 JSON Operations](#-json-operations)
    - [📝 Logging Operations](#-logging-operations)
    - [🌐 HTTP Request Operations](#-http-request-operations)
    - [🔐 Crypto Operations](#-crypto-operations)
    - [📦 Archive Operations](#-archive-operations)
    - [📈 Statistical Operations](#-statistical-operations)
    - [🔢 Matrix Operations](#-matrix-operations)
    - [📐 Trigonometric Operations](#-trigonometric-operations)
    - [💰 Financial Operations](#-financial-operations)
  - [📖 System Requirements](#-system-requirements)
    - [Tested Environments](#tested-environments)
    - [Dependencies](#dependencies)
    - [Build Requirements](#build-requirements)
  - [📚 Documentation](#-documentation)
  - [📊 Real-World Examples](#-real-world-examples)
  - [💬 Community \& Support](#-community--support)
  - [⚠️ Known Limitations](#️-known-limitations)
  - [✅ Testing](#-testing)
  - [🤝 Contributing](#-contributing)
  - [⚖️ License](#️-license)
  - [🙏 Acknowledgments](#-acknowledgments)
  - [🗺️ Roadmap](#️-roadmap)
    - [Planned for v0.2.0 (Q3 2025) - Simplify API](#planned-for-v020-q3-2025---simplify-api)
    - [Planned for v0.3.0 (Q4 2025) - Examples \& Refinements](#planned-for-v030-q4-2025---examples--refinements)
    - [Future Goals](#future-goals)

## 🏗️ Architectural Patterns

TidyKit currently uses a mix of architectural patterns. We are actively working towards a more simplified API for release **v0.2.0**.

**Current & Target State (v0.2.0):**

| Pattern | Target Modules (v0.2.0) | Characteristics | Memory Management | Status |
|---------|-------------------------|-----------------|-------------------|--------|
| **Factory/Interface** | `TidyKit.FS`, `TidyKit.JSON`, `TidyKit.Logger`, `TidyKit.Request`, `TidyKit.Matrices`* | <ul><li>Object-oriented design</li><li>Testability (Mocking)</li><li>Flexibility</li><li>Fluent API</li></ul> | Automatic reference counting via interfaces | `FS` (completed), `JSON`, `Logger` (require naming consistency). `Request`, `Matrices` planned for v0.2.0. |
| **Static Class Functions** | `TidyKit.DateTime`, `TidyKit.Strings`, `TidyKit.Archive`, `TidyKit.Crypto.*`, `TidyKit.Math.*` (excluding Matrices) | <ul><li>Procedural-style API</li><li>Simple usage for stateless utilities</li><li>No instance creation</li></ul> | No manual management needed | Stable |

\* `TidyKit.Matrices` is currently Class/Interface based and is planned to transition to Factory/Interface for consistency in v0.2.0.

> [!IMPORTANT]
> The goal for **v0.2.0** is to finalise this structure, providing a clear distinction: Factory/Interface for components interacting with external state or benefiting from mocking, and Static Methods for stateless utilities.

### Example: Static Class Functions (Stable Pattern)

```pascal
// TidyKit.Strings - No instance needed, simple function calls
IsValid := TStringKit.MatchesPattern('test@example.com', TStringKit.REGEX_EMAIL);
Padded := TStringKit.PadLeft('123', 5, '0'); // Result: '00123'
```

### Example: Factory/Interface (Target Pattern for FS, JSON, Logger, Request, Matrices)

```pascal
// TidyKit.FS - Using the factory and interface
var
  FS: IFileKit;
  Content: string;
begin
  FS := TFSFactory.CreateFileKit; // Factory creates the object
  FS.WriteTextFile('example.txt', 'Hello via Interface');
  Content := FS.ReadTextFile('example.txt');
  // FS interface goes out of scope, memory managed automatically
end;

// TidyKit.JSON - Existing Factory/Interface pattern
var
  Person: IJSONObject;
begin
  Person := TJSONFactory.CreateObject; // Factory method
  Person.Add('name', 'Jane Doe');
  // Person interface manages memory
end;
```

## ✨ Features

- 📝 **String Operations**
  - String manipulation and transformations
  - Case conversion and formatting
  - Pattern matching and validation
  - Unicode support
  - String comparison and searching
  - Text encoding/decoding

- 📂 **File System Operations**
  - File and directory manipulation
  - Path operations
  - File searching and filtering

- 📦 **Archive Operations**
  - ZIP file compression and decompression
  - TAR file creation and extraction
  - Pattern-based file filtering
  - Recursive directory handling

- 🔐 **Cryptography**
  - SHA3 implementation
  - SHA2 family (SHA-256, SHA-512, SHA-512/256)
  - AES-256 encryption
    - CBC and CTR modes
    - High-level interface with automatic Base64 encoding
    - Low-level interface with raw binary operations
    - Configurable padding modes (PKCS7 or None)
  - Secure hashing
  - Encryption utilities
  - Base64 encoding/decoding
  - Legacy support (MD5, SHA1, Blowfish)

- 🌐 **Network Operations**
  - HTTP client
  - Request handling
  - Response parsing

- 📊 **Math Operations**
  - 📈 Statistical Analysis
    - Basic statistics (mean, median, mode, range)
    - Variance and standard deviation (population and sample)
    - Distribution measures (skewness, kurtosis)
    - Correlation (Pearson, Spearman)
    - Advanced means (geometric, harmonic, trimmed)
    - Robust statistics (MAD, Huber M-estimator)
    - Hypothesis testing (t-test, Mann-Whitney U)
    - Effect size measures (Cohen's d, Hedges' g)
    - Bootstrap confidence intervals
  - 💰 Financial Mathematics
    - Time value of money (PV, FV)
    - Investment analysis (NPV, IRR)
    - Depreciation calculations
    - Return metrics (ROI, ROE)
  - 🔢 Matrix Operations
    - Basic operations (add, subtract, multiply)
    - Matrix creation (zeros, ones, identity)
    - Matrix transpose and inverse
    - Determinant and trace calculation
    - Matrix decompositions (LU, QR, Eigen, SVD, Cholesky)
    - Matrix properties (rank, condition number)
    - Matrix norms (one, infinity, Frobenius)
    - Special matrices (diagonal, symmetric, random, Hilbert, Toeplitz, Vandermonde)
    - Element-wise operations
    - Submatrix operations
    - Linear system solving (direct and iterative methods)
    - Matrix functions (exponential, power with support for fractional powers)
    - Pseudoinverse for non-square matrices
    - Vector operations (dot product, cross product, normalization)
    - Statistical operations (mean, covariance, correlation)
    - Advanced eigenvalue computation with PowerMethod
    - Memory-efficient sparse matrix implementation
    - Comprehensive error handling
    - Memory-safe interface design with automatic reference counting
    - String representations of matrices and decompositions
  - 📐 Trigonometry
    - Basic functions (sin, cos, tan, sec, csc, cot)
    - Inverse functions (arcsin, arccos, arctan, arctan2)
    - Hyperbolic functions (sinh, cosh, tanh)
    - Inverse hyperbolic functions (arcsinh, arccosh, arctanh)
    - Angle conversions (degrees, radians, grads)
    - Angle normalization
    - Triangle calculations (area, perimeter, radii)
    - Circle sector and segment calculations
    - Vector operations
  - ✅ All calculations use Double precision (64-bit) for accuracy

- 🔄 **JSON Operations**
  - Interface-based JSON manipulation with automatic memory management
  - Property order preservation in JSON objects
  - Full Unicode support
    - Unicode escape sequence parsing (\uXXXX)
    - UTF-8/16 character handling
  - Support for all JSON data types
    - Objects with ordered properties
    - Arrays with type-safe elements
    - Strings with proper escaping
    - Numbers (both integer and floating-point)
    - Booleans (true/false)
    - Null (singleton implementation)
  - Output formatting
    - Pretty printing with configurable indentation
    - Compact output for storage/transmission
  - Memory safety
    - Automatic reference counting through interfaces
    - Safe singleton management for null values
    - Proper cleanup of nested structures
  - Thoroughly tested with 17 comprehensive test cases

- 📝 **Logging Operations**
  - Easy to use logging system with multiple output destinations
  - Configurable log levels (Debug, Info, Warning, Error, Fatal)
  - Console and file output with automatic coloring
  - File rotation based on size
  - Multiple log file support
  - Category-based logging for better organization
  - Automatic context management with reference counting
  - Format string support for convenient message formatting
  - Thread-safety considerations for multi-threaded applications
  - Singleton pattern with unique instance tracking
  - Method chaining for fluent configuration
  - Error recovery to prevent logging failures from crashing the application
  - Default log directory creation
  - Extensible sink architecture with built-in implementations
  - Pattern-based message formatting
  - Structured logging for key-value data
  - Performance timing capabilities
  - Batch logging for improved performance
  - Environment and file-based configuration
  - Specialized logger factory methods
  - Thoroughly tested with 34 comprehensive test cases


## 💻 Installation (Lazarus IDE)

1. Clone the repository:

```bash
git clone https://github.com/ikelaiah/tidykit-fp
```

2. Open / start a new project in Lazarus IDE

3. Go to `Package` → `Open Package File (.lpk)...`

4. Navigate to the TidyKit packages in the `packages/lazarus/` folder and select `TidyKit.lpk`

5. In the package window that opens, click `Compile`

6. Click `Use → Add to Project` to install the package

The TidyKit package is now ready to use in your Lazarus project.


## 💻 Installation (General)

1. Clone the repository:

```bash
git clone https://github.com/ikelaiah/tidykit-fp
```

2. Add the source directory to your project's search path.


## 📝 Library Usage

Add either the all-inclusive TidyKit unit or choose specific units you need:

```pascal
// Option 1: All-inclusive unit (includes all functionality)
uses
  TidyKit;

// Option 2: Choose specific units based on your needs
uses
  // String manipulation unit
  TidyKit.Strings,           // String operations
  
  // File system operations
  TidyKit.FS,                // File system operations
  
  // Date Time manipulation unit
  TidyKit.DateTime,          // Date Time operations
  
  // JSON functionality
  TidyKit.JSON,              // All JSON functionality
  
  // Logging functionality
  TidyKit.Logger,            // Easy to use logging system
  
  // Network units
  TidyKit.Request,           // HTTP client with simple and advanced features
  
  // Cryptography units
  TidyKit.Crypto,            // Base crypto operations
  TidyKit.Crypto.SHA2,       // SHA2 implementation
  TidyKit.Crypto.SHA3,       // SHA3 implementation
  TidyKit.Crypto.AES256,     // AES-256 implementation
  
  // Archive operations
  TidyKit.Archive,           // Archive operations
  
  // Math-related units
  TidyKit.Math,              // Base math types and operations
  TidyKit.Math.Stats,        // Statistical calculations
  TidyKit.Math.Matrices,     // Matrix operations
  TidyKit.Math.Trigonometry, // Trigonometric functions
  TidyKit.Math.Finance;      // Financial mathematics
```

Choose Option 1 if you want to include all functionality with a single unit. This is convenient but may increase compilation time and executable size.

Choose Option 2 if you want to optimize your application by including only the specific functionality you need. This approach:
- ⚡ Reduces compilation time
- 📦 Minimizes executable size
- 🔍 Makes dependencies more explicit
- 🔧 Improves code maintainability

Note: Some units may have interdependencies. The compiler will inform you if additional units need to be included.

## 🚀 Quick Start

### 📝 String Operations
```pascal
var
  Text: string;
begin
  // Email validation
  if TStringKit.MatchesPattern('user@example.com', '^[\w\.-]+@[\w\.-]+\.\w+$') then
    WriteLn('Valid email');
    
  // Format phone number
  Text := TStringKit.PadLeft('5551234', 10, '0');  // Returns '0005551234'
  
  // Clean user input
  Text := TStringKit.CollapseWhitespace('  Hello    World  ');  // Returns 'Hello World'
  
  // Format product code
  Text := TStringKit.PadCenter('A123', 8, '-');  // Returns '--A123---'
end;
```

### 📂 File System Operations
```pascal
var
  FileKit: IFileKit;
begin
  // Create FileKit instance using factory
  FileKit := TFSFactory.CreateFileKit;
  
  // Basic file operations
  FileKit.WriteTextFile('example.txt', 'Hello World');
  Content := FileKit.ReadTextFile('example.txt');
  
  // Directory operations
  FileKit.CreateDirectory('new_folder');
  FileKit.EnsureDirectory('path/to/folder');
  
  // List files with pattern matching
  Files := FileKit.ListFiles('src', '*.pas', True, fsName);
  
  // Get file attributes
  Attrs := FileKit.GetAttributes('example.txt');
  WriteLn(Format('Read-only: %s', [BoolToStr(Attrs.ReadOnly, True)]));
  
  // File manipulation
  FileKit.CopyFile('source.txt', 'dest.txt');
  FileKit.MoveFile('old.txt', 'new.txt');
  FileKit.DeleteFile('temp.txt');
  
  // Path operations
  WriteLn(FileKit.GetFileName('path/to/file.txt')); // Returns 'file.txt'
  WriteLn(FileKit.GetExtension('script.pas')); // Returns '.pas'
  WriteLn(FileKit.ChangeExtension('test.txt', '.bak')); // Returns 'test.bak'
end;
```

### 📅 DateTime Operations
```pascal
var
  CurrentTime: TDateTime;
  NextWorkday: TDateTime;
  TZInfo: TTimeZoneInfo;
  OriginalTZ: string;
  DSTTransitionDate: TDateTime;
begin
  // Get next business day for delivery date
  CurrentTime := TDateTimeKit.GetNow;
  NextWorkday := TDateTimeKit.NextBusinessDay(CurrentTime);
  
  // Format for display
  WriteLn(TDateTimeKit.GetAsString(NextWorkday, 'yyyy-mm-dd'));
  
  // Check if within business hours (9 AM - 5 PM)
  if TDateTimeKit.IsWithinInterval(CurrentTime, 
     TDateTimeKit.CreateInterval(
       TDateTimeKit.StartOfDay(CurrentTime) + EncodeTime(9, 0, 0, 0),
       TDateTimeKit.StartOfDay(CurrentTime) + EncodeTime(17, 0, 0, 0)
     )) then
    WriteLn('Within business hours');
    
  // Cross-platform timezone handling
  TZInfo := TDateTimeKit.GetTimeZone(CurrentTime);
  WriteLn('Current timezone: ', TZInfo.Name);
  WriteLn('Offset from UTC: ', TZInfo.Offset, ' minutes');
  WriteLn('DST active: ', BoolToStr(TZInfo.IsDST, True));
  
  // Convert between timezones
  WriteLn('UTC time: ', TDateTimeKit.GetAsString(
    TDateTimeKit.WithTimeZone(CurrentTime, 'UTC'), 
    'yyyy-mm-dd hh:nn:ss'));
    
  // Cross-platform environment variables for testing
  OriginalTZ := GetEnvVar('TZ');
  try
    // Change timezone for testing
    SetEnvVar('TZ', 'America/New_York');
    
    // Create a datetime value near the US DST transition 
    // (2:00 AM on second Sunday in March 2024)
    DSTTransitionDate := EncodeDateTime(2024, 3, 10, 2, 0, 0, 0);
    
    // Check if this time is in DST using GetTimeZone
    TZInfo := TDateTimeKit.GetTimeZone(DSTTransitionDate);
    WriteLn('DST active: ', BoolToStr(TZInfo.IsDST, True));
    WriteLn('Timezone offset: ', TZInfo.Offset, ' minutes');
  finally
    // Restore original timezone
    SetEnvVar('TZ', OriginalTZ);
  end;
end;
```

### 🔄 JSON Operations
```pascal
var
  Person: IJSONObject;
  Address: IJSONObject;
  Hobbies: IJSONArray;
  JSON: string;
  Value: IJSONValue;
begin
  // Create a person object
  Person := TJSON.Obj;
  Person.Add('name', 'John Smith');
  Person.Add('age', 30);
  Person.Add('isActive', True);
  
  // Create and add an address object
  Address := TJSON.Obj;
  Address.Add('street', '123 Main St');
  Address.Add('city', 'Springfield');
  Address.Add('zipCode', '12345');
  Person.Add('address', Address);
  
  // Create and add a hobbies array
  Hobbies := TJSON.Arr;
  Hobbies.Add('reading');
  Hobbies.Add('cycling');
  Hobbies.Add('swimming');
  Person.Add('hobbies', Hobbies);
  
  // Convert to JSON string with pretty printing
  JSON := Person.ToString(True);
  WriteLn(JSON);
  
  // Parse JSON string
  JSON := '{"name":"Jane Doe","age":25,"skills":["Pascal","Python"]}';
  Value := TJSON.Parse(JSON);
  Person := Value.AsObject;
  
  // Access values
  WriteLn('Name: ', Person['name'].AsString);
  WriteLn('Age: ', Person['age'].AsInteger);
  WriteLn('First Skill: ', Person['skills'].AsArray[0].AsString);
  
  // Error handling
  try
    Value := TJSON.Parse('{invalid json}');
  except
    on E: EJSONException do
      WriteLn('Error: ', E.Message);
  end;
end;
```

### 📝 Logging Operations
```pascal
// Simple one-line setup for console and file logging
TLogger.CreateConsoleAndFileLogger('application.log', llInfo);

// Log messages with different levels
Logger.Debug('Processing started'); // Only shown if minimum level is Debug
Logger.Info('User %s logged in', ['JohnDoe']);
Logger.Warning('Disk space is low: %d%% remaining', [5]);
Logger.Error('Failed to save file: %s', ['Access denied']);
Logger.Fatal('Application crashed: %s', ['Segmentation fault']);

// Create category-based loggers for better organization
var
  UILogger, DBLogger: ILogContext;
begin
  UILogger := Logger.CreateContext('UI');
  DBLogger := Logger.CreateContext('DB');
  
  UILogger.Info('Window created');
  DBLogger.Warning('Slow query detected: %s', ['SELECT * FROM large_table']);
end;

// Time operations and log their duration
var
  Timer: ITimedOperation;
begin
  Timer := Logger.TimedBlock('Data processing');
  // ... perform long operation ...
  // Timer automatically logs completion with duration when it goes out of scope
end;

// IMPORTANT: Always close log files when shutting down
try
  // Your application logic with logging...
finally
  Logger.CloseLogFiles;  // Ensures all data is written to disk
end;
```

### 🌐 HTTP Request Operations
```pascal
var
  Response: TResponse;
  UserData: IJSONObject;
  ApiResponse: IJSONObject;
  Result: TRequestResult;
begin
  // Simple GET request with JSON response
  Response := Http.Get('https://api.example.com/data');
  if Response.StatusCode = 200 then
  begin
    ApiResponse := Response.JSON.AsObject;
    WriteLn('User ID: ', ApiResponse['id'].AsInteger);
    WriteLn('Username: ', ApiResponse['username'].AsString);
  end;
    
  // POST with JSON data
  UserData := TJSON.Obj;
  UserData.Add('name', 'John Smith');
  UserData.Add('email', 'john@example.com');
  UserData.Add('age', 30);
  
  Response := Http.PostJSON('https://api.example.com/users',
    UserData.ToString);
    
  if Response.StatusCode = 201 then
    WriteLn('User created with ID: ', Response.JSON.AsObject['id'].AsString);
    
  // Error handling with TryGet pattern
  Result := Http.TryGet('https://api.example.com/users');
  if Result.Success then
    WriteLn('Found ', Result.Response.JSON.AsObject['count'].AsInteger, ' users')
  else
    WriteLn('Error: ', Result.Error);
    
  // Download file with custom headers
  Response := Http.Get('https://example.com/large-file.zip');
  TFileKit.WriteFile('download.zip', Response.Text);
end;
```

### 🔐 Crypto Operations
```pascal
var
  Hash: string;
  Encrypted: string;
begin
  // Hash password for storage
  Hash := TCryptoKit.SHA512Hash('user_password');
  
  // Secure configuration data
  Encrypted := TCryptoKit.BlowfishCrypt(
    '{"api_key": "secret123"}',
    'encryption_key',
    bmEncrypt
  );
  
  // Verify file integrity
  Hash := TCryptoKit.SHA256Hash(TFileKit.ReadFile('important.dat'));
end;
```

### 📦 Archive Operations
```pascal
var
  SourceDir, DestDir: string;
begin
  // Create ZIP archive
  SourceDir := 'path/to/source';
  TArchiveKit.CompressToZip(SourceDir, 'archive.zip', True);  // Recursive
  
  // Extract specific files
  DestDir := 'path/to/extract';
  TArchiveKit.DecompressFromZip('archive.zip', DestDir, '*.txt');  // Only .txt files
  
  // Create TAR archive with specific files
  TArchiveKit.CompressToTar(SourceDir, 'backup.tar', True, '*.pas');  // Only .pas files
  
  // Extract entire TAR archive
  TArchiveKit.DecompressFromTar('backup.tar', DestDir);
end;
```

### 📈 Statistical Operations
```pascal
var
  Data: TDoubleArray;
  Stats: TDescriptiveStats;
  CI: TDoublePair;
  CorrelationCoef: Double;
  X, Y: TDoubleArray;
begin
  // Basic descriptive statistics
  Data := TDoubleArray.Create(4.5, 3.0, 5.0, 4.0, 4.8, 3.2, 4.5, 4.9);
  Stats := TStatsKit.Describe(Data);
  
  // Print formatted statistics
  WriteLn(Stats.ToString);
  // Or use individual properties
  WriteLn(Format('Average rating: %.2f', [Stats.Mean]));
  WriteLn(Format('Rating spread: %.2f', [Stats.StdDev]));
  WriteLn(Format('Most common: %.1f', [Stats.Mode]));
  WriteLn(Format('Median: %.1f', [Stats.Median]));
  WriteLn(Format('Range: %.1f', [Stats.Range]));
  
  // Advanced statistics
  WriteLn(Format('Distribution skewness: %.2f', [Stats.Skewness]));
  WriteLn(Format('Kurtosis: %.2f', [Stats.Kurtosis]));
  
  // Robust statistics
  WriteLn(Format('Median Absolute Deviation: %.2f', 
                [TStatsKit.MedianAbsoluteDeviation(Data)]));
  WriteLn(Format('Trimmed Mean (10%%): %.2f', 
                [TStatsKit.TrimmedMean(Data, 10)]));
  
  // Bootstrapping for confidence intervals
  CI := TStatsKit.BootstrapConfidenceInterval(Data);
  WriteLn(Format('95%% CI: [%.2f, %.2f]', [CI.Lower, CI.Upper]));
  
  // Correlation analysis
  X := TDoubleArray.Create(1.0, 2.0, 3.0, 4.0, 5.0);
  Y := TDoubleArray.Create(2.0, 3.5, 4.8, 6.1, 8.0);
  
  CorrelationCoef := TStatsKit.PearsonCorrelation(X, Y);
  WriteLn(Format('Pearson correlation: %.2f', [CorrelationCoef]));
  
  CorrelationCoef := TStatsKit.SpearmanCorrelation(X, Y);
  WriteLn(Format('Spearman correlation: %.2f', [CorrelationCoef]));
end;
```

### 🔢 Matrix Operations
```pascal
var
  A, B, C: IMatrix;
  begin
  // Create and initialize matrices
  A := TMatrixKit.CreateFromArray([
    [1.0, 2.0],
    [3.0, 4.0]
  ]);
  
  // Create identity matrix
  B := TMatrixKit.Identity(2);
  
  // Matrix multiplication
  C := A.Multiply(B);
  
  // Calculate properties
  WriteLn(Format('Determinant: %.2f', [A.Determinant]));
  WriteLn(Format('Trace: %.2f', [A.Trace]));
  
  // Matrix transpose and inverse
  C := A.Transpose;
  C := A.Inverse;
  
  // Matrix decompositions
  LU := A.LU;
  QR := A.QR;
  Eigen := A.EigenDecomposition;
  SVD := A.SVD;
  Chol := A.Cholesky;
  
  // Advanced matrix creation
  H := TMatrixKit.CreateHilbert(3);
  T := TMatrixKit.CreateToeplitz(FirstRow, FirstCol);
  V := TMatrixKit.CreateVandermonde(Vector);
  
  // Matrix functions
  E := A.Exp;                // Matrix exponential
  P := A.Power(0.5);         // Matrix square root
  
  // Vector operations
  V1 := TMatrixKit.CreateFromArray([[1.0], [2.0], [3.0]]);
  V2 := TMatrixKit.CreateFromArray([[4.0], [5.0], [6.0]]);
  DotProd := V1.DotProduct(V2);     // Dot/inner product
  Cross := V1.CrossProduct(V2);     // Cross product (3D vectors only)
  Norm := V1.Normalize;             // Normalize to unit length
  
  // Statistical operations
  Mean := A.Mean;                   // Overall mean
  ColMeans := A.Mean(0);            // Column means
  RowMeans := A.Mean(1);            // Row means
  Cov := A.Covariance;              // Covariance matrix
  Corr := A.Correlation;            // Correlation matrix
  
  // Solving linear systems
  X := A.Inverse.Multiply(B);                    // Direct solution
  X := A.PseudoInverse.Multiply(B);              // For non-square systems
  X := A.SolveIterative(B, imConjugateGradient); // Iterative solution
end;
```

### 📐 Trigonometric Operations
```pascal
var
  Angle, Height, Distance: Double;
begin
  // Calculate building height using angle
  Angle := TTrigKit.DegToRad(30);  // 30 degrees elevation
  Distance := 100;  // Distance from building
  Height := Distance * TTrigKit.Tan(Angle);
  
  // Calculate area of irregular field
  WriteLn(Format('Field area: %.2f',
    [TTrigKit.TriangleAreaSAS(100, TTrigKit.DegToRad(60), 120)]));
end;
```

### 💰 Financial Operations
```pascal
var
  CashFlows: TDoubleArray;
  NPV, IRR: Double;
begin
  // Investment analysis
  CashFlows := TDoubleArray.Create(-1000, 200, 300, 400, 500);
  
  NPV := TFinanceKit.NetPresentValue(0, CashFlows, 0.1);
  IRR := TFinanceKit.InternalRateOfReturn(0, CashFlows);
  
  WriteLn(Format('NPV: $%.2f', [NPV]));
  WriteLn(Format('IRR: %.2f%%', [IRR * 100]));
end;
```

## 📖 System Requirements

### Tested Environments

| Module                | Windows 11 | Ubuntu 24.04.2 |
|-----------------------|------------|----------------|
| TidyKit.Strings       | ✅         | ✅            |
| TidyKit.FS            | ✅         | ✅            |
| TidyKit.DateTime      | ✅         | ✅            |
| TidyKit.JSON          | ✅         | ✅            |
| TidyKit.Logger        | ✅         | ✅            |
| TidyKit.Request       | ✅         | ✅            |
| TidyKit.Crypto        | ✅         | ✅            |
| TidyKit.Archive       | ✅         | ✅            |
| TidyKit.Math.Stats    | ✅         | ✅            |
| TidyKit.Math.Matrices | ✅         | ✅            |
| TidyKit.Math.Trig     | ✅         | ✅            |
| TidyKit.Math.Finance  | ✅         | ✅            |


### Dependencies
- Windows
  - No external dependencies required
- Linux
  - Ubuntu/Debian: `sudo apt-get install libssl-dev` (needed for HTTPS in `TidyKit.Request`)
  - Fedora/RHEL: `sudo dnf install openssl-devel` (needed for HTTPS in `TidyKit.Request`)
- Uses only standard Free Pascal RTL units

### Build Requirements
- Free Pascal Compiler (FPC) 3.2.2+
- Lazarus 3.6+
- Basic development tools (git, terminal, etc)

## 📚 Documentation

For detailed documentation, see:
- 📋 [Cheat Sheet](docs/cheat-sheet.md)
- 📝 [Strings](docs/TidyKit.Strings.md)
- 📂 [File System](docs/TidyKit.FS.md)
- 📅 [DateTime](docs/TidyKit.DateTime.md)
- 🔄 [JSON](docs/TidyKit.JSON.md)
- 📝 [Logger](docs/TidyKit.Logger.md)
- 🌐 [Network](docs/TidyKit.Request.md)
- 🔐 [Crypto](docs/TidyKit.Crypto.md)
- 📦 [Archive](docs/TidyKit.Archive.md)
- 📊 [Math](docs/TidyKit.Math.md)
  - 📈 [Statistics](docs/TidyKit.Math.Stats.md)
  - 🔢 [Matrices](docs/TidyKit.Math.Matrices.md)
  - 📐 [Trigonometry](docs/TidyKit.Math.Trigonometry.md)
  - 💰 [Finance](docs/TidyKit.Math.Finance.md)

## 📊 Real-World Examples

TidyKit can be used to build a wide variety of applications quickly:

| Example Project | Description | Source Code |
|-----------------|-------------|-------------|
| File System Utility | Batch operations on files with pattern matching | [View Example](examples/FileKitExample/) |
| Simple File Explorer | Navigate directories, view files, and perform file operations with an interactive console UI | [View Example](examples/SimpleFileExplorer/) |
| File Analyzer | Analyze text files, count word frequencies, and gather directory statistics | [View Example](examples/FileAnalyzer/) |
| File Backup | Backup files and folders, supports simulate run | [View Example](examples/FileBackup/) |
| Simple Data Logger | Record sensor readings with timestamp and export as CSV | [View Example](examples/LoggerExample/) |
| Configuration Manager | Load, parse, and validate JSON configuration files | [View Example](examples/ConfigKitExample/) |
| Secure Password Storage | Hash and verify passwords with SHA-256 | [View Example](examples/CryptoKitExample/) |
| Date Calculator | Business day calculator with timezone handling | [View Example](examples/DateTimeExample/) |



## 💬 Community & Support

- **Questions?** [Open a discussion](https://github.com/ikelaiah/TidyKit/discussions)
- **Found a bug?** [Report an issue](https://github.com/ikelaiah/TidyKit/issues)

## ⚠️ Known Limitations

- **Platform Support**: Currently only tested on Windows 11 and Ubuntu 24.04. MacOS and FreeBSD are not officially supported.
- **Threading**: Batch logging mode is not thread-safe. Use in single-threaded contexts or implement your own synchronization.
- **HTTP SSL**: HTTPS requests require OpenSSL libraries on Linux systems (see Dependencies section).
- **Timezone Support**: Limited timezone support on Unix-like systems.
- **Language Features**: Designed for FPC 3.2.2, so no use of inline var declarations or anonymous functions.
- **Library Integration**: Currently no integration with package managers.

## ✅ Testing

1. Open the `TestRunner.lpi` using Lazarus IDE
2. Compile the project
3. Run the Test Runner:

```bash
$ cd tests
$ ./TestRunner.exe -a --format=plain
```

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request. For major changes, please open an issue first to discuss what you would like to change.

1. Fork the Project
2. Create your Feature Branch (git checkout -b feature/AmazingFeature)
3. Commit your Changes (git commit -m 'Add some AmazingFeature')
4. Push to the Branch (git push origin feature/AmazingFeature)
5. Open a Pull Request

## ⚖️ License

This project is licensed under the MIT License - see the [LICENSE](LICENSE.md) file for details.

## 🙏 Acknowledgments

- FPC Team for Free Pascal
- Contributors and maintainers

## 🗺️ Roadmap

TidyKit is under active development. Here's what's planned:

### Planned for v0.2.0 (Q3 2025) - Simplify API
- **Goal:** Provide a simpler, more consistent API.
- **`TidyKit.FS`:** Transition to Factory/Interface pattern. (completed)
- **`TidyKit.DateTime`, `Strings`, `Math.*` (excluding Matrices), `Crypto.*`, `Archive`:** Remain as Static Class Methods for stateless utilities.
- **`TidyKit.Matrices`:** Transition from Class/Interface to **Factory/Interface** for consistency with `JSON` and improved testability/flexibility.
- **`TidyKit.Request`:** Implement using **Factory/Interface** pattern for better testability (mocking network calls).
- **`TidyKit.JSON`, `TidyKit.Logger`:** Refine existing Factory/Interface patterns for consistency.
- Update all examples and documentation to reflect the simplified  API structure.

### Planned for v0.3.0 (Q4 2025) - Examples & Refinements
- Add **more real-world examples** and tutorials demonstrating common use cases.
- Performance optimizations based on profiling.
- Improved error messages and diagnostics.
- Expand unit test coverage, especially for edge cases identified during API simplification.

### Future Goals
- More advanced math/statistics modules.
- Easy to use API

---

*Feedback and suggestions are welcome! See the [issues](https://github.com/ikelaiah/TidyKit/issues) page to contribute ideas or track progress.*