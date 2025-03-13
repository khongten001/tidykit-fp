# 🧰 TidyKit

[![FPC](https://img.shields.io/badge/Free%20Pascal-3.2.2-blue.svg)](https://www.freepascal.org/)
[![License](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE.md)
[![Status](https://img.shields.io/badge/Status-Development-yellow.svg)]()
[![Version](https://img.shields.io/badge/Version-0.1.0-orange.svg)]()
[![Documentation](https://img.shields.io/badge/Docs-Available-brightgreen.svg)](docs/)
[![Tests](https://img.shields.io/badge/Tests-Passing-brightgreen.svg)](tests/)

A comprehensive toolkit providing essential utilities for development in Free Pascal.  
**Streamlining your Pascal programming experience with reliable tools.**

> [!WARNING]
> ⚠️ This library is under active development and is not yet ready for production use. APIs may change without notice.

## 📑 Table of Contents
- [🧰 TidyKit](#-tidykit)
  - [📑 Table of Contents](#-table-of-contents)
  - [✨ Features](#-features)
  - [💻 Installation](#-installation)
  - [🚀 Quick Start](#-quick-start)
    - [📂 File System Operations](#-file-system-operations)
    - [📝 String Operations](#-string-operations)
    - [📅 DateTime Operations](#-datetime-operations)
    - [🌐 HTTP Request Operations](#-http-request-operations)
    - [🔐 Crypto Operations](#-crypto-operations)
    - [📈 Statistical Operations](#-statistical-operations)
    - [🔢 Matrix Operations](#-matrix-operations)
    - [📐 Trigonometric Operations](#-trigonometric-operations)
    - [💰 Financial Operations](#-financial-operations)
    - [📦 Archive Operations](#-archive-operations)
    - [📝 Logging Operations](#-logging-operations)
  - [📖 System Requirements](#-system-requirements)
    - [Tested Environments](#tested-environments)
    - [Theoretical Compatibility](#theoretical-compatibility)
    - [Dependencies](#dependencies)
    - [Build Requirements](#build-requirements)
  - [📚 Documentation](#-documentation)
  - [✅ Testing](#-testing)
  - [🤝 Contributing](#-contributing)
  - [⚖️ License](#️-license)
  - [🙏 Acknowledgments](#-acknowledgments)


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
    - Matrix functions (exponential, power)
    - Pseudoinverse for non-square matrices
    - Vector operations (dot product, cross product, normalization)
    - Statistical operations (mean, covariance, correlation)
    - Comprehensive error handling
    - Memory-safe interface design
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
    - Control character escaping (\n, \r, \t, etc.)
  - Comprehensive error handling with detailed messages
  - Factory methods for easy value creation
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

## 💻 Installation

1. Clone the repository:
```bash
git clone https://github.com/yourusername/TidyKit.git
```

2. Add the source directory to your project's search path.

3. Add either the all-inclusive TidyKit unit or choose specific units you need:

```pascal
// Option 1: All-inclusive unit (includes all functionality)
uses
  TidyKit;

// Option 2: Choose specific units based on your needs
uses
  // JSON functionality
  TidyKit.JSON,              // All JSON functionality
  
  // Logging functionality
  TidyKit.Logger,            // Easy to use logging system
  
  // Math-related units
  TidyKit.Math,              // Base math types and operations
  TidyKit.Math.Stats,        // Statistical calculations
  TidyKit.Math.Finance,      // Financial mathematics
  TidyKit.Math.Matrices,     // Matrix operations
  TidyKit.Math.Trigonometry, // Trigonometric functions
  
  // String manipulation units
  TidyKit.String,            // String operations
  TidyKit.String.Format,     // String formatting
  TidyKit.String.Convert,    // String conversion
  
  // File system units
  TidyKit.FS,               // File system operations
  TidyKit.FS.Path,          // Path manipulation
  TidyKit.FS.Search,        // File searching
  
  // Cryptography units
  TidyKit.Crypto,           // Base crypto operations
  TidyKit.Crypto.SHA2,      // SHA2 implementation
  TidyKit.Crypto.SHA3,      // SHA3 implementation
  TidyKit.Crypto.AES256,    // AES-256 implementation
  
  // Network units
  TidyKit.Request;          // HTTP client with simple and advanced features

```

Choose Option 1 if you want to include all functionality with a single unit. This is convenient but may increase compilation time and executable size.

Choose Option 2 if you want to optimize your application by including only the specific functionality you need. This approach:
- ⚡ Reduces compilation time
- 📦 Minimizes executable size
- 🔍 Makes dependencies more explicit
- 🔧 Improves code maintainability

Note: Some units may have interdependencies. The compiler will inform you if additional units need to be included.

## 🚀 Quick Start

### 📂 File System Operations
```pascal
var
  Files: TFilePathArray;
  Attrs: TFileAttributes;
  Content: string;
begin
  // Basic file operations
  TFileKit.WriteFile('example.txt', 'Hello World');
  Content := TFileKit.ReadFile('example.txt');
  
  // Directory operations
  TFileKit.CreateDirectory('new_folder');
  TFileKit.EnsureDirectory('path/to/folder');
  
  // List files with pattern matching
  Files := TFileKit.ListFiles('src', '*.pas', True, fsName);
  
  // Get file attributes
  Attrs := TFileKit.GetAttributes('example.txt');
  WriteLn(Format('Read-only: %s', [BoolToStr(Attrs.ReadOnly, True)]));
  
  // File manipulation
  TFileKit.CopyFile('source.txt', 'dest.txt');
  TFileKit.MoveFile('old.txt', 'new.txt');
  TFileKit.DeleteFile('temp.txt');
  
  // Path operations
  WriteLn(TFileKit.GetFileName('path/to/file.txt')); // Returns 'file.txt'
  WriteLn(TFileKit.GetExtension('script.pas')); // Returns '.pas'
  WriteLn(TFileKit.ChangeExtension('test.txt', '.bak')); // Returns 'test.bak'
end;
```

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

### 📅 DateTime Operations
```pascal
var
  CurrentTime: TDateTime;
  NextWorkday: TDateTime;
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
end;
```

### 🌐 HTTP Request Operations
```pascal
var
  Response: TResponse;
  UserData: IJSONObject;
  ApiResponse: IJSONObject;
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
    
  // Download file with progress
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

### 📈 Statistical Operations
```pascal
var
  Data: TDoubleArray;
  Stats: TDescriptiveStats;
begin
  // Analyze product ratings
  Data := TDoubleArray.Create(4.5, 3.0, 5.0, 4.0, 4.8);
  Stats := TStatsKit.Describe(Data);
  
  WriteLn(Format('Average rating: %.2f', [Stats.Mean]));
  WriteLn(Format('Rating spread: %.2f', [Stats.StdDev]));
  WriteLn(Format('Most common: %.1f', [Stats.Mode]));
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
  DotProd := V1.DotProduct(V2);
  Cross := V1.CrossProduct(V2);
  Norm := V1.Normalize;
  
  // Statistical operations
  Mean := A.Mean;            // Overall mean
  ColMeans := A.Mean(0);     // Column means
  RowMeans := A.Mean(1);     // Row means
  Cov := A.Covariance;       // Covariance matrix
  Corr := A.Correlation;     // Correlation matrix
  
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

## 📖 System Requirements

### Tested Environments
- **Operating System**: Windows 11
- **Compiler**: Free Pascal (FPC) 3.2.2
- **IDE**: Lazarus 3.8

### Theoretical Compatibility
- **Operating Systems**:
  - Windows (7, 8, 10, 11)
  - Linux (Any distribution with FPC support)
  - macOS (with FPC support)
  - FreeBSD
- **Compiler**: Free Pascal 3.2.2 or higher
- **IDE**: Any IDE that supports Free Pascal
  - Lazarus 3.6 or higher
  - VS Code with OmniPascal
  - Other text editors

### Dependencies
- No external dependencies required
- Uses only standard Free Pascal RTL units

### Build Requirements
- Free Pascal Compiler (FPC) 3.2.2+
- Lazarus 3.6+
- Basic development tools (git, terminal, etc)

## 📚 Documentation

For detailed documentation, see:
- 📋 [Cheat Sheet](docs/cheat-sheet.md)
- 📊 [Math](docs/TidyKit.Math.md)
  - 📈 [Statistics](docs/TidyKit.Math.Stats.md)
  - 💰 [Finance](docs/TidyKit.Math.Finance.md)
  - 🔢 [Matrices](docs/TidyKit.Math.Matrices.md)
  - 📐 [Trigonometry](docs/TidyKit.Math.Trigonometry.md)
- 📂 [File System](docs/TidyKit.FS.md)
- 🔐 [Crypto](docs/TidyKit.Crypto.md)
- 🌐 [Network](docs/TidyKit.Request.md)
- 🔄 [JSON](docs/TidyKit.JSON.md)
- 📝 [Logger](docs/TidyKit.Logger.md)

## ✅ Testing

1. Open the TestRunner.lpi using Lazarus IDE
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