# 🧰 TidyKit

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

- 🔐 **Cryptography**
  - SHA3 implementation
  - Secure hashing
  - Encryption utilities

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
    - Matrix transpose
    - Determinant and trace calculation
    - Coming Soon:
      - Matrix decompositions (LU, QR)
      - Matrix inversion
      - Matrix rank calculation
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
  TidyKit.Crypto.SHA3,      // SHA3 implementation
  
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
begin
  // Simple GET request
  Response := Http.Get('https://api.example.com/data');
  if Response.StatusCode = 200 then
    WriteLn(Response.Text);
    
  // POST with JSON
  Response := Http.PostJSON('https://api.example.com/users',
    '{"name": "John", "email": "john@example.com"}');
    
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
  A, B, C: TMatrix;
begin
  // Create and initialize matrices
  A := TMatrixKit.CreateMatrix(2, 2);
  A[0,0] := 1; A[0,1] := 2;
  A[1,0] := 3; A[1,1] := 4;
  
  // Create identity matrix
  B := TMatrixKit.Identity(2);
  
  // Matrix multiplication
  C := TMatrixKit.Multiply(A, B);
  
  // Calculate properties
  WriteLn(Format('Determinant: %.2f', [TMatrixKit.Determinant(A)]));
  WriteLn(Format('Trace: %.2f', [TMatrixKit.Trace(A)]));
  
  // Matrix transpose
  C := TMatrixKit.Transpose(A);
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

## 📚 Documentation

For detailed documentation, see:
- 📋 [Cheat Sheet](docs/cheat-sheet.md)
- 📊 [Math Documentation](docs/TidyKit.Math.md)
- 📂 [File System Documentation](Coming soon)
- 🔐 [Crypto Documentation](Coming soon)
- 🌐 [Network Documentation](docs/TidyKit.Request.md)

## ✅ Testing

1. Open the TestRunner.lpi using Lazarus IDE
2. Compile the project
3. Run the Test Runner:

```bash
$ cd tests
$ ./TestRunner.exe -a --format=all
```

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request

## ⚖️ License

This project is licensed under the MIT License - see the LICENSE file for details.

## 🙏 Acknowledgments

- FPC Team for Free Pascal
- Contributors and maintainers