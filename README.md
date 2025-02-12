# TidyKit

A comprehensive Pascal toolkit providing essential utilities for modern Pascal development.

## Features

- **String Operations**
  - String manipulation and transformations
  - Case conversion and formatting
  - Pattern matching and validation
  - Unicode support
  - String comparison and searching
  - Text encoding/decoding


- **File System Operations**
  - File and directory manipulation
  - Path operations
  - File searching and filtering

- **Cryptography**
  - SHA3 implementation
  - Secure hashing
  - Encryption utilities

- **Network Operations**
  - HTTP client
  - Request handling
  - Response parsing

- **Math Operations**
  - Statistics (mean, median, variance, etc.)
  - Financial calculations (NPV, IRR, depreciation)
  - Matrix operations (addition, multiplication, determinant)
  - Trigonometric functions and calculations
  - All math operations use Double precision for accuracy

## Installation

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
- Reduces compilation time
- Minimizes executable size
- Makes dependencies more explicit
- Improves code maintainability

Note: Some units may have interdependencies. The compiler will inform you if additional units need to be included.

## Quick Start

### Statistical Operations
```pascal
var
  Data: TDoubleArray;
  Mean: Double;
begin
  Data := TDoubleArray.Create(1, 2, 3, 4, 5);
  Mean := TStatsKit.Mean(Data);  // Returns 3.0
end;
```

### Financial Calculations
```pascal
var
  NPV: Double;
  CashFlows: TDoubleArray;
begin
  CashFlows := TDoubleArray.Create(100, 200, 300);
  NPV := TFinanceKit.NetPresentValue(1000, CashFlows, 0.1);
end;
```

### Matrix Operations
```pascal
var
  A, B, C: TMatrix;
begin
  A := TMatrixKit.CreateMatrix(2, 2);
  B := TMatrixKit.CreateMatrix(2, 2);
  // Set values...
  C := TMatrixKit.Multiply(A, B);
end;
```

### Trigonometric Calculations
```pascal
var
  Angle: Double;
begin
  Angle := TTrigKit.DegToRad(45);  // Convert 45 degrees to radians
  WriteLn(TTrigKit.Sin(Angle));    // Calculate sine
end;
```

## Documentation

For detailed documentation, see:
- [Cheat Sheet](docs/cheat-sheet.md)
- [Math Documentation](docs/TidyKit.Math.md)
- [File System Documentation](Coming soon)
- [Crypto Documentation](Coming soon)
- [Network Documentation](docs/TidyKit.Request.md)

## Testing

1. Open the TestRunner.lpi using Lazarus IDE
2. Compile the project
3. Run the Test Runner:

```bash
$ cd tests
$ ./TestRunner.exe -a --format=all
```

## Contributing

1. Fork the repository
2. Create your feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- FPC Team for Free Pascal
- Contributors and maintainers