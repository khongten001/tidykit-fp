# TidyKit

A comprehensive Pascal toolkit providing essential utilities for modern Pascal development.

## Features

- **Math Operations**
  - Statistics (mean, median, variance, etc.)
  - Financial calculations (NPV, IRR, depreciation)
  - Matrix operations (addition, multiplication, determinant)
  - Trigonometric functions and calculations
  - All math operations use Double precision for accuracy

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

## Installation

1. Clone the repository:
```bash
git clone https://github.com/yourusername/TidyKit.git
```

2. Add the source directory to your project's search path.

3. Add the units you need to your uses clause:
```pascal
uses
  TidyKit.Math,
  TidyKit.Math.Stats,
  TidyKit.Math.Finance,
  TidyKit.Math.Matrices,
  TidyKit.Math.Trigonometry;
```

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
- [File System Documentation](docs/TidyKit.FS.md)
- [Crypto Documentation](docs/TidyKit.Crypto.md)
- [Network Documentation](docs/TidyKit.Request.md)

## Testing

Run the test suite:
```bash
cd tests
fpc TestRunner.lpr
./TestRunner
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