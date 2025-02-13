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
  - Statistical Analysis
    - Basic statistics (mean, median, mode, range)
    - Variance and standard deviation (population and sample)
    - Distribution measures (skewness, kurtosis)
    - Correlation (Pearson, Spearman)
    - Advanced means (geometric, harmonic, trimmed)
    - Robust statistics (MAD, Huber M-estimator)
    - Hypothesis testing (t-test, Mann-Whitney U)
    - Effect size measures (Cohen's d, Hedges' g)
    - Bootstrap confidence intervals
  - Financial Mathematics
    - Time value of money (PV, FV)
    - Investment analysis (NPV, IRR)
    - Depreciation calculations
    - Return metrics (ROI, ROE)
  - Matrix Operations
    - Basic operations (add, subtract, multiply)
    - Matrix transformations
    - Determinant and trace
    - Matrix decompositions (LU, QR)
  - Trigonometry
    - Basic functions (sin, cos, tan)
    - Inverse functions
    - Triangle calculations
    - Vector operations
  - All calculations use Double precision (64-bit) for accuracy

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
  Stats: TDescriptiveStats;
  IsNormal: Boolean;
  WPValue: Double;
begin
  // Create sample data
  Data := TDoubleArray.Create(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
  
  // Get comprehensive descriptive statistics
  Stats := TStatsKit.Describe(Data);
  
  // Pretty print all statistics (vertical format)
  WriteLn('Vertical Format:');
  WriteLn(Stats.ToString);
  
  WriteLn;
  WriteLn('Horizontal Format:');
  WriteLn(Stats.ToStringWide);
  
  // Test for normality
  TStatsKit.ShapiroWilkTest(Data, WPValue);
  IsNormal := WPValue >= 0.05;  // Using 5% significance level
  
  // Use robust statistics if not normal
  if not IsNormal then
    WriteLn(Format('Robust StdDev: %.4f', [TStatsKit.RobustStandardDeviation(Data)]));
end;

Example output (vertical format):
```
Descriptive Statistics
======================
N: 10
Central Tendency:
  Mean: 5.500000
  Median: 5.500000
  Mode: 1.000000
Dispersion:
  Range: 9.000000
  Variance: 8.250000
  StdDev: 2.872281
  SEM: 0.908295
  CV: 52.22%
Distribution Shape:
  Skewness: 0.000000
  Kurtosis: -1.200000
Quartiles:
  Min (0%): 1.000000
  Q1 (25%): 3.250000
  Q2 (50%): 5.500000
  Q3 (75%): 7.750000
  Max (100%): 10.000000
  IQR: 4.500000
```

Example output (horizontal format):
```
Descriptive Statistics
===================
N           |    Mean     |   Median    |   StdDev    |    SEM      |    CV(%)    
-----------------------------------------------------------------------------
    10      |   5.5000    |   5.5000    |   2.8723    |   0.9083    |   52.2200   

Shape       |  Skewness   |  Kurtosis   |   Range     |    IQR      
------------------------------------------------------------------------
            |   0.0000    |  -1.2000    |   9.0000    |   4.5000    

Quantiles   |    Min      |     Q1      |     Q2      |     Q3      |    Max      
-----------------------------------------------------------------------------
            |   1.0000    |   3.2500    |   5.5000    |   7.7500    |  10.0000    
```

### Financial Calculations
```pascal
var
  CashFlows: TDoubleArray;
  NPV, IRR: Double;
begin
  CashFlows := TDoubleArray.Create(100, 200, 300);
  
  // Calculate NPV with 6 decimal precision
  NPV := TFinanceKit.NetPresentValue(1000, CashFlows, 0.1, 6);
  
  // Calculate IRR with default precision
  IRR := TFinanceKit.InternalRateOfReturn(1000, CashFlows);
  
  // Calculate depreciation
  WriteLn(TFinanceKit.StraightLineDepreciation(10000, 1000, 5));
end;
```

### Matrix Operations
```pascal
var
  A, B, C: TMatrix;
begin
  // Create and initialize matrices
  A := TMatrixKit.CreateMatrix(2, 2);
  B := TMatrixKit.Identity(2);
  A[0,0] := 1; A[0,1] := 2;
  A[1,0] := 3; A[1,1] := 4;
  
  // Perform operations
  C := TMatrixKit.Multiply(A, B);
  WriteLn(Format('Determinant: %.4f', [TMatrixKit.Determinant(C)]));
  WriteLn(Format('Trace: %.4f', [TMatrixKit.Trace(C)]));
end;
```

### Trigonometric Calculations
```pascal
var
  Angle, Height: Double;
begin
  // Convert 45 degrees to radians
  Angle := TTrigKit.DegToRad(45);
  
  // Calculate height using sine
  Height := 10 * TTrigKit.Sin(Angle);
  
  // Calculate triangle area
  WriteLn(Format('Area: %.4f', [TTrigKit.TriangleAreaSAS(4, Angle, 5)]));
  
  // Calculate vector magnitude
  WriteLn(Format('Magnitude: %.4f', [TTrigKit.VectorMagnitude(3, 4)]));
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