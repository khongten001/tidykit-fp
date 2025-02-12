# TidyKit Math Library Documentation

The TidyKit Math library provides comprehensive mathematical operations using Double precision floating-point numbers. The library is divided into several specialized modules for different mathematical domains.

## Base Types

```pascal
uses TidyKit.Math;

type
  TDoubleArray = array of Double;
  TMatrix = array of array of Double;
```

## Precision in Calculations

TidyKit's math library uses Double precision (64-bit IEEE 754) for all calculations, with specific rounding rules for different operations:

### Financial Calculations
- All financial functions use bankers' rounding (round-to-even) via SimpleRoundTo
- Calculations maintain 6 decimal places for Australian financial standards compliance
- Present Value, Future Value, and Payment calculations use 6 decimal precision
- NPV and IRR calculations use 6 decimal places for intermediate steps
- Depreciation calculations round final results to 6 decimal places

### Statistical Calculations
- Basic statistics (mean, median, etc.) maintain full Double precision
- Standard deviation and variance use full precision for intermediate calculations
- Correlation and covariance maintain precision to avoid cumulative errors
- Z-scores and standardization preserve full Double precision

### Matrix Operations
- Matrix elements stored as Double (64-bit) values
- No rounding applied to preserve precision in linear algebra operations
- Determinant and inverse calculations maintain full Double precision
- Matrix multiplication uses full precision for intermediate sums

### Trigonometric Calculations
- Angular calculations maintain Double precision
- Trigonometric functions use system math library precision
- Triangle calculations preserve full precision for accurate results
- Vector operations maintain Double precision for magnitude and angles

### Precision Limits
- Double type range: ±5.0 × 10^−324 to ±1.7 × 10^308
- Approximately 15-17 significant decimal digits
- Epsilon (smallest difference): 2.2204460492503131e-16
- Pi constant precision: 15 significant digits

### Rounding Behavior
```pascal
// Financial calculations use bankers' rounding to 6 decimals
PV := TFinanceKit.PresentValue(1000, 0.05, 1);    // 952.380952
PMT := TFinanceKit.Payment(10000, 0.05, 10);      // 1075.684172

// Statistical calculations maintain full precision
Mean := TStatsKit.Mean(Data);                     // Full Double precision
StdDev := TStatsKit.StandardDeviation(Data);      // Full Double precision

// Matrix operations preserve precision
Det := TMatrixKit.Determinant(Matrix);            // Full Double precision
```

## Statistical Operations (TStatsKit)

The `TStatsKit` class provides comprehensive statistical calculations.

```pascal
uses TidyKit.Math.Stats;

// Basic statistics
Mean := TStatsKit.Mean(Data);                    // Arithmetic mean
Median := TStatsKit.Median(Data);                // Median value
Mode := TStatsKit.Mode(Data);                    // Most frequent value
Range := TStatsKit.Range(Data);                  // Range (max - min)

// Variance and standard deviation
Var := TStatsKit.Variance(Data);                 // Population variance
StdDev := TStatsKit.StandardDeviation(Data);     // Population std dev
SVar := TStatsKit.SampleVariance(Data);          // Sample variance
SStdDev := TStatsKit.SampleStandardDeviation(Data); // Sample std dev

// Distribution measures
Skew := TStatsKit.Skewness(Data);               // Distribution skewness
Kurt := TStatsKit.Kurtosis(Data);               // Distribution kurtosis
P50 := TStatsKit.Percentile(Data, 50);          // 50th percentile
Q1 := TStatsKit.Quartile1(Data);                // First quartile
Q3 := TStatsKit.Quartile3(Data);                // Third quartile
IQR := TStatsKit.InterquartileRange(Data);      // Interquartile range

// Correlation and covariance
Corr := TStatsKit.Correlation(X, Y);            // Pearson correlation
Cov := TStatsKit.Covariance(X, Y);              // Sample covariance
TStatsKit.Standardize(Data);                    // Convert to z-scores
Z := TStatsKit.ZScore(Value, Mean, StdDev);     // Calculate z-score
```

## Financial Calculations (TFinanceKit)

The `TFinanceKit` class provides financial mathematics calculations.

```pascal
uses TidyKit.Math.Finance;

// Present value calculations
PV := TFinanceKit.PresentValue(FV, Rate, Periods);    // Present value
FV := TFinanceKit.FutureValue(PV, Rate, Periods);     // Future value

// Interest and payments
CI := TFinanceKit.CompoundInterest(Principal, Rate, Periods);  // Compound interest
PMT := TFinanceKit.Payment(PV, Rate, Periods);                 // Periodic payment

// Investment analysis
NPV := TFinanceKit.NetPresentValue(Initial, CashFlows, Rate); // Net present value
IRR := TFinanceKit.InternalRateOfReturn(Initial, CashFlows);  // Internal rate of return

// Depreciation
SLD := TFinanceKit.StraightLineDepreciation(Cost, Salvage, Life);  // Straight-line
DBD := TFinanceKit.DecliningBalanceDepreciation(Cost, Salvage, Life, Period); // Declining

// Return calculations
ROI := TFinanceKit.ReturnOnInvestment(Gain, Cost);            // Return on investment
ROE := TFinanceKit.ReturnOnEquity(NetIncome, Equity);         // Return on equity
```

## Matrix Operations (TMatrixKit)

The `TMatrixKit` class provides matrix operations.

```pascal
uses TidyKit.Math.Matrices;

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
Inv := TMatrixKit.Inverse(A);                    // Matrix inverse

// Matrix properties
Det := TMatrixKit.Determinant(A);                // Calculate determinant
Tr := TMatrixKit.Trace(A);                       // Calculate trace
R := TMatrixKit.Rank(A);                         // Calculate rank

// Matrix decompositions
TMatrixKit.LUDecomposition(A, L, U);             // LU decomposition
TMatrixKit.QRDecomposition(A, Q, R);             // QR decomposition

// Helper functions
Rows := TMatrixKit.GetRows(A);                   // Get number of rows
Cols := TMatrixKit.GetCols(A);                   // Get number of columns
IsSquare := TMatrixKit.IsSquare(A);              // Check if matrix is square
```

## Trigonometric Operations (TTrigKit)

The `TTrigKit` class provides trigonometric calculations.

```pascal
uses TidyKit.Math.Trigonometry;

// Angle conversions
Rad := TTrigKit.DegToRad(Degrees);              // Degrees to radians
Deg := TTrigKit.RadToDeg(Radians);              // Radians to degrees

// Basic trigonometric functions
S := TTrigKit.Sin(X);                           // Sine
C := TTrigKit.Cos(X);                           // Cosine
T := TTrigKit.Tan(X);                           // Tangent

// Inverse trigonometric functions
AS := TTrigKit.ArcSin(X);                       // Inverse sine
AC := TTrigKit.ArcCos(X);                       // Inverse cosine
AT := TTrigKit.ArcTan(X);                       // Inverse tangent
AT2 := TTrigKit.ArcTan2(Y, X);                  // Two-argument inverse tangent

// Hyperbolic functions
SH := TTrigKit.Sinh(X);                         // Hyperbolic sine
CH := TTrigKit.Cosh(X);                         // Hyperbolic cosine
TH := TTrigKit.Tanh(X);                         // Hyperbolic tangent

// Triangle calculations
H := TTrigKit.Hypotenuse(A, B);                 // Calculate hypotenuse
Area1 := TTrigKit.TriangleArea(Base, Height);   // Area from base and height
Area2 := TTrigKit.TriangleAreaSAS(A, Angle, B); // Area from SAS
Area3 := TTrigKit.TriangleAreaSSS(A, B, C);     // Area from three sides

// Vector operations
Mag := TTrigKit.VectorMagnitude(X, Y);          // Vector magnitude
Angle := TTrigKit.VectorAngle(X1, Y1, X2, Y2);  // Angle between vectors
```

## Error Handling

All math operations include appropriate error checking:

- Division by zero
- Invalid matrix dimensions
- Undefined mathematical operations
- Out of range values

Errors are raised using standard Pascal exceptions with descriptive messages.

## Performance Considerations

- All calculations use Double precision (64-bit) IEEE 754 floating-point numbers
- Financial calculations round to 4 decimal places for consistency with financial standards
- Statistical operations maintain full Double precision to minimize cumulative errors
- Matrix operations preserve precision for numerical stability
- Memory usage is approximately 8 bytes per number for Double precision values
- Consider using Single (32-bit) precision only if memory is severely constrained and reduced accuracy is acceptable
- Large matrix operations may require significant memory due to Double precision storage
- Rounding is applied only where specifically required (e.g., financial calculations)
- Intermediate calculations maintain full precision to minimize error propagation

## Examples

### Statistical Analysis
```pascal
var
  Data: TDoubleArray;
  Mean, StdDev: Double;
begin
  Data := TDoubleArray.Create(1, 2, 3, 4, 5);
  Mean := TStatsKit.Mean(Data);
  StdDev := TStatsKit.StandardDeviation(Data);
  WriteLn('Mean: ', Mean:0:2);
  WriteLn('Standard Deviation: ', StdDev:0:2);
end;
```

### Financial Calculation
```pascal
var
  CashFlows: TDoubleArray;
  NPV: Double;
begin
  CashFlows := TDoubleArray.Create(100, 200, 300);
  NPV := TFinanceKit.NetPresentValue(1000, CashFlows, 0.1);
  WriteLn('Net Present Value: ', NPV:0:2);
end;
```

### Matrix Operation
```pascal
var
  A, B, C: TMatrix;
begin
  A := TMatrixKit.CreateMatrix(2, 2);
  B := TMatrixKit.Identity(2);
  A[0,0] := 1; A[0,1] := 2;
  A[1,0] := 3; A[1,1] := 4;
  
  C := TMatrixKit.Multiply(A, B);
  WriteLn('Determinant: ', TMatrixKit.Determinant(C):0:2);
end;
```

### Trigonometric Calculation
```pascal
var
  Angle, Height: Double;
begin
  Angle := TTrigKit.DegToRad(45);
  Height := 10 * TTrigKit.Sin(Angle);
  WriteLn('Height: ', Height:0:2);
end;
``` 