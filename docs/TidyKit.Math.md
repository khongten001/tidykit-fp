# TidyKit Math Library Documentation

The TidyKit Math library provides comprehensive mathematical operations using Double precision floating-point numbers. The library is divided into several specialized modules for different mathematical domains.


## Table of Contents

- [TidyKit Math Library Documentation](#tidykit-math-library-documentation)
  - [Table of Contents](#table-of-contents)
  - [Base Types](#base-types)
  - [Precision in Calculations](#precision-in-calculations)
    - [Financial Calculations](#financial-calculations)
    - [Statistical Calculations](#statistical-calculations)
    - [Matrix Operations](#matrix-operations)
    - [Trigonometric Calculations](#trigonometric-calculations)
      - [Angle Conversions](#angle-conversions)
      - [Basic Trigonometric Functions](#basic-trigonometric-functions)
      - [Inverse Trigonometric Functions](#inverse-trigonometric-functions)
      - [Hyperbolic Functions](#hyperbolic-functions)
      - [Triangle Calculations](#triangle-calculations)
      - [Circle Sector and Segment Calculations](#circle-sector-and-segment-calculations)
      - [Vector Operations](#vector-operations)
      - [Precision Notes](#precision-notes)
    - [Precision Limits](#precision-limits)
    - [Rounding Behavior](#rounding-behavior)
  - [Statistical Operations (TStatsKit)](#statistical-operations-tstatskit)
  - [Financial Calculations (TFinanceKit)](#financial-calculations-tfinancekit)
    - [Time Value of Money](#time-value-of-money)
    - [Investment Analysis](#investment-analysis)
    - [Depreciation](#depreciation)
    - [Return Metrics](#return-metrics)
    - [Modified Duration](#modified-duration)
    - [Black-Scholes Option Pricing](#black-scholes-option-pricing)
    - [Operating Leverage](#operating-leverage)
  - [Precision in Calculations](#precision-in-calculations-1)
    - [Default Precision](#default-precision)
    - [Custom Precision](#custom-precision)
    - [Rounding Behavior](#rounding-behavior-1)
    - [Best Practices](#best-practices)
  - [Matrix Operations (TMatrixKit)](#matrix-operations-tmatrixkit)
    - [Error Handling](#error-handling)
  - [Trigonometric Operations (TTrigKit)](#trigonometric-operations-ttrigkit)
    - [Angle Conventions](#angle-conventions)
    - [Basic Usage](#basic-usage)
    - [Triangle Calculations](#triangle-calculations-1)
    - [Vector Operations](#vector-operations-1)
    - [Common Pitfalls to Avoid](#common-pitfalls-to-avoid)
  - [Error Handling](#error-handling-1)
  - [Performance Considerations](#performance-considerations)
  - [Examples](#examples)
    - [Statistical Analysis](#statistical-analysis)
    - [Financial Calculation](#financial-calculation)
    - [Matrix Operation](#matrix-operation)
    - [Trigonometric Calculation](#trigonometric-calculation)
  - [Distribution Functions](#distribution-functions)
  - [Array Conversion Functions](#array-conversion-functions)
  - [Precision Notes](#precision-notes-1)
    - [Statistical Calculations](#statistical-calculations-1)
    - [Distribution Functions](#distribution-functions-1)
    - [Best Practices](#best-practices-1)



## Base Types

```pascal
uses TidyKit.Math;

type
  TIntegerArray = array of Integer;
  TDoubleArray = array of Double;
  TSingleArray = array of Single;
  TExtendedArray = array of Extended;
  TMatrix = array of array of Double;
  TDoublePair = record
    Lower: Double;
    Upper: Double;
  end;
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
- All angles are in radians unless specified otherwise
- Functions handle special cases (e.g., undefined values) gracefully

#### Angle Conversions
```pascal
// Convert between degrees, radians, and grads
Rad := TTrigKit.DegToRad(Degrees);           // Degrees to radians
Deg := TTrigKit.RadToDeg(Radians);           // Radians to degrees
Rad := TTrigKit.GradToRad(Grads);            // Grads to radians
Grad := TTrigKit.RadToGrad(Radians);         // Radians to grads

// Normalize angles to standard ranges
Rad := TTrigKit.NormalizeAngle(Radians);     // To [0, 2π]
Deg := TTrigKit.NormalizeAngleDeg(Degrees);  // To [0, 360]
```

#### Basic Trigonometric Functions
```pascal
// Primary functions
Sin := TTrigKit.Sin(X);                      // Sine
Cos := TTrigKit.Cos(X);                      // Cosine
Tan := TTrigKit.Tan(X);                      // Tangent

// Reciprocal functions
Sec := TTrigKit.Sec(X);                      // Secant
Csc := TTrigKit.Csc(X);                      // Cosecant
Cot := TTrigKit.Cot(X);                      // Cotangent
```

#### Inverse Trigonometric Functions
```pascal
// Primary inverse functions
ASin := TTrigKit.ArcSin(X);                  // Inverse sine
ACos := TTrigKit.ArcCos(X);                  // Inverse cosine
ATan := TTrigKit.ArcTan(X);                  // Inverse tangent
ATan2 := TTrigKit.ArcTan2(Y, X);            // Two-argument inverse tangent
```

#### Hyperbolic Functions
```pascal
// Direct hyperbolic functions
SinH := TTrigKit.Sinh(X);                    // Hyperbolic sine
CosH := TTrigKit.Cosh(X);                    // Hyperbolic cosine
TanH := TTrigKit.Tanh(X);                    // Hyperbolic tangent

// Inverse hyperbolic functions
ASinH := TTrigKit.ArcSinh(X);                // Inverse hyperbolic sine
ACosH := TTrigKit.ArcCosh(X);                // Inverse hyperbolic cosine
ATanH := TTrigKit.ArcTanh(X);                // Inverse hyperbolic tangent
```

#### Triangle Calculations
```pascal
// Area calculations
Area := TTrigKit.TriangleArea(Base, Height); // From base and height
Area := TTrigKit.TriangleAreaSAS(A, Angle, B); // From SAS
Area := TTrigKit.TriangleAreaSSS(A, B, C);   // From three sides

// Other triangle metrics
Perim := TTrigKit.TrianglePerimeter(A, B, C); // Perimeter
InRad := TTrigKit.TriangleInRadius(A, B, C);  // Inscribed circle radius
CircumRad := TTrigKit.TriangleCircumRadius(A, B, C); // Circumscribed circle radius
```

#### Circle Sector and Segment Calculations
```pascal
// Circular measurements
SectorArea := TTrigKit.CircularSectorArea(R, Angle); // Sector area
SegmentArea := TTrigKit.CircularSegmentArea(R, Angle); // Segment area
ChordLen := TTrigKit.ChordLength(R, Angle);  // Chord length
```

#### Vector Operations
```pascal
// Vector calculations
Mag := TTrigKit.VectorMagnitude(X, Y);       // Vector magnitude
Angle := TTrigKit.VectorAngle(X1, Y1, X2, Y2); // Angle between vectors
```

#### Precision Notes
- All trigonometric functions maintain Double precision (15-17 significant digits)
- Angle conversions are accurate to Double precision
- Triangle calculations preserve precision for accurate geometric results
- Special cases (undefined values, domain errors) are handled with exceptions
- Vector operations maintain precision for both magnitude and angles

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
Variance := TStatsKit.Variance(Data);                 // Sample variance (n-1)
StdDev := TStatsKit.StandardDeviation(Data);     // Population std dev (n)
SVar := TStatsKit.SampleVariance(Data);          // Sample variance (n-1)
SStdDev := TStatsKit.SampleStandardDeviation(Data); // Sample std dev (n-1)

// Distribution measures
Skew := TStatsKit.Skewness(Data);               // Distribution skewness
Kurt := TStatsKit.Kurtosis(Data);               // Distribution kurtosis
P50 := TStatsKit.Percentile(Data, 50);          // 50th percentile
Q1 := TStatsKit.Quartile1(Data);                // First quartile
Q3 := TStatsKit.Quartile3(Data);                // Third quartile
IQR := TStatsKit.InterquartileRange(Data);      // Interquartile range

// Correlation and covariance
PCorr := TStatsKit.PearsonCorrelation(X, Y);    // Pearson correlation
SCorr := TStatsKit.SpearmanCorrelation(X, Y);   // Spearman correlation
Cov := TStatsKit.Covariance(X, Y);              // Sample covariance

// Z-scores and standardization
TStatsKit.Standardize(Data);                    // Convert to z-scores
Z := TStatsKit.ZScore(Value, Mean, StdDev);     // Calculate z-score

// Additional means
GMean := TStatsKit.GeometricMean(Data);         // Geometric mean
HMean := TStatsKit.HarmonicMean(Data);          // Harmonic mean
TMean := TStatsKit.TrimmedMean(Data, 20);       // 20% trimmed mean
WMean := TStatsKit.WinsorizedMean(Data, 20);    // 20% winsorized mean

// Descriptive statistics
Stats := TStatsKit.Describe(Data);              // Get all descriptive stats
SEM := TStatsKit.StandardErrorOfMean(Data);     // Standard error of mean
CV := TStatsKit.CoefficientOfVariation(Data);   // Coefficient of variation

// Robust statistics
MAD := TStatsKit.MedianAbsoluteDeviation(Data); // Median absolute deviation
RSD := TStatsKit.RobustStandardDeviation(Data); // Robust standard deviation
HM := TStatsKit.HuberM(Data, 1.5);             // Huber M-estimator

// Hypothesis testing
TTest := TStatsKit.TTest(X, Y, TPValue);        // Independent t-test
UTest := TStatsKit.MannWhitneyU(X, Y, UPValue); // Mann-Whitney U test
KS := TStatsKit.KolmogorovSmirnovTest(Data, KSPValue); // K-S test
SW := TStatsKit.ShapiroWilkTest(Data, WPValue); // Shapiro-Wilk test

// Effect size measures
D := TStatsKit.CohensD(X, Y);                   // Cohen's d
G := TStatsKit.HedgesG(X, Y);                   // Hedges' g

// Non-parametric tests
Sign := TStatsKit.SignTest(X, Y);               // Sign test
W := TStatsKit.WilcoxonSignedRank(X, Y);        // Wilcoxon signed-rank
Tau := TStatsKit.KendallTau(X, Y);              // Kendall's tau

// Bootstrap methods
CI := TStatsKit.BootstrapConfidenceInterval(Data); // Bootstrap CI
```

## Financial Calculations (TFinanceKit)

The `TFinanceKit` class provides comprehensive financial calculations with configurable precision. All financial functions support an optional `ADecimals` parameter to control the number of decimal places in the result (default is 4 decimal places).

### Time Value of Money
```pascal
// Present Value with default 4 decimals
PV := TFinanceKit.PresentValue(FutureValue, Rate, Periods);

// Present Value with 6 decimal precision
PV := TFinanceKit.PresentValue(FutureValue, Rate, Periods, 6);

// Future Value with custom precision
FV := TFinanceKit.FutureValue(PresentValue, Rate, Periods, 3);
```

### Investment Analysis
```pascal
var
  CashFlows: TDoubleArray;
begin
  CashFlows := TDoubleArray.Create(100, 200, 300);
  
  // NPV with default precision (4 decimals)
  NPV := TFinanceKit.NetPresentValue(InitialInvestment, CashFlows, Rate);
  
  // NPV with 6 decimal precision
  NPV := TFinanceKit.NetPresentValue(InitialInvestment, CashFlows, Rate, 6);
  
  // IRR with custom precision
  IRR := TFinanceKit.InternalRateOfReturn(InitialInvestment, CashFlows, 3);
end;
```

### Depreciation
```pascal
// Straight-line depreciation with default precision
SLD := TFinanceKit.StraightLineDepreciation(Cost, Salvage, Life);

// Declining balance depreciation with 3 decimal precision
DDB := TFinanceKit.DecliningBalanceDepreciation(Cost, Salvage, Life, Period, 3);
```

### Return Metrics
```pascal
// ROI with default precision
ROI := TFinanceKit.ReturnOnInvestment(Gain, Cost);

// ROE with 5 decimal precision
ROE := TFinanceKit.ReturnOnEquity(NetIncome, ShareholdersEquity, 5);
```

### Modified Duration
```pascal
// Modified Duration calculation with default precision (4 decimals)
// For a 5-year bond with 6% annual coupon, 5% yield, semi-annual payments
// Expected value: 4.3009
ModDur := TFinanceKit.ModifiedDuration(
  1000.0,  // Face value
  0.06,    // Coupon rate (6%)
  0.05,    // Yield rate (5%)
  2,       // Periods per year (semi-annual)
  5        // Years to maturity
);
```

### Black-Scholes Option Pricing
```pascal
// Black-Scholes calculation with default precision (4 decimals)
// For a stock with:
// - Spot price: 100.0
// - Strike price: 100.0
// - Risk-free rate: 5%
// - Volatility: 20%
// - Time to maturity: 1 year
// Expected values:
// - Call option: 10.4506
// - Put option: 5.5723
CallPrice := TFinanceKit.BlackScholes(
  100.0,   // Spot price
  100.0,   // Strike price
  0.05,    // Risk-free rate (5%)
  0.20,    // Volatility (20%)
  1.0,     // Time to maturity (1 year)
  otCall   // Option type
);

PutPrice := TFinanceKit.BlackScholes(
  100.0,   // Spot price
  100.0,   // Strike price
  0.05,    // Risk-free rate (5%)
  0.20,    // Volatility (20%)
  1.0,     // Time to maturity (1 year)
  otPut    // Option type
);
```

### Operating Leverage
```pascal
// Operating Leverage calculation with default precision (4 decimals)
// For a business with:
// - Quantity: 10,000 units
// - Price per unit: $50
// - Variable cost per unit: $30
// - Fixed costs: $100,000
// Expected values:
// - DOL: 2.0000
// - Break-even point: 5,000 units
Leverage := TFinanceKit.OperatingLeverage(
  10000.0,  // Quantity
  50.0,     // Price per unit
  30.0,     // Variable cost per unit
  100000.0  // Fixed costs
);

// The Degree of Operating Leverage (DOL) is calculated as:
// DOL = (Q × CM) / EBIT
// where:
// Q = Quantity
// CM = Contribution Margin (Price - Variable Cost)
// EBIT = Earnings Before Interest and Taxes
//      = Q × CM - Fixed Costs
```

## Precision in Calculations

### Default Precision
- All financial calculations default to 4 decimal places for consistency
- This provides sufficient precision for most financial calculations while avoiding floating-point comparison issues

### Custom Precision
- Each financial function accepts an optional `ADecimals` parameter
- Range: typically 0 to 10 decimal places
- Examples:
  - `ADecimals = 2`: For monetary values (e.g., $123.45)
  - `ADecimals = 4`: Default, suitable for most calculations
  - `ADecimals = 6`: For high-precision requirements

### Rounding Behavior
- Uses banker's rounding (symmetric arithmetic rounding)
- Implemented via `SimpleRoundTo` function
- Ensures consistent results across calculations

### Best Practices
1. Use default precision (4 decimals) unless specific requirements exist
2. For monetary display, round to 2 decimals
3. For rate calculations, consider using 4-6 decimals
4. For internal calculations, use higher precision (6+ decimals)
5. Always use the same precision when comparing values

## Matrix Operations (TMatrixKit)

The `TMatrixKit` class provides basic matrix operations with Double precision.

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

// Matrix properties
Det := TMatrixKit.Determinant(A);                // Calculate determinant
Tr := TMatrixKit.Trace(A);                       // Calculate trace

// Helper functions
Rows := TMatrixKit.GetRows(A);                   // Get number of rows
Cols := TMatrixKit.GetCols(A);                   // Get number of columns
IsSquare := TMatrixKit.IsSquare(A);              // Check if matrix is square

// Note: The following features are planned for future implementation:
// - Matrix rank calculation
// - Matrix inversion
// - LU decomposition
// - QR decomposition
```

### Error Handling

Matrix operations include appropriate error checking:

- Invalid matrix dimensions (e.g., multiplication of incompatible matrices)
- Matrix size mismatches in operations (e.g., adding matrices of different sizes)
- Out of memory conditions when creating new matrices

Errors are raised using standard Pascal exceptions with descriptive messages.

## Trigonometric Operations (TTrigKit)

The `TTrigKit` class provides comprehensive trigonometric calculations. All angle-related functions follow these conventions:

### Angle Conventions
- All trigonometric functions (Sin, Cos, Tan) expect input angles in **radians**
- All inverse trigonometric functions (ArcSin, ArcCos, ArcTan, ArcTan2) return angles in **radians**
- Vector angle calculations return results in **radians**
- Use `DegToRad` and `RadToDeg` functions for angle conversions when working with degrees

### Basic Usage

```pascal
uses TidyKit.Math.Trigonometry;

// Converting between degrees and radians
Rad := TTrigKit.DegToRad(45);                   // Convert 45° to radians
Deg := TTrigKit.RadToDeg(Pi/4);                 // Convert π/4 rad to degrees

// Basic trigonometric functions (input in radians)
S := TTrigKit.Sin(Pi/6);                        // sin(π/6)
C := TTrigKit.Cos(Pi/3);                        // cos(π/3)
T := TTrigKit.Tan(Pi/4);                        // tan(π/4)

// Working with degrees (convert to radians first)
angle_deg := 45;
sin_45 := TTrigKit.Sin(TTrigKit.DegToRad(45));  // sin(45°)

// Inverse trigonometric functions (return radians)
AS := TTrigKit.ArcSin(0.5);                     // Returns angle in radians
AC := TTrigKit.ArcCos(0.5);                     // Returns angle in radians
AT := TTrigKit.ArcTan(1.0);                     // Returns angle in radians
AT2 := TTrigKit.ArcTan2(Y, X);                  // Returns angle in radians

// Convert inverse function results to degrees if needed
angle_deg := TTrigKit.RadToDeg(AS);             // Convert result to degrees
```

### Triangle Calculations

```pascal
// Basic triangle calculations
H := TTrigKit.Hypotenuse(3, 4);                 // Calculate hypotenuse
Area1 := TTrigKit.TriangleArea(Base, Height);   // Area from base and height

// Note: TriangleAreaSAS expects angle in radians
Area2 := TTrigKit.TriangleAreaSAS(4, Pi/3, 5);  // Area using angle in radians
// If you have angle in degrees, convert it:
Area3 := TTrigKit.TriangleAreaSAS(4, TTrigKit.DegToRad(60), 5);  // 60° angle

// Area from three sides (no angles needed)
Area4 := TTrigKit.TriangleAreaSSS(3, 4, 5);     // Area from three sides
```

### Vector Operations

```pascal
// Vector calculations
Mag := TTrigKit.VectorMagnitude(X, Y);          // Vector magnitude

// Vector angle returns result in radians
Angle := TTrigKit.VectorAngle(X1, Y1, X2, Y2);  // Returns angle in radians
// Convert to degrees if needed
AngleDeg := TTrigKit.RadToDeg(Angle);           // Convert to degrees
```

### Common Pitfalls to Avoid

1. **Wrong Angle Units**: Always ensure angles are in the correct units:
   ```pascal
   // INCORRECT - using degrees directly
   S := TTrigKit.Sin(45);  // Wrong! 45 degrees
   
   // CORRECT - convert to radians first
   S := TTrigKit.Sin(TTrigKit.DegToRad(45));  // Correct!
   ```

2. **Not Converting Inverse Function Results**: Remember that inverse functions return radians:
   ```pascal
   // INCORRECT - assuming result is in degrees
   angle_deg := TTrigKit.ArcSin(0.5);  // Wrong! Result is in radians
   
   // CORRECT - convert to degrees if needed
   angle_deg := TTrigKit.RadToDeg(TTrigKit.ArcSin(0.5));  // Correct!
   ```

3. **Triangle Area with Angle**: The `TriangleAreaSAS` function expects the angle in radians:
   ```pascal
   // INCORRECT - using degrees
   Area := TTrigKit.TriangleAreaSAS(4, 60, 5);  // Wrong! 60 degrees
   
   // CORRECT - convert to radians
   Area := TTrigKit.TriangleAreaSAS(4, TTrigKit.DegToRad(60), 5);  // Correct!
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

## Distribution Functions

The library provides several statistical distribution functions:

```pascal
uses TidyKit.Math;

// Distribution functions
P := StudentT(DF, X);        // Student's t-distribution CDF
P := BetaInc(A, B, X);       // Incomplete beta function
B := Beta(Z, W);             // Beta function
G := GammaLn(X);             // Natural log of gamma function
P := NormalCDF(X);           // Standard normal CDF
E := Erf(X);                 // Error function
```

## Array Conversion Functions

```pascal
uses TidyKit.Math;

// Convert arrays to TDoubleArray
DArr := ToDoubleArray(IntArray);    // From TIntegerArray
DArr := ToDoubleArray(SngArray);    // From TSingleArray
DArr := ToDoubleArray(ExtArray);    // From TExtendedArray
```

## Precision Notes

### Statistical Calculations
- Basic statistics (mean, median, etc.) maintain full Double precision
- Standard deviation and variance use full precision for intermediate calculations
- Correlation and covariance maintain precision to avoid cumulative errors
- Z-scores and standardization preserve full Double precision

### Distribution Functions
- All distribution functions maintain full Double precision
- Error function (Erf) uses polynomial approximation with high precision
- Beta and Gamma functions use accurate series expansions
- Normal CDF uses error function for high precision

### Best Practices
1. Use population statistics (StandardDeviation, Variance) when working with complete populations
2. Use sample statistics (SampleStandardDeviation, SampleVariance) when working with samples
3. For robust statistics, consider using MedianAbsoluteDeviation or RobustStandardDeviation
4. Use bootstrap methods for non-parametric confidence intervals
5. Check distribution normality with ShapiroWilkTest before using parametric tests 