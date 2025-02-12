# TidyKit Math Modules

The TidyKit Math modules provide comprehensive mathematical operations through specialized generic classes.

## Statistics (TStatsKit)

```pascal
type
  generic TStatsKit<T> = class
```

### Basic Statistics
- `Mean(const Data: TArray<T>)`: Calculate arithmetic mean
- `Median(const Data: TArray<T>)`: Calculate median value
- `Mode(const Data: TArray<T>)`: Find most frequent value
- `Range(const Data: TArray<T>)`: Calculate range (max - min)

### Variance and Standard Deviation
- `Variance(const Data: TArray<T>)`: Population variance
- `StandardDeviation(const Data: TArray<T>)`: Population standard deviation
- `SampleVariance(const Data: TArray<T>)`: Sample variance
- `SampleStandardDeviation(const Data: TArray<T>)`: Sample standard deviation

### Distribution Measures
- `Skewness(const Data: TArray<T>)`: Calculate distribution skewness
- `Kurtosis(const Data: TArray<T>)`: Calculate distribution kurtosis
- `Percentile(const Data: TArray<T>; const P: T)`: Calculate P-th percentile
- `Quartile1(const Data: TArray<T>)`: First quartile (25th percentile)
- `Quartile3(const Data: TArray<T>)`: Third quartile (75th percentile)
- `InterquartileRange(const Data: TArray<T>)`: IQR (Q3 - Q1)

### Correlation and Covariance
- `Correlation(const X, Y: TArray<T>)`: Pearson correlation coefficient
- `Covariance(const X, Y: TArray<T>)`: Sample covariance
- `Standardize(var Data: TArray<T>)`: Convert to z-scores
- `ZScore(const Value, Mean, StdDev: T)`: Calculate z-score

## Finance (TFinanceKit)

```pascal
type
  generic TFinanceKit<T> = class
```

### Present Value Calculations
- `PresentValue(const FutureValue, Rate: T; const Periods: Integer)`: Calculate present value
- `FutureValue(const PresentValue, Rate: T; const Periods: Integer)`: Calculate future value

### Interest and Payments
- `CompoundInterest(const Principal, Rate: T; const Periods: Integer)`: Calculate compound interest
- `Payment(const PresentValue, Rate: T; const Periods: Integer)`: Calculate periodic payment

### Investment Analysis
- `NetPresentValue(const InitialInvestment: T; const CashFlows: TArray<T>; const Rate: T)`: Calculate NPV
- `InternalRateOfReturn(const InitialInvestment: T; const CashFlows: TArray<T>)`: Calculate IRR

### Depreciation
- `StraightLineDepreciation(const Cost, Salvage: T; const Life: Integer)`: Straight-line depreciation
- `DecliningBalanceDepreciation(const Cost, Salvage: T; const Life, Period: Integer)`: Declining balance depreciation

### Return Calculations
- `ReturnOnInvestment(const Gain, Cost: T)`: Calculate ROI
- `ReturnOnEquity(const NetIncome, ShareholdersEquity: T)`: Calculate ROE

## Trigonometry (TTrigKit)

```pascal
type
  generic TTrigKit<T> = class
```

### Angle Conversions
- `DegToRad(const Degrees: T)`: Convert degrees to radians
- `RadToDeg(const Radians: T)`: Convert radians to degrees

### Basic Trigonometric Functions
- `Sin(const X: T)`: Sine function
- `Cos(const X: T)`: Cosine function
- `Tan(const X: T)`: Tangent function

### Inverse Trigonometric Functions
- `ArcSin(const X: T)`: Inverse sine
- `ArcCos(const X: T)`: Inverse cosine
- `ArcTan(const X: T)`: Inverse tangent
- `ArcTan2(const Y, X: T)`: Two-argument inverse tangent

### Hyperbolic Functions
- `Sinh(const X: T)`: Hyperbolic sine
- `Cosh(const X: T)`: Hyperbolic cosine
- `Tanh(const X: T)`: Hyperbolic tangent

### Triangle Calculations
- `Hypotenuse(const A, B: T)`: Calculate hypotenuse
- `TriangleArea(const Base, Height: T)`: Calculate triangle area
- `TriangleAreaSAS(const SideA, Angle, SideB: T)`: Area using SAS
- `TriangleAreaSSS(const A, B, C: T)`: Area using three sides

### Vector Operations
- `VectorMagnitude(const X, Y: T)`: Calculate vector magnitude
- `VectorAngle(const X1, Y1, X2, Y2: T)`: Calculate angle between vectors

## Matrices (TMatrixKit)

```pascal
type
  generic TMatrixKit<T> = class
```

### Matrix Creation
- `CreateMatrix(const Rows, Cols: Integer)`: Create empty matrix
- `Identity(const Size: Integer)`: Create identity matrix
- `Zeros(const Rows, Cols: Integer)`: Create zero matrix
- `Ones(const Rows, Cols: Integer)`: Create matrix of ones

### Basic Operations
- `Add(const A, B: TMatrixType)`: Matrix addition
- `Subtract(const A, B: TMatrixType)`: Matrix subtraction
- `Multiply(const A, B: TMatrixType)`: Matrix multiplication
- `ScalarMultiply(const A: TMatrixType; const Scalar: T)`: Scalar multiplication

### Matrix Properties
- `Transpose(const A: TMatrixType)`: Matrix transpose
- `GetRows(const A: TMatrixType)`: Get number of rows
- `GetCols(const A: TMatrixType)`: Get number of columns

### Matrix Operations
- `Determinant(const A: TMatrixType)`: Calculate determinant
- `Inverse(const A: TMatrixType)`: Calculate inverse matrix
- `LUDecomposition(const A: TMatrixType; out L, U: TMatrixType)`: LU decomposition

## Examples

### Statistics Example
```pascal
type
  TStats = specialize TStatsKit<Double>;
var
  Data: TArray<Double>;
begin
  Data := TArray<Double>.Create(1, 2, 3, 4, 5);
  WriteLn('Mean: ', TStats.Mean(Data):0:2);
  WriteLn('Std Dev: ', TStats.StandardDeviation(Data):0:2);
end;
```

### Finance Example
```pascal
type
  TFinance = specialize TFinanceKit<Double>;
var
  CashFlows: TArray<Double>;
begin
  CashFlows := TArray<Double>.Create(100, 200, 300);
  WriteLn('NPV: ', TFinance.NetPresentValue(1000, CashFlows, 0.1):0:2);
end;
```

### Trigonometry Example
```pascal
type
  TTrig = specialize TTrigKit<Double>;
begin
  WriteLn('Sin(45°): ', TTrig.Sin(TTrig.DegToRad(45)):0:4);
  WriteLn('Hypotenuse(3,4): ', TTrig.Hypotenuse(3, 4):0:2);
end;
```

### Matrix Example
```pascal
type
  TMatrix = specialize TMatrixKit<Double>;
var
  A, B, C: TMatrix.TMatrixType;
begin
  A := TMatrix.CreateMatrix(2, 2);
  B := TMatrix.CreateMatrix(2, 2);
  // Fill matrices...
  C := TMatrix.Multiply(A, B);
end;
``` 