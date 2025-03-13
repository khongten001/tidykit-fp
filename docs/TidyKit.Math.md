# TidyKit Math Library

The TidyKit Math library provides comprehensive mathematical operations using Double precision floating-point numbers. The library is divided into several specialized modules for different mathematical domains.

## Modules

The TidyKit Math library consists of the following modules:

- [**TidyKit.Math.Stats**](TidyKit.Stats.md) - Statistical analysis and calculations
- [**TidyKit.Math.Finance**](TidyKit.Finance.md) - Financial mathematics and calculations
- [**TidyKit.Math.Matrices**](TidyKit.Math.Matrices.md) - Matrix operations and linear algebra
- [**TidyKit.Math.Trigonometry**](TidyKit.Trigonometry.md) - Trigonometric functions and calculations

## Base Types

```pascal
uses TidyKit.Math;

type
  TIntegerArray = array of Integer;
  TDoubleArray = array of Double;
  TSingleArray = array of Single;
  TExtendedArray = array of Extended;
  TDoublePair = record
    Lower: Double;
    Upper: Double;
  end;
```

## Precision in Calculations

TidyKit's math library uses Double precision (64-bit IEEE 754) for all calculations, with specific rounding rules for different operations:

### Precision Limits
- Double type range: ±5.0 × 10^−324 to ±1.7 × 10^308
- Approximately 15-17 significant decimal digits
- Epsilon (smallest difference): 2.2204460492503131e-16
- Pi constant precision: 15 significant digits

## Distribution Functions

TidyKit.Math provides several statistical distribution functions:

```pascal
function StudentT(const DF: Integer; const X: Double): Double;
function BetaInc(const A, B, X: Double): Double;
function Beta(const Z, W: Double): Double;
function GammaLn(const X: Double): Double;
function NormalCDF(const X: Double): Double;
function Erf(const X: Double): Double;
```

## Array Conversion Functions

The library includes functions to convert between different numeric array types:

```pascal
function ToDoubleArray(const Data: TIntegerArray): TDoubleArray;
function ToDoubleArray(const Data: TSingleArray): TDoubleArray;
function ToDoubleArray(const Data: TExtendedArray): TDoubleArray;
```

## Error Handling

TidyKit's math library includes comprehensive error handling:

- Domain errors (e.g., taking square root of negative numbers)
- Overflow and underflow conditions
- Invalid input values
- Division by zero scenarios

Errors are raised using standard Pascal exceptions with descriptive messages.

## Performance Considerations

- All calculations use Double precision for optimal balance of speed and accuracy
- Critical operations are optimized for performance
- Large dataset operations are designed to minimize memory usage

For detailed information on specific mathematical domains, please refer to the respective module documentation linked above. 