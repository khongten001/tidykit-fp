unit TidyKit.Math;

{-----------------------------------------------------------------------------
 TidyKit.Math

 Foundational mathematical types and functions for the TidyKit.Math.* libraries
 
 This unit provides:
 - Common numeric array types (TIntegerArray, TDoubleArray, etc.)
 - Helper records (TDoublePair)
 - Core mathematical functions used by other TidyKit math units:
   - Statistical distributions (StudentT, NormalCDF)
   - Special functions (Beta, GammaLn, Erf)
   - Array type conversion helpers (ToDoubleArray)
 
 Design principles:
 - Provide base types for consistency across TidyKit.Math.* units
 - Encapsulate complex mathematical approximations (e.g., CDFs)
 - Support other TidyKit modules with necessary mathematical tools
-----------------------------------------------------------------------------}

{$mode objfpc}{$H+}{$J-}
{$modeswitch advancedrecords}

{
  TidyKit.Math is a helper unit providing foundational mathematical types and functions
  for other specialized math units in the TidyKit library (Statistics, Finance, etc.)
}

interface

uses
  Classes, SysUtils;

type
  
  { Standardized numeric array types for consistent type handling across the library }
  TIntegerArray = array of Integer;
  TDoubleArray = array of Double;
  TSingleArray = array of Single;
  TExtendedArray = array of Extended;
  
  { A record containing two double values representing a range or interval }
  TDoublePair = record
    Lower: Double;
    Upper: Double;
  end;

{
  @description Statistical distribution function for the Student's t cumulative distribution
  
  @usage Use to calculate p-values and critical values in hypothesis testing and when
         working with small samples where the normal approximation is not appropriate
  
  @param DF Degrees of freedom (integer)
  @param X The value for which to evaluate the CDF
  
  @returns The probability that a value sampled from a t-distribution with DF degrees of
           freedom is less than X
  
  @warning Numerical approximation, accuracy may diminish for very large or small values
  
  @example
    // Calculate the probability of observing a value less than 2.5
    // for a t-distribution with 10 degrees of freedom
    Probability := StudentT(10, 2.5);
}
function StudentT(const DF: Integer; const X: Double): Double;

{
  @description Incomplete beta function, used as a building block for many statistical distributions
  
  @usage Used internally by distribution functions; rarely needed directly
  
  @param A First shape parameter 
  @param B Second shape parameter
  @param X Value at which to evaluate the function (between 0 and 1)
  
  @returns The value of the incomplete beta function at X
}
function BetaInc(const A, B, X: Double): Double;

{
  @description Beta function, a special function defined by B(z,w) = Γ(z)Γ(w)/Γ(z+w)
  
  @usage Used in probability theory and mathematical statistics
  
  @param Z First parameter
  @param W Second parameter
  
  @returns The value of the beta function
}
function Beta(const Z, W: Double): Double;

{
  @description Natural logarithm of the gamma function
  
  @usage Used as a building block for many statistical calculations,
         especially when working with distributions
  
  @param X Input value (must be positive)
  
  @returns The natural logarithm of the gamma function at X
  
  @warning Computes ln(Γ(x)) rather than Γ(x) directly to avoid overflow
           for large values of X
}
function GammaLn(const X: Double): Double;

{
  @description Standard normal cumulative distribution function (CDF)
  
  @usage Use to calculate probabilities for the normal distribution
  
  @param X The value for which to evaluate the CDF
  
  @returns The probability that a standard normal random variable is less than X
  
  @example
    // Calculate the probability that a standard normal random variable
    // is less than 1.96 (97.5th percentile)
    Probability := NormalCDF(1.96); // Expected result: approx. 0.975
}
function NormalCDF(const X: Double): Double;

{
  @description Error function, a special function used in probability and statistics
  
  @usage Used as a building block for the normal distribution and related functions
  
  @param X Input value
  
  @returns The value of the error function at X
}
function Erf(const X: Double): Double;

{
  @description Converts an array of Integer values to an array of Double values
  
  @usage Use when you need to perform double-precision calculations on integer data
  
  @param Data The source integer array
  
  @returns A double-precision array with the same values
  
  @example
    var
      IntArray: TIntegerArray;
      DoubleArray: TDoubleArray;
    begin
      SetLength(IntArray, 3);
      IntArray[0] := 1;
      IntArray[1] := 2;
      IntArray[2] := 3;
      
      DoubleArray := ToDoubleArray(IntArray);
      // DoubleArray now contains [1.0, 2.0, 3.0]
    end;
}
function ToDoubleArray(const Data: TIntegerArray): TDoubleArray;

{
  @description Converts an array of Single values to an array of Double values
  
  @usage Use when you need increased precision for calculations
  
  @param Data The source single-precision array
  
  @returns A double-precision array with the same values
  
  @example
    var
      SingleArray: TSingleArray;
      DoubleArray: TDoubleArray;
    begin
      SetLength(SingleArray, 2);
      SingleArray[0] := 1.5;
      SingleArray[1] := 2.5;
      
      DoubleArray := ToDoubleArray(SingleArray);
      // DoubleArray now contains [1.5, 2.5] with double precision
    end;
}
function ToDoubleArray(const Data: TSingleArray): TDoubleArray;

{
  @description Converts an array of Extended values to an array of Double values
  
  @usage Use when you need to standardize extended-precision data to double precision
         for consistent calculations
  
  @param Data The source extended-precision array
  
  @returns A double-precision array with the same values
  
  @warning May lose precision if Extended has higher precision than Double 
           on your platform
  
  @example
    var
      ExtArray: TExtendedArray;
      DoubleArray: TDoubleArray;
    begin
      SetLength(ExtArray, 2);
      ExtArray[0] := 1.1;
      ExtArray[1] := 2.2;
      
      DoubleArray := ToDoubleArray(ExtArray);
      // DoubleArray now contains [1.1, 2.2] with double precision
    end;
}
function ToDoubleArray(const Data: TExtendedArray): TDoubleArray;

implementation

function ToDoubleArray(const Data: TIntegerArray): TDoubleArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Data));
  for I := 0 to High(Data) do
    Result[I] := Data[I];
end;

function ToDoubleArray(const Data: TSingleArray): TDoubleArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Data));
  for I := 0 to High(Data) do
    Result[I] := Data[I];
end;

function ToDoubleArray(const Data: TExtendedArray): TDoubleArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Data));
  for I := 0 to High(Data) do
    Result[I] := Data[I];
end;

function GammaLn(const X: Double): Double;
const
  Coefficients: array[0..5] of Double = (
    76.18009172947146,
    -86.50532032941677,
    24.01409824083091,
    -1.231739572450155,
    0.1208650973866179e-2,
    -0.5395239384953e-5
  );
var
  Y, Tmp, Ser: Double;
  J: Integer;
begin
  Y := X;
  Tmp := X + 5.5;
  Tmp := (X + 0.5) * Ln(Tmp) - Tmp;
  Ser := 1.000000000190015;
  for J := 0 to 5 do
    Ser := Ser + Coefficients[J] / (Y + J + 1);
  Result := Tmp + Ln(2.5066282746310005 * Ser / X);
end;

function Beta(const Z, W: Double): Double;
begin
  Result := Exp(GammaLn(Z) + GammaLn(W) - GammaLn(Z + W));
end;

function BetaInc(const A, B, X: Double): Double;
const
  MaxIter = 100;
  Epsilon = 3.0e-7;
var
  Qab, Qap, Qam, Am, Bm, Bz, Em, Tem, D, Ap, Bp, App, Bpp: Double;
  M: Integer;
begin
  if X = 0 then
    Exit(0)
  else if X = 1 then
    Exit(1);

  Qab := A + B;
  Qap := A + 1.0;
  Qam := A - 1.0;
  Bz := 1.0 - Qab * X / Qap;
  
  for M := 1 to MaxIter do
  begin
    Em := M;
    Tem := Em + Em;
    D := Em * (B - Em) * X / ((Qam + Tem) * (A + Tem));
    Ap := Bz + D * Am;
    Bp := 1.0 + D / Bm;
    
    if (Abs(Ap - 1.0) < Epsilon) and (Abs(Bp - 1.0) < Epsilon) then
      Break;
      
    Am := Ap;
    Bm := Bp;
    
    D := -(A + Em) * (Qab + Em) * X / ((A + Tem) * (Qap + Tem));
    App := Am + D * Bz;
    Bpp := Bp + D / Am;
    
    if (Abs(App - 1.0) < Epsilon) and (Abs(Bpp - 1.0) < Epsilon) then
      Break;
      
    Bz := App / Bpp;
  end;
  
  Result := Bz;
end;

function StudentT(const DF: Integer; const X: Double): Double;
var
  A: Double;
begin
  A := (DF + 1) / 2;
  Result := 1 - 0.5 * BetaInc(A, 0.5, DF / (DF + Sqr(X)));
end;

function Erf(const X: Double): Double;
const
  A1 = 0.254829592;
  A2 = -0.284496736;
  A3 = 1.421413741;
  A4 = -1.453152027;
  A5 = 1.061405429;
  P = 0.3275911;
var
  T, Y: Double;
  Sign: Double;
begin
  if X < 0 then
    Sign := -1
  else
    Sign := 1;
    
  Y := 1.0 / (1.0 + P * Abs(X));
  Result := Sign * (1 - ((((A5 * Y + A4) * Y + A3) * Y + A2) * Y + A1) * Y * Exp(-X * X));
end;

function NormalCDF(const X: Double): Double;
begin
  Result := 0.5 * (1 + Erf(X / Sqrt(2)));
end;

end. 
