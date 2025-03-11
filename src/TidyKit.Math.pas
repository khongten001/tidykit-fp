unit TidyKit.Math;

{$mode objfpc}{$H+}{$J-}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Math;

type
  { Array types for different numeric data }
  TIntegerArray = array of Integer;
  TDoubleArray = array of Double;
  TSingleArray = array of Single;
  TExtendedArray = array of Extended;
  
  { Pair of doubles used in various mathematical operations }
  TDoublePair = record
    Lower: Double;
    Upper: Double;
  end;

{ Statistical distribution functions }
function StudentT(const DF: Integer; const X: Double): Double;
function BetaInc(const A, B, X: Double): Double;
function Beta(const Z, W: Double): Double;
function GammaLn(const X: Double): Double;
function NormalCDF(const X: Double): Double;
function Erf(const X: Double): Double;

{ Array conversion functions }
function ToDoubleArray(const Data: TIntegerArray): TDoubleArray;
function ToDoubleArray(const Data: TSingleArray): TDoubleArray;
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
