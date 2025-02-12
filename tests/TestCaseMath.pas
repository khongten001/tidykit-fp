unit TestCaseMath;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, Math,
  TidyKit.Math,
  TidyKit.Math.Stats,
  TidyKit.Math.Finance,
  TidyKit.Math.Trigonometry,
  TidyKit.Math.Matrices;

type
  { Base test case for math operations }
  TTestCaseMathBase = class(TTestCase)
  protected
    const
      EPSILON = 1E-6;  // Tolerance for floating-point comparisons
    
    procedure AssertEquals(const Expected, Actual: Double; const Msg: String = ''); overload;
  end;

  { Test cases for statistical operations }
  TTestCaseStats = class(TTestCaseMathBase)
  published
    procedure TestMean;
    procedure TestMedian;
    procedure TestMode;
    procedure TestRange;
    procedure TestVariance;
    procedure TestStandardDeviation;
    procedure TestSkewness;
    procedure TestKurtosis;
    procedure TestPercentile;
    procedure TestQuartiles;
    procedure TestCorrelation;
    procedure TestCovariance;
    procedure TestZScore;
  end;

  { Test cases for financial operations }
  TTestCaseFinance = class(TTestCaseMathBase)
  published
    procedure TestPresentValue;
    procedure TestFutureValue;
    procedure TestCompoundInterest;
    procedure TestPayment;
    procedure TestNetPresentValue;
    procedure TestInternalRateOfReturn;
    procedure TestDepreciation;
    procedure TestROI;
  end;

  { Test cases for trigonometric operations }
  TTestCaseTrig = class(TTestCaseMathBase)
  published
    procedure TestAngleConversions;
    procedure TestBasicTrig;
    procedure TestInverseTrig;
    procedure TestHyperbolic;
    procedure TestTriangleCalculations;
    procedure TestVectorOperations;
  end;

  { Test cases for matrix operations }
  TTestCaseMatrix = class(TTestCaseMathBase)
  published
    procedure TestMatrixCreation;
    procedure TestMatrixAddition;
    procedure TestMatrixSubtraction;
    procedure TestMatrixMultiplication;
    procedure TestMatrixTranspose;
    procedure TestMatrixDeterminant;
  end;

implementation

{ TTestCaseMathBase }

procedure TTestCaseMathBase.AssertEquals(const Expected, Actual: Double; const Msg: String);
begin
  AssertTrue(Msg, Abs(Expected - Actual) < EPSILON);
end;

{ TTestCaseStats }

procedure TTestCaseStats.TestMean;
var
  Data: TDoubleArray;
begin
  Data := TDoubleArray.Create(1, 2, 3, 4, 5);
  AssertEquals(3.0, TStatsKit.Mean(Data), 'Mean calculation failed');
  
  Data := TDoubleArray.Create(2.5, 3.5, 4.5);
  AssertEquals(3.5, TStatsKit.Mean(Data), 'Mean with decimals failed');
end;

procedure TTestCaseStats.TestMedian;
var
  Data: TDoubleArray;
begin
  Data := TDoubleArray.Create(1, 3, 2, 4, 5);
  AssertEquals(3.0, TStatsKit.Median(Data), 'Median odd count failed');
  
  Data := TDoubleArray.Create(1, 2, 3, 4);
  AssertEquals(2.5, TStatsKit.Median(Data), 'Median even count failed');
end;

procedure TTestCaseStats.TestMode;
var
  Data: TDoubleArray;
begin
  Data := TDoubleArray.Create(1, 2, 2, 3, 2, 4);
  AssertEquals(2.0, TStatsKit.Mode(Data), 'Mode calculation failed');
end;

procedure TTestCaseStats.TestRange;
var
  Data: TDoubleArray;
begin
  Data := TDoubleArray.Create(1, 2, 3, 4, 5);
  AssertEquals(4.0, TStatsKit.Range(Data), 'Range calculation failed');
end;

procedure TTestCaseStats.TestVariance;
var
  Data: TDoubleArray;
begin
  Data := TDoubleArray.Create(2, 4, 4, 4, 6);
  AssertEquals(2.0, TStatsKit.Variance(Data), 'Variance calculation failed');
end;

procedure TTestCaseStats.TestStandardDeviation;
var
  Data: TDoubleArray;
begin
  Data := TDoubleArray.Create(2, 4, 4, 4, 6);
  AssertEquals(Sqrt(2.0), TStatsKit.StandardDeviation(Data), 'Standard deviation failed');
end;

procedure TTestCaseStats.TestSkewness;
var
  Data: TDoubleArray;
begin
  Data := TDoubleArray.Create(2, 4, 4, 4, 6, 6, 6, 8);
  // Test for approximate symmetry
  AssertTrue('Skewness should be close to 0 for symmetric data',
             Abs(TStatsKit.Skewness(Data)) < 0.5);
end;

procedure TTestCaseStats.TestKurtosis;
var
  Data: TDoubleArray;
begin
  Data := TDoubleArray.Create(2, 4, 4, 4, 6, 6, 6, 8);
  // Test for normal-like distribution
  AssertTrue('Kurtosis should be close to 0 for normal-like data',
             Abs(TStatsKit.Kurtosis(Data)) < 1.0);
end;

procedure TTestCaseStats.TestPercentile;
var
  Data: TDoubleArray;
begin
  Data := TDoubleArray.Create(1, 2, 3, 4, 5);
  AssertEquals(3.0, TStatsKit.Percentile(Data, 50), '50th percentile failed');
  AssertEquals(1.0, TStatsKit.Percentile(Data, 0), '0th percentile failed');
  AssertEquals(5.0, TStatsKit.Percentile(Data, 100), '100th percentile failed');
end;

procedure TTestCaseStats.TestQuartiles;
var
  Data: TDoubleArray;
begin
  Data := TDoubleArray.Create(1, 2, 3, 4, 5, 6, 7);
  AssertEquals(2.5, TStatsKit.Quartile1(Data), 'First quartile failed');
  AssertEquals(5.5, TStatsKit.Quartile3(Data), 'Third quartile failed');
  AssertEquals(3.0, TStatsKit.InterquartileRange(Data), 'IQR failed');
end;

procedure TTestCaseStats.TestCorrelation;
var
  X, Y: TDoubleArray;
begin
  X := TDoubleArray.Create(1, 2, 3, 4, 5);
  Y := TDoubleArray.Create(2, 4, 6, 8, 10);
  AssertEquals(1.0, TStatsKit.Correlation(X, Y), 'Perfect correlation failed');
end;

procedure TTestCaseStats.TestCovariance;
var
  X, Y: TDoubleArray;
begin
  X := TDoubleArray.Create(1, 2, 3, 4, 5);
  Y := TDoubleArray.Create(2, 4, 6, 8, 10);
  AssertEquals(5.0, TStatsKit.Covariance(X, Y), 'Covariance calculation failed');
end;

procedure TTestCaseStats.TestZScore;
begin
  AssertEquals(1.0, TStatsKit.ZScore(12, 10, 2), 'Z-score calculation failed');
end;

{ TTestCaseFinance }

procedure TTestCaseFinance.TestPresentValue;
begin
  AssertEquals(90.9091, TFinanceKit.PresentValue(100, 0.1, 1), 'Present value calculation failed');
end;

procedure TTestCaseFinance.TestFutureValue;
begin
  AssertEquals(110.0, TFinanceKit.FutureValue(100, 0.1, 1), 'Future value calculation failed');
end;

procedure TTestCaseFinance.TestCompoundInterest;
begin
  AssertEquals(10.0, TFinanceKit.CompoundInterest(100, 0.1, 1), 'Compound interest calculation failed');
end;

procedure TTestCaseFinance.TestPayment;
begin
  AssertEquals(1075.68, TFinanceKit.Payment(10000, 0.05, 10), 'Payment calculation failed');
end;

procedure TTestCaseFinance.TestNetPresentValue;
var
  CashFlows: TDoubleArray;
begin
  CashFlows := TDoubleArray.Create(100, 200, 300);
  AssertEquals(498.69, TFinanceKit.NetPresentValue(100, CashFlows, 0.1), 'NPV calculation failed');
end;

procedure TTestCaseFinance.TestInternalRateOfReturn;
var
  CashFlows: TDoubleArray;
begin
  CashFlows := TDoubleArray.Create(110, 121, 133.1);
  AssertTrue('IRR calculation failed',
             Abs(TFinanceKit.InternalRateOfReturn(100, CashFlows) - 0.1) < 0.01);
end;

procedure TTestCaseFinance.TestDepreciation;
begin
  AssertEquals(180.0, TFinanceKit.StraightLineDepreciation(1000, 100, 5), 'Straight-line depreciation failed');
  AssertEquals(200.0, TFinanceKit.DecliningBalanceDepreciation(1000, 100, 5, 1), 'Declining balance depreciation failed');
end;

procedure TTestCaseFinance.TestROI;
begin
  AssertEquals(0.25, TFinanceKit.ReturnOnInvestment(125, 100), 'ROI calculation failed');
  AssertEquals(0.15, TFinanceKit.ReturnOnEquity(15, 100), 'ROE calculation failed');
end;

{ TTestCaseTrig }

procedure TTestCaseTrig.TestAngleConversions;
begin
  AssertEquals(Pi/2, TTrigKit.DegToRad(90), 'Degrees to radians failed');
  AssertEquals(90.0, TTrigKit.RadToDeg(Pi/2), 'Radians to degrees failed');
end;

procedure TTestCaseTrig.TestBasicTrig;
begin
  AssertEquals(0.0, TTrigKit.Sin(0), 'Sin(0) failed');
  AssertEquals(1.0, TTrigKit.Cos(0), 'Cos(0) failed');
  AssertEquals(0.0, TTrigKit.Tan(0), 'Tan(0) failed');
end;

procedure TTestCaseTrig.TestInverseTrig;
begin
  AssertEquals(0.0, TTrigKit.ArcSin(0), 'ArcSin(0) failed');
  AssertEquals(Pi/2, TTrigKit.ArcCos(0), 'ArcCos(0) failed');
  AssertEquals(0.0, TTrigKit.ArcTan(0), 'ArcTan(0) failed');
  AssertEquals(Pi/4, TTrigKit.ArcTan2(1, 1), 'ArcTan2(1,1) failed');
end;

procedure TTestCaseTrig.TestHyperbolic;
begin
  AssertEquals(0.0, TTrigKit.Sinh(0), 'Sinh(0) failed');
  AssertEquals(1.0, TTrigKit.Cosh(0), 'Cosh(0) failed');
  AssertEquals(0.0, TTrigKit.Tanh(0), 'Tanh(0) failed');
end;

procedure TTestCaseTrig.TestTriangleCalculations;
begin
  AssertEquals(5.0, TTrigKit.Hypotenuse(3, 4), 'Hypotenuse calculation failed');
  AssertEquals(6.0, TTrigKit.TriangleArea(3, 4), 'Triangle area failed');
  AssertEquals(6.0, TTrigKit.TriangleAreaSAS(3, Pi/2, 4), 'Triangle area SAS failed');
  AssertEquals(6.0, TTrigKit.TriangleAreaSSS(3, 4, 5), 'Triangle area SSS failed');
end;

procedure TTestCaseTrig.TestVectorOperations;
begin
  AssertEquals(5.0, TTrigKit.VectorMagnitude(3, 4), 'Vector magnitude failed');
  AssertEquals(Pi/4, TTrigKit.VectorAngle(0, 0, 1, 1), 'Vector angle failed');
end;

{ TTestCaseMatrix }

procedure TTestCaseMatrix.TestMatrixCreation;
var
  M: TMatrix;
begin
  M := TMatrixKit.Identity(3);
  AssertEquals(1.0, M[0,0], 'Identity matrix creation failed');
  AssertEquals(0.0, M[0,1], 'Identity matrix creation failed');
  
  M := TMatrixKit.Zeros(2, 2);
  AssertEquals(0.0, M[0,0], 'Zero matrix creation failed');
  
  M := TMatrixKit.Ones(2, 2);
  AssertEquals(1.0, M[0,0], 'Ones matrix creation failed');
end;

procedure TTestCaseMatrix.TestMatrixAddition;
var
  A, B, C: TMatrix;
begin
  A := TMatrixKit.CreateMatrix(2, 2);
  B := TMatrixKit.CreateMatrix(2, 2);
  A[0,0] := 1; A[0,1] := 2; A[1,0] := 3; A[1,1] := 4;
  B[0,0] := 1; B[0,1] := 2; B[1,0] := 3; B[1,1] := 4;
  
  C := TMatrixKit.Add(A, B);
  AssertEquals(2.0, C[0,0], 'Matrix addition failed');
  AssertEquals(8.0, C[1,1], 'Matrix addition failed');
end;

procedure TTestCaseMatrix.TestMatrixSubtraction;
var
  A, B, C: TMatrix;
begin
  A := TMatrixKit.CreateMatrix(2, 2);
  B := TMatrixKit.CreateMatrix(2, 2);
  A[0,0] := 2; A[0,1] := 3; A[1,0] := 4; A[1,1] := 5;
  B[0,0] := 1; B[0,1] := 2; B[1,0] := 3; B[1,1] := 4;
  
  C := TMatrixKit.Subtract(A, B);
  AssertEquals(1.0, C[0,0], 'Matrix subtraction failed');
  AssertEquals(1.0, C[1,1], 'Matrix subtraction failed');
end;

procedure TTestCaseMatrix.TestMatrixMultiplication;
var
  A, B, C: TMatrix;
begin
  A := TMatrixKit.CreateMatrix(2, 2);
  B := TMatrixKit.CreateMatrix(2, 2);
  A[0,0] := 1; A[0,1] := 2; A[1,0] := 3; A[1,1] := 4;
  B[0,0] := 2; B[0,1] := 0; B[1,0] := 1; B[1,1] := 2;
  
  C := TMatrixKit.Multiply(A, B);
  AssertEquals(4.0, C[0,0], 'Matrix multiplication failed');
  AssertEquals(4.0, C[0,1], 'Matrix multiplication failed');
  AssertEquals(10.0, C[1,0], 'Matrix multiplication failed');
  AssertEquals(8.0, C[1,1], 'Matrix multiplication failed');
end;

procedure TTestCaseMatrix.TestMatrixTranspose;
var
  A, T: TMatrix;
begin
  A := TMatrixKit.CreateMatrix(2, 2);
  A[0,0] := 1; A[0,1] := 2; A[1,0] := 3; A[1,1] := 4;
  
  T := TMatrixKit.Transpose(A);
  AssertEquals(1.0, T[0,0], 'Matrix transpose failed');
  AssertEquals(3.0, T[0,1], 'Matrix transpose failed');
  AssertEquals(2.0, T[1,0], 'Matrix transpose failed');
  AssertEquals(4.0, T[1,1], 'Matrix transpose failed');
end;

procedure TTestCaseMatrix.TestMatrixDeterminant;
var
  A: TMatrix;
begin
  A := TMatrixKit.CreateMatrix(2, 2);
  A[0,0] := 1; A[0,1] := 2; A[1,0] := 3; A[1,1] := 4;
  
  AssertEquals(-2.0, TMatrixKit.Determinant(A), 'Matrix determinant failed');
end;

initialization
  RegisterTest(TTestCaseStats);
  RegisterTest(TTestCaseFinance);
  RegisterTest(TTestCaseTrig);
  RegisterTest(TTestCaseMatrix);
end. 