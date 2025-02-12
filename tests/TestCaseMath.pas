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
      EPSILON = 1E-6;  // General tolerance for floating-point comparisons
      FINANCE_EPSILON = 1E-6;  // Tolerance for financial calculations (6 decimals)
    
    procedure AssertEquals(const Expected, Actual: Double; const Msg: String = ''); overload;
    procedure AssertFinanceEquals(const Expected, Actual: Double; const Msg: String = ''); overload;
  end;

  { Test cases for statistical operations }
  TTestCaseStats = class(TTestCaseMathBase)
  published
    procedure Test01_Mean;
    procedure Test02_Median;
    procedure Test03_Mode;
    procedure Test04_Range;
    procedure Test05_Variance;
    procedure Test06_StandardDeviation;
    procedure Test07_Skewness;
    procedure Test08_Kurtosis;
    procedure Test09_Percentile;
    procedure Test10_Quartiles;
    procedure Test11_Correlation;
    procedure Test12_Covariance;
    procedure Test13_ZScore;
  end;

  { Test cases for financial operations }
  TTestCaseFinance = class(TTestCaseMathBase)
  published
    procedure Test01_PresentValue;
    procedure Test02_FutureValue;
    procedure Test03_CompoundInterest;
    procedure Test04_Payment;
    procedure Test05_NetPresentValue;
    procedure Test06_InternalRateOfReturn;
    procedure Test07_Depreciation;
    procedure Test08_ROI;
  end;

  { Test cases for trigonometric operations }
  TTestCaseTrig = class(TTestCaseMathBase)
  published
    procedure Test01_AngleConversions;
    procedure Test02_BasicTrig;
    procedure Test03_InverseTrig;
    procedure Test04_Hyperbolic;
    procedure Test05_TriangleCalculations;
    procedure Test06_VectorOperations;
  end;

  { Test cases for matrix operations }
  TTestCaseMatrix = class(TTestCaseMathBase)
  published
    procedure Test01_MatrixCreation;
    procedure Test02_MatrixAddition;
    procedure Test03_MatrixSubtraction;
    procedure Test04_MatrixMultiplication;
    procedure Test05_MatrixTranspose;
    procedure Test06_MatrixDeterminant;
  end;

implementation

{ TTestCaseMathBase }

procedure TTestCaseMathBase.AssertEquals(const Expected, Actual: Double; const Msg: String);
begin
  AssertTrue(Msg, Abs(Expected - Actual) < EPSILON);
end;

procedure TTestCaseMathBase.AssertFinanceEquals(const Expected, Actual: Double; const Msg: String);
begin
  if Abs(Expected - Actual) >= FINANCE_EPSILON then
    WriteLn(Format('Expected: %.10f, Actual: %.10f, Diff: %.10f', [Expected, Actual, Abs(Expected - Actual)]));
  AssertTrue(Msg, Abs(Expected - Actual) < FINANCE_EPSILON);
end;

{ TTestCaseStats }

procedure TTestCaseStats.Test01_Mean;
var
  Data: TDoubleArray;
begin
  Data := TDoubleArray.Create(1, 2, 3, 4, 5);
  AssertEquals(3.0, TStatsKit.Mean(Data), 'Mean calculation failed');
  
  Data := TDoubleArray.Create(2.5, 3.5, 4.5);
  AssertEquals(3.5, TStatsKit.Mean(Data), 'Mean with decimals failed');
end;

procedure TTestCaseStats.Test02_Median;
var
  Data: TDoubleArray;
begin
  Data := TDoubleArray.Create(1, 3, 2, 4, 5);
  AssertEquals(3.0, TStatsKit.Median(Data), 'Median odd count failed');
  
  Data := TDoubleArray.Create(1, 2, 3, 4);
  AssertEquals(2.5, TStatsKit.Median(Data), 'Median even count failed');
end;

procedure TTestCaseStats.Test03_Mode;
var
  Data: TDoubleArray;
begin
  Data := TDoubleArray.Create(1, 2, 2, 3, 2, 4);
  AssertEquals(2.0, TStatsKit.Mode(Data), 'Mode calculation failed');
end;

procedure TTestCaseStats.Test04_Range;
var
  Data: TDoubleArray;
begin
  Data := TDoubleArray.Create(1, 2, 3, 4, 5);
  AssertEquals(4.0, TStatsKit.Range(Data), 'Range calculation failed');
end;

procedure TTestCaseStats.Test05_Variance;
var
  Data: TDoubleArray;
begin
  Data := TDoubleArray.Create(2, 4, 4, 4, 6);
  AssertEquals(2.0, TStatsKit.Variance(Data), 'Variance calculation failed');
end;

procedure TTestCaseStats.Test06_StandardDeviation;
var
  Data: TDoubleArray;
begin
  Data := TDoubleArray.Create(2, 4, 4, 4, 6);
  AssertEquals(Sqrt(2.0), TStatsKit.StandardDeviation(Data), 'Standard deviation failed');
end;

procedure TTestCaseStats.Test07_Skewness;
var
  Data: TDoubleArray;
begin
  Data := TDoubleArray.Create(2, 4, 4, 4, 6, 6, 6, 8);
  // Test for approximate symmetry
  AssertTrue('Skewness should be close to 0 for symmetric data',
             Abs(TStatsKit.Skewness(Data)) < 0.5);
end;

procedure TTestCaseStats.Test08_Kurtosis;
var
  Data: TDoubleArray;
begin
  Data := TDoubleArray.Create(2, 4, 4, 4, 6, 6, 6, 8);
  // Test for normal-like distribution
  AssertTrue('Kurtosis should be close to 0 for normal-like data',
             Abs(TStatsKit.Kurtosis(Data)) < 1.0);
end;

procedure TTestCaseStats.Test09_Percentile;
var
  Data: TDoubleArray;
begin
  Data := TDoubleArray.Create(1, 2, 3, 4, 5);
  AssertEquals(3.0, TStatsKit.Percentile(Data, 50), '50th percentile failed');
  AssertEquals(1.0, TStatsKit.Percentile(Data, 0), '0th percentile failed');
  AssertEquals(5.0, TStatsKit.Percentile(Data, 100), '100th percentile failed');
end;

procedure TTestCaseStats.Test10_Quartiles;
var
  Data: TDoubleArray;
begin
  Data := TDoubleArray.Create(1, 2, 3, 4, 5, 6, 7);
  AssertEquals(2.5, TStatsKit.Quartile1(Data), 'First quartile failed');
  AssertEquals(5.5, TStatsKit.Quartile3(Data), 'Third quartile failed');
  AssertEquals(3.0, TStatsKit.InterquartileRange(Data), 'IQR failed');
end;

procedure TTestCaseStats.Test11_Correlation;
var
  X, Y: TDoubleArray;
begin
  X := TDoubleArray.Create(1, 2, 3, 4, 5);
  Y := TDoubleArray.Create(2, 4, 6, 8, 10);
  AssertEquals(1.0, TStatsKit.Correlation(X, Y), 'Perfect correlation failed');
end;

procedure TTestCaseStats.Test12_Covariance;
var
  X, Y: TDoubleArray;
begin
  X := TDoubleArray.Create(1, 2, 3, 4, 5);
  Y := TDoubleArray.Create(2, 4, 6, 8, 10);
  AssertEquals(5.0, TStatsKit.Covariance(X, Y), 'Covariance calculation failed');
end;

procedure TTestCaseStats.Test13_ZScore;
begin
  AssertEquals(1.0, TStatsKit.ZScore(12, 10, 2), 'Z-score calculation failed');
end;

{ TTestCaseFinance }

procedure TTestCaseFinance.Test01_PresentValue;
var
  Result: Double;
begin
  WriteLn('Test #1: Present Value Test');
  WriteLn('Input: FV=100, Rate=0.1, Periods=1');
  Result := TFinanceKit.PresentValue(100, 0.1, 1);
  WriteLn(Format('Expected: %.10f, Got: %.10f', [90.9091, Result]));
  AssertFinanceEquals(90.9091, Result, 'Present value calculation failed');
end;

procedure TTestCaseFinance.Test02_FutureValue;
begin
  AssertFinanceEquals(110.0, TFinanceKit.FutureValue(100, 0.1, 1), 'Future value calculation failed');
end;

procedure TTestCaseFinance.Test03_CompoundInterest;
begin
  AssertFinanceEquals(10.0, TFinanceKit.CompoundInterest(100, 0.1, 1), 'Compound interest calculation failed');
end;

procedure TTestCaseFinance.Test04_Payment;
var
  Result: Double;
begin
  WriteLn('Test #2: Payment Test');
  WriteLn('Input: PV=10000, Rate=0.05, Periods=10');
  Result := TFinanceKit.Payment(10000, 0.05, 10);
  WriteLn(Format('Expected: %.10f, Got: %.10f', [1075.6841, Result]));
  AssertFinanceEquals(1075.6841, Result, 'Payment calculation failed');
end;

procedure TTestCaseFinance.Test05_NetPresentValue;
var
  CashFlows: TDoubleArray;
  Result: Double;
begin
  WriteLn('Test #3: NPV Test');
  WriteLn('Input: Initial=100, CashFlows=[100,200,300], Rate=0.1');
  CashFlows := TDoubleArray.Create(100, 200, 300);
  Result := TFinanceKit.NetPresentValue(100, CashFlows, 0.1);
  WriteLn(Format('Expected: %.10f, Got: %.10f', [498.6858, Result]));
  AssertFinanceEquals(498.6858, Result, 'NPV calculation failed');
end;

procedure TTestCaseFinance.Test06_InternalRateOfReturn;
var
  CashFlows: TDoubleArray;
  Result: Double;
begin
  WriteLn('Test #4: IRR Test');
  WriteLn('Input: Initial=100, CashFlows=[110,121,133.1]');
  CashFlows := TDoubleArray.Create(110, 121, 133.1);
  Result := TFinanceKit.InternalRateOfReturn(100, CashFlows);
  WriteLn(Format('Expected: %.10f, Got: %.10f', [0.1, Result]));
  if Abs(Result - 0.1) >= FINANCE_EPSILON then
    WriteLn(Format('Expected: %.10f, Actual: %.10f, Diff: %.10f', [0.1, Result, Abs(0.1 - Result)]));
  AssertTrue('IRR calculation failed', Abs(Result - 0.1) < FINANCE_EPSILON);
end;

procedure TTestCaseFinance.Test07_Depreciation;
var
  SLResult, DBResult: Double;
begin
  WriteLn('Test #5: Depreciation Test');
  WriteLn('Input: Cost=1000, Salvage=100, Life=5, Period=1');
  SLResult := TFinanceKit.StraightLineDepreciation(1000, 100, 5);
  DBResult := TFinanceKit.DecliningBalanceDepreciation(1000, 100, 5, 1);
  WriteLn(Format('Expected SL: %.10f, Got: %.10f', [180.0, SLResult]));
  WriteLn(Format('Expected DB: %.10f, Got: %.10f', [400.0, DBResult]));
  AssertFinanceEquals(180.0, SLResult, 'Straight-line depreciation failed');
  AssertFinanceEquals(400.0, DBResult, 'Declining balance depreciation failed');
end;

procedure TTestCaseFinance.Test08_ROI;
begin
  AssertEquals(0.25, TFinanceKit.ReturnOnInvestment(125, 100), 'ROI calculation failed');
  AssertEquals(0.15, TFinanceKit.ReturnOnEquity(15, 100), 'ROE calculation failed');
end;

{ TTestCaseTrig }

procedure TTestCaseTrig.Test01_AngleConversions;
begin
  AssertEquals(Pi/2, TTrigKit.DegToRad(90), 'Degrees to radians failed');
  AssertEquals(90.0, TTrigKit.RadToDeg(Pi/2), 'Radians to degrees failed');
end;

procedure TTestCaseTrig.Test02_BasicTrig;
begin
  AssertEquals(0.0, TTrigKit.Sin(0), 'Sin(0) failed');
  AssertEquals(1.0, TTrigKit.Cos(0), 'Cos(0) failed');
  AssertEquals(0.0, TTrigKit.Tan(0), 'Tan(0) failed');
end;

procedure TTestCaseTrig.Test03_InverseTrig;
begin
  AssertEquals(0.0, TTrigKit.ArcSin(0), 'ArcSin(0) failed');
  AssertEquals(Pi/2, TTrigKit.ArcCos(0), 'ArcCos(0) failed');
  AssertEquals(0.0, TTrigKit.ArcTan(0), 'ArcTan(0) failed');
  AssertEquals(Pi/4, TTrigKit.ArcTan2(1, 1), 'ArcTan2(1,1) failed');
end;

procedure TTestCaseTrig.Test04_Hyperbolic;
begin
  AssertEquals(0.0, TTrigKit.Sinh(0), 'Sinh(0) failed');
  AssertEquals(1.0, TTrigKit.Cosh(0), 'Cosh(0) failed');
  AssertEquals(0.0, TTrigKit.Tanh(0), 'Tanh(0) failed');
end;

procedure TTestCaseTrig.Test05_TriangleCalculations;
begin
  AssertEquals(5.0, TTrigKit.Hypotenuse(3, 4), 'Hypotenuse calculation failed');
  AssertEquals(6.0, TTrigKit.TriangleArea(3, 4), 'Triangle area failed');
  AssertEquals(6.0, TTrigKit.TriangleAreaSAS(3, Pi/2, 4), 'Triangle area SAS failed');
  AssertEquals(6.0, TTrigKit.TriangleAreaSSS(3, 4, 5), 'Triangle area SSS failed');
end;

procedure TTestCaseTrig.Test06_VectorOperations;
begin
  AssertEquals(5.0, TTrigKit.VectorMagnitude(3, 4), 'Vector magnitude failed');
  AssertEquals(Pi/4, TTrigKit.VectorAngle(0, 0, 1, 1), 'Vector angle failed');
end;

{ TTestCaseMatrix }

procedure TTestCaseMatrix.Test01_MatrixCreation;
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

procedure TTestCaseMatrix.Test02_MatrixAddition;
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

procedure TTestCaseMatrix.Test03_MatrixSubtraction;
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

procedure TTestCaseMatrix.Test04_MatrixMultiplication;
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

procedure TTestCaseMatrix.Test05_MatrixTranspose;
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

procedure TTestCaseMatrix.Test06_MatrixDeterminant;
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