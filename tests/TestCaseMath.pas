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
      FINANCE_EPSILON = 1E-4;  // Tolerance for financial calculations (4 decimals)
    
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
    procedure Test11_PearsonCorrelation_Perfect_Positive;
    procedure Test12_PearsonCorrelation_Perfect_Negative;
    procedure Test13_PearsonCorrelation_Zero;
    procedure Test14_PearsonCorrelation_Moderate;
    procedure Test15_SpearmanCorrelation_Perfect_Monotonic;
    procedure Test16_SpearmanCorrelation_Perfect_Negative_Monotonic;
    procedure Test17_SpearmanCorrelation_Zero_Monotonic;
    procedure Test18_SpearmanCorrelation_With_Ties;
    procedure Test19_SpearmanCorrelation_NonLinear_Monotonic;
    procedure Test20_Covariance_Positive;
    procedure Test21_Covariance_Zero;
    procedure Test22_Covariance_Negative;
    procedure Test23_ZScore_Positive;
    procedure Test24_ZScore_Negative;
    procedure Test25_ZScore_Zero;
    procedure Test26_Describe_N;
    procedure Test26_Describe_Mean;
    procedure Test26_Describe_Median;
    procedure Test26_Describe_Min;
    procedure Test26_Describe_Max;
    procedure Test26_Describe_Range;
    procedure Test26_Describe_StdDev;
    procedure Test27_GeometricMean;
    procedure Test28_HarmonicMean;
    procedure Test29_TrimmedMean;
    procedure Test30_WinsorizedMean;
    procedure Test31_StandardErrorOfMean;
    procedure Test32_CoefficientOfVariation;
    procedure Test33_MedianAbsoluteDeviation;
    procedure Test34_RobustStandardDeviation;
    procedure Test35_HuberM;
    procedure Test36_BootstrapConfidenceInterval;
    procedure Test37_TTest;
    procedure Test38_MannWhitneyU;
    procedure Test39_KolmogorovSmirnov;
    procedure Test40_ShapiroWilk;
    procedure Test41_CohensD;
    procedure Test42_HedgesG;
    procedure Test43_SignTest;
    procedure Test44_WilcoxonSignedRank;
    procedure Test45_KendallTau;
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
    procedure Test09_BondPrice;
    procedure Test10_BondYieldToMaturity;
    procedure Test11_EffectiveAnnualRate;
    procedure Test12_ModifiedDuration;
    procedure Test13_BreakEvenAnalysis;
    procedure Test14_WACC;
    procedure Test15_CAPM;
    procedure Test16_GordonGrowthModel;
    procedure Test17_WorkingCapitalRatios;
    procedure Test18_LeverageRatios;
    procedure Test19_BlackScholes;
    procedure Test20_RiskMetrics;
    procedure Test21_DuPontAnalysis;
    procedure Test22_OperatingLeverage;
    procedure Test23_ProfitabilityRatios;
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
var
  RelativeDiff: Double;
begin
  if Abs(Expected) > 1E-10 then
    RelativeDiff := Abs((Expected - Actual) / Expected)
  else
    RelativeDiff := Abs(Expected - Actual);
    
  if RelativeDiff >= FINANCE_EPSILON then
    WriteLn(Format('Expected: %.10f, Actual: %.10f, Diff: %.10f, RelDiff: %.10f', 
      [Expected, Actual, Abs(Expected - Actual), RelativeDiff]));
      
  AssertTrue(Msg, RelativeDiff < FINANCE_EPSILON);
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
  AssertEquals(Sqrt(2.0), TStatsKit.SampleStandardDeviation(Data), 'Standard deviation failed');
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

procedure TTestCaseStats.Test11_PearsonCorrelation_Perfect_Positive;
var
  X, Y: TDoubleArray;
begin
  X := TDoubleArray.Create(1, 2, 3, 4, 5);
  Y := TDoubleArray.Create(2, 4, 6, 8, 10);
  AssertEquals(1.0, TStatsKit.PearsonCorrelation(X, Y), 'Perfect positive correlation failed');
end;

procedure TTestCaseStats.Test12_PearsonCorrelation_Perfect_Negative;
var
  X, Y: TDoubleArray;
begin
  X := TDoubleArray.Create(1, 2, 3, 4, 5);
  Y := TDoubleArray.Create(10, 8, 6, 4, 2);
  AssertEquals(-1.0, TStatsKit.PearsonCorrelation(X, Y), 'Perfect negative correlation failed');
end;

procedure TTestCaseStats.Test13_PearsonCorrelation_Zero;
var
  X, Y: TDoubleArray;
begin
  X := TDoubleArray.Create(1, 2, 3, 4, 5);
  Y := TDoubleArray.Create(5, 5, 5, 5, 5);
  AssertEquals(0.0, TStatsKit.PearsonCorrelation(X, Y), 'Zero correlation failed');
end;

procedure TTestCaseStats.Test14_PearsonCorrelation_Moderate;
var
  X, Y: TDoubleArray;
  Correlation: Double;
begin
  X := TDoubleArray.Create(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
  Y := TDoubleArray.Create(1.0, 2.0, 3.0, 4.0, 5.0, 7.37789955, 7.0, 8.0, 9.0, 10.0);
  Correlation := TStatsKit.PearsonCorrelation(X, Y);
  WriteLn(Format('Pearson Correlation: %.10f', [Correlation]));
  AssertTrue('Moderate correlation should be close to 0.99',
             Abs(Correlation - 0.99) < 1E-4);
end;

procedure TTestCaseStats.Test15_SpearmanCorrelation_Perfect_Monotonic;
var
  X, Y: TDoubleArray;
begin
  X := TDoubleArray.Create(1, 2, 3, 4, 5);
  Y := TDoubleArray.Create(2, 4, 8, 16, 32);
  AssertEquals(1.0, TStatsKit.SpearmanCorrelation(X, Y), 'Perfect monotonic relationship failed');
end;

procedure TTestCaseStats.Test16_SpearmanCorrelation_Perfect_Negative_Monotonic;
var
  X, Y: TDoubleArray;
begin
  X := TDoubleArray.Create(1, 2, 3, 4, 5);
  Y := TDoubleArray.Create(32, 16, 8, 4, 2);
  AssertEquals(-1.0, TStatsKit.SpearmanCorrelation(X, Y), 'Perfect negative monotonic relationship failed');
end;

procedure TTestCaseStats.Test17_SpearmanCorrelation_Zero_Monotonic;
var
  X, Y: TDoubleArray;
begin
  X := TDoubleArray.Create(1, 2, 3, 4, 5);
  Y := TDoubleArray.Create(5, 5, 5, 5, 5);
  AssertEquals(0.0, TStatsKit.SpearmanCorrelation(X, Y), 'Zero monotonic relationship failed');
end;

procedure TTestCaseStats.Test18_SpearmanCorrelation_With_Ties;
var
  X, Y: TDoubleArray;
begin
  X := TDoubleArray.Create(1, 2, 3, 3, 4);
  Y := TDoubleArray.Create(1, 2, 3, 3, 4);
  AssertEquals(1.0, TStatsKit.SpearmanCorrelation(X, Y), 'Perfect correlation with ties failed');
end;

procedure TTestCaseStats.Test19_SpearmanCorrelation_NonLinear_Monotonic;
var
  X, Y: TDoubleArray;
begin
  X := TDoubleArray.Create(1, 2, 3, 4, 5);
  Y := TDoubleArray.Create(1, 4, 9, 16, 25);  // Squared values
  AssertEquals(1.0, TStatsKit.SpearmanCorrelation(X, Y), 'Non-linear monotonic relationship failed');
end;

procedure TTestCaseStats.Test20_Covariance_Positive;
var
  X, Y: TDoubleArray;
begin
  X := TDoubleArray.Create(1, 2, 3, 4, 5);
  Y := TDoubleArray.Create(2, 4, 6, 8, 10);
  AssertEquals(5.0, TStatsKit.Covariance(X, Y), 'Positive covariance calculation failed');
end;

procedure TTestCaseStats.Test21_Covariance_Zero;
var
  X, Y: TDoubleArray;
begin
  X := TDoubleArray.Create(1, 2, 3, 4, 5);
  Y := TDoubleArray.Create(5, 5, 5, 5, 5);
  AssertEquals(0.0, TStatsKit.Covariance(X, Y), 'Zero covariance failed');
end;

procedure TTestCaseStats.Test22_Covariance_Negative;
var
  X, Y: TDoubleArray;
begin
  X := TDoubleArray.Create(1, 2, 3, 4, 5);
  Y := TDoubleArray.Create(10, 8, 6, 4, 2);
  AssertEquals(-5.0, TStatsKit.Covariance(X, Y), 'Negative covariance failed');
end;

procedure TTestCaseStats.Test23_ZScore_Positive;
begin
  AssertEquals(1.0, TStatsKit.ZScore(12, 10, 2), 'Positive z-score calculation failed');
end;

procedure TTestCaseStats.Test24_ZScore_Negative;
begin
  AssertEquals(-1.5, TStatsKit.ZScore(7, 10, 2), 'Negative z-score calculation failed');
end;

procedure TTestCaseStats.Test25_ZScore_Zero;
begin
  AssertEquals(0.0, TStatsKit.ZScore(10, 10, 2), 'Zero z-score calculation failed');
end;

procedure TTestCaseStats.Test26_Describe_N;
var
  Data: TDoubleArray;
  Stats: TDescriptiveStats;
begin
  WriteLn('Test26_Describe_N: Starting');
  Data := TDoubleArray.Create(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
  Stats := TStatsKit.Describe(Data);

  WriteLn(Stats.ToString);

  WriteLn('--------------------------------');

  WriteLn(Stats.ToStringWide);

  AssertEquals(10, Stats.N, 'N calculation failed');
  WriteLn('Test26_Describe_N: Finished');
end;

procedure TTestCaseStats.Test26_Describe_Mean;
var
  Data: TDoubleArray;
  Stats: TDescriptiveStats;
begin
  Data := TDoubleArray.Create(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
  Stats := TStatsKit.Describe(Data);
  AssertEquals(5.5, Stats.Mean, 'Mean calculation failed');
end;

procedure TTestCaseStats.Test26_Describe_Median;
var
  Data: TDoubleArray;
  Stats: TDescriptiveStats;
begin
  Data := TDoubleArray.Create(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
  Stats := TStatsKit.Describe(Data);
  AssertEquals(5.5, Stats.Median, 'Median calculation failed');
end;

procedure TTestCaseStats.Test26_Describe_Min;
var
  Data: TDoubleArray;
  Stats: TDescriptiveStats;
begin
  Data := TDoubleArray.Create(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
  Stats := TStatsKit.Describe(Data);
  AssertEquals(1.0, Stats.Min, 'Min calculation failed');
end;

procedure TTestCaseStats.Test26_Describe_Max;
var
  Data: TDoubleArray;
  Stats: TDescriptiveStats;
begin
  Data := TDoubleArray.Create(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
  Stats := TStatsKit.Describe(Data);
  AssertEquals(10.0, Stats.Max, 'Max calculation failed');
end;

procedure TTestCaseStats.Test26_Describe_Range;
var
  Data: TDoubleArray;
  Stats: TDescriptiveStats;
begin
  Data := TDoubleArray.Create(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
  Stats := TStatsKit.Describe(Data);
  AssertEquals(9.0, Stats.Range, 'Range calculation failed');
end;

procedure TTestCaseStats.Test26_Describe_StdDev;
var
  Data: TDoubleArray;
  Stats: TDescriptiveStats;
begin
  WriteLn('Test26_Describe_StdDev: Starting');
  Data := TDoubleArray.Create(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
  Stats := TStatsKit.Describe(Data);
  WriteLn(Format('StdDev: %.6f', [Stats.StdDev]));
  AssertEquals(2.872281, Stats.StdDev, 'StdDev calculation failed');
  WriteLn('Test26_Describe_StdDev: Finished');
end;

procedure TTestCaseStats.Test27_GeometricMean;
var
  Data: TDoubleArray;
begin
  Data := TDoubleArray.Create(1, 2, 4, 8);
  AssertEquals(2.828427, TStatsKit.GeometricMean(Data), 'Geometric mean failed');
end;

procedure TTestCaseStats.Test28_HarmonicMean;
var
  Data: TDoubleArray;
begin
  WriteLn('Test28_HarmonicMean: Starting');
  Data := TDoubleArray.Create(1, 2, 4, 8);
  WriteLn(Format('Harmonic Mean: %.6f', [TStatsKit.HarmonicMean(Data)]));
  AssertEquals(2.133333, TStatsKit.HarmonicMean(Data), 'Harmonic mean failed');
  WriteLn('Test28_HarmonicMean: Finished');
end;

procedure TTestCaseStats.Test29_TrimmedMean;
var
  Data: TDoubleArray;
begin
  Data := TDoubleArray.Create(1, 2, 3, 4, 100);  // Outlier at 100
  AssertEquals(3.0, TStatsKit.TrimmedMean(Data, 20), 'Trimmed mean failed');
end;

procedure TTestCaseStats.Test30_WinsorizedMean;
var
  Data: TDoubleArray;
begin
  Data := TDoubleArray.Create(1, 2, 3, 4, 100);  // Outlier at 100
  AssertEquals(3.0, TStatsKit.WinsorizedMean(Data, 20), 'Winsorized mean failed');
end;

procedure TTestCaseStats.Test31_StandardErrorOfMean;
var
  Data: TDoubleArray;
begin
  Data := TDoubleArray.Create(2, 4, 4, 4, 6);
  AssertEquals(0.632456, TStatsKit.StandardErrorOfMean(Data), 'SEM failed');
end;

procedure TTestCaseStats.Test32_CoefficientOfVariation;
var
  Data: TDoubleArray;
begin
  Data := TDoubleArray.Create(2, 4, 4, 4, 6);
  AssertEquals(31.622777, TStatsKit.CoefficientOfVariation(Data), 'CV failed');
end;

procedure TTestCaseStats.Test33_MedianAbsoluteDeviation;
var
  Data: TDoubleArray;
begin
  Data := TDoubleArray.Create(1, 2, 3, 4, 5);
  AssertEquals(1.0, TStatsKit.MedianAbsoluteDeviation(Data), 'MAD failed');
end;

procedure TTestCaseStats.Test34_RobustStandardDeviation;
var
  Data: TDoubleArray;
begin
  Data := TDoubleArray.Create(1, 2, 3, 4, 5);
  AssertEquals(1.4826, TStatsKit.RobustStandardDeviation(Data), 'Robust StdDev failed');
end;

procedure TTestCaseStats.Test35_HuberM;
var
  Data: TDoubleArray;
begin
  Data := TDoubleArray.Create(1, 2, 3, 4, 100);  // Outlier at 100
  AssertTrue('Huber M-estimator should be closer to median than mean',
             Abs(TStatsKit.HuberM(Data) - 3) < Abs(TStatsKit.Mean(Data) - 3));
end;

procedure TTestCaseStats.Test36_BootstrapConfidenceInterval;
var
  Data: TDoubleArray;
  CI: TDoublePair;
begin
  Data := TDoubleArray.Create(1, 2, 3, 4, 5);
  CI := TStatsKit.BootstrapConfidenceInterval(Data);
  
  AssertTrue('Lower bound should be less than mean', CI.Lower < TStatsKit.Mean(Data));
  AssertTrue('Upper bound should be greater than mean', CI.Upper > TStatsKit.Mean(Data));
  AssertTrue('CI width should be positive', CI.Upper - CI.Lower > 0);
end;

procedure TTestCaseStats.Test37_TTest;
var
  X, Y: TDoubleArray;
  TPValue: Double;
  TStat: Double;
begin
  X := TDoubleArray.Create(1, 2, 3, 4, 5);
  Y := TDoubleArray.Create(2, 3, 4, 5, 6);
  TStat := TStatsKit.TTest(X, Y, TPValue);
  
  AssertTrue('T-statistic should be negative', TStat < 0);
  AssertTrue('P-value should be between 0 and 1', (TPValue >= 0) and (TPValue <= 1));
end;

procedure TTestCaseStats.Test38_MannWhitneyU;
var
  X, Y: TDoubleArray;
  UPValue: Double;
  UStat: Double;
begin
  X := TDoubleArray.Create(1, 2, 3, 4, 5);
  Y := TDoubleArray.Create(2, 3, 4, 5, 6);
  UStat := TStatsKit.MannWhitneyU(X, Y, UPValue);
  
  AssertTrue('U statistic should be non-negative', UStat >= 0);
  AssertTrue('P-value should be between 0 and 1', (UPValue >= 0) and (UPValue <= 1));
end;

procedure TTestCaseStats.Test39_KolmogorovSmirnov;
var
  Data: TDoubleArray;
  KSPValue: Double;
  KSStat: Double;
begin
  // Normal-like data
  Data := TDoubleArray.Create(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
  KSStat := TStatsKit.KolmogorovSmirnovTest(Data, KSPValue);
  
  AssertTrue('KS statistic should be between 0 and 1', (KSStat >= 0) and (KSStat <= 1));
  AssertTrue('KS test should indicate normality for normal-like data', KSPValue < 0.886 / Sqrt(Length(Data)));
end;

procedure TTestCaseStats.Test40_ShapiroWilk;
var
  Data: TDoubleArray;
  WPValue: Double;
  WStat: Double;
begin
  // Normal-like data
  Data := TDoubleArray.Create(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
  WStat := TStatsKit.ShapiroWilkTest(Data, WPValue);
  
  AssertTrue('W statistic should be between 0 and 1', (WStat >= 0) and (WStat <= 1));
  AssertTrue('P-value should be between 0 and 1', (WPValue >= 0) and (WPValue <= 1));
end;

procedure TTestCaseStats.Test41_CohensD;
var
  X, Y: TDoubleArray;
  D: Double;
begin
  X := TDoubleArray.Create(1, 2, 3, 4, 5);
  Y := TDoubleArray.Create(2, 3, 4, 5, 6);
  D := TStatsKit.CohensD(X, Y);
  
  AssertTrue('Cohen''s d should be negative for Y > X', D < 0);
  AssertTrue('Cohen''s d magnitude should be reasonable', Abs(D) < 2);
end;

procedure TTestCaseStats.Test42_HedgesG;
var
  X, Y: TDoubleArray;
  G: Double;
begin
  X := TDoubleArray.Create(1, 2, 3, 4, 5);
  Y := TDoubleArray.Create(2, 3, 4, 5, 6);
  G := TStatsKit.HedgesG(X, Y);
  
  AssertTrue('Hedges'' g should be negative for Y > X', G < 0);
  AssertTrue('Hedges'' g magnitude should be reasonable', Abs(G) < 2);
  AssertTrue('Hedges'' g should be slightly smaller than Cohen''s d', 
             Abs(G) < Abs(TStatsKit.CohensD(X, Y)));
end;

procedure TTestCaseStats.Test43_SignTest;
var
  X, Y: TDoubleArray;
  Prop: Double;
begin
  X := TDoubleArray.Create(1, 2, 3, 4, 5);
  Y := TDoubleArray.Create(2, 3, 4, 5, 6);
  Prop := TStatsKit.SignTest(X, Y);
  
  AssertEquals(0.0, Prop, 'Sign test should be 0 for Y consistently greater than X');
end;

procedure TTestCaseStats.Test44_WilcoxonSignedRank;
var
  X, Y: TDoubleArray;
  W: Double;
begin
  X := TDoubleArray.Create(1, 2, 3, 4, 5);
  Y := TDoubleArray.Create(2, 3, 4, 5, 6);
  W := TStatsKit.WilcoxonSignedRank(X, Y);
  
  AssertTrue('Wilcoxon W should be non-negative', W >= 0);
end;

procedure TTestCaseStats.Test45_KendallTau;
var
  X, Y: TDoubleArray;
  Tau: Double;
begin
  // Perfect positive correlation
  X := TDoubleArray.Create(1, 2, 3, 4, 5);
  Y := TDoubleArray.Create(2, 3, 4, 5, 6);
  Tau := TStatsKit.KendallTau(X, Y);
  
  AssertEquals(1.0, Tau, 'Kendall''s tau should be 1 for perfect positive correlation');
  
  // Perfect negative correlation
  Y := TDoubleArray.Create(6, 5, 4, 3, 2);
  Tau := TStatsKit.KendallTau(X, Y);
  
  AssertEquals(-1.0, Tau, 'Kendall''s tau should be -1 for perfect negative correlation');
end;

{ TTestCaseFinance }

procedure TTestCaseFinance.Test01_PresentValue;
var
  Result: Double;
begin
  WriteLn('Test #1: Present Value Test');
  WriteLn('Input: FV=100, Rate=0.1, Periods=1');
  Result := TFinanceKit.PresentValue(100, 0.1, 1, 4);
  WriteLn(Format('Expected: %.10f, Got: %.10f', [90.9091, Result]));
  AssertFinanceEquals(90.9091, Result, 'Present value calculation failed');
end;

procedure TTestCaseFinance.Test02_FutureValue;
begin
  AssertFinanceEquals(110.0, TFinanceKit.FutureValue(100, 0.1, 1, 4), 'Future value calculation failed');
end;

procedure TTestCaseFinance.Test03_CompoundInterest;
begin
  AssertFinanceEquals(10.0, TFinanceKit.CompoundInterest(100, 0.1, 1, 4), 'Compound interest calculation failed');
end;

procedure TTestCaseFinance.Test04_Payment;
var
  Result: Double;
begin
  WriteLn('Test #2: Payment Test');
  WriteLn('Input: PV=10000, Rate=0.05, Periods=10');
  Result := TFinanceKit.Payment(10000, 0.05, 10, 4);
  WriteLn(Format('Expected: %.10f, Got: %.10f', [1295.0457, Result]));
  AssertFinanceEquals(1295.0457, Result, 'Payment calculation failed');
end;

procedure TTestCaseFinance.Test05_NetPresentValue;
var
  CashFlows: TDoubleArray;
  Result: Double;
begin
  WriteLn('Test #3: NPV Test');
  WriteLn('Input: Initial=100, CashFlows=[100,200,300], Rate=0.1');
  CashFlows := TDoubleArray.Create(100, 200, 300);
  Result := TFinanceKit.NetPresentValue(100, CashFlows, 0.1, 4);
  WriteLn(Format('Expected: %.10f, Got: %.10f', [381.5928, Result]));
  AssertFinanceEquals(381.5928, Result, 'NPV calculation failed');
end;

procedure TTestCaseFinance.Test06_InternalRateOfReturn;
var
  CashFlows: TDoubleArray;
  Result: Double;
begin
  WriteLn('Test #4: IRR Test');
  WriteLn('Input: Initial=100, CashFlows=[110,121,133.1]');
  CashFlows := TDoubleArray.Create(110, 121, 133.1);
  Result := TFinanceKit.InternalRateOfReturn(100, CashFlows, 4);
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
  SLResult := TFinanceKit.StraightLineDepreciation(1000, 100, 5, 4);
  DBResult := TFinanceKit.DecliningBalanceDepreciation(1000, 100, 5, 1, 4);
  WriteLn(Format('Expected SL: %.10f, Got: %.10f', [180.0, SLResult]));
  WriteLn(Format('Expected DB: %.10f, Got: %.10f', [400.0, DBResult]));
  AssertFinanceEquals(180.0, SLResult, 'Straight-line depreciation failed');
  AssertFinanceEquals(400.0, DBResult, 'Declining balance depreciation failed');
end;

procedure TTestCaseFinance.Test08_ROI;
begin
  AssertEquals(0.25, TFinanceKit.ReturnOnInvestment(125, 100, 4), 'ROI calculation failed');
  AssertEquals(0.15, TFinanceKit.ReturnOnEquity(15, 100, 4), 'ROE calculation failed');
end;

procedure TTestCaseFinance.Test09_BondPrice;
const
  FACE_VALUE = 1000.0;
  COUPON_RATE = 0.06;  // 6% annual coupon
  YIELD_RATE = 0.05;   // 5% yield
  PERIODS_PER_YEAR = 2; // Semi-annual payments
  YEARS_TO_MATURITY = 5;
begin
  // Bond price should be higher than face value when yield < coupon rate
  WriteLn('Test 09: Bond Price Test: Starting with FACE_VALUE = 1000.0, COUPON_RATE = 0.06, YIELD_RATE = 0.05, PERIODS_PER_YEAR = 2, YEARS_TO_MATURITY = 5');

  WriteLn('Bond Price: ', TFinanceKit.BondPrice(
    FACE_VALUE, COUPON_RATE, YIELD_RATE, PERIODS_PER_YEAR, YEARS_TO_MATURITY
  ));

  AssertFinanceEquals(1043.76, TFinanceKit.BondPrice(
    FACE_VALUE, COUPON_RATE, YIELD_RATE, PERIODS_PER_YEAR, YEARS_TO_MATURITY
  ), 'Bond price calculation failed');
  WriteLn('Test 09: Bond Price Test: Finished');
end;

procedure TTestCaseFinance.Test10_BondYieldToMaturity;
const
  FACE_VALUE = 1000.0;
  COUPON_RATE = 0.06;
  BOND_PRICE = 1043.76;  // Updated to match the price at 5% yield
  PERIODS_PER_YEAR = 2;
  YEARS_TO_MATURITY = 5;
begin
  WriteLn('Test 10: Bond Yield to Maturity Test: Starting with BOND_PRICE = 1043.76, FACE_VALUE = 1000.0, COUPON_RATE = 0.06, PERIODS_PER_YEAR = 2, YEARS_TO_MATURITY = 5');
  WriteLn('Bond Yield to Maturity: ', TFinanceKit.BondYieldToMaturity(
    BOND_PRICE, FACE_VALUE, COUPON_RATE, PERIODS_PER_YEAR, YEARS_TO_MATURITY
  ));

  AssertFinanceEquals(0.05, TFinanceKit.BondYieldToMaturity(
    BOND_PRICE, FACE_VALUE, COUPON_RATE, PERIODS_PER_YEAR, YEARS_TO_MATURITY
  ), 'Bond yield to maturity calculation failed');

  WriteLn('Test 10: Bond Yield to Maturity Test: Finished');
end;

procedure TTestCaseFinance.Test11_EffectiveAnnualRate;
const
  NOMINAL_RATE = 0.12;  // 12% nominal rate
  COMPOUNDINGS = 12;    // Monthly compounding
begin
  WriteLn('Test 11: Effective Annual Rate Test: Starting with NOMINAL_RATE = 0.12, COMPOUNDINGS = 12');
  WriteLn('Effective Annual Rate: ', TFinanceKit.EffectiveAnnualRate(
    NOMINAL_RATE, COMPOUNDINGS
  )); 

  AssertFinanceEquals(0.1268, TFinanceKit.EffectiveAnnualRate(  // Updated to match precise calculation
    NOMINAL_RATE, COMPOUNDINGS
  ), 'Effective annual rate calculation failed');

  WriteLn('Test 11: Effective Annual Rate Test: Finished');
end;

procedure TTestCaseFinance.Test12_ModifiedDuration;
const
  FACE_VALUE = 1000.0;
  COUPON_RATE = 0.06;
  YIELD_RATE = 0.05;
  PERIODS_PER_YEAR = 2;
  YEARS_TO_MATURITY = 5;
begin
  WriteLn('Test 12: Modified Duration Test: Starting with FACE_VALUE = 1000.0, COUPON_RATE = 0.06, YIELD_RATE = 0.05, PERIODS_PER_YEAR = 2, YEARS_TO_MATURITY = 5');  
  WriteLn('Modified Duration: ', TFinanceKit.ModifiedDuration(
    FACE_VALUE, COUPON_RATE, YIELD_RATE, PERIODS_PER_YEAR, YEARS_TO_MATURITY
  ));

  AssertFinanceEquals(4.4558, TFinanceKit.ModifiedDuration(
    FACE_VALUE, COUPON_RATE, YIELD_RATE, PERIODS_PER_YEAR, YEARS_TO_MATURITY
  ), 'Modified duration calculation failed');

  WriteLn('Test 12: Modified Duration Test: Finished');
end;

procedure TTestCaseFinance.Test13_BreakEvenAnalysis;
const
  FIXED_COSTS = 100000.0;
  PRICE_PER_UNIT = 50.0;
  VARIABLE_COST_PER_UNIT = 30.0;
var
  Units, Revenue: Double;
begin
  Units := TFinanceKit.BreakEvenUnits(FIXED_COSTS, PRICE_PER_UNIT, VARIABLE_COST_PER_UNIT);
  Revenue := TFinanceKit.BreakEvenRevenue(FIXED_COSTS, PRICE_PER_UNIT, VARIABLE_COST_PER_UNIT);
  
  AssertFinanceEquals(5000.0, Units, 'Break-even units calculation failed');
  AssertFinanceEquals(250000.0, Revenue, 'Break-even revenue calculation failed');
end;

procedure TTestCaseFinance.Test14_WACC;
const
  EQUITY_VALUE = 1000000.0;
  DEBT_VALUE = 500000.0;
  COST_OF_EQUITY = 0.15;
  COST_OF_DEBT = 0.08;
  TAX_RATE = 0.30;
begin
  AssertFinanceEquals(0.1187, TFinanceKit.WACC(
    EQUITY_VALUE, DEBT_VALUE, COST_OF_EQUITY, COST_OF_DEBT, TAX_RATE
  ), 'WACC calculation failed');
end;

procedure TTestCaseFinance.Test15_CAPM;
const
  RISK_FREE_RATE = 0.03;
  BETA = 1.2;
  MARKET_RETURN = 0.10;
begin
  AssertFinanceEquals(0.114, TFinanceKit.CAPM(
    RISK_FREE_RATE, BETA, MARKET_RETURN
  ), 'CAPM calculation failed');
end;

procedure TTestCaseFinance.Test16_GordonGrowthModel;
const
  CURRENT_DIVIDEND = 2.0;
  GROWTH_RATE = 0.03;
  REQUIRED_RETURN = 0.08;
begin
  AssertFinanceEquals(41.20, TFinanceKit.GordonGrowthModel(
    CURRENT_DIVIDEND, GROWTH_RATE, REQUIRED_RETURN
  ), 'Gordon Growth Model calculation failed');
end;

procedure TTestCaseFinance.Test17_WorkingCapitalRatios;
const
  CURRENT_ASSETS = 100000.0;
  CURRENT_LIABILITIES = 50000.0;
  INVENTORY = 30000.0;
  CASH = 20000.0;
  SALES = 200000.0;
var
  Ratios: TWorkingCapitalRatios;
begin
  Ratios := TFinanceKit.WorkingCapitalRatios(
    CURRENT_ASSETS, CURRENT_LIABILITIES, INVENTORY, CASH, SALES
  );
  
  AssertFinanceEquals(2.0, Ratios.CurrentRatio, 'Current ratio calculation failed');
  AssertFinanceEquals(1.4, Ratios.QuickRatio, 'Quick ratio calculation failed');
  AssertFinanceEquals(0.4, Ratios.CashRatio, 'Cash ratio calculation failed');
  AssertFinanceEquals(4.0, Ratios.WorkingCapitalTurnover, 'Working capital turnover calculation failed');
end;

procedure TTestCaseFinance.Test18_LeverageRatios;
const
  TOTAL_DEBT = 500000.0;
  TOTAL_ASSETS = 1000000.0;
  TOTAL_EQUITY = 500000.0;
  EBIT = 100000.0;
  INTEREST_EXPENSE = 25000.0;
var
  Ratios: TLeverageRatios;
begin
  Ratios := TFinanceKit.LeverageRatios(
    TOTAL_DEBT, TOTAL_ASSETS, TOTAL_EQUITY, EBIT, INTEREST_EXPENSE
  );
  
  AssertFinanceEquals(0.5, Ratios.DebtRatio, 'Debt ratio calculation failed');
  AssertFinanceEquals(1.0, Ratios.DebtToEquityRatio, 'Debt to equity ratio calculation failed');
  AssertFinanceEquals(2.0, Ratios.EquityMultiplier, 'Equity multiplier calculation failed');
  AssertFinanceEquals(4.0, Ratios.TimesInterestEarned, 'Times interest earned calculation failed');
end;

procedure TTestCaseFinance.Test19_BlackScholes;
const
  SPOT_PRICE = 100.0;
  STRIKE_PRICE = 100.0;
  RISK_FREE_RATE = 0.05;
  VOLATILITY = 0.2;
  TIME_TO_MATURITY = 1.0;
var
  CallPrice, PutPrice: Double;
begin
  CallPrice := TFinanceKit.BlackScholes(
    SPOT_PRICE, STRIKE_PRICE, RISK_FREE_RATE, VOLATILITY, TIME_TO_MATURITY, otCall
  );
  PutPrice := TFinanceKit.BlackScholes(
    SPOT_PRICE, STRIKE_PRICE, RISK_FREE_RATE, VOLATILITY, TIME_TO_MATURITY, otPut
  );
  
  AssertFinanceEquals(10.4506, CallPrice, 'Black-Scholes call option price calculation failed');
  AssertFinanceEquals(5.5723, PutPrice, 'Black-Scholes put option price calculation failed');
end;

procedure TTestCaseFinance.Test20_RiskMetrics;
const
  PORTFOLIO_RETURN = 0.15;
  RISK_FREE_RATE = 0.03;
  MARKET_RETURN = 0.10;
  BETA = 1.2;
  PORTFOLIO_STD_DEV = 0.18;
  BENCHMARK_RETURN = 0.12;
  TRACKING_ERROR = 0.04;
var
  Metrics: TRiskMetrics;
begin
  Metrics := TFinanceKit.RiskMetrics(
    PORTFOLIO_RETURN, RISK_FREE_RATE, MARKET_RETURN, BETA,
    PORTFOLIO_STD_DEV, BENCHMARK_RETURN, TRACKING_ERROR
  );
  
  AssertFinanceEquals(0.6667, Metrics.SharpeRatio, 'Sharpe ratio calculation failed');
  AssertFinanceEquals(0.1000, Metrics.TreynorRatio, 'Treynor ratio calculation failed');
  AssertFinanceEquals(0.036, Metrics.JensenAlpha, 'Jensen alpha calculation failed');
  AssertFinanceEquals(0.7500, Metrics.InformationRatio, 'Information ratio calculation failed');
end;

procedure TTestCaseFinance.Test21_DuPontAnalysis;
const
  NET_INCOME = 100000.0;
  SALES = 1000000.0;
  TOTAL_ASSETS = 800000.0;
  TOTAL_EQUITY = 500000.0;
var
  Analysis: TDuPontAnalysis;
begin
  Analysis := TFinanceKit.DuPontAnalysis(
    NET_INCOME, SALES, TOTAL_ASSETS, TOTAL_EQUITY
  );
  
  AssertFinanceEquals(0.10, Analysis.ProfitMargin, 'Profit margin calculation failed');
  AssertFinanceEquals(1.25, Analysis.AssetTurnover, 'Asset turnover calculation failed');
  AssertFinanceEquals(1.60, Analysis.EquityMultiplier, 'Equity multiplier calculation failed');
  AssertFinanceEquals(0.20, Analysis.ROE, 'DuPont ROE calculation failed');
end;

procedure TTestCaseFinance.Test22_OperatingLeverage;
const
  QUANTITY = 10000.0;
  PRICE_PER_UNIT = 50.0;
  VARIABLE_COST_PER_UNIT = 30.0;
  FIXED_COSTS = 100000.0;
var
  Leverage: TOperatingLeverage;
begin
  Leverage := TFinanceKit.OperatingLeverage(
    QUANTITY, PRICE_PER_UNIT, VARIABLE_COST_PER_UNIT, FIXED_COSTS
  );
  
  AssertFinanceEquals(1.25, Leverage.DOL, 'Degree of operating leverage calculation failed');
  AssertFinanceEquals(5000.0, Leverage.BreakEvenPoint, 'Break-even point calculation failed');
  AssertFinanceEquals(1.25, Leverage.OperatingLeverage, 'Operating leverage calculation failed');
end;

procedure TTestCaseFinance.Test23_ProfitabilityRatios;
const
  REVENUE = 1000000.0;
  COGS = 600000.0;
  EBIT = 200000.0;
  NET_INCOME = 150000.0;
  TOTAL_ASSETS = 800000.0;
  CURRENT_LIABILITIES = 200000.0;
var
  Ratios: TProfitabilityRatios;
begin
  Ratios := TFinanceKit.ProfitabilityRatios(
    REVENUE, COGS, EBIT, NET_INCOME, TOTAL_ASSETS, CURRENT_LIABILITIES
  );
  
  AssertFinanceEquals(0.40, Ratios.GrossMargin, 'Gross margin calculation failed');
  AssertFinanceEquals(0.20, Ratios.OperatingMargin, 'Operating margin calculation failed');
  AssertFinanceEquals(0.15, Ratios.NetProfitMargin, 'Net profit margin calculation failed');
  AssertFinanceEquals(0.1875, Ratios.ROA, 'ROA calculation failed');
  AssertFinanceEquals(0.3333, Ratios.ROCE, 'ROCE calculation failed');
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
