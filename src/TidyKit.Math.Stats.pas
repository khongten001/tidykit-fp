unit TidyKit.Math.Stats;

{$mode objfpc}{$H+}{$J-}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Math, TidyKit.Math, Generics.Collections;

type
  { Descriptive statistics record }
  TDescriptiveStats = record
    N: Integer;
    Mean: Double;
    Median: Double;
    Mode: Double;
    Q1: Double;
    Q3: Double;
    Min: Double;
    Max: Double;
    Range: Double;
    IQR: Double;
    Variance: Double;
    StdDev: Double;
    Skewness: Double;
    Kurtosis: Double;
    SEM: Double;  // Standard Error of Mean
    CV: Double;   // Coefficient of Variation
    
    function ToString: string;
    function ToStringWide: string;  // Horizontal format
  end;

  { Statistics calculations class }
  TStatsKit = class
  private
    class procedure Exchange(var A, B: Double); static;
    class procedure ExchangeInt(var A, B: Integer); static;
    class function RandomSample(const Data: TDoubleArray): TDoubleArray; static;
  public
    { Basic statistics }
    class function Mean(const Data: TDoubleArray): Double; static;
    class function Median(const Data: TDoubleArray): Double; static;
    class function Mode(const Data: TDoubleArray): Double; static;
    class function Range(const Data: TDoubleArray): Double; static;
    
    { Variance and standard deviation }
    class function Variance(const Data: TDoubleArray): Double; static;
    class function StandardDeviation(const Data: TDoubleArray): Double; static;
    class function SampleVariance(const Data: TDoubleArray): Double; static;
    class function SampleStandardDeviation(const Data: TDoubleArray): Double; static;
    
    { Distribution measures }
    class function Skewness(const Data: TDoubleArray): Double; static;
    class function Kurtosis(const Data: TDoubleArray): Double; static;
    
    { Percentiles and quartiles }
    class function Percentile(const Data: TDoubleArray; const P: Double): Double; static;
    class function Quartile1(const Data: TDoubleArray): Double; static;
    class function Quartile3(const Data: TDoubleArray): Double; static;
    class function InterquartileRange(const Data: TDoubleArray): Double; static;
    
    { Correlation and covariance }
    class function PearsonCorrelation(const X, Y: TDoubleArray): Double; static;
    class function SpearmanCorrelation(const X, Y: TDoubleArray): Double; static;
    class function Covariance(const X, Y: TDoubleArray): Double; static;
    
    { Z-score and normalization }
    class function ZScore(const Value, AMean, StdDev: Double): Double; static;
    class procedure Standardize(var Data: TDoubleArray); static;
    
    { Helper functions }
    class function Sum(const Data: TDoubleArray): Double; static;
    class function SumOfSquares(const Data: TDoubleArray): Double; static;
    class procedure Sort(var Data: TDoubleArray); static;

    { Descriptive Statistics }
    class function Describe(const Data: TDoubleArray): TDescriptiveStats; static;
    
    { Additional Basic Statistics }
    class function GeometricMean(const Data: TDoubleArray): Double; static;
    class function HarmonicMean(const Data: TDoubleArray): Double; static;
    class function TrimmedMean(const Data: TDoubleArray; const Percent: Double): Double; static;
    class function WinsorizedMean(const Data: TDoubleArray; const Percent: Double): Double; static;
    
    { Non-parametric Statistics }
    class function MedianAbsoluteDeviation(const Data: TDoubleArray): Double; static;
    class function SignTest(const X, Y: TDoubleArray): Double; static;
    class function WilcoxonSignedRank(const X, Y: TDoubleArray): Double; static;
    class function KendallTau(const X, Y: TDoubleArray): Double; static;
    
    { Hypothesis Testing }
    class function TTest(const X, Y: TDoubleArray; out TPValue: Double): Double; static;
    class function MannWhitneyU(const X, Y: TDoubleArray; out UPValue: Double): Double; static;
    class function KolmogorovSmirnovTest(const Data: TDoubleArray; out KSPValue: Double): Double; static;
    
    { Distribution Tests }
    class function IsNormal(const Data: TDoubleArray; const Alpha: Double = 0.05): Boolean; static;
    class function ShapiroWilkTest(const Data: TDoubleArray; out WPValue: Double): Double; static;
    
    { Effect Size Measures }
    class function CohensD(const X, Y: TDoubleArray): Double; static;
    class function HedgesG(const X, Y: TDoubleArray): Double; static;
    
    { Additional Measures }
    class function StandardErrorOfMean(const Data: TDoubleArray): Double; static;
    class function CoefficientOfVariation(const Data: TDoubleArray): Double; static;
    class function Quantile(const Data: TDoubleArray; const Q: Double): Double; static;
    
    { Robust Statistics }
    class function RobustStandardDeviation(const Data: TDoubleArray): Double; static;
    class function HuberM(const Data: TDoubleArray; const K: Double = 1.5): Double; static;
    
    { Bootstrap Methods }
    class function BootstrapMean(const Data: TDoubleArray; const Iterations: Integer): TDoubleArray; static;
    class function BootstrapConfidenceInterval(const Data: TDoubleArray; 
      const Alpha: Double = 0.05; const Iterations: Integer = 1000): TDoublePair; static;
  end;

implementation

{ TStatsKit }

class function TStatsKit.Mean(const Data: TDoubleArray): Double;
begin
  if Length(Data) = 0 then
    raise Exception.Create('Cannot calculate mean of empty array');
  Result := Sum(Data) / Length(Data);
end;

class function TStatsKit.Median(const Data: TDoubleArray): Double;
var
  SortedData: TDoubleArray;
  Mid: Integer;
begin
  if Length(Data) = 0 then
    raise Exception.Create('Cannot calculate median of empty array');
    
  SortedData := Copy(Data);
  Sort(SortedData);
  
  Mid := Length(SortedData) div 2;
  if Length(SortedData) mod 2 = 0 then
    Result := (SortedData[Mid - 1] + SortedData[Mid]) / 2
  else
    Result := SortedData[Mid];
end;

class function TStatsKit.Mode(const Data: TDoubleArray): Double;
var
  SortedData: TDoubleArray;
  I, MaxCount, CurrentCount: Integer;
  CurrentValue: Double;
begin
  if Length(Data) = 0 then
    raise Exception.Create('Cannot calculate mode of empty array');
    
  SortedData := Copy(Data);
  Sort(SortedData);
  
  Result := SortedData[0];
  MaxCount := 1;
  CurrentCount := 1;
  CurrentValue := SortedData[0];
  
  for I := 1 to High(SortedData) do
  begin
    if SortedData[I] = CurrentValue then
      Inc(CurrentCount)
    else
    begin
      if CurrentCount > MaxCount then
      begin
        MaxCount := CurrentCount;
        Result := CurrentValue;
      end;
      CurrentCount := 1;
      CurrentValue := SortedData[I];
    end;
  end;
  
  if CurrentCount > MaxCount then
    Result := CurrentValue;
end;

class function TStatsKit.Range(const Data: TDoubleArray): Double;
var
  SortedData: TDoubleArray;
begin
  if Length(Data) = 0 then
    raise Exception.Create('Cannot calculate range of empty array');
    
  SortedData := Copy(Data);
  Sort(SortedData);
  Result := SortedData[High(SortedData)] - SortedData[0];
end;

class function TStatsKit.Variance(const Data: TDoubleArray): Double;
var
  M, SumSq: Double;
  I, N: Integer;
begin
  N := Length(Data);
  if N < 2 then
    raise Exception.Create('Cannot calculate variance with less than 2 values');
    
  M := Mean(Data);
  SumSq := 0;
  for I := 0 to High(Data) do
    SumSq := SumSq + Sqr(Data[I] - M);
    
  Result := SumSq / (N - 1);  // Sample variance (n-1)
end;

class function TStatsKit.StandardDeviation(const Data: TDoubleArray): Double;
var
  M, S2: Double;
  I: Integer;
begin
  if Length(Data) < 2 then
    raise Exception.Create('Cannot calculate standard deviation with less than 2 values');
    
  M := Mean(Data);
  S2 := 0;
  
  for I := 0 to High(Data) do
    S2 := S2 + Sqr(Data[I] - M);
    
  Result := Sqrt(S2 / Length(Data));  // Population standard deviation (using n)
end;

class function TStatsKit.SampleVariance(const Data: TDoubleArray): Double;
var
  M, SumSq: Double;
  I, N: Integer;
begin
  N := Length(Data);
  if N < 2 then
    raise Exception.Create('Cannot calculate sample variance with less than 2 values');
    
  M := Mean(Data);
  SumSq := 0;
  for I := 0 to High(Data) do
    SumSq := SumSq + Sqr(Data[I] - M);
  Result := SumSq / (N - 1);
end;

class function TStatsKit.SampleStandardDeviation(const Data: TDoubleArray): Double;
begin
  Result := Sqrt(SampleVariance(Data));
end;

class function TStatsKit.Skewness(const Data: TDoubleArray): Double;
var
  M, S: Double;
  I: Integer;
  N: Integer;
  Total: Double;
begin
  N := Length(Data);
  if N < 3 then
    raise Exception.Create('Cannot calculate skewness with less than 3 values');
    
  M := Mean(Data);
  S := StandardDeviation(Data);
  
  Total := 0;
  for I := 0 to High(Data) do
    Total := Total + Power((Data[I] - M) / S, 3);
    
  Result := Total / N;
end;

class function TStatsKit.Kurtosis(const Data: TDoubleArray): Double;
var
  M, S: Double;
  I: Integer;
  N: Integer;
  Total: Double;
begin
  N := Length(Data);
  if N < 4 then
    raise Exception.Create('Cannot calculate kurtosis with less than 4 values');
    
  M := Mean(Data);
  S := SampleStandardDeviation(Data);
  
  Total := 0;
  for I := 0 to High(Data) do
    Total := Total + Power((Data[I] - M) / S, 4);
    
  // Adjusted formula for sample excess kurtosis
  Result := (N * (N + 1) * Total / ((N - 1) * (N - 2) * (N - 3))) - (3 * Sqr(N - 1) / ((N - 2) * (N - 3)));
end;

class function TStatsKit.Percentile(const Data: TDoubleArray; const P: Double): Double;
var
  SortedData: TDoubleArray;
  N: Integer;
  Position: Double;
  Index: Integer;
  Fraction: Double;
begin
  if (P < 0) or (P > 100) then
    raise Exception.Create('Percentile must be between 0 and 100');
    
  N := Length(Data);
  if N = 0 then
    raise Exception.Create('Cannot calculate percentile of empty array');
    
  SortedData := Copy(Data);
  Sort(SortedData);
  
  Position := (P / 100) * (N - 1);
  Index := Trunc(Position);
  Fraction := Frac(Position);
  
  if Index = High(SortedData) then
    Result := SortedData[Index]
  else
    Result := SortedData[Index] + Fraction * (SortedData[Index + 1] - SortedData[Index]);
end;

class function TStatsKit.Quartile1(const Data: TDoubleArray): Double;
begin
  Result := Percentile(Data, 25);
end;

class function TStatsKit.Quartile3(const Data: TDoubleArray): Double;
begin
  Result := Percentile(Data, 75);
end;

class function TStatsKit.InterquartileRange(const Data: TDoubleArray): Double;
begin
  Result := Quartile3(Data) - Quartile1(Data);
end;

class function TStatsKit.PearsonCorrelation(const X, Y: TDoubleArray): Double;
var
  N: Integer;
  MeanX, MeanY, SumXY, SumX2, SumY2: Double;
  I: Integer;
begin
  N := Length(X);
  if N <> Length(Y) then
    raise Exception.Create('Arrays must have equal length for Pearson correlation');
  if N < 2 then
    raise Exception.Create('Cannot calculate correlation with less than 2 values');
    
  MeanX := Mean(X);
  MeanY := Mean(Y);
  
  SumXY := 0;
  SumX2 := 0;
  SumY2 := 0;
  
  for I := 0 to N - 1 do
  begin
    SumXY := SumXY + (X[I] - MeanX) * (Y[I] - MeanY);
    SumX2 := SumX2 + Sqr(X[I] - MeanX);
    SumY2 := SumY2 + Sqr(Y[I] - MeanY);
  end;
  
  if (SumX2 = 0) or (SumY2 = 0) then
    Result := 0
  else
    Result := SumXY / Sqrt(SumX2 * SumY2);
end;

class function TStatsKit.SpearmanCorrelation(const X, Y: TDoubleArray): Double;
var
  N, I: Integer;
  RankX, RankY: TDoubleArray;
  
  function GetRanks(const Data: TDoubleArray): TDoubleArray;
  var
    I, J, TieCount: Integer;
    SortedIndices: array of Integer;
    SortedData: TDoubleArray;
    TieRankSum: Double;
  begin
    SetLength(SortedIndices, Length(Data));
    SetLength(SortedData, Length(Data));
    SetLength(Result, Length(Data));
    
    // Initialize indices and copy data
    for I := 0 to High(Data) do
    begin
      SortedIndices[I] := I;
      SortedData[I] := Data[I];
    end;
    
    // Sort indices based on data values
    for I := 0 to High(Data) - 1 do
      for J := I + 1 to High(Data) do
        if SortedData[J] < SortedData[I] then
        begin
          Exchange(SortedData[I], SortedData[J]);
          ExchangeInt(SortedIndices[I], SortedIndices[J]);
        end;
    
    // Assign ranks, handling ties
    I := 0;
    while I <= High(Data) do
    begin
      TieCount := 1;
      TieRankSum := I + 1;
      
      // Count ties
      while (I < High(Data)) and (SortedData[I] = SortedData[I + 1]) do
      begin
        Inc(I);
        Inc(TieCount);
        TieRankSum := TieRankSum + I + 1;
      end;
      
      // Assign average rank for ties
      if TieCount > 1 then
      begin
        for J := I - TieCount + 1 to I do
          Result[SortedIndices[J]] := TieRankSum / TieCount;
      end
      else
        Result[SortedIndices[I]] := I + 1;
      
      Inc(I);
    end;
  end;
  
begin
  N := Length(X);
  if N <> Length(Y) then
    raise Exception.Create('Arrays must have equal length for Spearman correlation');
  if N < 2 then
    raise Exception.Create('Cannot calculate correlation with less than 2 values');
  
  // Get ranks for both arrays
  RankX := GetRanks(X);
  RankY := GetRanks(Y);
  
  // Calculate Pearson correlation of ranks
  Result := PearsonCorrelation(RankX, RankY);
end;

class function TStatsKit.Covariance(const X, Y: TDoubleArray): Double;
var
  N: Integer;
  MeanX, MeanY: Double;
  I: Integer;
  Total: Double;
begin
  N := Length(X);
  if (N <> Length(Y)) or (N < 2) then
    raise Exception.Create('Arrays must have equal length and at least 2 elements');
    
  MeanX := Mean(X);
  MeanY := Mean(Y);
  
  Total := 0;
  for I := 0 to High(X) do
    Total := Total + (X[I] - MeanX) * (Y[I] - MeanY);
    
  Result := Total / (N - 1);
end;

class function TStatsKit.ZScore(const Value, AMean, StdDev: Double): Double;
begin
  if StdDev = 0 then
    raise Exception.Create('Standard deviation cannot be zero');
  Result := (Value - AMean) / StdDev;
end;

class procedure TStatsKit.Standardize(var Data: TDoubleArray);
var
  M, S: Double;
  I: Integer;
begin
  M := Mean(Data);
  S := StandardDeviation(Data);
  
  if S = 0 then
    raise Exception.Create('Cannot standardize data with zero standard deviation');
    
  for I := 0 to High(Data) do
    Data[I] := ZScore(Data[I], M, S);
end;

class function TStatsKit.Sum(const Data: TDoubleArray): Double;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(Data) do
    Result := Result + Data[I];
end;

class function TStatsKit.SumOfSquares(const Data: TDoubleArray): Double;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(Data) do
    Result := Result + Sqr(Data[I]);
end;

class procedure TStatsKit.Sort(var Data: TDoubleArray);
var
  I, J: Integer;
  Temp: Double;
begin
  for I := 0 to High(Data) - 1 do
    for J := I + 1 to High(Data) do
      if Data[J] < Data[I] then
      begin
        Temp := Data[I];
        Data[I] := Data[J];
        Data[J] := Temp;
      end;
end;

class procedure TStatsKit.Exchange(var A, B: Double);
var
  Temp: Double;
begin
  Temp := A;
  A := B;
  B := Temp;
end;

class procedure TStatsKit.ExchangeInt(var A, B: Integer);
var
  Temp: Integer;
begin
  Temp := A;
  A := B;
  B := Temp;
end;

class function TStatsKit.Describe(const Data: TDoubleArray): TDescriptiveStats;
var
  SortedData: TDoubleArray;
begin
  if Length(Data) = 0 then
    raise Exception.Create('Cannot calculate descriptive statistics of empty array');

  Result.N := Length(Data);
  Result.Mean := Mean(Data);
  Result.Median := Median(Data);
  Result.Mode := Mode(Data);
  Result.Q1 := Quartile1(Data);
  Result.Q3 := Quartile3(Data);
  
  // Calculate Min and Max
  SortedData := Copy(Data);
  Sort(SortedData);
  Result.Min := SortedData[0];
  Result.Max := SortedData[High(SortedData)];
  
  Result.Range := Range(Data);
  Result.IQR := InterquartileRange(Data);
  Result.Variance := Variance(Data);
  Result.StdDev := StandardDeviation(Data);
  Result.Skewness := Skewness(Data);
  Result.Kurtosis := Kurtosis(Data);
  Result.SEM := StandardErrorOfMean(Data);
  Result.CV := CoefficientOfVariation(Data);
end;

class function TStatsKit.GeometricMean(const Data: TDoubleArray): Double;
var
  I: Integer;
  LogSum: Double;
begin
  if Length(Data) = 0 then
    raise Exception.Create('Cannot calculate geometric mean of empty array');
    
  for I := 0 to High(Data) do
    if Data[I] <= 0 then
      raise Exception.Create('Geometric mean requires positive values');
      
  LogSum := 0;
  for I := 0 to High(Data) do
    LogSum := LogSum + Ln(Data[I]);
    
  Result := Exp(LogSum / Length(Data));
end;

class function TStatsKit.HarmonicMean(const Data: TDoubleArray): Double;
var
  I: Integer;
  ReciprocalSum: Double;
begin
  if Length(Data) = 0 then
    raise Exception.Create('Cannot calculate harmonic mean of empty array');
    
  ReciprocalSum := 0;
  for I := 0 to High(Data) do
  begin
    if Data[I] <= 0 then
      raise Exception.Create('Harmonic mean requires positive values');
    ReciprocalSum := ReciprocalSum + 1 / Data[I];  // Simple division
  end;
  
  if ReciprocalSum = 0 then
    raise Exception.Create('Cannot calculate harmonic mean when sum of reciprocals is zero');
    
  Result := Length(Data) / ReciprocalSum;  // Simple division
end;

class function TStatsKit.TrimmedMean(const Data: TDoubleArray; const Percent: Double): Double;
var
  SortedData: TDoubleArray;
  TrimCount, StartIdx, EndIdx: Integer;
  I: Integer;
  TrimmedSum: Double;
begin
  if (Percent < 0) or (Percent >= 50) then
    raise Exception.Create('Trim percentage must be between 0 and 50');
    
  if Length(Data) = 0 then
    raise Exception.Create('Cannot calculate trimmed mean of empty array');
    
  SortedData := Copy(Data);
  Sort(SortedData);
  
  TrimCount := Floor(Length(Data) * (Percent / 100));  // Calculate number to trim from each end
  StartIdx := TrimCount;
  EndIdx := Length(Data) - TrimCount - 1;
  
  TrimmedSum := 0;
  for I := StartIdx to EndIdx do
    TrimmedSum := TrimmedSum + SortedData[I];
    
  Result := TrimmedSum / (EndIdx - StartIdx + 1);
end;

class function TStatsKit.WinsorizedMean(const Data: TDoubleArray; const Percent: Double): Double;
var
  SortedData: TDoubleArray;
  WinsorCount: Integer;
  I: Integer;
  WinsorizedSum: Double;
begin
  if (Percent < 0) or (Percent >= 50) then
    raise Exception.Create('Winsorization percentage must be between 0 and 50');
    
  if Length(Data) = 0 then
    raise Exception.Create('Cannot calculate Winsorized mean of empty array');
    
  SortedData := Copy(Data);
  Sort(SortedData);
  
  WinsorCount := Round(Length(Data) * (Percent / 100));
  
  // Replace values with Winsorized values
  for I := 0 to WinsorCount - 1 do
    SortedData[I] := SortedData[WinsorCount];
    
  for I := Length(SortedData) - WinsorCount to High(SortedData) do
    SortedData[I] := SortedData[Length(SortedData) - WinsorCount - 1];
    
  WinsorizedSum := 0;
  for I := 0 to High(SortedData) do
    WinsorizedSum := WinsorizedSum + SortedData[I];
    
  Result := WinsorizedSum / Length(SortedData);
end;

class function TStatsKit.StandardErrorOfMean(const Data: TDoubleArray): Double;
begin
  if Length(Data) < 2 then
    raise Exception.Create('Cannot calculate SEM with less than 2 values');
    
  Result := SampleStandardDeviation(Data) / Sqrt(Length(Data));
end;

class function TStatsKit.CoefficientOfVariation(const Data: TDoubleArray): Double;
var
  M, S: Double;
begin
  if Length(Data) = 0 then
    raise Exception.Create('Cannot calculate CV of empty array');
    
  M := Mean(Data);
  if M = 0 then
    raise Exception.Create('Cannot calculate CV when mean is zero');
    
  S := StandardDeviation(Data);
  Result := (S / Abs(M)) * 100;  // Simple percentage calculation
end;

class function TStatsKit.MedianAbsoluteDeviation(const Data: TDoubleArray): Double;
var
  M: Double;
  Deviations: TDoubleArray;
  I: Integer;
begin
  if Length(Data) = 0 then
    raise Exception.Create('Cannot calculate MAD of empty array');
    
  M := Median(Data);
  SetLength(Deviations, Length(Data));  // Initialize the array
  
  for I := 0 to High(Data) do
    Deviations[I] := Abs(Data[I] - M);
    
  Result := Median(Deviations);
end;

class function TStatsKit.RobustStandardDeviation(const Data: TDoubleArray): Double;
begin
  // MAD * 1.4826 is a robust estimator of standard deviation for normal distributions
  Result := MedianAbsoluteDeviation(Data) * 1.4826;
end;

class function TStatsKit.HuberM(const Data: TDoubleArray; const K: Double = 1.5): Double;
var
  M, MAD: Double;
  I: Integer;
  WeightedSum, WeightSum: Double;
  Weight: Double;
begin
  if Length(Data) = 0 then
    raise Exception.Create('Cannot calculate Huber M-estimator of empty array');
    
  M := Median(Data);
  MAD := MedianAbsoluteDeviation(Data);
  
  if MAD = 0 then
    Exit(M);  // If MAD is 0, return median
    
  WeightedSum := 0;
  WeightSum := 0;
  
  for I := 0 to High(Data) do
  begin
    if Abs(Data[I] - M) <= K * MAD then
      Weight := 1
    else
      Weight := (K * MAD) / Abs(Data[I] - M);
      
    WeightedSum := WeightedSum + Weight * Data[I];
    WeightSum := WeightSum + Weight;
  end;
  
  Result := WeightedSum / WeightSum;
end;

class function TStatsKit.BootstrapMean(const Data: TDoubleArray; const Iterations: Integer): TDoubleArray;
var
  I: Integer;
  Sample: TDoubleArray;
begin
  SetLength(Result, Iterations);
  for I := 0 to Iterations - 1 do
  begin
    Sample := RandomSample(Data);
    Result[I] := Mean(Sample);
  end;
end;

class function TStatsKit.BootstrapConfidenceInterval(const Data: TDoubleArray; 
  const Alpha: Double = 0.05; const Iterations: Integer = 1000): TDoublePair;
var
  Means: TDoubleArray;
begin
  Means := BootstrapMean(Data, Iterations);
  Sort(Means);
  
  Result.Lower := Percentile(Means, (Alpha / 2) * 100);
  Result.Upper := Percentile(Means, (1 - (Alpha / 2)) * 100);
end;

class function TStatsKit.RandomSample(const Data: TDoubleArray): TDoubleArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Data));
  for I := 0 to High(Data) do
    Result[I] := Data[Random(Length(Data))];
end;

class function TStatsKit.TTest(const X, Y: TDoubleArray; out TPValue: Double): Double;
var
  MeanX, MeanY, VarX, VarY: Double;
  NX, NY: Integer;
  PooledVar, SE: Double;
  DF: Integer;
begin
  if (Length(X) < 2) or (Length(Y) < 2) then
    raise Exception.Create('T-test requires at least 2 values in each group');
    
  NX := Length(X);
  NY := Length(Y);
  MeanX := Mean(X);
  MeanY := Mean(Y);
  VarX := Variance(X);
  VarY := Variance(Y);
  
  // Pooled variance
  PooledVar := ((NX - 1) * VarX + (NY - 1) * VarY) / (NX + NY - 2);
  
  if PooledVar <= 0 then
    raise Exception.Create('Invalid pooled variance for T-test calculation');
    
  SE := Sqrt(PooledVar * (1.0/NX + 1.0/NY));
  
  if SE = 0 then
    raise Exception.Create('Cannot calculate T-statistic with zero standard error');
    
  Result := (MeanX - MeanY) / SE;
  
  // Degrees of freedom
  DF := NX + NY - 2;
  
  // Calculate p-value using Student's t-distribution
  TPValue := 2.0 * (1.0 - TidyKit.Math.StudentT(DF, Abs(Result)));
  
  // Ensure p-value is between 0 and 1
  if TPValue < 0 then
    TPValue := 0
  else if TPValue > 1 then
    TPValue := 1;
end;

class function TStatsKit.MannWhitneyU(const X, Y: TDoubleArray; out UPValue: Double): Double;
var
  NX, NY, I, J: Integer;
  RankSum: Double;
  AllData: array of record
    Value: Double;
    Group: Integer;  // 1 for X, 2 for Y
    Rank: Double;
  end;
  U1, U2: Double;
  TieCount: Integer;
  TieSum: Double;
  AverageRank: Double;
  TempValue: Double;
  TempGroup: Integer;
  TempRank: Double;
  ExpectedMean: Double;
  DistVariance: Double;
  Z: Double;
begin
  NX := Length(X);
  NY := Length(Y);
  if (NX < 2) or (NY < 2) then
    raise Exception.Create('Mann-Whitney U test requires at least 2 values in each group');
    
  // Initialize managed type
  SetLength(AllData, NX + NY);
  
  // Combine data and assign groups
  for I := 0 to NX - 1 do
  begin
    AllData[I].Value := X[I];
    AllData[I].Group := 1;
    AllData[I].Rank := 0;  // Initialize rank
  end;
  for I := 0 to NY - 1 do
  begin
    AllData[NX + I].Value := Y[I];
    AllData[NX + I].Group := 2;
    AllData[NX + I].Rank := 0;  // Initialize rank
  end;
  
  // Sort by value
  for I := 0 to High(AllData) - 1 do
    for J := I + 1 to High(AllData) do
      if AllData[J].Value < AllData[I].Value then
      begin
        // Store temporary values
        TempValue := AllData[I].Value;
        TempGroup := AllData[I].Group;
        TempRank := AllData[I].Rank;
        
        // Copy J to I
        AllData[I].Value := AllData[J].Value;
        AllData[I].Group := AllData[J].Group;
        AllData[I].Rank := AllData[J].Rank;
        
        // Copy temp to J
        AllData[J].Value := TempValue;
        AllData[J].Group := TempGroup;
        AllData[J].Rank := TempRank;
      end;
      
  // Assign ranks (handling ties)
  I := 0;
  while I <= High(AllData) do
  begin
    TieCount := 1;
    TieSum := I + 1;
    
    while (I < High(AllData)) and (AllData[I + 1].Value = AllData[I].Value) do
    begin
      Inc(I);
      Inc(TieCount);
      TieSum := TieSum + I + 1;
    end;
    
    AverageRank := TieSum / TieCount;
    for J := I - TieCount + 1 to I do
      AllData[J].Rank := AverageRank;
      
    Inc(I);
  end;
  
  // Calculate U statistics
  RankSum := 0;
  for I := 0 to High(AllData) do
    if AllData[I].Group = 1 then
      RankSum := RankSum + AllData[I].Rank;
      
  U1 := RankSum - (NX * (NX + 1)) / 2;
  U2 := NX * NY - U1;
  
  Result := Min(U1, U2);
  
  // Approximate p-value using normal distribution for large samples
  if (NX > 10) and (NY > 10) then
  begin
    ExpectedMean := NX * NY / 2;
    DistVariance := (NX * NY * (NX + NY + 1)) / 12;
    Z := (Result - ExpectedMean) / Sqrt(DistVariance);
    UPValue := 2 * (1 - TidyKit.Math.NormalCDF(Abs(Z)));
  end
  else
    UPValue := 1;  // Exact p-value calculation not implemented
end;

class function TStatsKit.KolmogorovSmirnovTest(const Data: TDoubleArray; out KSPValue: Double): Double;
var
  N, I: Integer;
  MaxDiff: Double;
  SortedData: TDoubleArray;
  ObservedCDF: Double;
  ExpectedCDF: Double;
  CriticalValue: Double;
begin
  N := Length(Data);
  if N < 5 then
    raise Exception.Create('K-S test requires at least 5 values');
    
  SortedData := Copy(Data);
  Sort(SortedData);
  
  MaxDiff := 0;
  for I := 0 to High(SortedData) do
  begin
    ObservedCDF := (I + 1) / N;
    ExpectedCDF := TidyKit.Math.NormalCDF((SortedData[I] - Mean(Data)) / StandardDeviation(Data));
    MaxDiff := Max(MaxDiff, Abs(ObservedCDF - ExpectedCDF));
  end;
  
  Result := MaxDiff;
  
  // Critical value at α = 0.05
  CriticalValue := 0.886 / Sqrt(N);
  KSPValue := MaxDiff;  // Return the actual statistic value, not the boolean comparison
end;

class function TStatsKit.IsNormal(const Data: TDoubleArray; const Alpha: Double): Boolean;
var
  KSPValue: Double;
begin
  KolmogorovSmirnovTest(Data, KSPValue);
  Result := KSPValue >= Alpha;
end;

class function TStatsKit.ShapiroWilkTest(const Data: TDoubleArray; out WPValue: Double): Double;
var
  N, I: Integer;
  SortedData: TDoubleArray;
  DataMean, S2: Double;
  B, W: Double;
  Weights: array of Double;
begin
  N := Length(Data);
  if (N < 3) or (N > 50) then
    raise Exception.Create('Shapiro-Wilk test requires 3-50 values');
    
  SortedData := Copy(Data);
  Sort(SortedData);
  
  // Calculate weights (approximation)
  SetLength(Weights, N div 2);
  for I := 0 to High(Weights) do
    Weights[I] := 0.7071 * (1 - (2 * I) / (N - 1));
  
  DataMean := Mean(SortedData);
  S2 := 0;
  for I := 0 to High(SortedData) do
    S2 := S2 + Sqr(SortedData[I] - DataMean);
    
  // Calculate B using weights
  B := 0;
  for I := 0 to (N div 2) - 1 do
    B := B + Weights[I] * (SortedData[N - 1 - I] - SortedData[I]);
  
  B := Sqr(B);
  if S2 = 0 then
    W := 0
  else
    W := B / S2;
    
  Result := Max(0, Min(1, W));  // Ensure W is between 0 and 1
  
  // Approximate p-value
  if W >= 1 then
    WPValue := 1
  else
    WPValue := Exp(-0.5 * Ln(1 - W) * (5.0 + N));
end;

class function TStatsKit.CohensD(const X, Y: TDoubleArray): Double;
var
  MeanX, MeanY, SDX, SDY: Double;
  PooledSD: Double;
begin
  if (Length(X) < 2) or (Length(Y) < 2) then
    raise Exception.Create('Cohen''s d requires at least 2 values in each group');
    
  MeanX := Mean(X);
  MeanY := Mean(Y);
  SDX := StandardDeviation(X);
  SDY := StandardDeviation(Y);
  
  // Pooled standard deviation
  PooledSD := Sqrt(((Length(X) - 1) * Sqr(SDX) + (Length(Y) - 1) * Sqr(SDY)) / 
                   (Length(X) + Length(Y) - 2));
                   
  Result := (MeanX - MeanY) / PooledSD;
end;

class function TStatsKit.HedgesG(const X, Y: TDoubleArray): Double;
var
  D: Double;
  CorrectionFactor: Double;
begin
  D := CohensD(X, Y);
  
  // Small sample size correction factor
  CorrectionFactor := 1 - (3 / (4 * (Length(X) + Length(Y) - 2) - 1));
  Result := D * CorrectionFactor;
end;

class function TStatsKit.SignTest(const X, Y: TDoubleArray): Double;
var
  I, NPos, NNeg, N: Integer;
begin
  if Length(X) <> Length(Y) then
    raise Exception.Create('Sign test requires equal length arrays');
    
  NPos := 0;
  NNeg := 0;
  
  for I := 0 to High(X) do
  begin
    if X[I] > Y[I] then
      Inc(NPos)
    else if X[I] < Y[I] then
      Inc(NNeg);
  end;
  
  N := NPos + NNeg;
  if N = 0 then
    raise Exception.Create('Sign test requires at least one non-zero difference');
    
  // Return proportion of positive differences
  Result := NPos / N;
end;

class function TStatsKit.WilcoxonSignedRank(const X, Y: TDoubleArray): Double;
var
  I, J, N: Integer;
  Differences: array of record
    Value: Double;
    AbsValue: Double;
    Rank: Double;
  end;
  RankSum: Double;
  TieCount: Integer;
  TieSum: Double;
  AverageRank: Double;
  TempValue: Double;
  TempAbsValue: Double;
  TempRank: Double;
begin
  if Length(X) <> Length(Y) then
    raise Exception.Create('Wilcoxon test requires equal length arrays');
    
  // Calculate differences and absolute values
  N := 0;
  SetLength(Differences, Length(X));
  for I := 0 to High(X) do
  begin
    if X[I] <> Y[I] then
    begin
      Differences[N].Value := X[I] - Y[I];
      Differences[N].AbsValue := Abs(Differences[N].Value);
      Differences[N].Rank := 0;  // Initialize rank
      Inc(N);
    end;
  end;
  SetLength(Differences, N);
  
  if N = 0 then
    raise Exception.Create('Wilcoxon test requires at least one non-zero difference');
    
  // Sort by absolute value
  for I := 0 to N - 2 do
    for J := I + 1 to N - 1 do
      if Differences[J].AbsValue < Differences[I].AbsValue then
      begin
        // Store temporary values
        TempValue := Differences[I].Value;
        TempAbsValue := Differences[I].AbsValue;
        TempRank := Differences[I].Rank;
        
        // Copy J to I
        Differences[I].Value := Differences[J].Value;
        Differences[I].AbsValue := Differences[J].AbsValue;
        Differences[I].Rank := Differences[J].Rank;
        
        // Copy temp to J
        Differences[J].Value := TempValue;
        Differences[J].AbsValue := TempAbsValue;
        Differences[J].Rank := TempRank;
      end;
      
  // Assign ranks (handling ties)
  I := 0;
  while I < N do
  begin
    TieCount := 1;
    TieSum := I + 1;
    
    while (I < N - 1) and (Differences[I + 1].AbsValue = Differences[I].AbsValue) do
    begin
      Inc(I);
      Inc(TieCount);
      TieSum := TieSum + I + 1;
    end;
    
    AverageRank := TieSum / TieCount;
    for J := I - TieCount + 1 to I do
      Differences[J].Rank := AverageRank;
      
    Inc(I);
  end;
  
  // Calculate rank sum for positive differences
  RankSum := 0;
  for I := 0 to N - 1 do
    if Differences[I].Value > 0 then
      RankSum := RankSum + Differences[I].Rank;
      
  Result := RankSum;
end;

class function TStatsKit.KendallTau(const X, Y: TDoubleArray): Double;
var
  I, J: Integer;
  Concordant, Discordant: Integer;
begin
  if Length(X) <> Length(Y) then
    raise Exception.Create('Kendall''s tau requires equal length arrays');
    
  Concordant := 0;
  Discordant := 0;
  
  for I := 0 to High(X) - 1 do
    for J := I + 1 to High(X) do
    begin
      if ((X[I] < X[J]) and (Y[I] < Y[J])) or
         ((X[I] > X[J]) and (Y[I] > Y[J])) then
        Inc(Concordant)
      else if ((X[I] < X[J]) and (Y[I] > Y[J])) or
              ((X[I] > X[J]) and (Y[I] < Y[J])) then
        Inc(Discordant);
    end;
    
  if (Concordant = 0) and (Discordant = 0) then
    Result := 0
  else
    Result := (Concordant - Discordant) / Sqrt((Concordant + Discordant) * (Concordant + Discordant));
end;

class function TStatsKit.Quantile(const Data: TDoubleArray; const Q: Double): Double;
var
  SortedData: TDoubleArray;
  N: Integer;
  Position: Double;
  Index: Integer;
  Fraction: Double;
begin
  if (Q < 0) or (Q > 1) then
    raise Exception.Create('Quantile must be between 0 and 1');
    
  N := Length(Data);
  if N = 0 then
    raise Exception.Create('Cannot calculate quantile of empty array');
    
  SortedData := Copy(Data);
  Sort(SortedData);
  
  Position := Q * (N - 1);
  Index := Trunc(Position);
  Fraction := Frac(Position);
  
  if Index = High(SortedData) then
    Result := SortedData[Index]
  else
    Result := SortedData[Index] + Fraction * (SortedData[Index + 1] - SortedData[Index]);
end;

{ TDescriptiveStats }

function TDescriptiveStats.ToString: string;
begin
  Result := 'Descriptive Statistics' + LineEnding +
            '======================' + LineEnding +
            Format('N: %d', [N]) + LineEnding +
            'Central Tendency:' + LineEnding +
            Format('  Mean: %.6f', [Mean]) + LineEnding +
            Format('  Median: %.6f', [Median]) + LineEnding +
            Format('  Mode: %.6f', [Mode]) + LineEnding +
            'Dispersion:' + LineEnding +
            Format('  Range: %.6f', [Range]) + LineEnding +
            Format('  Variance: %.6f', [Variance]) + LineEnding +
            Format('  StdDev: %.6f', [StdDev]) + LineEnding +
            Format('  SEM: %.6f', [SEM]) + LineEnding +
            Format('  CV: %.2f%%', [CV]) + LineEnding +
            'Distribution Shape:' + LineEnding +
            Format('  Skewness: %.6f', [Skewness]) + LineEnding +
            Format('  Kurtosis: %.6f', [Kurtosis]) + LineEnding +
            'Quartiles:' + LineEnding +
            Format('  Min (0%%): %.6f', [Min]) + LineEnding +
            Format('  Q1 (25%%): %.6f', [Q1]) + LineEnding +
            Format('  Q2 (50%%): %.6f', [Median]) + LineEnding +
            Format('  Q3 (75%%): %.6f', [Q3]) + LineEnding +
            Format('  Max (100%%): %.6f', [Max]) + LineEnding +
            Format('  IQR: %.6f', [IQR]);
end;

function TDescriptiveStats.ToStringWide: string;
const
  // Column widths for alignment
  COL_WIDTH = 12;
  
  function PadCenter(const S: string; Width: Integer): string;
  var
    PadLeft, PadRight: Integer;
  begin
    if Length(S) >= Width then
      Result := S
    else
    begin
      PadLeft := (Width - Length(S)) div 2;
      PadRight := Width - Length(S) - PadLeft;
      Result := StringOfChar(' ', PadLeft) + S + StringOfChar(' ', PadRight);
    end;
  end;
  
  function FormatValue(const Value: Double): string;
  begin
    Result := PadCenter(Format('%.4f', [Value]), COL_WIDTH);
  end;
  
begin
  Result := 'Descriptive Statistics' + LineEnding +
            '===================' + LineEnding +
            'N' + StringOfChar(' ', COL_WIDTH - 1) + '|' +
            PadCenter('Mean', COL_WIDTH) + '|' +
            PadCenter('Median', COL_WIDTH) + '|' +
            PadCenter('StdDev', COL_WIDTH) + '|' +
            PadCenter('SEM', COL_WIDTH) + '|' +
            PadCenter('CV(%)', COL_WIDTH) + LineEnding +
            StringOfChar('-', 6 * COL_WIDTH + 5) + LineEnding +
            PadCenter(Format('%d', [N]), COL_WIDTH) + '|' +
            FormatValue(Mean) + '|' +
            FormatValue(Median) + '|' +
            FormatValue(StdDev) + '|' +
            FormatValue(SEM) + '|' +
            FormatValue(CV) + LineEnding + LineEnding +
            
            'Shape' + StringOfChar(' ', COL_WIDTH - 5) + '|' +
            PadCenter('Skewness', COL_WIDTH) + '|' +
            PadCenter('Kurtosis', COL_WIDTH) + '|' +
            PadCenter('Range', COL_WIDTH) + '|' +
            PadCenter('IQR', COL_WIDTH) + LineEnding +
            StringOfChar('-', 5 * COL_WIDTH + 4) + LineEnding +
            StringOfChar(' ', COL_WIDTH) + '|' +
            FormatValue(Skewness) + '|' +
            FormatValue(Kurtosis) + '|' +
            FormatValue(Range) + '|' +
            FormatValue(IQR) + LineEnding + LineEnding +
            
            'Quantiles' + StringOfChar(' ', COL_WIDTH - 9) + '|' +
            PadCenter('Min', COL_WIDTH) + '|' +
            PadCenter('Q1', COL_WIDTH) + '|' +
            PadCenter('Q2', COL_WIDTH) + '|' +
            PadCenter('Q3', COL_WIDTH) + '|' +
            PadCenter('Max', COL_WIDTH) + LineEnding +
            StringOfChar('-', 6 * COL_WIDTH + 5) + LineEnding +
            StringOfChar(' ', COL_WIDTH) + '|' +
            FormatValue(Min) + '|' +
            FormatValue(Q1) + '|' +
            FormatValue(Median) + '|' +
            FormatValue(Q3) + '|' +
            FormatValue(Max);
end;

end. 
