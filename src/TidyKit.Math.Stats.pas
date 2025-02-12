unit TidyKit.Math.Stats;

{$mode objfpc}{$H+}{$J-}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Math, TidyKit.Math;

type


  { Statistics calculations class }
  TStatsKit = class
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
    class function Correlation(const X, Y: TDoubleArray): Double; static;
    class function Covariance(const X, Y: TDoubleArray): Double; static;
    
    { Z-score and normalization }
    class function ZScore(const Value, AMean, StdDev: Double): Double; static;
    class procedure Standardize(var Data: TDoubleArray); static;
    
    { Helper functions }
    class function Sum(const Data: TDoubleArray): Double; static;
    class function SumOfSquares(const Data: TDoubleArray): Double; static;
    class procedure Sort(var Data: TDoubleArray); static;
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
  M: Double;
begin
  if Length(Data) = 0 then
    raise Exception.Create('Cannot calculate variance of empty array');
    
  M := Mean(Data);
  Result := SumOfSquares(Data) / Length(Data) - Sqr(M);
end;

class function TStatsKit.StandardDeviation(const Data: TDoubleArray): Double;
begin
  Result := Sqrt(Variance(Data));
end;

class function TStatsKit.SampleVariance(const Data: TDoubleArray): Double;
var
  M: Double;
  N: Integer;
begin
  N := Length(Data);
  if N < 2 then
    raise Exception.Create('Cannot calculate sample variance with less than 2 values');
    
  M := Mean(Data);
  Result := (SumOfSquares(Data) - N * Sqr(M)) / (N - 1);
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
  S := StandardDeviation(Data);
  
  Total := 0;
  for I := 0 to High(Data) do
    Total := Total + Power((Data[I] - M) / S, 4);
    
  Result := Total / N - 3;  // Excess kurtosis (normal distribution = 0)
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

class function TStatsKit.Correlation(const X, Y: TDoubleArray): Double;
var
  N: Integer;
  MeanX, MeanY, StdDevX, StdDevY: Double;
  I: Integer;
  Total: Double;
begin
  N := Length(X);
  if (N <> Length(Y)) or (N < 2) then
    raise Exception.Create('Arrays must have equal length and at least 2 elements');
    
  MeanX := Mean(X);
  MeanY := Mean(Y);
  StdDevX := StandardDeviation(X);
  StdDevY := StandardDeviation(Y);
  
  Total := 0;
  for I := 0 to High(X) do
    Total := Total + ((X[I] - MeanX) / StdDevX) * ((Y[I] - MeanY) / StdDevY);
    
  Result := Total / (N - 1);
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

end. 
