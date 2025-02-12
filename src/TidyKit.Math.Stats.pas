unit TidyKit.Math.Stats;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, Math, TidyKit.Math;

type
  { Statistics calculations class }
  generic TStatsKit<T> = class
  public
    { Basic statistics }
    class function Mean(const Data: TArray<T>): T; static;
    class function Median(const Data: TArray<T>): T; static;
    class function Mode(const Data: TArray<T>): T; static;
    class function Range(const Data: TArray<T>): T; static;
    
    { Variance and standard deviation }
    class function Variance(const Data: TArray<T>): T; static;
    class function StandardDeviation(const Data: TArray<T>): T; static;
    class function SampleVariance(const Data: TArray<T>): T; static;
    class function SampleStandardDeviation(const Data: TArray<T>): T; static;
    
    { Distribution measures }
    class function Skewness(const Data: TArray<T>): T; static;
    class function Kurtosis(const Data: TArray<T>): T; static;
    
    { Percentiles and quartiles }
    class function Percentile(const Data: TArray<T>; const P: T): T; static;
    class function Quartile1(const Data: TArray<T>): T; static;
    class function Quartile3(const Data: TArray<T>): T; static;
    class function InterquartileRange(const Data: TArray<T>): T; static;
    
    { Correlation and covariance }
    class function Correlation(const X, Y: TArray<T>): T; static;
    class function Covariance(const X, Y: TArray<T>): T; static;
    
    { Z-score and normalization }
    class function ZScore(const Value, Mean, StdDev: T): T; static;
    class procedure Standardize(var Data: TArray<T>); static;
    
    { Helper functions }
    class function Sum(const Data: TArray<T>): T; static;
    class function SumOfSquares(const Data: TArray<T>): T; static;
    class procedure Sort(var Data: TArray<T>); static;
  end;

implementation

{ TStatsKit }

class function TStatsKit.Mean(const Data: TArray<T>): T;
begin
  if Length(Data) = 0 then
    raise Exception.Create('Cannot calculate mean of empty array');
  Result := Sum(Data) / Length(Data);
end;

class function TStatsKit.Median(const Data: TArray<T>): T;
var
  SortedData: TArray<T>;
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

class function TStatsKit.Mode(const Data: TArray<T>): T;
var
  SortedData: TArray<T>;
  I, MaxCount, CurrentCount: Integer;
  CurrentValue: T;
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

class function TStatsKit.Range(const Data: TArray<T>): T;
var
  SortedData: TArray<T>;
begin
  if Length(Data) = 0 then
    raise Exception.Create('Cannot calculate range of empty array');
    
  SortedData := Copy(Data);
  Sort(SortedData);
  Result := SortedData[High(SortedData)] - SortedData[0];
end;

class function TStatsKit.Variance(const Data: TArray<T>): T;
var
  M: T;
begin
  if Length(Data) = 0 then
    raise Exception.Create('Cannot calculate variance of empty array');
    
  M := Mean(Data);
  Result := SumOfSquares(Data) / Length(Data) - Sqr(M);
end;

class function TStatsKit.StandardDeviation(const Data: TArray<T>): T;
begin
  Result := Sqrt(Variance(Data));
end;

class function TStatsKit.SampleVariance(const Data: TArray<T>): T;
var
  M: T;
  N: Integer;
begin
  N := Length(Data);
  if N < 2 then
    raise Exception.Create('Cannot calculate sample variance with less than 2 values');
    
  M := Mean(Data);
  Result := (SumOfSquares(Data) - N * Sqr(M)) / (N - 1);
end;

class function TStatsKit.SampleStandardDeviation(const Data: TArray<T>): T;
begin
  Result := Sqrt(SampleVariance(Data));
end;

class function TStatsKit.Skewness(const Data: TArray<T>): T;
var
  M, S: T;
  I: Integer;
  N: Integer;
  Sum: T;
begin
  N := Length(Data);
  if N < 3 then
    raise Exception.Create('Cannot calculate skewness with less than 3 values');
    
  M := Mean(Data);
  S := StandardDeviation(Data);
  
  Sum := 0;
  for I := 0 to High(Data) do
    Sum := Sum + Power((Data[I] - M) / S, 3);
    
  Result := Sum / N;
end;

class function TStatsKit.Kurtosis(const Data: TArray<T>): T;
var
  M, S: T;
  I: Integer;
  N: Integer;
  Sum: T;
begin
  N := Length(Data);
  if N < 4 then
    raise Exception.Create('Cannot calculate kurtosis with less than 4 values');
    
  M := Mean(Data);
  S := StandardDeviation(Data);
  
  Sum := 0;
  for I := 0 to High(Data) do
    Sum := Sum + Power((Data[I] - M) / S, 4);
    
  Result := Sum / N - 3;  // Excess kurtosis (normal distribution = 0)
end;

class function TStatsKit.Percentile(const Data: TArray<T>; const P: T): T;
var
  SortedData: TArray<T>;
  N: Integer;
  Position: T;
  Index: Integer;
  Fraction: T;
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

class function TStatsKit.Quartile1(const Data: TArray<T>): T;
begin
  Result := Percentile(Data, 25);
end;

class function TStatsKit.Quartile3(const Data: TArray<T>): T;
begin
  Result := Percentile(Data, 75);
end;

class function TStatsKit.InterquartileRange(const Data: TArray<T>): T;
begin
  Result := Quartile3(Data) - Quartile1(Data);
end;

class function TStatsKit.Correlation(const X, Y: TArray<T>): T;
var
  N: Integer;
  MeanX, MeanY, StdDevX, StdDevY: T;
  I: Integer;
  Sum: T;
begin
  N := Length(X);
  if (N <> Length(Y)) or (N < 2) then
    raise Exception.Create('Arrays must have equal length and at least 2 elements');
    
  MeanX := Mean(X);
  MeanY := Mean(Y);
  StdDevX := StandardDeviation(X);
  StdDevY := StandardDeviation(Y);
  
  Sum := 0;
  for I := 0 to High(X) do
    Sum := Sum + ((X[I] - MeanX) / StdDevX) * ((Y[I] - MeanY) / StdDevY);
    
  Result := Sum / (N - 1);
end;

class function TStatsKit.Covariance(const X, Y: TArray<T>): T;
var
  N: Integer;
  MeanX, MeanY: T;
  I: Integer;
  Sum: T;
begin
  N := Length(X);
  if (N <> Length(Y)) or (N < 2) then
    raise Exception.Create('Arrays must have equal length and at least 2 elements');
    
  MeanX := Mean(X);
  MeanY := Mean(Y);
  
  Sum := 0;
  for I := 0 to High(X) do
    Sum := Sum + (X[I] - MeanX) * (Y[I] - MeanY);
    
  Result := Sum / (N - 1);
end;

class function TStatsKit.ZScore(const Value, Mean, StdDev: T): T;
begin
  if StdDev = 0 then
    raise Exception.Create('Standard deviation cannot be zero');
  Result := (Value - Mean) / StdDev;
end;

class procedure TStatsKit.Standardize(var Data: TArray<T>);
var
  M, S: T;
  I: Integer;
begin
  M := Mean(Data);
  S := StandardDeviation(Data);
  
  if S = 0 then
    raise Exception.Create('Cannot standardize data with zero standard deviation');
    
  for I := 0 to High(Data) do
    Data[I] := ZScore(Data[I], M, S);
end;

class function TStatsKit.Sum(const Data: TArray<T>): T;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(Data) do
    Result := Result + Data[I];
end;

class function TStatsKit.SumOfSquares(const Data: TArray<T>): T;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(Data) do
    Result := Result + Sqr(Data[I]);
end;

class procedure TStatsKit.Sort(var Data: TArray<T>);
var
  I, J: Integer;
  Temp: T;
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