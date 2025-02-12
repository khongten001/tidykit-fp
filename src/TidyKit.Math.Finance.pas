unit TidyKit.Math.Finance;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, Math, TidyKit.Math;

type
  { Financial calculations class }
  TFinanceKit = class
  public
    { Present Value calculations }
    class function PresentValue(const AFutureValue, ARate: Double; const APeriods: Integer): Double; static;
    class function FutureValue(const APresentValue, ARate: Double; const APeriods: Integer): Double; static;
    
    { Interest and payments }
    class function CompoundInterest(const APrincipal, ARate: Double; const APeriods: Integer): Double; static;
    class function Payment(const APresentValue, ARate: Double; const APeriods: Integer): Double; static;
    
    { Investment analysis }
    class function NetPresentValue(const AInitialInvestment: Double; const ACashFlows: TDoubleArray; const Rate: Double): Double; static;
    class function InternalRateOfReturn(const AInitialInvestment: Double; const ACashFlows: TDoubleArray): Double; static;
    
    { Depreciation }
    class function StraightLineDepreciation(const ACost, ASalvage: Double; const ALife: Integer): Double; static;
    class function DecliningBalanceDepreciation(const ACost, ASalvage: Double; const ALife: Integer; const APeriod: Integer): Double; static;
    
    { Return calculations }
    class function ReturnOnInvestment(const AGain, ACost: Double): Double; static;
    class function ReturnOnEquity(const ANetIncome, AShareholdersEquity: Double): Double; static;
  end;

implementation

{ TFinanceKit }

class function TFinanceKit.PresentValue(const AFutureValue, ARate: Double; const APeriods: Integer): Double;
begin
  if APeriods < 0 then
    raise Exception.Create('Number of periods must be non-negative');
  if Abs(ARate) < 1E-10 then
    Result := AFutureValue
  else
    Result := AFutureValue / Power(1 + ARate, APeriods);
end;

class function TFinanceKit.FutureValue(const APresentValue, ARate: Double; const APeriods: Integer): Double;
begin
  Result := APresentValue * Power(1 + ARate, APeriods);
end;

class function TFinanceKit.CompoundInterest(const APrincipal, ARate: Double; const APeriods: Integer): Double;
begin
  Result := APrincipal * (Power(1 + ARate, APeriods) - 1);
end;

class function TFinanceKit.Payment(const APresentValue, ARate: Double; const APeriods: Integer): Double;
begin
  if APeriods <= 0 then
    raise Exception.Create('Number of periods must be positive');
    
  if Abs(ARate) < 1E-10 then
    Result := APresentValue / APeriods
  else
    Result := (APresentValue * ARate) / (1 - Power(1 + ARate, -APeriods));
end;

class function TFinanceKit.NetPresentValue(const AInitialInvestment: Double; const ACashFlows: TDoubleArray; const Rate: Double): Double;
var
  I: Integer;
begin
  if Length(ACashFlows) = 0 then
    raise Exception.Create('Cash flows array cannot be empty');
    
  Result := -AInitialInvestment;
  for I := 0 to High(ACashFlows) do
    Result := Result + ACashFlows[I] / Power(1 + Rate, I + 1);
end;

class function TFinanceKit.InternalRateOfReturn(const AInitialInvestment: Double; const ACashFlows: TDoubleArray): Double;
const
  TOLERANCE = 1E-6;
  MAX_ITERATIONS = 1000;
  MIN_RATE = -0.999999;
  MAX_RATE = 100.0;
var
  Rate, NPV, LastRate, LastNPV: Double;
  I, Iteration: Integer;
  HasPositive, HasNegative: Boolean;
begin
  if Length(ACashFlows) = 0 then
    raise Exception.Create('Cash flows array cannot be empty');
    
  // Check if IRR calculation is possible
  HasPositive := False;
  HasNegative := AInitialInvestment > 0;
  for I := 0 to High(ACashFlows) do
  begin
    if ACashFlows[I] > 0 then
      HasPositive := True
    else if ACashFlows[I] < 0 then
      HasNegative := True;
  end;
  
  if not (HasPositive and HasNegative) then
    raise Exception.Create('Cash flows must have both positive and negative values for IRR calculation');
    
  Rate := 0.1; // Initial guess
  LastRate := 0.0;
  Iteration := 0;
  
  repeat
    LastNPV := NPV;
    LastRate := Rate;
    
    NPV := -AInitialInvestment;
    for I := 0 to High(ACashFlows) do
      NPV := NPV + ACashFlows[I] / Power(1 + Rate, I + 1);
      
    if Iteration > 0 then
    begin
      // Use secant method for better convergence
      if Abs(NPV - LastNPV) < 1E-10 then
        Break;
      Rate := Rate - NPV * (Rate - LastRate) / (NPV - LastNPV);
    end
    else
    begin
      // Use if-else instead of ternary operator
      if NPV > 0 then
        Rate := Rate + 0.1
      else
        Rate := Rate - 0.1;
    end;
      
    // Keep rate within reasonable bounds
    if Rate < MIN_RATE then
      Rate := MIN_RATE
    else if Rate > MAX_RATE then
      Rate := MAX_RATE;
      
    Inc(Iteration);
    
  until (Abs(NPV) < TOLERANCE) or (Iteration >= MAX_ITERATIONS);
  
  if Iteration >= MAX_ITERATIONS then
    raise Exception.Create('IRR calculation did not converge');
    
  Result := Rate;
end;

class function TFinanceKit.StraightLineDepreciation(const ACost, ASalvage: Double; const ALife: Integer): Double;
begin
  Result := (ACost - ASalvage) / ALife;
end;

class function TFinanceKit.DecliningBalanceDepreciation(const ACost, ASalvage: Double; const ALife: Integer; const APeriod: Integer): Double;
var
  Rate: Double;
begin
  if (ALife <= 0) or (APeriod <= 0) or (APeriod > ALife) then
    raise Exception.Create('Invalid life or period parameters');
  if ACost <= ASalvage then
    raise Exception.Create('Cost must be greater than salvage value');
    
  Rate := 2.0 / ALife;  // Double declining balance rate
  Result := ACost * Rate * Power(1 - Rate, APeriod - 1);
end;

class function TFinanceKit.ReturnOnInvestment(const AGain, ACost: Double): Double;
begin
  Result := (AGain - ACost) / ACost;
end;

class function TFinanceKit.ReturnOnEquity(const ANetIncome, AShareholdersEquity: Double): Double;
begin
  Result := ANetIncome / AShareholdersEquity;
end;

end. 
