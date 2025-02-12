unit TidyKit.Math.Finance;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, Math, TidyKit.Math;

type
  { Financial calculations class }
  TFinanceKit = class
  public
    { Present Value calculations
      
      Present Value (PV) represents the current worth of a future sum of money.
      Formula: PV = FV / (1 + r)^n
      where:
      - FV = Future Value
      - r = Interest rate per period
      - n = Number of periods
      
      Source: CFA Institute's Financial Mathematics }
    class function PresentValue(const AFutureValue, ARate: Double; const APeriods: Integer; const ADecimals: Integer = 4): Double; static;
    
    { Future Value represents the value of a present sum at a future date.
      Formula: FV = PV * (1 + r)^n
      where:
      - PV = Present Value
      - r = Interest rate per period
      - n = Number of periods
      
      Source: CFA Institute's Financial Mathematics }
    class function FutureValue(const APresentValue, ARate: Double; const APeriods: Integer; const ADecimals: Integer = 4): Double; static;
    
    { Compound Interest calculations
      
      Compound Interest is the interest earned on both initial principal and accumulated interest.
      Formula: CI = P * ((1 + r)^n - 1)
      where:
      - P = Principal amount
      - r = Interest rate per period
      - n = Number of periods
      
      Source: Financial Mathematics for Actuaries }
    class function CompoundInterest(const APrincipal, ARate: Double; const APeriods: Integer; const ADecimals: Integer = 4): Double; static;
    
    { Payment calculation for loans and annuities
      
      Calculates the periodic payment required to amortize a loan.
      Formula: PMT = PV * r * (1 + r)^n / ((1 + r)^n - 1)
      where:
      - PV = Present Value (loan amount)
      - r = Interest rate per period
      - n = Total number of payments
      
      This formula assumes:
      - Payments are made at the end of each period
      - Interest rate remains constant
      - All payments are equal
      
      Source: Financial Mathematics for Actuaries }
    class function Payment(const APresentValue, ARate: Double; const APeriods: Integer; const ADecimals: Integer = 4): Double; static;
    
    { Net Present Value (NPV) calculation
      
      NPV is the difference between the present value of cash inflows and outflows.
      Formula: NPV = -I + Σ(CFt / (1+r)^t)
      where:
      - I = Initial investment
      - CFt = Cash flow at time t
      - r = Discount rate
      - t = Time period
      
      A positive NPV indicates a profitable investment.
      
      Source: Corporate Finance Institute }
    class function NetPresentValue(const AInitialInvestment: Double; const ACashFlows: TDoubleArray; const Rate: Double; const ADecimals: Integer = 4): Double; static;
    
    { Internal Rate of Return (IRR) calculation
      
      IRR is the discount rate that makes NPV = 0.
      Found by solving: 0 = -I + Σ(CFt / (1+IRR)^t)
      
      Implementation uses the Secant Method with dampening:
      1. Start with initial guess
      2. Use secant method: r_new = r - NPV * (r - r_last)/(NPV - NPV_last)
      3. Apply dampening to improve convergence
      4. Repeat until |NPV| < tolerance
      
      Source: Financial Mathematics for Actuaries }
    class function InternalRateOfReturn(const AInitialInvestment: Double; const ACashFlows: TDoubleArray; const ADecimals: Integer = 4): Double; static;
    
    { Depreciation calculations
      
      Straight-Line Depreciation:
      Annual depreciation = (Cost - Salvage) / Life
      
      Source: IFRS IAS 16 }
    class function StraightLineDepreciation(const ACost, ASalvage: Double; const ALife: Integer; const ADecimals: Integer = 4): Double; static;
    
    { Double Declining Balance Depreciation
      
      A form of accelerated depreciation using twice the straight-line rate.
      Formula: Dep = Cost * Rate * (1-Rate)^(period-1)
      where:
      - Rate = 2/Life (twice straight-line rate)
      - Cost = Initial asset cost
      - Period = Current period
      
      Source: IFRS IAS 16 }
    class function DecliningBalanceDepreciation(const ACost, ASalvage: Double; const ALife: Integer; const APeriod: Integer; const ADecimals: Integer = 4): Double; static;
    
    { Return calculations
      
      ROI = (Gain - Cost) / Cost
      Measures the profitability of an investment.
      
      Source: Corporate Finance Institute }
    class function ReturnOnInvestment(const AGain, ACost: Double; const ADecimals: Integer = 4): Double; static;
    
    { Return on Equity (ROE)
      
      ROE = Net Income / Shareholders' Equity
      Measures a company's profitability in relation to shareholders' equity.
      
      Source: Corporate Finance Institute }
    class function ReturnOnEquity(const ANetIncome, AShareholdersEquity: Double; const ADecimals: Integer = 4): Double; static;
  end;

implementation

{ TFinanceKit - Financial Mathematics Implementation

  This unit implements standard financial mathematics formulas based on authoritative sources:
  - CFA Institute's Financial Mathematics
  - Financial Mathematics for Actuaries (2nd Edition)
  - Corporate Finance Institute's Financial Modeling
  - IFRS IAS 16 Standard for Asset Depreciation
  
  All calculations use Double precision and include appropriate rounding for financial reporting.
  Time value of money calculations assume:
  - End of period payments
  - Constant interest rate
  - No transaction costs
}

{ TFinanceKit }

class function TFinanceKit.PresentValue(const AFutureValue, ARate: Double; const APeriods: Integer; const ADecimals: Integer = 4): Double;
begin
  if APeriods < 0 then
    raise Exception.Create('Number of periods must be non-negative');
  if Abs(ARate) < 1E-10 then
    Result := AFutureValue
  else
    Result := SimpleRoundTo(AFutureValue / Power(1 + ARate, APeriods), -ADecimals);  // Round to ADecimals decimals for consistency
end;

class function TFinanceKit.FutureValue(const APresentValue, ARate: Double; const APeriods: Integer; const ADecimals: Integer = 4): Double;
begin
  Result := SimpleRoundTo(APresentValue * Power(1 + ARate, APeriods), -ADecimals);  // Use ADecimals decimals with bankers' rounding
end;

class function TFinanceKit.CompoundInterest(const APrincipal, ARate: Double; const APeriods: Integer; const ADecimals: Integer = 4): Double;
begin
  Result := SimpleRoundTo(APrincipal * (Power(1 + ARate, APeriods) - 1), -ADecimals);  // Use ADecimals decimals with bankers' rounding
end;

class function TFinanceKit.Payment(const APresentValue, ARate: Double; const APeriods: Integer; const ADecimals: Integer = 4): Double;
var
  Numerator, Denominator, PowerTerm: Double;
begin
  if APeriods <= 0 then
    raise Exception.Create('Number of periods must be positive');
    
  if Abs(ARate) < 1E-10 then
    Result := SimpleRoundTo(APresentValue / APeriods, -ADecimals)  // Round to ADecimals decimals
  else
  begin
    // PMT = PV * r * (1 + r)^n / ((1 + r)^n - 1)
    // Calculate each part separately to maintain precision
    PowerTerm := Math.Power(1 + ARate, APeriods);
    Numerator := APresentValue * ARate * PowerTerm;
    Denominator := PowerTerm - 1;
    Result := SimpleRoundTo(Numerator / Denominator, -ADecimals);
  end;
end;

class function TFinanceKit.NetPresentValue(const AInitialInvestment: Double; const ACashFlows: TDoubleArray; const Rate: Double; const ADecimals: Integer = 4): Double;
var
  I: Integer;
  NPV, Divisor: Double;
begin
  if Length(ACashFlows) = 0 then
    raise Exception.Create('Cash flows array cannot be empty');
    
  // NPV = -Initial + Σ(CFt / (1+r)^t)
  NPV := -AInitialInvestment;
  
  // Calculate each cash flow's present value
  for I := 0 to High(ACashFlows) do
  begin
    Divisor := Power(1 + Rate, I + 1);
    NPV := NPV + SimpleRoundTo(ACashFlows[I] / Divisor, -ADecimals);
  end;
  
  Result := SimpleRoundTo(NPV, -ADecimals);
end;

class function TFinanceKit.InternalRateOfReturn(const AInitialInvestment: Double; const ACashFlows: TDoubleArray; const ADecimals: Integer = 4): Double;
const
  TOLERANCE = 1E-6;
  MAX_ITERATIONS = 100;
  MIN_RATE = -0.999999;
  MAX_RATE = 1.0;  // Reduced maximum rate
var
  Rate, NPV, LastRate, LastNPV, NewRate: Double;
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
  LastRate := 0.05; // Start with a different rate
  LastNPV := 0;
  Iteration := 0;
  
  repeat
    NPV := -AInitialInvestment;
    for I := 0 to High(ACashFlows) do
      NPV := NPV + ACashFlows[I] / Power(1 + Rate, I + 1);
      
    if Iteration > 0 then
    begin
      // Use secant method with dampening
      if Abs(NPV) < TOLERANCE then
        Break;
        
      if Abs(NPV - LastNPV) < 1E-10 then
        Break;
        
      NewRate := Rate - NPV * (Rate - LastRate) / (NPV - LastNPV);
      
      // Stronger dampening for better stability
      if Abs(NewRate - Rate) > 0.001 then
        NewRate := Rate + 0.001 * Sign(NewRate - Rate);
        
      LastRate := Rate;
      Rate := NewRate;
    end;
    
    LastNPV := NPV;
    Inc(Iteration);
    
    // Keep rate within reasonable bounds
    if Rate < MIN_RATE then
      Rate := MIN_RATE
    else if Rate > MAX_RATE then
      Rate := MAX_RATE;
      
  until (Abs(NPV) < TOLERANCE) or (Iteration >= MAX_ITERATIONS);
  
  if Iteration >= MAX_ITERATIONS then
    raise Exception.Create('IRR calculation did not converge');
    
  Result := SimpleRoundTo(Rate, -ADecimals);  // Round to ADecimals decimals
end;

class function TFinanceKit.StraightLineDepreciation(const ACost, ASalvage: Double; const ALife: Integer; const ADecimals: Integer = 4): Double;
begin
  Result := (ACost - ASalvage) / ALife;
end;

class function TFinanceKit.DecliningBalanceDepreciation(const ACost, ASalvage: Double; const ALife: Integer; const APeriod: Integer; const ADecimals: Integer = 4): Double;
var
  Rate: Double;
begin
  if (ALife <= 0) or (APeriod <= 0) or (APeriod > ALife) then
    raise Exception.Create('Invalid life or period parameters');
  if ACost <= ASalvage then
    raise Exception.Create('Cost must be greater than salvage value');
    
  Rate := 2.0 / ALife;  // Double declining balance rate
  Result := SimpleRoundTo(ACost * Rate * Power(1 - Rate, APeriod - 1), -ADecimals);  // Round to ADecimals decimals
end;

class function TFinanceKit.ReturnOnInvestment(const AGain, ACost: Double; const ADecimals: Integer = 4): Double;
begin
  Result := (AGain - ACost) / ACost;
end;

class function TFinanceKit.ReturnOnEquity(const ANetIncome, AShareholdersEquity: Double; const ADecimals: Integer = 4): Double;
begin
  Result := ANetIncome / AShareholdersEquity;
end;

end. 
