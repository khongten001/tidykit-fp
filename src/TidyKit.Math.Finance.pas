unit TidyKit.Math.Finance;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, Math, TidyKit.Math;

type
  { Option type for Black-Scholes model }
  TOptionType = (otCall, otPut);

  { Working Capital Ratios }
  TWorkingCapitalRatios = record
    CurrentRatio: Double;           // Current Assets / Current Liabilities
    QuickRatio: Double;            // (Current Assets - Inventory) / Current Liabilities
    CashRatio: Double;             // Cash / Current Liabilities
    WorkingCapitalTurnover: Double; // Sales / Working Capital
  end;

  { Financial Leverage Ratios }
  TLeverageRatios = record
    DebtRatio: Double;              // Total Debt / Total Assets
    DebtToEquityRatio: Double;      // Total Debt / Total Equity
    EquityMultiplier: Double;       // Total Assets / Total Equity
    TimesInterestEarned: Double;    // EBIT / Interest Expense
  end;

  { Risk Metrics }
  TRiskMetrics = record
    SharpeRatio: Double;      // (Portfolio Return - Risk Free Rate) / Portfolio Standard Deviation
    TreynorRatio: Double;     // (Portfolio Return - Risk Free Rate) / Portfolio Beta
    JensenAlpha: Double;      // Actual Return - CAPM Expected Return
    InformationRatio: Double; // (Portfolio Return - Benchmark Return) / Tracking Error
  end;

  { DuPont Analysis }
  TDuPontAnalysis = record
    ProfitMargin: Double;     // Net Income / Sales
    AssetTurnover: Double;    // Sales / Total Assets
    EquityMultiplier: Double; // Total Assets / Total Equity
    ROE: Double;             // Final ROE from DuPont Analysis
  end;

  { Operating Leverage Analysis }
  TOperatingLeverage = record
    DOL: Double;              // Degree of Operating Leverage
    BreakEvenPoint: Double;   // Break-even point in units
    OperatingLeverage: Double; // % Change in EBIT / % Change in Sales
  end;

  { Profitability Ratios }
  TProfitabilityRatios = record
    GrossMargin: Double;           // (Revenue - COGS) / Revenue
    OperatingMargin: Double;       // EBIT / Revenue
    NetProfitMargin: Double;       // Net Income / Revenue
    ROA: Double;                   // Net Income / Total Assets
    ROCE: Double;                  // EBIT / (Total Assets - Current Liabilities)
  end;

  { Financial calculations class }
  TFinanceKit = class
  private
    { Helper function for Black-Scholes calculations }
    class function CumulativeNormal(const X: Double): Double; static;
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
    
    { Bond Price calculation
      
      Calculates the present value of a bond's cash flows.
      Formula: Price = C * [1 - (1+r)^-n]/r + F/(1+r)^n
      where:
      - C = Coupon payment
      - r = Yield rate per period
      - n = Number of periods
      - F = Face value
      
      Source: CFA Institute's Fixed Income Analysis }
    class function BondPrice(const AFaceValue, ACouponRate, AYieldRate: Double;
      const APeriodsPerYear, AYearsToMaturity: Integer; const ADecimals: Integer = 4): Double; static;
    
    { Bond Yield to Maturity
      
      Calculates the yield that makes present value equal to bond price.
      Uses iterative Newton-Raphson method to find the yield.
      
      Source: CFA Institute's Fixed Income Analysis }
    class function BondYieldToMaturity(const ABondPrice, AFaceValue, ACouponRate: Double;
      const APeriodsPerYear, AYearsToMaturity: Integer; const ADecimals: Integer = 4): Double; static;
    
    { Loan Amortization Schedule
      
      Calculates the principal and interest portions of a loan payment.
      Returns array of records containing payment details.
      
      Source: Financial Mathematics for Actuaries }
    type
      TAmortizationPayment = record
        PaymentNumber: Integer;
        Payment: Double;
        Principal: Double;
        Interest: Double;
        RemainingBalance: Double;
      end;
      TAmortizationArray = array of TAmortizationPayment;
      
    class function AmortizationSchedule(const ALoanAmount, ARate: Double; 
      const ANumberOfPayments: Integer; const ADecimals: Integer = 4): TAmortizationArray; static;
      
    { Effective Annual Rate (EAR)
      
      Converts nominal rates to effective annual rates considering compounding frequency.
      Formula: EAR = (1 + r/m)^m - 1
      where:
      - r = Nominal annual rate
      - m = Number of compounding periods per year
      
      Source: CFA Institute's Financial Mathematics }
    class function EffectiveAnnualRate(const ANominalRate: Double; const ACompoundingsPerYear: Integer;
      const ADecimals: Integer = 4): Double; static;
      
    { Modified Duration
      
      Measures bond price sensitivity to interest rate changes.
      Formula: ModDur = MacDur / (1 + y/n)
      where:
      - MacDur = Macaulay Duration
      - y = Yield to maturity
      - n = Number of payments per year
      
      Source: CFA Institute's Fixed Income Analysis }
    class function ModifiedDuration(const AFaceValue, ACouponRate, AYieldRate: Double;
      const APeriodsPerYear, AYearsToMaturity: Integer; const ADecimals: Integer = 4): Double; static;
      
    { Break-Even Analysis
      
      Calculates break-even point in units.
      Formula: BE = FC / (P - VC)
      where:
      - FC = Fixed costs
      - P = Price per unit
      - VC = Variable cost per unit
      
      Source: Corporate Finance Institute }
    class function BreakEvenUnits(const AFixedCosts, APricePerUnit, AVariableCostPerUnit: Double; 
      const ADecimals: Integer = 4): Double; static;
      
    { Break-Even Analysis in Currency
      
      Calculates break-even point in currency.
      Formula: BE = FC / (1 - VC/P)
      where:
      - FC = Fixed costs
      - VC = Variable cost per unit
      - P = Price per unit
      
      Source: Corporate Finance Institute }
    class function BreakEvenRevenue(const AFixedCosts, APricePerUnit, AVariableCostPerUnit: Double; 
      const ADecimals: Integer = 4): Double; static;
    
    { Weighted Average Cost of Capital (WACC)
      
      Calculates company's weighted cost of capital.
      Formula: WACC = (E/V * Re) + (D/V * Rd * (1-T))
      where:
      - E = Market value of equity
      - D = Market value of debt
      - V = Total market value (E + D)
      - Re = Cost of equity
      - Rd = Cost of debt
      - T = Tax rate
      
      Source: Corporate Finance Institute }
    class function WACC(const AEquityValue, ADebtValue, ACostOfEquity, ACostOfDebt, ATaxRate: Double;
      const ADecimals: Integer = 4): Double; static;
      
    { Capital Asset Pricing Model (CAPM)
      
      Calculates expected return of an asset.
      Formula: E(Ri) = Rf + βi(E(Rm) - Rf)
      where:
      - Rf = Risk-free rate
      - βi = Beta of the asset
      - E(Rm) = Expected market return
      
      Source: CFA Institute's Portfolio Management }
    class function CAPM(const ARiskFreeRate, ABeta, AExpectedMarketReturn: Double; 
      const ADecimals: Integer = 4): Double; static;
      
    { Gordon Growth Model (Dividend Discount Model)
      
      Calculates intrinsic value of a stock using constant dividend growth.
      Formula: P = D0(1+g)/(r-g)
      where:
      - D0 = Current dividend
      - g = Growth rate
      - r = Required rate of return
      
      Source: CFA Institute's Equity Analysis }
    class function GordonGrowthModel(const ACurrentDividend, AGrowthRate, ARequiredReturn: Double; 
      const ADecimals: Integer = 4): Double; static;
    
    { Working Capital Analysis
      
      Calculates key working capital ratios for liquidity analysis.
      
      Source: Corporate Finance Institute }
    class function WorkingCapitalRatios(const ACurrentAssets, ACurrentLiabilities, 
      AInventory, ACash, ASales: Double; const ADecimals: Integer = 4): TWorkingCapitalRatios; static;
      
    { Financial Leverage Analysis
      
      Calculates key financial leverage ratios for solvency analysis.
      
      Source: Corporate Finance Institute }
    class function LeverageRatios(const ATotalDebt, ATotalAssets, ATotalEquity,
      AEBIT, AInterestExpense: Double; const ADecimals: Integer = 4): TLeverageRatios; static;
      
    { Black-Scholes Option Pricing Model
      
      Calculates theoretical price of European call/put options.
      Formula for Call: C = S*N(d1) - K*e^(-rT)*N(d2)
      Formula for Put: P = K*e^(-rT)*N(-d2) - S*N(-d1)
      where:
      d1 = (ln(S/K) + (r + σ²/2)T) / (σ√T)
      d2 = d1 - σ√T
      
      Source: Options, Futures, and Other Derivatives by John C. Hull }
    class function BlackScholes(const ASpotPrice, AStrikePrice, ARiskFreeRate, AVolatility, ATimeToMaturity: Double;
      const AOptionType: TOptionType; const ADecimals: Integer = 4): Double; static;
      
    { Risk-Adjusted Performance Metrics
      
      Calculates various risk-adjusted return measures.
      
      Source: CFA Institute's Portfolio Management }
    class function RiskMetrics(const APortfolioReturn, ARiskFreeRate, AMarketReturn, ABeta,
      APortfolioStdDev, ABenchmarkReturn, ATrackingError: Double; const ADecimals: Integer = 4): TRiskMetrics; static;
      
    { DuPont Analysis
      
      Breaks down ROE into its component ratios.
      ROE = (Net Income/Sales) * (Sales/Total Assets) * (Total Assets/Total Equity)
      
      Source: Corporate Finance Institute }
    class function DuPontAnalysis(const ANetIncome, ASales, ATotalAssets, ATotalEquity: Double;
      const ADecimals: Integer = 4): TDuPontAnalysis; static;
      
    { Operating Leverage Analysis
      
      Measures business risk and operating efficiency.
      DOL = (Q * (P-V)) / (Q * (P-V) - F)
      where:
      Q = Quantity
      P = Price per unit
      V = Variable cost per unit
      F = Fixed costs
      
      Source: Financial Management: Theory and Practice }
    class function OperatingLeverage(const AQuantity, APricePerUnit, AVariableCostPerUnit,
      AFixedCosts: Double; const ADecimals: Integer = 4): TOperatingLeverage; static;
      
    { Profitability Analysis
      
      Calculates comprehensive set of profitability ratios.
      
      Source: Corporate Finance Institute }
    class function ProfitabilityRatios(const ARevenue, ACOGS, AEBIT, ANetIncome,
      ATotalAssets, ACurrentLiabilities: Double; const ADecimals: Integer = 4): TProfitabilityRatios; static;
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

class function TFinanceKit.CumulativeNormal(const X: Double): Double;
const
  A1 = 0.254829592;
  A2 = -0.284496736;
  A3 = 1.421413741;
  A4 = -1.453152027;
  A5 = 1.061405429;
  P = 0.3275911;
var
  Sign: Integer;
  T, Z: Double;
begin
  if X < 0 then
  begin
    Sign := -1;
    Z := -X;
  end
  else
  begin
    Sign := 1;
    Z := X;
  end;
  
  T := 1.0 / (1.0 + P * Z);
  Result := 0.5 * (1.0 + Sign * (1.0 - (((((A5 * T + A4) * T) + A3) * T + A2) * T + A1) * T * Exp(-Z * Z / 2.0)));
end;

class function TFinanceKit.PresentValue(const AFutureValue, ARate: Double; const APeriods: Integer; const ADecimals: Integer = 4): Double;
begin
  if APeriods < 0 then
    raise Exception.Create('Number of periods must be non-negative');
  if Abs(ARate) < 1E-10 then
    Result := AFutureValue
  else
    Result := SimpleRoundTo(AFutureValue / Power(1 + ARate, APeriods), -ADecimals);
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
    Result := SimpleRoundTo(APresentValue / APeriods, -ADecimals)
  else
  begin
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

class function TFinanceKit.BondPrice(const AFaceValue, ACouponRate, AYieldRate: Double;
  const APeriodsPerYear, AYearsToMaturity: Integer; const ADecimals: Integer = 4): Double;
var
  TotalPeriods: Integer;
  CouponPayment, PVCoupons, PVPrincipal: Double;
  YieldPerPeriod: Double;
  I: Integer;
begin
  if (APeriodsPerYear <= 0) or (AYearsToMaturity < 0) then
    raise Exception.Create('Invalid periods or maturity parameters');
    
  TotalPeriods := APeriodsPerYear * AYearsToMaturity;
  CouponPayment := (AFaceValue * ACouponRate) / APeriodsPerYear;
  YieldPerPeriod := AYieldRate / APeriodsPerYear;
  
  // Present value of coupon payments
  PVCoupons := 0;
  for I := 1 to TotalPeriods do
    PVCoupons := PVCoupons + CouponPayment / Power(1 + YieldPerPeriod, I);
  
  // Present value of principal
  PVPrincipal := AFaceValue / Power(1 + YieldPerPeriod, TotalPeriods);
  
  Result := SimpleRoundTo(PVCoupons + PVPrincipal, -ADecimals);
end;

class function TFinanceKit.BondYieldToMaturity(const ABondPrice, AFaceValue, ACouponRate: Double;
  const APeriodsPerYear, AYearsToMaturity: Integer; const ADecimals: Integer = 4): Double;
const
  MAX_ITERATIONS = 100;
  TOLERANCE = 1E-10;
var
  Yield, LastYield, Price, Derivative: Double;
  Iteration: Integer;
  TotalPeriods: Integer;
  CouponPayment: Double;
  YieldPerPeriod: Double;
  PriceDiff: Double;
  InitialGuess: Double;
begin
  if (APeriodsPerYear <= 0) or (AYearsToMaturity < 0) then
    raise Exception.Create('Invalid periods or maturity parameters');
    
  TotalPeriods := APeriodsPerYear * AYearsToMaturity;
  CouponPayment := (AFaceValue * ACouponRate) / APeriodsPerYear;
  
  // Better initial guess using current yield
  InitialGuess := (CouponPayment * APeriodsPerYear) / ABondPrice;
  if ABondPrice < AFaceValue then
    InitialGuess := InitialGuess + (AFaceValue - ABondPrice) / (ABondPrice * AYearsToMaturity)
  else if ABondPrice > AFaceValue then
    InitialGuess := InitialGuess - (ABondPrice - AFaceValue) / (ABondPrice * AYearsToMaturity);
    
  Yield := InitialGuess;
  Iteration := 0;
  
  repeat
    LastYield := Yield;
    YieldPerPeriod := Yield / APeriodsPerYear;
    
    // Calculate price at current yield
    Price := BondPrice(AFaceValue, ACouponRate, Yield, APeriodsPerYear, AYearsToMaturity);
    PriceDiff := Price - ABondPrice;
    
    // Calculate derivative (price sensitivity)
    Derivative := -TotalPeriods * Price / (1 + YieldPerPeriod);
    
    // Newton-Raphson iteration with dampening
    if Abs(Derivative) > 1E-10 then
    begin
      Yield := LastYield - PriceDiff / Derivative;
      // Apply dampening
      if Abs(Yield - LastYield) > 0.01 then
        Yield := LastYield + 0.01 * Sign(Yield - LastYield);
    end
    else
      Break;
    
    // Ensure yield stays reasonable
    if Yield < -0.99 then Yield := -0.99
    else if Yield > 1.0 then Yield := 1.0;
    
    Inc(Iteration);
  until (Abs(PriceDiff) < TOLERANCE) or (Iteration >= MAX_ITERATIONS);
  
  if Iteration >= MAX_ITERATIONS then
    raise Exception.Create('Yield calculation did not converge');
    
  Result := SimpleRoundTo(Yield, -ADecimals);
end;

class function TFinanceKit.AmortizationSchedule(const ALoanAmount, ARate: Double;
  const ANumberOfPayments: Integer; const ADecimals: Integer = 4): TAmortizationArray;
var
  I: Integer;
  Balance, PaymentAmount, InterestPayment, PrincipalPayment: Double;
begin
  if ANumberOfPayments <= 0 then
    raise Exception.Create('Number of payments must be positive');
    
  SetLength(Result, ANumberOfPayments);
  Balance := ALoanAmount;
  PaymentAmount := TFinanceKit.Payment(ALoanAmount, ARate, ANumberOfPayments);
  
  for I := 0 to ANumberOfPayments - 1 do
  begin
    InterestPayment := SimpleRoundTo(Balance * ARate, -ADecimals);
    PrincipalPayment := SimpleRoundTo(PaymentAmount - InterestPayment, -ADecimals);
    Balance := SimpleRoundTo(Balance - PrincipalPayment, -ADecimals);
    
    Result[I].PaymentNumber := I + 1;
    Result[I].Payment := PaymentAmount;
    Result[I].Principal := PrincipalPayment;
    Result[I].Interest := InterestPayment;
    Result[I].RemainingBalance := Balance;
  end;
end;

class function TFinanceKit.EffectiveAnnualRate(const ANominalRate: Double; const ACompoundingsPerYear: Integer;
  const ADecimals: Integer = 4): Double;
var
  CompoundFactor: Double;
begin
  if ACompoundingsPerYear <= 0 then
    raise Exception.Create('Number of compounding periods must be positive');
    
  if Abs(ANominalRate) < 1E-10 then
    Result := 0
  else
  begin
    CompoundFactor := 1 + (ANominalRate / ACompoundingsPerYear);
    Result := SimpleRoundTo(Power(CompoundFactor, ACompoundingsPerYear) - 1, -ADecimals);
  end;
end;

class function TFinanceKit.ModifiedDuration(const AFaceValue, ACouponRate, AYieldRate: Double;
  const APeriodsPerYear, AYearsToMaturity: Integer; const ADecimals: Integer = 4): Double;
var
  TotalPeriods, T: Integer;
  CouponPayment, BondPV, WeightedTime, MacDur: Double;
  YieldPerPeriod: Double;
  PVCashFlow: Double;
begin
  if (APeriodsPerYear <= 0) or (AYearsToMaturity < 0) then
    raise Exception.Create('Invalid periods or maturity parameters');
    
  TotalPeriods := APeriodsPerYear * AYearsToMaturity;
  CouponPayment := (AFaceValue * ACouponRate) / APeriodsPerYear;
  YieldPerPeriod := AYieldRate / APeriodsPerYear;
  
  // Calculate present value and weighted time sum for Macaulay Duration
  BondPV := 0;
  WeightedTime := 0;
  
  // Calculate weighted time for coupon payments
  for T := 1 to TotalPeriods do
  begin
    PVCashFlow := CouponPayment / Power(1 + YieldPerPeriod, T);
    BondPV := BondPV + PVCashFlow;
    WeightedTime := WeightedTime + (T * PVCashFlow);
  end;
  
  // Add final principal payment
  PVCashFlow := AFaceValue / Power(1 + YieldPerPeriod, TotalPeriods);
  BondPV := BondPV + PVCashFlow;
  WeightedTime := WeightedTime + (TotalPeriods * PVCashFlow);
  
  // Calculate Macaulay Duration (in periods)
  MacDur := WeightedTime / BondPV;
  
  // Convert to Modified Duration and scale to years
  Result := SimpleRoundTo(MacDur / ((1 + YieldPerPeriod) * APeriodsPerYear), -ADecimals);
end;

class function TFinanceKit.BreakEvenUnits(const AFixedCosts, APricePerUnit, AVariableCostPerUnit: Double;
  const ADecimals: Integer = 4): Double;
begin
  if Abs(APricePerUnit - AVariableCostPerUnit) < 1E-10 then
    raise Exception.Create('Price per unit must be different from variable cost per unit');
    
  if APricePerUnit <= AVariableCostPerUnit then
    raise Exception.Create('Price per unit must be greater than variable cost per unit for break-even to exist');
    
  Result := SimpleRoundTo(AFixedCosts / (APricePerUnit - AVariableCostPerUnit), -ADecimals);
end;

class function TFinanceKit.BreakEvenRevenue(const AFixedCosts, APricePerUnit, AVariableCostPerUnit: Double;
  const ADecimals: Integer = 4): Double;
begin
  if Abs(APricePerUnit) < 1E-10 then
    raise Exception.Create('Price per unit must be non-zero');
    
  if APricePerUnit <= AVariableCostPerUnit then
    raise Exception.Create('Price per unit must be greater than variable cost per unit for break-even to exist');
    
  Result := SimpleRoundTo(AFixedCosts / (1 - AVariableCostPerUnit/APricePerUnit), -ADecimals);
end;

class function TFinanceKit.WACC(const AEquityValue, ADebtValue, ACostOfEquity, ACostOfDebt, ATaxRate: Double;
  const ADecimals: Integer = 4): Double;
var
  TotalValue, EquityWeight: Double;
begin
  if (ATaxRate < 0) or (ATaxRate > 1) then
    raise Exception.Create('Tax rate must be between 0 and 1');
    
  TotalValue := AEquityValue + ADebtValue;
  if Abs(TotalValue) < 1E-10 then
    raise Exception.Create('Total firm value must be non-zero');
    
  // Calculate weights ensuring they sum to exactly 1
  EquityWeight := AEquityValue / TotalValue;
  
  Result := SimpleRoundTo(
    (EquityWeight * ACostOfEquity) + 
    ((1 - EquityWeight) * ACostOfDebt * (1 - ATaxRate)), 
    -ADecimals
  );
end;

class function TFinanceKit.CAPM(const ARiskFreeRate, ABeta, AExpectedMarketReturn: Double;
  const ADecimals: Integer = 4): Double;
begin
  if ABeta < -10 then  // Reasonable limit for beta
    raise Exception.Create('Beta value is unreasonably low');
  if ABeta > 10 then   // Reasonable limit for beta
    raise Exception.Create('Beta value is unreasonably high');
    
  Result := SimpleRoundTo(ARiskFreeRate + ABeta * (AExpectedMarketReturn - ARiskFreeRate), -ADecimals);
end;

class function TFinanceKit.GordonGrowthModel(const ACurrentDividend, AGrowthRate, ARequiredReturn: Double;
  const ADecimals: Integer = 4): Double;
begin
  if AGrowthRate >= ARequiredReturn then
    raise Exception.Create('Growth rate must be less than required return rate');
    
  if Abs(ARequiredReturn - AGrowthRate) < 1E-10 then
    raise Exception.Create('Required return must be significantly different from growth rate');
    
  Result := SimpleRoundTo(ACurrentDividend * (1 + AGrowthRate) / (ARequiredReturn - AGrowthRate), -ADecimals);
end;

class function TFinanceKit.WorkingCapitalRatios(const ACurrentAssets, ACurrentLiabilities,
  AInventory, ACash, ASales: Double; const ADecimals: Integer = 4): TWorkingCapitalRatios;
var
  WorkingCapital: Double;
begin
  if Abs(ACurrentLiabilities) < 1E-10 then
    raise Exception.Create('Current liabilities must be non-zero');
    
  // Current Ratio
  Result.CurrentRatio := SimpleRoundTo(ACurrentAssets / ACurrentLiabilities, -ADecimals);
  
  // Quick Ratio (Acid-Test Ratio)
  Result.QuickRatio := SimpleRoundTo((ACurrentAssets - AInventory) / ACurrentLiabilities, -ADecimals);
  
  // Cash Ratio
  Result.CashRatio := SimpleRoundTo(ACash / ACurrentLiabilities, -ADecimals);
  
  // Working Capital Turnover
  WorkingCapital := ACurrentAssets - ACurrentLiabilities;
  if Abs(WorkingCapital) < 1E-10 then
    Result.WorkingCapitalTurnover := 0  // Undefined case
  else
    Result.WorkingCapitalTurnover := SimpleRoundTo(ASales / WorkingCapital, -ADecimals);
end;

class function TFinanceKit.LeverageRatios(const ATotalDebt, ATotalAssets, ATotalEquity,
  AEBIT, AInterestExpense: Double; const ADecimals: Integer = 4): TLeverageRatios;
begin
  if Abs(ATotalAssets) < 1E-10 then
    raise Exception.Create('Total assets must be non-zero');
  if Abs(ATotalEquity) < 1E-10 then
    raise Exception.Create('Total equity must be non-zero');
    
  // Debt Ratio
  Result.DebtRatio := SimpleRoundTo(ATotalDebt / ATotalAssets, -ADecimals);
  
  // Debt to Equity Ratio
  Result.DebtToEquityRatio := SimpleRoundTo(ATotalDebt / ATotalEquity, -ADecimals);
  
  // Equity Multiplier
  Result.EquityMultiplier := SimpleRoundTo(ATotalAssets / ATotalEquity, -ADecimals);
  
  // Times Interest Earned
  if Abs(AInterestExpense) < 1E-10 then
    Result.TimesInterestEarned := 0  // Undefined case
  else
    Result.TimesInterestEarned := SimpleRoundTo(AEBIT / AInterestExpense, -ADecimals);
end;

class function TFinanceKit.BlackScholes(const ASpotPrice, AStrikePrice, ARiskFreeRate, AVolatility, ATimeToMaturity: Double;
  const AOptionType: TOptionType; const ADecimals: Integer = 4): Double;
var
  D1, D2: Double;
  DiscountFactor: Double;
  VolSqrtT: Double;
begin
  if ATimeToMaturity <= 0 then
    raise Exception.Create('Time to maturity must be positive');
  if AVolatility <= 0 then
    raise Exception.Create('Volatility must be positive');
  if (ASpotPrice <= 0) or (AStrikePrice <= 0) then
    raise Exception.Create('Prices must be positive');
    
  // Precalculate common terms
  VolSqrtT := AVolatility * Sqrt(ATimeToMaturity);
  
  // Calculate d1 and d2
  D1 := (Ln(ASpotPrice/AStrikePrice) + (ARiskFreeRate + Sqr(AVolatility)/2) * ATimeToMaturity) / VolSqrtT;
  D2 := D1 - VolSqrtT;
  
  // Calculate discount factor once
  DiscountFactor := Exp(-ARiskFreeRate * ATimeToMaturity);
  
  case AOptionType of
    otCall:
      Result := SimpleRoundTo(
        ASpotPrice * CumulativeNormal(D1) - 
        AStrikePrice * DiscountFactor * CumulativeNormal(D2),
        -ADecimals
      );
    otPut:
      Result := SimpleRoundTo(
        AStrikePrice * DiscountFactor * CumulativeNormal(-D2) - 
        ASpotPrice * CumulativeNormal(-D1),
        -ADecimals
      );
  end;
end;

class function TFinanceKit.RiskMetrics(const APortfolioReturn, ARiskFreeRate, AMarketReturn, ABeta,
  APortfolioStdDev, ABenchmarkReturn, ATrackingError: Double; const ADecimals: Integer = 4): TRiskMetrics;
begin
  // Sharpe Ratio
  if Abs(APortfolioStdDev) < 1E-10 then
    Result.SharpeRatio := 0
  else
    Result.SharpeRatio := SimpleRoundTo((APortfolioReturn - ARiskFreeRate) / APortfolioStdDev, -ADecimals);
  
  // Treynor Ratio
  if Abs(ABeta) < 1E-10 then
    Result.TreynorRatio := 0
  else
    Result.TreynorRatio := SimpleRoundTo((APortfolioReturn - ARiskFreeRate) / ABeta, -ADecimals);
  
  // Jensen's Alpha
  Result.JensenAlpha := SimpleRoundTo(
    APortfolioReturn - (ARiskFreeRate + ABeta * (AMarketReturn - ARiskFreeRate)),
    -ADecimals
  );
  
  // Information Ratio
  if Abs(ATrackingError) < 1E-10 then
    Result.InformationRatio := 0
  else
    Result.InformationRatio := SimpleRoundTo(
      (APortfolioReturn - ABenchmarkReturn) / ATrackingError,
      -ADecimals
    );
end;

class function TFinanceKit.DuPontAnalysis(const ANetIncome, ASales, ATotalAssets, ATotalEquity: Double;
  const ADecimals: Integer = 4): TDuPontAnalysis;
begin
  if Abs(ASales) < 1E-10 then
    raise Exception.Create('Sales must be non-zero');
  if Abs(ATotalAssets) < 1E-10 then
    raise Exception.Create('Total assets must be non-zero');
  if Abs(ATotalEquity) < 1E-10 then
    raise Exception.Create('Total equity must be non-zero');
    
  // Calculate components
  Result.ProfitMargin := SimpleRoundTo(ANetIncome / ASales, -ADecimals);
  Result.AssetTurnover := SimpleRoundTo(ASales / ATotalAssets, -ADecimals);
  Result.EquityMultiplier := SimpleRoundTo(ATotalAssets / ATotalEquity, -ADecimals);
  
  // Calculate final ROE using DuPont formula
  Result.ROE := SimpleRoundTo(
    Result.ProfitMargin * Result.AssetTurnover * Result.EquityMultiplier,
    -ADecimals
  );
end;

class function TFinanceKit.OperatingLeverage(const AQuantity, APricePerUnit, AVariableCostPerUnit,
  AFixedCosts: Double; const ADecimals: Integer = 4): TOperatingLeverage;
var
  ContributionMargin, EBIT, TotalRevenue, TotalVariableCosts: Double;
begin
  if APricePerUnit <= AVariableCostPerUnit then
    raise Exception.Create('Price must be greater than variable cost');
    
  ContributionMargin := APricePerUnit - AVariableCostPerUnit;
  TotalRevenue := AQuantity * APricePerUnit;
  TotalVariableCosts := AQuantity * AVariableCostPerUnit;
  EBIT := TotalRevenue - TotalVariableCosts - AFixedCosts;
  
  // Degree of Operating Leverage (DOL)
  if Abs(EBIT) < 1E-10 then
    Result.DOL := 0
  else
    Result.DOL := SimpleRoundTo((TotalRevenue - TotalVariableCosts) / EBIT, -ADecimals);
  
  // Break-even point in units
  Result.BreakEvenPoint := SimpleRoundTo(AFixedCosts / ContributionMargin, -ADecimals);
  
  // Operating Leverage (same as DOL)
  Result.OperatingLeverage := Result.DOL;
end;

class function TFinanceKit.ProfitabilityRatios(const ARevenue, ACOGS, AEBIT, ANetIncome,
  ATotalAssets, ACurrentLiabilities: Double; const ADecimals: Integer = 4): TProfitabilityRatios;
begin
  if Abs(ARevenue) < 1E-10 then
    raise Exception.Create('Revenue must be non-zero');
  if Abs(ATotalAssets) < 1E-10 then
    raise Exception.Create('Total assets must be non-zero');
    
  // Gross Margin
  Result.GrossMargin := SimpleRoundTo((ARevenue - ACOGS) / ARevenue, -ADecimals);
  
  // Operating Margin
  Result.OperatingMargin := SimpleRoundTo(AEBIT / ARevenue, -ADecimals);
  
  // Net Profit Margin
  Result.NetProfitMargin := SimpleRoundTo(ANetIncome / ARevenue, -ADecimals);
  
  // Return on Assets (ROA)
  Result.ROA := SimpleRoundTo(ANetIncome / ATotalAssets, -ADecimals);
  
  // Return on Capital Employed (ROCE)
  if Abs(ATotalAssets - ACurrentLiabilities) < 1E-10 then
    Result.ROCE := 0
  else
    Result.ROCE := SimpleRoundTo(AEBIT / (ATotalAssets - ACurrentLiabilities), -ADecimals);
end;

end. 
