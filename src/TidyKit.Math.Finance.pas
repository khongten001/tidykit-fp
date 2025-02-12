unit TidyKit.Math.Finance;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, Math, TidyKit.Math;

type
  { Financial calculations class }
  generic TFinanceKit<T> = class
  public
    { Present Value calculations }
    class function PresentValue(const FutureValue, Rate: T; const Periods: Integer): T; static;
    class function FutureValue(const PresentValue, Rate: T; const Periods: Integer): T; static;
    
    { Interest and payments }
    class function CompoundInterest(const Principal, Rate: T; const Periods: Integer): T; static;
    class function Payment(const PresentValue, Rate: T; const Periods: Integer): T; static;
    
    { Investment analysis }
    class function NetPresentValue(const InitialInvestment: T; const CashFlows: TArray<T>; const Rate: T): T; static;
    class function InternalRateOfReturn(const InitialInvestment: T; const CashFlows: TArray<T>): T; static;
    
    { Depreciation }
    class function StraightLineDepreciation(const Cost, Salvage: T; const Life: Integer): T; static;
    class function DecliningBalanceDepreciation(const Cost, Salvage: T; const Life: Integer; const Period: Integer): T; static;
    
    { Return calculations }
    class function ReturnOnInvestment(const Gain, Cost: T): T; static;
    class function ReturnOnEquity(const NetIncome, ShareholdersEquity: T): T; static;
  end;

implementation

{ TFinanceKit }

class function TFinanceKit.PresentValue(const FutureValue, Rate: T; const Periods: Integer): T;
begin
  Result := FutureValue / Power(1 + Rate, Periods);
end;

class function TFinanceKit.FutureValue(const PresentValue, Rate: T; const Periods: Integer): T;
begin
  Result := PresentValue * Power(1 + Rate, Periods);
end;

class function TFinanceKit.CompoundInterest(const Principal, Rate: T; const Periods: Integer): T;
begin
  Result := Principal * (Power(1 + Rate, Periods) - 1);
end;

class function TFinanceKit.Payment(const PresentValue, Rate: T; const Periods: Integer): T;
begin
  Result := (PresentValue * Rate * Power(1 + Rate, Periods)) / (Power(1 + Rate, Periods) - 1);
end;

class function TFinanceKit.NetPresentValue(const InitialInvestment: T; const CashFlows: TArray<T>; const Rate: T): T;
var
  I: Integer;
begin
  Result := -InitialInvestment;
  for I := 0 to High(CashFlows) do
    Result := Result + CashFlows[I] / Power(1 + Rate, I + 1);
end;

class function TFinanceKit.InternalRateOfReturn(const InitialInvestment: T; const CashFlows: TArray<T>): T;
const
  TOLERANCE = 0.0001;
  MAX_ITERATIONS = 100;
var
  Rate, NPV, DerivedNPV: T;
  I, Iteration: Integer;
begin
  Rate := 0.1; // Initial guess
  Iteration := 0;
  
  repeat
    NPV := -InitialInvestment;
    DerivedNPV := 0;
    
    for I := 0 to High(CashFlows) do
    begin
      NPV := NPV + CashFlows[I] / Power(1 + Rate, I + 1);
      DerivedNPV := DerivedNPV - (I + 1) * CashFlows[I] / Power(1 + Rate, I + 2);
    end;
    
    Rate := Rate - NPV / DerivedNPV;
    Inc(Iteration);
    
  until (Abs(NPV) < TOLERANCE) or (Iteration >= MAX_ITERATIONS);
  
  Result := Rate;
end;

class function TFinanceKit.StraightLineDepreciation(const Cost, Salvage: T; const Life: Integer): T;
begin
  Result := (Cost - Salvage) / Life;
end;

class function TFinanceKit.DecliningBalanceDepreciation(const Cost, Salvage: T; const Life: Integer; const Period: Integer): T;
var
  Rate: T;
begin
  Rate := 1 - Power(Salvage / Cost, 1 / Life);
  Result := Cost * Power(1 - Rate, Period - 1) * Rate;
end;

class function TFinanceKit.ReturnOnInvestment(const Gain, Cost: T): T;
begin
  Result := (Gain - Cost) / Cost;
end;

class function TFinanceKit.ReturnOnEquity(const NetIncome, ShareholdersEquity: T): T;
begin
  Result := NetIncome / ShareholdersEquity;
end;

end. 