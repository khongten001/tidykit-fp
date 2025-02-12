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
  Result := (APresentValue * ARate * Power(1 + ARate, APeriods)) / (Power(1 + ARate, APeriods) - 1);
end;

class function TFinanceKit.NetPresentValue(const AInitialInvestment: Double; const ACashFlows: TDoubleArray; const Rate: Double): Double;
var
  I: Integer;
begin
  Result := -AInitialInvestment;
  for I := 0 to High(ACashFlows) do
    Result := Result + ACashFlows[I] / Power(1 + Rate, I + 1);
end;

class function TFinanceKit.InternalRateOfReturn(const AInitialInvestment: Double; const ACashFlows: TDoubleArray): Double;
const
  TOLERANCE = 0.0001;
  MAX_ITERATIONS = 100;
var
  Rate, NPV, DerivedNPV: Double;
  I, Iteration: Integer;
begin
  Rate := 0.1; // Initial guess
  Iteration := 0;
  
  repeat
    NPV := -AInitialInvestment;
    DerivedNPV := 0;
    
    for I := 0 to High(ACashFlows) do
    begin
      NPV := NPV + ACashFlows[I] / Power(1 + Rate, I + 1);
      DerivedNPV := DerivedNPV - (I + 1) * ACashFlows[I] / Power(1 + Rate, I + 2);
    end;
    
    Rate := Rate - NPV / DerivedNPV;
    Inc(Iteration);
    
  until (Abs(NPV) < TOLERANCE) or (Iteration >= MAX_ITERATIONS);
  
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
  Rate := 1 - Power(ASalvage / ACost, 1 / ALife);
  Result := ACost * Power(1 - Rate, APeriod - 1) * Rate;
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
