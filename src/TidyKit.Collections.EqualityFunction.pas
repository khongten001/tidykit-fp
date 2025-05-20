unit TidyKit.Collections.EqualityFunction;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, Math;

// Standard equality comparisons
function TidyKitIntegerEquals(const A, B: Integer): Boolean;
function TidyKitStringEquals(const A, B: string): Boolean;
function TidyKitFloatEquals(const A, B: Extended): Boolean;
function TidyKitBooleanEquals(const A, B: Boolean): Boolean;
function TidyKitDateTimeEquals(const A, B: TDateTime): Boolean;
function TidyKitCharEquals(const A, B: Char): Boolean;
function TidyKitInt64Equals(const A, B: Int64): Boolean;

// Additional equality functions for complex types
function TidyKitPointEquals(const A, B: TPoint): Boolean;
function TidyKitRectEquals(const A, B: TRect): Boolean;

implementation

function TidyKitIntegerEquals(const A, B: Integer): Boolean;
begin
  Result := A = B;
end;

function TidyKitStringEquals(const A, B: string): Boolean;
begin
  Result := A = B;
end;

function TidyKitFloatEquals(const A, B: Extended): Boolean;
const
  Epsilon = 1E-10;
var
  AIsNegativeZero, BIsNegativeZero: Boolean;
  
  function IsNegativeZero(Value: Extended): Boolean;
  {$IFDEF LINUX}
  var
    ExtBytes: array[0..9] of Byte absolute Value;
  begin
    // On Linux, check the sign bit in the 80-bit Extended format (bit 79)
    Result := (ExtBytes[9] and $80) <> 0;
  end;
  {$ELSE}
  begin
    // On other platforms, use the original 64-bit check
    try
      Result := (PInt64(@Value)^ and $8000000000000000) <> 0;
    except
      Result := False;
    end;
  end;
  {$ENDIF}
  
begin
  if IsNan(A) then
    Result := IsNan(B)
  else if IsNan(B) then
    Result := False
  else if IsInfinite(A) and IsInfinite(B) then
    Result := (A > 0) = (B > 0)  // Check if both are +Inf or both are -Inf
  else if (A = 0) and (B = 0) then
  begin
    // Use the same negative zero detection as in FloatHash
    AIsNegativeZero := IsNegativeZero(A);
    BIsNegativeZero := IsNegativeZero(B);
    Result := AIsNegativeZero = BIsNegativeZero;
  end
  else
    Result := Abs(A - B) < Epsilon;
end;

function TidyKitBooleanEquals(const A, B: Boolean): Boolean;
begin
  Result := A = B;
end;

function TidyKitDateTimeEquals(const A, B: TDateTime): Boolean;
const
  Epsilon = 1 / (24 * 60 * 60 * 1000); // 1 millisecond
begin
  Result := Abs(A - B) < Epsilon;
end;

function TidyKitCharEquals(const A, B: Char): Boolean;
begin
  Result := A = B;
end;

function TidyKitInt64Equals(const A, B: Int64): Boolean;
begin
  Result := A = B;
end;

function TidyKitPointEquals(const A, B: TPoint): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y);
end;

function TidyKitRectEquals(const A, B: TRect): Boolean;
begin
  Result := (A.Left = B.Left) and (A.Top = B.Top) and
            (A.Right = B.Right) and (A.Bottom = B.Bottom);
end;

end.
