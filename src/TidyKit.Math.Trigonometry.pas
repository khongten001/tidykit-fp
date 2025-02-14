unit TidyKit.Math.Trigonometry;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, Math, TidyKit.Math;

type
  { Trigonometry calculations class.
    All trigonometric functions (Sin, Cos, Tan) expect input angles in radians.
    All inverse trigonometric functions (ArcSin, ArcCos, ArcTan, ArcTan2) return angles in radians.
    Vector angle calculations return results in radians.
    Use DegToRad and RadToDeg for angle conversions. }
  TTrigKit = class
  public
    { Angle conversions }
    class function DegToRad(const Degrees: Double): Double; static;  // Converts degrees to radians
    class function RadToDeg(const Radians: Double): Double; static;  // Converts radians to degrees
    
    { Additional angle conversions }
    class function GradToRad(const Grads: Double): Double; static;  // Converts grads to radians
    class function RadToGrad(const Radians: Double): Double; static;  // Converts radians to grads
    class function NormalizeAngle(const Angle: Double): Double; static;  // Normalizes angle to range [0, 2π]
    class function NormalizeAngleDeg(const Angle: Double): Double; static;  // Normalizes angle to range [0, 360]
    
    { Basic trigonometric functions - all expect input in radians }
    class function Sin(const X: Double): Double; static;  // Sine of angle X in radians
    class function Cos(const X: Double): Double; static;  // Cosine of angle X in radians 
    class function Tan(const X: Double): Double; static;  // Tangent of angle X in radians
    
    { Inverse trigonometric functions - all return angles in radians }
    class function ArcSin(const X: Double): Double; static;  // Inverse sine (arcsin), returns angle in radians
    class function ArcCos(const X: Double): Double; static;  // Inverse cosine (arccos), returns angle in radians
    class function ArcTan(const X: Double): Double; static;  // Inverse tangent (arctan), returns angle in radians
    class function ArcTan2(const Y, X: Double): Double; static;  // Two-argument inverse tangent, returns angle in radians in range [-π, π]
    
    { Hyperbolic functions }
    class function Sinh(const X: Double): Double; static;  // Hyperbolic sine
    class function Cosh(const X: Double): Double; static;  // Hyperbolic cosine
    class function Tanh(const X: Double): Double; static;  // Hyperbolic tangent
    
    { Additional inverse hyperbolic functions }
    class function ArcSinh(const X: Double): Double; static;  // Inverse hyperbolic sine
    class function ArcCosh(const X: Double): Double; static;  // Inverse hyperbolic cosine
    class function ArcTanh(const X: Double): Double; static;  // Inverse hyperbolic tangent
    
    { Secant, Cosecant, and Cotangent functions }
    class function Sec(const X: Double): Double; static;  // Secant of angle X in radians
    class function Csc(const X: Double): Double; static;  // Cosecant of angle X in radians
    class function Cot(const X: Double): Double; static;  // Cotangent of angle X in radians
    
    { Triangle calculations }
    class function Hypotenuse(const A, B: Double): Double; static;  // Calculates hypotenuse length given two sides using Pythagorean theorem
    class function TriangleArea(const Base, Height: Double): Double; static;  // Calculates triangle area using base and height
    { Calculates triangle area using Side-Angle-Side (SAS) formula:
      SideA and SideB are two known sides, Angle is the angle between them in radians.
      Uses the formula: Area = (1/2) * a * b * sin(C), where C is the included angle }
    class function TriangleAreaSAS(const SideA, Angle, SideB: Double): Double; static;
    { Calculates triangle area using Side-Side-Side (SSS) formula:
      A, B, C are the lengths of three sides.
      Uses Heron's formula with semi-perimeter }
    class function TriangleAreaSSS(const A, B, C: Double): Double; static;
    
    { Additional triangle calculations }
    class function TrianglePerimeter(const A, B, C: Double): Double; static;  // Calculates triangle perimeter
    class function TriangleInRadius(const A, B, C: Double): Double; static;  // Calculates radius of inscribed circle
    class function TriangleCircumRadius(const A, B, C: Double): Double; static;  // Calculates radius of circumscribed circle
    
    { Circle sector and segment calculations }
    class function CircularSectorArea(const Radius, Angle: Double): Double; static;  // Calculates area of circular sector
    class function CircularSegmentArea(const Radius, Angle: Double): Double; static;  // Calculates area of circular segment
    class function ChordLength(const Radius, Angle: Double): Double; static;  // Calculates chord length given central angle
    
    { Vector operations }
    class function VectorMagnitude(const X, Y: Double): Double; static;  // Calculates magnitude (length) of a 2D vector
    { Calculates angle between two points (vectors) in 2D space.
      Returns angle in radians in range [-π, π] relative to positive X axis.
      (X1,Y1) is the start point, (X2,Y2) is the end point }
    class function VectorAngle(const X1, Y1, X2, Y2: Double): Double; static;
  end;

implementation

{ TTrigKit }

class function TTrigKit.DegToRad(const Degrees: Double): Double;
begin
  Result := Degrees * Pi / 180;
end;

class function TTrigKit.RadToDeg(const Radians: Double): Double;
begin
  Result := Radians * 180 / Pi;
end;

class function TTrigKit.Sin(const X: Double): Double;
begin
  Result := System.Sin(X);
end;

class function TTrigKit.Cos(const X: Double): Double;
begin
  Result := System.Cos(X);
end;

class function TTrigKit.Tan(const X: Double): Double;
begin
  Result := Math.Tan(X);
end;

class function TTrigKit.ArcSin(const X: Double): Double;
begin
  Result := Math.ArcSin(X);
end;

class function TTrigKit.ArcCos(const X: Double): Double;
begin
  Result := Math.ArcCos(X);
end;

class function TTrigKit.ArcTan(const X: Double): Double;
begin
  Result := Math.ArcTan2(X, 1.0);
end;

class function TTrigKit.ArcTan2(const Y, X: Double): Double;
begin
  Result := Math.ArcTan2(Y, X);
end;

class function TTrigKit.Sinh(const X: Double): Double;
begin
  Result := (System.Exp(X) - System.Exp(-X)) / 2;
end;

class function TTrigKit.Cosh(const X: Double): Double;
begin
  Result := (System.Exp(X) + System.Exp(-X)) / 2;
end;

class function TTrigKit.Tanh(const X: Double): Double;
begin
  Result := Sinh(X) / Cosh(X);
end;

class function TTrigKit.Hypotenuse(const A, B: Double): Double;
begin
  Result := System.Sqrt(System.Sqr(A) + System.Sqr(B));
end;

class function TTrigKit.TriangleArea(const Base, Height: Double): Double;
begin
  Result := Base * Height / 2;
end;

class function TTrigKit.TriangleAreaSAS(const SideA, Angle, SideB: Double): Double;
begin
  Result := SideA * SideB * System.Sin(Angle) / 2;
end;

class function TTrigKit.TriangleAreaSSS(const A, B, C: Double): Double;
var
  S: Double;
begin
  S := (A + B + C) / 2;  // Semi-perimeter
  Result := System.Sqrt(S * (S - A) * (S - B) * (S - C));  // Heron's formula
end;

class function TTrigKit.VectorMagnitude(const X, Y: Double): Double;
begin
  Result := System.Sqrt(System.Sqr(X) + System.Sqr(Y));
end;

class function TTrigKit.VectorAngle(const X1, Y1, X2, Y2: Double): Double;
begin
  Result := Math.ArcTan2(Y2 - Y1, X2 - X1);
end;

{ Additional angle conversions }
class function TTrigKit.GradToRad(const Grads: Double): Double;
begin
  Result := Grads * Pi / 200;  // 400 grads = 2π radians
end;

class function TTrigKit.RadToGrad(const Radians: Double): Double;
begin
  Result := Radians * 200 / Pi;  // 2π radians = 400 grads
end;

class function TTrigKit.NormalizeAngle(const Angle: Double): Double;
begin
  Result := Angle;
  while Result < 0 do
    Result := Result + 2 * Pi;
  while Result >= 2 * Pi do
    Result := Result - 2 * Pi;
end;

class function TTrigKit.NormalizeAngleDeg(const Angle: Double): Double;
begin
  Result := Angle;
  while Result < 0 do
    Result := Result + 360;
  while Result >= 360 do
    Result := Result - 360;
end;

{ Additional inverse hyperbolic functions }
class function TTrigKit.ArcSinh(const X: Double): Double;
begin
  Result := System.Ln(X + System.Sqrt(System.Sqr(X) + 1));
end;

class function TTrigKit.ArcCosh(const X: Double): Double;
begin
  if X < 1 then
    Result := NaN
  else
    Result := System.Ln(X + System.Sqrt(System.Sqr(X) - 1));
end;

class function TTrigKit.ArcTanh(const X: Double): Double;
begin
  if (X <= -1) or (X >= 1) then
    Result := NaN
  else
    Result := 0.5 * System.Ln((1 + X) / (1 - X));
end;

{ Secant, Cosecant, and Cotangent functions }
class function TTrigKit.Sec(const X: Double): Double;
begin
  Result := 1 / System.Cos(X);
end;

class function TTrigKit.Csc(const X: Double): Double;
begin
  Result := 1 / System.Sin(X);
end;

class function TTrigKit.Cot(const X: Double): Double;
begin
  Result := 1 / Math.Tan(X);
end;

{ Additional triangle calculations }
class function TTrigKit.TrianglePerimeter(const A, B, C: Double): Double;
begin
  Result := A + B + C;
end;

class function TTrigKit.TriangleInRadius(const A, B, C: Double): Double;
var
  S, Area: Double;
begin
  S := (A + B + C) / 2;  // Semi-perimeter
  Area := TriangleAreaSSS(A, B, C);
  Result := 2 * Area / (A + B + C);  // r = 2A/P where A is area and P is perimeter
end;

class function TTrigKit.TriangleCircumRadius(const A, B, C: Double): Double;
var
  Area: Double;
begin
  Area := TriangleAreaSSS(A, B, C);
  Result := (A * B * C) / (4 * Area);  // R = abc/(4A) where A is area
end;

{ Circle sector and segment calculations }
class function TTrigKit.CircularSectorArea(const Radius, Angle: Double): Double;
begin
  Result := 0.5 * System.Sqr(Radius) * Angle;  // A = (1/2)r²θ where θ is in radians
end;

class function TTrigKit.CircularSegmentArea(const Radius, Angle: Double): Double;
begin
  Result := 0.5 * System.Sqr(Radius) * (Angle - System.Sin(Angle));
end;

class function TTrigKit.ChordLength(const Radius, Angle: Double): Double;
begin
  Result := 2 * Radius * System.Sin(Angle / 2);
end;

end. 
