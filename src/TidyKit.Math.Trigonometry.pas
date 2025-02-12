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
    
    { Basic trigonometric functions - all expect input in radians }
    class function Sin(const X: Double): Double; static;  // X in radians
    class function Cos(const X: Double): Double; static;  // X in radians
    class function Tan(const X: Double): Double; static;  // X in radians
    
    { Inverse trigonometric functions - all return angles in radians }
    class function ArcSin(const X: Double): Double; static;  // Returns angle in radians
    class function ArcCos(const X: Double): Double; static;  // Returns angle in radians
    class function ArcTan(const X: Double): Double; static;  // Returns angle in radians
    class function ArcTan2(const Y, X: Double): Double; static;  // Returns angle in radians
    
    { Hyperbolic functions }
    class function Sinh(const X: Double): Double; static;
    class function Cosh(const X: Double): Double; static;
    class function Tanh(const X: Double): Double; static;
    
    { Triangle calculations }
    class function Hypotenuse(const A, B: Double): Double; static;
    class function TriangleArea(const Base, Height: Double): Double; static;
    class function TriangleAreaSAS(const SideA, Angle, SideB: Double): Double; static;  // Angle in radians
    class function TriangleAreaSSS(const A, B, C: Double): Double; static;
    
    { Vector operations }
    class function VectorMagnitude(const X, Y: Double): Double; static;
    class function VectorAngle(const X1, Y1, X2, Y2: Double): Double; static;  // Returns angle in radians
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

end. 
