unit TidyKit.Math.Trigonometry;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, Math, TidyKit.Math;

type
  { Trigonometry calculations class }
  generic TTrigKit<T> = class
  public
    { Angle conversions }
    class function DegToRad(const Degrees: T): T; static;
    class function RadToDeg(const Radians: T): T; static;
    
    { Basic trigonometric functions }
    class function Sin(const X: T): T; static;
    class function Cos(const X: T): T; static;
    class function Tan(const X: T): T; static;
    
    { Inverse trigonometric functions }
    class function ArcSin(const X: T): T; static;
    class function ArcCos(const X: T): T; static;
    class function ArcTan(const X: T): T; static;
    class function ArcTan2(const Y, X: T): T; static;
    
    { Hyperbolic functions }
    class function Sinh(const X: T): T; static;
    class function Cosh(const X: T): T; static;
    class function Tanh(const X: T): T; static;
    
    { Triangle calculations }
    class function Hypotenuse(const A, B: T): T; static;
    class function TriangleArea(const Base, Height: T): T; static;
    class function TriangleAreaSAS(const SideA, Angle, SideB: T): T; static;
    class function TriangleAreaSSS(const A, B, C: T): T; static;
    
    { Vector operations }
    class function VectorMagnitude(const X, Y: T): T; static;
    class function VectorAngle(const X1, Y1, X2, Y2: T): T; static;
  end;

implementation

{ TTrigKit }

class function TTrigKit.DegToRad(const Degrees: T): T;
begin
  Result := Degrees * Pi / 180;
end;

class function TTrigKit.RadToDeg(const Radians: T): T;
begin
  Result := Radians * 180 / Pi;
end;

class function TTrigKit.Sin(const X: T): T;
begin
  Result := System.Sin(X);
end;

class function TTrigKit.Cos(const X: T): T;
begin
  Result := System.Cos(X);
end;

class function TTrigKit.Tan(const X: T): T;
begin
  Result := System.Tan(X);
end;

class function TTrigKit.ArcSin(const X: T): T;
begin
  Result := System.ArcSin(X);
end;

class function TTrigKit.ArcCos(const X: T): T;
begin
  Result := System.ArcCos(X);
end;

class function TTrigKit.ArcTan(const X: T): T;
begin
  Result := System.ArcTan(X);
end;

class function TTrigKit.ArcTan2(const Y, X: T): T;
begin
  Result := System.ArcTan2(Y, X);
end;

class function TTrigKit.Sinh(const X: T): T;
begin
  Result := (Exp(X) - Exp(-X)) / 2;
end;

class function TTrigKit.Cosh(const X: T): T;
begin
  Result := (Exp(X) + Exp(-X)) / 2;
end;

class function TTrigKit.Tanh(const X: T): T;
begin
  Result := Sinh(X) / Cosh(X);
end;

class function TTrigKit.Hypotenuse(const A, B: T): T;
begin
  Result := Sqrt(Sqr(A) + Sqr(B));
end;

class function TTrigKit.TriangleArea(const Base, Height: T): T;
begin
  Result := Base * Height / 2;
end;

class function TTrigKit.TriangleAreaSAS(const SideA, Angle, SideB: T): T;
begin
  Result := SideA * SideB * Sin(Angle) / 2;
end;

class function TTrigKit.TriangleAreaSSS(const A, B, C: T): T;
var
  S: T;
begin
  S := (A + B + C) / 2;  // Semi-perimeter
  Result := Sqrt(S * (S - A) * (S - B) * (S - C));  // Heron's formula
end;

class function TTrigKit.VectorMagnitude(const X, Y: T): T;
begin
  Result := Sqrt(Sqr(X) + Sqr(Y));
end;

class function TTrigKit.VectorAngle(const X1, Y1, X2, Y2: T): T;
begin
  Result := ArcTan2(Y2 - Y1, X2 - X1);
end;

end. 