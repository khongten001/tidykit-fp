unit TidyKit.Math.Trigonometry;

{-----------------------------------------------------------------------------
 TidyKit.Math.Trigonometry

 A library for trigonometric calculations in Free Pascal
 
 This unit provides:
 - Basic trigonometric functions (Sin, Cos, Tan)
 - Inverse trigonometric functions (ArcSin, ArcCos, ArcTan, ArcTan2)
 - Hyperbolic functions (Sinh, Cosh, Tanh) and their inverses
 - Reciprocal functions (Sec, Csc, Cot)
 - Angle conversions (Degrees, Radians, Grads)
 - Angle normalization
 - Triangle calculations (Area, Perimeter, InRadius, CircumRadius, Hypotenuse)
 - Circle calculations (Sector Area, Segment Area, Chord Length)
 - Basic 2D vector operations (Magnitude, Angle)
 
 Design principles:
 - Static class methods for easy access to functions
 - Consistent use of radians for trigonometric function inputs/outputs
 - Clear separation of angle units and conversions
 - Basic geometry calculations for triangles and circles
-----------------------------------------------------------------------------}

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
    { @description Converts degrees to radians
      
      @usage Use when converting angle measurements from degrees to radians for trigonometric functions.
      
      @param Degrees The angle value in degrees.
      
      @returns The equivalent angle value in radians.
      
      @references Formula: Radians = Degrees * Pi / 180
      
      @warning None
      
      @example
        var Radians: Double;
        begin
          Radians := TTrigKit.DegToRad(90.0); // Returns: 1.5707... (Pi/2)
        end; }
    class function DegToRad(const Degrees: Double): Double; static;

    { @description Converts radians to degrees
      
      @usage Use when converting angle measurements from radians (e.g., results of inverse trig functions) to degrees.
      
      @param Radians The angle value in radians.
      
      @returns The equivalent angle value in degrees.
      
      @references Formula: Degrees = Radians * 180 / Pi
      
      @warning None
      
      @example
        var Degrees: Double;
        begin
          Degrees := TTrigKit.RadToDeg(Pi); // Returns: 180.0
        end; }
    class function RadToDeg(const Radians: Double): Double; static;
    
    { @description Converts grads to radians
      
      @usage Use when converting angle measurements from grads (gons) to radians.
      
      @param Grads The angle value in grads.
      
      @returns The equivalent angle value in radians.
      
      @references Formula: Radians = Grads * Pi / 200 (since 400 grads = 2 Pi radians)
      
      @warning None
      
      @example
        var Radians: Double;
        begin
          Radians := TTrigKit.GradToRad(100.0); // Returns: 1.5707... (Pi/2)
        end; }
    class function GradToRad(const Grads: Double): Double; static;

    { @description Converts radians to grads
      
      @usage Use when converting angle measurements from radians to grads (gons).
      
      @param Radians The angle value in radians.
      
      @returns The equivalent angle value in grads.
      
      @references Formula: Grads = Radians * 200 / Pi (since 2 Pi radians = 400 grads)
      
      @warning None
      
      @example
        var Grads: Double;
        begin
          Grads := TTrigKit.RadToGrad(Pi / 2); // Returns: 100.0
        end; }
    class function RadToGrad(const Radians: Double): Double; static;

    { @description Normalizes angle to range [0, 2 Pi]
      
      @usage Use to bring an angle in radians into the standard range of 0 to 2 Pi (exclusive of 2 Pi).
      
      @param Angle The angle value in radians.
      
      @returns The normalized angle in the range [0, 2 Pi).
      
      @references None
      
      @warning None
      
      @example
        var NormAngle: Double;
        begin
          NormAngle := TTrigKit.NormalizeAngle(3 * Pi); // Returns: Pi
          NormAngle := TTrigKit.NormalizeAngle(-Pi / 2); // Returns: 3 * Pi / 2
        end; }
    class function NormalizeAngle(const Angle: Double): Double; static;

    { @description Normalizes angle to range [0, 360]
      
      @usage Use to bring an angle in degrees into the standard range of 0 to 360 (exclusive of 360).
      
      @param Angle The angle value in degrees.
      
      @returns The normalized angle in the range [0, 360).
      
      @references None
      
      @warning None
      
      @example
        var NormAngleDeg: Double;
        begin
          NormAngleDeg := TTrigKit.NormalizeAngleDeg(450.0); // Returns: 90.0
          NormAngleDeg := TTrigKit.NormalizeAngleDeg(-90.0); // Returns: 270.0
        end; }
    class function NormalizeAngleDeg(const Angle: Double): Double; static;
    
    { @description Sine of angle X in radians
      
      @usage Calculate the sine of an angle given in radians.
      
      @param X The angle in radians.
      
      @returns The sine of the angle, a value between -1 and 1.
      
      @references Based on the standard System.Sin function.
      
      @warning None
      
      @example
        var S: Double;
        begin
          S := TTrigKit.Sin(Pi / 2); // Returns: 1.0
        end; }
    class function Sin(const X: Double): Double; static;

    { @description Cosine of angle X in radians
      
      @usage Calculate the cosine of an angle given in radians.
      
      @param X The angle in radians.
      
      @returns The cosine of the angle, a value between -1 and 1.
      
      @references Based on the standard System.Cos function.
      
      @warning None
      
      @example
        var C: Double;
        begin
          C := TTrigKit.Cos(Pi); // Returns: -1.0
        end; }
    class function Cos(const X: Double): Double; static; 

    { @description Tangent of angle X in radians
      
      @usage Calculate the tangent of an angle given in radians.
      
      @param X The angle in radians.
      
      @returns The tangent of the angle.
      
      @references Based on the standard Math.Tan function.
      
      @warning Result is undefined for angles X where Cos(X) is 0 (e.g., Pi/2, 3*Pi/2). May return large values or raise exceptions depending on FPU settings.
      
      @example
        var T: Double;
        begin
          T := TTrigKit.Tan(Pi / 4); // Returns: 1.0
        end; }
    class function Tan(const X: Double): Double; static;
    
    { @description Inverse sine (arcsin), returns angle in radians
      
      @usage Calculate the angle (in radians) whose sine is X.
      
      @param X The sine value, must be in the range [-1, 1].
      
      @returns The angle in radians, in the range [-Pi/2, Pi/2].
      
      @references Based on the standard Math.ArcSin function.
      
      @warning Input X must be between -1 and 1, inclusive. Returns NaN for invalid inputs.
      
      @example
        var Angle: Double;
        begin
          Angle := TTrigKit.ArcSin(1.0); // Returns: Pi / 2
        end; }
    class function ArcSin(const X: Double): Double; static;

    { @description Inverse cosine (arccos), returns angle in radians
      
      @usage Calculate the angle (in radians) whose cosine is X.
      
      @param X The cosine value, must be in the range [-1, 1].
      
      @returns The angle in radians, in the range [0, Pi].
      
      @references Based on the standard Math.ArcCos function.
      
      @warning Input X must be between -1 and 1, inclusive. Returns NaN for invalid inputs.
      
      @example
        var Angle: Double;
        begin
          Angle := TTrigKit.ArcCos(-1.0); // Returns: Pi
        end; }
    class function ArcCos(const X: Double): Double; static;

    { @description Inverse tangent (arctan), returns angle in radians
      
      @usage Calculate the angle (in radians) whose tangent is X. Uses ArcTan2(X, 1.0) internally.
      
      @param X The tangent value.
      
      @returns The angle in radians, in the range [-Pi/2, Pi/2].
      
      @references Based on the standard Math.ArcTan2 function.
      
      @warning None
      
      @example
        var Angle: Double;
        begin
          Angle := TTrigKit.ArcTan(1.0); // Returns: Pi / 4
        end; }
    class function ArcTan(const X: Double): Double; static;

    { @description Two-argument inverse tangent, returns angle in radians in range [-Pi, Pi]
      
      @usage Calculate the angle (in radians) of the point (X, Y) in the Cartesian plane relative to the positive X-axis. Handles all four quadrants correctly.
      
      @param Y The Y-coordinate.
      @param X The X-coordinate.
      
      @returns The angle in radians, in the range [-Pi, Pi].
      
      @references Based on the standard Math.ArcTan2 function.
      
      @warning Returns 0 if both X and Y are 0.
      
      @example
        var Angle: Double;
        begin
          Angle := TTrigKit.ArcTan2(-1.0, -1.0); // Returns: -3 * Pi / 4
        end; }
    class function ArcTan2(const Y, X: Double): Double; static;
    
    { @description Hyperbolic sine
      
      @usage Calculate the hyperbolic sine of X.
      
      @param X The input value.
      
      @returns The hyperbolic sine of X.
      
      @references Formula: Sinh(X) = (e^X - e^(-X)) / 2
      
      @warning None
      
      @example
        var Sh: Double;
        begin
          Sh := TTrigKit.Sinh(1.0); // Returns: ~1.1752
        end; }
    class function Sinh(const X: Double): Double; static;

    { @description Hyperbolic cosine
      
      @usage Calculate the hyperbolic cosine of X.
      
      @param X The input value.
      
      @returns The hyperbolic cosine of X.
      
      @references Formula: Cosh(X) = (e^X + e^(-X)) / 2
      
      @warning None
      
      @example
        var Ch: Double;
        begin
          Ch := TTrigKit.Cosh(0.0); // Returns: 1.0
        end; }
    class function Cosh(const X: Double): Double; static;

    { @description Hyperbolic tangent
      
      @usage Calculate the hyperbolic tangent of X.
      
      @param X The input value.
      
      @returns The hyperbolic tangent of X.
      
      @references Formula: Tanh(X) = Sinh(X) / Cosh(X)
      
      @warning None
      
      @example
        var Th: Double;
        begin
          Th := TTrigKit.Tanh(1.0); // Returns: ~0.7616
        end; }
    class function Tanh(const X: Double): Double; static;
    
    { @description Inverse hyperbolic sine
      
      @usage Calculate the value whose hyperbolic sine is X.
      
      @param X The input value.
      
      @returns The inverse hyperbolic sine of X.
      
      @references Formula: ArcSinh(X) = ln(X + sqrt(X^2 + 1))
      
      @warning None
      
      @example
        var Asinh: Double;
        begin
          Asinh := TTrigKit.ArcSinh(1.1752); // Returns: ~1.0
        end; }
    class function ArcSinh(const X: Double): Double; static;

    { @description Inverse hyperbolic cosine
      
      @usage Calculate the value whose hyperbolic cosine is X.
      
      @param X The input value, must be >= 1.
      
      @returns The non-negative inverse hyperbolic cosine of X.
      
      @references Formula: ArcCosh(X) = ln(X + sqrt(X^2 - 1)) for X >= 1
      
      @warning Input X must be greater than or equal to 1. Returns NaN for X < 1.
      
      @example
        var Acosh: Double;
        begin
          Acosh := TTrigKit.ArcCosh(1.0); // Returns: 0.0
        end; }
    class function ArcCosh(const X: Double): Double; static;

    { @description Inverse hyperbolic tangent
      
      @usage Calculate the value whose hyperbolic tangent is X.
      
      @param X The input value, must be in the range (-1, 1).
      
      @returns The inverse hyperbolic tangent of X.
      
      @references Formula: ArcTanh(X) = 0.5 * ln((1 + X) / (1 - X)) for -1 < X < 1
      
      @warning Input X must be strictly between -1 and 1. Returns NaN for |X| >= 1.
      
      @example
        var Atanh: Double;
        begin
          Atanh := TTrigKit.ArcTanh(0.7616); // Returns: ~1.0
        end; }
    class function ArcTanh(const X: Double): Double; static;
    
    { @description Secant of angle X in radians
      
      @usage Calculate the secant (1/cosine) of an angle given in radians.
      
      @param X The angle in radians.
      
      @returns The secant of the angle.
      
      @references Formula: Sec(X) = 1 / Cos(X)
      
      @warning Result is undefined for angles X where Cos(X) is 0 (e.g., Pi/2, 3*Pi/2). May cause division by zero or return large values.
      
      @example
        var Sc: Double;
        begin
          Sc := TTrigKit.Sec(Pi / 4); // Returns: ~1.414 (sqrt(2))
        end; }
    class function Sec(const X: Double): Double; static;

    { @description Cosecant of angle X in radians
      
      @usage Calculate the cosecant (1/sine) of an angle given in radians.
      
      @param X The angle in radians.
      
      @returns The cosecant of the angle.
      
      @references Formula: Csc(X) = 1 / Sin(X)
      
      @warning Result is undefined for angles X where Sin(X) is 0 (e.g., 0, Pi, 2*Pi). May cause division by zero or return large values.
      
      @example
        var CscVal: Double;
        begin
          CscVal := TTrigKit.Csc(Pi / 2); // Returns: 1.0
        end; }
    class function Csc(const X: Double): Double; static;

    { @description Cotangent of angle X in radians
      
      @usage Calculate the cotangent (1/tangent or cosine/sine) of an angle given in radians.
      
      @param X The angle in radians.
      
      @returns The cotangent of the angle.
      
      @references Formula: Cot(X) = 1 / Tan(X) = Cos(X) / Sin(X)
      
      @warning Result is undefined for angles X where Sin(X) is 0 (e.g., 0, Pi, 2*Pi). May cause division by zero or return large values.
      
      @example
        var Ct: Double;
        begin
          Ct := TTrigKit.Cot(Pi / 4); // Returns: 1.0
        end; }
    class function Cot(const X: Double): Double; static;
    
    { @description Calculates hypotenuse length given two sides using Pythagorean theorem
      
      @usage Calculate the length of the hypotenuse of a right-angled triangle given the lengths of the other two sides (legs).
      
      @param A Length of the first leg.
      @param B Length of the second leg.
      
      @returns The length of the hypotenuse.
      
      @references Pythagorean theorem: c^2 = a^2 + b^2
      
      @warning Assumes inputs A and B represent lengths and are non-negative.
      
      @example
        var Hyp: Double;
        begin
          Hyp := TTrigKit.Hypotenuse(3.0, 4.0); // Returns: 5.0
        end; }
    class function Hypotenuse(const A, B: Double): Double; static;

    { @description Calculates triangle area using base and height
      
      @usage Calculate the area of any triangle when the base length and the corresponding perpendicular height are known.
      
      @param Base The length of the triangle's base.
      @param Height The perpendicular height from the base to the opposite vertex.
      
      @returns The area of the triangle.
      
      @references Formula: Area = (1/2) * Base * Height
      
      @warning Assumes inputs Base and Height represent lengths and are non-negative.
      
      @example
        var Area: Double;
        begin
          Area := TTrigKit.TriangleArea(10.0, 5.0); // Returns: 25.0
        end; }
    class function TriangleArea(const Base, Height: Double): Double; static;

    { @description Calculates triangle area using Side-Angle-Side (SAS) formula
      
      @usage Calculate the area of a triangle when the lengths of two sides and the measure of the included angle (in radians) are known.
      
      @param SideA Length of the first side.
      @param Angle The measure of the angle (in radians) between SideA and SideB.
      @param SideB Length of the second side.
      
      @returns The area of the triangle.
      
      @references Formula: Area = (1/2) * a * b * sin(C)
      
      @warning Assumes inputs SideA and SideB represent lengths and are non-negative. Angle must be in radians.
      
      @example
        var Area: Double;
        begin
          Area := TTrigKit.TriangleAreaSAS(5.0, Pi / 2, 4.0); // Returns: 10.0
        end; }
    class function TriangleAreaSAS(const SideA, Angle, SideB: Double): Double; static;

    { @description Calculates triangle area using Side-Side-Side (SSS) formula (Heron's formula)
      
      @usage Calculate the area of a triangle when the lengths of all three sides are known.
      
      @param A Length of the first side.
      @param B Length of the second side.
      @param C Length of the third side.
      
      @returns The area of the triangle.
      
      @references Heron's formula: Area = sqrt(s * (s - a) * (s - b) * (s - c)), where s is the semi-perimeter (a+b+c)/2.
      
      @warning Inputs A, B, C must form a valid triangle (sum of any two sides must be greater than the third). Returns NaN or raises exception for invalid triangles due to negative value under square root.
      
      @example
        var Area: Double;
        begin
          Area := TTrigKit.TriangleAreaSSS(3.0, 4.0, 5.0); // Returns: 6.0
        end; }
    class function TriangleAreaSSS(const A, B, C: Double): Double; static;
    
    { @description Calculates triangle perimeter
      
      @usage Calculate the perimeter (total length of all sides) of a triangle when the lengths of all three sides are known.
      
      @param A Length of the first side.
      @param B Length of the second side.
      @param C Length of the third side.
      
      @returns The perimeter of the triangle.
      
      @references Formula: Perimeter = A + B + C
      
      @warning Assumes inputs A, B, C represent lengths and are non-negative. Does not validate if sides form a valid triangle.
      
      @example
        var Perim: Double;
        begin
          Perim := TTrigKit.TrianglePerimeter(3.0, 4.0, 5.0); // Returns: 12.0
        end; }
    class function TrianglePerimeter(const A, B, C: Double): Double; static;

    { @description Calculates radius of inscribed circle (incircle)
      
      @usage Calculate the radius of the circle that can be inscribed within a triangle, tangent to all three sides.
      
      @param A Length of the first side.
      @param B Length of the second side.
      @param C Length of the third side.
      
      @returns The radius of the inscribed circle.
      
      @references Formula: r = Area / s = 2 * Area / Perimeter, where s is the semi-perimeter. Uses TriangleAreaSSS internally.
      
      @warning Inputs A, B, C must form a valid triangle. Behavior for invalid triangles depends on TriangleAreaSSS. Division by zero if perimeter is zero.
      
      @example
        var InRad: Double;
        begin
          InRad := TTrigKit.TriangleInRadius(3.0, 4.0, 5.0); // Returns: 1.0
        end; }
    class function TriangleInRadius(const A, B, C: Double): Double; static;

    { @description Calculates radius of circumscribed circle (circumcircle)
      
      @usage Calculate the radius of the circle that passes through all three vertices of a triangle.
      
      @param A Length of the first side.
      @param B Length of the second side.
      @param C Length of the third side.
      
      @returns The radius of the circumscribed circle.
      
      @references Formula: R = (a * b * c) / (4 * Area)
      
      @warning Inputs A, B, C must form a valid triangle. Behavior for invalid triangles depends on TriangleAreaSSS. Division by zero if area is zero (degenerate triangle).
      
      @example
        var CircumRad: Double;
        begin
          CircumRad := TTrigKit.TriangleCircumRadius(3.0, 4.0, 5.0); // Returns: 2.5
        end; }
    class function TriangleCircumRadius(const A, B, C: Double): Double; static;
    
    { @description Calculates area of circular sector
      
      @usage Calculate the area of a portion of a circle enclosed by two radii and an arc.
      
      @param Radius The radius of the circle.
      @param Angle The central angle of the sector in radians.
      
      @returns The area of the circular sector.
      
      @references Formula: Area = (1/2) * r^2 * Theta (where Theta is in radians)
      
      @warning Assumes Radius is non-negative. Angle must be in radians.
      
      @example
        var SectorArea: Double;
        begin
          SectorArea := TTrigKit.CircularSectorArea(10.0, Pi / 2); // Returns: ~78.54 (50 * Pi / 2)
        end; }
    class function CircularSectorArea(const Radius, Angle: Double): Double; static;

    { @description Calculates area of circular segment
      
      @usage Calculate the area of a portion of a circle enclosed by a chord and an arc.
      
      @param Radius The radius of the circle.
      @param Angle The central angle subtended by the chord in radians.
      
      @returns The area of the circular segment.
      
      @references Formula: Area = (1/2) * r^2 * (Theta - sin(Theta)) (where Theta is in radians)
      
      @warning Assumes Radius is non-negative. Angle must be in radians. Angle typically expected in [0, 2 Pi].
      
      @example
        var SegmentArea: Double;
        begin
          SegmentArea := TTrigKit.CircularSegmentArea(10.0, Pi / 2); // Returns: ~28.54 ((1/2)*100*(Pi/2 - 1))
        end; }
    class function CircularSegmentArea(const Radius, Angle: Double): Double; static;

    { @description Calculates chord length given central angle
      
      @usage Calculate the length of a line segment connecting two points on a circle's circumference, given the central angle subtended by the chord.
      
      @param Radius The radius of the circle.
      @param Angle The central angle subtended by the chord in radians.
      
      @returns The length of the chord.
      
      @references Formula: Length = 2 * r * sin(Theta / 2) (where Theta is in radians)
      
      @warning Assumes Radius is non-negative. Angle must be in radians.
      
      @example
        var Len: Double;
        begin
          Len := TTrigKit.ChordLength(10.0, Pi / 2); // Returns: ~14.14 (10 * sqrt(2))
        end; }
    class function ChordLength(const Radius, Angle: Double): Double; static;
    
    { @description Calculates magnitude (length) of a 2D vector
      
      @usage Calculate the Euclidean length of a 2D vector defined by its components (X, Y).
      
      @param X The X-component of the vector.
      @param Y The Y-component of the vector.
      
      @returns The magnitude (length) of the vector.
      
      @references Formula: Magnitude = sqrt(X^2 + Y^2)
      
      @warning None
      
      @example
        var Mag: Double;
        begin
          Mag := TTrigKit.VectorMagnitude(3.0, 4.0); // Returns: 5.0
        end; }
    class function VectorMagnitude(const X, Y: Double): Double; static;

    { @description Calculates angle of a vector defined by two points in 2D space relative to the positive X-axis.
      
      @usage Determine the angle (in radians) of the vector pointing from (X1, Y1) to (X2, Y2). Useful for finding the direction of a displacement.
      
      @param X1 X-coordinate of the start point.
      @param Y1 Y-coordinate of the start point.
      @param X2 X-coordinate of the end point.
      @param Y2 Y-coordinate of the end point.
      
      @returns The angle in radians, in the range [-Pi, Pi], relative to the positive X-axis.
      
      @references Uses Math.ArcTan2(Y2 - Y1, X2 - X1).
      
      @warning Returns 0 if start and end points are the same.
      
      @example
        var Angle: Double;
        begin
          Angle := TTrigKit.VectorAngle(0.0, 0.0, 1.0, 1.0); // Returns: Pi / 4
        end; }
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
