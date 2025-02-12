unit TidyKit.Math;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, Math;

type
  generic TNumeric<T> = record
  public
    class function Zero: T; static; abstract;
    class function One: T; static; abstract;
    class function Add(const A, B: T): T; static; abstract;
    class function Subtract(const A, B: T): T; static; abstract;
    class function Multiply(const A, B: T): T; static; abstract;
    class function Divide(const A, B: T): T; static; abstract;
  end;

  TDoubleNumeric = specialize TNumeric<Double>;
  TSingleNumeric = specialize TNumeric<Single>;
  TExtendedNumeric = specialize TNumeric<Extended>;
  
  { Matrix type used in matrix operations }
  generic TMatrix<T> = array of array of T;

implementation

end. 