unit TidyKit.Math;

{$mode objfpc}{$H+}{$J-}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Math;

type
  { Array types for different numeric data }
  TIntegerArray = array of Integer;
  TDoubleArray = array of Double;
  TSingleArray = array of Single;
  TExtendedArray = array of Extended;
  
  { Matrix type used in matrix operations }
  TMatrix = array of array of Double;

{ Array conversion functions }
function ToDoubleArray(const Data: TIntegerArray): TDoubleArray;
function ToDoubleArray(const Data: TSingleArray): TDoubleArray;
function ToDoubleArray(const Data: TExtendedArray): TDoubleArray;

implementation

function ToDoubleArray(const Data: TIntegerArray): TDoubleArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Data));
  for I := 0 to High(Data) do
    Result[I] := Data[I];
end;

function ToDoubleArray(const Data: TSingleArray): TDoubleArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Data));
  for I := 0 to High(Data) do
    Result[I] := Data[I];
end;

function ToDoubleArray(const Data: TExtendedArray): TDoubleArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Data));
  for I := 0 to High(Data) do
    Result[I] := Data[I];
end;

end. 
