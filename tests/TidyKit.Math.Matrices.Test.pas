unit TidyKit.Math.Matrices.Test;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, Math,
  TidyKit.Math,
  TidyKit.Math.Matrices;

type
  { TMatricesTest }
  TMatricesTest = class(TTestCase)
  private
    const
      EPSILON = 1E-6;  // Tolerance for floating-point comparisons
      MATRIX_SIZE = 3; // Default size for test matrices
      
    { Helper methods }
    procedure CheckMatrix(const Expected, Actual: IMatrix; const Msg: String = '');
    procedure CheckMatrixValue(ExpectedValue, ActualValue: Double; const Msg: String = '');
    function CreateTestMatrix1: IMatrix;
    function CreateTestMatrix2: IMatrix;
    function CreateSingularMatrix: IMatrix;
    
  published
    { Creation tests }
    procedure Test01_CreateMatrix;
    procedure Test02_CreateFromMatrix;
    procedure Test03_Identity;
    procedure Test04_Zeros;
    procedure Test05_Ones;
    
    { Property tests }
    procedure Test06_GetRows;
    procedure Test07_GetColumns;
    procedure Test08_GetSetValue;
    
    { Operation tests }
    procedure Test09_Add;
    procedure Test10_Subtract;
    procedure Test11_Multiply;
    procedure Test12_MultiplyScalar;
    procedure Test13_Transpose;
    
    { Advanced operations }
    procedure Test14_Determinant;
    procedure Test15_Inverse;
    procedure Test16_Solve;
    
    { Utility methods }
    procedure Test17_ToString;
    procedure Test18_Clone;
    procedure Test19_ToMatrix;
    
    { Error handling tests }
    procedure Test20_AddWithMismatchedDimensions;
    procedure Test21_SubtractWithMismatchedDimensions;
    procedure Test22_MultiplyWithInvalidDimensions;
    procedure Test23_InverseOfNonSquareMatrix;
    procedure Test24_InverseOfSingularMatrix;
    procedure Test25_DeterminantOfNonSquareMatrix;
    procedure Test26_OutOfBoundsAccess;
    procedure Test27_EmptyMatrixOperations;
  end;

implementation

{ Helper methods }

procedure TMatricesTest.CheckMatrix(const Expected, Actual: IMatrix; const Msg: String);
var
  I, J: Integer;
  Delta: Double;
begin
  AssertEquals(Msg + ' - Rows match', Expected.Rows, Actual.Rows);
  AssertEquals(Msg + ' - Columns match', Expected.Columns, Actual.Columns);
  
  for I := 0 to Expected.Rows - 1 do
    for J := 0 to Expected.Columns - 1 do
    begin
      Delta := Abs(Expected.Values[I, J] - Actual.Values[I, J]);
      AssertTrue(
        Format('%s - Element at [%d,%d]: expected %f but got %f (delta %f)',
               [Msg, I, J, Expected.Values[I, J], Actual.Values[I, J], Delta]),
        Delta < EPSILON
      );
    end;
end;

procedure TMatricesTest.CheckMatrixValue(ExpectedValue, ActualValue: Double; const Msg: String);
var
  Delta: Double;
begin
  Delta := Abs(ExpectedValue - ActualValue);
  AssertTrue(
    Format('%s: expected %f but got %f (delta %f)',
           [Msg, ExpectedValue, ActualValue, Delta]),
    Delta < EPSILON
  );
end;

function TMatricesTest.CreateTestMatrix1: IMatrix;
var
  Matrix: IMatrix;
begin
  Matrix := TMatrixKit.CreateFromMatrix([
    [4.0, 3.0, 2.0],
    [1.0, 3.0, 1.0],
    [2.0, 1.0, 3.0]
  ]);
  
  Result := Matrix;
end;

function TMatricesTest.CreateTestMatrix2: IMatrix;
var
  Matrix: IMatrix;
begin
  Matrix := TMatrixKit.CreateFromMatrix([
    [1.0, 0.0, 5.0],
    [2.0, 1.0, 6.0],
    [3.0, 4.0, 0.0]
  ]);
  
  Result := Matrix;
end;

function TMatricesTest.CreateSingularMatrix: IMatrix;
var
  Matrix: IMatrix;
begin
  Matrix := TMatrixKit.CreateFromMatrix([
    [1.0, 2.0, 3.0],
    [2.0, 4.0, 6.0],  // This row is 2x the first row
    [3.0, 5.0, 8.0]
  ]);
  
  Result := Matrix;
end;

{ Test methods }

procedure TMatricesTest.Test01_CreateMatrix;
var
  M: IMatrix;
  I, J: Integer;
begin
  M := TMatrixKit.CreateIMatrix(3, 4);
  
  AssertEquals('Row count matches', 3, M.Rows);
  AssertEquals('Column count matches', 4, M.Columns);
  
  // Check that all values are initialized to 0
  for I := 0 to M.Rows - 1 do
    for J := 0 to M.Columns - 1 do
      CheckMatrixValue(0.0, M.Values[I, J], Format('InitialValue[%d,%d]', [I, J]));
end;

procedure TMatricesTest.Test02_CreateFromMatrix;
var
  RawData: TMatrix;
  M: IMatrix;
  I, J: Integer;
begin
  SetLength(RawData, 2);
  for I := 0 to 1 do
    SetLength(RawData[I], 3);
    
  RawData[0, 0] := 1.0; RawData[0, 1] := 2.0; RawData[0, 2] := 3.0;
  RawData[1, 0] := 4.0; RawData[1, 1] := 5.0; RawData[1, 2] := 6.0;
  
  M := TMatrixKit.CreateFromMatrix(RawData);
  
  AssertEquals('Row count matches', 2, M.Rows);
  AssertEquals('Column count matches', 3, M.Columns);
  
  // Check values
  CheckMatrixValue(1.0, M.Values[0, 0], 'Value[0,0]');
  CheckMatrixValue(2.0, M.Values[0, 1], 'Value[0,1]');
  CheckMatrixValue(3.0, M.Values[0, 2], 'Value[0,2]');
  CheckMatrixValue(4.0, M.Values[1, 0], 'Value[1,0]');
  CheckMatrixValue(5.0, M.Values[1, 1], 'Value[1,1]');
  CheckMatrixValue(6.0, M.Values[1, 2], 'Value[1,2]');
  
  // Clean up raw data
  for I := 0 to High(RawData) do
    SetLength(RawData[I], 0);
  SetLength(RawData, 0);
end;

procedure TMatricesTest.Test03_Identity;
var
  M: IMatrix;
  I, J: Integer;
begin
  M := TMatrixKit.CreateIdentity(3);
  
  AssertEquals('Identity matrix is square', M.Rows, M.Columns);
  
  for I := 0 to M.Rows - 1 do
    for J := 0 to M.Columns - 1 do
      if I = J then
        CheckMatrixValue(1.0, M.Values[I, J], Format('Identity[%d,%d] = 1', [I, J]))
      else
        CheckMatrixValue(0.0, M.Values[I, J], Format('Identity[%d,%d] = 0', [I, J]));
end;

procedure TMatricesTest.Test04_Zeros;
var
  M: IMatrix;
  I, J: Integer;
begin
  M := TMatrixKit.CreateZeros(2, 3);
  
  AssertEquals('Row count matches', 2, M.Rows);
  AssertEquals('Column count matches', 3, M.Columns);
  
  for I := 0 to M.Rows - 1 do
    for J := 0 to M.Columns - 1 do
      CheckMatrixValue(0.0, M.Values[I, J], Format('Zeros[%d,%d]', [I, J]));
end;

procedure TMatricesTest.Test05_Ones;
var
  M: IMatrix;
  I, J: Integer;
begin
  M := TMatrixKit.CreateOnes(2, 3);
  
  AssertEquals('Row count matches', 2, M.Rows);
  AssertEquals('Column count matches', 3, M.Columns);
  
  for I := 0 to M.Rows - 1 do
    for J := 0 to M.Columns - 1 do
      CheckMatrixValue(1.0, M.Values[I, J], Format('Ones[%d,%d]', [I, J]));
end;

procedure TMatricesTest.Test06_GetRows;
var
  M: IMatrix;
begin
  M := TMatrixKit.CreateIMatrix(4, 3);
  AssertEquals('Row count matches', 4, M.Rows);
end;

procedure TMatricesTest.Test07_GetColumns;
var
  M: IMatrix;
begin
  M := TMatrixKit.CreateIMatrix(4, 3);
  AssertEquals('Column count matches', 3, M.Columns);
end;

procedure TMatricesTest.Test08_GetSetValue;
var
  M: IMatrix;
begin
  M := TMatrixKit.CreateIMatrix(2, 2);
  
  // Set values
  M.Values[0, 0] := 1.5;
  M.Values[0, 1] := 2.7;
  M.Values[1, 0] := 3.8;
  M.Values[1, 1] := 4.2;
  
  // Get values
  CheckMatrixValue(1.5, M.Values[0, 0], 'Value[0,0]');
  CheckMatrixValue(2.7, M.Values[0, 1], 'Value[0,1]');
  CheckMatrixValue(3.8, M.Values[1, 0], 'Value[1,0]');
  CheckMatrixValue(4.2, M.Values[1, 1], 'Value[1,1]');
end;

procedure TMatricesTest.Test09_Add;
var
  A, B, C, Expected: IMatrix;
begin
  A := CreateTestMatrix1;
  B := CreateTestMatrix2;
  
  C := A.Add(B);
  
  Expected := TMatrixKit.CreateFromMatrix([
    [5.0, 3.0, 7.0],
    [3.0, 4.0, 7.0],
    [5.0, 5.0, 3.0]
  ]);
  
  CheckMatrix(Expected, C, 'Matrix addition');
end;

procedure TMatricesTest.Test10_Subtract;
var
  A, B, C, Expected: IMatrix;
begin
  A := CreateTestMatrix1;
  B := CreateTestMatrix2;
  
  C := A.Subtract(B);
  
  Expected := TMatrixKit.CreateFromMatrix([
    [3.0, 3.0, -3.0],
    [-1.0, 2.0, -5.0],
    [-1.0, -3.0, 3.0]
  ]);
  
  CheckMatrix(Expected, C, 'Matrix subtraction');
end;

procedure TMatricesTest.Test11_Multiply;
var
  A, B, C, Expected: IMatrix;
begin
  A := CreateTestMatrix1;
  B := CreateTestMatrix2;
  
  C := A.Multiply(B);
  
  // Fix the expected values to match the correct matrix multiplication result
  Expected := TMatrixKit.CreateFromMatrix([
    [16.0, 11.0, 38.0],
    [10.0, 7.0, 23.0],
    [13.0, 13.0, 16.0]
  ]);
  
  CheckMatrix(Expected, C, 'Matrix multiplication');
end;

procedure TMatricesTest.Test12_MultiplyScalar;
var
  A, C, Expected: IMatrix;
begin
  A := CreateTestMatrix1;
  
  C := A.MultiplyScalar(2.5);
  
  Expected := TMatrixKit.CreateFromMatrix([
    [10.0, 7.5, 5.0],
    [2.5, 7.5, 2.5],
    [5.0, 2.5, 7.5]
  ]);
  
  CheckMatrix(Expected, C, 'Scalar multiplication');
end;

procedure TMatricesTest.Test13_Transpose;
var
  A, C, Expected: IMatrix;
begin
  A := TMatrixKit.CreateFromMatrix([
    [1.0, 2.0, 3.0],
    [4.0, 5.0, 6.0]
  ]);
  
  C := A.Transpose;
  
  Expected := TMatrixKit.CreateFromMatrix([
    [1.0, 4.0],
    [2.0, 5.0],
    [3.0, 6.0]
  ]);
  
  CheckMatrix(Expected, C, 'Matrix transpose');
end;

procedure TMatricesTest.Test14_Determinant;
var
  A: IMatrix;
  Det: Double;
begin
  A := TMatrixKit.CreateFromMatrix([
    [4.0, 3.0, 2.0],
    [1.0, 3.0, 1.0],
    [2.0, 1.0, 3.0]
  ]);
  
  Det := A.Determinant;
  
  // Det = 4(3*3-1*1) - 3(1*3-1*2) + 2(1*1-3*2) = 4*8 - 3*1 + 2*(-5) = 32 - 3 - 10 = 19
  CheckMatrixValue(19.0, Det, 'Determinant');
end;

procedure TMatricesTest.Test15_Inverse;
var
  A, AInv, Product, Expected: IMatrix;
begin
  A := CreateTestMatrix1;
  AInv := A.Inverse;
  
  // Verify that A * A^-1 = I
  Product := A.Multiply(AInv);
  Expected := TMatrixKit.CreateIdentity(3);
  
  CheckMatrix(Expected, Product, 'A * A^-1 = I');
end;

procedure TMatricesTest.Test16_Solve;
var
  A, B, X, AxB: IMatrix;
begin
  A := CreateTestMatrix1;
  B := TMatrixKit.CreateFromMatrix([
    [1.0],
    [2.0],
    [3.0]
  ]);
  
  X := A.Solve(B);
  
  // Verify that A * X = B
  AxB := A.Multiply(X);
  
  CheckMatrix(B, AxB, 'A * X = B');
end;

procedure TMatricesTest.Test17_ToString;
var
  A: IMatrix;
  Str: String;
begin
  A := TMatrixKit.CreateFromMatrix([
    [1.0, 2.0],
    [3.0, 4.0]
  ]);
  
  Str := A.ToString;
  
  AssertTrue('ToString contains matrix data', Pos('1', Str) > 0);
  AssertTrue('ToString contains matrix data', Pos('2', Str) > 0);
  AssertTrue('ToString contains matrix data', Pos('3', Str) > 0);
  AssertTrue('ToString contains matrix data', Pos('4', Str) > 0);
end;

procedure TMatricesTest.Test18_Clone;
var
  A, Clone: IMatrix;
begin
  A := CreateTestMatrix1;
  Clone := A.Clone;
  
  // Verify clone has same values
  CheckMatrix(A, Clone, 'Clone has same values');
  
  // Modify the clone and verify original is unchanged
  Clone.Values[0, 0] := 99.0;
  AssertFalse('Original unchanged after clone modified', 
              Abs(A.Values[0, 0] - 99.0) < EPSILON);
end;

procedure TMatricesTest.Test19_ToMatrix;
var
  A: IMatrix;
  M: TMatrix;
begin
  A := CreateTestMatrix1;
  M := A.ToMatrix;
  
  // Verify the raw matrix has correct dimensions
  AssertEquals('TMatrix row count', 3, Length(M));
  AssertEquals('TMatrix column count', 3, Length(M[0]));
  
  // Verify some values
  CheckMatrixValue(4.0, M[0, 0], 'TMatrix[0,0]');
  CheckMatrixValue(3.0, M[0, 1], 'TMatrix[0,1]');
  CheckMatrixValue(2.0, M[0, 2], 'TMatrix[0,2]');
  
  // Clean up raw matrix
  M := nil;
end;

procedure TMatricesTest.Test20_AddWithMismatchedDimensions;
var
  A, B: IMatrix;
begin
  A := TMatrixKit.CreateIMatrix(2, 3);
  B := TMatrixKit.CreateIMatrix(2, 2);
  
  try
    A.Add(B);
    Fail('EMatrixError expected - mismatched dimensions in Add');
  except
    on E: EMatrixError do
      ; // Expected exception
    on E: Exception do
      Fail('Wrong exception type raised: ' + E.ClassName);
  end;
end;

procedure TMatricesTest.Test21_SubtractWithMismatchedDimensions;
var
  A, B: IMatrix;
begin
  A := TMatrixKit.CreateIMatrix(2, 3);
  B := TMatrixKit.CreateIMatrix(2, 2);
  
  try
    A.Subtract(B);
    Fail('EMatrixError expected - mismatched dimensions in Subtract');
  except
    on E: EMatrixError do
      ; // Expected exception
    on E: Exception do
      Fail('Wrong exception type raised: ' + E.ClassName);
  end;
end;

procedure TMatricesTest.Test22_MultiplyWithInvalidDimensions;
var
  A, B: IMatrix;
begin
  A := TMatrixKit.CreateIMatrix(2, 3);
  B := TMatrixKit.CreateIMatrix(2, 2);
  
  try
    A.Multiply(B);
    Fail('EMatrixError expected - invalid dimensions in Multiply');
  except
    on E: EMatrixError do
      ; // Expected exception
    on E: Exception do
      Fail('Wrong exception type raised: ' + E.ClassName);
  end;
end;

procedure TMatricesTest.Test23_InverseOfNonSquareMatrix;
var
  A: IMatrix;
begin
  A := TMatrixKit.CreateIMatrix(2, 3);
  
  try
    A.Inverse;
    Fail('EMatrixError expected - non-square matrix inversion');
  except
    on E: EMatrixError do
      ; // Expected exception
    on E: Exception do
      Fail('Wrong exception type raised: ' + E.ClassName);
  end;
end;

procedure TMatricesTest.Test24_InverseOfSingularMatrix;
var
  A: IMatrix;
begin
  A := CreateSingularMatrix;
  
  try
    A.Inverse;
    Fail('EMatrixError expected - singular matrix inversion');
  except
    on E: EMatrixError do
      ; // Expected exception
    on E: Exception do
      Fail('Wrong exception type raised: ' + E.ClassName);
  end;
end;

procedure TMatricesTest.Test25_DeterminantOfNonSquareMatrix;
var
  A: IMatrix;
begin
  A := TMatrixKit.CreateIMatrix(2, 3);
  
  try
    A.Determinant;
    Fail('EMatrixError expected - determinant of non-square matrix');
  except
    on E: EMatrixError do
      ; // Expected exception
    on E: Exception do
      Fail('Wrong exception type raised: ' + E.ClassName);
  end;
end;

procedure TMatricesTest.Test26_OutOfBoundsAccess;
var
  A: IMatrix;
  Dummy: Double;
begin
  A := TMatrixKit.CreateIMatrix(2, 3);
  
  try
    Dummy := A.Values[2, 1];
    Fail('EMatrixError expected - out-of-bounds row access');
  except
    on E: EMatrixError do
      ; // Expected exception
    on E: Exception do
      Fail('Wrong exception type raised: ' + E.ClassName);
  end;
  
  try
    Dummy := A.Values[1, 3];
    Fail('EMatrixError expected - out-of-bounds column access');
  except
    on E: EMatrixError do
      ; // Expected exception
    on E: Exception do
      Fail('Wrong exception type raised: ' + E.ClassName);
  end;
  
  try
    A.Values[2, 1] := 1.0;
    Fail('EMatrixError expected - out-of-bounds row assignment');
  except
    on E: EMatrixError do
      ; // Expected exception
    on E: Exception do
      Fail('Wrong exception type raised: ' + E.ClassName);
  end;
  
  try
    A.Values[1, 3] := 1.0;
    Fail('EMatrixError expected - out-of-bounds column assignment');
  except
    on E: EMatrixError do
      ; // Expected exception
    on E: Exception do
      Fail('Wrong exception type raised: ' + E.ClassName);
  end;
end;

procedure TMatricesTest.Test27_EmptyMatrixOperations;
var
  A, B, C: IMatrix;
begin
  A := TMatrixKit.CreateIMatrix(0, 0);
  
  AssertEquals('Empty matrix rows', 0, A.Rows);
  AssertEquals('Empty matrix columns', 0, A.Columns);
  
  // Test operating with empty matrices
  B := TMatrixKit.CreateIMatrix(0, 0);
  
  try
    C := A.Add(B);
    AssertEquals('Empty matrix add rows', 0, C.Rows);
    AssertEquals('Empty matrix add columns', 0, C.Columns);
  except
    Fail('Empty matrix add failed');
  end;
  
  try
    C := A.Subtract(B);
    AssertEquals('Empty matrix subtract rows', 0, C.Rows);
    AssertEquals('Empty matrix subtract columns', 0, C.Columns);
  except
    Fail('Empty matrix subtract failed');
  end;
  
  try
    C := A.MultiplyScalar(2.0);
    AssertEquals('Empty matrix scalar multiply rows', 0, C.Rows);
    AssertEquals('Empty matrix scalar multiply columns', 0, C.Columns);
  except
    Fail('Empty matrix scalar multiply failed');
  end;
  
  try
    C := A.Transpose;
    AssertEquals('Empty matrix transpose rows', 0, C.Rows);
    AssertEquals('Empty matrix transpose columns', 0, C.Columns);
  except
    Fail('Empty matrix transpose failed');
  end;
  
  try
    A.ToString;
  except
    Fail('Empty matrix toString failed');
  end;
end;

initialization
  RegisterTest(TMatricesTest);
end. 