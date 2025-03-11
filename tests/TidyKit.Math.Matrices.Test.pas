unit TidyKit.Math.Matrices.Test;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Math, TidyKit.Math.Matrices;

type
  TMatrixTest = class(TTestCase)
  published
    procedure Test01_CreateFromArray;
    procedure Test02_Identity;
    procedure Test03_Zeros;
    procedure Test04_Ones;
    procedure Test05_Add;
    procedure Test06_Subtract;
    procedure Test07_Multiply;
    procedure Test08_ScalarMultiply;
    procedure Test09_Transpose;
    procedure Test10_Determinant;
    procedure Test11_Trace;
    procedure Test12_Rank;
    procedure Test13_Inverse;
    procedure Test14_LUDecomposition;
    procedure Test15_QRDecomposition;
    procedure Test16_EigenDecomposition;
    procedure Test17_SingularMatrix;
    procedure Test18_ToString;
    procedure Test19_MatrixNorms;
    procedure Test20_SpecialMatrices;
    procedure Test21_MatrixProperties;
    procedure Test22_BlockOperations;
    procedure Test23_ElementWiseOperations;
    procedure Test24_EdgeCases;
    procedure Test25_ErrorConditions;
    procedure Test26_AdditionalProperties;
  end;

implementation

procedure TMatrixTest.Test01_CreateFromArray;
var
  M: IMatrix;
begin
  M := TMatrixKit.CreateFromArray([
    [1.0, 2.0, 3.0],
    [4.0, 5.0, 6.0],
    [7.0, 8.0, 9.0]
  ]);
  
  AssertEquals('Rows count', 3, M.Rows);
  AssertEquals('Cols count', 3, M.Cols);
  AssertEquals('Value at [0,0]', 1.0, M.Values[0, 0]);
  AssertEquals('Value at [1,1]', 5.0, M.Values[1, 1]);
  AssertEquals('Value at [2,2]', 9.0, M.Values[2, 2]);
end;

procedure TMatrixTest.Test02_Identity;
var
  M: IMatrix;
begin
  M := TMatrixKit.Identity(3);
  
  AssertEquals('Identity size', 3, M.Rows);
  AssertEquals('Diagonal element [0,0]', 1.0, M.Values[0, 0]);
  AssertEquals('Diagonal element [1,1]', 1.0, M.Values[1, 1]);
  AssertEquals('Diagonal element [2,2]', 1.0, M.Values[2, 2]);
  AssertEquals('Off-diagonal element [0,1]', 0.0, M.Values[0, 1]);
  AssertEquals('Off-diagonal element [1,0]', 0.0, M.Values[1, 0]);
end;

procedure TMatrixTest.Test03_Zeros;
var
  M: IMatrix;
begin
  M := TMatrixKit.Zeros(2, 3);
  
  AssertEquals('Rows count', 2, M.Rows);
  AssertEquals('Cols count', 3, M.Cols);
  AssertEquals('Element [0,0]', 0.0, M.Values[0, 0]);
  AssertEquals('Element [1,2]', 0.0, M.Values[1, 2]);
end;

procedure TMatrixTest.Test04_Ones;
var
  M: IMatrix;
begin
  M := TMatrixKit.Ones(2, 3);
  
  AssertEquals('Rows count', 2, M.Rows);
  AssertEquals('Cols count', 3, M.Cols);
  AssertEquals('Element [0,0]', 1.0, M.Values[0, 0]);
  AssertEquals('Element [1,2]', 1.0, M.Values[1, 2]);
end;

procedure TMatrixTest.Test05_Add;
var
  A, B, C: IMatrix;
begin
  A := TMatrixKit.CreateFromArray([
    [1.0, 2.0],
    [3.0, 4.0]
  ]);
  
  B := TMatrixKit.CreateFromArray([
    [5.0, 6.0],
    [7.0, 8.0]
  ]);
  
  C := A.Add(B);
  
  AssertEquals('Sum [0,0]', 6.0, C.Values[0, 0]);
  AssertEquals('Sum [0,1]', 8.0, C.Values[0, 1]);
  AssertEquals('Sum [1,0]', 10.0, C.Values[1, 0]);
  AssertEquals('Sum [1,1]', 12.0, C.Values[1, 1]);
end;

procedure TMatrixTest.Test06_Subtract;
var
  A, B, C: IMatrix;
begin
  A := TMatrixKit.CreateFromArray([
    [5.0, 6.0],
    [7.0, 8.0]
  ]);
  
  B := TMatrixKit.CreateFromArray([
    [1.0, 2.0],
    [3.0, 4.0]
  ]);
  
  C := A.Subtract(B);
  
  AssertEquals('Difference [0,0]', 4.0, C.Values[0, 0]);
  AssertEquals('Difference [0,1]', 4.0, C.Values[0, 1]);
  AssertEquals('Difference [1,0]', 4.0, C.Values[1, 0]);
  AssertEquals('Difference [1,1]', 4.0, C.Values[1, 1]);
end;

procedure TMatrixTest.Test07_Multiply;
var
  A, B, C: IMatrix;
begin
  A := TMatrixKit.CreateFromArray([
    [1.0, 2.0],
    [3.0, 4.0]
  ]);
  
  B := TMatrixKit.CreateFromArray([
    [5.0, 6.0],
    [7.0, 8.0]
  ]);
  
  C := A.Multiply(B);
  
  AssertEquals('Product [0,0]', 19.0, C.Values[0, 0]);
  AssertEquals('Product [0,1]', 22.0, C.Values[0, 1]);
  AssertEquals('Product [1,0]', 43.0, C.Values[1, 0]);
  AssertEquals('Product [1,1]', 50.0, C.Values[1, 1]);
end;

procedure TMatrixTest.Test08_ScalarMultiply;
var
  A, B: IMatrix;
begin
  A := TMatrixKit.CreateFromArray([
    [1.0, 2.0],
    [3.0, 4.0]
  ]);
  
  B := A.ScalarMultiply(2.0);
  
  AssertEquals('Scaled [0,0]', 2.0, B.Values[0, 0]);
  AssertEquals('Scaled [0,1]', 4.0, B.Values[0, 1]);
  AssertEquals('Scaled [1,0]', 6.0, B.Values[1, 0]);
  AssertEquals('Scaled [1,1]', 8.0, B.Values[1, 1]);
end;

procedure TMatrixTest.Test09_Transpose;
var
  A, B: IMatrix;
begin
  A := TMatrixKit.CreateFromArray([
    [1.0, 2.0, 3.0],
    [4.0, 5.0, 6.0]
  ]);
  
  B := A.Transpose;
  
  AssertEquals('Transposed rows', 3, B.Rows);
  AssertEquals('Transposed cols', 2, B.Cols);
  AssertEquals('Transposed [0,0]', 1.0, B.Values[0, 0]);
  AssertEquals('Transposed [1,0]', 2.0, B.Values[1, 0]);
  AssertEquals('Transposed [2,0]', 3.0, B.Values[2, 0]);
  AssertEquals('Transposed [0,1]', 4.0, B.Values[0, 1]);
  AssertEquals('Transposed [1,1]', 5.0, B.Values[1, 1]);
  AssertEquals('Transposed [2,1]', 6.0, B.Values[2, 1]);
end;

procedure TMatrixTest.Test10_Determinant;
var
  A: IMatrix;
begin
  A := TMatrixKit.CreateFromArray([
    [1.0, 2.0],
    [3.0, 4.0]
  ]);
  
  AssertEquals('Determinant', -2.0, A.Determinant);
end;

procedure TMatrixTest.Test11_Trace;
var
  A: IMatrix;
begin
  A := TMatrixKit.CreateFromArray([
    [1.0, 2.0],
    [3.0, 4.0]
  ]);
  
  AssertEquals('Trace', 5.0, A.Trace);
end;

procedure TMatrixTest.Test12_Rank;
var
  A: IMatrix;
begin
  // Full rank matrix
  A := TMatrixKit.CreateFromArray([
    [1.0, 0.0, 0.0],
    [0.0, 1.0, 0.0],
    [0.0, 0.0, 1.0]
  ]);
  AssertEquals('Full rank matrix', 3, A.Rank);
  
  // Rank 2 matrix
  A := TMatrixKit.CreateFromArray([
    [1.0, 0.0, 1.0],
    [0.0, 1.0, 0.0],
    [2.0, 0.0, 2.0]
  ]);
  AssertEquals('Rank 2 matrix', 2, A.Rank);
  
  // Rank 1 matrix
  A := TMatrixKit.CreateFromArray([
    [1.0, 2.0, 3.0],
    [2.0, 4.0, 6.0],
    [3.0, 6.0, 9.0]
  ]);
  AssertEquals('Rank 1 matrix', 1, A.Rank);
end;

procedure TMatrixTest.Test13_Inverse;
var
  A, B, C: IMatrix;
  I, J: Integer;
  Tolerance: Double;
begin
  A := TMatrixKit.CreateFromArray([
    [4.0, 7.0],
    [2.0, 6.0]
  ]);
  
  B := A.Inverse;
  C := A.Multiply(B);  // Should be identity matrix
  
  Tolerance := 1E-12;
  for I := 0 to 1 do
    for J := 0 to 1 do
      if I = J then
        AssertTrue('Identity diagonal element', Abs(C.Values[I, J] - 1.0) < Tolerance)
      else
        AssertTrue('Identity off-diagonal element', Abs(C.Values[I, J]) < Tolerance);
end;

procedure TMatrixTest.Test14_LUDecomposition;
var
  A: IMatrix;
  LU: TLUDecomposition;
  Product, Original: IMatrix;
  I, J: Integer;
  Tolerance: Double;
begin
  A := TMatrixKit.CreateFromArray([
    [4.0, 3.0],
    [6.0, 3.0]
  ]);
  
  LU := A.LU;
  Product := LU.L.Multiply(LU.U);
  
  // Reconstruct original matrix considering permutation
  Original := TMatrixKit.Zeros(2, 2);
  for I := 0 to 1 do
    for J := 0 to 1 do
      Original.Values[LU.P[I], J] := A.Values[I, J];
  
  Tolerance := 1E-12;
  for I := 0 to 1 do
    for J := 0 to 1 do
      AssertTrue('LU product matches original', 
        Abs(Product.Values[I, J] - Original.Values[I, J]) < Tolerance);
end;

procedure TMatrixTest.Test15_QRDecomposition;
var
  A: IMatrix;
  QR: TQRDecomposition;
  Product: IMatrix;
  I, J: Integer;
  Tolerance: Double;
begin
  A := TMatrixKit.CreateFromArray([
    [12.0, -51.0],
    [6.0, 167.0],
    [-4.0, 24.0]
  ]);
  
  QR := A.QR;
  Product := QR.Q.Multiply(QR.R);
  
  // Check if Q*R equals original matrix
  Tolerance := 1E-12;
  for I := 0 to A.Rows - 1 do
    for J := 0 to A.Cols - 1 do
      AssertTrue('QR product matches original', 
        Abs(Product.Values[I, J] - A.Values[I, J]) < Tolerance);
        
  // Check if Q is orthogonal (Q^T * Q = I)
  Product := QR.Q.Transpose.Multiply(QR.Q);
  for I := 0 to A.Cols - 1 do
    for J := 0 to A.Cols - 1 do
      if I = J then
        AssertTrue('Q orthogonal diagonal', Abs(Product.Values[I, J] - 1.0) < Tolerance)
      else
        AssertTrue('Q orthogonal off-diagonal', Abs(Product.Values[I, J]) < Tolerance);
end;

procedure TMatrixTest.Test16_EigenDecomposition;
var
  A: IMatrix;
  Eigen: TEigenDecomposition;
  Tolerance: Double;
begin
  // Test with a simple 2x2 matrix with known eigenvalues
  A := TMatrixKit.CreateFromArray([
    [3.0, -2.0],
    [1.0, 4.0]
  ]);
  
  Eigen := A.EigenDecomposition;
  Tolerance := 1E-6;  // Larger tolerance due to iterative nature
  
  // Sort eigenvalues for consistent testing
  if Eigen.EigenValues[0] > Eigen.EigenValues[1] then
  begin
    AssertTrue('Larger eigenvalue', Abs(Eigen.EigenValues[0] - 5.0) < Tolerance);
    AssertTrue('Smaller eigenvalue', Abs(Eigen.EigenValues[1] - 2.0) < Tolerance);
  end
  else
  begin
    AssertTrue('Larger eigenvalue', Abs(Eigen.EigenValues[1] - 5.0) < Tolerance);
    AssertTrue('Smaller eigenvalue', Abs(Eigen.EigenValues[0] - 2.0) < Tolerance);
  end;
end;

procedure TMatrixTest.Test17_SingularMatrix;
var
  A: IMatrix;
begin
  A := TMatrixKit.CreateFromArray([
    [1.0, 1.0],
    [1.0, 1.0]
  ]);

  try
    A.Inverse;
    Fail('Should raise EMatrixError for singular matrix');
  except
    on E: EMatrixError do
      ; // Expected exception
  end;
  
  try
    A.LU;
    Fail('Should raise EMatrixError for singular matrix in LU decomposition');
  except
    on E: EMatrixError do
      ; // Expected exception
  end;
end;

procedure TMatrixTest.Test18_ToString;
var
  M: IMatrix;
  Expected, Actual: string;
begin
  // Test 2x2 matrix with different number formats
  M := TMatrixKit.CreateFromArray([
    [1.0, -2.5],
    [3.14159, 0.1]
  ]);

  // Column 1 width = 7 (length of "3.14159")
  // Column 2 width = 4 (length of "-2.5")
  Expected := '|      1 -2.5|' + sLineBreak +
              '|3.14159  0.1|';
  Actual := M.ToString;
  AssertEquals('Matrix string representation', Expected, Actual);
end;

procedure TMatrixTest.Test19_MatrixNorms;
var
  M: IMatrix;
  Tolerance: Double;
begin
  M := TMatrixKit.CreateFromArray([
    [1.0, -2.0],
    [3.0,  4.0]
  ]);
  
  Tolerance := 1E-12;
  
  // NormOne (maximum column sum)
  // Column 1: |1| + |3| = 4
  // Column 2: |-2| + |4| = 6
  // Maximum = 6
  AssertTrue('Matrix one norm', 
    Abs(M.NormOne - 6.0) < Tolerance);
  
  // NormInf (maximum row sum)
  // Row 1: |1| + |-2| = 3
  // Row 2: |3| + |4| = 7
  // Maximum = 7
  AssertTrue('Matrix infinity norm', 
    Abs(M.NormInf - 7.0) < Tolerance);
  
  // NormFrobenius (square root of sum of squares)
  // sqrt(1² + (-2)² + 3² + 4²) = sqrt(30)
  AssertTrue('Matrix Frobenius norm', 
    Abs(M.NormFrobenius - Sqrt(30)) < Tolerance);
end;

procedure TMatrixTest.Test20_SpecialMatrices;
var
  M: IMatrix;
  Diagonal: array[0..2] of Double;
begin
  // Test band matrix
  M := TMatrixKit.CreateBandMatrix(3, 1, 1);
  AssertEquals('Band matrix size', 3, M.Rows);
  AssertEquals('Band matrix [0,0]', 1.0, M.Values[0, 0]);
  AssertEquals('Band matrix [0,1]', 1.0, M.Values[0, 1]);
  AssertEquals('Band matrix [0,2]', 0.0, M.Values[0, 2]);
  AssertEquals('Band matrix [1,0]', 1.0, M.Values[1, 0]);
  
  // Test symmetric matrix
  M := TMatrixKit.CreateSymmetric([
    [1.0, 2.0],
    [2.0, 3.0]
  ]);
  AssertEquals('Symmetric matrix size', 2, M.Rows);
  AssertEquals('Symmetric matrix [0,1]', 2.0, M.Values[0, 1]);
  AssertEquals('Symmetric matrix [1,0]', 2.0, M.Values[1, 0]);
  
  // Test diagonal matrix
  Diagonal[0] := 1.0;
  Diagonal[1] := 2.0;
  Diagonal[2] := 3.0;
  M := TMatrixKit.CreateDiagonal(Diagonal);
  AssertEquals('Diagonal matrix size', 3, M.Rows);
  AssertEquals('Diagonal matrix [0,0]', 1.0, M.Values[0, 0]);
  AssertEquals('Diagonal matrix [1,1]', 2.0, M.Values[1, 1]);
  AssertEquals('Diagonal matrix [0,1]', 0.0, M.Values[0, 1]);
  
  // Test random matrix
  M := TMatrixKit.CreateRandom(2, 2, 0.0, 1.0);
  AssertEquals('Random matrix size', 2, M.Rows);
  AssertTrue('Random matrix range', (M.Values[0, 0] >= 0.0) and (M.Values[0, 0] <= 1.0));
end;

procedure TMatrixTest.Test21_MatrixProperties;
var
  M: IMatrix;
begin
  // Test symmetric property
  M := TMatrixKit.CreateSymmetric([
    [1.0, 2.0],
    [2.0, 3.0]
  ]);
  AssertTrue('IsSymmetric true case', M.IsSymmetric);
  
  M := TMatrixKit.CreateFromArray([
    [1.0, 2.0],
    [3.0, 4.0]
  ]);
  AssertFalse('IsSymmetric false case', M.IsSymmetric);
  
  // Test diagonal property
  M := TMatrixKit.CreateDiagonal([1.0, 2.0]);
  AssertTrue('IsDiagonal true case', M.IsDiagonal);
  
  M := TMatrixKit.CreateFromArray([
    [1.0, 2.0],
    [0.0, 3.0]
  ]);
  AssertFalse('IsDiagonal false case', M.IsDiagonal);
  
  // Test triangular property
  M := TMatrixKit.CreateFromArray([
    [1.0, 2.0],
    [0.0, 3.0]
  ]);
  AssertTrue('IsTriangular upper true case', M.IsTriangular(True));
  AssertFalse('IsTriangular lower false case', M.IsTriangular(False));
end;

procedure TMatrixTest.Test22_BlockOperations;
var
  M, Sub: IMatrix;
begin
  M := TMatrixKit.CreateFromArray([
    [1.0, 2.0, 3.0],
    [4.0, 5.0, 6.0],
    [7.0, 8.0, 9.0]
  ]);
  
  // Test GetSubMatrix
  Sub := M.GetSubMatrix(0, 0, 2, 2);
  AssertEquals('SubMatrix size', 2, Sub.Rows);
  AssertEquals('SubMatrix [0,0]', 1.0, Sub.Values[0, 0]);
  AssertEquals('SubMatrix [1,1]', 5.0, Sub.Values[1, 1]);
  
  // Test SetSubMatrix
  Sub := TMatrixKit.CreateFromArray([
    [10.0, 11.0],
    [12.0, 13.0]
  ]);
  M.SetSubMatrix(0, 0, Sub);
  AssertEquals('SetSubMatrix [0,0]', 10.0, M.Values[0, 0]);
  AssertEquals('SetSubMatrix [1,1]', 13.0, M.Values[1, 1]);
end;

procedure TMatrixTest.Test23_ElementWiseOperations;
var
  A, B, C: IMatrix;
begin
  A := TMatrixKit.CreateFromArray([
    [1.0, 2.0],
    [3.0, 4.0]
  ]);
  
  B := TMatrixKit.CreateFromArray([
    [2.0, 3.0],
    [4.0, 5.0]
  ]);
  
  // Test element-wise multiplication
  C := A.ElementWiseMultiply(B);
  AssertEquals('ElementWiseMultiply [0,0]', 2.0, C.Values[0, 0]);
  AssertEquals('ElementWiseMultiply [1,1]', 20.0, C.Values[1, 1]);
  
  // Test element-wise division
  C := A.ElementWiseDivide(B);
  AssertEquals('ElementWiseDivide [0,0]', 0.5, C.Values[0, 0]);
  AssertEquals('ElementWiseDivide [1,1]', 0.8, C.Values[1, 1]);
end;

procedure TMatrixTest.Test24_EdgeCases;
var
  M: IMatrix;
  I, J: Integer;
  LargeSize: Integer;
begin
  // Test 1x1 matrix
  M := TMatrixKit.CreateFromArray([[42.0]]);
  AssertEquals('1x1 matrix size', 1, M.Rows);
  AssertEquals('1x1 matrix value', 42.0, M.Values[0, 0]);
  
  // Test empty matrix (0x0)
  M := TMatrixKit.Zeros(0, 0);
  AssertEquals('Empty matrix rows', 0, M.Rows);
  AssertEquals('Empty matrix cols', 0, M.Cols);
  
  // Test large matrix (100x100)
  LargeSize := 100;
  M := TMatrixKit.Zeros(LargeSize, LargeSize);
  AssertEquals('Large matrix size', LargeSize, M.Rows);
  
  // Test extreme values
  M := TMatrixKit.CreateFromArray([
    [1.0E10, 1.0],
    [1.0, -1.0E10]
  ]);
  AssertTrue('Extreme max value within tolerance', 
    Abs(M.Values[0, 0] - 1.0E10) < 1.0E-6);
  AssertTrue('Extreme min value within tolerance', 
    Abs(M.Values[1, 1] - (-1.0E10)) < 1.0E-6);
end;

procedure TMatrixTest.Test25_ErrorConditions;
var
  M, Other: IMatrix;
begin
  M := TMatrixKit.CreateFromArray([
    [1.0, 2.0],
    [3.0, 4.0]
  ]);
  
  // Test invalid dimensions for operations
  Other := TMatrixKit.CreateFromArray([[1.0]]);
  try
    M.Add(Other);
    Fail('Should raise EMatrixError for mismatched dimensions in Add');
  except
    on E: EMatrixError do
      ; // Expected exception
  end;
  
  // Test element-wise division by zero
  Other := TMatrixKit.Zeros(2, 2);
  try
    M.ElementWiseDivide(Other);
    Fail('Should raise exception for division by zero');
  except
    on E: EMatrixError do
      ; // Expected exception
    on E: EZeroDivide do
      ; // Also acceptable
  end;
  
  // Test invalid submatrix access
  try
    M.GetSubMatrix(-1, 0, 1, 1);
    Fail('Should raise EMatrixError for invalid submatrix indices');
  except
    on E: EMatrixError do
      ; // Expected exception
  end;
  
  // Test invalid matrix creation
  try
    M := TMatrixKit.CreateFromArray([
      [1.0, 2.0],
      [3.0]  // Different column count
    ]);
    Fail('Should raise EMatrixError for inconsistent row lengths');
  except
    on E: EMatrixError do
      ; // Expected exception
  end;
end;

procedure TMatrixTest.Test26_AdditionalProperties;
var
  M: IMatrix;
  Tolerance: Double;
begin
  Tolerance := 1E-12;
  
  // Test positive definite matrix
  M := TMatrixKit.CreateFromArray([
    [2.0, -1.0],
    [-1.0, 2.0]
  ]);
  AssertTrue('Should be positive definite', M.IsPositiveDefinite);
  
  // Test non-positive definite matrix
  M := TMatrixKit.CreateFromArray([
    [1.0, 2.0],
    [2.0, 1.0]
  ]);
  AssertFalse('Should not be positive definite', M.IsPositiveDefinite);
  
  // Test orthogonal matrix (rotation matrix)
  M := TMatrixKit.CreateFromArray([
    [0.0, -1.0],
    [1.0, 0.0]
  ]);
  AssertTrue('Should be orthogonal', M.IsOrthogonal);
  
  // Test condition number
  M := TMatrixKit.CreateFromArray([
    [1.0, 0.0],
    [0.0, 2.0]
  ]);
  AssertTrue('Condition number should be approximately 2', 
    Abs(M.Condition - 2.0) < 1.0E-3);  // More lenient tolerance
  
  // Test non-orthogonal matrix
  M := TMatrixKit.CreateFromArray([
    [1.0, 1.0],
    [0.0, 1.0]
  ]);
  AssertFalse('Should not be orthogonal', M.IsOrthogonal);
end;

initialization
  RegisterTest(TMatrixTest);
end. 
