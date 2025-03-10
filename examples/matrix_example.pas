program matrix_example;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils, TidyKit;

var
  A, B, C, Solution: IMatrix;
  I, J: Integer;
  
  { Helper procedure to print a matrix to console }
  procedure PrintMatrix(const Name: string; const Matrix: IMatrix);
  begin
    WriteLn('Matrix ', Name, ' (', Matrix.Rows, 'x', Matrix.Columns, '):');
    WriteLn(Matrix.ToString);
    WriteLn;
  end;

begin
  try
    WriteLn('TidyKit Interface-Based Matrix Example');
    WriteLn('=====================================');
    WriteLn;
    
    // Create matrices using factory methods
    A := TMatrixKit.CreateFromMatrix([
      [4.0, 3.0, 2.0],
      [1.0, 3.0, 1.0],
      [2.0, 1.0, 3.0]
    ]);
    
    B := TMatrixKit.CreateFromMatrix([
      [1.0, 0.0, 5.0],
      [2.0, 1.0, 6.0],
      [3.0, 4.0, 0.0]
    ]);
    
    // Display the matrices
    PrintMatrix('A', A);
    PrintMatrix('B', B);
    
    // Matrix addition
    C := A.Add(B);
    PrintMatrix('A + B', C);
    
    // Matrix multiplication
    C := A.Multiply(B);
    PrintMatrix('A * B', C);
    
    // Scalar multiplication
    C := A.MultiplyScalar(2.5);
    PrintMatrix('A * 2.5', C);
    
    // Transpose
    C := A.Transpose;
    PrintMatrix('A transpose', C);
    
    // Determinant
    WriteLn('Determinant of A: ', A.Determinant:0:4);
    WriteLn;
    
    // Matrix inversion
    C := A.Inverse;
    PrintMatrix('A inverse', C);
    
    // Verify the inverse (A * A^-1 should be identity)
    PrintMatrix('A * A^-1 (should be identity)', A.Multiply(C));
    
    // Solving a linear system
    B := TMatrixKit.CreateFromMatrix([
      [1.0],
      [2.0],
      [3.0]
    ]);
    
    WriteLn('Solving the linear system Ax = B:');
    PrintMatrix('B (right-hand side)', B);
    
    Solution := A.Solve(B);
    PrintMatrix('Solution x', Solution);
    
    // Verify the solution
    PrintMatrix('A * x (should equal B)', A.Multiply(Solution));
    
    // Create matrices with factory methods
    WriteLn('Creating special matrices:');
    
    // Identity matrix
    C := TMatrixKit.CreateIdentity(3);
    PrintMatrix('3x3 Identity', C);
    
    // Zero matrix
    C := TMatrixKit.CreateZeros(2, 4);
    PrintMatrix('2x4 Zeros', C);
    
    // Ones matrix
    C := TMatrixKit.CreateOnes(3, 2);
    PrintMatrix('3x2 Ones', C);
    
    WriteLn('Matrix example completed successfully.');
    
  except
    on E: EMatrixError do
    begin
      WriteLn('Matrix error: ', E.Message);
      ExitCode := 1;
    end;
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      ExitCode := 2;
    end;
  end;
  
  // Wait for user input
  WriteLn('Press Enter to exit...');
  ReadLn;
end. 