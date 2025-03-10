unit TidyKit.Math.Matrices;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, Math, TidyKit.Math;

type
  { Matrix operations exception }
  EMatrixError = class(Exception);

  { Matrix interface for memory-managed matrices }
  IMatrix = interface
    ['{A1B2C3D4-E5F6-4789-ABCD-EF1234567890}']
    // Basic properties
    function GetRows: Integer;
    function GetColumns: Integer;
    function GetValue(Row, Column: Integer): Double;
    procedure SetValue(Row, Column: Integer; const Value: Double);
    
    // Property accessors
    property Rows: Integer read GetRows;
    property Columns: Integer read GetColumns;
    property Values[Row, Column: Integer]: Double read GetValue write SetValue; default;
    
    // Basic matrix operations
    function Transpose: IMatrix;
    function Add(const Matrix: IMatrix): IMatrix;
    function Subtract(const Matrix: IMatrix): IMatrix;
    function Multiply(const Matrix: IMatrix): IMatrix;
    function MultiplyScalar(const Scalar: Double): IMatrix;
    
    // Advanced operations
    function Determinant: Double;
    function Inverse: IMatrix;
    function Solve(const B: IMatrix): IMatrix; // Solves AX = B for X
    
    // Utility methods
    function ToString: string;
    function Clone: IMatrix;
    
    // Conversion methods
    function ToMatrix: TMatrix;
  end;
  
  { Matrix implementation with interface-based memory management }
  TMatrixImpl = class(TInterfacedObject, IMatrix)
  private
    FData: TMatrix;
    
    // Interface method implementations
    function GetRows: Integer;
    function GetColumns: Integer;
    function GetValue(Row, Column: Integer): Double;
    procedure SetValue(Row, Column: Integer; const Value: Double);
    
  public
    constructor Create(Rows, Columns: Integer);
    constructor CreateFromMatrix(const AMatrix: TMatrix);
    destructor Destroy; override;
    
    // IMatrix implementation
    function Transpose: IMatrix;
    function Add(const Matrix: IMatrix): IMatrix;
    function Subtract(const Matrix: IMatrix): IMatrix;
    function Multiply(const Matrix: IMatrix): IMatrix;
    function MultiplyScalar(const Scalar: Double): IMatrix;
    function Determinant: Double;
    function Inverse: IMatrix;
    function Solve(const B: IMatrix): IMatrix;
    function ToString: string; override;
    function Clone: IMatrix;
    function ToMatrix: TMatrix;
  end;

  { Matrix operations class - maintained for compatibility }
  TMatrixKit = class
  public    
    { Matrix creation }
    class function CreateMatrix(const Rows, Cols: Integer): TMatrix; static;
    class function Identity(const Size: Integer): TMatrix; static;
    class function Zeros(const Rows, Cols: Integer): TMatrix; static;
    class function Ones(const Rows, Cols: Integer): TMatrix; static;
    
    { Factory methods for interface-based matrices }
    class function CreateIMatrix(const Rows, Cols: Integer): IMatrix; static;
    class function CreateFromMatrix(const AMatrix: TMatrix): IMatrix; static;
    class function CreateIdentity(const Size: Integer): IMatrix; static;
    class function CreateZeros(const Rows, Cols: Integer): IMatrix; static;
    class function CreateOnes(const Rows, Cols: Integer): IMatrix; static;
    
    { Basic operations }
    class function Add(const A, B: TMatrix): TMatrix; static;
    class function Subtract(const A, B: TMatrix): TMatrix; static;
    class function Multiply(const A, B: TMatrix): TMatrix; static;
    class function ScalarMultiply(const A: TMatrix; const Scalar: Double): TMatrix; static;
    
    { Matrix transformations }
    class function Transpose(const A: TMatrix): TMatrix; static;
    class function Inverse(const A: TMatrix): TMatrix; static;
    
    { Matrix properties }
    class function Determinant(const A: TMatrix): Double; static;
    class function Trace(const A: TMatrix): Double; static;
    class function Rank(const A: TMatrix): Integer; static;
    
    { Matrix decompositions }
    class procedure LUDecomposition(const A: TMatrix; out L, U: TMatrix); static;
    class procedure QRDecomposition(const A: TMatrix; out Q, R: TMatrix); static;
    
    { Helper functions }
    class function GetRows(const A: TMatrix): Integer; static;
    class function GetCols(const A: TMatrix): Integer; static;
    class function IsSquare(const A: TMatrix): Boolean; static;
  end;

implementation

{ TMatrixImpl }

constructor TMatrixImpl.Create(Rows, Columns: Integer);
var
  I: Integer;
begin
  inherited Create;
  SetLength(FData, Rows);
  for I := 0 to Rows - 1 do
    SetLength(FData[I], Columns);
end;

constructor TMatrixImpl.CreateFromMatrix(const AMatrix: TMatrix);
var
  I, J, Rows, Cols: Integer;
begin
  Rows := Length(AMatrix);
  if Rows > 0 then
    Cols := Length(AMatrix[0])
  else
    Cols := 0;
    
  Create(Rows, Cols);
  
  for I := 0 to Rows - 1 do
    for J := 0 to Cols - 1 do
      FData[I][J] := AMatrix[I][J];
end;

destructor TMatrixImpl.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FData) do
    SetLength(FData[I], 0);
  SetLength(FData, 0);
  inherited;
end;

function TMatrixImpl.GetRows: Integer;
begin
  Result := Length(FData);
end;

function TMatrixImpl.GetColumns: Integer;
begin
  if Length(FData) > 0 then
    Result := Length(FData[0])
  else
    Result := 0;
end;

function TMatrixImpl.GetValue(Row, Column: Integer): Double;
begin
  if (Row < 0) or (Row >= GetRows) or (Column < 0) or (Column >= GetColumns) then
    raise EMatrixError.Create('Matrix index out of bounds');
  Result := FData[Row][Column];
end;

procedure TMatrixImpl.SetValue(Row, Column: Integer; const Value: Double);
begin
  if (Row < 0) or (Row >= GetRows) or (Column < 0) or (Column >= GetColumns) then
    raise EMatrixError.Create('Matrix index out of bounds');
  FData[Row][Column] := Value;
end;

function TMatrixImpl.Transpose: IMatrix;
var
  I, J, Rows, Cols: Integer;
  ResultMatrix: TMatrixImpl;
begin
  Rows := GetRows;
  Cols := GetColumns;
  ResultMatrix := TMatrixImpl.Create(Cols, Rows);
  
  for I := 0 to Rows - 1 do
    for J := 0 to Cols - 1 do
      ResultMatrix.FData[J][I] := FData[I][J];
      
  Result := ResultMatrix;
end;

function TMatrixImpl.Add(const Matrix: IMatrix): IMatrix;
var
  I, J, Rows, Cols: Integer;
  ResultMatrix: TMatrixImpl;
begin
  Rows := GetRows;
  Cols := GetColumns;
  
  if (Rows <> Matrix.Rows) or (Cols <> Matrix.Columns) then
    raise EMatrixError.Create('Matrix dimensions must match for addition');
    
  ResultMatrix := TMatrixImpl.Create(Rows, Cols);
  for I := 0 to Rows - 1 do
    for J := 0 to Cols - 1 do
      ResultMatrix.FData[I][J] := FData[I][J] + Matrix.Values[I, J];
      
  Result := ResultMatrix;
end;

function TMatrixImpl.Subtract(const Matrix: IMatrix): IMatrix;
var
  I, J, Rows, Cols: Integer;
  ResultMatrix: TMatrixImpl;
begin
  Rows := GetRows;
  Cols := GetColumns;
  
  if (Rows <> Matrix.Rows) or (Cols <> Matrix.Columns) then
    raise EMatrixError.Create('Matrix dimensions must match for subtraction');
    
  ResultMatrix := TMatrixImpl.Create(Rows, Cols);
  for I := 0 to Rows - 1 do
    for J := 0 to Cols - 1 do
      ResultMatrix.FData[I][J] := FData[I][J] - Matrix.Values[I, J];
      
  Result := ResultMatrix;
end;

function TMatrixImpl.Multiply(const Matrix: IMatrix): IMatrix;
var
  I, J, K, Rows, Cols, ColsB: Integer;
  Sum: Double;
  ResultMatrix: TMatrixImpl;
begin
  Rows := GetRows;
  Cols := GetColumns;
  ColsB := Matrix.Columns;
  
  if Cols <> Matrix.Rows then
    raise EMatrixError.Create('Invalid matrix dimensions for multiplication');
    
  ResultMatrix := TMatrixImpl.Create(Rows, ColsB);
  
  for I := 0 to Rows - 1 do
    for J := 0 to ColsB - 1 do
    begin
      Sum := 0;
      for K := 0 to Cols - 1 do
        Sum := Sum + FData[I][K] * Matrix.Values[K, J];
      ResultMatrix.FData[I][J] := Sum;
    end;
    
  Result := ResultMatrix;
end;

function TMatrixImpl.MultiplyScalar(const Scalar: Double): IMatrix;
var
  I, J, Rows, Cols: Integer;
  ResultMatrix: TMatrixImpl;
begin
  Rows := GetRows;
  Cols := GetColumns;
  ResultMatrix := TMatrixImpl.Create(Rows, Cols);
  
  for I := 0 to Rows - 1 do
    for J := 0 to Cols - 1 do
      ResultMatrix.FData[I][J] := FData[I][J] * Scalar;
      
  Result := ResultMatrix;
end;

function TMatrixImpl.Determinant: Double;
var
  N, I, J, K: Integer;
  Temp: TMatrix;
  Factor: Double;
  
  function MinorDeterminant(const M: TMatrix; const Size: Integer): Double;
  var
    I, J, K, L: Integer;
    SubMatrix: TMatrix;
    Sign: Double;
  begin
    if Size = 1 then
      Result := M[0][0]
    else if Size = 2 then
      Result := M[0][0] * M[1][1] - M[0][1] * M[1][0]
    else
    begin
      Result := 0;
      SetLength(SubMatrix, Size - 1);
      for I := 0 to Size - 2 do
        SetLength(SubMatrix[I], Size - 1);
        
      for K := 0 to Size - 1 do
      begin
        for I := 1 to Size - 1 do
          for J := 0 to Size - 1 do
            if J < K then
              SubMatrix[I-1][J] := M[I][J]
            else if J > K then
              SubMatrix[I-1][J-1] := M[I][J];
              
        if K mod 2 = 0 then
          Sign := 1
        else
          Sign := -1;
          
        Result := Result + Sign * M[0][K] * MinorDeterminant(SubMatrix, Size - 1);
      end;
      
      for I := 0 to Size - 2 do
        SetLength(SubMatrix[I], 0);
      SetLength(SubMatrix, 0);
    end;
  end;
  
begin
  N := GetRows;
  if N <> GetColumns then
    raise EMatrixError.Create('Matrix must be square to calculate determinant');
    
  if N = 0 then
    Result := 0
  else if N = 1 then
    Result := FData[0][0]
  else if N = 2 then
    Result := FData[0][0] * FData[1][1] - FData[0][1] * FData[1][0]
  else
    Result := MinorDeterminant(FData, N);
end;

function TMatrixImpl.Inverse: IMatrix;
var
  N, I, J, K: Integer;
  Pivot, DetValue: Double;
  Temp: Double;
  AugMatrix: TMatrix;
  ResultMatrix: TMatrixImpl;
begin
  N := GetRows;
  if N <> GetColumns then
    raise EMatrixError.Create('Only square matrices can be inverted');
    
  DetValue := Determinant;
  if Abs(DetValue) < 1E-10 then
    raise EMatrixError.Create('Matrix is singular and cannot be inverted');
    
  // Create augmented matrix [A|I]
  SetLength(AugMatrix, N);
  for I := 0 to N - 1 do
  begin
    SetLength(AugMatrix[I], 2 * N);
    for J := 0 to N - 1 do
    begin
      AugMatrix[I][J] := FData[I][J];
      if I = J then
        AugMatrix[I][J + N] := 1
      else
        AugMatrix[I][J + N] := 0;
    end;
  end;
  
  // Perform Gauss-Jordan elimination
  for I := 0 to N - 1 do
  begin
    // Find pivot
    Pivot := AugMatrix[I][I];
    if Abs(Pivot) < 1E-10 then
    begin
      K := I + 1;
      while (K < N) and (Abs(AugMatrix[K][I]) < 1E-10) do
        Inc(K);
        
      if K = N then
      begin
        for K := 0 to N - 1 do
          SetLength(AugMatrix[K], 0);
        SetLength(AugMatrix, 0);
        raise EMatrixError.Create('Matrix is singular and cannot be inverted');
      end;
      
      // Swap rows
      for J := 0 to 2 * N - 1 do
      begin
        Temp := AugMatrix[I][J];
        AugMatrix[I][J] := AugMatrix[K][J];
        AugMatrix[K][J] := Temp;
      end;
      
      Pivot := AugMatrix[I][I];
    end;
    
    // Scale row
    for J := 0 to 2 * N - 1 do
      AugMatrix[I][J] := AugMatrix[I][J] / Pivot;
      
    // Eliminate other rows
    for K := 0 to N - 1 do
      if K <> I then
      begin
        Temp := AugMatrix[K][I];
        for J := 0 to 2 * N - 1 do
          AugMatrix[K][J] := AugMatrix[K][J] - Temp * AugMatrix[I][J];
      end;
  end;
  
  // Extract inverse matrix
  ResultMatrix := TMatrixImpl.Create(N, N);
  for I := 0 to N - 1 do
    for J := 0 to N - 1 do
      ResultMatrix.FData[I][J] := AugMatrix[I][J + N];
      
  // Clean up
  for I := 0 to N - 1 do
    SetLength(AugMatrix[I], 0);
  SetLength(AugMatrix, 0);
      
  Result := ResultMatrix;
end;

function TMatrixImpl.Solve(const B: IMatrix): IMatrix;
var
  InvA: IMatrix;
begin
  if GetRows <> GetColumns then
    raise EMatrixError.Create('Coefficient matrix must be square to solve system');
    
  if GetRows <> B.Rows then
    raise EMatrixError.Create('Number of equations must match number of unknowns');
    
  // Solving AX = B by X = A^-1 * B
  InvA := Inverse;
  Result := InvA.Multiply(B);
end;

function TMatrixImpl.ToString: string;
var
  I, J, Rows, Cols: Integer;
  RowStr: string;
begin
  Rows := GetRows;
  Cols := GetColumns;
  Result := '[';
  
  for I := 0 to Rows - 1 do
  begin
    RowStr := '[';
    for J := 0 to Cols - 1 do
    begin
      RowStr := RowStr + FloatToStr(FData[I][J]);
      if J < Cols - 1 then
        RowStr := RowStr + ', ';
    end;
    Result := Result + RowStr + ']';
    if I < Rows - 1 then
      Result := Result + #13#10;
  end;
  
  Result := Result + ']';
end;

function TMatrixImpl.Clone: IMatrix;
var
  I, J, Rows, Cols: Integer;
  ResultMatrix: TMatrixImpl;
begin
  Rows := GetRows;
  Cols := GetColumns;
  ResultMatrix := TMatrixImpl.Create(Rows, Cols);
  
  for I := 0 to Rows - 1 do
    for J := 0 to Cols - 1 do
      ResultMatrix.FData[I][J] := FData[I][J];
      
  Result := ResultMatrix;
end;

function TMatrixImpl.ToMatrix: TMatrix;
var
  I, J, Rows, Cols: Integer;
  OutputMatrix: TMatrix;
begin
  Rows := GetRows;
  Cols := GetColumns;
  
  SetLength(OutputMatrix, Rows);
  for I := 0 to Rows - 1 do
  begin
    SetLength(OutputMatrix[I], Cols);
    for J := 0 to Cols - 1 do
      OutputMatrix[I][J] := FData[I][J];
  end;
  
  Result := OutputMatrix;
end;

{ TMatrixKit - Factory methods for interface-based matrices }

class function TMatrixKit.CreateIMatrix(const Rows, Cols: Integer): IMatrix;
begin
  Result := TMatrixImpl.Create(Rows, Cols);
end;

class function TMatrixKit.CreateFromMatrix(const AMatrix: TMatrix): IMatrix;
begin
  Result := TMatrixImpl.CreateFromMatrix(AMatrix);
end;

class function TMatrixKit.CreateIdentity(const Size: Integer): IMatrix;
var
  I: Integer;
  Mat: TMatrixImpl;
begin
  Mat := TMatrixImpl.Create(Size, Size);
  for I := 0 to Size - 1 do
    Mat.FData[I][I] := 1.0;
  Result := Mat;
end;

class function TMatrixKit.CreateZeros(const Rows, Cols: Integer): IMatrix;
begin
  Result := TMatrixImpl.Create(Rows, Cols);
  // Matrix values are already initialized to zero
end;

class function TMatrixKit.CreateOnes(const Rows, Cols: Integer): IMatrix;
var
  I, J: Integer;
  Mat: TMatrixImpl;
begin
  Mat := TMatrixImpl.Create(Rows, Cols);
  for I := 0 to Rows - 1 do
    for J := 0 to Cols - 1 do
      Mat.FData[I][J] := 1.0;
  Result := Mat;
end;

{ TMatrixKit }

class function TMatrixKit.CreateMatrix(const Rows, Cols: Integer): TMatrix;
var
  I: Integer;
begin
  SetLength(Result, Rows);
  for I := 0 to Rows - 1 do
    SetLength(Result[I], Cols);
end;

class function TMatrixKit.Identity(const Size: Integer): TMatrix;
var
  I, J: Integer;
begin
  Result := CreateMatrix(Size, Size);
  for I := 0 to Size - 1 do
    for J := 0 to Size - 1 do
      if I = J then
        Result[I, J] := 1.0
      else
        Result[I, J] := 0.0;
end;

class function TMatrixKit.Zeros(const Rows, Cols: Integer): TMatrix;
var
  I, J: Integer;
begin
  Result := CreateMatrix(Rows, Cols);
  for I := 0 to Rows - 1 do
    for J := 0 to Cols - 1 do
      Result[I, J] := Default(Double);
end;

class function TMatrixKit.Ones(const Rows, Cols: Integer): TMatrix;
var
  I, J: Integer;
begin
  Result := CreateMatrix(Rows, Cols);
  for I := 0 to Rows - 1 do
    for J := 0 to Cols - 1 do
      Result[I, J] := 1.0;
end;

class function TMatrixKit.Add(const A, B: TMatrix): TMatrix;
var
  I, J, Rows, Cols: Integer;
begin
  Rows := GetRows(A);
  Cols := GetCols(A);
  
  if (Rows <> GetRows(B)) or (Cols <> GetCols(B)) then
    raise EMatrixError.Create('Matrix dimensions must match for addition');
    
  Result := CreateMatrix(Rows, Cols);
  for I := 0 to Rows - 1 do
    for J := 0 to Cols - 1 do
      Result[I, J] := A[I, J] + B[I, J];
end;

class function TMatrixKit.Subtract(const A, B: TMatrix): TMatrix;
var
  I, J, Rows, Cols: Integer;
begin
  Rows := GetRows(A);
  Cols := GetCols(A);
  
  if (Rows <> GetRows(B)) or (Cols <> GetCols(B)) then
    raise EMatrixError.Create('Matrix dimensions must match for subtraction');
    
  Result := CreateMatrix(Rows, Cols);
  for I := 0 to Rows - 1 do
    for J := 0 to Cols - 1 do
      Result[I, J] := A[I, J] - B[I, J];
end;

class function TMatrixKit.Multiply(const A, B: TMatrix): TMatrix;
var
  I, J, K, RowsA, ColsA, ColsB: Integer;
  Sum: Double;
begin
  RowsA := GetRows(A);
  ColsA := GetCols(A);
  ColsB := GetCols(B);
  
  if ColsA <> GetRows(B) then
    raise EMatrixError.Create('Invalid matrix dimensions for multiplication');
    
  Result := CreateMatrix(RowsA, ColsB);
  for I := 0 to RowsA - 1 do
    for J := 0 to ColsB - 1 do
    begin
      Sum := Default(Double);
      for K := 0 to ColsA - 1 do
        Sum := Sum + A[I, K] * B[K, J];
      Result[I, J] := Sum;
    end;
end;

class function TMatrixKit.ScalarMultiply(const A: TMatrix; const Scalar: Double): TMatrix;
var
  I, J, Rows, Cols: Integer;
begin
  Rows := GetRows(A);
  Cols := GetCols(A);
  Result := CreateMatrix(Rows, Cols);
  
  for I := 0 to Rows - 1 do
    for J := 0 to Cols - 1 do
      Result[I, J] := A[I, J] * Scalar;
end;

class function TMatrixKit.Transpose(const A: TMatrix): TMatrix;
var
  I, J, Rows, Cols: Integer;
begin
  Rows := GetRows(A);
  Cols := GetCols(A);
  Result := CreateMatrix(Cols, Rows);
  
  for I := 0 to Rows - 1 do
    for J := 0 to Cols - 1 do
      Result[J, I] := A[I, J];
end;

class function TMatrixKit.GetRows(const A: TMatrix): Integer;
begin
  Result := Length(A);
end;

class function TMatrixKit.GetCols(const A: TMatrix): Integer;
begin
  if Length(A) > 0 then
    Result := Length(A[0])
  else
    Result := 0;
end;

class function TMatrixKit.IsSquare(const A: TMatrix): Boolean;
begin
  Result := (GetRows(A) = GetCols(A));
end;

class function TMatrixKit.Determinant(const A: TMatrix): Double;
var
  N, I, J, K: Integer;
  Factor: Double;
  Temp: TMatrix;
  
  function MinorDeterminant(const M: TMatrix; const Size: Integer): Double;
  var
    I, J, K, L: Integer;
    SubMatrix: TMatrix;
    Sign: Double;
  begin
    if Size = 1 then
      Result := M[0, 0]
    else if Size = 2 then
      Result := M[0, 0] * M[1, 1] - M[0, 1] * M[1, 0]
    else
    begin
      Result := 0;
      SubMatrix := CreateMatrix(Size - 1, Size - 1);
      for K := 0 to Size - 1 do
      begin
        L := 0;
        for I := 1 to Size - 1 do
        begin
          for J := 0 to Size - 1 do
            if J <> K then
            begin
              SubMatrix[I - 1, L] := M[I, J];
              Inc(L);
            end;
          L := 0;
        end;
        
        if K mod 2 = 0 then
          Sign := 1
        else
          Sign := -1;
          
        Result := Result + Sign * M[0, K] * MinorDeterminant(SubMatrix, Size - 1);
      end;
    end;
  end;
  
begin
  N := GetRows(A);
  if not IsSquare(A) then
    raise EMatrixError.Create('Matrix must be square to calculate determinant');
    
  if N = 0 then
    Result := 0
  else if N = 1 then
    Result := A[0, 0]
  else if N = 2 then
    Result := A[0, 0] * A[1, 1] - A[0, 1] * A[1, 0]
  else
    Result := MinorDeterminant(A, N);
end;

class function TMatrixKit.Trace(const A: TMatrix): Double;
var
  I, Rows: Integer;
  Sum: Double;
begin
  Rows := GetRows(A);
  Sum := Default(Double);
  for I := 0 to Rows - 1 do
    Sum := Sum + A[I, I];
  Result := Sum;
end;

class function TMatrixKit.Rank(const A: TMatrix): Integer;
begin
  // TODO: Implement rank calculation
  Result := 0;
end;

class procedure TMatrixKit.LUDecomposition(const A: TMatrix; out L, U: TMatrix);
begin
  // TODO: Implement LU decomposition
end;

class procedure TMatrixKit.QRDecomposition(const A: TMatrix; out Q, R: TMatrix);
begin
  // TODO: Implement QR decomposition
end;

class function TMatrixKit.Inverse(const A: TMatrix): TMatrix;
begin
  // TODO: Implement matrix inversion
  Result := CreateMatrix(0, 0);
end;

end. 