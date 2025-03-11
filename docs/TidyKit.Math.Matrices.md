# 🔢 TidyKit.Math.Matrices User Manual

A comprehensive guide to using the matrix operations in TidyKit.

## Table of Contents
- [Basic Operations](#basic-operations)
- [Matrix Creation](#matrix-creation)
- [Matrix Properties](#matrix-properties)
- [Matrix Decompositions](#matrix-decompositions)
- [Common Use Cases](#common-use-cases)

## Basic Operations

### Creating Matrices
```pascal
var
  A, B, C: IMatrix;
begin
  // Create from array
  A := TMatrixKit.CreateFromArray([
    [1.0, 2.0],
    [3.0, 4.0]
  ]);

  // Create special matrices
  B := TMatrixKit.Identity(2);     // 2x2 identity matrix
  C := TMatrixKit.Zeros(2, 3);     // 2x3 matrix of zeros
  C := TMatrixKit.Ones(3, 2);      // 3x2 matrix of ones
end;
```

For example, the above code creates:

$$
A = \begin{bmatrix}
1.0 & 2.0 \\
3.0 & 4.0
\end{bmatrix}, \quad
B = \begin{bmatrix}
1.0 & 0.0 \\
0.0 & 1.0
\end{bmatrix}, \quad
C = \begin{bmatrix}
0.0 & 0.0 & 0.0 \\
0.0 & 0.0 & 0.0
\end{bmatrix}
$$

### Matrix Arithmetic
For a matrix $A$ with elements $a_{ij}$ and a matrix $B$ with elements $b_{ij}$ of compatible dimensions:

- Addition: $C = A + B$
- Subtraction: $C = A - B$
- Matrix multiplication: $C = AB$ where $C_{ij} = \sum_{k=1}^n A_{ik}B_{kj}$
- Scalar multiplication: $C = \alpha A$ where $\alpha$ is a scalar
- Element-wise multiplication (Hadamard product): $C = A \odot B$ where $C_{ij} = A_{ij}B_{ij}$
- Element-wise division: $C_{ij} = A_{ij}/B_{ij}$

```pascal
var
  A, B, C: IMatrix;
begin
  // Addition
  C := A.Add(B);

  // Subtraction
  C := A.Subtract(B);

  // Matrix multiplication
  C := A.Multiply(B);

  // Scalar multiplication
  C := A.ScalarMultiply(2.0);
  
  // Element-wise operations
  C := A.ElementWiseMultiply(B);
  C := A.ElementWiseDivide(B);
end;
```

## Matrix Properties

### Basic Properties
For a matrix $A$:

- Determinant: $\det(A)$ or $|A|$
- Trace: $\text{tr}(A) = \sum_{i=1}^n A_{ii}$
- Rank: $r = \text{rank}(A)$, number of linearly independent rows/columns
- Condition number: $\kappa(A) = \|A\| \cdot \|A^{-1}\|$

```pascal
var
  M: IMatrix;
  Val: Double;
begin
  // Dimensions
  WriteLn('Rows: ', M.Rows);
  WriteLn('Columns: ', M.Cols);

  // Access elements
  Val := M.Values[0, 0];  // First element
  M.Values[1, 1] := 2.0;  // Set element

  // Properties
  WriteLn('Determinant: ', M.Determinant);
  WriteLn('Trace: ', M.Trace);
  WriteLn('Rank: ', M.Rank);
  WriteLn('Condition number: ', M.Condition);
end;
```

### Matrix Type Checks
```pascal
var
  M: IMatrix;
begin
  if M.IsSquare then
    WriteLn('Matrix is square');
  if M.IsSymmetric then
    WriteLn('Matrix is symmetric');
  if M.IsDiagonal then
    WriteLn('Matrix is diagonal');
  if M.IsTriangular(True) then
    WriteLn('Matrix is upper triangular');
  if M.IsPositiveDefinite then
    WriteLn('Matrix is positive definite');
  if M.IsOrthogonal then
    WriteLn('Matrix is orthogonal');
end;
```

### Matrix Norms
For a matrix $A$:

- One norm (maximum column sum): $\|A\|_1 = \max_{j} \sum_{i=1}^m |a_{ij}|$
- Infinity norm (maximum row sum): $\|A\|_\infty = \max_{i} \sum_{j=1}^n |a_{ij}|$
- Frobenius norm: $\|A\|_F = \sqrt{\sum_{i=1}^m \sum_{j=1}^n |a_{ij}|^2}$

```pascal
var
  M: IMatrix;
begin
  WriteLn('One norm: ', M.NormOne);
  WriteLn('Infinity norm: ', M.NormInf);
  WriteLn('Frobenius norm: ', M.NormFrobenius);
end;
```

## Matrix Decompositions

### LU Decomposition
Factors a matrix $A$ into:
$$A = PLU$$
where:
- $P$ is a permutation matrix
- $L$ is lower triangular with ones on diagonal
- $U$ is upper triangular

```pascal
var
  M: IMatrix;
  LU: TLUDecomposition;
begin
  LU := M.LU;
  // LU.L is the lower triangular matrix
  // LU.U is the upper triangular matrix
  // LU.P contains the permutation indices
end;
```

### QR Decomposition
Factors a matrix $A$ into:
$$A = QR$$
where:
- $Q$ is orthogonal ($Q^TQ = I$)
- $R$ is upper triangular

```pascal
var
  M: IMatrix;
  QR: TQRDecomposition;
begin
  QR := M.QR;
  // QR.Q is the orthogonal matrix
  // QR.R is the upper triangular matrix
end;
```

### Eigendecomposition
For a square matrix $A$:
$$A\mathbf{v} = \lambda\mathbf{v}$$
where:
- $\lambda$ is an eigenvalue
- $\mathbf{v}$ is the corresponding eigenvector

```pascal
var
  M: IMatrix;
  Eigen: TEigenDecomposition;
begin
  Eigen := M.EigenDecomposition;
  // Eigen.EigenValues contains the eigenvalues
  // Eigen.EigenVectors contains the eigenvectors
end;
```

## Common Use Cases

### Solving Linear Systems
For a system $A\mathbf{x} = \mathbf{b}$:
- Direct solution: $\mathbf{x} = A^{-1}\mathbf{b}$
- LU solution: Solve $L\mathbf{y} = P\mathbf{b}$, then $U\mathbf{x} = \mathbf{y}$

The system in matrix form:

$$
\begin{bmatrix}
5 & 7 & 6 & 5 \\
7 & 10 & 8 & 7 \\
6 & 8 & 10 & 9 \\
5 & 7 & 9 & 10
\end{bmatrix}
\begin{bmatrix}
x_1 \\
x_2 \\
x_3 \\
x_4
\end{bmatrix} =
\begin{bmatrix}
57 \\
79 \\
88 \\
86
\end{bmatrix}
$$

```pascal
// Given the system Ax = b
var
  A, B, X: IMatrix;
begin
  // Create coefficient matrix A
  A := TMatrixKit.CreateFromArray([
    [5.0, 7.0, 6.0, 5.0],
    [7.0, 10.0, 8.0, 7.0],
    [6.0, 8.0, 10.0, 9.0],
    [5.0, 7.0, 9.0, 10.0]
  ]);

  // Create right-hand side vector b
  B := TMatrixKit.CreateFromArray([
    [57.0],
    [79.0],
    [88.0],
    [86.0]
  ]);

  // Solve using inverse
  X := A.Inverse.Multiply(B);
  
  // Or using LU decomposition
  LU := A.LU;
  X := LU.U.Inverse.Multiply(
       LU.L.Inverse.Multiply(B));
end;
```

### Matrix Transformations
```pascal
var
  M: IMatrix;
begin
  // Transpose
  M := M.Transpose;

  // Inverse (if exists)
  try
    M := M.Inverse;
  except
    on E: EMatrixError do
      WriteLn('Matrix is singular');
  end;
end;
```

### Working with Submatrices
```pascal
var
  M, Sub: IMatrix;
begin
  // Get a submatrix
  Sub := M.GetSubMatrix(0, 0, 2, 2);  // 2x2 from top-left

  // Set a submatrix
  M.SetSubMatrix(1, 1, Sub);  // Place at position (1,1)
end;
```

### Error Handling
```pascal
var
  M: IMatrix;
begin
  try
    M := M.Inverse;
  except
    on E: EMatrixError do
      WriteLn('Matrix error: ', E.Message);
  end;
end;
```

## Best Practices

1. **Numerical Stability**
   - Check condition number $\kappa(A)$ before inversion
   - For ill-conditioned matrices ($\kappa(A) \gg 1$), use decompositions
   - Consider scaling if values differ by many orders of magnitude

2. **Performance**
   - Use appropriate decomposition for your problem:
     - LU for general linear systems
     - QR for least squares problems
     - Eigendecomposition for finding principal components
   - Avoid explicit inverse when possible

3. **Memory Management**
   - Use the `IMatrix` interface for automatic memory management
   - Let the interface handle cleanup through reference counting

4. **Error Handling**
   - Always handle potential exceptions for:
     - Matrix inversion (singular matrices)
     - Decompositions (numerical instability)
     - Operations with mismatched dimensions 