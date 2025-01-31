# SHA-512 Implementation in Free Pascal

## Overview

This document describes the implementation details and key findings for SHA-512 hash function implementation in Free Pascal, particularly focusing on handling 64-bit arithmetic operations.

## The Challenge

When implementing SHA-512 in Free Pascal, we encountered arithmetic overflow issues due to the language's default overflow checking behavior. The main challenge was that SHA-512 relies on modulo 2^64 arithmetic, which naturally involves overflow as part of the algorithm.

## Initial Approaches (That Didn't Work)

### 1. Using TUInt128 for Intermediate Calculations

Our first attempt was to use a 128-bit type (TUInt128) to handle potential overflows:

```pascal
type
  TUInt128 = record
    Lo: QWord;
    Hi: QWord;
  end;
```

This approach:
- Was overly complex
- Didn't match reference implementations
- Still resulted in overflow errors
- Added unnecessary overhead

### 2. Explicit Overflow Handling

We then tried to handle overflow explicitly:

```pascal
class procedure Add128(var Sum: TUInt128; Value: QWord);
begin
  if Sum.Lo > (Sum.Lo + Value) then  // Check for overflow
    Inc(Sum.Hi);
  Inc(Sum.Lo, Value);
end;
```

This also:
- Was error-prone
- Didn't match how SHA-512 is meant to work
- Still resulted in overflow errors

## The Solution

The key insight was understanding that SHA-512 is designed to work with modulo 2^64 arithmetic. In most languages, this happens naturally:

- C/C++: Natural overflow with uint64_t
- Java: Natural overflow with long
- Python: Built-in modulo behavior

In Free Pascal, we needed to explicitly disable overflow checking:

```pascal
{$Q-}  // Disable overflow checking
// SHA-512 arithmetic operations here
{$Q+}  // Re-enable overflow checking
```

### Implementation Example

```pascal
class procedure TSHA2.SHA512Transform(var State: TSHA2_512State; const Block: TSHA2_512Block);
var
  W: array[0..79] of QWord;
  A, B, C, D, E, F, G, H, T1, T2: QWord;
  I: Integer;
begin
  // Initialize working variables...

  {$Q-}  // Disable overflow checking
  for I := 16 to 79 do
  begin
    W[I] := SmallSigma1_64(W[I-2]) + W[I-7] + 
            SmallSigma0_64(W[I-15]) + W[I-16];
  end;

  // Main loop
  for I := 0 to 79 do
  begin
    T1 := H + BigSigma1_64(E) + Ch64(E, F, G) + 
          SHA512_K[I] + W[I];
    T2 := BigSigma0_64(A) + Maj64(A, B, C);
    // ... state updates ...
  end;

  // Update state...
  {$Q+}  // Re-enable overflow checking
end;
```

## Benefits of This Approach

1. **Simplicity**
   - Cleaner code
   - No complex overflow handling
   - More maintainable

2. **Correctness**
   - Matches the SHA-512 specification
   - Behaves like reference implementations
   - Properly handles modulo 2^64 arithmetic

3. **Performance**
   - No overhead from overflow checking
   - No extra calculations for overflow handling
   - Direct hardware operations

## Key Learnings

1. Understanding the algorithm's requirements is crucial
   - SHA-512 needs modulo 2^64 arithmetic
   - Overflow is part of the design, not a bug

2. Language-specific considerations
   - Free Pascal's default overflow checking affects crypto implementations
   - Use compiler directives to match other languages' behavior

3. Sometimes simpler is better
   - Removing complex overflow handling fixed the issue
   - Matching reference implementations led to correct behavior

## References

- [SHA-2 Wikipedia](https://en.wikipedia.org/wiki/SHA-2)
- [FIPS 180-4](https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.180-4.pdf)
- [Free Pascal Compiler Directives](https://www.freepascal.org/docs-html/prog/progsu60.html) 