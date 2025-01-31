# SHA-512/256 Implementation in Free Pascal

## Overview

SHA-512/256 is a truncated version of SHA-512 that provides 256 bits of security while maintaining the internal structure of SHA-512. This document details our implementation findings and key learnings.

## Initial Implementation Issue

Our initial implementation had a critical flaw:

```pascal
// INCORRECT initial implementation
class function TSHA2.SHA512_256(const Data: string): string;
var
  FullHash: string;
begin
  FullHash := SHA512(Data);
  Result := Copy(FullHash, 1, 64); // Take first 256 bits (64 hex characters)
end;
```

This approach was incorrect because:
1. It used SHA-512's initial hash values
2. Simply truncated the final output to 256 bits

## Correct Implementation

SHA-512/256 requires its own set of initial hash values as specified in FIPS 180-4:

```pascal
// CORRECT implementation
class function TSHA2.SHA512_256(const Data: string): string;
var
  State: TSHA2_512State;
begin
  // Initialize state with SHA-512/256 specific values
  State[0] := QWord($22312194FC2BF72C);
  State[1] := QWord($9F555FA3C84C64C2);
  State[2] := QWord($2393B86B6F53B151);
  State[3] := QWord($963877195940EABD);
  State[4] := QWord($96283EE2A88EFFE3);
  State[5] := QWord($BE5E1E2553863992);
  State[6] := QWord($2B0199FC2C85B8AA);
  State[7] := QWord($0EB72DDC81C52CA2);
  
  // ... process blocks using SHA-512 algorithm ...
  
  // Only output first 256 bits (4 state words)
  Result := '';
  for I := 0 to 3 do
    Result := Result + IntToHex(State[I], 16);
end;
```

## Test Vector Confusion

We encountered confusion with test vectors because:
1. Some test vectors were from SHA-512 truncated to 256 bits
2. Others were genuine SHA-512/256 test vectors

### Correct Test Vectors (NIST Verified)

1. Empty string:
```
C672B8D1EF56ED28AB87C3622C5114069BDD3AD7B8F9737498D0C01ECEF0967A
```

2. "The quick brown fox jumps over the lazy dog":
```
DD9D67B371519C339ED8DBD25AF90E976A1EEEFD4AD3D889005E532FC5BEF04D
```

## Key Differences from SHA-512

1. **Initial Hash Values**: 
   - SHA-512/256 uses different initial hash values
   - These values are derived through a specific process defined in FIPS 180-4

2. **Output Size**:
   - SHA-512 outputs 512 bits
   - SHA-512/256 outputs only the first 256 bits of the final state

3. **Internal Operation**:
   - Uses the same block size (1024 bits)
   - Uses the same round functions
   - Uses the same number of rounds (80)
   - Only differs in initial values and output length

## Implementation Requirements

1. **Block Processing**:
   - Must use 1024-bit (128-byte) blocks
   - Padding rules are identical to SHA-512

2. **State Management**:
   - Must maintain 512-bit internal state
   - Only output first 256 bits of final state

3. **Endianness**:
   - All operations are big-endian
   - Must handle byte ordering correctly on little-endian systems

## Testing Considerations

1. **Empty String Test**:
   - Critical test case
   - Verifies correct initialization and padding

2. **Known Answer Tests**:
   - Use NIST test vectors
   - Don't confuse with truncated SHA-512

3. **Block Boundary Tests**:
   - Test strings of various lengths
   - Particularly around 128-byte boundary

## References

1. [FIPS 180-4](https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.180-4.pdf) - Official SHA-2 specification
2. [NIST Test Vectors](https://csrc.nist.gov/projects/cryptographic-algorithm-validation-program)
3. [RFC 6234](https://tools.ietf.org/html/rfc6234) - SHA-2 implementation guidance 