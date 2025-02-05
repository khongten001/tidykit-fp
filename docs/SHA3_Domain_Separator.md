# SHA3 Domain Separator Implementation Notes

## Overview

This document details the domain separator implementation in TidyKit's SHA3 hash functions, specifically focusing on the differences between Keccak and SHA3 domain separators.

## Domain Separator Values

Different domain separators are used for different variants of the Keccak family:

| Algorithm  | Domain Separator | Description |
|------------|-----------------|-------------|
| Keccak     | `0x01`         | Original Keccak submission |
| SHA3       | `0x06`         | FIPS 202 standardized version |
| RawSHAKE   | `0x1F`         | Raw SHAKE variant |

## Implementation Details

In TidyKit's SHA3 implementation (`TidyKit.Crypto.SHA3.pas`), we use `0x06` as the domain separator:

```pascal
// Apply domain separation and padding
PadPos := DataLen mod BlockSize;
Buffer[PadPos] := Buffer[PadPos] xor $06;  // Domain separator for SHA3 (FIPS 202)
Buffer[BlockSize - 1] := Buffer[BlockSize - 1] xor $80;  // Final bit
```

This choice is based on:
1. FIPS 202 standard requirements
2. Reference implementation from the Keccak Team (XKCP)
3. Successful validation against official test vectors

## Test Vectors

Our implementation with `0x06` domain separator produces the correct FIPS 202 SHA3 outputs:

### Empty String Test Vectors
- SHA3-224: `6B4E03423667DBB73B6E15454F0EB1ABD4597F9A1B078E3F5B5A6BC7`
- SHA3-256: `A7FFC6F8BF1ED76651C14756A061D662F580FF4DE43B49FA82D80A4B80F8434A`
- SHA3-384: `0C63A75B845E4F7D01107D852E4C2485C51A50AAAA94FC61995E71BBEE983A2AC3713831264ADB47FB6BD1E058D5F004`
- SHA3-512: `A69F73CCA23A9AC5C8B567DC185A756E97C982164FE25859E0D1DCC1475C80A615B2123AF1F5F94C11E3E9402C3AC558F500199D95B6D3E301758586281DCD26`

### "abc" Test Vectors
- SHA3-224: `E642824C3F8CF24AD09234EE7D3C766FC9A3A5168D0C94AD73B46FDF`
- SHA3-256: `3A985DA74FE225B2045C172D6BD390BD855F086E3E9D525B46BFE24511431532`
- SHA3-384: `EC01498288516FC926459F58E2C6AD8DF9B473CB0FC08C2596DA7CF0E49BE4B298D88CEA927AC7F539F1EDF228376D25`
- SHA3-512: `B751850B1A57168A5693CD924B6B096E08F621827444F70D884F5D0240D2712E10E116E9192AF3C91A7EC57647E3934057340B4CF408D5A56592F8274EEC53F0`

## References

1. NIST FIPS 202 Standard
2. [Keccak Code Package (XKCP)](https://github.com/XKCP/XKCP/blob/master/Standalone/CompactFIPS202/C/Keccak-readable-and-compact.c)
3. [SHA3 Implementation Guide](https://medium.com/better-programming/learning-rust-with-sha-3-and-friends-34c840fcb13)
4. [Test Vectors Source](https://www.di-mgt.com.au/sha_testvectors.html) 