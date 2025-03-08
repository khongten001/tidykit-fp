# 🔒 TidyKit.Crypto Documentation

## Overview

The TidyKit.Crypto module provides a comprehensive cryptographic toolkit with both high-level and low-level interfaces. Key features include:

- AES-256 encryption with CBC and CTR modes
- SHA2 and SHA3 hash functions
- Secure key generation and derivation
- Base64 encoding/decoding
- Legacy support for older algorithms

## AES-256 Encryption

### High-Level Interface

The high-level interface provides string-based operations with automatic Base64 encoding:

```pascal
var
  Key: TAESKey;
  IV: TAESBlock;
  PlainText, CipherText: string;
begin
  // Generate secure key and IV
  Key := TCryptoKit.GenerateRandomKey;
  IV := TCryptoKit.GenerateIV;
  
  // CBC Mode (with PKCS7 padding)
  CipherText := TCryptoKit.AES256EncryptCBC(PlainText, Key, IV);
  PlainText := TCryptoKit.AES256DecryptCBC(CipherText, Key, IV);
  
  // CTR Mode (no padding needed)
  CipherText := TCryptoKit.AES256EncryptCTR(PlainText, Key, IV);
  PlainText := TCryptoKit.AES256DecryptCTR(CipherText, Key, IV);
end;
```

### Low-Level Interface

The low-level interface works with raw bytes and provides configurable padding modes:

```pascal
var
  Key: TAESKey;
  IV: TAESBlock;
  PlainBytes, CipherBytes: TBytes;
begin
  // CBC Mode with PKCS7 padding
  CipherBytes := TAES256.EncryptCBC(PlainBytes, Key, IV, apPKCS7);
  PlainBytes := TAES256.DecryptCBC(CipherBytes, Key, IV, apPKCS7);
  
  // CBC Mode without padding (for NIST test vectors)
  CipherBytes := TAES256.EncryptCBC(PlainBytes, Key, IV, apNone);
  PlainBytes := TAES256.DecryptCBC(CipherBytes, Key, IV, apNone);
  
  // CTR Mode (no padding needed)
  CipherBytes := TAES256.EncryptCTR(PlainBytes, Key, IV);
  PlainBytes := TAES256.DecryptCTR(CipherBytes, Key, IV);
end;
```

### Key Management

```pascal
var
  Key: TAESKey;
  IV: TAESBlock;
begin
  // Generate cryptographically secure random key
  Key := TCryptoKit.GenerateRandomKey;
  
  // Generate random IV
  IV := TCryptoKit.GenerateIV;
  
  // Derive key from password using PBKDF2-SHA256
  Key := TCryptoKit.DeriveKey(
    'password',  // Password
    'salt',      // Salt (empty for random)
    100000       // Iterations
  );
end;
```

## Hash Functions

### Modern Hash Functions

```pascal
// SHA2 Family
Hash := TCryptoKit.SHA256Hash('text');      // SHA-256
Hash := TCryptoKit.SHA512Hash('text');      // SHA-512
Hash := TCryptoKit.SHA512_256Hash('text');  // SHA-512/256

// SHA3 Family
Hash := TCryptoKit.SHA3_224Hash('text');    // SHA3-224
Hash := TCryptoKit.SHA3_256Hash('text');    // SHA3-256
Hash := TCryptoKit.SHA3_384Hash('text');    // SHA3-384
Hash := TCryptoKit.SHA3_512Hash('text');    // SHA3-512
```

### Legacy Hash Functions

```pascal
// Note: These are provided for compatibility only
Hash := TCryptoKit.MD5Hash('text');         // MD5 (not secure)
Hash := TCryptoKit.SHA1Hash('text');        // SHA1 (not secure)
```

## Base64 Encoding

```pascal
// Encode/decode strings
Encoded := TCryptoKit.Base64Encode('text');
Decoded := TCryptoKit.Base64Decode(Encoded);
```

## Legacy Encryption

### Blowfish

```pascal
// Note: Provided for compatibility only
Encrypted := TCryptoKit.BlowfishCrypt('text', 'key', bmEncrypt);
Decrypted := TCryptoKit.BlowfishCrypt(Encrypted, 'key', bmDecrypt);
```

### XOR Encryption

```pascal
// Note: Not cryptographically secure
Encrypted := TCryptoKit.XORCrypt('text', 'key');
Decrypted := TCryptoKit.XORCrypt(Encrypted, 'key');
```

## Security Best Practices

1. Key Generation and Storage
   - Always use `TCryptoKit.GenerateRandomKey` for key generation
   - Use `TCryptoKit.DeriveKey` for password-based keys
   - Never store keys in source code
   - Implement secure key storage
   - Rotate keys periodically

2. IV Handling
   - Always use `TCryptoKit.GenerateIV` for IV generation
   - Never reuse an IV with the same key
   - Store IV alongside ciphertext (it's not secret)
   - Use unique IVs for each encryption operation

3. Mode Selection
   - Use CBC mode with PKCS7 padding for general encryption
   - Use CTR mode for streaming or random access
   - Use raw mode (apNone) only for NIST compliance testing
   - Be aware of padding oracle attacks in CBC mode

4. Hash Selection
   - Use SHA-256 or stronger for general hashing
   - Use SHA-512 for password hashing
   - Avoid MD5 and SHA1 for security-critical operations
   - Consider using dedicated password hashing in the future

## Error Handling

The TidyKit.Crypto module uses dedicated exception classes for different types of cryptographic errors:

- `ECryptoError`: General cryptographic errors (random number generation, key management, etc.)
- `EAESError`: Specific to AES encryption/decryption operations

This allows you to catch specific types of errors:

```pascal
try
  CipherText := TCryptoKit.AES256EncryptCBC(PlainText, Key, IV);
except
  on E: EAESError do
    // Handle AES-specific encryption errors (invalid key length, etc.)
  on E: ECryptoError do
    // Handle general cryptography errors (random number generation, etc.)
  on E: Exception do
    // Handle other types of errors
end;

try
  PlainText := TCryptoKit.AES256DecryptCBC(CipherText, Key, IV);
except
  on E: EAESError do
    // Handle AES-specific decryption errors (invalid padding, etc.)
  on E: ECryptoError do
    // Handle general cryptography errors
  on E: Exception do
    // Handle other types of errors
end;
```

## Standards Compliance

- AES-256 implementation follows NIST FIPS 197
- CBC and CTR modes follow NIST SP 800-38A
- PKCS7 padding follows RFC 5652
- SHA-2 follows FIPS 180-4
- SHA-3 follows FIPS 202
- Test vectors from NIST are supported using raw mode

## Features

- 🔑 **AES-256 Encryption**
  - High-level string operations with automatic Base64 encoding
  - CBC mode with PKCS7 padding
  - CTR mode for streaming
  - NIST SP 800-38A compliant
  - Secure key and IV handling

- 🔒 **Secure Hashing**
  - SHA2 family (SHA-256, SHA-512, SHA-512/256)
  - SHA3 family (SHA3-224, SHA3-256, SHA3-384, SHA3-512)
  - FIPS 180-4 and FIPS 202 compliant

- 📝 **Encoding**
  - Base64 encoding/decoding
  - Binary-to-text conversion

- 🏛️ **Legacy Support**
  - MD5 hashing
  - SHA1 hashing
  - Blowfish encryption
  - XOR encryption

## Usage Examples

### AES-256 Encryption

```pascal
var
  Key: TAESKey;
  IV: TAESBlock;
  PlainText, CipherText: string;
begin
  // Initialize Key and IV (use secure random generation in practice)
  FillChar(Key, SizeOf(Key), 0);
  FillChar(IV, SizeOf(IV), 0);
  
  // CBC Mode - High-level string operations with automatic Base64 encoding
  CipherText := TCryptoKit.AES256EncryptCBC('secret text', Key, IV);
  PlainText := TCryptoKit.AES256DecryptCBC(CipherText, Key, IV);
  
  // CTR Mode - High-level string operations with automatic Base64 encoding
  CipherText := TCryptoKit.AES256EncryptCTR('secret text', Key, IV);
  PlainText := TCryptoKit.AES256DecryptCTR(CipherText, Key, IV);
end;
```

### Low-Level Binary Operations

For NIST compliance testing or when working directly with binary data, use the TAES256 class:

```pascal
var
  Key: TAESKey;
  IV: TAESBlock;
  PlainBytes, CipherBytes: TBytes;
begin
  // Initialize Key and IV (use secure random generation in practice)
  FillChar(Key, SizeOf(Key), 0);
  FillChar(IV, SizeOf(IV), 0);
  
  // CBC Mode - Raw binary operations
  CipherBytes := TAES256.EncryptCBC(PlainBytes, Key, IV);
  PlainBytes := TAES256.DecryptCBC(CipherBytes, Key, IV);
  
  // CTR Mode - Raw binary operations
  CipherBytes := TAES256.EncryptCTR(PlainBytes, Key, IV);
  PlainBytes := TAES256.DecryptCTR(CipherBytes, Key, IV);
end;
```

### Secure Hashing

```pascal
var
  Hash: string;
begin
  // SHA2 family
  Hash := TCryptoKit.SHA256Hash('text');
  Hash := TCryptoKit.SHA512Hash('text');
  Hash := TCryptoKit.SHA512_256Hash('text');
  
  // SHA3 family
  Hash := TCryptoKit.SHA3_224Hash('text');
  Hash := TCryptoKit.SHA3_256Hash('text');
  Hash := TCryptoKit.SHA3_384Hash('text');
  Hash := TCryptoKit.SHA3_512Hash('text');
end;
```

### Base64 Encoding

```pascal
var
  Encoded, Decoded: string;
begin
  Encoded := TCryptoKit.Base64Encode('Hello, World!');
  Decoded := TCryptoKit.Base64Decode(Encoded);
end;
```

### Legacy Operations

```pascal
var
  Hash, Encrypted, Decrypted: string;
begin
  // Legacy hashing (not recommended for security-critical operations)
  Hash := TCryptoKit.MD5Hash('text');
  Hash := TCryptoKit.SHA1Hash('text');
  
  // Blowfish encryption
  Encrypted := TCryptoKit.BlowfishCrypt('text', 'key', bmEncrypt);
  Decrypted := TCryptoKit.BlowfishCrypt(Encrypted, 'key', bmDecrypt);
  
  // XOR encryption (for basic data obfuscation only)
  Encrypted := TCryptoKit.XORCrypt('text', 'key');
  Decrypted := TCryptoKit.XORCrypt(Encrypted, 'key');
end;
```

## Security Best Practices

### Key Management

1. **Key Generation**
   - Use cryptographically secure random number generators
   - Never hardcode keys in source code
   - Use appropriate key lengths (256 bits for AES)

2. **Key Storage**
   - Store keys securely (e.g., hardware security modules, secure key stores)
   - Never store keys alongside encrypted data
   - Use key derivation functions when appropriate

3. **Key Lifecycle**
   - Implement key rotation policies
   - Securely delete keys when no longer needed
   - Have procedures for key compromise recovery

### IV (Initialization Vector) Handling

1. **IV Generation**
   - Use cryptographically secure random number generators
   - Generate a new IV for each encryption operation
   - Never reuse IVs with the same key

2. **IV Storage**
   - IVs can be stored with encrypted data (they're not secret)
   - Ensure IV integrity to prevent tampering
   - Use appropriate IV size (16 bytes for AES)

### Mode Selection

1. **CBC Mode**
   - Use when data integrity is important
   - Always use with PKCS7 padding
   - Be aware of padding oracle attacks
   - Suitable for file encryption

2. **CTR Mode**
   - Use for streaming or random access
   - No padding required
   - Must ensure counter uniqueness
   - Suitable for network protocols

## Error Handling

```pascal
try
  CipherText := TCryptoKit.AES256EncryptCBC(PlainText, Key, IV);
except
  on E: EAESError do
    // Handle AES-specific encryption errors (invalid key length, etc.)
  on E: ECryptoError do
    // Handle general cryptography errors (random number generation, etc.)
  on E: Exception do
    // Handle other types of errors
end;

try
  PlainText := TCryptoKit.AES256DecryptCBC(CipherText, Key, IV);
except
  on E: EAESError do
    // Handle AES-specific decryption errors (invalid padding, etc.)
  on E: ECryptoError do
    // Handle general cryptography errors
  on E: Exception do
    // Handle other types of errors
end;
```

## Performance Considerations

1. **Hash Selection**
   - SHA-512 is faster on 64-bit systems
   - SHA-256 is faster on 32-bit systems
   - SHA3 has consistent performance across platforms

2. **Encryption Modes**
   - CTR mode is parallelizable
   - CBC mode requires sequential processing
   - Consider buffer sizes for large data

3. **Memory Usage**
   - AES operations work on 16-byte blocks
   - Hash functions have fixed memory overhead
   - Base64 encoding increases size by ~33%

## Standards Compliance

1. **AES-256**
   - NIST FIPS 197 compliant
   - NIST SP 800-38A compliant modes (CBC, CTR)
   - PKCS7 padding (RFC 5652)

2. **Hash Functions**
   - SHA2: FIPS 180-4 compliant
   - SHA3: FIPS 202 compliant
   - Test vectors from NIST

## Migration Guide

### From Legacy to Modern Crypto

1. **Hashing**
   ```pascal
   // Old (not recommended)
   Hash := TCryptoKit.MD5Hash(Text);
   
   // New (recommended)
   Hash := TCryptoKit.SHA256Hash(Text);  // or SHA3_256Hash
   ```

2. **Encryption**
   ```pascal
   // Old (not recommended)
   Encrypted := TCryptoKit.BlowfishCrypt(Text, Key, bmEncrypt);
   
   // New (recommended)
   Encrypted := TCryptoKit.AES256EncryptCBC(Text, Key, IV);
   ```

## Future Development

1. **Planned Features**
   - Key derivation functions (PBKDF2, Argon2)
   - Authenticated encryption (AES-GCM)
   - Digital signatures
   - Certificate handling

2. **Deprecation Schedule**
   - MD5 and SHA1 marked as legacy
   - Blowfish marked as legacy
   - XOR encryption marked as basic

## Contributing

We welcome contributions to improve the crypto module. Please ensure:

1. All changes are thoroughly tested
2. Security implications are documented
3. Test vectors are included
4. Best practices are followed

## License

This module is part of TidyKit and is licensed under the same terms as the main project. 

## Architecture

### High-Level vs Low-Level APIs

1. **TCryptoKit (High-Level)**
   - String-based operations
   - Automatic Base64 encoding/decoding
   - Easy to use for common scenarios
   - Handles binary-to-text conversion

2. **TAES256 (Low-Level)**
   - Raw binary operations
   - NIST test vectors compliance
   - Direct byte array manipulation
   - No automatic encoding

Choose the appropriate API based on your needs:
- Use TCryptoKit for general application development
- Use TAES256 for cryptographic protocol implementation or testing 