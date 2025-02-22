# 🔒 TidyKit.Crypto Documentation

## Overview

The TidyKit.Crypto module provides a comprehensive set of cryptographic operations, including modern encryption algorithms, secure hashing functions, and encoding utilities. All functionality is exposed through the `TCryptoKit` class, which provides static methods for ease of use.

## Features

- 🔑 **AES-256 Encryption**
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
  
  // CBC Mode
  CipherText := TCryptoKit.AES256EncryptCBC('secret text', Key, IV);
  PlainText := TCryptoKit.AES256DecryptCBC(CipherText, Key, IV);
  
  // CTR Mode
  CipherText := TCryptoKit.AES256EncryptCTR('secret text', Key, IV);
  PlainText := TCryptoKit.AES256DecryptCTR(CipherText, Key, IV);
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
    // Handle encryption errors
  on E: Exception do
    // Handle other errors
end;

try
  PlainText := TCryptoKit.AES256DecryptCBC(CipherText, Key, IV);
except
  on E: EAESError do
    // Handle decryption errors (including padding errors)
  on E: Exception do
    // Handle other errors
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