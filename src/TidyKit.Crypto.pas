unit TidyKit.Crypto;

{$mode objfpc}{$H+}{$J-}

{*******************************************************************************
  TidyKit.Crypto - High-Level Cryptographic Operations Interface
  
  This unit provides a high-level wrapper around various cryptographic operations,
  making them easy to use with string inputs and outputs. It handles:
  - String to bytes conversion
  - Base64 encoding of binary cryptographic outputs
  - Base64 decoding of encrypted data for decryption
  
  Base64 encoding is used because encrypted binary data cannot be safely stored
  in strings (they may contain null bytes or non-printable characters).
  This ensures encrypted data can be safely stored and transmitted as text.
  
  For low-level binary operations and NIST compliance testing, use the underlying
  implementation units directly (e.g., TidyKit.Crypto.AES256).
  
  AES-256 Implementation:
  1. High-Level Interface (this unit)
     - String-based operations with automatic Base64 encoding
     - Always uses PKCS7 padding in CBC mode for safety
     - CTR mode for streaming (no padding needed)
     - Suitable for most applications
  
  2. Low-Level Interface (TidyKit.Crypto.AES256)
     - Raw binary operations
     - Configurable padding modes:
       * PKCS7 for general use
       * No padding for NIST testing
     - Direct byte array manipulation
     - Used for testing and special protocols
  
  @author   TidyKit Team
  @version  1.0
  @date     2024
*******************************************************************************}

interface

uses
  Classes, SysUtils, Base64, MD5, SHA1, BlowFish, Math, TidyKit.Crypto.SHA2, TidyKit.Crypto.SHA3, TidyKit.Crypto.AES256
{$IFDEF MSWINDOWS}
  , Windows
{$ENDIF}
  ;

const
{$IFDEF MSWINDOWS}
  PROV_RSA_FULL = 1;
  CRYPT_VERIFYCONTEXT = $F0000000;
{$ENDIF}

type
  { TBlowfishMode
    -------------
    Defines the operation mode for Blowfish encryption/decryption }
  TBlowfishMode = (bmEncrypt, bmDecrypt);

  { TAESKey - 256-bit key for AES encryption }
  TAESKey = TidyKit.Crypto.AES256.TAESKey;

  { TAESBlock - 128-bit block for AES encryption }
  TAESBlock = TidyKit.Crypto.AES256.TAESBlock;

type
  { TCryptoKit
    ----------
    A comprehensive toolkit for cryptographic operations.
    Provides methods for hashing, encoding, and encryption.
    All methods are static (class functions) for ease of use - 
    no need to create instances. }
  TCryptoKit = class
  private
    class procedure InitBlowfish(var Context: TBlowFish; const Key: string); static;
    class function BytesToHexStr(const Bytes: array of Byte): string; static;
    class procedure FillRandomBytes(var Buffer; Count: Integer); static;
  public    
    { Generates a cryptographically secure random AES-256 key.
      Uses OS-provided secure random number generator when available.
      
      Returns:
        A randomly generated 256-bit key suitable for AES encryption. }
    class function GenerateRandomKey: TAESKey; static;
    
    { Generates a cryptographically secure random initialization vector.
      Uses OS-provided secure random number generator when available.
      
      Returns:
        A randomly generated 128-bit block suitable for use as an IV. }
    class function GenerateIV: TAESBlock; static;
    
    { Derives an AES-256 key from a password using PBKDF2-SHA256.
      
      Parameters:
        Password - The password to derive the key from.
        Salt - Optional salt value (if not provided, a random salt will be used).
        Iterations - Number of iterations (default: 100000).
        
      Returns:
        A 256-bit key derived from the password using PBKDF2. }
    class function DeriveKey(const Password: string; const Salt: string;
      Iterations: Integer): TAESKey; static;
    
    { Computes MD5 hash of a string.
      
      Parameters:
        Text - The string to hash.
        
      Returns:
        MD5 hash as a hexadecimal string. }
    class function MD5Hash(const Text: string): string; static;
    
    { Computes SHA1 hash of a string.
      
      Parameters:
        Text - The string to hash.
        
      Returns:
        SHA1 hash as a hexadecimal string. }
    class function SHA1Hash(const Text: string): string; static;
    
    { Computes SHA256 hash of a string.
      
      Parameters:
        Text - The string to hash.
        
      Returns:
        SHA256 hash as a hexadecimal string. }
    class function SHA256Hash(const Text: string): string; static;
    
    { Computes SHA512 hash of a string.
      
      Parameters:
        Text - The string to hash.
        
      Returns:
        SHA512 hash as a hexadecimal string. }
    class function SHA512Hash(const Text: string): string; static;
    
    { Computes SHA512/256 hash of a string.
      This is a truncated version of SHA512, providing 256-bit security.
      
      Parameters:
        Text - The string to hash.
        
      Returns:
        SHA512/256 hash as a hexadecimal string. }
    class function SHA512_256Hash(const Text: string): string; static;

    { Computes SHA3-224 hash of a string.
      
      Parameters:
        Text - The string to hash.
        
      Returns:
        SHA3-224 hash as a hexadecimal string. }
    class function SHA3_224Hash(const Text: string): string; static;
    
    { Computes SHA3-256 hash of a string.
      
      Parameters:
        Text - The string to hash.
        
      Returns:
        SHA3-256 hash as a hexadecimal string. }
    class function SHA3_256Hash(const Text: string): string; static;
    
    { Computes SHA3-384 hash of a string.
      
      Parameters:
        Text - The string to hash.
        
      Returns:
        SHA3-384 hash as a hexadecimal string. }
    class function SHA3_384Hash(const Text: string): string; static;
    
    { Computes SHA3-512 hash of a string.
      
      Parameters:
        Text - The string to hash.
        
      Returns:
        SHA3-512 hash as a hexadecimal string. }
    class function SHA3_512Hash(const Text: string): string; static;
    
    { Encodes a string to Base64.
      
      Parameters:
        Text - The string to encode.
        
      Returns:
        Base64 encoded string. }
    class function Base64Encode(const Text: string): string; static;
    
    { Decodes a Base64 string.
      
      Parameters:
        Base64Text - The Base64 string to decode.
        
      Returns:
        Decoded string. }
    class function Base64Decode(const Base64Text: string): string; static;
    
    { Simple XOR encryption/decryption of a string.
      
      Parameters:
        Text - The string to encrypt/decrypt.
        Key - The encryption key.
        
      Returns:
        Encrypted/decrypted string. }
    class function XORCrypt(const Text, Key: string): string; static;

    { Blowfish encryption/decryption of a string.
      
      Parameters:
        Text - The string to encrypt/decrypt.
        Key - The encryption key (up to 56 bytes).
        Mode - Encryption or decryption mode.
        
      Returns:
        Encrypted/decrypted string in Base64 format. }
    class function BlowfishCrypt(const Text, Key: string; Mode: TBlowfishMode): string; static;

    { Encrypts data using AES-256 in CBC mode.
      This is a high-level wrapper that handles string conversion and Base64 encoding.
      For raw binary operations, use TAES256.EncryptCBC directly.
      
      Parameters:
        Data - The string data to encrypt.
        Key - 256-bit encryption key.
        IV - 128-bit initialization vector.
        
      Returns:
        Base64-encoded encrypted data string. The Base64 encoding ensures the binary
        encrypted data can be safely stored and transmitted as text. }
    class function AES256EncryptCBC(const Data: string; const Key: TAESKey; const IV: TAESBlock): string; static;
    
    { Decrypts data using AES-256 in CBC mode.
      This is a high-level wrapper that handles Base64 decoding and string conversion.
      For raw binary operations, use TAES256.DecryptCBC directly.
      
      Parameters:
        Base64Data - The Base64-encoded encrypted data string.
        Key - 256-bit encryption key.
        IV - 128-bit initialization vector.
        
      Returns:
        Decrypted string data. }
    class function AES256DecryptCBC(const Base64Data: string; const Key: TAESKey; const IV: TAESBlock): string; static;
    
    { Encrypts data using AES-256 in CTR mode.
      This is a high-level wrapper that handles string conversion and Base64 encoding.
      For raw binary operations, use TAES256.EncryptCTR directly.
      
      Parameters:
        Data - The string data to encrypt.
        Key - 256-bit encryption key.
        IV - 128-bit initialization vector (nonce + counter).
        
      Returns:
        Base64-encoded encrypted data string. The Base64 encoding ensures the binary
        encrypted data can be safely stored and transmitted as text. }
    class function AES256EncryptCTR(const Data: string; const Key: TAESKey; const IV: TAESBlock): string; static;
    
    { Decrypts data using AES-256 in CTR mode.
      This is a high-level wrapper that handles Base64 decoding and string conversion.
      For raw binary operations, use TAES256.DecryptCTR directly.
      
      Parameters:
        Base64Data - The Base64-encoded encrypted data string.
        Key - 256-bit encryption key.
        IV - 128-bit initialization vector (nonce + counter).
        
      Returns:
        Decrypted string data. }
    class function AES256DecryptCTR(const Base64Data: string; const Key: TAESKey; const IV: TAESBlock): string; static;
  end;

implementation

{$IFDEF MSWINDOWS}
function CryptoAcquireContext(var hProv: THandle; pszContainer: PChar;
  pszProvider: PChar; dwProvType: DWORD; dwFlags: DWORD): BOOL; stdcall;
  external advapi32 name 'CryptAcquireContextA';

function CryptoGenRandom(hProv: THandle; dwLen: DWORD; pbBuffer: Pointer): BOOL;
  stdcall; external advapi32 name 'CryptGenRandom';

function CryptoReleaseContext(hProv: THandle; dwFlags: DWORD): BOOL; stdcall;
  external advapi32 name 'CryptReleaseContext';
{$ENDIF}

class function TCryptoKit.BytesToHexStr(const Bytes: array of Byte): string;
var
  I: Integer;
begin
  SetLength(Result, Length(Bytes) * 2);
  for I := 0 to Length(Bytes) - 1 do
    Result := Result + IntToHex(Bytes[I], 2);
end;

class procedure TCryptoKit.InitBlowfish(var Context: TBlowFish; const Key: string);
var
  BFKey: TBlowFishKey;
  KeyLen: Integer;
begin
  KeyLen := Length(Key);
  if KeyLen > SizeOf(BFKey) then
    KeyLen := SizeOf(BFKey);
  FillChar(BFKey, SizeOf(BFKey), 0);
  if KeyLen > 0 then
    Move(Key[1], BFKey, KeyLen);
  Context := TBlowFish.Create(BFKey, KeyLen);
end;

class function TCryptoKit.MD5Hash(const Text: string): string;
begin
  Result := MD5Print(MD5String(Text));
end;

class function TCryptoKit.SHA1Hash(const Text: string): string;
begin
  Result := SHA1Print(SHA1String(Text));
end;

class function TCryptoKit.SHA256Hash(const Text: string): string;
begin
  Result := TSHA2.SHA256(Text);
end;

class function TCryptoKit.SHA512Hash(const Text: string): string;
begin
  Result := TSHA2.SHA512(Text);
end;

class function TCryptoKit.SHA512_256Hash(const Text: string): string;
begin
  Result := TSHA2.SHA512_256(Text);
end;

class function TCryptoKit.SHA3_224Hash(const Text: string): string;
begin
  Result := TSHA3.SHA3_224(Text);
end;

class function TCryptoKit.SHA3_256Hash(const Text: string): string;
begin
  Result := TSHA3.SHA3_256(Text);
end;

class function TCryptoKit.SHA3_384Hash(const Text: string): string;
begin
  Result := TSHA3.SHA3_384(Text);
end;

class function TCryptoKit.SHA3_512Hash(const Text: string): string;
begin
  Result := TSHA3.SHA3_512(Text);
end;

class function TCryptoKit.Base64Encode(const Text: string): string;
begin
  Result := EncodeStringBase64(Text);
end;

class function TCryptoKit.Base64Decode(const Base64Text: string): string;
begin
  Result := DecodeStringBase64(Base64Text);
end;

class function TCryptoKit.XORCrypt(const Text, Key: string): string;
var
  I, J: Integer;
begin
  if (Length(Text) = 0) or (Length(Key) = 0) then
    Exit('');

  SetLength(Result, Length(Text));
  J := 1;
  for I := 1 to Length(Text) do
  begin
    Result[I] := Chr(Ord(Text[I]) xor Ord(Key[J]));
    Inc(J);
    if J > Length(Key) then
      J := 1;
  end;
end;

class function TCryptoKit.BlowfishCrypt(const Text, Key: string; Mode: TBlowfishMode): string;
var
  Context: TBlowFish;
  Buffer: array of Byte;
  PaddedText: string;
  PaddedLen, I: Integer;
  Block: TBFBlock;
begin
  if (Length(Text) = 0) or (Length(Key) = 0) then
    Exit('');

  InitBlowfish(Context, Key);
  try
    case Mode of
      bmEncrypt:
        begin
          // Calculate padded length (multiple of 8 bytes)
          PaddedLen := ((Length(Text) + 7) div 8) * 8;
          
          // Create padded text
          SetLength(PaddedText, PaddedLen);
          if Length(Text) > 0 then
            Move(Text[1], PaddedText[1], Length(Text));
          for I := Length(Text) + 1 to PaddedLen do
            PaddedText[I] := Chr(PaddedLen - Length(Text));
          
          // Encrypt
          SetLength(Buffer, PaddedLen);
          Move(PaddedText[1], Buffer[0], PaddedLen);
          for I := 0 to (PaddedLen div 8) - 1 do
          begin
            Move(Buffer[I * 8], Block, 8);
            Context.Encrypt(Block);
            Move(Block, Buffer[I * 8], 8);
          end;
          
          // Convert to Base64
          SetString(Result, PChar(@Buffer[0]), Length(Buffer));
          Result := Base64Encode(Result);
        end;
      
      bmDecrypt:
        begin
          try
            // Decode Base64
            PaddedText := Base64Decode(Text);
            if (Length(PaddedText) = 0) or (Length(PaddedText) mod 8 <> 0) then
              Exit('');
            
            SetLength(Buffer, Length(PaddedText));
            Move(PaddedText[1], Buffer[0], Length(PaddedText));
            
            // Decrypt
            for I := 0 to (Length(Buffer) div 8) - 1 do
            begin
              Move(Buffer[I * 8], Block, 8);
              Context.Decrypt(Block);
              Move(Block, Buffer[I * 8], 8);
            end;
            
            // Remove padding
            if Length(Buffer) > 0 then
            begin
              I := Buffer[Length(Buffer) - 1];
              if (I > 0) and (I <= 8) then
                SetLength(Buffer, Length(Buffer) - I);
            end;
            
            // Convert to string
            SetString(Result, PChar(@Buffer[0]), Length(Buffer));
          except
            Result := '';
          end;
        end;
    end;
  finally
    Context.Free;
  end;
end;

{ TCryptoKit - AES-256 Methods }

class function TCryptoKit.AES256EncryptCBC(const Data: string; const Key: TAESKey; const IV: TAESBlock): string;
var
  DataBytes, PaddedBytes, EncryptedBytes: TBytes;
  I, LastBlockSize, PaddingSize: Integer;
  RawStr: string;
begin
  if Length(Data) = 0 then
    Exit('');
    
  // Convert string to bytes preserving all values
  SetLength(DataBytes, Length(Data));
  for I := 1 to Length(Data) do
    DataBytes[I-1] := Byte(Data[I]);
    
  // Add PKCS7 padding
  LastBlockSize := Length(DataBytes) mod 16;
  PaddingSize := 16 - LastBlockSize;
  SetLength(PaddedBytes, Length(DataBytes) + PaddingSize);
  Move(DataBytes[0], PaddedBytes[0], Length(DataBytes));
  for I := Length(DataBytes) to Length(PaddedBytes) - 1 do
    PaddedBytes[I] := PaddingSize;
    
  // Encrypt the padded data
  EncryptedBytes := TAES256.EncryptCBC(PaddedBytes, Key, IV);
  
  // Convert encrypted bytes to raw string for Base64
  SetLength(RawStr, Length(EncryptedBytes));
  for I := 1 to Length(EncryptedBytes) do
    RawStr[I] := Chr(EncryptedBytes[I-1]);
  Result := EncodeStringBase64(RawStr);
end;

class function TCryptoKit.AES256DecryptCBC(const Base64Data: string; const Key: TAESKey; const IV: TAESBlock): string;
var
  EncryptedData, DecryptedBytes: TBytes;
  DecodedStr: string;
  I, PaddingSize: Integer;
begin
  if Length(Base64Data) = 0 then
    Exit('');
    
  // Decode Base64 to raw string first
  DecodedStr := DecodeStringBase64(Base64Data);
  
  // Convert raw string to bytes
  SetLength(EncryptedData, Length(DecodedStr));
  for I := 1 to Length(DecodedStr) do
    EncryptedData[I-1] := Byte(DecodedStr[I]);
  
  // Decrypt the data
  DecryptedBytes := TAES256.DecryptCBC(EncryptedData, Key, IV);
  
  // Verify and remove PKCS7 padding
  if Length(DecryptedBytes) = 0 then
    Exit('');
    
  PaddingSize := DecryptedBytes[Length(DecryptedBytes) - 1];
  if (PaddingSize = 0) or (PaddingSize > 16) then
    raise EAESError.Create('Invalid padding');
    
  // Verify all padding bytes match
  for I := Length(DecryptedBytes) - PaddingSize to Length(DecryptedBytes) - 1 do
    if DecryptedBytes[I] <> PaddingSize then
      raise EAESError.Create('Invalid padding');
  
  // Convert decrypted bytes back to string, removing padding
  SetLength(Result, Length(DecryptedBytes) - PaddingSize);
  for I := 1 to Length(Result) do
    Result[I] := Chr(DecryptedBytes[I-1]);
end;

class function TCryptoKit.AES256EncryptCTR(const Data: string; const Key: TAESKey; const IV: TAESBlock): string;
var
  DataBytes, EncryptedBytes: TBytes;
  I: Integer;
  RawStr: string;
begin
  if Length(Data) = 0 then
    Exit('');
    
  // Convert string to bytes preserving all values
  SetLength(DataBytes, Length(Data));
  for I := 0 to Length(Data) - 1 do
    DataBytes[I] := Byte(Data[I + 1]);
    
  // Encrypt the data
  EncryptedBytes := TAES256.EncryptCTR(DataBytes, Key, IV);
  
  // Convert bytes to Base64 directly
  SetLength(RawStr, Length(EncryptedBytes));
  for I := 0 to Length(EncryptedBytes) - 1 do
    RawStr[I + 1] := Chr(EncryptedBytes[I]);
  Result := EncodeStringBase64(RawStr);
end;

class function TCryptoKit.AES256DecryptCTR(const Base64Data: string; const Key: TAESKey; const IV: TAESBlock): string;
var
  EncryptedData, DecryptedBytes: TBytes;
  DecodedStr: string;
  I: Integer;
begin
  if Length(Base64Data) = 0 then
    Exit('');
    
  // Decode Base64 to raw string first
  DecodedStr := DecodeStringBase64(Base64Data);
  
  // Convert raw string to bytes
  SetLength(EncryptedData, Length(DecodedStr));
  for I := 0 to Length(DecodedStr) - 1 do
    EncryptedData[I] := Byte(DecodedStr[I + 1]);
  
  // Decrypt the data
  DecryptedBytes := TAES256.DecryptCTR(EncryptedData, Key, IV);
  
  // Convert decrypted bytes back to string
  SetLength(Result, Length(DecryptedBytes));
  for I := 0 to Length(DecryptedBytes) - 1 do
    Result[I + 1] := Chr(DecryptedBytes[I]);
end;

class procedure TCryptoKit.FillRandomBytes(var Buffer; Count: Integer);
{$IFDEF MSWINDOWS}
var
  hProv: THandle;
begin
  if not CryptoAcquireContext(hProv, nil, nil, PROV_RSA_FULL, CRYPT_VERIFYCONTEXT) then
    raise Exception.Create('Failed to acquire crypto context');
  try
    if not CryptoGenRandom(hProv, Count, @Buffer) then
      raise Exception.Create('Failed to generate random bytes');
  finally
    CryptoReleaseContext(hProv, 0);
  end;
{$ELSE}
var
  F: file;
begin
  AssignFile(F, '/dev/urandom');
  try
    Reset(F, 1);
    BlockRead(F, Buffer, Count);
  finally
    CloseFile(F);
  end;
{$ENDIF}
end;

class function TCryptoKit.GenerateRandomKey: TAESKey;
begin
  FillRandomBytes(Result, SizeOf(TAESKey));
end;

class function TCryptoKit.GenerateIV: TAESBlock;
begin
  FillRandomBytes(Result, SizeOf(TAESBlock));
end;

class function TCryptoKit.DeriveKey(const Password: string; const Salt: string;
  Iterations: Integer): TAESKey;
{$R-} // Disable range checking for array operations
var
  I, J: Integer;
  Counter: Cardinal;
  Block: array[0..31] of Byte;
  SaltBytes: array of Byte;
  PasswordBytes: array of Byte;
  TempKey: array[0..31] of Byte;
  Temp: array[0..31] of Byte;
  HashStr: string;
begin
  // Initialize result
  FillChar(Result, SizeOf(Result), 0);
  
  // Convert password and salt to bytes
  SetLength(PasswordBytes, Length(Password));
  for I := 0 to Length(Password) - 1 do
    PasswordBytes[I] := Byte(Password[I + 1]);
    
  if Salt = '' then
  begin
    SetLength(SaltBytes, 16);
    FillRandomBytes(SaltBytes[0], 16);
  end
  else
  begin
    SetLength(SaltBytes, Length(Salt));
    for I := 0 to Length(Salt) - 1 do
      SaltBytes[I] := Byte(Salt[I + 1]);
  end;

  // Initialize working arrays
  FillChar(Block, SizeOf(Block), 0);
  FillChar(TempKey, SizeOf(TempKey), 0);
  FillChar(Temp, SizeOf(Temp), 0);

  // PBKDF2-SHA256 implementation
  Counter := 1;
  
  // First pass
  SetString(HashStr, PChar(@PasswordBytes[0]), Length(PasswordBytes));
  HashStr := HashStr + string(PChar(@SaltBytes[0]));
  HashStr := HashStr + string(PChar(@Counter));
  HashStr := TSHA2.SHA256(HashStr);
  HexToBin(PChar(HashStr), @Block[0], 32);
  Move(Block, TempKey, 32);
  
  // Additional iterations
  for I := 2 to Iterations do
  begin
    HashStr := string(PChar(@PasswordBytes[0])) + string(PChar(@Block[0]));
    HashStr := TSHA2.SHA256(HashStr);
    HexToBin(PChar(HashStr), @Block[0], 32);
    
    Move(Block, Temp, 32);
    for J := 0 to 31 do
      TempKey[J] := TempKey[J] xor Temp[J];
  end;
  
  // XOR into final key
  for I := 0 to 31 do
    Result[I] := Result[I] xor TempKey[I];
end;
{$R+} // Re-enable range checking

end. 
