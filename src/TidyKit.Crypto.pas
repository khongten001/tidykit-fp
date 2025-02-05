unit TidyKit.Crypto;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, Base64, MD5, SHA1, BlowFish, Math, 
  TidyKit.Crypto.SHA2, TidyKit.Crypto.SHA3, TidyKit.Crypto.AES;

type
  { TBlowfishMode
    -------------
    Defines the operation mode for Blowfish encryption/decryption }
  TBlowfishMode = (bmEncrypt, bmDecrypt);

  { Re-export AES types }
  TAESMode = TidyKit.Crypto.AES.TAESMode;
  TAESGCMResult = TidyKit.Crypto.AES.TAESGCMResult;
  ETidyKitAESException = TidyKit.Crypto.AES.ETidyKitAESException;

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
  public    
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

    { AES-256-GCM encryption.
      
      Parameters:
        PlainText - The data to encrypt.
        Key - The 32-byte encryption key.
        IV - The 12-byte initialization vector.
        AAD - Optional additional authenticated data.
        
      Returns:
        A record containing the ciphertext and authentication tag. }
    class function AES256GCMEncrypt(const PlainText: TBytes; const Key: TBytes; 
      const IV: TBytes; const AAD: TBytes = nil): TAESGCMResult; static;
    
    { AES-256-GCM decryption.
      
      Parameters:
        CipherText - The data to decrypt.
        Key - The 32-byte encryption key.
        IV - The 12-byte initialization vector.
        Tag - The 16-byte authentication tag.
        AAD - Optional additional authenticated data.
        
      Returns:
        The decrypted data.
        
      Raises:
        ETidyKitAESException if authentication fails. }
    class function AES256GCMDecrypt(const CipherText: TBytes; const Key: TBytes;
      const IV: TBytes; const Tag: TBytes; const AAD: TBytes = nil): TBytes; static;
    
    { AES-256-CTR encryption.
      
      Parameters:
        PlainText - The data to encrypt.
        Key - The 32-byte encryption key.
        IV - The 16-byte initialization vector/nonce.
        
      Returns:
        The encrypted data. }
    class function AES256CTREncrypt(const PlainText: TBytes; const Key: TBytes;
      const IV: TBytes): TBytes; static;
    
    { AES-256-CTR decryption.
      
      Parameters:
        CipherText - The data to decrypt.
        Key - The 32-byte encryption key.
        IV - The 16-byte initialization vector/nonce.
        
      Returns:
        The decrypted data. }
    class function AES256CTRDecrypt(const CipherText: TBytes; const Key: TBytes;
      const IV: TBytes): TBytes; static;
  end;

implementation

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

{ AES-256 methods }

class function TCryptoKit.AES256GCMEncrypt(const PlainText: TBytes; const Key: TBytes;
  const IV: TBytes; const AAD: TBytes = nil): TAESGCMResult;
begin
  Result := TAES256.EncryptGCM(PlainText, Key, IV, AAD);
end;

class function TCryptoKit.AES256GCMDecrypt(const CipherText: TBytes; const Key: TBytes;
  const IV: TBytes; const Tag: TBytes; const AAD: TBytes = nil): TBytes;
begin
  Result := TAES256.DecryptGCM(CipherText, Key, IV, Tag, AAD);
end;

class function TCryptoKit.AES256CTREncrypt(const PlainText: TBytes; const Key: TBytes;
  const IV: TBytes): TBytes;
begin
  Result := TAES256.EncryptCTR(PlainText, Key, IV);
end;

class function TCryptoKit.AES256CTRDecrypt(const CipherText: TBytes; const Key: TBytes;
  const IV: TBytes): TBytes;
begin
  Result := TAES256.DecryptCTR(CipherText, Key, IV);
end;

end. 
