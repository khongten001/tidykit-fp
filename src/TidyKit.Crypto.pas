unit TidyKit.Crypto;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, Base64, MD5, SHA1, BlowFish, Math,
  TidyKit.Crypto.SHA256, TidyKit.Crypto.AES;

type
  { TBlowfishMode
    -------------
    Defines the operation mode for Blowfish encryption/decryption }
  TBlowfishMode = (bmEncrypt, bmDecrypt);

  { AES types }
  TAESKeySize = TidyKit.Crypto.AES.TAESKeySize;
  TAESMode = TidyKit.Crypto.AES.TAESMode;
  TAESBlock = TidyKit.Crypto.AES.TAESBlock;

  { TAESBlockMode
    -------------
    Defines the block operation mode for AES:
    
    - CBC (Cipher Block Chaining):
      * Most widely used traditional mode
      * Requires padding
      * Sequential encryption (not parallelizable)
      * Best for: File encryption, data at rest
      * Provides confidentiality with integrity when used with proper authentication }
  TAESBlockMode = TidyKit.Crypto.AES.TAESBlockMode;

  { TCryptoKit
    ----------
    A comprehensive toolkit for cryptographic operations.
    Provides methods for hashing, encoding, and encryption.
    All methods are static (class functions) for ease of use - 
    no need to create instances. }
  TCryptoKit = class
  private
    FAES: TidyKit.Crypto.AES.TAES;
    class procedure InitBlowfish(var Context: TBlowFish; const Key: string); static;
    class function BytesToHexStr(const Bytes: array of Byte): string; static;
    class function StringToBytes(const Str: string): TBytes; static;
    class function BytesToString(const Bytes: TBytes): string; static;
    class function GetAESKeySize(KeySize: TAESKeySize): Integer; static;
    class function PKCS7Pad(const Data: string; BlockSize: Integer): string; static;
    class function PKCS7Unpad(const Data: string): string; static;
  public
    constructor Create;
    destructor Destroy; override;
    
    { AES encryption/decryption }
    function AESEncrypt(const Data: TBytes; KeySize: TAESKeySize; Mode: TAESMode; const Key, IV: TBytes): TBytes;
    function AESDecrypt(const Data: TBytes; KeySize: TAESKeySize; Mode: TAESMode; const Key, IV: TBytes): TBytes;
    
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

    { AES encryption/decryption with support for multiple modes.
      
      Parameters:
        Text - The string to encrypt/decrypt.
        Key - The encryption key (will be hashed if not exact size).
        IV - Initialization vector (16 bytes, will be hashed if not exact size).
        KeySize - AES key size (128, 192, or 256 bits).
        Mode - Operation mode (ECB, CBC, CTR, GCM).
        
      Returns:
        Encrypted string in Base64 format or decrypted string.
        
      Usage Guide:
        1. For general file encryption, use CBC mode
        2. For streaming data, use CTR mode
        3. For authenticated encryption, use GCM mode
        4. Never use ECB mode for sensitive data
        5. Never reuse IV/nonce with the same key
        6. Use ak256 key size for maximum security }
    class function AESCrypt(const Text, Key, IV: string; KeySize: TAESKeySize;
      Mode: TAESMode): string; static;
  end;

implementation

constructor TCryptoKit.Create;
begin
  FAES := TidyKit.Crypto.AES.TAES.Create;
end;

destructor TCryptoKit.Destroy;
begin
  FAES.Free;
  inherited Destroy;
end;

class function TCryptoKit.BytesToHexStr(const Bytes: array of Byte): string;
var
  I: Integer;
begin
  SetLength(Result, Length(Bytes) * 2);
  for I := 0 to Length(Bytes) - 1 do
    Result := Result + IntToHex(Bytes[I], 2);
end;

class function TCryptoKit.GetAESKeySize(KeySize: TAESKeySize): Integer;
begin
  case KeySize of
    ak128: Result := 16;
    ak192: Result := 24;
    ak256: Result := 32;
  else
    Result := 16;
  end;
end;

class function TCryptoKit.PKCS7Pad(const Data: string; BlockSize: Integer): string;
var
  PadSize: Integer;
  I: Integer;
begin
  PadSize := BlockSize - (Length(Data) mod BlockSize);
  SetLength(Result, Length(Data) + PadSize);
  Move(Data[1], Result[1], Length(Data));
  for I := Length(Data) + 1 to Length(Result) do
    Result[I] := Chr(PadSize);
end;

class function TCryptoKit.PKCS7Unpad(const Data: string): string;
var
  PadSize: Integer;
begin
  if Length(Data) = 0 then
    Exit('');
  PadSize := Ord(Data[Length(Data)]);
  if (PadSize = 0) or (PadSize > Length(Data)) then
    Exit(Data);
  Result := Copy(Data, 1, Length(Data) - PadSize);
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
  Result := TSHA256.HashToString(Text);
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
  BF: TBlowFish;
  Buffer: array of Byte;
  PaddedText: string;
  PaddedLen, I: Integer;
  BFKey: TBlowFishKey;
  KeyLen: Integer;
  Block: TBFBlock;
  Base64: string;
  PadByte: Byte;
begin
  Result := '';
  if (Length(Text) = 0) or (Length(Key) = 0) then
    Exit;

  // Hash the key to get consistent length
  KeyLen := Min(Length(Key), SizeOf(BFKey));
  FillChar(BFKey, SizeOf(BFKey), 0);
  Move(Key[1], BFKey, KeyLen);

  BF := TBlowFish.Create(BFKey, KeyLen);
  try
    case Mode of
      bmEncrypt:
      begin
        // Calculate padded length (PKCS7)
        PaddedLen := ((Length(Text) + 7) div 8) * 8;
        PadByte := PaddedLen - Length(Text);
        if PadByte = 0 then
        begin
          Inc(PaddedLen, 8);
          PadByte := 8;
        end;
        
        // Create padded text
        SetLength(PaddedText, PaddedLen);
        if Length(Text) > 0 then
          Move(Text[1], PaddedText[1], Length(Text));
        for I := Length(Text) + 1 to PaddedLen do
          PaddedText[I] := Chr(PadByte);
        
        // Encrypt
        SetLength(Buffer, PaddedLen);
        Move(PaddedText[1], Buffer[0], PaddedLen);
        for I := 0 to (PaddedLen div 8) - 1 do
        begin
          Move(Buffer[I * 8], Block, 8);
          BF.Encrypt(Block);
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
          Base64 := Base64Decode(Text);
          if (Length(Base64) = 0) or (Length(Base64) mod 8 <> 0) then
            Exit;
            
          SetLength(Buffer, Length(Base64));
          Move(Base64[1], Buffer[0], Length(Base64));
          
          // Decrypt
          for I := 0 to (Length(Buffer) div 8) - 1 do
          begin
            Move(Buffer[I * 8], Block, 8);
            BF.Decrypt(Block);
            Move(Block, Buffer[I * 8], 8);
          end;
          
          // Remove PKCS7 padding
          if Length(Buffer) > 0 then
          begin
            PadByte := Buffer[Length(Buffer) - 1];
            if (PadByte > 0) and (PadByte <= 8) then
            begin
              // Verify padding
              for I := Length(Buffer) - PadByte to Length(Buffer) - 1 do
                if Buffer[I] <> PadByte then
                  Exit;
              SetLength(Buffer, Length(Buffer) - PadByte);
            end;
          end;
          
          // Convert to string
          SetString(Result, PChar(@Buffer[0]), Length(Buffer));
        except
          Result := '';
        end;
      end;
    end;
  finally
    BF.Free;
  end;
end;

class function TCryptoKit.StringToBytes(const Str: string): TBytes;
begin
  SetLength(Result, Length(Str));
  if Length(Str) > 0 then
    Move(Str[1], Result[0], Length(Str));
end;

class function TCryptoKit.BytesToString(const Bytes: TBytes): string;
begin
  SetLength(Result, Length(Bytes));
  if Length(Bytes) > 0 then
    Move(Bytes[0], Result[1], Length(Bytes));
end;

class function TCryptoKit.AESCrypt(const Text, Key, IV: string; KeySize: TAESKeySize;
  Mode: TAESMode): string;
var
  AES: TAES;
  KeyBytes, IVBytes, DataBytes: TBytes;
  ResultBytes: TBytes;
begin
  { Create AES instance }
  AES := TAES.Create(KeySize);
  try
    { Convert inputs to bytes }
    KeyBytes := StringToBytes(Key);
    IVBytes := StringToBytes(IV);
    DataBytes := StringToBytes(Text);
    
    { Set key and IV }
    AES.SetKey(KeyBytes);
    if Mode in [amCBC, amCTR, amGCM] then
      AES.SetIV(IVBytes);
    
    { Set operation mode }
    AES.Mode := Mode;
    
    { Perform encryption }
    ResultBytes := AES.Encrypt(DataBytes);
    
    { Convert result to Base64 }
    Result := Base64Encode(BytesToString(ResultBytes));
  finally
    AES.Free;
  end;
end;

function TCryptoKit.AESEncrypt(const Data: TBytes; KeySize: TAESKeySize; Mode: TAESMode; const Key, IV: TBytes): TBytes;
begin
  FAES.SetKey(Key);
  FAES.Mode := Mode;
  Result := FAES.Encrypt(Data);
end;

function TCryptoKit.AESDecrypt(const Data: TBytes; KeySize: TAESKeySize; Mode: TAESMode; const Key, IV: TBytes): TBytes;
begin
  FAES.SetKey(Key);
  FAES.Mode := Mode;
  Result := FAES.Decrypt(Data);
end;

end. 
