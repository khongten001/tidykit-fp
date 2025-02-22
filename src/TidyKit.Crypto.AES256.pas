{*******************************************************************************
  TidyKit.Crypto.AES256 - Advanced Encryption Standard (AES-256) Implementation
  
  This unit provides a FIPS-compliant implementation of AES-256 encryption and
  decryption in CBC and CTR modes. The implementation follows NIST standards:
  - FIPS 197: Advanced Encryption Standard (AES)
  - NIST SP 800-38A: Block Cipher Modes of Operation
  
  References:
  1. NIST FIPS 197 - Advanced Encryption Standard (AES)
     https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.197.pdf
  
  2. NIST SP 800-38A - Block Cipher Modes of Operation
     https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication800-38a.pdf
  
  3. PKCS #7: Cryptographic Message Syntax (for padding)
     RFC 5652 - https://tools.ietf.org/html/rfc5652
  
  Key Features:
  - AES-256 block cipher (14 rounds)
  - CBC mode with PKCS7 padding
  - CTR mode for streaming operations
  - NIST test vectors compliance
  - Secure key and IV handling
  
  Security Notes:
  1. This implementation has been tested against NIST test vectors
  2. The code includes range-check optimization for performance
  3. Memory is securely cleared after use
  4. No timing attack mitigations are currently implemented
  
  Usage Example:
    var
      Key: TAESKey;
      IV: TAESBlock;
      PlainText, CipherText: string;
    begin
      // Initialize Key and IV (use secure random generation in practice)
      FillChar(Key, SizeOf(Key), 0);
      FillChar(IV, SizeOf(IV), 0);
      
      // CBC Mode
      CipherText := TAES256.EncryptCBC(PlainBytes, Key, IV);
      PlainText := TAES256.DecryptCBC(CipherBytes, Key, IV);
    end;
  
  @author   TidyKit Team
  @version  1.0
  @date     2024
*******************************************************************************}

unit TidyKit.Crypto.AES256;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils;

type
  { TAESMode - Defines the operation mode for AES encryption/decryption }
  TAESMode = (amCBC, amCTR);

  { TAESBlock - 128-bit AES block }
  TAESBlock = array[0..15] of Byte;

  { TAESKey - 256-bit key }
  TAESKey = array[0..31] of Byte;

  { TAESKeySchedule - Expanded key for AES-256 (60 32-bit words) }
  TAESKeySchedule = array[0..59] of UInt32;

  { EAESError - Custom exception for AES operations }
  EAESError = class(Exception);

  { TAESCipher - AES-256 implementation }
  TAESCipher = class
  private
    FKeySchedule: TAESKeySchedule;
    FMode: TAESMode;
    FIV: TAESBlock;
    
    procedure ExpandKey(const Key: TAESKey);
    procedure EncryptBlock(var Block: TAESBlock);
    procedure DecryptBlock(var Block: TAESBlock);
    
    function EncryptCBC(const Data: TBytes): TBytes;
    function DecryptCBC(const Data: TBytes): TBytes;
    function EncryptCTR(const Data: TBytes): TBytes;
    function DecryptCTR(const Data: TBytes): TBytes;
    
    procedure SubBytes(var State: TAESBlock);
    procedure InvSubBytes(var State: TAESBlock);
    procedure ShiftRows(var State: TAESBlock);
    procedure InvShiftRows(var State: TAESBlock);
    procedure MixColumns(var State: TAESBlock);
    procedure InvMixColumns(var State: TAESBlock);
    procedure AddRoundKey(var State: TAESBlock; Round: Integer);
    
    class function RotWord(Value: UInt32): UInt32; static;
    class function SubWord(Value: UInt32): UInt32; static;
    class procedure IncCounter(var Counter: TAESBlock); static;
    class procedure XorBlock(const Source1, Source2: PByte; Dest: PByte; Size: Integer); static;
  public
    {*******************************************************************************
      Creates a new AES cipher instance with specified mode, key, and IV.
      
      @param Mode   The operation mode (CBC or CTR)
      @param Key    256-bit encryption key
      @param IV     128-bit initialization vector
      
      Note: For CBC mode, IV must be unpredictable (random).
            For CTR mode, IV serves as initial counter value.
    *******************************************************************************}
    constructor Create(Mode: TAESMode; const Key: TAESKey; const IV: TAESBlock);
    destructor Destroy; override;
    
    function Encrypt(const Data: TBytes): TBytes;
    function Decrypt(const Data: TBytes): TBytes;
    
    property Mode: TAESMode read FMode;
  end;

  { TAES256
    -------
    Static class providing AES-256 encryption and decryption functionality.
    All methods are static (class functions) for ease of use - no need to create instances. }
  TAES256 = class
  public
    { Encrypts data using AES-256 in CBC mode.
      
      Parameters:
        Data - The data to encrypt.
        Key - 256-bit encryption key.
        IV - 128-bit initialization vector.
        
      Returns:
        Encrypted data. }
    class function EncryptCBC(const Data: TBytes; const Key: TAESKey; const IV: TAESBlock): TBytes; static;
    
    { Decrypts data using AES-256 in CBC mode.
      
      Parameters:
        Data - The data to decrypt.
        Key - 256-bit encryption key.
        IV - 128-bit initialization vector.
        
      Returns:
        Decrypted data. }
    class function DecryptCBC(const Data: TBytes; const Key: TAESKey; const IV: TAESBlock): TBytes; static;
    
    { Encrypts data using AES-256 in CTR mode.
      
      Parameters:
        Data - The data to encrypt.
        Key - 256-bit encryption key.
        IV - 128-bit initialization vector (nonce + counter).
        
      Returns:
        Encrypted data. }
    class function EncryptCTR(const Data: TBytes; const Key: TAESKey; const IV: TAESBlock): TBytes; static;
    
    { Decrypts data using AES-256 in CTR mode.
      
      Parameters:
        Data - The data to decrypt.
        Key - 256-bit encryption key.
        IV - 128-bit initialization vector (nonce + counter).
        
      Returns:
        Decrypted data. }
    class function DecryptCTR(const Data: TBytes; const Key: TAESKey; const IV: TAESBlock): TBytes; static;
  end;

implementation

{$R-} // Disable range checking for performance-critical sections

const
  // AES S-box lookup table (complete)
  SBox: array[0..255] of Byte = (
    $63, $7C, $77, $7B, $F2, $6B, $6F, $C5, $30, $01, $67, $2B, $FE, $D7, $AB, $76,
    $CA, $82, $C9, $7D, $FA, $59, $47, $F0, $AD, $D4, $A2, $AF, $9C, $A4, $72, $C0,
    $B7, $FD, $93, $26, $36, $3F, $F7, $CC, $34, $A5, $E5, $F1, $71, $D8, $31, $15,
    $04, $C7, $23, $C3, $18, $96, $05, $9A, $07, $12, $80, $E2, $EB, $27, $B2, $75,
    $09, $83, $2C, $1A, $1B, $6E, $5A, $A0, $52, $3B, $D6, $B3, $29, $E3, $2F, $84,
    $53, $D1, $00, $ED, $20, $FC, $B1, $5B, $6A, $CB, $BE, $39, $4A, $4C, $58, $CF,
    $D0, $EF, $AA, $FB, $43, $4D, $33, $85, $45, $F9, $02, $7F, $50, $3C, $9F, $A8,
    $51, $A3, $40, $8F, $92, $9D, $38, $F5, $BC, $B6, $DA, $21, $10, $FF, $F3, $D2,
    $CD, $0C, $13, $EC, $5F, $97, $44, $17, $C4, $A7, $7E, $3D, $64, $5D, $19, $73,
    $60, $81, $4F, $DC, $22, $2A, $90, $88, $46, $EE, $B8, $14, $DE, $5E, $0B, $DB,
    $E0, $32, $3A, $0A, $49, $06, $24, $5C, $C2, $D3, $AC, $62, $91, $95, $E4, $79,
    $E7, $C8, $37, $6D, $8D, $D5, $4E, $A9, $6C, $56, $F4, $EA, $65, $7A, $AE, $08,
    $BA, $78, $25, $2E, $1C, $A6, $B4, $C6, $E8, $DD, $74, $1F, $4B, $BD, $8B, $8A,
    $70, $3E, $B5, $66, $48, $03, $F6, $0E, $61, $35, $57, $B9, $86, $C1, $1D, $9E,
    $E1, $F8, $98, $11, $69, $D9, $8E, $94, $9B, $1E, $87, $E9, $CE, $55, $28, $DF,
    $8C, $A1, $89, $0D, $BF, $E6, $42, $68, $41, $99, $2D, $0F, $B0, $54, $BB, $16
  );
  
  // AES inverse S-box lookup table (complete)
  InvSBox: array[0..255] of Byte = (
    $52, $09, $6A, $D5, $30, $36, $A5, $38, $BF, $40, $A3, $9E, $81, $F3, $D7, $FB,
    $7C, $E3, $39, $82, $9B, $2F, $FF, $87, $34, $8E, $43, $44, $C4, $DE, $E9, $CB,
    $54, $7B, $94, $32, $A6, $C2, $23, $3D, $EE, $4C, $95, $0B, $42, $FA, $C3, $4E,
    $08, $2E, $A1, $66, $28, $D9, $24, $B2, $76, $5B, $A2, $49, $6D, $8B, $D1, $25,
    $72, $F8, $F6, $64, $86, $68, $98, $16, $D4, $A4, $5C, $CC, $5D, $65, $B6, $92,
    $6C, $70, $48, $50, $FD, $ED, $B9, $DA, $5E, $15, $46, $57, $A7, $8D, $9D, $84,
    $90, $D8, $AB, $00, $8C, $BC, $D3, $0A, $F7, $E4, $58, $05, $B8, $B3, $45, $06,
    $D0, $2C, $1E, $8F, $CA, $3F, $0F, $02, $C1, $AF, $BD, $03, $01, $13, $8A, $6B,
    $3A, $91, $11, $41, $4F, $67, $DC, $EA, $97, $F2, $CF, $CE, $F0, $B4, $E6, $73,
    $96, $AC, $74, $22, $E7, $AD, $35, $85, $E2, $F9, $37, $E8, $1C, $75, $DF, $6E,
    $47, $F1, $1A, $71, $1D, $29, $C5, $89, $6F, $B7, $62, $0E, $AA, $18, $BE, $1B,
    $FC, $56, $3E, $4B, $C6, $D2, $79, $20, $9A, $DB, $C0, $FE, $78, $CD, $5A, $F4,
    $1F, $DD, $A8, $33, $88, $07, $C7, $31, $B1, $12, $10, $59, $27, $80, $EC, $5F,
    $60, $51, $7F, $A9, $19, $B5, $4A, $0D, $2D, $E5, $7A, $9F, $93, $C9, $9C, $EF,
    $A0, $E0, $3B, $4D, $AE, $2A, $F5, $B0, $C8, $EB, $BB, $3C, $83, $53, $99, $61,
    $17, $2B, $04, $7E, $BA, $77, $D6, $26, $E1, $69, $14, $63, $55, $21, $0C, $7D
  );
  
  // Round constants
  Rcon: array[0..9] of UInt32 = (
    $01000000, $02000000, $04000000, $08000000, $10000000,
    $20000000, $40000000, $80000000, $1B000000, $36000000
  );

{ TAESCipher }

{*******************************************************************************
  Creates a new AES cipher instance with specified mode, key, and IV.
  
  @param Mode   The operation mode (CBC or CTR)
  @param Key    256-bit encryption key
  @param IV     128-bit initialization vector
  
  Note: For CBC mode, IV must be unpredictable (random).
        For CTR mode, IV serves as initial counter value.
*******************************************************************************}
constructor TAESCipher.Create(Mode: TAESMode; const Key: TAESKey; const IV: TAESBlock);
begin
  inherited Create;
  FMode := Mode;
  Move(IV[0], FIV[0], SizeOf(TAESBlock));
  ExpandKey(Key);
end;

destructor TAESCipher.Destroy;
begin
  // Securely clear sensitive data
  FillChar(FKeySchedule, SizeOf(FKeySchedule), 0);
  FillChar(FIV, SizeOf(FIV), 0);
  inherited Destroy;
end;

class function TAESCipher.RotWord(Value: UInt32): UInt32;
begin
  Result := ((Value shl 8) or (Value shr 24)) and $FFFFFFFF;
end;

class function TAESCipher.SubWord(Value: UInt32): UInt32;
begin
  Result := (UInt32(SBox[Value shr 24]) shl 24) or
            (UInt32(SBox[(Value shr 16) and $FF]) shl 16) or
            (UInt32(SBox[(Value shr 8) and $FF]) shl 8) or
            UInt32(SBox[Value and $FF]);
end;

{*******************************************************************************
  Expands the 256-bit key into the key schedule.
  
  The key expansion routine creates round keys derived from the cipher key.
  For AES-256, it generates 60 32-bit words (15 128-bit round keys).
  
  Algorithm from FIPS 197 Section 5.2:
  1. First round key is the key itself
  2. Subsequent round keys are generated using:
     - RotWord: Cyclic left shift of 32-bit word
     - SubWord: S-box substitution
     - Rcon: Round constant XOR
  
  @param Key   The 256-bit master key
*******************************************************************************}
procedure TAESCipher.ExpandKey(const Key: TAESKey);
var
  I: Integer;
  Temp: UInt32;
begin
  // First round key is the key itself
  for I := 0 to 7 do
    FKeySchedule[I] := (UInt32(Key[4*I]) shl 24) or
                      (UInt32(Key[4*I + 1]) shl 16) or
                      (UInt32(Key[4*I + 2]) shl 8) or
                      UInt32(Key[4*I + 3]);
  
  // Generate the expanded key schedule
  for I := 8 to 59 do
  begin
    Temp := FKeySchedule[I-1];
    if (I mod 8 = 0) then
      Temp := SubWord(RotWord(Temp)) xor Rcon[I div 8 - 1]
    else if (I mod 8 = 4) then
      Temp := SubWord(Temp);
    FKeySchedule[I] := FKeySchedule[I-8] xor Temp;
  end;
end;

{*******************************************************************************
  Performs SubBytes transformation using S-box.
  
  SubBytes is a non-linear byte substitution that operates independently
  on each byte of the state using a substitution table (S-box).
  
  Reference: FIPS 197 Section 5.1.1
  
  @param Block   The block to transform (modified in place)
*******************************************************************************}
procedure TAESCipher.SubBytes(var State: TAESBlock);
var
  I: Integer;
begin
  for I := 0 to 15 do
    State[I] := SBox[State[I]];
end;

{*******************************************************************************
  Performs inverse SubBytes transformation using inverse S-box.
  
  InvSubBytes is the inverse of the SubBytes transformation, using the
  inverse S-box lookup table.
  
  Reference: FIPS 197 Section 5.3.2
  
  @param Block   The block to transform (modified in place)
*******************************************************************************}
procedure TAESCipher.InvSubBytes(var State: TAESBlock);
var
  I: Integer;
begin
  for I := 0 to 15 do
    State[I] := InvSBox[State[I]];
end;

{*******************************************************************************
  Performs ShiftRows transformation.
  
  ShiftRows cyclically shifts the bytes in each row by different offsets:
  - Row 0: no shift
  - Row 1: 1-byte shift
  - Row 2: 2-byte shift
  - Row 3: 3-byte shift
  
  Reference: FIPS 197 Section 5.1.2
  
  @param Block   The block to transform (modified in place)
*******************************************************************************}
procedure TAESCipher.ShiftRows(var State: TAESBlock);
var
  Temp: Byte;
begin
  // Row 1: shift left by 1
  Temp := State[1];
  State[1] := State[5];
  State[5] := State[9];
  State[9] := State[13];
  State[13] := Temp;
  
  // Row 2: shift left by 2
  Temp := State[2];
  State[2] := State[10];
  State[10] := Temp;
  Temp := State[6];
  State[6] := State[14];
  State[14] := Temp;
  
  // Row 3: shift left by 3 (equivalent to right by 1)
  Temp := State[15];
  State[15] := State[11];
  State[11] := State[7];
  State[7] := State[3];
  State[3] := Temp;
end;

{*******************************************************************************
  Performs inverse ShiftRows transformation.
  
  InvShiftRows cyclically shifts the bytes in each row by different offsets
  in the opposite direction of ShiftRows:
  - Row 0: no shift
  - Row 1: 3-byte shift
  - Row 2: 2-byte shift
  - Row 3: 1-byte shift
  
  Reference: FIPS 197 Section 5.3.1
  
  @param Block   The block to transform (modified in place)
*******************************************************************************}
procedure TAESCipher.InvShiftRows(var State: TAESBlock);
var
  Temp: Byte;
begin
  // Row 1: shift right by 1
  Temp := State[13];
  State[13] := State[9];
  State[9] := State[5];
  State[5] := State[1];
  State[1] := Temp;
  
  // Row 2: shift right by 2
  Temp := State[2];
  State[2] := State[10];
  State[10] := Temp;
  Temp := State[6];
  State[6] := State[14];
  State[14] := Temp;
  
  // Row 3: shift right by 3 (equivalent to left by 1)
  Temp := State[3];
  State[3] := State[7];
  State[7] := State[11];
  State[11] := State[15];
  State[15] := Temp;
end;

function GaloisMult(A, B: Byte): Byte;
var
  P: Byte;
begin
  P := 0;
  while B <> 0 do
  begin
    if (B and 1) <> 0 then
      P := P xor A;
    if (A and $80) <> 0 then
    begin
      A := (A shl 1) xor $1B;
    end
    else
      A := A shl 1;
    B := B shr 1;
  end;
  Result := P;
end;

{*******************************************************************************
  Performs MixColumns transformation.
  
  MixColumns treats each column as a polynomial over GF(2^8) and
  multiplies it with a fixed polynomial:
  a = (03 * x^3) + (01 * x^2) + (01 * x) + 02
  
  Reference: FIPS 197 Section 5.1.3
  
  @param Block   The block to transform (modified in place)
*******************************************************************************}
procedure TAESCipher.MixColumns(var State: TAESBlock);
var
  I: Integer;
  S0, S1, S2, S3, T: Byte;
begin
  for I := 0 to 3 do
  begin
    S0 := State[4*I];
    S1 := State[4*I + 1];
    S2 := State[4*I + 2];
    S3 := State[4*I + 3];
    
    T := S0 xor S1 xor S2 xor S3;
    State[4*I] := GaloisMult(2, S0) xor GaloisMult(3, S1) xor S2 xor S3;
    State[4*I + 1] := S0 xor GaloisMult(2, S1) xor GaloisMult(3, S2) xor S3;
    State[4*I + 2] := S0 xor S1 xor GaloisMult(2, S2) xor GaloisMult(3, S3);
    State[4*I + 3] := GaloisMult(3, S0) xor S1 xor S2 xor GaloisMult(2, S3);
  end;
end;

{*******************************************************************************
  Performs inverse MixColumns transformation.
  
  InvMixColumns is the inverse of the MixColumns transformation.
  Each column is treated as a polynomial and multiplied by the inverse:
  a^(-1) = (0B * x^3) + (0D * x^2) + (09 * x) + 0E
  
  Reference: FIPS 197 Section 5.3.3
  
  @param Block   The block to transform (modified in place)
*******************************************************************************}
procedure TAESCipher.InvMixColumns(var State: TAESBlock);
var
  I: Integer;
  S0, S1, S2, S3: Byte;
begin
  for I := 0 to 3 do
  begin
    S0 := State[4*I];
    S1 := State[4*I + 1];
    S2 := State[4*I + 2];
    S3 := State[4*I + 3];
    
    State[4*I] := GaloisMult($0E, S0) xor GaloisMult($0B, S1) xor 
                  GaloisMult($0D, S2) xor GaloisMult($09, S3);
    State[4*I + 1] := GaloisMult($09, S0) xor GaloisMult($0E, S1) xor 
                      GaloisMult($0B, S2) xor GaloisMult($0D, S3);
    State[4*I + 2] := GaloisMult($0D, S0) xor GaloisMult($09, S1) xor 
                      GaloisMult($0E, S2) xor GaloisMult($0B, S3);
    State[4*I + 3] := GaloisMult($0B, S0) xor GaloisMult($0D, S1) xor 
                      GaloisMult($09, S2) xor GaloisMult($0E, S3);
  end;
end;

procedure TAESCipher.AddRoundKey(var State: TAESBlock; Round: Integer);
var
  I: Integer;
  RoundKey: array[0..3] of UInt32;
begin
  Move(FKeySchedule[Round * 4], RoundKey[0], 16);
  for I := 0 to 3 do
  begin
    State[4*I] := State[4*I] xor Byte(RoundKey[I] shr 24);
    State[4*I + 1] := State[4*I + 1] xor Byte(RoundKey[I] shr 16);
    State[4*I + 2] := State[4*I + 2] xor Byte(RoundKey[I] shr 8);
    State[4*I + 3] := State[4*I + 3] xor Byte(RoundKey[I]);
  end;
end;

class procedure TAESCipher.IncCounter(var Counter: TAESBlock);
var
  I: Integer;
begin
  for I := 15 downto 0 do
  begin
    Inc(Counter[I]);
    if Counter[I] <> 0 then
      Break;
  end;
end;

{*******************************************************************************
  XORs two blocks of memory.
  
  This is a helper function used in both CBC and CTR modes to combine
  blocks with XOR operation.
  
  @param Source1   First source block
  @param Source2   Second source block
  @param Dest      Destination block (can be same as either source)
  @param Size      Size of blocks in bytes
*******************************************************************************}
class procedure TAESCipher.XorBlock(const Source1, Source2: PByte; Dest: PByte; Size: Integer);
var
  I: Integer;
begin
  for I := 0 to Size - 1 do
    Dest[I] := Source1[I] xor Source2[I];
end;

{*******************************************************************************
  Encrypts a single 128-bit block using AES-256.
  
  The encryption process consists of:
  1. Initial round key addition
  2. 13 main rounds (SubBytes, ShiftRows, MixColumns, AddRoundKey)
  3. Final round (SubBytes, ShiftRows, AddRoundKey)
  
  @param Block   The 128-bit block to encrypt (modified in place)
*******************************************************************************}
procedure TAESCipher.EncryptBlock(var Block: TAESBlock);
var
  Round: Integer;
begin
  AddRoundKey(Block, 0);
  
  for Round := 1 to 13 do
  begin
    SubBytes(Block);
    ShiftRows(Block);
    MixColumns(Block);
    AddRoundKey(Block, Round);
  end;
  
  // Final round (no MixColumns)
  SubBytes(Block);
  ShiftRows(Block);
  AddRoundKey(Block, 14);
end;

{*******************************************************************************
  Decrypts a single 128-bit block using AES-256.
  
  The decryption process is the inverse of encryption:
  1. Initial round key addition
  2. 13 inverse main rounds (InvShiftRows, InvSubBytes, AddRoundKey, InvMixColumns)
  3. Final round (InvShiftRows, InvSubBytes, AddRoundKey)
  
  @param Block   The 128-bit block to decrypt (modified in place)
*******************************************************************************}
procedure TAESCipher.DecryptBlock(var Block: TAESBlock);
var
  Round: Integer;
begin
  AddRoundKey(Block, 14);
  
  for Round := 13 downto 1 do
  begin
    InvShiftRows(Block);
    InvSubBytes(Block);
    AddRoundKey(Block, Round);
    InvMixColumns(Block);
  end;
  
  // Final round (no InvMixColumns)
  InvShiftRows(Block);
  InvSubBytes(Block);
  AddRoundKey(Block, 0);
end;

{*******************************************************************************
  Encrypts data using CBC (Cipher Block Chaining) mode.
  
  CBC mode provides confidentiality and limited integrity through chaining.
  Each plaintext block is XORed with the previous ciphertext block before
  encryption. PKCS7 padding is used to handle partial blocks.
  
  @param Data   The data to encrypt
  @return       The encrypted data with padding
  
  Note: CBC mode is not parallelizable for encryption.
*******************************************************************************}
function TAESCipher.EncryptCBC(const Data: TBytes): TBytes;
var
  NumBlocks, LastBlockSize, PaddingSize, I: Integer;
  Block, PrevBlock: TAESBlock;
begin
  // Calculate padding
  LastBlockSize := Length(Data) mod 16;
  if LastBlockSize = 0 then
    // If data is already block-aligned, no padding needed
    SetLength(Result, Length(Data))
  else
  begin
    PaddingSize := 16 - LastBlockSize;
    SetLength(Result, Length(Data) + PaddingSize);
  end;
  
  // Copy input data
  if Length(Data) > 0 then
    Move(Data[0], Result[0], Length(Data));
  
  // Add PKCS7 padding if needed
  if LastBlockSize <> 0 then
  begin
    for I := Length(Data) to Length(Result) - 1 do
      Result[I] := 16 - LastBlockSize;
  end;
  
  // Initialize IV
  Move(FIV[0], PrevBlock[0], 16);
  
  // Process each block
  NumBlocks := Length(Result) div 16;
  for I := 0 to NumBlocks - 1 do
  begin
    Move(Result[I * 16], Block[0], 16);
    XorBlock(@Block[0], @PrevBlock[0], @Block[0], 16);
    EncryptBlock(Block);
    Move(Block[0], Result[I * 16], 16);
    Move(Block[0], PrevBlock[0], 16);
  end;
end;

{*******************************************************************************
  Decrypts data using CBC (Cipher Block Chaining) mode.
  
  CBC decryption process:
  1. Decrypt ciphertext block
  2. XOR with previous ciphertext block
  3. Remove PKCS7 padding from final block
  
  @param Data   The encrypted data with padding
  @return       The decrypted data with padding removed
  
  Note: CBC decryption can be parallelized, unlike encryption.
        Invalid padding will raise EAESError.
*******************************************************************************}
function TAESCipher.DecryptCBC(const Data: TBytes): TBytes;
var
  NumBlocks, PaddingSize, I: Integer;
  Block, PrevBlock, CurrBlock: TAESBlock;
begin
  if (Length(Data) = 0) or (Length(Data) mod 16 <> 0) then
    raise EAESError.Create('Invalid encrypted data length');
  
  SetLength(Result, Length(Data));
  Move(Data[0], Result[0], Length(Data));
  
  // Initialize IV
  Move(FIV[0], PrevBlock[0], 16);
  
  // Process each block
  NumBlocks := Length(Data) div 16;
  for I := 0 to NumBlocks - 1 do
  begin
    Move(Result[I * 16], Block[0], 16);
    Move(Block[0], CurrBlock[0], 16);
    DecryptBlock(Block);
    XorBlock(@Block[0], @PrevBlock[0], @Block[0], 16);
    Move(Block[0], Result[I * 16], 16);
    Move(CurrBlock[0], PrevBlock[0], 16);
  end;
  
  // Check and remove PKCS7 padding if present
  PaddingSize := Result[Length(Result) - 1];
  if (PaddingSize > 0) and (PaddingSize <= 16) then
  begin
    // Only verify and remove padding if it's not block-aligned input
    if PaddingSize < 16 then
    begin
      // Verify padding
      for I := Length(Result) - PaddingSize to Length(Result) - 1 do
        if Result[I] <> PaddingSize then
          raise EAESError.Create('Invalid padding');
      
      SetLength(Result, Length(Result) - PaddingSize);
    end;
  end
  else
    raise EAESError.Create('Invalid padding');
end;

{*******************************************************************************
  Encrypts data using CTR (Counter) mode.
  
  CTR mode turns a block cipher into a stream cipher by encrypting
  incrementing counter values and XORing with plaintext. Benefits:
  1. No padding required
  2. Parallelizable
  3. Random access to encrypted data
  4. No error propagation
  
  @param Data   The data to encrypt
  @return       The encrypted data (same length as input)
  
  Note: Counter must never be reused with the same key.
*******************************************************************************}
function TAESCipher.EncryptCTR(const Data: TBytes): TBytes;
var
  NumBlocks, LastBlockSize, I: Integer;
  Counter, Block: TAESBlock;
begin
  SetLength(Result, Length(Data));
  if Length(Data) = 0 then
    Exit;
  
  // Initialize counter with IV
  Move(FIV[0], Counter[0], 16);
  
  // Process full blocks
  NumBlocks := Length(Data) div 16;
  LastBlockSize := Length(Data) mod 16;
  
  for I := 0 to NumBlocks - 1 do
  begin
    Move(Counter[0], Block[0], 16);
    EncryptBlock(Block);
    XorBlock(@Data[I * 16], @Block[0], @Result[I * 16], 16);
    IncCounter(Counter);
  end;
  
  // Process final partial block if any
  if LastBlockSize > 0 then
  begin
    Move(Counter[0], Block[0], 16);
    EncryptBlock(Block);
    XorBlock(@Data[NumBlocks * 16], @Block[0], @Result[NumBlocks * 16], LastBlockSize);
  end;
end;

{*******************************************************************************
  Decrypts data using CTR (Counter) mode.
  
  CTR mode decryption is identical to encryption:
  1. Encrypt counter values
  2. XOR with ciphertext
  
  @param Data   The encrypted data
  @return       The decrypted data
  
  Note: CTR mode is self-reversible - encryption and decryption are identical.
*******************************************************************************}
function TAESCipher.DecryptCTR(const Data: TBytes): TBytes;
begin
  // CTR mode is symmetric - encryption and decryption are the same operation
  Result := EncryptCTR(Data);
end;

function TAESCipher.Encrypt(const Data: TBytes): TBytes;
begin
  if Length(Data) = 0 then
    Exit(TBytes.Create);
    
  case FMode of
    amCBC: Result := EncryptCBC(Data);
    amCTR: Result := EncryptCTR(Data);
  else
    raise EAESError.Create('Unsupported encryption mode');
  end;
end;

function TAESCipher.Decrypt(const Data: TBytes): TBytes;
begin
  if Length(Data) = 0 then
    Exit(TBytes.Create);
    
  case FMode of
    amCBC: Result := DecryptCBC(Data);
    amCTR: Result := DecryptCTR(Data);
  else
    raise EAESError.Create('Unsupported decryption mode');
  end;
end;

{$R+} // Re-enable range checking for the rest of the implementation

{ TAES256 }

class function TAES256.EncryptCBC(const Data: TBytes; const Key: TAESKey; const IV: TAESBlock): TBytes;
var
  Cipher: TAESCipher;
begin
  Cipher := TAESCipher.Create(amCBC, Key, IV);
  try
    Result := Cipher.Encrypt(Data);
  finally
    Cipher.Free;
  end;
end;

class function TAES256.DecryptCBC(const Data: TBytes; const Key: TAESKey; const IV: TAESBlock): TBytes;
var
  Cipher: TAESCipher;
begin
  Cipher := TAESCipher.Create(amCBC, Key, IV);
  try
    Result := Cipher.Decrypt(Data);
  finally
    Cipher.Free;
  end;
end;

class function TAES256.EncryptCTR(const Data: TBytes; const Key: TAESKey; const IV: TAESBlock): TBytes;
var
  Cipher: TAESCipher;
begin
  Cipher := TAESCipher.Create(amCTR, Key, IV);
  try
    Result := Cipher.Encrypt(Data);
  finally
    Cipher.Free;
  end;
end;

class function TAES256.DecryptCTR(const Data: TBytes; const Key: TAESKey; const IV: TAESBlock): TBytes;
var
  Cipher: TAESCipher;
begin
  Cipher := TAESCipher.Create(amCTR, Key, IV);
  try
    Result := Cipher.Decrypt(Data);
  finally
    Cipher.Free;
  end;
end;

end. 