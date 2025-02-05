unit TidyKit.Crypto.AES;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils;

type
  { AES-specific exception type }
  ETidyKitAESException = class(Exception);

  { Array types }
  TByteShortArray = array[0..3] of Byte;
  TAESBlock = array[0..15] of Byte;
  
  { Key sizes }
  TAESKeySize = (ks256);  // We only implement AES-256 as requested
  
  { Operation modes }
  TAESMode = (mGCM, mCTR);
  
  { Internal types }
  TAESState = array[0..3, 0..3] of Byte;
  TAESExpandedKey = array[0..14, 0..3, 0..3] of Byte;
  
  { GCM types }
  TGCMHashKey = array[0..15] of Byte;
  TGCMHashTable = array[0..15, 0..255] of QWord;

  { Result record for authenticated encryption }
  TAESGCMResult = record
    CipherText: TBytes;
    Tag: TBytes;
  end;

  { TAES256 }
  TAES256 = class
  private
    class var FSBox: array[0..255] of Byte;
    class var FInvSBox: array[0..255] of Byte;
    class var FRoundConst: array[0..9] of Byte;
    class var FInitialized: Boolean;
    
    class procedure InitializeTables;
    class procedure InitSBox;
    class procedure InitInvSBox;
    class procedure InitRoundConst;
    
    { Core AES operations }
    class procedure AddRoundKey(var State: TAESState; const RoundKey: TAESExpandedKey; Round: Integer);
    class procedure SubBytes(var State: TAESState);
    class procedure InvSubBytes(var State: TAESState);
    class procedure ShiftRows(var State: TAESState);
    class procedure InvShiftRows(var State: TAESState);
    class procedure MixColumns(var State: TAESState);
    class procedure InvMixColumns(var State: TAESState);
    
    { Key schedule }
    class procedure ExpandKey(const Key: TBytes; var ExpandedKey: TAESExpandedKey);
    class procedure SubWord(var Word: TByteShortArray);
    class procedure RotWord(var Word: TByteShortArray);
    
    { Block operations }
    class procedure EncryptBlock(const InBlock: TAESBlock; const ExpandedKey: TAESExpandedKey; out OutBlock: TAESBlock);
    class procedure DecryptBlock(const InBlock: TAESBlock; const ExpandedKey: TAESExpandedKey; out OutBlock: TAESBlock);
    
    { GCM operations }
    {$R-} // Disable range checking for GCM operations
    class procedure GCMInit(const H: TGCMHashKey; var Table: TGCMHashTable);
    class procedure GCMMultiply(var X: TBytes; const Table: TGCMHashTable);
    class procedure GCMGHash(const Data: TBytes; var X: TBytes; const Table: TGCMHashTable);
    class function GCMGenerateTag(const AAD, CipherText: TBytes; const Table: TGCMHashTable; 
      const J0: TBytes; AADLen, CipherLen: QWord): TBytes;
    {$R+} // Re-enable range checking
    
    { CTR operations }
    {$R-} // Disable range checking for CTR operations
    class procedure CTRIncrement(var Counter: TBytes);
    {$R+} // Re-enable range checking
    
    { Utility functions }
    {$R-} // Disable range checking
    class function GaloisMultiply(X, Y: Byte): Byte;
    {$R+} // Re-enable range checking
    class procedure XORBlock(var A: TAESBlock; const B: TAESBlock);
    class function ValidateKey(const Key: TBytes): Boolean;
    class function ValidateIV(const IV: TBytes; Mode: TAESMode): Boolean;
  public
    { Initialize the class }
    class constructor Create;
    
    { GCM mode encryption }
    class function EncryptGCM(
      const PlainText: TBytes;     // Data to encrypt
      const Key: TBytes;           // 32 bytes (256-bit) key
      const IV: TBytes;            // 12 bytes (96-bit) IV
      const AAD: TBytes = nil      // Optional additional authenticated data
    ): TAESGCMResult;
    
    { GCM mode decryption }
    class function DecryptGCM(
      const CipherText: TBytes;    // Data to decrypt
      const Key: TBytes;           // 32 bytes (256-bit) key
      const IV: TBytes;            // 12 bytes (96-bit) IV
      const Tag: TBytes;           // Authentication tag
      const AAD: TBytes = nil      // Optional additional authenticated data
    ): TBytes;
    
    { CTR mode encryption }
    class function EncryptCTR(
      const PlainText: TBytes;     // Data to encrypt
      const Key: TBytes;           // 32 bytes (256-bit) key
      const IV: TBytes             // 16 bytes (128-bit) IV/nonce
    ): TBytes;
    
    { CTR mode decryption (same as encryption) }
    class function DecryptCTR(
      const CipherText: TBytes;    // Data to decrypt
      const Key: TBytes;           // 32 bytes (256-bit) key
      const IV: TBytes             // 16 bytes (128-bit) IV/nonce
    ): TBytes;
  end;

implementation

{ Class constructor to initialize lookup tables }
class constructor TAES256.Create;
begin
  if not FInitialized then
  begin
    InitializeTables;
    FInitialized := True;
  end;
end;

{ Initialize all lookup tables }
class procedure TAES256.InitializeTables;
begin
  InitSBox;
  InitInvSBox;
  InitRoundConst;
end;

{ Initialize the S-box }
class procedure TAES256.InitSBox;
const
  // AES S-box values
  SBoxValues: array[0..255] of Byte = (
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
begin
  Move(SBoxValues, FSBox, SizeOf(FSBox));
end;

{ Initialize the inverse S-box }
class procedure TAES256.InitInvSBox;
var
  I: Integer;
begin
  for I := 0 to 255 do
    FInvSBox[FSBox[I]] := I;
end;

{ Initialize round constants }
class procedure TAES256.InitRoundConst;
var
  I: Integer;
  X: Byte;
begin
  X := 1;
  for I := 0 to 9 do
  begin
    FRoundConst[I] := X;
    X := GaloisMultiply(X, 2);
  end;
end;

{ Galois field multiplication }
{$R-} // Disable range checking
class function TAES256.GaloisMultiply(X, Y: Byte): Byte;
var
  P: Byte;
  I: Integer;
  HiBitSet: Boolean;
begin
  P := 0;
  for I := 0 to 7 do
  begin
    if (Y and 1) <> 0 then
      P := P xor X;
    HiBitSet := (X and $80) <> 0;
    X := X shl 1;
    if HiBitSet then
      X := X xor $1B;  // AES irreducible polynomial
    Y := Y shr 1;
  end;
  Result := P;
end;
{$R+} // Re-enable range checking

{ Validate key size }
class function TAES256.ValidateKey(const Key: TBytes): Boolean;
begin
  Result := (Length(Key) = 32);  // 256 bits
end;

{ Validate IV size based on mode }
class function TAES256.ValidateIV(const IV: TBytes; Mode: TAESMode): Boolean;
begin
  case Mode of
    mGCM: Result := (Length(IV) = 12);  // 96 bits
    mCTR: Result := (Length(IV) = 16);  // 128 bits
  end;
end;

{ Core AES transformations }

class procedure TAES256.AddRoundKey(var State: TAESState; const RoundKey: TAESExpandedKey; Round: Integer);
var
  I, J: Integer;
begin
  for I := 0 to 3 do
    for J := 0 to 3 do
      State[I, J] := State[I, J] xor RoundKey[Round, I, J];
end;

class procedure TAES256.SubBytes(var State: TAESState);
var
  I, J: Integer;
begin
  for I := 0 to 3 do
    for J := 0 to 3 do
      State[I, J] := FSBox[State[I, J]];
end;

class procedure TAES256.InvSubBytes(var State: TAESState);
var
  I, J: Integer;
begin
  for I := 0 to 3 do
    for J := 0 to 3 do
      State[I, J] := FInvSBox[State[I, J]];
end;

class procedure TAES256.ShiftRows(var State: TAESState);
var
  Temp: Byte;
begin
  // Row 1: shift left by 1
  Temp := State[1, 0];
  State[1, 0] := State[1, 1];
  State[1, 1] := State[1, 2];
  State[1, 2] := State[1, 3];
  State[1, 3] := Temp;
  
  // Row 2: shift left by 2
  Temp := State[2, 0];
  State[2, 0] := State[2, 2];
  State[2, 2] := Temp;
  Temp := State[2, 1];
  State[2, 1] := State[2, 3];
  State[2, 3] := Temp;
  
  // Row 3: shift left by 3 (equivalent to right by 1)
  Temp := State[3, 3];
  State[3, 3] := State[3, 2];
  State[3, 2] := State[3, 1];
  State[3, 1] := State[3, 0];
  State[3, 0] := Temp;
end;

class procedure TAES256.InvShiftRows(var State: TAESState);
var
  Temp: Byte;
begin
  // Row 1: shift right by 1
  Temp := State[1, 3];
  State[1, 3] := State[1, 2];
  State[1, 2] := State[1, 1];
  State[1, 1] := State[1, 0];
  State[1, 0] := Temp;
  
  // Row 2: shift right by 2
  Temp := State[2, 0];
  State[2, 0] := State[2, 2];
  State[2, 2] := Temp;
  Temp := State[2, 1];
  State[2, 1] := State[2, 3];
  State[2, 3] := Temp;
  
  // Row 3: shift right by 3 (equivalent to left by 1)
  Temp := State[3, 0];
  State[3, 0] := State[3, 1];
  State[3, 1] := State[3, 2];
  State[3, 2] := State[3, 3];
  State[3, 3] := Temp;
end;

class procedure TAES256.MixColumns(var State: TAESState);
var
  I: Integer;
  S0, S1, S2, S3: Byte;
begin
  for I := 0 to 3 do
  begin
    S0 := State[0, I];
    S1 := State[1, I];
    S2 := State[2, I];
    S3 := State[3, I];
    
    State[0, I] := GaloisMultiply(2, S0) xor GaloisMultiply(3, S1) xor S2 xor S3;
    State[1, I] := S0 xor GaloisMultiply(2, S1) xor GaloisMultiply(3, S2) xor S3;
    State[2, I] := S0 xor S1 xor GaloisMultiply(2, S2) xor GaloisMultiply(3, S3);
    State[3, I] := GaloisMultiply(3, S0) xor S1 xor S2 xor GaloisMultiply(2, S3);
  end;
end;

class procedure TAES256.InvMixColumns(var State: TAESState);
var
  I: Integer;
  S0, S1, S2, S3: Byte;
begin
  for I := 0 to 3 do
  begin
    S0 := State[0, I];
    S1 := State[1, I];
    S2 := State[2, I];
    S3 := State[3, I];
    
    State[0, I] := GaloisMultiply($0E, S0) xor GaloisMultiply($0B, S1) xor 
                   GaloisMultiply($0D, S2) xor GaloisMultiply($09, S3);
    State[1, I] := GaloisMultiply($09, S0) xor GaloisMultiply($0E, S1) xor 
                   GaloisMultiply($0B, S2) xor GaloisMultiply($0D, S3);
    State[2, I] := GaloisMultiply($0D, S0) xor GaloisMultiply($09, S1) xor 
                   GaloisMultiply($0E, S2) xor GaloisMultiply($0B, S3);
    State[3, I] := GaloisMultiply($0B, S0) xor GaloisMultiply($0D, S1) xor 
                   GaloisMultiply($09, S2) xor GaloisMultiply($0E, S3);
  end;
end;

class procedure TAES256.XORBlock(var A: TAESBlock; const B: TAESBlock);
var
  I: Integer;
begin
  for I := 0 to 15 do
    A[I] := A[I] xor B[I];
end;

{ Key schedule operations }

class procedure TAES256.SubWord(var Word: TByteShortArray);
var
  I: Integer;
begin
  for I := 0 to 3 do
    Word[I] := FSBox[Word[I]];
end;

class procedure TAES256.RotWord(var Word: TByteShortArray);
var
  Temp: Byte;
begin
  Temp := Word[0];
  Word[0] := Word[1];
  Word[1] := Word[2];
  Word[2] := Word[3];
  Word[3] := Temp;
end;

class procedure TAES256.ExpandKey(const Key: TBytes; var ExpandedKey: TAESExpandedKey);
var
  I, J: Integer;
  Temp: TByteShortArray;
  TempWord: TByteShortArray;
begin
  // First round key is the key itself
  for I := 0 to 7 do
    for J := 0 to 3 do
      ExpandedKey[I div 4, I mod 4, J] := Key[4*I + J];

  // Generate the remaining round keys
  for I := 8 to 59 do
  begin
    // Save the previous word
    for J := 0 to 3 do
      Temp[J] := ExpandedKey[(I-1) div 4, (I-1) mod 4, J];

    if (I mod 8 = 0) then
    begin
      // RotWord, SubWord and XOR with round constant
      Move(Temp, TempWord, 4);
      RotWord(TempWord);
      SubWord(TempWord);
      TempWord[0] := TempWord[0] xor FRoundConst[I div 8 - 1];
      Move(TempWord, Temp, 4);
    end
    else if (I mod 8 = 4) then
    begin
      // SubWord only
      SubWord(Temp);
    end;

    // XOR with word 8 positions earlier
    for J := 0 to 3 do
      ExpandedKey[I div 4, I mod 4, J] := 
        ExpandedKey[(I-8) div 4, (I-8) mod 4, J] xor Temp[J];
  end;
end;

{ Block encryption/decryption }

class procedure TAES256.EncryptBlock(const InBlock: TAESBlock; const ExpandedKey: TAESExpandedKey; 
  out OutBlock: TAESBlock);
var
  State: TAESState;
  Round: Integer;
  I, J: Integer;
begin
  // Convert input block to state array
  for I := 0 to 3 do
    for J := 0 to 3 do
      State[J, I] := InBlock[4*I + J];

  // Initial round
  AddRoundKey(State, ExpandedKey, 0);

  // Main rounds
  for Round := 1 to 13 do
  begin
    SubBytes(State);
    ShiftRows(State);
    MixColumns(State);
    AddRoundKey(State, ExpandedKey, Round);
  end;

  // Final round (no MixColumns)
  SubBytes(State);
  ShiftRows(State);
  AddRoundKey(State, ExpandedKey, 14);

  // Convert state array to output block
  for I := 0 to 3 do
    for J := 0 to 3 do
      OutBlock[4*I + J] := State[J, I];
end;

class procedure TAES256.DecryptBlock(const InBlock: TAESBlock; const ExpandedKey: TAESExpandedKey; 
  out OutBlock: TAESBlock);
var
  State: TAESState;
  Round: Integer;
  I, J: Integer;
begin
  // Convert input block to state array
  for I := 0 to 3 do
    for J := 0 to 3 do
      State[J, I] := InBlock[4*I + J];

  // Initial round
  AddRoundKey(State, ExpandedKey, 14);

  // Main rounds
  for Round := 13 downto 1 do
  begin
    InvShiftRows(State);
    InvSubBytes(State);
    AddRoundKey(State, ExpandedKey, Round);
    InvMixColumns(State);
  end;

  // Final round (no InvMixColumns)
  InvShiftRows(State);
  InvSubBytes(State);
  AddRoundKey(State, ExpandedKey, 0);

  // Convert state array to output block
  for I := 0 to 3 do
    for J := 0 to 3 do
      OutBlock[4*I + J] := State[J, I];
end;

{ GCM mode operations }

{$R-} // Disable range checking for GCM operations
class procedure TAES256.GCMInit(const H: TGCMHashKey; var Table: TGCMHashTable);
var
  I, J, K: Integer;
  V: QWord;
  HRev: array[0..15] of Byte;
  Z: array[0..15] of Byte;
  X: Byte;
begin
  // Reverse H for bit operations
  for I := 0 to 15 do
    HRev[I] := H[15 - I];

  // Initialize lookup table
  for I := 0 to 15 do
  begin
    for J := 0 to 255 do
    begin
      FillChar(Z, 16, 0);
      X := J;
      for K := 0 to 7 do
      begin
        if (X and $80) <> 0 then
        begin
          Z[0] := Z[0] xor HRev[(I * 8 + K) mod 16];
          Z[1] := Z[1] xor HRev[(I * 8 + K + 1) mod 16];
          Z[2] := Z[2] xor HRev[(I * 8 + K + 2) mod 16];
          Z[3] := Z[3] xor HRev[(I * 8 + K + 3) mod 16];
          Z[4] := Z[4] xor HRev[(I * 8 + K + 4) mod 16];
          Z[5] := Z[5] xor HRev[(I * 8 + K + 5) mod 16];
          Z[6] := Z[6] xor HRev[(I * 8 + K + 6) mod 16];
          Z[7] := Z[7] xor HRev[(I * 8 + K + 7) mod 16];
        end;
        X := X shl 1;
      end;
      V := (QWord(Z[0]) shl 56) or (QWord(Z[1]) shl 48) or
           (QWord(Z[2]) shl 40) or (QWord(Z[3]) shl 32) or
           (QWord(Z[4]) shl 24) or (QWord(Z[5]) shl 16) or
           (QWord(Z[6]) shl 8) or QWord(Z[7]);
      Table[I, J] := V;
    end;
  end;
end;

class procedure TAES256.GCMMultiply(var X: TBytes; const Table: TGCMHashTable);
var
  I: Integer;
  Z: array[0..15] of Byte;
  V: QWord;
begin
  FillChar(Z, SizeOf(Z), 0);
  
  for I := 0 to 15 do
  begin
    V := Table[I, X[15-I]];
    Z[0] := Z[0] xor Byte(V shr 56);
    Z[1] := Z[1] xor Byte(V shr 48);
    Z[2] := Z[2] xor Byte(V shr 40);
    Z[3] := Z[3] xor Byte(V shr 32);
    Z[4] := Z[4] xor Byte(V shr 24);
    Z[5] := Z[5] xor Byte(V shr 16);
    Z[6] := Z[6] xor Byte(V shr 8);
    Z[7] := Z[7] xor Byte(V);
  end;
  
  Move(Z, X[0], 16);
end;

class procedure TAES256.GCMGHash(const Data: TBytes; var X: TBytes; const Table: TGCMHashTable);
var
  I, NumBlocks, RemBytes: Integer;
  Block: array[0..15] of Byte;
begin
  NumBlocks := Length(Data) div 16;
  RemBytes := Length(Data) mod 16;
  
  for I := 0 to NumBlocks - 1 do
  begin
    Move(Data[I * 16], Block, 16);
    X[0] := X[0] xor Block[0];
    X[1] := X[1] xor Block[1];
    X[2] := X[2] xor Block[2];
    X[3] := X[3] xor Block[3];
    X[4] := X[4] xor Block[4];
    X[5] := X[5] xor Block[5];
    X[6] := X[6] xor Block[6];
    X[7] := X[7] xor Block[7];
    X[8] := X[8] xor Block[8];
    X[9] := X[9] xor Block[9];
    X[10] := X[10] xor Block[10];
    X[11] := X[11] xor Block[11];
    X[12] := X[12] xor Block[12];
    X[13] := X[13] xor Block[13];
    X[14] := X[14] xor Block[14];
    X[15] := X[15] xor Block[15];
    GCMMultiply(X, Table);
  end;
  
  if RemBytes > 0 then
  begin
    FillChar(Block, 16, 0);
    Move(Data[NumBlocks * 16], Block, RemBytes);
    X[0] := X[0] xor Block[0];
    X[1] := X[1] xor Block[1];
    X[2] := X[2] xor Block[2];
    X[3] := X[3] xor Block[3];
    X[4] := X[4] xor Block[4];
    X[5] := X[5] xor Block[5];
    X[6] := X[6] xor Block[6];
    X[7] := X[7] xor Block[7];
    X[8] := X[8] xor Block[8];
    X[9] := X[9] xor Block[9];
    X[10] := X[10] xor Block[10];
    X[11] := X[11] xor Block[11];
    X[12] := X[12] xor Block[12];
    X[13] := X[13] xor Block[13];
    X[14] := X[14] xor Block[14];
    X[15] := X[15] xor Block[15];
    GCMMultiply(X, Table);
  end;
end;

class function TAES256.GCMGenerateTag(const AAD, CipherText: TBytes; const Table: TGCMHashTable;
  const J0: TBytes; AADLen, CipherLen: QWord): TBytes;
var
  X: TBytes;
  LenBlock: array[0..15] of Byte;
  ExpandedKey: TAESExpandedKey;
  InBlock, OutBlock: TAESBlock;
begin
  SetLength(X, 16);
  FillChar(X[0], 16, 0);
  
  if AADLen > 0 then
    GCMGHash(AAD, X, Table);
  
  if CipherLen > 0 then
    GCMGHash(CipherText, X, Table);
  
  // Length block
  FillChar(LenBlock, 16, 0);
  PQWord(@LenBlock[8])^ := AADLen * 8;
  PQWord(@LenBlock[0])^ := CipherLen * 8;
  
  X[0] := X[0] xor LenBlock[0];
  X[1] := X[1] xor LenBlock[1];
  X[2] := X[2] xor LenBlock[2];
  X[3] := X[3] xor LenBlock[3];
  X[4] := X[4] xor LenBlock[4];
  X[5] := X[5] xor LenBlock[5];
  X[6] := X[6] xor LenBlock[6];
  X[7] := X[7] xor LenBlock[7];
  X[8] := X[8] xor LenBlock[8];
  X[9] := X[9] xor LenBlock[9];
  X[10] := X[10] xor LenBlock[10];
  X[11] := X[11] xor LenBlock[11];
  X[12] := X[12] xor LenBlock[12];
  X[13] := X[13] xor LenBlock[13];
  X[14] := X[14] xor LenBlock[14];
  X[15] := X[15] xor LenBlock[15];
  GCMMultiply(X, Table);
  
  // XOR with encrypted J0
  Move(J0[0], InBlock, 16);
  EncryptBlock(InBlock, ExpandedKey, OutBlock);
  X[0] := X[0] xor OutBlock[0];
  X[1] := X[1] xor OutBlock[1];
  X[2] := X[2] xor OutBlock[2];
  X[3] := X[3] xor OutBlock[3];
  X[4] := X[4] xor OutBlock[4];
  X[5] := X[5] xor OutBlock[5];
  X[6] := X[6] xor OutBlock[6];
  X[7] := X[7] xor OutBlock[7];
  X[8] := X[8] xor OutBlock[8];
  X[9] := X[9] xor OutBlock[9];
  X[10] := X[10] xor OutBlock[10];
  X[11] := X[11] xor OutBlock[11];
  X[12] := X[12] xor OutBlock[12];
  X[13] := X[13] xor OutBlock[13];
  X[14] := X[14] xor OutBlock[14];
  X[15] := X[15] xor OutBlock[15];
  
  Result := X;
end;
{$R+} // Re-enable range checking

{ CTR operations }

{$R-} // Disable range checking for CTR operations
class procedure TAES256.CTRIncrement(var Counter: TBytes);
var
  I: Integer;
begin
  for I := Length(Counter) - 1 downto 0 do
  begin
    Inc(Counter[I]);
    if Counter[I] <> 0 then
      Break;
  end;
end;
{$R+} // Re-enable range checking

{ Public encryption/decryption methods }

class function TAES256.EncryptGCM(const PlainText: TBytes; const Key: TBytes; const IV: TBytes; 
  const AAD: TBytes = nil): TAESGCMResult;
var
  ExpandedKey: TAESExpandedKey;
  Counter, J0, H: TBytes;
  HashTable: TGCMHashTable;
  I, NumBlocks, RemBytes: Integer;
  InBlock, OutBlock: TAESBlock;
  AADLen, CipherLen: QWord;
  TempHashKey: TGCMHashKey;
begin
  // Validate inputs
  if not ValidateKey(Key) then
    raise ETidyKitAESException.Create('Invalid key size. AES-256 requires a 32-byte key.');
  if not ValidateIV(IV, mGCM) then
    raise ETidyKitAESException.Create('Invalid IV size. GCM mode requires a 12-byte IV.');

  // Expand the key
  ExpandKey(Key, ExpandedKey);

  // Initialize GCM - compute H = E(K, 0^128)
  SetLength(H, 16);
  FillChar(H[0], 16, 0);
  Move(H[0], InBlock, 16);
  EncryptBlock(InBlock, ExpandedKey, OutBlock);
  Move(OutBlock, TempHashKey, 16);
  GCMInit(TempHashKey, HashTable);

  // Create J0 (initial counter) = IV || 0^31 || 1
  SetLength(J0, 16);
  Move(IV[0], J0[0], 12);
  FillChar(J0[12], 3, 0);
  J0[15] := 1;

  // Initialize counter for encryption = IV || 0^31 || 2
  SetLength(Counter, 16);
  Move(IV[0], Counter[0], 12);
  FillChar(Counter[12], 3, 0);
  Counter[15] := 2;

  // Prepare output
  CipherLen := Length(PlainText);
  SetLength(Result.CipherText, CipherLen);
  AADLen := Length(AAD);

  // Process full blocks
  NumBlocks := CipherLen div 16;
  RemBytes := CipherLen mod 16;

  for I := 0 to NumBlocks - 1 do
  begin
    // Generate counter block
    Move(Counter[0], InBlock, 16);
    EncryptBlock(InBlock, ExpandedKey, OutBlock);
    CTRIncrement(Counter);

    // XOR with plaintext
    Move(PlainText[I * 16], InBlock, 16);
    XORBlock(OutBlock, InBlock);
    Move(OutBlock, Result.CipherText[I * 16], 16);
  end;

  // Process remaining bytes
  if RemBytes > 0 then
  begin
    Move(Counter[0], InBlock, 16);
    EncryptBlock(InBlock, ExpandedKey, OutBlock);
    
    Move(PlainText[NumBlocks * 16], InBlock, RemBytes);
    for I := 0 to RemBytes - 1 do
      OutBlock[I] := OutBlock[I] xor InBlock[I];
    Move(OutBlock, Result.CipherText[NumBlocks * 16], RemBytes);
  end;

  // Generate authentication tag
  Result.Tag := GCMGenerateTag(AAD, Result.CipherText, HashTable, J0, AADLen, CipherLen);
end;

class function TAES256.DecryptGCM(const CipherText: TBytes; const Key: TBytes; const IV: TBytes;
  const Tag: TBytes; const AAD: TBytes = nil): TBytes;
var
  ExpandedKey: TAESExpandedKey;
  Counter, J0, H, ComputedTag: TBytes;
  HashTable: TGCMHashTable;
  I, NumBlocks, RemBytes: Integer;
  InBlock, OutBlock, TempBlock: TAESBlock;
  AADLen, CipherLen: QWord;
  TempHashKey: TGCMHashKey;
begin
  // Validate inputs
  if not ValidateKey(Key) then
    raise ETidyKitAESException.Create('Invalid key size. AES-256 requires a 32-byte key.');
  if not ValidateIV(IV, mGCM) then
    raise ETidyKitAESException.Create('Invalid IV size. GCM mode requires a 12-byte IV.');
  if Length(Tag) <> 16 then
    raise ETidyKitAESException.Create('Invalid tag size. GCM requires a 16-byte tag.');

  // Expand the key
  ExpandKey(Key, ExpandedKey);

  // Initialize GCM
  SetLength(H, 16);
  FillChar(H[0], 16, 0);
  Move(H[0], TempBlock, 16);
  EncryptBlock(TempBlock, ExpandedKey, OutBlock);
  Move(OutBlock, H[0], 16);
  Move(H[0], TempHashKey, 16);
  GCMInit(TempHashKey, HashTable);

  // Create J0 (initial counter)
  SetLength(J0, 16);
  Move(IV[0], J0[0], 12);
  J0[15] := 1;

  // Verify tag
  AADLen := Length(AAD);
  CipherLen := Length(CipherText);
  ComputedTag := GCMGenerateTag(AAD, CipherText, HashTable, J0, AADLen, CipherLen);
  for I := 0 to 15 do
    if ComputedTag[I] <> Tag[I] then
      raise ETidyKitAESException.Create('Authentication failed. Tag mismatch.');

  // Initialize counter for decryption
  SetLength(Counter, 16);
  Move(IV[0], Counter[0], 12);
  Counter[15] := 2;

  // Prepare output
  SetLength(Result, CipherLen);

  // Process full blocks
  NumBlocks := CipherLen div 16;
  RemBytes := CipherLen mod 16;

  for I := 0 to NumBlocks - 1 do
  begin
    // Generate counter block
    Move(Counter[0], InBlock, 16);
    EncryptBlock(InBlock, ExpandedKey, OutBlock);
    CTRIncrement(Counter);

    // XOR with ciphertext
    Move(CipherText[I * 16], InBlock, 16);
    XORBlock(OutBlock, InBlock);
    Move(OutBlock, Result[I * 16], 16);
  end;

  // Process remaining bytes
  if RemBytes > 0 then
  begin
    Move(Counter[0], InBlock, 16);
    EncryptBlock(InBlock, ExpandedKey, OutBlock);
    
    Move(CipherText[NumBlocks * 16], InBlock, RemBytes);
    for I := 0 to RemBytes - 1 do
      OutBlock[I] := OutBlock[I] xor InBlock[I];
    Move(OutBlock, Result[NumBlocks * 16], RemBytes);
  end;
end;

class function TAES256.EncryptCTR(const PlainText: TBytes; const Key: TBytes; const IV: TBytes): TBytes;
var
  ExpandedKey: TAESExpandedKey;
  Counter: TBytes;
  I, NumBlocks, RemBytes: Integer;
  InBlock, OutBlock: TAESBlock;
begin
  // Validate inputs
  if not ValidateKey(Key) then
    raise ETidyKitAESException.Create('Invalid key size. AES-256 requires a 32-byte key.');
  if not ValidateIV(IV, mCTR) then
    raise ETidyKitAESException.Create('Invalid IV size. CTR mode requires a 16-byte IV.');

  // Expand the key
  ExpandKey(Key, ExpandedKey);

  // Initialize counter with IV
  SetLength(Counter, 16);
  Move(IV[0], Counter[0], 16);

  // Prepare output
  SetLength(Result, Length(PlainText));

  // Process full blocks
  NumBlocks := Length(PlainText) div 16;
  RemBytes := Length(PlainText) mod 16;

  for I := 0 to NumBlocks - 1 do
  begin
    // Generate counter block
    Move(Counter[0], InBlock, 16);
    EncryptBlock(InBlock, ExpandedKey, OutBlock);
    CTRIncrement(Counter);

    // XOR with plaintext
    Move(PlainText[I * 16], InBlock, 16);
    XORBlock(OutBlock, InBlock);
    Move(OutBlock, Result[I * 16], 16);
  end;

  // Process remaining bytes
  if RemBytes > 0 then
  begin
    Move(Counter[0], InBlock, 16);
    EncryptBlock(InBlock, ExpandedKey, OutBlock);
    
    Move(PlainText[NumBlocks * 16], InBlock, RemBytes);
    for I := 0 to RemBytes - 1 do
      OutBlock[I] := OutBlock[I] xor InBlock[I];
    Move(OutBlock, Result[NumBlocks * 16], RemBytes);
  end;
end;

class function TAES256.DecryptCTR(const CipherText: TBytes; const Key: TBytes; const IV: TBytes): TBytes;
begin
  // CTR mode decryption is identical to encryption
  Result := EncryptCTR(CipherText, Key, IV);
end;

end. 