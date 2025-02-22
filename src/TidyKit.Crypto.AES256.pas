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
    constructor Create(Mode: TAESMode; const Key: TAESKey; const IV: TAESBlock);
    destructor Destroy; override;
    
    function Encrypt(const Data: TBytes): TBytes;
    function Decrypt(const Data: TBytes): TBytes;
    
    property Mode: TAESMode read FMode;
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

procedure TAESCipher.SubBytes(var State: TAESBlock);
var
  I: Integer;
begin
  for I := 0 to 15 do
    State[I] := SBox[State[I]];
end;

procedure TAESCipher.InvSubBytes(var State: TAESBlock);
var
  I: Integer;
begin
  for I := 0 to 15 do
    State[I] := InvSBox[State[I]];
end;

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

class procedure TAESCipher.XorBlock(const Source1, Source2: PByte; Dest: PByte; Size: Integer);
var
  I: Integer;
begin
  for I := 0 to Size - 1 do
    Dest[I] := Source1[I] xor Source2[I];
end;

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
 
end. 