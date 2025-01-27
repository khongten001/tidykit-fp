{ TidyKit AES Implementation Unit
  ============================
  
  This unit provides a pure Pascal implementation of the AES (Advanced Encryption Standard)
  algorithm as defined by FIPS 197. The implementation supports:
  
  1. All standard key sizes (128, 192, 256 bits)
  2. Multiple modes of operation (ECB, CBC, CTR, GCM)
  3. PKCS7 padding for modes requiring it
  
  Usage Example:
  -------------
  var
    AES: TAES;
    CipherText: TBytes;
  begin
    AES := TAES.Create(ak256); // Create AES with 256-bit key
    try
      AES.SetKey(Key);
      AES.SetIV(IV);  // For CBC/CTR/GCM modes
      AES.Mode := amCBC;
      CipherText := AES.Encrypt(PlainText);
    finally
      AES.Free;
    end;
  end;
}
unit TidyKit.Crypto.AES;

{$mode objfpc}{$H+}{$J-}

interface

uses
  SysUtils, Classes, TidyKit.Crypto.SHA256;

type
  { AES key sizes }
  TAESKeySize = (ak128, ak192, ak256);
  
  { AES operation modes }
  TAESMode = (amECB, amCBC, amCTR, amGCM);
  
  { AES block mode for compatibility with older code }
  TAESBlockMode = (bmECB, bmCBC, bmCTR, bmGCM);
  
  { AES block size is fixed at 128 bits (16 bytes) }
  TAESBlock = array[0..15] of Byte;
  
  { AES state matrix - 4x4 array of bytes }
  TAESState = array[0..3, 0..3] of Byte;
  
  { Round key type - maximum size for 256-bit key (15 rounds) }
  TAESExpandedKey = array[0..14, 0..3, 0..3] of Byte;

  { TAES - Main class implementing AES algorithm }
  TAES = class
  private
    FKeySize: TAESKeySize;
    FMode: TAESMode;
    FRounds: Integer;
    FState: TAESState;
    FExpandedKey: TAESExpandedKey;
    FIV: TAESBlock;
    FCounter: TAESBlock;
    FAuthenticated: Boolean;
    
    { Key expansion helpers }
    procedure ExpandKey(const Key; KeySize: TAESKeySize);
    procedure SubWord(var Word: Cardinal);
    procedure RotWord(var Word: Cardinal);
    
    { Round transformations }
    procedure SubBytes;
    procedure ShiftRows;
    procedure MixColumns;
    procedure AddRoundKey(Round: Integer);
    
    { Inverse round transformations }
    procedure InvSubBytes;
    procedure InvShiftRows;
    procedure InvMixColumns;
    
    { Block processing }
    procedure EncryptBlock(var Block: TAESBlock);
    procedure DecryptBlock(var Block: TAESBlock);
    
    { Mode-specific operations }
    procedure ProcessCBC(var Block: TAESBlock; Encrypt: Boolean);
    procedure ProcessCTR(var Block: TAESBlock);
    procedure ProcessGCM(var Block: TAESBlock; Encrypt: Boolean);
    
    { Helper functions }
    procedure XorBlock(var Block: TAESBlock; const Other: TAESBlock);
    procedure IncCounter;
    
  public
    constructor Create(KeySize: TAESKeySize = ak256);
    destructor Destroy; override;
    
    { Key and IV management }
    procedure SetKey(const Key; KeySize: TAESKeySize); overload;
    procedure SetKey(const Key: TBytes); overload;
    procedure SetIV(const IV: TAESBlock); overload;
    procedure SetIV(const IV: TBytes); overload;
    
    { Main encryption/decryption methods }
    function Encrypt(const Data; DataSize: Integer): TBytes; overload;
    function Encrypt(const Data: TBytes): TBytes; overload;
    function Decrypt(const Data; DataSize: Integer): TBytes; overload;
    function Decrypt(const Data: TBytes): TBytes; overload;
    
    { Properties }
    property Mode: TAESMode read FMode write FMode;
    property KeySize: TAESKeySize read FKeySize;
    property Authenticated: Boolean read FAuthenticated;
  end;

implementation

const
  { S-box and inverse S-box tables }
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

  { Round constants }
  Rcon: array[0..9] of Cardinal = (
    $01000000, $02000000, $04000000, $08000000, $10000000,
    $20000000, $40000000, $80000000, $1B000000, $36000000
  );

{ TAES implementation }

constructor TAES.Create(KeySize: TAESKeySize);
begin
  inherited Create;
  FKeySize := KeySize;
  case KeySize of
    ak128: FRounds := 10;
    ak192: FRounds := 12;
    ak256: FRounds := 14;
  end;
  FMode := amCBC; // Default to CBC mode
  FAuthenticated := False;
  FillChar(FIV, SizeOf(FIV), 0);
  FillChar(FCounter, SizeOf(FCounter), 0);
end;

destructor TAES.Destroy;
begin
  { Clear sensitive data }
  FillChar(FState, SizeOf(FState), 0);
  FillChar(FExpandedKey, SizeOf(FExpandedKey), 0);
  FillChar(FIV, SizeOf(FIV), 0);
  FillChar(FCounter, SizeOf(FCounter), 0);
  inherited Destroy;
end;

procedure TAES.SubWord(var Word: Cardinal);
var
  Bytes: array[0..3] of Byte absolute Word;
  i: Integer;
begin
  for i := 0 to 3 do
    Bytes[i] := SBox[Bytes[i]];
end;

procedure TAES.RotWord(var Word: Cardinal);
var
  Temp: Byte;
  Bytes: array[0..3] of Byte absolute Word;
begin
  Temp := Bytes[0];
  Bytes[0] := Bytes[1];
  Bytes[1] := Bytes[2];
  Bytes[2] := Bytes[3];
  Bytes[3] := Temp;
end;

procedure TAES.ExpandKey(const Key; KeySize: TAESKeySize);
var
  KeyWords: array[0..7] of Cardinal;
  Temp: Cardinal;
  i, Nk, Nr: Integer;
begin
  case KeySize of
    ak128: begin Nk := 4; Nr := 10; end;
    ak192: begin Nk := 6; Nr := 12; end;
    ak256: begin Nk := 8; Nr := 14; end;
  else
    raise Exception.Create('Invalid key size');
  end;

  { Copy the key into the first Nk words }
  Move(Key, KeyWords, 4 * Nk);

  i := Nk;
  while i < 4 * (Nr + 1) do
  begin
    Temp := KeyWords[(i-1)];
    
    if (i mod Nk) = 0 then
    begin
      RotWord(Temp);
      SubWord(Temp);
      Temp := Temp xor Rcon[(i div Nk) - 1];
    end
    else if (Nk > 6) and ((i mod Nk) = 4) then
      SubWord(Temp);
      
    KeyWords[i] := KeyWords[i - Nk] xor Temp;
    Inc(i);
  end;

  { Convert key schedule to state matrix format }
  for i := 0 to Nr do
    Move(KeyWords[4*i], FExpandedKey[i], 16);
end;

procedure TAES.SubBytes;
var
  i, j: Integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      FState[i,j] := SBox[FState[i,j]];
end;

procedure TAES.ShiftRows;
var
  Temp: array[0..3] of Byte;
begin
  { Row 1: shift left by 1 }
  Temp[0] := FState[1,0];
  FState[1,0] := FState[1,1];
  FState[1,1] := FState[1,2];
  FState[1,2] := FState[1,3];
  FState[1,3] := Temp[0];
  
  { Row 2: shift left by 2 }
  Temp[0] := FState[2,0];
  Temp[1] := FState[2,1];
  FState[2,0] := FState[2,2];
  FState[2,1] := FState[2,3];
  FState[2,2] := Temp[0];
  FState[2,3] := Temp[1];
  
  { Row 3: shift left by 3 (or right by 1) }
  Temp[0] := FState[3,3];
  FState[3,3] := FState[3,2];
  FState[3,2] := FState[3,1];
  FState[3,1] := FState[3,0];
  FState[3,0] := Temp[0];
end;

procedure TAES.AddRoundKey(Round: Integer);
var
  i, j: Integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      FState[i,j] := FState[i,j] xor FExpandedKey[Round, i, j];
end;

procedure TAES.MixColumns;
const
  { Multiplication lookup tables for MixColumns }
  Mul2: array[0..255] of Byte = (
    $00,$02,$04,$06,$08,$0A,$0C,$0E,$10,$12,$14,$16,$18,$1A,$1C,$1E,
    $20,$22,$24,$26,$28,$2A,$2C,$2E,$30,$32,$34,$36,$38,$3A,$3C,$3E,
    $40,$42,$44,$46,$48,$4A,$4C,$4E,$50,$52,$54,$56,$58,$5A,$5C,$5E,
    $60,$62,$64,$66,$68,$6A,$6C,$6E,$70,$72,$74,$76,$78,$7A,$7C,$7E,
    $80,$82,$84,$86,$88,$8A,$8C,$8E,$90,$92,$94,$96,$98,$9A,$9C,$9E,
    $A0,$A2,$A4,$A6,$A8,$AA,$AC,$AE,$B0,$B2,$B4,$B6,$B8,$BA,$BC,$BE,
    $C0,$C2,$C4,$C6,$C8,$CA,$CC,$CE,$D0,$D2,$D4,$D6,$D8,$DA,$DC,$DE,
    $E0,$E2,$E4,$E6,$E8,$EA,$EC,$EE,$F0,$F2,$F4,$F6,$F8,$FA,$FC,$FE,
    $1B,$19,$1F,$1D,$13,$11,$17,$15,$0B,$09,$0F,$0D,$03,$01,$07,$05,
    $3B,$39,$3F,$3D,$33,$31,$37,$35,$2B,$29,$2F,$2D,$23,$21,$27,$25,
    $5B,$59,$5F,$5D,$53,$51,$57,$55,$4B,$49,$4F,$4D,$43,$41,$47,$45,
    $7B,$79,$7F,$7D,$73,$71,$77,$75,$6B,$69,$6F,$6D,$63,$61,$67,$65,
    $9B,$99,$9F,$9D,$93,$91,$97,$95,$8B,$89,$8F,$8D,$83,$81,$87,$85,
    $BB,$B9,$BF,$BD,$B3,$B1,$B7,$B5,$AB,$A9,$AF,$AD,$A3,$A1,$A7,$A5,
    $DB,$D9,$DF,$DD,$D3,$D1,$D7,$D5,$CB,$C9,$CF,$CD,$C3,$C1,$C7,$C5,
    $FB,$F9,$FF,$FD,$F3,$F1,$F7,$F5,$EB,$E9,$EF,$ED,$E3,$E1,$E7,$E5
  );
  
  Mul3: array[0..255] of Byte = (
    $00,$03,$06,$05,$0C,$0F,$0A,$09,$18,$1B,$1E,$1D,$14,$17,$12,$11,
    $30,$33,$36,$35,$3C,$3F,$3A,$39,$28,$2B,$2E,$2D,$24,$27,$22,$21,
    $60,$63,$66,$65,$6C,$6F,$6A,$69,$78,$7B,$7E,$7D,$74,$77,$72,$71,
    $50,$53,$56,$55,$5C,$5F,$5A,$59,$48,$4B,$4E,$4D,$44,$47,$42,$41,
    $C0,$C3,$C6,$C5,$CC,$CF,$CA,$C9,$D8,$DB,$DE,$DD,$D4,$D7,$D2,$D1,
    $F0,$F3,$F6,$F5,$FC,$FF,$FA,$F9,$E8,$EB,$EE,$ED,$E4,$E7,$E2,$E1,
    $A0,$A3,$A6,$A5,$AC,$AF,$AA,$A9,$B8,$BB,$BE,$BD,$B4,$B7,$B2,$B1,
    $90,$93,$96,$95,$9C,$9F,$9A,$99,$88,$8B,$8E,$8D,$84,$87,$82,$81,
    $9B,$98,$9D,$9E,$97,$94,$91,$92,$83,$80,$85,$86,$8F,$8C,$89,$8A,
    $AB,$A8,$AD,$AE,$A7,$A4,$A1,$A2,$B3,$B0,$B5,$B6,$BF,$BC,$B9,$BA,
    $FB,$F8,$FD,$FE,$F7,$F4,$F1,$F2,$E3,$E0,$E5,$E6,$EF,$EC,$E9,$EA,
    $CB,$C8,$CD,$CE,$C7,$C4,$C1,$C2,$D3,$D0,$D5,$D6,$DF,$DC,$D9,$DA,
    $5B,$58,$5D,$5E,$57,$54,$51,$52,$43,$40,$45,$46,$4F,$4C,$49,$4A,
    $6B,$68,$6D,$6E,$67,$64,$61,$62,$73,$70,$75,$76,$7F,$7C,$79,$7A,
    $3B,$38,$3D,$3E,$37,$34,$31,$32,$23,$20,$25,$26,$2F,$2C,$29,$2A,
    $0B,$08,$0D,$0E,$07,$04,$01,$02,$13,$10,$15,$16,$1F,$1C,$19,$1A
  );
var
  i: Integer;
  s0, s1, s2, s3: Byte;
begin
  for i := 0 to 3 do
  begin
    s0 := FState[0,i];
    s1 := FState[1,i];
    s2 := FState[2,i];
    s3 := FState[3,i];
    
    FState[0,i] := Mul2[s0] xor Mul3[s1] xor s2 xor s3;
    FState[1,i] := s0 xor Mul2[s1] xor Mul3[s2] xor s3;
    FState[2,i] := s0 xor s1 xor Mul2[s2] xor Mul3[s3];
    FState[3,i] := Mul3[s0] xor s1 xor s2 xor Mul2[s3];
  end;
end;

procedure TAES.EncryptBlock(var Block: TAESBlock);
var
  Round: Integer;
begin
  { Convert block to state array }
  Move(Block[0], FState[0,0], 16);
  
  { Initial round }
  AddRoundKey(0);
  
  { Main rounds }
  for Round := 1 to FRounds - 1 do
  begin
    SubBytes;
    ShiftRows;
    MixColumns;
    AddRoundKey(Round);
  end;
  
  { Final round }
  SubBytes;
  ShiftRows;
  AddRoundKey(FRounds);
  
  { Convert state back to block }
  Move(FState[0,0], Block[0], 16);
end;

procedure TAES.DecryptBlock(var Block: TAESBlock);
var
  Round: Integer;
begin
  { Convert block to state array }
  Move(Block[0], FState[0,0], 16);
  
  { Initial round }
  AddRoundKey(FRounds);
  
  { Main rounds }
  for Round := FRounds - 1 downto 1 do
  begin
    InvShiftRows;
    InvSubBytes;
    AddRoundKey(Round);
    InvMixColumns;
  end;
  
  { Final round }
  InvShiftRows;
  InvSubBytes;
  AddRoundKey(0);
  
  { Convert state back to block }
  Move(FState[0,0], Block[0], 16);
end;

procedure TAES.ProcessCBC(var Block: TAESBlock; Encrypt: Boolean);
begin
  if Encrypt then
  begin
    XorBlock(Block, FIV);
    EncryptBlock(Block);
    Move(Block[0], FIV[0], 16);
  end
  else
  begin
    Move(Block[0], FIV[0], 16);
    DecryptBlock(Block);
    XorBlock(Block, FIV);
  end;
end;

procedure TAES.ProcessCTR(var Block: TAESBlock);
var
  KeyStream: TAESBlock;
begin
  { Generate keystream }
  Move(FCounter[0], KeyStream[0], 16);
  EncryptBlock(KeyStream);
  
  { XOR with input block }
  XorBlock(Block, KeyStream);
  
  { Increment counter }
  IncCounter;
end;

procedure TAES.ProcessGCM(var Block: TAESBlock; Encrypt: Boolean);
const
  { GCM reduction polynomial }
  R = $E1000000;
var
  H, X, Y: TAESBlock;
  i, j, k: Integer;
  Bit: Byte;
  V: Cardinal;
begin
  if not FAuthenticated then
  begin
    { Initialize H = E(K, 0^128) }
    FillChar(H, 16, 0);
    EncryptBlock(H);
    FAuthenticated := True;
  end;
  
  if Encrypt then
  begin
    { Generate keystream }
    Move(FCounter[0], X[0], 16);
    EncryptBlock(X);
    
    { XOR with plaintext }
    Move(Block[0], Y[0], 16);
    XorBlock(Y, X);
    Move(Y[0], Block[0], 16);
    
    { Update GHASH }
    XorBlock(Y, H);
    
    { Galois field multiplication }
    FillChar(X, 16, 0);
    for i := 0 to 15 do
      for j := 7 downto 0 do
      begin
        if (Y[i] and (1 shl j)) <> 0 then
          XorBlock(X, H);
          
        { Right shift H }
        V := 0;
        for k := 15 downto 0 do
        begin
          V := (V shl 8) or H[k];
          if k > 0 then
            H[k] := Byte(V shr 8);
        end;
        if (V and 1) <> 0 then
          H[0] := H[0] xor R;
      end;
  end
  else
  begin
    { Generate keystream }
    Move(FCounter[0], X[0], 16);
    EncryptBlock(X);
    
    { Update GHASH }
    Move(Block[0], Y[0], 16);
    XorBlock(Y, H);
    
    { Galois field multiplication }
    FillChar(X, 16, 0);
    for i := 0 to 15 do
      for j := 7 downto 0 do
      begin
        if (Y[i] and (1 shl j)) <> 0 then
          XorBlock(X, H);
          
        { Right shift H }
        V := 0;
        for k := 15 downto 0 do
        begin
          V := (V shl 8) or H[k];
          if k > 0 then
            H[k] := Byte(V shr 8);
        end;
        if (V and 1) <> 0 then
          H[0] := H[0] xor R;
      end;
    
    { XOR with ciphertext }
    XorBlock(Block, X);
  end;
  
  { Increment counter }
  IncCounter;
end;

procedure TAES.IncCounter;
var
  i: Integer;
begin
  for i := 15 downto 0 do
  begin
    Inc(FCounter[i]);
    if FCounter[i] <> 0 then
      Break;
  end;
end;

procedure TAES.XorBlock(var Block: TAESBlock; const Other: TAESBlock);
var
  i: Integer;
begin
  for i := 0 to 15 do
    Block[i] := Block[i] xor Other[i];
end;

function TAES.Encrypt(const Data; DataSize: Integer): TBytes;
var
  PData: PByte;
  Block: TAESBlock;
  BlockCount, Padding, i: Integer;
begin
  if DataSize <= 0 then
    Exit(nil);
    
  { Calculate padding size (PKCS7) }
  BlockCount := (DataSize + 15) div 16;
  Padding := BlockCount * 16 - DataSize;
  
  { Allocate output buffer }
  SetLength(Result, BlockCount * 16);
  PData := @Data;
  
  { Process all blocks }
  for i := 0 to BlockCount - 1 do
  begin
    if i = BlockCount - 1 then
    begin
      { Last block - add padding }
      FillChar(Block, 16, Padding);
      Move(PData^, Block[0], 16 - Padding);
    end
    else
    begin
      Move(PData^, Block[0], 16);
      Inc(PData, 16);
    end;
    
    case FMode of
      amECB: EncryptBlock(Block);
      amCBC: ProcessCBC(Block, True);
      amCTR: ProcessCTR(Block);
      amGCM: ProcessGCM(Block, True);
    end;
    
    Move(Block[0], Result[i * 16], 16);
  end;
end;

function TAES.Encrypt(const Data: TBytes): TBytes;
begin
  if Length(Data) > 0 then
    Result := Encrypt(Data[0], Length(Data))
  else
    Result := nil;
end;

function TAES.Decrypt(const Data; DataSize: Integer): TBytes;
var
  PData: PByte;
  Block: TAESBlock;
  BlockCount, Padding, i: Integer;
begin
  if (DataSize <= 0) or (DataSize mod 16 <> 0) then
    Exit(nil);
    
  BlockCount := DataSize div 16;
  SetLength(Result, DataSize);
  PData := @Data;
  
  { Process all blocks }
  for i := 0 to BlockCount - 1 do
  begin
    Move(PData^, Block[0], 16);
    Inc(PData, 16);
    
    case FMode of
      amECB: DecryptBlock(Block);
      amCBC: ProcessCBC(Block, False);
      amCTR: ProcessCTR(Block);
      amGCM: ProcessGCM(Block, False);
    end;
    
    Move(Block[0], Result[i * 16], 16);
  end;
  
  { Remove padding if not CTR/GCM mode }
  if (FMode in [amECB, amCBC]) then
  begin
    Padding := Result[DataSize - 1];
    if (Padding > 0) and (Padding <= 16) then
      SetLength(Result, DataSize - Padding);
  end;
end;

function TAES.Decrypt(const Data: TBytes): TBytes;
begin
  if Length(Data) > 0 then
    Result := Decrypt(Data[0], Length(Data))
  else
    Result := nil;
end;

procedure TAES.SetKey(const Key; KeySize: TAESKeySize);
begin
  FKeySize := KeySize;
  case KeySize of
    ak128: FRounds := 10;
    ak192: FRounds := 12;
    ak256: FRounds := 14;
  end;
  ExpandKey(Key, KeySize);
end;

procedure TAES.SetKey(const Key: TBytes);
var
  Size: TAESKeySize;
  TempKey: array[0..31] of Byte;
  KeyHash: array[0..31] of Byte;
  Ctx: TSHA256;
begin
  FillChar(TempKey, SizeOf(TempKey), 0);
  FillChar(KeyHash, SizeOf(KeyHash), 0);
  
  // Hash the key to get proper length
  Ctx := TSHA256.Create;
  try
    if Length(Key) = 0 then
      raise Exception.Create('Key cannot be empty');
      
    Ctx.Update(Key[0], Length(Key));
    Ctx.Final(KeyHash);
  finally
    Ctx.Free;
  end;
  
  // Use first 16/24/32 bytes based on desired key size
  case FKeySize of
    ak128: Size := ak128;
    ak192: Size := ak192;
    ak256: Size := ak256;
  else
    Size := ak256;  // Default to strongest
  end;
  
  case Size of
    ak128: Move(KeyHash[0], TempKey[0], 16);
    ak192: Move(KeyHash[0], TempKey[0], 24);
    ak256: Move(KeyHash[0], TempKey[0], 32);
  end;
  
  SetKey(TempKey, Size);
end;

procedure TAES.SetIV(const IV: TAESBlock);
begin
  Move(IV[0], FIV[0], 16);
end;

procedure TAES.SetIV(const IV: TBytes);
var
  TempIV: TAESBlock;
begin
  FillChar(TempIV, SizeOf(TempIV), 0);
  
  if Length(IV) < 16 then
    raise Exception.Create('IV must be at least 16 bytes');
    
  Move(IV[0], TempIV[0], 16);
  SetIV(TempIV);
end;

procedure TAES.InvSubBytes;
var
  i, j: Integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      FState[i,j] := InvSBox[FState[i,j]];
end;

procedure TAES.InvShiftRows;
var
  Temp: array[0..3] of Byte;
begin
  { Row 1: shift right by 1 }
  Temp[0] := FState[1,3];
  FState[1,3] := FState[1,2];
  FState[1,2] := FState[1,1];
  FState[1,1] := FState[1,0];
  FState[1,0] := Temp[0];
  
  { Row 2: shift right by 2 }
  Temp[0] := FState[2,2];
  Temp[1] := FState[2,3];
  FState[2,3] := FState[2,1];
  FState[2,2] := FState[2,0];
  FState[2,1] := Temp[0];
  FState[2,0] := Temp[1];
  
  { Row 3: shift right by 3 (or left by 1) }
  Temp[0] := FState[3,0];
  FState[3,0] := FState[3,1];
  FState[3,1] := FState[3,2];
  FState[3,2] := FState[3,3];
  FState[3,3] := Temp[0];
end;

procedure TAES.InvMixColumns;
const
  { Multiplication lookup tables for InvMixColumns }
  Mul9: array[0..255] of Byte = (
    $00,$09,$12,$1B,$24,$2D,$36,$3F,$48,$41,$5A,$53,$6C,$65,$7E,$77,
    $90,$99,$82,$8B,$B4,$BD,$A6,$AF,$D8,$D1,$CA,$C3,$FC,$F5,$EE,$E7,
    $3B,$32,$29,$20,$1F,$16,$0D,$04,$73,$7A,$61,$68,$57,$5E,$45,$4C,
    $AB,$A2,$B9,$B0,$8F,$86,$9D,$94,$E3,$EA,$F1,$F8,$C7,$CE,$D5,$DC,
    $76,$7F,$64,$6D,$52,$5B,$40,$49,$3E,$37,$2C,$25,$1A,$13,$08,$01,
    $E6,$EF,$F4,$FD,$C2,$CB,$D0,$D9,$AE,$A7,$BC,$B5,$8A,$83,$98,$91,
    $4D,$44,$5F,$56,$69,$60,$7B,$72,$05,$0C,$17,$1E,$21,$28,$33,$3A,
    $DD,$D4,$CF,$C6,$F9,$F0,$EB,$E2,$95,$9C,$87,$8E,$B1,$B8,$A3,$AA,
    $EC,$E5,$FE,$F7,$C8,$C1,$DA,$D3,$A4,$AD,$B6,$BF,$80,$89,$92,$9B,
    $7C,$75,$6E,$67,$58,$51,$4A,$43,$34,$3D,$26,$2F,$10,$19,$02,$0B,
    $D7,$DE,$C5,$CC,$F3,$FA,$E1,$E8,$9F,$96,$8D,$84,$BB,$B2,$A9,$A0,
    $47,$4E,$55,$5C,$63,$6A,$71,$78,$0F,$06,$1D,$14,$2B,$22,$39,$30,
    $9A,$93,$88,$81,$BE,$B7,$AC,$A5,$D2,$DB,$C0,$C9,$F6,$FF,$E4,$ED,
    $0A,$03,$18,$11,$2E,$27,$3C,$35,$42,$4B,$50,$59,$66,$6F,$74,$7D,
    $A1,$A8,$B3,$BA,$85,$8C,$97,$9E,$E9,$E0,$FB,$F2,$C3,$CA,$D5,$DC,
    $31,$38,$23,$2A,$15,$1C,$07,$0E,$79,$70,$6B,$62,$5D,$54,$4F,$46
  );
  
  MulB: array[0..255] of Byte = (
    $00,$0B,$16,$1D,$2C,$27,$3A,$31,$58,$53,$4E,$45,$74,$7F,$62,$69,
    $B0,$BB,$A6,$AD,$9C,$97,$8A,$81,$E8,$E3,$FE,$F5,$C4,$CF,$D2,$D9,
    $7B,$70,$6D,$66,$57,$5C,$41,$4A,$23,$28,$35,$3E,$0F,$04,$19,$12,
    $CB,$C0,$DD,$D6,$E7,$EC,$F1,$FA,$93,$98,$85,$8E,$BF,$B4,$A9,$A2,
    $F6,$FD,$E0,$EB,$DA,$D1,$CC,$C7,$AE,$A5,$B8,$B3,$82,$89,$94,$9F,
    $46,$4D,$50,$5B,$6A,$61,$7C,$77,$1E,$15,$08,$03,$32,$3F,$28,$25,
    $8D,$86,$9B,$90,$A1,$AA,$B7,$BC,$D5,$DE,$C3,$C8,$F9,$F2,$EF,$E4,
    $3D,$36,$2B,$20,$11,$1A,$07,$0C,$65,$6E,$73,$78,$49,$42,$5F,$54,
    $F7,$FC,$E1,$EA,$DB,$D0,$CD,$C6,$AF,$A4,$B9,$B2,$83,$88,$95,$9E,
    $47,$4C,$51,$5A,$6B,$60,$7D,$76,$1F,$14,$09,$02,$33,$3E,$29,$24,
    $8C,$87,$9A,$91,$A0,$AB,$B6,$BD,$D4,$DF,$C2,$C9,$F8,$F3,$EE,$E5,
    $3C,$37,$2A,$21,$10,$1B,$06,$0D,$64,$6F,$72,$79,$48,$43,$5E,$55,
    $01,$0A,$17,$1C,$2D,$26,$3B,$30,$59,$52,$4F,$44,$75,$7E,$63,$68,
    $B1,$BA,$A7,$AC,$9D,$96,$8B,$80,$E9,$E2,$FF,$F4,$C5,$CE,$D3,$D8,
    $7A,$71,$6C,$67,$56,$5D,$40,$4B,$22,$29,$34,$3F,$0E,$05,$18,$13,
    $CA,$C1,$DC,$D7,$E6,$ED,$F0,$FB,$92,$99,$84,$8F,$BE,$B5,$A8,$A3
  );
  
  MulD: array[0..255] of Byte = (
    $00,$0D,$1A,$17,$34,$39,$2E,$23,$68,$65,$72,$7F,$5C,$51,$46,$4B,
    $D0,$DD,$CA,$C7,$E4,$E9,$FE,$F3,$B8,$B5,$A2,$AF,$8C,$81,$96,$9B,
    $BB,$B6,$A1,$AC,$8F,$82,$95,$98,$D3,$DE,$C9,$C4,$E7,$EA,$FD,$F0,
    $6B,$66,$71,$7C,$5F,$52,$45,$48,$03,$0E,$19,$14,$37,$3A,$2D,$20,
    $6D,$60,$77,$7A,$59,$54,$43,$4E,$05,$08,$1F,$12,$31,$3C,$2B,$26,
    $BD,$B0,$A7,$AA,$89,$84,$93,$9E,$D5,$D8,$CB,$C6,$E5,$E8,$FB,$F6,
    $D6,$DB,$CC,$C1,$E2,$EF,$F8,$F5,$BE,$B3,$A4,$A9,$8A,$87,$90,$9D,
    $06,$0B,$1C,$11,$32,$3F,$28,$25,$6E,$63,$74,$79,$5A,$57,$40,$4D,
    $DA,$D7,$C0,$CD,$EC,$E1,$F6,$F3,$B8,$B5,$A2,$AF,$8C,$81,$96,$9B,
    $4D,$40,$57,$5A,$79,$74,$63,$6E,$25,$28,$3F,$32,$11,$1C,$0B,$06,
    $5D,$50,$67,$6A,$49,$44,$53,$5E,$15,$18,$0F,$02,$21,$2C,$3B,$36,
    $B1,$BC,$AB,$A6,$85,$88,$9F,$92,$D9,$D4,$C3,$CE,$ED,$E0,$F7,$FA,
    $B7,$BA,$AD,$A0,$83,$8E,$99,$94,$DF,$D2,$C5,$C8,$EB,$E6,$F1,$F2,
    $41,$4C,$5B,$56,$75,$78,$6F,$62,$29,$24,$33,$3E,$1D,$10,$07,$0A,
    $5D,$50,$67,$6A,$49,$44,$53,$5E,$15,$18,$0F,$02,$21,$2C,$3B,$36,
    $B1,$BC,$AB,$A6,$85,$88,$9F,$92,$D9,$D4,$C3,$CE,$ED,$E0,$F7,$FA
  );
  
  MulE: array[0..255] of Byte = (
    $00,$0E,$1C,$12,$38,$36,$24,$2A,$70,$7E,$6C,$62,$48,$46,$54,$5A,
    $E0,$EE,$FC,$F2,$D8,$D6,$C4,$CA,$90,$9E,$8C,$82,$A8,$A6,$B4,$BA,
    $DB,$D5,$C7,$C9,$E3,$ED,$FF,$F1,$AB,$A5,$B7,$B9,$93,$9D,$8F,$81,
    $3B,$35,$27,$29,$03,$0D,$1F,$11,$4B,$45,$57,$59,$73,$7D,$6F,$61,
    $AD,$A3,$B1,$BF,$95,$9B,$89,$87,$DD,$D3,$C1,$CF,$E5,$EB,$F9,$F7,
    $4D,$43,$51,$5F,$75,$7B,$69,$67,$3D,$33,$21,$2F,$05,$0B,$19,$17,
    $76,$78,$6A,$64,$4E,$40,$52,$5C,$06,$08,$1A,$14,$3E,$30,$22,$2C,
    $96,$98,$8A,$84,$AE,$A0,$B2,$BC,$E6,$E8,$FA,$F4,$DE,$D0,$C2,$CC,
    $41,$4F,$5D,$53,$79,$77,$65,$6B,$31,$3F,$2D,$23,$09,$07,$15,$1B,
    $A1,$AF,$BD,$B3,$99,$97,$85,$8B,$D1,$DF,$CD,$C3,$E9,$E7,$F5,$F3,
    $9A,$94,$86,$88,$A2,$AC,$BE,$B0,$EA,$E4,$F6,$F8,$D2,$DC,$CE,$C0,
    $7A,$74,$66,$68,$42,$4C,$5E,$50,$0A,$04,$16,$18,$32,$3C,$2E,$20,
    $EC,$E2,$F0,$FE,$D4,$DA,$C8,$C6,$9C,$92,$80,$8E,$A4,$AA,$B8,$B6,
    $0C,$02,$10,$1E,$34,$3A,$28,$26,$7C,$72,$60,$6E,$44,$4A,$58,$56,
    $37,$39,$2B,$25,$0F,$01,$13,$1D,$47,$49,$5B,$55,$7F,$71,$63,$6D,
    $D7,$D9,$CB,$C5,$EF,$E1,$F3,$FD,$A7,$A9,$BB,$B5,$9F,$91,$83,$8D
  );
var
  i: Integer;
  s0, s1, s2, s3: Byte;
begin
  for i := 0 to 3 do
  begin
    s0 := FState[0,i];
    s1 := FState[1,i];
    s2 := FState[2,i];
    s3 := FState[3,i];
    
    FState[0,i] := MulE[s0] xor MulB[s1] xor MulD[s2] xor Mul9[s3];
    FState[1,i] := Mul9[s0] xor MulE[s1] xor MulB[s2] xor MulD[s3];
    FState[2,i] := MulD[s0] xor Mul9[s1] xor MulE[s2] xor MulB[s3];
    FState[3,i] := MulB[s0] xor MulD[s1] xor Mul9[s2] xor MulE[s3];
  end;
end;

end. 