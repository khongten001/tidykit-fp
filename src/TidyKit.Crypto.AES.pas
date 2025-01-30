unit TidyKit.Crypto.AES;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, Base64;

type
  TAESKeySize = (ks128, ks192, ks256);

const
  // Block length in bytes - AES is 128b block only
  AES_BLOCKLEN = 16;
  
  // Key lengths for different AES variants
  AES_KEYLEN: array[TAESKeySize] of Integer = (16, 24, 32);
  AES_KEYEXPSIZE: array[TAESKeySize] of Integer = (176, 208, 240);
  AES_NK: array[TAESKeySize] of Integer = (4, 6, 8);
  AES_NR: array[TAESKeySize] of Integer = (10, 12, 14);
  
  // Number of columns comprising a state in AES
  Nb = 4;

type
  // State array type for AES operations
  TState = array[0..3, 0..3] of Byte;
  PState = ^TState;

  // AES context structure
  TAESContext = record
    RoundKey: array[0..255] of Byte;  // Large enough for all key sizes
    IV: array[0..AES_BLOCKLEN-1] of Byte;
    KeySize: TAESKeySize;
  end;
  PAESContext = ^TAESContext;

  // AES operation modes
  TAESMode = (amECB, amCBC, amCTR);

  { TAESKit }
  TAESKit = class
  private
    class procedure KeyExpansion(var RoundKey: array of Byte; const Key: array of Byte; KeySize: TAESKeySize);
    class procedure AddRoundKey(Round: Byte; var State: TState; const RoundKey: array of Byte);
    class procedure SubBytes(var State: TState);
    class procedure InvSubBytes(var State: TState);
    class procedure ShiftRows(var State: TState);
    class procedure InvShiftRows(var State: TState);
    class procedure MixColumns(var State: TState);
    class procedure InvMixColumns(var State: TState);
    class function XTime(X: Byte): Byte;
    class procedure Cipher(var State: TState; const RoundKey: array of Byte; KeySize: TAESKeySize);
    class procedure InvCipher(var State: TState; const RoundKey: array of Byte; KeySize: TAESKeySize);
    class procedure XorWithIv(var Buf; const Iv: array of Byte; BlockLen: Integer);
  public
    class procedure InitContext(var Context: TAESContext; const Key: array of Byte; KeySize: TAESKeySize = ks128);
    class procedure InitContextIV(var Context: TAESContext; const Key, IV: array of Byte; KeySize: TAESKeySize = ks128);
    class procedure SetIV(var Context: TAESContext; const IV: array of Byte);
    
    // ECB mode operations
    class procedure ECBEncrypt(const Context: TAESContext; var Buffer; BlockLen: Integer);
    class procedure ECBDecrypt(const Context: TAESContext; var Buffer; BlockLen: Integer);
    
    // CBC mode operations
    class procedure CBCEncrypt(var Context: TAESContext; var Buffer; Length: Integer);
    class procedure CBCDecrypt(var Context: TAESContext; var Buffer; Length: Integer);
    
    // CTR mode operations
    class procedure CTRCrypt(var Context: TAESContext; var Buffer; Length: Integer);
    
    // Helper methods for string operations
    class function EncryptString(const Text, Key: string; Mode: TAESMode; KeySize: TAESKeySize = ks128): string;
    class function DecryptString(const CipherText, Key: string; Mode: TAESMode; KeySize: TAESKeySize = ks128): string;
  end;

// Lookup tables
const
  // Forward S-box
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

  // Inverse S-box
  RSBox: array[0..255] of Byte = (
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

  // Round constant word array
  Rcon: array[0..10] of Byte = (
    $01, $02, $04, $08, $10, $20, $40, $80, $1B, $36, $00
  );

  // Add lookup tables for Galois Field multiplication
  Mul9: array[0..255] of Byte = (
    $00,$09,$12,$1b,$24,$2d,$36,$3f,$48,$41,$5a,$53,$6c,$65,$7e,$77,
    $90,$99,$82,$8b,$b4,$bd,$a6,$af,$d8,$d1,$ca,$c3,$fc,$f5,$ee,$e7,
    $3b,$32,$29,$20,$1f,$16,$0d,$04,$73,$7a,$61,$68,$57,$5e,$45,$4c,
    $ab,$a2,$b9,$b0,$8f,$86,$9d,$94,$e3,$ea,$f1,$f8,$c7,$ce,$d5,$dc,
    $76,$7f,$64,$6d,$52,$5b,$40,$49,$3e,$37,$2c,$25,$1a,$13,$08,$01,
    $e6,$ef,$f4,$fd,$c2,$cb,$d0,$d9,$ae,$a5,$b8,$b3,$82,$89,$94,$9f,
    $46,$4d,$50,$5b,$6a,$61,$7c,$77,$1e,$15,$08,$03,$32,$39,$24,$2f,
    $dd,$d6,$c1,$c8,$e5,$ea,$f7,$fc,$95,$9e,$89,$80,$af,$a4,$b1,$ba,
    $db,$d0,$c5,$cc,$e9,$e2,$f5,$fe,$93,$98,$8d,$84,$a7,$ac,$bf,$b4,
    $0a,$01,$16,$1f,$32,$39,$2e,$25,$6e,$65,$72,$7b,$56,$5d,$4a,$43,
    $3d,$36,$21,$28,$05,$0c,$1b,$12,$7f,$74,$63,$6a,$47,$4c,$59,$50,
    $ad,$a6,$b1,$b8,$95,$9e,$89,$80,$e3,$e8,$fd,$f4,$d9,$d2,$c5,$cc,
    $29,$22,$35,$3c,$11,$1a,$0d,$04,$67,$6c,$7b,$72,$5f,$54,$43,$4a,
    $f6,$f1,$e6,$ef,$cc,$c7,$d0,$d9,$b4,$bf,$a8,$a1,$8c,$87,$90,$99,
    $4d,$46,$51,$58,$75,$7e,$69,$60,$03,$08,$1f,$14,$39,$32,$25,$2c,
    $dd,$d6,$c1,$c8,$e5,$ea,$f7,$fc,$95,$9e,$89,$80,$af,$a4,$b1,$ba
  );

  Mul11: array[0..255] of Byte = (
    $00,$0b,$16,$1d,$2c,$27,$3a,$31,$58,$53,$4e,$45,$74,$7f,$62,$69,
    $b0,$bb,$a6,$ad,$9c,$97,$8a,$81,$e8,$e3,$fe,$f5,$c4,$cf,$d2,$d9,
    $7b,$70,$6d,$66,$57,$5c,$41,$4a,$23,$28,$35,$3e,$0f,$04,$19,$12,
    $cb,$c0,$dd,$d6,$e7,$ec,$f1,$fa,$93,$98,$85,$8e,$bf,$b4,$a9,$a2,
    $f6,$fd,$e0,$eb,$da,$d1,$cc,$c7,$ae,$a5,$b8,$b3,$82,$89,$94,$9f,
    $46,$4d,$50,$5b,$75,$7e,$6c,$61,$3b,$30,$2d,$26,$0a,$01,$1e,$15,
    $8d,$86,$9b,$90,$a1,$aa,$b7,$bc,$d5,$de,$c3,$c8,$e5,$ee,$f3,$f8,
    $6b,$60,$7d,$76,$58,$53,$4e,$45,$29,$22,$3f,$34,$1c,$17,$0a,$01,
    $f7,$fc,$e1,$ea,$c8,$c3,$d8,$d1,$b4,$bf,$a2,$a9,$8f,$84,$99,$92,
    $4a,$41,$5c,$57,$79,$72,$6f,$64,$0c,$07,$1a,$11,$33,$38,$25,$2e,
    $96,$9d,$80,$8b,$a5,$ae,$b3,$b8,$d4,$df,$c2,$c9,$e7,$ec,$f1,$f8,
    $7a,$71,$6c,$67,$49,$42,$5f,$54,$3c,$37,$2a,$21,$03,$08,$15,$1e,
    $a6,$ad,$b0,$bb,$95,$9e,$83,$88,$e4,$ef,$f2,$f9,$d7,$dc,$c1,$c8,
    $20,$2b,$36,$3d,$13,$18,$05,$0e,$62,$69,$74,$7f,$51,$5a,$47,$4c,
    $f8,$f3,$e6,$ef,$cc,$c7,$d2,$db,$bd,$b6,$a3,$aa,$84,$8f,$92,$99,
    $6b,$60,$7d,$76,$58,$53,$4e,$45,$29,$22,$3f,$34,$1c,$17,$0a,$01
  );

  Mul13: array[0..255] of Byte = (
    $00,$0d,$1a,$17,$34,$39,$2e,$23,$68,$65,$72,$7f,$5c,$51,$46,$4b,
    $d0,$dd,$ca,$c7,$e4,$e9,$fe,$f3,$b8,$b5,$a2,$af,$8c,$81,$96,$9b,
    $bb,$b6,$a1,$ac,$8f,$82,$95,$98,$d3,$de,$c9,$c4,$e7,$ea,$fd,$f0,
    $6b,$66,$71,$7c,$5f,$52,$45,$48,$03,$0e,$19,$14,$37,$3a,$2d,$20,
    $6d,$60,$77,$7a,$59,$54,$43,$4e,$05,$08,$1f,$12,$31,$3c,$2b,$26,
    $bd,$b0,$a7,$aa,$89,$84,$93,$9e,$d5,$d8,$cb,$c6,$e5,$e8,$fb,$f6,
    $4a,$47,$50,$5d,$7e,$73,$64,$69,$22,$2f,$38,$35,$16,$1b,$0c,$01,
    $9b,$96,$81,$8c,$af,$a2,$b5,$b8,$d7,$da,$cd,$c0,$e3,$ee,$f9,$f4,
    $19,$14,$03,$0e,$2d,$20,$37,$3a,$71,$7c,$6b,$66,$45,$48,$5f,$52,
    $3f,$32,$25,$28,$0b,$06,$11,$1c,$57,$5a,$4d,$40,$63,$6e,$79,$74,
    $b9,$b4,$a3,$ae,$8d,$80,$97,$9a,$d1,$dc,$cb,$c6,$e5,$e8,$fb,$f6,
    $7f,$72,$65,$68,$4b,$46,$51,$5c,$17,$1a,$0d,$00,$23,$2e,$39,$34,
    $8e,$83,$94,$99,$ba,$b7,$a0,$ad,$82,$8f,$98,$95,$b6,$bb,$ac,$a1,
    $4d,$40,$57,$5a,$79,$74,$63,$6e,$25,$28,$3f,$32,$11,$1c,$0b,$06,
    $cf,$c2,$d5,$d8,$fb,$f6,$e1,$ec,$a7,$aa,$b1,$bc,$9f,$92,$85,$88,
    $9b,$96,$81,$8c,$af,$a2,$b5,$b8,$d7,$da,$cd,$c0,$e3,$ee,$f9,$f4
  );

  Mul14: array[0..255] of Byte = (
    $00,$0e,$1c,$12,$38,$36,$24,$2a,$70,$7e,$6c,$62,$48,$46,$54,$5a,
    $e0,$ee,$fc,$f2,$d8,$d6,$c4,$ca,$90,$9e,$8c,$82,$a8,$a6,$b4,$ba,
    $db,$d5,$c7,$c9,$e3,$ed,$ff,$f1,$ab,$a5,$b7,$b9,$93,$9d,$8f,$81,
    $3b,$35,$27,$29,$03,$0d,$1f,$11,$4b,$45,$57,$59,$73,$7d,$6f,$61,
    $ad,$a3,$b1,$bf,$95,$9b,$89,$87,$dd,$d3,$c1,$cf,$e5,$eb,$f9,$f7,
    $4d,$43,$51,$5f,$75,$7b,$69,$67,$3d,$33,$21,$2f,$05,$0b,$19,$17,
    $76,$78,$6a,$64,$4e,$40,$52,$5c,$06,$08,$1a,$14,$3e,$30,$22,$2c,
    $96,$98,$8a,$84,$ae,$a0,$b2,$bc,$e6,$e8,$fa,$f4,$de,$d0,$c2,$cc,
    $41,$4f,$5d,$53,$79,$77,$65,$6b,$31,$3f,$2d,$23,$09,$07,$15,$1b,
    $a1,$af,$bd,$b3,$99,$97,$85,$8b,$d1,$df,$cd,$c3,$e9,$e7,$f5,$fb,
    $9a,$94,$86,$88,$a2,$ac,$be,$b0,$ea,$e4,$f6,$f8,$d2,$dc,$ce,$c0,
    $7a,$74,$66,$68,$42,$4c,$5e,$50,$0a,$04,$16,$18,$32,$3c,$2e,$20,
    $ec,$e2,$f0,$fe,$d4,$da,$c8,$c6,$9c,$92,$80,$8e,$a4,$aa,$b8,$b6,
    $0c,$02,$10,$1e,$34,$3a,$28,$26,$7c,$72,$60,$6e,$44,$4a,$58,$56,
    $37,$39,$2b,$25,$0f,$01,$13,$1d,$47,$49,$5b,$55,$7f,$71,$63,$6d,
    $d7,$d9,$cb,$c5,$ef,$e1,$f3,$fd,$a7,$a9,$bb,$b5,$9f,$91,$83,$8d
  );

implementation

{ TAESKit }

class procedure TAESKit.KeyExpansion(var RoundKey: array of Byte; const Key: array of Byte; KeySize: TAESKeySize);
var
  I: Integer;
  Tempa: array[0..3] of Byte;
  K: Integer;
  Nk: Integer;
  Nr: Integer;
begin
  Nk := AES_NK[KeySize];
  Nr := AES_NR[KeySize];

  // The first round key is the key itself
  Move(Key[0], RoundKey[0], Nk * 4);

  // All other round keys are found from the previous round keys
  I := Nk;
  while (I < Nb * (Nr + 1)) do
  begin
    // Copy last word
    Move(RoundKey[(I - 1) * 4], Tempa[0], 4);

    if (I mod Nk = 0) then
    begin
      // Rotate word
      K := Tempa[0];
      Tempa[0] := Tempa[1];
      Tempa[1] := Tempa[2];
      Tempa[2] := Tempa[3];
      Tempa[3] := K;

      // SubWord
      Tempa[0] := SBox[Tempa[0]];
      Tempa[1] := SBox[Tempa[1]];
      Tempa[2] := SBox[Tempa[2]];
      Tempa[3] := SBox[Tempa[3]];

      // XOR with Rcon
      Tempa[0] := Tempa[0] xor Rcon[I div Nk];
    end
    else if (Nk > 6) and (I mod Nk = 4) then
    begin
      // SubWord only for AES-256
      Tempa[0] := SBox[Tempa[0]];
      Tempa[1] := SBox[Tempa[1]];
      Tempa[2] := SBox[Tempa[2]];
      Tempa[3] := SBox[Tempa[3]];
    end;

    // XOR with previous round key word
    for K := 0 to 3 do
      RoundKey[I * 4 + K] := RoundKey[(I - Nk) * 4 + K] xor Tempa[K];
    
    Inc(I);
  end;
end;

class function TAESKit.XTime(X: Byte): Byte;
begin
  // Multiply by x (2) in GF(2^8)
  Result := (X shl 1) and $FF;
  if (X and $80) <> 0 then
    Result := Result xor $1B;  // Reduction modulo x^8 + x^4 + x^3 + x + 1
end;

class procedure TAESKit.AddRoundKey(Round: Byte; var State: TState; const RoundKey: array of Byte);
var
  I, J: Integer;
begin
  // The round key is added to the state by an XOR operation
  // State[row][col] XOR RoundKey[Round * Nb * 4 + col * 4 + row]
  for I := 0 to 3 do
    for J := 0 to 3 do
      State[I][J] := State[I][J] xor RoundKey[Round * Nb * 4 + J * 4 + I];
end;

class procedure TAESKit.SubBytes(var State: TState);
var
  I, J: Integer;
begin
  for J := 0 to 3 do
    for I := 0 to 3 do
      State[I][J] := SBox[State[I][J]];
end;

class procedure TAESKit.InvSubBytes(var State: TState);
var
  I, J: Integer;
begin
  for J := 0 to 3 do
    for I := 0 to 3 do
      State[I][J] := RSBox[State[I][J]];
end;

class procedure TAESKit.ShiftRows(var State: TState);
var
  Temp: array[0..3] of Byte;
  I: Integer;
begin
  // Row 1: shift left by 1
  Temp[0] := State[1][0];
  State[1][0] := State[1][1];
  State[1][1] := State[1][2];
  State[1][2] := State[1][3];
  State[1][3] := Temp[0];

  // Row 2: shift left by 2
  Temp[0] := State[2][0];
  Temp[1] := State[2][1];
  State[2][0] := State[2][2];
  State[2][1] := State[2][3];
  State[2][2] := Temp[0];
  State[2][3] := Temp[1];

  // Row 3: shift left by 3 (or right by 1)
  Temp[0] := State[3][3];
  State[3][3] := State[3][2];
  State[3][2] := State[3][1];
  State[3][1] := State[3][0];
  State[3][0] := Temp[0];
end;

class procedure TAESKit.InvShiftRows(var State: TState);
var
  Temp: array[0..3] of Byte;
  I: Integer;
begin
  // Row 1: shift right by 1
  Temp[0] := State[1][3];
  State[1][3] := State[1][2];
  State[1][2] := State[1][1];
  State[1][1] := State[1][0];
  State[1][0] := Temp[0];

  // Row 2: shift right by 2
  Temp[0] := State[2][2];
  Temp[1] := State[2][3];
  State[2][3] := State[2][1];
  State[2][2] := State[2][0];
  State[2][1] := Temp[0];
  State[2][0] := Temp[1];

  // Row 3: shift right by 3 (or left by 1)
  Temp[0] := State[3][0];
  State[3][0] := State[3][1];
  State[3][1] := State[3][2];
  State[3][2] := State[3][3];
  State[3][3] := Temp[0];
end;

class procedure TAESKit.MixColumns(var State: TState);
var
  I: Integer;
  T0, T1, T2, T3: Byte;
  S0, S1, S2, S3: Byte;
begin
  for I := 0 to 3 do
  begin
    S0 := State[0][I];
    S1 := State[1][I];
    S2 := State[2][I];
    S3 := State[3][I];

    // MixColumns matrix multiplication:
    // [02 03 01 01] [S0]
    // [01 02 03 01] [S1]
    // [01 01 02 03] [S2]
    // [03 01 01 02] [S3]
    
    T0 := XTime(S0) xor (XTime(S1) xor S1) xor S2 xor S3;
    T1 := S0 xor XTime(S1) xor (XTime(S2) xor S2) xor S3;
    T2 := S0 xor S1 xor XTime(S2) xor (XTime(S3) xor S3);
    T3 := (XTime(S0) xor S0) xor S1 xor S2 xor XTime(S3);

    State[0][I] := T0;
    State[1][I] := T1;
    State[2][I] := T2;
    State[3][I] := T3;
  end;
end;

class procedure TAESKit.InvMixColumns(var State: TState);
var
  I: Integer;
  T0, T1, T2, T3: Byte;
  S0, S1, S2, S3: Byte;
begin
  for I := 0 to 3 do
  begin
    S0 := State[0][I];
    S1 := State[1][I];
    S2 := State[2][I];
    S3 := State[3][I];

    // InvMixColumns matrix multiplication:
    // [0E 0B 0D 09] [S0]
    // [09 0E 0B 0D] [S1]
    // [0D 09 0E 0B] [S2]
    // [0B 0D 09 0E] [S3]

    T0 := Mul14[S0] xor Mul11[S1] xor Mul13[S2] xor Mul9[S3];
    T1 := Mul9[S0] xor Mul14[S1] xor Mul11[S2] xor Mul13[S3];
    T2 := Mul13[S0] xor Mul9[S1] xor Mul14[S2] xor Mul11[S3];
    T3 := Mul11[S0] xor Mul13[S1] xor Mul9[S2] xor Mul14[S3];

    State[0][I] := T0;
    State[1][I] := T1;
    State[2][I] := T2;
    State[3][I] := T3;
  end;
end;

class procedure TAESKit.Cipher(var State: TState; const RoundKey: array of Byte; KeySize: TAESKeySize);
var
  Round: Byte;
  Nr: Integer;
  I, J: Integer;
  HexStr: string;
begin
  Nr := AES_NR[KeySize];
  
  // Debug: Print initial state in column-major order
  WriteLn('Initial state (column-major):');
  for J := 0 to 3 do
  begin
    Write('Column ', J, ': ');
    for I := 0 to 3 do
      Write(IntToHex(State[I][J], 2), ' ');
    WriteLn;
  end;
  
  // Add the First round key to the state before starting the rounds
  AddRoundKey(0, State, RoundKey);

  // Debug: Print round key and state after initial AddRoundKey
  WriteLn('Round key 0:');
  for J := 0 to 3 do
  begin
    Write('Column ', J, ': ');
    for I := 0 to 3 do
      Write(IntToHex(RoundKey[J * 4 + I], 2), ' ');
    WriteLn;
  end;
  
  WriteLn('After initial AddRoundKey:');
  for J := 0 to 3 do
  begin
    Write('Column ', J, ': ');
    for I := 0 to 3 do
      Write(IntToHex(State[I][J], 2), ' ');
    WriteLn;
  end;

  // There will be Nr rounds
  // The first Nr-1 rounds are identical
  for Round := 1 to Nr - 1 do
  begin
    SubBytes(State);
    ShiftRows(State);
    MixColumns(State);
    AddRoundKey(Round, State, RoundKey);

    // Debug: Print state after each round
    WriteLn('After round ', Round, ':');
    for J := 0 to 3 do
    begin
      Write('Column ', J, ': ');
      for I := 0 to 3 do
        Write(IntToHex(State[I][J], 2), ' ');
      WriteLn;
    end;
  end;

  // The last round is given below
  // The MixColumns function is not here in the last round
  SubBytes(State);
  ShiftRows(State);
  AddRoundKey(Nr, State, RoundKey);

  // Debug: Print final state
  WriteLn('Final state:');
  for J := 0 to 3 do
  begin
    Write('Column ', J, ': ');
    for I := 0 to 3 do
      Write(IntToHex(State[I][J], 2), ' ');
    WriteLn;
  end;
end;

class procedure TAESKit.InvCipher(var State: TState; const RoundKey: array of Byte; KeySize: TAESKeySize);
var
  Round: Byte;
  Nr: Integer;
begin
  Nr := AES_NR[KeySize];

  // Add the First round key to the state before starting the rounds
  AddRoundKey(Nr, State, RoundKey);

  // There will be Nr rounds
  // The first Nr-1 rounds are identical
  for Round := Nr - 1 downto 1 do
  begin
    InvShiftRows(State);
    InvSubBytes(State);
    AddRoundKey(Round, State, RoundKey);
    InvMixColumns(State);
  end;

  // The last round is given below
  // The InvMixColumns function is not here in the last round
  InvShiftRows(State);
  InvSubBytes(State);
  AddRoundKey(0, State, RoundKey);
end;

class procedure TAESKit.XorWithIv(var Buf; const Iv: array of Byte; BlockLen: Integer);
var
  I: Integer;
  PBuf: PByte;
begin
  PBuf := @Buf;
  for I := 0 to BlockLen - 1 do
    PBuf[I] := PBuf[I] xor Iv[I];
end;

class procedure TAESKit.InitContext(var Context: TAESContext; const Key: array of Byte; KeySize: TAESKeySize = ks128);
begin
  if Length(Key) < AES_KEYLEN[KeySize] then
    raise Exception.CreateFmt('Key length must be at least %d bytes for AES-%d', 
      [AES_KEYLEN[KeySize], (AES_KEYLEN[KeySize] * 8)]);

  Context.KeySize := KeySize;
  KeyExpansion(Context.RoundKey, Key, KeySize);
  FillChar(Context.IV, SizeOf(Context.IV), 0);
end;

class procedure TAESKit.InitContextIV(var Context: TAESContext; const Key, IV: array of Byte; KeySize: TAESKeySize = ks128);
begin
  InitContext(Context, Key, KeySize);
  Move(IV[0], Context.IV[0], AES_BLOCKLEN);
end;

class procedure TAESKit.SetIV(var Context: TAESContext; const IV: array of Byte);
begin
  Move(IV[0], Context.IV[0], AES_BLOCKLEN);
end;

class procedure TAESKit.ECBEncrypt(const Context: TAESContext; var Buffer; BlockLen: Integer);
var
  State: TState;
  PBuf: PByte;
  I, J: Integer;
begin
  if BlockLen <> AES_BLOCKLEN then
    raise Exception.Create('Invalid block length for ECB encryption');
    
  // Copy input bytes to state array in column-major order
  // input[r + 4c] = state[r,c] for 0 ≤ r < 4 and 0 ≤ c < 4
  PBuf := @Buffer;
  for J := 0 to 3 do
    for I := 0 to 3 do
      State[I][J] := PBuf[I + 4*J];
  
  // Encrypt state
  Cipher(State, Context.RoundKey, Context.KeySize);
  
  // Copy state array to output bytes in column-major order
  // output[r + 4c] = state[r,c] for 0 ≤ r < 4 and 0 ≤ c < 4
  for J := 0 to 3 do
    for I := 0 to 3 do
      PBuf[I + 4*J] := State[I][J];
end;

class procedure TAESKit.ECBDecrypt(const Context: TAESContext; var Buffer; BlockLen: Integer);
var
  State: TState;
  PBuf: PByte;
  I, J: Integer;
begin
  if BlockLen <> AES_BLOCKLEN then
    raise Exception.Create('Invalid block length for ECB decryption');
    
  // Copy input bytes to state array in column-major order
  PBuf := @Buffer;
  for J := 0 to 3 do
    for I := 0 to 3 do
      State[I][J] := PBuf[I + 4*J];
  
  // Decrypt state
  InvCipher(State, Context.RoundKey, Context.KeySize);
  
  // Copy state array to output bytes in column-major order
  for J := 0 to 3 do
    for I := 0 to 3 do
      PBuf[I + 4*J] := State[I][J];
end;

class procedure TAESKit.CBCEncrypt(var Context: TAESContext; var Buffer; Length: Integer);
var
  I: Integer;
  PBuf: PByte;
  TempState: TState;
begin
  if Length mod AES_BLOCKLEN <> 0 then
    raise Exception.Create('Invalid buffer length for CBC encryption');

  PBuf := @Buffer;
  I := 0;
  while I < Length do
  begin
    XorWithIv(PBuf[I], Context.IV, AES_BLOCKLEN);
    Move(PBuf[I], TempState, AES_BLOCKLEN);
    Cipher(TempState, Context.RoundKey, Context.KeySize);
    Move(TempState, PBuf[I], AES_BLOCKLEN);
    Move(PBuf[I], Context.IV[0], AES_BLOCKLEN);
    Inc(I, AES_BLOCKLEN);
  end;
end;

class procedure TAESKit.CBCDecrypt(var Context: TAESContext; var Buffer; Length: Integer);
var
  I: Integer;
  PBuf: PByte;
  StoreNextIv: array[0..AES_BLOCKLEN-1] of Byte;
  TempState: TState;
begin
  if Length mod AES_BLOCKLEN <> 0 then
    raise Exception.Create('Invalid buffer length for CBC decryption');

  PBuf := @Buffer;
  I := 0;
  while I < Length do
  begin
    Move(PBuf[I], StoreNextIv[0], AES_BLOCKLEN);
    Move(PBuf[I], TempState, AES_BLOCKLEN);
    InvCipher(TempState, Context.RoundKey, Context.KeySize);
    Move(TempState, PBuf[I], AES_BLOCKLEN);
    XorWithIv(PBuf[I], Context.IV, AES_BLOCKLEN);
    Move(StoreNextIv[0], Context.IV[0], AES_BLOCKLEN);
    Inc(I, AES_BLOCKLEN);
  end;
end;

class procedure TAESKit.CTRCrypt(var Context: TAESContext; var Buffer; Length: Integer);
var
  Buffer16: array[0..AES_BLOCKLEN-1] of Byte;
  I, Bi: Integer;
  PBuf: PByte;
begin
  PBuf := @Buffer;
  Bi := AES_BLOCKLEN;

  for I := 0 to Length - 1 do
  begin
    if Bi = AES_BLOCKLEN then
    begin
      Move(Context.IV[0], Buffer16[0], AES_BLOCKLEN);
      Cipher(TState(Buffer16), Context.RoundKey, Context.KeySize);

      // Increment IV
      for Bi := AES_BLOCKLEN - 1 downto 0 do
      begin
        if Context.IV[Bi] = 255 then
        begin
          Context.IV[Bi] := 0;
          continue;
        end;
        Inc(Context.IV[Bi]);
        break;
      end;
      Bi := 0;
    end;

    PBuf[I] := PBuf[I] xor Buffer16[Bi];
    Inc(Bi);
  end;
end;

class function TAESKit.EncryptString(const Text, Key: string; Mode: TAESMode; KeySize: TAESKeySize = ks128): string;
var
  Context: TAESContext;
  Data, KeyBytes, TextBytes, IVBytes: TBytes;
  PaddedLen, I, PaddingSize: Integer;
  TempStr: AnsiString;
begin
  // Handle empty input
  if Text = '' then
    Exit('');

  try
    // Convert text to bytes
    TextBytes := TEncoding.UTF8.GetBytes(Text);
    
    // Ensure key is long enough
    KeyBytes := TEncoding.UTF8.GetBytes(Key);
    if Length(KeyBytes) < AES_KEYLEN[KeySize] then
      SetLength(KeyBytes, AES_KEYLEN[KeySize]);

    // Calculate padded length and prepare data buffer
    if Mode = amCTR then
    begin
      PaddedLen := Length(TextBytes);
      SetLength(Data, PaddedLen);
      if PaddedLen > 0 then
        Move(TextBytes[0], Data[0], PaddedLen);
    end
    else
    begin
      // For ECB and CBC, calculate padding
      // PKCS7: If length is already multiple of block size, add full block of padding
      PaddingSize := AES_BLOCKLEN - (Length(TextBytes) mod AES_BLOCKLEN);
      if PaddingSize = 0 then
        PaddingSize := AES_BLOCKLEN;
        
      PaddedLen := Length(TextBytes) + PaddingSize;
      SetLength(Data, PaddedLen);
      
      // Copy original data
      if Length(TextBytes) > 0 then
        Move(TextBytes[0], Data[0], Length(TextBytes));
      
      // Add PKCS7 padding
      for I := Length(TextBytes) to PaddedLen - 1 do
        Data[I] := Byte(PaddingSize);
    end;

    // Generate random IV for CBC and CTR modes
    if Mode in [amCBC, amCTR] then
    begin
      SetLength(IVBytes, AES_BLOCKLEN);
      for I := 0 to AES_BLOCKLEN - 1 do
        IVBytes[I] := Random(256);
      InitContextIV(Context, KeyBytes, IVBytes, KeySize);
    end
    else
      InitContext(Context, KeyBytes, KeySize);

    // Encrypt based on mode
    case Mode of
      amECB: 
        begin
          I := 0;
          while I < Length(Data) do
          begin
            ECBEncrypt(Context, Data[I], AES_BLOCKLEN);
            Inc(I, AES_BLOCKLEN);
          end;
        end;
      amCBC: CBCEncrypt(Context, Data[0], Length(Data));
      amCTR: CTRCrypt(Context, Data[0], Length(Data));
    end;

    // For CBC and CTR modes, prepend IV to the result
    if Mode in [amCBC, amCTR] then
    begin
      SetLength(TextBytes, Length(IVBytes) + Length(Data));
      Move(IVBytes[0], TextBytes[0], Length(IVBytes));
      Move(Data[0], TextBytes[Length(IVBytes)], Length(Data));
      
      // Convert to Base64
      SetString(TempStr, PAnsiChar(@TextBytes[0]), Length(TextBytes));
      Result := string(EncodeStringBase64(TempStr));
    end
    else
    begin
      // Convert to Base64
      SetString(TempStr, PAnsiChar(@Data[0]), Length(Data));
      Result := string(EncodeStringBase64(TempStr));
    end;
  finally
    SetLength(Data, 0);
    SetLength(KeyBytes, 0);
    SetLength(TextBytes, 0);
    SetLength(IVBytes, 0);
  end;
end;

class function TAESKit.DecryptString(const CipherText, Key: string; Mode: TAESMode; KeySize: TAESKeySize = ks128): string;
var
  Context: TAESContext;
  Data, KeyBytes, TextBytes, IVBytes: TBytes;
  PaddingLen: Integer;
  BlockPos, I: Integer;
  DecodedStr: AnsiString;
begin
  // Handle empty input
  if CipherText = '' then
    Exit('');

  try
    // Initialize arrays to prevent hints
    SetLength(IVBytes, 0);
    SetLength(Data, 0);
    SetLength(KeyBytes, 0);
    SetLength(TextBytes, 0);
    
    // Decode Base64 first using FPC RTL
    try
      DecodedStr := DecodeStringBase64(AnsiString(CipherText));
      SetLength(TextBytes, Length(DecodedStr));
      Move(PAnsiChar(DecodedStr)^, TextBytes[0], Length(DecodedStr));
    except
      raise Exception.Create('Invalid Base64 encoding');
    end;
    
    KeyBytes := TEncoding.UTF8.GetBytes(Key);
    
    // Ensure key is long enough
    if Length(KeyBytes) < AES_KEYLEN[KeySize] then
      SetLength(KeyBytes, AES_KEYLEN[KeySize]);
    
    // For CBC and CTR modes, extract IV from the beginning
    if Mode in [amCBC, amCTR] then
    begin
      if Length(TextBytes) < AES_BLOCKLEN then
        raise Exception.Create('Invalid ciphertext: too short for IV');
        
      SetLength(IVBytes, AES_BLOCKLEN);
      Move(TextBytes[0], IVBytes[0], AES_BLOCKLEN);
      
      SetLength(Data, Length(TextBytes) - AES_BLOCKLEN);
      if Length(Data) > 0 then
        Move(TextBytes[AES_BLOCKLEN], Data[0], Length(Data));
        
      InitContextIV(Context, KeyBytes, IVBytes, KeySize);
    end
    else
    begin
      SetLength(Data, Length(TextBytes));
      if Length(TextBytes) > 0 then
        Move(TextBytes[0], Data[0], Length(TextBytes));
      InitContext(Context, KeyBytes, KeySize);
    end;

    if Length(Data) = 0 then
      Exit('');

    if (Mode <> amCTR) and (Length(Data) mod AES_BLOCKLEN <> 0) then
      raise Exception.Create('Invalid ciphertext: length not multiple of block size');

    // Decrypt based on mode
    case Mode of
      amECB:
        begin
          BlockPos := 0;
          while BlockPos < Length(Data) do
          begin
            ECBDecrypt(Context, Data[BlockPos], AES_BLOCKLEN);
            Inc(BlockPos, AES_BLOCKLEN);
          end;
        end;
      amCBC: CBCDecrypt(Context, Data[0], Length(Data));
      amCTR: CTRCrypt(Context, Data[0], Length(Data));
    end;

    // Remove PKCS7 padding (not needed for CTR mode)
    if (Mode <> amCTR) and (Length(Data) >= AES_BLOCKLEN) then
    begin
      PaddingLen := Data[Length(Data) - 1];
      
      // Check if padding length is valid
      if (PaddingLen = 0) or (PaddingLen > AES_BLOCKLEN) then
        raise Exception.Create('Invalid padding');
      
      // Verify padding - all padding bytes must be equal to PaddingLen
      for I := Length(Data) - PaddingLen to Length(Data) - 1 do
      begin
        if Data[I] <> PaddingLen then
          raise Exception.Create('Invalid padding');
      end;
      
      SetLength(Data, Length(Data) - PaddingLen);
    end;

    Result := TEncoding.UTF8.GetString(Data);
  finally
    SetLength(Data, 0);
    SetLength(KeyBytes, 0);
    SetLength(TextBytes, 0);
    SetLength(IVBytes, 0);
  end;
end;

end. 
