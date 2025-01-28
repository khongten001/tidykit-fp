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

implementation

{ TAESKit }

class procedure TAESKit.KeyExpansion(var RoundKey: array of Byte; const Key: array of Byte; KeySize: TAESKeySize);
var
  I, J, K: Integer;
  Tempa: array[0..3] of Byte;
begin
  // The first round key is the key itself
  for I := 0 to AES_NK[KeySize] - 1 do
  begin
    RoundKey[I * 4] := Key[I * 4];
    RoundKey[I * 4 + 1] := Key[I * 4 + 1];
    RoundKey[I * 4 + 2] := Key[I * 4 + 2];
    RoundKey[I * 4 + 3] := Key[I * 4 + 3];
  end;

  // All other round keys are found from the previous round keys
  I := AES_NK[KeySize];
  while I < Nb * (AES_NR[KeySize] + 1) do
  begin
    // Copy previous word
    K := (I - 1) * 4;
    Tempa[0] := RoundKey[K];
    Tempa[1] := RoundKey[K + 1];
    Tempa[2] := RoundKey[K + 2];
    Tempa[3] := RoundKey[K + 3];

    if (I mod AES_NK[KeySize] = 0) then
    begin
      // Rotate word
      K := Tempa[0];
      Tempa[0] := Tempa[1];
      Tempa[1] := Tempa[2];
      Tempa[2] := Tempa[3];
      Tempa[3] := K;

      // Apply S-box
      Tempa[0] := SBox[Tempa[0]];
      Tempa[1] := SBox[Tempa[1]];
      Tempa[2] := SBox[Tempa[2]];
      Tempa[3] := SBox[Tempa[3]];

      // XOR with Rcon
      Tempa[0] := Tempa[0] xor Rcon[I div AES_NK[KeySize]];
    end
    else if (AES_NK[KeySize] > 6) and (I mod AES_NK[KeySize] = 4) then
    begin
      // Apply S-box
      Tempa[0] := SBox[Tempa[0]];
      Tempa[1] := SBox[Tempa[1]];
      Tempa[2] := SBox[Tempa[2]];
      Tempa[3] := SBox[Tempa[3]];
    end;

    // XOR with previous round key
    J := I * 4;
    K := (I - AES_NK[KeySize]) * 4;
    RoundKey[J] := RoundKey[K] xor Tempa[0];
    RoundKey[J + 1] := RoundKey[K + 1] xor Tempa[1];
    RoundKey[J + 2] := RoundKey[K + 2] xor Tempa[2];
    RoundKey[J + 3] := RoundKey[K + 3] xor Tempa[3];

    Inc(I);
  end;
end;

class function TAESKit.XTime(X: Byte): Byte;
begin
  // Multiply by x (2) in GF(2^8)
  // The $1B is the irreducible polynomial x^8 + x^4 + x^3 + x + 1
  if (X and $80) <> 0 then
    Result := ((X shl 1) and $FF) xor $1B
  else
    Result := (X shl 1) and $FF;
end;

class procedure TAESKit.AddRoundKey(Round: Byte; var State: TState; const RoundKey: array of Byte);
var
  I, J: Integer;
begin
  for I := 0 to 3 do
    for J := 0 to 3 do
      State[I][J] := State[I][J] xor RoundKey[(Round * Nb * 4) + (I * Nb) + J];
end;

class procedure TAESKit.SubBytes(var State: TState);
var
  I, J: Integer;
begin
  for I := 0 to 3 do
    for J := 0 to 3 do
      State[I][J] := SBox[State[I][J]];
end;

class procedure TAESKit.InvSubBytes(var State: TState);
var
  I, J: Integer;
begin
  for I := 0 to 3 do
    for J := 0 to 3 do
      State[I][J] := RSBox[State[I][J]];
end;

class procedure TAESKit.ShiftRows(var State: TState);
var
  Temp: Byte;
begin
  // Row 1
  Temp := State[1][0];
  State[1][0] := State[1][1];
  State[1][1] := State[1][2];
  State[1][2] := State[1][3];
  State[1][3] := Temp;

  // Row 2
  Temp := State[2][0];
  State[2][0] := State[2][2];
  State[2][2] := Temp;
  Temp := State[2][1];
  State[2][1] := State[2][3];
  State[2][3] := Temp;

  // Row 3
  Temp := State[3][0];
  State[3][0] := State[3][3];
  State[3][3] := State[3][2];
  State[3][2] := State[3][1];
  State[3][1] := Temp;
end;

class procedure TAESKit.InvShiftRows(var State: TState);
var
  Temp: Byte;
begin
  // Row 1
  Temp := State[1][3];
  State[1][3] := State[1][2];
  State[1][2] := State[1][1];
  State[1][1] := State[1][0];
  State[1][0] := Temp;

  // Row 2
  Temp := State[2][0];
  State[2][0] := State[2][2];
  State[2][2] := Temp;
  Temp := State[2][1];
  State[2][1] := State[2][3];
  State[2][3] := Temp;

  // Row 3
  Temp := State[3][0];
  State[3][0] := State[3][1];
  State[3][1] := State[3][2];
  State[3][2] := State[3][3];
  State[3][3] := Temp;
end;

class procedure TAESKit.MixColumns(var State: TState);
var
  I: Integer;
  Tmp, Tm, T: Byte;
begin
  for I := 0 to 3 do
  begin
    T := State[0][I];
    Tmp := State[0][I] xor State[1][I] xor State[2][I] xor State[3][I];
    Tm := State[0][I] xor State[1][I];
    Tm := XTime(Tm);
    State[0][I] := State[0][I] xor Tm xor Tmp;
    Tm := State[1][I] xor State[2][I];
    Tm := XTime(Tm);
    State[1][I] := State[1][I] xor Tm xor Tmp;
    Tm := State[2][I] xor State[3][I];
    Tm := XTime(Tm);
    State[2][I] := State[2][I] xor Tm xor Tmp;
    Tm := State[3][I] xor T;
    Tm := XTime(Tm);
    State[3][I] := State[3][I] xor Tm xor Tmp;
  end;
end;

class procedure TAESKit.InvMixColumns(var State: TState);
var
  I: Integer;
  A, B, C, D: Byte;
begin
  for I := 0 to 3 do
  begin
    A := State[I][0];
    B := State[I][1];
    C := State[I][2];
    D := State[I][3];

    State[I][0] := (XTime(XTime(XTime(A)))) xor
                   (XTime(XTime(B)) xor XTime(B)) xor
                   (XTime(C) xor C) xor
                   (D);

    State[I][1] := (A) xor
                   (XTime(XTime(XTime(B)))) xor
                   (XTime(XTime(C)) xor XTime(C)) xor
                   (XTime(D) xor D);

    State[I][2] := (XTime(A) xor A) xor
                   (B) xor
                   (XTime(XTime(XTime(C)))) xor
                   (XTime(XTime(D)) xor XTime(D));

    State[I][3] := (XTime(XTime(A)) xor XTime(A)) xor
                   (XTime(B) xor B) xor
                   (C) xor
                   (XTime(XTime(XTime(D))));
  end;
end;

class procedure TAESKit.Cipher(var State: TState; const RoundKey: array of Byte; KeySize: TAESKeySize);
var
  Round: Byte;
  Nr: Integer;
begin
  Nr := AES_NR[KeySize];
  AddRoundKey(0, State, RoundKey);

  for Round := 1 to Nr do
  begin
    SubBytes(State);
    ShiftRows(State);
    if Round < Nr then
      MixColumns(State);
    AddRoundKey(Round, State, RoundKey);
  end;
end;

class procedure TAESKit.InvCipher(var State: TState; const RoundKey: array of Byte; KeySize: TAESKeySize);
var
  Round: Byte;
  Nr: Integer;
begin
  Nr := AES_NR[KeySize];
  AddRoundKey(Nr, State, RoundKey);

  for Round := Nr - 1 downto 0 do
  begin
    InvShiftRows(State);
    InvSubBytes(State);
    AddRoundKey(Round, State, RoundKey);
    if Round > 0 then
      InvMixColumns(State);
  end;
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
begin
  if BlockLen <> AES_BLOCKLEN then
    raise Exception.Create('Invalid block length for ECB encryption');
  Cipher(TState(Buffer), Context.RoundKey, Context.KeySize);
end;

class procedure TAESKit.ECBDecrypt(const Context: TAESContext; var Buffer; BlockLen: Integer);
begin
  if BlockLen <> AES_BLOCKLEN then
    raise Exception.Create('Invalid block length for ECB decryption');
  InvCipher(TState(Buffer), Context.RoundKey, Context.KeySize);
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
  PaddedLen: Integer;
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

    // Calculate padded length (multiple of AES_BLOCKLEN)
    PaddedLen := ((Length(TextBytes) + AES_BLOCKLEN - 1) div AES_BLOCKLEN) * AES_BLOCKLEN;
    SetLength(Data, PaddedLen);
    
    // Copy text and add PKCS7 padding
    if Length(TextBytes) > 0 then
      Move(TextBytes[0], Data[0], Length(TextBytes));
    for PaddedLen := Length(TextBytes) to Length(Data) - 1 do
      Data[PaddedLen] := Byte(Length(Data) - Length(TextBytes));

    // Generate random IV for CBC and CTR modes
    if Mode in [amCBC, amCTR] then
    begin
      SetLength(IVBytes, AES_BLOCKLEN);
      for PaddedLen := 0 to AES_BLOCKLEN - 1 do
        IVBytes[PaddedLen] := Random(256);
      InitContextIV(Context, KeyBytes, IVBytes, KeySize);
    end
    else
      InitContext(Context, KeyBytes, KeySize);

    // Encrypt based on mode
    case Mode of
      amECB: 
        begin
          PaddedLen := 0;
          while PaddedLen < Length(Data) do
          begin
            ECBEncrypt(Context, Data[PaddedLen], AES_BLOCKLEN);
            Inc(PaddedLen, AES_BLOCKLEN);
          end;
        end;
      amCBC: CBCEncrypt(Context, Data[0], Length(Data));
      amCTR: CTRCrypt(Context, Data[0], Length(Data));
    end;

    // For CBC and CTR modes, prepend IV to the result
    if Mode in [amCBC, amCTR] then
    begin
      // Combine IV and Data into a single buffer
      SetLength(TextBytes, Length(IVBytes) + Length(Data));
      Move(IVBytes[0], TextBytes[0], Length(IVBytes));
      Move(Data[0], TextBytes[Length(IVBytes)], Length(Data));
      
      // Convert to Base64 using FPC RTL
      SetString(TempStr, PAnsiChar(@TextBytes[0]), Length(TextBytes));
      Result := string(EncodeStringBase64(TempStr));
    end
    else
    begin
      // Convert to Base64 using FPC RTL
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
  BlockPos: Integer;
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

    if Length(Data) mod AES_BLOCKLEN <> 0 then
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

    // Remove PKCS7 padding
    if Length(Data) > 0 then
    begin
      PaddingLen := Data[Length(Data) - 1];
      if (PaddingLen = 0) or (PaddingLen > AES_BLOCKLEN) then
        raise Exception.Create('Invalid padding length');
        
      // Verify padding
      for BlockPos := Length(Data) - PaddingLen to Length(Data) - 1 do
        if Data[BlockPos] <> PaddingLen then
          raise Exception.Create('Invalid padding');
          
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
