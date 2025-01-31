unit TidyKit.Crypto.SHA2;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils;

type
  TSHA2_256State = array[0..7] of Cardinal;
  TSHA2_512State = array[0..7] of QWord;
  TSHA2_256Block = array[0..15] of Cardinal;
  TSHA2_512Block = array[0..15] of QWord;

  { UInt128 record for handling 128-bit arithmetic }
  TUInt128 = record
    Lo: QWord;
    Hi: QWord;
  end;

  { TSHA2 }
  TSHA2 = class
  private
    class procedure SHA256Transform(var State: TSHA2_256State; const Block: TSHA2_256Block); static;
    class procedure SHA512Transform(var State: TSHA2_512State; const Block: TSHA2_512Block); static;
    class function SwapEndian(Value: Cardinal): Cardinal; static;
    class function SwapEndian64(Value: QWord): QWord; static;
    class function RightRotate(Value: Cardinal; Bits: Byte): Cardinal; static;
    class function RightRotate64(Value: QWord; Bits: Byte): QWord; static;
    class function Ch(x, y, z: Cardinal): Cardinal; static;
    class function Ch64(x, y, z: QWord): QWord; static;
    class function Maj(x, y, z: Cardinal): Cardinal; static;
    class function Maj64(x, y, z: QWord): QWord; static;
    class function BigSigma0(x: Cardinal): Cardinal; static;
    class function BigSigma0_64(x: QWord): QWord; static;
    class function BigSigma1(x: Cardinal): Cardinal; static;
    class function BigSigma1_64(x: QWord): QWord; static;
    class function SmallSigma0(x: Cardinal): Cardinal; static;
    class function SmallSigma0_64(x: QWord): QWord; static;
    class function SmallSigma1(x: Cardinal): Cardinal; static;
    class function SmallSigma1_64(x: QWord): QWord; static;
    { Helper function for 128-bit addition }
    class procedure Add128(var Sum: TUInt128; Value: QWord);
  public
    class function SHA256(const Data: string): string; static;
    class function SHA512(const Data: string): string; static;
    class function SHA512_256(const Data: string): string; static;
  end;

implementation

const
  SHA256_K: array[0..63] of Cardinal = (
    $428A2F98, $71374491, $B5C0FBCF, $E9B5DBA5, $3956C25B, $59F111F1, $923F82A4, $AB1C5ED5,
    $D807AA98, $12835B01, $243185BE, $550C7DC3, $72BE5D74, $80DEB1FE, $9BDC06A7, $C19BF174,
    $E49B69C1, $EFBE4786, $0FC19DC6, $240CA1CC, $2DE92C6F, $4A7484AA, $5CB0A9DC, $76F988DA,
    $983E5152, $A831C66D, $B00327C8, $BF597FC7, $C6E00BF3, $D5A79147, $06CA6351, $14292967,
    $27B70A85, $2E1B2138, $4D2C6DFC, $53380D13, $650A7354, $766A0ABB, $81C2C92E, $92722C85,
    $A2BFE8A1, $A81A664B, $C24B8B70, $C76C51A3, $D192E819, $D6990624, $F40E3585, $106AA070,
    $19A4C116, $1E376C08, $2748774C, $34B0BCB5, $391C0CB3, $4ED8AA4A, $5B9CCA4F, $682E6FF3,
    $748F82EE, $78A5636F, $84C87814, $8CC70208, $90BEFFFA, $A4506CEB, $BEF9A3F7, $C67178F2
  );

  SHA512_K: array[0..79] of QWord = (
    QWord($428A2F98D728AE22), QWord($7137449123EF65CD), QWord($B5C0FBCFEC4D3B2F), QWord($E9B5DBA58189DBBC),
    QWord($3956C25BF348B538), QWord($59F111F1B605D019), QWord($923F82A4AF194F9B), QWord($AB1C5ED5DA6D8118),
    QWord($D807AA98A3030242), QWord($12835B0145706FBE), QWord($243185BE4EE4B28C), QWord($550C7DC3D5FFB4E2),
    QWord($72BE5D74F27B896F), QWord($80DEB1FE3B1696B1), QWord($9BDC06A725C71235), QWord($C19BF174CF692694),
    QWord($E49B69C19EF14AD2), QWord($EFBE4786384F25E3), QWord($0FC19DC68B8CD5B5), QWord($240CA1CC77AC9C65),
    QWord($2DE92C6F592B0275), QWord($4A7484AA6EA6E483), QWord($5CB0A9DCBD41FBD4), QWord($76F988DA831153B5),
    QWord($983E5152EE66DFAB), QWord($A831C66D2DB43210), QWord($B00327C898FB213F), QWord($BF597FC7BEEF0EE4),
    QWord($C6E00BF33DA88FC2), QWord($D5A79147930AA725), QWord($06CA6351E003826F), QWord($142929670A0E6E70),
    QWord($27B70A8546D22FFC), QWord($2E1B21385C26C926), QWord($4D2C6DFC5AC42AED), QWord($53380D139D95B3DF),
    QWord($650A73548BAF63DE), QWord($766A0ABB3C77B2A8), QWord($81C2C92E47EDAEE6), QWord($92722C851482353B),
    QWord($A2BFE8A14CF10364), QWord($A81A664BBC423001), QWord($C24B8B70D0F89791), QWord($C76C51A30654BE30),
    QWord($D192E819D6EF5218), QWord($D69906245565A910), QWord($F40E35855771202A), QWord($106AA07032BBD1B8),
    QWord($19A4C116B8D2D0C8), QWord($1E376C085141AB53), QWord($2748774CDF8EEB99), QWord($34B0BCB5E19B48A8),
    QWord($391C0CB3C5C95A63), QWord($4ED8AA4AE3418ACB), QWord($5B9CCA4F7763E373), QWord($682E6FF3D6B2B8A3),
    QWord($748F82EE5DEFB2FC), QWord($78A5636F43172F60), QWord($84C87814A1F0AB72), QWord($8CC702081A6439EC),
    QWord($90BEFFFA23631E28), QWord($A4506CEBDE82BDE9), QWord($BEF9A3F7B2C67915), QWord($C67178F2E372532B),
    QWord($CA273ECEEA26619C), QWord($D186B8C721C0C207), QWord($EADA7DD6CDE0EB1E), QWord($F57D4F7FEE6ED178),
    QWord($06F067AA72176FBA), QWord($0A637DC5A2C898A6), QWord($113F9804BEF90DAE), QWord($1B710B35131C471B),
    QWord($28DB77F523047D84), QWord($32CAAB7B40C72493), QWord($3C9EBE0A15C9BEBC), QWord($431D67C49C100D4C),
    QWord($4CC5D4BECB3E42B6), QWord($597F299CFC657E2A), QWord($5FCB6FAB3AD6FAEC), QWord($6C44198C4A475817)
  );

{ Helper functions implementation }
class function TSHA2.SwapEndian(Value: Cardinal): Cardinal;
begin
  Result := (Value shr 24) or
            ((Value shr 8) and $0000FF00) or
            ((Value shl 8) and $00FF0000) or
            (Value shl 24);
end;

class function TSHA2.SwapEndian64(Value: QWord): QWord;
begin
  Result := (Value shr 56) or
            ((Value shr 40) and QWord($000000000000FF00)) or
            ((Value shr 24) and QWord($0000000000FF0000)) or
            ((Value shr 8)  and QWord($00000000FF000000)) or
            ((Value shl 8)  and QWord($000000FF00000000)) or
            ((Value shl 24) and QWord($0000FF0000000000)) or
            ((Value shl 40) and QWord($00FF000000000000)) or
            (Value shl 56);
end;

class function TSHA2.RightRotate(Value: Cardinal; Bits: Byte): Cardinal;
begin
  Result := (Value shr Bits) or (Value shl (32 - Bits));
end;

class function TSHA2.RightRotate64(Value: QWord; Bits: Byte): QWord;
begin
  Result := (Value shr Bits) or (Value shl (64 - Bits));
end;

class function TSHA2.Ch(x, y, z: Cardinal): Cardinal;
begin
  Result := (x and y) xor (not x and z);
end;

class function TSHA2.Ch64(x, y, z: QWord): QWord;
begin
  Result := (x and y) xor (not x and z);
end;

class function TSHA2.Maj(x, y, z: Cardinal): Cardinal;
begin
  Result := (x and y) xor (x and z) xor (y and z);
end;

class function TSHA2.Maj64(x, y, z: QWord): QWord;
begin
  Result := (x and y) xor (x and z) xor (y and z);
end;

class function TSHA2.BigSigma0(x: Cardinal): Cardinal;
begin
  Result := RightRotate(x, 2) xor RightRotate(x, 13) xor RightRotate(x, 22);
end;

class function TSHA2.BigSigma0_64(x: QWord): QWord;
begin
  Result := RightRotate64(x, 28) xor RightRotate64(x, 34) xor RightRotate64(x, 39);
end;

class function TSHA2.BigSigma1(x: Cardinal): Cardinal;
begin
  Result := RightRotate(x, 6) xor RightRotate(x, 11) xor RightRotate(x, 25);
end;

class function TSHA2.BigSigma1_64(x: QWord): QWord;
begin
  Result := RightRotate64(x, 14) xor RightRotate64(x, 18) xor RightRotate64(x, 41);
end;

class function TSHA2.SmallSigma0(x: Cardinal): Cardinal;
begin
  Result := RightRotate(x, 7) xor RightRotate(x, 18) xor (x shr 3);
end;

class function TSHA2.SmallSigma0_64(x: QWord): QWord;
begin
  Result := RightRotate64(x, 1) xor RightRotate64(x, 8) xor (x shr 7);
end;

class function TSHA2.SmallSigma1(x: Cardinal): Cardinal;
begin
  Result := RightRotate(x, 17) xor RightRotate(x, 19) xor (x shr 10);
end;

class function TSHA2.SmallSigma1_64(x: QWord): QWord;
begin
  Result := RightRotate64(x, 19) xor RightRotate64(x, 61) xor (x shr 6);
end;

{ Helper function for 128-bit addition }
class procedure TSHA2.Add128(var Sum: TUInt128; Value: QWord);
var
  PrevLo: QWord;
begin
  PrevLo := Sum.Lo;
  Sum.Lo := Sum.Lo + Value;
  // If Sum.Lo < PrevLo, it means we had an overflow
  if Sum.Lo < PrevLo then
    Inc(Sum.Hi);
end;

{ Main transform procedures }
class procedure TSHA2.SHA256Transform(var State: TSHA2_256State; const Block: TSHA2_256Block);
var
  W: array[0..63] of Cardinal;
  A, B, C, D, E, F, G, H, T1, T2: Cardinal;
  I: Integer;
  Sum: QWord; // Use QWord for intermediate calculations to prevent overflow
begin
  // Initialize working variables
  A := State[0];
  B := State[1];
  C := State[2];
  D := State[3];
  E := State[4];
  F := State[5];
  G := State[6];
  H := State[7];

  // Prepare message schedule
  for I := 0 to 15 do
    W[I] := SwapEndian(Block[I]);
  for I := 16 to 63 do
  begin
    // Use QWord for intermediate calculations
    Sum := QWord(SmallSigma1(W[I-2])) + QWord(W[I-7]) + QWord(SmallSigma0(W[I-15])) + QWord(W[I-16]);
    W[I] := Cardinal(Sum and $FFFFFFFF); // Take lower 32 bits
  end;

  // Main loop
  for I := 0 to 63 do
  begin
    // Use QWord for intermediate calculations
    Sum := QWord(H) + QWord(BigSigma1(E)) + QWord(Ch(E, F, G)) + QWord(SHA256_K[I]) + QWord(W[I]);
    T1 := Cardinal(Sum and $FFFFFFFF); // Take lower 32 bits

    Sum := QWord(BigSigma0(A)) + QWord(Maj(A, B, C));
    T2 := Cardinal(Sum and $FFFFFFFF); // Take lower 32 bits

    H := G;
    G := F;
    F := E;
    Sum := QWord(D) + QWord(T1);
    E := Cardinal(Sum and $FFFFFFFF); // Take lower 32 bits
    D := C;
    C := B;
    B := A;
    Sum := QWord(T1) + QWord(T2);
    A := Cardinal(Sum and $FFFFFFFF); // Take lower 32 bits
  end;

  // Update state
  Sum := QWord(State[0]) + QWord(A);
  State[0] := Cardinal(Sum and $FFFFFFFF);
  Sum := QWord(State[1]) + QWord(B);
  State[1] := Cardinal(Sum and $FFFFFFFF);
  Sum := QWord(State[2]) + QWord(C);
  State[2] := Cardinal(Sum and $FFFFFFFF);
  Sum := QWord(State[3]) + QWord(D);
  State[3] := Cardinal(Sum and $FFFFFFFF);
  Sum := QWord(State[4]) + QWord(E);
  State[4] := Cardinal(Sum and $FFFFFFFF);
  Sum := QWord(State[5]) + QWord(F);
  State[5] := Cardinal(Sum and $FFFFFFFF);
  Sum := QWord(State[6]) + QWord(G);
  State[6] := Cardinal(Sum and $FFFFFFFF);
  Sum := QWord(State[7]) + QWord(H);
  State[7] := Cardinal(Sum and $FFFFFFFF);
end;

class procedure TSHA2.SHA512Transform(var State: TSHA2_512State; const Block: TSHA2_512Block);
var
  W: array[0..79] of QWord;
  A, B, C, D, E, F, G, H, T1, T2: QWord;
  I: Integer;
begin
  // Initialize working variables
  A := State[0];
  B := State[1];
  C := State[2];
  D := State[3];
  E := State[4];
  F := State[5];
  G := State[6];
  H := State[7];

  // Prepare message schedule
  for I := 0 to 15 do
    W[I] := SwapEndian64(Block[I]);

  {$Q-}  // Disable overflow checking
  for I := 16 to 79 do
  begin
    W[I] := SmallSigma1_64(W[I-2]) + W[I-7] + SmallSigma0_64(W[I-15]) + W[I-16];
  end;

  // Main loop
  for I := 0 to 79 do
  begin
    T1 := H + BigSigma1_64(E) + Ch64(E, F, G) + SHA512_K[I] + W[I];
    T2 := BigSigma0_64(A) + Maj64(A, B, C);

    H := G;
    G := F;
    F := E;
    E := D + T1;
    D := C;
    C := B;
    B := A;
    A := T1 + T2;
  end;

  // Update state
  State[0] := State[0] + A;
  State[1] := State[1] + B;
  State[2] := State[2] + C;
  State[3] := State[3] + D;
  State[4] := State[4] + E;
  State[5] := State[5] + F;
  State[6] := State[6] + G;
  State[7] := State[7] + H;
  {$Q+}  // Re-enable overflow checking
end;

{ Public hash functions }
class function TSHA2.SHA256(const Data: string): string;
var
  State: TSHA2_256State;
  Block: TSHA2_256Block;
  Buffer: array of Byte;
  DataLen, PaddedLen, I: Integer;
  TotalBits: QWord;
begin
  // Initialize state
  State[0] := $6A09E667;
  State[1] := $BB67AE85;
  State[2] := $3C6EF372;
  State[3] := $A54FF53A;
  State[4] := $510E527F;
  State[5] := $9B05688C;
  State[6] := $1F83D9AB;
  State[7] := $5BE0CD19;

  DataLen := Length(Data);
  TotalBits := QWord(DataLen) * 8;

  // Calculate padded length (multiple of 64 bytes)
  PaddedLen := ((DataLen + 9 + 63) div 64) * 64;
  SetLength(Buffer, PaddedLen);

  // Copy data and add padding
  if DataLen > 0 then
    Move(Data[1], Buffer[0], DataLen);
  Buffer[DataLen] := $80;
  for I := DataLen + 1 to PaddedLen - 8 do
    Buffer[I] := 0;

  // Add length in bits as big-endian 64-bit integer
  for I := 0 to 7 do
    Buffer[PaddedLen - 8 + I] := Byte((TotalBits shr ((7 - I) * 8)) and $FF);

  // Process blocks
  for I := 0 to (PaddedLen div 64) - 1 do
  begin
    Move(Buffer[I * 64], Block, 64);
    SHA256Transform(State, Block);
  end;

  // Convert state to hexadecimal string
  Result := '';
  for I := 0 to 7 do
    Result := Result + IntToHex(State[I], 8);
end;

class function TSHA2.SHA512(const Data: string): string;
var
  State: TSHA2_512State;
  Block: TSHA2_512Block;
  Buffer: array of Byte;
  DataLen, PaddedLen, I: Integer;
  TotalBits: QWord;
begin
  // Initialize state
  State[0] := QWord($6A09E667F3BCC908);
  State[1] := QWord($BB67AE8584CAA73B);
  State[2] := QWord($3C6EF372FE94F82B);
  State[3] := QWord($A54FF53A5F1D36F1);
  State[4] := QWord($510E527FADE682D1);
  State[5] := QWord($9B05688C2B3E6C1F);
  State[6] := QWord($1F83D9ABFB41BD6B);
  State[7] := QWord($5BE0CD19137E2179);

  DataLen := Length(Data);
  TotalBits := QWord(DataLen) * 8;

  // Calculate padded length (multiple of 128 bytes)
  PaddedLen := ((DataLen + 17 + 127) div 128) * 128;
  SetLength(Buffer, PaddedLen);

  // Copy data and add padding
  if DataLen > 0 then
    Move(Data[1], Buffer[0], DataLen);
  Buffer[DataLen] := $80;
  for I := DataLen + 1 to PaddedLen - 16 do
    Buffer[I] := 0;

  // Add length in bits as big-endian 128-bit integer (high 64 bits are zero)
  for I := 0 to 7 do
  begin
    Buffer[PaddedLen - 16 + I] := 0;
    Buffer[PaddedLen - 8 + I] := Byte((TotalBits shr ((7 - I) * 8)) and $FF);
  end;

  // Process blocks
  for I := 0 to (PaddedLen div 128) - 1 do
  begin
    Move(Buffer[I * 128], Block, 128);
    SHA512Transform(State, Block);
  end;

  // Convert state to hexadecimal string
  Result := '';
  for I := 0 to 7 do
    Result := Result + IntToHex(State[I], 16);
end;

class function TSHA2.SHA512_256(const Data: string): string;
var
  State: TSHA2_512State;
  Block: TSHA2_512Block;
  Buffer: array of Byte;
  DataLen, PaddedLen, I: Integer;
  TotalBits: QWord;
begin
  // Initialize state with SHA-512/256 specific values
  State[0] := QWord($22312194FC2BF72C);
  State[1] := QWord($9F555FA3C84C64C2);
  State[2] := QWord($2393B86B6F53B151);
  State[3] := QWord($963877195940EABD);
  State[4] := QWord($96283EE2A88EFFE3);
  State[5] := QWord($BE5E1E2553863992);
  State[6] := QWord($2B0199FC2C85B8AA);
  State[7] := QWord($0EB72DDC81C52CA2);

  DataLen := Length(Data);
  TotalBits := QWord(DataLen) * 8;

  // Calculate padded length (multiple of 128 bytes)
  PaddedLen := ((DataLen + 17 + 127) div 128) * 128;
  SetLength(Buffer, PaddedLen);

  // Copy data and add padding
  if DataLen > 0 then
    Move(Data[1], Buffer[0], DataLen);
  Buffer[DataLen] := $80;
  for I := DataLen + 1 to PaddedLen - 16 do
    Buffer[I] := 0;

  // Add length in bits as big-endian 128-bit integer (high 64 bits are zero)
  for I := 0 to 7 do
  begin
    Buffer[PaddedLen - 16 + I] := 0;
    Buffer[PaddedLen - 8 + I] := Byte((TotalBits shr ((7 - I) * 8)) and $FF);
  end;

  // Process blocks
  for I := 0 to (PaddedLen div 128) - 1 do
  begin
    Move(Buffer[I * 128], Block, 128);
    SHA512Transform(State, Block);
  end;

  // Convert first 256 bits of state to hexadecimal string
  Result := '';
  for I := 0 to 3 do
    Result := Result + IntToHex(State[I], 16);
end;

end.

