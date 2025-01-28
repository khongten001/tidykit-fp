{ TidyKit SHA256 Implementation Unit
  ================================
  
  This unit provides a pure Pascal implementation of the SHA256 hashing algorithm
  as defined by NIST FIPS 180-4. The implementation is designed to be:
  
  1. Easy to understand and maintain
  2. Memory efficient
  3. Secure according to NIST standards
  
  Usage Example:
  -------------
  var
    SHA256: TSHA256;
    Hash: TSHA256Digest;
  begin
    SHA256 := TSHA256.Create;
    try
      SHA256.Update('Hello, World!');
      SHA256.Final(Hash);
      // Use Hash...
    finally
      SHA256.Free;
    end;
  end;
  
  Or use the simpler static methods:
  
  HashStr := TSHA256.HashToString('Hello, World!');
}
unit TidyKit.Crypto.SHA256;

{$mode objfpc}{$H+}{$J-}

interface

type
  { TSHA256State - Internal state of SHA256
    Contains the eight 32-bit working variables (a,b,c,d,e,f,g,h) }
  TSHA256State = array[0..7] of Cardinal;
  
  { TSHA256Block - 512-bit message block
    Used for processing input data in 512-bit chunks }
  TSHA256Block = array[0..15] of Cardinal;

  { TSHA256Digest - 256-bit output hash
    The final hash value as a 32-byte array }
  TSHA256Digest = array[0..31] of Byte;

  { TSHA256Buffer - Internal buffer for partial blocks
    Stores incomplete blocks until we have 512 bits }
  TSHA256Buffer = array[0..63] of Byte;

  { Message schedule array type }
  TSHA256Schedule = array[0..63] of Cardinal;

  { TSHA256
    Main class implementing the SHA256 algorithm }
  TSHA256 = class
  private
    { Internal state }
    FState: TSHA256State;
    FBuffer: TSHA256Buffer;
    FBufLen: Integer;
    FTotalLen: Int64;
    
    { Message schedule array }
    FW: TSHA256Schedule;

    { Initialize state with constants H0-H7 }
    procedure InitializeState;
    
    { Process a complete 512-bit block }
    procedure ProcessBlock(const Block: TSHA256Block);
    
    { Prepare message schedule W[0..63] from current block }
    procedure PrepareSchedule(const Block: TSHA256Block);
    
    { Core SHA256 compression function }
    procedure Compress;
    
    { Helper functions as defined by NIST }
    class function Ch(x, y, z: Cardinal): Cardinal; static; inline;
    class function Maj(x, y, z: Cardinal): Cardinal; static; inline;
    class function BigSigma0(x: Cardinal): Cardinal; static; inline;
    class function BigSigma1(x: Cardinal): Cardinal; static; inline;
    class function SmallSigma0(x: Cardinal): Cardinal; static; inline;
    class function SmallSigma1(x: Cardinal): Cardinal; static; inline;
    
    { Convert Cardinal to bytes in big-endian order }
    class procedure CardinalToBigEndian(Value: Cardinal; var Bytes; StartIndex: Integer = 0); static;
    
    { Convert bytes in big-endian order to Cardinal }
    class function BigEndianToCardinal(const Bytes; StartIndex: Integer = 0): Cardinal; static;
  public
    { Constructor - initializes hash state }
    constructor Create;
    
    { Reset hash to initial state }
    procedure Reset;
    
    { Add more data to the hash }
    procedure Update(const Data; DataLen: Integer);
    procedure Update(const Text: string);
    
    { Finalize and get the hash value }
    procedure Final(var Digest: TSHA256Digest);
    
    { Static helper methods for simple hashing }
    class function HashData(const Data; DataLen: Integer): TSHA256Digest; static;
    class function HashString(const Text: string): TSHA256Digest; static;
    class function HashToString(const Text: string): string; static;
  end;

implementation

const
  { Initial hash values H[0..7] - First 32 bits of the fractional parts of the
    square roots of the first 8 primes 2..19 }
  InitialHash: TSHA256State = (
    $6A09E667, $BB67AE85, $3C6EF372, $A54FF53A,
    $510E527F, $9B05688C, $1F83D9AB, $5BE0CD19
  );

  { Round constants K[0..63] - First 32 bits of the fractional parts of the
    cube roots of the first 64 primes 2..311 }
  RoundConstants: array[0..63] of Cardinal = (
    $428A2F98, $71374491, $B5C0FBCF, $E9B5DBA5, $3956C25B, $59F111F1, $923F82A4, $AB1C5ED5,
    $D807AA98, $12835B01, $243185BE, $550C7DC3, $72BE5D74, $80DEB1FE, $9BDC06A7, $C19BF174,
    $E49B69C1, $EFBE4786, $0FC19DC6, $240CA1CC, $2DE92C6F, $4A7484AA, $5CB0A9DC, $76F988DA,
    $983E5152, $A831C66D, $B00327C8, $BF597FC7, $C6E00BF3, $D5A79147, $06CA6351, $14292967,
    $27B70A85, $2E1B2138, $4D2C6DFC, $53380D13, $650A7354, $766A0ABB, $81C2C92E, $92722C85,
    $A2BFE8A1, $A81A664B, $C24B8B70, $C76C51A3, $D192E819, $D6990624, $F40E3585, $106AA070,
    $19A4C116, $1E376C08, $2748774C, $34B0BCB5, $391C0CB3, $4ED8AA4A, $5B9CCA4F, $682E6FF3,
    $748F82EE, $78A5636F, $84C87814, $8CC70208, $90BEFFFA, $A4506CEB, $BEF9A3F7, $C67178F2
  );

{ Helper functions as defined by NIST FIPS 180-4 }

class function TSHA256.Ch(x, y, z: Cardinal): Cardinal;
begin
  Result := (x and y) xor ((not x) and z);
end;

class function TSHA256.Maj(x, y, z: Cardinal): Cardinal;
begin
  Result := (x and y) xor (x and z) xor (y and z);
end;

class function TSHA256.BigSigma0(x: Cardinal): Cardinal;
begin
  Result := RorDWord(x, 2) xor RorDWord(x, 13) xor RorDWord(x, 22);
end;

class function TSHA256.BigSigma1(x: Cardinal): Cardinal;
begin
  Result := RorDWord(x, 6) xor RorDWord(x, 11) xor RorDWord(x, 25);
end;

class function TSHA256.SmallSigma0(x: Cardinal): Cardinal;
var
  r1, r2, s: Cardinal;
begin
  r1 := RorDWord(x, 7);
  r2 := RorDWord(x, 18);
  s := x shr 3;
  Result := r1 xor r2 xor s;
end;

class function TSHA256.SmallSigma1(x: Cardinal): Cardinal;
var
  r1, r2, s: Cardinal;
begin
  r1 := RorDWord(x, 17);
  r2 := RorDWord(x, 19);
  s := x shr 10;
  Result := r1 xor r2 xor s;
end;

class procedure TSHA256.CardinalToBigEndian(Value: Cardinal; var Bytes; StartIndex: Integer = 0);
var
  PBytes: PByte;
begin
  PBytes := @Bytes;
  Inc(PBytes, StartIndex);
  PBytes^ := Value shr 24;
  Inc(PBytes);
  PBytes^ := (Value shr 16) and $FF;
  Inc(PBytes);
  PBytes^ := (Value shr 8) and $FF;
  Inc(PBytes);
  PBytes^ := Value and $FF;
end;

class function TSHA256.BigEndianToCardinal(const Bytes; StartIndex: Integer = 0): Cardinal;
var
  PBytes: PByte;
begin
  PBytes := @Bytes;
  Inc(PBytes, StartIndex);
  Result := PBytes^ shl 24;
  Inc(PBytes);
  Result := Result or (PBytes^ shl 16);
  Inc(PBytes);
  Result := Result or (PBytes^ shl 8);
  Inc(PBytes);
  Result := Result or PBytes^;
end;

constructor TSHA256.Create;
begin
  inherited Create;
  Reset;
end;

procedure TSHA256.InitializeState;
var
  i: Integer;
begin
  { Initialize hash state with constants }
  for i := 0 to 7 do
    FState[i] := InitialHash[i];
    
  { Initialize buffer and counters }
  for i := 0 to 63 do
  begin
    FBuffer[i] := 0;
    FW[i] := 0;
  end;
  
  FBufLen := 0;
  FTotalLen := 0;
end;

procedure TSHA256.Reset;
begin
  InitializeState;
end;

procedure TSHA256.PrepareSchedule(const Block: TSHA256Block);
var
  t: Integer;
  s0, s1, temp: Cardinal;
begin
  { First 16 words are the block itself }
  for t := 0 to 15 do
    FW[t] := Block[t];
  
  { Extend the first 16 words into the remaining 48 words }
  for t := 16 to 63 do
  begin
    { Calculate sigma values first }
    s0 := SmallSigma0(FW[t-15]);
    s1 := SmallSigma1(FW[t-2]);
    
    { Calculate new word using modular arithmetic }
    temp := FW[t-16];
    temp := (temp + s0) and $FFFFFFFF;
    temp := (temp + FW[t-7]) and $FFFFFFFF;
    temp := (temp + s1) and $FFFFFFFF;
    FW[t] := temp;
  end;
end;

procedure TSHA256.Compress;
var
  a, b, c, d, e, f, g, h: Cardinal;
  t: Integer;
  T1, T2, Ch_efg, Maj_abc, Sigma1_e, Sigma0_a: Cardinal;
begin
  { Initialize working variables with current hash value }
  a := FState[0];
  b := FState[1];
  c := FState[2];
  d := FState[3];
  e := FState[4];
  f := FState[5];
  g := FState[6];
  h := FState[7];
  
  { Main loop }
  for t := 0 to 63 do
  begin
    { Calculate intermediate values with proper modular arithmetic }
    Sigma1_e := BigSigma1(e);
    Ch_efg := Ch(e, f, g);
    T1 := h;
    T1 := (T1 + Sigma1_e) and $FFFFFFFF;
    T1 := (T1 + Ch_efg) and $FFFFFFFF;
    T1 := (T1 + RoundConstants[t]) and $FFFFFFFF;
    T1 := (T1 + FW[t]) and $FFFFFFFF;
    
    Sigma0_a := BigSigma0(a);
    Maj_abc := Maj(a, b, c);
    T2 := (Sigma0_a + Maj_abc) and $FFFFFFFF;
    
    h := g;
    g := f;
    f := e;
    e := (d + T1) and $FFFFFFFF;
    d := c;
    c := b;
    b := a;
    a := (T1 + T2) and $FFFFFFFF;
  end;
  
  { Update hash state }
  FState[0] := (FState[0] + a) and $FFFFFFFF;
  FState[1] := (FState[1] + b) and $FFFFFFFF;
  FState[2] := (FState[2] + c) and $FFFFFFFF;
  FState[3] := (FState[3] + d) and $FFFFFFFF;
  FState[4] := (FState[4] + e) and $FFFFFFFF;
  FState[5] := (FState[5] + f) and $FFFFFFFF;
  FState[6] := (FState[6] + g) and $FFFFFFFF;
  FState[7] := (FState[7] + h) and $FFFFFFFF;
end;

procedure TSHA256.ProcessBlock(const Block: TSHA256Block);
begin
  PrepareSchedule(Block);
  Compress;
end;

procedure TSHA256.Update(const Data; DataLen: Integer);
var
  PData: PByte;
  Block: array[0..15] of Cardinal;
  Remaining: Integer;
  i: Integer;
begin
  if DataLen <= 0 then
    Exit;
    
  PData := @Data;
  FTotalLen := FTotalLen + DataLen;
  
  { Handle remaining buffer data }
  if FBufLen > 0 then
  begin
    Remaining := 64 - FBufLen;
    if DataLen < Remaining then
    begin
      Move(PData^, FBuffer[FBufLen], DataLen);
      Inc(FBufLen, DataLen);
      Exit;
    end;
    
    { Fill buffer and process it }
    Move(PData^, FBuffer[FBufLen], Remaining);
    for i := 0 to 15 do
      Block[i] := BigEndianToCardinal(FBuffer, i * 4);
    PrepareSchedule(Block);
    Compress;
    Inc(PData, Remaining);
    Dec(DataLen, Remaining);
    FBufLen := 0;
  end;
  
  { Process full blocks }
  while DataLen >= 64 do
  begin
    for i := 0 to 15 do
      Block[i] := BigEndianToCardinal(PData^, i * 4);
    PrepareSchedule(Block);
    Compress;
    Inc(PData, 64);
    Dec(DataLen, 64);
  end;
  
  { Store remaining bytes }
  if DataLen > 0 then
  begin
    Move(PData^, FBuffer[0], DataLen);
    FBufLen := DataLen;
  end;
end;

procedure TSHA256.Update(const Text: string);
begin
  if Length(Text) > 0 then
    Update(Text[1], Length(Text));
end;

procedure TSHA256.Final(var Digest: TSHA256Digest);
var
  Block: TSHA256Block;
  PadLen: Integer;
  LenBits: Int64;
  i: Integer;
begin
  { Convert total length to bits }
  LenBits := FTotalLen * 8;
  
  { Add padding }
  PadLen := 56 - ((FTotalLen + 1) mod 64);
  if PadLen <= 0 then
    PadLen := 64 + PadLen;
    
  { Add 1 bit followed by zeros }
  FBuffer[FBufLen] := $80;
  Inc(FBufLen);
  FillChar(FBuffer[FBufLen], PadLen - 1, 0);
  Inc(FBufLen, PadLen - 1);
  
  { Add length in bits as big-endian 64-bit integer }
  CardinalToBigEndian(LenBits shr 32, FBuffer[FBufLen]);
  CardinalToBigEndian(LenBits and $FFFFFFFF, FBuffer[FBufLen + 4]);
  Inc(FBufLen, 8);
  
  { Process final block(s) }
  for i := 0 to 15 do
    Block[i] := BigEndianToCardinal(FBuffer, i * 4);
  ProcessBlock(Block);
  
  { Convert state to bytes in big-endian order }
  for i := 0 to 7 do
    CardinalToBigEndian(FState[i], Digest, i * 4);
end;

class function TSHA256.HashData(const Data; DataLen: Integer): TSHA256Digest;
var
  SHA256: TSHA256;
begin
  SHA256 := TSHA256.Create;
  try
    SHA256.Update(Data, DataLen);
    SHA256.Final(Result);
  finally
    SHA256.Free;
  end;
end;

class function TSHA256.HashString(const Text: string): TSHA256Digest;
begin
  if Length(Text) = 0 then
    Result := HashData('', 0)
  else
    Result := HashData(Text[1], Length(Text));
end;

class function TSHA256.HashToString(const Text: string): string;
const
  HexDigits: array[0..15] of Char = '0123456789ABCDEF';
var
  Digest: TSHA256Digest;
  I: Integer;
begin
  Digest := HashString(Text);
  SetLength(Result, 64);
  for I := 0 to 31 do
  begin
    Result[I * 2 + 1] := HexDigits[Digest[I] shr 4];
    Result[I * 2 + 2] := HexDigits[Digest[I] and $F];
  end;
end;

end. 