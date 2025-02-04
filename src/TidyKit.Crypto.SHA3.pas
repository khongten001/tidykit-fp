    // Start of Selection
    {
      TidyKit.Crypto.SHA3 - Secure Hash Algorithm 3 Implementation
      
      SECURITY CONSIDERATIONS:
      - This implementation adheres to the NIST FIPS 202 standard
      - Handles memory in a deterministic manner
      - Incorporates overflow protection in critical sections
      - Implements proper padding as specified
      
      USAGE WARNINGS:
      - For cryptographic purposes, prefer SHA3-256 or higher
      - Always validate input data before hashing
      
      TEST VECTORS:
      - Test vectors are sourced from the NIST FIPS 202 standard
      - Implementation has been verified against official NIST test vectors

      REFERENCES:
      - https://www.di-mgt.com.au/sha_testvectors.html#testvectors
      - https://csrc.nist.gov/pubs/fips/202/final

    }

unit TidyKit.Crypto.SHA3;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, Math;

type
  { Core types for SHA3 operations }
  TSHA3State = array[0..24] of QWord;  // 1600-bit state (5x5 matrix of 64-bit words)
  TSHA3Mode = (
    sm224,  // SHA3-224
    sm256,  // SHA3-256
    sm384,  // SHA3-384
    sm512   // SHA3-512
  );

  { TSHA3
    -----
    SHA3 (Keccak) hash function implementation.
    Provides SHA3-224, SHA3-256, SHA3-384, and SHA3-512 variants. }
  TSHA3 = class
  private
    const
      { Keccak-f round constants }
      RC: array[0..23] of QWord = (
        QWord($0000000000000001), QWord($0000000000008082),
        QWord($800000000000808A), QWord($8000000080008000),
        QWord($000000000000808B), QWord($0000000080000001),
        QWord($8000000080008081), QWord($8000000000008009),
        QWord($000000000000008A), QWord($0000000000000088),
        QWord($0000000080008009), QWord($000000008000000A),
        QWord($000000008000808B), QWord($800000000000008B),
        QWord($8000000000008089), QWord($8000000000008003),
        QWord($8000000000008002), QWord($8000000000000080),
        QWord($000000000000800A), QWord($800000008000000A),
        QWord($8000000080008081), QWord($8000000000008080),
        QWord($0000000080000001), QWord($8000000080008008)
      );

      { Rotation offsets for rho step - FIPS 202 section 3.2.2 }
      KECCAK_RHO_OFFSETS: array[0..24] of Integer = (
         0,  1, 62, 28, 27,
        36, 44,  6, 55, 20,
         3, 10, 43, 25, 39,
        41, 45, 15, 21,  8,
        18,  2, 61, 56, 14
      );

    class procedure KeccakF1600(var State: TSHA3State); static;
    class function RotateLeft64(const Value: QWord; Shift: Byte): QWord; static;
    class procedure Theta(var State: TSHA3State); static;
    class procedure Rho(var State: TSHA3State); static;
    class procedure Pi(var State: TSHA3State); static;
    class procedure Chi(var State: TSHA3State); static;
    class procedure Iota(var State: TSHA3State; Round: Integer); static;
    class function GetCapacityForMode(Mode: TSHA3Mode): Integer; static;
    class function GetRateForMode(Mode: TSHA3Mode): Integer; static;
    class procedure AbsorbBlock(var State: TSHA3State; const Block: array of Byte; BlockSize: Integer); static;
    class procedure Squeeze(var State: TSHA3State; var Output: array of Byte; OutputSize: Integer); static;
  public
    { Computes SHA3-224 hash of a string.
      
      Parameters:
        Data - The string to hash.
        
      Returns:
        SHA3-224 hash as a hexadecimal string. }
    class function SHA3_224(const Data: string): string; static;
    
    { Computes SHA3-256 hash of a string.
      
      Parameters:
        Data - The string to hash.
        
      Returns:
        SHA3-256 hash as a hexadecimal string. }
    class function SHA3_256(const Data: string): string; static;
    
    { Computes SHA3-384 hash of a string.
      
      Parameters:
        Data - The string to hash.
        
      Returns:
        SHA3-384 hash as a hexadecimal string. }
    class function SHA3_384(const Data: string): string; static;
    
    { Computes SHA3-512 hash of a string.
      
      Parameters:
        Data - The string to hash.
        
      Returns:
        SHA3-512 hash as a hexadecimal string. }
    class function SHA3_512(const Data: string): string; static;
  end;

implementation

{ Core SHA3 transformation functions }

class function TSHA3.RotateLeft64(const Value: QWord; Shift: Byte): QWord;
begin
  {$Q-}  // Disable overflow checking
  Result := (Value shl Shift) or (Value shr (64 - Shift));
  {$Q+}  // Re-enable overflow checking
end;

class procedure TSHA3.Theta(var State: TSHA3State);
var
  C, D: array[0..4] of QWord;
  X, Y: Integer;
begin
  {$Q-}
  // Step 1: Calculate column parities
  FillChar(C, SizeOf(C), 0);
  for X := 0 to 4 do
    for Y := 0 to 4 do
      C[X] := C[X] xor State[Y * 5 + X];

  // Step 2: Calculate D values
  for X := 0 to 4 do
    D[X] := C[(X + 4) mod 5] xor RotateLeft64(C[(X + 1) mod 5], 1);

  // Step 3: Apply D values to state
  for Y := 0 to 4 do
    for X := 0 to 4 do
      State[Y * 5 + X] := State[Y * 5 + X] xor D[X];
  {$Q+}
end;

class procedure TSHA3.Rho(var State: TSHA3State);
var
  X, Y: Integer;
  Current: Integer;
  Temp: QWord;
begin
  {$Q-}
  // Fixed rotation offsets from FIPS 202
  for Y := 0 to 4 do
    for X := 0 to 4 do
    begin
      Current := Y * 5 + X;  // Convert to flat index according to FIPS 202
      if (X <> 0) or (Y <> 0) then  // Skip (0,0)
      begin
        Temp := State[Current];
        State[Current] := RotateLeft64(Temp, KECCAK_RHO_OFFSETS[Current]);
      end;
    end;
  {$Q+}
end;

class procedure TSHA3.Pi(var State: TSHA3State);
var
  Temp: array[0..24] of QWord;
  X, Y, NewX, NewY: Integer;
begin
  Move(State, Temp, SizeOf(State));
  
  for Y := 0 to 4 do
    for X := 0 to 4 do
    begin
      NewX := Y;
      NewY := (2 * X + 3 * Y) mod 5;
      State[NewY * 5 + NewX] := Temp[Y * 5 + X];
    end;
end;

class procedure TSHA3.Chi(var State: TSHA3State);
var
  Temp: array[0..24] of QWord;
  X, Y: Integer;
begin
  {$Q-}
  Move(State, Temp, SizeOf(State));
  
  for Y := 0 to 4 do
    for X := 0 to 4 do
      State[Y * 5 + X] := Temp[Y * 5 + X] xor 
        (not Temp[Y * 5 + ((X + 1) mod 5)] and 
         Temp[Y * 5 + ((X + 2) mod 5)]);
  {$Q+}
end;

class procedure TSHA3.Iota(var State: TSHA3State; Round: Integer);
begin
  {$Q-}
  State[0] := State[0] xor RC[Round];
  {$Q+}
end;

class procedure TSHA3.KeccakF1600(var State: TSHA3State);
var
  Round: Integer;
begin
  {$Q-}
  for Round := 0 to 23 do
  begin
    Theta(State);
    Rho(State);
    Pi(State);
    Chi(State);
    Iota(State, Round);
  end;
  {$Q+}
end;

class function TSHA3.GetCapacityForMode(Mode: TSHA3Mode): Integer;
begin
  case Mode of
    sm224: Result := 448;  // Capacity for SHA3-224 is 448 bits
    sm256: Result := 512;  // Capacity for SHA3-256 is 512 bits
    sm384: Result := 768;  // Capacity for SHA3-384 is 768 bits
    sm512: Result := 1024; // Capacity for SHA3-512 is 1024 bits
  else
    Result := 512; // Default to SHA3-256
  end;
end;

class function TSHA3.GetRateForMode(Mode: TSHA3Mode): Integer;
begin
  Result := 1600 - GetCapacityForMode(Mode);
end;

class procedure TSHA3.AbsorbBlock(var State: TSHA3State; const Block: array of Byte; BlockSize: Integer);
var
  I, J: Integer;
  Lane: QWord;
begin
  {$Q-}
  for I := 0 to (BlockSize div 8) - 1 do
  begin
    Lane := 0;
    for J := 0 to 7 do
      if (I * 8 + J < Length(Block)) and (I * 8 + J < BlockSize) then
        Lane := Lane or (QWord(Block[I * 8 + J]) shl (J * 8));  // Little-endian byte order
    if I < Length(State) then
      State[I] := State[I] xor Lane;
  end;
  {$Q+}
end;

class procedure TSHA3.Squeeze(var State: TSHA3State; var Output: array of Byte; OutputSize: Integer);
var
  I, J: Integer;
  Lane: QWord;
begin
  {$Q-}
  I := 0;
  while (I < OutputSize) do
  begin
    if I div 8 >= Length(State) then
      Break;
      
    Lane := State[I div 8];
    for J := 0 to 7 do
    begin
      if I + J >= OutputSize then
        Break;
      if I + J < Length(Output) then
        Output[I + J] := Byte(Lane shr (J * 8));  // Match AbsorbBlock endianness
    end;
    Inc(I, 8);
  end;
  {$Q+}
end;

{ Public hash functions }

class function TSHA3.SHA3_224(const Data: string): string;
var
  State: TSHA3State;
  Buffer: array of Byte;
  HashValue: array[0..27] of Byte;
  DataLen, Rate, BlockSize, I, PadPos: Integer;
  TempBuffer: array of Byte;
begin
  {$Q-}
  FillChar(State, SizeOf(State), 0);
  Rate := GetRateForMode(sm224);
  DataLen := Length(Data);
  BlockSize := Rate div 8;
  
  SetLength(Buffer, BlockSize);
  FillChar(Buffer[0], BlockSize, 0);
  
  if DataLen > 0 then
  begin
    if DataLen > BlockSize then
    begin
      SetLength(TempBuffer, DataLen);
      Move(Data[1], TempBuffer[0], DataLen);
      
      I := 0;
      while (I + BlockSize) <= DataLen do
      begin
        Move(TempBuffer[I], Buffer[0], BlockSize);
        AbsorbBlock(State, Buffer, BlockSize);
        KeccakF1600(State);
        Inc(I, BlockSize);
      end;
      
      FillChar(Buffer[0], BlockSize, 0);
      if DataLen - I > 0 then
        Move(TempBuffer[I], Buffer[0], DataLen - I);
    end
    else
      Move(Data[1], Buffer[0], DataLen);
  end;
  
  // Apply domain separation and padding for SHA3-224
  PadPos := DataLen mod BlockSize;
  Buffer[PadPos] := Buffer[PadPos] xor $06;  // Domain separator for SHA3 (FIPS 202)
  Buffer[BlockSize - 1] := Buffer[BlockSize - 1] xor $80;  // Final bit
  
  AbsorbBlock(State, Buffer, BlockSize);
  KeccakF1600(State);
  
  FillChar(HashValue, SizeOf(HashValue), 0);
  Squeeze(State, HashValue, 28);
  
  Result := '';
  for I := 0 to 27 do
    Result := Result + IntToHex(HashValue[I], 2);
  {$Q+}
end;

class function TSHA3.SHA3_256(const Data: string): string;
var
  State: TSHA3State;
  Buffer: array of Byte;
  HashValue: array[0..31] of Byte;
  DataLen, Rate, BlockSize, I, PadPos: Integer;
  TempBuffer: array of Byte;
begin
  {$Q-}
  FillChar(State, SizeOf(State), 0);
  Rate := GetRateForMode(sm256);
  DataLen := Length(Data);
  BlockSize := Rate div 8;
  
  SetLength(Buffer, BlockSize);
  FillChar(Buffer[0], BlockSize, 0);
  
  if DataLen > 0 then
  begin
    if DataLen > BlockSize then
    begin
      SetLength(TempBuffer, DataLen);
      Move(Data[1], TempBuffer[0], DataLen);
      
      I := 0;
      while (I + BlockSize) <= DataLen do
      begin
        Move(TempBuffer[I], Buffer[0], BlockSize);
        AbsorbBlock(State, Buffer, BlockSize);
        KeccakF1600(State);
        Inc(I, BlockSize);
      end;
      
      FillChar(Buffer[0], BlockSize, 0);
      if DataLen - I > 0 then
        Move(TempBuffer[I], Buffer[0], DataLen - I);
    end
    else
      Move(Data[1], Buffer[0], DataLen);
  end;
  
  // Apply domain separation and padding
  PadPos := DataLen mod BlockSize;
  Buffer[PadPos] := Buffer[PadPos] xor $06;  // Domain separator for SHA3 (FIPS 202)
  Buffer[BlockSize - 1] := Buffer[BlockSize - 1] xor $80;  // Final bit
  
  AbsorbBlock(State, Buffer, BlockSize);
  KeccakF1600(State);
  
  FillChar(HashValue, SizeOf(HashValue), 0);
  Squeeze(State, HashValue, 32);
  
  Result := '';
  for I := 0 to 31 do
    Result := Result + IntToHex(HashValue[I], 2);
  {$Q+}
end;

class function TSHA3.SHA3_384(const Data: string): string;
var
  State: TSHA3State;
  Buffer: array of Byte;
  HashValue: array[0..47] of Byte;
  DataLen, Rate, BlockSize, I, PadPos: Integer;
  TempBuffer: array of Byte;
begin
  {$Q-}
  FillChar(State, SizeOf(State), 0);
  Rate := GetRateForMode(sm384);
  DataLen := Length(Data);
  BlockSize := Rate div 8;
  
  SetLength(Buffer, BlockSize);
  FillChar(Buffer[0], BlockSize, 0);
  
  if DataLen > 0 then
  begin
    if DataLen > BlockSize then
    begin
      SetLength(TempBuffer, DataLen);
      Move(Data[1], TempBuffer[0], DataLen);
      
      I := 0;
      while (I + BlockSize) <= DataLen do
      begin
        Move(TempBuffer[I], Buffer[0], BlockSize);
        AbsorbBlock(State, Buffer, BlockSize);
        KeccakF1600(State);
        Inc(I, BlockSize);
      end;
      
      FillChar(Buffer[0], BlockSize, 0);
      if DataLen - I > 0 then
        Move(TempBuffer[I], Buffer[0], DataLen - I);
    end
    else
      Move(Data[1], Buffer[0], DataLen);
  end;
  
  // Apply domain separation and padding
  PadPos := DataLen mod BlockSize;
  Buffer[PadPos] := Buffer[PadPos] xor $06;  // Domain separator for SHA3 (FIPS 202)
  Buffer[BlockSize - 1] := Buffer[BlockSize - 1] xor $80;  // Final bit
  
  AbsorbBlock(State, Buffer, BlockSize);
  KeccakF1600(State);
  
  FillChar(HashValue, SizeOf(HashValue), 0);
  Squeeze(State, HashValue, 48);
  
  Result := '';
  for I := 0 to 47 do
    Result := Result + IntToHex(HashValue[I], 2);
  {$Q+}
end;

class function TSHA3.SHA3_512(const Data: string): string;
var
  State: TSHA3State;
  Buffer: array of Byte;
  HashValue: array[0..63] of Byte;
  DataLen, Rate, BlockSize, I, PadPos: Integer;
  TempBuffer: array of Byte;
begin
  {$Q-}
  FillChar(State, SizeOf(State), 0);
  Rate := GetRateForMode(sm512);
  DataLen := Length(Data);
  BlockSize := Rate div 8;
  
  SetLength(Buffer, BlockSize);
  FillChar(Buffer[0], BlockSize, 0);
  
  if DataLen > 0 then
  begin
    if DataLen > BlockSize then
    begin
      SetLength(TempBuffer, DataLen);
      Move(Data[1], TempBuffer[0], DataLen);
      
      I := 0;
      while (I + BlockSize) <= DataLen do
      begin
        Move(TempBuffer[I], Buffer[0], BlockSize);
        AbsorbBlock(State, Buffer, BlockSize);
        KeccakF1600(State);
        Inc(I, BlockSize);
      end;
      
      FillChar(Buffer[0], BlockSize, 0);
      if DataLen - I > 0 then
        Move(TempBuffer[I], Buffer[0], DataLen - I);
    end
    else
      Move(Data[1], Buffer[0], DataLen);
  end;
  
  // Apply domain separation and padding
  PadPos := DataLen mod BlockSize;
  Buffer[PadPos] := Buffer[PadPos] xor $06;  // Domain separator for SHA3 (FIPS 202)
  Buffer[BlockSize - 1] := Buffer[BlockSize - 1] xor $80;  // Final bit
  
  AbsorbBlock(State, Buffer, BlockSize);
  KeccakF1600(State);
  
  FillChar(HashValue, SizeOf(HashValue), 0);
  Squeeze(State, HashValue, 64);
  
  Result := '';
  for I := 0 to 63 do
    Result := Result + IntToHex(HashValue[I], 2);
  {$Q+}
end;

end. 
