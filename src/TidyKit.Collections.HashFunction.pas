unit TidyKit.Collections.HashFunction;

{$mode objfpc}{$H+}{$J-}
{$R-}
interface

uses
  Math; // For IsNan, IsInfinite

// Modern hash function declarations - add const to parameters
function XXHash32(const Key: string): Integer;
function FNV1aHash(const Key: string): Integer;
function MultiplicativeHash(const Key: Integer): Integer;
function DefaultHash(const Key; Size: Integer): Integer;

// Special case hash functions - add const to parameters
function FloatHash(const Value: Extended): Integer;
function BooleanHash(const Value: Boolean): Integer;
function DateTimeHash(const Value: TDateTime): Integer;
function CharHash(const Value: Char): Integer;
function Int64Hash(const Value: Int64): Integer;

implementation

const
  // XXHash constants
  PRIME32_1 = 2654435761;
  PRIME32_2 = 2246822519;
  PRIME32_3 = 3266489917;
  PRIME32_4 = 668265263;
  PRIME32_5 = 374761393;

  // FNV constants
  FNV_PRIME = 16777619;
  FNV_OFFSET_BASIS = 2166136261;

function XXHash32(const Key: string): Integer;
var
  I, Len: Integer;
  H32: Cardinal;
  Data: PByte;
begin
  Len := Length(Key);
  if Len = 0 then
  begin
    Result := 0;
    Exit;
  end;
  
  H32 := PRIME32_5;
  Data := @Key[1];

  // Process 4 bytes at a time
  while Len >= 4 do
  begin
    H32 := H32 + PLongWord(Data)^ * PRIME32_3;
    H32 := ((H32 shl 17) or (H32 shr 15)) * PRIME32_4;
    Inc(Data, 4);
    Dec(Len, 4);
  end;

  // Process remaining bytes
  while Len > 0 do
  begin
    H32 := H32 + Data^ * PRIME32_5;
    H32 := ((H32 shl 11) or (H32 shr 21)) * PRIME32_1;
    Inc(Data);
    Dec(Len);
  end;

  // Finalization
  H32 := H32 xor (H32 shr 15);
  H32 := H32 * PRIME32_2;
  H32 := H32 xor (H32 shr 13);
  H32 := H32 * PRIME32_3;
  H32 := H32 xor (H32 shr 16);

  // Ensure non-negative result for use with HashSet
  Result := Integer(H32 and $7FFFFFFF);
end;

function FNV1aHash(const Key: string): Integer;
var
  I: Integer;
  Hash: Cardinal;
begin
  Hash := FNV_OFFSET_BASIS;
  for I := 1 to Length(Key) do
  begin
    Hash := Hash xor Ord(Key[I]);
    Hash := Hash * FNV_PRIME;
  end;
  Result := Integer(Hash and $7FFFFFFF);
end;

function MultiplicativeHash(const Key: Integer): Integer; // Added const here
const
  GOLDEN_RATIO = 2654435769;  // 2^32 * (sqrt(5)-1)/2
var
  Hash: Cardinal;
begin
  Hash := Cardinal(Key) * GOLDEN_RATIO;
  Result := Integer(Hash and $7FFFFFFF);
end;

function DefaultHash(const Key; Size: Integer): Integer;
var
  Data: PByte;
  I: Integer;
  Hash: Cardinal;
begin
  Hash := FNV_OFFSET_BASIS;
  Data := @Key;
  
  for I := 0 to Size - 1 do
  begin
    Hash := Hash xor Data[I];
    Hash := Hash * FNV_PRIME;
  end;
  
  Result := Integer(Hash and $7FFFFFFF);
end;

function FloatHash(const Value: Extended): Integer;
var
  IsNegativeZero: Boolean;
  HashValue: Integer;
begin
  // Handle special floating-point values explicitly
  if IsNan(Value) then
    HashValue := 1
  else if Value = 0 then 
  begin
    // More reliable test for negative zero - using bit pattern check instead of division
    // This avoids division by zero exceptions
    try
      IsNegativeZero := False;
      // Check sign bit by examining memory representation
      if (PInt64(@Value)^ and $8000000000000000) <> 0 then
        IsNegativeZero := True;
    except
      // If bit manipulation fails, fall back to a safe default
      IsNegativeZero := False;
    end;
    
    if IsNegativeZero then
      HashValue := 3  // Negative zero
    else
      HashValue := 2  // Positive zero
  end
  else if IsInfinite(Value) then
    if Value > 0 then
      HashValue := 4  // +Infinity
    else
      HashValue := 5  // -Infinity
  else if (Abs(Value) < 1E-100) or (Abs(Value) > 1E100) then
    // Very small or large values - use simplified handling to avoid precision issues
    HashValue := (Ord(Value > 0) * 2) + 6  // 6 for small positive, 7 for small negative, etc.
  else
    // For normal values, use DefaultHash on the raw bytes
    HashValue := DefaultHash(Value, SizeOf(Value));
  
  Result := HashValue;
end;

function BooleanHash(const Value: Boolean): Integer;
begin
  Result := Ord(Value);
end;

function DateTimeHash(const Value: TDateTime): Integer;
var
  Days: Integer;
begin
  Days := Abs(Trunc(Value)) and $3FFFFFFF;
  if Value < 0 then
    Days := Days or $40000000;
  Result := Days;
end;

function CharHash(const Value: Char): Integer;
begin
  Result := Ord(Value);
end;

function Int64Hash(const Value: Int64): Integer;
begin
  Result := DefaultHash(Value, SizeOf(Value));
end;

end.
{$R+}