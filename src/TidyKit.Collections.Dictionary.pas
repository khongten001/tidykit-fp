unit TidyKit.Collections.Dictionary;

{$mode objfpc}{$H+}{$J-}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils, Math, TidyKit.Collections.EqualityFunction, TidyKit.Collections.HashFunction, TypInfo;

type
  { Function pointer types for key hashing and comparison }
  generic TKeyHashFunc<K> = function(const Key: K): Integer;
  generic TKeyEqualityFunc<K> = function(const A, B: K): Boolean;

  { Interface for automatic memory management }
  generic IDictionary<K, V> = interface
    ['{D764F3A2-E581-4B89-9D3C-78A5129B4D27}']
    function GetCount: Integer;
    function GetItem(const Key: K): V;
    procedure SetItem(const Key: K; const Value: V);
    
    function Add(const Key: K; const Value: V): Boolean;
    function Remove(const Key: K): Boolean;
    function ContainsKey(const Key: K): Boolean;
    function TryGetValue(const Key: K; out Value: V): Boolean;
    function GetKeys: specialize TArray<K>;
    function GetValues: specialize TArray<V>;
    procedure Clear;
    
    property Count: Integer read GetCount;
    property Items[const Key: K]: V read GetItem write SetItem; default;
  end;

  { Main dictionary implementation class }
  generic TDictionary<K, V> = class(TInterfacedObject, specialize IDictionary<K, V>)
  private
    type
      // Inner private entry type to avoid name collisions
      TEntry = record
        HashCode: Integer;
        NextEntry: Integer; // Index of next entry in FEntries for the same bucket, or next free slot
        Key: K;
        Value: V;
      end;
  private
    FBuckets: array of Integer; // Stores index of first entry in FEntries for each bucket (-1 if empty)
    FEntries: array of TEntry; // Stores all entries
    FCount: Integer;           // Number of elements in the dictionary
    FSlotsInUse: Integer;      // Number of slots used in FEntries (either for data or on free list head)
    FFreeListHead: Integer;    // Head of list of free slots in FEntries (-1 if none)
    FKeyHashFunc: specialize TKeyHashFunc<K>;
    FKeyEqualityFunc: specialize TKeyEqualityFunc<K>;
    FLoadFactor: Single;
    FVersion: Integer;         // For enumerator validation (future use)

    procedure Initialize(InitialCapacity: Integer);
    procedure Resize;
    procedure GrowEntries;
    function GetBucketIndex(HashCode: Integer): Integer;

    function GetCount: Integer;
    function GetItem(const Key: K): V;
    procedure SetItem(const Key: K; const Value: V);
    function Add(const Key: K; const Value: V): Boolean;
    function Remove(const Key: K): Boolean;
    function ContainsKey(const Key: K): Boolean;
    function TryGetValue(const Key: K; out Value: V): Boolean;
    function GetKeys: specialize TArray<K>;
    function GetValues: specialize TArray<V>;
    procedure Clear;
  public
    constructor Create(KeyHashFunc: specialize TKeyHashFunc<K>; KeyEqualityFunc: specialize TKeyEqualityFunc<K>; InitialCapacity: Integer = 16; LoadFactor: Single = 0.75);
    destructor Destroy; override;
    class function New(KeyHashFunc: specialize TKeyHashFunc<K>; KeyEqualityFunc: specialize TKeyEqualityFunc<K>; InitialCapacity: Integer = 16; LoadFactor: Single = 0.75): specialize IDictionary<K, V>; static;
  end;

  { Helper function to simplify dictionary creation with automatic memory management }
  generic function CreateDictionary<K, V>(KeyHashFunc: specialize TKeyHashFunc<K>; KeyEqualityFunc: specialize TKeyEqualityFunc<K>; InitialCapacity: Integer = 16; LoadFactor: Single = 0.75): specialize IDictionary<K, V>;

implementation

{ TDictionary<K, V> }

constructor TDictionary.Create(KeyHashFunc: specialize TKeyHashFunc<K>; KeyEqualityFunc: specialize TKeyEqualityFunc<K>; InitialCapacity: Integer; LoadFactor: Single);
begin
  inherited Create;
  if not Assigned(KeyHashFunc) then
    raise Exception.Create('KeyHashFunc cannot be nil.');
  if not Assigned(KeyEqualityFunc) then
    raise Exception.Create('KeyEqualityFunc cannot be nil.');

  Self.FKeyHashFunc := KeyHashFunc;
  Self.FKeyEqualityFunc := KeyEqualityFunc;
  Self.FLoadFactor := LoadFactor;
  if Self.FLoadFactor <= 0 then Self.FLoadFactor := 0.75; // Default if invalid
  Initialize(InitialCapacity);
end;

destructor TDictionary.Destroy;
begin
  SetLength(FBuckets, 0);
  SetLength(FEntries, 0); // This will finalize elements if K or V are managed (string, interface)
  inherited Destroy;
end;

procedure TDictionary.Initialize(InitialCapacity: Integer);
var
  Cap, I: Integer;
begin
  Cap := InitialCapacity;
  if Cap < 4 then Cap := 4; // Minimum capacity

  SetLength(FBuckets, Cap);
  for I := 0 to High(FBuckets) do
    FBuckets[I] := -1;

  SetLength(FEntries, Cap); // Initial size for entries, can grow independently
  FCount := 0;
  FSlotsInUse := 0;
  FFreeListHead := -1;
  FVersion := 0;
end;

function TDictionary.GetBucketIndex(HashCode: Integer): Integer;
begin
  Result := (HashCode and $7FFFFFFF) mod Length(FBuckets);
end;

procedure TDictionary.GrowEntries;
var
  NewSize: Integer;
begin
  NewSize := Length(FEntries) * 2;
  if NewSize = 0 then NewSize := 4; // Default initial size if FEntries was empty

  SetLength(FEntries, NewSize);
end;

procedure TDictionary.Resize;
var
  NewBucketCapacity, I: Integer;
  OldItems: array of TEntry;
  OldCount: Integer;
begin
  // Save old entries
  OldCount := FCount;
  SetLength(OldItems, OldCount);
  OldCount := 0;
  
  for I := 0 to FSlotsInUse - 1 do
  begin
    if (FEntries[I].NextEntry <> -2) then // Check if not on free list (using -2 as a marker for entries on the free list)
    begin
      OldItems[OldCount] := FEntries[I];
      Inc(OldCount);
    end;
  end;
  
  // Reset to empty state
  NewBucketCapacity := Length(FBuckets) * 2;
  if NewBucketCapacity = 0 then NewBucketCapacity := 4;
  
  SetLength(FBuckets, NewBucketCapacity);
  for I := 0 to High(FBuckets) do
    FBuckets[I] := -1;
    
  SetLength(FEntries, NewBucketCapacity);
  FCount := 0;
  FSlotsInUse := 0;
  FFreeListHead := -1;

  // Re-add all entries
  for I := 0 to OldCount - 1 do
  begin
    Self.Add(OldItems[I].Key, OldItems[I].Value); // This will use the new FBuckets and FEntries
  end;
end;

function TDictionary.Add(const Key: K; const Value: V): Boolean;
var
  HashCode, BucketIdx, EntryIdx, NewEntryIdx: Integer;
begin
  Result := False;
  HashCode := FKeyHashFunc(Key);
  BucketIdx := GetBucketIndex(HashCode);

  // Check if key already exists
  EntryIdx := FBuckets[BucketIdx];
  while EntryIdx <> -1 do
  begin
    if (FEntries[EntryIdx].HashCode = HashCode) and FKeyEqualityFunc(FEntries[EntryIdx].Key, Key) then
      Exit; // Key already exists, don't add
    EntryIdx := FEntries[EntryIdx].NextEntry;
  end;

  // Find a slot for the new entry
  if FFreeListHead <> -1 then
  begin
    NewEntryIdx := FFreeListHead;
    FFreeListHead := FEntries[NewEntryIdx].NextEntry;
  end
  else
  begin
    if FSlotsInUse = Length(FEntries) then
    begin
      GrowEntries;
    end;
    NewEntryIdx := FSlotsInUse;
    Inc(FSlotsInUse);
  end;

  // Add the new entry
  FEntries[NewEntryIdx].HashCode := HashCode;
  FEntries[NewEntryIdx].Key := Key;
  FEntries[NewEntryIdx].Value := Value;
  FEntries[NewEntryIdx].NextEntry := FBuckets[BucketIdx];
  FBuckets[BucketIdx] := NewEntryIdx;

  Inc(FCount);
  Inc(FVersion);
  Result := True;

  // Check if resizing is needed
  if (Length(FBuckets) > 0) and ((FCount / Length(FBuckets)) > FLoadFactor) then
  begin
    if Length(FBuckets) < MaxInt div 2 then
      Resize;
  end;
end;

function TDictionary.Remove(const Key: K): Boolean;
var
  HashCode, BucketIdx, EntryIdx, PrevEntryIdx: Integer;
begin
  Result := False;
  HashCode := FKeyHashFunc(Key);
  BucketIdx := GetBucketIndex(HashCode);

  EntryIdx := FBuckets[BucketIdx];
  PrevEntryIdx := -1;

  while EntryIdx <> -1 do
  begin
    if (FEntries[EntryIdx].HashCode = HashCode) and FKeyEqualityFunc(FEntries[EntryIdx].Key, Key) then
    begin
      // Remove from bucket chain
      if PrevEntryIdx = -1 then
        FBuckets[BucketIdx] := FEntries[EntryIdx].NextEntry
      else
        FEntries[PrevEntryIdx].NextEntry := FEntries[EntryIdx].NextEntry;

      // Clear entry content
      FEntries[EntryIdx].Key := Default(K);
      FEntries[EntryIdx].Value := Default(V);

      // Add to free list
      FEntries[EntryIdx].NextEntry := FFreeListHead;
      FFreeListHead := EntryIdx;

      Dec(FCount);
      Inc(FVersion);
      Result := True;
      Exit;
    end;
    PrevEntryIdx := EntryIdx;
    EntryIdx := FEntries[EntryIdx].NextEntry;
  end;
end;

function TDictionary.ContainsKey(const Key: K): Boolean;
var
  HashCode, BucketIdx, EntryIdx: Integer;
begin
  Result := False;
  HashCode := FKeyHashFunc(Key);
  BucketIdx := GetBucketIndex(HashCode);

  EntryIdx := FBuckets[BucketIdx];
  while EntryIdx <> -1 do
  begin
    if (FEntries[EntryIdx].HashCode = HashCode) and FKeyEqualityFunc(FEntries[EntryIdx].Key, Key) then
    begin
      Result := True;
      Exit;
    end;
    EntryIdx := FEntries[EntryIdx].NextEntry;
  end;
end;

function TDictionary.TryGetValue(const Key: K; out Value: V): Boolean;
var
  HashCode, BucketIdx, EntryIdx: Integer;
begin
  Result := False;
  HashCode := FKeyHashFunc(Key);
  BucketIdx := GetBucketIndex(HashCode);

  EntryIdx := FBuckets[BucketIdx];
  while EntryIdx <> -1 do
  begin
    if (FEntries[EntryIdx].HashCode = HashCode) and FKeyEqualityFunc(FEntries[EntryIdx].Key, Key) then
    begin
      Value := FEntries[EntryIdx].Value;
      Result := True;
      Exit;
    end;
    EntryIdx := FEntries[EntryIdx].NextEntry;
  end;
  
  Value := Default(V);
end;

function TDictionary.GetItem(const Key: K): V;
var
  Found: Boolean;
begin
  Found := TryGetValue(Key, Result);
  if not Found then
    raise Exception.CreateFmt('Key not found in dictionary: %s', [Key]);
end;

procedure TDictionary.SetItem(const Key: K; const Value: V);
var
  HashCode, BucketIdx, EntryIdx, NewEntryIdx: Integer;
begin
  HashCode := FKeyHashFunc(Key);
  BucketIdx := GetBucketIndex(HashCode);

  // Check if key already exists
  EntryIdx := FBuckets[BucketIdx];
  while EntryIdx <> -1 do
  begin
    if (FEntries[EntryIdx].HashCode = HashCode) and FKeyEqualityFunc(FEntries[EntryIdx].Key, Key) then
    begin
      // Update existing value
      FEntries[EntryIdx].Value := Value;
      Inc(FVersion);
      Exit;
    end;
    EntryIdx := FEntries[EntryIdx].NextEntry;
  end;

  // Key doesn't exist, add new entry
  if FFreeListHead <> -1 then
  begin
    NewEntryIdx := FFreeListHead;
    FFreeListHead := FEntries[NewEntryIdx].NextEntry;
  end
  else
  begin
    if FSlotsInUse = Length(FEntries) then
    begin
      GrowEntries;
    end;
    NewEntryIdx := FSlotsInUse;
    Inc(FSlotsInUse);
  end;

  // Add the new entry
  FEntries[NewEntryIdx].HashCode := HashCode;
  FEntries[NewEntryIdx].Key := Key;
  FEntries[NewEntryIdx].Value := Value;
  FEntries[NewEntryIdx].NextEntry := FBuckets[BucketIdx];
  FBuckets[BucketIdx] := NewEntryIdx;

  Inc(FCount);
  Inc(FVersion);

  // Check if resizing is needed
  if (Length(FBuckets) > 0) and ((FCount / Length(FBuckets)) > FLoadFactor) then
  begin
    if Length(FBuckets) < MaxInt div 2 then
      Resize;
  end;
end;

procedure TDictionary.Clear;
var
  I: Integer;
begin
  if FCount = 0 then Exit;

  if FSlotsInUse > 0 then
  begin
    for I := 0 to FSlotsInUse - 1 do
    begin
      FEntries[I].Key := Default(K);
      FEntries[I].Value := Default(V);
    end;
  end;

  for I := 0 to High(FBuckets) do
    FBuckets[I] := -1;

  FCount := 0;
  FSlotsInUse := 0;
  FFreeListHead := -1;
  Inc(FVersion);
end;

function TDictionary.GetCount: Integer;
begin
  Result := FCount;
end;

function TDictionary.GetKeys: specialize TArray<K>;
var
  KeysBuffer: array of K;
  CurrentIdx, BucketIdx, EntryIdx: Integer;
begin
  SetLength(KeysBuffer, FCount);
  CurrentIdx := 0;

  if FCount > 0 then
  begin
    for BucketIdx := 0 to High(FBuckets) do
    begin
      EntryIdx := FBuckets[BucketIdx];
      while EntryIdx <> -1 do
      begin
        if CurrentIdx < FCount then
        begin
          KeysBuffer[CurrentIdx] := FEntries[EntryIdx].Key;
          Inc(CurrentIdx);
        end;
        EntryIdx := FEntries[EntryIdx].NextEntry;
      end;
    end;
  end;

  SetLength(Result, Length(KeysBuffer));
  for CurrentIdx := 0 to High(KeysBuffer) do
  begin
    Result[CurrentIdx] := KeysBuffer[CurrentIdx];
  end;
end;

function TDictionary.GetValues: specialize TArray<V>;
var
  ValuesBuffer: array of V;
  CurrentIdx, BucketIdx, EntryIdx: Integer;
begin
  SetLength(ValuesBuffer, FCount);
  CurrentIdx := 0;

  if FCount > 0 then
  begin
    for BucketIdx := 0 to High(FBuckets) do
    begin
      EntryIdx := FBuckets[BucketIdx];
      while EntryIdx <> -1 do
      begin
        if CurrentIdx < FCount then
        begin
          ValuesBuffer[CurrentIdx] := FEntries[EntryIdx].Value;
          Inc(CurrentIdx);
        end;
        EntryIdx := FEntries[EntryIdx].NextEntry;
      end;
    end;
  end;

  SetLength(Result, Length(ValuesBuffer));
  for CurrentIdx := 0 to High(ValuesBuffer) do
  begin
    Result[CurrentIdx] := ValuesBuffer[CurrentIdx];
  end;
end;

class function TDictionary.New(KeyHashFunc: specialize TKeyHashFunc<K>; KeyEqualityFunc: specialize TKeyEqualityFunc<K>; InitialCapacity: Integer; LoadFactor: Single): specialize IDictionary<K, V>;
begin
  Result := Create(KeyHashFunc, KeyEqualityFunc, InitialCapacity, LoadFactor);
end;

generic function CreateDictionary<K, V>(KeyHashFunc: specialize TKeyHashFunc<K>; KeyEqualityFunc: specialize TKeyEqualityFunc<K>; InitialCapacity: Integer; LoadFactor: Single): specialize IDictionary<K, V>;
begin
  Result := specialize TDictionary<K, V>.Create(KeyHashFunc, KeyEqualityFunc, InitialCapacity, LoadFactor);
end;

end.
