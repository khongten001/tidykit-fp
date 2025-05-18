unit TidyKit.Collections.Dictionary.Test;

{$mode ObjFPC}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, DateUtils, StrUtils,
  TidyKit.Collections.Dictionary, TidyKit.Collections.HashFunction, TidyKit.Collections.EqualityFunction;


const
  // Benchmark sizes
  SMALL_BENCHMARK_SIZE = 10000;
  MEDIUM_BENCHMARK_SIZE = 100000;
  LARGE_BENCHMARK_SIZE = 1000000;
  
  type
  TDictionaryTest = class(TTestCase)
  private
    FStringToIntDictionary: specialize IDictionary<string, Integer>;
    FIntToStringDictionary: specialize IDictionary<Integer, string>;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Basic functionality tests
    procedure Test01_Initialization;
    procedure Test02_AddAndRetrieve;
    procedure Test03_AddDuplicate;
    procedure Test04_ContainsKey;
    procedure Test05_TryGetValue;
    procedure Test06_Remove;
    procedure Test07_Clear;
    procedure Test08_GetCount;
    
    // Element access tests
    procedure Test09_SetItem;
    procedure Test10_GetItem;
    procedure Test11_GetKeys;
    procedure Test12_GetValues;
    
    // Memory management tests
    procedure Test13_InterfaceBasedMemoryManagement;
    
    // Performance tests
    procedure Test14_ManyItems;
    
    // Benchmark tests
    procedure Test15_BenchmarkAddSmall;
    procedure Test16_BenchmarkAddMedium;
    procedure Test17_BenchmarkAddLarge;
    
    procedure Test18_BenchmarkLookupSmall;
    procedure Test19_BenchmarkLookupMedium;
    procedure Test20_BenchmarkLookupLarge;
    
    procedure Test21_BenchmarkRemoveSmall;
    procedure Test22_BenchmarkRemoveMedium;
    
    procedure Test23_BenchmarkMixedOperationsSmall;
    procedure Test24_BenchmarkMixedOperationsMedium;
  end;

implementation

procedure TDictionaryTest.SetUp;
begin
  // Create a dictionary with string keys and integer values
  FStringToIntDictionary := specialize CreateDictionary<string, Integer>(@FNV1aHash, @TidyKitStringEquals);
  
  // Create a dictionary with integer keys and string values
  FIntToStringDictionary := specialize CreateDictionary<Integer, string>(@MultiplicativeHash, @TidyKitIntegerEquals);
end;

procedure TDictionaryTest.TearDown;
begin
  FStringToIntDictionary := nil;
  FIntToStringDictionary := nil;
end;

procedure TDictionaryTest.Test01_Initialization;
begin
  // Test initialization with proper state
  AssertEquals('New dictionary should be empty', 0, FStringToIntDictionary.Count);
  AssertEquals('New dictionary should be empty', 0, FIntToStringDictionary.Count);
  
  // Verify ContainsKey works on empty dictionary
  AssertFalse('Empty dictionary should not contain any keys', FStringToIntDictionary.ContainsKey('test'));
  AssertFalse('Empty dictionary should not contain any keys', FIntToStringDictionary.ContainsKey(42));
end;

procedure TDictionaryTest.Test02_AddAndRetrieve;
begin
  // Adding to String-Int dictionary
  AssertTrue('Add should succeed with new key', FStringToIntDictionary.Add('one', 1));
  AssertTrue('Add should succeed with new key', FStringToIntDictionary.Add('two', 2));
  AssertTrue('Add should succeed with new key', FStringToIntDictionary.Add('three', 3));
  
  // Retrieving from String-Int dictionary
  AssertEquals('Retrieved value should match added value', 1, FStringToIntDictionary['one']);
  AssertEquals('Retrieved value should match added value', 2, FStringToIntDictionary['two']);
  AssertEquals('Retrieved value should match added value', 3, FStringToIntDictionary['three']);
  
  // Adding to Int-String dictionary
  AssertTrue('Add should succeed with new key', FIntToStringDictionary.Add(1, 'one'));
  AssertTrue('Add should succeed with new key', FIntToStringDictionary.Add(2, 'two'));
  AssertTrue('Add should succeed with new key', FIntToStringDictionary.Add(3, 'three'));
  
  // Retrieving from Int-String dictionary
  AssertEquals('Retrieved value should match added value', 'one', FIntToStringDictionary[1]);
  AssertEquals('Retrieved value should match added value', 'two', FIntToStringDictionary[2]);
  AssertEquals('Retrieved value should match added value', 'three', FIntToStringDictionary[3]);
end;

procedure TDictionaryTest.Test03_AddDuplicate;
begin
  // Test adding duplicate keys
  FStringToIntDictionary.Add('key', 1);
  AssertFalse('Adding duplicate key should return False', FStringToIntDictionary.Add('key', 2));
  
  // Verify the value wasn't changed
  AssertEquals('Value should not change when adding duplicate', 1, FStringToIntDictionary['key']);
  
  // Same test with Int-String dictionary
  FIntToStringDictionary.Add(1, 'one');
  AssertFalse('Adding duplicate key should return False', FIntToStringDictionary.Add(1, 'ONE'));
  
  // Verify the value wasn't changed
  AssertEquals('Value should not change when adding duplicate', 'one', FIntToStringDictionary[1]);
end;

procedure TDictionaryTest.Test04_ContainsKey;
begin
  // Test ContainsKey
  FStringToIntDictionary.Add('existing', 123);
  
  AssertTrue('Dictionary should contain added key', FStringToIntDictionary.ContainsKey('existing'));
  AssertFalse('Dictionary should not contain non-added key', FStringToIntDictionary.ContainsKey('nonexistent'));
  
  // Case sensitivity test
  AssertFalse('Dictionary should be case sensitive', FStringToIntDictionary.ContainsKey('EXISTING'));
  
  // Test with integer keys
  FIntToStringDictionary.Add(42, 'forty-two');
  
  AssertTrue('Dictionary should contain added key', FIntToStringDictionary.ContainsKey(42));
  AssertFalse('Dictionary should not contain non-added key', FIntToStringDictionary.ContainsKey(24));
end;

procedure TDictionaryTest.Test05_TryGetValue;
var
  Value: Integer;
  StrValue: string;
begin
  // Test TryGetValue
  FStringToIntDictionary.Add('key', 999);
  
  // Successful retrieval
  AssertTrue('TryGetValue should return True for existing key', 
    FStringToIntDictionary.TryGetValue('key', Value));
  AssertEquals('TryGetValue should return correct value', 999, Value);
  
  // Failed retrieval
  Value := -1; // Set to a non-default value to ensure it gets reset
  AssertFalse('TryGetValue should return False for non-existent key',
    FStringToIntDictionary.TryGetValue('nonexistent', Value));
  AssertEquals('TryGetValue should set Value to default for non-existent key', 0, Value);
  
  // Test with integer keys
  FIntToStringDictionary.Add(123, 'one-two-three');
  
  // Successful retrieval
  AssertTrue('TryGetValue should return True for existing key',
    FIntToStringDictionary.TryGetValue(123, StrValue));
  AssertEquals('TryGetValue should return correct value', 'one-two-three', StrValue);
  
  // Failed retrieval
  StrValue := 'not-empty'; // Set to a non-default value
  AssertFalse('TryGetValue should return False for non-existent key',
    FIntToStringDictionary.TryGetValue(456, StrValue));
  AssertEquals('TryGetValue should set Value to default for non-existent key', '', StrValue);
end;

procedure TDictionaryTest.Test06_Remove;
begin
  // Add some items
  FStringToIntDictionary.Add('one', 1);
  FStringToIntDictionary.Add('two', 2);
  FStringToIntDictionary.Add('three', 3);
  
  // Remove an existing item
  AssertTrue('Remove should return True for existing key', FStringToIntDictionary.Remove('two'));
  AssertEquals('Count should decrease after remove', 2, FStringToIntDictionary.Count);
  AssertFalse('Removed key should no longer exist', FStringToIntDictionary.ContainsKey('two'));
  
  // Remove a non-existing item
  AssertFalse('Remove should return False for non-existing key', FStringToIntDictionary.Remove('nonexistent'));
  AssertEquals('Count should not change after removing non-existing key', 2, FStringToIntDictionary.Count);
  
  // Remove the remaining items
  AssertTrue('Remove should return True for existing key', FStringToIntDictionary.Remove('one'));
  AssertTrue('Remove should return True for existing key', FStringToIntDictionary.Remove('three'));
  AssertEquals('Dictionary should be empty after removing all items', 0, FStringToIntDictionary.Count);
end;

procedure TDictionaryTest.Test07_Clear;
begin
  // Add some items
  FStringToIntDictionary.Add('one', 1);
  FStringToIntDictionary.Add('two', 2);
  FStringToIntDictionary.Add('three', 3);
  
  // Clear the dictionary
  FStringToIntDictionary.Clear;
  
  // Verify the dictionary is empty
  AssertEquals('Dictionary should be empty after Clear', 0, FStringToIntDictionary.Count);
  AssertFalse('Dictionary should not contain keys after Clear', FStringToIntDictionary.ContainsKey('one'));
  AssertFalse('Dictionary should not contain keys after Clear', FStringToIntDictionary.ContainsKey('two'));
  AssertFalse('Dictionary should not contain keys after Clear', FStringToIntDictionary.ContainsKey('three'));
end;

procedure TDictionaryTest.Test08_GetCount;
begin
  // Test Count property
  AssertEquals('New dictionary Count should be 0', 0, FStringToIntDictionary.Count);
  
  // Add items
  FStringToIntDictionary.Add('one', 1);
  AssertEquals('Count should be 1 after adding one item', 1, FStringToIntDictionary.Count);
  
  FStringToIntDictionary.Add('two', 2);
  AssertEquals('Count should be 2 after adding two items', 2, FStringToIntDictionary.Count);
  
  // Remove an item
  FStringToIntDictionary.Remove('one');
  AssertEquals('Count should be 1 after removing an item', 1, FStringToIntDictionary.Count);
  
  // Clear the dictionary
  FStringToIntDictionary.Clear;
  AssertEquals('Count should be 0 after clearing', 0, FStringToIntDictionary.Count);
end;

procedure TDictionaryTest.Test09_SetItem;
begin
  // Test indexed setter
  
  // Set new value
  FStringToIntDictionary['key'] := 100;
  AssertEquals('Dictionary should contain set value', 100, FStringToIntDictionary['key']);
  AssertEquals('Count should be 1 after setting new key', 1, FStringToIntDictionary.Count);
  
  // Update existing value
  FStringToIntDictionary['key'] := 200;
  AssertEquals('Dictionary should contain updated value', 200, FStringToIntDictionary['key']);
  AssertEquals('Count should still be 1 after updating existing key', 1, FStringToIntDictionary.Count);
  
  // Test with integer keys
  FIntToStringDictionary[1] := 'one';
  AssertEquals('Dictionary should contain set value', 'one', FIntToStringDictionary[1]);
  
  FIntToStringDictionary[1] := 'ONE';
  AssertEquals('Dictionary should contain updated value', 'ONE', FIntToStringDictionary[1]);
end;

procedure TDictionaryTest.Test10_GetItem;
begin
  // Test indexed getter
  
  // Test existing key
  FStringToIntDictionary.Add('key', 42);
  AssertEquals('Dictionary should return correct value for existing key', 42, FStringToIntDictionary['key']);
  
  // Test exception when key doesn't exist
  try
    FStringToIntDictionary['nonexistent'];
    Fail('Accessing non-existent key should raise an exception');
  except
    on E: Exception do
      // Expected exception
      AssertTrue('Exception message should mention key not found', Pos('not found', E.Message) > 0);
  end;
end;

procedure TDictionaryTest.Test11_GetKeys;
var
  Keys: specialize TArray<string>;
  ContainsKey: Boolean;
  I: Integer;
begin
  // Add some items
  FStringToIntDictionary.Add('one', 1);
  FStringToIntDictionary.Add('two', 2);
  FStringToIntDictionary.Add('three', 3);
  
  // Get keys
  Keys := FStringToIntDictionary.GetKeys;
  
  // Verify key count
  AssertEquals('Keys array length should match Count', 3, Length(Keys));
  
  // Verify all expected keys are present
  // Since order is not guaranteed, we need to check for presence of each key
  ContainsKey := False;
  for I := 0 to Length(Keys) - 1 do
    if Keys[I] = 'one' then
      ContainsKey := True;
  AssertTrue('Keys should contain "one"', ContainsKey);
  
  ContainsKey := False;
  for I := 0 to Length(Keys) - 1 do
    if Keys[I] = 'two' then
      ContainsKey := True;
  AssertTrue('Keys should contain "two"', ContainsKey);
  
  ContainsKey := False;
  for I := 0 to Length(Keys) - 1 do
    if Keys[I] = 'three' then
      ContainsKey := True;
  AssertTrue('Keys should contain "three"', ContainsKey);
end;

procedure TDictionaryTest.Test12_GetValues;
var
  Values: specialize TArray<Integer>;
  ContainsValue: Boolean;
  I: Integer;
begin
  // Add some items
  FStringToIntDictionary.Add('one', 1);
  FStringToIntDictionary.Add('two', 2);
  FStringToIntDictionary.Add('three', 3);
  
  // Get values
  Values := FStringToIntDictionary.GetValues;
  
  // Verify value count
  AssertEquals('Values array length should match Count', 3, Length(Values));
  
  // Verify all expected values are present
  // Since order is not guaranteed, we need to check for presence of each value
  ContainsValue := False;
  for I := 0 to Length(Values) - 1 do
    if Values[I] = 1 then
      ContainsValue := True;
  AssertTrue('Values should contain 1', ContainsValue);
  
  ContainsValue := False;
  for I := 0 to Length(Values) - 1 do
    if Values[I] = 2 then
      ContainsValue := True;
  AssertTrue('Values should contain 2', ContainsValue);
  
  ContainsValue := False;
  for I := 0 to Length(Values) - 1 do
    if Values[I] = 3 then
      ContainsValue := True;
  AssertTrue('Values should contain 3', ContainsValue);
end;

procedure TDictionaryTest.Test13_InterfaceBasedMemoryManagement;
var
  Dict1, Dict2: specialize IDictionary<string, Integer>;
begin
  // Create dictionary using interface-based approach
  Dict1 := specialize CreateDictionary<string, Integer>(@FNV1aHash, @TidyKitStringEquals);
  
  // Test basic operations
  Dict1.Add('one', 1);
  Dict1.Add('two', 2);
  
  AssertEquals('Interface-based dictionary should have proper count', 2, Dict1.Count);
  AssertEquals('Retrieved value should match added value', 1, Dict1['one']);
  AssertEquals('Retrieved value should match added value', 2, Dict1['two']);
  
  // Create another reference to same dictionary
  Dict2 := Dict1;
  
  // Modify through the second reference
  Dict2.Add('three', 3);
  Dict2['one'] := 100; // Update existing value
  
  // Both references should see the changes
  AssertEquals('Dict1 count after changes via Dict2', 3, Dict1.Count);
  AssertEquals('Dict1 should see updated value', 100, Dict1['one']);
  AssertTrue('Dict1 should see new key', Dict1.ContainsKey('three'));
  
  // Let one reference go out of scope - should not cause issues
  Dict2 := nil;
  
  // Original reference should still work
  AssertEquals('Original reference should still be valid', 3, Dict1.Count);
  
  // Add a new item
  Dict1.Add('four', 4);
  AssertEquals('Should be able to add items after second ref is nil', 4, Dict1.Count);
  
  // No explicit Free needed - memory will be released when Dict1 goes out of scope
end;

procedure TDictionaryTest.Test14_ManyItems;
var
  I: Integer;
  Key: string;
  StartTime, EndTime: TDateTime;
  ElapsedMilliseconds: Integer;
const
  ITEM_COUNT = 10000; // Increased item count for performance test
  MAX_ACCEPTABLE_MS = 5000; // Maximum acceptable time in milliseconds
begin
  StartTime := Now;
  
  // Add many items to test performance
  for I := 1 to ITEM_COUNT do
  begin
    Key := 'key' + IntToStr(I);
    FStringToIntDictionary.Add(Key, I);
  end;
  
  // Verify count
  AssertEquals('Dictionary should contain all added items', ITEM_COUNT, FStringToIntDictionary.Count);
  
  // Verify access to each item
  for I := 1 to ITEM_COUNT do
  begin
    Key := 'key' + IntToStr(I);
    AssertEquals('Value should match for key: ' + Key, I, FStringToIntDictionary[Key]);
  end;
  
  // Verify contains for each item
  for I := 1 to ITEM_COUNT do
  begin
    Key := 'key' + IntToStr(I);
    AssertTrue('Dictionary should contain key: ' + Key, FStringToIntDictionary.ContainsKey(Key));
  end;
  
  EndTime := Now;
  ElapsedMilliseconds := Round((EndTime - StartTime) * 24 * 60 * 60 * 1000);
  
  // Print elapsed time - useful for debugging performance issues
  // WriteLn('Performance test completed in ', ElapsedMilliseconds, ' ms');
  
  // Ensure performance is reasonable
  // Note: This may need adjustment based on the actual machine running the tests
  AssertTrue('Performance test should complete in less than ' + IntToStr(MAX_ACCEPTABLE_MS) + ' ms', 
    ElapsedMilliseconds < MAX_ACCEPTABLE_MS);
end;

procedure TDictionaryTest.Test15_BenchmarkAddSmall;
var
  StartTime, EndTime: TDateTime;
  ElapsedMS: Int64;
  I: Integer;
  Key: string;
  Dict: specialize IDictionary<string, Integer>;
  ItemsPerSecond: Double;
begin
  Dict := specialize CreateDictionary<string, Integer>(@FNV1aHash, @TidyKitStringEquals);
  
  StartTime := Now;
  
  for I := 1 to SMALL_BENCHMARK_SIZE do
  begin
    Key := 'key' + IntToStr(I);
    Dict.Add(Key, I);
  end;
    
  EndTime := Now;
  ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
  
  // Avoid division by zero
  if ElapsedMS > 0 then
    ItemsPerSecond := SMALL_BENCHMARK_SIZE / (ElapsedMS / 1000)
  else
    ItemsPerSecond := 0;
    
  WriteLn('BenchmarkAddSmall: ', SMALL_BENCHMARK_SIZE, 
          ' items in ', ElapsedMS, ' ms', 
          IfThen(ElapsedMS > 0, Format(' (%.2f items/sec)', [ItemsPerSecond]), ''));
  
  AssertEquals('Count should match inserted items', SMALL_BENCHMARK_SIZE, Dict.Count);
end;

procedure TDictionaryTest.Test16_BenchmarkAddMedium;
var
  StartTime, EndTime: TDateTime;
  ElapsedMS: Int64;
  I: Integer;
  Key: string;
  Dict: specialize IDictionary<string, Integer>;
  ItemsPerSecond: Double;
begin
  Dict := specialize CreateDictionary<string, Integer>(@FNV1aHash, @TidyKitStringEquals);
  
  StartTime := Now;
  
  for I := 1 to MEDIUM_BENCHMARK_SIZE do
  begin
    Key := 'key' + IntToStr(I);
    Dict.Add(Key, I);
  end;
    
  EndTime := Now;
  ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
  
  // Avoid division by zero
  if ElapsedMS > 0 then
    ItemsPerSecond := MEDIUM_BENCHMARK_SIZE / (ElapsedMS / 1000)
  else
    ItemsPerSecond := 0;
    
  WriteLn('BenchmarkAddMedium: ', MEDIUM_BENCHMARK_SIZE, 
          ' items in ', ElapsedMS, ' ms', 
          IfThen(ElapsedMS > 0, Format(' (%.2f items/sec)', [ItemsPerSecond]), ''));
  
  AssertEquals('Count should match inserted items', MEDIUM_BENCHMARK_SIZE, Dict.Count);
end;

procedure TDictionaryTest.Test17_BenchmarkAddLarge;
var
  StartTime, EndTime: TDateTime;
  ElapsedMS: Int64;
  I: Integer;
  Key: string;
  Dict: specialize IDictionary<string, Integer>;
  ItemsPerSecond: Double;
begin
  Dict := specialize CreateDictionary<string, Integer>(@FNV1aHash, @TidyKitStringEquals);
  
  StartTime := Now;
  
  for I := 1 to LARGE_BENCHMARK_SIZE do
  begin
    Key := 'key' + IntToStr(I);
    Dict.Add(Key, I);
  end;
    
  EndTime := Now;
  ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
  
  // Avoid division by zero
  if ElapsedMS > 0 then
    ItemsPerSecond := LARGE_BENCHMARK_SIZE / (ElapsedMS / 1000)
  else
    ItemsPerSecond := 0;
    
  WriteLn('BenchmarkAddLarge: ', LARGE_BENCHMARK_SIZE, 
          ' items in ', ElapsedMS, ' ms', 
          IfThen(ElapsedMS > 0, Format(' (%.2f items/sec)', [ItemsPerSecond]), ''));
  
  AssertEquals('Count should match inserted items', LARGE_BENCHMARK_SIZE, Dict.Count);
end;

procedure TDictionaryTest.Test18_BenchmarkLookupSmall;
var
  StartTime, EndTime: TDateTime;
  ElapsedMS: Int64;
  I, FoundCount: Integer;
  Key: string;
  Dict: specialize IDictionary<string, Integer>;
  Value: Integer;
  Found: Boolean;
  OpsPerSecond: Double;
begin
  Dict := specialize CreateDictionary<string, Integer>(@FNV1aHash, @TidyKitStringEquals);
  
  // First populate the dictionary
  for I := 1 to SMALL_BENCHMARK_SIZE do
  begin
    Key := 'key' + IntToStr(I);
    Dict.Add(Key, I);
  end;
  
  // Now benchmark lookups
  StartTime := Now;
  FoundCount := 0;
  
  // Lookup all existing items
  for I := 1 to SMALL_BENCHMARK_SIZE do
  begin
    Key := 'key' + IntToStr(I);
    Found := Dict.TryGetValue(Key, Value);
    if Found and (Value = I) then
      Inc(FoundCount);
  end;
    
  EndTime := Now;
  ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
  
  // Avoid division by zero
  if ElapsedMS > 0 then
    OpsPerSecond := SMALL_BENCHMARK_SIZE / (ElapsedMS / 1000)
  else
    OpsPerSecond := 0;
    
  WriteLn('BenchmarkLookupSmall: ', SMALL_BENCHMARK_SIZE, 
          ' lookups in ', ElapsedMS, ' ms', 
          IfThen(ElapsedMS > 0, Format(' (%.2f lookups/sec)', [OpsPerSecond]), ''));
  
  AssertEquals('All lookups should be successful', SMALL_BENCHMARK_SIZE, FoundCount);
end;

procedure TDictionaryTest.Test19_BenchmarkLookupMedium;
var
  StartTime, EndTime: TDateTime;
  ElapsedMS: Int64;
  I, FoundCount: Integer;
  Key: string;
  Dict: specialize IDictionary<string, Integer>;
  Value: Integer;
  Found: Boolean;
  OpsPerSecond: Double;
begin
  Dict := specialize CreateDictionary<string, Integer>(@FNV1aHash, @TidyKitStringEquals);
  
  // First populate the dictionary
  for I := 1 to MEDIUM_BENCHMARK_SIZE do
  begin
    Key := 'key' + IntToStr(I);
    Dict.Add(Key, I);
  end;
  
  // Now benchmark lookups
  StartTime := Now;
  FoundCount := 0;
  
  // Lookup all existing items
  for I := 1 to MEDIUM_BENCHMARK_SIZE do
  begin
    Key := 'key' + IntToStr(I);
    Found := Dict.TryGetValue(Key, Value);
    if Found and (Value = I) then
      Inc(FoundCount);
  end;
    
  EndTime := Now;
  ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
  
  // Avoid division by zero
  if ElapsedMS > 0 then
    OpsPerSecond := MEDIUM_BENCHMARK_SIZE / (ElapsedMS / 1000)
  else
    OpsPerSecond := 0;
    
  WriteLn('BenchmarkLookupMedium: ', MEDIUM_BENCHMARK_SIZE, 
          ' lookups in ', ElapsedMS, ' ms', 
          IfThen(ElapsedMS > 0, Format(' (%.2f lookups/sec)', [OpsPerSecond]), ''));
  
  AssertEquals('All lookups should be successful', MEDIUM_BENCHMARK_SIZE, FoundCount);
end;

procedure TDictionaryTest.Test20_BenchmarkLookupLarge;
var
  StartTime, EndTime: TDateTime;
  ElapsedMS: Int64;
  I, FoundCount: Integer;
  Key: string;
  Dict: specialize IDictionary<string, Integer>;
  Value: Integer;
  Found: Boolean;
  OpsPerSecond: Double;
begin
  Dict := specialize CreateDictionary<string, Integer>(@FNV1aHash, @TidyKitStringEquals);
  
  // First populate the dictionary
  for I := 1 to LARGE_BENCHMARK_SIZE do
  begin
    Key := 'key' + IntToStr(I);
    Dict.Add(Key, I);
  end;
  
  // Now benchmark lookups
  StartTime := Now;
  FoundCount := 0;
  
  // Lookup a sample of items (all would take too long)
  for I := 1 to LARGE_BENCHMARK_SIZE do
  begin
    Key := 'key' + IntToStr(I);
    Found := Dict.TryGetValue(Key, Value);
    if Found and (Value = I) then
      Inc(FoundCount);
  end;
    
  EndTime := Now;
  ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
  
  // Avoid division by zero
  if ElapsedMS > 0 then
    OpsPerSecond := LARGE_BENCHMARK_SIZE / (ElapsedMS / 1000)
  else
    OpsPerSecond := 0;
    
  WriteLn('BenchmarkLookupLarge: ', LARGE_BENCHMARK_SIZE, 
          ' lookups in ', ElapsedMS, ' ms', 
          IfThen(ElapsedMS > 0, Format(' (%.2f lookups/sec)', [OpsPerSecond]), ''));
  
  AssertEquals('All lookups should be successful', LARGE_BENCHMARK_SIZE, FoundCount);
end;

procedure TDictionaryTest.Test21_BenchmarkRemoveSmall;
var
  StartTime, EndTime: TDateTime;
  ElapsedMS: Int64;
  I, RemovedCount: Integer;
  Key: string;
  Dict: specialize IDictionary<string, Integer>;
  OpsPerSecond: Double;
begin
  Dict := specialize CreateDictionary<string, Integer>(@FNV1aHash, @TidyKitStringEquals);
  
  // First populate the dictionary
  for I := 1 to SMALL_BENCHMARK_SIZE do
  begin
    Key := 'key' + IntToStr(I);
    Dict.Add(Key, I);
  end;
  
  // Now benchmark removals
  StartTime := Now;
  RemovedCount := 0;
  
  // Remove all items
  for I := 1 to SMALL_BENCHMARK_SIZE do
  begin
    Key := 'key' + IntToStr(I);
    if Dict.Remove(Key) then
      Inc(RemovedCount);
  end;
    
  EndTime := Now;
  ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
  
  // Avoid division by zero
  if ElapsedMS > 0 then
    OpsPerSecond := SMALL_BENCHMARK_SIZE / (ElapsedMS / 1000)
  else
    OpsPerSecond := 0;
    
  WriteLn('BenchmarkRemoveSmall: ', SMALL_BENCHMARK_SIZE, 
          ' removals in ', ElapsedMS, ' ms', 
          IfThen(ElapsedMS > 0, Format(' (%.2f removals/sec)', [OpsPerSecond]), ''));
  
  AssertEquals('All removals should be successful', SMALL_BENCHMARK_SIZE, RemovedCount);
  AssertEquals('Dictionary should be empty after all removals', 0, Dict.Count);
end;

procedure TDictionaryTest.Test22_BenchmarkRemoveMedium;
var
  StartTime, EndTime: TDateTime;
  ElapsedMS: Int64;
  I, RemovedCount: Integer;
  Key: string;
  Dict: specialize IDictionary<string, Integer>;
  OpsPerSecond: Double;
begin
  Dict := specialize CreateDictionary<string, Integer>(@FNV1aHash, @TidyKitStringEquals);
  
  // First populate the dictionary
  for I := 1 to MEDIUM_BENCHMARK_SIZE do
  begin
    Key := 'key' + IntToStr(I);
    Dict.Add(Key, I);
  end;
  
  // Now benchmark removals
  StartTime := Now;
  RemovedCount := 0;
  
  // Remove all items
  for I := 1 to MEDIUM_BENCHMARK_SIZE do
  begin
    Key := 'key' + IntToStr(I);
    if Dict.Remove(Key) then
      Inc(RemovedCount);
  end;
    
  EndTime := Now;
  ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
  
  // Avoid division by zero
  if ElapsedMS > 0 then
    OpsPerSecond := MEDIUM_BENCHMARK_SIZE / (ElapsedMS / 1000)
  else
    OpsPerSecond := 0;
    
  WriteLn('BenchmarkRemoveMedium: ', MEDIUM_BENCHMARK_SIZE, 
          ' removals in ', ElapsedMS, ' ms', 
          IfThen(ElapsedMS > 0, Format(' (%.2f removals/sec)', [OpsPerSecond]), ''));
  
  AssertEquals('All removals should be successful', MEDIUM_BENCHMARK_SIZE, RemovedCount);
  AssertEquals('Dictionary should be empty after all removals', 0, Dict.Count);
end;

procedure TDictionaryTest.Test23_BenchmarkMixedOperationsSmall;
var
  StartTime, EndTime: TDateTime;
  ElapsedMS: Int64;
  I, Value: Integer;
  Key: string;
  Dict: specialize IDictionary<string, Integer>;
  OpsPerSecond: Double;
begin
  Dict := specialize CreateDictionary<string, Integer>(@FNV1aHash, @TidyKitStringEquals);
  
  StartTime := Now;
  
  // Mix of add/remove/lookup operations
  for I := 1 to SMALL_BENCHMARK_SIZE do
  begin
    // Add new item
    Key := 'key' + IntToStr(I);
    Dict.Add(Key, I);
    
    // Lookup an existing item (the one we just added)
    Dict.TryGetValue(Key, Value);
    
    // After every 10 items, remove one
    if (I > 10) and (I mod 10 = 0) then
    begin
      Key := 'key' + IntToStr(I - 10);
      Dict.Remove(Key);
    end;
  end;
    
  EndTime := Now;
  ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
  
  // Calculate total operations (add + lookup + some removes)
  Value := SMALL_BENCHMARK_SIZE * 2 + (SMALL_BENCHMARK_SIZE div 10);
  
  // Avoid division by zero
  if ElapsedMS > 0 then
    OpsPerSecond := Value / (ElapsedMS / 1000)
  else
    OpsPerSecond := 0;
    
  WriteLn('BenchmarkMixedOperationsSmall: ', Value, 
          ' operations in ', ElapsedMS, ' ms', 
          IfThen(ElapsedMS > 0, Format(' (%.2f ops/sec)', [OpsPerSecond]), ''));
  
  // Expected count is total adds minus total removes
  AssertEquals('Dictionary count should match expected', 
               SMALL_BENCHMARK_SIZE - (SMALL_BENCHMARK_SIZE div 10) + 1, Dict.Count);
end;

procedure TDictionaryTest.Test24_BenchmarkMixedOperationsMedium;
var
  StartTime, EndTime: TDateTime;
  ElapsedMS: Int64;
  I, Value: Integer;
  Key: string;
  Dict: specialize IDictionary<string, Integer>;
  OpsPerSecond: Double;
begin
  Dict := specialize CreateDictionary<string, Integer>(@FNV1aHash, @TidyKitStringEquals);
  
  StartTime := Now;
  
  // Mix of add/remove/lookup operations
  for I := 1 to MEDIUM_BENCHMARK_SIZE do
  begin
    // Add new item
    Key := 'key' + IntToStr(I);
    Dict.Add(Key, I);
    
    // Lookup an existing item (the one we just added)
    Dict.TryGetValue(Key, Value);
    
    // After every 10 items, remove one
    if (I > 10) and (I mod 10 = 0) then
    begin
      Key := 'key' + IntToStr(I - 10);
      Dict.Remove(Key);
    end;
  end;
    
  EndTime := Now;
  ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
  
  // Calculate total operations (add + lookup + some removes)
  Value := MEDIUM_BENCHMARK_SIZE * 2 + (MEDIUM_BENCHMARK_SIZE div 10);
  
  // Avoid division by zero
  if ElapsedMS > 0 then
    OpsPerSecond := Value / (ElapsedMS / 1000)
  else
    OpsPerSecond := 0;
    
  WriteLn('BenchmarkMixedOperationsMedium: ', Value, 
          ' operations in ', ElapsedMS, ' ms', 
          IfThen(ElapsedMS > 0, Format(' (%.2f ops/sec)', [OpsPerSecond]), ''));
  
  // Expected count is total adds minus total removes
  AssertEquals('Dictionary count should match expected', 
               MEDIUM_BENCHMARK_SIZE - (MEDIUM_BENCHMARK_SIZE div 10) + 1, Dict.Count);
end;

initialization
  RegisterTest(TDictionaryTest);
end.
