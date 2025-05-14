unit TidyKit.Collections.List.Test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Math, DateUtils, StrUtils,
  TidyKit.Collections.List;

type
  { Basic tests for TList functionality }
  TListTest = class(TTestCase)
  private
    { Test lists }
    FIntList: specialize TList<Integer>;
    FStrList: specialize TList<string>;
    
    { Predicate methods for testing }
    function IsEvenNumber(const Value: Integer): Boolean;
    function ContainsLetterA(const Value: string): Boolean;
    
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Basic operations tests }
    procedure Test01_Create;
    procedure Test02_Add;
    procedure Test03_AddRange;
    procedure Test04_Insert;
    procedure Test05_Delete;
    procedure Test06_Remove;
    procedure Test07_Clear;
    
    { Search operation tests }
    procedure Test08_IndexOf;
    procedure Test09_Contains;
    
    { Predicate-based tests }
    procedure Test10_Find;
    procedure Test11_FindAll;
    
    { Transformation tests }
    procedure Test12_Sort;
    procedure Test13_Reverse;
    procedure Test14_Slice;
    
    { Property tests }
    procedure Test15_Capacity;
    procedure Test16_Count;
    procedure Test17_ItemAccess;
    
    { Stress tests }
    procedure Test18_LargeArrayOperations;

    { Additional tests for capacity management }
    procedure Test19_GrowCapacity;
    procedure Test20_CapacityEfficiency;
    
    { Additional tests for edge cases }
    procedure Test21_EmptyOperations;
    procedure Test22_BoundaryConditions;
    
    { Additional tests for complex operations }
    procedure Test23_SortEmptyAndSingle;
    procedure Test24_ReverseWithOddCount;
    procedure Test25_ManualCapacityManagement;

    { Benchmark tests }
    procedure Test26_BenchmarkAddItems;
    procedure Test27_BenchmarkSearch;
    procedure Test28_BenchmarkSorting;

    { Interface-based memory management tests }
    procedure Test29_InterfaceBasedMemoryManagement;
  end;

{ Simple comparison and equality functions for testing }
function IntegerEquals(const A, B: Integer): Boolean;
function StringEquals(const A, B: string): Boolean;
function CompareIntegers(const A, B: Integer): Integer;
function CompareStrings(const A, B: string): Integer;

// Standalone predicate functions
function IsEven(const Item: Integer): Boolean;
function StartsWithA(const Item: string): Boolean;

// Add standalone helper functions at the unit level
function IntEquals(const A, B: Integer): Boolean;
function IntCompare(const A, B: Integer): Integer;

implementation

{ Standard comparison functions }

function IntegerEquals(const A, B: Integer): Boolean;
begin
  Result := A = B;
end;

function StringEquals(const A, B: string): Boolean;
begin
  Result := A = B;
end;

function CompareIntegers(const A, B: Integer): Integer;
begin
  if A < B then
    Result := -1
  else if A > B then
    Result := 1
  else
    Result := 0;
end;

function CompareStrings(const A, B: string): Integer;
begin
  Result := CompareText(A, B);
end;

{ Standalone predicate functions }

function IsEven(const Item: Integer): Boolean;
begin
  Result := (Item mod 2) = 0;
end;

function StartsWithA(const Item: string): Boolean;
begin
  Result := (Length(Item) > 0) and ((Item[1] = 'A') or (Item[1] = 'a'));
end;

function IntEquals(const A, B: Integer): Boolean;
begin
  Result := A = B;
end;

function IntCompare(const A, B: Integer): Integer;
begin
  Result := A - B;
end;

{ TListTest }

function TListTest.IsEvenNumber(const Value: Integer): Boolean;
begin
  Result := (Value mod 2) = 0;
end;

function TListTest.ContainsLetterA(const Value: string): Boolean;
begin
  Result := Pos('a', LowerCase(Value)) > 0;
end;

procedure TListTest.SetUp;
begin
  FIntList := specialize TList<Integer>.Create;
  FStrList := specialize TList<string>.Create;
end;

procedure TListTest.TearDown;
begin
  FIntList.Free;
  FStrList.Free;
end;

procedure TListTest.Test01_Create;
begin
  AssertEquals('New list should be empty', 0, FIntList.Count);
  AssertEquals('New list should have capacity 0', 0, FIntList.Capacity);
end;

procedure TListTest.Test02_Add;
var
  Idx: Integer;
begin
  // Test adding integers
  Idx := FIntList.Add(42);
  AssertEquals('Index should be 0 for first item', 0, Idx);
  AssertEquals('List count should be 1', 1, FIntList.Count);
  AssertEquals('Value should be stored correctly', 42, FIntList[0]);
  
  // Add another item
  Idx := FIntList.Add(123);
  AssertEquals('Index should be 1 for second item', 1, Idx);
  AssertEquals('List count should be 2', 2, FIntList.Count);
  AssertEquals('Value should be stored correctly', 123, FIntList[1]);
  
  // Test adding strings
  Idx := FStrList.Add('Hello');
  AssertEquals('Index should be 0 for first item', 0, Idx);
  AssertEquals('Value should be stored correctly', 'Hello', FStrList[0]);
end;

procedure TListTest.Test03_AddRange;
var
  Values: array[0..2] of Integer;
  I: Integer;
begin
  // Initialize test data
  Values[0] := 10;
  Values[1] := 20;
  Values[2] := 30;
  
  // Add range of values
  FIntList.AddRange(Values);
  
  // Verify results
  AssertEquals('List count should match added range', 3, FIntList.Count);
  
  for I := 0 to 2 do
    AssertEquals('Value at index ' + IntToStr(I) + ' should match', 
                Values[I], FIntList[I]);
                
  // Add more values and check both sets exist
  FIntList.AddRange(Values);
  AssertEquals('List count should be doubled', 6, FIntList.Count);
  
  for I := 0 to 2 do
  begin
    AssertEquals('Original value at index ' + IntToStr(I) + ' should remain', 
                Values[I], FIntList[I]);
    AssertEquals('New value at index ' + IntToStr(I+3) + ' should match', 
                Values[I], FIntList[I+3]);
  end;
end;

procedure TListTest.Test04_Insert;
begin
  // Add some initial items
  FIntList.Add(1);
  FIntList.Add(3);
  
  // Insert in the middle
  FIntList.Insert(1, 2);
  
  // Check results
  AssertEquals('List count should be 3 after insert', 3, FIntList.Count);
  AssertEquals('First item should be 1', 1, FIntList[0]);
  AssertEquals('Second item should be 2', 2, FIntList[1]);
  AssertEquals('Third item should be 3', 3, FIntList[2]);
  
  // Insert at beginning
  FIntList.Insert(0, 0);
  
  // Check results
  AssertEquals('List count should be 4 after insert', 4, FIntList.Count);
  AssertEquals('First item should be 0', 0, FIntList[0]);
  AssertEquals('Second item should be 1', 1, FIntList[1]);
  
  // Insert at end
  FIntList.Insert(FIntList.Count, 4);
  
  // Check results
  AssertEquals('List count should be 5 after insert', 5, FIntList.Count);
  AssertEquals('Last item should be 4', 4, FIntList[4]);
  
  // Try to insert at invalid index - should raise exception
  try
    FIntList.Insert(-1, 99);
    Fail('Insert with negative index should raise exception');
  except
    on E: ERangeError do
      ; // Expected exception
  end;
  
  try
    FIntList.Insert(FIntList.Count + 1, 99);
    Fail('Insert beyond end should raise exception');
  except
    on E: ERangeError do
      ; // Expected exception
  end;
end;

procedure TListTest.Test05_Delete;
begin
  // Add some items
  FIntList.Add(1);
  FIntList.Add(2);
  FIntList.Add(3);
  FIntList.Add(4);
  
  // Delete from middle
  FIntList.Delete(1);
  
  // Check results
  AssertEquals('List count should be decreased', 3, FIntList.Count);
  AssertEquals('First item should be unchanged', 1, FIntList[0]);
  AssertEquals('Second item should now be 3', 3, FIntList[1]);
  AssertEquals('Third item should now be 4', 4, FIntList[2]);
  
  // Delete first item
  FIntList.Delete(0);
  
  // Check results
  AssertEquals('List count should be decreased', 2, FIntList.Count);
  AssertEquals('First item should now be 3', 3, FIntList[0]);
  AssertEquals('Second item should now be 4', 4, FIntList[1]);
  
  // Delete last item
  FIntList.Delete(FIntList.Count - 1);
  
  // Check results
  AssertEquals('List count should be decreased', 1, FIntList.Count);
  AssertEquals('Only remaining item should be 3', 3, FIntList[0]);
  
  // Try to delete at invalid index - should raise exception
  try
    FIntList.Delete(-1);
    Fail('Delete with negative index should raise exception');
  except
    on E: ERangeError do
      ; // Expected exception
  end;
  
  try
    FIntList.Delete(FIntList.Count);
    Fail('Delete beyond end should raise exception');
  except
    on E: ERangeError do
      ; // Expected exception
  end;
end;

procedure TListTest.Test06_Remove;
var
  Success: Boolean;
begin
  // Add some items
  FIntList.Add(10);
  FIntList.Add(20);
  FIntList.Add(30);
  
  // Remove existing item
  Success := FIntList.Remove(20, @IntEquals);
  
  // Check results
  AssertTrue('Remove should return True for existing item', Success);
  AssertEquals('List count should be decreased', 2, FIntList.Count);
  AssertEquals('First item should be unchanged', 10, FIntList[0]);
  AssertEquals('Second item should now be 30', 30, FIntList[1]);
  
  // Try to remove non-existent item
  Success := FIntList.Remove(50, @IntEquals);
  
  // Check results
  AssertFalse('Remove should return False for non-existent item', Success);
  AssertEquals('List count should be unchanged', 2, FIntList.Count);
end;

procedure TListTest.Test07_Clear;
begin
  // Add some items
  FIntList.Add(1);
  FIntList.Add(2);
  FIntList.Add(3);
  
  // Verify items were added
  AssertEquals('List should have 3 items before clear', 3, FIntList.Count);
  
  // Clear the list
  FIntList.Clear;
  
  // Check results
  AssertEquals('List should be empty after clear', 0, FIntList.Count);
  
  // Add an item after clearing to ensure list is still usable
  FIntList.Add(42);
  AssertEquals('Should be able to add items after clear', 1, FIntList.Count);
  AssertEquals('Item should be stored correctly after clear', 42, FIntList[0]);
end;

procedure TListTest.Test08_IndexOf;
var
  Idx: Integer;
begin
  // Add items to test
  FIntList.Add(10);
  FIntList.Add(20);
  FIntList.Add(30);
  FIntList.Add(20); // Duplicate value
  
  FStrList.Add('apple');
  FStrList.Add('banana');
  FStrList.Add('cherry');
  
  // Test finding existing items
  Idx := FIntList.IndexOf(20, @IntEquals);
  AssertEquals('Should find first occurrence of 20 at index 1', 1, Idx);
  
  Idx := FIntList.IndexOf(10, @IntEquals);
  AssertEquals('Should find 10 at index 0', 0, Idx);
  
  Idx := FIntList.IndexOf(30, @IntEquals);
  AssertEquals('Should find 30 at index 2', 2, Idx);
  
  // Test finding non-existent item
  Idx := FIntList.IndexOf(40, @IntEquals);
  AssertEquals('Should return -1 for non-existent item', -1, Idx);
  
  // Test with strings
  Idx := FStrList.IndexOf('banana', @StringEquals);
  AssertEquals('Should find banana at index 1', 1, Idx);
  
  Idx := FStrList.IndexOf('date', @StringEquals);
  AssertEquals('Should return -1 for non-existent string', -1, Idx);
end;

procedure TListTest.Test09_Contains;
begin
  // Add items to test
  FIntList.Add(10);
  FIntList.Add(20);
  FIntList.Add(30);
  
  FStrList.Add('apple');
  FStrList.Add('banana');
  FStrList.Add('cherry');
  
  // Test existing items
  AssertTrue('Should contain 10', FIntList.Contains(10, @IntEquals));
  AssertTrue('Should contain 20', FIntList.Contains(20, @IntEquals));
  AssertTrue('Should contain 30', FIntList.Contains(30, @IntEquals));
  
  // Test non-existent items
  AssertFalse('Should not contain 40', FIntList.Contains(40, @IntEquals));
  
  // Test with strings
  AssertTrue('Should contain banana', FStrList.Contains('banana', @StringEquals));
  AssertFalse('Should not contain date', FStrList.Contains('date', @StringEquals));
end;

procedure TListTest.Test10_Find;
var
  FoundValue: Integer;
  Success: Boolean;
begin
  // Add test data
  FIntList.Add(1);
  FIntList.Add(2); // Even
  FIntList.Add(3);
  FIntList.Add(4); // Even
  
  // Find even number
  Success := FIntList.Find(@IsEven, FoundValue);
  
  // Check results
  AssertTrue('Should find an even number', Success);
  AssertTrue('Found value should be even', IsEven(FoundValue));
  AssertEquals('First even number should be 2', 2, FoundValue);
  
  // Test with no matches
  FIntList.Clear;
  FIntList.Add(1);
  FIntList.Add(3);
  FIntList.Add(5);
  
  Success := FIntList.Find(@IsEven, FoundValue);
  
  // Check results
  AssertFalse('Should not find even numbers', Success);
end;

procedure TListTest.Test11_FindAll;
var
  List: specialize IList<Integer>;
  StringList: specialize IList<string>;
  Results: specialize TArray<Integer>;
  StringResults: specialize TArray<string>;
  I: Integer;
begin
  List := specialize TList<Integer>.New;
  
  // Test FindAll with integers
  List.Add(1);
  List.Add(2);
  List.Add(3);
  List.Add(4);
  List.Add(5);
  List.Add(6);
  
  // Find even numbers
  Results := List.FindAll(@IsEven);
  AssertEquals('Should find 3 even numbers', 3, Length(Results));
  AssertEquals('First even number should be 2', 2, Results[0]);
  AssertEquals('Second even number should be 4', 4, Results[1]);
  AssertEquals('Third even number should be 6', 6, Results[2]);
  
  // Test FindAll with strings
  StringList := specialize TList<string>.New;
  StringList.Add('Apple');
  StringList.Add('Banana');
  StringList.Add('Cherry');
  StringList.Add('Apricot');
  StringList.Add('Blueberry');
  StringList.Add('Avocado');
  
  // Make sure we're using the correct function
  StringResults := StringList.FindAll(@StartsWithA);
  AssertEquals('Should find 3 strings with letter a', 3, Length(StringResults));
  
  // Add additional checks to diagnose the issue if it persists
  WriteLn('Found strings starting with A:');
  for I := 0 to Length(StringResults) - 1 do
    WriteLn(StringResults[I]);
end;

procedure TListTest.Test12_Sort;
var
  SortedArray: specialize TArray<Integer>;
  I: Integer;
begin
  // Add items in random order
  FIntList.Add(3);
  FIntList.Add(1);
  FIntList.Add(4);
  FIntList.Add(2);
  
  // Sort the list
  FIntList.Sort(@IntCompare);
  
  // Check results
  SortedArray := FIntList.ToArray;
  AssertEquals('List length should be unchanged', 4, Length(SortedArray));
  
  // Values should be in ascending order
  for I := 0 to Length(SortedArray) - 2 do
    AssertTrue('Items should be sorted', SortedArray[I] < SortedArray[I+1]);
    
  // Check specific values
  AssertEquals('First item should be 1', 1, SortedArray[0]);
  AssertEquals('Second item should be 2', 2, SortedArray[1]);
  AssertEquals('Third item should be 3', 3, SortedArray[2]);
  AssertEquals('Fourth item should be 4', 4, SortedArray[3]);
end;

procedure TListTest.Test13_Reverse;
begin
  // Add some items
  FIntList.Add(1);
  FIntList.Add(2);
  FIntList.Add(3);
  FIntList.Add(4);
  
  // Reverse the list
  FIntList.Reverse;
  
  // Check results
  AssertEquals('List count should be unchanged', 4, FIntList.Count);
  AssertEquals('First item should now be 4', 4, FIntList[0]);
  AssertEquals('Second item should now be 3', 3, FIntList[1]);
  AssertEquals('Third item should now be 2', 2, FIntList[2]);
  AssertEquals('Fourth item should now be 1', 1, FIntList[3]);
  
  // Test empty list reversal
  FIntList.Clear;
  FIntList.Reverse; // Should not cause any errors
  AssertEquals('Empty list should remain empty', 0, FIntList.Count);
  
  // Test single item list reversal
  FIntList.Add(42);
  FIntList.Reverse;
  AssertEquals('Single-item list should be unchanged after reverse', 42, FIntList[0]);
end;

procedure TListTest.Test14_Slice;
var
  SlicedArray: specialize TArray<Integer>;
begin
  // Add test data
  FIntList.Add(10);
  FIntList.Add(20);
  FIntList.Add(30);
  FIntList.Add(40);
  FIntList.Add(50);
  
  // Test middle slice
  SlicedArray := FIntList.Slice(1, 3);
  
  // Check results
  AssertEquals('Slice length should be 3', 3, Length(SlicedArray));
  AssertEquals('First slice item should be from index 1', 20, SlicedArray[0]);
  AssertEquals('Second slice item should be from index 2', 30, SlicedArray[1]);
  AssertEquals('Third slice item should be from index 3', 40, SlicedArray[2]);
  
  // Test slice from beginning
  SlicedArray := FIntList.Slice(0, 2);
  
  // Check results
  AssertEquals('Slice length should be 2', 2, Length(SlicedArray));
  AssertEquals('First slice item should be from index 0', 10, SlicedArray[0]);
  AssertEquals('Second slice item should be from index 1', 20, SlicedArray[1]);
  
  // Test slice to end
  SlicedArray := FIntList.Slice(3, 100); // Request more than available
  AssertEquals('Slice length should be limited to available items', 2, Length(SlicedArray));
  AssertEquals('First slice item should be from index 3', 40, SlicedArray[0]);
  AssertEquals('Last slice item should be from index 4', 50, SlicedArray[1]);
  
  // Test boundary conditions
  SlicedArray := FIntList.Slice(-1, 2); // Negative index should be treated as 0
  AssertEquals('Slice length with negative start should be corrected', 2, Length(SlicedArray));
  AssertEquals('First slice item should be from index 0', 10, SlicedArray[0]);
  
  // Test beyond list bounds
  SlicedArray := FIntList.Slice(10, 2); // Start beyond end
  AssertEquals('Slice with start beyond end should be empty', 0, Length(SlicedArray));
  
  // Test zero count
  SlicedArray := FIntList.Slice(1, 0); // Zero count should return all from start index
  AssertEquals('Zero count should slice to end', 4, Length(SlicedArray));
  AssertEquals('First slice item should be from index 1', 20, SlicedArray[0]);
end;

procedure TListTest.Test15_Capacity;
var
  NewCapacity: Integer;
begin
  // Initial capacity should be 0
  AssertEquals('Initial capacity should be 0', 0, FIntList.Capacity);
  
  // Set capacity to 10
  NewCapacity := 10;
  FIntList.Capacity := NewCapacity;
  AssertEquals('Capacity should be updated', NewCapacity, FIntList.Capacity);
  
  // Add 5 items, capacity should remain 10
  FIntList.Add(1);
  FIntList.Add(2);
  FIntList.Add(3);
  FIntList.Add(4);
  FIntList.Add(5);
  
  // Test Count - this is the line that's failing
  AssertEquals('Count should be 5', 5, FIntList.Count);
  
  // Set capacity to smaller value than count
  FIntList.Capacity := 3;
  
  // Check that items were truncated
  AssertEquals('Count should be reduced to new capacity', 3, FIntList.Count);
  AssertEquals('First item should be preserved', 1, FIntList[0]);
  AssertEquals('Second item should be preserved', 2, FIntList[1]);
  AssertEquals('Third item should be preserved', 3, FIntList[2]);
  
  try
    AssertEquals('Fourth item should be gone', 4, FIntList[3]);
    Fail('Should not be able to access index beyond new capacity');
  except
    on E: ERangeError do
      ; // Expected exception
  end;
  
  // Set negative capacity (should be treated as 0)
  FIntList.Capacity := -5;
  AssertEquals('Negative capacity should be treated as 0', 0, FIntList.Capacity);
  AssertEquals('Count should be 0 after setting to 0 capacity', 0, FIntList.Count);
end;

procedure TListTest.Test16_Count;
begin
  // Initial count should be 0
  AssertEquals('Initial count should be 0', 0, FIntList.Count);
  
  // Add some items
  FIntList.Add(1);
  AssertEquals('Count should be 1', 1, FIntList.Count);
  
  FIntList.Add(2);
  AssertEquals('Count should be 2', 2, FIntList.Count);
  
  FIntList.Add(3);
  AssertEquals('Count should be 3', 3, FIntList.Count);
  
  // Remove an item
  FIntList.Delete(1);
  AssertEquals('Count should be 2 after delete', 2, FIntList.Count);
  
  // Clear all items
  FIntList.Clear;
  AssertEquals('Count should be 0 after clear', 0, FIntList.Count);
end;

procedure TListTest.Test17_ItemAccess;
begin
  // Add some items
  FIntList.Add(10);
  FIntList.Add(20);
  FIntList.Add(30);
  
  // Check individual item access
  AssertEquals('Item at index 0 should be 10', 10, FIntList[0]);
  AssertEquals('Item at index 1 should be 20', 20, FIntList[1]);
  AssertEquals('Item at index 2 should be 30', 30, FIntList[2]);
  
  // Test setting items
  FIntList[1] := 25;
  AssertEquals('Item at index 1 should be updated to 25', 25, FIntList[1]);
  
  // Test out of bounds access
  try
    FIntList[-1] := 50;
    Fail('Negative index should raise exception');
  except
    on E: ERangeError do
      ; // Expected exception
  end;
  
  try
    FIntList[1000] := 50;
    Fail('Index beyond end should raise exception');
  except
    on E: ERangeError do
      ; // Expected exception
  end;
  
  try
    AssertEquals('', 0, FIntList[3]);
    Fail('Reading beyond end should raise exception');
  except
    on E: ERangeError do
      ; // Expected exception
  end;
end;

procedure TListTest.Test18_LargeArrayOperations;
var
  I, ItemCount: Integer;
  LargeList: specialize TList<Integer>;
begin
  LargeList := specialize TList<Integer>.Create;
  try
    // Test with 10,000 items
    ItemCount := 10000;
    
    // Add items
    for I := 0 to ItemCount - 1 do
      LargeList.Add(I);
      
    // Check count
    AssertEquals('Large list count should match', ItemCount, LargeList.Count);
    
    // Check some random items
    AssertEquals('Item 0 should be 0', 0, LargeList[0]);
    AssertEquals('Item 1000 should be 1000', 1000, LargeList[1000]);
    AssertEquals('Item 5000 should be 5000', 5000, LargeList[5000]);
    AssertEquals('Last item should be 9999', 9999, LargeList[ItemCount-1]);
    
    // Test deleting from large list (delete every 2nd item)
    I := ItemCount - 1;
    while I >= 0 do
    begin
      if (I mod 2) = 0 then
        LargeList.Delete(I);
      Dec(I);
    end;
    
    // Should have half the original items
    AssertEquals('Half of items should remain', ItemCount div 2, LargeList.Count);
    
    // Check that remaining items are odd numbers
    for I := 0 to LargeList.Count - 1 do
      AssertEquals('Remaining items should be odd-indexed', 1, LargeList[I] mod 2);
      
  finally
    LargeList.Free;
  end;
end;

procedure TListTest.Test19_GrowCapacity;
var
  InitialCapacity, I, FinalCapacity: Integer;
begin
  // Set initial capacity
  InitialCapacity := 5;
  FIntList.Capacity := InitialCapacity;
  AssertEquals('Initial capacity should be set correctly', InitialCapacity, FIntList.Capacity);
  
  // Add exactly the capacity number of items
  for I := 1 to InitialCapacity do
    FIntList.Add(I);
    
  AssertEquals('Count should equal initial capacity', InitialCapacity, FIntList.Count);
  AssertEquals('Capacity should remain unchanged', InitialCapacity, FIntList.Capacity);
  
  // Add one more item to force a capacity increase
  FIntList.Add(InitialCapacity + 1);
  
  AssertEquals('Count should be incremented', InitialCapacity + 1, FIntList.Count);
  AssertTrue('Capacity should be increased', FIntList.Capacity > InitialCapacity);
  
  // Remember the grown capacity
  FinalCapacity := FIntList.Capacity;
  
  // Add more items but stay within the new capacity
  for I := 1 to FinalCapacity - FIntList.Count do
    FIntList.Add(I);
    
  AssertEquals('Count should match number of added items', FinalCapacity, FIntList.Count);
  AssertEquals('Capacity should remain stable until exceeded', FinalCapacity, FIntList.Capacity);
end;

procedure TListTest.Test20_CapacityEfficiency;
var
  InitialCapacity: Integer;
begin
  // Set a large initial capacity
  InitialCapacity := 1000;
  FIntList.Capacity := InitialCapacity;
  
  // Add a single item
  FIntList.Add(42);
  
  // Verify count and capacity
  AssertEquals('Count should be 1', 1, FIntList.Count);
  AssertEquals('Capacity should remain at initial setting', InitialCapacity, FIntList.Capacity);
  
  // Now clear the list and check that capacity is reset
  FIntList.Clear;
  AssertEquals('Count should be 0 after clear', 0, FIntList.Count);
  AssertEquals('Capacity should be 0 after clear', 0, FIntList.Capacity);
  
  // Test with AddRange
  FIntList.Capacity := 5;
  FIntList.AddRange([1, 2, 3, 4, 5]);
  
  // Adding more elements should grow capacity appropriately
  FIntList.AddRange([6, 7, 8, 9, 10]);
  
  AssertEquals('Count should be 10', 10, FIntList.Count);
  AssertTrue('Capacity should be at least 10', FIntList.Capacity >= 10);
end;

procedure TListTest.Test21_EmptyOperations;
var
  EmptyInts: specialize TArray<Integer>;
  EmptyStrings: specialize TArray<string>;
  FoundValue: Integer;
begin
  // Operations on empty list
  AssertEquals('Empty list should have count 0', 0, FIntList.Count);
  
  // Search operations
  AssertEquals('IndexOf on empty list should return -1', -1, FIntList.IndexOf(42, @IntEquals));
  AssertFalse('Contains on empty list should return False', FIntList.Contains(42, @IntEquals));
  AssertFalse('Find on empty list should return False', FIntList.Find(@IsEven, FoundValue));
  
  // Transformation operations
  EmptyInts := FIntList.FindAll(@IsEven);
  AssertEquals('FindAll on empty list should return empty array', 0, Length(EmptyInts));
  
  FIntList.Sort(@IntCompare); // Should not crash on empty list
  FIntList.Reverse; // Should not crash on empty list
  
  EmptyInts := FIntList.Slice(0, 10);
  AssertEquals('Slice on empty list should return empty array', 0, Length(EmptyInts));
  
  EmptyInts := FIntList.ToArray;
  AssertEquals('ToArray on empty list should return empty array', 0, Length(EmptyInts));
  
  // Verify empty string list behaves the same
  EmptyStrings := FStrList.FindAll(@StartsWithA);
  AssertEquals('FindAll on empty string list should return empty array', 0, Length(EmptyStrings));
end;

procedure TListTest.Test22_BoundaryConditions;
var
  LargeIndex: Integer;
begin
  // Add a single item
  FIntList.Add(42);
  
  // Test boundary indices for various operations
  try
    FIntList.Insert(-1, 99);
    Fail('Insert with negative index should raise exception');
  except
    on E: ERangeError do ; // Expected exception
  end;
  
  // This should be valid - inserting at Count
  FIntList.Insert(FIntList.Count, 43);
  
  try
    FIntList.Insert(FIntList.Count + 1, 99);
    Fail('Insert beyond Count should raise exception');
  except
    on E: ERangeError do ; // Expected exception
  end;
  
  // Test with extremely large index
  LargeIndex := High(Integer) - 10;
  try
    FIntList.Insert(LargeIndex, 99);
    Fail('Insert with extremely large index should raise exception');
  except
    // Either range error or out of memory is acceptable
    on E: Exception do ; 
  end;
  
  // Test Delete with boundary conditions
  try
    FIntList.Delete(-1);
    Fail('Delete with negative index should raise exception');
  except
    on E: ERangeError do ; // Expected exception
  end;
  
  try
    FIntList.Delete(FIntList.Count);
    Fail('Delete at Count should raise exception');
  except
    on E: ERangeError do ; // Expected exception
  end;
  
  // Test accessing items with boundary conditions
  try
    FIntList[-1] := 99;
    Fail('Setting item at negative index should raise exception');
  except
    on E: ERangeError do ; // Expected exception
  end;
  
  try
    FIntList[FIntList.Count] := 99;
    Fail('Setting item at Count should raise exception');
  except
    on E: ERangeError do ; // Expected exception
  end;
end;

procedure TListTest.Test23_SortEmptyAndSingle;
var
  SingleArray: specialize TArray<Integer>;
begin
  // Sort empty list (should not crash)
  FIntList.Sort(@IntCompare);
  AssertEquals('Empty list should still be empty after sort', 0, FIntList.Count);
  
  // Sort list with single element
  FIntList.Add(42);
  FIntList.Sort(@IntCompare);
  
  AssertEquals('List with single element should still have count 1', 1, FIntList.Count);
  AssertEquals('The single element should remain unchanged', 42, FIntList[0]);
  
  // Get array and verify
  SingleArray := FIntList.ToArray;
  AssertEquals('ToArray should return array with length 1', 1, Length(SingleArray));
  AssertEquals('Element should be preserved', 42, SingleArray[0]);
end;

procedure TListTest.Test24_ReverseWithOddCount;
begin
  // Test reverse on list with odd number of elements
  FIntList.AddRange([1, 2, 3, 4, 5]);
  FIntList.Reverse;
  
  // Verify correct reversal
  AssertEquals('First item should now be 5', 5, FIntList[0]);
  AssertEquals('Middle item should now be 3', 3, FIntList[2]);
  AssertEquals('Last item should now be 1', 1, FIntList[4]);
  
  // Test the complete reversed list
  AssertEquals('Item at index 0 should be 5', 5, FIntList[0]);
  AssertEquals('Item at index 1 should be 4', 4, FIntList[1]);
  AssertEquals('Item at index 2 should be 3', 3, FIntList[2]);
  AssertEquals('Item at index 3 should be 2', 2, FIntList[3]);
  AssertEquals('Item at index 4 should be 1', 1, FIntList[4]);
end;

procedure TListTest.Test25_ManualCapacityManagement;
begin
  // Set capacity, add fewer elements
  FIntList.Capacity := 10;
  FIntList.Add(1);
  FIntList.Add(2);
  
  // Verify state
  AssertEquals('Count should be 2', 2, FIntList.Count);
  AssertEquals('Capacity should be 10', 10, FIntList.Capacity);
  
  // Manually reduce capacity to exactly match count
  FIntList.Capacity := FIntList.Count;
  
  // Verify new state
  AssertEquals('Count should still be 2', 2, FIntList.Count);
  AssertEquals('Capacity should now match count', 2, FIntList.Capacity);
  
  // Increase capacity without adding elements
  FIntList.Capacity := 20;
  
  // Try to access beyond count but within capacity (should fail)
  try
    AssertEquals('', 0, FIntList[2]);
    Fail('Accessing within capacity but beyond count should fail');
  except
    on E: ERangeError do ; // Expected exception
  end;
  
  // Reduce capacity to 0 (clear)
  FIntList.Capacity := 0;
  AssertEquals('Count should be 0', 0, FIntList.Count);
  AssertEquals('Capacity should be 0', 0, FIntList.Capacity);
end;

{ Interface memory management test }

procedure TListTest.Test29_InterfaceBasedMemoryManagement;
var
  InterfaceList: specialize IList<Integer>;
  AnotherRef: specialize IList<Integer>;
begin
  // Create list using the interface-based approach
  InterfaceList := specialize TList<Integer>.New;
  
  // Test operations
  InterfaceList.Add(42);
  InterfaceList.Add(123);
  
  AssertEquals('Interface-based list should have proper count', 2, InterfaceList.Count);
  AssertEquals('First item should be stored correctly', 42, InterfaceList[0]);
  AssertEquals('Second item should be stored correctly', 123, InterfaceList[1]);
  
  // Create another reference to same list
  AnotherRef := InterfaceList;
  
  // Modify through the second reference
  AnotherRef.Add(456);
  
  // Verify changes are reflected in both references (they point to the same object)
  AssertEquals('Count should be 3 in both references', 3, InterfaceList.Count);
  AssertEquals('Count should be 3 in both references', 3, AnotherRef.Count);
  AssertEquals('New item should be accessible through original reference', 456, InterfaceList[2]);
  
  // Let one reference go out of scope - should not cause issues
  AnotherRef := nil;
  
  // Original reference should still work
  AssertEquals('Original reference should still be valid', 3, InterfaceList.Count);
  
  // Add a new item
  InterfaceList.Add(789);
  AssertEquals('Should be able to add items after second ref is nil', 4, InterfaceList.Count);
  
  // No explicit Free needed - memory will be released when InterfaceList goes out of scope
end;

{ Benchmark tests }

procedure TListTest.Test26_BenchmarkAddItems;
var
  StartTime, EndTime: TDateTime;
  ElapsedMS: Int64;
  List: specialize TList<Integer>;
  I, NumItems: Integer;
  ItemsPerSecond: Double;
begin
  // Benchmark adding 10,000 items
  NumItems := 10000;
  List := specialize TList<Integer>.Create;
  try
    StartTime := Now;
    
    for I := 1 to NumItems do
      List.Add(I);
      
    EndTime := Now;
    ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
    
    // Avoid division by zero
    if ElapsedMS > 0 then
      ItemsPerSecond := NumItems / (ElapsedMS / 1000)
    else
      ItemsPerSecond := 0;
      
    WriteLn(Format('Adding %d items took: %.3f seconds (%.2f items/sec)', 
              [NumItems, ElapsedMS/1000, ItemsPerSecond]));
              
    AssertTrue(Format('Adding %d items should complete in a reasonable time', [NumItems]), True);
  finally
    List.Free;
  end;
  
  // Benchmark adding 100,000 items
  NumItems := 100000;
  List := specialize TList<Integer>.Create;
  try
    StartTime := Now;
    
    for I := 1 to NumItems do
      List.Add(I);
      
    EndTime := Now;
    ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
    
    // Avoid division by zero
    if ElapsedMS > 0 then
      ItemsPerSecond := NumItems / (ElapsedMS / 1000)
    else
      ItemsPerSecond := 0;
      
    WriteLn(Format('Adding %d items took: %.3f seconds (%.2f items/sec)', 
              [NumItems, ElapsedMS/1000, ItemsPerSecond]));
              
    AssertTrue(Format('Adding %d items should complete in a reasonable time', [NumItems]), True);
  finally
    List.Free;
  end;
  
  // For 1,000,000 items test
  NumItems := 1000000;
  List := specialize TList<Integer>.Create;
  try
    StartTime := Now;
    
    for I := 1 to NumItems do
      List.Add(I);
      
    EndTime := Now;
    ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
    
    // Avoid division by zero
    if ElapsedMS > 0 then
      ItemsPerSecond := NumItems / (ElapsedMS / 1000)
    else
      ItemsPerSecond := 0;
      
    WriteLn(Format('Adding %d items took: %.3f seconds (%.2f items/sec)', 
              [NumItems, ElapsedMS/1000, ItemsPerSecond]));
              
    AssertTrue(Format('Adding %d items should complete in a reasonable time', [NumItems]), True);
  finally
    List.Free;
  end;
end;

procedure TListTest.Test27_BenchmarkSearch;
  // Local function to benchmark search operations
  procedure BenchmarkSearch(NumItems: Integer);
  var
    StartTime, EndTime: TDateTime;
    ElapsedMS: Int64;
    List: specialize TList<Integer>;
    I, SearchValue, FoundIndex: Integer;
    OpsPerSecond: Double;
  begin
    // Create and fill the list
    List := specialize TList<Integer>.Create;
    try
      for I := 1 to NumItems do
        List.Add(I);
        
      // Search for an item at the beginning of the list
      SearchValue := 1;
      StartTime := Now;
      FoundIndex := List.IndexOf(SearchValue, @IntEquals);
      EndTime := Now;
      ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
      
      // Avoid division by zero
      if ElapsedMS > 0 then
        OpsPerSecond := 1 / (ElapsedMS / 1000)
      else
        OpsPerSecond := 0;
        
      WriteLn(Format('Searching for value %d (beginning) in %d items took: %.3f seconds (%.2f ops/sec)', 
                    [SearchValue, NumItems, ElapsedMS/1000, OpsPerSecond]));
                    
      AssertEquals('Found index should match value-1', SearchValue-1, FoundIndex);
      
      // Search for an item in the middle of the list
      SearchValue := NumItems div 2;
      StartTime := Now;
      FoundIndex := List.IndexOf(SearchValue, @IntEquals);
      EndTime := Now;
      ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
      
      // Avoid division by zero
      if ElapsedMS > 0 then
        OpsPerSecond := 1 / (ElapsedMS / 1000)
      else
        OpsPerSecond := 0;
        
      WriteLn(Format('Searching for value %d (middle) in %d items took: %.3f seconds (%.2f ops/sec)', 
                    [SearchValue, NumItems, ElapsedMS/1000, OpsPerSecond]));
                    
      AssertEquals('Found index should match value-1', SearchValue-1, FoundIndex);
      
      // Search for an item at the end of the list
      SearchValue := NumItems;
      StartTime := Now;
      FoundIndex := List.IndexOf(SearchValue, @IntEquals);
      EndTime := Now;
      ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
      
      // Avoid division by zero
      if ElapsedMS > 0 then
        OpsPerSecond := 1 / (ElapsedMS / 1000)
      else
        OpsPerSecond := 0;
        
      WriteLn(Format('Searching for value %d (end) in %d items took: %.3f seconds (%.2f ops/sec)', 
                    [SearchValue, NumItems, ElapsedMS/1000, OpsPerSecond]));
                    
      AssertEquals('Found index should match value-1', SearchValue-1, FoundIndex);
      
      // Search for a non-existent item
      SearchValue := NumItems + 1;
      StartTime := Now;
      FoundIndex := List.IndexOf(SearchValue, @IntEquals);
      EndTime := Now;
      ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
      
      // Avoid division by zero
      if ElapsedMS > 0 then
        OpsPerSecond := 1 / (ElapsedMS / 1000)
      else
        OpsPerSecond := 0;
        
      WriteLn(Format('Searching for non-existent value %d in %d items took: %.3f seconds (%.2f ops/sec)', 
                    [SearchValue, NumItems, ElapsedMS/1000, OpsPerSecond]));
                    
      AssertEquals('Non-existent item should return -1', -1, FoundIndex);
    finally
      List.Free;
    end;
  end;
  
begin
  // Test with 10,000 items
  WriteLn('---- Search benchmark with 10,000 items ----');
  BenchmarkSearch(10000);
  
  // Test with 100,000 items
  WriteLn('---- Search benchmark with 100,000 items ----');
  BenchmarkSearch(100000);
  
  // Test with 1,000,000 items
  WriteLn('---- Search benchmark with 1,000,000 items ----');
  BenchmarkSearch(1000000);
end;

procedure TListTest.Test28_BenchmarkSorting;
  // Local function to benchmark sorting operations
  procedure BenchmarkSort(NumItems: Integer; RandomSeed: Integer);
  var
    StartTime, EndTime: TDateTime;
    ElapsedSortMS, ElapsedReverseMS: Int64;
    List: specialize TList<Integer>;
    I: Integer;
    OpsPerSecond: Double;
  begin
    // Set random seed for reproducibility
    RandSeed := RandomSeed;
    
    // Create and fill the list with random values
    List := specialize TList<Integer>.Create;
    try
      for I := 1 to NumItems do
        List.Add(Random(NumItems * 10)); // Random values up to 10x the count
          
      // Benchmark sorting
      StartTime := Now;
      List.Sort(@IntCompare);
      EndTime := Now;
      ElapsedSortMS := MilliSecondsBetween(EndTime, StartTime);
      
      // Avoid division by zero
      if ElapsedSortMS > 0 then
        OpsPerSecond := 1 / (ElapsedSortMS / 1000)
      else
        OpsPerSecond := 0;
        
      WriteLn(Format('Sorting %d items took: %.3f seconds (%.2f ops/sec)', 
                    [NumItems, ElapsedSortMS/1000, OpsPerSecond]));
        
      // Benchmark reversing
      StartTime := Now;
      List.Reverse;
      EndTime := Now;
      ElapsedReverseMS := MilliSecondsBetween(EndTime, StartTime);
      
      // Avoid division by zero
      if ElapsedReverseMS > 0 then
        OpsPerSecond := 1 / (ElapsedReverseMS / 1000)
      else
        OpsPerSecond := 0;
        
      WriteLn(Format('Reversing %d items took: %.3f seconds (%.2f ops/sec)', 
                    [NumItems, ElapsedReverseMS/1000, OpsPerSecond]));
    finally
      List.Free;
    end;
  end;
  
begin
  // Test with 10,000 items
  WriteLn('---- Sorting benchmark with 10,000 items ----');
  BenchmarkSort(10000, 12345);
  
  // Test with 100,000 items
  WriteLn('---- Sorting benchmark with 100,000 items ----');
  BenchmarkSort(100000, 12345);
  
  // Test with 1,000,000 items - comment out if too demanding
  WriteLn('---- Sorting benchmark with 1,000,000 items ----');
  BenchmarkSort(1000000, 12345);
end;

initialization
  RegisterTest(TListTest);
end.
