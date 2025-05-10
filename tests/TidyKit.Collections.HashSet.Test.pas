unit TidyKit.Collections.HashSet.Test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, DateUtils, // Added fpcunit, testregistry
  TidyKit.Collections.HashSet;

type
  THashSetTest = class(TTestCase)
  private
    // No private fields needed for these tests as instances are created per method
  protected
    // procedure SetUp; override; // Optional: if common setup is needed
    // procedure TearDown; override; // Optional: if common teardown is needed
  published
    procedure TestBasicAddAndContains_Integer;
    procedure TestRemove_String;
    procedure TestClear_Record;
    procedure TestToArray_Integer;
    procedure TestCollisions;
    procedure TestResizeBehavior;
    procedure TestBenchmarkAddItems_Integer;
    procedure TestConstructor_Validations;      // New Test
    procedure TestToArray_EmptyAndCleared;    // New Test
    procedure TestInterfaceBasedMemoryManagement; // New Test
    procedure TestBenchmarkContains_Integer; // New Benchmark Test
  end;

implementation

// Helper functions remain at unit level

// Integer Hash and Equality
function IntegerHash(const Value: Integer): Integer;
begin
  Result := Value;
end;

function IntegerEquals(const A, B: Integer): Boolean;
begin
  Result := A = B;
end;

// String Hash and Equality
function StringHash(const Value: string): Integer;
var
  I: Integer;
  H: Cardinal;
begin
  H := 0;
  for I := 1 to Length(Value) do
    H := 31 * H + Ord(Value[I]);
  Result := Integer(H and $7FFFFFFF); // Ensure positive hash
end;

function StringEquals(const A, B: string): Boolean;
begin
  Result := A = B;
end;

// Record Type for Testing
type
  TTestRecord = record
    ID: Integer;
    Name: string;
  end;

function TestRecordHash(const Value: TTestRecord): Integer;
var
  HName: Integer;
begin
  HName := StringHash(Value.Name);
  Result := IntegerHash(Value.ID) xor HName;
end;

function TestRecordEquals(const A, B: TTestRecord): Boolean;
begin
  Result := (A.ID = B.ID) and (A.Name = B.Name);
end;

// Hash function that intentionally creates collisions for testing
const
  CollisionTestHashValue = 5;
function CollisionHashForTest(const Value: Integer): Integer;
begin
  Result := CollisionTestHashValue;
end;

{ THashSetTest }

procedure THashSetTest.TestBasicAddAndContains_Integer;
var
  SetInstance: specialize IHashSet<Integer>;
  I: Integer;
begin
  // WriteLn('  TestBasicAddAndContains_Integer...'); // fpcunit runner provides test names
  SetInstance := specialize CreateHashSet<Integer>(@IntegerHash, @IntegerEquals);

  AssertTrue('Add 10', SetInstance.Add(10));
  AssertEquals('Count after adding 10', 1, SetInstance.Count);
  AssertTrue('Contains 10', SetInstance.Contains(10));

  AssertTrue('Add 20', SetInstance.Add(20));
  AssertEquals('Count after adding 20', 2, SetInstance.Count);
  AssertTrue('Contains 20', SetInstance.Contains(20));

  AssertFalse('Does not contain 30', SetInstance.Contains(30));
  AssertFalse('Adding duplicate 10 should return False', SetInstance.Add(10));
  AssertEquals('Count should remain 2 after duplicate add', 2, SetInstance.Count);

  for I := 1 to 50 do
    SetInstance.Add(I * 100);

  AssertTrue('Contains 1000 after many adds', SetInstance.Contains(1000));
  AssertTrue('Contains 5000 after many adds', SetInstance.Contains(5000));
  AssertEquals('Count after 50 more unique adds', 2 + 50, SetInstance.Count);
end;

procedure THashSetTest.TestRemove_String;
var
  SetInstance: specialize IHashSet<string>;
begin
  SetInstance := specialize CreateHashSet<string>(@StringHash, @StringEquals);

  SetInstance.Add('apple');
  SetInstance.Add('banana');
  SetInstance.Add('cherry');
  AssertEquals('Count after 3 string adds', 3, SetInstance.Count);

  AssertTrue('Remove existing "banana"', SetInstance.Remove('banana'));
  AssertEquals('Count after removing "banana"', 2, SetInstance.Count);
  AssertFalse('"banana" should not be present', SetInstance.Contains('banana'));
  AssertTrue('"apple" should still be present', SetInstance.Contains('apple'));

  AssertFalse('Remove non-existing "grape"', SetInstance.Remove('grape'));
  AssertEquals('Count should remain 2 after trying to remove non-existing', 2, SetInstance.Count);

  AssertTrue('Remove "apple"', SetInstance.Remove('apple'));
  AssertTrue('Remove "cherry"', SetInstance.Remove('cherry'));
  AssertEquals('Count should be 0 after removing all', 0, SetInstance.Count);
end;

procedure THashSetTest.TestClear_Record;
var
  SetInstance: specialize IHashSet<TTestRecord>;
  Rec1, Rec2: TTestRecord;
begin
  SetInstance := specialize CreateHashSet<TTestRecord>(@TestRecordHash, @TestRecordEquals);

  Rec1.ID := 1; Rec1.Name := 'Test1';
  Rec2.ID := 2; Rec2.Name := 'Test2';

  SetInstance.Add(Rec1);
  SetInstance.Add(Rec2);
  AssertEquals('Count before clear', 2, SetInstance.Count);

  SetInstance.Clear;
  AssertEquals('Count after clear should be 0', 0, SetInstance.Count);
  AssertFalse('Should not contain Rec1 after clear', SetInstance.Contains(Rec1));
  AssertFalse('Should not contain Rec2 after clear', SetInstance.Contains(Rec2));

  SetInstance.Add(Rec1);
  AssertEquals('Count after adding post-clear', 1, SetInstance.Count);
  AssertTrue('Should contain Rec1 after adding post-clear', SetInstance.Contains(Rec1));
end;

procedure THashSetTest.TestToArray_Integer;
var
  SetInstance: specialize IHashSet<Integer>;
  Arr: specialize TArray<Integer>; // Use specialize TArray<T> for consistency with HashSet.ToArray
  I, J: Integer;
  Found: Boolean;
begin
  SetInstance := specialize CreateHashSet<Integer>(@IntegerHash, @IntegerEquals, 5);

  SetInstance.Add(1);
  SetInstance.Add(5);
  SetInstance.Add(3);
  SetInstance.Add(2);
  SetInstance.Add(4);
  SetInstance.Add(10);
  SetInstance.Add(100);

  Arr := SetInstance.ToArray;
  AssertEquals('ToArray length matches Set Count', SetInstance.Count, Length(Arr));
  AssertEquals('ToArray length is 7', 7, Length(Arr));

  for I := 1 to 5 do
  begin
    Found := False;
    for J := 0 to High(Arr) do
      if Arr[J] = I then Found := True;
    AssertTrue('Element ' + IntToStr(I) + ' should be in ToArray result', Found);
  end;
  Found := False; for J := 0 to High(Arr) do if Arr[J] = 10 then Found := True;
  AssertTrue('Element 10 should be in ToArray result', Found);
  Found := False; for J := 0 to High(Arr) do if Arr[J] = 100 then Found := True;
  AssertTrue('Element 100 should be in ToArray result', Found);
end;

procedure THashSetTest.TestCollisions;
var
  SetInstance: specialize IHashSet<Integer>;
  I: Integer;
const
  NumItems = 10;
begin
  SetInstance := specialize CreateHashSet<Integer>(@CollisionHashForTest, @IntegerEquals, 4);

  for I := 1 to NumItems do
  begin
    AssertTrue('Add item ' + IntToStr(I) + ' with collision hash', SetInstance.Add(I));
  end;

  AssertEquals('Count should be NumItems after all adds', NumItems, SetInstance.Count);

  for I := 1 to NumItems do
  begin
    AssertTrue('Set should contain ' + IntToStr(I), SetInstance.Contains(I));
  end;

  AssertTrue('Remove an item from collided bucket', SetInstance.Remove(NumItems div 2));
  AssertEquals('Count after removing one item', NumItems - 1, SetInstance.Count);
  AssertFalse('Removed item should not be present', SetInstance.Contains(NumItems div 2));
  AssertTrue('Other item in bucket should still be present', SetInstance.Contains(NumItems div 2 + 1));
end;

procedure THashSetTest.TestResizeBehavior;
var
  SetInstance: specialize IHashSet<Integer>;
  I: Integer;
  InitialCap: Integer = 4;
  NumToAdd: Integer = 20;
begin
  SetInstance := specialize CreateHashSet<Integer>(@IntegerHash, @IntegerEquals, InitialCap, 0.75);

  for I := 1 to NumToAdd do
  begin
    SetInstance.Add(I);
    AssertTrue('Contains ' + IntToStr(I) + ' after add during resize test', SetInstance.Contains(I));
    AssertEquals('Count is ' + IntToStr(I) + ' during resize test', I, SetInstance.Count);
  end;

  AssertEquals('Final count after many adds causing resizes', NumToAdd, SetInstance.Count);
  for I := 1 to NumToAdd do
  begin
    AssertTrue('Final check: Contains ' + IntToStr(I), SetInstance.Contains(I));
  end;

  SetInstance.Remove(1);
  SetInstance.Remove(NumToAdd div 2);
  SetInstance.Remove(NumToAdd);
  AssertEquals('Count after some removals post-resize', NumToAdd - 3, SetInstance.Count);
  AssertFalse('Item 1 removed', SetInstance.Contains(1));
  AssertTrue('Item 2 still present', SetInstance.Contains(2));
end;

procedure THashSetTest.TestBenchmarkAddItems_Integer;
var
  SetInstance: specialize IHashSet<Integer>;
  I, NumItems: Integer;
  StartTime, EndTime: TDateTime;
  ElapsedMS: Int64;
  ItemsPerSecond: Double;
  Counts: array[0..2] of Integer;
  J: Integer;
begin
  Counts[0] := 10000;
  Counts[1] := 100000;
  Counts[2] := 1000000;

  for J := Low(Counts) to High(Counts) do
  begin
    NumItems := Counts[J];
    WriteLn(Format('    Benchmarking Add for %d items...', [NumItems])); // Keep WriteLn for benchmarks
    SetInstance := specialize CreateHashSet<Integer>(@IntegerHash, @IntegerEquals);

    StartTime := Now;
    for I := 1 to NumItems do
      SetInstance.Add(I);
    EndTime := Now;
    ElapsedMS := MilliSecondsBetween(EndTime, StartTime);

    AssertEquals(Format('Count should be %d after adding all items', [NumItems]), NumItems, SetInstance.Count);

    if ElapsedMS > 0 then
      ItemsPerSecond := NumItems / (ElapsedMS / 1000.0)
    else
      ItemsPerSecond := 0;

    WriteLn(Format('    Adding %d items took: %.3f seconds (%.2f items/sec)',
              [NumItems, ElapsedMS / 1000.0, ItemsPerSecond]));
    // No specific assert for benchmark time, just that it completes and count is correct.
  end;
end;

procedure THashSetTest.TestBenchmarkContains_Integer;
var
  SetInstance: specialize IHashSet<Integer>;
  I, NumItems, ValueToTest: Integer;
  StartTime, EndTime: TDateTime;
  ElapsedMS: Int64;
  OpsPerSecond: Double;
  Counts: array[0..2] of Integer;
  J: Integer;
  Found: Boolean; // To ensure the compiler doesn't optimize away the Contains call
begin
  Counts[0] := 10000;
  Counts[1] := 100000;
  Counts[2] := 1000000;

  for J := Low(Counts) to High(Counts) do
  begin
    NumItems := Counts[J];
    WriteLn(Format('    Benchmarking Contains for %d items...', [NumItems]));
    SetInstance := specialize CreateHashSet<Integer>(@IntegerHash, @IntegerEquals);

    // Populate the set
    for I := 1 to NumItems do
      SetInstance.Add(I);

    AssertEquals(Format('Count should be %d before Contains benchmark', [NumItems]), NumItems, SetInstance.Count);

    // Benchmark Contains for existing items
    StartTime := Now;
    Found := True; // Initialize to prevent uninitialized warning, actual value doesn't matter here
    for I := 1 to NumItems do
    begin
      ValueToTest := (I mod NumItems) + 1; // Cycle through existing items
      Found := SetInstance.Contains(ValueToTest);
      // AssertTrue('Item should be found', Found); // Optional: too slow for benchmark loop
    end;
    EndTime := Now;
    ElapsedMS := MilliSecondsBetween(EndTime, StartTime);

    if ElapsedMS > 0 then
      OpsPerSecond := NumItems / (ElapsedMS / 1000.0)
    else
      OpsPerSecond := 0;

    WriteLn(Format('    Contains (existing) %d items took: %.3f seconds (%.2f ops/sec)',
              [NumItems, ElapsedMS / 1000.0, OpsPerSecond]));

    // Benchmark Contains for non-existing items
    StartTime := Now;
    for I := 1 to NumItems do
    begin
      ValueToTest := NumItems + I; // Items guaranteed not to be in the set
      Found := SetInstance.Contains(ValueToTest);
      // AssertFalse('Item should not be found', Found); // Optional: too slow for benchmark loop
    end;
    EndTime := Now;
    ElapsedMS := MilliSecondsBetween(EndTime, StartTime);

    if ElapsedMS > 0 then
      OpsPerSecond := NumItems / (ElapsedMS / 1000.0)
    else
      OpsPerSecond := 0;

    WriteLn(Format('    Contains (non-existing) %d items took: %.3f seconds (%.2f ops/sec)',
              [NumItems, ElapsedMS / 1000.0, OpsPerSecond]));
    // SetInstance will be freed by ARC
  end;
end;

procedure THashSetTest.TestConstructor_Validations;
var
  SetInstance: specialize IHashSet<Integer>;
  RaisedException: Boolean;
begin
  RaisedException := False;
  try
    SetInstance := specialize CreateHashSet<Integer>(nil, @IntegerEquals);
    Fail('Constructor should raise exception for nil HashFunc');
  except
    on E: Exception do
    begin
      RaisedException := True;
      AssertEquals('Nil HashFunc exception message', 'HashFunc cannot be nil.', E.Message);
    end;
  end;
  AssertTrue('Exception was raised for nil HashFunc', RaisedException);

  RaisedException := False;
  try
    SetInstance := specialize CreateHashSet<Integer>(@IntegerHash, nil);
    Fail('Constructor should raise exception for nil EqualityFunc');
  except
    on E: Exception do
    begin
      RaisedException := True;
      AssertEquals('Nil EqualityFunc exception message', 'EqualityFunc cannot be nil.', E.Message);
    end;
  end;
  AssertTrue('Exception was raised for nil EqualityFunc', RaisedException);

  // Test successful creation with valid functions (implicitly tested elsewhere, but good for completeness)
  SetInstance := specialize CreateHashSet<Integer>(@IntegerHash, @IntegerEquals);
  AssertNotNull('SetInstance should not be nil with valid functions', SetInstance);
end;

procedure THashSetTest.TestToArray_EmptyAndCleared;
var
  SetInstance: specialize IHashSet<Integer>;
  Arr: specialize TArray<Integer>;
begin
  // Test ToArray on a newly created (empty) set
  SetInstance := specialize CreateHashSet<Integer>(@IntegerHash, @IntegerEquals);
  Arr := SetInstance.ToArray;
  AssertEquals('ToArray on new empty set should return empty array', 0, Length(Arr));

  // Add an item, then clear, then ToArray
  SetInstance.Add(100);
  AssertEquals('Count should be 1 after adding an item', 1, SetInstance.Count);
  SetInstance.Clear;
  AssertEquals('Count should be 0 after clear', 0, SetInstance.Count);
  Arr := SetInstance.ToArray;
  AssertEquals('ToArray after clear should return empty array', 0, Length(Arr));

  // Add an item after clear, then ToArray
  SetInstance.Add(200);
  Arr := SetInstance.ToArray;
  AssertEquals('ToArray after clear and re-add should have 1 item', 1, Length(Arr));
  if Length(Arr) = 1 then // Prevent range check error if previous assert fails
    AssertEquals('Item in ToArray after clear and re-add should be 200', 200, Arr[0]);
end;

procedure THashSetTest.TestInterfaceBasedMemoryManagement;
var
  Set1: specialize IHashSet<Integer>;
  Set2: specialize IHashSet<Integer>;
begin
  Set1 := specialize CreateHashSet<Integer>(@IntegerHash, @IntegerEquals);
  Set1.Add(10);
  Set1.Add(20);
  AssertEquals('Set1 count after adds', 2, Set1.Count);

  Set2 := Set1; // Assign to another interface variable
  AssertEquals('Set2 count should match Set1', 2, Set2.Count);
  AssertTrue('Set2 should contain 10', Set2.Contains(10));

  Set1.Add(30);
  AssertEquals('Set1 count after adding to original', 3, Set1.Count);
  AssertEquals('Set2 count should reflect change via Set1', 3, Set2.Count);
  AssertTrue('Set2 should contain 30', Set2.Contains(30));

  Set1 := nil; // Release one reference
  // Set2 should still be valid and hold the data
  AssertNotNull('Set2 should still be valid', Set2);
  AssertEquals('Set2 count should remain 3', 3, Set2.Count);
  AssertTrue('Set2 should still contain 20', Set2.Contains(20));

  Set2.Remove(20);
  AssertEquals('Set2 count after remove', 2, Set2.Count);
  AssertFalse('Set2 should not contain 20 after remove', Set2.Contains(20));

  // Set2 will be released when it goes out of scope, ARC handles freeing the THashSet object
end;

initialization
  RegisterTest(THashSetTest);

end.
