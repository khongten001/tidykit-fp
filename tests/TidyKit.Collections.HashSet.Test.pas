unit TidyKit.Collections.HashSet.Test;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, DateUtils, Math,
  TidyKit.Collections.HashSet,
  TidyKit.Collections.HashFunction,
  TidyKit.Collections.EqualityFunction;

type
  // Custom record type for testing hash set performance
  TStudent = record
    StudentID: Integer;
    FirstName: string;
    LastName: string;
    GPA: Single;
  end;

  THashSetTest = class(TTestCase)
  published
    // Existing tests
    procedure Test01_BasicAddAndContains_Integer;
    procedure Test02_Remove_String;
    procedure Test03_Clear_Record;
    procedure Test04_ToArray_Integer;
    procedure Test05_Collisions;
    procedure Test06_ResizeBehavior;
    procedure Test07_BenchmarkAddItems_Integer;
    procedure Test08_Constructor_Validations;       
    procedure Test09_ToArray_EmptyAndCleared;       
    procedure Test10_InterfaceBasedMemoryManagement;
    procedure Test11_BenchmarkContains_Integer; 
    procedure Test12_BasicOperations_Student;
    procedure Test13_BenchmarkAddItems_Student;
    procedure Test14_BenchmarkContains_Student;
    procedure Test15_BasicOperations_Float;
    procedure Test16_EdgeCases_Float;
    procedure Test17_BasicOperations_Boolean;
    procedure Test18_BasicOperations_DateTime;
    procedure Test19_BasicOperations_Char;
    procedure Test20_BasicOperations_Int64;
    procedure Test21_LargeValues_Int64;

    // New tests using advanced hash functions
    procedure Test22_AdvancedHashFunctions;
  end;

implementation

// Record Type for Testing
type
  TTestRecord = record
    ID: Integer;
    Name: string;
  end;

// Updated to use basic hash functions directly
function TestRecordHash(const Value: TTestRecord): Integer;
var
  HName: Integer;
begin
  HName := XXHash32(Value.Name);
  Result := MultiplicativeHash(Value.ID) xor HName;
end;

function TestRecordEquals(const A, B: TTestRecord): Boolean;
begin
  Result := (A.ID = B.ID) and (A.Name = B.Name);
end;

// TStudent hash function updated to use basic hash functions
function StudentHash(const Value: TStudent): Integer;
var
  H_ID, H_FirstName, H_LastName, H_GPA: Integer;
  GPAAsInt: Integer;
begin
  H_ID := MultiplicativeHash(Value.StudentID);
  H_FirstName := XXHash32(Value.FirstName);
  H_LastName := XXHash32(Value.LastName);
  
  if IsNan(Value.GPA) then
    GPAAsInt := 0
  else
    GPAAsInt := Round(Value.GPA * 100.0);
  
  H_GPA := MultiplicativeHash(GPAAsInt);
  Result := (H_ID xor H_FirstName xor H_LastName xor H_GPA) and $7FFFFFFF;
end;

function StudentEquals(const A, B: TStudent): Boolean;
const
  Epsilon = 0.001; // Small epsilon for floating point comparison
begin
  Result := (A.StudentID = B.StudentID) and
           (A.FirstName = B.FirstName) and
           (A.LastName = B.LastName) and
           (Abs(A.GPA - B.GPA) < Epsilon); // Compare floats with tolerance
end;

// Hash function that intentionally creates collisions for testing
const
  CollisionTestHashValue = 5;
function CollisionHashForTest(const Value: Integer): Integer;
begin
  Result := CollisionTestHashValue;
end;

{ THashSetTest implementation }

procedure THashSetTest.Test01_BasicAddAndContains_Integer;
var
  SetInstance: specialize IHashSet<Integer>;
  I: Integer;
begin
  // Use basic hash function directly
  SetInstance := specialize CreateHashSet<Integer>(@MultiplicativeHash, @TidyKitIntegerEquals);

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

procedure THashSetTest.Test02_Remove_String;
var
  SetInstance: specialize IHashSet<string>;
begin
  SetInstance := specialize CreateHashSet<string>(@XXHash32, @TidyKitStringEquals);

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

procedure THashSetTest.Test03_Clear_Record;
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

procedure THashSetTest.Test04_ToArray_Integer;
var
  SetInstance: specialize IHashSet<Integer>;
  Arr: specialize TArray<Integer>;
  I, J: Integer;
  Found: Boolean;
begin
  SetInstance := specialize CreateHashSet<Integer>(@MultiplicativeHash, @TidyKitIntegerEquals, 5);

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

procedure THashSetTest.Test05_Collisions;
var
  SetInstance: specialize IHashSet<Integer>;
  I: Integer;
const
  NumItems = 10;
begin
  SetInstance := specialize CreateHashSet<Integer>(@CollisionHashForTest, @TidyKitIntegerEquals, 4);

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

procedure THashSetTest.Test06_ResizeBehavior;
var
  SetInstance: specialize IHashSet<Integer>;
  I: Integer;
  InitialCap: Integer = 4;
  NumToAdd: Integer = 20;
begin
  SetInstance := specialize CreateHashSet<Integer>(@MultiplicativeHash, @TidyKitIntegerEquals, InitialCap, 0.75);

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

procedure THashSetTest.Test07_BenchmarkAddItems_Integer;
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
    WriteLn(Format('    Benchmarking Add for %d items...', [NumItems]));
    SetInstance := specialize CreateHashSet<Integer>(@MultiplicativeHash, @TidyKitIntegerEquals);

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
  end;
end;

procedure THashSetTest.Test08_Constructor_Validations;
var
  SetInstance: specialize IHashSet<Integer>;
  RaisedException: Boolean;
begin
  RaisedException := False;
  try
    SetInstance := specialize CreateHashSet<Integer>(nil, @TidyKitIntegerEquals);
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
    SetInstance := specialize CreateHashSet<Integer>(@MultiplicativeHash, nil);
    Fail('Constructor should raise exception for nil EqualityFunc');
  except
    on E: Exception do
    begin
      RaisedException := True;
      AssertEquals('Nil EqualityFunc exception message', 'EqualityFunc cannot be nil.', E.Message);
    end;
  end;
  AssertTrue('Exception was raised for nil EqualityFunc', RaisedException);

  // Test successful creation with valid functions
  SetInstance := specialize CreateHashSet<Integer>(@MultiplicativeHash, @TidyKitIntegerEquals);
  AssertNotNull('SetInstance should not be nil with valid functions', SetInstance);
end;

procedure THashSetTest.Test09_ToArray_EmptyAndCleared;
var
  SetInstance: specialize IHashSet<Integer>;
  Arr: specialize TArray<Integer>;
begin
  // Test ToArray on a newly created (empty) set
  SetInstance := specialize CreateHashSet<Integer>(@MultiplicativeHash, @TidyKitIntegerEquals);
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

procedure THashSetTest.Test10_InterfaceBasedMemoryManagement;
var
  Set1: specialize IHashSet<Integer>;
  Set2: specialize IHashSet<Integer>;
begin
  Set1 := specialize CreateHashSet<Integer>(@MultiplicativeHash, @TidyKitIntegerEquals);
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

procedure THashSetTest.Test11_BenchmarkContains_Integer;
var
  SetInstance: specialize IHashSet<Integer>;
  I, NumItems, ValueToTest: Integer;
  StartTime, EndTime: TDateTime;
  ElapsedMS: Int64;
  OpsPerSecond: Double;
  Counts: array[0..2] of Integer;
  J: Integer;
  Found: Boolean;
begin
  Counts[0] := 10000;
  Counts[1] := 100000;
  Counts[2] := 1000000;

  for J := Low(Counts) to High(Counts) do
  begin
    NumItems := Counts[J];
    WriteLn(Format('    Benchmarking Contains for %d items...', [NumItems]));
    SetInstance := specialize CreateHashSet<Integer>(@MultiplicativeHash, @TidyKitIntegerEquals);

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

procedure THashSetTest.Test12_BasicOperations_Student;
var
  StudentSet: specialize IHashSet<TStudent>;
  S1, S2, S3, S1_Duplicate: TStudent;
begin
  StudentSet := specialize CreateHashSet<TStudent>(@StudentHash, @StudentEquals);

  S1.StudentID := 101; S1.FirstName := 'Alice'; S1.LastName := 'Smith'; S1.GPA := 3.5;
  S2.StudentID := 102; S2.FirstName := 'Bob'; S2.LastName := 'Johnson'; S2.GPA := 3.2;
  S3.StudentID := 103; S3.FirstName := 'Carol'; S3.LastName := 'Williams'; S3.GPA := 3.8;
  
  // Create a duplicate of S1 for testing
  S1_Duplicate.StudentID := 101; 
  S1_Duplicate.FirstName := 'Alice';
  S1_Duplicate.LastName := 'Smith'; 
  S1_Duplicate.GPA := 3.5;

  // Test Add and Contains
  AssertTrue('Add S1', StudentSet.Add(S1));
  AssertEquals('Count after S1', 1, StudentSet.Count);
  AssertTrue('Contains S1', StudentSet.Contains(S1));

  AssertTrue('Add S2', StudentSet.Add(S2));
  AssertEquals('Count after S2', 2, StudentSet.Count);

  // Test adding a duplicate
  AssertFalse('Adding duplicate S1 should return False', StudentSet.Add(S1_Duplicate));
  AssertEquals('Count should remain 2 after duplicate S1 add', 2, StudentSet.Count);

  // Test adding a third unique student
  AssertTrue('Add S3', StudentSet.Add(S3));
  AssertEquals('Count after S3', 3, StudentSet.Count);
  
  // Test removal
  AssertTrue('Remove S2', StudentSet.Remove(S2));
  AssertEquals('Count after removing S2', 2, StudentSet.Count);
  AssertFalse('S2 should not be present after removal', StudentSet.Contains(S2));
  AssertTrue('S1 should still be present', StudentSet.Contains(S1));

  // Test Clear
  StudentSet.Clear;
  AssertEquals('Count after clear should be 0', 0, StudentSet.Count);
  AssertFalse('Should not contain S1 after clear', StudentSet.Contains(S1));
end;

procedure THashSetTest.Test13_BenchmarkAddItems_Student;
var
  StudentSet: specialize IHashSet<TStudent>;
  I, NumItems: Integer;
  StartTime, EndTime: TDateTime;
  ElapsedMS: Int64;
  ItemsPerSecond: Double;
  Student: TStudent;
  Counts: array[0..1] of Integer;
  J: Integer;
begin
  // Use smaller item counts for more complex record benchmarks
  Counts[0] := 10000;  
  Counts[1] := 50000;  

  for J := Low(Counts) to High(Counts) do
  begin
    NumItems := Counts[J];
    WriteLn(Format('    Benchmarking Add for %d TStudent records...', [NumItems]));
    
    StudentSet := specialize CreateHashSet<TStudent>(@StudentHash, @StudentEquals);

    StartTime := Now;
    
    // Add items with unique IDs but some common patterns in names
    for I := 1 to NumItems do
    begin
      Student.StudentID := I;
      Student.FirstName := 'FirstName' + IntToStr(I mod 100); // Create some common first names
      Student.LastName := 'LastName' + IntToStr(I mod 500);   // Create some common last names
      Student.GPA := 2.0 + ((I mod 21) / 10.0);              // GPAs between 2.0 and 4.0
      StudentSet.Add(Student);
    end;
    
    EndTime := Now;
    ElapsedMS := MilliSecondsBetween(EndTime, StartTime);

    // Verify correct number of adds
    AssertEquals(Format('Count should be %d after adding all TStudent records', [NumItems]), NumItems, StudentSet.Count);

    // Calculate and display performance metrics
    if ElapsedMS > 0 then
      ItemsPerSecond := NumItems / (ElapsedMS / 1000.0)
    else
      ItemsPerSecond := 0; 

    WriteLn(Format('    Adding %d TStudent records took: %.3f seconds (%.2f items/sec)',
              [NumItems, ElapsedMS / 1000.0, ItemsPerSecond]));
  end;
end;

procedure THashSetTest.Test14_BenchmarkContains_Student;
var
  StudentSet: specialize IHashSet<TStudent>;
  I, NumItems: Integer;
  StartTime, EndTime: TDateTime;
  ElapsedMS: Int64;
  OpsPerSecond: Double;
  StudentToTest, TempStudent: TStudent;
  Counts: array[0..1] of Integer;
  J: Integer;
  Found: Boolean;
begin
  // Smaller item counts for more complex record benchmarks
  Counts[0] := 10000;
  Counts[1] := 50000;

  for J := Low(Counts) to High(Counts) do
  begin
    NumItems := Counts[J];
    WriteLn(Format('    Benchmarking Contains for %d TStudent records...', [NumItems]));
    
    // Create and populate the set
    StudentSet := specialize CreateHashSet<TStudent>(@StudentHash, @StudentEquals);
    for I := 1 to NumItems do
    begin
      TempStudent.StudentID := I;
      TempStudent.FirstName := 'FirstName' + IntToStr(I mod 100);
      TempStudent.LastName := 'LastName' + IntToStr(I mod 500);
      TempStudent.GPA := 2.0 + ((I mod 21) / 10.0);
      StudentSet.Add(TempStudent);
    end;
    
    AssertEquals(Format('Count should be %d before TStudent Contains benchmark', [NumItems]), NumItems, StudentSet.Count);

    // Benchmark Contains for existing items
    StartTime := Now;
    Found := False; // Initialize
    
    for I := 1 to NumItems do
    begin
      // Look for items we know exist (cycle through all student IDs)
      StudentToTest.StudentID := (I mod NumItems) + 1; 
      StudentToTest.FirstName := 'FirstName' + IntToStr(StudentToTest.StudentID mod 100);
      StudentToTest.LastName := 'LastName' + IntToStr(StudentToTest.StudentID mod 500);
      StudentToTest.GPA := 2.0 + ((StudentToTest.StudentID mod 21) / 10.0);
      
      Found := StudentSet.Contains(StudentToTest); 
    end;
    
    EndTime := Now;
    ElapsedMS := MilliSecondsBetween(EndTime, StartTime);

    if ElapsedMS > 0 then
      OpsPerSecond := NumItems / (ElapsedMS / 1000.0)
    else
      OpsPerSecond := 0;
      
    WriteLn(Format('    Contains (existing) %d TStudent records took: %.3f seconds (%.2f ops/sec)',
              [NumItems, ElapsedMS / 1000.0, OpsPerSecond]));

    // Benchmark Contains for non-existing items
    StartTime := Now;
    
    for I := 1 to NumItems do
    begin
      // Create students with IDs guaranteed not to be in the set
      StudentToTest.StudentID := NumItems + I; 
      StudentToTest.FirstName := 'NonExistent' + IntToStr(I mod 100);
      StudentToTest.LastName := 'NoSuchStudent' + IntToStr(I mod 500);
      StudentToTest.GPA := 1.5 + ((I mod 10) / 10.0); // Different GPA range
      
      Found := StudentSet.Contains(StudentToTest);
    end;
    
    EndTime := Now;
    ElapsedMS := MilliSecondsBetween(EndTime, StartTime);

    if ElapsedMS > 0 then
      OpsPerSecond := NumItems / (ElapsedMS / 1000.0)
    else
      OpsPerSecond := 0;
      
    WriteLn(Format('    Contains (non-existing) %d TStudent records took: %.3f seconds (%.2f ops/sec)',
              [NumItems, ElapsedMS / 1000.0, OpsPerSecond]));
  end;
end;

procedure THashSetTest.Test15_BasicOperations_Float;
var
  SetInstance: specialize IHashSet<Extended>;
begin
  SetInstance := specialize CreateHashSet<Extended>(@FloatHash, @TidyKitFloatEquals);
  
  AssertTrue('Add 3.14159', SetInstance.Add(3.14159));
  AssertTrue('Add 2.71828', SetInstance.Add(2.71828));
  AssertTrue('Add 1.41421', SetInstance.Add(1.41421));
  
  AssertEquals('Count should be 3', 3, SetInstance.Count);
  
  AssertTrue('Contains 3.14159', SetInstance.Contains(3.14159));
  AssertTrue('Contains 2.71828', SetInstance.Contains(2.71828));
  AssertFalse('Should not contain 1.61803', SetInstance.Contains(1.61803));
  
  AssertTrue('Remove 2.71828', SetInstance.Remove(2.71828));
  AssertEquals('Count after remove', 2, SetInstance.Count);
  AssertFalse('Should not contain removed item', SetInstance.Contains(2.71828));
end;

procedure THashSetTest.Test16_EdgeCases_Float;
var
  SetInstance: specialize IHashSet<Extended>;
  PositiveInfinity, NegativeInfinity, PositiveZero, NegativeZero: Extended;
  NaNValue: Extended;
begin
  SetInstance := specialize CreateHashSet<Extended>(@FloatHash, @TidyKitFloatEquals);
  
  PositiveInfinity := Infinity;
  NegativeInfinity := -Infinity;
  NaNValue := NaN;
  PositiveZero := 0.0;
  
  // Create a true negative zero using a division that ensures sign bit is preserved
  NegativeZero := -1.0;
  NegativeZero := NegativeZero * 0.0; // This will create -0.0 reliably
  
  // Test NaN handling
  AssertTrue('Add NaN', SetInstance.Add(NaNValue));
  AssertTrue('Contains NaN', SetInstance.Contains(NaNValue));
  
  // Test infinities
  AssertTrue('Add +Infinity', SetInstance.Add(PositiveInfinity));
  AssertTrue('Add -Infinity', SetInstance.Add(NegativeInfinity));
  AssertTrue('Contains +Infinity', SetInstance.Contains(PositiveInfinity));
  AssertTrue('Contains -Infinity', SetInstance.Contains(NegativeInfinity));
  
  // Test both zero values
  AssertTrue('Add +0.0', SetInstance.Add(PositiveZero));
  AssertTrue('Add -0.0', SetInstance.Add(NegativeZero));
  
  // Test very small/large values
  AssertTrue('Add very small value', SetInstance.Add(1.0e-300));
  AssertTrue('Add very large value', SetInstance.Add(1.0e+300));
  
  AssertEquals('Count should be 7 after adding edge cases', 7, SetInstance.Count);
end;

procedure THashSetTest.Test17_BasicOperations_Boolean;
var
  SetInstance: specialize IHashSet<Boolean>;
begin
  SetInstance := specialize CreateHashSet<Boolean>(@BooleanHash, @TidyKitBooleanEquals);
  
  AssertTrue('Add True', SetInstance.Add(True));
  AssertEquals('Count after adding True', 1, SetInstance.Count);
  
  AssertTrue('Add False', SetInstance.Add(False));
  AssertEquals('Count after adding False', 2, SetInstance.Count);
  
  // Test adding duplicates
  AssertFalse('Add True again should fail', SetInstance.Add(True));
  AssertFalse('Add False again should fail', SetInstance.Add(False));
  AssertEquals('Count should still be 2', 2, SetInstance.Count);
  
  // Test contains
  AssertTrue('Contains True', SetInstance.Contains(True));
  AssertTrue('Contains False', SetInstance.Contains(False));
  
  // Test remove
  AssertTrue('Remove True', SetInstance.Remove(True));
  AssertEquals('Count after removing True', 1, SetInstance.Count);
  AssertFalse('Should not contain True after removal', SetInstance.Contains(True));
  AssertTrue('Should still contain False', SetInstance.Contains(False));
end;

procedure THashSetTest.Test18_BasicOperations_DateTime;
var
  SetInstance: specialize IHashSet<TDateTime>;
  Date1, Date2, Date3: TDateTime;
begin
  SetInstance := specialize CreateHashSet<TDateTime>(@DateTimeHash, @TidyKitDateTimeEquals);
  
  Date1 := EncodeDate(2023, 6, 15) + EncodeTime(10, 30, 0, 0);
  Date2 := EncodeDate(2023, 6, 16) + EncodeTime(10, 30, 0, 0);
  Date3 := EncodeDate(2023, 6, 15) + EncodeTime(10, 30, 1, 0);
  
  AssertTrue('Add Date1', SetInstance.Add(Date1));
  AssertTrue('Add Date2', SetInstance.Add(Date2));
  AssertTrue('Add Date3', SetInstance.Add(Date3));
  AssertEquals('Count after adding dates', 3, SetInstance.Count);
  
  AssertTrue('Contains Date1', SetInstance.Contains(Date1));
  AssertTrue('Contains Date2', SetInstance.Contains(Date2));
  
  // Test with reconstructed but equivalent date
  AssertTrue('Contains reconstructed date', 
    SetInstance.Contains(EncodeDate(2023, 6, 16) + EncodeTime(10, 30, 0, 0)));
    
  // Test removal
  AssertTrue('Remove Date2', SetInstance.Remove(Date2));
  AssertEquals('Count after removing Date2', 2, SetInstance.Count);
  AssertFalse('Should not contain Date2 after removal', SetInstance.Contains(Date2));
end;

procedure THashSetTest.Test19_BasicOperations_Char;
var
  SetInstance: specialize IHashSet<Char>;
begin
  SetInstance := specialize CreateHashSet<Char>(@CharHash, @TidyKitCharEquals);
  
  AssertTrue('Add ''a''', SetInstance.Add('a'));
  AssertTrue('Add ''b''', SetInstance.Add('b'));
  AssertTrue('Add ''Z''', SetInstance.Add('Z'));
  AssertTrue('Add special character', SetInstance.Add('#'));
  
  AssertEquals('Count after adding characters', 4, SetInstance.Count);
  
  AssertTrue('Contains ''a''', SetInstance.Contains('a'));
  AssertFalse('Does not contain ''c''', SetInstance.Contains('c'));
  
  // Test adding duplicate
  AssertFalse('Adding duplicate ''Z'' should fail', SetInstance.Add('Z'));
  AssertEquals('Count should remain 4', 4, SetInstance.Count);
  
  // Test removal
  AssertTrue('Remove ''b''', SetInstance.Remove('b'));
  AssertEquals('Count after removal', 3, SetInstance.Count);
  AssertFalse('Should not contain ''b'' after removal', SetInstance.Contains('b'));
end;

procedure THashSetTest.Test20_BasicOperations_Int64;
var
  SetInstance: specialize IHashSet<Int64>;
begin
  SetInstance := specialize CreateHashSet<Int64>(@Int64Hash, @TidyKitInt64Equals);
  
  AssertTrue('Add 1000', SetInstance.Add(1000));
  AssertTrue('Add 2000', SetInstance.Add(2000));
  AssertTrue('Add 3000', SetInstance.Add(3000));
  
  AssertEquals('Count should be 3', 3, SetInstance.Count);
  
  AssertTrue('Contains 2000', SetInstance.Contains(2000));
  AssertFalse('Does not contain 4000', SetInstance.Contains(4000));
  
  // Test removing
  AssertTrue('Remove 1000', SetInstance.Remove(1000));
  AssertEquals('Count after removal', 2, SetInstance.Count);
  AssertFalse('Should not contain 1000 after removal', SetInstance.Contains(1000));
end;

procedure THashSetTest.Test21_LargeValues_Int64;
var
  SetInstance: specialize IHashSet<Int64>;
  LargeValue1, LargeValue2: Int64;
begin
  SetInstance := specialize CreateHashSet<Int64>(@Int64Hash, @TidyKitInt64Equals);
  
  // Test values beyond 32-bit range
  LargeValue1 := 2147483648;  // 2^31, just beyond 32-bit signed int
  LargeValue2 := 9223372036854775807;  // Maximum Int64 value
  
  AssertTrue('Add value beyond 32-bit range', SetInstance.Add(LargeValue1));
  AssertTrue('Add maximum Int64 value', SetInstance.Add(LargeValue2));
  AssertEquals('Count after adding large values', 2, SetInstance.Count);
  
  AssertTrue('Contains value beyond 32-bit range', SetInstance.Contains(LargeValue1));
  AssertTrue('Contains maximum Int64 value', SetInstance.Contains(LargeValue2));
  
  // Test negative large values
  AssertTrue('Add large negative value', SetInstance.Add(-LargeValue1));
  AssertEquals('Count after adding negative value', 3, SetInstance.Count);
  AssertTrue('Contains large negative value', SetInstance.Contains(-LargeValue1));
  
  // Test removal of large value
  AssertTrue('Remove maximum Int64 value', SetInstance.Remove(LargeValue2));
  AssertEquals('Count after removal', 2, SetInstance.Count);
  AssertFalse('Should not contain maximum Int64 after removal', 
    SetInstance.Contains(LargeValue2));
end;

// New test for advanced hash functions
procedure THashSetTest.Test22_AdvancedHashFunctions;
var
  IntSet: specialize IHashSet<Integer>;
  StrSet: specialize IHashSet<string>;
  FloatSet: specialize IHashSet<Extended>;
begin
  // Test Integer with MultiplicativeHash
  IntSet := specialize CreateHashSet<Integer>(@MultiplicativeHash, @TidyKitIntegerEquals);
  AssertTrue('Add int 123', IntSet.Add(123));
  AssertTrue('Add int 456', IntSet.Add(456));
  AssertTrue('Contains 123', IntSet.Contains(123));
  AssertEquals('Count should be 2', 2, IntSet.Count);
  
  // Test String with XXHash32
  StrSet := specialize CreateHashSet<string>(@XXHash32, @TidyKitStringEquals);
  AssertTrue('Add string "test"', StrSet.Add('test'));
  AssertTrue('Add string "example"', StrSet.Add('example'));
  AssertTrue('Contains "test"', StrSet.Contains('test'));
  AssertEquals('Count should be 2', 2, StrSet.Count);
  
  // Test Float with FloatHash
  FloatSet := specialize CreateHashSet<Extended>(@FloatHash, @TidyKitFloatEquals);
  AssertTrue('Add float 3.14', FloatSet.Add(3.14));
  AssertTrue('Add float 2.71', FloatSet.Add(2.71));
  AssertTrue('Contains 3.14', FloatSet.Contains(3.14));
  AssertEquals('Count should be 2', 2, FloatSet.Count);
end;

initialization
  RegisterTest(THashSetTest);

end.
