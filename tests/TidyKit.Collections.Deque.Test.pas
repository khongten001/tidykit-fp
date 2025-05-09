unit TidyKit.Collections.Deque.Test;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  TidyKit.Collections.Deque, DateUtils, StrUtils;

// Standalone helper functions for testing
function IntEquals(const A, B: Integer): Boolean;
function IntCompare(const A, B: Integer): Integer;
function IsEven(const Item: Integer): Boolean;

const
  // Benchmark sizes
  SMALL_BENCHMARK_SIZE = 10000;
  MEDIUM_BENCHMARK_SIZE = 100000;
  LARGE_BENCHMARK_SIZE = 1000000;

type
  { TDequeTest }
  TDequeTest = class(TTestCase)
  private
    FDeque: specialize IDeque<Integer>;
    
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    
  published
    // Basic operations
    procedure TestCreate;
    procedure TestPushFrontPopFront;
    procedure TestPushBackPopBack;
    procedure TestMixedOperations;
    procedure TestPeek;
    procedure TestClear;
    
    // Access operations
    procedure TestGetSetItem;
    
    // Search operations
    procedure TestContains;
    procedure TestIndexOf;
    
    // Transformation methods
    procedure TestReverse;
    procedure TestToArray;
    
    // Error conditions
    procedure TestPopEmptyDeque;
    procedure TestPeekEmptyDeque;
    procedure TestAccessOutOfBounds;
    
    // Memory management
    procedure TestAutoCreateInterface;
    procedure TestCapacityManagement;
    
    // Benchmarks
    procedure BenchmarkPushBackSmall;
    procedure BenchmarkPushBackMedium;
    procedure BenchmarkPushBackLarge;
    
    procedure BenchmarkPushFrontSmall;
    procedure BenchmarkPushFrontMedium;
    
    procedure BenchmarkPopBackSmall;
    procedure BenchmarkPopBackMedium;
    procedure BenchmarkPopBackLarge;
    
    procedure BenchmarkPopFrontSmall;
    procedure BenchmarkPopFrontMedium;
    procedure BenchmarkPopFrontLarge;
    
    procedure BenchmarkReverseSmall;
    procedure BenchmarkReverseMedium;
    procedure BenchmarkReverseLarge;
    
    procedure BenchmarkMixedOperationsSmall;
    procedure BenchmarkMixedOperationsMedium;
  end;

implementation

// Implement the standalone helper functions
function IntEquals(const A, B: Integer): Boolean;
begin
  Result := A = B;
end;

function IntCompare(const A, B: Integer): Integer;
begin
  Result := A - B;
end;

function IsEven(const Item: Integer): Boolean;
begin
  Result := (Item mod 2) = 0;
end;

procedure TDequeTest.SetUp;
begin
  FDeque := specialize TDeque<Integer>.Create;
end;

procedure TDequeTest.TearDown;
begin
  FDeque := nil; // This should release the interface
end;

procedure TDequeTest.TestCreate;
begin
  AssertEquals('New deque should be empty', 0, FDeque.Count);
end;

procedure TDequeTest.TestPushFrontPopFront;
var
  I, Value: Integer;
begin
  // Add 10 values to the front
  for I := 1 to 10 do
    FDeque.PushFront(I);
    
  AssertEquals('Deque should have 10 elements', 10, FDeque.Count);
  
  // Pop them and check for reverse order
  for I := 1 to 10 do
  begin
    Value := FDeque.PopFront;
    AssertEquals('Value should be in reverse order', 11 - I, Value);
  end;
  
  AssertEquals('Deque should be empty after popping', 0, FDeque.Count);
end;

procedure TDequeTest.TestPushBackPopBack;
var
  I, Value: Integer;
begin
  // Add 10 values to the back
  for I := 1 to 10 do
    FDeque.PushBack(I);
    
  AssertEquals('Deque should have 10 elements', 10, FDeque.Count);
  
  // Pop them and check for reverse order
  for I := 1 to 10 do
  begin
    Value := FDeque.PopBack;
    AssertEquals('Value should be in reverse order', 11 - I, Value);
  end;
  
  AssertEquals('Deque should be empty after popping', 0, FDeque.Count);
end;

procedure TDequeTest.TestMixedOperations;
var
  Value: Integer;
begin
  // Mix push front/back and pop front/back
  FDeque.PushFront(1);
  FDeque.PushBack(2);
  FDeque.PushFront(3);
  FDeque.PushBack(4);
  // Deque should be [3, 1, 2, 4]
  
  Value := FDeque.PopFront;
  AssertEquals('First value should be 3', 3, Value);
  
  Value := FDeque.PopBack;
  AssertEquals('Last value should be 4', 4, Value);
  
  Value := FDeque.PopFront;
  AssertEquals('First value should be 1', 1, Value);
  
  Value := FDeque.PopBack;
  AssertEquals('Last value should be 2', 2, Value);
  
  AssertEquals('Deque should be empty', 0, FDeque.Count);
end;

procedure TDequeTest.TestPeek;
begin
  FDeque.PushFront(1);
  FDeque.PushBack(2);
  
  AssertEquals('PeekFront should return 1', 1, FDeque.PeekFront);
  AssertEquals('PeekBack should return 2', 2, FDeque.PeekBack);
  
  // Values should still be in deque
  AssertEquals('Deque count should still be 2', 2, FDeque.Count);
end;

procedure TDequeTest.TestClear;
begin
  FDeque.PushFront(1);
  FDeque.PushBack(2);
  FDeque.Clear;
  
  AssertEquals('Deque should be empty after Clear', 0, FDeque.Count);
end;

procedure TDequeTest.TestGetSetItem;
var
  I: Integer;
begin
  // Add 5 items
  for I := 0 to 4 do
    FDeque.PushBack(I * 10);
    
  // Test getter
  for I := 0 to 4 do
    AssertEquals('GetItem should return correct value', I * 10, FDeque.Items[I]);
    
  // Test setter
  for I := 0 to 4 do
    FDeque.Items[I] := I * 100;
    
  // Verify values were set
  for I := 0 to 4 do
    AssertEquals('GetItem should return updated value', I * 100, FDeque.Items[I]);
end;

procedure TDequeTest.TestContains;
begin
  FDeque.PushBack(10);
  FDeque.PushBack(20);
  FDeque.PushBack(30);
  
  AssertTrue('Deque should contain 10', FDeque.Contains(10, @IntEquals));
  AssertTrue('Deque should contain 20', FDeque.Contains(20, @IntEquals));
  AssertTrue('Deque should contain 30', FDeque.Contains(30, @IntEquals));
  AssertFalse('Deque should not contain 40', FDeque.Contains(40, @IntEquals));
end;

procedure TDequeTest.TestIndexOf;
begin
  FDeque.PushBack(10);
  FDeque.PushBack(20);
  FDeque.PushBack(30);
  
  AssertEquals('IndexOf 10 should be 0', 0, FDeque.IndexOf(10, @IntEquals));
  AssertEquals('IndexOf 20 should be 1', 1, FDeque.IndexOf(20, @IntEquals));
  AssertEquals('IndexOf 30 should be 2', 2, FDeque.IndexOf(30, @IntEquals));
  AssertEquals('IndexOf 40 should be -1', -1, FDeque.IndexOf(40, @IntEquals));
end;

procedure TDequeTest.TestReverse;
begin
  FDeque.PushBack(10);
  FDeque.PushBack(20);
  FDeque.PushBack(30);
  FDeque.PushBack(40);
  
  FDeque.Reverse;
  
  AssertEquals('After reverse, index 0 should be 40', 40, FDeque.Items[0]);
  AssertEquals('After reverse, index 1 should be 30', 30, FDeque.Items[1]);
  AssertEquals('After reverse, index 2 should be 20', 20, FDeque.Items[2]);
  AssertEquals('After reverse, index 3 should be 10', 10, FDeque.Items[3]);
end;

procedure TDequeTest.TestToArray;
var
  Arr: specialize TArray<Integer>;
begin
  FDeque.PushBack(10);
  FDeque.PushBack(20);
  FDeque.PushBack(30);
  
  Arr := FDeque.ToArray;
  
  AssertEquals('Array length should match deque count', 3, Length(Arr));
  AssertEquals('Array[0] should be 10', 10, Arr[0]);
  AssertEquals('Array[1] should be 20', 20, Arr[1]);
  AssertEquals('Array[2] should be 30', 30, Arr[2]);
end;

procedure TDequeTest.TestPopEmptyDeque;
var
  ExceptionRaised: Boolean;
begin
  ExceptionRaised := False;
  try
    FDeque.PopFront;
  except
    on E: Exception do
      ExceptionRaised := True;
  end;
  AssertTrue('PopFront on empty deque should raise exception', ExceptionRaised);
  
  ExceptionRaised := False;
  try
    FDeque.PopBack;
  except
    on E: Exception do
      ExceptionRaised := True;
  end;
  AssertTrue('PopBack on empty deque should raise exception', ExceptionRaised);
end;

procedure TDequeTest.TestPeekEmptyDeque;
var
  ExceptionRaised: Boolean;
begin
  ExceptionRaised := False;
  try
    FDeque.PeekFront;
  except
    on E: Exception do
      ExceptionRaised := True;
  end;
  AssertTrue('PeekFront on empty deque should raise exception', ExceptionRaised);
  
  ExceptionRaised := False;
  try
    FDeque.PeekBack;
  except
    on E: Exception do
      ExceptionRaised := True;
  end;
  AssertTrue('PeekBack on empty deque should raise exception', ExceptionRaised);
end;

procedure TDequeTest.TestAccessOutOfBounds;
var
  ExceptionRaised: Boolean;
  Dummy: Integer;
begin
  FDeque.PushBack(10);
  
  // Test access with negative index
  ExceptionRaised := False;
  try
    Dummy := FDeque.Items[-1];
  except
    on E: ERangeError do
      ExceptionRaised := True;
  end;
  AssertTrue('Access with negative index should raise exception', ExceptionRaised);
  
  // Test access with too large index
  ExceptionRaised := False;
  try
    Dummy := FDeque.Items[1]; // Only have item at index 0
  except
    on E: ERangeError do
      ExceptionRaised := True;
  end;
  AssertTrue('Access with too large index should raise exception', ExceptionRaised);
end;

procedure TDequeTest.TestAutoCreateInterface;
var
  Deque: specialize IDeque<Integer>;
begin
  // Test the factory method for automatic creation
  Deque := specialize TDeque<Integer>.New;
  
  AssertEquals('New deque should be empty', 0, Deque.Count);
  
  Deque.PushBack(10);
  Deque.PushBack(20);
  
  AssertEquals('Deque should have 2 elements', 2, Deque.Count);
  AssertEquals('First element should be 10', 10, Deque.Items[0]);
  
  // Interface should be automatically freed
end;

procedure TDequeTest.TestCapacityManagement;
var
  I: Integer;
begin
  // Test that capacity grows as needed
  for I := 1 to 100 do
    FDeque.PushBack(I);
    
  AssertEquals('Deque should have 100 elements', 100, FDeque.Count);
  AssertTrue('Capacity should be at least as big as count', FDeque.Capacity >= FDeque.Count);
  
  // Test manual capacity setting
  FDeque.Clear;
  FDeque.Capacity := 50;
  
  for I := 1 to 25 do
  begin
    FDeque.PushFront(I);
    FDeque.PushBack(I);
  end;
  
  AssertEquals('Deque should have 50 elements', 50, FDeque.Count);
  AssertEquals('Capacity should be exactly 50', 50, FDeque.Capacity);
end;

// Benchmarking procedures
procedure TDequeTest.BenchmarkPushBackSmall;
var
  StartTime, EndTime: TDateTime;
  ElapsedMS: Int64;
  I: Integer;
  NewDeque: specialize IDeque<Integer>;
  ItemsPerSecond: Double;
begin
  NewDeque := specialize TDeque<Integer>.New;
  
  StartTime := Now;
  
  for I := 1 to SMALL_BENCHMARK_SIZE do
    NewDeque.PushBack(I);
    
  EndTime := Now;
  ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
  
  // Avoid division by zero
  if ElapsedMS > 0 then
    ItemsPerSecond := SMALL_BENCHMARK_SIZE / (ElapsedMS / 1000)
  else
    ItemsPerSecond := 0;
    
  WriteLn('BenchmarkPushBackSmall: ', SMALL_BENCHMARK_SIZE, 
          ' items in ', ElapsedMS, ' ms', 
          IfThen(ElapsedMS > 0, Format(' (%.2f items/sec)', [ItemsPerSecond]), ''));
  
  AssertEquals('Count should match inserted items', SMALL_BENCHMARK_SIZE, NewDeque.Count);
end;

procedure TDequeTest.BenchmarkPushBackMedium;
var
  StartTime, EndTime: TDateTime;
  ElapsedMS: Int64;
  I: Integer;
  NewDeque: specialize IDeque<Integer>;
  ItemsPerSecond: Double;
begin
  NewDeque := specialize TDeque<Integer>.New;
  
  StartTime := Now;
  
  for I := 1 to MEDIUM_BENCHMARK_SIZE do
    NewDeque.PushBack(I);
    
  EndTime := Now;
  ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
  
  // Avoid division by zero
  if ElapsedMS > 0 then
    ItemsPerSecond := MEDIUM_BENCHMARK_SIZE / (ElapsedMS / 1000)
  else
    ItemsPerSecond := 0;
    
  WriteLn('BenchmarkPushBackMedium: ', MEDIUM_BENCHMARK_SIZE, 
          ' items in ', ElapsedMS, ' ms', 
          IfThen(ElapsedMS > 0, Format(' (%.2f items/sec)', [ItemsPerSecond]), ''));
  
  AssertEquals('Count should match inserted items', MEDIUM_BENCHMARK_SIZE, NewDeque.Count);
end;

procedure TDequeTest.BenchmarkPushBackLarge;
var
  StartTime, EndTime: TDateTime;
  ElapsedMS: Int64;
  I: Integer;
  NewDeque: specialize IDeque<Integer>;
  ItemsPerSecond: Double;
begin
  NewDeque := specialize TDeque<Integer>.New;
  
  StartTime := Now;
  
  for I := 1 to LARGE_BENCHMARK_SIZE do
    NewDeque.PushBack(I);
    
  EndTime := Now;
  ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
  
  // Avoid division by zero
  if ElapsedMS > 0 then
    ItemsPerSecond := LARGE_BENCHMARK_SIZE / (ElapsedMS / 1000)
  else
    ItemsPerSecond := 0;
    
  WriteLn('BenchmarkPushBackLarge: ', LARGE_BENCHMARK_SIZE, 
          ' items in ', ElapsedMS, ' ms', 
          IfThen(ElapsedMS > 0, Format(' (%.2f items/sec)', [ItemsPerSecond]), ''));
  
  AssertEquals('Count should match inserted items', LARGE_BENCHMARK_SIZE, NewDeque.Count);
end;

procedure TDequeTest.BenchmarkPushFrontSmall;
var
  StartTime, EndTime: TDateTime;
  ElapsedMS: Int64;
  I: Integer;
  NewDeque: specialize IDeque<Integer>;
  ItemsPerSecond: Double;
begin
  NewDeque := specialize TDeque<Integer>.New;
  
  StartTime := Now;
  
  for I := 1 to SMALL_BENCHMARK_SIZE do
    NewDeque.PushFront(I);
    
  EndTime := Now;
  ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
  
  // Avoid division by zero
  if ElapsedMS > 0 then
    ItemsPerSecond := SMALL_BENCHMARK_SIZE / (ElapsedMS / 1000)
  else
    ItemsPerSecond := 0;
    
  WriteLn('BenchmarkPushFrontSmall: ', SMALL_BENCHMARK_SIZE, 
          ' items in ', ElapsedMS, ' ms', 
          IfThen(ElapsedMS > 0, Format(' (%.2f items/sec)', [ItemsPerSecond]), ''));
  
  AssertEquals('Count should match inserted items', SMALL_BENCHMARK_SIZE, NewDeque.Count);
end;

procedure TDequeTest.BenchmarkPushFrontMedium;
var
  StartTime, EndTime: TDateTime;
  ElapsedMS: Int64;
  I: Integer;
  NewDeque: specialize IDeque<Integer>;
  ItemsPerSecond: Double;
begin
  NewDeque := specialize TDeque<Integer>.New;
  
  StartTime := Now;
  
  for I := 1 to MEDIUM_BENCHMARK_SIZE do
    NewDeque.PushFront(I);
    
  EndTime := Now;
  ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
  
  // Avoid division by zero
  if ElapsedMS > 0 then
    ItemsPerSecond := MEDIUM_BENCHMARK_SIZE / (ElapsedMS / 1000)
  else
    ItemsPerSecond := 0;
    
  WriteLn('BenchmarkPushFrontMedium: ', MEDIUM_BENCHMARK_SIZE, 
          ' items in ', ElapsedMS, ' ms', 
          IfThen(ElapsedMS > 0, Format(' (%.2f items/sec)', [ItemsPerSecond]), ''));
  
  AssertEquals('Count should match inserted items', MEDIUM_BENCHMARK_SIZE, NewDeque.Count);
end;

procedure TDequeTest.BenchmarkPopBackSmall;
var
  StartTime, EndTime: TDateTime;
  ElapsedMS: Int64;
  I, Value: Integer;
  NewDeque: specialize IDeque<Integer>;
  ItemsPerSecond: Double;
begin
  NewDeque := specialize TDeque<Integer>.New;
  
  // Fill the deque first
  for I := 1 to SMALL_BENCHMARK_SIZE do
    NewDeque.PushBack(I);
    
  StartTime := Now;
  
  for I := 1 to SMALL_BENCHMARK_SIZE do
    Value := NewDeque.PopBack;
    
  EndTime := Now;
  ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
  
  // Avoid division by zero
  if ElapsedMS > 0 then
    ItemsPerSecond := SMALL_BENCHMARK_SIZE / (ElapsedMS / 1000)
  else
    ItemsPerSecond := 0;
    
  WriteLn('BenchmarkPopBackSmall: ', SMALL_BENCHMARK_SIZE, 
          ' items in ', ElapsedMS, ' ms', 
          IfThen(ElapsedMS > 0, Format(' (%.2f items/sec)', [ItemsPerSecond]), ''));
  
  AssertEquals('Deque should be empty after popping', 0, NewDeque.Count);
end;

procedure TDequeTest.BenchmarkPopBackMedium;
var
  StartTime, EndTime: TDateTime;
  ElapsedMS: Int64;
  I, Value: Integer;
  NewDeque: specialize IDeque<Integer>;
  ItemsPerSecond: Double;
begin
  NewDeque := specialize TDeque<Integer>.New;
  
  // Fill the deque first
  for I := 1 to MEDIUM_BENCHMARK_SIZE do
    NewDeque.PushBack(I);
    
  StartTime := Now;
  
  for I := 1 to MEDIUM_BENCHMARK_SIZE do
    Value := NewDeque.PopBack;
    
  EndTime := Now;
  ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
  
  // Avoid division by zero
  if ElapsedMS > 0 then
    ItemsPerSecond := MEDIUM_BENCHMARK_SIZE / (ElapsedMS / 1000)
  else
    ItemsPerSecond := 0;
    
  WriteLn('BenchmarkPopBackMedium: ', MEDIUM_BENCHMARK_SIZE, 
          ' items in ', ElapsedMS, ' ms', 
          IfThen(ElapsedMS > 0, Format(' (%.2f items/sec)', [ItemsPerSecond]), ''));
  
  AssertEquals('Deque should be empty after popping', 0, NewDeque.Count);
end;

procedure TDequeTest.BenchmarkPopBackLarge;
var
  StartTime, EndTime: TDateTime;
  ElapsedMS: Int64;
  I, Value: Integer;
  NewDeque: specialize IDeque<Integer>;
  ItemsPerSecond: Double;
begin
  NewDeque := specialize TDeque<Integer>.New;
  
  // Fill the deque first
  for I := 1 to LARGE_BENCHMARK_SIZE do
    NewDeque.PushBack(I);
    
  StartTime := Now;
  
  for I := 1 to LARGE_BENCHMARK_SIZE do
    Value := NewDeque.PopBack;
    
  EndTime := Now;
  ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
  
  // Avoid division by zero
  if ElapsedMS > 0 then
    ItemsPerSecond := LARGE_BENCHMARK_SIZE / (ElapsedMS / 1000)
  else
    ItemsPerSecond := 0;
    
  WriteLn('BenchmarkPopBackLarge: ', LARGE_BENCHMARK_SIZE, 
          ' items in ', ElapsedMS, ' ms', 
          IfThen(ElapsedMS > 0, Format(' (%.2f items/sec)', [ItemsPerSecond]), ''));
  
  AssertEquals('Deque should be empty after popping', 0, NewDeque.Count);
end;

procedure TDequeTest.BenchmarkPopFrontSmall;
var
  StartTime, EndTime: TDateTime;
  ElapsedMS: Int64;
  I, Value: Integer;
  NewDeque: specialize IDeque<Integer>;
  ItemsPerSecond: Double;
begin
  NewDeque := specialize TDeque<Integer>.New;
  
  // Fill the deque first
  for I := 1 to SMALL_BENCHMARK_SIZE do
    NewDeque.PushBack(I);
    
  StartTime := Now;
  
  for I := 1 to SMALL_BENCHMARK_SIZE do
    Value := NewDeque.PopFront;
    
  EndTime := Now;
  ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
  
  // Avoid division by zero
  if ElapsedMS > 0 then
    ItemsPerSecond := SMALL_BENCHMARK_SIZE / (ElapsedMS / 1000)
  else
    ItemsPerSecond := 0;
    
  WriteLn('BenchmarkPopFrontSmall: ', SMALL_BENCHMARK_SIZE, 
          ' items in ', ElapsedMS, ' ms', 
          IfThen(ElapsedMS > 0, Format(' (%.2f items/sec)', [ItemsPerSecond]), ''));
  
  AssertEquals('Deque should be empty after popping', 0, NewDeque.Count);
end;

procedure TDequeTest.BenchmarkPopFrontMedium;
var
  StartTime, EndTime: TDateTime;
  ElapsedMS: Int64;
  I, Value: Integer;
  NewDeque: specialize IDeque<Integer>;
  ItemsPerSecond: Double;
begin
  NewDeque := specialize TDeque<Integer>.New;
  
  // Fill the deque first
  for I := 1 to MEDIUM_BENCHMARK_SIZE do
    NewDeque.PushBack(I);
    
  StartTime := Now;
  
  for I := 1 to MEDIUM_BENCHMARK_SIZE do
    Value := NewDeque.PopFront;
    
  EndTime := Now;
  ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
  
  // Avoid division by zero
  if ElapsedMS > 0 then
    ItemsPerSecond := MEDIUM_BENCHMARK_SIZE / (ElapsedMS / 1000)
  else
    ItemsPerSecond := 0;
    
  WriteLn('BenchmarkPopFrontMedium: ', MEDIUM_BENCHMARK_SIZE, 
          ' items in ', ElapsedMS, ' ms', 
          IfThen(ElapsedMS > 0, Format(' (%.2f items/sec)', [ItemsPerSecond]), ''));
  
  AssertEquals('Deque should be empty after popping', 0, NewDeque.Count);
end;

procedure TDequeTest.BenchmarkPopFrontLarge;
var
  StartTime, EndTime: TDateTime;
  ElapsedMS: Int64;
  I, Value: Integer;
  NewDeque: specialize IDeque<Integer>;
  ItemsPerSecond: Double;
begin
  NewDeque := specialize TDeque<Integer>.New;
  
  // Fill the deque first
  for I := 1 to LARGE_BENCHMARK_SIZE do
    NewDeque.PushBack(I);
    
  StartTime := Now;
  
  for I := 1 to LARGE_BENCHMARK_SIZE do
    Value := NewDeque.PopFront;
    
  EndTime := Now;
  ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
  
  // Avoid division by zero
  if ElapsedMS > 0 then
    ItemsPerSecond := LARGE_BENCHMARK_SIZE / (ElapsedMS / 1000)
  else
    ItemsPerSecond := 0;
    
  WriteLn('BenchmarkPopFrontLarge: ', LARGE_BENCHMARK_SIZE, 
          ' items in ', ElapsedMS, ' ms', 
          IfThen(ElapsedMS > 0, Format(' (%.2f items/sec)', [ItemsPerSecond]), ''));
  
  AssertEquals('Deque should be empty after popping', 0, NewDeque.Count);
end;

procedure TDequeTest.BenchmarkReverseSmall;
var
  StartTime, EndTime: TDateTime;
  ElapsedMS: Int64;
  I: Integer;
  NewDeque: specialize IDeque<Integer>;
begin
  NewDeque := specialize TDeque<Integer>.New;
  
  // Fill the deque first
  for I := 1 to SMALL_BENCHMARK_SIZE do
    NewDeque.PushBack(I);
    
  StartTime := Now;
  
  NewDeque.Reverse;
    
  EndTime := Now;
  ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
  
  WriteLn('BenchmarkReverseSmall: ', SMALL_BENCHMARK_SIZE, 
          ' items in ', ElapsedMS, ' ms');
  
  AssertEquals('First item should be last after reverse', SMALL_BENCHMARK_SIZE, NewDeque.Items[0]);
  AssertEquals('Last item should be first after reverse', 1, NewDeque.Items[SMALL_BENCHMARK_SIZE-1]);
end;

procedure TDequeTest.BenchmarkReverseMedium;
var
  StartTime, EndTime: TDateTime;
  ElapsedMS: Int64;
  I: Integer;
  NewDeque: specialize IDeque<Integer>;
begin
  NewDeque := specialize TDeque<Integer>.New;
  
  // Fill the deque first
  for I := 1 to MEDIUM_BENCHMARK_SIZE do
    NewDeque.PushBack(I);
    
  StartTime := Now;
  
  NewDeque.Reverse;
    
  EndTime := Now;
  ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
  
  WriteLn('BenchmarkReverseMedium: ', MEDIUM_BENCHMARK_SIZE, 
          ' items in ', ElapsedMS, ' ms');
  
  AssertEquals('First item should be last after reverse', MEDIUM_BENCHMARK_SIZE, NewDeque.Items[0]);
  AssertEquals('Last item should be first after reverse', 1, NewDeque.Items[MEDIUM_BENCHMARK_SIZE-1]);
end;

procedure TDequeTest.BenchmarkReverseLarge;
var
  StartTime, EndTime: TDateTime;
  ElapsedMS: Int64;
  I: Integer;
  NewDeque: specialize IDeque<Integer>;
begin
  NewDeque := specialize TDeque<Integer>.New;
  
  // Fill the deque first
  for I := 1 to LARGE_BENCHMARK_SIZE do
    NewDeque.PushBack(I);
    
  StartTime := Now;
  
  NewDeque.Reverse;
    
  EndTime := Now;
  ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
  
  WriteLn('BenchmarkReverseLarge: ', LARGE_BENCHMARK_SIZE, 
          ' items in ', ElapsedMS, ' ms');
  
  AssertEquals('First item should be last after reverse', LARGE_BENCHMARK_SIZE, NewDeque.Items[0]);
  AssertEquals('Last item should be first after reverse', 1, NewDeque.Items[LARGE_BENCHMARK_SIZE-1]);
end;

procedure TDequeTest.BenchmarkMixedOperationsSmall;
var
  StartTime, EndTime: TDateTime;
  ElapsedMS: Int64;
  I, Value: Integer;
  NewDeque: specialize IDeque<Integer>;
  OpsPerSecond: Double;
begin
  NewDeque := specialize TDeque<Integer>.New;
  
  StartTime := Now;
  
  // Mix of push/pop operations
  for I := 1 to SMALL_BENCHMARK_SIZE do
  begin
    if I mod 4 = 0 then
      NewDeque.PushFront(I)
    else
      NewDeque.PushBack(I);
      
    if (I > 10) and (I mod 10 = 0) then
    begin
      Value := NewDeque.PopFront;
      Value := NewDeque.PopBack;
    end;
  end;
    
  EndTime := Now;
  ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
  
  // Avoid division by zero
  if ElapsedMS > 0 then
    OpsPerSecond := SMALL_BENCHMARK_SIZE / (ElapsedMS / 1000)
  else
    OpsPerSecond := 0;
    
  WriteLn('BenchmarkMixedOperationsSmall: ', SMALL_BENCHMARK_SIZE, 
          ' operations in ', ElapsedMS, ' ms', 
          IfThen(ElapsedMS > 0, Format(' (%.2f ops/sec)', [OpsPerSecond]), ''));
  
  // We don't assert the count here because it depends on the mix of operations
end;

procedure TDequeTest.BenchmarkMixedOperationsMedium;
var
  StartTime, EndTime: TDateTime;
  ElapsedMS: Int64;
  I, Value: Integer;
  NewDeque: specialize IDeque<Integer>;
  OpsPerSecond: Double;
begin
  NewDeque := specialize TDeque<Integer>.New;
  
  StartTime := Now;
  
  // Mix of push/pop operations
  for I := 1 to MEDIUM_BENCHMARK_SIZE do
  begin
    if I mod 4 = 0 then
      NewDeque.PushFront(I)
    else
      NewDeque.PushBack(I);
      
    if (I > 10) and (I mod 10 = 0) then
    begin
      Value := NewDeque.PopFront;
      Value := NewDeque.PopBack;
    end;
  end;
    
  EndTime := Now;
  ElapsedMS := MilliSecondsBetween(EndTime, StartTime);
  
  // Avoid division by zero
  if ElapsedMS > 0 then
    OpsPerSecond := MEDIUM_BENCHMARK_SIZE / (ElapsedMS / 1000)
  else
    OpsPerSecond := 0;
    
  WriteLn('BenchmarkMixedOperationsMedium: ', MEDIUM_BENCHMARK_SIZE, 
          ' operations in ', ElapsedMS, ' ms', 
          IfThen(ElapsedMS > 0, Format(' (%.2f ops/sec)', [OpsPerSecond]), ''));
  
  // We don't assert the count here because it depends on the mix of operations
end;

initialization
  RegisterTest(TDequeTest);

end.
