program ArrayHelperDemo;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Generics.Defaults, TidyKit.Arrays;

type
  TIntArray = specialize TArrayHelper<Integer>;
  TPersonRecord = record
    Name: string;
    Age: Integer;
  end;
  TPersonArray = specialize TArrayHelper<TPersonRecord>;

var
  Numbers: TIntArray;
  People: TPersonArray; 
  I: Integer;
  FoundValue: Integer;
  EvenNumbers: TArray<Integer>;
  FoundPerson: TPersonRecord;
  
// Predicate method for finding even numbers
function IsEven(const Value: Integer): Boolean; of object;
begin
  Result := (Value mod 2) = 0;
end;

// Predicate method for finding adults
function IsAdult(const Person: TPersonRecord): Boolean; of object;
begin
  Result := Person.Age >= 18;
end;

// Sample procedure to demonstrate BatchUpdate
procedure AddSeveralNumbers;
var
  I: Integer;
begin
  for I := 1 to 5 do
    Numbers.Add(I * 10);
end;

begin
  try
    // Integer array demo
    WriteLn('=== Integer Array Demo ===');
    Numbers := TIntArray.Create;
    try
      // Add some numbers
      WriteLn('Adding numbers...');
      Numbers.Add(1);
      Numbers.Add(2);
      Numbers.Add(3);
      Numbers.Add(4);
      
      WriteLn('Current array contents:');
      for I := 0 to Numbers.Count - 1 do
        Write(Numbers[I], ' ');
      WriteLn;
      
      // Batch update demo
      WriteLn('Performing batch update...');
      Numbers.BatchUpdate(@AddSeveralNumbers);
      
      WriteLn('Array after batch update:');
      for I := 0 to Numbers.Count - 1 do
        Write(Numbers[I], ' ');
      WriteLn;
      
      // Find an even number
      if Numbers.Find(@IsEven, FoundValue) then
        WriteLn('Found even number: ', FoundValue)
      else
        WriteLn('No even numbers found.');
      
      // Get all even numbers
      WriteLn('All even numbers:');
      EvenNumbers := Numbers.FindAll(@IsEven);
      for I := 0 to Length(EvenNumbers) - 1 do
        Write(EvenNumbers[I], ' ');
      WriteLn;
      
      // Sort the array
      WriteLn('Sorting array...');
      Numbers.Sort(TComparer<Integer>.Default);
      
      WriteLn('Sorted array:');
      for I := 0 to Numbers.Count - 1 do
        Write(Numbers[I], ' ');
      WriteLn;
      
      // Reverse the array
      WriteLn('Reversing array...');
      Numbers.Reverse;
      
      WriteLn('Reversed array:');
      for I := 0 to Numbers.Count - 1 do
        Write(Numbers[I], ' ');
      WriteLn;
      
    finally
      Numbers.Free;
    end;
    
    // Person record array demo
    WriteLn;
    WriteLn('=== Person Array Demo ===');
    People := TPersonArray.Create;
    try
      // Add some people
      WriteLn('Adding people...');
      with FoundPerson do begin
        Name := 'Alice';
        Age := 25;
        People.Add(FoundPerson);
        
        Name := 'Bob';
        Age := 17;
        People.Add(FoundPerson);
        
        Name := 'Charlie';
        Age := 32;
        People.Add(FoundPerson);
      end;
      
      WriteLn('People in array:');
      for I := 0 to People.Count - 1 do
        WriteLn('  ', People[I].Name, ' (Age: ', People[I].Age, ')');
      
      // Find an adult
      if People.Find(@IsAdult, FoundPerson) then
        WriteLn('Found adult: ', FoundPerson.Name, ' (Age: ', FoundPerson.Age, ')')
      else
        WriteLn('No adults found.');
        
    finally
      People.Free;
    end;
    
    WriteLn;
    WriteLn('Press Enter to exit...');
    ReadLn;
    
  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
end.
