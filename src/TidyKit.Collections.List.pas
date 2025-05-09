unit TidyKit.Collections.List;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils;

type
  { Function pointer types for comparison and predicate operations }
  generic TCompareFunc<T> = function(const A, B: T): Integer;
  generic TEqualityFunc<T> = function(const A, B: T): Boolean;
  generic TPredicateFunc<T> = function(const Item: T): Boolean;

  { Interface for automatic memory management }
  generic IList<T> = interface
    ['{EDCB14B2-D9F7-4AAB-B5D8-FB311E643233}']
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    function GetCount: Integer;
    function GetItem(Index: Integer): T;
    procedure SetItem(Index: Integer; const Value: T);
    
    function Add(const Value: T): Integer;
    procedure AddRange(const Values: array of T);
    procedure Insert(Index: Integer; const Value: T);
    procedure Delete(Index: Integer);
    function Remove(const Value: T; EqualityFunc: specialize TEqualityFunc<T>): Boolean;
    procedure Clear;
    
    function IndexOf(const Value: T; EqualityFunc: specialize TEqualityFunc<T>): Integer;
    function Contains(const Value: T; EqualityFunc: specialize TEqualityFunc<T>): Boolean;
    
    function Find(Predicate: specialize TPredicateFunc<T>; out FoundValue: T): Boolean;
    function FindAll(Predicate: specialize TPredicateFunc<T>): specialize TArray<T>;
    
    procedure Sort(CompareFunc: specialize TCompareFunc<T>);
    procedure Reverse;
    function Slice(StartIndex: Integer; Count: Integer): specialize TArray<T>;
    function ToArray: specialize TArray<T>;
    
    property Count: Integer read GetCount;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Items[Index: Integer]: T read GetItem write SetItem; default;
  end;
  
  { A full-featured generic list implementation compatible with FPC 3.2.2 }
  generic TList<T> = class(TInterfacedObject, specialize IList<T>)
  private
    FArray: array of T;
    FCount: Integer; // The number of actual elements in the array
    function GetCount: Integer;
    function GetItem(Index: Integer): T;
    procedure SetItem(Index: Integer; const Value: T);
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    
    // Basic operations
    function Add(const Value: T): Integer;
    procedure AddRange(const Values: array of T);
    procedure Insert(Index: Integer; const Value: T);
    procedure Delete(Index: Integer);
    function Remove(const Value: T; EqualityFunc: specialize TEqualityFunc<T>): Boolean;
    procedure Clear;
    
    // Search operations
    function IndexOf(const Value: T; EqualityFunc: specialize TEqualityFunc<T>): Integer;
    function Contains(const Value: T; EqualityFunc: specialize TEqualityFunc<T>): Boolean;
    
    // Find with predicate
    function Find(Predicate: specialize TPredicateFunc<T>; out FoundValue: T): Boolean;
    function FindAll(Predicate: specialize TPredicateFunc<T>): specialize TArray<T>;
    
    // Transformation methods
    procedure Sort(CompareFunc: specialize TCompareFunc<T>);
    procedure Reverse;
    function Slice(StartIndex: Integer; Count: Integer): specialize TArray<T>;
    
    // Access to underlying array
    function ToArray: specialize TArray<T>;
    
    // Properties
    property Count: Integer read GetCount;      // Number of actual elements in the array
    property Capacity: Integer read GetCapacity write SetCapacity; // Allocated memory space
    property Items[Index: Integer]: T read GetItem write SetItem; default;
    
    { Static factory method for convenient creation }
    class function New: specialize IList<T>; static;
  end;
  
  { Helper function to simplify list creation with automatic memory management }
  generic function CreateList<T>: specialize IList<T>;

implementation

{ TList<T> }

constructor TList.Create;
begin
  inherited Create;
  SetLength(FArray, 0);
  FCount := 0;
end;

destructor TList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TList.GetCount: Integer;
begin
  Result := FCount;
end;

function TList.GetItem(Index: Integer): T;
begin
  if (Index < 0) or (Index >= FCount) then
    raise ERangeError.CreateFmt('Index out of bounds: %d', [Index]);
  Result := FArray[Index];
end;

procedure TList.SetItem(Index: Integer; const Value: T);
begin
  if (Index < 0) or (Index >= FCount) then
    raise ERangeError.CreateFmt('Index out of bounds: %d', [Index]);
  FArray[Index] := Value;
end;

function TList.GetCapacity: Integer;
begin
  Result := Length(FArray);
end;

procedure TList.SetCapacity(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
    
  if Value < FCount then
    FCount := Value;
    
  SetLength(FArray, Value);
end;

function TList.Add(const Value: T): Integer;
begin
  if FCount = Length(FArray) then // Check if FCount has reached current capacity
  begin
    if Length(FArray) = 0 then
      SetCapacity(4) // Default initial capacity, e.g., 4
    else
      SetCapacity(Length(FArray) * 2); // Double the capacity
  end;
  
  FArray[FCount] := Value;
  Result := FCount; // Index of the new item
  Inc(FCount);
end;

procedure TList.AddRange(const Values: array of T);
var
  OldLen, I: Integer;
begin
  if Length(Values) = 0 then
    Exit;
    
  OldLen := FCount;
  
  SetCapacity(FCount + Length(Values));
  
  for I := 0 to High(Values) do
    FArray[OldLen + I] := Values[I];
    
  FCount := FCount + Length(Values);
end;

procedure TList.Insert(Index: Integer; const Value: T);
var
  I: Integer;
begin
  if (Index < 0) or (Index > FCount) then
    raise ERangeError.CreateFmt('Insert index out of bounds: %d', [Index]);
  
  SetCapacity(FCount + 1);
  
  for I := FCount downto Index + 1 do
    FArray[I] := FArray[I - 1];
  
  FArray[Index] := Value;
  Inc(FCount);
end;

procedure TList.Delete(Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= FCount) then
    raise ERangeError.CreateFmt('Delete index out of bounds: %d', [Index]);
  
  for I := Index to FCount - 2 do
    FArray[I] := FArray[I + 1];
  
  Dec(FCount);
end;

function TList.Remove(const Value: T; EqualityFunc: specialize TEqualityFunc<T>): Boolean;
var
  Idx: Integer;
begin
  Result := False;
  Idx := IndexOf(Value, EqualityFunc);
  if Idx >= 0 then
  begin
    Delete(Idx);
    Result := True;
  end;
end;

procedure TList.Clear;
begin
  SetLength(FArray, 0);
  FCount := 0;
end;

function TList.IndexOf(const Value: T; EqualityFunc: specialize TEqualityFunc<T>): Integer;
var
  I: Integer;
begin
  Result := -1;
  
  for I := 0 to FCount - 1 do
    if EqualityFunc(FArray[I], Value) then
    begin
      Result := I;
      Break;
    end;
end;

function TList.Contains(const Value: T; EqualityFunc: specialize TEqualityFunc<T>): Boolean;
begin
  Result := IndexOf(Value, EqualityFunc) >= 0;
end;

function TList.Find(Predicate: specialize TPredicateFunc<T>; out FoundValue: T): Boolean;
var
  I: Integer;
begin
  Result := False;
  
  for I := 0 to FCount - 1 do
    if Predicate(FArray[I]) then
    begin
      FoundValue := FArray[I];
      Result := True;
      Break;
    end;
end;

function TList.FindAll(Predicate: specialize TPredicateFunc<T>): specialize TArray<T>;
var
  I, ResultIndex: Integer;
  TempArray: array of T;
begin
  SetLength(TempArray, Count);
  ResultIndex := 0;
  
  for I := 0 to Count - 1 do
  begin
    if Predicate(FArray[I]) then
    begin
      TempArray[ResultIndex] := FArray[I];
      Inc(ResultIndex);
    end;
  end;
  
  SetLength(Result, ResultIndex);
  for I := 0 to ResultIndex - 1 do
    Result[I] := TempArray[I];
end;

procedure TList.Sort(CompareFunc: specialize TCompareFunc<T>);
  
  procedure QuickSort(var A: array of T; iLo, iHi: Integer);
  var
    Lo, Hi: Integer;
    Mid, Temp: T;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid := A[(Lo + Hi) div 2];
    
    repeat
      while CompareFunc(A[Lo], Mid) < 0 do Inc(Lo);
      while CompareFunc(A[Hi], Mid) > 0 do Dec(Hi);
      
      if Lo <= Hi then
      begin
        Temp := A[Lo];
        A[Lo] := A[Hi];
        A[Hi] := Temp;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    
    if iLo < Hi then QuickSort(A, iLo, Hi);
    if Lo < iHi then QuickSort(A, Lo, iHi);
  end;
  
begin
  if FCount > 1 then
    QuickSort(FArray, 0, FCount - 1);
end;

procedure TList.Reverse;
var
  I, J: Integer;
  Temp: T;
begin
  I := 0;
  J := FCount - 1;
  while I < J do
  begin
    Temp := FArray[I];
    FArray[I] := FArray[J];
    FArray[J] := Temp;
    Inc(I);
    Dec(J);
  end;
end;

function TList.Slice(StartIndex: Integer; Count: Integer): specialize TArray<T>;
var
  I, EndIndex, ActualCount: Integer;
begin
  SetLength(Result, 0);
  
  if StartIndex < 0 then
    StartIndex := 0;
    
  if StartIndex >= FCount then
    Exit;
  
  EndIndex := StartIndex + Count - 1;
  if (Count <= 0) or (EndIndex >= FCount) then
    EndIndex := FCount - 1;
    
  ActualCount := EndIndex - StartIndex + 1;
  SetLength(Result, ActualCount);
  
  for I := 0 to ActualCount - 1 do
    Result[I] := FArray[StartIndex + I];
end;

function TList.ToArray: specialize TArray<T>;
var
  I: Integer;
begin
  SetLength(Result, 0);
  
  SetLength(Result, FCount);
  for I := 0 to FCount - 1 do
    Result[I] := FArray[I];
end;

class function TList.New: specialize IList<T>;
begin
  Result := Create;
end;

generic function CreateList<T>: specialize IList<T>;
begin
  Result := specialize TList<T>.Create;
end;

end.