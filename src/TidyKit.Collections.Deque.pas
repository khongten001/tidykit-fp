unit TidyKit.Collections.Deque;

{$mode objfpc}{$H+}{$J-}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils;

type
  { Function pointer types for comparison and predicate operations
    Note: These are function pointers that can accept standalone functions.
    For method pointers (class methods), you'll need to use the appropriate syntax.
  }
  generic TCompareFunc<T> = function(const A, B: T): Integer;
  generic TEqualityFunc<T> = function(const A, B: T): Boolean;
  generic TPredicateFunc<T> = function(const Item: T): Boolean;
  
  { Interface for automatic memory management }
  generic IDeque<T> = interface
    ['{5FC7B4A2-E1D7-4B9D-8C55-F2D8A3712B45}']
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    function GetCount: Integer;
    function GetItem(Index: Integer): T;
    procedure SetItem(Index: Integer; const Value: T);
    
    { Add/remove operations at both ends }
    procedure PushFront(const Value: T);
    procedure PushBack(const Value: T);
    function PopFront: T;
    function PopBack: T;
    function PeekFront: T;
    function PeekBack: T;
    
    { Standard collection operations }
    procedure Clear;
    function ToArray: specialize TArray<T>;
    
    { Search operations }
    function Contains(const Value: T; EqualityFunc: specialize TEqualityFunc<T>): Boolean;
    function IndexOf(const Value: T; EqualityFunc: specialize TEqualityFunc<T>): Integer;
    
    { Transformation methods }
    procedure Reverse;
    
    property Count: Integer read GetCount;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Items[Index: Integer]: T read GetItem write SetItem; default;
  end;
  
  { A full-featured generic deque implementation }
  generic TDeque<T> = class(TInterfacedObject, specialize IDeque<T>)
  private
    FArray: array of T;
    FHead: Integer;   // Index of the first element
    FTail: Integer;   // Index one past the last element
    FCount: Integer;  // Current element count
    
    { Helper methods }
    procedure Grow;
    procedure MoveNext(var Index: Integer);
    procedure MovePrev(var Index: Integer);
    function GetRealIndex(LogicalIndex: Integer): Integer;
    
    { Property getters/setters }
    function GetCount: Integer;
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    function GetItem(Index: Integer): T;
    procedure SetItem(Index: Integer; const Value: T);
  public
    constructor Create;
    destructor Destroy; override;
    
    { Add/remove operations at both ends }
    procedure PushFront(const Value: T);
    procedure PushBack(const Value: T);
    function PopFront: T;
    function PopBack: T;
    function PeekFront: T;
    function PeekBack: T;
    
    { Standard collection operations }
    procedure Clear;
    function ToArray: specialize TArray<T>;
    
    { Search operations }
    function Contains(const Value: T; EqualityFunc: specialize TEqualityFunc<T>): Boolean;
    function IndexOf(const Value: T; EqualityFunc: specialize TEqualityFunc<T>): Integer;
    
    { Transformation methods }
    procedure Reverse;
    
    property Count: Integer read GetCount;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Items[Index: Integer]: T read GetItem write SetItem; default;
    
    { Static factory method for convenient creation }
    class function New: specialize IDeque<T>; static;
  end;
  
  { Helper function to simplify deque creation with automatic memory management }
  generic function CreateDeque<T>: specialize IDeque<T>;

implementation

{ TDeque<T> }

constructor TDeque.Create;
begin
  inherited Create;
  FHead := 0;
  FTail := 0;
  FCount := 0;
  SetLength(FArray, 0);
end;

destructor TDeque.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TDeque.Grow;
var
  NewCapacity, I, OldIndex, NewIndex: Integer;
  NewArray: array of T;
begin
  if Length(FArray) = 0 then
    NewCapacity := 4
  else
    NewCapacity := Length(FArray) * 2;
    
  SetLength(NewArray, NewCapacity);
  
  // Copy elements to the new array at the beginning
  for I := 0 to FCount - 1 do
  begin
    OldIndex := GetRealIndex(I);
    NewIndex := I; // In the new array, we start from 0
    NewArray[NewIndex] := FArray[OldIndex];
  end;
  
  // Replace the old array with the new one
  FArray := NewArray;
  FHead := 0;
  FTail := FCount;
end;

function TDeque.GetRealIndex(LogicalIndex: Integer): Integer;
begin
  if (LogicalIndex < 0) or (LogicalIndex >= FCount) then
    raise ERangeError.CreateFmt('Index out of bounds: %d', [LogicalIndex]);
    
  Result := (FHead + LogicalIndex) mod Length(FArray);
end;

procedure TDeque.MoveNext(var Index: Integer);
begin
  if Length(FArray) = 0 then
    Index := 0
  else
    Index := (Index + 1) mod Length(FArray);
end;

procedure TDeque.MovePrev(var Index: Integer);
begin
  if Length(FArray) = 0 then
    Index := 0
  else if Index = 0 then
    Index := Length(FArray) - 1
  else
    Dec(Index);
end;

function TDeque.GetCount: Integer;
begin
  Result := FCount;
end;

function TDeque.GetCapacity: Integer;
begin
  Result := Length(FArray);
end;

procedure TDeque.SetCapacity(Value: Integer);
var
  NewArray: array of T;
  I, OldIndex, NewIndex: Integer;
begin
  if Value = Length(FArray) then
    Exit;
    
  if Value < FCount then
    Value := FCount;
    
  if Value = 0 then
  begin
    SetLength(FArray, 0);
    FHead := 0;
    FTail := 0;
    Exit;
  end;
  
  SetLength(NewArray, Value);
  
  // Copy elements to the new array
  for I := 0 to FCount - 1 do
  begin
    OldIndex := GetRealIndex(I);
    NewIndex := I; // In the new array, we'll reindex from 0
    NewArray[NewIndex] := FArray[OldIndex];
  end;
  
  FArray := NewArray;
  FHead := 0;
  FTail := FCount;
end;

function TDeque.GetItem(Index: Integer): T;
begin
  Result := FArray[GetRealIndex(Index)];
end;

procedure TDeque.SetItem(Index: Integer; const Value: T);
begin
  FArray[GetRealIndex(Index)] := Value;
end;

procedure TDeque.PushFront(const Value: T);
begin
  if Length(FArray) = 0 then
  begin
    SetLength(FArray, 4); // Initialize with a minimum capacity
    FHead := 0;
    FTail := 0;
  end
  else if FCount = Length(FArray) then
    Grow;
    
  MovePrev(FHead);
  FArray[FHead] := Value;
  Inc(FCount);
end;

procedure TDeque.PushBack(const Value: T);
begin
  if Length(FArray) = 0 then
  begin
    SetLength(FArray, 4); // Initialize with a minimum capacity
    FHead := 0;
    FTail := 0;
  end
  else if FCount = Length(FArray) then
    Grow;
    
  FArray[FTail] := Value;
  MoveNext(FTail);
  Inc(FCount);
end;

function TDeque.PopFront: T;
begin
  if FCount = 0 then
    raise Exception.Create('Cannot pop from an empty deque');
    
  Result := FArray[FHead];
  MoveNext(FHead);
  Dec(FCount);
end;

function TDeque.PopBack: T;
begin
  if FCount = 0 then
    raise Exception.Create('Cannot pop from an empty deque');
    
  MovePrev(FTail);
  Result := FArray[FTail];
  Dec(FCount);
end;

function TDeque.PeekFront: T;
begin
  if FCount = 0 then
    raise Exception.Create('Cannot peek from an empty deque');
    
  Result := FArray[FHead];
end;

function TDeque.PeekBack: T;
var
  Index: Integer;
begin
  if FCount = 0 then
    raise Exception.Create('Cannot peek from an empty deque');
    
  Index := FTail;
  MovePrev(Index);
  Result := FArray[Index];
end;

procedure TDeque.Clear;
begin
  SetLength(FArray, 0);
  FHead := 0;
  FTail := 0;
  FCount := 0;
end;

function TDeque.ToArray: specialize TArray<T>;
var
  I: Integer;
begin
  SetLength(Result, FCount);
  
  for I := 0 to FCount - 1 do
    Result[I] := GetItem(I);
end;

function TDeque.Contains(const Value: T; EqualityFunc: specialize TEqualityFunc<T>): Boolean;
begin
  Result := IndexOf(Value, EqualityFunc) >= 0;
end;

function TDeque.IndexOf(const Value: T; EqualityFunc: specialize TEqualityFunc<T>): Integer;
var
  I, RealIndex: Integer;
begin
  Result := -1;
  
  for I := 0 to FCount - 1 do
  begin
    RealIndex := GetRealIndex(I);
    if EqualityFunc(FArray[RealIndex], Value) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

procedure TDeque.Reverse;
var
  Left, Right: Integer;
  Temp: T;
begin
  if FCount <= 1 then
    Exit;
    
  Left := 0;
  Right := FCount - 1;
  
  while Left < Right do
  begin
    Temp := GetItem(Left);
    SetItem(Left, GetItem(Right));
    SetItem(Right, Temp);
    
    Inc(Left);
    Dec(Right);
  end;
end;

class function TDeque.New: specialize IDeque<T>;
begin
  Result := Create;
end;

generic function CreateDeque<T>: specialize IDeque<T>;
begin
  Result := specialize TDeque<T>.New;
end;

end.
