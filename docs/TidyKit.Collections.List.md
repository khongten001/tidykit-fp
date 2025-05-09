# TidyKit.Collections.List

## Overview

TidyKit.Collections.List provides a full-featured generic list implementation for FreePascal 3.2.2+. It offers similar functionality to the .NET `List<T>` class or C++ `std::vector`, allowing you to work with dynamically-sized collections of any type.

The implementation supports automatic memory management through interfaces, while providing advanced features like sorting, filtering, slicing, and more.

## Key Features

- Dynamic resizing with efficient capacity management
- Generic implementation supporting any type
- Rich set of operations (add, insert, delete, search, sort, etc.)
- Automatic memory management when using the interface version
- Predicate-based operations (`Find`, `FindAll`)
- Excellent performance metrics even with large collections

## Getting Started

### Basic Usage

```pascal
uses
  TidyKit.Collections.List;

// Manual memory management:
var
  MyList: TList<Integer>;
begin
  MyList := TList<Integer>.Create;
  try
    MyList.Add(42);
    MyList.Add(123);
    // Use the list...
    WriteLn('First item: ', MyList[0]);
    WriteLn('Count: ', MyList.Count);
  finally
    MyList.Free;  // Manual cleanup required
  end;
end;

// Automatic memory management (recommended):
var
  MyList: IList<Integer>;
begin
  // The New class method creates and returns a TList<Integer> as IList<Integer>
  MyList := TList<Integer>.New;
  MyList.Add(42);
  MyList.Add(123);
  // Use the list...
  WriteLn('First item: ', MyList[0]);
  WriteLn('Count: ', MyList.Count);
  // No Free needed - automatic cleanup when MyList goes out of scope
end;
```

### Using Custom Types

```pascal
type
  TPoint = record
    X, Y: Integer;
  end;

function PointEquals(const A, B: TPoint): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y);
end;

function ComparePointsByX(const A, B: TPoint): Integer;
begin
  if A.X < B.X then
    Result := -1
  else if A.X > B.X then
    Result := 1
  else
    Result := 0;
end;

var
  Points: IList<TPoint>;
  P1, P2, Found: TPoint;
begin
  Points := TList<TPoint>.New;
  
  // Create and add points
  P1.X := 10; P1.Y := 20;
  P2.X := 30; P2.Y := 40;
  
  Points.Add(P1);
  Points.Add(P2);
  
  // Search for a point
  if Points.Contains(P1, @PointEquals) then
    WriteLn('Point found');
  
  // Sort points by X coordinate
  Points.Sort(@ComparePointsByX);
end;
```

## API Reference

### Core Types

```pascal
generic TCompareFunc<T> = function(const A, B: T): Integer;
generic TEqualityFunc<T> = function(const A, B: T): Boolean;
generic TPredicateFunc<T> = function(const Item: T): Boolean;

generic IList<T> = interface
  // Interface definition for automatic memory management
  // ...methods and properties...
end;

generic TList<T> = class(TInterfacedObject, specialize IList<T>)
  // Implementation of the IList interface
  // ...methods and properties...
end;
```

### Properties

| Property | Description |
|----------|-------------|
| `Count: Integer` | Gets the number of elements in the list. |
| `Capacity: Integer` | Gets or sets the capacity (allocated size) of the list. When setting, if the new capacity is less than the current count, the list will be truncated. |
| `Items[Index: Integer]: T` | Default indexed property for accessing elements. |

### Construction and Destruction

| Method | Description |
|--------|-------------|
| `Create` | Creates a new empty list |
| `New: IList<T>` | Static method that creates a new list with interface-based memory management |

### Adding Elements

| Method | Description |
|--------|-------------|
| `Add(const Value: T): Integer` | Adds an item to the end of the list and returns its index |
| `AddRange(const Values: array of T)` | Adds multiple items to the end of the list |
| `Insert(Index: Integer; const Value: T)` | Inserts an item at the specified index |

### Removing Elements

| Method | Description |
|--------|-------------|
| `Delete(Index: Integer)` | Removes the element at the specified index |
| `Remove(const Value: T; EqualityFunc: TEqualityFunc<T>): Boolean` | Removes the first occurrence of a specific element |
| `Clear` | Removes all elements from the list |

### Searching and Querying

| Method | Description |
|--------|-------------|
| `IndexOf(const Value: T; EqualityFunc: TEqualityFunc<T>): Integer` | Finds the index of the first occurrence of a value |
| `Contains(const Value: T; EqualityFunc: TEqualityFunc<T>): Boolean` | Determines if the list contains a specific value |
| `Find(Predicate: TPredicateFunc<T>; out FoundValue: T): Boolean` | Finds the first element that matches a predicate |
| `FindAll(Predicate: TPredicateFunc<T>): TArray<T>` | Returns all elements that match a predicate |

### Transformations

| Method | Description |
|--------|-------------|
| `Sort(CompareFunc: TCompareFunc<T>)` | Sorts the elements using the provided comparison function (QuickSort algorithm). |
| `Reverse` | Reverses the order of elements in the list. |
| `Slice(StartIndex: Integer; Count: Integer): TArray<T>` | Extracts a portion of the list |
| `ToArray: TArray<T>` | Creates a copy of the list as a standard array |

## Performance

The TidyKit.Collections.List implementation offers excellent performance characteristics, especially after optimizations to its growth strategy:

| Operation          | 10,000 elements | 100,000 elements | 1,000,000 elements | Complexity |
|--------------------|-----------------|------------------|--------------------|------------|
| Add (amortized)    | <0.001s         | <0.001s          | ~0.023s            | O(1)*      |
| Search (beginning) | <0.001s         | <0.001s          | <0.001s            | O(1)       |
| Search (middle)    | <0.001s         | <0.001s          | ~0.003s            | O(n)       |
| Search (end)       | <0.001s         | <0.001s          | ~0.005s            | O(n)       |
| Sort               | <0.001s         | ~0.031s          | ~0.336s            | O(n log n) |
| Reverse            | <0.001s         | <0.001s          | ~0.016s            | O(n)       |

*\* Amortized complexity. Timings are illustrative and may vary based on system and conditions. The "Add" operation's performance reflects a growth strategy that doubles capacity when needed.*

## Implementation Details

The list is implemented using a dynamic array.

Key aspects of the capacity management and implementation:
- **Initial Capacity:** The list starts with a capacity of 0.
- **Growth Strategy:** When an `Add` operation causes the count to reach the current capacity:
    - If the capacity was 0, it grows to 4.
    - Otherwise, the capacity is doubled.
  This ensures amortized O(1) time complexity for `Add` operations.
- **`AddRange` Capacity:** The `AddRange` method calculates the exact required capacity and resizes once if necessary.
- **`Insert` Capacity:** The `Insert` method increases capacity by 1 if `FCount + 1` exceeds current capacity. This is typically O(n) due to element shifting, and the single capacity increment is minor in comparison.
- **`SetCapacity` Behavior:**
    - If the new capacity is less than the current `FCount`, `FCount` is truncated to the new capacity.
    - Setting capacity to 0 effectively clears the list and releases its internal array.
- **`Clear` Operation:** Resets `FCount` to 0 and sets the internal array length to 0, releasing its memory.
- **Sorting:** Uses a QuickSort algorithm.

## Capacity Management

```pascal
var
  List: IList<Integer>;
begin
  List := TList<Integer>.New;
  
  // Pre-allocate space for 1000 items
  List.Capacity := 1000;
  
  // Add items without causing repeated reallocations
  for I := 1 to 1000 do
    List.Add(I);
    
  // Trim excess capacity
  List.Capacity := List.Count;
end;
```

## Best Practices

1. **Use interface-based memory management** when possible to avoid memory leaks.
2. **Pre-allocate capacity** when you know approximately how many items you'll add.
3. **Provide appropriate equality and comparison functions** for your custom types.
4. **Consider performance implications** when working with very large lists.
5. **Use predicate functions** for more complex filtering operations.

## Example: Filtering and Sorting

```pascal
function IsEven(const Value: Integer): Boolean; 
begin
  Result := Value mod 2 = 0;
end;

function CompareDescending(const A, B: Integer): Integer;
begin
  if A > B then 
    Result := -1
  else if A < B then 
    Result := 1
  else 
    Result := 0;
end;

var
  List: IList<Integer>;
  EvenNumbers: TArray<Integer>;
  I: Integer;
begin
  List := TList<Integer>.New;
  
  // Add some numbers
  for I := 1 to 10 do
    List.Add(I);
    
  // Find all even numbers
  EvenNumbers := List.FindAll(@IsEven);
  
  // Sort the list in descending order
  List.Sort(@CompareDescending);
end;
```
