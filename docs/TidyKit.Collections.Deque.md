# TidyKit.Collections.Deque

## Overview

`TidyKit.Collections.Deque<T>` is a generic double-ended queue implementation in Object Pascal. A deque (pronounced "deck") allows efficient insertion and removal of elements at both ends. This implementation provides O(1) amortized time complexity for operations at either end of the deque.

The implementation uses a circular buffer with automatic resizing to efficiently manage elements. It supports common deque operations like pushing and popping from both ends, as well as standard collection operations like searching, iterating, and transforming.

## Features

- Generic implementation that works with any type
- Interface-based for automatic memory management
- O(1) amortized time complexity for operations at both ends
- Dynamic resizing
- Random access to elements via index
- Search operations
- Transformation methods
- Comprehensive test suite

## Usage

### Creating a Deque

There are two ways to create a deque:

```pascal
// Using the constructor
var
  MyDeque: specialize TDeque<Integer>;
begin
  MyDeque := specialize TDeque<Integer>.Create;
  try
    // Use MyDeque...
  finally
    MyDeque.Free;
  end;
end;

// Using the interface for automatic memory management
var
  MyDeque: specialize IDeque<string>;
begin
  MyDeque := specialize TDeque<string>.New;
  // Use MyDeque - no need to free it
end;
```

### Basic Operations

```pascal
// Add elements to the front and back
MyDeque.PushFront(1);  // Adds 1 to the front
MyDeque.PushBack(2);   // Adds 2 to the back

// Remove elements from the front and back
FrontValue := MyDeque.PopFront; // Removes and returns the front element
BackValue := MyDeque.PopBack;   // Removes and returns the back element

// Look at elements without removing them
FrontValue := MyDeque.PeekFront; // Returns the front element
BackValue := MyDeque.PeekBack;   // Returns the back element

// Check size
Count := MyDeque.Count;
```

### Accessing Elements

```pascal
// Access elements by index (0-based)
FirstElement := MyDeque[0];
LastElement := MyDeque[MyDeque.Count - 1];

// Modify elements by index
MyDeque[0] := NewValue;
```

### Searching

```pascal
// Check if the deque contains an element
function StringEquals(const A, B: string): Boolean;
begin
  Result := A = B;
end;

if MyDeque.Contains('apple', @StringEquals) then
  WriteLn('Found apple!');
  
// Find the index of an element
Index := MyDeque.IndexOf('banana', @StringEquals);
if Index >= 0 then
  WriteLn('Found banana at index ', Index);
```

### Transformation

```pascal
// Reverse the deque
MyDeque.Reverse;

// Convert to array
MyArray := MyDeque.ToArray;
```

### Capacity Management

```pascal
// Set capacity explicitly
MyDeque.Capacity := 100;

// Clear the deque
MyDeque.Clear;
```

## API Reference

### IDeque<T> Interface

```pascal
generic IDeque<T> = interface
  ['{5FC7B4A2-E1D7-4B9D-8C55-F2D8A3712B45}']
  // Property getters and setters
  function GetCapacity: Integer;
  procedure SetCapacity(Value: Integer);
  function GetCount: Integer;
  function GetItem(Index: Integer): T;
  procedure SetItem(Index: Integer; const Value: T);
  
  // Add/remove operations at both ends
  procedure PushFront(const Value: T);
  procedure PushBack(const Value: T);
  function PopFront: T;
  function PopBack: T;
  function PeekFront: T;
  function PeekBack: T;
  
  // Standard collection operations
  procedure Clear;
  function ToArray: specialize TArray<T>;
  
  // Search operations
  function Contains(const Value: T; EqualityFunc: specialize TEqualityFunc<T>): Boolean;
  function IndexOf(const Value: T; EqualityFunc: specialize TEqualityFunc<T>): Integer;
  
  // Transformation methods
  procedure Reverse;
  
  // Properties
  property Count: Integer read GetCount;
  property Capacity: Integer read GetCapacity write SetCapacity;
  property Items[Index: Integer]: T read GetItem write SetItem; default;
end;
```

### TDeque<T> Methods

| Method | Description | Complexity |
|--------|-------------|------------|
| `Create` | Creates a new empty deque. | O(1) |
| `PushFront(Value: T)` | Adds an element to the front of the deque. | O(1)* |
| `PushBack(Value: T)` | Adds an element to the back of the deque. | O(1)* |
| `PopFront: T` | Removes and returns the front element. | O(1) |
| `PopBack: T` | Removes and returns the back element. | O(1) |
| `PeekFront: T` | Returns the front element without removing it. | O(1) |
| `PeekBack: T` | Returns the back element without removing it. | O(1) |
| `Clear` | Removes all elements from the deque. | O(1) |
| `Contains(Value: T, EqualityFunc): Boolean` | Checks if the deque contains the specified value. | O(n) |
| `IndexOf(Value: T, EqualityFunc): Integer` | Returns the index of the first occurrence of the specified value. | O(n) |
| `Reverse` | Reverses the order of elements in the deque. | O(n) |
| `ToArray: TArray<T>` | Returns an array containing all elements in the deque. | O(n) |
| `New: IDeque<T>` | Static factory method that creates a new deque wrapped in an interface. | O(1) |

\* Amortized - may require occasional resizing which is O(n)

### Properties

| Property | Description | Get Complexity | Set Complexity |
|----------|-------------|----------------|----------------|
| `Count: Integer` | Number of elements in the deque. | O(1) | - |
| `Capacity: Integer` | Current capacity of the deque. When setting, if the new value is less than the current count, the capacity will be adjusted to at least match the count. | O(1) | O(n) |
| `Items[Index: Integer]: T` | Gets or sets the element at the specified logical index. | O(1) | O(1) |

### Function Types

```pascal
generic TCompareFunc<T> = function(const A, B: T): Integer;
generic TEqualityFunc<T> = function(const A, B: T): Boolean;
generic TPredicateFunc<T> = function(const Item: T): Boolean;
```

### Helper Function

```pascal
generic function CreateDeque<T>: specialize IDeque<T>;
```
Creates and returns a new deque wrapped in an interface.

## Implementation Details

The deque is implemented using a circular buffer (array-based) with two indices: `FHead` points to the first element, and `FTail` points one past the last element. When elements are added or removed, these indices are adjusted accordingly. When the buffer becomes full, it is resized to accommodate more elements.

The circular buffer implementation provides efficient use of memory by allowing elements to wrap around the physical array. This means that internally, a logical index in the deque is mapped to a physical array position using modular arithmetic. The `GetRealIndex` method handles this translation, making element access O(1) despite the circular arrangement.

Key aspects of the implementation:

- Initial capacity is 0, but grows to 4 when the first element is added
- When capacity is exceeded, the buffer size is doubled
- Clear operation resets capacity to 0, releasing memory
- Proper exception handling for empty deque operations and out-of-bounds access
- When resizing, elements are rearranged to start from index 0 in the new array

This implementation ensures efficient operations at both ends of the deque while maintaining good memory utilization.

## Performance Characteristics

- Adding/removing elements at either end: O(1) amortized
- Accessing elements by index: O(1)
- Searching: O(n)
- Reversal: O(n)
- Memory usage: O(n) where n is the number of elements

## Examples

### Using the Deque as a Queue

```pascal
var
  Queue: specialize IDeque<Integer>;
begin
  Queue := specialize TDeque<Integer>.New;
  
  // Enqueue elements (add to back)
  Queue.PushBack(1);
  Queue.PushBack(2);
  Queue.PushBack(3);
  
  // Dequeue elements (remove from front)
  WriteLn(Queue.PopFront); // Outputs: 1
  WriteLn(Queue.PopFront); // Outputs: 2
  WriteLn(Queue.PopFront); // Outputs: 3
end;
```

### Using the Deque as a Stack

```pascal
var
  Stack: specialize IDeque<Integer>;
begin
  Stack := specialize TDeque<Integer>.New;
  
  // Push elements (add to back)
  Stack.PushBack(1);
  Stack.PushBack(2);
  Stack.PushBack(3);
  
  // Pop elements (remove from back)
  WriteLn(Stack.PopBack); // Outputs: 3
  WriteLn(Stack.PopBack); // Outputs: 2
  WriteLn(Stack.PopBack); // Outputs: 1
end;
```

### Using the Deque for Sliding Window Operations

```pascal
var
  Window: specialize IDeque<Integer>;
  I: Integer;
begin
  Window := specialize TDeque<Integer>.New;
  
  // Process a sliding window of size 3
  for I := 1 to 10 do
  begin
    // Add new element
    Window.PushBack(I);
    
    // Keep window size at 3
    if Window.Count > 3 then
      Window.PopFront;
      
    // Process current window
    Write('Window:');
    for J := 0 to Window.Count - 1 do
      Write(' ', Window[J]);
    WriteLn;
  end;
end;
```
