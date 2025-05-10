# TidyKit.Collections.HashSet

The `TidyKit.Collections.HashSet` unit provides a generic hash set collection, `THashSet<T>`. This collection is designed to store unique elements and offers high-performance operations for adding, removing, and checking for the existence of elements (contains). It is optimized for scenarios where fast lookups, additions, and deletions of unique items are critical.

## Key Features

*   **Stores Unique Elements**: Automatically ensures that all elements in the set are unique based on the provided equality function.
*   **High Performance**: Provides average O(1) time complexity for `Add`, `Remove`, and `Contains` operations, assuming a good hash function that distributes elements evenly.
*   **Generic**: Can be used with any data type `T`.
*   **Customizable Hashing and Equality**: Allows users to provide custom functions for hashing elements and checking their equality. This is crucial for correct behavior with custom types.
*   **Interface-Based**: Implements `IHashSet<T>`, enabling usage with interfaces for automatic memory management (ARC) and polymorphism.
*   **Dynamic Resizing**: Automatically resizes its internal storage to maintain performance as the number of elements grows, based on a configurable load factor.

## Generic Type Parameters

*   `T`: The type of elements stored in the hash set.

## Delegate Types

These function pointer types are essential for the `THashSet<T>` to operate correctly with the elements of type `T`.

### `generic THashFunc<T> = function(const Value: T): Integer;`

A function pointer type used to generate a hash code for an element of type `T`. A good hash function should produce a wide distribution of hash codes for different inputs to minimize collisions.

*   **Parameters**:
    *   `Value: T`: The element to hash.
*   **Returns**: `Integer` - The hash code for the element. The hash set implementation will handle negative hash codes by taking the absolute value.

### `generic TEqualityFunc<T> = function(const A, B: T): Boolean;`

A function pointer type used to compare two elements of type `T` for equality. This function is used to determine if an element already exists in the set and during removal operations.

*   **Parameters**:
    *   `A: T`: The first element.
    *   `B: T`: The second element.
*   **Returns**: `Boolean` - `True` if the elements are considered equal, `False` otherwise.

## Interface: `generic IHashSet<T>`

Defines the contract for a generic hash set, promoting abstraction and enabling automatic reference counting (ARC).

```pascal
generic IHashSet<T> = interface
  ['{7A5C1D3E-8B0F-42A9-9D61-0C8F3B276E4F}']
  function GetCount: Integer;
  function Add(const Value: T): Boolean;
  function Remove(const Value: T): Boolean;
  function Contains(const Value: T): Boolean;
  procedure Clear;
  function ToArray: specialize TArray<T>;
  property Count: Integer read GetCount;
end;
```

The members of the `IHashSet<T>` interface are summarized below:

| Member         | Type / Signature                          | Description                                                                                                |
|----------------|-------------------------------------------|------------------------------------------------------------------------------------------------------------|
| `Count`        | `property Count: Integer read GetCount`   | Gets the number of unique elements currently stored in the hash set.                                       |
| `Add`          | `function Add(const Value: T): Boolean`   | Adds `Value` to the set if it is not already present. Returns `True` if the element was added, `False` if it was already present. |
| `Remove`       | `function Remove(const Value: T): Boolean`| Removes `Value` from the set. Returns `True` if the element was successfully found and removed, `False` if the element was not found. |
| `Contains`     | `function Contains(const Value: T): Boolean`| Determines whether the set contains `Value`. Returns `True` if the element is found, `False` otherwise.      |
| `Clear`        | `procedure Clear`                         | Removes all elements from the set, resetting its count to zero.                                            |
| `ToArray`      | `function ToArray: specialize TArray<T>`  | Copies the elements of the set to a new dynamic array of type `T`. The order of elements in the resulting array is not guaranteed. |

## Class: `generic THashSet<T>`

The concrete implementation of the `IHashSet<T>` interface. It manages the internal data structures (buckets and entries) for efficient hash-based storage.

```pascal
generic THashSet<T> = class(TInterfacedObject, specialize IHashSet<T>)
private
  FBuckets: array of Integer;
  FEntries: array of specialize THashSetEntry<T>;
  FCount: Integer;
  FSlotsInUse: Integer;
  FFreeListHead: Integer;
  FHashFunc: specialize THashFunc<T>;
  FEqualityFunc: specialize TEqualityFunc<T>;
  FLoadFactor: Single;
  FVersion: Integer;

  procedure Initialize(InitialCapacity: Integer);
  procedure Resize;
  procedure GrowEntries;
  function GetBucketIndex(HashCode: Integer): Integer;

  // IHashSet<T> interface method implementations
  function GetCount: Integer;
  function Add(const Value: T): Boolean;
  function Remove(const Value: T): Boolean;
  function Contains(const Value: T): Boolean;
  procedure Clear;
  function ToArray: specialize TArray<T>;
public
  constructor Create(HashFunc: specialize THashFunc<T>; EqualityFunc: specialize TEqualityFunc<T>; InitialCapacity: Integer = 16; LoadFactor: Single = 0.75);
  destructor Destroy; override;
  class function New(HashFunc: specialize THashFunc<T>; EqualityFunc: specialize TEqualityFunc<T>; InitialCapacity: Integer = 16; LoadFactor: Single = 0.75): specialize IHashSet<T>; static;
  // The methods and properties of IHashSet<T> (e.g., Add, Remove, Count)
  // are accessed via an IHashSet<T> interface reference.
end;
```

### Constructor

*   **`constructor Create(HashFunc: specialize THashFunc<T>; EqualityFunc: specialize TEqualityFunc<T>; InitialCapacity: Integer = 16; LoadFactor: Single = 0.75);`**
    *   Initializes a new instance of the `THashSet<T>` class.
    *   **Parameters**:
        *   `HashFunc`: A `THashFunc<T>` delegate used to generate hash codes for elements. This argument is mandatory and cannot be `nil`.
        *   `EqualityFunc`: A `TEqualityFunc<T>` delegate used to compare elements for equality. This argument is mandatory and cannot be `nil`.
        *   `InitialCapacity`: The initial number of buckets the hash set will allocate. Defaults to `16`. The actual minimum capacity used internally is `4`. This value influences when the first resize might occur.
        *   `LoadFactor`: The maximum ratio of elements to buckets (i.e., `Count / Length(FBuckets)`) before the hash set automatically resizes its internal bucket array to maintain performance. Defaults to `0.75`. If an invalid value (e.g., less than or equal to 0) is provided, it defaults to `0.75`.
    *   Raises an `Exception` if `HashFunc` or `EqualityFunc` is `nil`.

### Static Factory Method

*   **`class function New(HashFunc: specialize THashFunc<T>; EqualityFunc: specialize TEqualityFunc<T>; InitialCapacity: Integer = 16; LoadFactor: Single = 0.75): specialize IHashSet<T>; static;`**
    *   A convenience static method that creates a new instance of `THashSet<T>` and returns it as an `IHashSet<T>` interface. This is the recommended way to create instances when automatic reference counting (ARC) is desired.
    *   Parameters are identical to the `Create` constructor.

### Methods and Properties

The `THashSet<T>` class implements all methods and properties defined in the `IHashSet<T>` interface, as described in the "Interface: `generic IHashSet<T>`" section.

## Helper Function: `CreateHashSet<T>`

A global generic function provided for convenience to simplify the creation of `THashSet<T>` instances, returning an `IHashSet<T>` interface.

```pascal
generic function CreateHashSet<T>(
  HashFunc: specialize THashFunc<T>; 
  EqualityFunc: specialize TEqualityFunc<T>; 
  InitialCapacity: Integer = 16; 
  LoadFactor: Single = 0.75
): specialize IHashSet<T>;
```

*   Creates and returns a new `THashSet<T>` instance, managed via its `IHashSet<T>` interface. This is often the preferred way to instantiate the hash set.
*   Parameters are identical to the `THashSet.Create` constructor.

## Usage Example

```pascal
uses
  SysUtils, TidyKit.Collections.HashSet;

// Define hash and equality functions for Integer
function MyIntegerHash(const Value: Integer): Integer;
begin
  Result := Value; // Simple hash for integers
end;

function MyIntegerEquals(const A, B: Integer): Boolean;
begin
  Result := A = B;
end;

// Define a record type and its hash/equality functions
type
  TPerson = record
    ID: Integer;
    Name: string;
  end;

function PersonHash(const Value: TPerson): Integer;
var
  HName: Cardinal;
  I: Integer;
begin
  // Simple string hash for name
  HName := 0;
  for I := 1 to Length(Value.Name) do
    HName := 31 * HName + Ord(Value.Name[I]);
  // Combine hashes
  Result := Integer(Cardinal(Value.ID) xor HName);
end;

function PersonEquals(const A, B: TPerson): Boolean;
begin
  Result := (A.ID = B.ID) and (A.Name = B.Name);
end;

var
  IntSet: specialize IHashSet<Integer>;
  PersonSet: specialize IHashSet<TPerson>;
  NumArray: specialize TArray<Integer>;
  P1, P2, P3: TPerson;
  I: Integer;
begin
  // --- Integer HashSet Example ---
  WriteLn('--- Integer HashSet Example ---');
  IntSet := CreateHashSet<Integer>(@MyIntegerHash, @MyIntegerEquals, 8, 0.8); // Custom capacity and load factor

  // Add elements
  if IntSet.Add(10) then WriteLn('10 added.');
  IntSet.Add(20);
  IntSet.Add(30);

  // Adding a duplicate
  if not IntSet.Add(10) then WriteLn('10 was already in the set.'); // This will be true

  WriteLn('Integer Set count: ', IntSet.Count); // Output: 3

  // Check for an element
  if IntSet.Contains(20) then WriteLn('Set contains 20.');

  // Remove an element
  if IntSet.Remove(20) then WriteLn('20 removed.');

  WriteLn('Integer Set count after removal: ', IntSet.Count); // Output: 2

  // Convert to array
  NumArray := IntSet.ToArray;
  Write('Elements in array: ');
  for I := Low(NumArray) to High(NumArray) do
    Write(NumArray[I], ' '); // Order not guaranteed, e.g., "10 30 " or "30 10 "
  WriteLn;

  IntSet.Clear;
  WriteLn('Integer Set count after clear: ', IntSet.Count); // Output: 0
  WriteLn;

  // --- Custom Record HashSet Example ---
  WriteLn('--- TPerson HashSet Example ---');
  PersonSet := CreateHashSet<TPerson>(@PersonHash, @PersonEquals);

  P1.ID := 1; P1.Name := 'Alice';
  P2.ID := 2; P2.Name := 'Bob';
  P3.ID := 1; P3.Name := 'Alice'; // Duplicate of P1

  PersonSet.Add(P1);
  PersonSet.Add(P2);

  WriteLn('Person Set count: ', PersonSet.Count); // Output: 2

  if PersonSet.Contains(P1) then WriteLn(P1.Name, ' is in the set.');
  if not PersonSet.Add(P3) then WriteLn(P3.Name, ' (duplicate) was not added again.');

  WriteLn('Person Set count (should still be 2): ', PersonSet.Count); // Output: 2

  // PersonSet and IntSet will be automatically freed when they go out of scope
  // due to ARC, as they are interface variables.
end.
```

This manual provides a comprehensive overview of the `TidyKit.Collections.HashSet` unit. The performance and correctness of the hash set depend significantly on the quality and consistency of the user-provided hash (`THashFunc<T>`) and equality (`TEqualityFunc<T>`) functions.
