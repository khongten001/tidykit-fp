# TidyKit.Collections.Dictionary

The `TidyKit.Collections.Dictionary` unit provides a generic key-value dictionary collection, `TDictionary<K, V>`. This collection is designed to store unique keys and their associated values, offering high-performance operations for adding, removing, and looking up values by their keys. It is optimized for scenarios where fast key-based lookups are critical.

## Implementation Details

`TDictionary<K, V>` is implemented as a **separate chaining hash table**, which provides the following characteristics:

- **Collision Resolution**: Uses separate chaining with linked lists (implemented via indices for better memory locality)
- **Time Complexity**: 
  - Average case: O(1) for add, remove, and lookup operations
  - Worst case: O(n) when all keys hash to the same bucket (unlikely with a good hash function)
- **Memory Usage**: 
  - Uses an array of buckets (indices) and a separate array for entries
  - Maintains a free list for reusing slots from removed entries
- **Resizing**: Automatically grows when the load factor (default 0.75) is exceeded, doubling the bucket count
- **Thread Safety**: Not thread-safe by default (synchronization must be handled by the caller)

This implementation is similar to Java's `HashMap` (pre-Java 8) and provides a good balance between performance and memory usage for most use cases. The separate chaining approach makes it more resilient to poor hash functions compared to open addressing schemes.

## Key Features

*   **Key-Value Storage**: Associates values with unique keys for efficient retrieval.
*   **High Performance**: Provides average O(1) time complexity for `Add`, `Remove`, and key lookups, assuming a good hash function that distributes keys evenly.
*   **Generic**: Can be used with any key type `K` and value type `V`.
*   **Customizable Hashing and Equality**: Allows users to provide custom functions for hashing and checking equality of keys. This is crucial for correct behavior with custom types.
*   **Interface-Based**: Implements `IDictionary<K, V>`, enabling usage with interfaces for automatic memory management (ARC) and polymorphism.
*   **Dynamic Resizing**: Automatically resizes its internal storage to maintain performance as the number of key-value pairs grows, based on a configurable load factor.

## Supporting Units

The Dictionary implementation relies on two important supporting units:

### TidyKit.Collections.HashFunction

The `TidyKit.Collections.HashFunction.pas` unit provides ready-to-use hash functions for common data types, including:

* `XXHash32` - An optimized hash function for strings and byte sequences
* `FNV1aHash` - An alternative string hash function with good distribution
* `MultiplicativeHash` - Fast integer hash function
* `FloatHash` - Specialized hash function for floating point values with proper handling of NaN, infinity, and zero values
* `Int64Hash`, `BooleanHash`, `DateTimeHash`, `CharHash` - Type-specific hash functions

### TidyKit.Collections.EqualityFunction

The `TidyKit.Collections.EqualityFunction.pas` unit provides ready-to-use equality comparison functions:

* `TidyKitIntegerEquals`, `TidyKitStringEquals`
* `TidyKitFloatEquals` - With special handling for NaN, infinity, and negative zero values
* `TidyKitBooleanEquals`, `TidyKitDateTimeEquals`, `TidyKitCharEquals`, `TidyKitInt64Equals`
* Additional functions for comparing complex types like `TidyKitPointEquals` and `TidyKitRectEquals`

These units eliminate the need to write your own hash and equality functions for common data types.

> **Note:** Enumeration with `for..in..do` is NOT supported by Dictionary. Use `GetKeys`, `GetValues`, or other methods to traverse elements.

## Generic Type Parameters

*   `K`: The type of keys used to identify values in the dictionary.
*   `V`: The type of values stored in the dictionary.

## Delegate Types

These function pointer types are essential for the `TDictionary<K, V>` to operate correctly with keys of type `K`.

### `generic TKeyHashFunc<K> = function(const Key: K): Integer;`

A function pointer type used to generate a hash code for a key of type `K`. A good hash function should produce a wide distribution of hash codes for different inputs to minimize collisions.

*   **Parameters**:
    *   `Key: K`: The key to hash.
*   **Returns**: `Integer` - The hash code for the key. The dictionary implementation will handle negative hash codes by taking the absolute value.

### `generic TKeyEqualityFunc<K> = function(const A, B: K): Boolean;`

A function pointer type used to compare two keys of type `K` for equality. This function is used to determine if a key already exists in the dictionary and during lookup operations.

*   **Parameters**:
    *   `A: K`: The first key.
    *   `B: K`: The second key.
*   **Returns**: `Boolean` - `True` if the keys are considered equal, `False` otherwise.

## Interface: `generic IDictionary<K, V>`

Defines the contract for a generic dictionary, promoting abstraction and enabling automatic reference counting (ARC).

```pascal
generic IDictionary<K, V> = interface
  ['{D764F3A2-E581-4B89-9D3C-78A5129B4D27}']
  function GetCount: Integer;
  function GetItem(const Key: K): V;
  procedure SetItem(const Key: K; const Value: V);
  
  function Add(const Key: K; const Value: V): Boolean;
  function Remove(const Key: K): Boolean;
  function ContainsKey(const Key: K): Boolean;
  function TryGetValue(const Key: K; out Value: V): Boolean;
  function GetKeys: specialize TArray<K>;
  function GetValues: specialize TArray<V>;
  procedure Clear;
  
  property Count: Integer read GetCount;
  property Items[const Key: K]: V read GetItem write SetItem; default;
end;
```

The members of the `IDictionary<K, V>` interface are summarized below:

| Member         | Type / Signature                                         | Description                                                                                               |
|----------------|----------------------------------------------------------|-----------------------------------------------------------------------------------------------------------|
| `Count`        | `property Count: Integer read GetCount`                  | Gets the number of key-value pairs currently stored in the dictionary.                                   |
| `Items`        | `property Items[const Key: K]: V read GetItem write SetItem` | Gets or sets the value associated with the specified key. Default indexed property for convenient access. |
| `Add`          | `function Add(const Key: K; const Value: V): Boolean`      | Adds a key-value pair to the dictionary if the key is not already present. Returns `True` if the pair was added, `False` if the key already existed. |
| `Remove`       | `function Remove(const Key: K): Boolean`                   | Removes the key-value pair with the specified key. Returns `True` if the pair was successfully found and removed, `False` if the key was not found. |
| `ContainsKey`  | `function ContainsKey(const Key: K): Boolean`              | Determines whether the dictionary contains the specified key. Returns `True` if the key is found, `False` otherwise. |
| `TryGetValue`  | `function TryGetValue(const Key: K; out Value: V): Boolean` | Attempts to get the value associated with the specified key. Returns `True` and sets `Value` if the key was found, returns `False` otherwise. |
| `GetKeys`      | `function GetKeys: specialize TArray<K>`                   | Returns an array containing all the keys in the dictionary. The order is not guaranteed. |
| `GetValues`    | `function GetValues: specialize TArray<V>`                 | Returns an array containing all the values in the dictionary. The order is not guaranteed. |
| `Clear`        | `procedure Clear`                                         | Removes all key-value pairs from the dictionary, resetting its count to zero. |

## Class: `generic TDictionary<K, V>`

The concrete implementation of the `IDictionary<K, V>` interface. It manages the internal data structures (buckets and entries) for efficient hash-based storage.

```pascal
generic TDictionary<K, V> = class(TInterfacedObject, specialize IDictionary<K, V>)
private
  type
    TEntry = record
      HashCode: Integer;
      NextEntry: Integer;
      Key: K;
      Value: V;
    end;
private
  FBuckets: array of Integer;
  FEntries: array of TEntry;
  FCount: Integer;
  FSlotsInUse: Integer;
  FFreeListHead: Integer;
  FKeyHashFunc: specialize TKeyHashFunc<K>;
  FKeyEqualityFunc: specialize TKeyEqualityFunc<K>;
  FLoadFactor: Single;
  FVersion: Integer;

  procedure Initialize(InitialCapacity: Integer);
  procedure Resize;
  procedure GrowEntries;
  function GetBucketIndex(HashCode: Integer): Integer;

  // IDictionary<K, V> interface method implementations
  function GetCount: Integer;
  function GetItem(const Key: K): V;
  procedure SetItem(const Key: K; const Value: V);
  function Add(const Key: K; const Value: V): Boolean;
  function Remove(const Key: K): Boolean;
  function ContainsKey(const Key: K): Boolean;
  function TryGetValue(const Key: K; out Value: V): Boolean;
  function GetKeys: specialize TArray<K>;
  function GetValues: specialize TArray<V>;
  procedure Clear;
public
  constructor Create(KeyHashFunc: specialize TKeyHashFunc<K>; KeyEqualityFunc: specialize TKeyEqualityFunc<K>; InitialCapacity: Integer = 16; LoadFactor: Single = 0.75);
  destructor Destroy; override;
  class function New(KeyHashFunc: specialize TKeyHashFunc<K>; KeyEqualityFunc: specialize TKeyEqualityFunc<K>; InitialCapacity: Integer = 16; LoadFactor: Single = 0.75): specialize IDictionary<K, V>; static;
  // The methods and properties of IDictionary<K, V> are accessed via an IDictionary<K, V> interface reference.
end;
```

### Constructor

*   **`constructor Create(KeyHashFunc: specialize TKeyHashFunc<K>; KeyEqualityFunc: specialize TKeyEqualityFunc<K>; InitialCapacity: Integer = 16; LoadFactor: Single = 0.75);`**
    *   Initializes a new instance of the `TDictionary<K, V>` class.
    *   **Parameters**:
        *   `KeyHashFunc`: A `TKeyHashFunc<K>` delegate used to generate hash codes for keys. This argument is mandatory and cannot be `nil`.
        *   `KeyEqualityFunc`: A `TKeyEqualityFunc<K>` delegate used to compare keys for equality. This argument is mandatory and cannot be `nil`.
        *   `InitialCapacity`: The initial number of buckets the dictionary will allocate. Defaults to `16`. The actual minimum capacity used internally is `4`. This value influences when the first resize might occur.
        *   `LoadFactor`: The maximum ratio of elements to buckets (i.e., `Count / Length(FBuckets)`) before the dictionary automatically resizes its internal bucket array to maintain performance. Defaults to `0.75`. If an invalid value (e.g., less than or equal to 0) is provided, it defaults to `0.75`.
    *   Raises an `Exception` if `KeyHashFunc` or `KeyEqualityFunc` is `nil`.

### Static Factory Method

*   **`class function New(KeyHashFunc: specialize TKeyHashFunc<K>; KeyEqualityFunc: specialize TKeyEqualityFunc<K>; InitialCapacity: Integer = 16; LoadFactor: Single = 0.75): specialize IDictionary<K, V>; static;`**
    *   A convenience static method that creates a new instance of `TDictionary<K, V>` and returns it as an `IDictionary<K, V>` interface. This is the recommended way to create instances when automatic reference counting (ARC) is desired.
    *   Parameters are identical to the `Create` constructor.

### Methods and Properties

The `TDictionary<K, V>` class implements all methods and properties defined in the `IDictionary<K, V>` interface, as described in the "Interface: `generic IDictionary<K, V>`" section.

## Helper Function: `CreateDictionary<K, V>`

A global generic function provided for convenience to simplify the creation of `TDictionary<K, V>` instances, returning an `IDictionary<K, V>` interface.

```pascal
generic function CreateDictionary<K, V>(
  KeyHashFunc: specialize TKeyHashFunc<K>; 
  KeyEqualityFunc: specialize TKeyEqualityFunc<K>; 
  InitialCapacity: Integer = 16; 
  LoadFactor: Single = 0.75
): specialize IDictionary<K, V>;
```

*   Creates and returns a new `TDictionary<K, V>` instance, managed via its `IDictionary<K, V>` interface. This is often the preferred way to instantiate the dictionary.
*   Parameters are identical to the `TDictionary.Create` constructor.

## Usage Examples

### Basic Example: String to Integer Dictionary

This basic example demonstrates the fundamental operations using a simple string-to-integer dictionary:

```pascal
uses
  SysUtils, 
  TidyKit.Collections.Dictionary,
  TidyKit.Collections.HashFunction,
  TidyKit.Collections.EqualityFunction;

var
  NameToAge: specialize IDictionary<string, Integer>;
  Age: Integer;
begin
  // Create a dictionary mapping names (string) to ages (integer)
  NameToAge := CreateDictionary<string, Integer>(@FNV1aHash, @TidyKitStringEquals);
  
  // Add key-value pairs
  NameToAge.Add('Alice', 30);
  NameToAge.Add('Bob', 25);
  NameToAge.Add('Charlie', 35);
  
  // Access values using the default indexed property
  WriteLn('Alice''s age: ', NameToAge['Alice']);
  
  // Update a value
  NameToAge['Bob'] := 26;
  WriteLn('Bob''s updated age: ', NameToAge['Bob']);
  
  // Check if a key exists
  if NameToAge.ContainsKey('David') then
    WriteLn('David found')
  else
    WriteLn('David not found');
  
  // Safe value retrieval
  if NameToAge.TryGetValue('Charlie', Age) then
    WriteLn('Charlie''s age: ', Age);
    
  // Get all keys
  WriteLn('Dictionary contains ', NameToAge.Count, ' entries');
  
  // Remove an entry
  if NameToAge.Remove('Bob') then
    WriteLn('Bob removed from dictionary');
    
  // Clear the dictionary
  NameToAge.Clear;
  WriteLn('Dictionary count after clear: ', NameToAge.Count);
end;
```

### Advanced Example: Custom Key Type

This example demonstrates how to use `TDictionary` with a custom record type as the key:

```pascal
uses
  SysUtils, 
  TidyKit.Collections.Dictionary,
  TidyKit.Collections.HashFunction,
  TidyKit.Collections.EqualityFunction;

type
  TPoint = record
    X, Y: Integer;
  end;

// Define hash function for TPoint
function PointHash(const Point: TPoint): Integer;
begin
  // Use built-in hash function and combine X and Y values
  Result := MultiplicativeHash(Point.X) xor MultiplicativeHash(Point.Y);
end;

function PointEquals(const A, B: TPoint): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y);
end;

var
  PointToColor: specialize IDictionary<TPoint, string>;
  Point: TPoint;
  Colors: specialize TArray<string>;
  I: Integer;
begin
  // Create a dictionary mapping points to color names
  PointToColor := CreateDictionary<TPoint, string>(@PointHash, @PointEquals);
  
  // Add some points with associated colors
  Point.X := 0; Point.Y := 0;
  PointToColor.Add(Point, 'Black');
  
  Point.X := 255; Point.Y := 0;
  PointToColor.Add(Point, 'Red');
  
  Point.X := 0; Point.Y := 255;
  PointToColor.Add(Point, 'Green');
  
  Point.X := 255; Point.Y := 255;
  PointToColor.Add(Point, 'Yellow');
  
  // Lookup a point
  Point.X := 0; Point.Y := 0;
  if PointToColor.ContainsKey(Point) then
    WriteLn('Color at origin: ', PointToColor[Point]);
  
  // Get all colors
  Colors := PointToColor.GetValues;
  WriteLn('Colors in dictionary:');
  for I := Low(Colors) to High(Colors) do
    WriteLn(' - ', Colors[I]);
end;
```

This manual provides a comprehensive overview of the `TidyKit.Collections.Dictionary` unit. The performance and correctness of the dictionary depend significantly on the quality and consistency of the user-provided hash (`TKeyHashFunc<K>`) and equality (`TKeyEqualityFunc<K>`) functions. Fortunately, TidyKit provides ready-to-use implementations for common data types in the `TidyKit.Collections.HashFunction` and `TidyKit.Collections.EqualityFunction` units.
