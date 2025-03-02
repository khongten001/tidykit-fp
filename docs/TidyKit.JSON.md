# TidyKit.JSON Library

A user-friendly JSON library for Free Pascal with automatic reference counting (ARC) support. This library provides an intuitive API for working with JSON data while ensuring proper memory management through interfaces.

## Features

- Interface-based design for automatic memory management
- Comprehensive JSON parsing and generation
- Support for all JSON data types (objects, arrays, strings, numbers, booleans, null)
- Pretty printing and compact output options
- Full Unicode support
- Proper handling of escape sequences
- Error handling with descriptive messages
- Easy-to-use factory methods
- Compatible with Free Pascal 3.2.2

## Installation

The TidyKit.JSON library is part of the TidyKit package. To use it in your project:

1. Add the TidyKit source directory to your project's search path
2. Add the following units to your uses clause:
   ```pascal
   uses
     TidyKit.JSON,         // Core JSON interfaces
     TidyKit.JSON.Types,   // JSON value implementations
     TidyKit.JSON.Factory; // Factory methods (TJSON)
   ```

## Quick Start

### Creating JSON

```pascal
var
  Person: IJSONObject;
  Address: IJSONObject;
  Hobbies: IJSONArray;
begin
  // Create a person object
  Person := TJSON.Obj;
  Person.Add('name', 'John Smith');
  Person.Add('age', 30);
  Person.Add('isActive', True);
  
  // Create and add an address object
  Address := TJSON.Obj;
  Address.Add('street', '123 Main St');
  Address.Add('city', 'Springfield');
  Address.Add('zipCode', '12345');
  Person.Add('address', Address);
  
  // Create and add a hobbies array
  Hobbies := TJSON.Arr;
  Hobbies.Add('reading');
  Hobbies.Add('cycling');
  Hobbies.Add('swimming');
  Person.Add('hobbies', Hobbies);
  
  // Convert to JSON string (pretty-printed)
  WriteLn(Person.ToString(True));
end;
```

### Parsing JSON

```pascal
var
  JSON: string;
  Value: IJSONValue;
  Person: IJSONObject;
begin
  JSON := '{"name":"Jane Doe","age":25,"skills":["Pascal","Python"]}';
  
  // Parse JSON string
  Value := TJSON.Parse(JSON);
  Person := Value.AsObject;
  
  // Access values
  WriteLn('Name: ', Person['name'].AsString);
  WriteLn('Age: ', Person['age'].AsInteger);
  WriteLn('First Skill: ', Person['skills'].AsArray[0].AsString);
end;
```

### Error Handling

```pascal
var
  Success: Boolean;
  Value: IJSONValue;
begin
  // Using TryParse
  Success := TJSON.TryParse('{invalid json}', Value);
  if not Success then
    WriteLn('Failed to parse JSON');
    
  // Using exception handling
  try
    Value := TJSON.Parse('[1,2,]'); // Trailing comma
  except
    on E: EJSONException do
      WriteLn('Error: ', E.Message);
  end;
end;
```

## API Reference

### Factory Methods (TJSON)

- `TJSON.Obj`: Create an empty JSON object
- `TJSON.Arr`: Create an empty JSON array
- `TJSON.Str(Value: string)`: Create a JSON string value
- `TJSON.Num(Value: Double)`: Create a JSON number value
- `TJSON.Int(Value: Integer)`: Create a JSON integer value
- `TJSON.Bool(Value: Boolean)`: Create a JSON boolean value
- `TJSON.Null`: Create a JSON null value
- `TJSON.Parse(JSON: string)`: Parse a JSON string
- `TJSON.TryParse(JSON: string; out Value: IJSONValue)`: Try to parse a JSON string
- `TJSON.PrettyPrint(JSON: string)`: Format JSON with indentation
- `TJSON.Compact(JSON: string)`: Format JSON without whitespace

### IJSONValue Interface

Base interface for all JSON values:

```pascal
IJSONValue = interface
  function GetAsString: string;
  function GetAsNumber: Double;
  function GetAsInteger: Integer;
  function GetAsBoolean: Boolean;
  function GetAsObject: IJSONObject;
  function GetAsArray: IJSONArray;
  
  function IsString: Boolean;
  function IsNumber: Boolean;
  function IsBoolean: Boolean;
  function IsObject: Boolean;
  function IsArray: Boolean;
  function IsNull: Boolean;
  
  function ToString(Pretty: Boolean = False): string;
  
  property AsString: string read GetAsString;
  property AsNumber: Double read GetAsNumber;
  property AsInteger: Integer read GetAsInteger;
  property AsBoolean: Boolean read GetAsBoolean;
  property AsObject: IJSONObject read GetAsObject;
  property AsArray: IJSONArray read GetAsArray;
end;
```

### IJSONObject Interface

Interface for JSON objects:

```pascal
IJSONObject = interface(IJSONValue)
  function GetValue(const Name: string): IJSONValue;
  procedure SetValue(const Name: string; Value: IJSONValue);
  function GetCount: Integer;
  function GetNames: TStringArray;
  
  procedure Add(const Name: string; Value: IJSONValue); overload;
  procedure Add(const Name: string; const Value: string); overload;
  procedure Add(const Name: string; Value: Integer); overload;
  procedure Add(const Name: string; Value: Double); overload;
  procedure Add(const Name: string; Value: Boolean); overload;
  
  procedure Remove(const Name: string);
  function Contains(const Name: string): Boolean;
  
  property Values[const Name: string]: IJSONValue read GetValue write SetValue; default;
  property Count: Integer read GetCount;
  property Names: TStringArray read GetNames;
end;
```

### IJSONArray Interface

Interface for JSON arrays:

```pascal
IJSONArray = interface(IJSONValue)
  function GetItem(Index: Integer): IJSONValue;
  procedure SetItem(Index: Integer; Value: IJSONValue);
  function GetCount: Integer;
  
  procedure Add(Value: IJSONValue); overload;
  procedure Add(const Value: string); overload;
  procedure Add(Value: Integer); overload;
  procedure Add(Value: Double); overload;
  procedure Add(Value: Boolean); overload;
  
  procedure Delete(Index: Integer);
  procedure Clear;
  
  property Items[Index: Integer]: IJSONValue read GetItem write SetItem; default;
  property Count: Integer read GetCount;
end;
```

## Best Practices

1. **Use Interface References**: Always use interface types (IJSONValue, IJSONObject, IJSONArray) instead of concrete classes to ensure proper memory management.

2. **Error Handling**: Use TryParse when you want to handle parsing errors gracefully, or wrap Parse calls in try-except blocks.

3. **Type Checking**: Always check value types before conversion:
   ```pascal
   if Value.IsObject then
     // Use Value.AsObject
   else if Value.IsArray then
     // Use Value.AsArray
   ```

4. **Memory Management**: Let the interface references handle memory management. Don't try to manually free JSON values.

5. **String Formatting**: Use ToString(True) for human-readable output and ToString(False) for compact storage.

## Examples

See the `examples/json_example.pas` file for a complete working example that demonstrates:
- Creating JSON objects and arrays
- Parsing JSON strings
- Modifying JSON data
- Error handling
- Pretty printing and compact output

## Contributing

Contributions are welcome! Please see the main TidyKit repository's CONTRIBUTING.md file for guidelines.

## License

This library is part of TidyKit and is available under the same license terms as the main project. 