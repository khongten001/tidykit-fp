# TidyKit.JSON Library

A user-friendly JSON library for Free Pascal with automatic reference counting (ARC) support. This library provides an intuitive API for working with JSON data while ensuring proper memory management through interfaces.

## Features

- Interface-based design for automatic memory management
- Comprehensive JSON parsing and generation
- Support for all JSON data types (objects, arrays, strings, numbers, booleans, null)
- Pretty printing and compact output options
- Full Unicode support with proper escape sequence handling
- Error handling with descriptive messages
- Easy-to-use factory methods
- Compatible with Free Pascal 3.2.2
- Thoroughly tested with 17 comprehensive test cases

## Standards Conformance

TidyKit.JSON strictly adheres to the JSON standards defined in:
- ECMA-404 (The JSON Data Interchange Syntax)
- RFC 8259 (The JavaScript Object Notation (JSON) Data Interchange Format)

### Number Format
- No leading zeros (except for 0)
- Decimal point must be followed by at least one digit
- Scientific notation supported (e.g., 1.23e2, 1.23e-2)
- No octal or hexadecimal notation
- No special values (NaN, Infinity)
- Proper handling of integer range limits
- Proper handling of decimal points and trailing zeros

### String Format
- Double quotes required
- Full escape sequence support:
  - Control characters (\b, \f, \n, \r, \t)
  - Unicode escapes (\uXXXX)
  - Quotation mark (\")
  - Reverse solidus (\\)
  - Solidus (\/)
- Proper handling of surrogate pairs for characters outside BMP
- No unescaped control characters (0x00 through 0x1F)

### Structural Characters
- Objects: { }
- Arrays: [ ]
- Name separator: :
- Value separator: ,
- No trailing commas
- No comments allowed

### Unicode Support
- Full UTF-16 support
- Proper surrogate pair handling
- \u escape sequence support for all Unicode characters
- Validation of surrogate pairs
- Proper encoding of control characters

### Additional Features Beyond Standard
- Property order preservation in objects (not required by standard)
- Pretty printing with configurable indentation
- Compact output option
- Type-safe access through interfaces
- Memory safety through ARC
- Detailed error messages for parsing failures

### Technical Implementation Details
- Numbers stored as Double (64-bit) for maximum precision
- Strict validation of number formats during parsing
- Singleton implementation for null values
- Interface-based memory management
- Lazy parsing of JSON strings
- Ordered property storage using TDictionary and TList
- Comprehensive error checking during parsing and type conversion

## Installation

The TidyKit.JSON library is part of the TidyKit package. To use it in your project:

1. Add the TidyKit source directory to your project's search path
2. Add the following unit to your uses clause:
   ```pascal
   uses
     TidyKit.JSON;  // All JSON functionality
   ```

That's it! The TidyKit.JSON unit provides everything you need for working with JSON, including:
- All JSON interfaces (IJSONValue, IJSONObject, IJSONArray)
- The TJSON factory class for creating and parsing JSON
- Exception handling through EJSONException

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
  function GetOrderedKeys: TStringArray;  // Returns keys in insertion order
  
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

## Test Cases

The library includes 17 comprehensive test cases that verify its functionality:

1. Test01_CreateEmptyObject: Creating and verifying empty JSON objects
2. Test02_CreateEmptyArray: Creating and verifying empty JSON arrays
3. Test03_CreateString: String value creation and verification
4. Test04_CreateNumber: Numeric value handling
5. Test05_CreateBoolean: Boolean value handling
6. Test06_CreateNull: Null value handling
7. Test07_ObjectAddAndGet: Object property manipulation
8. Test08_ArrayAddAndGet: Array element manipulation
9. Test09_ParseSimpleObject: Basic object parsing
10. Test10_ParseSimpleArray: Basic array parsing
11. Test11_ParseComplexObject: Nested object parsing
12. Test12_ParseComplexArray: Nested array parsing
13. Test13_ParseInvalidJSON: Error handling for invalid JSON
14. Test14_PrettyPrint: JSON formatting with indentation
15. Test15_Compact: JSON compression
16. Test16_UnicodeString: Unicode character handling
17. Test17_EscapeSequences: Special character escape sequences
18. Test18_StrictNumberFormat: Strict JSON number format compliance
   - Validates correct number formats (integers, decimals, scientific notation)
   - Verifies rejection of invalid formats (leading zeros, trailing decimals)
   - Tests number range and precision handling

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