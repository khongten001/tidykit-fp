unit TidyKit.JSON.Test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  TidyKit,
  // Alternatively, TidyKit.JSON,
  Math;

type
  TJSONTest = class(TTestCase)
  published
    procedure Test01_CreateEmptyObject;
    procedure Test02_CreateEmptyArray;
    procedure Test03_CreateString;
    procedure Test04_CreateNumber;
    procedure Test05_CreateBoolean;
    procedure Test06_CreateNull;
    procedure Test07_ObjectAddAndGet;
    procedure Test08_ArrayAddAndGet;
    procedure Test09_ParseSimpleObject;
    procedure Test10_ParseSimpleArray;
    procedure Test11_ParseComplexObject;
    procedure Test12_ParseComplexArray;
    procedure Test13_ParseInvalidJSON;
    procedure Test14_PrettyPrint;
    procedure Test15_Compact;
    procedure Test16_UnicodeString;
    procedure Test17_EscapeSequences;
    procedure Test18_StrictNumberFormat;
  end;

implementation

procedure TJSONTest.Test01_CreateEmptyObject;
var
  Obj: IJSONObject;
begin
  Obj := TJSON.Obj;
  AssertNotNull('Object should not be nil', Obj);
  AssertEquals('Empty object should have count 0', 0, Obj.Count);
  AssertEquals('Empty object should serialize as {}', '{}', Obj.ToString(False));
end;

procedure TJSONTest.Test02_CreateEmptyArray;
var
  Arr: IJSONArray;
begin
  Arr := TJSON.Arr;
  AssertNotNull('Array should not be nil', Arr);
  AssertEquals('Empty array should have count 0', 0, Arr.Count);
  AssertEquals('Empty array should serialize as []', '[]', Arr.ToString(False));
end;

procedure TJSONTest.Test03_CreateString;
var
  Str: IJSONValue;
begin
  Str := TJSON.Str('test');
  AssertNotNull('String should not be nil', Str);
  AssertTrue('Value should be string', Str.IsString);
  AssertEquals('String value should match', 'test', Str.AsString);
  AssertEquals('String should serialize with quotes', '"test"', Str.ToString(False));
end;

procedure TJSONTest.Test04_CreateNumber;
var
  IntNum, FloatNum: IJSONValue;
begin
  // Test integer number
  IntNum := TJSON.Num(123);
  AssertNotNull('Integer number should not be nil', IntNum);
  AssertTrue('Integer value should be number', IntNum.IsNumber);
  AssertEquals('Integer value should match', 123.0, IntNum.AsNumber, 0.001);
  AssertEquals('Integer conversion should work', 123, IntNum.AsInteger);

  // Test floating-point number
  FloatNum := TJSON.Num(123.45);
  AssertNotNull('Float number should not be nil', FloatNum);
  AssertTrue('Float value should be number', FloatNum.IsNumber);
  AssertEquals('Float value should match', 123.45, FloatNum.AsNumber, 0.001);
  
  // Verify that non-integer to integer conversion is not allowed
  try
    FloatNum.AsInteger;
    Fail('Converting non-integer to integer should raise an exception');
  except
    on E: EJSONException do
      AssertEquals('Error message should match',
        'Cannot convert non-integer number to integer', E.Message);
  end;
end;

procedure TJSONTest.Test05_CreateBoolean;
var
  Bool: IJSONValue;
begin
  Bool := TJSON.Bool(True);
  AssertNotNull('Boolean should not be nil', Bool);
  AssertTrue('Value should be boolean', Bool.IsBoolean);
  AssertTrue('Boolean value should be true', Bool.AsBoolean);
  AssertEquals('Boolean should serialize as true', 'true', Bool.ToString(False));
end;

procedure TJSONTest.Test06_CreateNull;
var
  Null: IJSONValue;
begin
  Null := TJSON.Null;
  AssertNotNull('Null value should not be nil', Null);
  AssertTrue('Value should be null', Null.IsNull);
  AssertEquals('Null should serialize as null', 'null', Null.ToString(False));
end;

procedure TJSONTest.Test07_ObjectAddAndGet;
var
  Obj: IJSONObject;
begin
  Obj := TJSON.Obj;
  Obj.Add('string', 'value');
  Obj.Add('number', 123);
  Obj.Add('boolean', True);
  Obj.Add('null', TJSON.Null);
  
  AssertEquals('Object should have 4 items', 4, Obj.Count);
  AssertEquals('String value should match', 'value', Obj['string'].AsString);
  AssertEquals('Number value should match', 123, Obj['number'].AsInteger);
  AssertTrue('Boolean value should be true', Obj['boolean'].AsBoolean);
  AssertTrue('Null value should be null', Obj['null'].IsNull);
  
  Obj.Remove('number');
  AssertEquals('Object should have 3 items after remove', 3, Obj.Count);
  AssertNull('Removed item should return nil', Obj['number']);
end;

procedure TJSONTest.Test08_ArrayAddAndGet;
var
  Arr: IJSONArray;
begin
  Arr := TJSON.Arr;
  Arr.Add('string');
  Arr.Add(123);
  Arr.Add(True);
  Arr.Add(TJSON.Null);
  
  AssertEquals('Array should have 4 items', 4, Arr.Count);
  AssertEquals('String value should match', 'string', Arr[0].AsString);
  AssertEquals('Number value should match', 123, Arr[1].AsInteger);
  AssertTrue('Boolean value should be true', Arr[2].AsBoolean);
  AssertTrue('Null value should be null', Arr[3].IsNull);
  
  Arr.Delete(1);
  AssertEquals('Array should have 3 items after delete', 3, Arr.Count);
  AssertTrue('Second item should now be boolean', Arr[1].IsBoolean);
end;

procedure TJSONTest.Test09_ParseSimpleObject;
var
  JSON: string;
  Value: IJSONValue;
begin
  JSON := '{"name":"test","value":123}';
  Value := TJSON.Parse(JSON);
  
  AssertNotNull('Parsed value should not be nil', Value);
  AssertTrue('Value should be object', Value.IsObject);
  AssertEquals('Object should have 2 items', 2, Value.AsObject.Count);
  AssertEquals('Name should match', 'test', Value.AsObject['name'].AsString);
  AssertEquals('Value should match', 123, Value.AsObject['value'].AsInteger);
end;

procedure TJSONTest.Test10_ParseSimpleArray;
var
  JSON: string;
  Value: IJSONValue;
begin
  JSON := '["test",123,true,null]';
  Value := TJSON.Parse(JSON);
  
  AssertNotNull('Parsed value should not be nil', Value);
  AssertTrue('Value should be array', Value.IsArray);
  AssertEquals('Array should have 4 items', 4, Value.AsArray.Count);
  AssertEquals('First item should be string', 'test', Value.AsArray[0].AsString);
  AssertEquals('Second item should be number', 123, Value.AsArray[1].AsInteger);
  AssertTrue('Third item should be true', Value.AsArray[2].AsBoolean);
  AssertTrue('Fourth item should be null', Value.AsArray[3].IsNull);
end;

procedure TJSONTest.Test11_ParseComplexObject;
var
  JSON: string;
  Value: IJSONValue;
  Obj: IJSONObject;
begin
  JSON := '{"name":"test","array":[1,2,3],"object":{"key":"value"}}';
  Value := TJSON.Parse(JSON);
  
  AssertNotNull('Parsed value should not be nil', Value);
  AssertTrue('Value should be object', Value.IsObject);
  
  Obj := Value.AsObject;
  AssertEquals('Object should have 3 items', 3, Obj.Count);
  AssertEquals('Name should match', 'test', Obj['name'].AsString);
  
  AssertTrue('Array property should be array', Obj['array'].IsArray);
  AssertEquals('Array should have 3 items', 3, Obj['array'].AsArray.Count);
  
  AssertTrue('Object property should be object', Obj['object'].IsObject);
  AssertEquals('Nested object value should match', 'value',
    Obj['object'].AsObject['key'].AsString);
end;

procedure TJSONTest.Test12_ParseComplexArray;
var
  JSON: string;
  Value: IJSONValue;
  Arr: IJSONArray;
begin
  JSON := '[{"name":"test"},["nested"],{"key":123}]';
  Value := TJSON.Parse(JSON);
  
  AssertNotNull('Parsed value should not be nil', Value);
  AssertTrue('Value should be array', Value.IsArray);
  
  Arr := Value.AsArray;
  AssertEquals('Array should have 3 items', 3, Arr.Count);
  
  AssertTrue('First item should be object', Arr[0].IsObject);
  AssertEquals('First object name should match', 'test',
    Arr[0].AsObject['name'].AsString);
  
  AssertTrue('Second item should be array', Arr[1].IsArray);
  AssertEquals('Nested array value should match', 'nested',
    Arr[1].AsArray[0].AsString);
  
  AssertTrue('Third item should be object', Arr[2].IsObject);
  AssertEquals('Third object value should match', 123,
    Arr[2].AsObject['key'].AsInteger);
end;

procedure TJSONTest.Test13_ParseInvalidJSON;
var
  Success: Boolean;
  Value: IJSONValue;
begin
  Success := TJSON.TryParse('{invalid}', Value);
  AssertFalse('Parsing invalid JSON should fail', Success);
  AssertNull('Value should be nil on failure', Value);
  
  Success := TJSON.TryParse('[1,2,]', Value);
  AssertFalse('Parsing trailing comma should fail', Success);
  AssertNull('Value should be nil on failure', Value);
end;

procedure TJSONTest.Test14_PrettyPrint;
var
  JSON: string;
  Pretty: string;
begin
  JSON := '{"name":"test","object":{"key":"value"},"array":[1,2,3]}';
  Pretty := TJSON.PrettyPrint(JSON);
  
  AssertTrue('Pretty output should contain newlines',
    Pos(LineEnding, Pretty) > 0);
  AssertTrue('Pretty output should contain indentation',
    Pos('  ', Pretty) > 0);
end;

procedure TJSONTest.Test15_Compact;
var
  JSON: string;
  Compact: string;
begin
  JSON := '{ "name": "test", "array": [ 1, 2, 3 ] }';
  Compact := TJSON.Compact(JSON);
  
  AssertEquals('Compact output should not contain spaces',
    '{"name":"test","array":[1,2,3]}', Compact);
end;

procedure TJSONTest.Test16_UnicodeString;
var
  JSON: string;
  Value: IJSONValue;
begin
  JSON := '{"text":"\u0048\u0065\u006C\u006C\u006F"}';
  Value := TJSON.Parse(JSON);
  
  AssertEquals('Unicode string should be decoded correctly',
    'Hello', Value.AsObject['text'].AsString);
end;

procedure TJSONTest.Test17_EscapeSequences;
var
  JSON: string;
  Value: IJSONValue;
begin
  JSON := '{"text":"Line1\nLine2\tTabbed\r\nWindows"}';
  Value := TJSON.Parse(JSON);
  
  AssertEquals('Escape sequences should be decoded correctly',
    'Line1'#10'Line2'#9'Tabbed'#13#10'Windows',
    Value.AsObject['text'].AsString);
end;

procedure TJSONTest.Test18_StrictNumberFormat;
var
  Value: IJSONValue;
  ExceptionRaised: Boolean;
begin
  // Valid numbers
  Value := TJSON.Parse('123');
  AssertEquals('Integer should parse correctly', 123, Value.AsInteger);
  
  Value := TJSON.Parse('-123');
  AssertEquals('Negative integer should parse correctly', -123, Value.AsInteger);
  
  Value := TJSON.Parse('123.456');
  AssertEquals('Float should parse correctly', 123.456, Value.AsNumber, 0.001);
  
  Value := TJSON.Parse('0.123');
  AssertEquals('Decimal less than 1 should parse correctly', 0.123, Value.AsNumber, 0.001);
  
  Value := TJSON.Parse('-0.123');
  AssertEquals('Negative decimal should parse correctly', -0.123, Value.AsNumber, 0.001);
  
  Value := TJSON.Parse('1.23e2');
  AssertEquals('Scientific notation should parse correctly', 123.0, Value.AsNumber, 0.001);
  
  Value := TJSON.Parse('1.23e-2');
  AssertEquals('Scientific notation with negative exponent should parse correctly', 0.0123, Value.AsNumber, 0.001);
  
  // Invalid numbers
  ExceptionRaised := False;
  try
    Value := TJSON.Parse('01');  // Leading zero
  except
    on E: EJSONException do
      ExceptionRaised := True;
  end;
  AssertTrue('Leading zeros should not be allowed', ExceptionRaised);
  
  ExceptionRaised := False;
  try
    Value := TJSON.Parse('1.');  // Trailing decimal point
  except
    on E: EJSONException do
      ExceptionRaised := True;
  end;
  AssertTrue('Trailing decimal point should not be allowed', ExceptionRaised);
  
  ExceptionRaised := False;
  try
    Value := TJSON.Parse('1.e2');  // Missing decimal digits
  except
    on E: EJSONException do
      ExceptionRaised := True;
  end;
  AssertTrue('Missing decimal digits should not be allowed', ExceptionRaised);
  
  ExceptionRaised := False;
  try
    Value := TJSON.Parse('1e');  // Missing exponent
  except
    on E: EJSONException do
      ExceptionRaised := True;
  end;
  AssertTrue('Missing exponent should not be allowed', ExceptionRaised);
  
  ExceptionRaised := False;
  try
    Value := TJSON.Parse('.123');  // Missing leading zero
  except
    on E: EJSONException do
      ExceptionRaised := True;
  end;
  AssertTrue('Missing leading zero should not be allowed', ExceptionRaised);
  
  // Special values
  ExceptionRaised := False;
  try
    Value := TJSON.Num(Infinity);  // Infinity not allowed in JSON
  except
    on E: EJSONException do
      ExceptionRaised := True;
  end;
  AssertTrue('Infinity should not be allowed', ExceptionRaised);
  
  ExceptionRaised := False;
  try
    Value := TJSON.Num(NaN);  // NaN not allowed in JSON
  except
    on E: EJSONException do
      ExceptionRaised := True;
  end;
  AssertTrue('NaN should not be allowed', ExceptionRaised);
  
  // Integer range
  ExceptionRaised := False;
  try
    Value := TJSON.Parse('1e100');  // Too large for Integer
    Value.AsInteger;  // Should raise exception
  except
    on E: EJSONException do
      ExceptionRaised := True;
  end;
  AssertTrue('Number too large for Integer should raise exception', ExceptionRaised);
end;

initialization
  RegisterTest(TJSONTest);
end. 
