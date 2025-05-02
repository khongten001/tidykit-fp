unit TidyKit.ParseArgs.Test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, TidyKit.ParseArgs;

type
  TTestParseArgs = class(TTestCase)
  private
    FParser: TArgParser;
    FTestValue: TArgValue;
    
    procedure HandleTestValue(const Value: TArgValue);
    procedure HandleTestValueClass(const Value: TArgValue);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test01_AddOptions;
    procedure Test02_ParseString;
    procedure Test03_ParseInteger;
    procedure Test04_ParseFloat;
    procedure Test05_ParseBoolean;
    procedure Test06_ParseArray;
    procedure Test07_RequiredOptions;
    procedure Test08_ErrorHandling;
    procedure Test09_HelpAndUsage;
    procedure Test10_Callbacks;
    procedure Test11_MixedOptions;
  end;

implementation

var
  GlobalTestValue: TArgValue;

procedure GlobalHandleTestValue(const Value: TArgValue);
begin
  GlobalTestValue := Value;
end;

procedure TTestParseArgs.HandleTestValue(const Value: TArgValue);
begin
  FTestValue := Value;
end;

procedure TTestParseArgs.HandleTestValueClass(const Value: TArgValue);
begin
  FTestValue := Value;
end;

procedure TTestParseArgs.SetUp;
begin
  FParser.Init;
  FParser.SetUsage('testprogram [options]');
end;

procedure TTestParseArgs.TearDown;
begin
  // No need to Free since it's a record
end;

procedure TTestParseArgs.Test01_AddOptions;
var
  DefaultVal: TArgValue;
begin
  DefaultVal.ArgType := atBoolean;
  DefaultVal.Bool := False;
  FParser.Add('v', 'verbose', atBoolean, 'Enable verbose mode', @GlobalHandleTestValue, @Self.HandleTestValueClass, False, DefaultVal);
  DefaultVal.ArgType := atInteger;
  DefaultVal.Int := 0;
  FParser.Add('c', 'count', atInteger, 'Set count value', @GlobalHandleTestValue, @Self.HandleTestValueClass, True, DefaultVal);
  DefaultVal.ArgType := atString;
  DefaultVal.Str := '';
  FParser.Add('f', 'file', atString, 'Input file', @GlobalHandleTestValue, @Self.HandleTestValueClass, False, DefaultVal);
  
  CheckEquals(3, FParser.OptionCount, 'Should have 3 options added');
end;

procedure TTestParseArgs.Test02_ParseString;
var
  DefaultVal: TArgValue;
begin
  DefaultVal.ArgType := atString;
  DefaultVal.Str := '';
  FParser.Add('f', 'file', atString, 'Input file', @GlobalHandleTestValue, nil, False, DefaultVal);
  FParser.Parse(['--file', 'test.txt']);
  
  CheckEquals(False, FParser.HasError, 'No error should occur');
  CheckEquals('test.txt', GlobalTestValue.Str, 'String value should be parsed correctly');
end;

procedure TTestParseArgs.Test03_ParseInteger;
var
  DefaultVal: TArgValue;
begin
  DefaultVal.ArgType := atInteger;
  DefaultVal.Int := 0;
  FParser.Add('c', 'count', atInteger, 'Set count value', nil, @Self.HandleTestValue, False, DefaultVal);
  FParser.Parse(['--count', '42']);
  
  CheckEquals(False, FParser.HasError, 'No error should occur');
  CheckEquals(42, FTestValue.Int, 'Integer value should be parsed correctly');
end;

procedure TTestParseArgs.Test04_ParseFloat;
var
  DefaultVal: TArgValue;
begin
  DefaultVal.ArgType := atFloat;
  DefaultVal.Flt := 0;
  FParser.Add('p', 'precision', atFloat, 'Set precision', nil, @Self.HandleTestValue, False, DefaultVal);
  FParser.Parse(['--precision', '3.14159']);
  
  CheckEquals(False, FParser.HasError, 'No error should occur');
  CheckEquals(3.14159, FTestValue.Flt, 'Float value should be parsed correctly');
end;

procedure TTestParseArgs.Test05_ParseBoolean;
var
  DefaultVal: TArgValue;
begin
  DefaultVal.ArgType := atBoolean;
  DefaultVal.Bool := False;
  FParser.Add('v', 'verbose', atBoolean, 'Enable verbose mode', nil, @Self.HandleTestValue, False, DefaultVal);
  FParser.Parse(['--verbose']);
  
  CheckEquals(False, FParser.HasError, 'No error should occur');
  CheckEquals(True, FTestValue.Bool, 'Boolean value should be parsed correctly');
end;

procedure TTestParseArgs.Test06_ParseArray;
var
  DefaultVal: TArgValue;
begin
  DefaultVal.ArgType := atArray;
  SetLength(DefaultVal.Arr, 0);
  FParser.Add('l', 'list', atArray, 'List of items', nil, @Self.HandleTestValue, False, DefaultVal);
  FParser.Parse(['--list', 'item1,item2,item3']);
  
  CheckEquals(False, FParser.HasError, 'No error should occur');
  CheckEquals(3, Length(FTestValue.Arr), 'Array should have 3 items');
  CheckEquals('item1', FTestValue.Arr[0], 'First item should be correct');
  CheckEquals('item2', FTestValue.Arr[1], 'Second item should be correct');
  CheckEquals('item3', FTestValue.Arr[2], 'Third item should be correct');
end;

procedure TTestParseArgs.Test07_RequiredOptions;
var
  DefaultVal: TArgValue;
begin
  DefaultVal.ArgType := atInteger;
  DefaultVal.Int := 0;
  FParser.Add('c', 'count', atInteger, 'Set count value', nil, nil, True, DefaultVal);
  FParser.Parse([]);
  
  CheckEquals(True, FParser.HasError, 'Missing required option should cause error');
  CheckTrue(Pos('count', FParser.Error) > 0, 'Error message should mention missing option');
end;

procedure TTestParseArgs.Test08_ErrorHandling;
var
  DefaultVal: TArgValue;
begin
  DefaultVal.ArgType := atInteger;
  DefaultVal.Int := 0;
  FParser.Add('c', 'count', atInteger, 'Set count value', nil, nil, False, DefaultVal);
  FParser.Parse(['--count', 'notanumber']);
  
  CheckEquals(True, FParser.HasError, 'Invalid value should cause error');
  CheckTrue(Pos('count', FParser.Error) > 0, 'Error message should mention invalid value');
end;

procedure TTestParseArgs.Test09_HelpAndUsage;
var
  DefaultVal: TArgValue;
begin
  DefaultVal.ArgType := atBoolean;
  DefaultVal.Bool := False;
  FParser.Add('h', 'help', atBoolean, 'Show help', nil, nil, False, DefaultVal);
  
  FParser.ShowHelp;
  FParser.ShowUsage;
end;

procedure TTestParseArgs.Test10_Callbacks;
var
  DefaultVal: TArgValue;
begin
  DefaultVal.ArgType := atBoolean;
  DefaultVal.Bool := False;
  FParser.Add('v', 'verbose', atBoolean, 'Enable verbose mode', nil, @Self.HandleTestValueClass, False, DefaultVal);
  FParser.Parse(['--verbose']);
  
  CheckEquals(False, FParser.HasError, 'No error should occur');
  CheckEquals(True, FTestValue.Bool, 'Callback should be called with correct value');
end;

procedure TTestParseArgs.Test11_MixedOptions;
var
  DefaultValBool, DefaultValInt, DefaultValStr: TArgValue;
begin
  DefaultValBool.ArgType := atBoolean;
  DefaultValBool.Bool := False;
  FParser.Add('v', 'verbose', atBoolean, 'Enable verbose mode', nil, nil, False, DefaultValBool);

  DefaultValInt.ArgType := atInteger;
  DefaultValInt.Int := 0;
  FParser.Add('c', 'count', atInteger, 'Set count value', nil, nil, False, DefaultValInt);

  DefaultValStr.ArgType := atString;
  DefaultValStr.Str := '';
  FParser.Add('f', 'file', atString, 'Input file', nil, nil, False, DefaultValStr);

  FParser.Parse(['-v', '--count', '10', '--file', 'test.txt']);

  CheckEquals(False, FParser.HasError, 'No error should occur');
end;

initialization
  RegisterTest(TTestParseArgs);
end.
