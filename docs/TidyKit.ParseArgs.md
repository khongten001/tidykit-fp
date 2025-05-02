# TidyKit.ParseArgs – Beginner’s Guide

A simple, record-based command-line argument parser for Free Pascal (FPC 3.2.2+).

---

## 1. Introduction

`TidyKit.ParseArgs` helps you:

- Define short (`-f`) and long (`--file`) options
- Parse values: string, integer, float, boolean, or array
- Attach callbacks to handle options
- Auto-generate usage and help text

Ideal for small to medium Pascal programs.

---

## 2. Installation

1. Copy `TidyKit.ParseArgs.pas` into your project folder.
2. Add to your program’s `uses` clause:
   ```pascal
   uses
     SysUtils,
     TidyKit.ParseArgs;
   ```
3. Compile with FPC:
   ```bash
   fpc MyProgram.pas
   ```

---

## 3. Quick Start

```pascal
var
  Parser: TArgParser;
begin
  Parser.Init;
  Parser.SetUsage('myapp [options]');

  // Add options here (see Section 4)

  Parser.Parse(ParamStrArray);
  if Parser.HasError then
  begin
    Writeln('Error: ', Parser.Error);
    Parser.ShowUsage;
    Halt(1);
  end;

  // Continue program
end.
```

---

## 4. Defining Options

Call `Parser.Add` with **eight parameters**:

```pascal
Parser.Add(
  'n',               // Short: -n
  'number',          // Long:  --number
  atInteger,         // Type
  'Number of items', // Help text
  nil,               // Free procedure callback or nil
  @OnNumber,         // Method callback or nil
  False,             // Required (True = must appear)
  DefaultValue       // A TArgValue record with default set
);
```

- Initialize `DefaultValue` before use:
  ```pascal
  var V: TArgValue;
  V.ArgType := atInteger;
  V.Int := 10;  // default value
  ```
- Use `nil` if you skip a callback.

---

## 5. Example Program

```pascal
program MyApp;

uses
  SysUtils,
  TidyKit.ParseArgs;

var
  Parser: TArgParser;
  V: TArgValue;

procedure OnCount(const Value: TArgValue);
begin
  Writeln('Count = ', Value.Int);
end;

procedure OnVerbose(const Value: TArgValue);
begin
  if Value.Bool then
    Writeln('Verbose mode ON');
end;

begin
  // Initialize
  Parser.Init;
  Parser.SetUsage('MyApp [options]');

  // --count, default 5
  V.ArgType := atInteger;
  V.Int := 5;
  Parser.Add('c','count', atInteger, 'Set count', nil, @OnCount, False, V);

  // --verbose flag
  V.ArgType := atBoolean;
  V.Bool := False;
  Parser.Add('v','verbose', atBoolean, 'Enable verbose', nil, @OnVerbose, False, V);

  // Parse and handle errors
  Parser.Parse(ParamStrArray);
  if Parser.HasError then
  begin
    Writeln('Error: ', Parser.Error);
    Parser.ShowUsage;
    Halt(1);
  end;

  Writeln('Done.');
end.
```

---

## 6. API Reference

```pascal
TArgType = (atString, atInteger, atFloat, atBoolean, atArray);

TArgValue = record
  ArgType: TArgType;
  Str:     string;
  Int:     Integer;
  Flt:     Double;
  Bool:    Boolean;
  Arr:     array of string;
end;

TArgCallback      = procedure(const Value: TArgValue);
TArgCallbackClass = procedure(const Value: TArgValue) of object;

TArgParser = record
  procedure Init;
  procedure SetUsage(const AUsage: string);
  procedure Add(
    ShortOpt: Char;
    LongOpt: string;
    ArgType: TArgType;
    HelpText: string;
    Callback: TArgCallback;
    CallbackClass: TArgCallbackClass;
    Required: Boolean;
    DefaultValue: TArgValue
  );
  procedure Parse(const Args: array of string);
  function HasError: Boolean;
  property Error: string read GetError;
  procedure ShowUsage;
  procedure ShowHelp;
  function OptionCount: Integer;
end;
```

---

## 7. Tips & Best Practices

- Always initialize the `DefaultValue` record before `Add`.
- For boolean flags, presence ⇒ `True`.
- Set `Required := True` to enforce mandatory options.
- Choose method callbacks (`CallbackClass`) when updating object state.

Happy Free Pascal coding!  
