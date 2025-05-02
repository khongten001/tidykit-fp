unit TidyKit.ParseArgs;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils;

type
  TArgType = (atString, atInteger, atFloat, atBoolean, atArray);
  
  TArrayOfString = array of string;

  TArgValue = record
    ArgType: TArgType;
    Str: string;
    Int: Integer;
    Flt: Double;
    Bool: Boolean;
    Arr: TArrayOfString;
  end;

  TArgCallback = procedure(const Value: TArgValue);
  TArgCallbackClass = procedure(const Value: TArgValue) of object;

  TArgOption = record
    ShortOpt: Char;
    LongOpt: string;
    ArgType: TArgType;
    HelpText: string;
    DefaultValue: TArgValue;
    Callback: TArgCallback;
    CallbackClass: TArgCallbackClass;
    Required: Boolean;
  end;

  TOptionsArray = array of TArgOption;

  TArgParser = record
  private
    FOptions: TOptionsArray;
    FUsage: string;
    FError: string;
    FHasError: Boolean;
    
    function FindOption(const Opt: string): Integer;
    function ParseValue(const ValueStr: string; const ArgType: TArgType; out Value: TArgValue): Boolean;
    procedure AddOption(const Option: TArgOption);
    procedure SetError(const AError: string);
    function GetError: string;
  public
    procedure Init;
    
    procedure Add(const ShortOpt: Char; const LongOpt: string;
      const ArgType: TArgType; const HelpText: string;
      const Callback: TArgCallback;
      const CallbackClass: TArgCallbackClass;
      const Required: Boolean;
      const DefaultValue: TArgValue);
    
    procedure Parse(const Args: array of string);
    function HasError: Boolean;
    property Error: string read GetError;
    
    procedure ShowHelp;
    procedure ShowUsage;
    procedure SetUsage(const AUsage: string);
    function OptionCount: Integer;
  end;

implementation

{ TArgParser }


procedure TArgParser.Init;
begin
  SetLength(FOptions, 0);
  FUsage := '';
  FError := '';
  FHasError := False;
end;

procedure TArgParser.Add(const ShortOpt: Char; const LongOpt: string;
  const ArgType: TArgType; const HelpText: string;
  const Callback: TArgCallback;
  const CallbackClass: TArgCallbackClass;
  const Required: Boolean;
  const DefaultValue: TArgValue);
var
  Option: TArgOption;
begin
  Option.ShortOpt := ShortOpt;
  Option.LongOpt := LongOpt;
  Option.ArgType := ArgType;
  Option.HelpText := HelpText;
  Option.Callback := Callback;
  Option.CallbackClass := CallbackClass;
  Option.Required := Required;
  Option.DefaultValue := DefaultValue;
  
  AddOption(Option);
end;

function TArgParser.FindOption(const Opt: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FOptions) do
  begin
    if (Length(Opt) = 2) and (Opt[1] = '-') and (FOptions[i].ShortOpt = Opt[2]) then
    begin
      Result := i;
      Exit;
    end
    else if (Length(Opt) > 2) and (Opt[1] = '-') and (Opt[2] = '-') and
      (FOptions[i].LongOpt = Copy(Opt, 3, MaxInt)) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function SplitString(const S, Delimiter: string): TArrayOfString;
var
  StartPos, DelimPos, ArrLen: Integer;
  Temp: string;
begin
  Result := nil;
  ArrLen := 0;
  SetLength(Result, 0);
  StartPos := 1;
  while StartPos <= Length(S) do
  begin
    DelimPos := Pos(Delimiter, Copy(S, StartPos, Length(S) - StartPos + 1));
    if DelimPos = 0 then
    begin
      SetLength(Result, ArrLen + 1);
      Result[ArrLen] := Copy(S, StartPos, Length(S) - StartPos + 1);
      Break;
    end
    else
    begin
      SetLength(Result, ArrLen + 1);
      Result[ArrLen] := Copy(S, StartPos, DelimPos - 1);
      StartPos := StartPos + DelimPos;
      Inc(ArrLen);
    end;
  end;
end;

function TArgParser.ParseValue(const ValueStr: string; const ArgType: TArgType; out Value: TArgValue): Boolean;
var
  TempArr: TArrayOfString;
begin
  Result := False;
  Value.ArgType := ArgType;
  case ArgType of
    atString:
      begin
        Value.Str := ValueStr;
        Result := True;
      end;
    atInteger:
      begin
        if TryStrToInt(ValueStr, Value.Int) then
          Result := True;
      end;
    atFloat:
      begin
        if TryStrToFloat(ValueStr, Value.Flt) then
          Result := True;
      end;
    atBoolean:
      begin
        if TryStrToBool(ValueStr, Value.Bool) then
          Result := True;
      end;
    atArray:
      begin
        TempArr := SplitString(ValueStr, ',');
        Value.Arr := TempArr;
        Result := True;
      end;
  end;
end;


procedure TArgParser.Parse(const Args: array of string);
var
  i, j, OptionIdx: Integer;
  CurrentOpt: string;
  Value: TArgValue;
  HasValue: Boolean;
begin
  FHasError := False;
  FError := '';
  
  i := Low(Args);
  while i <= High(Args) do
  begin
    CurrentOpt := Args[i];
    
    if (Length(CurrentOpt) = 0) or (CurrentOpt[1] <> '-') then
    begin
      SetError('Invalid argument format');
      Exit;
    end;
    
    OptionIdx := FindOption(CurrentOpt);
    if OptionIdx = -1 then
    begin
      SetError('Unknown option: ' + CurrentOpt);
      Exit;
    end;
    
    HasValue := False;
    if (i < High(Args)) and (Args[i+1][1] <> '-') then
    begin
      HasValue := True;
      if not ParseValue(Args[i+1], FOptions[OptionIdx].ArgType, Value) then
      begin
        SetError('Invalid value for option ' + CurrentOpt);
        Exit;
      end;
      Inc(i); // Skip the value
    end
    else if FOptions[OptionIdx].ArgType <> atBoolean then
    begin
      SetError('Option ' + CurrentOpt + ' requires a value');
      Exit;
    end;
    
    if HasValue then
    begin
      if Assigned(FOptions[OptionIdx].Callback) then
        FOptions[OptionIdx].Callback(Value);
      if Assigned(FOptions[OptionIdx].CallbackClass) then
        FOptions[OptionIdx].CallbackClass(Value);
    end
    else
    begin
      // For boolean flags without value, interpret presence as True
      if FOptions[OptionIdx].ArgType = atBoolean then
        Value.Bool := True
      else
        Value := FOptions[OptionIdx].DefaultValue;
      if Assigned(FOptions[OptionIdx].Callback) then
        FOptions[OptionIdx].Callback(Value);
      if Assigned(FOptions[OptionIdx].CallbackClass) then
        FOptions[OptionIdx].CallbackClass(Value);
    end;
    Inc(i);
  end;
  
  // Check for required options
  for j := Low(FOptions) to High(FOptions) do
  begin
    if FOptions[j].Required and (FOptions[j].ArgType <> atBoolean) then
    begin
      SetError('Missing required option: ' + FOptions[j].LongOpt);
      Exit;
    end;
  end;
end;

procedure TArgParser.ShowHelp;
var
  i: Integer;
  MaxShort, MaxLong: Integer;
begin
  // Calculate max widths for formatting
  MaxShort := 0;
  MaxLong := 0;
  for i := Low(FOptions) to High(FOptions) do
  begin
    if Length(FOptions[i].ShortOpt) > MaxShort then
      MaxShort := Length(FOptions[i].ShortOpt);
    if Length(FOptions[i].LongOpt) > MaxLong then
      MaxLong := Length(FOptions[i].LongOpt);
  end;
  
  WriteLn('Usage: ' + FUsage);
  WriteLn;
  WriteLn('Options:');
  for i := Low(FOptions) to High(FOptions) do
  begin
    Write('  ');
    Write('-' + FOptions[i].ShortOpt);
    Write(Space(MaxShort - Length(FOptions[i].ShortOpt)));
    Write(', --' + FOptions[i].LongOpt);
    Write(Space(MaxLong - Length(FOptions[i].LongOpt)));
    Write('  ');
    WriteLn(FOptions[i].HelpText);
  end;
end;

procedure TArgParser.ShowUsage;
begin
  WriteLn('Usage: ' + FUsage);
end;

procedure TArgParser.SetUsage(const AUsage: string);
begin
  FUsage := AUsage;
end;

procedure TArgParser.SetError(const AError: string);
begin
  FHasError := True;
  FError := AError;
end;

function TArgParser.GetError: string;
begin
  Result := FError;
end;

function TArgParser.HasError: Boolean;
begin
  Result := FHasError;
end;

function TArgParser.OptionCount: Integer;
begin
  Result := Length(FOptions);
end;

procedure TArgParser.AddOption(const Option: TArgOption);
begin
  SetLength(FOptions, Length(FOptions) + 1);
  FOptions[High(FOptions)] := Option;
end;

end.
