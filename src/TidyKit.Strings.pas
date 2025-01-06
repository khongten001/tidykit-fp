unit TidyKit.Strings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RegExpr, TidyKit.Core;

type
  { String match result }
  TStringMatch = record
    Text: string;
    Position: Integer;
    Length: Integer;
  end;
  TStringMatches = array of TStringMatch;

  { Interface for string operations }
  IStringKit = interface(IChainable)
    ['{F1A2B3C4-D5E6-F7G8-H9I0-J1K2L3M4N5O6}']
    function GetValue: string;
    
    { Basic operations }
    function From(const AValue: string): IStringKit;
    function ToString: string;
    
    { Transformations - Basic }
    function Trim: IStringKit;
    function TrimLeft: IStringKit;
    function TrimRight: IStringKit;
    function ToUpper: IStringKit;
    function ToLower: IStringKit;
    function Capitalize: IStringKit;
    
    { Transformations - Advanced }
    function Reverse: IStringKit;
    function Duplicate(const Count: Integer): IStringKit;
    function PadCenter(const TotalWidth: Integer; const PadChar: Char = ' '): IStringKit;
    function RemoveWhitespace: IStringKit;
    function CollapseWhitespace: IStringKit;  // Multiple spaces to single space
    
    { Modifications }
    function Replace(const OldPattern, NewPattern: string): IStringKit;
    function ReplaceRegEx(const Pattern, Replacement: string): IStringKit;
    function PadLeft(const TotalWidth: Integer; const PadChar: Char = ' '): IStringKit;
    function PadRight(const TotalWidth: Integer; const PadChar: Char = ' '): IStringKit;
    
    { Extractions }
    function SubString(const StartPos: Integer; const Length: Integer = -1): IStringKit;
    function Left(const Length: Integer): IStringKit;
    function Right(const Length: Integer): IStringKit;
    function Extract(const Pattern: string): TStringMatches;
    function ExtractAll(const Pattern: string): TStringArray;
    function Words: TStringArray;  // Split into words
    
    { Tests }
    function IsEmpty: Boolean;
    function Contains(const SubStr: string; const CaseSensitive: Boolean = True): Boolean;
    function StartsWith(const Prefix: string; const CaseSensitive: Boolean = True): Boolean;
    function EndsWith(const Suffix: string; const CaseSensitive: Boolean = True): Boolean;
    function Matches(const Pattern: string): Boolean;
    function Length: Integer;
    function CountSubString(const SubStr: string): Integer;
    
    { Properties }
    property Value: string read GetValue;
  end;

  { Implementation of string operations }
  TStringKit = class(TKitBase, IStringKit)
  private
    FValue: string;
    function GetValue: string;
  public
    constructor Create;
    
    { Interface implementations }
    function From(const AValue: string): IStringKit;
    function ToString: string; override;
    
    function Trim: IStringKit;
    function TrimLeft: IStringKit;
    function TrimRight: IStringKit;
    function ToUpper: IStringKit;
    function ToLower: IStringKit;
    function Capitalize: IStringKit;
    
    function Reverse: IStringKit;
    function Duplicate(const Count: Integer): IStringKit;
    function PadCenter(const TotalWidth: Integer; const PadChar: Char = ' '): IStringKit;
    function RemoveWhitespace: IStringKit;
    function CollapseWhitespace: IStringKit;
    
    function Replace(const OldPattern, NewPattern: string): IStringKit;
    function ReplaceRegEx(const Pattern, Replacement: string): IStringKit;
    function PadLeft(const TotalWidth: Integer; const PadChar: Char = ' '): IStringKit;
    function PadRight(const TotalWidth: Integer; const PadChar: Char = ' '): IStringKit;
    
    function SubString(const StartPos: Integer; const Length: Integer = -1): IStringKit;
    function Left(const Length: Integer): IStringKit;
    function Right(const Length: Integer): IStringKit;
    function Extract(const Pattern: string): TStringMatches;
    function ExtractAll(const Pattern: string): TStringArray;
    function Words: TStringArray;
    
    function IsEmpty: Boolean;
    function Contains(const SubStr: string; const CaseSensitive: Boolean = True): Boolean;
    function StartsWith(const Prefix: string; const CaseSensitive: Boolean = True): Boolean;
    function EndsWith(const Suffix: string; const CaseSensitive: Boolean = True): Boolean;
    function Matches(const Pattern: string): Boolean;
    function Length: Integer;
    function CountSubString(const SubStr: string): Integer;
  end;

implementation

{ TStringKit }

constructor TStringKit.Create;
begin
  inherited Create;
  FValue := '';
end;

function TStringKit.GetValue: string;
begin
  Result := FValue;
end;

function TStringKit.From(const AValue: string): IStringKit;
begin
  FValue := AValue;
  Result := Self;
end;

function TStringKit.ToString: string;
begin
  Result := FValue;
end;

function TStringKit.Trim: IStringKit;
begin
  FValue := SysUtils.Trim(FValue);
  Result := Self;
end;

function TStringKit.TrimLeft: IStringKit;
begin
  FValue := SysUtils.TrimLeft(FValue);
  Result := Self;
end;

function TStringKit.TrimRight: IStringKit;
begin
  FValue := SysUtils.TrimRight(FValue);
  Result := Self;
end;

function TStringKit.ToUpper: IStringKit;
begin
  FValue := UpperCase(FValue);
  Result := Self;
end;

function TStringKit.ToLower: IStringKit;
begin
  FValue := LowerCase(FValue);
  Result := Self;
end;

function TStringKit.Capitalize: IStringKit;
begin
  if Length(FValue) > 0 then
    FValue := UpperCase(FValue[1]) + Copy(FValue, 2, Length(FValue));
  Result := Self;
end;

function TStringKit.Replace(const OldPattern, NewPattern: string): IStringKit;
begin
  FValue := StringReplace(FValue, OldPattern, NewPattern, [rfReplaceAll]);
  Result := Self;
end;

function TStringKit.PadLeft(const TotalWidth: Integer; const PadChar: Char): IStringKit;
begin
  while Length(FValue) < TotalWidth do
    FValue := PadChar + FValue;
  Result := Self;
end;

function TStringKit.PadRight(const TotalWidth: Integer; const PadChar: Char): IStringKit;
begin
  while Length(FValue) < TotalWidth do
    FValue := FValue + PadChar;
  Result := Self;
end;

function TStringKit.SubString(const StartPos: Integer; const Length: Integer): IStringKit;
begin
  if Length < 0 then
    FValue := Copy(FValue, StartPos, System.Length(FValue))
  else
    FValue := Copy(FValue, StartPos, Length);
  Result := Self;
end;

function TStringKit.Left(const Length: Integer): IStringKit;
begin
  FValue := Copy(FValue, 1, Length);
  Result := Self;
end;

function TStringKit.Right(const Length: Integer): IStringKit;
begin
  FValue := Copy(FValue, System.Length(FValue) - Length + 1, Length);
  Result := Self;
end;

function TStringKit.IsEmpty: Boolean;
begin
  Result := FValue = '';
end;

function TStringKit.Contains(const SubStr: string; const CaseSensitive: Boolean): Boolean;
begin
  if CaseSensitive then
    Result := Pos(SubStr, FValue) > 0
  else
    Result := Pos(LowerCase(SubStr), LowerCase(FValue)) > 0;
end;

function TStringKit.StartsWith(const Prefix: string; const CaseSensitive: Boolean): Boolean;
begin
  if CaseSensitive then
    Result := Copy(FValue, 1, Length(Prefix)) = Prefix
  else
    Result := Copy(LowerCase(FValue), 1, Length(Prefix)) = LowerCase(Prefix);
end;

function TStringKit.EndsWith(const Suffix: string; const CaseSensitive: Boolean): Boolean;
begin
  if CaseSensitive then
    Result := Copy(FValue, Length(FValue) - Length(Suffix) + 1, Length(Suffix)) = Suffix
  else
    Result := Copy(LowerCase(FValue), Length(FValue) - Length(Suffix) + 1, Length(Suffix)) = LowerCase(Suffix);
end;

function TStringKit.Reverse: IStringKit;
var
  I: Integer;
  Len: Integer;
begin
  Len := System.Length(FValue);
  SetLength(Result, Len);
  for I := 1 to Len do
    Result[I] := FValue[Len - I + 1];
  FValue := Result;
  Result := Self;
end;

function TStringKit.Duplicate(const Count: Integer): IStringKit;
var
  I: Integer;
  Temp: string;
begin
  Temp := '';
  for I := 1 to Count do
    Temp := Temp + FValue;
  FValue := Temp;
  Result := Self;
end;

function TStringKit.PadCenter(const TotalWidth: Integer; const PadChar: Char): IStringKit;
var
  CurrentLen, PadLen, LeftPad: Integer;
begin
  CurrentLen := System.Length(FValue);
  if CurrentLen < TotalWidth then
  begin
    PadLen := TotalWidth - CurrentLen;
    LeftPad := PadLen div 2;
    FValue := StringOfChar(PadChar, LeftPad) + 
              FValue + 
              StringOfChar(PadChar, PadLen - LeftPad);
  end;
  Result := Self;
end;

function TStringKit.RemoveWhitespace: IStringKit;
var
  I: Integer;
  Temp: string;
begin
  Temp := '';
  for I := 1 to System.Length(FValue) do
    if not IsWhiteSpace(FValue[I]) then
      Temp := Temp + FValue[I];
  FValue := Temp;
  Result := Self;
end;

function TStringKit.CollapseWhitespace: IStringKit;
var
  I: Integer;
  Temp: string;
  LastWasSpace: Boolean;
begin
  Temp := '';
  LastWasSpace := False;
  for I := 1 to System.Length(FValue) do
  begin
    if IsWhiteSpace(FValue[I]) then
    begin
      if not LastWasSpace then
      begin
        Temp := Temp + ' ';
        LastWasSpace := True;
      end;
    end
    else
    begin
      Temp := Temp + FValue[I];
      LastWasSpace := False;
    end;
  end;
  FValue := Temp;
  Result := Self;
end;

function TStringKit.ReplaceRegEx(const Pattern, Replacement: string): IStringKit;
var
  RegEx: TRegExpr;
begin
  RegEx := TRegExpr.Create;
  try
    RegEx.Expression := Pattern;
    FValue := RegEx.Replace(FValue, Replacement, True);
  finally
    RegEx.Free;
  end;
  Result := Self;
end;

function TStringKit.Extract(const Pattern: string): TStringMatches;
var
  RegEx: TRegExpr;
  MatchCount: Integer;
begin
  SetLength(Result, 0);
  RegEx := TRegExpr.Create;
  try
    RegEx.Expression := Pattern;
    if RegEx.Exec(FValue) then
    begin
      repeat
        MatchCount := Length(Result);
        SetLength(Result, MatchCount + 1);
        Result[MatchCount].Text := RegEx.Match[0];
        Result[MatchCount].Position := RegEx.MatchPos[0];
        Result[MatchCount].Length := RegEx.MatchLen[0];
      until not RegEx.ExecNext;
    end;
  finally
    RegEx.Free;
  end;
end;

function TStringKit.ExtractAll(const Pattern: string): TStringArray;
var
  Matches: TStringMatches;
  I: Integer;
begin
  Matches := Extract(Pattern);
  SetLength(Result, Length(Matches));
  for I := 0 to High(Matches) do
    Result[I] := Matches[I].Text;
end;

function TStringKit.Words: TStringArray;
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    List.Delimiter := ' ';
    List.DelimitedText := CollapseWhitespace.Trim.Value;
    SetLength(Result, List.Count);
    Move(List.Strings[0], Result[0], List.Count * SizeOf(string));
  finally
    List.Free;
  end;
end;

function TStringKit.CountSubString(const SubStr: string): Integer;
var
  Offset: Integer;
begin
  Result := 0;
  Offset := 1;
  while True do
  begin
    Offset := PosEx(SubStr, FValue, Offset);
    if Offset = 0 then
      Break;
    Inc(Result);
    Inc(Offset);
  end;
end;

function TStringKit.Matches(const Pattern: string): Boolean;
var
  RegEx: TRegExpr;
begin
  RegEx := TRegExpr.Create;
  try
    RegEx.Expression := Pattern;
    Result := RegEx.Exec(FValue);
  finally
    RegEx.Free;
  end;
end;

end. 