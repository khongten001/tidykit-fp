unit TidyKit.Strings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RegExpr, StrUtils, TidyKit.Core;

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
    ['{A1B2C3D4-5678-9ABC-DEF0-123456789ABC}']
    function GetContent: string;
    
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
    function GetWords: TStringArray;  // Split into words
    
    { Tests }
    function IsEmpty: Boolean;
    function Contains(const SubStr: string; const CaseSensitive: Boolean = True): Boolean;
    function StartsWith(const Prefix: string; const CaseSensitive: Boolean = True): Boolean;
    function EndsWith(const Suffix: string; const CaseSensitive: Boolean = True): Boolean;
    function MatchesPattern(const Pattern: string): Boolean;
    function TextLength: Integer;
    function CountSubString(const SubStr: string): Integer;
    
    { Properties }
    property Content: string read GetContent;
    property Words: TStringArray read GetWords;
  end;

  { Implementation of string operations }
  TStringKit = class(TKitBase, IStringKit)
  private
    FValue: string;
    function GetContent: string;
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
    function GetWords: TStringArray;
    
    function IsEmpty: Boolean;
    function Contains(const SubStr: string; const CaseSensitive: Boolean = True): Boolean;
    function StartsWith(const Prefix: string; const CaseSensitive: Boolean = True): Boolean;
    function EndsWith(const Suffix: string; const CaseSensitive: Boolean = True): Boolean;
    function MatchesPattern(const Pattern: string): Boolean;
    function TextLength: Integer;
    function CountSubString(const SubStr: string): Integer;
  end;

implementation

{ Helper functions }
function IsWhiteSpace(const C: Char): Boolean;
begin
  Result := C in [' ', #9, #10, #13];
end;

{ TStringKit }

constructor TStringKit.Create;
begin
  inherited Create;
  FValue := '';
end;

function TStringKit.GetContent: string;
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
var
  Words: TStringList;
  I: Integer;
  S: string;
begin
  Result := Self;
  if TextLength > 0 then
  begin
    Words := TStringList.Create;
    try
      Words.Delimiter := ',';
      Words.StrictDelimiter := True;
      Words.DelimitedText := FValue;
      for I := 0 to Words.Count - 1 do
        if Words[I] <> '' then
        begin
          S := Words[I];
          while (Length(S) > 0) and IsWhiteSpace(S[1]) do
            Delete(S, 1, 1);
          if Length(S) > 0 then
            Words[I] := UpperCase(S[1]) + Copy(S, 2, Length(S));
        end;
      FValue := StringReplace(Words.DelimitedText, ',', ', ', [rfReplaceAll]);
    finally
      Words.Free;
    end;
  end;
end;

function TStringKit.GetWords: TStringArray;
var
  List: TStringList;
  I: Integer;
  Temp: string;
begin
  Result := nil;
  List := TStringList.Create;
  try
    // First, replace commas with spaces and collapse whitespace
    Temp := StringReplace(FValue, ', ', ' ', [rfReplaceAll]);
    Temp := StringReplace(Temp, ',', ' ', [rfReplaceAll]);
    Temp := CollapseWhitespace.From(Temp).Trim.GetContent;
    
    List.Delimiter := ' ';
    List.StrictDelimiter := True;
    List.DelimitedText := Temp;
    
    SetLength(Result, List.Count);
    for I := 0 to List.Count - 1 do
      Result[I] := SysUtils.Trim(List[I]);
  finally
    List.Free;
  end;
end;

function TStringKit.Replace(const OldPattern, NewPattern: string): IStringKit;
begin
  FValue := StringReplace(FValue, OldPattern, NewPattern, [rfReplaceAll]);
  Result := Self;
end;

function TStringKit.PadLeft(const TotalWidth: Integer; const PadChar: Char): IStringKit;
begin
  Result := Self;
  while TextLength < TotalWidth do
    FValue := PadChar + FValue;
end;

function TStringKit.PadRight(const TotalWidth: Integer; const PadChar: Char): IStringKit;
begin
  Result := Self;
  while TextLength < TotalWidth do
    FValue := FValue + PadChar;
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
    Result := Copy(FValue, 1, System.Length(Prefix)) = Prefix
  else
    Result := Copy(LowerCase(FValue), 1, System.Length(Prefix)) = LowerCase(Prefix);
end;

function TStringKit.EndsWith(const Suffix: string; const CaseSensitive: Boolean): Boolean;
begin
  if CaseSensitive then
    Result := Copy(FValue, System.Length(FValue) - System.Length(Suffix) + 1, System.Length(Suffix)) = Suffix
  else
    Result := Copy(LowerCase(FValue), System.Length(FValue) - System.Length(Suffix) + 1, System.Length(Suffix)) = LowerCase(Suffix);
end;

function TStringKit.Reverse: IStringKit;
var
  I: Integer;
  Temp: string;
begin
  Result := Self;
  Temp := '';
  for I := System.Length(FValue) downto 1 do
    Temp := Temp + FValue[I];
  FValue := Temp;
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
  Result := Self;
  CurrentLen := System.Length(FValue);
  if CurrentLen < TotalWidth then
  begin
    PadLen := TotalWidth - CurrentLen;
    LeftPad := PadLen div 2;
    FValue := StringOfChar(PadChar, LeftPad) + 
              FValue + 
              StringOfChar(PadChar, PadLen - LeftPad);
  end;
end;

function TStringKit.RemoveWhitespace: IStringKit;
var
  I: Integer;
  Temp: string;
begin
  Result := Self;
  Temp := '';
  for I := 1 to System.Length(FValue) do
    if not IsWhiteSpace(FValue[I]) then
      Temp := Temp + FValue[I];
  FValue := Temp;
end;

function TStringKit.CollapseWhitespace: IStringKit;
var
  I: Integer;
  Temp: string;
  LastWasSpace: Boolean;
begin
  Result := Self;
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
  Result := nil;
  SetLength(Result, 0);
  RegEx := TRegExpr.Create;
  try
    RegEx.Expression := Pattern;
    if RegEx.Exec(FValue) then
    begin
      repeat
        MatchCount := System.Length(Result);
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
  Result := nil;
  SetLength(Result, 0);
  Matches := Extract(Pattern);
  SetLength(Result, System.Length(Matches));
  for I := 0 to High(Matches) do
    Result[I] := Matches[I].Text;
end;

function TStringKit.CountSubString(const SubStr: string): Integer;
var
  P: Integer;
begin
  Result := 0;
  if SubStr = '' then
    Exit;
    
  P := Pos(SubStr, FValue);
  while P > 0 do
  begin
    Inc(Result);
    P := Pos(SubStr, FValue, P + 1);
  end;
end;

function TStringKit.MatchesPattern(const Pattern: string): Boolean;
var
  RegEx: TRegExpr;
begin
  Result := False;
  RegEx := TRegExpr.Create;
  try
    RegEx.Expression := Pattern;
    Result := RegEx.Exec(FValue);
  finally
    RegEx.Free;
  end;
end;

function TStringKit.TextLength: Integer;
begin
  Result := System.Length(FValue);
end;

end. 