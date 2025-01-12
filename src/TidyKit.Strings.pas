unit TidyKit.Strings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RegExpr, StrUtils;

type
  { String match result }
  TStringMatch = record
    Text: string;
    Position: Integer;
    Length: Integer;
  end;
  TStringMatches = array of TStringMatch;
  TStringArray = array of string;

  { String operations }
  TStringKit = class
  private
    class function IsWhiteSpace(const C: Char): Boolean; static;
  public
    // Basic string operations
    class function Trim(const Text: string): string; static;
    class function TrimLeft(const Text: string): string; static;
    class function TrimRight(const Text: string): string; static;
    class function ToUpper(const Text: string): string; static;
    class function ToLower(const Text: string): string; static;
    
    // Advanced string operations
    class function PadCenter(const Text: string; Width: Integer; PadChar: Char = ' '): string; static;
    class function PadLeft(const Text: string; Width: Integer; PadChar: Char = ' '): string; static;
    class function PadRight(const Text: string; Width: Integer; PadChar: Char = ' '): string; static;
    
    // Whitespace handling
    class function CollapseWhitespace(const Text: string): string; static;
    class function RemoveWhitespace(const Text: string): string; static;
    
    // Text manipulation
    class function DuplicateText(const Text: string; Count: Integer): string; static;
    class function ReverseText(const Text: string): string; static;
    class function CapitalizeText(const Text: string): string; static;
    
    // Pattern matching and extraction
    class function ExtractMatches(const Text, Pattern: string): TStringMatches; static;
    class function ExtractAllMatches(const Text, Pattern: string): TStringArray; static;
    class function MatchesPattern(const Text, Pattern: string): Boolean; static;
    class function ReplaceRegEx(const Text, Pattern, Replacement: string): string; static;
    class function ReplaceText(const Text, OldText, NewText: string): string; static;
    
    // Word operations
    class function GetWords(const Text: string): TStringArray; static;
    class function CountSubString(const Text, SubStr: string): Integer; static;
    
    // String tests
    class function Contains(const Text, SubStr: string): Boolean; static;
    class function StartsWith(const Text, Prefix: string): Boolean; static;
    class function EndsWith(const Text, Suffix: string): Boolean; static;
    class function IsEmpty(const Text: string): Boolean; static;
    class function GetLength(const Text: string): Integer; static;
    
    // Substring operations
    class function SubString(const Text: string; StartPos, Length: Integer): string; static;
    class function LeftStr(const Text: string; Length: Integer): string; static;
    class function RightStr(const Text: string; Length: Integer): string; static;
  end;

implementation

{ TStringKit }

class function TStringKit.IsWhiteSpace(const C: Char): Boolean;
begin
  Result := C in [' ', #9, #10, #13];
end;

class function TStringKit.Trim(const Text: string): string;
begin
  Result := SysUtils.Trim(Text);
end;

class function TStringKit.TrimLeft(const Text: string): string;
begin
  Result := SysUtils.TrimLeft(Text);
end;

class function TStringKit.TrimRight(const Text: string): string;
begin
  Result := SysUtils.TrimRight(Text);
end;

class function TStringKit.ToUpper(const Text: string): string;
begin
  Result := UpperCase(Text);
end;

class function TStringKit.ToLower(const Text: string): string;
begin
  Result := LowerCase(Text);
end;

class function TStringKit.PadCenter(const Text: string; Width: Integer; PadChar: Char): string;
var
  CurrentLen, PadLen, LeftPad: Integer;
begin
  CurrentLen := Length(Text);
  if CurrentLen < Width then
  begin
    PadLen := Width - CurrentLen;
    LeftPad := PadLen div 2;
    Result := StringOfChar(PadChar, LeftPad) + 
              Text + 
              StringOfChar(PadChar, PadLen - LeftPad);
  end
  else
    Result := Text;
end;

class function TStringKit.PadLeft(const Text: string; Width: Integer; PadChar: Char): string;
begin
  if Length(Text) < Width then
    Result := StringOfChar(PadChar, Width - Length(Text)) + Text
  else
    Result := Text;
end;

class function TStringKit.PadRight(const Text: string; Width: Integer; PadChar: Char): string;
begin
  if Length(Text) < Width then
    Result := Text + StringOfChar(PadChar, Width - Length(Text))
  else
    Result := Text;
end;

class function TStringKit.CollapseWhitespace(const Text: string): string;
var
  I: Integer;
  LastWasSpace: Boolean;
begin
  Result := '';
  LastWasSpace := False;
  for I := 1 to Length(Text) do
  begin
    if IsWhiteSpace(Text[I]) then
    begin
      if not LastWasSpace then
      begin
        Result := Result + ' ';
        LastWasSpace := True;
      end;
    end
    else
    begin
      Result := Result + Text[I];
      LastWasSpace := False;
    end;
  end;
end;

class function TStringKit.RemoveWhitespace(const Text: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(Text) do
    if not IsWhiteSpace(Text[I]) then
      Result := Result + Text[I];
end;

class function TStringKit.DuplicateText(const Text: string; Count: Integer): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Count do
    Result := Result + Text;
end;

class function TStringKit.ReverseText(const Text: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := Length(Text) downto 1 do
    Result := Result + Text[I];
end;

class function TStringKit.CapitalizeText(const Text: string): string;
var
  Words: TStringList;
  I: Integer;
  S: string;
begin
  Result := Text;
  if Length(Text) > 0 then
  begin
    Words := TStringList.Create;
    try
      Words.Delimiter := ' ';
      Words.StrictDelimiter := True;
      Words.DelimitedText := Text;
      
      for I := 0 to Words.Count - 1 do
        if Words[I] <> '' then
        begin
          S := Words[I];
          while (Length(S) > 0) and IsWhiteSpace(S[1]) do
            Delete(S, 1, 1);
          if Length(S) > 0 then
            Words[I] := UpperCase(S[1]) + Copy(S, 2, Length(S));
        end;
      
      Result := StringReplace(Words.DelimitedText, Words.Delimiter, ' ', [rfReplaceAll]);
    finally
      Words.Free;
    end;
  end;
end;

class function TStringKit.ExtractMatches(const Text, Pattern: string): TStringMatches;
var
  RegEx: TRegExpr;
  MatchCount: Integer;
begin
  Result := nil;
  SetLength(Result, 0);
  RegEx := TRegExpr.Create;
  try
    RegEx.Expression := Pattern;
    if RegEx.Exec(Text) then
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

class function TStringKit.ExtractAllMatches(const Text, Pattern: string): TStringArray;
var
  Matches: TStringMatches;
  I: Integer;
begin
  Result := nil;
  SetLength(Result, 0);
  Matches := ExtractMatches(Text, Pattern);
  SetLength(Result, Length(Matches));
  for I := 0 to High(Matches) do
    Result[I] := Matches[I].Text;
end;

class function TStringKit.MatchesPattern(const Text, Pattern: string): Boolean;
var
  RegEx: TRegExpr;
begin
  Result := False;
  RegEx := TRegExpr.Create;
  try
    RegEx.Expression := Pattern;
    Result := RegEx.Exec(Text);
  finally
    RegEx.Free;
  end;
end;

class function TStringKit.ReplaceRegEx(const Text, Pattern, Replacement: string): string;
var
  RegEx: TRegExpr;
begin
  RegEx := TRegExpr.Create;
  try
    RegEx.Expression := Pattern;
    Result := RegEx.Replace(Text, Replacement, True);
  finally
    RegEx.Free;
  end;
end;

class function TStringKit.ReplaceText(const Text, OldText, NewText: string): string;
begin
  Result := StringReplace(Text, OldText, NewText, [rfReplaceAll]);
end;

class function TStringKit.GetWords(const Text: string): TStringArray;
var
  List: TStringList;
  I: Integer;
  Temp: string;
begin
  Result := nil;
  List := TStringList.Create;
  try
    // First, replace commas with spaces and collapse whitespace
    Temp := StringReplace(Text, ', ', ' ', [rfReplaceAll]);
    Temp := StringReplace(Temp, ',', ' ', [rfReplaceAll]);
    Temp := CollapseWhitespace(Temp);
    Temp := Trim(Temp);
    
    List.Delimiter := ' ';
    List.StrictDelimiter := True;
    List.DelimitedText := Temp;
    
    SetLength(Result, List.Count);
    for I := 0 to List.Count - 1 do
      Result[I] := Trim(List[I]);
  finally
    List.Free;
  end;
end;

class function TStringKit.CountSubString(const Text, SubStr: string): Integer;
var
  P: Integer;
begin
  Result := 0;
  if SubStr = '' then
    Exit;
    
  P := Pos(SubStr, Text);
  while P > 0 do
  begin
    Inc(Result);
    P := Pos(SubStr, Text, P + 1);
  end;
end;

class function TStringKit.Contains(const Text, SubStr: string): Boolean;
begin
  Result := Pos(SubStr, Text) > 0;
end;

class function TStringKit.StartsWith(const Text, Prefix: string): Boolean;
begin
  Result := Copy(Text, 1, Length(Prefix)) = Prefix;
end;

class function TStringKit.EndsWith(const Text, Suffix: string): Boolean;
begin
  Result := Copy(Text, Length(Text) - Length(Suffix) + 1, Length(Suffix)) = Suffix;
end;

class function TStringKit.IsEmpty(const Text: string): Boolean;
begin
  Result := Text = '';
end;

class function TStringKit.GetLength(const Text: string): Integer;
begin
  Result := Length(Text);
end;

class function TStringKit.SubString(const Text: string; StartPos, Length: Integer): string;
begin
  Result := Copy(Text, StartPos, Length);
end;

class function TStringKit.LeftStr(const Text: string; Length: Integer): string;
begin
  Result := Copy(Text, 1, Length);
end;

class function TStringKit.RightStr(const Text: string; Length: Integer): string;
begin
  Result := Copy(Text, System.Length(Text) - Length + 1, Length);
end;

end. 