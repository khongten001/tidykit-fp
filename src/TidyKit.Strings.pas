unit TidyKit.Strings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RegExpr, StrUtils;

type
  { TStringMatch
    ------------
    Represents a single match result from pattern matching operations.
    Contains the matched text, its position in the original string,
    and the length of the match. }
  TStringMatch = record
    Text: string;      // The matched text
    Position: Integer; // Starting position in original string (1-based)
    Length: Integer;   // Length of the matched text
  end;
  
  { Array types for storing multiple results }
  TStringMatches = array of TStringMatch;
  TStringArray = array of string;

  { TStringKit
    ----------
    A comprehensive toolkit for string manipulation operations.
    Provides methods for common string operations like trimming,
    case conversion, padding, pattern matching, and more.
    
    All methods are static (class functions) for ease of use - 
    no need to create instances. }
  TStringKit = class
  private
    { Checks if a character is considered whitespace (space, tab, CR, LF).
      
      Parameters:
        C - The character to check.
        
      Returns:
        True if the character is whitespace, False otherwise. }
    class function IsWhiteSpace(const C: Char): Boolean; static;
  public
    { Removes leading and trailing whitespace from a string.
      
      Parameters:
        Text - The string to trim.
        
      Returns:
        The trimmed string. }
    class function Trim(const Text: string): string; static;
    
    { Removes leading whitespace from a string.
      
      Parameters:
        Text - The string to trim.
        
      Returns:
        The string with leading whitespace removed. }
    class function TrimLeft(const Text: string): string; static;
    
    { Removes trailing whitespace from a string.
      
      Parameters:
        Text - The string to trim.
        
      Returns:
        The string with trailing whitespace removed. }
    class function TrimRight(const Text: string): string; static;
    
    { Converts a string to uppercase.
      
      Parameters:
        Text - The string to convert.
        
      Returns:
        The uppercase version of the string. }
    class function ToUpper(const Text: string): string; static;
    
    { Converts a string to lowercase.
      
      Parameters:
        Text - The string to convert.
        
      Returns:
        The lowercase version of the string. }
    class function ToLower(const Text: string): string; static;
    
    { Centers text by adding padding characters on both sides.
      
      Parameters:
        Text - The string to pad.
        Width - The desired total width.
        PadChar - The character to use for padding (default space).
        
      Returns:
        The padded string centered within Width characters. }
    class function PadCenter(const Text: string; Width: Integer; PadChar: Char = ' '): string; static;
    
    { Adds padding characters to the left of a string.
      
      Parameters:
        Text - The string to pad.
        Width - The desired total width.
        PadChar - The character to use for padding (default space).
        
      Returns:
        The left-padded string. }
    class function PadLeft(const Text: string; Width: Integer; PadChar: Char = ' '): string; static;
    
    { Adds padding characters to the right of a string.
      
      Parameters:
        Text - The string to pad.
        Width - The desired total width.
        PadChar - The character to use for padding (default space).
        
      Returns:
        The right-padded string. }
    class function PadRight(const Text: string; Width: Integer; PadChar: Char = ' '): string; static;
    
    { Replaces multiple consecutive whitespace characters with a single space.
      
      Parameters:
        Text - The string to process.
        
      Returns:
        The string with collapsed whitespace. }
    class function CollapseWhitespace(const Text: string): string; static;
    
    { Removes all whitespace characters from a string.
      
      Parameters:
        Text - The string to process.
        
      Returns:
        The string with all whitespace removed. }
    class function RemoveWhitespace(const Text: string): string; static;
    
    { Repeats a string a specified number of times.
      
      Parameters:
        Text - The string to duplicate.
        Count - Number of times to repeat the string.
        
      Returns:
        The concatenated result. }
    class function DuplicateText(const Text: string; Count: Integer): string; static;
    
    { Reverses the characters in a string.
      
      Parameters:
        Text - The string to reverse.
        
      Returns:
        The reversed string. }
    class function ReverseText(const Text: string): string; static;
    
    { Capitalizes the first character of a string.
      
      Parameters:
        Text - The string to capitalize.
        
      Returns:
        The string with first character capitalized. }
    class function CapitalizeText(const Text: string): string; static;
    
    { Finds all matches of a regular expression pattern in text.
      
      Parameters:
        Text - The string to search in.
        Pattern - The regular expression pattern.
        
      Returns:
        Array of TStringMatch records with match details. }
    class function ExtractMatches(const Text, Pattern: string): TStringMatches; static;
    
    { Extracts all matching substrings using a pattern.
      
      Parameters:
        Text - The string to search in.
        Pattern - The regular expression pattern.
        
      Returns:
        Array of matched substrings. }
    class function ExtractAllMatches(const Text, Pattern: string): TStringArray; static;
    
    { Tests if a string matches a regular expression pattern.
      
      Parameters:
        Text - The string to test.
        Pattern - The regular expression pattern.
        
      Returns:
        True if the string matches the pattern. }
    class function MatchesPattern(const Text, Pattern: string): Boolean; static;
    
    { Replaces text matching a regular expression pattern.
      
      Parameters:
        Text - The string to process.
        Pattern - The regular expression pattern to match.
        Replacement - The replacement text.
        
      Returns:
        The string with replacements made. }
    class function ReplaceRegEx(const Text, Pattern, Replacement: string): string; static;
    
    { Replaces all occurrences of a substring.
      
      Parameters:
        Text - The string to process.
        OldText - The text to find and replace.
        NewText - The replacement text.
        
      Returns:
        The string with replacements made. }
    class function ReplaceText(const Text, OldText, NewText: string): string; static;
    
    { Splits text into an array of words.
      
      Parameters:
        AText - The text to split.
        
      Returns:
        Array of words from the text. }
    class function GetWords(const AText: string): TStringArray; static;
    
    { Counts occurrences of a substring in text.
      
      Parameters:
        Text - The string to search in.
        SubStr - The substring to count.
        
      Returns:
        Number of times the substring appears. }
    class function CountSubString(const Text, SubStr: string): Integer; static;
    
    { Tests if a string contains a substring.
      
      Parameters:
        Text - The string to search in.
        SubStr - The substring to find.
        
      Returns:
        True if substring is found. }
    class function Contains(const Text, SubStr: string): Boolean; static;
    
    { Tests if a string starts with a prefix.
      
      Parameters:
        Text - The string to test.
        Prefix - The prefix to check for.
        
      Returns:
        True if the string starts with prefix. }
    class function StartsWith(const Text, Prefix: string): Boolean; static;
    
    { Tests if a string ends with a suffix.
      
      Parameters:
        Text - The string to test.
        Suffix - The suffix to check for.
        
      Returns:
        True if the string ends with suffix. }
    class function EndsWith(const Text, Suffix: string): Boolean; static;
    
    { Tests if a string is empty.
      
      Parameters:
        Text - The string to test.
        
      Returns:
        True if the string is empty. }
    class function IsEmpty(const Text: string): Boolean; static;
    
    { Gets the length of a string.
      
      Parameters:
        Text - The string to measure.
        
      Returns:
        Number of characters in the string. }
    class function GetLength(const Text: string): Integer; static;
    
    { Extracts a substring.
      
      Parameters:
        Text - The source string.
        StartPos - Starting position (1-based).
        Length - Number of characters to extract.
        
      Returns:
        The extracted substring. }
    class function SubString(const Text: string; StartPos, Length: Integer): string; static;
    
    { Gets characters from start of string.
      
      Parameters:
        Text - The source string.
        Length - Number of characters to get.
        
      Returns:
        The leftmost characters. }
    class function LeftStr(const Text: string; Length: Integer): string; static;
    
    { Gets characters from end of string.
      
      Parameters:
        Text - The source string.
        Length - Number of characters to get.
        
      Returns:
        The rightmost characters. }
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

class function TStringKit.GetWords(const AText: string): TStringArray;
var
  WordList: TStringList;
  I: Integer;
  Word: string;
  Ch: Char;
  InWord: Boolean;
begin
  WordList := TStringList.Create;
  try
    Word := '';
    InWord := False;
    
    for Ch in AText do
    begin
      if Ch in ['A'..'Z', 'a'..'z', '0'..'9'] then
      begin
        Word := Word + Ch;
        InWord := True;
      end
      else if InWord then
      begin
        if Word <> '' then
          WordList.Add(Word);
        Word := '';
        InWord := False;
      end;
    end;
    
    if Word <> '' then
      WordList.Add(Word);
      
    SetLength(Result, WordList.Count);
    for I := 0 to WordList.Count - 1 do
      Result[I] := WordList[I];
  finally
    WordList.Free;
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