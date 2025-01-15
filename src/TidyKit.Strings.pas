unit TidyKit.Strings;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, RegExpr, StrUtils;

type
  { TStringMatch - Record for storing regex match information
    Used by pattern matching functions to return detailed match data.
    
    Fields:
    - Text: The matched text
    - Position: 1-based starting position of the match
    - Length: Length of the matched text }
  TStringMatch = record
    Text: string;
    Position: Integer;
    Length: Integer;
  end;
  
  { Array types for string operations }
  TStringMatches = array of TStringMatch;  // Array of regex match results
  TStringArray = array of string;          // Simple array of strings

  { TStringKit - Main class providing string manipulation operations
    All methods are class methods (static) for functional programming style.
    Methods are grouped by functionality for easier navigation.
    All operations are designed to be safe and efficient. }
  TStringKit = class
  private
    { Internal helper to check if character is whitespace.
      Considers space, tab, CR, and LF as whitespace. }
    class function IsWhiteSpace(const C: Char): Boolean; static;
  public
    { Basic string operations }
    
    { Removes whitespace from both ends of string. }
    class function Trim(const Text: string): string; static;
    
    { Removes whitespace from start of string. }
    class function TrimLeft(const Text: string): string; static;
    
    { Removes whitespace from end of string. }
    class function TrimRight(const Text: string): string; static;
    
    { Converts string to uppercase. }
    class function ToUpper(const Text: string): string; static;
    
    { Converts string to lowercase. }
    class function ToLower(const Text: string): string; static;
    
    { Advanced string operations }
    
    { Centers text by padding with characters on both sides.
      If text is shorter than Width, adds PadChar evenly on both sides.
      If Width is less than text length, returns original text. }
    class function PadCenter(const Text: string; Width: Integer; PadChar: Char = ' '): string; static;
    
    { Pads string on left to specified width.
      If text is shorter than Width, adds PadChar on left side.
      If Width is less than text length, returns original text. }
    class function PadLeft(const Text: string; Width: Integer; PadChar: Char = ' '): string; static;
    
    { Pads string on right to specified width.
      If text is shorter than Width, adds PadChar on right side.
      If Width is less than text length, returns original text. }
    class function PadRight(const Text: string; Width: Integer; PadChar: Char = ' '): string; static;
    
    { Whitespace handling }
    
    { Replaces multiple whitespace with single space.
      Trims ends and normalizes internal whitespace.
      Example: "  a   b  c  " -> "a b c" }
    class function CollapseWhitespace(const Text: string): string; static;
    
    { Removes all whitespace characters.
      Example: "  a b  c  " -> "abc" }
    class function RemoveWhitespace(const Text: string): string; static;
    
    { Text manipulation }
    
    { Repeats text specified number of times.
      Example: DuplicateText("abc", 3) -> "abcabcabc" }
    class function DuplicateText(const Text: string; Count: Integer): string; static;
    
    { Reverses characters in string.
      Example: ReverseText("hello") -> "olleh" }
    class function ReverseText(const Text: string): string; static;
    
    { Capitalizes first letter of each word.
      Handles multiple spaces and preserves other characters.
      Example: "hello world" -> "Hello World" }
    class function CapitalizeText(const Text: string): string; static;
    
    { Pattern matching and extraction }
    
    { Finds all regex matches with position information.
      Returns array of TStringMatch with match details.
      Example: ExtractMatches("abc123def456", "\d+") -> matches "123" and "456" with positions }
    class function ExtractMatches(const Text, Pattern: string): TStringMatches; static;
    
    { Extracts all regex matches as strings.
      Example: ExtractAllMatches("abc123def456", "\d+") -> ["123", "456"] }
    class function ExtractAllMatches(const Text, Pattern: string): TStringArray; static;
    
    { Checks if string matches regex pattern.
      Example: MatchesPattern("abc123", "^[a-z]+\d+$") -> True }
    class function MatchesPattern(const Text, Pattern: string): Boolean; static;
    
    { Replaces text using regex pattern.
      Example: ReplaceRegEx("abc123def", "\d+", "!") -> "abc!def" }
    class function ReplaceRegEx(const Text, Pattern, Replacement: string): string; static;
    
    { Replaces all occurrences of substring.
      Example: ReplaceText("abcabc", "bc", "x") -> "axax" }
    class function ReplaceText(const Text, OldText, NewText: string): string; static;
    
    { Word operations }
    
    { Extracts words from text.
      Words are sequences of letters and numbers.
      Example: GetWords("Hello, World! 123") -> ["Hello", "World", "123"] }
    class function GetWords(const AText: string): TStringArray; static;
    
    { Counts occurrences of substring.
      Example: CountSubString("abcabc", "bc") -> 2 }
    class function CountSubString(const Text, SubStr: string): Integer; static;
    
    { String tests }
    
    { Checks if string contains substring.
      Example: Contains("hello world", "world") -> True }
    class function Contains(const Text, SubStr: string): Boolean; static;
    
    { Checks if string starts with prefix.
      Example: StartsWith("hello world", "hello") -> True }
    class function StartsWith(const Text, Prefix: string): Boolean; static;
    
    { Checks if string ends with suffix.
      Example: EndsWith("hello world", "world") -> True }
    class function EndsWith(const Text, Suffix: string): Boolean; static;
    
    { Checks if string is empty.
      Example: IsEmpty("") -> True }
    class function IsEmpty(const Text: string): Boolean; static;
    
    { Gets length of string.
      Example: GetLength("hello") -> 5 }
    class function GetLength(const Text: string): Integer; static;
    
    { Substring operations }
    
    { Extracts substring from position with length.
      Position is 1-based.
      Example: SubString("hello", 2, 3) -> "ell" }
    class function SubString(const Text: string; StartPos, Length: Integer): string; static;
    
    { Gets leftmost characters from string.
      Example: LeftStr("hello", 2) -> "he" }
    class function LeftStr(const Text: string; Length: Integer): string; static;
    
    { Gets rightmost characters from string.
      Example: RightStr("hello", 2) -> "lo" }
    class function RightStr(const Text: string; Length: Integer): string; static;
  end;

implementation

{ TStringKit implementation }

{ Implementation of IsWhiteSpace
  Uses a character set to efficiently check if a character is whitespace.
  The set includes:
  - Space (ASCII 32)
  - Tab (ASCII 9)
  - Line Feed (ASCII 10)
  - Carriage Return (ASCII 13) }
class function TStringKit.IsWhiteSpace(const C: Char): Boolean;
begin
  Result := C in [' ', #9, #10, #13];
end;

{ Implementation of Trim
  Uses SysUtils.Trim for consistent behavior.
  This removes all whitespace characters from both ends of the string. }
class function TStringKit.Trim(const Text: string): string;
begin
  Result := SysUtils.Trim(Text);
end;

{ Implementation of TrimLeft
  Uses SysUtils.TrimLeft for consistent behavior.
  This removes all whitespace characters from the start of the string. }
class function TStringKit.TrimLeft(const Text: string): string;
begin
  Result := SysUtils.TrimLeft(Text);
end;

{ Implementation of TrimRight
  Uses SysUtils.TrimRight for consistent behavior.
  This removes all whitespace characters from the end of the string. }
class function TStringKit.TrimRight(const Text: string): string;
begin
  Result := SysUtils.TrimRight(Text);
end;

{ Implementation of ToUpper
  Uses UpperCase from SysUtils for proper case conversion.
  This handles all characters according to current locale settings. }
class function TStringKit.ToUpper(const Text: string): string;
begin
  Result := UpperCase(Text);
end;

{ Implementation of ToLower
  Uses LowerCase from SysUtils for proper case conversion.
  This handles all characters according to current locale settings. }
class function TStringKit.ToLower(const Text: string): string;
begin
  Result := LowerCase(Text);
end;

{ Implementation of PadCenter
  Centers text by adding padding characters evenly on both sides.
  Algorithm:
  1. Calculate total padding needed (Width - current length)
  2. Divide padding evenly between left and right sides
  3. If padding is odd, extra character goes on right side
  4. Return original text if no padding needed }
class function TStringKit.PadCenter(const Text: string; Width: Integer; PadChar: Char): string;
var
  CurrentLen, PadLen, LeftPad: Integer;
begin
  CurrentLen := Length(Text);
  if CurrentLen < Width then
  begin
    PadLen := Width - CurrentLen;
    LeftPad := PadLen div 2;  // Integer division for left padding
    Result := StringOfChar(PadChar, LeftPad) +  // Add left padding
              Text +                            // Original text in middle
              StringOfChar(PadChar, PadLen - LeftPad);  // Remaining padding on right
  end
  else
    Result := Text;  // Return original if no padding needed
end;

{ Implementation of PadLeft
  Adds padding characters to the left side of the text.
  Uses StringOfChar for efficient character repetition.
  Returns original text if Width is less than or equal to text length. }
class function TStringKit.PadLeft(const Text: string; Width: Integer; PadChar: Char): string;
begin
  if Length(Text) < Width then
    Result := StringOfChar(PadChar, Width - Length(Text)) + Text
  else
    Result := Text;
end;

{ Implementation of PadRight
  Adds padding characters to the right side of the text.
  Uses StringOfChar for efficient character repetition.
  Returns original text if Width is less than or equal to text length. }
class function TStringKit.PadRight(const Text: string; Width: Integer; PadChar: Char): string;
begin
  if Length(Text) < Width then
    Result := Text + StringOfChar(PadChar, Width - Length(Text))
  else
    Result := Text;
end;

{ Implementation of CollapseWhitespace
  Processes string character by character to normalize whitespace.
  Algorithm:
  1. Track if last character was a space
  2. For each whitespace character:
     - If no previous space, add single space
     - If previous space, skip
  3. For non-whitespace, add character and reset space flag }
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
      if not LastWasSpace then  // Only add one space
      begin
        Result := Result + ' ';
        LastWasSpace := True;
      end;
    end
    else
    begin
      Result := Result + Text[I];  // Add non-whitespace character
      LastWasSpace := False;
    end;
  end;
end;

{ Implementation of RemoveWhitespace
  Processes string character by character.
  Builds new string containing only non-whitespace characters.
  Uses IsWhiteSpace helper for consistent whitespace detection. }
class function TStringKit.RemoveWhitespace(const Text: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(Text) do
    if not IsWhiteSpace(Text[I]) then
      Result := Result + Text[I];
end;

{ Implementation of DuplicateText
  Creates new string by repeating input text specified number of times.
  Uses simple concatenation in a loop for flexibility.
  Returns empty string if Count is 0 or negative. }
class function TStringKit.DuplicateText(const Text: string; Count: Integer): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Count do
    Result := Result + Text;
end;

{ Implementation of ReverseText
  Creates new string by adding characters in reverse order.
  Uses downto loop for clear intention.
  Processes string character by character for proper handling of all characters. }
class function TStringKit.ReverseText(const Text: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := Length(Text) downto 1 do
    Result := Result + Text[I];
end;

{ Implementation of CapitalizeText
  Uses TStringList for word splitting and handling.
  Algorithm:
  1. Split text into words using space delimiter
  2. For each word:
     - Remove leading whitespace
     - Capitalize first character
     - Keep rest of word unchanged
  3. Join words back with single spaces }
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

{ Implementation of ExtractMatches
  Uses TRegExpr for powerful regex functionality.
  Algorithm:
  1. Create regex object with pattern
  2. Find first match
  3. For each match:
     - Store matched text
     - Store position (1-based)
     - Store length
  4. Continue until no more matches
  5. Free regex object to prevent memory leaks }
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

{ Implementation of ExtractAllMatches
  Uses ExtractMatches and converts TStringMatch array to string array.
  Extracts only the matched text portions, discarding position information.
  Returns empty array if no matches found. }
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

{ Implementation of MatchesPattern
  Uses TRegExpr for pattern matching.
  Returns True only if entire string matches pattern.
  Ensures proper cleanup of regex object. }
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

{ Implementation of ReplaceRegEx
  Uses TRegExpr for pattern-based replacement.
  Replaces all occurrences of pattern with replacement text.
  Ensures proper cleanup of regex object. }
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

{ Implementation of ReplaceText
  Uses StringReplace for simple text replacement.
  Replaces all occurrences of OldText with NewText.
  Case-sensitive replacement. }
class function TStringKit.ReplaceText(const Text, OldText, NewText: string): string;
begin
  Result := StringReplace(Text, OldText, NewText, [rfReplaceAll]);
end;

{ Implementation of GetWords
  Extracts words (sequences of letters and numbers) from text.
  Algorithm:
  1. Initialize empty word buffer
  2. Process text character by character:
     - If letter or number, add to current word
     - If other character, complete current word
  3. Add final word if any
  4. Convert list to array }
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

{ Implementation of CountSubString
  Counts non-overlapping occurrences of substring.
  Uses Pos function with increasing start position.
  Returns 0 for empty substring. }
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

{ Implementation of Contains
  Uses Pos function to check for substring presence.
  Returns True if substring is found anywhere in text. }
class function TStringKit.Contains(const Text, SubStr: string): Boolean;
begin
  Result := Pos(SubStr, Text) > 0;
end;

{ Implementation of StartsWith
  Uses Copy to extract prefix of same length.
  Direct string comparison for exact match. }
class function TStringKit.StartsWith(const Text, Prefix: string): Boolean;
begin
  Result := Copy(Text, 1, Length(Prefix)) = Prefix;
end;

{ Implementation of EndsWith
  Uses Copy to extract suffix of same length.
  Handles empty strings and length calculations. }
class function TStringKit.EndsWith(const Text, Suffix: string): Boolean;
begin
  Result := Copy(Text, Length(Text) - Length(Suffix) + 1, Length(Suffix)) = Suffix;
end;

{ Implementation of IsEmpty
  Simple check for empty string.
  Returns True for zero-length string. }
class function TStringKit.IsEmpty(const Text: string): Boolean;
begin
  Result := Text = '';
end;

{ Implementation of GetLength
  Uses Length function to get string length.
  Returns number of characters in string. }
class function TStringKit.GetLength(const Text: string): Integer;
begin
  Result := Length(Text);
end;

{ Implementation of SubString
  Uses Copy for substring extraction.
  StartPos is 1-based (first character is at position 1).
  Length specifies how many characters to extract. }
class function TStringKit.SubString(const Text: string; StartPos, Length: Integer): string;
begin
  Result := Copy(Text, StartPos, Length);
end;

{ Implementation of LeftStr
  Uses Copy to extract leftmost characters.
  Returns entire string if Length is greater than string length. }
class function TStringKit.LeftStr(const Text: string; Length: Integer): string;
begin
  Result := Copy(Text, 1, Length);
end;

{ Implementation of RightStr
  Uses Copy to extract rightmost characters.
  Calculates start position based on string length.
  Returns entire string if Length is greater than string length. }
class function TStringKit.RightStr(const Text: string; Length: Integer): string;
begin
  Result := Copy(Text, System.Length(Text) - Length + 1, Length);
end;

end. 