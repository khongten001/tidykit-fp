unit TidyKit.Strings;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, RegExpr, StrUtils;

type
  { TStringMatch - Record for storing regex match information
    Used by pattern matching functions to return detailed match data
    
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
    { Internal helper to check if character is whitespace
      Considers space, tab, CR, and LF as whitespace
      @param C Character to check
      @returns True if character is whitespace }
    class function IsWhiteSpace(const C: Char): Boolean; static;
  public
    { Basic string operations }
    
    { Removes whitespace from both ends of string
      @param Text String to trim
      @returns Trimmed string }
    class function Trim(const Text: string): string; static;
    
    { Removes whitespace from start of string
      @param Text String to trim
      @returns Left-trimmed string }
    class function TrimLeft(const Text: string): string; static;
    
    { Removes whitespace from end of string
      @param Text String to trim
      @returns Right-trimmed string }
    class function TrimRight(const Text: string): string; static;
    
    { Converts string to uppercase
      @param Text String to convert
      @returns Uppercase string }
    class function ToUpper(const Text: string): string; static;
    
    { Converts string to lowercase
      @param Text String to convert
      @returns Lowercase string }
    class function ToLower(const Text: string): string; static;
    
    { Advanced string operations }
    
    { Centers text by padding with characters on both sides
      @param Text String to pad
      @param Width Desired total width
      @param PadChar Character to use for padding (default space)
      @returns Centered string of specified width }
    class function PadCenter(const Text: string; Width: Integer; PadChar: Char = ' '): string; static;
    
    { Pads string on left to specified width
      @param Text String to pad
      @param Width Desired total width
      @param PadChar Character to use for padding (default space)
      @returns Left-padded string }
    class function PadLeft(const Text: string; Width: Integer; PadChar: Char = ' '): string; static;
    
    { Pads string on right to specified width
      @param Text String to pad
      @param Width Desired total width
      @param PadChar Character to use for padding (default space)
      @returns Right-padded string }
    class function PadRight(const Text: string; Width: Integer; PadChar: Char = ' '): string; static;
    
    { Whitespace handling }
    
    { Replaces multiple whitespace with single space
      Trims ends and normalizes internal whitespace
      @param Text String to process
      @returns String with collapsed whitespace }
    class function CollapseWhitespace(const Text: string): string; static;
    
    { Removes all whitespace characters
      @param Text String to process
      @returns String with no whitespace }
    class function RemoveWhitespace(const Text: string): string; static;
    
    { Text manipulation }
    
    { Repeats text specified number of times
      @param Text String to duplicate
      @param Count Number of times to repeat
      @returns Concatenated result }
    class function DuplicateText(const Text: string; Count: Integer): string; static;
    
    { Reverses characters in string
      @param Text String to reverse
      @returns Reversed string }
    class function ReverseText(const Text: string): string; static;
    
    { Capitalizes first letter of each word
      Handles multiple spaces and preserves other characters
      @param Text String to capitalize
      @returns Text with capitalized words }
    class function CapitalizeText(const Text: string): string; static;
    
    { Pattern matching and extraction }
    
    { Finds all regex matches with position information
      @param Text String to search in
      @param Pattern Regular expression pattern
      @returns Array of TStringMatch with match details
      @note Uses TRegExpr for regex support }
    class function ExtractMatches(const Text, Pattern: string): TStringMatches; static;
    
    { Extracts all regex matches as strings
      @param Text String to search in
      @param Pattern Regular expression pattern
      @returns Array of matched strings }
    class function ExtractAllMatches(const Text, Pattern: string): TStringArray; static;
    
    { Checks if string matches regex pattern
      @param Text String to check
      @param Pattern Regular expression pattern
      @returns True if string matches pattern }
    class function MatchesPattern(const Text, Pattern: string): Boolean; static;
    
    { Replaces text using regex pattern
      @param Text String to process
      @param Pattern Regular expression pattern
      @param Replacement Replacement text (can use regex groups)
      @returns Processed string }
    class function ReplaceRegEx(const Text, Pattern, Replacement: string): string; static;
    
    { Replaces all occurrences of substring
      @param Text String to process
      @param OldText Text to replace
      @param NewText Replacement text
      @returns Processed string }
    class function ReplaceText(const Text, OldText, NewText: string): string; static;
    
    { Word operations }
    
    { Extracts words from text
      Words are sequences of letters and numbers
      @param AText Text to process
      @returns Array of words }
    class function GetWords(const AText: string): TStringArray; static;
    
    { Counts occurrences of substring
      @param Text String to search in
      @param SubStr Substring to count
      @returns Number of occurrences }
    class function CountSubString(const Text, SubStr: string): Integer; static;
    
    { String tests }
    
    { Checks if string contains substring
      @param Text String to search in
      @param SubStr Substring to find
      @returns True if substring found }
    class function Contains(const Text, SubStr: string): Boolean; static;
    
    { Checks if string starts with prefix
      @param Text String to check
      @param Prefix Prefix to look for
      @returns True if string starts with prefix }
    class function StartsWith(const Text, Prefix: string): Boolean; static;
    
    { Checks if string ends with suffix
      @param Text String to check
      @param Suffix Suffix to look for
      @returns True if string ends with suffix }
    class function EndsWith(const Text, Suffix: string): Boolean; static;
    
    { Checks if string is empty
      @param Text String to check
      @returns True if string is empty }
    class function IsEmpty(const Text: string): Boolean; static;
    
    { Gets length of string
      @param Text String to measure
      @returns Length in characters }
    class function GetLength(const Text: string): Integer; static;
    
    { Substring operations }
    
    { Extracts substring from position with length
      @param Text Source string
      @param StartPos Starting position (1-based)
      @param Length Number of characters
      @returns Extracted substring }
    class function SubString(const Text: string; StartPos, Length: Integer): string; static;
    
    { Gets leftmost characters from string
      @param Text Source string
      @param Length Number of characters
      @returns Left part of string }
    class function LeftStr(const Text: string; Length: Integer): string; static;
    
    { Gets rightmost characters from string
      @param Text Source string
      @param Length Number of characters
      @returns Right part of string }
    class function RightStr(const Text: string; Length: Integer): string; static;
  end;

implementation

{ TStringKit implementation }

{ IsWhiteSpace - Internal helper to check if character is whitespace
  Considers space, tab, CR, and LF as whitespace
  @param C Character to check
  @returns True if character is whitespace }
class function TStringKit.IsWhiteSpace(const C: Char): Boolean;
begin
  Result := C in [' ', #9, #10, #13];
end;

// ... existing code ...

end. 