unit TidyKit.Strings;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, RegExpr, StrUtils, Math, DateUtils;

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
  
  {
    TMatchResults
    --------------
    Array of TStringMatch records for pattern matching results.
  }
  TMatchesResults = array of TStringMatch;

  {
   TMatchStrings
   -------------
   Array of strings for general string list operations or extracted matches.
  }
  TMatchStrings = array of string;

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
    
    { Computes minimum of three integer values.
      Used internally by the Levenshtein distance algorithm.
      
      Parameters:
        A, B, C - Three integer values to compare
        
      Returns:
        The smallest of the three values. }
    class function Min3(A, B, C: Integer): Integer; static;
    
    { Computes minimum of two integer values.
      Used internally by similarity algorithms.
      
      Parameters:
        A, B - Two integer values to compare
        
      Returns:
        The smaller of the two values. }
    class function Min2(A, B: Integer): Integer; static;
    
    { Computes maximum of two integer values.
      Used internally by similarity algorithms.
      
      Parameters:
        A, B - Two integer values to compare
        
      Returns:
        The larger of the two values. }
    class function Max2(A, B: Integer): Integer; static;
    
    { Counts the number of vowel groups in a word.
      Used for syllable estimation in readability calculations.
      
      Parameters:
        Word - The word to analyze.
        
      Returns:
        The estimated number of syllables. }
    class function CountVowelGroups(const Word: string): Integer; static;
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
    class function ExtractMatches(const Text, Pattern: string): TMatchesResults; static;
    
    { Extracts all matching substrings using a pattern.
      
      Parameters:
        Text - The string to search in.
        Pattern - The regular expression pattern.
        
      Returns:
        Array of matched substrings. }
    class function ExtractAllMatches(const Text, Pattern: string): TMatchStrings; static;
    
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
    class function GetWords(const AText: string): TMatchStrings; static;
    
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
    
    { Calculates the Levenshtein distance between two strings.
      The Levenshtein distance is the minimum number of single-character
      edits (insertions, deletions, or substitutions) required to
      change one string into another.
      
      Reference: https://en.wikipedia.org/wiki/Levenshtein_distance
      
      Parameters:
        S1, S2 - The two strings to compare.
        
      Returns:
        The Levenshtein distance (higher means more different). }
    class function LevenshteinDistance(const S1, S2: string): Integer; static;
    
    { Calculates the Levenshtein similarity ratio between two strings.
      Returns a value from 0 to 1, where 1 means identical strings.
      Formula: 1 - (LevenshteinDistance / max(length(S1), length(S2)))
      
      Reference: https://en.wikipedia.org/wiki/Levenshtein_distance#Relative_distance
      
      Parameters:
        S1, S2 - The two strings to compare.
        
      Returns:
        Similarity ratio from 0 to 1 (higher means more similar). }
    class function LevenshteinSimilarity(const S1, S2: string): Double; static;
    
    { Calculates the Hamming distance between two strings.
      Hamming distance counts the positions where corresponding symbols differ.
      Both strings must have the same length.
      
      Reference: https://en.wikipedia.org/wiki/Hamming_distance
      
      Parameters:
        S1, S2 - The two strings to compare (must be same length).
        
      Returns:
        The Hamming distance or -1 if strings have different lengths. }
    class function HammingDistance(const S1, S2: string): Integer; static;
    
    { Calculates the Jaro similarity between two strings.
      Jaro similarity is a measure of similarity between strings,
      especially good for short strings like names.
      
      Reference: https://en.wikipedia.org/wiki/Jaro%E2%80%93Winkler_distance#Jaro_similarity
      Formula: 1/3 * (m/|s₁| + m/|s₂| + (m-t)/m)
      
      Parameters:
        S1, S2 - The two strings to compare.
        
      Returns:
        Similarity value from 0 to 1 (higher means more similar). }
    class function JaroSimilarity(const S1, S2: string): Double; static;
    
    { Calculates the Jaro-Winkler similarity between two strings.
      A variant of Jaro that gives more weight to matching prefixes.
      
      Reference: https://en.wikipedia.org/wiki/Jaro%E2%80%93Winkler_distance#Jaro%E2%80%93Winkler_similarity
      Formula: Jaro + l*p*(1-Jaro) where l is prefix length (up to 4) and p is scaling factor
      
      Parameters:
        S1, S2 - The two strings to compare.
        
      Returns:
        Similarity value from 0 to 1 (higher means more similar). }
    class function JaroWinklerSimilarity(const S1, S2: string): Double; static;
    
    { Finds the longest common subsequence of two strings.
      A subsequence is a sequence derived from another sequence by
      deleting some elements without changing the order of the remaining elements.
      
      Reference: https://en.wikipedia.org/wiki/Longest_common_subsequence_problem
      
      Parameters:
        S1, S2 - The two strings to compare.
        
      Returns:
        The longest common subsequence as a string. }
    class function LongestCommonSubsequence(const S1, S2: string): string; static;
    
    { Calculates the similarity ratio based on the longest common subsequence.
      Formula: Length(LCS) / Max(Length(S1), Length(S2))
      
      Reference: https://en.wikipedia.org/wiki/Longest_common_subsequence_problem#Relation_to_other_problems
      
      Parameters:
        S1, S2 - The two strings to compare.
        
      Returns:
        Similarity ratio from 0 to 1 (higher means more similar). }
    class function LCSSimilarity(const S1, S2: string): Double; static;
    
    { Determines if two strings are "fuzzy" matches of each other.
      Returns true if their similarity ratio is above the specified threshold.
      
      Parameters:
        S1, S2 - The two strings to compare.
        Threshold - Minimum similarity ratio to consider a match (0 to 1).
        Method - Algorithm to use: 0=Levenshtein, 1=Jaro-Winkler, 2=LCS.
        
      Returns:
        True if the strings are similar enough, False otherwise. }
    class function IsFuzzyMatch(const S1, S2: string; Threshold: Double = 0.7; Method: Integer = 0): Boolean; static;
    
    { -------------------- Case Conversion Variants -------------------- }
    
    { Converts text to title case (first letter of each word capitalized).
      
      Parameters:
        Text - The string to convert.
        
      Returns:
        The text in title case format. }
    class function ToTitleCase(const Text: string): string; static;
    
    { Converts text to camel case (first word lowercase, subsequent words capitalized).
      
      Parameters:
        Text - The string to convert.
        
      Returns:
        The text in camelCase format. }
    class function ToCamelCase(const Text: string): string; static;
    
    { Converts text to Pascal case (all words capitalized with no separators).
      
      Parameters:
        Text - The string to convert.
        
      Returns:
        The text in PascalCase format. }
    class function ToPascalCase(const Text: string): string; static;
    
    { Converts text to snake case (lowercase with underscores).
      
      Parameters:
        Text - The string to convert.
        
      Returns:
        The text in snake_case format. }
    class function ToSnakeCase(const Text: string): string; static;
    
    { Converts text to kebab case (lowercase with hyphens).
      
      Parameters:
        Text - The string to convert.
        
      Returns:
        The text in kebab-case format. }
    class function ToKebabCase(const Text: string): string; static;
    
    { -------------------- String Validation Functions -------------------- }
    
    { Checks if a string is a valid email address.
      
      Parameters:
        Text - The string to check.
        
      Returns:
        True if the string is a valid email address, False otherwise. }
    class function IsValidEmail(const Text: string): Boolean; static;
    
    { Checks if a string is a valid URL.
      
      Parameters:
        Text - The string to check.
        
      Returns:
        True if the string is a valid URL, False otherwise. }
    class function IsValidURL(const Text: string): Boolean; static;
    
    { Checks if a string is a valid IP address (v4 or v6).
      
      Parameters:
        Text - The string to check.
        
      Returns:
        True if the string is a valid IP address, False otherwise. }
    class function IsValidIP(const Text: string): Boolean; static;
    
    { Checks if a string is a valid IPv4 address.
      
      Parameters:
        Text - The string to check.
        
      Returns:
        True if the string is a valid IPv4 address, False otherwise. }
    class function IsValidIPv4(const Text: string): Boolean; static;
    
    { Checks if a string is a valid IPv6 address.
      
      Parameters:
        Text - The string to check.
        
      Returns:
        True if the string is a valid IPv6 address, False otherwise. }
    class function IsValidIPv6(const Text: string): Boolean; static;
    
    { Checks if a string is a valid date according to the specified format.
      
      Parameters:
        Text - The string to check.
        Format - The date format (e.g. 'yyyy-mm-dd').
        
      Returns:
        True if the string is a valid date, False otherwise. }
    class function IsValidDate(const Text, Format: string): Boolean; static;
    
    { -------------------- String Transformation and Formatting -------------------- }
    
    { Truncates a string with ellipsis if it exceeds the specified length.
      
      Parameters:
        Text - The string to truncate.
        MaxLength - The maximum allowed length.
        Ellipsis - The ellipsis to append (default '...').
        
      Returns:
        The truncated string with ellipsis if needed. }
    class function Truncate(const Text: string; MaxLength: Integer; const Ellipsis: string = '...'): string; static;
    
    { Formats a file size in bytes to a human-readable string (KB, MB, GB, etc.).
      
      Parameters:
        Size - The file size in bytes.
        
      Returns:
        A human-readable string representation of the file size. }
    class function FormatFileSize(Size: Int64): string; static;
    
    { Formats a number with thousand separators.
      
      Parameters:
        Value - The number to format.
        ThousandSeparator - The character to use as thousand separator (default ',').
        
      Returns:
        The formatted number string. }
    class function FormatNumber(const Value: Int64; ThousandSeparator: Char = ','): string; static;
    
    { Formats a floating-point number with specified decimal and thousand separators.
      
      Parameters:
        Value - The number to format.
        Decimals - The number of decimal places (default 2).
        DecimalSeparator - The character to use as decimal separator (default '.').
        ThousandSeparator - The character to use as thousand separator (default ',').
        
      Returns:
        The formatted number string. }
    class function FormatFloat(const Value: Double; Decimals: Integer = 2; DecimalSeparator: Char = '.'; ThousandSeparator: Char = ','): string; static;
    
    { -------------------- String Splitting and Joining -------------------- }
    
    { Joins an array of strings with a delimiter.
      
      Parameters:
        Strings - The array of strings to join.
        Delimiter - The delimiter to insert between strings.
        
      Returns:
        The joined string. }
    class function Join(const Strings: TMatchStrings; const Delimiter: string): string; static;
    
    { Splits a string by a delimiter with advanced options.
      
      Parameters:
        Text - The string to split.
        Delimiter - The delimiter to split by.
        MaxSplit - Maximum number of splits (0 = unlimited, default).
        RemoveEmptyEntries - Whether to remove empty entries (default False).
        
      Returns:
        An array of substrings. }
    class function Split(const Text, Delimiter: string; MaxSplit: Integer = 0; RemoveEmptyEntries: Boolean = False): TMatchStrings; static;
    
    { -------------------- Phonetic Algorithms -------------------- }
    
    { Generates a Soundex code for phonetic matching.
      Soundex is a phonetic algorithm for indexing names by sound, as pronounced in English.
      
      Reference: https://en.wikipedia.org/wiki/Soundex
      
      Parameters:
        Text - The string to encode.
        
      Returns:
        The Soundex code. }
    class function Soundex(const Text: string): string; static;
    
    { Generates a Metaphone code for phonetic matching.
      Metaphone is a phonetic algorithm for indexing words by their English pronunciation.
      It's an improvement over Soundex with better handling of irregularities.
      
      Reference: https://en.wikipedia.org/wiki/Metaphone
      
      Parameters:
        Text - The string to encode.
        
      Returns:
        The Metaphone code. }
    class function Metaphone(const Text: string): string; static;
    
    { -------------------- Text Analysis -------------------- }
    
    { Counts the number of words in a text.
      
      Parameters:
        Text - The text to analyze.
        
      Returns:
        The number of words. }
    class function CountWords(const Text: string): Integer; static;
    
    { Calculates the Flesch-Kincaid readability score for a text.
      This score indicates how difficult a passage in English is to understand.
      Higher scores indicate material that is easier to read; lower scores indicate difficulty.
      
      Formula: 206.835 - 1.015 × (words/sentences) - 84.6 × (syllables/words)
      
      Reference: https://en.wikipedia.org/wiki/Flesch%E2%80%93Kincaid_readability_tests
      
      Parameters:
        Text - The text to analyze.
        
      Returns:
        The readability score (higher = easier to read). }
    class function FleschKincaidReadability(const Text: string): Double; static;
    
    { Generates n-grams from a text.
      N-grams are contiguous sequences of n items from a given sample of text.
      
      Reference: https://en.wikipedia.org/wiki/N-gram
      
      Parameters:
        Text - The text to process.
        N - The size of each n-gram.
        
      Returns:
        An array of n-grams. }
    class function GenerateNGrams(const Text: string; N: Integer): TMatchStrings; static;
    
    { -------------------- Encoding/Decoding Functions -------------------- }
    
    { Encodes a string for safe use in HTML.
      
      Parameters:
        Text - The string to encode.
        
      Returns:
        The HTML-encoded string. }
    class function HTMLEncode(const Text: string): string; static;
    
    { Decodes an HTML-encoded string.
      
      Parameters:
        Text - The string to decode.
        
      Returns:
        The decoded string. }
    class function HTMLDecode(const Text: string): string; static;
    
    { Encodes a string for safe use in URLs.
      
      Parameters:
        Text - The string to encode.
        
      Returns:
        The URL-encoded string. }
    class function URLEncode(const Text: string): string; static;
    
    { Decodes a URL-encoded string.
      
      Parameters:
        Text - The string to decode.
        
      Returns:
        The decoded string. }
    class function URLDecode(const Text: string): string; static;
    
    { -------------------- Number Conversions -------------------- }
    
    { Converts an integer to a Roman numeral string.
      
      Parameters:
        Value - The integer to convert (must be between 1 and 3999).
        
      Returns:
        The Roman numeral representation or empty string if out of range. }
    class function ToRoman(Value: Integer): string; static;
    
    { Converts a Roman numeral string to an integer.
      
      Parameters:
        RomanNumeral - The Roman numeral string to convert.
        
      Returns:
        The integer value or 0 if invalid. }
    class function FromRoman(const RomanNumeral: string): Integer; static;
    
    { Converts an integer to its ordinal text form.
      
      Parameters:
        Value - The integer to convert.
        
      Returns:
        The ordinal text (e.g., '1st', '2nd', '3rd', '42nd'). }
    class function ToOrdinal(Value: Integer): string; static;
    
    { Converts a number to its word representation.
      
      Parameters:
        Value - The number to convert.
        
      Returns:
        The number as words (e.g., 'forty-two' for 42). }
    class function NumberToWords(Value: Int64): string; static;
    
    { -------------------- Encoding/Decoding Functions -------------------- }
    
    { Encodes a string to Base64.
      
      Parameters:
        Text - The string to encode.
        
      Returns:
        The Base64-encoded string. }
    class function Base64Encode(const Text: string): string; static;
    
    { Decodes a Base64 string.
      
      Parameters:
        Base64Text - The Base64 string to decode.
        
      Returns:
        The decoded string. }
    class function Base64Decode(const Base64Text: string): string; static;
    
    { Converts a string to hexadecimal representation.
      
      Parameters:
        Text - The string to convert.
        
      Returns:
        The hexadecimal representation. }
    class function HexEncode(const Text: string): string; static;
    
    { Converts a hexadecimal string back to its original form.
      
      Parameters:
        HexText - The hexadecimal string to convert.
        
      Returns:
        The original string. }
    class function HexDecode(const HexText: string): string; static;
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

class function TStringKit.ExtractMatches(const Text, Pattern: string): TMatchesResults;
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

class function TStringKit.ExtractAllMatches(const Text, Pattern: string): TMatchStrings;
var
  Matches: TMatchesResults;
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

class function TStringKit.GetWords(const AText: string): TMatchStrings;
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

class function TStringKit.Min3(A, B, C: Integer): Integer;
begin
  if A < B then
    if A < C then
      Result := A
    else
      Result := C
  else
    if B < C then
      Result := B
    else
      Result := C;
end;

class function TStringKit.Min2(A, B: Integer): Integer;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

class function TStringKit.Max2(A, B: Integer): Integer;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

class function TStringKit.LevenshteinDistance(const S1, S2: string): Integer;
var
  D: array of array of Integer;
  I, J, M, N, Cost: Integer;
begin
  // Special case for empty strings
  if S1 = '' then
    Exit(Length(S2));
  if S2 = '' then
    Exit(Length(S1));

  M := Length(S1);
  N := Length(S2);
  SetLength(D, M + 1, N + 1);

  for I := 0 to M do
    D[I, 0] := I;
  for J := 0 to N do
    D[0, J] := J;

  for I := 1 to M do
  begin
    for J := 1 to N do
    begin
      if S1[I] = S2[J] then
        Cost := 0
      else
        Cost := 1;
      D[I, J] := Min3(D[I - 1, J] + 1, D[I, J - 1] + 1, D[I - 1, J - 1] + Cost);
    end;
  end;

  Result := D[M, N];
end;

class function TStringKit.LevenshteinSimilarity(const S1, S2: string): Double;
var
  Distance: Integer;
begin
  Distance := LevenshteinDistance(S1, S2);
  Result := 1.0 - (Distance / Max(Length(S1), Length(S2)));
end;

class function TStringKit.HammingDistance(const S1, S2: string): Integer;
var
  I, Distance: Integer;
begin
  Distance := 0;
  if Length(S1) <> Length(S2) then
    Exit(-1);
  for I := 1 to Length(S1) do
    if S1[I] <> S2[I] then
      Inc(Distance);
  Result := Distance;
end;

class function TStringKit.JaroSimilarity(const S1, S2: string): Double;
var
  M, T, MatchCount: Integer;
  MatchDistance: array of Boolean;
  I: Integer;
begin
  // Handle empty strings
  if (Length(S1) = 0) and (Length(S2) = 0) then
    Exit(1.0);
  if (Length(S1) = 0) or (Length(S2) = 0) then
    Exit(0.0);

  // For Jaro similarity, strings should be of the same length
  // If they are different, we'll consider them completely dissimilar
  if Length(S1) <> Length(S2) then
    Exit(0.0);

  M := 0;
  T := 0;
  SetLength(MatchDistance, Length(S1));

  for I := 0 to Length(S1) - 1 do
  begin
    if S1[I+1] = S2[I+1] then
    begin
      MatchDistance[I] := True;
      Inc(M);
    end
    else
    begin
      MatchDistance[I] := False;
      Inc(T);
    end;
  end;

  if M = 0 then
    Result := 0.0
  else
  begin
    // Calculate Jaro similarity
    Result := (M / Length(S1) + M / Length(S2) + (M - T/2) / M) / 3;
  end;
end;

class function TStringKit.JaroWinklerSimilarity(const S1, S2: string): Double;
var
  JaroDistance: Double;
  PrefixLength: Integer;
begin
  // Special case for empty strings
  if (S1 = '') or (S2 = '') then
    Exit(JaroSimilarity(S1, S2));
    
  JaroDistance := JaroSimilarity(S1, S2);
  PrefixLength := 0;

  // Calculate the length of the common prefix (up to 4 characters max)
  while (PrefixLength < Length(S1)) and 
        (PrefixLength < Length(S2)) and 
        (PrefixLength < 4) and 
        (S1[PrefixLength+1] = S2[PrefixLength+1]) do
    Inc(PrefixLength);

  // Apply the Winkler adjustment (common prefix improves the similarity score)
  Result := JaroDistance + (PrefixLength * 0.1 * (1 - JaroDistance));
  
  // Ensure the result is between 0 and 1
  if Result > 1.0 then
    Result := 1.0;
end;

class function TStringKit.LongestCommonSubsequence(const S1, S2: string): string;
var
  LCS: array of array of Integer;
  I, J: Integer;
begin
  // Handle empty strings
  if (S1 = '') or (S2 = '') then
    Exit('');
    
  SetLength(LCS, Length(S1) + 1, Length(S2) + 1);

  for I := 0 to Length(S1) do
    LCS[I, 0] := 0;
  for J := 0 to Length(S2) do
    LCS[0, J] := 0;

  for I := 1 to Length(S1) do
  begin
    for J := 1 to Length(S2) do
    begin
      if S1[I] = S2[J] then
        LCS[I, J] := LCS[I - 1, J - 1] + 1
      else
        LCS[I, J] := Max2(LCS[I - 1, J], LCS[I, J - 1]);
    end;
  end;

  // Backtrack to find the actual subsequence
  Result := '';
  I := Length(S1);
  J := Length(S2);
  
  while (I > 0) and (J > 0) do
  begin
    if S1[I] = S2[J] then
    begin
      Result := S1[I] + Result;
      Dec(I);
      Dec(J);
    end
    else if LCS[I - 1, J] >= LCS[I, J - 1] then
      Dec(I)
    else
      Dec(J);
  end;
end;

class function TStringKit.LCSSimilarity(const S1, S2: string): Double;
var
  LCS: string;
begin
  LCS := LongestCommonSubsequence(S1, S2);
  Result := Length(LCS) / Max(Length(S1), Length(S2));
end;

class function TStringKit.IsFuzzyMatch(const S1, S2: string; Threshold: Double = 0.7; Method: Integer = 0): Boolean;
var
  Similarity: Double;
begin
  // Special case: identical strings are always a match
  if S1 = S2 then
    Exit(True);
    
  // Special case: if either string is empty (but not both), return false
  if ((S1 = '') and (S2 <> '')) or ((S1 <> '') and (S2 = '')) then
    Exit(False);
  
  // Calculate similarity based on chosen method
  case Method of
    0: Similarity := LevenshteinSimilarity(S1, S2);
    1: Similarity := JaroWinklerSimilarity(S1, S2);
    2: Similarity := LCSSimilarity(S1, S2);
  else
    Similarity := 0.0;
  end;
  
  Result := Similarity >= Threshold;
end;

class function TStringKit.ToTitleCase(const Text: string): string;
var
  I: Integer;
  PrevIsSpace: Boolean;
begin
  Result := LowerCase(Text);
  if Result = '' then
    Exit;
    
  PrevIsSpace := True; // Start with True to capitalize first letter
  
  for I := 1 to Length(Result) do
  begin
    if Result[I] in [' ', #9, #10, #13, '-', '.', '_', ':', ';', '!', '?'] then
      PrevIsSpace := True
    else if PrevIsSpace then
    begin
      Result[I] := UpCase(Result[I]);
      PrevIsSpace := False;
    end;
  end;
end;

class function TStringKit.ToCamelCase(const Text: string): string;
var
  Words: TMatchStrings;
  I: Integer;
  Word: string;
begin
  Words := GetWords(Text);
  Result := '';
  
  if Length(Words) = 0 then
    Exit;
    
  // First word is lowercase
  if Length(Words) > 0 then
    Result := LowerCase(Words[0]);
    
  // Rest of words have first letter capitalized
  for I := 1 to High(Words) do
  begin
    Word := Words[I];
    if Word <> '' then
      Result := Result + UpperCase(Word[1]) + LowerCase(Copy(Word, 2, Length(Word)));
  end;
end;

class function TStringKit.ToPascalCase(const Text: string): string;
var
  Words: TMatchStrings;
  I: Integer;
  Word: string;
begin
  Words := GetWords(Text);
  Result := '';
  
  // All words have first letter capitalized
  for I := 0 to High(Words) do
  begin
    Word := Words[I];
    if Word <> '' then
      Result := Result + UpperCase(Word[1]) + LowerCase(Copy(Word, 2, Length(Word)));
  end;
end;

class function TStringKit.ToSnakeCase(const Text: string): string;
var
  Words: TMatchStrings;
  I: Integer;
begin
  Words := GetWords(Text);
  Result := '';
  
  for I := 0 to High(Words) do
  begin
    if I > 0 then
      Result := Result + '_';
    Result := Result + LowerCase(Words[I]);
  end;
end;

class function TStringKit.ToKebabCase(const Text: string): string;
var
  Words: TMatchStrings;
  I: Integer;
begin
  Words := GetWords(Text);
  Result := '';
  
  for I := 0 to High(Words) do
  begin
    if I > 0 then
      Result := Result + '-';
    Result := Result + LowerCase(Words[I]);
  end;
end;

class function TStringKit.IsValidEmail(const Text: string): Boolean;
begin
  // Match pattern for email addresses
  // Simple pattern: word@word.word, more complex patterns possible
  Result := MatchesPattern(Text, '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$');
end;

class function TStringKit.IsValidURL(const Text: string): Boolean;
begin
  // Match pattern for URLs
  // This covers http, https, ftp protocols with domain names
  Result := MatchesPattern(Text, '^(https?|ftp)://[-a-zA-Z0-9@:%._\+~#=]{2,256}\.[a-z]{2,6}\b([-a-zA-Z0-9@:%_\+.~#?&//=]*)$') or
           MatchesPattern(Text, '^(www)\.[-a-zA-Z0-9@:%._\+~#=]{2,256}\.[a-z]{2,6}\b([-a-zA-Z0-9@:%_\+.~#?&//=]*)$');
end;

class function TStringKit.IsValidIP(const Text: string): Boolean;
begin
  // Check if it's either a valid IPv4 or IPv6 address
  Result := IsValidIPv4(Text) or IsValidIPv6(Text);
end;

class function TStringKit.IsValidIPv4(const Text: string): Boolean;
begin
  // Pattern for IPv4: x.x.x.x where x is 0-255
  Result := MatchesPattern(Text, '^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$');
end;

class function TStringKit.IsValidIPv6(const Text: string): Boolean;
begin
  // Pattern for IPv6: 8 groups of 1-4 hexadecimal digits separated by colons
  // Also allows compressed IPv6 notation with :: for groups of zeros
  Result := MatchesPattern(Text, '^(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))$');
end;

class function TStringKit.IsValidDate(const Text, Format: string): Boolean;
var
  Day, Month, Year: Word;
  FormatLC: string;
  TextCopy: string;
  I, DayPos, MonthPos, YearPos: Integer;
  Ch: Char;
begin
  // If empty text, not valid
  if Text = '' then
    Exit(False);
  
  // Manual validation based on format pattern and text
  FormatLC := LowerCase(Format);
  TextCopy := Text;
  
  // Find positions of day, month, year in format
  DayPos := Pos('dd', FormatLC);
  MonthPos := Pos('mm', FormatLC);
  YearPos := Pos('yyyy', FormatLC);
  
  // Ensure all components are in the format
  if (DayPos = 0) or (MonthPos = 0) or (YearPos = 0) then
    Exit(False);
  
  // Substitute all non-digits to format separators
  for I := 1 to Length(TextCopy) do
    if not (TextCopy[I] in ['0'..'9']) then
      TextCopy[I] := ' ';
  
  // Extract day, month, year based on positions in format
  try
    // Extract components from TextCopy using positions from format
    if (DayPos > 0) and (MonthPos > 0) and (YearPos > 0) then
    begin
      // Simple ordering logic based on positions
      if (DayPos < MonthPos) and (MonthPos < YearPos) then // dd-mm-yyyy
      begin
        Day := StrToIntDef(ExtractWord(1, TextCopy, [' ']), 0);
        Month := StrToIntDef(ExtractWord(2, TextCopy, [' ']), 0);
        Year := StrToIntDef(ExtractWord(3, TextCopy, [' ']), 0);
      end
      else if (YearPos < MonthPos) and (MonthPos < DayPos) then // yyyy-mm-dd
      begin
        Year := StrToIntDef(ExtractWord(1, TextCopy, [' ']), 0);
        Month := StrToIntDef(ExtractWord(2, TextCopy, [' ']), 0);
        Day := StrToIntDef(ExtractWord(3, TextCopy, [' ']), 0);
      end
      else if (MonthPos < DayPos) and (DayPos < YearPos) then // mm-dd-yyyy
      begin
        Month := StrToIntDef(ExtractWord(1, TextCopy, [' ']), 0);
        Day := StrToIntDef(ExtractWord(2, TextCopy, [' ']), 0);
        Year := StrToIntDef(ExtractWord(3, TextCopy, [' ']), 0);
      end
      else
      begin
        // Extract words regardless of format order (default)
        // Here we're assuming they're in dd mm yyyy order if not recognized
        Day := StrToIntDef(ExtractWord(1, TextCopy, [' ']), 0);
        Month := StrToIntDef(ExtractWord(2, TextCopy, [' ']), 0);
        Year := StrToIntDef(ExtractWord(3, TextCopy, [' ']), 0);
      end;
      
      // Validate components
      Result := (Year >= 1) and (Year <= 9999) and
                (Month >= 1) and (Month <= 12) and
                (Day >= 1) and (Day <= DaysInAMonth(Year, Month));
    end
    else
      Result := False;
  except
    Result := False;
  end;
end;

class function TStringKit.Truncate(const Text: string; MaxLength: Integer; const Ellipsis: string = '...'): string;
begin
  if Length(Text) <= MaxLength then
    Result := Text
  else
    Result := Copy(Text, 1, MaxLength - Length(Ellipsis)) + Ellipsis;
end;

class function TStringKit.FormatFileSize(Size: Int64): string;
const
  KB = 1024;
  MB = KB * 1024;
  GB = MB * 1024;
  TB = GB * 1024;
begin
  if Size < KB then
    Result := Format('%d B', [Size])
  else if Size < MB then
    Result := Format('%.2f KB', [Size / KB])
  else if Size < GB then
    Result := Format('%.2f MB', [Size / MB])
  else if Size < TB then
    Result := Format('%.2f GB', [Size / GB])
  else
    Result := Format('%.2f TB', [Size / TB]);
end;

class function TStringKit.FormatNumber(const Value: Int64; ThousandSeparator: Char = ','): string;
var
  I, GroupCount, Len: Integer;
  Temp: string;
begin
  // Convert to string first
  Result := IntToStr(Value);
  Len := Length(Result);
  
  // Add thousand separators from right to left
  if (Len > 3) and (Value >= 0) then
  begin
    Temp := '';
    GroupCount := 0;
    
    for I := Len downto 1 do
    begin
      Temp := Result[I] + Temp;
      Inc(GroupCount);
      
      if (GroupCount = 3) and (I > 1) then
      begin
        Temp := ThousandSeparator + Temp;
        GroupCount := 0;
      end;
    end;
    
    Result := Temp;
  end;
end;

class function TStringKit.FormatFloat(const Value: Double; Decimals: Integer = 2; DecimalSeparator: Char = '.'; ThousandSeparator: Char = ','): string;
var
  I, IntegerPartLen: Integer;
  IntegerPart, DecimalPart, FormattedIntegerPart: string;
  IntValue: Int64;
  DecValue: Int64;
  Factor: Int64;
  IsNegative: Boolean;
begin
  IsNegative := Value < 0;
  
  // Split the number into integer and decimal parts
  IntValue := Trunc(Abs(Value));
  
  // Calculate decimal part
  Factor := 1;
  for I := 1 to Decimals do
    Factor := Factor * 10;
  
  DecValue := Round(Frac(Abs(Value)) * Factor);
  
  // Handle rounding issues
  if DecValue = Factor then
  begin
    DecValue := 0;
    IntValue := IntValue + 1;
  end;
  
  // Format integer part with thousand separators
  FormattedIntegerPart := FormatNumber(IntValue, ThousandSeparator);
  
  // Format decimal part
  if Decimals > 0 then
  begin
    DecimalPart := IntToStr(DecValue);
    while Length(DecimalPart) < Decimals do
      DecimalPart := '0' + DecimalPart;
    
    Result := FormattedIntegerPart + DecimalSeparator + DecimalPart;
  end
  else
    Result := FormattedIntegerPart;
  
  // Add negative sign if needed
  if IsNegative then
    Result := '-' + Result;
end;

class function TStringKit.Join(const Strings: TMatchStrings; const Delimiter: string): string;
var
  I: Integer;
begin
  Result := '';
  
  if Length(Strings) = 0 then
    Exit;
    
  Result := Strings[0];
  
  for I := 1 to High(Strings) do
    Result := Result + Delimiter + Strings[I];
end;

class function TStringKit.Split(const Text, Delimiter: string; MaxSplit: Integer = 0; RemoveEmptyEntries: Boolean = False): TMatchStrings;
var
  SplitList: TStringList;
  I, SplitCount: Integer;
  Remaining, Current: string;
  DelimPos: Integer;
begin
  SplitList := TStringList.Create;
  try
    // Special case for empty text - should return array with one empty element
    if Text = '' then
    begin
      if not RemoveEmptyEntries then
        SplitList.Add('');
    end
    else
    begin
      Remaining := Text;
      SplitCount := 0;
      
      while (Remaining <> '') and ((MaxSplit = 0) or (SplitCount < MaxSplit)) do
      begin
        DelimPos := Pos(Delimiter, Remaining);
        
        if DelimPos > 0 then
        begin
          Current := Copy(Remaining, 1, DelimPos - 1);
          Remaining := Copy(Remaining, DelimPos + Length(Delimiter), Length(Remaining));
          
          if (not RemoveEmptyEntries) or (Current <> '') then
          begin
            SplitList.Add(Current);
            Inc(SplitCount);
          end;
        end
        else
        begin
          // No more delimiters, add the remaining text
          if (not RemoveEmptyEntries) or (Remaining <> '') then
            SplitList.Add(Remaining);
          Break;
        end;
      end;
      
      // If we reached MaxSplit, add the remaining text as the last part
      if (MaxSplit > 0) and (SplitCount = MaxSplit) and (Remaining <> '') then
        SplitList.Add(Remaining);
    end;
    
    // Convert the TStringList to the result array
    SetLength(Result, SplitList.Count);
    for I := 0 to SplitList.Count - 1 do
      Result[I] := SplitList[I];
  finally
    SplitList.Free;
  end;
end;

class function TStringKit.Soundex(const Text: string): string;
var
  I, ResultLen: Integer;
  PrevCode, CurCode: Char;
  Ch: Char;
  UpperText: string;
begin
  // Initialize with empty result
  Result := '';
  if Text = '' then
    Exit;
    
  // Convert to uppercase for processing
  UpperText := UpperCase(Text);
  
  // First letter is preserved
  Result := UpperText[1];
  PrevCode := '0';  // Init previous code
  ResultLen := 1;   // Length of result so far (first letter already added)
  
  // Process remaining letters
  for I := 2 to Length(UpperText) do
  begin
    Ch := UpperText[I];
    
    // Skip non-alphabetic characters
    if not (Ch in ['A'..'Z']) then
      Continue;
      
    // Assign codes according to Soundex rules
    case Ch of
      'B', 'F', 'P', 'V': CurCode := '1';
      'C', 'G', 'J', 'K', 'Q', 'S', 'X', 'Z': CurCode := '2';
      'D', 'T': CurCode := '3';
      'L': CurCode := '4';
      'M', 'N': CurCode := '5';
      'R': CurCode := '6';
      else CurCode := '0';  // A, E, I, O, U, H, W, Y
    end;
    
    // Skip vowels and 'H', 'W', 'Y'
    if CurCode = '0' then
      Continue;
      
    // Skip repeated consonant codes
    if CurCode = PrevCode then
      Continue;
      
    // Add the code to the result
    Result := Result + CurCode;
    PrevCode := CurCode;
    Inc(ResultLen);
    
    // Stop after reaching length of 4
    if ResultLen = 4 then
      Break;
  end;
  
  // Pad with zeros if shorter than 4 characters
  while Length(Result) < 4 do
    Result := Result + '0';
end;

class function TStringKit.Metaphone(const Text: string): string;
var
  I, Len: Integer;
  UpperText, NormalizedText: string;
  Ch, NextCh, NextNextCh: Char;
  Skip: Boolean;
begin
  Result := '';
  if Text = '' then
    Exit;
    
  // Convert to uppercase and get length
  UpperText := UpperCase(Text);
  
  // Normalize ending 'S' for plurals - remove trailing S for consistency in matching
  NormalizedText := UpperText;
  if (Length(NormalizedText) > 2) and (NormalizedText[Length(NormalizedText)] = 'S') then
    NormalizedText := Copy(NormalizedText, 1, Length(NormalizedText) - 1);
  
  Len := Length(NormalizedText);
  
  // Process each character
  I := 1;
  while I <= Len do
  begin
    Ch := NormalizedText[I];
    Skip := False;
    
    // Get next characters if available
    if I < Len then
      NextCh := NormalizedText[I + 1]
    else
      NextCh := #0;
      
    if I < Len - 1 then
      NextNextCh := NormalizedText[I + 2]
    else
      NextNextCh := #0;
    
    // Apply Metaphone rules
    case Ch of
      // Vowels are only encoded at the beginning
      'A', 'E', 'I', 'O', 'U':
        begin
          if I = 1 then
            Result := Result + Ch;
        end;
      
      'B':
        begin
          Result := Result + 'B';
          // Skip duplicate B's
          if NextCh = 'B' then
            Inc(I);
        end;
      
      'C':
        begin
          if NextCh = 'H' then
          begin
            Result := Result + 'X'; // CH -> X
            Inc(I);  // Skip 'H'
          end
          else if (NextCh = 'I') or (NextCh = 'E') or (NextCh = 'Y') then
            Result := Result + 'S'  // Soft C
          else
            Result := Result + 'K'; // Hard C
        end;
      
      'D':
        begin
          if (NextCh = 'G') and (NextNextCh in ['E', 'I', 'Y']) then
          begin
            Result := Result + 'J'; // DGE, DGI, DGY -> J
            Inc(I, 2); // Skip 'G' and the vowel
          end
          else
            Result := Result + 'T'; // D -> T
        end;
      
      'F':
        Result := Result + 'F';
      
      'G':
        begin
          if NextCh = 'H' then
          begin
            if I = 1 then
              Result := Result + 'K'  // Initial GH -> K
            else
              Skip := True;  // Non-initial GH is silent
            Inc(I);  // Skip 'H'
          end
          else if (NextCh = 'N') and (I = 1) then
          begin
            Skip := True;  // Initial GN is silent
            Inc(I);  // Skip 'N'
          end
          else if (NextCh in ['E', 'I', 'Y']) then
            Result := Result + 'J'  // Soft G
          else
            Result := Result + 'K'; // Hard G
        end;
      
      'H':
        begin
          if I = 1 then
            Result := Result + 'H'  // Initial H is preserved
          else
            Skip := True;  // Non-initial H is often silent
        end;
      
      'J':
        Result := Result + 'J';
      
      'K':
        begin
          if NextCh <> 'N' then
            Result := Result + 'K';
        end;
      
      'L':
        Result := Result + 'L';
      
      'M':
        Result := Result + 'M';
      
      'N':
        Result := Result + 'N';
      
      'P':
        begin
          if NextCh = 'H' then
          begin
            Result := Result + 'F';
            Inc(I);  // Skip 'H'
          end
          else
            Result := Result + 'P';
        end;
      
      'Q':
        Result := Result + 'K';
      
      'R':
        Result := Result + 'R';
      
      'S':
        begin
          if NextCh = 'H' then
          begin
            Result := Result + 'X';
            Inc(I);  // Skip 'H'
          end
          else if (NextCh = 'I') and ((NextNextCh = 'O') or (NextNextCh = 'A')) then
          begin
            Result := Result + 'X';
            Inc(I, 2);  // Skip 'IO' or 'IA'
          end
          else
            Result := Result + 'S';
        end;
      
      'T':
        begin
          if (NextCh = 'I') and ((NextNextCh = 'O') or (NextNextCh = 'A')) then
          begin
            Result := Result + 'X';
            Inc(I, 2);  // Skip 'IO' or 'IA'
          end
          else if NextCh = 'H' then
          begin
            Result := Result + '0';  // TH -> 0 (theta)
            Inc(I);  // Skip 'H'
          end
          else
            Result := Result + 'T';
        end;
      
      'V':
        Result := Result + 'F';
      
      'W':
        begin
          if I = 1 then
            Result := Result + 'W'  // Initial W is preserved
          else
            Skip := True;  // Non-initial W is often part of a vowel sound
        end;
      
      'X':
        begin
          if I = 1 then
            Result := Result + 'S'  // Initial X -> S
          else
            Result := Result + 'KS'; // Non-initial X -> KS
        end;
      
      'Y':
        begin
          if I = 1 then
            Result := Result + 'Y'  // Initial Y is preserved
          else
            Skip := True;  // Non-initial Y is often part of a vowel sound
        end;
      
      'Z':
        Result := Result + 'S';
    end;
    
    if not Skip then
      Inc(I)
    else
      Inc(I);
  end;
end;

class function TStringKit.CountWords(const Text: string): Integer;
var
  Words: TMatchStrings;
begin
  Words := GetWords(Text);
  Result := Length(Words);
end;

class function TStringKit.FleschKincaidReadability(const Text: string): Double;
var
  Words: TMatchStrings;
  Sentences, Syllables, I, WordCount: Integer;
  Word: string;
begin
  // Count words
  Words := GetWords(Text);
  WordCount := Length(Words);
  
  if WordCount = 0 then
  begin
    Result := 0;
    Exit;
  end;
  
  // Count sentences (roughly by counting sentence-ending punctuation)
  Sentences := 0;
  for I := 1 to Length(Text) do
    if Text[I] in ['.', '!', '?'] then
      Inc(Sentences);
      
  // Ensure at least one sentence
  if Sentences = 0 then
    Sentences := 1;
  
  // Count syllables (very approximate method)
  Syllables := 0;
  for I := 0 to High(Words) do
  begin
    Word := LowerCase(Words[I]);
    
    // Count vowel groups as syllables
    // This is a very basic approximation
    Syllables := Syllables + CountVowelGroups(Word);
    
    // Words with no detected syllables get at least one
    if CountVowelGroups(Word) = 0 then
      Inc(Syllables);
  end;
  
  // Calculate Flesch-Kincaid Reading Ease score
  // Formula: 206.835 - 1.015 * (words/sentences) - 84.6 * (syllables/words)
  Result := 206.835 - 1.015 * (WordCount / Sentences) - 84.6 * (Syllables / WordCount);
  
  // Constrain the result between 0 and 100
  if Result < 0 then
    Result := 0
  else if Result > 100 then
    Result := 100;
end;

class function TStringKit.GenerateNGrams(const Text: string; N: Integer): TMatchStrings;
var
  Words: TMatchStrings;
  NGramList: TStringList;
  I, J, NGramCount: Integer;
  NGram: string;
begin
  Result := nil;
  SetLength(Result, 0);
  
  if (N <= 0) or (Text = '') then
    Exit;
  
  // Get words from text
  Words := GetWords(Text);
  
  // If not enough words for n-grams, return empty array
  if Length(Words) < N then
    Exit;
  
  // Create a list to hold n-grams
  NGramList := TStringList.Create;
  try
    // Generate all possible n-grams
    for I := 0 to Length(Words) - N do
    begin
      NGram := Words[I];
      
      // Combine N consecutive words
      for J := 1 to N - 1 do
        NGram := NGram + ' ' + Words[I + J];
        
      NGramList.Add(NGram);
    end;
    
    // Convert to result array
    SetLength(Result, NGramList.Count);
    for I := 0 to NGramList.Count - 1 do
      Result[I] := NGramList[I];
  finally
    NGramList.Free;
  end;
end;

// Helper function to count vowel groups for syllable estimation
class function TStringKit.CountVowelGroups(const Word: string): Integer;
var
  I: Integer;
  InVowelGroup: Boolean;
begin
  Result := 0;
  InVowelGroup := False;
  
  for I := 1 to Length(Word) do
  begin
    if Word[I] in ['a', 'e', 'i', 'o', 'u', 'y'] then
    begin
      if not InVowelGroup then
      begin
        InVowelGroup := True;
        Inc(Result);
      end;
    end
    else
      InVowelGroup := False;
  end;
  
  // Special case for English: silent 'e' at end of word usually doesn't count
  if (Length(Word) > 2) and (Word[Length(Word)] = 'e') and 
     not (Word[Length(Word) - 1] in ['a', 'e', 'i', 'o', 'u', 'y']) then
    Dec(Result);
    
  // If negative due to rules (e.g., single "me"), ensure at least 1
  if Result < 1 then
    Result := 1;
end;

class function TStringKit.HTMLEncode(const Text: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(Text) do
  begin
    case Text[I] of
      '<': Result := Result + '&lt;';
      '>': Result := Result + '&gt;';
      '&': Result := Result + '&amp;';
      '"': Result := Result + '&quot;';
      '''': Result := Result + '&#39;';  // Single quote
      else
        Result := Result + Text[I];
    end;
  end;
end;

class function TStringKit.HTMLDecode(const Text: string): string;
begin
  Result := Text;
  
  // Replace HTML entities with their corresponding characters
  Result := ReplaceText(Result, '&lt;', '<');
  Result := ReplaceText(Result, '&gt;', '>');
  Result := ReplaceText(Result, '&amp;', '&');
  Result := ReplaceText(Result, '&quot;', '"');
  Result := ReplaceText(Result, '&#39;', '''');  // Single quote
  Result := ReplaceText(Result, '&nbsp;', ' ');
end;

class function TStringKit.URLEncode(const Text: string): string;
const
  // Characters that don't need encoding
  SAFE_CHARS = ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.', '~'];
var
  I: Integer;
  HexStr: string;
begin
  Result := '';
  for I := 1 to Length(Text) do
  begin
    if Text[I] in SAFE_CHARS then
      Result := Result + Text[I]
    else if Text[I] = ' ' then
      Result := Result + '+'
    else
    begin
      // Convert character to hexadecimal representation
      HexStr := IntToHex(Ord(Text[I]), 2);
      Result := Result + '%' + HexStr;
    end;
  end;
end;

class function TStringKit.URLDecode(const Text: string): string;
var
  I: Integer;
  HexCode: string;
  CharCode: Integer;
begin
  Result := '';
  I := 1;
  
  while I <= Length(Text) do
  begin
    if Text[I] = '%' then
    begin
      // Check if there are at least 2 more characters for a hex code
      if I + 2 <= Length(Text) then
      begin
        HexCode := Copy(Text, I + 1, 2);
        try
          // Convert hex to integer and then to character
          CharCode := StrToInt('$' + HexCode);
          Result := Result + Chr(CharCode);
        except
          // If conversion fails, include the % character as-is
          Result := Result + '%';
          Dec(I, 2); // Adjust to process the next two characters normally
        end;
        Inc(I, 2); // Skip the two hex characters
      end
      else
        Result := Result + Text[I]; // Incomplete % sequence
    end
    else if Text[I] = '+' then
      Result := Result + ' '
    else
      Result := Result + Text[I];
      
    Inc(I);
  end;
end;

class function TStringKit.ToRoman(Value: Integer): string;
const
  RomanDigits: array[1..13] of string = (
    'M', 'CM', 'D', 'CD', 'C', 'XC', 'L', 'XL', 'X', 'IX', 'V', 'IV', 'I');
  RomanValues: array[1..13] of Integer = (
    1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1);
var
  I: Integer;
  Num: Integer;
begin
  Result := '';
  
  // Roman numerals can only represent numbers from 1 to 3999
  if (Value < 1) or (Value > 3999) then
    Exit;
  
  Num := Value;
  I := 1;
  
  while (Num > 0) and (I <= 13) do
  begin
    while Num >= RomanValues[I] do
    begin
      Result := Result + RomanDigits[I];
      Num := Num - RomanValues[I];
    end;
    Inc(I);
  end;
end;

class function TStringKit.FromRoman(const RomanNumeral: string): Integer;
const
  RomanDigits: array[1..7] of Char = ('I', 'V', 'X', 'L', 'C', 'D', 'M');
  RomanValues: array[1..7] of Integer = (1, 5, 10, 50, 100, 500, 1000);
var
  I, J, Value: Integer;
  PrevValue, CurrValue: Integer;
  UpperRoman: string;
begin
  Result := 0;
  
  // Empty string has no value
  if RomanNumeral = '' then
    Exit;
  
  // Convert to uppercase for processing
  UpperRoman := UpperCase(RomanNumeral);
  
  // Validate that all characters are valid Roman numerals
  for I := 1 to Length(UpperRoman) do
  begin
    Value := 0;
    for J := 1 to 7 do
      if UpperRoman[I] = RomanDigits[J] then
      begin
        Value := RomanValues[J];
        Break;
      end;
      
    if Value = 0 then
      Exit(0); // Invalid character found
  end;
  
  // Process the Roman numeral from right to left
  PrevValue := 0;
  for I := Length(UpperRoman) downto 1 do
  begin
    CurrValue := 0;
    for J := 1 to 7 do
      if UpperRoman[I] = RomanDigits[J] then
      begin
        CurrValue := RomanValues[J];
        Break;
      end;
      
    if CurrValue >= PrevValue then
      Result := Result + CurrValue
    else
      Result := Result - CurrValue;
      
    PrevValue := CurrValue;
  end;
end;

class function TStringKit.ToOrdinal(Value: Integer): string;
var
  LastDigit, LastTwoDigits: Integer;
begin
  // Convert the number to string first
  Result := IntToStr(Value);
  
  // Get the last digit and last two digits
  LastDigit := Abs(Value) mod 10;
  LastTwoDigits := Abs(Value) mod 100;
  
  // Apply the appropriate suffix based on rules
  if (LastTwoDigits >= 11) and (LastTwoDigits <= 13) then
    Result := Result + 'th'
  else
    case LastDigit of
      1: Result := Result + 'st';
      2: Result := Result + 'nd';
      3: Result := Result + 'rd';
      else Result := Result + 'th';
    end;
end;

class function TStringKit.NumberToWords(Value: Int64): string;

  // Helper function to convert numbers less than 1000 to words
  function NumberToWordsLessThan1000(Num: Integer): string;
  const
    Units: array[0..19] of string = (
      'zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine',
      'ten', 'eleven', 'twelve', 'thirteen', 'fourteen', 'fifteen', 'sixteen',
      'seventeen', 'eighteen', 'nineteen');
    Tens: array[2..9] of string = (
      'twenty', 'thirty', 'forty', 'fifty', 'sixty', 'seventy', 'eighty', 'ninety');
  var
    Hundreds, TensUnits: Integer;
  begin
    Result := '';
    
    if Num = 0 then
      Exit('zero');
      
    // Handle hundreds
    Hundreds := Num div 100;
    if Hundreds > 0 then
    begin
      Result := Units[Hundreds] + ' hundred';
      Num := Num mod 100;
      if Num > 0 then
        Result := Result + ' and ';
    end;
    
    // Handle tens and units
    if Num > 0 then
    begin
      if Num < 20 then
        Result := Result + Units[Num]
      else
      begin
        TensUnits := Num mod 10;
        Result := Result + Tens[Num div 10];
        if TensUnits > 0 then
          Result := Result + '-' + Units[TensUnits];
      end;
    end;
  end;

var
  Num: Int64;
  IsNegative: Boolean;
  Billions, Millions, Thousands, Remainder: Int64;
begin
  Result := '';
  
  // Handle zero separately
  if Value = 0 then
    Exit('zero');
    
  // Handle negative numbers
  IsNegative := Value < 0;
  Num := Abs(Value);
  
  // Handle billions (for values that go into billions)
  Billions := Num div 1000000000;
  if Billions > 0 then
  begin
    Result := NumberToWordsLessThan1000(Billions) + ' billion';
    Num := Num mod 1000000000;
    if Num > 0 then
      Result := Result + ' ';
  end;
  
  // Handle millions
  Millions := Num div 1000000;
  if Millions > 0 then
  begin
    Result := Result + NumberToWordsLessThan1000(Millions) + ' million';
    Num := Num mod 1000000;
    if Num > 0 then
      Result := Result + ' ';
  end;
  
  // Handle thousands
  Thousands := Num div 1000;
  if Thousands > 0 then
  begin
    Result := Result + NumberToWordsLessThan1000(Thousands) + ' thousand';
    Num := Num mod 1000;
    if Num > 0 then
      Result := Result + ' ';
  end;
  
  // Handle the remainder (less than 1000)
  Remainder := Num;
  if Remainder > 0 then
  begin
    // Add 'and' for British-style if there's already something in the result
    if (Result <> '') and ((Remainder < 100) or ((Value > 1000) and (Value < 2000))) then
      Result := Result + 'and ';
    Result := Result + NumberToWordsLessThan1000(Remainder);
  end;
  
  // Add 'negative' for negative numbers
  if IsNegative then
    Result := 'negative ' + Result;
end;

class function TStringKit.Base64Encode(const Text: string): string;
const
  Base64Chars: string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var
  I, J: Integer;
  Bytes: array of Byte;
  B1, B2, B3: Byte;
  EncodedLen: Integer;
begin
  Result := '';
  if Text = '' then
    Exit;
    
  // Convert string to byte array
  SetLength(Bytes, Length(Text));
  for I := 1 to Length(Text) do
    Bytes[I-1] := Ord(Text[I]);
    
  // Calculate encoded length (must be multiple of 4)
  EncodedLen := ((Length(Bytes) + 2) div 3) * 4;
  SetLength(Result, EncodedLen);
  
  // Process input in 3-byte chunks
  I := 0;
  J := 1;
  while I < Length(Bytes) do
  begin
    // Get next 3 bytes (or fewer at the end)
    B1 := Bytes[I];
    Inc(I);
    
    if I < Length(Bytes) then
      B2 := Bytes[I]
    else
      B2 := 0;
    Inc(I);
    
    if I < Length(Bytes) then
      B3 := Bytes[I]
    else
      B3 := 0;
    Inc(I);
    
    // Encode to 4 characters
    Result[J] := Base64Chars[(B1 shr 2) + 1];
    Result[J+1] := Base64Chars[(((B1 and $03) shl 4) or (B2 shr 4)) + 1];
    
    if I - 2 > Length(Bytes) then
      Result[J+2] := '='
    else
      Result[J+2] := Base64Chars[(((B2 and $0F) shl 2) or (B3 shr 6)) + 1];
      
    if I - 1 > Length(Bytes) then
      Result[J+3] := '='
    else
      Result[J+3] := Base64Chars[(B3 and $3F) + 1];
      
    Inc(J, 4);
  end;
end;

class function TStringKit.Base64Decode(const Base64Text: string): string;
const
  Base64DecodeTable: array[#0..#127] of SmallInt = (
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  // 0-15
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  // 16-31
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 62, -1, -1, -1, 63,  // 32-47
    52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -1, -1, -1, -1, -1, -1,  // 48-63
    -1,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14,  // 64-79
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, -1, -1, -1, -1, -1,  // 80-95
    -1, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,  // 96-111
    41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, -1, -1, -1, -1, -1   // 112-127
  );
var
  I, J, BytesLen: Integer;
  Bytes: array of Byte;
  C1, C2, C3, C4: SmallInt;
  InputLen: Integer;
  CleanInput: string;
begin
  Result := '';
  
  // Remove any non-base64 characters (including whitespace)
  CleanInput := '';
  for I := 1 to Length(Base64Text) do
    if (Base64Text[I] in ['A'..'Z', 'a'..'z', '0'..'9', '+', '/', '=']) then
      CleanInput := CleanInput + Base64Text[I];
  
  InputLen := Length(CleanInput);
  if InputLen = 0 then
    Exit;
  
  // Base64 string must be multiple of 4 in length
  if InputLen mod 4 <> 0 then
    Exit;
  
  // Calculate decoded length (3 bytes for every 4 chars, adjusted for padding)
  BytesLen := (InputLen div 4) * 3;
  if CleanInput[InputLen] = '=' then Dec(BytesLen);
  if CleanInput[InputLen-1] = '=' then Dec(BytesLen);
  
  SetLength(Bytes, BytesLen);
  
  // Process input in 4-char chunks
  I := 1;
  J := 0;
  while I <= InputLen do
  begin
    // Get 4 characters and convert to values
    C1 := Base64DecodeTable[CleanInput[I]];
    C2 := Base64DecodeTable[CleanInput[I+1]];
    
    if (CleanInput[I+2] = '=') then
      C3 := -1
    else
      C3 := Base64DecodeTable[CleanInput[I+2]];
      
    if (CleanInput[I+3] = '=') then
      C4 := -1
    else
      C4 := Base64DecodeTable[CleanInput[I+3]];
    
    // Invalid character found
    if (C1 < 0) or (C2 < 0) or ((C3 < 0) and (CleanInput[I+2] <> '=')) or
       ((C4 < 0) and (CleanInput[I+3] <> '=')) then
      Exit;
    
    // Decode the 4 characters into 1-3 bytes
    if J < BytesLen then
      Bytes[J] := ((C1 shl 2) or (C2 shr 4)) and $FF;
    Inc(J);
    
    if (C3 >= 0) and (J < BytesLen) then
      Bytes[J] := ((C2 shl 4) or (C3 shr 2)) and $FF;
    Inc(J);
    
    if (C4 >= 0) and (J < BytesLen) then
      Bytes[J] := ((C3 shl 6) or C4) and $FF;
    Inc(J);
    
    Inc(I, 4);
  end;
  
  // Convert byte array back to string
  SetLength(Result, Length(Bytes));
  for I := 0 to Length(Bytes) - 1 do
    Result[I+1] := Chr(Bytes[I]);
end;

class function TStringKit.HexEncode(const Text: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(Text) do
    Result := Result + IntToHex(Ord(Text[I]), 2);
end;

class function TStringKit.HexDecode(const HexText: string): string;
var
  I, CharCode: Integer;
  HexPair: string;
begin
  Result := '';
  
  // Validate that we have an even number of hex characters
  if (Length(HexText) mod 2 <> 0) then
    Exit;
  
  I := 1;
  while I < Length(HexText) do
  begin
    HexPair := Copy(HexText, I, 2);
    
    // Skip non-hex characters
    if not MatchesPattern(HexPair, '^[0-9A-Fa-f]{2}$') then
    begin
      Inc(I);
      Continue;
    end;
    
    try
      CharCode := StrToInt('$' + HexPair);
      Result := Result + Chr(CharCode);
    except
      // Ignore invalid hex pairs
    end;
    
    Inc(I, 2);
  end;
end;

end. 
