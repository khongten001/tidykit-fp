program StringKitExample;

{$mode objfpc}{$H+}{$J-}

uses
  Classes, SysUtils, TidyKit;

type
  TTextProcessor = class
  private
    FText: string;
  public
    constructor Create(const AText: string);
    procedure ProcessText;
    procedure ShowBasicOperations;
    procedure ShowAdvancedOperations;
    procedure ShowPatternMatching;
    procedure ShowStringAnalysis;
    procedure ShowStringExtraction;
  end;

{ TTextProcessor }

constructor TTextProcessor.Create(const AText: string);
begin
  FText := AText;
end;

procedure TTextProcessor.ShowBasicOperations;
begin
  WriteLn('Basic String Operations:');
  WriteLn('----------------------');
  WriteLn('Original Text: "', FText, '"');
  WriteLn('Trimmed: "', TStringKit.Trim(FText), '"');
  WriteLn('TrimLeft: "', TStringKit.TrimLeft(FText), '"');
  WriteLn('TrimRight: "', TStringKit.TrimRight(FText), '"');
  WriteLn('Uppercase: ', TStringKit.ToUpper(FText));
  WriteLn('Lowercase: ', TStringKit.ToLower(FText));
  WriteLn('Capitalized: ', TStringKit.CapitalizeText(FText));
  WriteLn;
end;

procedure TTextProcessor.ShowAdvancedOperations;
begin
  WriteLn('Advanced String Operations:');
  WriteLn('-------------------------');
  WriteLn('Reversed: ', TStringKit.ReverseText(FText));
  WriteLn('Duplicated (2x): ', TStringKit.DuplicateText(FText, 2));
  WriteLn('Left Padded (30): ', TStringKit.PadLeft(FText, 30, '*'));
  WriteLn('Right Padded (30): ', TStringKit.PadRight(FText, 30, '*'));
  WriteLn('Center Padded (30): ', TStringKit.PadCenter(FText, 30, '*'));
  WriteLn('Collapsed Whitespace: ', TStringKit.CollapseWhitespace(FText));
  WriteLn('Removed Whitespace: ', TStringKit.RemoveWhitespace(FText));
  WriteLn('Replaced Text (Hello->Hi): ', TStringKit.ReplaceText(FText, 'Hello', 'Hi'));
  WriteLn;
end;

procedure TTextProcessor.ShowPatternMatching;
var
  EmailMatches: TStringMatches;
  AllMatches: TStringArray;
  I: Integer;
begin
  WriteLn('Pattern Matching:');
  WriteLn('-----------------');
  
  // Regular expression pattern for email addresses
  EmailMatches := TStringKit.ExtractMatches(FText, '\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}\b');
  WriteLn('Email Addresses Found:');
  if Length(EmailMatches) > 0 then
  begin
    for I := 0 to High(EmailMatches) do
      WriteLn('- ', EmailMatches[I].Text, ' (at position ', EmailMatches[I].Position, ')');
  end
  else
    WriteLn('No email addresses found.');
    
  // Extract all word matches
  AllMatches := TStringKit.ExtractAllMatches(FText, '\b\w+\b');
  WriteLn('All Words Found:');
  for I := 0 to High(AllMatches) do
    Write(AllMatches[I], ', ');
  WriteLn;
  
  // Pattern matching test
  WriteLn('Contains email? ', TStringKit.MatchesPattern(FText, '@'));
  WriteLn('Contains number? ', TStringKit.MatchesPattern(FText, '\d+'));
  WriteLn;
end;

procedure TTextProcessor.ShowStringAnalysis;
begin
  WriteLn('String Analysis:');
  WriteLn('----------------');
  WriteLn('Length: ', TStringKit.GetLength(FText));
  WriteLn('Is Empty? ', TStringKit.IsEmpty(FText));
  WriteLn('Contains "Hello"? ', TStringKit.Contains(FText, 'Hello'));
  WriteLn('Starts with "The"? ', TStringKit.StartsWith(FText, 'The'));
  WriteLn('Ends with "!"? ', TStringKit.EndsWith(FText, '!'));
  WriteLn('Count of "l": ', TStringKit.CountSubString(FText, 'l'));
  WriteLn;
end;

procedure TTextProcessor.ShowStringExtraction;
var
  Words:TStringArray;
  I:Integer;
begin
  WriteLn('String Extraction:');
  WriteLn('-----------------');
  WriteLn('First 5 chars: ', TStringKit.LeftStr(FText, 5));
  WriteLn('Last 5 chars: ', TStringKit.RightStr(FText, 5));
  WriteLn('Substring (6,5): ', TStringKit.SubString(FText, 6, 5));
  
  WriteLn('Words:');
  Words := TStringKit.GetWords(FText);
  for I := 0 to High(Words) do
    WriteLn('- ', Words[I]);
  WriteLn;
end;

procedure TTextProcessor.ProcessText;
begin
  WriteLn('Processing Text Sample');
  WriteLn('====================');
  WriteLn;
  
  ShowBasicOperations;
  ShowAdvancedOperations;
  ShowPatternMatching;
  ShowStringAnalysis;
  ShowStringExtraction;
end;

var
  Processor: TTextProcessor;
  SampleText: string;

begin
  try
    SampleText := 
      '  The quick brown fox jumps over the lazy dog!  ' + LineEnding +
      'Contact us at support@example.com or info@tidykit.com for more information.' + LineEnding +
      'This is an    example   with    multiple    spaces and numbers 12345.';

    Processor := TTextProcessor.Create(SampleText);
    try
      Processor.ProcessText;
    finally
      Processor.Free;
    end;

    WriteLn('Press Enter to exit...');
    ReadLn;
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end. 
