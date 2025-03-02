unit TidyKit.JSON.Scanner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TidyKit.JSON;
  
type
  { JSON scanner class }
  TJSONScanner = class
  private
    FSource: string;
    FPosition: Integer;
    
    function IsWhitespace(C: Char): Boolean;
    function IsDigit(C: Char): Boolean;
    function IsNumberStart(C: Char): Boolean;
    function PeekChar: Char;
    function ReadChar: Char;
    function ReadString: string;
    function ReadNumber: string;
    procedure SkipWhitespace;
  public
    constructor Create(const ASource: string);
    function GetNextToken: TJSONToken;
  end;

implementation

constructor TJSONScanner.Create(const ASource: string);
begin
  inherited Create;
  FSource := ASource;
  FPosition := 1;
end;

function TJSONScanner.IsWhitespace(C: Char): Boolean;
begin
  Result := (C = ' ') or (C = #9) or (C = #10) or (C = #13);
end;

function TJSONScanner.IsDigit(C: Char): Boolean;
begin
  Result := (C >= '0') and (C <= '9');
end;

function TJSONScanner.IsNumberStart(C: Char): Boolean;
begin
  Result := IsDigit(C) or (C = '-');
end;

function TJSONScanner.PeekChar: Char;
begin
  if FPosition <= Length(FSource) then
    Result := FSource[FPosition]
  else
    Result := #0;
end;

function TJSONScanner.ReadChar: Char;
begin
  if FPosition <= Length(FSource) then
  begin
    Result := FSource[FPosition];
    Inc(FPosition);
  end
  else
    Result := #0;
end;

function TJSONScanner.ReadString: string;
var
  C: Char;
  Escaped: Boolean;
begin
  Result := '';
  Escaped := False;
  
  // Skip opening quote
  ReadChar;
  
  while FPosition <= Length(FSource) do
  begin
    C := ReadChar;
    
    if Escaped then
    begin
      case C of
        '"', '\', '/': Result := Result + C;
        'b': Result := Result + #8;
        't': Result := Result + #9;
        'n': Result := Result + #10;
        'f': Result := Result + #12;
        'r': Result := Result + #13;
        'u':
        begin
          // TODO: Handle Unicode escape sequences
          raise EJSONException.Create('Unicode escape sequences not yet supported');
        end;
        else
          raise EJSONException.Create('Invalid escape sequence: \' + C);
      end;
      Escaped := False;
    end
    else if C = '\' then
      Escaped := True
    else if C = '"' then
      Break
    else if Ord(C) < 32 then
      raise EJSONException.Create('Invalid control character in string')
    else
      Result := Result + C;
  end;
end;

function TJSONScanner.ReadNumber: string;
var
  C: Char;
  HasDecimal, HasExponent: Boolean;
begin
  Result := '';
  HasDecimal := False;
  HasExponent := False;
  
  // Read minus sign if present
  C := PeekChar;
  if C = '-' then
  begin
    Result := Result + ReadChar;
    C := PeekChar;
  end;
  
  // Read integer part
  if not IsDigit(C) then
    raise EJSONException.Create('Invalid number format');
    
  if C = '0' then
  begin
    Result := Result + ReadChar;
    C := PeekChar;
    if IsDigit(C) then
      raise EJSONException.Create('Leading zeros not allowed');
  end
  else
    while IsDigit(C) do
    begin
      Result := Result + ReadChar;
      C := PeekChar;
    end;
    
  // Read decimal part
  if C = '.' then
  begin
    HasDecimal := True;
    Result := Result + ReadChar;
    C := PeekChar;
    
    if not IsDigit(C) then
      raise EJSONException.Create('Expected digit after decimal point');
      
    while IsDigit(C) do
    begin
      Result := Result + ReadChar;
      C := PeekChar;
    end;
  end;
  
  // Read exponent
  if (C = 'e') or (C = 'E') then
  begin
    HasExponent := True;
    Result := Result + ReadChar;
    C := PeekChar;
    
    if (C = '+') or (C = '-') then
    begin
      Result := Result + ReadChar;
      C := PeekChar;
    end;
    
    if not IsDigit(C) then
      raise EJSONException.Create('Expected digit in exponent');
      
    while IsDigit(C) do
    begin
      Result := Result + ReadChar;
      C := PeekChar;
    end;
  end;
end;

procedure TJSONScanner.SkipWhitespace;
begin
  while (FPosition <= Length(FSource)) and IsWhitespace(FSource[FPosition]) do
    Inc(FPosition);
end;

function TJSONScanner.GetNextToken: TJSONToken;
var
  C: Char;
begin
  SkipWhitespace;
  
  if FPosition > Length(FSource) then
  begin
    Result.TokenType := ttNone;
    Result.Value := '';
    Exit;
  end;
  
  C := PeekChar;
  case C of
    '{':
    begin
      ReadChar;
      Result.TokenType := ttCurlyOpen;
      Result.Value := '{';
    end;
    '}':
    begin
      ReadChar;
      Result.TokenType := ttCurlyClose;
      Result.Value := '}';
    end;
    '[':
    begin
      ReadChar;
      Result.TokenType := ttSquareOpen;
      Result.Value := '[';
    end;
    ']':
    begin
      ReadChar;
      Result.TokenType := ttSquareClose;
      Result.Value := ']';
    end;
    ':':
    begin
      ReadChar;
      Result.TokenType := ttColon;
      Result.Value := ':';
    end;
    ',':
    begin
      ReadChar;
      Result.TokenType := ttComma;
      Result.Value := ',';
    end;
    '"':
    begin
      Result.TokenType := ttString;
      Result.Value := ReadString;
    end;
    't':
    begin
      if Copy(FSource, FPosition, 4) = 'true' then
      begin
        Inc(FPosition, 4);
        Result.TokenType := ttTrue;
        Result.Value := 'true';
      end
      else
        raise EJSONException.Create('Invalid token');
    end;
    'f':
    begin
      if Copy(FSource, FPosition, 5) = 'false' then
      begin
        Inc(FPosition, 5);
        Result.TokenType := ttFalse;
        Result.Value := 'false';
      end
      else
        raise EJSONException.Create('Invalid token');
    end;
    'n':
    begin
      if Copy(FSource, FPosition, 4) = 'null' then
      begin
        Inc(FPosition, 4);
        Result.TokenType := ttNull;
        Result.Value := 'null';
      end
      else
        raise EJSONException.Create('Invalid token');
    end;
    else
    begin
      if IsNumberStart(C) then
      begin
        Result.TokenType := ttNumber;
        Result.Value := ReadNumber;
      end
      else
        raise EJSONException.Create('Invalid token');
    end;
  end;
end;

end. 