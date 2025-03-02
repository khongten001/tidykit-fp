unit TidyKit.JSON.Writer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TidyKit.JSON;

type
  { JSON writer class }
  TJSONWriter = class
  private
    FStringBuilder: TStringBuilder;
    FIndentLevel: Integer;
    FIndentChars: string;
    FCompact: Boolean;
    
    procedure WriteIndent;
    procedure WriteNewLine;
    procedure WriteString(const S: string);
    procedure WriteValue(Value: IJSONValue);
    procedure WriteObject(Obj: IJSONObject);
    procedure WriteArray(Arr: IJSONArray);
  public
    constructor Create(ACompact: Boolean = False; const AIndentChars: string = '  ');
    destructor Destroy; override;
    function Write(Value: IJSONValue): string;
  end;

implementation

constructor TJSONWriter.Create(ACompact: Boolean; const AIndentChars: string);
begin
  inherited Create;
  FStringBuilder := TStringBuilder.Create;
  FIndentLevel := 0;
  FIndentChars := AIndentChars;
  FCompact := ACompact;
end;

destructor TJSONWriter.Destroy;
begin
  FStringBuilder.Free;
  inherited Destroy;
end;

procedure TJSONWriter.WriteIndent;
var
  I: Integer;
begin
  if not FCompact then
    for I := 1 to FIndentLevel do
      FStringBuilder.Append(FIndentChars);
end;

procedure TJSONWriter.WriteNewLine;
begin
  if not FCompact then
    FStringBuilder.Append(LineEnding);
end;

procedure TJSONWriter.WriteString(const S: string);
begin
  FStringBuilder.Append(S);
end;

procedure TJSONWriter.WriteValue(Value: IJSONValue);
begin
  if Value = nil then
    WriteString('null')
  else if Value.IsObject then
    WriteObject(Value.AsObject)
  else if Value.IsArray then
    WriteArray(Value.AsArray)
  else
    WriteString(Value.ToString(False));
end;

procedure TJSONWriter.WriteObject(Obj: IJSONObject);
var
  Names: TStringArray;
  I: Integer;
  First: Boolean;
begin
  WriteString('{');
  Inc(FIndentLevel);
  
  Names := Obj.Names;
  First := True;
  
  for I := 0 to High(Names) do
  begin
    if not First then
      WriteString(',');
    WriteNewLine;
    WriteIndent;
    
    // Write property name
    WriteString('"' + Names[I] + '":');
    if not FCompact then
      WriteString(' ');
      
    // Write property value
    WriteValue(Obj.Values[Names[I]]);
    First := False;
  end;
  
  Dec(FIndentLevel);
  if not First then
  begin
    WriteNewLine;
    WriteIndent;
  end;
  WriteString('}');
end;

procedure TJSONWriter.WriteArray(Arr: IJSONArray);
var
  I: Integer;
  First: Boolean;
begin
  WriteString('[');
  Inc(FIndentLevel);
  
  First := True;
  for I := 0 to Arr.Count - 1 do
  begin
    if not First then
      WriteString(',');
    WriteNewLine;
    WriteIndent;
    
    WriteValue(Arr.Items[I]);
    First := False;
  end;
  
  Dec(FIndentLevel);
  if not First then
  begin
    WriteNewLine;
    WriteIndent;
  end;
  WriteString(']');
end;

function TJSONWriter.Write(Value: IJSONValue): string;
begin
  FStringBuilder.Clear;
  WriteValue(Value);
  Result := FStringBuilder.ToString;
end;

end. 