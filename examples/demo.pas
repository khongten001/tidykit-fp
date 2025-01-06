program demo;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, TidyKit;

procedure DemoStrings;
var
  Result: string;
begin
  WriteLn('String manipulation demo:');
  WriteLn('------------------------');
  
  Result := Strings
    .From('  Hello, World!  ')
    .Trim
    .Replace('Hello', 'Hi')
    .ToLower
    .ToString;
    
  WriteLn('Original: "  Hello, World!  "');
  WriteLn('Modified: "', Result, '"');
  WriteLn;
end;

procedure DemoDateTime;
var
  DateKit: IDateTimeKit;
  Today, Tomorrow: TDateTime;
begin
  WriteLn('DateTime manipulation demo:');
  WriteLn('-------------------------');
  
  DateKit := DateTime;
  
  Today := DateKit
    .From(Now)
    .StartOfDay
    .ToDateTime;
    
  Tomorrow := DateKit
    .From(Today)
    .AddDays(1)
    .ToDateTime;
    
  WriteLn('Today: ', DateToStr(Today));
  WriteLn('Tomorrow: ', DateToStr(Tomorrow));
  WriteLn;
end;

procedure DemoFiles;
var
  FileKit: IFileKit;
begin
  WriteLn('File manipulation demo:');
  WriteLn('----------------------');
  
  // Create a test file
  FileKit := Files;
  FileKit
    .SetContent('Hello from TidyKit!')
    .WriteFile('test.txt');
    
  WriteLn('Created test.txt');
  
  // Read and modify the file
  FileKit
    .ReadFile('test.txt')
    .AppendText(LineEnding + 'This is a new line.')
    .WriteFile('test_modified.txt');
    
  WriteLn('Created test_modified.txt with additional content');
  
  // Display file information
  WriteLn('File size: ', FileKit.Size, ' bytes');
  WriteLn('Last modified: ', DateTimeToStr(FileKit.LastWriteTime));
  WriteLn;
  
  // Clean up
  FileKit
    .ReadFile('test.txt')
    .DeleteFile;
    
  FileKit
    .ReadFile('test_modified.txt')
    .DeleteFile;
    
  WriteLn('Cleaned up test files');
  WriteLn;
end;

begin
  try
    WriteLn('TidyKit Demo');
    WriteLn('===========');
    WriteLn;
    
    DemoStrings;
    DemoDateTime;
    DemoFiles;
    
    WriteLn('Demo completed successfully!');
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end. 