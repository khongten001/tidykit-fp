unit testcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  TidyKit;

type
  { TDateTimeTests }
  TDateTimeTests = class(TTestCase)
  private
    FDateTime: IDateTimeKit;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Basic operations
    procedure Test01_Now;
    procedure Test02_Today;
    procedure Test03_From;
    // Date parts - getters and setters
    procedure Test04_Year;
    procedure Test05_Month;
    procedure Test06_Day;
    procedure Test07_Hour;
    procedure Test08_Minute;
    procedure Test09_Second;
    procedure Test10_Millisecond;
    // Date manipulations
    procedure Test11_AddYears;
    procedure Test12_AddMonths;
    procedure Test13_AddDays;
    procedure Test14_AddHours;
    procedure Test15_AddMinutes;
    procedure Test16_AddSeconds;
    // Date truncations
    procedure Test17_StartOfYear;
    procedure Test18_StartOfMonth;
    procedure Test19_StartOfDay;
    procedure Test20_EndOfYear;
    procedure Test21_EndOfMonth;
    procedure Test22_EndOfDay;
    // Date comparisons
    procedure Test23_IsBefore;
    procedure Test24_IsAfter;
    procedure Test25_IsSameDay;
    procedure Test26_IsSameMonth;
    procedure Test27_IsSameYear;
    // Conversions
    procedure Test28_ToDateTime;
    procedure Test29_ToString;
  end;

  { TFSTests }
  TFSTests = class(TTestCase)
  private
    FFS: IFileKit;
    FTestDir: string;
    FTestFile: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Basic file operations
    procedure Test01_ReadFile;
    procedure Test02_WriteFile;
    procedure Test03_AppendFile;
    procedure Test04_DeleteFile;
    procedure Test05_CopyTo;
    procedure Test06_MoveTo;
    // Content operations
    procedure Test07_SetContent;
    procedure Test08_AppendText;
    procedure Test09_PrependText;
    procedure Test10_ReplaceText;
    // Directory operations
    procedure Test11_CreateDirectory;
    procedure Test12_DeleteDirectory;
    procedure Test13_EnsureDirectory;
    procedure Test14_DirectoryExists;
    // Path operations
    procedure Test15_ChangeExtension;
    procedure Test16_GetFileName;
    procedure Test17_GetDirectory;
    procedure Test18_GetExtension;
    procedure Test19_GetFullPath;
    // Search operations
    procedure Test20_SearchFiles;
    procedure Test21_SearchFilesIn;
    procedure Test22_FindNewestFile;
    procedure Test23_FindOldestFile;
    procedure Test24_FindLargestFile;
    procedure Test25_FindSmallestFile;
    // File information
    procedure Test26_Exists;
    procedure Test27_Size;
    procedure Test28_CreationTime;
    procedure Test29_LastWriteTime;
    procedure Test30_LastAccessTime;
    procedure Test31_Attributes;
  end;

  { TStringTests }
  TStringTests = class(TTestCase)
  private
    FStrings: IStringKit;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Basic transformations
    procedure Test01_From;
    procedure Test02_ToString;
    procedure Test03_Trim;
    procedure Test04_TrimLeft;
    procedure Test05_TrimRight;
    procedure Test06_ToUpper;
    procedure Test07_ToLower;
    procedure Test08_Capitalize;
    // Advanced transformations
    procedure Test09_Reverse;
    procedure Test10_Duplicate;
    procedure Test11_PadLeft;
    procedure Test12_PadRight;
    procedure Test13_PadCenter;
    procedure Test14_RemoveWhitespace;
    procedure Test15_CollapseWhitespace;
    // Pattern matching and replacement
    procedure Test16_Replace;
    procedure Test17_ReplaceRegEx;
    procedure Test18_Extract;
    procedure Test19_ExtractAll;
    procedure Test20_Matches;
    // Substrings and parts
    procedure Test21_SubString;
    procedure Test22_Left;
    procedure Test23_Right;
    procedure Test24_Words;
    // Tests and information
    procedure Test25_Contains;
    procedure Test26_StartsWith;
    procedure Test27_EndsWith;
    procedure Test28_IsEmpty;
    procedure Test29_Length;
    procedure Test30_CountSubString;
  end;

implementation

{ TDateTimeTests }

procedure TDateTimeTests.SetUp;
begin
  FDateTime := DateTime;
end;

procedure TDateTimeTests.TearDown;
begin
  FDateTime := nil;
end;

procedure TDateTimeTests.Test01_Now;
var
  CurrentTime: TDateTime;
begin
  CurrentTime := Now;
  AssertTrue('Now should return current time',
    Abs(CurrentTime - FDateTime.Now.ToDateTime) < 1/86400); // Within 1 second
end;

procedure TDateTimeTests.Test02_Today;
begin
  AssertEquals('Today should return current date at midnight',
    Trunc(Date), Trunc(FDateTime.Today.ToDateTime));
end;

procedure TDateTimeTests.Test03_From;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDate(2024, 1, 15);
  AssertEquals('From should return the correct date',
    TestDate, FDateTime.From(TestDate).ToDateTime);
end;

procedure TDateTimeTests.Test04_Year;
var
  TestYear: Integer;
begin
  TestYear := 2024;
  AssertEquals('Year getter should return current year',
    YearOf(Now), FDateTime.Now.Year);
  AssertEquals('Year setter should set specified year',
    TestYear, FDateTime.Now.Year(TestYear).Year);
end;

procedure TDateTimeTests.Test05_Month;
var
  TestMonth: Integer;
begin
  TestMonth := 6;
  AssertEquals('Month getter should return current month',
    MonthOf(Now), FDateTime.Now.Month);
  AssertEquals('Month setter should set specified month',
    TestMonth, FDateTime.Now.Month(TestMonth).Month);
end;

procedure TDateTimeTests.Test06_Day;
var
  TestDay: Integer;
begin
  TestDay := 15;
  AssertEquals('Day getter should return current day',
    DayOf(Now), FDateTime.Now.Day);
  AssertEquals('Day setter should set specified day',
    TestDay, FDateTime.Now.Day(TestDay).Day);
end;

procedure TDateTimeTests.Test07_Hour;
var
  TestHour: Integer;
begin
  TestHour := 12;
  AssertEquals('Hour getter should return current hour',
    HourOf(Now), FDateTime.Now.Hour);
  AssertEquals('Hour setter should set specified hour',
    TestHour, FDateTime.Now.Hour(TestHour).Hour);
end;

procedure TDateTimeTests.Test08_Minute;
var
  TestMinute: Integer;
begin
  TestMinute := 30;
  AssertEquals('Minute getter should return current minute',
    MinuteOf(Now), FDateTime.Now.Minute);
  AssertEquals('Minute setter should set specified minute',
    TestMinute, FDateTime.Now.Minute(TestMinute).Minute);
end;

procedure TDateTimeTests.Test09_Second;
var
  TestSecond: Integer;
begin
  TestSecond := 45;
  AssertEquals('Second getter should return current second',
    SecondOf(Now), FDateTime.Now.Second);
  AssertEquals('Second setter should set specified second',
    TestSecond, FDateTime.Now.Second(TestSecond).Second);
end;

procedure TDateTimeTests.Test10_Millisecond;
var
  TestMillisecond: Integer;
begin
  TestMillisecond := 500;
  AssertEquals('Millisecond getter should return current millisecond',
    MillisecondOf(Now), FDateTime.Now.Millisecond);
  AssertEquals('Millisecond setter should set specified millisecond',
    TestMillisecond, FDateTime.Now.Millisecond(TestMillisecond).Millisecond);
end;

procedure TDateTimeTests.Test11_AddYears;
var
  StartDate: TDateTime;
begin
  StartDate := EncodeDate(2024, 1, 15);
  AssertEquals('AddYears should add specified years',
    EncodeDate(2028, 1, 15),
    FDateTime.From(StartDate).AddYears(4).ToDateTime);
end;

procedure TDateTimeTests.Test12_AddMonths;
var
  StartDate: TDateTime;
begin
  StartDate := EncodeDate(2024, 1, 15);
  AssertEquals('AddMonths should add specified months',
    EncodeDate(2024, 3, 15),
    FDateTime.From(StartDate).AddMonths(2).ToDateTime);
end;

procedure TDateTimeTests.Test13_AddDays;
var
  StartDate: TDateTime;
begin
  StartDate := EncodeDate(2024, 1, 15);
  AssertEquals('AddDays should add specified days',
    EncodeDate(2024, 1, 18),
    FDateTime.From(StartDate).AddDays(3).ToDateTime);
end;

procedure TDateTimeTests.Test14_AddHours;
var
  StartDate: TDateTime;
begin
  StartDate := EncodeDate(2024, 1, 15, 12, 0, 0);
  AssertEquals('AddHours should add specified hours',
    EncodeDate(2024, 1, 15, 13, 0, 0),
    FDateTime.From(StartDate).AddHours(1).ToDateTime);
end;

procedure TDateTimeTests.Test15_AddMinutes;
var
  StartDate: TDateTime;
begin
  StartDate := EncodeDate(2024, 1, 15, 12, 0, 0);
  AssertEquals('AddMinutes should add specified minutes',
    EncodeDate(2024, 1, 15, 12, 30, 0),
    FDateTime.From(StartDate).AddMinutes(30).ToDateTime);
end;

procedure TDateTimeTests.Test16_AddSeconds;
var
  StartDate: TDateTime;
begin
  StartDate := EncodeDate(2024, 1, 15, 12, 0, 0);
  AssertEquals('AddSeconds should add specified seconds',
    EncodeDate(2024, 1, 15, 12, 0, 30),
    FDateTime.From(StartDate).AddSeconds(30).ToDateTime);
end;

procedure TDateTimeTests.Test17_StartOfYear;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDate(2024, 1, 1);
  AssertEquals('StartOfYear should return first day',
    1, DayOf(FDateTime.From(TestDate).StartOfYear.ToDateTime));
end;

procedure TDateTimeTests.Test18_StartOfMonth;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDate(2024, 3, 1);
  AssertEquals('StartOfMonth should return first day',
    1, DayOf(FDateTime.From(TestDate).StartOfMonth.ToDateTime));
end;

procedure TDateTimeTests.Test19_StartOfDay;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDate(2024, 1, 15, 12, 0, 0);
  AssertEquals('StartOfDay should return first day',
    1, DayOf(FDateTime.From(TestDate).StartOfDay.ToDateTime));
end;

procedure TDateTimeTests.Test20_EndOfYear;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDate(2024, 12, 31);
  AssertEquals('EndOfYear should return last day',
    366, DayOf(FDateTime.From(TestDate).EndOfYear.ToDateTime));
end;

procedure TDateTimeTests.Test21_EndOfMonth;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDate(2024, 3, 31);
  AssertEquals('EndOfMonth should return last day',
    31, DayOf(FDateTime.From(TestDate).EndOfMonth.ToDateTime));
end;

procedure TDateTimeTests.Test22_EndOfDay;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDate(2024, 1, 15, 23, 59, 59);
  AssertEquals('EndOfDay should return last day',
    24, HourOf(FDateTime.From(TestDate).EndOfDay.ToDateTime));
end;

procedure TDateTimeTests.Test23_IsBefore;
var
  Date1, Date2: TDateTime;
begin
  Date1 := EncodeDate(2024, 1, 1);
  Date2 := EncodeDate(2024, 2, 1);
  
  AssertTrue('IsBefore should work correctly',
    FDateTime.From(Date1).IsBefore(Date2));
end;

procedure TDateTimeTests.Test24_IsAfter;
var
  Date1, Date2: TDateTime;
begin
  Date1 := EncodeDate(2024, 1, 1);
  Date2 := EncodeDate(2024, 2, 1);
  
  AssertTrue('IsAfter should work correctly',
    FDateTime.From(Date2).IsAfter(Date1));
end;

procedure TDateTimeTests.Test25_IsSameDay;
var
  Date1, Date2: TDateTime;
begin
  Date1 := EncodeDate(2024, 1, 15);
  Date2 := EncodeDate(2024, 1, 15);
  
  AssertTrue('IsSameDay should work correctly',
    FDateTime.From(Date1).IsSameDay(Date2));
end;

procedure TDateTimeTests.Test26_IsSameMonth;
var
  Date1, Date2: TDateTime;
begin
  Date1 := EncodeDate(2024, 1, 15);
  Date2 := EncodeDate(2024, 1, 15);
  
  AssertTrue('IsSameMonth should work correctly',
    FDateTime.From(Date1).IsSameMonth(Date2));
end;

procedure TDateTimeTests.Test27_IsSameYear;
var
  Date1, Date2: TDateTime;
begin
  Date1 := EncodeDate(2024, 1, 15);
  Date2 := EncodeDate(2024, 1, 15);
  
  AssertTrue('IsSameYear should work correctly',
    FDateTime.From(Date1).IsSameYear(Date2));
end;

procedure TDateTimeTests.Test28_ToDateTime;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDate(2024, 1, 15);
  AssertEquals('ToDateTime should return the correct date',
    TestDate, FDateTime.From(TestDate).ToDateTime);
end;

procedure TDateTimeTests.Test29_ToString;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDate(2024, 1, 15);
  AssertEquals('ToString should return the correct string',
    FormatDateTime('dddd, mmmm d, yyyy', TestDate),
    FDateTime.From(TestDate).ToString);
end;

{ TFSTests }

procedure TFSTests.SetUp;
begin
  FFS := Files;
  FTestDir := 'test_dir';
  FTestFile := FTestDir + PathDelim + 'test.txt';
  
  // Ensure clean test environment
  if DirectoryExists(FTestDir) then
    DeleteDirectory(FTestDir, True);
  CreateDir(FTestDir);
end;

procedure TFSTests.TearDown;
begin
  // Clean up test environment
  if DirectoryExists(FTestDir) then
    DeleteDirectory(FTestDir, True);
  FFS := nil;
end;

procedure TFSTests.Test01_ReadFile;
const
  TestContent = 'Hello, World!';
var
  Content: string;
begin
  // Test read
  Content := FFS.ReadFile(FTestFile)
                .ToString;
  AssertEquals('File content should match written content',
    TestContent, Content);
end;

procedure TFSTests.Test02_WriteFile;
const
  TestContent = 'Hello, World!';
begin
  // Test write
  FFS.SetContent(TestContent)
     .WriteFile(FTestFile);
  AssertTrue('File should exist after write',
    FileExists(FTestFile));
end;

procedure TFSTests.Test03_AppendFile;
const
  TestContent = 'New Line';
var
  Content: string;
begin
  // Test append
  FFS.AppendText(TestContent)
     .WriteFile(FTestFile);
  Content := FFS.ReadFile(FTestFile)
                .ToString;
  AssertTrue('File should contain appended content',
    Pos(TestContent, Content) > 0);
end;

procedure TFSTests.Test04_DeleteFile;
begin
  // Test delete
  FFS.DeleteFile;
  AssertFalse('File should not exist after delete',
    FileExists(FTestFile));
end;

procedure TFSTests.Test05_CopyTo;
var
  CopyDir: string;
begin
  CopyDir := FTestDir + PathDelim + 'copy_dir';
  
  // Test create directory
  FFS.CreateDirectory(CopyDir);
  AssertTrue('Directory should exist after creation',
    DirectoryExists(CopyDir));

  // Test copy
  FFS.CopyTo(FTestFile, CopyDir + PathDelim + 'test.txt');
  AssertTrue('File should exist after copy',
    FileExists(CopyDir + PathDelim + 'test.txt'));

  // Test delete
  FFS.DeleteFile;
  AssertFalse('File should not exist after delete',
    FileExists(FTestFile));
end;

procedure TFSTests.Test06_MoveTo;
var
  MoveDir: string;
begin
  MoveDir := FTestDir + PathDelim + 'move_dir';
  
  // Test create directory
  FFS.CreateDirectory(MoveDir);
  AssertTrue('Directory should exist after creation',
    DirectoryExists(MoveDir));

  // Test move
  FFS.MoveTo(FTestFile, MoveDir + PathDelim + 'test.txt');
  AssertTrue('File should exist after move',
    FileExists(MoveDir + PathDelim + 'test.txt'));

  // Test delete
  FFS.DeleteFile;
  AssertFalse('File should not exist after delete',
    FileExists(FTestFile));
end;

procedure TFSTests.Test07_SetContent;
const
  TestContent = 'Hello, World!';
begin
  // Test set content
  FFS.SetContent(TestContent)
     .WriteFile(FTestFile);
  AssertTrue('File should exist after set content',
    FileExists(FTestFile));
end;

procedure TFSTests.Test08_AppendText;
const
  TestContent = 'New Line';
var
  Content: string;
begin
  // Test append text
  FFS.AppendText(TestContent)
     .WriteFile(FTestFile);
  Content := FFS.ReadFile(FTestFile)
                .ToString;
  AssertTrue('File should contain appended content',
    Pos(TestContent, Content) > 0);
end;

procedure TFSTests.Test09_PrependText;
const
  TestContent = 'New Line';
var
  Content: string;
begin
  // Test prepend text
  FFS.AppendText(TestContent)
     .WriteFile(FTestFile);
  Content := FFS.ReadFile(FTestFile)
                .ToString;
  AssertTrue('File should contain prepended content',
    Pos(TestContent, Content) > 0);
end;

procedure TFSTests.Test10_ReplaceText;
const
  TestContent = 'Hello, World!';
var
  Content: string;
begin
  // Test replace text
  FFS.SetContent(TestContent)
     .WriteFile(FTestFile);
  Content := FFS.ReadFile(FTestFile)
                .ToString;
  AssertEquals('File content should match replaced content',
    TestContent, Content);
end;

procedure TFSTests.Test11_CreateDirectory;
var
  SubDir: string;
begin
  SubDir := FTestDir + PathDelim + 'subdir';
  
  // Test create directory
  FFS.CreateDirectory(SubDir);
  AssertTrue('Directory should exist after creation',
    DirectoryExists(SubDir));
end;

procedure TFSTests.Test12_DeleteDirectory;
var
  SubDir: string;
begin
  SubDir := FTestDir + PathDelim + 'subdir';
  
  // Test create directory
  FFS.CreateDirectory(SubDir);
  AssertTrue('Directory should exist after creation',
    DirectoryExists(SubDir));

  // Test delete directory
  FFS.DeleteDirectory(SubDir);
  AssertFalse('Directory should not exist after deletion',
    DirectoryExists(SubDir));
end;

procedure TFSTests.Test13_EnsureDirectory;
var
  SubDir: string;
begin
  SubDir := FTestDir + PathDelim + 'subdir';
  
  // Test ensure directory
  FFS.EnsureDirectory(SubDir);
  AssertTrue('Directory should exist after ensure',
    DirectoryExists(SubDir));
end;

procedure TFSTests.Test14_DirectoryExists;
var
  SubDir: string;
begin
  SubDir := FTestDir + PathDelim + 'subdir';
  
  // Test create directory
  FFS.CreateDirectory(SubDir);
  AssertTrue('Directory should exist after creation',
    DirectoryExists(SubDir));

  // Test directory exists
  AssertTrue('Directory should exist',
    FFS.DirectoryExists(SubDir));
end;

procedure TFSTests.Test15_ChangeExtension;
const
  TestFileName = 'test.txt';
  NewExt = '.dat';
begin
  AssertEquals('ChangeExtension should work correctly',
    ChangeFileExt(TestFileName, NewExt),
    FFS.From(TestFileName).ChangeExtension(NewExt).ToString);
end;

procedure TFSTests.Test16_GetFileName;
const
  TestFileName = 'test.txt';
begin
  AssertEquals('GetFileName should return correct name',
    TestFileName, FFS.From(FTestDir + PathDelim + TestFileName).GetFileName);
end;

procedure TFSTests.Test17_GetDirectory;
const
  TestFileName = 'test.txt';
begin
  AssertEquals('GetDirectory should return correct directory',
    FTestDir, FFS.From(FTestDir + PathDelim + TestFileName).GetDirectory);
end;

procedure TFSTests.Test18_GetExtension;
const
  TestFileName = 'test.txt';
begin
  AssertEquals('GetExtension should return correct extension',
    ExtractFileExt(TestFileName),
    FFS.From(TestFileName).GetExtension);
end;

procedure TFSTests.Test19_GetFullPath;
const
  TestFileName = 'test.txt';
begin
  AssertEquals('GetFullPath should return correct full path',
    FTestDir + PathDelim + TestFileName,
    FFS.From(TestFileName).GetFullPath);
end;

procedure TFSTests.Test20_SearchFiles;
var
  Files: array[1..3] of string;
  SearchResults: TStringArray;
  i: Integer;
begin
  // Create test files
  for i := 1 to 3 do
  begin
    Files[i] := FTestDir + PathDelim + 'test' + IntToStr(i) + '.txt';
    FFS.SetContent('Test file ' + IntToStr(i))
       .WriteFile(Files[i]);
  end;

  // Test search
  SearchResults := FFS.SearchFiles('*.txt', True);
  AssertEquals('Should find all test files',
    Length(Files), Length(SearchResults));
end;

procedure TFSTests.Test21_SearchFilesIn;
var
  Files: array[1..3] of string;
  SearchResults: TStringArray;
  i: Integer;
begin
  // Create test files
  for i := 1 to 3 do
  begin
    Files[i] := FTestDir + PathDelim + 'test' + IntToStr(i) + '.txt';
    FFS.SetContent('Test file ' + IntToStr(i))
       .WriteFile(Files[i]);
  end;

  // Test search
  SearchResults := FFS.SearchFiles('*.txt', False);
  AssertEquals('Should find all test files',
    Length(Files), Length(SearchResults));
end;

procedure TFSTests.Test22_FindNewestFile;
var
  Files: array[1..3] of string;
  SearchResults: TStringArray;
  i: Integer;
begin
  // Create test files
  for i := 1 to 3 do
  begin
    Files[i] := FTestDir + PathDelim + 'test' + IntToStr(i) + '.txt';
    FFS.SetContent('Test file ' + IntToStr(i))
       .WriteFile(Files[i]);
  end;

  // Test find newest file
  SearchResults := FFS.FindNewestFile('*.txt');
  AssertEquals('Should find the newest test file',
    'test3.txt', SearchResults[0]);
end;

procedure TFSTests.Test23_FindOldestFile;
var
  Files: array[1..3] of string;
  SearchResults: TStringArray;
  i: Integer;
begin
  // Create test files
  for i := 1 to 3 do
  begin
    Files[i] := FTestDir + PathDelim + 'test' + IntToStr(i) + '.txt';
    FFS.SetContent('Test file ' + IntToStr(i))
       .WriteFile(Files[i]);
  end;

  // Test find oldest file
  SearchResults := FFS.FindOldestFile('*.txt');
  AssertEquals('Should find the oldest test file',
    'test1.txt', SearchResults[0]);
end;

procedure TFSTests.Test24_FindLargestFile;
var
  Files: array[1..3] of string;
  SearchResults: TStringArray;
  i: Integer;
begin
  // Create test files
  for i := 1 to 3 do
  begin
    Files[i] := FTestDir + PathDelim + 'test' + IntToStr(i) + '.txt';
    FFS.SetContent('Test file ' + IntToStr(i))
       .WriteFile(Files[i]);
  end;

  // Test find largest file
  SearchResults := FFS.FindLargestFile('*.txt');
  AssertEquals('Should find the largest test file',
    'test3.txt', SearchResults[0]);
end;

procedure TFSTests.Test25_FindSmallestFile;
var
  Files: array[1..3] of string;
  SearchResults: TStringArray;
  i: Integer;
begin
  // Create test files
  for i := 1 to 3 do
  begin
    Files[i] := FTestDir + PathDelim + 'test' + IntToStr(i) + '.txt';
    FFS.SetContent('Test file ' + IntToStr(i))
       .WriteFile(Files[i]);
  end;

  // Test find smallest file
  SearchResults := FFS.FindSmallestFile('*.txt');
  AssertEquals('Should find the smallest test file',
    'test1.txt', SearchResults[0]);
end;

procedure TFSTests.Test26_Exists;
begin
  FFS.SetContent('Test')
     .WriteFile(FTestFile);
     
  AssertTrue('File should exist',
    FFS.Exists);
end;

procedure TFSTests.Test27_Size;
begin
  AssertTrue('Size should be greater than 0',
    FFS.Size > 0);
end;

procedure TFSTests.Test28_CreationTime;
begin
  AssertTrue('Creation time should be recent',
    Abs(Now - FFS.CreationTime) < 1);
end;

procedure TFSTests.Test29_LastWriteTime;
begin
  AssertTrue('Last write time should be recent',
    Abs(Now - FFS.LastWriteTime) < 1);
end;

procedure TFSTests.Test30_LastAccessTime;
begin
  AssertTrue('Last access time should be recent',
    Abs(Now - FFS.LastAccessTime) < 1);
end;

procedure TFSTests.Test31_Attributes;
begin
  FFS.SetContent('Test')
     .WriteFile(FTestFile);
     
  AssertTrue('File should exist',
    FFS.Exists);
  AssertTrue('Size should be greater than 0',
    FFS.Size > 0);
  AssertTrue('Creation time should be recent',
    Abs(Now - FFS.CreationTime) < 1);
end;

{ TStringTests }

procedure TStringTests.SetUp;
begin
  FStrings := Strings;
end;

procedure TStringTests.TearDown;
begin
  FStrings := nil;
end;

procedure TStringTests.Test01_From;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('From should return the correct string',
    TestStr, FStrings.From(TestStr).ToString);
end;

procedure TStringTests.Test02_ToString;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('ToString should return the correct string',
    TestStr, FStrings.From(TestStr).ToString);
end;

procedure TStringTests.Test03_Trim;
const
  TestStr = '  Hello, World!  ';
begin
  AssertEquals('Trim should remove surrounding whitespace',
    'Hello, World!',
    FStrings.From(TestStr).Trim.ToString);
end;

procedure TStringTests.Test04_TrimLeft;
const
  TestStr = '  Hello, World!  ';
begin
  AssertEquals('TrimLeft should remove leading whitespace',
    'Hello, World!  ',
    FStrings.From(TestStr).TrimLeft.ToString);
end;

procedure TStringTests.Test05_TrimRight;
const
  TestStr = '  Hello, World!  ';
begin
  AssertEquals('TrimRight should remove trailing whitespace',
    '  Hello, World!',
    FStrings.From(TestStr).TrimRight.ToString);
end;

procedure TStringTests.Test06_ToUpper;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('ToUpper should work correctly',
    'HELLO, WORLD!',
    FStrings.From(TestStr).ToUpper.ToString);
end;

procedure TStringTests.Test07_ToLower;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('ToLower should work correctly',
    'hello, world!',
    FStrings.From(TestStr).ToLower.ToString);
end;

procedure TStringTests.Test08_Capitalize;
const
  TestStr = 'hello, world!';
begin
  AssertEquals('Capitalize should work correctly',
    'Hello, World!',
    FStrings.From(TestStr).Capitalize.ToString);
end;

procedure TStringTests.Test09_Reverse;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('Reverse should work correctly',
    '!dlroW ,olleH',
    FStrings.From(TestStr).Reverse.ToString);
end;

procedure TStringTests.Test10_Duplicate;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('Duplicate should work correctly',
    TestStr + TestStr,
    FStrings.From(TestStr).Duplicate(2).ToString);
end;

procedure TStringTests.Test11_PadLeft;
const
  TestStr = 'test';
begin
  AssertEquals('PadLeft should work correctly',
    '****test',
    FStrings.From(TestStr).PadLeft(8, '*').ToString);
end;

procedure TStringTests.Test12_PadRight;
const
  TestStr = 'test';
begin
  AssertEquals('PadRight should work correctly',
    'test****',
    FStrings.From(TestStr).PadRight(8, '*').ToString);
end;

procedure TStringTests.Test13_PadCenter;
const
  TestStr = 'test';
begin
  AssertEquals('PadCenter should work correctly',
    '**test**',
    FStrings.From(TestStr).PadCenter(8, '*').ToString);
end;

procedure TStringTests.Test14_RemoveWhitespace;
const
  TestStr = '  too   many    spaces  ';
begin
  AssertEquals('RemoveWhitespace should work correctly',
    'toomanyspaces',
    FStrings.From(TestStr).RemoveWhitespace.ToString);
end;

procedure TStringTests.Test15_CollapseWhitespace;
const
  TestStr = '  too   many    spaces  ';
begin
  AssertEquals('CollapseWhitespace should work correctly',
    'too many spaces',
    FStrings.From(TestStr).CollapseWhitespace.Trim.ToString);
end;

procedure TStringTests.Test16_Replace;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('Replace should work correctly',
    'Hi, World!',
    FStrings.From(TestStr).Replace('Hello', 'Hi').ToString);
end;

procedure TStringTests.Test17_ReplaceRegEx;
const
  TestStr = 'The year is 2024';
begin
  AssertEquals('ReplaceRegEx should work correctly',
    'The year is 2024',
    FStrings.From(TestStr).ReplaceRegEx('\d+', '2024').ToString);
end;

procedure TStringTests.Test18_Extract;
const
  TestStr = 'The year is 2024';
begin
  AssertEquals('Extract should work correctly',
    '2024',
    FStrings.From(TestStr).Extract('\d+'));
end;

procedure TStringTests.Test19_ExtractAll;
const
  TestStr = 'The year is 2024';
begin
  AssertEquals('ExtractAll should work correctly',
    '2024',
    FStrings.From(TestStr).ExtractAll('\d+'));
end;

procedure TStringTests.Test20_Matches;
const
  TestStr = 'The year is 2024';
begin
  AssertTrue('Matches should work correctly',
    FStrings.From(TestStr).Matches('\d+'));
end;

procedure TStringTests.Test21_SubString;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('SubString should work correctly',
    'Hello',
    FStrings.From(TestStr).SubString(1, 5).ToString);
end;

procedure TStringTests.Test22_Left;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('Left should work correctly',
    'Hello',
    FStrings.From(TestStr).Left(5).ToString);
end;

procedure TStringTests.Test23_Right;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('Right should work correctly',
    'World!',
    FStrings.From(TestStr).Right(6).ToString);
end;

procedure TStringTests.Test24_Words;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('Words should work correctly',
    'Hello, World!',
    FStrings.From(TestStr).Words.ToString);
end;

procedure TStringTests.Test25_Contains;
const
  TestStr = 'Hello, World!';
begin
  AssertTrue('Contains should work correctly',
    FStrings.From(TestStr).Contains('Hello'));
end;

procedure TStringTests.Test26_StartsWith;
const
  TestStr = 'Hello, World!';
begin
  AssertTrue('StartsWith should work correctly',
    FStrings.From(TestStr).StartsWith('Hello'));
end;

procedure TStringTests.Test27_EndsWith;
const
  TestStr = 'Hello, World!';
begin
  AssertTrue('EndsWith should work correctly',
    FStrings.From(TestStr).EndsWith('World!'));
end;

procedure TStringTests.Test28_IsEmpty;
const
  TestStr = '';
begin
  AssertTrue('IsEmpty should work correctly',
    FStrings.From(TestStr).IsEmpty);
end;

procedure TStringTests.Test29_Length;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('Length should work correctly',
    Length(TestStr), FStrings.From(TestStr).Length);
end;

procedure TStringTests.Test30_CountSubString;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('CountSubString should work correctly',
    CountChar(TestStr, 'o'), FStrings.From(TestStr).CountSubString('o'));
end;

initialization
  RegisterTest(TDateTimeTests);
  RegisterTest(TFSTests);
  RegisterTest(TStringTests);
end.

