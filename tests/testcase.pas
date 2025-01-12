unit testcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, fpcunit, testutils, testregistry,
  TidyKit, TidyKit.FS, TidyKit.Strings;

type
  TStringArray = array of string;

  { TDateTimeTests }
  TDateTimeTests = class(TTestCase)
  private
    FDateTime: TDateTimeKit;
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
    // Business day functions
    procedure Test30_IsBusinessDay;
    procedure Test31_NextBusinessDay;
    procedure Test32_PreviousBusinessDay;
    procedure Test33_AddBusinessDays;
  end;

  { TFSTests }
  TFSTests = class(TTestCase)
  private
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
    procedure Test07_AppendText;
    procedure Test08_PrependText;
    procedure Test09_ReplaceText;
    // Directory operations
    procedure Test10_CreateDirectory;
    procedure Test11_DeleteDirectory;
    procedure Test12_EnsureDirectory;
    procedure Test13_GetFileName;
    procedure Test14_GetFileNameWithoutExt;
    procedure Test15_GetDirectory;
    procedure Test16_GetExtension;
    // File information
    procedure Test17_Exists;
    procedure Test18_DirectoryExists;
    procedure Test19_GetSize;
    procedure Test20_GetCreationTime;
    procedure Test21_GetLastAccessTime;
    procedure Test22_GetLastWriteTime;
    procedure Test23_GetAttributes;
    // Search operations
    procedure Test24_SearchFiles;
    procedure Test25_FindNewestFile;
  end;

  { TStringTests }
  TStringTests = class(TTestCase)
  private
    FStrings: TStringKit;
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
  // No setup needed for static functions
end;

procedure TDateTimeTests.TearDown;
begin
  // No teardown needed for static functions
end;

procedure TDateTimeTests.Test01_Now;
var
  CurrentTime: TDateTime;
begin
  CurrentTime := Now;
  AssertTrue('Now should return current time',
    Abs(CurrentTime - TDateTimeKit.GetNow) < 1/86400); // Within 1 second
end;

procedure TDateTimeTests.Test02_Today;
begin
  AssertEquals('Today should return current date at midnight',
    Trunc(Date), Trunc(TDateTimeKit.GetToday));
end;

procedure TDateTimeTests.Test03_From;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDate(2024, 1, 15);
  AssertEquals('GetDateTime should return the correct date',
    TestDate, TDateTimeKit.GetDateTime(TestDate));
end;

procedure TDateTimeTests.Test04_Year;
var
  TestYear: Integer;
  TestDate: TDateTime;
begin
  TestYear := 2024;
  TestDate := Now;
  AssertEquals('Year getter should return current year',
    YearOf(TestDate), TDateTimeKit.GetYear(TestDate));
  AssertEquals('Year setter should set specified year',
    TestYear, TDateTimeKit.GetYear(TDateTimeKit.SetYear(TestDate, TestYear)));
end;

procedure TDateTimeTests.Test05_Month;
var
  TestMonth: Integer;
  TestDate: TDateTime;
begin
  TestMonth := 6;
  TestDate := Now;
  AssertEquals('Month getter should return current month',
    MonthOf(TestDate), TDateTimeKit.GetMonth(TestDate));
  AssertEquals('Month setter should set specified month',
    TestMonth, TDateTimeKit.GetMonth(TDateTimeKit.SetMonth(TestDate, TestMonth)));
end;

procedure TDateTimeTests.Test06_Day;
var
  TestDay: Integer;
  TestDate: TDateTime;
begin
  TestDay := 15;
  TestDate := Now;
  AssertEquals('Day getter should return current day',
    DayOf(TestDate), TDateTimeKit.GetDay(TestDate));
  AssertEquals('Day setter should set specified day',
    TestDay, TDateTimeKit.GetDay(TDateTimeKit.SetDay(TestDate, TestDay)));
end;

procedure TDateTimeTests.Test07_Hour;
var
  TestHour: Integer;
  TestDate: TDateTime;
begin
  TestHour := 14;
  TestDate := Now;
  AssertEquals('Hour getter should return current hour',
    HourOf(TestDate), TDateTimeKit.GetHour(TestDate));
  AssertEquals('Hour setter should set specified hour',
    TestHour, TDateTimeKit.GetHour(TDateTimeKit.SetHour(TestDate, TestHour)));
end;

procedure TDateTimeTests.Test08_Minute;
var
  TestMinute: Integer;
  TestDate: TDateTime;
begin
  TestMinute := 30;
  TestDate := Now;
  AssertEquals('Minute getter should return current minute',
    MinuteOf(TestDate), TDateTimeKit.GetMinute(TestDate));
  AssertEquals('Minute setter should set specified minute',
    TestMinute, TDateTimeKit.GetMinute(TDateTimeKit.SetMinute(TestDate, TestMinute)));
end;

procedure TDateTimeTests.Test09_Second;
var
  TestSecond: Integer;
  TestDate: TDateTime;
begin
  TestSecond := 45;
  TestDate := Now;
  AssertEquals('Second getter should return current second',
    SecondOf(TestDate), TDateTimeKit.GetSecond(TestDate));
  AssertEquals('Second setter should set specified second',
    TestSecond, TDateTimeKit.GetSecond(TDateTimeKit.SetSecond(TestDate, TestSecond)));
end;

procedure TDateTimeTests.Test10_Millisecond;
var
  TestMillisecond: Integer;
  TestDate: TDateTime;
begin
  TestMillisecond := 500;
  TestDate := Now;
  AssertEquals('Millisecond getter should return current millisecond',
    MilliSecondOf(TestDate), TDateTimeKit.GetMillisecond(TestDate));
  AssertEquals('Millisecond setter should set specified millisecond',
    TestMillisecond, TDateTimeKit.GetMillisecond(TDateTimeKit.SetMilliSecond(TestDate, TestMillisecond)));
end;

procedure TDateTimeTests.Test11_AddYears;
var
  StartDate, Expected: TDateTime;
begin
  StartDate := EncodeDate(2024, 1, 15);
  Expected := EncodeDate(2028, 1, 15);
  AssertEquals('AddYears should add specified years',
    Expected, TDateTimeKit.AddYears(StartDate, 4));
end;

procedure TDateTimeTests.Test12_AddMonths;
var
  StartDate, Expected: TDateTime;
begin
  StartDate := EncodeDate(2024, 1, 15);
  Expected := EncodeDate(2024, 3, 15);
  AssertEquals('AddMonths should add specified months',
    Expected, TDateTimeKit.AddMonths(StartDate, 2));
end;

procedure TDateTimeTests.Test13_AddDays;
var
  StartDate, Expected: TDateTime;
begin
  StartDate := EncodeDate(2024, 1, 15);
  Expected := EncodeDate(2024, 1, 17);
  AssertEquals('AddDays should add specified days',
    Expected, TDateTimeKit.AddDays(StartDate, 2));
end;

procedure TDateTimeTests.Test14_AddHours;
var
  StartDate, Expected: TDateTime;
begin
  StartDate := EncodeDate(2024, 1, 15) + EncodeTime(12, 0, 0, 0);
  Expected := EncodeDate(2024, 1, 15) + EncodeTime(14, 0, 0, 0);
  AssertEquals('AddHours should add specified hours',
    Expected, TDateTimeKit.AddHours(StartDate, 2));
end;

procedure TDateTimeTests.Test15_AddMinutes;
var
  StartDate, Expected: TDateTime;
begin
  StartDate := EncodeDate(2024, 1, 15) + EncodeTime(12, 0, 0, 0);
  Expected := EncodeDate(2024, 1, 15) + EncodeTime(12, 30, 0, 0);
  AssertEquals('AddMinutes should add specified minutes',
    Expected, TDateTimeKit.AddMinutes(StartDate, 30));
end;

procedure TDateTimeTests.Test16_AddSeconds;
var
  StartDate, Expected: TDateTime;
begin
  StartDate := EncodeDate(2024, 1, 15) + EncodeTime(12, 0, 0, 0);
  Expected := EncodeDate(2024, 1, 15) + EncodeTime(12, 0, 30, 0);
  AssertEquals('AddSeconds should add specified seconds',
    Expected, TDateTimeKit.AddSeconds(StartDate, 30));
end;

procedure TDateTimeTests.Test17_StartOfYear;
var
  TestDate, Expected: TDateTime;
begin
  TestDate := EncodeDate(2024, 6, 15) + EncodeTime(12, 30, 45, 500);
  Expected := EncodeDate(2024, 1, 1);
  AssertEquals('StartOfYear should set to start of year',
    Expected, TDateTimeKit.StartOfYear(TestDate));
end;

procedure TDateTimeTests.Test18_StartOfMonth;
var
  TestDate, Expected: TDateTime;
begin
  TestDate := EncodeDate(2024, 6, 15) + EncodeTime(12, 30, 45, 500);
  Expected := EncodeDate(2024, 6, 1);
  AssertEquals('StartOfMonth should set to start of month',
    Expected, TDateTimeKit.StartOfMonth(TestDate));
end;

procedure TDateTimeTests.Test19_StartOfDay;
var
  TestDate, Expected: TDateTime;
begin
  TestDate := EncodeDate(2024, 6, 15) + EncodeTime(12, 30, 45, 500);
  Expected := EncodeDate(2024, 6, 15);
  AssertEquals('StartOfDay should set to start of day',
    Expected, TDateTimeKit.StartOfDay(TestDate));
end;

procedure TDateTimeTests.Test20_EndOfYear;
var
  TestDate, Expected: TDateTime;
begin
  TestDate := EncodeDate(2024, 6, 15) + EncodeTime(12, 30, 45, 500);
  Expected := EncodeDate(2024, 12, 31) + EncodeTime(23, 59, 59, 999);
  AssertEquals('EndOfYear should set to end of year',
    Expected, TDateTimeKit.EndOfYear(TestDate));
end;

procedure TDateTimeTests.Test21_EndOfMonth;
var
  TestDate, Expected: TDateTime;
begin
  TestDate := EncodeDate(2024, 6, 15) + EncodeTime(12, 30, 45, 500);
  Expected := EncodeDate(2024, 6, 30) + EncodeTime(23, 59, 59, 999);
  AssertEquals('EndOfMonth should set to end of month',
    Expected, TDateTimeKit.EndOfMonth(TestDate));
end;

procedure TDateTimeTests.Test22_EndOfDay;
var
  TestDate, Expected: TDateTime;
begin
  TestDate := EncodeDate(2024, 6, 15) + EncodeTime(12, 30, 45, 500);
  Expected := EncodeDate(2024, 6, 15) + EncodeTime(23, 59, 59, 999);
  AssertEquals('EndOfDay should set to end of day',
    Expected, TDateTimeKit.EndOfDay(TestDate));
end;

procedure TDateTimeTests.Test23_IsBefore;
var
  Date1, Date2: TDateTime;
begin
  Date1 := EncodeDate(2024, 1, 15);
  Date2 := EncodeDate(2024, 1, 16);
  AssertTrue('IsBefore should work correctly',
    TDateTimeKit.IsBefore(Date1, Date2));
end;

procedure TDateTimeTests.Test24_IsAfter;
var
  Date1, Date2: TDateTime;
begin
  Date1 := EncodeDate(2024, 1, 16);
  Date2 := EncodeDate(2024, 1, 15);
  AssertTrue('IsAfter should work correctly',
    TDateTimeKit.IsAfter(Date1, Date2));
end;

procedure TDateTimeTests.Test25_IsSameDay;
var
  Date1, Date2: TDateTime;
begin
  Date1 := EncodeDate(2024, 1, 15) + EncodeTime(10, 0, 0, 0);
  Date2 := EncodeDate(2024, 1, 15) + EncodeTime(14, 30, 0, 0);
  AssertTrue('IsSameDay should work correctly',
    TDateTimeKit.IsSameDay(Date1, Date2));
end;

procedure TDateTimeTests.Test26_IsSameMonth;
var
  Date1, Date2: TDateTime;
begin
  Date1 := EncodeDate(2024, 1, 15);
  Date2 := EncodeDate(2024, 1, 20);
  AssertTrue('IsSameMonth should work correctly',
    TDateTimeKit.IsSameMonth(Date1, Date2));
end;

procedure TDateTimeTests.Test27_IsSameYear;
var
  Date1, Date2: TDateTime;
begin
  Date1 := EncodeDate(2024, 1, 15);
  Date2 := EncodeDate(2024, 6, 15);
  AssertTrue('IsSameYear should work correctly',
    TDateTimeKit.IsSameYear(Date1, Date2));
end;

procedure TDateTimeTests.Test28_ToDateTime;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDate(2024, 1, 15);
  AssertEquals('GetDateTime should return the correct date',
    TestDate, TDateTimeKit.GetDateTime(TestDate));
end;

procedure TDateTimeTests.Test29_ToString;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDate(2024, 1, 15);
  AssertEquals('GetAsString should return the correct string',
    FormatDateTime('dd/mm/yyyy', TestDate),
    TDateTimeKit.GetAsString(TestDate, 'dd/mm/yyyy'));
end;

// Add new tests for business day functions
procedure TDateTimeTests.Test30_IsBusinessDay;
var
  Monday, Saturday: TDateTime;
begin
  Monday := EncodeDate(2024, 1, 15);    // Monday
  Saturday := EncodeDate(2024, 1, 20);  // Saturday
  AssertTrue('Monday should be a business day',
    TDateTimeKit.IsBusinessDay(Monday));
  AssertFalse('Saturday should not be a business day',
    TDateTimeKit.IsBusinessDay(Saturday));
end;

procedure TDateTimeTests.Test31_NextBusinessDay;
var
  Friday, Monday: TDateTime;
begin
  Friday := EncodeDate(2024, 1, 19);    // Friday
  Monday := EncodeDate(2024, 1, 22);    // Next Monday
  AssertEquals('Next business day after Friday should be Monday',
    Monday, TDateTimeKit.NextBusinessDay(Friday));
end;

procedure TDateTimeTests.Test32_PreviousBusinessDay;
var
  Monday, Friday: TDateTime;
begin
  Monday := EncodeDate(2024, 1, 22);    // Monday
  Friday := EncodeDate(2024, 1, 19);    // Previous Friday
  AssertEquals('Previous business day before Monday should be Friday',
    Friday, TDateTimeKit.PreviousBusinessDay(Monday));
end;

procedure TDateTimeTests.Test33_AddBusinessDays;
var
  StartDate, Expected: TDateTime;
begin
  StartDate := EncodeDate(2024, 1, 15); // Monday
  Expected := EncodeDate(2024, 1, 19);  // Friday (4 business days later)
  AssertEquals('AddBusinessDays should skip weekends',
    Expected, TDateTimeKit.AddBusinessDays(StartDate, 4));
end;

{ TFSTests }

procedure TFSTests.SetUp;
begin
  FTestDir := GetTempDir + PathDelim + 'TidyKitTest';
  FTestFile := FTestDir + PathDelim + 'test.txt';
  
  // Ensure clean test environment
  if DirectoryExists(FTestDir) then
    RemoveDir(FTestDir);
  CreateDir(FTestDir);
end;

procedure TFSTests.TearDown;
begin
  // Clean up test environment
  if DirectoryExists(FTestDir) then
    RemoveDir(FTestDir);
end;

procedure TFSTests.Test01_ReadFile;
const
  TestContent = 'Test Content';
begin
  // Write test content first
  TFileKit.WriteFile(FTestFile, TestContent);
  
  // Test read
  AssertEquals('ReadFile should read the correct content',
    TestContent, TFileKit.ReadFile(FTestFile));
end;

procedure TFSTests.Test02_WriteFile;
const
  TestContent = 'Test Content';
begin
  // Test write
  TFileKit.WriteFile(FTestFile, TestContent);
  AssertTrue('File should exist after write',
    FileExists(FTestFile));
end;

procedure TFSTests.Test03_AppendFile;
const
  FirstLine = 'First Line';
  SecondLine = 'Second Line';
begin
  // Create initial file
  TFileKit.WriteFile(FTestFile, FirstLine);
  
  // Test append
  TFileKit.AppendFile(FTestFile, SecondLine);
  
  // Verify content
  AssertEquals('AppendFile should append content correctly',
    FirstLine + SecondLine, TFileKit.ReadFile(FTestFile));
end;

procedure TFSTests.Test04_DeleteFile;
const
  TestContent = 'Test Content';
begin
  // Create test file
  TFileKit.WriteFile(FTestFile, TestContent);
  
  // Test delete
  TFileKit.DeleteFile(FTestFile);
  AssertFalse('File should not exist after delete',
    FileExists(FTestFile));
end;

procedure TFSTests.Test05_CopyTo;
const
  TestContent = 'Test Content';
var
  CopyFile: string;
begin
  CopyFile := FTestDir + PathDelim + 'copy.txt';
  
  // Create source file
  TFileKit.WriteFile(FTestFile, TestContent);
  
  // Test copy
  TFileKit.CopyFile(FTestFile, CopyFile);
  
  AssertTrue('Destination file should exist after copy',
    FileExists(CopyFile));
  AssertEquals('Copied content should match source',
    TestContent, TFileKit.ReadFile(CopyFile));
end;

procedure TFSTests.Test06_MoveTo;
const
  TestContent = 'Test Content';
var
  MoveFile: string;
begin
  MoveFile := FTestDir + PathDelim + 'moved.txt';
  
  // Create source file
  TFileKit.WriteFile(FTestFile, TestContent);
  
  // Test move
  TFileKit.MoveFile(FTestFile, MoveFile);
  
  AssertFalse('Source file should not exist after move',
    FileExists(FTestFile));
  AssertTrue('Destination file should exist after move',
    FileExists(MoveFile));
  AssertEquals('Moved content should match source',
    TestContent, TFileKit.ReadFile(MoveFile));
end;

procedure TFSTests.Test07_AppendText;
const
  FirstLine = 'First Line';
  SecondLine = 'Second Line';
begin
  // Create initial file
  TFileKit.WriteFile(FTestFile, FirstLine);
  
  // Test append text
  TFileKit.AppendText(FTestFile, SecondLine);
  
  AssertEquals('AppendText should append content correctly',
    FirstLine + SecondLine, TFileKit.ReadFile(FTestFile));
end;

procedure TFSTests.Test08_PrependText;
const
  FirstLine = 'First Line';
  SecondLine = 'Second Line';
begin
  // Create initial file
  TFileKit.WriteFile(FTestFile, SecondLine);
  
  // Test prepend text
  TFileKit.PrependText(FTestFile, FirstLine);
  
  AssertEquals('PrependText should prepend content correctly',
    FirstLine + SecondLine, TFileKit.ReadFile(FTestFile));
end;

procedure TFSTests.Test09_ReplaceText;
const
  OriginalText = 'Hello, World!';
  OldText = 'World';
  NewText = 'TidyKit';
begin
  // Create initial file
  TFileKit.WriteFile(FTestFile, OriginalText);
  
  // Test replace text
  TFileKit.ReplaceText(FTestFile, OldText, NewText);
  
  AssertEquals('ReplaceText should replace content correctly',
    'Hello, TidyKit!', TFileKit.ReadFile(FTestFile));
end;

procedure TFSTests.Test10_CreateDirectory;
var
  TestSubDir: string;
begin
  TestSubDir := FTestDir + PathDelim + 'subdir';
  
  // Test create directory
  TFileKit.CreateDirectory(TestSubDir);
  
  AssertTrue('Directory should exist after creation',
    DirectoryExists(TestSubDir));
end;

procedure TFSTests.Test11_DeleteDirectory;
var
  TestSubDir: string;
  TestSubFile: string;
begin
  TestSubDir := FTestDir + PathDelim + 'subdir';
  TestSubFile := TestSubDir + PathDelim + 'test.txt';
  
  // Create test structure
  TFileKit.CreateDirectory(TestSubDir);
  TFileKit.WriteFile(TestSubFile, 'Test Content');
  
  // Test delete directory
  TFileKit.DeleteDirectory(TestSubDir, True);
  
  AssertFalse('Directory should not exist after deletion',
    DirectoryExists(TestSubDir));
end;

procedure TFSTests.Test12_EnsureDirectory;
var
  DeepDir: string;
begin
  DeepDir := FTestDir + PathDelim + 'deep' + PathDelim + 'path';
  
  // Test ensure directory
  TFileKit.EnsureDirectory(DeepDir + PathDelim + 'file.txt');
  
  AssertTrue('Directory structure should be created',
    DirectoryExists(DeepDir));
end;

procedure TFSTests.Test13_GetFileName;
const
  TestFileName = 'test.txt';
begin
  AssertEquals('GetFileName should return correct name',
    TestFileName, TFileKit.GetFileName(FTestDir + PathDelim + TestFileName));
end;

procedure TFSTests.Test14_GetFileNameWithoutExt;
const
  TestFileName = 'test.txt';
begin
  AssertEquals('GetFileNameWithoutExt should return correct name',
    'test', TFileKit.GetFileNameWithoutExt(FTestDir + PathDelim + TestFileName));
end;

procedure TFSTests.Test15_GetDirectory;
begin
  AssertEquals('GetDirectory should return correct directory',
    ExtractFileName(ExcludeTrailingPathDelimiter(FTestDir)),
    TFileKit.GetDirectory(FTestFile));
end;

procedure TFSTests.Test16_GetExtension;
const
  TestFileName = 'test.txt';
begin
  AssertEquals('GetExtension should return correct extension',
    '.txt', TFileKit.GetExtension(FTestDir + PathDelim + TestFileName));
end;

procedure TFSTests.Test17_Exists;
begin
  // Create test file
  TFileKit.WriteFile(FTestFile, 'Test Content');
  
  AssertTrue('Exists should return true for existing file',
    TFileKit.Exists(FTestFile));
  AssertFalse('Exists should return false for non-existing file',
    TFileKit.Exists(FTestDir + PathDelim + 'nonexistent.txt'));
end;

procedure TFSTests.Test18_DirectoryExists;
begin
  AssertTrue('DirectoryExists should return true for existing directory',
    TFileKit.DirectoryExists(FTestDir));
  AssertFalse('DirectoryExists should return false for non-existing directory',
    TFileKit.DirectoryExists(FTestDir + PathDelim + 'nonexistent'));
end;

procedure TFSTests.Test19_GetSize;
const
  TestContent = 'Test Content';
begin
  // Create test file
  TFileKit.WriteFile(FTestFile, TestContent);
  
  AssertEquals('GetSize should return correct file size',
    Length(TestContent), TFileKit.GetSize(FTestFile));
end;

procedure TFSTests.Test20_GetCreationTime;
begin
  // Create test file
  TFileKit.WriteFile(FTestFile, 'Test Content');
  
  AssertTrue('GetCreationTime should return valid timestamp',
    TFileKit.GetCreationTime(FTestFile) > 0);
end;

procedure TFSTests.Test21_GetLastAccessTime;
begin
  // Create test file
  TFileKit.WriteFile(FTestFile, 'Test Content');
  
  AssertTrue('GetLastAccessTime should return valid timestamp',
    TFileKit.GetLastAccessTime(FTestFile) > 0);
end;

procedure TFSTests.Test22_GetLastWriteTime;
begin
  // Create test file
  TFileKit.WriteFile(FTestFile, 'Test Content');
  
  AssertTrue('GetLastWriteTime should return valid timestamp',
    TFileKit.GetLastWriteTime(FTestFile) > 0);
end;

procedure TFSTests.Test23_GetAttributes;
begin
  // Create test file
  TFileKit.WriteFile(FTestFile, 'Test Content');
  
  AssertFalse('New file should not be read-only',
    TFileKit.GetAttributes(FTestFile).ReadOnly);
end;

procedure TFSTests.Test24_SearchFiles;
var
  Results: TSearchResults;
begin
  // Create test files
  TFileKit.WriteFile(FTestDir + PathDelim + 'test1.txt', 'Content 1');
  TFileKit.WriteFile(FTestDir + PathDelim + 'test2.txt', 'Content 2');
  
  Results := TFileKit.SearchFiles(FTestDir, '*.txt', False);
  AssertEquals('SearchFiles should find correct number of files',
    2, Length(Results));
end;

procedure TFSTests.Test25_FindNewestFile;
var
  NewestFile: string;
begin
  // Create test files
  TFileKit.WriteFile(FTestDir + PathDelim + 'test1.txt', 'Content 1');
  Sleep(100);
  TFileKit.WriteFile(FTestDir + PathDelim + 'test2.txt', 'Content 2');
  
  NewestFile := TFileKit.FindNewestFile(FTestDir, '*.txt', False);
  AssertEquals('FindNewestFile should find the newest file',
    'test2.txt', ExtractFileName(NewestFile));
end;

{ TStringTests }

procedure TStringTests.SetUp;
begin
  // No setup needed for static functions
end;

procedure TStringTests.TearDown;
begin
  // No teardown needed for static functions
end;

procedure TStringTests.Test01_From;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('String value should remain unchanged',
    TestStr, TestStr);
end;

procedure TStringTests.Test02_ToString;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('String value should remain unchanged',
    TestStr, TestStr);
end;

procedure TStringTests.Test03_Trim;
const
  TestStr = '  Hello, World!  ';
begin
  AssertEquals('Trim should remove surrounding whitespace',
    'Hello, World!', TStringKit.Trim(TestStr));
end;

procedure TStringTests.Test04_TrimLeft;
const
  TestStr = '  Hello, World!  ';
begin
  AssertEquals('TrimLeft should remove leading whitespace',
    'Hello, World!  ', TStringKit.TrimLeft(TestStr));
end;

procedure TStringTests.Test05_TrimRight;
const
  TestStr = '  Hello, World!  ';
begin
  AssertEquals('TrimRight should remove trailing whitespace',
    '  Hello, World!', TStringKit.TrimRight(TestStr));
end;

procedure TStringTests.Test06_ToUpper;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('ToUpper should work correctly',
    'HELLO, WORLD!', TStringKit.ToUpper(TestStr));
end;

procedure TStringTests.Test07_ToLower;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('ToLower should work correctly',
    'hello, world!', TStringKit.ToLower(TestStr));
end;

procedure TStringTests.Test08_Capitalize;
const
  TestStr = 'hello, world!';
begin
  AssertEquals('CapitalizeText should work correctly',
    'Hello, World!', TStringKit.CapitalizeText(TestStr));
end;

procedure TStringTests.Test09_Reverse;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('ReverseText should work correctly',
    '!dlroW ,olleH', TStringKit.ReverseText(TestStr));
end;

procedure TStringTests.Test10_Duplicate;
const
  TestStr = 'Hello';
begin
  AssertEquals('DuplicateText should work correctly',
    'HelloHello', TStringKit.DuplicateText(TestStr, 2));
end;

procedure TStringTests.Test11_PadLeft;
const
  TestStr = 'test';
begin
  AssertEquals('PadLeft should work correctly',
    '****test', TStringKit.PadLeft(TestStr, 8, '*'));
end;

procedure TStringTests.Test12_PadRight;
const
  TestStr = 'test';
begin
  AssertEquals('PadRight should work correctly',
    'test****', TStringKit.PadRight(TestStr, 8, '*'));
end;

procedure TStringTests.Test13_PadCenter;
const
  TestStr = 'test';
begin
  AssertEquals('PadCenter should work correctly',
    '**test**', TStringKit.PadCenter(TestStr, 8, '*'));
end;

procedure TStringTests.Test14_RemoveWhitespace;
const
  TestStr = '  too   many    spaces  ';
begin
  AssertEquals('RemoveWhitespace should work correctly',
    'toomanyspaces', TStringKit.RemoveWhitespace(TestStr));
end;

procedure TStringTests.Test15_CollapseWhitespace;
const
  TestStr = '  too   many    spaces  ';
begin
  AssertEquals('CollapseWhitespace should work correctly',
    'too many spaces', TStringKit.Trim(TStringKit.CollapseWhitespace(TestStr)));
end;

procedure TStringTests.Test16_Replace;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('ReplaceText should work correctly',
    'Hi, World!', TStringKit.ReplaceText(TestStr, 'Hello', 'Hi'));
end;

procedure TStringTests.Test17_ReplaceRegEx;
const
  TestStr = 'The year is 2024';
begin
  AssertEquals('ReplaceRegEx should work correctly',
    'The year is 2024', TStringKit.ReplaceRegEx(TestStr, '\d+', '2024'));
end;

procedure TStringTests.Test18_Extract;
const
  TestStr = 'The year is 2024';
var
  Matches: TStringMatches;
begin
  Matches := TStringKit.ExtractMatches(TestStr, '\d+');
  AssertEquals('ExtractMatches should work correctly',
    '2024', Matches[0].Text);
end;

procedure TStringTests.Test19_ExtractAll;
const
  TestStr = 'The year is 2024';
var
  Results: TStringArray;
begin
  Results := TStringKit.ExtractAllMatches(TestStr, '\d+');
  AssertEquals('ExtractAllMatches should work correctly',
    '2024', Results[0]);
end;

procedure TStringTests.Test20_Matches;
const
  TestStr = 'The year is 2024';
begin
  AssertTrue('MatchesPattern should work correctly',
    TStringKit.MatchesPattern(TestStr, '\d+'));
end;

procedure TStringTests.Test21_SubString;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('SubString should work correctly',
    'Hello', TStringKit.SubString(TestStr, 1, 5));
end;

procedure TStringTests.Test22_Left;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('LeftStr should work correctly',
    'Hello', TStringKit.LeftStr(TestStr, 5));
end;

procedure TStringTests.Test23_Right;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('RightStr should work correctly',
    'World!', TStringKit.RightStr(TestStr, 6));
end;

procedure TStringTests.Test24_Words;
const
  TestStr = 'Hello, World!';
var
  WordArray: TStringArray;
begin
  WordArray := TStringKit.GetWords(TestStr);
  AssertEquals('GetWords should work correctly',
    'Hello', WordArray[0]);
  AssertEquals('GetWords should work correctly',
    'World', WordArray[1]);
end;

procedure TStringTests.Test25_Contains;
const
  TestStr = 'Hello, World!';
begin
  AssertTrue('Contains should work correctly',
    TStringKit.Contains(TestStr, 'World'));
end;

procedure TStringTests.Test26_StartsWith;
const
  TestStr = 'Hello, World!';
begin
  AssertTrue('StartsWith should work correctly',
    TStringKit.StartsWith(TestStr, 'Hello'));
end;

procedure TStringTests.Test27_EndsWith;
const
  TestStr = 'Hello, World!';
begin
  AssertTrue('EndsWith should work correctly',
    TStringKit.EndsWith(TestStr, 'World!'));
end;

procedure TStringTests.Test28_IsEmpty;
begin
  AssertTrue('IsEmpty should work correctly',
    TStringKit.IsEmpty(''));
end;

procedure TStringTests.Test29_Length;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('GetLength should work correctly',
    13, TStringKit.GetLength(TestStr));
end;

procedure TStringTests.Test30_CountSubString;
const
  TestStr = 'Hello, Hello, Hello!';
begin
  AssertEquals('CountSubString should work correctly',
    3, TStringKit.CountSubString(TestStr, 'Hello'));
end;

initialization
  RegisterTest(TDateTimeTests);
  RegisterTest(TFSTests);
  RegisterTest(TStringTests);
end.

  // Clean up test environment
  if DirectoryExists(FTestDir) then
    RemoveDir(FTestDir);
  FFS.Free;
  FFS := nil;
end;

procedure TFSTests.Test01_ReadFile;
begin
  // Create test file
  FFS.SetPath(FTestFile);
  FFS.SetContent('Test Content');
  FFS.WriteFile;
  
  // Test read file
  FFS.SetPath(FTestFile);
  FFS.ReadFile;
  AssertEquals('ReadFile should read the correct content',
    'Test Content', FFS.GetContent);
end;

procedure TFSTests.Test02_WriteFile;
begin
  FFS.SetPath(FTestFile);
  FFS.SetContent('Test Content');
  FFS.WriteFile;
  
  AssertTrue('File should exist after write',
    FileExists(FTestFile));
end;

procedure TFSTests.Test03_AppendFile;
begin
  // Create initial file
  FFS.SetPath(FTestFile);
  FFS.SetContent('First Line');
  FFS.WriteFile;
  
  // Test append
  FFS.SetContent('Second Line');
  FFS.AppendFile;
  
  // Verify content
  FFS.ReadFile;
  AssertEquals('AppendFile should append content correctly',
    'First LineSecond Line', FFS.GetContent);
end;

procedure TFSTests.Test04_DeleteFile;
begin
  // Create test file
  FFS.SetPath(FTestFile);
  FFS.SetContent('Test Content');
  FFS.WriteFile;
  
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
  
  // Create source file
  FFS.SetPath(FTestFile);
  FFS.SetContent('Test Content');
  FFS.WriteFile;
  
  // Create target directory
  CreateDir(CopyDir);
  
  // Test copy
  FFS.CopyTo(CopyDir + PathDelim + 'test.txt');
  AssertTrue('File should exist after copy',
    FileExists(CopyDir + PathDelim + 'test.txt'));
end;

procedure TFSTests.Test06_MoveTo;
var
  MoveDir: string;
  SourceFile: string;
  DestFile: string;
begin
  MoveDir := FTestDir + PathDelim + 'move_dir';
  SourceFile := FTestDir + PathDelim + 'test.txt';
  DestFile := MoveDir + PathDelim + 'test.txt';
  
  // Create source file
  FFS.SetPath(SourceFile);
  FFS.SetContent('Test');
  FFS.WriteFile;
  
  // Create target directory
  CreateDir(MoveDir);
  
  // Test move
  FFS.MoveTo(DestFile);
  Sleep(100); // Give the file system time to update
  AssertTrue('File should exist after move',
    FileExists(DestFile));
end;

procedure TFSTests.Test07_SetContent;
const
  TestContent = 'Hello, World!';
begin
  FFS.SetPath(FTestFile);
  FFS.SetContent(TestContent);
  FFS.WriteFile;
  
  AssertTrue('File should exist after set content',
    FileExists(FTestFile));
end;

procedure TFSTests.Test08_AppendText;
begin
  // Create initial file
  FFS.SetPath(FTestFile);
  FFS.SetContent('First Line');
  FFS.WriteFile;
  
  // Test append text
  FFS.AppendText('Second Line');
  FFS.WriteFile;
  
  // Verify content
  FFS.ReadFile;
  AssertEquals('AppendText should append content correctly',
    'First LineSecond Line', FFS.GetContent);
end;

procedure TFSTests.Test09_PrependText;
begin
  // Create initial file
  FFS.SetPath(FTestFile);
  FFS.SetContent('Second Line');
  FFS.WriteFile;
  
  // Test prepend text
  FFS.PrependText('First Line');
  FFS.WriteFile;
  
  // Verify content
  FFS.ReadFile;
  AssertEquals('PrependText should prepend content correctly',
    'First LineSecond Line', FFS.GetContent);
end;

procedure TFSTests.Test10_ReplaceText;
begin
  // Create initial file
  FFS.SetPath(FTestFile);
  FFS.SetContent('Hello, World!');
  FFS.WriteFile;
  
  // Test replace text
  FFS.ReplaceText('Hello', 'Hi');
  FFS.WriteFile;
  
  // Verify content
  FFS.ReadFile;
  AssertEquals('ReplaceText should replace content correctly',
    'Hi, World!', FFS.GetContent);
end;

procedure TFSTests.Test11_CreateDirectory;
var
  NewDir: string;
begin
  NewDir := FTestDir + PathDelim + 'new_dir';
  FFS.SetPath(NewDir);
  FFS.CreateDirectory;
  
  AssertTrue('Directory should exist after creation',
    DirectoryExists(NewDir));
end;

procedure TFSTests.Test12_DeleteDirectory;
var
  TestDir: string;
  TestSubDir: string;
  TestFile: string;
begin
  TestDir := FTestDir + PathDelim + 'test_delete_dir';
  TestSubDir := TestDir + PathDelim + 'subdir';
  TestFile := TestSubDir + PathDelim + 'test.txt';
  
  // Create test directory structure
  CreateDir(TestDir);
  CreateDir(TestSubDir);
  
  FFS.SetPath(TestFile);
  FFS.SetContent('Test');
  FFS.WriteFile;
  
  // Test delete directory
  FFS.SetPath(TestDir);
  FFS.DeleteDirectory(True);
  
  AssertFalse('Directory should not exist after deletion',
    DirectoryExists(TestDir));
end;

procedure TFSTests.Test13_EnsureDirectory;
var
  DeepDir: string;
begin
  DeepDir := FTestDir + PathDelim + 'deep' + PathDelim + 'nested' + PathDelim + 'dir';
  FFS.SetPath(DeepDir + PathDelim + 'test.txt');
  FFS.EnsureDirectory;
  
  AssertTrue('Directory structure should exist after ensure',
    DirectoryExists(DeepDir));
end;

procedure TFSTests.Test14_DirectoryExists;
begin
  FFS.SetPath(FTestDir);
  AssertTrue('DirectoryExists should return true for existing directory',
    FFS.DirectoryExists);
end;

procedure TFSTests.Test15_ChangeExtension;
begin
  FFS.SetPath(FTestFile);
  FFS.ChangeExtension('.dat');
  AssertEquals('ChangeExtension should change the extension correctly',
    '.dat', FFS.GetExtension);
end;

procedure TFSTests.Test16_GetFileName;
begin
  FFS.SetPath(FTestFile);
  AssertEquals('GetFileName should return the correct filename',
    'test.txt', FFS.GetFileName);
end;

procedure TFSTests.Test17_GetDirectory;
begin
  FFS.SetPath(FTestFile);
  AssertEquals('GetDirectory should return the correct directory',
    'test_dir', FFS.GetDirectory);
end;

procedure TFSTests.Test18_GetExtension;
begin
  FFS.SetPath(FTestFile);
  AssertEquals('GetExtension should return the correct extension',
    '.txt', FFS.GetExtension);
end;

procedure TFSTests.Test19_GetFullPath;
begin
  FFS.SetPath(FTestFile);
  AssertEquals('GetFullPath should return the correct full path',
    FTestDir + PathDelim + 'test.txt', FFS.GetFullPath);
end;

procedure TFSTests.Test20_SearchFiles;
var
  Files: array[1..3] of string;
  Results: TSearchResults;
  i: Integer;
begin
  // Create test files
  for i := 1 to 3 do
  begin
    Files[i] := FTestDir + PathDelim + 'test' + IntToStr(i) + '.txt';
    FFS.SetPath(Files[i]);
    FFS.SetContent('Test ' + IntToStr(i));
    FFS.WriteFile;
  end;
  
  // Test search
  FFS.SetPath(FTestDir);
  Results := FFS.SearchFiles('*.txt');
  AssertEquals('SearchFiles should find all test files',
    3, Length(Results));
end;

procedure TFSTests.Test21_SearchFilesIn;
var
  SubDir: string;
  Files: array[1..3] of string;
  Results: TSearchResults;
  i: Integer;
begin
  SubDir := FTestDir + PathDelim + 'search_dir';
  CreateDir(SubDir);
  
  // Create test files
  for i := 1 to 3 do
  begin
    Files[i] := SubDir + PathDelim + 'test' + IntToStr(i) + '.txt';
    FFS.SetPath(Files[i]);
    FFS.SetContent('Test ' + IntToStr(i));
    FFS.WriteFile;
  end;
  
  // Test search
  Results := FFS.SearchFilesIn(SubDir, '*.txt', False);
  AssertEquals('SearchFilesIn should find all test files',
    3, Length(Results));
end;

procedure TFSTests.Test22_FindNewestFile;
var
  Files: array[1..3] of string;
  NewestFile: string;
  i: Integer;
begin
  // Create test files
  for i := 1 to 3 do
  begin
    Files[i] := FTestDir + PathDelim + 'test' + IntToStr(i) + '.txt';
    FFS.SetPath(Files[i]);
    FFS.SetContent('Test ' + IntToStr(i));
    FFS.WriteFile;
    Sleep(100); // Ensure different timestamps
  end;
  
  // Test find newest file
  FFS.SetPath(FTestDir);
  NewestFile := FFS.FindNewestFile('*.txt');
  AssertEquals('Should find the newest test file',
    'test3.txt', ExtractFileName(NewestFile));
end;

procedure TFSTests.Test23_FindOldestFile;
var
  Files: array[1..3] of string;
  OldestFile: string;
  i: Integer;
begin
  // Create test files
  for i := 1 to 3 do
  begin
    Files[i] := FTestDir + PathDelim + 'test' + IntToStr(i) + '.txt';
    FFS.SetPath(Files[i]);
    FFS.SetContent('Test ' + IntToStr(i));
    FFS.WriteFile;
    Sleep(100); // Ensure different timestamps
  end;
  
  // Test find oldest file
  FFS.SetPath(FTestDir);
  OldestFile := FFS.FindOldestFile('*.txt');
  AssertEquals('Should find the oldest test file',
    'test1.txt', ExtractFileName(OldestFile));
end;

procedure TFSTests.Test24_FindLargestFile;
var
  Files: array[1..3] of string;
  LargestFile: string;
  i: Integer;
begin
  // Create test files
  for i := 1 to 3 do
  begin
    Files[i] := FTestDir + PathDelim + 'test' + IntToStr(i) + '.txt';
    FFS.SetPath(Files[i]);
    FFS.SetContent(StringOfChar('A', i * 100)); // Different sizes
    FFS.WriteFile;
  end;
  
  // Test find largest file
  FFS.SetPath(FTestDir);
  LargestFile := FFS.FindLargestFile('*.txt');
  AssertEquals('Should find the largest test file',
    'test3.txt', ExtractFileName(LargestFile));
end;

procedure TFSTests.Test25_FindSmallestFile;
var
  Files: array[1..3] of string;
  SmallestFile: string;
  i: Integer;
begin
  // Create test files
  for i := 1 to 3 do
  begin
    Files[i] := FTestDir + PathDelim + 'test' + IntToStr(i) + '.txt';
    FFS.SetPath(Files[i]);
    FFS.SetContent(StringOfChar('A', (4-i) * 100)); // Different sizes
    FFS.WriteFile;
  end;
  
  // Test find smallest file
  FFS.SetPath(FTestDir);
  SmallestFile := FFS.FindSmallestFile('*.txt');
  AssertEquals('Should find the smallest test file',
    'test3.txt', ExtractFileName(SmallestFile));
end;

procedure TFSTests.Test26_Exists;
begin
  FFS.SetPath(FTestFile);
  FFS.SetContent('Test');
  FFS.WriteFile;
  
  AssertTrue('File should exist',
    FFS.Exists);
end;

procedure TFSTests.Test27_Size;
begin
  FFS.SetPath(FTestFile);
  FFS.SetContent('Test');
  FFS.WriteFile;
  
  AssertTrue('Size should be greater than 0',
    FFS.GetSize > 0);
end;

procedure TFSTests.Test28_CreationTime;
begin
  FFS.SetPath(FTestFile);
  FFS.SetContent('Test');
  FFS.WriteFile;
  
  AssertTrue('Creation time should be recent',
    Abs(Now - FFS.GetCreationTime) < 1);
end;

procedure TFSTests.Test29_LastWriteTime;
begin
  FFS.SetPath(FTestFile);
  FFS.SetContent('Test');
  FFS.WriteFile;
  
  AssertTrue('Last write time should be recent',
    Abs(Now - FFS.GetLastWriteTime) < 1);
end;

procedure TFSTests.Test30_LastAccessTime;
begin
  FFS.SetPath(FTestFile);
  FFS.SetContent('Test');
  FFS.WriteFile;
  
  AssertTrue('Last access time should be recent',
    Abs(Now - FFS.GetLastAccessTime) < 1);
end;

procedure TFSTests.Test31_Attributes;
begin
  FFS.SetPath(FTestFile);
  FFS.SetContent('Test');
  FFS.WriteFile;
  
  AssertTrue('File should exist',
    FFS.Exists);
  AssertTrue('Size should be greater than 0',
    FFS.GetSize > 0);
  AssertTrue('Creation time should be recent',
    Abs(Now - FFS.GetCreationTime) < 1);
end;

{ TStringTests }

procedure TStringTests.SetUp;
begin
  // No setup needed for static functions
end;

procedure TStringTests.TearDown;
begin
  // No teardown needed for static functions
end;

procedure TStringTests.Test01_From;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('String value should remain unchanged',
    TestStr, TestStr);
end;

procedure TStringTests.Test02_ToString;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('String value should remain unchanged',
    TestStr, TestStr);
end;

procedure TStringTests.Test03_Trim;
const
  TestStr = '  Hello, World!  ';
begin
  AssertEquals('Trim should remove surrounding whitespace',
    'Hello, World!', TStringKit.Trim(TestStr));
end;

procedure TStringTests.Test04_TrimLeft;
const
  TestStr = '  Hello, World!  ';
begin
  AssertEquals('TrimLeft should remove leading whitespace',
    'Hello, World!  ', TStringKit.TrimLeft(TestStr));
end;

procedure TStringTests.Test05_TrimRight;
const
  TestStr = '  Hello, World!  ';
begin
  AssertEquals('TrimRight should remove trailing whitespace',
    '  Hello, World!', TStringKit.TrimRight(TestStr));
end;

procedure TStringTests.Test06_ToUpper;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('ToUpper should work correctly',
    'HELLO, WORLD!', TStringKit.ToUpper(TestStr));
end;

procedure TStringTests.Test07_ToLower;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('ToLower should work correctly',
    'hello, world!', TStringKit.ToLower(TestStr));
end;

procedure TStringTests.Test08_Capitalize;
const
  TestStr = 'hello, world!';
begin
  AssertEquals('CapitalizeText should work correctly',
    'Hello, World!', TStringKit.CapitalizeText(TestStr));
end;

procedure TStringTests.Test09_Reverse;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('ReverseText should work correctly',
    '!dlroW ,olleH', TStringKit.ReverseText(TestStr));
end;

procedure TStringTests.Test10_Duplicate;
const
  TestStr = 'Hello';
begin
  AssertEquals('DuplicateText should work correctly',
    'HelloHello', TStringKit.DuplicateText(TestStr, 2));
end;

procedure TStringTests.Test11_PadLeft;
const
  TestStr = 'test';
begin
  AssertEquals('PadLeft should work correctly',
    '****test', TStringKit.PadLeft(TestStr, 8, '*'));
end;

procedure TStringTests.Test12_PadRight;
const
  TestStr = 'test';
begin
  AssertEquals('PadRight should work correctly',
    'test****', TStringKit.PadRight(TestStr, 8, '*'));
end;

procedure TStringTests.Test13_PadCenter;
const
  TestStr = 'test';
begin
  AssertEquals('PadCenter should work correctly',
    '**test**', TStringKit.PadCenter(TestStr, 8, '*'));
end;

procedure TStringTests.Test14_RemoveWhitespace;
const
  TestStr = '  too   many    spaces  ';
begin
  AssertEquals('RemoveWhitespace should work correctly',
    'toomanyspaces', TStringKit.RemoveWhitespace(TestStr));
end;

procedure TStringTests.Test15_CollapseWhitespace;
const
  TestStr = '  too   many    spaces  ';
begin
  AssertEquals('CollapseWhitespace should work correctly',
    'too many spaces', TStringKit.Trim(TStringKit.CollapseWhitespace(TestStr)));
end;

procedure TStringTests.Test16_Replace;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('ReplaceText should work correctly',
    'Hi, World!', TStringKit.ReplaceText(TestStr, 'Hello', 'Hi'));
end;

procedure TStringTests.Test17_ReplaceRegEx;
const
  TestStr = 'The year is 2024';
begin
  AssertEquals('ReplaceRegEx should work correctly',
    'The year is 2024', TStringKit.ReplaceRegEx(TestStr, '\d+', '2024'));
end;

procedure TStringTests.Test18_Extract;
const
  TestStr = 'The year is 2024';
var
  Matches: TStringMatches;
begin
  Matches := TStringKit.ExtractMatches(TestStr, '\d+');
  AssertEquals('ExtractMatches should work correctly',
    '2024', Matches[0].Text);
end;

procedure TStringTests.Test19_ExtractAll;
const
  TestStr = 'The year is 2024';
var
  Results: TStringArray;
begin
  Results := TStringKit.ExtractAllMatches(TestStr, '\d+');
  AssertEquals('ExtractAllMatches should work correctly',
    '2024', Results[0]);
end;

procedure TStringTests.Test20_Matches;
const
  TestStr = 'The year is 2024';
begin
  AssertTrue('MatchesPattern should work correctly',
    TStringKit.MatchesPattern(TestStr, '\d+'));
end;

procedure TStringTests.Test21_SubString;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('SubString should work correctly',
    'Hello', TStringKit.SubString(TestStr, 1, 5));
end;

procedure TStringTests.Test22_Left;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('LeftStr should work correctly',
    'Hello', TStringKit.LeftStr(TestStr, 5));
end;

procedure TStringTests.Test23_Right;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('RightStr should work correctly',
    'World!', TStringKit.RightStr(TestStr, 6));
end;

procedure TStringTests.Test24_Words;
const
  TestStr = 'Hello, World!';
var
  WordArray: TStringArray;
begin
  WordArray := TStringKit.GetWords(TestStr);
  AssertEquals('GetWords should work correctly',
    'Hello', WordArray[0]);
  AssertEquals('GetWords should work correctly',
    'World', WordArray[1]);
end;

procedure TStringTests.Test25_Contains;
const
  TestStr = 'Hello, World!';
begin
  AssertTrue('Contains should work correctly',
    TStringKit.Contains(TestStr, 'World'));
end;

procedure TStringTests.Test26_StartsWith;
const
  TestStr = 'Hello, World!';
begin
  AssertTrue('StartsWith should work correctly',
    TStringKit.StartsWith(TestStr, 'Hello'));
end;

procedure TStringTests.Test27_EndsWith;
const
  TestStr = 'Hello, World!';
begin
  AssertTrue('EndsWith should work correctly',
    TStringKit.EndsWith(TestStr, 'World!'));
end;

procedure TStringTests.Test28_IsEmpty;
begin
  AssertTrue('IsEmpty should work correctly',
    TStringKit.IsEmpty(''));
end;

procedure TStringTests.Test29_Length;
const
  TestStr = 'Hello, World!';
begin
  AssertEquals('GetLength should work correctly',
    13, TStringKit.GetLength(TestStr));
end;

procedure TStringTests.Test30_CountSubString;
const
  TestStr = 'Hello, Hello, Hello!';
begin
  AssertEquals('CountSubString should work correctly',
    3, TStringKit.CountSubString(TestStr, 'Hello'));
end;

initialization
  RegisterTest(TDateTimeTests);
  RegisterTest(TFSTests);
  RegisterTest(TStringTests);
end.
