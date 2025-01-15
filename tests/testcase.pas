unit testcase;

{$mode objfpc}{$H+}{$J-}

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
    procedure Test24_IsTextFile;
    procedure Test25_GetFileEncoding;
    // Search operations
    procedure Test26_SearchFiles;
    procedure Test27_FindLastModifiedFile;
    procedure Test27b_FindLastModifiedFileRecursive;
    procedure Test28_FindFirstModifiedFile;
    procedure Test28b_FindFirstModifiedFileRecursive;
    procedure Test29_FindLargestFile;
    procedure Test29b_FindLargestFileRecursive;
    procedure Test30_FindSmallestFile;
    procedure Test30b_FindSmallestFileRecursive;
    // Directory information
    procedure Test31_GetUserDir;
    procedure Test32_GetCurrentDir;
    procedure Test33_GetTempDir;
    procedure Test34_GetParentDir;
    // Path manipulation
    procedure Test35_CombinePaths;
    procedure Test36_IsAbsolutePath;
    procedure Test37_NormalizePath;
    // File system operations
    procedure Test38_CreateTempFile;
    procedure Test39_CreateTempDirectory;
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

procedure TFSTests.Test24_IsTextFile;
var
  TextFile, BinaryFile: string;
  BinStream: TFileStream;
begin
  TextFile := FTestDir + PathDelim + 'test.txt';
  BinaryFile := FTestDir + PathDelim + 'test.bin';
  
  // Create a text file
  TFileKit.WriteFile(TextFile, 'This is a text file');
  
  // Create a binary file
  BinStream := TFileStream.Create(BinaryFile, fmCreate);
  try
    BinStream.Write(#0#1#2#3#4#5, 6);
  finally
    BinStream.Free;
  end;
  
  try
    AssertTrue('Text file should be detected as text',
      TFileKit.IsTextFile(TextFile));
    AssertFalse('Binary file should not be detected as text',
      TFileKit.IsTextFile(BinaryFile));
  finally
    if FileExists(TextFile) then
      DeleteFile(TextFile);
    if FileExists(BinaryFile) then
      DeleteFile(BinaryFile);
  end;
end;

procedure TFSTests.Test25_GetFileEncoding;
var
  UTF8File: string;
  UTF8Stream: TFileStream;
  UTF8BOM: array[0..2] of Byte;
  UTF8Text: AnsiString;
begin
  UTF8File := FTestDir + PathDelim + 'utf8.txt';
  
  // Create UTF-8 file with BOM
  UTF8Stream := TFileStream.Create(UTF8File, fmCreate);
  try
    // Write UTF-8 BOM
    UTF8BOM[0] := $EF;
    UTF8BOM[1] := $BB;
    UTF8BOM[2] := $BF;
    UTF8Stream.WriteBuffer(UTF8BOM, 3);
    
    // Write some UTF-8 text
    UTF8Text := 'Hello, UTF-8 World!';
    UTF8Stream.WriteBuffer(UTF8Text[1], Length(UTF8Text));
  finally
    UTF8Stream.Free;
  end;
  
  try
    AssertEquals('UTF-8 file should be detected correctly',
      'UTF-8', TFileKit.GetFileEncoding(UTF8File));
  finally
    if FileExists(UTF8File) then
      DeleteFile(UTF8File);
  end;
end;

procedure TFSTests.Test26_SearchFiles;
var
  Results: TSearchResults;
  SR: TSearchRec;
  SubDir: string;
begin
  // Clean up any existing txt files
  if FindFirst(FTestDir + PathDelim + '*.txt', faAnyFile, SR) = 0 then
  try
    repeat
      DeleteFile(FTestDir + PathDelim + SR.Name);
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;

  // Create test files in root directory
  TFileKit.WriteFile(FTestDir + PathDelim + 'test1.txt', 'Content 1');
  TFileKit.WriteFile(FTestDir + PathDelim + 'test2.txt', 'Content 2');

  // Create subdirectory with more test files
  SubDir := FTestDir + PathDelim + 'subdir';
  TFileKit.CreateDirectory(SubDir);
  TFileKit.WriteFile(SubDir + PathDelim + 'test3.txt', 'Content 3');
  TFileKit.WriteFile(SubDir + PathDelim + 'test4.txt', 'Content 4');

  // Test non-recursive search (default)
  Results := TFileKit.SearchFiles(FTestDir, '*.txt');
  try
    AssertEquals('Non-recursive SearchFiles should only find files in root directory',
      2, Length(Results));
  finally
    SetLength(Results, 0);
  end;

  // Test recursive search
  Results := TFileKit.SearchFiles(FTestDir, '*.txt', True);
  try
    AssertEquals('Recursive SearchFiles should find all files',
      4, Length(Results));
  finally
    SetLength(Results, 0);
  end;
end;

procedure TFSTests.Test27_FindLastModifiedFile;
var
  NewestFile: string;
  SR: TSearchRec;
  SubDir: string;
begin
  WriteLn('Test27_FindLastModifiedFile: Start');

  // Clean up any existing txt files
  if FindFirst(FTestDir + PathDelim + '*.txt', faAnyFile, SR) = 0 then
  try
    repeat
      DeleteFile(FTestDir + PathDelim + SR.Name);
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;

  // Create test files with delay to ensure different timestamps
  TFileKit.WriteFile(FTestDir + PathDelim + 'test1.txt', 'Content 1');
  Sleep(1000);
  
  // Create subdirectory with newer file
  SubDir := FTestDir + PathDelim + 'subdir';
  TFileKit.CreateDirectory(SubDir);
  Sleep(1000);
  TFileKit.WriteFile(SubDir + PathDelim + 'test2.txt', 'Content 2');
  Sleep(1000);
  TFileKit.WriteFile(FTestDir + PathDelim + 'test3.txt', 'Content 3');
  Sleep(1000);
  
  // Test non-recursive search
  NewestFile := TFileKit.FindLastModifiedFile(FTestDir, '*.txt', False);
  AssertEquals('Non-recursive FindLastModifiedFile should find newest file in root directory',
    'test3.txt', NewestFile);

  WriteLn('Test27_FindLastModifiedFile: End');
end;

procedure TFSTests.Test27b_FindLastModifiedFileRecursive;
var
  NewestFile: string;
  SR: TSearchRec;
  SubDir: string;
begin
  WriteLn('Test27b_FindLastModifiedFileRecursive: Start');

  // Clean up any existing txt files
  if FindFirst(FTestDir + PathDelim + '*.txt', faAnyFile, SR) = 0 then
  try
    repeat
      DeleteFile(FTestDir + PathDelim + SR.Name);
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;

  // Create test files with delay to ensure different timestamps
  TFileKit.WriteFile(FTestDir + PathDelim + 'test1.txt', 'Content 1');
  Sleep(1000);
  
  // Create subdirectory with newer file
  SubDir := FTestDir + PathDelim + 'subdir';
  TFileKit.CreateDirectory(SubDir);
  Sleep(1000);
  TFileKit.WriteFile(SubDir + PathDelim + 'test2.txt', 'Content 2');
  Sleep(1000);
  TFileKit.WriteFile(FTestDir + PathDelim + 'test3.txt', 'Content 3');
  Sleep(1000);
  
  // Test recursive search
  NewestFile := TFileKit.FindLastModifiedFile(FTestDir, '*.txt', True);
  AssertEquals('Recursive FindLastModifiedFile should find newest file in any directory',
    'test3.txt', NewestFile);

  WriteLn('Test27b_FindLastModifiedFileRecursive: End');
end;

procedure TFSTests.Test28_FindFirstModifiedFile;
var
  OldestFile: string;
  SR: TSearchRec;
  SubDir: string;
begin
  WriteLn('Test28_FindFirstModifiedFile: Start');
  // Clean up any existing txt files
  if FindFirst(FTestDir + PathDelim + '*.txt', faAnyFile, SR) = 0 then
  try
    repeat
      DeleteFile(FTestDir + PathDelim + SR.Name);
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;

  // Create test files with delay to ensure different timestamps
  TFileKit.WriteFile(FTestDir + PathDelim + 'test1.txt', 'Content 1');
  FileSetDate(FTestDir + PathDelim + 'test1.txt', DateTimeToFileDate(EncodeDateTime(2025, 1, 14, 21, 48, 32, 0)));
  
  Sleep(2000);
  
  // Create subdirectory with newer files
  SubDir := FTestDir + PathDelim + 'subdir';
  TFileKit.CreateDirectory(SubDir);
  TFileKit.WriteFile(SubDir + PathDelim + 'test2.txt', 'Content 2');
  FileSetDate(SubDir + PathDelim + 'test2.txt', DateTimeToFileDate(EncodeDateTime(2025, 1, 14, 21, 48, 36, 0)));
  
  Sleep(2000);
  
  TFileKit.WriteFile(FTestDir + PathDelim + 'test3.txt', 'Content 3');
  FileSetDate(FTestDir + PathDelim + 'test3.txt', DateTimeToFileDate(EncodeDateTime(2025, 1, 14, 21, 48, 38, 0)));
  
  Sleep(2000);
  
  // Test non-recursive search
  WriteLn('Test28_FindFirstModifiedFile: Non-recursive search');
  OldestFile := TFileKit.FindFirstModifiedFile(FTestDir, '*.txt', False);
  AssertEquals('Non-recursive FindFirstModifiedFile should find oldest file in root directory',
    'test1.txt', OldestFile);

  WriteLn('Test28_FindFirstModifiedFile: End');
end;

procedure TFSTests.Test28b_FindFirstModifiedFileRecursive;
var
  OldestFile: string;
  SR: TSearchRec;
  SubDir: string;
begin
  WriteLn('Test28b_FindFirstModifiedFileRecursive: Start');
  // Clean up any existing txt files
  if FindFirst(FTestDir + PathDelim + '*.txt', faAnyFile, SR) = 0 then
  try
    repeat
      DeleteFile(FTestDir + PathDelim + SR.Name);
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;

  // Create test files with delay to ensure different timestamps
  TFileKit.WriteFile(FTestDir + PathDelim + 'test1.txt', 'Content 1');
  FileSetDate(FTestDir + PathDelim + 'test1.txt', DateTimeToFileDate(EncodeDateTime(2025, 1, 14, 21, 48, 32, 0)));
  
  Sleep(2000);
  
  // Create subdirectory with newer files
  SubDir := FTestDir + PathDelim + 'subdir';
  TFileKit.CreateDirectory(SubDir);
  TFileKit.WriteFile(SubDir + PathDelim + 'test2.txt', 'Content 2');
  FileSetDate(SubDir + PathDelim + 'test2.txt', DateTimeToFileDate(EncodeDateTime(2025, 1, 14, 21, 48, 36, 0)));
  
  Sleep(2000);
  
  TFileKit.WriteFile(FTestDir + PathDelim + 'test3.txt', 'Content 3');
  FileSetDate(FTestDir + PathDelim + 'test3.txt', DateTimeToFileDate(EncodeDateTime(2025, 1, 14, 21, 48, 38, 0)));
  
  Sleep(2000);
  
  // Test recursive search
  WriteLn('Test28b_FindFirstModifiedFileRecursive: Recursive search');
  OldestFile := TFileKit.FindFirstModifiedFile(FTestDir, '*.txt', True);
  AssertEquals('Recursive FindFirstModifiedFile should find oldest file in any directory',
    'test1.txt', OldestFile);

  WriteLn('Test28b_FindFirstModifiedFileRecursive: End');
end;

procedure TFSTests.Test29_FindLargestFile;
var
  LargestFile: string;
  SR: TSearchRec;
  SubDir: string;
begin
  WriteLn('Test29_FindLargestFile: Start');
  // Clean up any existing txt files
  if FindFirst(FTestDir + PathDelim + '*.txt', faAnyFile, SR) = 0 then
  try
    repeat
      DeleteFile(FTestDir + PathDelim + SR.Name);
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;

  // Create test files with different sizes
  TFileKit.WriteFile(FTestDir + PathDelim + 'test1.txt', StringOfChar('A', 100));
  TFileKit.WriteFile(FTestDir + PathDelim + 'test3.txt', StringOfChar('C', 300));
  
  // Test non-recursive search
  WriteLn('Test29_FindLargestFile: Non-recursive search');
  LargestFile := TFileKit.FindLargestFile(FTestDir, '*.txt', False);
  AssertEquals('Non-recursive FindLargestFile should find largest file in root directory',
    'test3.txt', LargestFile);

  WriteLn('Test29_FindLargestFile: End');
end;

procedure TFSTests.Test29b_FindLargestFileRecursive;
var
  LargestFile: string;
  SR: TSearchRec;
  SubDir: string;
begin
  WriteLn('Test29b_FindLargestFileRecursive: Start');
  // Clean up any existing txt files
  if FindFirst(FTestDir + PathDelim + '*.txt', faAnyFile, SR) = 0 then
  try
    repeat
      DeleteFile(FTestDir + PathDelim + SR.Name);
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;

  // Create test files with different sizes
  TFileKit.WriteFile(FTestDir + PathDelim + 'test1.txt', StringOfChar('A', 100));
  
  // Create subdirectory with larger file
  SubDir := FTestDir + PathDelim + 'subdir';
  TFileKit.CreateDirectory(SubDir);
  TFileKit.WriteFile(SubDir + PathDelim + 'test2.txt', StringOfChar('B', 500));
  
  TFileKit.WriteFile(FTestDir + PathDelim + 'test3.txt', StringOfChar('C', 300));
  
  // Test recursive search
  WriteLn('Test29b_FindLargestFileRecursive: Recursive search');
  LargestFile := TFileKit.FindLargestFile(FTestDir, '*.txt', True);
  AssertEquals('Recursive FindLargestFile should find largest file in any directory',
    'test2.txt', LargestFile);

  WriteLn('Test29b_FindLargestFileRecursive: End');
end;

procedure TFSTests.Test30_FindSmallestFile;
var
  SmallestFile: string;
  SR: TSearchRec;
  SubDir: string;
begin
  WriteLn('Test30_FindSmallestFile: Start');
  // Clean up any existing txt files
  if FindFirst(FTestDir + PathDelim + '*.txt', faAnyFile, SR) = 0 then
  try
    repeat
      DeleteFile(FTestDir + PathDelim + SR.Name);
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;

  // Create test files with different sizes
  TFileKit.WriteFile(FTestDir + PathDelim + 'test1.txt', StringOfChar('A', 100));
  TFileKit.WriteFile(FTestDir + PathDelim + 'test3.txt', StringOfChar('C', 50));
  
  // Test non-recursive search
  WriteLn('Test30_FindSmallestFile: Non-recursive search');
  SmallestFile := TFileKit.FindSmallestFile(FTestDir, '*.txt', False);
  AssertEquals('Non-recursive FindSmallestFile should find smallest file in root directory',
    'test3.txt', SmallestFile);

  WriteLn('Test30_FindSmallestFile: End');
end;

procedure TFSTests.Test30b_FindSmallestFileRecursive;
var
  SmallestFile: string;
  SR: TSearchRec;
  SubDir: string;
begin
  WriteLn('Test30b_FindSmallestFileRecursive: Start');
  // Clean up any existing txt files
  if FindFirst(FTestDir + PathDelim + '*.txt', faAnyFile, SR) = 0 then
  try
    repeat
      DeleteFile(FTestDir + PathDelim + SR.Name);
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;

  // Create test files with different sizes
  TFileKit.WriteFile(FTestDir + PathDelim + 'test1.txt', StringOfChar('A', 100));
  
  // Create subdirectory with smaller file
  SubDir := FTestDir + PathDelim + 'subdir';
  TFileKit.CreateDirectory(SubDir);
  TFileKit.WriteFile(SubDir + PathDelim + 'test2.txt', StringOfChar('B', 25));
  
  TFileKit.WriteFile(FTestDir + PathDelim + 'test3.txt', StringOfChar('C', 50));
  
  // Test recursive search
  WriteLn('Test30b_FindSmallestFileRecursive: Recursive search');
  SmallestFile := TFileKit.FindSmallestFile(FTestDir, '*.txt', True);
  AssertEquals('Recursive FindSmallestFile should find smallest file in any directory',
    'test3.txt', SmallestFile);

  WriteLn('Test30b_FindSmallestFileRecursive: End');
end;

procedure TFSTests.Test31_GetUserDir;
var
  UserDir: string;
begin
  UserDir := TFileKit.GetUserDir;
  AssertTrue('GetUserDir should return non-empty string', UserDir <> '');
  AssertTrue('GetUserDir should return existing directory', DirectoryExists(UserDir));
end;

procedure TFSTests.Test32_GetCurrentDir;
var
  CurDir: string;
begin
  CurDir := TFileKit.GetCurrentDir;
  AssertTrue('GetCurrentDir should return non-empty string', CurDir <> '');
  AssertTrue('GetCurrentDir should return existing directory', DirectoryExists(CurDir));
  AssertEquals('GetCurrentDir should match system current directory', 
    ExcludeTrailingPathDelimiter(GetCurrentDir), 
    ExcludeTrailingPathDelimiter(CurDir));
end;

procedure TFSTests.Test33_GetTempDir;
var
  TempDir: string;
begin
  TempDir := TFileKit.GetTempDir;
  AssertTrue('GetTempDir should return non-empty string', TempDir <> '');
  AssertTrue('GetTempDir should return existing directory', DirectoryExists(TempDir));
end;

procedure TFSTests.Test34_GetParentDir;
begin
  AssertEquals('GetParentDir should return correct parent directory',
    TFileKit.NormalizePath(ExcludeTrailingPathDelimiter(FTestDir)),
    TFileKit.NormalizePath(ExcludeTrailingPathDelimiter(TFileKit.GetParentDir(FTestFile))));
end;

procedure TFSTests.Test35_CombinePaths;
begin
  AssertEquals('CombinePaths should combine paths correctly',
    TFileKit.NormalizePath(IncludeTrailingPathDelimiter(FTestDir) + 'test.txt'),
    TFileKit.NormalizePath(TFileKit.CombinePaths(FTestDir, 'test.txt')));
    
  AssertEquals('CombinePaths should handle empty first path',
    'test.txt',
    TFileKit.CombinePaths('', 'test.txt'));
    
  AssertEquals('CombinePaths should handle empty second path',
    TFileKit.NormalizePath(ExcludeTrailingPathDelimiter(FTestDir)),
    TFileKit.NormalizePath(ExcludeTrailingPathDelimiter(TFileKit.CombinePaths(FTestDir, ''))));
end;

procedure TFSTests.Test36_IsAbsolutePath;
begin
  {$IFDEF WINDOWS}
  AssertTrue('Windows drive path should be absolute',
    TFileKit.IsAbsolutePath('C:\Windows'));
  AssertFalse('Relative Windows path should not be absolute',
    TFileKit.IsAbsolutePath('Windows\System32'));
  {$ENDIF}
  
  {$IFDEF UNIX}
  AssertTrue('Unix root path should be absolute',
    TFileKit.IsAbsolutePath('/usr/local'));
  AssertFalse('Relative Unix path should not be absolute',
    TFileKit.IsAbsolutePath('usr/local'));
  {$ENDIF}
end;

procedure TFSTests.Test37_NormalizePath;
var
  TestPath: string;
begin
  {$IFDEF WINDOWS}
  TestPath := 'C:/Windows/System32';
  AssertEquals('NormalizePath should convert forward slashes to backslashes on Windows',
    'C:\Windows\System32',
    TFileKit.NormalizePath(TestPath));
  {$ENDIF}
  
  {$IFDEF UNIX}
  TestPath := '/usr\local\bin';
  AssertEquals('NormalizePath should convert backslashes to forward slashes on Unix',
    '/usr/local/bin',
    TFileKit.NormalizePath(TestPath));
  {$ENDIF}
end;

procedure TFSTests.Test38_CreateTempFile;
var
  TempFile: string;
begin
  TempFile := TFileKit.CreateTempFile('test');
  try
    AssertTrue('CreateTempFile should create file', FileExists(TempFile));
    AssertEquals('CreateTempFile should create file with prefix',
      'test_', Copy(ExtractFileName(TempFile), 1, 5));
  finally
    if FileExists(TempFile) then
      DeleteFile(TempFile);
  end;
end;

procedure TFSTests.Test39_CreateTempDirectory;
var
  TempDir: string;
begin
  TempDir := TFileKit.CreateTempDirectory('test');
  try
    AssertTrue('CreateTempDirectory should create directory',
      DirectoryExists(TempDir));
    AssertEquals('CreateTempDirectory should create directory with prefix',
      'test_', Copy(ExtractFileName(TempDir), 1, 5));
  finally
    if DirectoryExists(TempDir) then
      RemoveDir(TempDir);
  end;
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
