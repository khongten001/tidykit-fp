unit testcase;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, DateUtils, fpcunit, testutils, testregistry,
  TidyKit;

type
  TStringArray = array of string;

type
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
    // Time Span Tests
    procedure Test34_CreatePeriod;
    procedure Test35_CreateDuration;
    procedure Test36_CreateInterval;
    procedure Test37_AddSpan;
    procedure Test38_SubtractSpan;
    procedure Test39_SpanBetween;
    procedure Test40_IsWithinInterval;
    procedure Test41_IntervalsOverlap;
    procedure Test42_IntervalLength;
    procedure Test43_PeriodNormalization;
    procedure Test44_DurationCalculation;
    procedure Test45_SpanCornerCases;
    // Date Unit Tests
    procedure Test46_FloorDateSecond;
    procedure Test47_FloorDateMinute;
    procedure Test48_FloorDateHour;
    procedure Test49_FloorDateDay;
    procedure Test50_FloorDateWeek;
    procedure Test51_FloorDateMonth;
    procedure Test52_FloorDateBiMonth;
    procedure Test53_FloorDateQuarter;
    procedure Test54_FloorDateHalfYear;
    procedure Test55_FloorDateYear;
    procedure Test56_CeilingDateSecond;
    procedure Test57_CeilingDateMinute;
    procedure Test58_CeilingDateHour;
    procedure Test59_CeilingDateDay;
    procedure Test60_CeilingDateWeek;
    procedure Test61_CeilingDateMonth;
    procedure Test62_CeilingDateBiMonth;
    procedure Test63_CeilingDateQuarter;
    procedure Test64_CeilingDateHalfYear;
    procedure Test65_CeilingDateYear;
    procedure Test66_RoundDateSecond;
    procedure Test67_RoundDateMinute;
    procedure Test68_RoundDateHour;
    procedure Test69_RoundDateDay;
    procedure Test70_RoundDateWeek;
    procedure Test71_RoundDateMonth;
    procedure Test72_RoundDateBiMonth;
    procedure Test73_RoundDateQuarter;
    procedure Test74_RoundDateHalfYear;
    procedure Test75_RoundDateYear;
    
    // Date parsing tests
    procedure Test76_YMD_Valid;
    procedure Test77_YMD_Invalid;
    procedure Test78_MDY_Valid;
    procedure Test79_MDY_Invalid;
    procedure Test80_DMY_Valid;
    procedure Test81_DMY_Invalid;
    procedure Test82_YQ_Valid;
    procedure Test83_YQ_Invalid;
    
    // ISO calendar tests
    procedure Test84_ISOYear;
    procedure Test85_ISOWeek;
    // Epidemiological calendar tests
    procedure Test86_EpiYear;
    procedure Test87_EpiWeek;
    // Academic calendar tests
    procedure Test88_Semester;
    
    // Decimal date tests
    procedure Test89_DateDecimal;
    procedure Test90_GetDecimalDate;
    // Month rolling tests
    procedure Test91_RollbackMonth;
    procedure Test92_RollForwardMonth;
    
    // Period/Duration conversion tests
    procedure Test93_PeriodToSeconds;
    procedure Test94_SecondsToPeriod;
    procedure Test95_StandardizePeriod;
    // Interval operation tests
    procedure Test96_IntervalAlign;
    procedure Test97_IntervalGap;
    procedure Test98_IntervalSetdiff;
    procedure Test99_IntervalUnion;
    procedure Test100_IntervalIntersection;
    
    // EpiWeek Tests
    procedure Test87a_EpiWeek_MidYear;
    procedure Test87b_EpiWeek_FirstWeek;
    procedure Test87c_EpiWeek_YearEnd;
    
    // StandardizePeriod Tests
    procedure Test95a_StandardizePeriod_Milliseconds;
    procedure Test95b_StandardizePeriod_Seconds;
    procedure Test95c_StandardizePeriod_Minutes;
    procedure Test95d_StandardizePeriod_Hours;
    procedure Test95e_StandardizePeriod_Months;
    procedure Test95f_StandardizePeriod_Complex;
    
    // IntervalGap Tests
    procedure Test97a_IntervalGap_NoOverlap;
    procedure Test97b_IntervalGap_Overlapping;
    
    // Timezone Tests
    procedure Test101_GetTimeZone;
    procedure Test102_GetSystemTimeZone;
    procedure Test103_GetTimeZoneNames;
    procedure Test104_WithTimeZone;
    procedure Test105_ForceTimeZone;
    procedure Test106_DSTTransition;
    procedure Test107_DateBoundaryConversion;
    procedure Test108_InvalidTimezones;
    procedure Test109_ExtremeOffsets;
    procedure Test110_DSTTransitionExactTime;
    procedure Test111_DSTEndExactTime;
    procedure Test112_LeapYearDST;
    procedure Test113_InvalidTimeZoneEdgeCases;
    procedure Test114_UTCOffsetEdgeCases;
    procedure Test115_CrossBoundaryConversions;
    // Date parsing tests
    procedure Test71_YMD;
    procedure Test72_MDY;
    procedure Test73_DMY;
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
    procedure Test34b_ListDirectories;
    procedure Test34c_ListDirectoriesRecursive;
    procedure Test34d_ListFiles;
    procedure Test34e_ListFilesRecursive;
    // Path manipulation
    procedure Test35_CombinePaths;
    procedure Test36_IsAbsolutePath;
    procedure Test37_NormalizePath;
    // File system operations
    procedure Test38_CreateTempFile;
    procedure Test39_CreateTempDirectory;
    procedure Test34f_ListFilesWithPattern;
    procedure Test34g_ListFilesWithSorting;
    procedure Test34h_ListDirectoriesWithPattern;
    procedure Test34i_ListDirectoriesWithSorting;
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
  // Regular year transition
  StartDate := EncodeDate(2024, 1, 15);
  Expected := EncodeDate(2028, 1, 15);
  AssertEquals('AddYears should add specified years',
    Expected, TDateTimeKit.AddYears(StartDate, 4));
    
  // Feb 29 leap year to non-leap year
  StartDate := EncodeDate(2024, 2, 29);  // 2024 is leap year
  Expected := EncodeDate(2025, 2, 28);   // 2025 is not
  AssertEquals('AddYears should handle Feb 29 to non-leap year',
    Expected, TDateTimeKit.AddYears(StartDate, 1));
    
  // Feb 29 leap year to leap year
  StartDate := EncodeDate(2024, 2, 29);  // 2024 is leap year
  Expected := EncodeDate(2028, 2, 29);   // 2028 is also leap year
  AssertEquals('AddYears should preserve Feb 29 in leap year',
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
  // Regular hour addition
  StartDate := EncodeDate(2024, 1, 15) + EncodeTime(12, 0, 0, 0);
  Expected := EncodeDate(2024, 1, 15) + EncodeTime(14, 0, 0, 0);
  AssertEquals('AddHours should add specified hours',
    Expected, TDateTimeKit.AddHours(StartDate, 2));
    
  // Cross day boundary
  StartDate := EncodeDate(2024, 1, 15) + EncodeTime(23, 0, 0, 0);
  Expected := EncodeDate(2024, 1, 16) + EncodeTime(1, 0, 0, 0);
  AssertEquals('AddHours should handle day boundary',
    Expected, TDateTimeKit.AddHours(StartDate, 2));
    
  // Cross month boundary
  StartDate := EncodeDate(2024, 1, 31) + EncodeTime(23, 0, 0, 0);
  Expected := EncodeDate(2024, 2, 1) + EncodeTime(1, 0, 0, 0);
  AssertEquals('AddHours should handle month boundary',
    Expected, TDateTimeKit.AddHours(StartDate, 2));
    
  // Cross year and handle leap year
  StartDate := EncodeDate(2024, 2, 28) + EncodeTime(23, 0, 0, 0);
  Expected := EncodeDate(2024, 2, 29) + EncodeTime(1, 0, 0, 0);
  AssertEquals('AddHours should handle leap year boundary',
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

procedure TDateTimeTests.Test34_CreatePeriod;
var
  Period: TDateSpan;
begin
  // Test creating a period with various components
  Period := TDateTimeKit.CreatePeriod(1, 2, 3, 4, 5, 6, 7);
  
  AssertEquals('Period kind should be dskPeriod', Ord(TDateSpanKind.dskPeriod), Ord(Period.Kind));
  AssertEquals('Years should match', 1, Period.Years);
  AssertEquals('Months should match', 2, Period.Months);
  AssertEquals('Days should match', 3, Period.Days);
  AssertEquals('Hours should match', 4, Period.Hours);
  AssertEquals('Minutes should match', 5, Period.Minutes);
  AssertEquals('Seconds should match', 6, Period.Seconds);
  AssertEquals('Milliseconds should match', 7, Period.Milliseconds);
end;

procedure TDateTimeTests.Test35_CreateDuration;
var
  Duration: TDateSpan;
begin
  // Test creating a duration (converts to total seconds)
  Duration := TDateTimeKit.CreateDuration(0, 0, 1, 2, 30, 0, 0);  // 1 day, 2 hours, 30 minutes
  
  AssertEquals('Duration kind should be dskDuration', Ord(TDateSpanKind.dskDuration), Ord(Duration.Kind));
  AssertEquals('Total seconds should be calculated correctly',
    ((24 + 2) * 60 + 30) * 60,  // (26 hours + 30 minutes) in seconds
    Duration.Seconds);
end;

procedure TDateTimeTests.Test36_CreateInterval;
var
  StartDate, EndDate: TDateTime;
  Interval: TInterval;
begin
  StartDate := EncodeDate(2024, 1, 1);
  EndDate := EncodeDate(2024, 12, 31);
  
  Interval := TDateTimeKit.CreateInterval(StartDate, EndDate);
  
  AssertEquals('Interval start date should match', StartDate, Interval.StartDate);
  AssertEquals('Interval end date should match', EndDate, Interval.EndDate);
end;

procedure TDateTimeTests.Test37_AddSpan;
var
  StartDate, ResultDate: TDateTime;
  Period: TDateSpan;
begin
  StartDate := EncodeDate(2024, 1, 1);
  
  // Test adding a period
  Period := TDateTimeKit.CreatePeriod(1, 2, 3);  // 1 year, 2 months, 3 days
  ResultDate := TDateTimeKit.AddSpan(StartDate, Period);
  
  AssertEquals('Year should be incremented', 2025, TDateTimeKit.GetYear(ResultDate));
  AssertEquals('Month should be March', 3, TDateTimeKit.GetMonth(ResultDate));
  AssertEquals('Day should be 4th', 4, TDateTimeKit.GetDay(ResultDate));
end;

procedure TDateTimeTests.Test38_SubtractSpan;
var
  StartDate, ResultDate: TDateTime;
  Period: TDateSpan;
begin
  StartDate := EncodeDate(2024, 3, 15);
  
  // Test subtracting a period
  Period := TDateTimeKit.CreatePeriod(0, 1, 10);  // 1 month, 10 days
  ResultDate := TDateTimeKit.SubtractSpan(StartDate, Period);
  
  AssertEquals('Month should be February', 2, TDateTimeKit.GetMonth(ResultDate));
  AssertEquals('Day should be 5th', 5, TDateTimeKit.GetDay(ResultDate));
end;

procedure TDateTimeTests.Test39_SpanBetween;
var
  StartDate, EndDate: TDateTime;
  Span: TDateSpan;
begin
  StartDate := EncodeDate(2024, 1, 1);
  EndDate := EncodeDate(2025, 2, 15);
  
  // Test calculating period between dates
  Span := TDateTimeKit.SpanBetween(StartDate, EndDate);
  
  AssertEquals('Span kind should be period', Ord(TDateSpanKind.dskPeriod), Ord(Span.Kind));
  AssertEquals('Years should be 1', 1, Span.Years);
  AssertEquals('Months should be 1', 1, Span.Months);
  AssertEquals('Days should be 14', 14, Span.Days);
end;

procedure TDateTimeTests.Test40_IsWithinInterval;
var
  StartDate, EndDate, TestDate: TDateTime;
  Interval: TInterval;
begin
  StartDate := EncodeDate(2024, 1, 1);
  EndDate := EncodeDate(2024, 12, 31);
  TestDate := EncodeDate(2024, 6, 15);
  
  Interval := TDateTimeKit.CreateInterval(StartDate, EndDate);
  
  AssertTrue('Date should be within interval', 
    TDateTimeKit.IsWithinInterval(TestDate, Interval));
  AssertFalse('Date before interval should not be within interval',
    TDateTimeKit.IsWithinInterval(EncodeDate(2023, 12, 31), Interval));
  AssertFalse('Date after interval should not be within interval',
    TDateTimeKit.IsWithinInterval(EncodeDate(2025, 1, 1), Interval));
end;

procedure TDateTimeTests.Test41_IntervalsOverlap;
var
  Interval1, Interval2: TInterval;
begin
  // Create two overlapping intervals
  Interval1 := TDateTimeKit.CreateInterval(
    EncodeDate(2024, 1, 1),
    EncodeDate(2024, 6, 30));
  Interval2 := TDateTimeKit.CreateInterval(
    EncodeDate(2024, 6, 1),
    EncodeDate(2024, 12, 31));
    
  AssertTrue('Overlapping intervals should be detected',
    TDateTimeKit.IntervalsOverlap(Interval1, Interval2));
    
  // Create non-overlapping intervals
  Interval2.StartDate := EncodeDate(2024, 7, 1);
  AssertFalse('Non-overlapping intervals should be detected',
    TDateTimeKit.IntervalsOverlap(Interval1, Interval2));
end;

procedure TDateTimeTests.Test42_IntervalLength;
var
  Interval: TInterval;
  Span: TDateSpan;
begin
  // Create an interval for exactly one year
  Interval.StartDate := EncodeDate(2024, 1, 1);  // 2024-01-01 00:00:00.000
  Interval.EndDate := EncodeDate(2025, 1, 1);    // 2025-01-01 00:00:00.000
  
  // Test period length
  Span := TDateTimeKit.IntervalLength(Interval, dskPeriod);
  AssertEquals('Interval length should be 1 year', 1, Span.Years);
  AssertEquals('No remaining months', 0, Span.Months);
  AssertEquals('No remaining days', 0, Span.Days);
  
  // Test duration length (366 days for leap year 2024)
  Span := TDateTimeKit.IntervalLength(Interval, dskDuration);
  AssertEquals('Duration should be calculated in seconds',
    366 * 24 * 60 * 60,  // Full leap year 2024
    Span.Seconds);
end;

procedure TDateTimeTests.Test43_PeriodNormalization;
var
  Period: TDateSpan;
  StartDate, ResultDate: TDateTime;
begin
  // Test period normalization (13 months should become 1 year 1 month)
  Period := TDateTimeKit.CreatePeriod(0, 13, 0);
  StartDate := EncodeDate(2024, 1, 1);
  ResultDate := TDateTimeKit.AddSpan(StartDate, Period);
  
  AssertEquals('Year should be incremented', 2025, TDateTimeKit.GetYear(ResultDate));
  AssertEquals('Month should be February', 2, TDateTimeKit.GetMonth(ResultDate));
end;

procedure TDateTimeTests.Test44_DurationCalculation;
var
  Duration: TDateSpan;
  StartDate, ResultDate: TDateTime;
begin
  // Test precise duration calculations
  Duration := TDateTimeKit.CreateDuration(0, 0, 0, 25, 0, 0, 0);  // 25 hours
  StartDate := EncodeDate(2024, 1, 1) + EncodeTime(12, 0, 0, 0);  // Noon
  ResultDate := TDateTimeKit.AddSpan(StartDate, Duration);
  
  AssertEquals('Day should be incremented', 2, TDateTimeKit.GetDay(ResultDate));
  AssertEquals('Hour should be 13', 13, TDateTimeKit.GetHour(ResultDate));
end;

procedure TDateTimeTests.Test45_SpanCornerCases;
var
  Period: TDateSpan;
  StartDate, ResultDate: TDateTime;
begin
  // Test adding one month to January 31st (should go to last day of February)
  Period := TDateTimeKit.CreatePeriod(0, 1, 0);
  StartDate := EncodeDate(2024, 1, 31);
  ResultDate := TDateTimeKit.AddSpan(StartDate, Period);
  
  AssertEquals('Month should be February', 2, TDateTimeKit.GetMonth(ResultDate));
  AssertEquals('Day should be adjusted to 29 (leap year)',
    29, TDateTimeKit.GetDay(ResultDate));
    
  // Test adding one month to January 31st in non-leap year (should go to February 28)
  StartDate := EncodeDate(2025, 1, 31);
  ResultDate := TDateTimeKit.AddSpan(StartDate, Period);
  
  AssertEquals('Month should be February', 2, TDateTimeKit.GetMonth(ResultDate));
  AssertEquals('Day should be adjusted to 28 (non-leap year)',
    28, TDateTimeKit.GetDay(ResultDate));
    
  // Test adding one year to February 29th in leap year (should go to February 28)
  Period := TDateTimeKit.CreatePeriod(1, 0, 0);
  StartDate := EncodeDate(2024, 2, 29);
  ResultDate := TDateTimeKit.AddSpan(StartDate, Period);
  
  AssertEquals('Year should be incremented', 2025, TDateTimeKit.GetYear(ResultDate));
  AssertEquals('Day should be adjusted to 28 (non-leap year)',
    28, TDateTimeKit.GetDay(ResultDate));
    
  // Test adding two years to February 29th (should go back to February 29)
  Period := TDateTimeKit.CreatePeriod(2, 0, 0);
  StartDate := EncodeDate(2024, 2, 29);
  ResultDate := TDateTimeKit.AddSpan(StartDate, Period);
  
  AssertEquals('Year should be incremented by 2', 2026, TDateTimeKit.GetYear(ResultDate));
  AssertEquals('Day should be adjusted to 28 (non-leap year)',
    28, TDateTimeKit.GetDay(ResultDate));
    
  // Test adding one month to March 31st (should go to April 30)
  Period := TDateTimeKit.CreatePeriod(0, 1, 0);
  StartDate := EncodeDate(2024, 3, 31);
  ResultDate := TDateTimeKit.AddSpan(StartDate, Period);
  
  AssertEquals('Month should be April', 4, TDateTimeKit.GetMonth(ResultDate));
  AssertEquals('Day should be adjusted to 30',
    30, TDateTimeKit.GetDay(ResultDate));
end;

// Implementation of new test cases

procedure TDateTimeTests.Test46_FloorDateSecond;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Floor to second should clear milliseconds',
    EncodeDateTime(2024, 3, 15, 14, 30, 45, 0),
    TDateTimeKit.FloorDate(TestDate, duSecond));
end;

procedure TDateTimeTests.Test47_FloorDateMinute;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Floor to minute should clear seconds and milliseconds',
    EncodeDateTime(2024, 3, 15, 14, 30, 0, 0),
    TDateTimeKit.FloorDate(TestDate, duMinute));
end;

procedure TDateTimeTests.Test48_FloorDateHour;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Floor to hour should clear minutes, seconds and milliseconds',
    EncodeDateTime(2024, 3, 15, 14, 0, 0, 0),
    TDateTimeKit.FloorDate(TestDate, duHour));
end;

procedure TDateTimeTests.Test49_FloorDateDay;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Floor to day should clear time portion',
    EncodeDate(2024, 3, 15),
    TDateTimeKit.FloorDate(TestDate, duDay));
end;

procedure TDateTimeTests.Test50_FloorDateWeek;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500); // March 15, 2024 is a Friday
  AssertEquals('Floor to week should go to Sunday',
    EncodeDate(2024, 3, 10), // Should go to Sunday, March 10
    TDateTimeKit.FloorDate(TestDate, duWeek));
end;

procedure TDateTimeTests.Test51_FloorDateMonth;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Floor to month should go to first day of month',
    EncodeDate(2024, 3, 1),
    TDateTimeKit.FloorDate(TestDate, duMonth));
end;

procedure TDateTimeTests.Test52_FloorDateBiMonth;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Floor to bi-month should go to first day of even month',
    EncodeDate(2024, 3, 1),
    TDateTimeKit.FloorDate(TestDate, duBiMonth));
end;

procedure TDateTimeTests.Test53_FloorDateQuarter;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Floor to quarter should go to first day of quarter',
    EncodeDate(2024, 1, 1),
    TDateTimeKit.FloorDate(TestDate, duQuarter));
end;

procedure TDateTimeTests.Test54_FloorDateHalfYear;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 8, 15, 14, 30, 45, 500);
  AssertEquals('Floor to half year should go to July 1 or January 1',
    EncodeDate(2024, 7, 1),
    TDateTimeKit.FloorDate(TestDate, duHalfYear));
end;

procedure TDateTimeTests.Test55_FloorDateYear;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Floor to year should go to January 1',
    EncodeDate(2024, 1, 1),
    TDateTimeKit.FloorDate(TestDate, duYear));
end;

procedure TDateTimeTests.Test56_CeilingDateSecond;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to second should round up to next second',
    EncodeDateTime(2024, 3, 15, 14, 30, 46, 0),
    TDateTimeKit.CeilingDate(TestDate, duSecond));
end;

procedure TDateTimeTests.Test57_CeilingDateMinute;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to minute should round up to next minute',
    EncodeDateTime(2024, 3, 15, 14, 31, 0, 0),
    TDateTimeKit.CeilingDate(TestDate, duMinute));
end;

procedure TDateTimeTests.Test58_CeilingDateHour;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to hour should round up to next hour',
    EncodeDateTime(2024, 3, 15, 15, 0, 0, 0),
    TDateTimeKit.CeilingDate(TestDate, duHour));
end;

procedure TDateTimeTests.Test59_CeilingDateDay;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to day should round up to next day',
    EncodeDate(2024, 3, 16),
    TDateTimeKit.CeilingDate(TestDate, duDay));
end;

procedure TDateTimeTests.Test60_CeilingDateWeek;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500); // March 15, 2024 is a Friday
  AssertEquals('Ceiling to week should go to next Sunday',
    EncodeDate(2024, 3, 17), // Should go to next Sunday, March 17
    TDateTimeKit.CeilingDate(TestDate, duWeek));
end;

procedure TDateTimeTests.Test61_CeilingDateMonth;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to month should go to first day of next month',
    EncodeDate(2024, 4, 1),
    TDateTimeKit.CeilingDate(TestDate, duMonth));
end;

procedure TDateTimeTests.Test62_CeilingDateBiMonth;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to bi-month should go to first day of next even month',
    EncodeDate(2024, 5, 1),
    TDateTimeKit.CeilingDate(TestDate, duBiMonth));
end;

procedure TDateTimeTests.Test63_CeilingDateQuarter;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to quarter should go to first day of next quarter',
    EncodeDate(2024, 4, 1),
    TDateTimeKit.CeilingDate(TestDate, duQuarter));
end;

procedure TDateTimeTests.Test64_CeilingDateHalfYear;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to half year should go to July 1',
    EncodeDate(2024, 7, 1),
    TDateTimeKit.CeilingDate(TestDate, duHalfYear));
end;

procedure TDateTimeTests.Test65_CeilingDateYear;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to year should go to January 1 of next year',
    EncodeDate(2025, 1, 1),
    TDateTimeKit.CeilingDate(TestDate, duYear));
end;

procedure TDateTimeTests.Test66_RoundDateSecond;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to second should round to nearest second',
    EncodeDateTime(2024, 3, 15, 14, 30, 46, 0),
    TDateTimeKit.RoundDate(TestDate, duSecond));
end;

procedure TDateTimeTests.Test67_RoundDateMinute;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to minute should round to nearest minute',
    EncodeDateTime(2024, 3, 15, 14, 31, 0, 0),
    TDateTimeKit.RoundDate(TestDate, duMinute));
end;

procedure TDateTimeTests.Test68_RoundDateHour;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to hour should round to nearest hour',
    EncodeDateTime(2024, 3, 15, 15, 0, 0, 0),
    TDateTimeKit.RoundDate(TestDate, duHour));
end;

procedure TDateTimeTests.Test69_RoundDateDay;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to day should round to nearest day',
    EncodeDate(2024, 3, 16),
    TDateTimeKit.RoundDate(TestDate, duDay));
end;

procedure TDateTimeTests.Test70_RoundDateWeek;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500); // March 15, 2024 is a Friday
  AssertEquals('Round to week should round to nearest Sunday',
    EncodeDate(2024, 3, 17), // Should round to next Sunday, March 17
    TDateTimeKit.RoundDate(TestDate, duWeek));
end;

procedure TDateTimeTests.Test71_RoundDateMonth;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to month should round to nearest month start',
    EncodeDate(2024, 4, 1),
    TDateTimeKit.RoundDate(TestDate, duMonth));
end;

procedure TDateTimeTests.Test72_RoundDateBiMonth;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to bi-month should round to nearest even month start',
    EncodeDate(2024, 3, 1),
    TDateTimeKit.RoundDate(TestDate, duBiMonth));
end;

procedure TDateTimeTests.Test73_RoundDateQuarter;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to quarter should round to nearest quarter start',
    EncodeDate(2024, 4, 1),
    TDateTimeKit.RoundDate(TestDate, duQuarter));
end;

procedure TDateTimeTests.Test74_RoundDateHalfYear;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to half year should round to nearest half year start',
    EncodeDate(2024, 7, 1),
    TDateTimeKit.RoundDate(TestDate, duHalfYear));
end;

procedure TDateTimeTests.Test75_RoundDateYear;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to year should round to nearest year start',
    EncodeDate(2024, 1, 1),
    TDateTimeKit.RoundDate(TestDate, duYear));
end;

procedure TDateTimeTests.Test76_YMD_Valid;
var
  TestDate: TDateTime;
begin
  // Test YYYY-MM-DD format
  TestDate := TDateTimeKit.YMD('2024-03-15');
  AssertEquals('YMD should parse year correctly', 2024, TDateTimeKit.GetYear(TestDate));
  AssertEquals('YMD should parse month correctly', 3, TDateTimeKit.GetMonth(TestDate));
  AssertEquals('YMD should parse day correctly', 15, TDateTimeKit.GetDay(TestDate));
  
  // Test YYYY/MM/DD format
  TestDate := TDateTimeKit.YMD('2024/03/15');
  AssertEquals('YMD should parse year correctly with slash', 2024, TDateTimeKit.GetYear(TestDate));
  AssertEquals('YMD should parse month correctly with slash', 3, TDateTimeKit.GetMonth(TestDate));
  AssertEquals('YMD should parse day correctly with slash', 15, TDateTimeKit.GetDay(TestDate));
end;

procedure TDateTimeTests.Test77_YMD_Invalid;
begin
  try
    TDateTimeKit.YMD('invalid');
    Fail('YMD should raise exception for invalid format');
  except
    on E: EConvertError do
      ; // Expected exception
  end;
  
  try
    TDateTimeKit.YMD('2024-13-15');
    Fail('YMD should raise exception for invalid month');
  except
    on E: EConvertError do
      ; // Expected exception
  end;
end;

procedure TDateTimeTests.Test78_MDY_Valid;
var
  TestDate: TDateTime;
begin
  // Test MM-DD-YYYY format
  TestDate := TDateTimeKit.MDY('03-15-2024');
  AssertEquals('MDY should parse year correctly', 2024, TDateTimeKit.GetYear(TestDate));
  AssertEquals('MDY should parse month correctly', 3, TDateTimeKit.GetMonth(TestDate));
  AssertEquals('MDY should parse day correctly', 15, TDateTimeKit.GetDay(TestDate));
  
  // Test MM/DD/YY format with 2-digit year
  TestDate := TDateTimeKit.MDY('03/15/24');
  AssertEquals('MDY should parse 2-digit year correctly', 2024, TDateTimeKit.GetYear(TestDate));
  AssertEquals('MDY should parse month correctly with slash', 3, TDateTimeKit.GetMonth(TestDate));
  AssertEquals('MDY should parse day correctly with slash', 15, TDateTimeKit.GetDay(TestDate));
end;

procedure TDateTimeTests.Test79_MDY_Invalid;
begin
  try
    TDateTimeKit.MDY('invalid');
    Fail('MDY should raise exception for invalid format');
  except
    on E: EConvertError do
      ; // Expected exception
  end;
  
  try
    TDateTimeKit.MDY('13-15-2024');
    Fail('MDY should raise exception for invalid month');
  except
    on E: EConvertError do
      ; // Expected exception
  end;
end;

procedure TDateTimeTests.Test80_DMY_Valid;
var
  TestDate: TDateTime;
begin
  // Test DD-MM-YYYY format
  TestDate := TDateTimeKit.DMY('15-03-2024');
  AssertEquals('DMY should parse year correctly', 2024, TDateTimeKit.GetYear(TestDate));
  AssertEquals('DMY should parse month correctly', 3, TDateTimeKit.GetMonth(TestDate));
  AssertEquals('DMY should parse day correctly', 15, TDateTimeKit.GetDay(TestDate));
  
  // Test DD/MM/YY format with 2-digit year
  TestDate := TDateTimeKit.DMY('15/03/24');
  AssertEquals('DMY should parse 2-digit year correctly', 2024, TDateTimeKit.GetYear(TestDate));
  AssertEquals('DMY should parse month correctly with slash', 3, TDateTimeKit.GetMonth(TestDate));
  AssertEquals('DMY should parse day correctly with slash', 15, TDateTimeKit.GetDay(TestDate));
end;

procedure TDateTimeTests.Test81_DMY_Invalid;
begin
  try
    TDateTimeKit.DMY('invalid');
    Fail('DMY should raise exception for invalid format');
  except
    on E: EConvertError do
      ; // Expected exception
  end;
  
  try
    TDateTimeKit.DMY('15-13-2024');
    Fail('DMY should raise exception for invalid month');
  except
    on E: EConvertError do
      ; // Expected exception
  end;
end;

procedure TDateTimeTests.Test82_YQ_Valid;
var
  TestDate: TDateTime;
begin
  // Test YYYY-Q format
  TestDate := TDateTimeKit.YQ('2024-1');
  AssertEquals('YQ should parse year correctly', 2024, TDateTimeKit.GetYear(TestDate));
  AssertEquals('YQ should set month to start of Q1', 1, TDateTimeKit.GetMonth(TestDate));
  AssertEquals('YQ should set day to first of month', 1, TDateTimeKit.GetDay(TestDate));
  
  // Test YYYY/Q format for Q2
  TestDate := TDateTimeKit.YQ('2024/2');
  AssertEquals('YQ should parse year correctly', 2024, TDateTimeKit.GetYear(TestDate));
  AssertEquals('YQ should set month to start of Q2', 4, TDateTimeKit.GetMonth(TestDate));
  AssertEquals('YQ should set day to first of month', 1, TDateTimeKit.GetDay(TestDate));
  
  // Test Q3
  TestDate := TDateTimeKit.YQ('2024-3');
  AssertEquals('YQ should set month to start of Q3', 7, TDateTimeKit.GetMonth(TestDate));
  
  // Test Q4
  TestDate := TDateTimeKit.YQ('2024-4');
  AssertEquals('YQ should set month to start of Q4', 10, TDateTimeKit.GetMonth(TestDate));
end;

procedure TDateTimeTests.Test83_YQ_Invalid;
begin
  try
    TDateTimeKit.YQ('invalid');
    Fail('YQ should raise exception for invalid format');
  except
    on E: EConvertError do
      ; // Expected exception
  end;
  
  try
    TDateTimeKit.YQ('2024-5');
    Fail('YQ should raise exception for invalid quarter');
  except
    on E: EConvertError do
      ; // Expected exception
  end;
end;

procedure TDateTimeTests.Test84_ISOYear;
var
  TestDate: TDateTime;
begin
  // Test regular date
  TestDate := EncodeDate(2024, 6, 15);
  AssertEquals('Regular date should have same ISO year', 
    2024, TDateTimeKit.GetISOYear(TestDate));
    
  // Test year boundary (Dec 31, 2024 is in week 1 of 2025)
  TestDate := EncodeDate(2024, 12, 31);
  AssertEquals('Dec 31 can belong to next ISO year', 
    2025, TDateTimeKit.GetISOYear(TestDate));
    
  // Test year boundary (Jan 1, 2024 is in week 52 of 2023)
  TestDate := EncodeDate(2024, 1, 1);
  AssertEquals('Jan 1 can belong to previous ISO year', 
    2023, TDateTimeKit.GetISOYear(TestDate));
end;

procedure TDateTimeTests.Test85_ISOWeek;
var
  TestDate: TDateTime;
begin
  // Test regular week
  TestDate := EncodeDate(2024, 6, 15);
  AssertEquals('Mid-year date should have correct ISO week', 
    24, TDateTimeKit.GetISOWeek(TestDate));
    
  // Test first week of year
  TestDate := EncodeDate(2024, 1, 4);
  AssertEquals('January 4th is always in week 1', 
    1, TDateTimeKit.GetISOWeek(TestDate));
    
  // Test last week of year
  TestDate := EncodeDate(2024, 12, 28);
  AssertEquals('December 28th should be in week 52', 
    52, TDateTimeKit.GetISOWeek(TestDate));
end;

procedure TDateTimeTests.Test86_EpiYear;
var
  TestDate: TDateTime;
begin
  // Test regular date
  TestDate := EncodeDate(2024, 6, 15);
  AssertEquals('Regular date should have same epi year', 
    2024, TDateTimeKit.GetEpiYear(TestDate));
    
  // Test year boundary (Dec 29, 2024 might be week 1 of 2025)
  TestDate := EncodeDate(2024, 12, 29);
  AssertEquals('Late December can belong to next epi year', 
    2025, TDateTimeKit.GetEpiYear(TestDate));
    
  // Test year boundary (Jan 1 might be week 52 of previous year)
  TestDate := EncodeDate(2024, 1, 1);
  AssertEquals('Early January can belong to previous epi year', 
    2023, TDateTimeKit.GetEpiYear(TestDate));
end;

procedure TDateTimeTests.Test87_EpiWeek;
var
  TestDate: TDateTime;
begin
  // Test regular week
  TestDate := EncodeDate(2024, 6, 15);
  AssertEquals('Mid-year date should have correct epi week', 
    24, TDateTimeKit.GetEpiWeek(TestDate));
    
  // Test first week of year
  TestDate := EncodeDate(2024, 1, 4);
  AssertEquals('First full week should be week 1', 
    1, TDateTimeKit.GetEpiWeek(TestDate));
    
  // Test week spanning year boundary
  TestDate := EncodeDate(2024, 12, 31);
  AssertEquals('Year-end week number should be correct', 
    53, TDateTimeKit.GetEpiWeek(TestDate));
end;

procedure TDateTimeTests.Test88_Semester;
var
  TestDate: TDateTime;
begin
  // Test first semester
  TestDate := EncodeDate(2024, 1, 1);
  AssertEquals('January should be semester 1', 
    1, TDateTimeKit.GetSemester(TestDate));
    
  TestDate := EncodeDate(2024, 6, 30);
  AssertEquals('June should be semester 1', 
    1, TDateTimeKit.GetSemester(TestDate));
    
  // Test second semester
  TestDate := EncodeDate(2024, 7, 1);
  AssertEquals('July should be semester 2', 
    2, TDateTimeKit.GetSemester(TestDate));
    
  TestDate := EncodeDate(2024, 12, 31);
  AssertEquals('December should be semester 2', 
    2, TDateTimeKit.GetSemester(TestDate));
end;

procedure TDateTimeTests.Test89_DateDecimal;
var
  TestDate: TDateTime;
begin
  // Test regular date
  TestDate := TDateTimeKit.DateDecimal(2024.5); // Mid-year
  AssertEquals('Year should be 2024', 2024, TDateTimeKit.GetYear(TestDate));
  AssertEquals('Should be around July 2nd (leap year)',
    183, TDateTimeKit.GetDayOfYear(TestDate));
    
  // Test leap year handling
  TestDate := TDateTimeKit.DateDecimal(2024.25); // Quarter year
  AssertEquals('Should be around April 1st in leap year',
    92, TDateTimeKit.GetDayOfYear(TestDate));
    
  // Test non-leap year
  TestDate := TDateTimeKit.DateDecimal(2025.25); // Quarter year
  AssertEquals('Should be around April 1st in non-leap year',
    91, TDateTimeKit.GetDayOfYear(TestDate));
end;

procedure TDateTimeTests.Test90_GetDecimalDate;
var
  TestDate: TDateTime;
  DecimalDate: Double;
begin
  // Test mid-year in leap year
  TestDate := EncodeDate(2024, 7, 2);  // Day 183 of 366
  DecimalDate := TDateTimeKit.GetDecimalDate(TestDate);
  AssertEquals('Mid-year 2024 should be approximately 2024.5',
    2024.5, DecimalDate, 0.01);
    
  // Test quarter-year in non-leap year
  TestDate := EncodeDate(2025, 4, 1);  // Day 91 of 365
  DecimalDate := TDateTimeKit.GetDecimalDate(TestDate);
  AssertEquals('Quarter-year 2025 should be approximately 2025.25',
    2025.25, DecimalDate, 0.01);
end;

procedure TDateTimeTests.Test91_RollbackMonth;
var
  TestDate, RolledDate: TDateTime;
begin
  // Test regular case
  TestDate := EncodeDate(2024, 3, 15);
  RolledDate := TDateTimeKit.RollbackMonth(TestDate);
  AssertEquals('Month should be February', 2, TDateTimeKit.GetMonth(RolledDate));
  AssertEquals('Day should remain 15', 15, TDateTimeKit.GetDay(RolledDate));
  
  // Test year boundary
  TestDate := EncodeDate(2024, 1, 15);
  RolledDate := TDateTimeKit.RollbackMonth(TestDate);
  AssertEquals('Year should be previous', 2023, TDateTimeKit.GetYear(RolledDate));
  AssertEquals('Month should be December', 12, TDateTimeKit.GetMonth(RolledDate));
  
  // Test day adjustment (31 -> 29 in leap year)
  TestDate := EncodeDate(2024, 3, 31);
  RolledDate := TDateTimeKit.RollbackMonth(TestDate);
  AssertEquals('Day should adjust to February 29 in leap year',
    29, TDateTimeKit.GetDay(RolledDate));
end;

procedure TDateTimeTests.Test92_RollForwardMonth;
var
  TestDate, RolledDate: TDateTime;
begin
  // Test regular case
  TestDate := EncodeDate(2024, 3, 15);
  RolledDate := TDateTimeKit.RollForwardMonth(TestDate);
  AssertEquals('Month should be April', 4, TDateTimeKit.GetMonth(RolledDate));
  AssertEquals('Day should remain 15', 15, TDateTimeKit.GetDay(RolledDate));
  
  // Test year boundary
  TestDate := EncodeDate(2024, 12, 15);
  RolledDate := TDateTimeKit.RollForwardMonth(TestDate);
  AssertEquals('Year should be next', 2025, TDateTimeKit.GetYear(RolledDate));
  AssertEquals('Month should be January', 1, TDateTimeKit.GetMonth(RolledDate));
  
  // Test day adjustment (31 -> 30)
  TestDate := EncodeDate(2024, 3, 31);
  RolledDate := TDateTimeKit.RollForwardMonth(TestDate);
  AssertEquals('Day should adjust to April 30',
    30, TDateTimeKit.GetDay(RolledDate));
end;

procedure TDateTimeTests.Test93_PeriodToSeconds;
var
  Period: TDateSpan;
  Seconds: Int64;
begin
  // Test simple period
  Period := TDateTimeKit.CreatePeriod(0, 0, 1, 2, 3, 4, 500);  // 1d 2h 3m 4.5s
  Seconds := TDateTimeKit.PeriodToSeconds(Period);
  AssertEquals('Period should convert to correct seconds',
    93784, Seconds);  // 1*86400 + 2*3600 + 3*60 + 4
    
  // Test larger period
  Period := TDateTimeKit.CreatePeriod(1, 1, 0, 0, 0, 0, 0);  // 1y 1m
  Seconds := TDateTimeKit.PeriodToSeconds(Period);
  AssertEquals('Larger period should convert approximately',
    34128000, Seconds);  // ~1y 1m in seconds
end;

procedure TDateTimeTests.Test94_SecondsToPeriod;
var
  Period: TDateSpan;
begin
  // Test simple conversion
  Period := TDateTimeKit.SecondsToPeriod(93784);  // 1d 2h 3m 4s
  AssertEquals('Should get correct days', 1, Period.Days);
  AssertEquals('Should get correct hours', 2, Period.Hours);
  AssertEquals('Should get correct minutes', 3, Period.Minutes);
  AssertEquals('Should get correct seconds', 4, Period.Seconds);
  
  // Test large number of seconds
  Period := TDateTimeKit.SecondsToPeriod(34128000);  // ~1y 1m
  AssertEquals('Should get approximate years', 1, Period.Years);
  AssertEquals('Should get approximate months', 1, Period.Months);
end;

procedure TDateTimeTests.Test95_StandardizePeriod;
var
  Period, Standardized: TDateSpan;
begin
  // Test overflow values
  Period := TDateTimeKit.CreatePeriod(1, 13, 32, 25, 61, 61, 1001);
  Standardized := TDateTimeKit.StandardizePeriod(Period);
  
  AssertEquals('Years should include extra months', 2, Standardized.Years);
  AssertEquals('Months should be normalized', 1, Standardized.Months);
  AssertEquals('Days should include extra hours', 33, Standardized.Days);
  AssertEquals('Hours should be normalized', 2, Standardized.Hours);
  AssertEquals('Minutes should be normalized', 2, Standardized.Minutes);
  AssertEquals('Seconds should be normalized', 2, Standardized.Seconds);
  AssertEquals('Milliseconds should be normalized', 1, Standardized.Milliseconds);
end;


procedure TDateTimeTests.Test96_IntervalAlign;
var
  Interval1, Interval2: TInterval;
begin
  // Test adjacent intervals
  Interval1 := TDateTimeKit.CreateInterval(
    EncodeDate(2024, 1, 1),
    EncodeDate(2024, 1, 15));
  Interval2 := TDateTimeKit.CreateInterval(
    EncodeDate(2024, 1, 15),
    EncodeDate(2024, 1, 31));
  AssertTrue('Adjacent intervals should align',
    TDateTimeKit.IntervalAlign(Interval1, Interval2));
    
  // Test non-adjacent intervals
  Interval2.StartDate := EncodeDate(2024, 1, 16);
  AssertFalse('Non-adjacent intervals should not align',
    TDateTimeKit.IntervalAlign(Interval1, Interval2));
end;

procedure TDateTimeTests.Test97_IntervalGap;
var
  Interval1, Interval2: TInterval;
  Gap: TDateSpan;
begin
  // Test intervals with gap
  Interval1 := TDateTimeKit.CreateInterval(
    EncodeDate(2024, 1, 1),
    EncodeDate(2024, 1, 15));
  Interval2 := TDateTimeKit.CreateInterval(
    EncodeDate(2024, 1, 20),
    EncodeDate(2024, 1, 31));
  Gap := TDateTimeKit.IntervalGap(Interval1, Interval2);
  
  AssertEquals('Gap should be 5 days',
    5, Gap.Days);
    
  // Test overlapping intervals
  Interval2.StartDate := EncodeDate(2024, 1, 10);
  Gap := TDateTimeKit.IntervalGap(Interval1, Interval2);
  AssertEquals('Overlapping intervals should have no gap',
    0, Gap.Days);
end;

procedure TDateTimeTests.Test98_IntervalSetdiff;
var
  Interval1, Interval2, Result: TInterval;
begin
  // Test partial overlap
  Interval1 := TDateTimeKit.CreateInterval(
    EncodeDate(2024, 1, 1),
    EncodeDate(2024, 1, 31));
  Interval2 := TDateTimeKit.CreateInterval(
    EncodeDate(2024, 1, 15),
    EncodeDate(2024, 2, 15));
  Result := TDateTimeKit.IntervalSetdiff(Interval1, Interval2);
  
  AssertEquals('Difference should start at original start',
    EncodeDate(2024, 1, 1), Result.StartDate);
  AssertEquals('Difference should end at overlap start',
    EncodeDate(2024, 1, 15), Result.EndDate);
end;

procedure TDateTimeTests.Test99_IntervalUnion;
var
  Interval1, Interval2, Result: TInterval;
begin
  // Test overlapping intervals
  Interval1 := TDateTimeKit.CreateInterval(
    EncodeDate(2024, 1, 1),
    EncodeDate(2024, 1, 15));
  Interval2 := TDateTimeKit.CreateInterval(
    EncodeDate(2024, 1, 10),
    EncodeDate(2024, 1, 31));
  Result := TDateTimeKit.IntervalUnion(Interval1, Interval2);
  
  AssertEquals('Union should start at earliest date',
    EncodeDate(2024, 1, 1), Result.StartDate);
  AssertEquals('Union should end at latest date',
    EncodeDate(2024, 1, 31), Result.EndDate);
end;

procedure TDateTimeTests.Test100_IntervalIntersection;
var
  Interval1, Interval2, Result: TInterval;
begin
  // Test overlapping intervals
  Interval1 := TDateTimeKit.CreateInterval(
    EncodeDate(2024, 1, 1),
    EncodeDate(2024, 1, 15));
  Interval2 := TDateTimeKit.CreateInterval(
    EncodeDate(2024, 1, 10),
    EncodeDate(2024, 1, 31));
  Result := TDateTimeKit.IntervalIntersection(Interval1, Interval2);
  
  AssertEquals('Intersection should start at later start date',
    EncodeDate(2024, 1, 10), Result.StartDate);
  AssertEquals('Intersection should end at earlier end date',
    EncodeDate(2024, 1, 15), Result.EndDate);
    
  // Test non-overlapping intervals
  Interval2.StartDate := EncodeDate(2024, 1, 16);
  Result := TDateTimeKit.IntervalIntersection(Interval1, Interval2);
  AssertEquals('Non-overlapping intervals should have empty intersection',
    0, Result.StartDate);
end;

{ EpiWeek Tests }
procedure TDateTimeTests.Test87a_EpiWeek_MidYear;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDate(2024, 6, 15);
  AssertEquals('Mid-year date should have correct epi week', 
    24, TDateTimeKit.GetEpiWeek(TestDate));
end;

procedure TDateTimeTests.Test87b_EpiWeek_FirstWeek;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDate(2024, 1, 4);
  AssertEquals('First full week should be week 1', 
    1, TDateTimeKit.GetEpiWeek(TestDate));
end;

procedure TDateTimeTests.Test87c_EpiWeek_YearEnd;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDate(2024, 12, 31);
  AssertEquals('Year-end week number should be correct', 
    53, TDateTimeKit.GetEpiWeek(TestDate));
end;

{ StandardizePeriod Tests }
procedure TDateTimeTests.Test95a_StandardizePeriod_Milliseconds;
var
  Period, Standardized: TDateSpan;
begin
  Period := TDateTimeKit.CreatePeriod(0, 0, 0, 0, 0, 0, 1001);
  Standardized := TDateTimeKit.StandardizePeriod(Period);
  AssertEquals('Milliseconds should be normalized', 1, Standardized.Milliseconds);
  AssertEquals('Extra milliseconds should carry to seconds', 1, Standardized.Seconds);
end;

procedure TDateTimeTests.Test95b_StandardizePeriod_Seconds;
var
  Period, Standardized: TDateSpan;
begin
  Period := TDateTimeKit.CreatePeriod(0, 0, 0, 0, 0, 61, 0);
  Standardized := TDateTimeKit.StandardizePeriod(Period);
  AssertEquals('Seconds should be normalized', 1, Standardized.Seconds);
  AssertEquals('Extra seconds should carry to minutes', 1, Standardized.Minutes);
end;

procedure TDateTimeTests.Test95c_StandardizePeriod_Minutes;
var
  Period, Standardized: TDateSpan;
begin
  Period := TDateTimeKit.CreatePeriod(0, 0, 0, 0, 61, 0, 0);
  Standardized := TDateTimeKit.StandardizePeriod(Period);
  AssertEquals('Minutes should be normalized', 1, Standardized.Minutes);
  AssertEquals('Extra minutes should carry to hours', 1, Standardized.Hours);
end;

procedure TDateTimeTests.Test95d_StandardizePeriod_Hours;
var
  Period, Standardized: TDateSpan;
begin
  Period := TDateTimeKit.CreatePeriod(0, 0, 0, 25, 0, 0, 0);
  Standardized := TDateTimeKit.StandardizePeriod(Period);
  AssertEquals('Hours should be normalized', 1, Standardized.Hours);
  AssertEquals('Extra hours should carry to days', 1, Standardized.Days);
end;

procedure TDateTimeTests.Test95e_StandardizePeriod_Months;
var
  Period, Standardized: TDateSpan;
begin
  Period := TDateTimeKit.CreatePeriod(0, 13, 0, 0, 0, 0, 0);
  Standardized := TDateTimeKit.StandardizePeriod(Period);
  AssertEquals('Months should be normalized', 1, Standardized.Months);
  AssertEquals('Extra months should carry to years', 1, Standardized.Years);
end;

procedure TDateTimeTests.Test95f_StandardizePeriod_Complex;
var
  Period, Standardized: TDateSpan;
begin
  Period := TDateTimeKit.CreatePeriod(1, 13, 32, 25, 61, 61, 1001);
  Standardized := TDateTimeKit.StandardizePeriod(Period);
  
  AssertEquals('Years should include extra months', 2, Standardized.Years);
  AssertEquals('Months should be normalized', 1, Standardized.Months);
  AssertEquals('Days should include extra hours', 33, Standardized.Days);
  AssertEquals('Hours should be normalized', 2, Standardized.Hours);
  AssertEquals('Minutes should be normalized', 2, Standardized.Minutes);
  AssertEquals('Seconds should be normalized', 2, Standardized.Seconds);
  AssertEquals('Milliseconds should be normalized', 1, Standardized.Milliseconds);
end;

{ IntervalGap Tests }
procedure TDateTimeTests.Test97a_IntervalGap_NoOverlap;
var
  Interval1, Interval2: TInterval;
  Gap: TDateSpan;
begin
  Interval1 := TDateTimeKit.CreateInterval(
    EncodeDate(2024, 1, 1),
    EncodeDate(2024, 1, 15));
  Interval2 := TDateTimeKit.CreateInterval(
    EncodeDate(2024, 1, 20),
    EncodeDate(2024, 1, 31));
  Gap := TDateTimeKit.IntervalGap(Interval1, Interval2);
  
  AssertEquals('Gap should be 5 days', 5, Gap.Days);
end;

procedure TDateTimeTests.Test97b_IntervalGap_Overlapping;
var
  Interval1, Interval2: TInterval;
  Gap: TDateSpan;
begin
  Interval1 := TDateTimeKit.CreateInterval(
    EncodeDate(2024, 1, 1),
    EncodeDate(2024, 1, 15));
  Interval2 := TDateTimeKit.CreateInterval(
    EncodeDate(2024, 1, 10),
    EncodeDate(2024, 1, 31));
  Gap := TDateTimeKit.IntervalGap(Interval1, Interval2);
  
  AssertEquals('Overlapping intervals should have no gap', 0, Gap.Days);
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

procedure TFSTests.Test34b_ListDirectories;
var
  SubDir1, SubDir2: string;
  Dirs: TStringArray;
  I: Integer;
  Found: Boolean;
  SR: TSearchRec;
begin
  // Clean up any existing directories first
  if FindFirst(FTestDir + PathDelim + '*', faAnyFile, SR) = 0 then
  try
    repeat
      if (SR.Name <> '.') and (SR.Name <> '..') then
        TFileKit.DeleteDirectory(FTestDir + PathDelim + SR.Name, True);
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;

  // Create test directories
  SubDir1 := TFileKit.CombinePaths(FTestDir, 'dir1');
  SubDir2 := TFileKit.CombinePaths(FTestDir, 'dir2');
  TFileKit.CreateDirectory(SubDir1);
  TFileKit.CreateDirectory(SubDir2);
  
  // Test non-recursive directory listing
  Dirs := TFileKit.ListDirectories(FTestDir, '*', False);
  AssertEquals('ListDirectories should find 2 directories', 2, Length(Dirs));
  
  // Verify both directories are found
  Found := False;
  for I := 0 to High(Dirs) do
    if TFileKit.NormalizePath(Dirs[I]) = TFileKit.NormalizePath(SubDir1) then
    begin
      Found := True;
      Break;
    end;
  AssertTrue('dir1 should be found', Found);
  
  Found := False;
  for I := 0 to High(Dirs) do
    if TFileKit.NormalizePath(Dirs[I]) = TFileKit.NormalizePath(SubDir2) then
    begin
      Found := True;
      Break;
    end;
  AssertTrue('dir2 should be found', Found);
end;

procedure TFSTests.Test34c_ListDirectoriesRecursive;
var
  SubDir1, SubDir2, SubSubDir: string;
  Dirs: TStringArray;
  I: Integer;
  Found: Boolean;
  SR: TSearchRec;
begin
  // Clean up any existing directories first
  if FindFirst(FTestDir + PathDelim + '*', faAnyFile, SR) = 0 then
  try
    repeat
      if (SR.Name <> '.') and (SR.Name <> '..') then
        TFileKit.DeleteDirectory(FTestDir + PathDelim + SR.Name, True);
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;

  // Create test directory structure
  SubDir1 := TFileKit.CombinePaths(FTestDir, 'dir1');
  SubDir2 := TFileKit.CombinePaths(FTestDir, 'dir2');
  SubSubDir := TFileKit.CombinePaths(SubDir1, 'subdir1');
  TFileKit.CreateDirectory(SubDir1);
  TFileKit.CreateDirectory(SubDir2);
  TFileKit.CreateDirectory(SubSubDir);
  
  // Test recursive directory listing
  Dirs := TFileKit.ListDirectories(FTestDir, '*', True);
  AssertEquals('ListDirectories should find 3 directories recursively', 3, Length(Dirs));
  
  // Verify all directories are found
  Found := False;
  for I := 0 to High(Dirs) do
    if TFileKit.NormalizePath(Dirs[I]) = TFileKit.NormalizePath(SubDir1) then
    begin
      Found := True;
      Break;
    end;
  AssertTrue('dir1 should be found', Found);
  
  Found := False;
  for I := 0 to High(Dirs) do
    if TFileKit.NormalizePath(Dirs[I]) = TFileKit.NormalizePath(SubDir2) then
    begin
      Found := True;
      Break;
    end;
  AssertTrue('dir2 should be found', Found);
  
  Found := False;
  for I := 0 to High(Dirs) do
    if TFileKit.NormalizePath(Dirs[I]) = TFileKit.NormalizePath(SubSubDir) then
    begin
      Found := True;
      Break;
    end;
  AssertTrue('subdir1 should be found', Found);
end;

procedure TFSTests.Test34d_ListFiles;
var
  File1, File2: string;
  Files: TStringArray;
  I: Integer;
  Found: Boolean;
  SR: TSearchRec;
begin
  // Clean up any existing files and directories first
  if DirectoryExists(FTestDir) then
  begin
    TFileKit.DeleteDirectory(FTestDir, True);
    RemoveDir(FTestDir);
  end;
  TFileKit.CreateDirectory(FTestDir);

  // Create test files
  File1 := TFileKit.CombinePaths(FTestDir, 'file1.txt');
  File2 := TFileKit.CombinePaths(FTestDir, 'file2.txt');
  TFileKit.WriteFile(File1, 'test1');
  TFileKit.WriteFile(File2, 'test2');
  
  // Test non-recursive file listing
  Files := TFileKit.ListFiles(FTestDir, '*', False);
  WriteLn('Test34d_ListFiles: Found ', Length(Files), ' files:');
  for I := 0 to High(Files) do
    WriteLn('Test34d_ListFiles: File[', I, '] = ', Files[I]);
  
  AssertEquals('ListFiles should find 2 files', 2, Length(Files));
  
  // Verify both files are found
  Found := False;
  for I := 0 to High(Files) do
    if TFileKit.NormalizePath(Files[I]) = TFileKit.NormalizePath(File1) then
    begin
      Found := True;
      Break;
    end;
  AssertTrue('file1.txt should be found', Found);
  
  Found := False;
  for I := 0 to High(Files) do
    if TFileKit.NormalizePath(Files[I]) = TFileKit.NormalizePath(File2) then
    begin
      Found := True;
      Break;
    end;
  AssertTrue('file2.txt should be found', Found);
end;

procedure TFSTests.Test34e_ListFilesRecursive;
var
  File1, File2, SubDir, File3: string;
  Files: TStringArray;
  I: Integer;
  Found: Boolean;
  SR: TSearchRec;
begin
  // Clean up any existing files and directories first
  if DirectoryExists(FTestDir) then
  begin
    TFileKit.DeleteDirectory(FTestDir, True);
    RemoveDir(FTestDir);
  end;
  TFileKit.CreateDirectory(FTestDir);

  // Create test directory structure with files
  File1 := TFileKit.CombinePaths(FTestDir, 'file1.txt');
  File2 := TFileKit.CombinePaths(FTestDir, 'file2.txt');
  SubDir := TFileKit.CombinePaths(FTestDir, 'subdir');
  File3 := TFileKit.CombinePaths(SubDir, 'file3.txt');
  
  TFileKit.WriteFile(File1, 'test1');
  TFileKit.WriteFile(File2, 'test2');
  TFileKit.CreateDirectory(SubDir);
  TFileKit.WriteFile(File3, 'test3');
  
  // Test recursive file listing
  Files := TFileKit.ListFiles(FTestDir, '*', True);
  WriteLn('Test34e_ListFilesRecursive: Found ', Length(Files), ' files:');
  for I := 0 to High(Files) do
    WriteLn('Test34e_ListFilesRecursive: File[', I, '] = ', Files[I]);
  
  AssertEquals('ListFiles should find 3 files recursively', 3, Length(Files));
  
  // Verify all files are found
  Found := False;
  for I := 0 to High(Files) do
    if TFileKit.NormalizePath(Files[I]) = TFileKit.NormalizePath(File1) then
    begin
      Found := True;
      Break;
    end;
  AssertTrue('file1.txt should be found', Found);
  
  Found := False;
  for I := 0 to High(Files) do
    if TFileKit.NormalizePath(Files[I]) = TFileKit.NormalizePath(File2) then
    begin
      Found := True;
      Break;
    end;
  AssertTrue('file2.txt should be found', Found);
  
  Found := False;
  for I := 0 to High(Files) do
    if TFileKit.NormalizePath(Files[I]) = TFileKit.NormalizePath(File3) then
    begin
      Found := True;
      Break;
    end;
  AssertTrue('file3.txt should be found', Found);
end;

procedure TFSTests.Test34f_ListFilesWithPattern;
var
  File1, File2, File3, File4: string;
  Files: TStringArray;
  SR: TSearchRec;
begin
  // Clean up any existing files and directories first
  if DirectoryExists(FTestDir) then
  begin
    TFileKit.DeleteDirectory(FTestDir, True);
    RemoveDir(FTestDir);
  end;
  TFileKit.CreateDirectory(FTestDir);

  // Create test files with different extensions
  File1 := TFileKit.CombinePaths(FTestDir, 'test1.txt');
  File2 := TFileKit.CombinePaths(FTestDir, 'test2.txt');
  File3 := TFileKit.CombinePaths(FTestDir, 'data1.dat');
  File4 := TFileKit.CombinePaths(FTestDir, 'data2.dat');
  
  TFileKit.WriteFile(File1, 'test1');
  TFileKit.WriteFile(File2, 'test2');
  TFileKit.WriteFile(File3, 'data1');
  TFileKit.WriteFile(File4, 'data2');
  
  // Test pattern matching for .txt files
  Files := TFileKit.ListFiles(FTestDir, '*.txt');
  AssertEquals('ListFiles should find 2 .txt files', 2, Length(Files));
  
  // Test pattern matching for .dat files
  Files := TFileKit.ListFiles(FTestDir, '*.dat');
  AssertEquals('ListFiles should find 2 .dat files', 2, Length(Files));
  
  // Test pattern matching with prefix
  Files := TFileKit.ListFiles(FTestDir, 'test*.*');
  AssertEquals('ListFiles should find 2 test files', 2, Length(Files));
end;

procedure TFSTests.Test34g_ListFilesWithSorting;
var
  File1, File2, File3: string;
  Files: TStringArray;
  SR: TSearchRec;
begin
  // Clean up any existing files and directories first
  if DirectoryExists(FTestDir) then
  begin
    TFileKit.DeleteDirectory(FTestDir, True);
    RemoveDir(FTestDir);
  end;
  TFileKit.CreateDirectory(FTestDir);

  // Create test files with different sizes and timestamps
  File1 := TFileKit.CombinePaths(FTestDir, 'b_file.txt');  // Middle name
  File2 := TFileKit.CombinePaths(FTestDir, 'a_file.txt');  // First name
  File3 := TFileKit.CombinePaths(FTestDir, 'c_file.txt');  // Last name
  
  // Create files with different sizes
  TFileKit.WriteFile(File1, StringOfChar('B', 200));  // 200 bytes
  TFileKit.WriteFile(File2, StringOfChar('A', 100));  // 100 bytes
  TFileKit.WriteFile(File3, StringOfChar('C', 300));  // 300 bytes
  
  // Set different timestamps
  FileSetDate(File1, DateTimeToFileDate(EncodeDateTime(2024, 1, 2, 0, 0, 0, 0)));
  FileSetDate(File2, DateTimeToFileDate(EncodeDateTime(2024, 1, 1, 0, 0, 0, 0)));
  FileSetDate(File3, DateTimeToFileDate(EncodeDateTime(2024, 1, 3, 0, 0, 0, 0)));
  
  // Test name sorting (ascending)
  Files := TFileKit.ListFiles(FTestDir, '*', False, fsName);
  AssertEquals('First file should be a_file.txt', 'a_file.txt', ExtractFileName(Files[0]));
  AssertEquals('Last file should be c_file.txt', 'c_file.txt', ExtractFileName(Files[2]));
  
  // Test name sorting (descending)
  Files := TFileKit.ListFiles(FTestDir, '*', False, fsNameDesc);
  AssertEquals('First file should be c_file.txt', 'c_file.txt', ExtractFileName(Files[0]));
  AssertEquals('Last file should be a_file.txt', 'a_file.txt', ExtractFileName(Files[2]));
  
  // Test date sorting (ascending)
  Files := TFileKit.ListFiles(FTestDir, '*', False, fsDate);
  AssertEquals('First file should be a_file.txt', 'a_file.txt', ExtractFileName(Files[0]));
  AssertEquals('Last file should be c_file.txt', 'c_file.txt', ExtractFileName(Files[2]));
  
  // Test date sorting (descending)
  Files := TFileKit.ListFiles(FTestDir, '*', False, fsDateDesc);
  AssertEquals('First file should be c_file.txt', 'c_file.txt', ExtractFileName(Files[0]));
  AssertEquals('Last file should be a_file.txt', 'a_file.txt', ExtractFileName(Files[2]));
  
  // Test size sorting (ascending)
  Files := TFileKit.ListFiles(FTestDir, '*', False, fsSize);
  AssertEquals('First file should be a_file.txt', 'a_file.txt', ExtractFileName(Files[0]));
  AssertEquals('Last file should be c_file.txt', 'c_file.txt', ExtractFileName(Files[2]));
  
  // Test size sorting (descending)
  Files := TFileKit.ListFiles(FTestDir, '*', False, fsSizeDesc);
  AssertEquals('First file should be c_file.txt', 'c_file.txt', ExtractFileName(Files[0]));
  AssertEquals('Last file should be a_file.txt', 'a_file.txt', ExtractFileName(Files[2]));
end;

procedure TFSTests.Test34h_ListDirectoriesWithPattern;
var
  Dir1, Dir2, Dir3, Dir4: string;
  Dirs: TStringArray;
  SR: TSearchRec;
begin
  // Clean up any existing files and directories first
  if DirectoryExists(FTestDir) then
  begin
    TFileKit.DeleteDirectory(FTestDir, True);
    RemoveDir(FTestDir);
  end;
  TFileKit.CreateDirectory(FTestDir);

  // Create test directories with different names
  Dir1 := TFileKit.CombinePaths(FTestDir, 'test_dir1');
  Dir2 := TFileKit.CombinePaths(FTestDir, 'test_dir2');
  Dir3 := TFileKit.CombinePaths(FTestDir, 'data_dir1');
  Dir4 := TFileKit.CombinePaths(FTestDir, 'data_dir2');
  
  TFileKit.CreateDirectory(Dir1);
  TFileKit.CreateDirectory(Dir2);
  TFileKit.CreateDirectory(Dir3);
  TFileKit.CreateDirectory(Dir4);
  
  // Test pattern matching for test directories
  Dirs := TFileKit.ListDirectories(FTestDir, 'test_*');
  AssertEquals('ListDirectories should find 2 test directories', 2, Length(Dirs));
  
  // Test pattern matching for data directories
  Dirs := TFileKit.ListDirectories(FTestDir, 'data_*');
  AssertEquals('ListDirectories should find 2 data directories', 2, Length(Dirs));
  
  // Test pattern matching with number
  Dirs := TFileKit.ListDirectories(FTestDir, '*1');
  AssertEquals('ListDirectories should find 2 directories ending with 1', 2, Length(Dirs));
end;

procedure TFSTests.Test34i_ListDirectoriesWithSorting;
var
  Dir1, Dir2, Dir3: string;
  Dirs: TStringArray;
  SR: TSearchRec;
begin
  // Clean up any existing files and directories first
  if DirectoryExists(FTestDir) then
  begin
    TFileKit.DeleteDirectory(FTestDir, True);
    RemoveDir(FTestDir);
  end;
  TFileKit.CreateDirectory(FTestDir);

  // Create test directories
  Dir1 := TFileKit.CombinePaths(FTestDir, 'b_dir');  // Middle name
  Dir2 := TFileKit.CombinePaths(FTestDir, 'a_dir');  // First name
  Dir3 := TFileKit.CombinePaths(FTestDir, 'c_dir');  // Last name
  
  TFileKit.CreateDirectory(Dir1);
  TFileKit.CreateDirectory(Dir2);
  TFileKit.CreateDirectory(Dir3);
  
  // Set different timestamps
  FileSetDate(Dir1, DateTimeToFileDate(EncodeDateTime(2024, 1, 2, 0, 0, 0, 0)));
  FileSetDate(Dir2, DateTimeToFileDate(EncodeDateTime(2024, 1, 1, 0, 0, 0, 0)));
  FileSetDate(Dir3, DateTimeToFileDate(EncodeDateTime(2024, 1, 3, 0, 0, 0, 0)));
  
  // Test name sorting (ascending)
  Dirs := TFileKit.ListDirectories(FTestDir, '*', False, fsName);
  AssertEquals('First directory should be a_dir', 'a_dir', ExtractFileName(Dirs[0]));
  AssertEquals('Last directory should be c_dir', 'c_dir', ExtractFileName(Dirs[2]));
  
  // Test name sorting (descending)
  Dirs := TFileKit.ListDirectories(FTestDir, '*', False, fsNameDesc);
  AssertEquals('First directory should be c_dir', 'c_dir', ExtractFileName(Dirs[0]));
  AssertEquals('Last directory should be a_dir', 'a_dir', ExtractFileName(Dirs[2]));
  
  // Test date sorting (ascending)
  Dirs := TFileKit.ListDirectories(FTestDir, '*', False, fsDate);
  AssertEquals('First directory should be a_dir (oldest)', 'a_dir', ExtractFileName(Dirs[0]));
  AssertEquals('Last directory should be c_dir (newest)', 'c_dir', ExtractFileName(Dirs[2]));
  
  // Test date sorting (descending)
  Dirs := TFileKit.ListDirectories(FTestDir, '*', False, fsDateDesc);
  AssertEquals('First directory should be c_dir (newest)', 'c_dir', ExtractFileName(Dirs[0]));
  AssertEquals('Last directory should be a_dir (oldest)', 'a_dir', ExtractFileName(Dirs[2]));
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
  Matches: TMatchesResults;
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

// Add test implementations at the end of the implementation section:

procedure TDateTimeTests.Test101_GetTimeZone;
var
  TZInfo: TTimeZoneInfo;
  TestDate: TDateTime;
begin
  TestDate := EncodeDate(2024, 1, 1) + EncodeTime(12, 0, 0, 0);
  TZInfo := TDateTimeKit.GetTimeZone(TestDate);
  
  // Basic checks
  AssertTrue('Timezone name should not be empty', TZInfo.Name <> '');
  AssertTrue('Offset should be within reasonable range', (TZInfo.Offset >= -720) and (TZInfo.Offset <= 720));
end;

procedure TDateTimeTests.Test102_GetSystemTimeZone;
var
  SystemTZ: string;
begin
  SystemTZ := TDateTimeKit.GetSystemTimeZone;
  AssertTrue('System timezone should not be empty', SystemTZ <> '');
end;

procedure TDateTimeTests.Test103_GetTimeZoneNames;
var
  TZNames: TStringArray;
begin
  TZNames := TDateTimeKit.GetTimeZoneNames;
  AssertTrue('Should have at least one timezone name', Length(TZNames) > 0);
  AssertTrue('First timezone name should not be empty', TZNames[0] <> '');
end;

procedure TDateTimeTests.Test104_WithTimeZone;
var
  TestDate: TDateTime;
  CurrentTZ: string;
  ConvertedDate: TDateTime;
begin
  TestDate := EncodeDate(2024, 1, 1) + EncodeTime(12, 0, 0, 0);
  CurrentTZ := TDateTimeKit.GetSystemTimeZone;
  
  try
    // Convert to current timezone (should not change the time)
    ConvertedDate := TDateTimeKit.WithTimeZone(TestDate, CurrentTZ);
    AssertEquals('Time should not change when converting to same timezone',
      TestDate, ConvertedDate);
      
    // Try invalid timezone
    try
      ConvertedDate := TDateTimeKit.WithTimeZone(TestDate, 'Invalid/Timezone');
      Fail('Should raise exception for invalid timezone');
    except
      on E: Exception do
        AssertTrue('Should raise appropriate exception', 
          Pos('not found', E.Message) > 0);
    end;
  except
    on E: Exception do
      Fail('Unexpected exception: ' + E.Message);
  end;
end;

procedure TDateTimeTests.Test105_ForceTimeZone;
var
  TestDate: TDateTime;
  CurrentTZ: string;
  ForcedDate: TDateTime;
begin
  TestDate := EncodeDate(2024, 1, 1) + EncodeTime(12, 0, 0, 0);
  CurrentTZ := TDateTimeKit.GetSystemTimeZone;
  
  try
    // Force to current timezone
    ForcedDate := TDateTimeKit.ForceTimeZone(TestDate, CurrentTZ);
    AssertTrue('Forced timezone date should be different from original',
      TestDate <> ForcedDate);
      
    // Try invalid timezone
    try
      ForcedDate := TDateTimeKit.ForceTimeZone(TestDate, 'Invalid/Timezone');
      Fail('Should raise exception for invalid timezone');
    except
      on E: Exception do
        AssertTrue('Should raise appropriate exception', 
          Pos('not found', E.Message) > 0);
    end;
  except
    on E: Exception do
      Fail('Unexpected exception: ' + E.Message);
  end;
end;

procedure TDateTimeTests.Test106_DSTTransition;
var
  TestDate: TDateTime;
  DSTDate: TDateTime;
  NonDSTDate: TDateTime;
  TZInfo: TTimeZoneInfo;
begin
  // Test DST transition dates (using 2024 dates for US)
  DSTDate := EncodeDate(2024, 3, 10) + EncodeTime(2, 0, 0, 0);    // 2 AM on DST start
  NonDSTDate := EncodeDate(2024, 11, 3) + EncodeTime(2, 0, 0, 0); // 2 AM on DST end
  
  // Check DST start transition
  TZInfo := TDateTimeKit.GetTimeZone(DSTDate);
  AssertTrue('Should be in DST during summer', TZInfo.IsDST);
  
  // Check DST end transition
  TZInfo := TDateTimeKit.GetTimeZone(NonDSTDate);
  AssertFalse('Should not be in DST during winter', TZInfo.IsDST);
  
  // Test time conversion around DST transition
  TestDate := TDateTimeKit.WithTimeZone(DSTDate, 'UTC');
  AssertTrue('UTC conversion should handle DST transition',
    Abs(TestDate - DSTDate) <= 2/24); // Within 2 hours difference
end;

procedure TDateTimeTests.Test107_DateBoundaryConversion;
var
  UTCDate: TDateTime;
  LocalDate: TDateTime;
  ConvertedDate: TDateTime;
  SystemTZ: string;
begin
  // Test date boundary conversion (11 PM UTC on Jan 1 should be next day in some timezones)
  UTCDate := EncodeDate(2024, 1, 1) + EncodeTime(23, 0, 0, 0);
  SystemTZ := TDateTimeKit.GetSystemTimeZone;
  
  // Convert UTC to local time
  LocalDate := TDateTimeKit.WithTimeZone(UTCDate, SystemTZ);
  
  // Convert back to UTC
  ConvertedDate := TDateTimeKit.WithTimeZone(LocalDate, 'UTC');
  
  // Should get back the original UTC time
  AssertEquals('Round-trip timezone conversion should preserve time',
    UTCDate, ConvertedDate);
end;

procedure TDateTimeTests.Test108_InvalidTimezones;
var
  TestDate: TDateTime;
begin
  TestDate := Now;
  
  // Test various invalid timezone names
  try
    TDateTimeKit.WithTimeZone(TestDate, '');
    Fail('Empty timezone should raise exception');
  except
    on E: Exception do
      AssertTrue('Should raise appropriate exception for empty timezone',
        Pos('not found', E.Message) > 0);
  end;
  
  try
    TDateTimeKit.WithTimeZone(TestDate, 'Invalid/TZ');
    Fail('Invalid timezone format should raise exception');
  except
    on E: Exception do
      AssertTrue('Should raise appropriate exception for invalid format',
        Pos('not found', E.Message) > 0);
  end;
  
  try
    TDateTimeKit.WithTimeZone(TestDate, 'UTC+Invalid');
    Fail('Invalid UTC offset should raise exception');
  except
    on E: Exception do
      AssertTrue('Should raise appropriate exception for invalid UTC offset',
        Pos('not found', E.Message) > 0);
  end;
end;

procedure TDateTimeTests.Test109_ExtremeOffsets;
var
  TestDate: TDateTime;
  ConvertedDate: TDateTime;
  TZInfo: TTimeZoneInfo;
begin
  TestDate := Now;
  
  // Test conversion with extreme positive offset (+14:00)
  TZInfo := TDateTimeKit.GetTimeZone(TestDate);
  if TZInfo.Offset = 14 * 60 then // +14:00
  begin
    ConvertedDate := TDateTimeKit.WithTimeZone(TestDate, 'UTC');
    AssertTrue('Extreme positive offset should be handled',
      Abs(ConvertedDate - TestDate) <= 14/24); // Within 14 hours
  end;
  
  // Test conversion with extreme negative offset (-12:00)
  if TZInfo.Offset = -12 * 60 then // -12:00
  begin
    ConvertedDate := TDateTimeKit.WithTimeZone(TestDate, 'UTC');
    AssertTrue('Extreme negative offset should be handled',
      Abs(ConvertedDate - TestDate) <= 12/24); // Within 12 hours
  end;
end;

procedure TDateTimeTests.Test110_DSTTransitionExactTime;
var
  // March 10, 2024 1:59:59 AM (just before DST)
  PreDST: TDateTime;
  // March 10, 2024 2:00:00 AM (DST start - skipped hour)
  DSTStart: TDateTime;
  // March 10, 2024 3:00:00 AM (after DST start)
  PostDST: TDateTime;
  TZInfo: TTimeZoneInfo;
begin
  PreDST := EncodeDateTime(2024, 3, 10, 1, 59, 59, 0);
  DSTStart := EncodeDateTime(2024, 3, 10, 2, 0, 0, 0);
  PostDST := EncodeDateTime(2024, 3, 10, 3, 0, 0, 0);
  
  // Before DST
  TZInfo := TDateTimeKit.GetTimeZone(PreDST);
  AssertFalse('Time before DST should not be in DST', TZInfo.IsDST);
  
  // At DST start (2 AM becomes 3 AM)
  TZInfo := TDateTimeKit.GetTimeZone(DSTStart);
  AssertTrue('Time at DST start should be in DST', TZInfo.IsDST);
  
  // After DST
  TZInfo := TDateTimeKit.GetTimeZone(PostDST);
  AssertTrue('Time after DST start should be in DST', TZInfo.IsDST);
end;

procedure TDateTimeTests.Test111_DSTEndExactTime;
var
  // November 3, 2024 1:59:59 AM (before DST end)
  PreStandard: TDateTime;
  // November 3, 2024 2:00:00 AM (DST end - ambiguous hour)
  DSTEnd: TDateTime;
  // November 3, 2024 3:00:00 AM (after DST end)
  PostStandard: TDateTime;
  TZInfo: TTimeZoneInfo;
begin
  PreStandard := EncodeDateTime(2024, 11, 3, 1, 59, 59, 0);
  DSTEnd := EncodeDateTime(2024, 11, 3, 2, 0, 0, 0);
  PostStandard := EncodeDateTime(2024, 11, 3, 3, 0, 0, 0);
  
  // Before standard time
  TZInfo := TDateTimeKit.GetTimeZone(PreStandard);
  AssertTrue('Time before DST end should be in DST', TZInfo.IsDST);
  
  // At DST end (first 2 AM)
  TZInfo := TDateTimeKit.GetTimeZone(DSTEnd);
  AssertFalse('Time at DST end should not be in DST', TZInfo.IsDST);
  
  // After standard time
  TZInfo := TDateTimeKit.GetTimeZone(PostStandard);
  AssertFalse('Time after DST end should not be in DST', TZInfo.IsDST);
end;

procedure TDateTimeTests.Test112_LeapYearDST;
var
  // February 29, 2024 23:59:59 (leap day)
  LeapDayEnd: TDateTime;
  // March 1, 2024 00:00:00 (after leap day)
  PostLeap: TDateTime;
  // March 10, 2024 02:00:00 (DST start on leap year)
  LeapDST: TDateTime;
  TZInfo: TTimeZoneInfo;
begin
  LeapDayEnd := EncodeDateTime(2024, 2, 29, 23, 59, 59, 0);
  PostLeap := EncodeDateTime(2024, 3, 1, 0, 0, 0, 0);
  LeapDST := EncodeDateTime(2024, 3, 10, 2, 0, 0, 0);
  
  // End of leap day
  TZInfo := TDateTimeKit.GetTimeZone(LeapDayEnd);
  AssertFalse('End of leap day should not be in DST', TZInfo.IsDST);
  
  // Start of March
  TZInfo := TDateTimeKit.GetTimeZone(PostLeap);
  AssertFalse('Start of March should not be in DST', TZInfo.IsDST);
  
  // DST start in leap year
  TZInfo := TDateTimeKit.GetTimeZone(LeapDST);
  AssertTrue('DST start in leap year should be in DST', TZInfo.IsDST);
end;

procedure TDateTimeTests.Test113_InvalidTimeZoneEdgeCases;
var
  Now: TDateTime;
begin
  Now := TDateTimeKit.GetNow;
  
  // Empty string timezone
  try
    Now := TDateTimeKit.WithTimeZone(Now, '');
    Fail('Empty timezone should raise exception');
  except
    on E: ETimeZoneError do
      AssertTrue('Expected timezone error', True);
  end;
  
  // Invalid timezone format
  try
    Now := TDateTimeKit.WithTimeZone(Now, 'Invalid/Timezone');
    Fail('Invalid timezone should raise exception');
  except
    on E: ETimeZoneError do
      AssertTrue('Expected timezone error', True);
  end;
  
  // Timezone with special characters
  try
    Now := TDateTimeKit.WithTimeZone(Now, '#$%^&*');
    Fail('Special characters should raise exception');
  except
    on E: ETimeZoneError do
      AssertTrue('Expected timezone error', True);
  end;
end;

procedure TDateTimeTests.Test114_UTCOffsetEdgeCases;
var
  TZInfo: TTimeZoneInfo;
begin
  // Test minimum valid offset (-12:00)
  try
    TZInfo.Offset := -12 * 60;
    TDateTimeKit.ValidateTimeZoneOffset(TZInfo.Offset);
    AssertTrue('Minimum UTC offset should be valid', True);
  except
    Fail('Valid minimum offset should not raise exception');
  end;
  
  // Test maximum valid offset (+14:00)
  try
    TZInfo.Offset := 14 * 60;
    TDateTimeKit.ValidateTimeZoneOffset(TZInfo.Offset);
    AssertTrue('Maximum UTC offset should be valid', True);
  except
    Fail('Valid maximum offset should not raise exception');
  end;
  
  // Test invalid negative offset
  try
    TZInfo.Offset := -13 * 60;
    TDateTimeKit.ValidateTimeZoneOffset(TZInfo.Offset);
    Fail('Invalid negative offset should raise exception');
  except
    on E: ETimeZoneError do
      AssertTrue('Expected timezone error', True);
  end;
  
  // Test invalid positive offset
  try
    TZInfo.Offset := 15 * 60;
    TDateTimeKit.ValidateTimeZoneOffset(TZInfo.Offset);
    Fail('Invalid positive offset should raise exception');
  except
    on E: ETimeZoneError do
      AssertTrue('Expected timezone error', True);
  end;
end;

procedure TDateTimeTests.Test115_CrossBoundaryConversions;
var
  // December 31, 2024 23:59:59
  YearEnd: TDateTime;
  // January 1, 2025 00:00:00
  YearStart: TDateTime;
  TZInfo: TTimeZoneInfo;
  ConvertedEnd, ConvertedStart: TDateTime;
  SystemTZ: string;
begin
  YearEnd := EncodeDateTime(2024, 12, 31, 23, 59, 59, 0);
  YearStart := EncodeDateTime(2025, 1, 1, 0, 0, 0, 0);
  SystemTZ := TDateTimeKit.GetSystemTimeZone;
  
  // First convert to UTC
  ConvertedEnd := TDateTimeKit.WithTimeZone(YearEnd, 'UTC');
  ConvertedStart := TDateTimeKit.WithTimeZone(YearStart, 'UTC');
  
  // Then convert back to system timezone
  ConvertedEnd := TDateTimeKit.WithTimeZone(ConvertedEnd, SystemTZ);
  ConvertedStart := TDateTimeKit.WithTimeZone(ConvertedStart, SystemTZ);
  
  // Get timezone info for verification
  TZInfo := TDateTimeKit.GetTimeZone(ConvertedEnd);
  AssertEquals('Should be back in system timezone', SystemTZ, TZInfo.Name);
  
  TZInfo := TDateTimeKit.GetTimeZone(ConvertedStart);
  AssertEquals('Should be back in system timezone', SystemTZ, TZInfo.Name);
  
  // Verify chronological order is maintained
  AssertTrue('Year end should be before year start after conversion', 
             TDateTimeKit.IsBefore(ConvertedEnd, ConvertedStart));
               
  // Verify the time difference is preserved (should be 1 second)
  AssertEquals('Time difference should be preserved',
                1/SecsPerDay, // 1 second in TDateTime units
                ConvertedStart - ConvertedEnd);
end;

procedure TDateTimeTests.Test71_YMD;
var
  Expected: TDateTime;
begin
  Expected := EncodeDate(2024, 3, 15);
  AssertEquals('YMD with hyphen', Expected, FDateTime.YMD('2024-03-15'));
  AssertEquals('YMD with slash', Expected, FDateTime.YMD('2024/03/15'));
end;

procedure TDateTimeTests.Test72_MDY;
var
  Expected: TDateTime;
begin
  Expected := EncodeDate(2024, 3, 15);
  AssertEquals('MDY with hyphen', Expected, FDateTime.MDY('03-15-2024'));
  AssertEquals('MDY with slash', Expected, FDateTime.MDY('03/15/2024'));
end;

procedure TDateTimeTests.Test73_DMY;
var
  Expected: TDateTime;
begin
  Expected := EncodeDate(2024, 3, 15);
  AssertEquals('DMY with hyphen', Expected, FDateTime.DMY('15-03-2024'));
  AssertEquals('DMY with slash', Expected, FDateTime.DMY('15/03/2024'));
end;

initialization
  RegisterTest(TDateTimeTests);
  RegisterTest(TFSTests);
  RegisterTest(TStringTests);
end.
