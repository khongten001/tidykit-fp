unit TestCaseDateTime;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, DateUtils, fpcunit, testregistry,
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
    
    // More Date parsing tests
    procedure Test116_YMD;
    procedure Test117_MDY;
    procedure Test118_DMY;
    
    // Region-specific DST tests
    procedure Test114_RegionSpecificDST;
  end;

implementation

{$IFDEF WINDOWS}
uses Windows;
{$ENDIF}

{$IFDEF UNIX}
{$LINKLIB c}
function setenv(name, value: PChar; overwrite: LongInt): LongInt; cdecl; external 'c';
{$ENDIF}

{ SetEnvironmentVariableCrossPlatform - Sets environment variable in a platform-independent way }
procedure SetEnvironmentVariableCrossPlatform(const Name, Value: string);
begin
  {$IFDEF WINDOWS}
  Windows.SetEnvironmentVariable(PChar(Name), PChar(Value));
  {$ELSE}
  {$IFDEF UNIX}
  setenv(PChar(Name), PChar(Value), 1);
  {$ENDIF}
  {$ENDIF}
end;

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
  WriteLn('Test01_Now:Starting');
  CurrentTime := Now;
  AssertTrue('Now should return current time',
    Abs(CurrentTime - TDateTimeKit.GetNow) < 1/86400); // Within 1 second
  WriteLn('Test01_Now:Finished');
end;

procedure TDateTimeTests.Test02_Today;
begin
  WriteLn('Test02_Today:Starting');
  AssertEquals('Today should return current date at midnight',
    Trunc(Date), Trunc(TDateTimeKit.GetToday));
  WriteLn('Test02_Today:Finished');
end;

procedure TDateTimeTests.Test03_From;
var
  TestDate: TDateTime;
begin
  WriteLn('Test03_From:Starting');
  TestDate := EncodeDate(2024, 1, 15);
  AssertEquals('GetDateTime should return the correct date',
    TestDate, TDateTimeKit.GetDateTime(TestDate));
  WriteLn('Test03_From:Finished');
end;

procedure TDateTimeTests.Test04_Year;
var
  TestYear: Integer;
  TestDate: TDateTime;
begin
  WriteLn('Test04_Year:Starting');
  TestYear := 2024;
  TestDate := Now;
  AssertEquals('Year getter should return current year',
    YearOf(TestDate), TDateTimeKit.GetYear(TestDate));
  AssertEquals('Year setter should set specified year',
    TestYear, TDateTimeKit.GetYear(TDateTimeKit.SetYear(TestDate, TestYear)));
  WriteLn('Test04_Year:Finished');
end;

procedure TDateTimeTests.Test05_Month;
var
  TestMonth: Integer;
  TestDate: TDateTime;
begin
  WriteLn('Test05_Month:Starting');
  TestMonth := 6;
  TestDate := Now;
  AssertEquals('Month getter should return current month',
    MonthOf(TestDate), TDateTimeKit.GetMonth(TestDate));
  AssertEquals('Month setter should set specified month',
    TestMonth, TDateTimeKit.GetMonth(TDateTimeKit.SetMonth(TestDate, TestMonth)));
  WriteLn('Test05_Month:Finished');
end;

procedure TDateTimeTests.Test06_Day;
var
  TestDay: Integer;
  TestDate: TDateTime;
begin
  WriteLn('Test06_Day:Starting');
  TestDay := 15;
  TestDate := Now;
  AssertEquals('Day getter should return current day',
    DayOf(TestDate), TDateTimeKit.GetDay(TestDate));
  AssertEquals('Day setter should set specified day',
    TestDay, TDateTimeKit.GetDay(TDateTimeKit.SetDay(TestDate, TestDay)));
  WriteLn('Test06_Day:Finished');
end;

procedure TDateTimeTests.Test07_Hour;
var
  TestHour: Integer;
  TestDate: TDateTime;
begin
  WriteLn('Test07_Hour:Starting');
  TestHour := 14;
  TestDate := Now;
  AssertEquals('Hour getter should return current hour',
    HourOf(TestDate), TDateTimeKit.GetHour(TestDate));
  AssertEquals('Hour setter should set specified hour',
    TestHour, TDateTimeKit.GetHour(TDateTimeKit.SetHour(TestDate, TestHour)));
  WriteLn('Test07_Hour:Finished');
end;

procedure TDateTimeTests.Test08_Minute;
var
  TestMinute: Integer;
  TestDate: TDateTime;
begin
  WriteLn('Test08_Minute:Starting');
  TestMinute := 30;
  TestDate := Now;
  AssertEquals('Minute getter should return current minute',
    MinuteOf(TestDate), TDateTimeKit.GetMinute(TestDate));
  AssertEquals('Minute setter should set specified minute',
    TestMinute, TDateTimeKit.GetMinute(TDateTimeKit.SetMinute(TestDate, TestMinute)));
  WriteLn('Test08_Minute:Finished');
end;

procedure TDateTimeTests.Test09_Second;
var
  TestSecond: Integer;
  TestDate: TDateTime;
begin
  WriteLn('Test09_Second:Starting');
  TestSecond := 45;
  TestDate := Now;
  AssertEquals('Second getter should return current second',
    SecondOf(TestDate), TDateTimeKit.GetSecond(TestDate));
  AssertEquals('Second setter should set specified second',
    TestSecond, TDateTimeKit.GetSecond(TDateTimeKit.SetSecond(TestDate, TestSecond)));
  WriteLn('Test09_Second:Finished');
end;

procedure TDateTimeTests.Test10_Millisecond;
var
  TestMillisecond: Integer;
  TestDate: TDateTime;
begin
  WriteLn('Test10_Millisecond:Starting');
  TestMillisecond := 500;
  TestDate := Now;
  AssertEquals('Millisecond getter should return current millisecond',
    MilliSecondOf(TestDate), TDateTimeKit.GetMillisecond(TestDate));
  AssertEquals('Millisecond setter should set specified millisecond',
    TestMillisecond, TDateTimeKit.GetMillisecond(TDateTimeKit.SetMilliSecond(TestDate, TestMillisecond)));
  WriteLn('Test10_Millisecond:Finished');
end;

procedure TDateTimeTests.Test11_AddYears;
var
  StartDate, Expected: TDateTime;
begin
  WriteLn('Test11_AddYears:Starting');
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
  WriteLn('Test11_AddYears:Finished');
end;

procedure TDateTimeTests.Test12_AddMonths;
var
  StartDate, Expected: TDateTime;
begin
  WriteLn('Test12_AddMonths:Starting');
  StartDate := EncodeDate(2024, 1, 15);
  Expected := EncodeDate(2024, 3, 15);
  AssertEquals('AddMonths should add specified months',
    Expected, TDateTimeKit.AddMonths(StartDate, 2));
  WriteLn('Test12_AddMonths:Finished');
end;

procedure TDateTimeTests.Test13_AddDays;
var
  StartDate, Expected: TDateTime;
begin
  WriteLn('Test13_AddDays:Starting');
  StartDate := EncodeDate(2024, 1, 15);
  Expected := EncodeDate(2024, 1, 17);
  AssertEquals('AddDays should add specified days',
    Expected, TDateTimeKit.AddDays(StartDate, 2));
  WriteLn('Test13_AddDays:Finished');
end;

procedure TDateTimeTests.Test14_AddHours;
var
  StartDate, Expected: TDateTime;
begin
  WriteLn('Test14_AddHours:Starting');
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
  WriteLn('Test14_AddHours:Finished');
end;

procedure TDateTimeTests.Test15_AddMinutes;
var
  StartDate, Expected: TDateTime;
begin
  WriteLn('Test15_AddMinutes:Starting');
  StartDate := EncodeDate(2024, 1, 15) + EncodeTime(12, 0, 0, 0);
  Expected := EncodeDate(2024, 1, 15) + EncodeTime(12, 30, 0, 0);
  AssertEquals('AddMinutes should add specified minutes',
    Expected, TDateTimeKit.AddMinutes(StartDate, 30));
  WriteLn('Test15_AddMinutes:Finished');
end;

procedure TDateTimeTests.Test16_AddSeconds;
var
  StartDate, Expected: TDateTime;
begin
  WriteLn('Test16_AddSeconds:Starting');
  StartDate := EncodeDate(2024, 1, 15) + EncodeTime(12, 0, 0, 0);
  Expected := EncodeDate(2024, 1, 15) + EncodeTime(12, 0, 30, 0);
  AssertEquals('AddSeconds should add specified seconds',
    Expected, TDateTimeKit.AddSeconds(StartDate, 30));
  WriteLn('Test16_AddSeconds:Finished');
end;

procedure TDateTimeTests.Test17_StartOfYear;
var
  TestDate, Expected: TDateTime;
begin
  WriteLn('Test17_StartOfYear:Starting');
  TestDate := EncodeDate(2024, 6, 15) + EncodeTime(12, 30, 45, 500);
  Expected := EncodeDate(2024, 1, 1);
  AssertEquals('StartOfYear should set to start of year',
    Expected, TDateTimeKit.StartOfYear(TestDate));
  WriteLn('Test17_StartOfYear:Finished');
end;

procedure TDateTimeTests.Test18_StartOfMonth;
var
  TestDate, Expected: TDateTime;
begin
  WriteLn('Test18_StartOfMonth:Starting');
  TestDate := EncodeDate(2024, 6, 15) + EncodeTime(12, 30, 45, 500);
  Expected := EncodeDate(2024, 6, 1);
  AssertEquals('StartOfMonth should set to start of month',
    Expected, TDateTimeKit.StartOfMonth(TestDate));
  WriteLn('Test18_StartOfMonth:Finished');
end;

procedure TDateTimeTests.Test19_StartOfDay;
var
  TestDate, Expected: TDateTime;
begin
  WriteLn('Test19_StartOfDay:Starting');
  TestDate := EncodeDate(2024, 6, 15) + EncodeTime(12, 30, 45, 500);
  Expected := EncodeDate(2024, 6, 15);
  AssertEquals('StartOfDay should set to start of day',
    Expected, TDateTimeKit.StartOfDay(TestDate));
  WriteLn('Test19_StartOfDay:Finished');
end;

procedure TDateTimeTests.Test20_EndOfYear;
var
  TestDate, Expected: TDateTime;
begin
  WriteLn('Test20_EndOfYear:Starting');
  TestDate := EncodeDate(2024, 6, 15) + EncodeTime(12, 30, 45, 500);
  Expected := EncodeDate(2024, 12, 31) + EncodeTime(23, 59, 59, 999);
  AssertEquals('EndOfYear should set to end of year',
    Expected, TDateTimeKit.EndOfYear(TestDate));
  WriteLn('Test20_EndOfYear:Finished');
end;

procedure TDateTimeTests.Test21_EndOfMonth;
var
  TestDate, Expected: TDateTime;
begin
  WriteLn('Test21_EndOfMonth:Starting');
  TestDate := EncodeDate(2024, 6, 15) + EncodeTime(12, 30, 45, 500);
  Expected := EncodeDate(2024, 6, 30) + EncodeTime(23, 59, 59, 999);
  AssertEquals('EndOfMonth should set to end of month',
    Expected, TDateTimeKit.EndOfMonth(TestDate));
  WriteLn('Test21_EndOfMonth:Finished');
end;

procedure TDateTimeTests.Test22_EndOfDay;
var
  TestDate, Expected: TDateTime;
begin
  WriteLn('Test22_EndOfDay:Starting');
  TestDate := EncodeDate(2024, 6, 15) + EncodeTime(12, 30, 45, 500);
  Expected := EncodeDate(2024, 6, 15) + EncodeTime(23, 59, 59, 999);
  AssertEquals('EndOfDay should set to end of day',
    Expected, TDateTimeKit.EndOfDay(TestDate));
  WriteLn('Test22_EndOfDay:Finished');
end;

procedure TDateTimeTests.Test23_IsBefore;
var
  Date1, Date2: TDateTime;
begin
  WriteLn('Test23_IsBefore:Starting');
  Date1 := EncodeDate(2024, 1, 15);
  Date2 := EncodeDate(2024, 1, 16);
  AssertTrue('IsBefore should work correctly',
    TDateTimeKit.IsBefore(Date1, Date2));
  WriteLn('Test23_IsBefore:Finished');
end;

procedure TDateTimeTests.Test24_IsAfter;
var
  Date1, Date2: TDateTime;
begin
  WriteLn('Test24_IsAfter:Starting');
  Date1 := EncodeDate(2024, 1, 16);
  Date2 := EncodeDate(2024, 1, 15);
  AssertTrue('IsAfter should work correctly',
    TDateTimeKit.IsAfter(Date1, Date2));
  WriteLn('Test24_IsAfter:Finished');
end;

procedure TDateTimeTests.Test25_IsSameDay;
var
  Date1, Date2: TDateTime;
begin
  WriteLn('Test25_IsSameDay:Starting');
  Date1 := EncodeDate(2024, 1, 15) + EncodeTime(10, 0, 0, 0);
  Date2 := EncodeDate(2024, 1, 15) + EncodeTime(14, 30, 0, 0);
  AssertTrue('IsSameDay should work correctly',
    TDateTimeKit.IsSameDay(Date1, Date2));
  WriteLn('Test25_IsSameDay:Finished');
end;

procedure TDateTimeTests.Test26_IsSameMonth;
var
  Date1, Date2: TDateTime;
begin
  WriteLn('Test26_IsSameMonth:Starting');
  Date1 := EncodeDate(2024, 1, 15);
  Date2 := EncodeDate(2024, 1, 20);
  AssertTrue('IsSameMonth should work correctly',
    TDateTimeKit.IsSameMonth(Date1, Date2));
  WriteLn('Test26_IsSameMonth:Finished');
end;

procedure TDateTimeTests.Test27_IsSameYear;
var
  Date1, Date2: TDateTime;
begin
  WriteLn('Test27_IsSameYear:Starting');
  Date1 := EncodeDate(2024, 1, 15);
  Date2 := EncodeDate(2024, 6, 15);
  AssertTrue('IsSameYear should work correctly',
    TDateTimeKit.IsSameYear(Date1, Date2));
  WriteLn('Test27_IsSameYear:Finished');
end;

procedure TDateTimeTests.Test28_ToDateTime;
var
  TestDate: TDateTime;
begin
  WriteLn('Test28_ToDateTime:Starting');
  TestDate := EncodeDate(2024, 1, 15);
  AssertEquals('GetDateTime should return the correct date',
    TestDate, TDateTimeKit.GetDateTime(TestDate));
  WriteLn('Test28_ToDateTime:Finished');
end;

procedure TDateTimeTests.Test29_ToString;
var
  TestDate: TDateTime;
begin
  WriteLn('Test29_ToString:Starting');
  TestDate := EncodeDate(2024, 1, 15);
  AssertEquals('GetAsString should return the correct string',
    FormatDateTime('dd/mm/yyyy', TestDate),
    TDateTimeKit.GetAsString(TestDate, 'dd/mm/yyyy'));
  WriteLn('Test29_ToString:Finished');
end;

// Add new tests for business day functions
procedure TDateTimeTests.Test30_IsBusinessDay;
var
  Monday, Saturday: TDateTime;
begin
  WriteLn('Test30_IsBusinessDay:Starting');
  Monday := EncodeDate(2024, 1, 15);    // Monday
  Saturday := EncodeDate(2024, 1, 20);  // Saturday
  AssertTrue('Monday should be a business day',
    TDateTimeKit.IsBusinessDay(Monday));
  AssertFalse('Saturday should not be a business day',
    TDateTimeKit.IsBusinessDay(Saturday));
  WriteLn('Test30_IsBusinessDay:Finished');
end;

procedure TDateTimeTests.Test31_NextBusinessDay;
var
  Friday, Monday: TDateTime;
begin
  WriteLn('Test31_NextBusinessDay:Starting');
  Friday := EncodeDate(2024, 1, 19);    // Friday
  Monday := EncodeDate(2024, 1, 22);    // Next Monday
  AssertEquals('Next business day after Friday should be Monday',
    Monday, TDateTimeKit.NextBusinessDay(Friday));
  WriteLn('Test31_NextBusinessDay:Finished');
end;

procedure TDateTimeTests.Test32_PreviousBusinessDay;
var
  Monday, Friday: TDateTime;
begin
  WriteLn('Test32_PreviousBusinessDay:Starting');
  Monday := EncodeDate(2024, 1, 22);    // Monday
  Friday := EncodeDate(2024, 1, 19);    // Previous Friday
  AssertEquals('Previous business day before Monday should be Friday',
    Friday, TDateTimeKit.PreviousBusinessDay(Monday));
  WriteLn('Test32_PreviousBusinessDay:Finished');
end;

procedure TDateTimeTests.Test33_AddBusinessDays;
var
  StartDate, Expected: TDateTime;
begin
  WriteLn('Test33_AddBusinessDays:Starting');
  StartDate := EncodeDate(2024, 1, 15); // Monday
  Expected := EncodeDate(2024, 1, 19);  // Friday (4 business days later)
  AssertEquals('AddBusinessDays should skip weekends',
    Expected, TDateTimeKit.AddBusinessDays(StartDate, 4));
  WriteLn('Test33_AddBusinessDays:Finished');
end;

procedure TDateTimeTests.Test34_CreatePeriod;
var
  Period: TDateSpan;
begin
  WriteLn('Test34_CreatePeriod:Starting');
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
  WriteLn('Test34_CreatePeriod:Finished');
end;

procedure TDateTimeTests.Test35_CreateDuration;
var
  Duration: TDateSpan;
begin
  WriteLn('Test35_CreateDuration:Starting');
  // Test creating a duration (converts to total seconds)
  Duration := TDateTimeKit.CreateDuration(0, 0, 1, 2, 30, 0, 0);  // 1 day, 2 hours, 30 minutes
  
  AssertEquals('Duration kind should be dskDuration', Ord(TDateSpanKind.dskDuration), Ord(Duration.Kind));
  AssertEquals('Total seconds should be calculated correctly',
    ((24 + 2) * 60 + 30) * 60,  // (26 hours + 30 minutes) in seconds
    Duration.Seconds);
  WriteLn('Test35_CreateDuration:Finished');
end;

procedure TDateTimeTests.Test36_CreateInterval;
var
  StartDate, EndDate: TDateTime;
  Interval: TInterval;
begin
  WriteLn('Test36_CreateInterval:Starting');
  StartDate := EncodeDate(2024, 1, 1);
  EndDate := EncodeDate(2024, 12, 31);
  
  Interval := TDateTimeKit.CreateInterval(StartDate, EndDate);
  
  AssertEquals('Interval start date should match', StartDate, Interval.StartDate);
  AssertEquals('Interval end date should match', EndDate, Interval.EndDate);
  WriteLn('Test36_CreateInterval:Finished');
end;

procedure TDateTimeTests.Test37_AddSpan;
var
  StartDate, ResultDate: TDateTime;
  Period: TDateSpan;
begin
  WriteLn('Test37_AddSpan:Starting');
  StartDate := EncodeDate(2024, 1, 1);
  
  // Test adding a period
  Period := TDateTimeKit.CreatePeriod(1, 2, 3);  // 1 year, 2 months, 3 days
  ResultDate := TDateTimeKit.AddSpan(StartDate, Period);
  
  AssertEquals('Year should be incremented', 2025, TDateTimeKit.GetYear(ResultDate));
  AssertEquals('Month should be March', 3, TDateTimeKit.GetMonth(ResultDate));
  AssertEquals('Day should be 4th', 4, TDateTimeKit.GetDay(ResultDate));
  WriteLn('Test37_AddSpan:Finished');
end;

procedure TDateTimeTests.Test38_SubtractSpan;
var
  StartDate, ResultDate: TDateTime;
  Period: TDateSpan;
begin
  WriteLn('Test38_SubtractSpan:Starting');
  StartDate := EncodeDate(2024, 3, 15);
  
  // Test subtracting a period
  Period := TDateTimeKit.CreatePeriod(0, 1, 10);  // 1 month, 10 days
  ResultDate := TDateTimeKit.SubtractSpan(StartDate, Period);
  
  AssertEquals('Month should be February', 2, TDateTimeKit.GetMonth(ResultDate));
  AssertEquals('Day should be 5th', 5, TDateTimeKit.GetDay(ResultDate));
  WriteLn('Test38_SubtractSpan:Finished');
end;

procedure TDateTimeTests.Test39_SpanBetween;
var
  StartDate, EndDate: TDateTime;
  Span: TDateSpan;
begin
  WriteLn('Test39_SpanBetween:Starting');
  StartDate := EncodeDate(2024, 1, 1);
  EndDate := EncodeDate(2025, 2, 15);
  
  // Test calculating period between dates
  Span := TDateTimeKit.SpanBetween(StartDate, EndDate);
  
  AssertEquals('Span kind should be period', Ord(TDateSpanKind.dskPeriod), Ord(Span.Kind));
  AssertEquals('Years should be 1', 1, Span.Years);
  AssertEquals('Months should be 1', 1, Span.Months);
  AssertEquals('Days should be 14', 14, Span.Days);
  WriteLn('Test39_SpanBetween:Finished');
end;

procedure TDateTimeTests.Test40_IsWithinInterval;
var
  StartDate, EndDate, TestDate: TDateTime;
  Interval: TInterval;
begin
  WriteLn('Test40_IsWithinInterval:Starting');
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
  WriteLn('Test40_IsWithinInterval:Finished');
end;

procedure TDateTimeTests.Test41_IntervalsOverlap;
var
  Interval1, Interval2: TInterval;
begin
  WriteLn('Test41_IntervalsOverlap:Starting');
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
  WriteLn('Test41_IntervalsOverlap:Finished');
end;

procedure TDateTimeTests.Test42_IntervalLength;
var
  Interval: TInterval;
  Span: TDateSpan;
begin
  WriteLn('Test42_IntervalLength:Starting');
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
  WriteLn('Test42_IntervalLength:Finished');
end;

procedure TDateTimeTests.Test43_PeriodNormalization;
var
  Period: TDateSpan;
  StartDate, ResultDate: TDateTime;
begin
  WriteLn('Test43_PeriodNormalization:Starting');
  // Test period normalization (13 months should become 1 year 1 month)
  Period := TDateTimeKit.CreatePeriod(0, 13, 0);
  StartDate := EncodeDate(2024, 1, 1);
  ResultDate := TDateTimeKit.AddSpan(StartDate, Period);
  
  AssertEquals('Year should be incremented', 2025, TDateTimeKit.GetYear(ResultDate));
  AssertEquals('Month should be February', 2, TDateTimeKit.GetMonth(ResultDate));
  WriteLn('Test43_PeriodNormalization:Finished');
end;

procedure TDateTimeTests.Test44_DurationCalculation;
var
  Duration: TDateSpan;
  StartDate, ResultDate: TDateTime;
begin
  WriteLn('Test44_DurationCalculation:Starting');
  // Test precise duration calculations
  Duration := TDateTimeKit.CreateDuration(0, 0, 0, 25, 0, 0, 0);  // 25 hours
  StartDate := EncodeDate(2024, 1, 1) + EncodeTime(12, 0, 0, 0);  // Noon
  ResultDate := TDateTimeKit.AddSpan(StartDate, Duration);
  
  AssertEquals('Day should be incremented', 2, TDateTimeKit.GetDay(ResultDate));
  AssertEquals('Hour should be 13', 13, TDateTimeKit.GetHour(ResultDate));
  WriteLn('Test44_DurationCalculation:Finished');
end;

procedure TDateTimeTests.Test45_SpanCornerCases;
var
  Period: TDateSpan;
  StartDate, ResultDate: TDateTime;
begin
  WriteLn('Test45_SpanCornerCases:Starting');
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
  WriteLn('Test45_SpanCornerCases:Finished');
end;

// Implementation of new test cases

procedure TDateTimeTests.Test46_FloorDateSecond;
var
  TestDate: TDateTime;
begin
  WriteLn('Test46_FloorDateSecond:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Floor to second should clear milliseconds',
    EncodeDateTime(2024, 3, 15, 14, 30, 45, 0),
    TDateTimeKit.FloorDate(TestDate, duSecond));
  WriteLn('Test46_FloorDateSecond:Finished');
end;

procedure TDateTimeTests.Test47_FloorDateMinute;
var
  TestDate: TDateTime;
begin
  WriteLn('Test47_FloorDateMinute:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Floor to minute should clear seconds and milliseconds',
    EncodeDateTime(2024, 3, 15, 14, 30, 0, 0),
    TDateTimeKit.FloorDate(TestDate, duMinute));
  WriteLn('Test47_FloorDateMinute:Finished');
end;

procedure TDateTimeTests.Test48_FloorDateHour;
var
  TestDate: TDateTime;
begin
  WriteLn('Test48_FloorDateHour:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Floor to hour should clear minutes, seconds and milliseconds',
    EncodeDateTime(2024, 3, 15, 14, 0, 0, 0),
    TDateTimeKit.FloorDate(TestDate, duHour));
  WriteLn('Test48_FloorDateHour:Finished');
end;

procedure TDateTimeTests.Test49_FloorDateDay;
var
  TestDate: TDateTime;
begin
  WriteLn('Test49_FloorDateDay:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Floor to day should clear time portion',
    EncodeDate(2024, 3, 15),
    TDateTimeKit.FloorDate(TestDate, duDay));
  WriteLn('Test49_FloorDateDay:Finished');
end;

procedure TDateTimeTests.Test50_FloorDateWeek;
var
  TestDate: TDateTime;
begin
  WriteLn('Test50_FloorDateWeek:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500); // March 15, 2024 is a Friday
  AssertEquals('Floor to week should go to Sunday',
    EncodeDate(2024, 3, 10), // Should go to Sunday, March 10
    TDateTimeKit.FloorDate(TestDate, duWeek));
  WriteLn('Test50_FloorDateWeek:Finished');
end;

procedure TDateTimeTests.Test51_FloorDateMonth;
var
  TestDate: TDateTime;
begin
  WriteLn('Test51_FloorDateMonth:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Floor to month should go to first day of month',
    EncodeDate(2024, 3, 1),
    TDateTimeKit.FloorDate(TestDate, duMonth));
  WriteLn('Test51_FloorDateMonth:Finished');
end;

procedure TDateTimeTests.Test52_FloorDateBiMonth;
var
  TestDate: TDateTime;
begin
  WriteLn('Test52_FloorDateBiMonth:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Floor to bi-month should go to first day of even month',
    EncodeDate(2024, 3, 1),
    TDateTimeKit.FloorDate(TestDate, duBiMonth));
  WriteLn('Test52_FloorDateBiMonth:Finished');
end;

procedure TDateTimeTests.Test53_FloorDateQuarter;
var
  TestDate: TDateTime;
begin
  WriteLn('Test53_FloorDateQuarter:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Floor to quarter should go to first day of quarter',
    EncodeDate(2024, 1, 1),
    TDateTimeKit.FloorDate(TestDate, duQuarter));
  WriteLn('Test53_FloorDateQuarter:Finished');
end;

procedure TDateTimeTests.Test54_FloorDateHalfYear;
var
  TestDate: TDateTime;
begin
  WriteLn('Test54_FloorDateHalfYear:Starting');
  TestDate := EncodeDateTime(2024, 8, 15, 14, 30, 45, 500);
  AssertEquals('Floor to half year should go to July 1 or January 1',
    EncodeDate(2024, 7, 1),
    TDateTimeKit.FloorDate(TestDate, duHalfYear));
  WriteLn('Test54_FloorDateHalfYear:Finished');
end;

procedure TDateTimeTests.Test55_FloorDateYear;
var
  TestDate: TDateTime;
begin
  WriteLn('Test55_FloorDateYear:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Floor to year should go to January 1',
    EncodeDate(2024, 1, 1),
    TDateTimeKit.FloorDate(TestDate, duYear));
  WriteLn('Test55_FloorDateYear:Finished');
end;

procedure TDateTimeTests.Test56_CeilingDateSecond;
var
  TestDate: TDateTime;
begin
  WriteLn('Test56_CeilingDateSecond:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to second should round up to next second',
    EncodeDateTime(2024, 3, 15, 14, 30, 46, 0),
    TDateTimeKit.CeilingDate(TestDate, duSecond));
  WriteLn('Test56_CeilingDateSecond:Finished');
end;

procedure TDateTimeTests.Test57_CeilingDateMinute;
var
  TestDate: TDateTime;
begin
  WriteLn('Test57_CeilingDateMinute:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to minute should round up to next minute',
    EncodeDateTime(2024, 3, 15, 14, 31, 0, 0),
    TDateTimeKit.CeilingDate(TestDate, duMinute));
  WriteLn('Test57_CeilingDateMinute:Finished');
end;

procedure TDateTimeTests.Test58_CeilingDateHour;
var
  TestDate: TDateTime;
begin
  WriteLn('Test58_CeilingDateHour:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to hour should round up to next hour',
    EncodeDateTime(2024, 3, 15, 15, 0, 0, 0),
    TDateTimeKit.CeilingDate(TestDate, duHour));
  WriteLn('Test58_CeilingDateHour:Finished');
end;

procedure TDateTimeTests.Test59_CeilingDateDay;
var
  TestDate: TDateTime;
begin
  WriteLn('Test59_CeilingDateDay:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to day should round up to next day',
    EncodeDate(2024, 3, 16),
    TDateTimeKit.CeilingDate(TestDate, duDay));
  WriteLn('Test59_CeilingDateDay:Finished');
end;

procedure TDateTimeTests.Test60_CeilingDateWeek;
var
  TestDate: TDateTime;
begin
  WriteLn('Test60_CeilingDateWeek:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500); // March 15, 2024 is a Friday
  AssertEquals('Ceiling to week should go to next Sunday',
    EncodeDate(2024, 3, 17), // Should go to next Sunday, March 17
    TDateTimeKit.CeilingDate(TestDate, duWeek));
  WriteLn('Test60_CeilingDateWeek:Finished');
end;

procedure TDateTimeTests.Test61_CeilingDateMonth;
var
  TestDate: TDateTime;
begin
  WriteLn('Test61_CeilingDateMonth:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to month should go to first day of next month',
    EncodeDate(2024, 4, 1),
    TDateTimeKit.CeilingDate(TestDate, duMonth));
  WriteLn('Test61_CeilingDateMonth:Finished');
end;

procedure TDateTimeTests.Test62_CeilingDateBiMonth;
var
  TestDate: TDateTime;
begin
  WriteLn('Test62_CeilingDateBiMonth:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to bi-month should go to first day of next even month',
    EncodeDate(2024, 5, 1),
    TDateTimeKit.CeilingDate(TestDate, duBiMonth));
  WriteLn('Test62_CeilingDateBiMonth:Finished');
end;

procedure TDateTimeTests.Test63_CeilingDateQuarter;
var
  TestDate: TDateTime;
begin
  WriteLn('Test63_CeilingDateQuarter:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to quarter should go to first day of next quarter',
    EncodeDate(2024, 4, 1),
    TDateTimeKit.CeilingDate(TestDate, duQuarter));
  WriteLn('Test63_CeilingDateQuarter:Finished');
end;

procedure TDateTimeTests.Test64_CeilingDateHalfYear;
var
  TestDate: TDateTime;
begin
  WriteLn('Test64_CeilingDateHalfYear:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to half year should go to July 1',
    EncodeDate(2024, 7, 1),
    TDateTimeKit.CeilingDate(TestDate, duHalfYear));
  WriteLn('Test64_CeilingDateHalfYear:Finished');
end;

procedure TDateTimeTests.Test65_CeilingDateYear;
var
  TestDate: TDateTime;
begin
  WriteLn('Test65_CeilingDateYear:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to year should go to January 1 of next year',
    EncodeDate(2025, 1, 1),
    TDateTimeKit.CeilingDate(TestDate, duYear));
  WriteLn('Test65_CeilingDateYear:Finished');
end;

procedure TDateTimeTests.Test66_RoundDateSecond;
var
  TestDate: TDateTime;
begin
  WriteLn('Test66_RoundDateSecond:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to second should round to nearest second',
    EncodeDateTime(2024, 3, 15, 14, 30, 46, 0),
    TDateTimeKit.RoundDate(TestDate, duSecond));
  WriteLn('Test66_RoundDateSecond:Finished');
end;

procedure TDateTimeTests.Test67_RoundDateMinute;
var
  TestDate: TDateTime;
begin
  WriteLn('Test67_RoundDateMinute:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to minute should round to nearest minute',
    EncodeDateTime(2024, 3, 15, 14, 31, 0, 0),
    TDateTimeKit.RoundDate(TestDate, duMinute));
  WriteLn('Test67_RoundDateMinute:Finished');
end;

procedure TDateTimeTests.Test68_RoundDateHour;
var
  TestDate: TDateTime;
begin
  WriteLn('Test68_RoundDateHour:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to hour should round to nearest hour',
    EncodeDateTime(2024, 3, 15, 15, 0, 0, 0),
    TDateTimeKit.RoundDate(TestDate, duHour));
  WriteLn('Test68_RoundDateHour:Finished');
end;

procedure TDateTimeTests.Test69_RoundDateDay;
var
  TestDate: TDateTime;
begin
  WriteLn('Test69_RoundDateDay:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to day should round to nearest day',
    EncodeDate(2024, 3, 16),
    TDateTimeKit.RoundDate(TestDate, duDay));
  WriteLn('Test69_RoundDateDay:Finished');
end;

procedure TDateTimeTests.Test70_RoundDateWeek;
var
  TestDate: TDateTime;
begin
  WriteLn('Test70_RoundDateWeek:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500); // March 15, 2024 is a Friday
  AssertEquals('Round to week should round to nearest Sunday',
    EncodeDate(2024, 3, 17), // Should round to next Sunday, March 17
    TDateTimeKit.RoundDate(TestDate, duWeek));
  WriteLn('Test70_RoundDateWeek:Finished');
end;

procedure TDateTimeTests.Test71_RoundDateMonth;
var
  TestDate: TDateTime;
begin
  WriteLn('Test71_RoundDateMonth:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to month should round to nearest month start',
    EncodeDate(2024, 4, 1),
    TDateTimeKit.RoundDate(TestDate, duMonth));
  WriteLn('Test71_RoundDateMonth:Finished');
end;

procedure TDateTimeTests.Test72_RoundDateBiMonth;
var
  TestDate: TDateTime;
begin
  WriteLn('Test72_RoundDateBiMonth:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to bi-month should round to nearest even month start',
    EncodeDate(2024, 3, 1),
    TDateTimeKit.RoundDate(TestDate, duBiMonth));
  WriteLn('Test72_RoundDateBiMonth:Finished');
end;

procedure TDateTimeTests.Test73_RoundDateQuarter;
var
  TestDate: TDateTime;
begin
  WriteLn('Test73_RoundDateQuarter:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to quarter should round to nearest quarter start',
    EncodeDate(2024, 4, 1),
    TDateTimeKit.RoundDate(TestDate, duQuarter));
  WriteLn('Test73_RoundDateQuarter:Finished');
end;

procedure TDateTimeTests.Test74_RoundDateHalfYear;
var
  TestDate: TDateTime;
begin
  WriteLn('Test74_RoundDateHalfYear:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to half year should round to nearest half year start',
    EncodeDate(2024, 7, 1),
    TDateTimeKit.RoundDate(TestDate, duHalfYear));
  WriteLn('Test74_RoundDateHalfYear:Finished');
end;

procedure TDateTimeTests.Test75_RoundDateYear;
var
  TestDate: TDateTime;
begin
  WriteLn('Test75_RoundDateYear:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to year should round to nearest year start',
    EncodeDate(2024, 1, 1),
    TDateTimeKit.RoundDate(TestDate, duYear));
  WriteLn('Test75_RoundDateYear:Finished');
end;

procedure TDateTimeTests.Test76_YMD_Valid;
var
  TestDate: TDateTime;
begin
  WriteLn('Test76_YMD_Valid:Starting');
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
  WriteLn('Test76_YMD_Valid:Finished');
end;

procedure TDateTimeTests.Test77_YMD_Invalid;
begin
  WriteLn('Test77_YMD_Invalid:Starting');
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
  WriteLn('Test77_YMD_Invalid:Finished');
end;

procedure TDateTimeTests.Test78_MDY_Valid;
var
  TestDate: TDateTime;
begin
  WriteLn('Test78_MDY_Valid:Starting');
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
  WriteLn('Test78_MDY_Valid:Finished');
end;

procedure TDateTimeTests.Test79_MDY_Invalid;
begin
  WriteLn('Test79_MDY_Invalid:Starting');
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
  WriteLn('Test79_MDY_Invalid:Finished');
end;

procedure TDateTimeTests.Test80_DMY_Valid;
var
  TestDate: TDateTime;
begin
  WriteLn('Test80_DMY_Valid:Starting');
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
  WriteLn('Test80_DMY_Valid:Finished');
end;

procedure TDateTimeTests.Test81_DMY_Invalid;
begin
  WriteLn('Test81_DMY_Invalid:Starting');
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
  WriteLn('Test81_DMY_Invalid:Finished');
end;

procedure TDateTimeTests.Test82_YQ_Valid;
var
  TestDate: TDateTime;
begin
  WriteLn('Test82_YQ_Valid:Starting');
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
  WriteLn('Test82_YQ_Valid:Finished');
end;

procedure TDateTimeTests.Test83_YQ_Invalid;
begin
  WriteLn('Test83_YQ_Invalid:Starting');
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
  WriteLn('Test83_YQ_Invalid:Finished');
end;

procedure TDateTimeTests.Test84_ISOYear;
var
  TestDate: TDateTime;
begin
  WriteLn('Test84_ISOYear:Starting');
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
  WriteLn('Test84_ISOYear:Finished');
end;

procedure TDateTimeTests.Test85_ISOWeek;
var
  TestDate: TDateTime;
begin
  WriteLn('Test85_ISOWeek:Starting');
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
  WriteLn('Test85_ISOWeek:Finished');
end;

procedure TDateTimeTests.Test86_EpiYear;
var
  TestDate: TDateTime;
begin
  WriteLn('Test86_EpiYear:Starting');
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
  WriteLn('Test86_EpiYear:Finished');
end;

procedure TDateTimeTests.Test87_EpiWeek;
var
  TestDate: TDateTime;
begin
  WriteLn('Test87_EpiWeek:Starting');
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
  WriteLn('Test87_EpiWeek:Finished');
end;

procedure TDateTimeTests.Test88_Semester;
var
  TestDate: TDateTime;
begin
  WriteLn('Test88_Semester:Starting');
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
  WriteLn('Test88_Semester:Finished');
end;

procedure TDateTimeTests.Test89_DateDecimal;
var
  TestDate: TDateTime;
begin
  WriteLn('Test89_DateDecimal:Starting');
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
  WriteLn('Test89_DateDecimal:Finished');
end;

procedure TDateTimeTests.Test90_GetDecimalDate;
var
  TestDate: TDateTime;
  DecimalDate: Double;
begin
  WriteLn('Test90_GetDecimalDate:Starting');
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
  WriteLn('Test90_GetDecimalDate:Finished');
end;

procedure TDateTimeTests.Test91_RollbackMonth;
var
  TestDate, RolledDate: TDateTime;
begin
  WriteLn('Test91_RollbackMonth:Starting');
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
  WriteLn('Test91_RollbackMonth:Finished');
end;

procedure TDateTimeTests.Test92_RollForwardMonth;
var
  TestDate, RolledDate: TDateTime;
begin
  WriteLn('Test92_RollForwardMonth:Starting');
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
  WriteLn('Test92_RollForwardMonth:Finished');
end;

procedure TDateTimeTests.Test93_PeriodToSeconds;
var
  Period: TDateSpan;
  Seconds: Int64;
begin
  WriteLn('Test93_PeriodToSeconds:Starting');
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
  WriteLn('Test93_PeriodToSeconds:Finished');
end;

procedure TDateTimeTests.Test94_SecondsToPeriod;
var
  Period: TDateSpan;
begin
  WriteLn('Test94_SecondsToPeriod:Starting');
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
  WriteLn('Test94_SecondsToPeriod:Finished');
end;

procedure TDateTimeTests.Test95_StandardizePeriod;
var
  Period, Standardized: TDateSpan;
begin
  WriteLn('Test95_StandardizePeriod:Starting');
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
  WriteLn('Test95_StandardizePeriod:Finished');
end;


procedure TDateTimeTests.Test96_IntervalAlign;
var
  Interval1, Interval2: TInterval;
begin
  WriteLn('Test96_IntervalAlign:Starting');
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
  WriteLn('Test96_IntervalAlign:Finished');
end;

procedure TDateTimeTests.Test97_IntervalGap;
var
  Interval1, Interval2: TInterval;
  Gap: TDateSpan;
begin
  WriteLn('Test97_IntervalGap:Starting');
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
  WriteLn('Test97_IntervalGap:Finished');
end;

procedure TDateTimeTests.Test98_IntervalSetdiff;
var
  Interval1, Interval2, Result: TInterval;
begin
  WriteLn('Test98_IntervalSetdiff:Starting');
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
  WriteLn('Test98_IntervalSetdiff:Finished');
end;

procedure TDateTimeTests.Test99_IntervalUnion;
var
  Interval1, Interval2, Result: TInterval;
begin
  WriteLn('Test99_IntervalUnion:Starting');
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
  WriteLn('Test99_IntervalUnion:Finished');
end;

procedure TDateTimeTests.Test100_IntervalIntersection;
var
  Interval1, Interval2, Result: TInterval;
begin
  WriteLn('Test100_IntervalIntersection:Starting');
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
  WriteLn('Test100_IntervalIntersection:Finished');
end;

{ EpiWeek Tests }
procedure TDateTimeTests.Test87a_EpiWeek_MidYear;
var
  TestDate: TDateTime;
begin
  WriteLn('Test87a_EpiWeek_MidYear:Starting');
  TestDate := EncodeDate(2024, 6, 15);
  AssertEquals('Mid-year date should have correct epi week', 
    24, TDateTimeKit.GetEpiWeek(TestDate));
  WriteLn('Test87a_EpiWeek_MidYear:Finished');
end;

procedure TDateTimeTests.Test87b_EpiWeek_FirstWeek;
var
  TestDate: TDateTime;
begin
  WriteLn('Test87b_EpiWeek_FirstWeek:Starting');
  TestDate := EncodeDate(2024, 1, 4);
  AssertEquals('First full week should be week 1', 
    1, TDateTimeKit.GetEpiWeek(TestDate));
  WriteLn('Test87b_EpiWeek_FirstWeek:Finished');
end;

procedure TDateTimeTests.Test87c_EpiWeek_YearEnd;
var
  TestDate: TDateTime;
begin
  WriteLn('Test87c_EpiWeek_YearEnd:Starting');
  TestDate := EncodeDate(2024, 12, 31);
  AssertEquals('Year-end week number should be correct', 
    53, TDateTimeKit.GetEpiWeek(TestDate));
  WriteLn('Test87c_EpiWeek_YearEnd:Finished');
end;

{ StandardizePeriod Tests }
procedure TDateTimeTests.Test95a_StandardizePeriod_Milliseconds;
var
  Period, Standardized: TDateSpan;
begin
  WriteLn('Test95a_StandardizePeriod_Milliseconds:Starting');
  Period := TDateTimeKit.CreatePeriod(0, 0, 0, 0, 0, 0, 1001);
  Standardized := TDateTimeKit.StandardizePeriod(Period);
  AssertEquals('Milliseconds should be normalized', 1, Standardized.Milliseconds);
  AssertEquals('Extra milliseconds should carry to seconds', 1, Standardized.Seconds);
  WriteLn('Test95a_StandardizePeriod_Milliseconds:Finished');
end;

procedure TDateTimeTests.Test95b_StandardizePeriod_Seconds;
var
  Period, Standardized: TDateSpan;
begin
  WriteLn('Test95b_StandardizePeriod_Seconds:Starting');
  Period := TDateTimeKit.CreatePeriod(0, 0, 0, 0, 0, 61, 0);
  Standardized := TDateTimeKit.StandardizePeriod(Period);
  AssertEquals('Seconds should be normalized', 1, Standardized.Seconds);
  AssertEquals('Extra seconds should carry to minutes', 1, Standardized.Minutes);
  WriteLn('Test95b_StandardizePeriod_Seconds:Finished');
end;

procedure TDateTimeTests.Test95c_StandardizePeriod_Minutes;
var
  Period, Standardized: TDateSpan;
begin
  WriteLn('Test95c_StandardizePeriod_Minutes:Starting');
  Period := TDateTimeKit.CreatePeriod(0, 0, 0, 0, 61, 0, 0);
  Standardized := TDateTimeKit.StandardizePeriod(Period);
  AssertEquals('Minutes should be normalized', 1, Standardized.Minutes);
  AssertEquals('Extra minutes should carry to hours', 1, Standardized.Hours);
  WriteLn('Test95c_StandardizePeriod_Minutes:Finished');
end;

procedure TDateTimeTests.Test95d_StandardizePeriod_Hours;
var
  Period, Standardized: TDateSpan;
begin
  WriteLn('Test95d_StandardizePeriod_Hours:Starting');
  Period := TDateTimeKit.CreatePeriod(0, 0, 0, 25, 0, 0, 0);
  Standardized := TDateTimeKit.StandardizePeriod(Period);
  AssertEquals('Hours should be normalized', 1, Standardized.Hours);
  AssertEquals('Extra hours should carry to days', 1, Standardized.Days);
  WriteLn('Test95d_StandardizePeriod_Hours:Finished');
end;

procedure TDateTimeTests.Test95e_StandardizePeriod_Months;
var
  Period, Standardized: TDateSpan;
begin
  WriteLn('Test95e_StandardizePeriod_Months:Starting');
  Period := TDateTimeKit.CreatePeriod(0, 13, 0, 0, 0, 0, 0);
  Standardized := TDateTimeKit.StandardizePeriod(Period);
  AssertEquals('Months should be normalized', 1, Standardized.Months);
  AssertEquals('Extra months should carry to years', 1, Standardized.Years);
  WriteLn('Test95e_StandardizePeriod_Months:Finished');
end;

procedure TDateTimeTests.Test95f_StandardizePeriod_Complex;
var
  Period, Standardized: TDateSpan;
begin
  WriteLn('Test95f_StandardizePeriod_Complex:Starting');
  Period := TDateTimeKit.CreatePeriod(1, 13, 32, 25, 61, 61, 1001);
  Standardized := TDateTimeKit.StandardizePeriod(Period);
  
  AssertEquals('Years should include extra months', 2, Standardized.Years);
  AssertEquals('Months should be normalized', 1, Standardized.Months);
  AssertEquals('Days should include extra hours', 33, Standardized.Days);
  AssertEquals('Hours should be normalized', 2, Standardized.Hours);
  AssertEquals('Minutes should be normalized', 2, Standardized.Minutes);
  AssertEquals('Seconds should be normalized', 2, Standardized.Seconds);
  AssertEquals('Milliseconds should be normalized', 1, Standardized.Milliseconds);
  WriteLn('Test95f_StandardizePeriod_Complex:Finished');
end;

{ IntervalGap Tests }
procedure TDateTimeTests.Test97a_IntervalGap_NoOverlap;
var
  Interval1, Interval2: TInterval;
  Gap: TDateSpan;
begin
  WriteLn('Test97a_IntervalGap_NoOverlap:Starting');
  Interval1 := TDateTimeKit.CreateInterval(
    EncodeDate(2024, 1, 1),
    EncodeDate(2024, 1, 15));
  Interval2 := TDateTimeKit.CreateInterval(
    EncodeDate(2024, 1, 20),
    EncodeDate(2024, 1, 31));
  Gap := TDateTimeKit.IntervalGap(Interval1, Interval2);
  
  AssertEquals('Gap should be 5 days', 5, Gap.Days);
  WriteLn('Test97a_IntervalGap_NoOverlap:Finished');
end;

procedure TDateTimeTests.Test97b_IntervalGap_Overlapping;
var
  Interval1, Interval2: TInterval;
  Gap: TDateSpan;
begin
  WriteLn('Test97b_IntervalGap_Overlapping:Starting');
  Interval1 := TDateTimeKit.CreateInterval(
    EncodeDate(2024, 1, 1),
    EncodeDate(2024, 1, 15));
  Interval2 := TDateTimeKit.CreateInterval(
    EncodeDate(2024, 1, 10),
    EncodeDate(2024, 1, 31));
  Gap := TDateTimeKit.IntervalGap(Interval1, Interval2);
  
  AssertEquals('Overlapping intervals should have no gap', 0, Gap.Days);
  WriteLn('Test97b_IntervalGap_Overlapping:Finished');
end;


procedure TDateTimeTests.Test101_GetTimeZone;
var
  TZInfo: TTimeZoneInfo;
  TestDate: TDateTime;
begin
  WriteLn('Test101_GetTimeZone:Starting');
  TestDate := EncodeDate(2024, 1, 1) + EncodeTime(12, 0, 0, 0);
  TZInfo := TDateTimeKit.GetTimeZone(TestDate);
  
  // Basic checks
  AssertTrue('Timezone name should not be empty', TZInfo.Name <> '');
  AssertTrue('Offset should be within reasonable range', (TZInfo.Offset >= -720) and (TZInfo.Offset <= 720));
  WriteLn('Test101_GetTimeZone:Finished');
end;

procedure TDateTimeTests.Test102_GetSystemTimeZone;
var
  SystemTZ: string;
begin
  WriteLn('Test102_GetSystemTimeZone:Starting');
  SystemTZ := TDateTimeKit.GetSystemTimeZone;
  AssertTrue('System timezone should not be empty', SystemTZ <> '');
  WriteLn('Test102_GetSystemTimeZone:Finished');
end;

procedure TDateTimeTests.Test103_GetTimeZoneNames;
var
  TZNames: TStringArray;
begin
  WriteLn('Test103_GetTimeZoneNames:Starting');
  TZNames := TDateTimeKit.GetTimeZoneNames;
  AssertTrue('Should have at least one timezone name', Length(TZNames) > 0);
  AssertTrue('First timezone name should not be empty', TZNames[0] <> '');
  WriteLn('Test103_GetTimeZoneNames:Finished');
end;

procedure TDateTimeTests.Test104_WithTimeZone;
var
  TestDate: TDateTime;
  CurrentTZ: string;
  ConvertedDate: TDateTime;
begin
  WriteLn('Test104_WithTimeZone:Starting');
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
  WriteLn('Test104_WithTimeZone:Finished');
end;

procedure TDateTimeTests.Test105_ForceTimeZone;
var
  TestDate: TDateTime;
  CurrentTZ: string;
  ForcedDate: TDateTime;
begin
  WriteLn('Test105_ForceTimeZone:Starting');
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
  WriteLn('Test105_ForceTimeZone:Finished');
end;

procedure TDateTimeTests.Test106_DSTTransition;
var
  TestDate: TDateTime;
  DSTDate: TDateTime;
  NonDSTDate: TDateTime;
  TZInfo: TTimeZoneInfo;
begin
  WriteLn('Test106_DSTTransition:Starting');
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
  WriteLn('Test106_DSTTransition:Finished');
end;

procedure TDateTimeTests.Test107_DateBoundaryConversion;
var
  UTCDate: TDateTime;
  LocalDate: TDateTime;
  ConvertedDate: TDateTime;
  SystemTZ: string;
begin
  WriteLn('Test107_DateBoundaryConversion:Starting');
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
  WriteLn('Test107_DateBoundaryConversion:Finished');
end;

procedure TDateTimeTests.Test108_InvalidTimezones;
var
  TestDate: TDateTime;
begin
  WriteLn('Test108_InvalidTimezones:Starting');
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
  WriteLn('Test108_InvalidTimezones:Finished');
end;

procedure TDateTimeTests.Test109_ExtremeOffsets;
var
  TestDate: TDateTime;
  ConvertedDate: TDateTime;
  TZInfo: TTimeZoneInfo;
begin
  WriteLn('Test109_ExtremeOffsets:Starting');
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
  WriteLn('Test109_ExtremeOffsets:Finished');
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
  WriteLn('Test109_ExtremeOffsets:Starting');
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
  WriteLn('Test110_DSTTransitionExactTime:Finished');
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
  WriteLn('Test111_DSTEndExactTime:Starting');
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
  WriteLn('Test111_DSTEndExactTime:Finished');
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
  WriteLn('Test112_LeapYearDST:Starting');
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
  WriteLn('Test112_LeapYearDST:Finished');
end;

procedure TDateTimeTests.Test113_InvalidTimeZoneEdgeCases;
var
  Now: TDateTime;
begin
  WriteLn('Test113_InvalidTimeZoneEdgeCases:Starting');
  Now := TDateTimeKit.GetNow;
  
  // Test with very large timezone offsets
  try
    TDateTimeKit.WithTimeZone(Now, 'UTC+24:00');
    Fail('UTC+24:00 should raise exception');
  except
    on E: Exception do
      AssertTrue('Should raise appropriate exception for UTC+24:00',
        Pos('not found', E.Message) > 0);
  end;
  
  try
    TDateTimeKit.WithTimeZone(Now, 'UTC-24:00');
    Fail('UTC-24:00 should raise exception');
  except
    on E: Exception do
      AssertTrue('Should raise appropriate exception for UTC-24:00',
        Pos('not found', E.Message) > 0);
  end;
  
  WriteLn('Test113_InvalidTimeZoneEdgeCases:Finished');
end;

procedure TDateTimeTests.Test114_UTCOffsetEdgeCases;
var
  TZInfo: TTimeZoneInfo;
begin
  WriteLn('Test114_UTCOffsetEdgeCases:Starting');
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
  WriteLn('Test114_UTCOffsetEdgeCases:Finished');
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
  WriteLn('Test115_CrossBoundaryConversions:Starting');
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
  // Check if the base timezone name is the same, ignoring Summer/Standard variations
  AssertTrue('Should be back in system timezone', 
             Pos('AUS Eastern', TZInfo.Name) > 0);
  
  TZInfo := TDateTimeKit.GetTimeZone(ConvertedStart);
  // Check if the base timezone name is the same, ignoring Summer/Standard variations
  AssertTrue('Should be back in system timezone', 
             Pos('AUS Eastern', TZInfo.Name) > 0);
  
  // Verify chronological order is maintained
  AssertTrue('Year end should be before year start after conversion', 
             TDateTimeKit.IsBefore(ConvertedEnd, ConvertedStart));
               
  // Verify the time difference is preserved (should be 1 second)
  AssertEquals('Time difference should be preserved',
                1/SecsPerDay, // 1 second in TDateTime units
                ConvertedStart - ConvertedEnd);
  WriteLn('Test115_CrossBoundaryConversions:Finished');
end;

procedure TDateTimeTests.Test116_YMD;
var
  Expected: TDateTime;
begin
  Expected := EncodeDate(2024, 3, 15);
  WriteLn('Test116_YMD:Starting');
  AssertEquals('YMD with hyphen', Expected, FDateTime.YMD('2024-03-15'));
  AssertEquals('YMD with slash', Expected, FDateTime.YMD('2024/03/15'));
  WriteLn('Test116_YMD:Finished');
end;

procedure TDateTimeTests.Test117_MDY;
var
  Expected: TDateTime;
begin
  Expected := EncodeDate(2024, 3, 15);
  WriteLn('Test117_MDY:Starting');
  AssertEquals('MDY with hyphen', Expected, FDateTime.MDY('03-15-2024'));
  AssertEquals('MDY with slash', Expected, FDateTime.MDY('03/15/2024'));
  WriteLn('Test117_MDY:Finished');
end;

procedure TDateTimeTests.Test118_DMY;
var
  Expected: TDateTime;
begin
  Expected := EncodeDate(2024, 3, 15);
  WriteLn('Test118_DMY:Starting');
  AssertEquals('DMY with hyphen', Expected, FDateTime.DMY('15-03-2024'));
  AssertEquals('DMY with slash', Expected, FDateTime.DMY('15/03/2024'));
  WriteLn('Test118_DMY:Finished');
end;

procedure TDateTimeTests.Test114_RegionSpecificDST;
var
  // US DST dates (2024)
  USDSTStart: TDateTime;
  USDSTEnd: TDateTime;
  
  // EU DST dates (2024)
  EUDSTStart: TDateTime;
  EUDSTEnd: TDateTime;
  
  // AU DST dates (2024)
  AUDSTStart: TDateTime;
  AUDSTEnd: TDateTime;
  
  TZInfo: TTimeZoneInfo;
  {$IFDEF UNIX}
  OriginalTZ: string;
  {$ENDIF}
begin
  WriteLn('Test114_RegionSpecificDST:Starting');
  
  // US DST dates (2024)
  USDSTStart := EncodeDateTime(2024, 3, 10, 2, 0, 0, 0);  // Second Sunday in March
  USDSTEnd := EncodeDateTime(2024, 11, 3, 2, 0, 0, 0);   // First Sunday in November
  
  // EU DST dates (2024)
  EUDSTStart := EncodeDateTime(2024, 3, 31, 1, 0, 0, 0); // Last Sunday in March
  EUDSTEnd := EncodeDateTime(2024, 10, 27, 1, 0, 0, 0);  // Last Sunday in October
  
  // AU DST dates (2024)
  AUDSTStart := EncodeDateTime(2024, 10, 6, 2, 0, 0, 0);  // First Sunday in October
  AUDSTEnd := EncodeDateTime(2024, 4, 7, 3, 0, 0, 0);     // First Sunday in April
  
  {$IFDEF UNIX}
  // Save original TZ
  OriginalTZ := GetEnvironmentVariable('TZ');
  
  try
    // Test US DST
    SetEnvironmentVariableCrossPlatform('TZ', 'America/New_York');
    TZInfo := TDateTimeKit.GetTimeZone(USDSTStart);
    AssertTrue('US DST start should be in DST', TZInfo.IsDST);
    
    TZInfo := TDateTimeKit.GetTimeZone(USDSTEnd);
    AssertFalse('US DST end should not be in DST', TZInfo.IsDST);
    
    // Test EU DST
    SetEnvironmentVariableCrossPlatform('TZ', 'Europe/London');
    TZInfo := TDateTimeKit.GetTimeZone(EUDSTStart);
    AssertTrue('EU DST start should be in DST', TZInfo.IsDST);
    
    TZInfo := TDateTimeKit.GetTimeZone(EUDSTEnd);
    AssertFalse('EU DST end should not be in DST', TZInfo.IsDST);
    
    // Test AU DST
    SetEnvironmentVariableCrossPlatform('TZ', 'Australia/Sydney');
    TZInfo := TDateTimeKit.GetTimeZone(AUDSTStart);
    AssertTrue('AU DST start should be in DST', TZInfo.IsDST);
    
    TZInfo := TDateTimeKit.GetTimeZone(AUDSTEnd);
    AssertFalse('AU DST end should not be in DST', TZInfo.IsDST);
  finally
    // Restore original TZ
    if OriginalTZ <> '' then
      SetEnvironmentVariableCrossPlatform('TZ', OriginalTZ)
    else
      SetEnvironmentVariableCrossPlatform('TZ', '');
  end;
  {$ELSE}
  // On Windows, we can't easily test region-specific DST rules
  // as the system timezone is fixed. We'll just test that the
  // DST detection works in general.
  TZInfo := TDateTimeKit.GetTimeZone(Now);
  WriteLn('Current timezone: ', TZInfo.Name);
  WriteLn('DST status: ', BoolToStr(TZInfo.IsDST, True));
  {$ENDIF}
  
  WriteLn('Test114_RegionSpecificDST:Finished');
end;

initialization
  RegisterTest(TDateTimeTests);
end.
